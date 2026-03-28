use crate::types::{
	GrugValue, GrugFileId, GrugEntity, GameFnPtr,
};
use crate::ast::{
	Expr, ExprData, OnFunction, Statement,
	BinaryOperator, GrugType, HelperFunction, UnaryOperator
};
use crate::ntstring::{NTStrPtr, NTStr};
use crate::xar::{ErasedXar, ErasedPtr};
use gruggers_core::runtime_error::{RuntimeError, ON_FN_TIME_LIMIT, MAX_RECURSION_LIMIT};
use gruggers_core::state::State;
use gruggers_core::export_backend;
use crate::backend::Backend;
use crate::ast::GrugAst;

use std::collections::{HashMap, HashSet};
use std::ptr::NonNull;
use std::pin::Pin;
use std::cell::{Cell, RefCell};
use std::alloc::Layout;
use std::sync::Arc;
use std::time::{Duration, Instant};

struct Compiler<'a> {
	globals: HashMap<&'a str, u32>,
	locals: Vec<HashMap<&'a str, u32>>,
	locals_sizes: Vec<usize>,
	current_scope_size: usize,
	locals_size_max: u32,
	helper_fn_patches: Vec<(/* location of call instruction */ usize, /* arg_count */ u16, /* name */ &'a str)>,
	while_loop_patches: Vec<(/* continue destination */ usize, Vec</* break patch locations */ usize>)>,
}

impl<'a> Compiler<'a> {
	fn new() -> Self {
		Self {
			globals: HashMap::new(),
			locals: Vec::new(),
			locals_sizes: Vec::new(),
			current_scope_size: 0,
			locals_size_max: 0,
			helper_fn_patches: Vec::new(),
			while_loop_patches: Vec::new(),
		}
	}

	fn compile(ast: GrugAst<'a>) -> CompiledFile {
		let mut compiler = Compiler::new();
		let mut instructions = Instructions::new();

		let globals_size = ast.members.len() + 1;

		instructions.insert_on_fn("init_globals", 0, 0, 1, 0);
		let me_location = compiler.insert_global_variable("me");
		instructions.stream.push(Op::StoreGlobal{index: me_location});
		for global in ast.members.into_iter() {
			compiler.compile_expr(&mut instructions, &global.assignment_expr);
			let i = compiler.insert_global_variable(global.name.to_str());
			instructions.stream.push(Op::StoreGlobal{index: i});
		}
		instructions.stream.push(Op::ReturnVoid);

		for (i, on_function) in ast.on_functions.into_iter().enumerate() {
			let Some(on_function) = on_function else {continue};
			compiler.compile_on_fn(&mut instructions, *on_function, i + 1);
		}

		for helper_function in ast.helper_functions {
			compiler.compile_helper_fn(&mut instructions, helper_function);
		}
		for (patch_loc, args, name) in compiler.helper_fn_patches {
			let (location, locals_size) = instructions.get_helper_fn_info(&name)
				.expect("helper function exists");
			instructions.try_patch(Op::CallHelperFunction{args, locals_size, location}, patch_loc).unwrap();
		}
		// panic!("{}", instructions);
		CompiledFile {
			instructions,
			globals_size,
			entities: RefCell::new(Vec::new()),
			data: ErasedXar::new(Layout::array::<GrugValue>(globals_size)
				 .expect("invalid layout")
			),
		}
	}
	
	fn compile_helper_fn(&mut self, instructions: &mut Instructions, helper_function: &'a HelperFunction) {
		debug_assert_eq!(self.locals.len(), 0);
		debug_assert_eq!(self.current_scope_size, 0);
		debug_assert_eq!(self.locals_size_max, 0);
		debug_assert_eq!(self.while_loop_patches.len(), 0);
		self.push_scope();
		let begin_location = instructions.get_loc();
		for arg in helper_function.arguments {
			self.insert_local_variable(arg.name.to_str());
		}
		for statement in &*helper_function.body_statements {
			self.compile_statement(instructions, statement);
		}
		instructions.stream.push(Op::ReturnVoid);
		instructions.insert_helper_fn(helper_function.name.to_str(), begin_location, self.locals_size_max);
		self.locals_size_max = 0;
		self.pop_scope();
	}

	fn compile_on_fn(&mut self, instructions: &mut Instructions, on_function: &'a OnFunction, index: usize) {
		debug_assert_eq!(self.locals.len(), 0);
		debug_assert_eq!(self.current_scope_size, 0);
		debug_assert_eq!(self.locals_size_max, 0);
		debug_assert_eq!(self.while_loop_patches.len(), 0);
		self.push_scope();
		let begin_location = instructions.get_loc();
		let arg_count = on_function.arguments.len();
		for arg in on_function.arguments {
			self.insert_local_variable(arg.name.to_str());
		}
		for statement in &*on_function.body_statements {
			self.compile_statement(instructions, statement);
		}
		instructions.stream.push(Op::ReturnVoid);
		instructions.insert_on_fn(on_function.name.to_str(), index, begin_location, arg_count, self.locals_size_max);
		self.locals_size_max = 0;
		self.pop_scope();
	}

	fn compile_statement(&mut self, instructions: &mut Instructions, statement: &'a Statement) {
		match statement {
			Statement::Variable{
				name,
				ty,
				assignment_expr,
			} => {
				let name = name.to_str();
				self.compile_expr(instructions, assignment_expr);
				if let Some(_) = ty {
					let loc = self.insert_local_variable(name);
					instructions.stream.push(Op::StoreLocal{index: loc});
				} else {
					if let Some(loc) = self.get_local_location(name) {
						instructions.stream.push(Op::StoreLocal{index: loc});
					} else if let Some(loc) = self.get_global_location(name) {
						instructions.stream.push(Op::StoreGlobal{index: loc});
					} else {
						unreachable!();
					}
				}
			}
			Statement::If {
				condition,
				is_chained,
				if_block,
				else_block,
			} => {
				let mut condition = condition;
				let mut is_chained = is_chained;
				let mut if_block = if_block;
				let mut else_block = else_block;
				let mut end_patches = Vec::new();
				
				loop {
					self.compile_expr(instructions, condition);

					let condition_patch_loc = instructions.get_loc();
					instructions.stream.push(Op::JmpIfNot{offset: 0});

					self.push_scope();
					for statement in &**if_block {
						self.compile_statement(instructions, statement);
					}
					self.pop_scope();

					if !else_block.is_empty() {
						// Save the patch location from the end of this if
						// block to the end of the else block or the end of the
						// last if block
						end_patches.push(instructions.get_loc());
						instructions.stream.push(Op::Jmp{offset: 0});

						let cur_loc = instructions.get_loc();
						// jump from the false condtion to the start of the else block
						instructions.try_patch(Op::JmpIfNot{offset: Op::calc_offset(condition_patch_loc, cur_loc)}, condition_patch_loc)
							.expect("Could not patch jump because offset is too large");
						if *is_chained {
							debug_assert!(else_block.len() == 1);
							let [statement] = else_block else {unreachable!()};
							(condition, is_chained, if_block, else_block) = match statement {
								Statement::If{condition, is_chained, if_block, else_block} => (condition, is_chained, if_block, else_block),
								_ => unreachable!(),
							};
							continue;
						} else {
							self.push_scope();
							for statement in &**else_block {
								self.compile_statement(instructions, statement);
							}
							self.pop_scope();
						}
					} else {
						let cur_loc = instructions.get_loc();
						// jump from the false condtion to the end of the if statement
						instructions.try_patch(Op::JmpIfNot{offset: Op::calc_offset(condition_patch_loc, cur_loc)}, condition_patch_loc)
							.expect("Could not patch jump because offset is too large");
					}
					break;
				}
				let end_loc = instructions.get_loc();
				for end_patch_loc in end_patches {
					// jump from the end of the each if block to the end of the else block
					instructions.try_patch(Op::Jmp{offset: Op::calc_offset(end_patch_loc, end_loc)}, end_patch_loc)
						.expect("Could not patch jump because offset is too large");
				}
			}
			Statement::While {
				condition,
				block,
			} => {
				let continue_loc = instructions.get_loc();
				self.compile_expr(instructions, condition);
				let break_patch_loc = instructions.get_loc();
				instructions.stream.push(Op::JmpIfNot{offset: 0});

				self.while_loop_patches.push((continue_loc, Vec::new()));
				
				for statement in &**block {
					self.compile_statement(instructions, statement);
				}
				let end_loc = instructions.get_loc();
				instructions.stream.push(Op::Jmp{offset: Op::calc_offset(end_loc, continue_loc)});
				let break_loc = instructions.get_loc();
				instructions.try_patch(Op::JmpIfNot{offset: Op::calc_offset(break_patch_loc, break_loc)}, break_patch_loc).unwrap();

				for break_patch_loc in self.while_loop_patches.pop().unwrap().1 {
					instructions.try_patch(Op::Jmp{offset: Op::calc_offset(break_patch_loc, break_loc)}, break_patch_loc).unwrap();
				}
			}
			Statement::Call(expr) => self.compile_expr(instructions, expr),
			Statement::Return{expr} => {
				if let Some(expr) = expr {
					self.compile_expr(instructions, expr);
					instructions.stream.push(Op::ReturnValue);
				} else {
					instructions.stream.push(Op::ReturnVoid);
				}
			}
			Statement::Break => {
				self.while_loop_patches.last_mut().unwrap().1.push(instructions.get_loc());
				instructions.stream.push(Op::Jmp{offset: 0});
			}
			Statement::Continue => {
				let continue_loc = self.while_loop_patches.last().unwrap().0;
				instructions.stream.push(Op::Jmp{offset: Op::calc_offset(instructions.get_loc(), continue_loc)});
			}
			Statement::EmptyLine => {},
			Statement::Comment{..}        => {},
		}
	}

	fn compile_expr(&mut self, instructions: &mut Instructions, expr: &'a Expr) {
		match &expr.data {
			ExprData::True  => instructions.stream.push(Op::LoadTrue ),
			ExprData::False => instructions.stream.push(Op::LoadFalse),
			ExprData::String(value)   |
			ExprData::Resource(value) |
			ExprData::Entity(value)   => {
				match instructions.strings.get(value.to_ntstr()) {
					None => {
						let value = NTStr::arc_from_str(value.to_ntstr());
						// SAFETY: This returned instruction stream is only valid as long as this list of strings is available
						let string = unsafe{value.as_ntstrptr().detach_lifetime()};
						instructions.stream.push(Op::LoadStr{string});
						instructions.strings.insert(value);
					}
					Some(value) => {
						let string = unsafe{value.as_ntstrptr().detach_lifetime()};
						instructions.stream.push(Op::LoadStr{string})
					}
				}
			}
			ExprData::Number (value, _) => instructions.stream.push(Op::LoadNumber{number: *value}),
			ExprData::Identifier(name) => {
				let name = name.to_str();
				if let Some(loc) = self.get_local_location(name) {
					instructions.stream.push(Op::LoadLocal{index: loc});
				} else if let Some(loc) = self.get_global_location(name) {
					instructions.stream.push(Op::LoadGlobal{index: loc});
				} else {
					unreachable!();
				}
			}
			ExprData::Binary {
				left,
				right,
				op,
			} => {
				self.compile_expr(instructions, left);
				match op {
					BinaryOperator::Greater       => {self.compile_expr(instructions, right); instructions.stream.push(Op::CmpG);}
					BinaryOperator::GreaterEquals => {self.compile_expr(instructions, right); instructions.stream.push(Op::CmpGe);}
					BinaryOperator::Less          => {self.compile_expr(instructions, right); instructions.stream.push(Op::CmpL);}
					BinaryOperator::LessEquals    => {self.compile_expr(instructions, right); instructions.stream.push(Op::CmpLe);}
					BinaryOperator::Plus          => {self.compile_expr(instructions, right); instructions.stream.push(Op::Add);}
					BinaryOperator::Minus         => {self.compile_expr(instructions, right); instructions.stream.push(Op::Sub);}
					BinaryOperator::Multiply      => {self.compile_expr(instructions, right); instructions.stream.push(Op::Mul);}
					BinaryOperator::Division      => {self.compile_expr(instructions, right); instructions.stream.push(Op::Div);}
					BinaryOperator::Remainder     => {self.compile_expr(instructions, right); instructions.stream.push(Op::Rem);}
					BinaryOperator::DoubleEquals  => {
						match right.result_type.unwrap() {
							GrugType::String => {
								self.compile_expr(instructions, right);
								instructions.stream.push(Op::StrEq);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(instructions, right);
								instructions.stream.push(Op::CmpEq);
							}
						}
					}
					BinaryOperator::NotEquals     => {
						match right.result_type.unwrap() {
							GrugType::String => {
								self.compile_expr(instructions, right);
								instructions.stream.push(Op::StrEq);
								instructions.stream.push(Op::Not);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(instructions, right);
								instructions.stream.push(Op::CmpNeq);
							}
						}
					}
					BinaryOperator::Or            => {
						instructions.stream.push(Op::Dup{index: 0});
						let first_patch_loc = instructions.get_loc();
						instructions.stream.push(Op::JmpIf{offset: 0});
						self.compile_expr(instructions, right);
						instructions.try_patch(
							Op::JmpIf{
								offset: Op::calc_offset(first_patch_loc, instructions.get_loc()),
							},
							first_patch_loc,
						).unwrap();
					}
					BinaryOperator::And           => {
						instructions.stream.push(Op::Dup{index: 0});
						let first_patch_loc = instructions.get_loc();
						instructions.stream.push(Op::JmpIfNot{offset: 0});
						self.compile_expr(instructions, right);
						instructions.try_patch(
							Op::JmpIfNot{
								offset: Op::calc_offset(first_patch_loc, instructions.get_loc()),
							},
							first_patch_loc,
						).unwrap();
					}
				}
			}
			ExprData::Unary {
				op,
				expr,
			} => {
				self.compile_expr(instructions, expr);
				match op {
					UnaryOperator::Not   => instructions.stream.push(Op::Not),
					UnaryOperator::Minus => {instructions.stream.push(Op::LoadNumber{number: -1.0}); instructions.stream.push(Op::Mul);}
				}
			}
			ExprData::Call {
				name,
				args,
				ptr: None,
			} => {
				let args_count = args.len();
				if args_count > u16::MAX as usize {
					panic!("cannot have more than {} arguments in a helper functions", u16::MAX);
				}
				for argument in &**args {
					self.compile_expr(instructions, argument);
				}

				let (location, locals_size) = if let Some(info) = instructions.get_helper_fn_info(name.to_str()) {
					info
				} else {
					self.helper_fn_patches.push((instructions.get_loc(), args_count as u16, name.to_str()));
					(0, 0)
				};
				instructions.stream.push(Op::CallHelperFunction{args: args_count as u16, locals_size, location});
			},
			ExprData::Call {
				name: _,
				args,
				ptr: Some(ptr)
			} => {
				let args_count = args.len();
				for argument in &**args {
					self.compile_expr(instructions, argument);
				}
				
				let has_return = *expr.result_type.unwrap() != GrugType::Void;
				instructions.stream.push(Op::CallGameFunction {
					has_return,
					args: args_count as u32,
					ptr: *ptr,
				});
			}
			ExprData::Parenthesized(expr) => self.compile_expr(instructions, expr),
		}
	}

	fn insert_global_variable(&mut self, name: &'a str) -> u32 {
		let check = self.globals.insert(name, self.globals.len() as u32);
		debug_assert!(check.is_none());
		(self.globals.len() - 1) as u32
	}

	fn insert_local_variable(&mut self, name: &'a str) -> u32 {
		let check = self.locals.last_mut().unwrap().insert(name, self.current_scope_size as u32);
		debug_assert!(check.is_none());
		self.current_scope_size += 1;
		if self.current_scope_size > u32::MAX as usize {
			panic!("Cannot have more than {} local variables", u32::MAX);
		}
		self.locals_size_max = std::cmp::max(self.locals_size_max, self.current_scope_size as u32);
		(self.current_scope_size - 1) as u32
	}

	fn pop_scope(&mut self) {
		self.locals.pop().unwrap();
		self.current_scope_size = self.locals_sizes.pop().unwrap();
	}

	fn push_scope(&mut self) {
		self.locals.push(HashMap::new());
		self.locals_sizes.push(self.current_scope_size);
	}

	fn get_local_location(&self, name: &str) -> Option<u32> {
		self.locals.iter().rev().find_map(|x| x.get(name)).copied()
	}

	fn get_global_location(&self, name: &str) -> Option<u32> {
		self.globals.get(name).copied()
	}
}

#[derive(Debug)]
struct CompiledFile {
	instructions: Instructions,
	globals_size: usize,
	entities: RefCell<Vec<NonNull<GrugEntity>>>,
	data: ErasedXar,
}

pub struct BytecodeBackend {
	files: RefCell<Vec<CompiledFile>>,
	stacks: RefCell<Vec<Stack>>,
}

impl BytecodeBackend {
	pub fn new() -> Self {
		Self {
			files: RefCell::new(Vec::new()),
			stacks: RefCell::new(Vec::new()),
		}
	}
}

impl Backend for BytecodeBackend {
	fn insert_file<GrugState: State>(&self, state: &GrugState, id: GrugFileId, file: GrugAst) {
		let mut compiled_file = Compiler::compile(file);
		let mut files = self.files.borrow_mut();
		if let Some(old_file) = files.get_mut(id.0 as usize) {
			let mut old_entities = std::mem::replace(&mut *old_file.entities.borrow_mut(), std::vec::Vec::new());
			
			old_entities.extract_if(.., |old_entity| {
				debug_assert!(id == unsafe{(*old_entity.as_ptr()).file_id});
				let globals = unsafe{&*compiled_file.data.get_slot().write_slice(compiled_file.globals_size, Cell::new(GrugValue{void: ()}))};
				let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());

				stack.stack.push(GrugValue{id: unsafe{(*old_entity.as_ptr()).id}});
				let ret_val = unsafe{stack.run(state, globals, &compiled_file.instructions, 1, 0)}.is_some();
				unsafe{(*old_entity.as_ptr()).members.set(NonNull::from_ref(globals).cast::<()>())};

				self.stacks.borrow_mut().push(stack);

				!ret_val
			}).for_each(drop);
			*compiled_file.entities.get_mut() = old_entities;
			*old_file = compiled_file;
		} else if files.len() == id.0 as usize {
			files.push(compiled_file);
		} else {
			unreachable!("GrugScriptIds must be contigious, Expected {}, got {}", files.len(), id.0);
		}
	}
	fn init_entity<GrugState: State>(&self, state: &GrugState, entity: Pin<&GrugEntity>) -> bool {
		let files = self.files.borrow();
		let file = files.get(entity.file_id.0 as usize)
			.expect("file already compiled");
		
		let globals = unsafe{&*file.data.get_slot().write_slice(file.globals_size, Cell::new(GrugValue{void: ()}))};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		stack.stack.push(GrugValue{id: entity.id});
		let ret_val = unsafe{stack.run(state, globals, &file.instructions, 1, 0)}.is_some();
		entity.members.set(NonNull::from_ref(globals).cast::<()>());

		file.entities.borrow_mut().push(NonNull::from_ref(Pin::get_ref(entity)));

		stack = stack.reset();
		self.stacks.borrow_mut().push(stack);
		ret_val
	}
	fn clear_entities(&mut self) {
		for file in self.files.get_mut().iter_mut() {
			file.data.clear();
			file.entities.borrow_mut().clear();
		}
	}
	fn destroy_entity_data(&self, entity: &GrugEntity) {
		let files = self.files.borrow();
		let file = files.get(entity.file_id.0 as usize)
			.expect("file already compiled");
		unsafe{file.data.delete(ErasedPtr::from_ptr(entity.members.get()))};
		file.entities.borrow_mut().extract_if(.., |en| std::ptr::eq(en.as_ptr().cast_const(), entity)).for_each(|_| {});
	}
	unsafe fn call_on_function_raw<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		let files = self.files.borrow();
		let file = files.get(entity.file_id.0 as usize)
			.expect("file already compiled");

		let globals = unsafe{std::slice::from_raw_parts(entity.members.get().cast::<Cell<GrugValue>>().as_ptr(), file.globals_size)};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		let Some((start_loc, argument_count, locals_size)) = file.instructions.on_fn_locations[on_fn_index + 1] else {
			return false;
		};
		for i in 0..argument_count {
			unsafe{stack.stack.push(*values.add(i))}
		}
		let ret_val = unsafe{stack.run(state, globals, &file.instructions, locals_size, start_loc)}.is_some();

		stack = stack.reset();
		self.stacks.borrow_mut().push(stack);
		ret_val
	}
	fn call_on_function<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		let files = self.files.borrow();
		let file = files.get(entity.file_id.0 as usize)
			.expect("file already compiled");

		let globals = unsafe{std::slice::from_raw_parts(entity.members.get().cast::<Cell<GrugValue>>().as_ptr(), file.globals_size)};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		let Some(&Some((start_loc, argument_count, locals_size))) = file.instructions.on_fn_locations.get(on_fn_index + 1) else {
			return false;
		};
		if values.len() != argument_count {return false;}
		for value in values {
			stack.stack.push(*value)
		}
		let ret_val = unsafe{stack.run(state, globals, &file.instructions, locals_size, start_loc)}.is_some();

		stack = stack.reset();
		self.stacks.borrow_mut().push(stack);
		ret_val
	}
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Op {
	ReturnVoid = 0x00,
	ReturnValue,
	LoadNumber {
		number: f64,
	},
	LoadStr {
		string: NTStrPtr<'static>,
	},
	LoadFalse,
	LoadTrue,
	Dup{
		index: u32,
	},
	// Pop,
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	// And,
	// Or,
	Not,
	CmpEq,
	CmpNeq,
	StrEq,
	CmpG,
	CmpGe,
	CmpL,
	CmpLe,
	// PrintStr,
	LoadGlobal {
		index: u32,
	},
	StoreGlobal {
		index: u32,
	},
	Jmp {
		offset: i32,
	},
	JmpIfNot {
		offset: i32,
	},
	JmpIf {
		offset: i32,
	},
	LoadLocal {
		index: u32,
	},
	StoreLocal {
		index: u32,
	},
	CallHelperFunction {
		args: u16,
		locals_size: u32,
		location: usize,
	},
	CallGameFunction {
		has_return: bool,
		args: u32,
		ptr: GameFnPtr,
	},
}

impl PartialEq for Op {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::ReturnVoid, Self::ReturnVoid)       => true,
			(Self::ReturnValue, Self::ReturnValue)     => true,
			(Self::LoadNumber{number: n_0}, Self::LoadNumber{number: n_1}) => n_0 == n_1,
			(Self::LoadStr{string: s_0}, Self::LoadStr{string: s_1}) => s_0.to_str() == s_1.to_str(),
			(Self::LoadFalse, Self::LoadFalse)  => true,
			(Self::LoadTrue , Self::LoadTrue )  => true,
			(Self::Dup{index: i0}  , Self::Dup{index: i1})  => i0 == i1,
			// (Self::Pop      , Self::Pop      )  => true,
			(Self::Add      , Self::Add      )  => true,
			(Self::Sub      , Self::Sub      )  => true,
			(Self::Mul      , Self::Mul      )  => true,
			(Self::Div      , Self::Div      )  => true,
			(Self::Rem      , Self::Rem      )  => true,
			// (Self::And      , Self::And      )  => true,
			// (Self::Or       , Self::Or       )  => true,
			(Self::Not      , Self::Not      )  => true,
			(Self::CmpEq    , Self::CmpEq    )  => true,
			(Self::CmpNeq   , Self::CmpNeq   )  => true,
			(Self::StrEq    , Self::StrEq    )  => true,
			(Self::CmpG     , Self::CmpG     )  => true,
			(Self::CmpGe    , Self::CmpGe    )  => true,
			(Self::CmpL     , Self::CmpL     )  => true,
			(Self::CmpLe    , Self::CmpLe    )  => true,
			// (Self::PrintStr , Self::PrintStr )  => true,
			(Self::LoadGlobal {index: o1} , Self::LoadGlobal {index: o2}) => o1 == o2,
			(Self::StoreGlobal{index: o1} , Self::StoreGlobal{index: o2}) => o1 == o2,
			(Self::Jmp  {offset: o1} , Self::Jmp  {offset: o2}) => o1 == o2,
			(Self::JmpIf{offset: o1} , Self::JmpIf{offset: o2}) => o1 == o2,
			(Self::LoadLocal  {index: o1} , Self::LoadLocal  {index: o2}) => o1 == o2,
			(Self::StoreLocal {index: o1} , Self::StoreLocal {index: o2}) => o1 == o2,
			(Self::CallHelperFunction {args: args0, locals_size: locals_size0, location: idx0}, 
			 Self::CallHelperFunction {args: args1, locals_size: locals_size1, location: idx1}) => args0 == args1 && idx0 == idx1 && locals_size0 == locals_size1,
			(Self::CallGameFunction {has_return: kind0, args: args0, ptr: ptr0}, 
			 Self::CallGameFunction {has_return: kind1, args: args1, ptr: ptr1}) => kind0 == kind1 && args0 == args1 && ptr0 == ptr1,
			_ => false,
		}
	}
}

impl Op {
	fn calc_offset(from: usize, to: usize) -> i32 {
		let offset_from_start = to as isize - from as isize ;
		// opcode is 1 byte, offset fits in two bytes
		if offset_from_start.abs() > i32::MAX as isize {
			panic!("offset {} greater than max allowed {}", offset_from_start.abs(), i32::MAX);
		}
		(offset_from_start - 1) as i32
	}
}

struct Instructions{
	stream: Vec<Op>,
	on_fn_locations: Vec<
		Option<(
			/* start location in instruction stream */ 
			usize, 
			/* number of arguments */
			usize, 
			/* number of locals */ 
			u32,
		)>
	>,
	helper_fn_locations: HashMap<Arc<str>, (/* location */ usize, /* locals_size */ u32)>,
	fn_labels: HashMap<usize, Arc<str>>,
	strings: HashSet<Arc<NTStr>>,
	// jumps_count: usize,
	// jumps_end: HashMap</* to */ usize, (/* from */ Vec<usize>, /* label */ usize)>,
	// jumps_start: HashMap</* from */ usize, /* to */ usize>,
}

impl Instructions {
	pub fn new() -> Self {
		Self {
			stream: Vec::new(),
			on_fn_locations: Vec::new(),
			helper_fn_locations: HashMap::new(),
			fn_labels: HashMap::new(),
			strings: HashSet::new(),
			// jumps_count: 0,
			// jumps_start: HashMap::new(),
			// jumps_end: HashMap::new(),
		}
	}

	#[allow(unused)]
	pub fn clear(&mut self) { 
		self.stream.clear();
		self.on_fn_locations.clear();
		self.helper_fn_locations.clear();
		self.fn_labels.clear();
		self.strings.clear();
		// self.jumps_count = 0;
		// self.jumps_end.clear();
		// self.jumps_start.clear();
	}

	pub fn get_jump_ends(&self) -> HashMap<usize, usize> {
		let mut jumps = HashMap::new();
		for ins in &self.stream {
			// address of ins is within self.stream
			let addr = unsafe{(ins as *const Op).offset_from(self.stream.as_ptr())};
			match ins {
				Op::Jmp{offset} | 
				Op::JmpIf{offset} | 
				Op::JmpIfNot{offset} => {
					// This will never underflow for a well formed program
					debug_assert!(addr + 1 + *offset as isize >= 0);
					let cur_len = jumps.len();
					jumps.entry((addr + 1 + *offset as isize) as usize).or_insert(cur_len);
				}
				_ => (),
			}
		}
		jumps
	}

	pub fn insert_on_fn(&mut self, name: &str, index: usize, location: usize, argument_count: usize, locals_size: u32) {
		let name = Arc::from(name);
		if self.on_fn_locations.len() <= index {
			self.on_fn_locations.resize(index + 1, None);
		}
		self.on_fn_locations[index] = Some((location, argument_count, locals_size));
		self.fn_labels.insert(location, name);
	}

	pub fn get_helper_fn_info(&mut self, name: &str) -> Option<(/* location */ usize, /* locals_size */ u32)>{
		self.helper_fn_locations.get(name).copied()
	}

	pub fn insert_helper_fn(&mut self, name: &str, location: usize, locals_size: u32) {
		let name = Arc::from(name);
		self.helper_fn_locations.insert(Arc::clone(&name), (location, locals_size));
		self.fn_labels.insert(location, name);
	}

	/// SAFETY:
	/// `location` must be at the end of an instruction and the beginning of
	/// the next one
	/// Invalidates all instructions upto `location`.
	/// Any jumps that ends up past `location` needs to be redone
	///
	// pub unsafe fn rewind_to(&mut self, location: usize) {
	// 	self.stream.truncate(location);
	// 	for (start, end) in self.jumps_start.iter() {
	// 		if *start > location || *end > location {
	// 			self.jumps_end.get_mut(end).unwrap().0.retain(|x| x != start);
	// 		}
	// 	}
	// }

	pub fn get_loc(&self) -> usize {
		self.stream.len()
	}

	pub fn try_patch(&mut self, op: Op, location: usize) -> Option<()> {
		match (self.stream.get_mut(location)?, op) {
			(Op::Jmp{offset: patch}, Op::Jmp{offset}) | 
			(Op::JmpIf{offset: patch}, Op::JmpIf{offset}) |
			(Op::JmpIfNot{offset: patch}, Op::JmpIfNot{offset}) => {
				*patch = offset;
				Some(())
			}
			(Op::CallHelperFunction{args: patch_args, locals_size: patch_locals_size, location: patch_location}, 
			 Op::CallHelperFunction{args, locals_size, location}) => {
				*patch_args = args;
				*patch_locals_size = locals_size;
				*patch_location = location;
				Some(())
			}
			_ => None,
		}
	}
}

impl std::fmt::Debug for Instructions {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_list()
			.entries(
				self.stream.iter()
			)
			.finish()
	}
}

impl std::fmt::Display for Instructions {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let stream = &mut &*self.stream;
		let jumps_end = self.get_jump_ends();
		while let Some((ins, next)) = stream.split_first() {
			let addr = unsafe{(ins as *const Op).offset_from(self.stream.as_ptr())};
			if let Some(name) = self.fn_labels.get(&(addr as usize)) {
				writeln!(f, "{}:", name)?;
			}
			if let Some(label) = jumps_end.get(&(addr as usize)) {
				writeln!(f, "L_{}: ", label)?;
			}

			*stream = next;
			let end_addr = unsafe{stream.as_ptr().offset_from(self.stream.as_ptr())};
			write!(f, " 0x{:08x} ", addr)?;
			match ins {
				Op::ReturnVoid => write!(f, "ReturnVoid"),
				Op::ReturnValue => write!(f, "ReturnValue"),
				Op::LoadStr {
					string,
				} => write!(f, "LoadStr {:?}", string.to_str()),
				Op::LoadNumber {
					number,
				} => write!(f, "Load {:?}", number),
				Op::LoadFalse => write!(f, "LoadFalse"),
				Op::LoadTrue => write!(f, "LoadTrue"),
				Op::Dup{
					index,
				} => write!(f, "Dup {}", index),
				// Op::Pop => write!(f, "Pop"),
				Op::Add => write!(f, "Add"),
				Op::Sub => write!(f, "Sub"),
				Op::Mul => write!(f, "Mul"),
				Op::Div => write!(f, "Div"),
				Op::Rem => write!(f, "Rem"),
				// Op::And => write!(f, "And"),
				// Op::Or => write!(f, "Or"),
				Op::Not => write!(f, "Not"),
				Op::CmpEq => write!(f, "CmpEq"),
				Op::CmpNeq => write!(f, "CmpNeq"),
				Op::StrEq => write!(f, "StrEq"),
				Op::CmpG => write!(f, "CmpG"),
				Op::CmpGe => write!(f, "CmpGe"),
				Op::CmpL => write!(f, "CmpL"),
				Op::CmpLe => write!(f, "CmpLe"),
				// Op::PrintStr => write!(f, "PrintStr"),
				Op::LoadGlobal {
					index,
				} => write!(f, "LoadGlobal {}", index),
				Op::StoreGlobal {
					index,
				} => write!(f, "StoreGlobal {}", index),
				Op::Jmp {
					offset,
				} => write!(f, "Jmp L_{}", jumps_end.get(&((end_addr + *offset as isize) as usize)).unwrap()),
				Op::JmpIf {
					offset,
				} => write!(f, "JmpIf L_{}", jumps_end.get(&((end_addr + *offset as isize) as usize)).unwrap()),
				Op::JmpIfNot {
					offset,
				} => write!(f, "JmpIfNot L_{}", jumps_end.get(&((end_addr + *offset as isize) as usize)).unwrap()),
				Op::LoadLocal {
					index,
				} => write!(f, "LoadLocal {}", index),
				Op::StoreLocal {
					index,
				} => write!(f, "StoreLocal {}", index),
				Op::CallHelperFunction {
					args,
					locals_size,
					location,
				} => write!(f, "CallHelperFunction {} {} {}", args, locals_size, self.fn_labels.get(&location).unwrap()),
				Op::CallGameFunction {
					has_return,
					args,
					ptr,
				} => write!(f, "CallGameFunction {} {} 0x{:016x}", has_return, args, &unsafe{std::mem::transmute::<GameFnPtr, *const ()>(*ptr).addr()}),
			}?;
			// print labels: 
			write!(f, "\n")?;
		}
		Ok(())
	}
}

pub struct Stack {
	stack: Vec<GrugValue>,
	stack_frames: Vec<(/* rbp */ usize, /* ip */ usize)>,
	rbp: usize,
}

impl Stack {
	pub fn new() -> Self {
		Self {
			stack: Vec::with_capacity(1024),
			stack_frames: Vec::with_capacity(64),
			rbp: 0,
		}
	}

	pub fn reset(mut self) -> Self {
		self.stack.clear();
		self.stack_frames.clear();
		self.rbp = 0;
		self
	}

	unsafe fn run<GrugState: State>(&mut self, state: &GrugState, globals: &[Cell<GrugValue>], instructions: &Instructions, locals_size: u32, start_loc: usize) -> Option<GrugValue> {
		let mut stream = &instructions.stream[start_loc..];
		let start_time = Instant::now();
		self.stack.resize(self.rbp + locals_size as usize, GrugValue{void: ()});
		let mut i_count: usize = 1;
		loop {
			let (ins, next) = unsafe{stream.split_first().unwrap_unchecked()};
			stream = next;
			match *ins {
				Op::ReturnVoid         => {
					self.stack.truncate(self.rbp);
					if let Some((rbp, ip)) = self.stack_frames.pop() {
						self.rbp = rbp;
						stream = &mut &*instructions.stream.get(ip..)?;
					} else {
						return Some(GrugValue{void: ()});
					}
				}
				Op::ReturnValue        => {
					let ret_val = self.stack.pop()?;
					self.stack.truncate(self.rbp);
					if let Some((rbp, ip)) = self.stack_frames.pop() {
						self.stack.push(ret_val);
						self.rbp = rbp;
						stream = &mut &*instructions.stream.get(ip..)?;
					} else {
						return Some(ret_val);
					}
				}
				Op::LoadNumber{number} => self.stack.push(GrugValue{number}),
				Op::LoadStr{string}    => self.stack.push(GrugValue{string}),
				Op::LoadFalse          => self.stack.push(GrugValue{bool: 0}),
				Op::LoadTrue           => self.stack.push(GrugValue{bool: 1}),
				Op::Dup{index}         => {
					self.stack.push(*self.stack.get(self.stack.len() - 1 - index as usize)?)
				}
				// Op::Pop                => {self.stack.pop()?;}
				Op::Add                |
				Op::Sub                |
				Op::Mul                |
				Op::Div                |
				Op::Rem                => {
					let second = unsafe{self.stack.pop()?.number};
					let first = unsafe{self.stack.pop()?.number};
					let value = match ins {
						Op::Add => first + second,
						Op::Sub => first - second,
						Op::Mul => first * second,
						Op::Div => first / second,
						Op::Rem => first % second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{number: value});
				}
				// Op::And                |
				// Op::Or                 => {
				// 	let second = unsafe{self.stack.pop()?.bool};
				// 	let first = unsafe{self.stack.pop()?.bool};
				// 	let value = match ins {
				// 		Op::And => (first != 0) && (second != 0),
				// 		Op::Or  => (first != 0) || (second != 0),
				// 		_ => unreachable!(),
				// 	} as u8;
				// 	self.stack.push(GrugValue{bool: value});
				// }
				Op::Not                => {
					let value = unsafe{self.stack.pop()?.bool};
					self.stack.push(GrugValue{bool: (value == 0) as u8});
				}
				Op::CmpEq | Op::CmpNeq => {
					let second = self.stack.pop()?.as_bytes();
					let first = self.stack.pop()?.as_bytes();
					let value = match ins {
						Op::CmpEq  => first == second,
						Op::CmpNeq => first != second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				Op::StrEq              => {
					let second = unsafe{self.stack.pop()?.string};
					let first = unsafe{self.stack.pop()?.string};
					self.stack.push(GrugValue{bool: (first == second) as u8});
				}
				Op::CmpG  | Op::CmpGe  |
				Op::CmpL  | Op::CmpLe  => {
					let second = unsafe{self.stack.pop()?.number};
					let first = unsafe{self.stack.pop()?.number};
					let value = match ins {
						Op::CmpG  => first >  second,
						Op::CmpGe => first >= second,
						Op::CmpL  => first <  second,
						Op::CmpLe => first <= second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				// Op::PrintStr           => {
				// 	use std::ptr::NonNull;
				// 	let str = unsafe{
				// 		NTStrPtr::from_ptr(
				// 			NonNull::new_unchecked(
				// 				std::ptr::with_exposed_provenance_mut(
				// 					usize::from_ne_bytes(self.stack.pop()?.as_bytes())
				// 				)
				// 			)
				// 		).to_str()
				// 	};
				// 	println!("{}", str);
				// }
				Op::LoadGlobal{index}  => {
					self.stack.push(unsafe{globals.get_unchecked(index as usize)}.get());
				}
				Op::StoreGlobal{index} => {
					unsafe{globals.get_unchecked(index as usize)}.set(self.stack.pop()?);
				}
				Op::Jmp{offset}        => {
					stream = unsafe{
						std::slice::from_raw_parts(
							instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
							(stream.len() as isize - offset as isize) as usize
						)
					}
				}
				Op::JmpIf{offset}      => {
					if unsafe{self.stack.pop()?.bool} != 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
								(stream.len() as isize - offset as isize) as usize
							)
						}
					}
				}
				Op::JmpIfNot{offset}      => {
					if unsafe{self.stack.pop()?.bool} == 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
								(stream.len() as isize - offset as isize) as usize
							)
						}
					}
				}
				Op::LoadLocal{index}   => {
					 let value = *self.stack.get(self.rbp + index as usize)?;
					 self.stack.push(value);
				}
				Op::StoreLocal{index}  => {
					let value = self.stack.pop()?;
					*self.stack.get_mut(self.rbp + index as usize)? = value;
				}
				Op::CallHelperFunction {
					args,
					locals_size,
					location,
				} => {
					self.stack_frames.push((
						self.rbp,
						unsafe{stream.as_ptr().offset_from(instructions.stream.as_ptr()) as usize},
					));
					self.rbp = self.stack.len() - args as usize;
					self.stack.resize(self.rbp + locals_size as usize, GrugValue{void: ()});
					stream = instructions.stream.get(location as usize..)?;
				}
				Op::CallGameFunction {
					has_return,
					args,
					ptr,
				} => {
					let value = unsafe{(ptr.as_ptr())(state, self.stack.as_ptr().add(self.stack.len() - args as usize))};
					self.stack.truncate(self.stack.len() - args as usize);
					if has_return {
						self.stack.push(value);
					}
					if state.is_errorring() {
						return None
					}
				}
			}
			if i_count & 0xFFFFF == 0 {
				if start_time.elapsed() > Duration::from_millis(ON_FN_TIME_LIMIT) {
					state.set_runtime_error(RuntimeError::ExceededTimeLimit);
					return None;
				}
			}
			i_count += 1;
			if self.stack_frames.len() >= MAX_RECURSION_LIMIT {
				state.set_runtime_error(RuntimeError::StackOverflow);
				return None;
			}
		}
	}
}

export_backend!{BytecodeBackend::new()}

#[cfg(test)]
mod test {
	use super::*;
	use crate::state::GrugState;

	const MOD_API: &'static str /* ' */= r#"
	{
		"entities" : {
			"A" : {
				"description": "A",
				"on_functions": {
					"on_fib_naive" : {
						"description": "calculate the fibonacci number at index i using a recursive algorithm",
						"arguments": [
							{
								"name": "number",
								"type": "number"
							}
						]
					},
					"on_fib" : {
						"description": "calculate the fibonacci number at index i",
						"arguments": [
							{
								"name": "number",
								"type": "number"
							}
						]
					},
					"on_double" : {
						"description": "doubles the input number",
						"arguments": [
							{
								"name": "input",
								"type": "number"
							}
						]
					}
				}
			}
		},
		"game_functions": {
			"identity": {
				"description": "returns the same number as the input",
				"return_type": "number",
				"arguments": [
					{
						"name": "input",
						"type": "number"
					}
				]
			}
		}
	}
	"#;
	const GRUG_FILE_TEXT: &'static str = 
r#"global: number = 2

on_fib_naive(number: number) {
    identity(helper_fib_naive(number))
}

on_fib(number: number) {
    result: number = 0
    if number <= 0 {
        result = 0
    } else if number == 1 {
        result = 1
    } else {
        a: number = 1
        b: number = 1
        i: number = 2
        while i < number {
            temp: number = a + b
            a = b
            b = temp
            i = i + 1
        }
        result = b
    }
    identity(result)
}

on_double(input: number) {
    identity(2 * input)
}

helper_fib_naive(n: number) number {
    if n < 1 {
        return 0
    }
    if (n == 1 or n == 2) {
        return 1
    }
    return helper_fib_naive(n - 1) + helper_fib_naive(n - 2)
}
"#; // '
	static mut IDENTITY_ARG: f64 = 0.;
	extern "C" fn identity(_: &GrugState, arguments: *const GrugValue) -> GrugValue {
		unsafe{println!("{:?}", (*arguments).number)};
		unsafe{IDENTITY_ARG = (*arguments).number;}
		unsafe{*arguments}
	}

	// extern "C" fn void_argless(_: &GrugState) {
	// }
	// extern "C" fn void_arg(_: &GrugState, _arguments: *const GrugValue) {
	// }
	// extern "C" fn value_argless(_: &GrugState) -> GrugValue {
	// 	GrugValue{void: ()}
	// }
	// extern "C" fn value_arg(_: &GrugState, _arguments: *const GrugValue) -> GrugValue {
	// 	GrugValue{void: ()}
	// }

	fn get_state() -> GrugState {
		let mut state = GrugState::new_from_text(
			MOD_API,
			"doesn't matter",
			Default::default(),
			BytecodeBackend::new(),
		).unwrap();
		state.register_game_fn("identity", identity as for<'a> extern "C" fn (&'a _, _) -> _).unwrap();
		state.all_game_fns_registered().unwrap();
		state
	}

	#[test]
	fn vm_test_state() {
		let state = get_state();

		let on_fib_id      = state.get_on_fn_id("A", "on_fib").unwrap();
		let on_fn_naive_id = state.get_on_fn_id("A", "on_fib_naive").unwrap();
		let on_double_id   = state.get_on_fn_id("A", "on_double").unwrap();
		let _file = state.compile_grug_file_from_str("path/test-A.gru ", GRUG_FILE_TEXT).unwrap();
		let entity = state.create_entity(_file).unwrap();
		for i in 0..20 {
			let fib = {
				if i == 0 {
					0
				} else if i == 1 {
					1
				} else {
					let mut a = 1;
					let mut b = 1;
					let mut counter = 2;
					while counter < i {
						let temp = a + b;
						b = a;
						a = temp;
						counter += 1;
					}
					a
				}
			};
			assert!(state.call_on_function(&entity, on_fib_id, &[GrugValue{number: i as f64}]));
			unsafe{assert_eq!(*&raw const IDENTITY_ARG, fib as f64)}
			assert!(state.call_on_function(&entity, on_fn_naive_id, &[GrugValue{number: i as f64}]), "{i}");
			unsafe{assert_eq!(*&raw const IDENTITY_ARG, fib as f64)}
		}
		
		for i in 0..20 {
			assert!(state.call_on_function(&entity, on_double_id, &[GrugValue{number: i as f64}]));
			unsafe{assert_eq!(*&raw const IDENTITY_ARG, (i * 2) as f64)}
		}
	}

	#[test]
	fn vm_test_1() {
		let state = get_state();
		let mut stream = Instructions::new();
		stream.stream.push(Op::ReturnVoid);
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some()});
	}

	#[test]
	fn vm_test_2() {
		let state = get_state();
		let mut stream = Instructions::new();
		stream.stream.push(Op::LoadNumber{number:25.});
		stream.stream.push(Op::ReturnValue);
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == 25.)});
	}

	#[test]
	fn vm_test_3() {
		let state = get_state();
		let mut stream = Instructions::new();
		let mut vm = Stack::new();

		for i in 0..5 {
			for j in 1..5 {
				let i = i as f64;
				let j = j as f64;
				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::Add);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i + j)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::Sub);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i - j)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::Mul);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i * j)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::Div);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i / j)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::Rem);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i % j)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_5() {
		let state = get_state();
		let mut stream = Instructions::new();
		let mut vm = Stack::new();

		for i in 0..10 {
			for j in 0..10 {
				let i = i as f64;
				let j = j as f64;
				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpEq);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i == j) as u8)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpNeq);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i != j) as u8)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpG);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i > j) as u8)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpGe);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i >= j) as u8)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpL);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i < j) as u8)});
				stream.clear();

				stream.stream.push(Op::LoadNumber{number:i});
				stream.stream.push(Op::LoadNumber{number:j});
				stream.stream.push(Op::CmpLe);
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i <= j) as u8)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_7() {
		let state = get_state();
		let mut stream = Instructions::new();
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			for j in 0..10 {
				let i = i as f64;
				let j = j as f64;
				stream.stream.push(Op::LoadNumber {number: i});
				stream.stream.push(Op::StoreGlobal{index: 0});
				stream.stream.push(Op::LoadNumber {number: j});
				stream.stream.push(Op::StoreGlobal{index: 1});
				stream.stream.push(Op::LoadGlobal {index: 0});
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| x.number == i)});
				stream.clear();

				stream.stream.push(Op::LoadNumber {number: i});
				stream.stream.push(Op::StoreGlobal{index: 0});
				stream.stream.push(Op::LoadNumber {number: j});
				stream.stream.push(Op::StoreGlobal{index: 1});
				stream.stream.push(Op::LoadGlobal {index: 0});
				stream.stream.push(Op::LoadGlobal {index: 1});
				stream.stream.push(Op::Add);
				stream.stream.push(Op::StoreGlobal{index: 2});
				stream.stream.push(Op::LoadGlobal {index: 2});
				stream.stream.push(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| x.number == i + j)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_8() {
		let state = get_state();
		let mut stream = Instructions::new();
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::StoreGlobal{index: 0});
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::StoreGlobal{index: 1});
			stream.stream.push(Op::LoadNumber{number: i as f64});
			stream.stream.push(Op::StoreGlobal{index: 2});
			let cond_start = stream.get_loc();
			stream.stream.push(Op::LoadGlobal{index: 2});
			stream.stream.push(Op::LoadNumber{number: 0.});
			stream.stream.push(Op::CmpLe);
			let block_begin_jump = stream.get_loc();
			stream.stream.push(Op::JmpIf{offset: 0});
			stream.stream.push(Op::LoadGlobal{index: 1});
			stream.stream.push(Op::LoadGlobal{index: 0});
			stream.stream.push(Op::LoadGlobal{index: 0});
			stream.stream.push(Op::StoreGlobal{index: 1});
			stream.stream.push(Op::Add);
			stream.stream.push(Op::StoreGlobal{index: 0});
			stream.stream.push(Op::LoadGlobal{index: 2});
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::Sub);
			stream.stream.push(Op::StoreGlobal{index: 2});
			let block_end_loc = stream.get_loc();
			stream.stream.push(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc();
			stream.stream.push(Op::LoadGlobal{index: 0});
			stream.stream.push(Op::ReturnValue);

			stream.try_patch(Op::JmpIf{offset: Op::calc_offset(block_begin_jump, block_continue_loc)}, block_begin_jump).unwrap();
			stream.try_patch(Op::Jmp{offset: Op::calc_offset(block_end_loc, cond_start)}, block_end_loc).unwrap();
			let fib = {
				let mut a = 1.;
				let mut b = 1.;
				let mut i = i as f64;
				while i > 0. {
					let _b0 = b;
					let _a0 = a;
					let _a1 = a;
					b = _a1;
					let _0 = _b0 + _a0;
					a = _0;
					let _i = i;
					let _1 = 1.;
					i = _i - _1;
				}
				a
			};
			assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| {assert_eq!(x.number, fib); true})});
			// panic!("{:#?}", stream);
			stream.clear();
		}
	}

	#[test]
	fn vm_test_9() {
		let state = get_state();
		let mut stream = Instructions::new();
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.clear();
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::StoreLocal{index: 0});
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::StoreLocal{index: 1});
			stream.stream.push(Op::LoadNumber{number: i as f64});
			stream.stream.push(Op::StoreLocal{index: 2});
			let cond_start = stream.get_loc();
			stream.stream.push(Op::LoadLocal{index: 2});
			stream.stream.push(Op::LoadNumber{number: 0.});
			stream.stream.push(Op::CmpLe);
			let block_begin_jump = stream.get_loc();
			stream.stream.push(Op::JmpIf{offset: 0});
			stream.stream.push(Op::LoadLocal{index: 1});
			stream.stream.push(Op::LoadLocal{index: 0});
			stream.stream.push(Op::LoadLocal{index: 0});
			stream.stream.push(Op::StoreLocal{index: 1});
			stream.stream.push(Op::Add);
			stream.stream.push(Op::StoreLocal{index: 0});
			stream.stream.push(Op::LoadLocal{index: 2});
			stream.stream.push(Op::LoadNumber{number: 1.});
			stream.stream.push(Op::Sub);
			stream.stream.push(Op::StoreLocal{index: 2});
			let block_end_loc = stream.get_loc();
			stream.stream.push(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc();
			stream.stream.push(Op::LoadLocal{index: 0});
			stream.stream.push(Op::ReturnValue);

			stream.try_patch(Op::JmpIf{offset: Op::calc_offset(block_begin_jump, block_continue_loc)}, block_begin_jump).unwrap();
			stream.try_patch(Op::Jmp{offset: Op::calc_offset(block_end_loc, cond_start)}, block_end_loc).unwrap();
			let fib = {
				let mut a = 1.;
				let mut b = 1.;
				let mut i = i as f64;
				while i > 0. {
					let _b0 = b;
					let _a0 = a;
					let _a1 = a;
					b = _a1;
					let _0 = _b0 + _a0;
					a = _0;
					let _i = i;
					let _1 = 1.;
					i = _i - _1;
				}
				a
			};
			assert!(unsafe{vm.run(&state, &globals, &stream, 3, 0).is_some_and(|x| {assert_eq!(x.number, fib); true})});
		}
		println!("{}", stream);
	}

	#[test]
	fn vm_test_10() {
		let state = get_state();
		let mut stream = Instructions::new();
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.stream.push(Op::LoadNumber{number: i as f64});
			stream.stream.push(Op::LoadNumber{number: i as f64 - 1.});
			stream.stream.push(Op::Dup{index: 1});
			stream.stream.push(Op::Add);
			stream.stream.push(Op::Add);
			stream.stream.push(Op::ReturnValue);
			assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| {assert_eq!(x.number, 3. * i as f64 - 1.); true})});
			stream.clear();
		}
	}
}
