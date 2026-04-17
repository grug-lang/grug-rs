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
	helper_fn_patches: Vec<(/* location of call instruction */ usize, /* name */ &'a str)>,
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
		for (patch_loc, name) in compiler.helper_fn_patches {
			let data_loc = instructions.get_helper_fn_info(&name)
				.expect("helper function exists");
			instructions.try_patch(Op::CallHelperFunction{data_loc}, patch_loc).unwrap();
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
		for param in helper_function.parameters {
			self.insert_local_variable(param.name.to_str());
		}
		for statement in &*helper_function.body_statements {
			self.compile_statement(instructions, statement);
		}
		instructions.stream.push(Op::ReturnVoid);
		instructions.insert_helper_fn(helper_function.name.to_str(), helper_function.parameters.len() as u32, self.locals_size_max, begin_location);
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
		let param_count = on_function.parameters.len();
		for param in on_function.parameters {
			self.insert_local_variable(param.name.to_str());
		}
		for statement in &*on_function.body_statements {
			self.compile_statement(instructions, statement);
		}
		instructions.stream.push(Op::ReturnVoid);
		instructions.insert_on_fn(on_function.name.to_str(), index, begin_location, param_count, self.locals_size_max);
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
						let data_loc = instructions.insert_string(string);
						instructions.stream.push(Op::LoadStr{data_loc});
						instructions.strings.insert(value);
					}
					Some(value) => {
						let string = unsafe{value.as_ntstrptr().detach_lifetime()};
						let data_loc = instructions.insert_string(string);
						instructions.stream.push(Op::LoadStr{data_loc});
					}
				}
			}
			ExprData::Number (value, _) => {
				let data_loc = instructions.insert_number(*value);
				instructions.stream.push(Op::LoadNumber{data_loc});
			}
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
				op_span: _,
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
				op_span: _,
			} => {
				self.compile_expr(instructions, expr);
				match op {
					UnaryOperator::Not   => instructions.stream.push(Op::Not),
					// TODO: Add Negate instruction to do this in a single instruction
					UnaryOperator::Minus => {
						let data_loc = instructions.insert_number(-1.);
						instructions.stream.push(Op::LoadNumber{data_loc}); 
						instructions.stream.push(Op::Mul);
					}
				}
			}
			ExprData::Call {
				name,
				args,
				ptr: None,
				name_span: _,
			} => {
				let args_count = args.len();
				if args_count > u16::MAX as usize {
					panic!("cannot have more than {} arguments in a helper functions", u16::MAX);
				}
				for argument in &**args {
					self.compile_expr(instructions, argument);
				}

				let data_loc = instructions.get_helper_fn_info(name.to_str()).unwrap_or_else(|| {
					self.helper_fn_patches.push((instructions.get_loc(), name.to_str()));
					0
				});
				instructions.stream.push(Op::CallHelperFunction{data_loc});
			},
			ExprData::Call {
				name: _,
				args,
				ptr: Some(ptr),
				name_span: _,
			} => {
				let args_count = args.len();
				for argument in &**args {
					self.compile_expr(instructions, argument);
				}
				
				let has_return = *expr.result_type.unwrap() != GrugType::Void;
				let data_loc = instructions.insert_game_fn_data(args_count as u32, *ptr);
				instructions.stream.push(Op::CallGameFunction {
					has_return,
					data_loc
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
	#[inline]
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
	#[inline]
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
	#[inline]
	fn clear_entities(&mut self) {
		for file in self.files.get_mut().iter_mut() {
			file.data.clear();
			file.entities.borrow_mut().clear();
		}
	}
	#[inline]
	fn destroy_entity_data(&self, entity: &GrugEntity) {
		let files = self.files.borrow();
		let file = files.get(entity.file_id.0 as usize)
			.expect("file already compiled");
		unsafe{file.data.delete(ErasedPtr::from_ptr(entity.members.get()))};
		file.entities.borrow_mut().extract_if(.., |en| std::ptr::eq(en.as_ptr().cast_const(), entity)).for_each(|_| {});
	}
	#[inline]
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
	#[inline]
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
enum Op {
	ReturnVoid = 0x00,
	ReturnValue,
	LoadNumber {
		data_loc: u32,
	},
	LoadStr {
		data_loc: u32,
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
		data_loc: u32,
	},
	CallGameFunction {
		has_return: bool,
		data_loc: u32,
	},
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

pub union ConstantData {
	number: f64,
	string: NTStrPtr<'static>,
	helper_fn_data: (/* args: */ u32, /* locals_size: */ u32, /* location: */ usize),
	game_fn_data: (/* args: */ u32, /* ptr: */ GameFnPtr),
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
	constants: Vec<ConstantData>,
	// TODO: this Arc<str> should prolly be replaced with an &'static into an arena
	helper_fn_locations: HashMap<Arc<str>, /* constant location */ u32>,
	game_fn_locations: HashMap</* GameFnPtr as usize */ usize, /* constant location */ u32>,
	fn_labels: HashMap<usize, Arc<str>>,
	strings: HashSet<Arc<NTStr>>,
}

impl Instructions {
	pub fn new() -> Self {
		Self {
			stream: Vec::new(),
			on_fn_locations: Vec::new(),
			constants: Vec::new(),
			helper_fn_locations: HashMap::new(),
			game_fn_locations: HashMap::new(),
			fn_labels: HashMap::new(),
			strings: HashSet::new(),
			// jumps_count: 0,
			// jumps_start: HashMap::new(),
			// jumps_end: HashMap::new(),
		}
	}

	pub fn insert_number(&mut self, number: f64) -> u32 {
		let ret_val = self.constants.len();
		self.constants.push(ConstantData{number});
		assert!(ret_val < u32::MAX as usize, "internal error: script has more than {} constants", u32::MAX);
		ret_val as u32
	}

	pub fn insert_string(&mut self, string: NTStrPtr<'static>) -> u32 {
		let ret_val = self.constants.len();
		self.constants.push(ConstantData{string});
		assert!(ret_val < u32::MAX as usize, "internal error: script has more than {} constants", u32::MAX);
		ret_val as u32
	}

	#[allow(unused)]
	pub fn clear(&mut self) { 
		self.stream.clear();
		self.on_fn_locations.clear();
		self.constants.clear();
		self.helper_fn_locations.clear();
		self.game_fn_locations.clear();
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

	pub fn get_helper_fn_info(&mut self, name: &str) -> Option<u32> {
		self.helper_fn_locations.get(name).copied()
	}

	pub fn insert_helper_fn(&mut self, name: &str, args: u32, locals_size: u32, location: usize) {
		let name = Arc::from(name);
		let const_location = self.constants.len();
		assert!(const_location < u32::MAX as usize);
		self.constants.push(ConstantData{helper_fn_data: (args, locals_size, location)});
		self.helper_fn_locations.insert(Arc::clone(&name), const_location as u32);
		self.fn_labels.insert(location, name);
	}

	pub fn insert_game_fn_data(&mut self, args: u32, ptr: GameFnPtr) -> u32 {
		*self.game_fn_locations.entry(ptr.as_usize()).or_insert_with(|| {
			let ret_val = self.constants.len();
			self.constants.push(ConstantData{game_fn_data: (args, ptr)});
			assert!(ret_val < u32::MAX as usize, "internal error: script has more than {} constants", u32::MAX);
			ret_val as u32
		})
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
			(Op::CallHelperFunction{data_loc: patch_data_loc}, 
			 Op::CallHelperFunction{data_loc}) => {
				*patch_data_loc = data_loc;
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
					data_loc,
				} => write!(f, "LoadStr {:?}", unsafe{self.constants.get_unchecked(*data_loc as usize).string.to_str()}),
				Op::LoadNumber {
					data_loc,
				} => write!(f, "Load {:?}", unsafe{self.constants.get_unchecked(*data_loc as usize).number}),
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
					data_loc,
				} => {
					let (args, locals_size, location) = unsafe{self.constants[*data_loc as usize].helper_fn_data};
					write!(f, "CallHelperFunction {} {} {}", args, locals_size, self.fn_labels.get(&location).unwrap())
				}
				Op::CallGameFunction {
					has_return,
					data_loc,
				} => {
					let (args, ptr) = unsafe{self.constants[*data_loc as usize].game_fn_data};
					write!(f, "CallGameFunction {} {} 0x{:016x}", has_return, args, &unsafe{std::mem::transmute::<GameFnPtr, *const ()>(ptr).addr()})
				}
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
				Op::ReturnVoid           => {
					self.stack.truncate(self.rbp);
					if let Some((rbp, ip)) = self.stack_frames.pop() {
						self.rbp = rbp;
						stream = unsafe{&mut &*instructions.stream.get(ip..).unwrap_unchecked()};
					} else {
						return Some(GrugValue{void: ()});
					}
				}
				Op::ReturnValue          => {
					let ret_val = unsafe{self.stack.pop().unwrap_unchecked()};
					self.stack.truncate(self.rbp);
					if let Some((rbp, ip)) = self.stack_frames.pop() {
						self.stack.push(ret_val);
						self.rbp = rbp;
						stream = unsafe{&mut &*instructions.stream.get(ip..).unwrap_unchecked()};
					} else {
						return Some(ret_val);
					}
				}
				Op::LoadNumber{data_loc} => unsafe{self.stack.push(GrugValue{number: instructions.constants.get_unchecked(data_loc as usize).number})},
				Op::LoadStr{data_loc}    => unsafe{self.stack.push(GrugValue{string: instructions.constants.get_unchecked(data_loc as usize).string})},
				Op::LoadFalse            => self.stack.push(GrugValue{bool: 0}),
				Op::LoadTrue             => self.stack.push(GrugValue{bool: 1}),
				Op::Dup{index}           => {
					unsafe{self.stack.push(*self.stack.get(self.stack.len() - 1 - index as usize).unwrap_unchecked())}
				}
				// Op::Pop                => {unsafe{self.stack.pop().unwrap_unchecked()};}
				Op::Add                  |
				Op::Sub                  |
				Op::Mul                  |
				Op::Div                  |
				Op::Rem                  => {
					let second = unsafe{self.stack.pop().unwrap_unchecked().number};
					let first = unsafe{self.stack.pop().unwrap_unchecked().number};
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
				// Op::And                  |
				// Op::Or                   => {
				// 	let second = unsafe{self.stack.pop().unwrap_unchecked()}.bool};
				// 	let first = unsafe{self.stack.pop().unwrap_unchecked()}.bool};
				// 	let value = match ins {
				// 		Op::And => (first != 0) && (second != 0),
				// 		Op::Or  => (first != 0) || (second != 0),
				// 		_ => unreachable!(),
				// 	} as u8;
				// 	self.stack.push(GrugValue{bool: value});
				// }
				Op::Not                  => {
					let value = unsafe{self.stack.pop().unwrap_unchecked().bool};
					self.stack.push(GrugValue{bool: (value == 0) as u8});
				}
				Op::CmpEq | Op::CmpNeq   => {
					let second = unsafe{self.stack.pop().unwrap_unchecked()}.as_bytes();
					let first = unsafe{self.stack.pop().unwrap_unchecked()}.as_bytes();
					let value = match ins {
						Op::CmpEq  => first == second,
						Op::CmpNeq => first != second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				Op::StrEq                => {
					let second = unsafe{self.stack.pop().unwrap_unchecked().string};
					let first = unsafe{self.stack.pop().unwrap_unchecked().string};
					self.stack.push(GrugValue{bool: (first == second) as u8});
				}
				Op::CmpG  | Op::CmpGe    |
				Op::CmpL  | Op::CmpLe    => {
					let second = unsafe{self.stack.pop().unwrap_unchecked().number};
					let first = unsafe{self.stack.pop().unwrap_unchecked().number};
					let value = match ins {
						Op::CmpG  => first >  second,
						Op::CmpGe => first >= second,
						Op::CmpL  => first <  second,
						Op::CmpLe => first <= second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				// Op::PrintStr             => {
				// 	use std::ptr::NonNull;
				// 	let str = unsafe{
				// 		NTStrPtr::from_ptr(
				// 			NonNull::new_unchecked(
				// 				std::ptr::with_exposed_provenance_mut(
				// 					unsafe{usize::from_ne_bytes(self.stack.pop().unwrap_unchecked()}.as_bytes())
				// 				)
				// 			)
				// 		).to_str()
				// 	};
				// 	println!("{}", str);
				// }
				Op::LoadGlobal{index}    => {
					self.stack.push(unsafe{globals.get_unchecked(index as usize)}.get());
				}
				Op::StoreGlobal{index}   => {
					unsafe{globals.get_unchecked(index as usize).set(self.stack.pop().unwrap_unchecked())};
				}
				Op::Jmp{offset}          => {
					stream = unsafe{
						std::slice::from_raw_parts(
							instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
							(stream.len() as isize - offset as isize) as usize
						)
					}
				}
				Op::JmpIf{offset}        => {
					if unsafe{self.stack.pop().unwrap_unchecked().bool} != 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
								(stream.len() as isize - offset as isize) as usize
							)
						}
					}
				}
				Op::JmpIfNot{offset}     => {
					if unsafe{self.stack.pop().unwrap_unchecked().bool} == 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset as isize),
								(stream.len() as isize - offset as isize) as usize
							)
						}
					}
				}
				Op::LoadLocal{index}     => {
					 let value = unsafe{*self.stack.get(self.rbp + index as usize).unwrap_unchecked()};
					 self.stack.push(value);
				}
				Op::StoreLocal{index}    => {
					let value = unsafe{self.stack.pop().unwrap_unchecked()};
					*unsafe{self.stack.get_mut(self.rbp + index as usize).unwrap_unchecked()} = value;
				}
				Op::CallHelperFunction {
					data_loc,
				} => {
					let (args, locals_size, location) = unsafe{instructions.constants[data_loc as usize].helper_fn_data};
					self.stack_frames.push((
						self.rbp,
						unsafe{stream.as_ptr().offset_from(instructions.stream.as_ptr()) as usize},
					));
					self.rbp = self.stack.len() - args as usize;
					self.stack.resize(self.rbp + locals_size as usize, GrugValue{void: ()});
					stream = unsafe{instructions.stream.get(location as usize..).unwrap_unchecked()};
				}
				Op::CallGameFunction {
					has_return,
					data_loc,
				} => {
					let (args, ptr) = unsafe{instructions.constants[data_loc as usize].game_fn_data};
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
