use crate::types::{
	GrugValue, GrugScriptId, GrugEntity, GameFnPtr,
};
use crate::ast::{
	Expr, ExprData, OnFunction, Statement,
	BinaryOperator, GrugType, HelperFunction, UnaryOperator
};
use crate::ntstring::{NTStrPtr, NTStr};
use crate::cachemap::CacheMap;
use crate::state::GrugState;
use crate::xar::{ErasedXar, ErasedPtr};
use crate::error::{RuntimeError, ON_FN_TIME_LIMIT, MAX_RECURSION_LIMIT};
use super::{Backend, GrugAst};

use std::collections::{HashMap, HashSet};
use std::ptr::NonNull;
use std::cell::{Cell, RefCell};
use std::alloc::Layout;
use std::sync::Arc;
use std::time::{Duration, Instant};

struct Compiler<'a> {
	globals: HashMap<&'a str, usize>,
	locals: Vec<HashMap<&'a str, usize>>,
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
		instructions.push_op(Op::StoreGlobal{index: me_location});
		for global in ast.members.into_iter() {
			compiler.compile_expr(&mut instructions, &global.assignment_expr);
			let i = compiler.insert_global_variable(global.name.to_str());
			instructions.push_op(Op::StoreGlobal{index: i});
		}
		instructions.push_op(Op::ReturnVoid);

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
			entities: RefCell::new(Vec::new()),
			globals_size,
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
		instructions.push_op(Op::ReturnVoid);
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
		instructions.push_op(Op::ReturnVoid);
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
					instructions.push_op(Op::StoreLocal{index: loc});
				} else {
					if let Some(loc) = self.get_local_location(name) {
						instructions.push_op(Op::StoreLocal{index: loc});
					} else if let Some(loc) = self.get_global_location(name) {
						instructions.push_op(Op::StoreGlobal{index: loc});
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
				self.compile_expr(instructions, condition);

				instructions.push_op(Op::Not);
				let condition_patch_loc = instructions.get_loc();
				instructions.push_op(Op::JmpIf{offset: 0});

				self.push_scope();
				for statement in &**if_block {
					self.compile_statement(instructions, statement);
				}
				self.pop_scope();

				if !else_block.is_empty() {
					let end_patch_loc = instructions.get_loc();
					instructions.push_op(Op::Jmp{offset: 0});
					let cur_loc = instructions.get_loc();
					// jump from the false condtion to the start of the else block
					instructions.try_patch(Op::JmpIf{offset: Op::calc_offset(condition_patch_loc, cur_loc)}, condition_patch_loc)
						.expect("Could not patch jump because offset is too large");
					if *is_chained {
						for statement in &**else_block {
							self.compile_statement(instructions, statement);
						}
					} else {
						self.push_scope();
						for statement in &**else_block {
							self.compile_statement(instructions, statement);
						}
						self.pop_scope();
					}
					let end_loc = instructions.get_loc();
					// jump from the end of the previous block to the end of the else block
					instructions.try_patch(Op::Jmp{offset: Op::calc_offset(end_patch_loc, end_loc)}, end_patch_loc)
						.expect("Could not patch jump because offset is too large");
				} else {
					let cur_loc = instructions.get_loc();
					// jump from the false condtion to the end of the if statement
					instructions.try_patch(Op::JmpIf{offset: Op::calc_offset(condition_patch_loc, cur_loc)}, condition_patch_loc)
						.expect("Could not patch jump because offset is too large");
				}
			}
			Statement::While {
				condition,
				block,
			} => {
				let continue_loc = instructions.get_loc();
				self.compile_expr(instructions, condition);
				instructions.push_op(Op::Not);
				let break_patch_loc = instructions.get_loc();
				instructions.push_op(Op::JmpIf{offset: 0});

				self.while_loop_patches.push((continue_loc, Vec::new()));
				
				for statement in &**block {
					self.compile_statement(instructions, statement);
				}
				let end_loc = instructions.get_loc();
				instructions.push_op(Op::Jmp{offset: Op::calc_offset(end_loc, continue_loc)});
				let break_loc = instructions.get_loc();
				instructions.try_patch(Op::JmpIf{offset: Op::calc_offset(break_patch_loc, break_loc)}, break_patch_loc).unwrap();

				for break_patch_loc in self.while_loop_patches.pop().unwrap().1 {
					instructions.try_patch(Op::Jmp{offset: Op::calc_offset(break_patch_loc, break_loc)}, break_patch_loc).unwrap();
				}
			}
			Statement::Call(expr) => self.compile_expr(instructions, expr),
			Statement::Return{expr} => {
				if let Some(expr) = expr {
					self.compile_expr(instructions, expr);
					instructions.push_op(Op::ReturnValue);
				} else {
					instructions.push_op(Op::ReturnVoid);
				}
			}
			Statement::Break => {
				self.while_loop_patches.last_mut().unwrap().1.push(instructions.get_loc());
				instructions.push_op(Op::Jmp{offset: 0});
			}
			Statement::Continue => {
				let continue_loc = self.while_loop_patches.last().unwrap().0;
				instructions.push_op(Op::Jmp{offset: Op::calc_offset(instructions.get_loc(), continue_loc)});
			}
			Statement::EmptyLine => {},
			Statement::Comment{..}        => {},
		}
	}

	fn compile_expr(&mut self, instructions: &mut Instructions, expr: &'a Expr) {
		match &expr.data {
			ExprData::True  => instructions.push_op(Op::LoadTrue ),
			ExprData::False => instructions.push_op(Op::LoadFalse),
			ExprData::String(value)   |
			ExprData::Resource(value) |
			ExprData::Entity(value)   => {
				match instructions.strings.get(value.to_ntstr()) {
					None => {
						instructions.strings.insert(NTStr::arc_from_str(value.to_ntstr()));
						// SAFETY: This returned instruction stream is only valid as long as this list of strings is available
						let string = unsafe{value.detach_lifetime()};
						instructions.push_op(Op::LoadStr{string})
					}
					Some(value) => {
						let string = unsafe{value.as_ntstrptr().detach_lifetime()};
						instructions.push_op(Op::LoadStr{string})
					}
				}
			}
			ExprData::Number (value, _) => instructions.push_op(Op::LoadNumber{number: *value}),
			ExprData::Identifier(name) => {
				let name = name.to_str();
				if let Some(loc) = self.get_local_location(name) {
					instructions.push_op(Op::LoadLocal{index: loc});
				} else if let Some(loc) = self.get_global_location(name) {
					instructions.push_op(Op::LoadGlobal{index: loc});
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
					BinaryOperator::Greater       => {self.compile_expr(instructions, right); instructions.push_op(Op::CmpG);}
					BinaryOperator::GreaterEquals => {self.compile_expr(instructions, right); instructions.push_op(Op::CmpGe);}
					BinaryOperator::Less          => {self.compile_expr(instructions, right); instructions.push_op(Op::CmpL);}
					BinaryOperator::LessEquals    => {self.compile_expr(instructions, right); instructions.push_op(Op::CmpLe);}
					BinaryOperator::Plus          => {self.compile_expr(instructions, right); instructions.push_op(Op::Add);}
					BinaryOperator::Minus         => {self.compile_expr(instructions, right); instructions.push_op(Op::Sub);}
					BinaryOperator::Multiply      => {self.compile_expr(instructions, right); instructions.push_op(Op::Mul);}
					BinaryOperator::Division      => {self.compile_expr(instructions, right); instructions.push_op(Op::Div);}
					BinaryOperator::Remainder     => {self.compile_expr(instructions, right); instructions.push_op(Op::Rem);}
					BinaryOperator::DoubleEquals  => {
						match right.result_type.unwrap() {
							GrugType::String => {
								self.compile_expr(instructions, right);
								instructions.push_op(Op::StrEq);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(instructions, right);
								instructions.push_op(Op::CmpEq);
							}
						}
					}
					BinaryOperator::NotEquals     => {
						match right.result_type.unwrap() {
							GrugType::String => {
								self.compile_expr(instructions, right);
								instructions.push_op(Op::StrEq);
								instructions.push_op(Op::Not);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(instructions, right);
								instructions.push_op(Op::CmpNeq);
							}
						}
					}
					BinaryOperator::Or            => {
						instructions.push_op(Op::Dup{index: 0});
						let first_patch_loc = instructions.get_loc();
						instructions.push_op(Op::JmpIf{offset: 0});
						self.compile_expr(instructions, right);
						instructions.try_patch(
							Op::JmpIf{
								offset: Op::calc_offset(first_patch_loc, instructions.get_loc()),
							},
							first_patch_loc,
						).unwrap();
					}
					BinaryOperator::And           => {
						instructions.push_op(Op::Dup{index: 0});
						instructions.push_op(Op::Not);
						let first_patch_loc = instructions.get_loc();
						instructions.push_op(Op::JmpIf{offset: 0});
						self.compile_expr(instructions, right);
						instructions.try_patch(
							Op::JmpIf{
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
					UnaryOperator::Not   => instructions.push_op(Op::Not),
					UnaryOperator::Minus => {instructions.push_op(Op::LoadNumber{number: -1.0}); instructions.push_op(Op::Mul);}
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
				instructions.push_op(Op::CallHelperFunction{args: args_count as u16, locals_size, location});
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
				instructions.push_op(Op::CallGameFunction {
					has_return,
					args: args_count as u32,
					ptr: *ptr,
				});
			}
			ExprData::Parenthesized(expr) => self.compile_expr(instructions, expr),
		}
	}

	fn insert_global_variable(&mut self, name: &'a str) -> usize {
		let check = self.globals.insert(name, self.globals.len());
		debug_assert!(check.is_none());
		self.globals.len() - 1
	}

	fn insert_local_variable(&mut self, name: &'a str) -> usize {
		let check = self.locals.last_mut().unwrap().insert(name, self.current_scope_size);
		debug_assert!(check.is_none());
		self.current_scope_size += 1;
		if self.current_scope_size > u32::MAX as usize {
			panic!("Cannot have more than {} local variables", u32::MAX);
		}
		self.locals_size_max = std::cmp::max(self.locals_size_max, self.current_scope_size as u32);
		self.current_scope_size - 1
	}

	fn pop_scope(&mut self) {
		self.locals.pop().unwrap();
		self.current_scope_size = self.locals_sizes.pop().unwrap();
	}

	fn push_scope(&mut self) {
		self.locals.push(HashMap::new());
		self.locals_sizes.push(self.current_scope_size);
	}

	fn get_local_location(&self, name: &str) -> Option<usize> {
		self.locals.iter().rev().find_map(|x| x.get(name)).copied()
	}

	fn get_global_location(&self, name: &str) -> Option<usize> {
		self.globals.get(name).copied()
	}
}

#[derive(Debug)]
struct CompiledFile {
	instructions: Instructions,
	entities: RefCell<Vec<NonNull<GrugEntity>>>,
	globals_size: usize,
	data: ErasedXar,
}

pub struct BytecodeBackend {
	files: CacheMap<GrugScriptId, CompiledFile>,
	stacks: RefCell<Vec<Stack>>,
}

impl BytecodeBackend {
	pub fn new() -> Self {
		Self {
			files: CacheMap::new(),
			stacks: RefCell::new(Vec::new()),
		}
	}
}

unsafe impl Backend for BytecodeBackend {
	fn insert_file(&self, id: GrugScriptId, file: GrugAst) {
		let compiled_file = Compiler::compile(file);
		match self.files.try_insert(id, compiled_file) {
			Ok(()) => (),
			Err((_id, _compiled_file)) => {
				unimplemented!();
			}
		}
	}
	fn init_entity(&self, state: &GrugState, entity: &GrugEntity) -> bool {
		let file = self.files.get(&entity.file_id)
			.expect("file already compiled");
		let globals = unsafe{&*file.data.get_slot().write_slice(file.globals_size, Cell::new(GrugValue{void: ()}))};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		stack.stack.push(GrugValue{id: entity.id});
		let ret_val = unsafe{stack.run(state, globals, &file.instructions, 1, 0)}.is_some();
		entity.members.set(NonNull::from_ref(globals).cast::<()>());

		stack = stack.reset();
		self.stacks.borrow_mut().push(stack);
		ret_val
	}
	fn clear_entities(&mut self) {
		for (_, file) in self.files.iter_mut() {
			file.entities.get_mut().clear();
			file.data.clear();
		}
	}
	fn destroy_entity_data(&self, entity: &GrugEntity) -> bool {
		let file = self.files.get(&entity.file_id)
			.expect("file already compiled");
		if file.entities.borrow_mut().extract_if(.., |x: &mut NonNull<GrugEntity>| !std::ptr::eq(x.as_ptr().cast_const(), entity))
			.next().is_some() {
			unsafe{file.data.delete(ErasedPtr::from_ptr(entity.members.get()))};
			true
		} else {
			false
		}
	}
	unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		let file = self.files.get(&entity.file_id)
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
	fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		let file = self.files.get(&entity.file_id)
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
		index: usize,
	},
	Pop,
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	And,
	Or,
	Not,
	CmpEq,
	CmpNeq,
	StrEq,
	CmpG,
	CmpGe,
	CmpL,
	CmpLe,
	PrintStr,
	LoadGlobal {
		index: usize,
	},
	StoreGlobal {
		index: usize,
	},
	Jmp {
		offset: isize,
	},
	JmpIfNot {
		offset: isize,
	},
	JmpIf {
		offset: isize,
	},
	LoadLocal {
		index: usize,
	},
	StoreLocal {
		index: usize,
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


#[allow(unused)]
const DISPATCH_TABLE: [DispatchFn; Op::num_opcodes() as usize] = const {
	let mut functions = [(|_, _, _, _, _| None) as DispatchFn; Op::num_opcodes() as usize];
	let mut idx = 0; 
	while idx < functions.len() {
		functions[idx] = match idx {
			0x00 => (|stack: &mut Stack, state, globals, instructions: &Instructions, _| {
				stack.stack.truncate(stack.rbp);
				if let Some((rbp, ip)) = stack.stack_frames.pop() {
					stack.rbp = rbp;
					let next_ptr = unsafe{instructions.stream.as_ptr().add(ip)};
					unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
				} else {
					return Some(GrugValue{void: ()});
				}
			}) as DispatchFn,
			0x01 => (|stack: &mut Stack, state, globals, instructions: &Instructions, _| {
				let ret_val = stack.stack.pop()?;
				stack.stack.truncate(stack.rbp);
				if let Some((rbp, ip)) = stack.stack_frames.pop() {
					stack.stack.push(ret_val);
					stack.rbp = rbp;
					let next_ptr = unsafe{instructions.stream.as_ptr().add(ip)};
					unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
				} else {
					return Some(ret_val);
				}
			}) as DispatchFn,
			0x02 => (|stack: &mut Stack, state, globals, instructions: &Instructions, ptr| {
				match unsafe{*ptr} {
					Op::LoadNumber{number} => stack.stack.push(GrugValue{number}),
					_ => unsafe{std::hint::unreachable_unchecked()},
				}
				let next_ptr = unsafe{ptr.add(1)};
				unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
			}) as DispatchFn,
			0x03 => (|stack: &mut Stack, state, globals, instructions: &Instructions, ptr| {
				match unsafe{*ptr} {
					Op::LoadStr{string}    => stack.stack.push(GrugValue{string}),
					_ => unsafe{std::hint::unreachable_unchecked()},
				}
				let next_ptr = unsafe{ptr.add(1)};
				unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
			}) as DispatchFn,
			0x04 => (|stack: &mut Stack, state, globals, instructions: &Instructions, ptr| {
				match unsafe{*ptr} {
					Op::LoadFalse          => stack.stack.push(GrugValue{bool: 0}),
					_ => unsafe{std::hint::unreachable_unchecked()},
				}
				let next_ptr = unsafe{ptr.add(1)};
				unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
			}) as DispatchFn,
			0x05 => (|stack: &mut Stack, state, globals, instructions: &Instructions, ptr| {
				match unsafe{*ptr} {
					Op::LoadTrue           => stack.stack.push(GrugValue{bool: 1}),
					_ => unsafe{std::hint::unreachable_unchecked()},
				}
				let next_ptr = unsafe{ptr.add(1)};
				unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
			}) as DispatchFn,
			0x06 => (|stack: &mut Stack, state, globals, instructions: &Instructions, ptr| {
				match unsafe{*ptr} {
					Op::Dup{index}         => {
						stack.stack.push(*stack.stack.get(stack.stack.len() - 1 - index)?)
					}
					_ => unsafe{std::hint::unreachable_unchecked()},
				}
				let next_ptr = unsafe{ptr.add(1)};
				unsafe{Op::get_dispatch_fn(next_ptr)(stack, state, globals, instructions, next_ptr)}
			}) as DispatchFn,
			_ => {idx += 1; continue},
		};
		idx += 1;
	}
	functions
};

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
			(Self::Pop      , Self::Pop      )  => true,
			(Self::Add      , Self::Add      )  => true,
			(Self::Sub      , Self::Sub      )  => true,
			(Self::Mul      , Self::Mul      )  => true,
			(Self::Div      , Self::Div      )  => true,
			(Self::Rem      , Self::Rem      )  => true,
			(Self::And      , Self::And      )  => true,
			(Self::Or       , Self::Or       )  => true,
			(Self::Not      , Self::Not      )  => true,
			(Self::CmpEq    , Self::CmpEq    )  => true,
			(Self::CmpNeq   , Self::CmpNeq   )  => true,
			(Self::StrEq    , Self::StrEq    )  => true,
			(Self::CmpG     , Self::CmpG     )  => true,
			(Self::CmpGe    , Self::CmpGe    )  => true,
			(Self::CmpL     , Self::CmpL     )  => true,
			(Self::CmpLe    , Self::CmpLe    )  => true,
			(Self::PrintStr , Self::PrintStr )  => true,
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

type DispatchFn = unsafe fn(&mut Stack, state: &GrugState, globals: &[Cell<GrugValue>], instructions: &Instructions, *const Op) -> Option<GrugValue>;

impl Op {
	fn calc_offset(from: usize, to: usize) -> isize {
		let offset_from_start = to as isize - from as isize ;
		// opcode is 1 byte, offset fits in two bytes
		offset_from_start - 1
	}

	const fn num_opcodes() -> u8 {
		unsafe{*(&Self::CallGameFunction{
			has_return: false,
			args: 0,
			ptr: GameFnPtr{value: std::mem::transmute(std::ptr::null::<()>())},
		} as *const _ as *const u8)}
	}

	fn get_dispatch_fn(ptr: *const Self) -> DispatchFn {
		DISPATCH_TABLE[unsafe{*(ptr as *const u8)} as usize]
	}
}

pub struct Instructions{
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
	jumps_count: usize,
	jumps_end: HashMap</* to */ usize, (/* from */ Vec<usize>, /* label */ usize)>,
	jumps_start: HashMap</* from */ usize, /* to */ usize>,
}

impl Instructions {
	pub fn new() -> Self {
		Self {
			stream: Vec::new(),
			on_fn_locations: Vec::new(),
			helper_fn_locations: HashMap::new(),
			fn_labels: HashMap::new(),
			strings: HashSet::new(),
			jumps_count: 0,
			jumps_start: HashMap::new(),
			jumps_end: HashMap::new(),
		}
	}

	pub fn clear(&mut self) { 
		self.stream.clear();
		self.on_fn_locations.clear();
		self.helper_fn_locations.clear();
		self.fn_labels.clear();
		self.strings.clear();
		self.jumps_count = 0;
		self.jumps_end.clear();
		self.jumps_start.clear();
	}

	pub fn push_op(&mut self, op: Op) {
		self.stream.push(op);
		match op {
			Op::Jmp{offset} | Op::JmpIf{offset} | Op::JmpIfNot{offset} => {
				self.insert_jmp(self.stream.len(), offset);
			}
			_ => (),
		}
	}

	pub fn insert_jmp(&mut self, from: usize, offset: isize) {
		let end = (from as isize + offset) as usize;
		self.jumps_start.insert(from, end);
		self.jumps_end.entry(end).or_insert_with(|| {
			self.jumps_count += 1;
			(Vec::new(), self.jumps_count - 1)
		}).0.push(from);
		self.assert_jumps_consistency();
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

	pub fn assert_jumps_consistency(&self) {
		for (end, starts) in &self.jumps_end {
			for start in &starts.0 {
				let left = self.jumps_start.get(start).unwrap();
				assert_eq!(left, end);
			}
		}
	}

	/// SAFETY:
	/// `location` must be at the end of an instruction and the beginning of
	/// the next one
	/// Invalidates all instructions upto `location`.
	/// Any jumps that ends up past `location` needs to be redone
	///
	pub unsafe fn rewind_to(&mut self, location: usize) {
		self.stream.truncate(location);
		for (start, end) in self.jumps_start.iter() {
			if *start > location || *end > location {
				self.jumps_end.get_mut(end).unwrap().0.retain(|x| x != start);
			}
		}
	}

	pub fn get_loc(&self) -> usize {
		self.stream.len()
	}

	pub fn try_patch(&mut self, op: Op, location: usize) -> Option<()> {
		let (old_start, new_offset)  = match (self.stream.get_mut(location)?, op) {
			(Op::Jmp{offset: patch}, Op::Jmp{offset}) | 
			(Op::JmpIf{offset: patch}, Op::JmpIf{offset}) |
			(Op::JmpIfNot{offset: patch}, Op::JmpIfNot{offset}) => {
				*patch = offset;
				(location + 1, offset)
			}
			(Op::CallHelperFunction{args: patch_args, locals_size: patch_locals_size, location: patch_location}, 
			 Op::CallHelperFunction{args, locals_size, location}) => {
				*patch_args = args;
				*patch_locals_size = locals_size;
				*patch_location = location;
				return Some(());
			}
			_ => return None,
		};
		
		let old_end = self.jumps_start.remove(&old_start).unwrap();
		self.jumps_end.get_mut(&old_end).unwrap().0.extract_if(.., |start| *start == old_start).count();

		self.insert_jmp(old_start, new_offset);
		self.assert_jumps_consistency();
		Some(())
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
		let mut addr = unsafe{stream.as_ptr().offset_from(self.stream.as_ptr())};
		if let Some(name) = self.fn_labels.get(&(addr as usize)) {
			writeln!(f, "{}:", name)?;
		}
		if let Some((froms, label)) = self.jumps_end.get(&(addr as usize)) && !froms.is_empty() {
			writeln!(f, "L_{}: ", label)?;
		}
		while let Some((ins, next)) = stream.split_first() {
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
				Op::Pop => write!(f, "Pop"),
				Op::Add => write!(f, "Add"),
				Op::Sub => write!(f, "Sub"),
				Op::Mul => write!(f, "Mul"),
				Op::Div => write!(f, "Div"),
				Op::Rem => write!(f, "Rem"),
				Op::And => write!(f, "And"),
				Op::Or => write!(f, "Or"),
				Op::Not => write!(f, "Not"),
				Op::CmpEq => write!(f, "CmpEq"),
				Op::CmpNeq => write!(f, "CmpNeq"),
				Op::StrEq => write!(f, "StrEq"),
				Op::CmpG => write!(f, "CmpG"),
				Op::CmpGe => write!(f, "CmpGe"),
				Op::CmpL => write!(f, "CmpL"),
				Op::CmpLe => write!(f, "CmpLe"),
				Op::PrintStr => write!(f, "PrintStr"),
				Op::LoadGlobal {
					index,
				} => write!(f, "LoadGlobal {}", index),
				Op::StoreGlobal {
					index,
				} => write!(f, "StoreGlobal {}", index),
				Op::Jmp {
					offset: _,
				} => write!(f, "Jmp L_{}", self.jumps_end.get(self.jumps_start.get(&(end_addr as usize)).unwrap()).unwrap().1),
				Op::JmpIf {
					offset: _,
				} => write!(f, "JmpIf L_{}", self.jumps_end.get(self.jumps_start.get(&(end_addr as usize)).unwrap()).unwrap().1),
				Op::JmpIfNot {
					offset: _,
				} => write!(f, "JmpIfNot L_{}", self.jumps_end.get(self.jumps_start.get(&(end_addr as usize)).unwrap()).unwrap().1),
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
			addr = end_addr;
			// print labels: 
			write!(f, "\n")?;
			if let Some(name) = self.fn_labels.get(&(addr as usize)) {
				writeln!(f, "{}:", name)?;
			}
			if let Some((froms, label)) = self.jumps_end.get(&(addr as usize)) && !froms.is_empty() {
				writeln!(f, "L_{}: ", label)?;
			}
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
			stack: Vec::new(),
			stack_frames: Vec::new(),
			rbp: 0,
		}
	}

	pub fn reset(mut self) -> Self {
		self.stack.clear();
		self.stack_frames.clear();
		self.rbp = 0;
		self
	}

	pub unsafe fn run(&mut self, state: &GrugState, globals: &[Cell<GrugValue>], instructions: &Instructions, locals_size: u32, start_loc: usize) -> Option<GrugValue> {
		let mut stream = &instructions.stream[start_loc..];
		let start_time = Instant::now();
		self.stack.resize(self.rbp + locals_size as usize, GrugValue{void: ()});
		let mut i_count: usize = 0;
		while let Some((ins, next)) = stream.split_first() {
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
					self.stack.push(*self.stack.get(self.stack.len() - 1 - index)?)
				}
				Op::Pop                => {self.stack.pop()?;}
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
				Op::And                |
				Op::Or                 => {
					let second = unsafe{self.stack.pop()?.bool};
					let first = unsafe{self.stack.pop()?.bool};
					let value = match ins {
						Op::And => (first != 0) && (second != 0),
						Op::Or  => (first != 0) || (second != 0),
						_ => unreachable!(),
					} as u8;
					self.stack.push(GrugValue{bool: value});
				}
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
				Op::PrintStr           => {
					use std::ptr::NonNull;
					let str = unsafe{
						NTStrPtr::from_ptr(
							NonNull::new_unchecked(
								std::ptr::with_exposed_provenance_mut(
									usize::from_ne_bytes(self.stack.pop()?.as_bytes())
								)
							)
						).to_str()
					};
					println!("{}", str);
				}
				Op::LoadGlobal{index}  => {
					self.stack.push(globals.get(index)?.get());
				}
				Op::StoreGlobal{index} => {
					globals.get(index)?.set(self.stack.pop()?);
				}
				Op::Jmp{offset}        => {
					stream = unsafe{
						std::slice::from_raw_parts(
							instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset),
							(stream.len() as isize - offset) as usize
						)
					}
				}
				Op::JmpIf{offset}      => {
					if unsafe{self.stack.pop()?.bool} != 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset),
								(stream.len() as isize - offset) as usize
							)
						}
					}
				}
				Op::JmpIfNot{offset}      => {
					if unsafe{self.stack.pop()?.bool} == 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								instructions.stream.as_ptr().with_addr(stream.as_ptr().addr()).offset(offset),
								(stream.len() as isize - offset) as usize
							)
						}
					}
				}
				Op::LoadLocal{index}   => {
					 let value = *self.stack.get(self.rbp + index)?;
					 self.stack.push(value);
				}
				Op::StoreLocal{index}  => {
					let value = self.stack.pop()?;
					*self.stack.get_mut(self.rbp + index)? = value;
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
					match (has_return, args) {
						(false, 0) => unsafe{(ptr.void_argless)(state)},
						(false, n) => {
							unsafe{(ptr.void)(state, self.stack.as_ptr().add(self.stack.len() - n as usize))};
							self.stack.truncate(self.stack.len() - n as usize);
						}
						(true , 0) => {
							let value = unsafe{(ptr.value_argless)(state)};
							self.stack.push(value);
						}
						(true , n) => {
							let value = unsafe{(ptr.value)(state, self.stack.as_ptr().add(self.stack.len() - n as usize))};
							self.stack.truncate(self.stack.len() - n as usize);
							self.stack.push(value);
						}
					}
					if state.is_errorring.get() {
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
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::nt;
	use crate::state::GrugState;
	use crate::types::GameFnPtrValue;

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
		state.register_game_fn("identity", identity as GameFnPtrValue).unwrap();
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
	fn vm_test_0() {
		let state = get_state();
		let stream = Instructions::new();
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_none()});
	}

	#[test]
	fn vm_test_1() {
		let state = get_state();
		let mut stream = Instructions::new();
		stream.push_op(Op::ReturnVoid);
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some()});
	}

	#[test]
	fn vm_test_2() {
		let state = get_state();
		let mut stream = Instructions::new();
		stream.push_op(Op::LoadNumber{number:25.});
		stream.push_op(Op::ReturnValue);
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
				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Add);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i + j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Sub);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i - j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Mul);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i * j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Div);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i / j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Rem);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.number == i % j)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_4() {
		let state = get_state();
		let mut stream = Instructions::new();
		let mut vm = Stack::new();

		for i in 0..1 {
			for j in 0..1 {
				let i = i != 0;
				let j = j != 0;
				if i {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::And);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i && j) as u8)});
				stream.clear();

				if i {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::Or);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i || j) as u8)});
				stream.clear();

				if i {stream.push_op(Op::LoadTrue)} else {stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::Or);
				stream.push_op(Op::ReturnValue);
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
				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpEq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i == j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpNeq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i != j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpG);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i > j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpGe);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i >= j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpL);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i < j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpLe);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i <= j) as u8)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_6() {
		let state = get_state();
		let mut stream = Instructions::new();
		let mut vm = Stack::new();

		let strings = [
			nt!("a"),
			nt!("b"),
			nt!("c"),
			nt!("d"),
			nt!("e"),
			nt!("f"),
			nt!("g"),
			nt!("aa"),
			nt!("ba"),
			nt!("ca"),
			nt!("da"),
			nt!("ea"),
			nt!("fa"),
			nt!("ga"),
		];

		for i in &strings {
			for j in &strings {
				let i = i.as_ntstrptr();
				let j = j.as_ntstrptr();
				stream.push_op(Op::LoadStr{string:i});
				stream.push_op(Op::LoadStr{string:j});
				stream.push_op(Op::StrEq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some_and(|x| x.bool == (i == j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadStr{string:i});
				stream.push_op(Op::LoadStr{string:j});
				stream.push_op(Op::PrintStr);
				stream.push_op(Op::PrintStr);
				stream.push_op(Op::ReturnVoid);
				assert!(unsafe{vm.run(&state, &[], &stream, 0, 0).is_some()});
				// panic!("{}", stream);
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
				stream.push_op(Op::LoadNumber {number: i});
				stream.push_op(Op::StoreGlobal{index: 0});
				stream.push_op(Op::LoadNumber {number: j});
				stream.push_op(Op::StoreGlobal{index: 1});
				stream.push_op(Op::LoadGlobal {index: 0});
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| x.number == i)});
				stream.clear();

				stream.push_op(Op::LoadNumber {number: i});
				stream.push_op(Op::StoreGlobal{index: 0});
				stream.push_op(Op::LoadNumber {number: j});
				stream.push_op(Op::StoreGlobal{index: 1});
				stream.push_op(Op::LoadGlobal {index: 0});
				stream.push_op(Op::LoadGlobal {index: 1});
				stream.push_op(Op::Add);
				stream.push_op(Op::StoreGlobal{index: 2});
				stream.push_op(Op::LoadGlobal {index: 2});
				stream.push_op(Op::ReturnValue);
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
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreGlobal{index: 0});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreGlobal{index: 1});
			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::StoreGlobal{index: 2});
			let cond_start = stream.get_loc();
			stream.push_op(Op::LoadGlobal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc();
			stream.push_op(Op::JmpIf{offset: 0});
			stream.push_op(Op::LoadGlobal{index: 1});
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::StoreGlobal{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::StoreGlobal{index: 0});
			stream.push_op(Op::LoadGlobal{index: 2});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::Sub);
			stream.push_op(Op::StoreGlobal{index: 2});
			let block_end_loc = stream.get_loc();
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc();
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::ReturnValue);

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
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreLocal{index: 0});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreLocal{index: 1});
			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::StoreLocal{index: 2});
			let cond_start = stream.get_loc();
			stream.push_op(Op::LoadLocal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc();
			stream.push_op(Op::JmpIf{offset: 0});
			stream.push_op(Op::LoadLocal{index: 1});
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::StoreLocal{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::StoreLocal{index: 0});
			stream.push_op(Op::LoadLocal{index: 2});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::Sub);
			stream.push_op(Op::StoreLocal{index: 2});
			let block_end_loc = stream.get_loc();
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc();
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::ReturnValue);

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
			// panic!("{}", stream);
			stream.clear();
		}
	}

	#[test]
	fn vm_test_10() {
		let state = get_state();
		let mut stream = Instructions::new();
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.push_op(Op::LoadNumber{number: 0.});
			for j in 0..i {
				stream.push_op(Op::LoadNumber{number: (i - j) as f64});
			}
			for _ in 0..(i-1) {
				stream.push_op(Op::Pop);
			}
			stream.push_op(Op::ReturnValue);
			
			assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| {assert_eq!(x.number, i as f64); true})});
			stream.clear();

			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::LoadNumber{number: i as f64 - 1.});
			stream.push_op(Op::Dup{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::Add);
			stream.push_op(Op::ReturnValue);
			assert!(unsafe{vm.run(&state, &globals, &stream, 0, 0).is_some_and(|x| {assert_eq!(x.number, 3. * i as f64 - 1.); true})});
			stream.clear();
		}
	}
}
