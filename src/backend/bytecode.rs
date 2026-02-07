#![allow(warnings)]
use crate::types::{GrugValue, GrugId, GrugScriptId, GrugOnFnId, GrugEntity, Expr, ExprType, LiteralExpr, OnFunction, Statement, Variable, BinaryOperator, GrugType, GameFnPtr, HelperFunction};
use crate::ntstring::{NTStrPtr, NTStr};
use crate::cachemap::CacheMap;
use crate::state::{GrugState, OnFnEntry};
use crate::xar::ErasedXar;
use super::{Backend, GrugAst};

use std::collections::{HashMap, HashSet};
use std::ptr::NonNull;
use std::cell::{Cell, RefCell};
use std::mem::size_of;
use std::alloc::Layout;
use std::sync::Arc;

struct Compiler {
	globals: HashMap<Arc<str>, usize>,
	locals: Vec<HashMap<Arc<str>, usize>>,
	locals_sizes: Vec<usize>,
	current_scope_size: usize,
	locals_size_max: usize,
	strings: HashSet<Arc<NTStr>>,
}

impl Compiler {
	fn new() -> Self {
		Self {
			globals: HashMap::new(),
			locals: Vec::new(),
			locals_sizes: Vec::new(),
			current_scope_size: 0,
			locals_size_max: 0,
			strings: HashSet::new(),
		}
	}

	fn compile(state: &GrugState, ast: GrugAst, init_fn_id: GrugOnFnId) -> CompiledFile {
		let mut compiler = Compiler::new();
		let mut instructions = Instructions::new();

		let globals_size = ast.global_variables.len();

		instructions.insert_on_fn(Arc::from("init_globals"), 0, 0, 0);
		for (i, global) in ast.global_variables.into_iter().enumerate() {
			compiler.compile_expr(state, &mut instructions, global.assignment_expr);
			instructions.push_op(Op::StoreGlobal{index: i});
		}
		instructions.push_op(Op::ReturnVoid);

		for (i, on_function) in ast.on_functions.into_iter().enumerate() {
			let Some(on_function) = on_function else {continue};
			let loc = instructions.get_loc();
			compiler.compile_on_fn(state, &mut instructions, on_function, i + 1);
		}

		for helper_function in ast.helper_functions {
			compiler.compile_helper_fn(state, &mut instructions, helper_function);
		}
		CompiledFile {
			init_fn_id,
			instructions,
			entities: RefCell::new(Vec::new()),
			globals_size,
			data: ErasedXar::new(Layout::array::<GrugValue>(globals_size)
				 .expect("invalid layout")
			),
		}
	}
	
	fn compile_helper_fn(&mut self, state: &GrugState, instructions: &mut Instructions, on_function: HelperFunction) {
		todo!();
	}

	fn compile_on_fn(&mut self, state: &GrugState, instructions: &mut Instructions, on_function: OnFunction, index: usize) {
		debug_assert_eq!(self.locals.len(), 0);
		debug_assert_eq!(self.current_scope_size, 0);
		self.push_scope();
		let begin_location = instructions.get_loc();
		for arg in on_function.arguments {
			self.insert_local_variable(Arc::clone(&arg.name));
		}
		for statement in on_function.body_statements {
			self.compile_statement(state, instructions, statement);
		}
		instructions.push_op(Op::ReturnVoid);
		instructions.insert_on_fn(on_function.name, index, begin_location, self.locals_size_max);
		self.locals_size_max = 0;
		self.pop_scope();
	}

	fn compile_statement(&mut self, state: &GrugState, instructions: &mut Instructions, statement: Statement) {
		match statement {
			Statement::Variable(Variable{
				name,
				ty,
				assignment_expr,
			}) => {
				self.compile_expr(state, instructions, assignment_expr);
				if let Some(ty) = ty {
					let loc = self.insert_local_variable(name);
					instructions.push_op(Op::StoreLocal{index: loc});
				} else {
					if let Some(loc) = self.get_local_location(&name) {
						instructions.push_op(Op::StoreLocal{index: loc});
					} else if let Some(loc) = self.get_global_location(&name) {
						instructions.push_op(Op::StoreGlobal{index: loc});
					} else {
						unreachable!();
					}
				}
			}
			Statement::IfStatement {
				condition,
				if_statements,
				else_if_statements,
				else_statements,
			} => {
				let mut end_patches = Vec::new();

				self.compile_expr(state, instructions, condition);

				instructions.push_op(Op::Not);
				let mut cur_patch_loc = instructions.get_loc() as isize;
				instructions.push_op(Op::JmpIf{offset: 0});

				self.push_scope();
				for statement in if_statements {
					self.compile_statement(state, instructions, statement);
				}
				self.pop_scope();
				end_patches.push(instructions.get_loc() as isize);

				instructions.push_op(Op::Jmp{offset: 0});

				let cur_loc = instructions.get_loc() as isize;
				unsafe{instructions.try_patch_jump(Op::JmpIf{offset: cur_loc - (cur_patch_loc + 3)}, cur_patch_loc as usize)}
					.expect("Could not patch jump because offset is too large");

				for (cond, elif_statements) in else_if_statements {
					self.compile_expr(state, instructions, cond);

					instructions.push_op(Op::Not);
					cur_patch_loc = instructions.get_loc() as isize;
					instructions.push_op(Op::JmpIf{offset: 0});

					self.push_scope();
					for statement in elif_statements {
						self.compile_statement(state, instructions, statement);
					}
					self.pop_scope();
					end_patches.push(instructions.get_loc() as isize);

					instructions.push_op(Op::Jmp{offset: 0});

					let cur_loc = instructions.get_loc() as isize;
					unsafe{instructions.try_patch_jump(Op::JmpIf{offset: cur_loc - (cur_patch_loc + 3)}, cur_patch_loc as usize)}
						.expect("Could not patch jump because offset is too large");
				}
				
				if let Some(else_statements) = else_statements {
					self.push_scope();
					for statement in else_statements {
						self.compile_statement(state, instructions, statement);
					}
					self.pop_scope();
				} else {
					let new_end = end_patches.pop().unwrap();
					unsafe{instructions.try_patch_jump(Op::JmpIf{offset: new_end - (cur_patch_loc + 3)}, cur_patch_loc as usize)}
						.expect("Could not patch jump because offset is too large");
					unsafe{instructions.rewind_to(new_end as usize)};
				}

				let end_loc = instructions.get_loc();
				for end_patch in end_patches {
					unsafe{instructions.try_patch_jump(Op::Jmp{offset: end_loc as isize - (end_patch + 3)}, end_patch as usize)}
						.expect("Could not patch jump because offset is too large");
				}
			}
			Statement::WhileStatement {
				condition,
				statements,
			} => {
				// TODO: add handling for breaks and continues
				let continue_loc = instructions.get_loc() as isize;
				self.compile_expr(state, instructions, condition);
				instructions.push_op(Op::Not);
				let break_patch_loc = instructions.get_loc() as isize;
				instructions.push_op(Op::JmpIf{offset: 0});
				
				for statement in statements {
					self.compile_statement(state, instructions, statement);
				}
				let end_loc = instructions.get_loc() as isize;
				instructions.push_op(Op::Jmp{offset: Op::calc_offset(end_loc, continue_loc)});
				let break_loc = instructions.get_loc() as isize;
				unsafe{instructions.try_patch_jump(Op::JmpIf{offset: Op::calc_offset(break_patch_loc, break_loc)}, break_patch_loc as usize)}.unwrap();
			}
			Statement::CallStatement {
				expr
			} => self.compile_expr(state, instructions, expr),
			_ => {
				panic!();
			}
		}
	}

	fn compile_expr(&mut self, state: &GrugState, instructions: &mut Instructions, expr: Expr) {
		match expr.ty {
			ExprType::LiteralExpr {
				expr,
				line: _,
				col: _,
			} => {
				match expr {
					LiteralExpr::TrueExpr  => instructions.push_op(Op::LoadTrue ),
					LiteralExpr::FalseExpr => instructions.push_op(Op::LoadFalse),
					LiteralExpr::StringExpr{
						value,
					}                      |
					LiteralExpr::ResourceExpr{
						value,
					}                      |
					LiteralExpr::EntityExpr{
						value,
					} => {
						match self.strings.get(&*value) {
							None => {
								self.strings.insert(Arc::clone(&value));
								// SAFETY: This returned instruction stream is only valid as long as this list of strings is available
								let string = unsafe{value.as_ntstrptr().detach_lifetime()};
								instructions.push_op(Op::LoadStr{string})
							}
							Some(value) => {
								let string = unsafe{value.as_ntstrptr().detach_lifetime()};
								instructions.push_op(Op::LoadStr{string})
							}
						}
					}
					LiteralExpr::NumberExpr {
						value,
						string: _,
					} => instructions.push_op(Op::LoadNumber{number: value}),
					LiteralExpr::IdentifierExpr{
						name
					} => {
						if let Some(loc) = self.get_local_location(&name) {
							instructions.push_op(Op::LoadLocal{index: loc});
						} else if let Some(loc) = self.get_global_location(&name) {
							instructions.push_op(Op::LoadGlobal{index: loc});
						} else {
							unreachable!();
						}
					}
					_ => todo!(),
				}
			}
			ExprType::BinaryExpr {
				operands,
				operator,
			} => {
				self.compile_expr(state, instructions, operands.0);
				match operator {
					BinaryOperator::Greater       => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::CmpG);}
					BinaryOperator::GreaterEquals => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::CmpGe);}
					BinaryOperator::Less          => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::CmpL);}
					BinaryOperator::LessEquals    => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::CmpLe);}
					BinaryOperator::Plus          => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::Add);}
					BinaryOperator::Minus         => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::Sub);}
					BinaryOperator::Multiply      => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::Mul);}
					BinaryOperator::Division      => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::Div);}
					BinaryOperator::Remainder     => {self.compile_expr(state, instructions, operands.1); instructions.push_op(Op::Rem);}
					BinaryOperator::DoubleEquals  => {
						match &operands.1.result_ty.as_ref().unwrap() {
							GrugType::String => {
								self.compile_expr(state, instructions, operands.1);
								instructions.push_op(Op::StrEq);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(state, instructions, operands.1);
								instructions.push_op(Op::CmpEq);
							}
						}
					}
					BinaryOperator::NotEquals     => {
						match operands.1.result_ty.as_ref().unwrap() {
							GrugType::String => {
								self.compile_expr(state, instructions, operands.1);
								instructions.push_op(Op::StrEq);
								instructions.push_op(Op::Not);
							}
							GrugType::Void   => unreachable!(),
							_ => {
								self.compile_expr(state, instructions, operands.1);
								instructions.push_op(Op::CmpNeq);
							}
						}
					}
					_ => {
						println!("{}", instructions);
						panic!();
					}
				}
			}
			ExprType::CallExpr {
				function_name,
				arguments,
				line: _,
				col: _,
			} if function_name.starts_with("helper_") => {unimplemented!()},
			ExprType::CallExpr {
				function_name,
				arguments,
				line: _,
				col: _,
			} => {
				let args = arguments.len();
				for argument in arguments {
					self.compile_expr(state, instructions, argument);
				}
				let fn_ptr = state.game_functions.get(&*function_name)
					.expect("Can't find game function");
				
				let has_return = state.mod_api.game_functions().get(&function_name).unwrap().return_ty != GrugType::Void;
				instructions.push_op(Op::CallGameFunction {
					has_return,
					args,
					ptr: *fn_ptr,
				});
			}
			_ => {
				println!("{}", instructions);
				panic!();
			}
		}
	}

	fn insert_local_variable(&mut self, name: Arc<str>) -> usize {
		debug_assert!(self.locals.last_mut().unwrap().insert(name, self.current_scope_size).is_none());
		self.current_scope_size += 1;
		self.locals_size_max = std::cmp::max(self.locals_size_max, self.current_scope_size);
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
	init_fn_id: GrugOnFnId,
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
	fn insert_file(&self, state: &GrugState, id: GrugScriptId, on_functions: &[OnFnEntry], file: GrugAst) {
		let compiled_file = Compiler::compile(state, file, on_functions[0].id);
		match self.files.try_insert(id, compiled_file) {
			Ok(()) => (),
			Err((id, compiled_file)) => {
				unimplemented!();
			}
		}
	}

	fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> bool {
		let file = self.files.get(&entity.file_id)
			.expect("file already compiled");
		let globals = unsafe{&*file.data.get_slot().write_slice(file.globals_size, Cell::new(GrugValue{void: ()}))};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		let ret_val = unsafe{stack.run(state, globals, &file.instructions, 0, 0)}.is_some();

		entity.members.set(NonNull::from_ref(globals).cast::<()>());

		stack = stack.reset();
		self.stacks.borrow_mut().push(stack);
		ret_val
	}
	fn clear_entities(&mut self) {
		todo!()
	}
	fn destroy_entity_data(&self, entity: &GrugEntity) -> bool {
		todo!()
	}
	unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> bool {
		todo!()
	}
	fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> bool {
		let file = self.files.get(&entity.file_id)
			.expect("file already compiled");

		let globals = unsafe{std::slice::from_raw_parts(entity.members.get().cast::<Cell<GrugValue>>().as_ptr(), file.globals_size)};
		let mut stack = self.stacks.borrow_mut().pop().unwrap_or_else(|| Stack::new());
		let Some((start_loc, locals_size)) = file.instructions.on_fn_locations[(on_fn_id - file.init_fn_id) as usize] else {
			return false;
		};
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
	// 0x00
	ReturnVoid,
	// 0x01
	ReturnValue,
	// All the four following types have the same instruction
	// representation. The variants just exist to make encoding easier
	// 0x03
	LoadQW {
		bytes: [u8; 8],
	},
	// 0x03
	LoadNumber {
		number: f64,
	},
	// 0x04
	LoadStr {
		string: NTStrPtr<'static>,
	},
	// 0x05
	LoadFalse,
	// 0x06
	LoadTrue,
	// 0x07        | 0x08
	// 0b00000111  | 0b00001000
	// index as u8 | index as u16
	Dup{
		index: usize,
	},
	// 0x09
	Pop,
	// 0b00010000
	// 0x10
	Add,
	// 0x11
	Sub,
	// 0x12
	Mul,
	// 0x13
	Div,
	// 0x14
	Rem,
	// 0x15
	And,
	// 0x16
	Or,
	// 0x17
	Not,
	// 0x18
	CmpEq,
	// 0x19
	CmpNeq,
	// 0x1a
	StrEq,
	// 0x1b
	CmpG,
	// 0x1c
	CmpGe,
	// 0x1d
	CmpL,
	// 0x1e
	CmpLe,
	// 0x1f
	PrintStr,
	// 0x20          | 0x21
	// 0b00100000    | 0b00100001
	// index as u8   | index as u16
	LoadGlobal {
		index: usize,
	},
	// 0x22          | 0x23
	// 0b00100010    | 0b00100011
	// index as u8   | index as u16
	StoreGlobal {
		index: usize,
	},
	// 0x24          | 0x25
	// 0b00100100    | 0b00100101
	// offset as i16 | offset as isize
	Jmp {
		offset: isize,
	},
	// 0x26          | 0x27
	// 0b00100110    | 0b00100111
	// offset as i16 | offset as isize
	JmpIf {
		offset: isize,
	},
	// 0x28          | 0x29
	// 0b00101000    | 0b00101001
	LoadLocal {
		index: usize,
	},
	// 0x2a          | 0x2b
	// 0b00101010    | 0b00101011
	StoreLocal {
		index: usize,
	},
	// 0x2c          | 0x2d
	// 0b00101100    | 0b00101101
	// CallHelper {
	// 	args: usize,
	// 	helper_index: usize
	// },
	// 0x2e          | 0x2f
	// 0b00101110    | 0b00101111
	// args as u7    | args as u15
	CallGameFunction {
		has_return: bool,
		args: usize,
		ptr: GameFnPtr,
	},
}

impl PartialEq for Op {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::ReturnVoid, Self::ReturnVoid)       => true,
			(Self::ReturnValue, Self::ReturnValue)     => true,
			(Self::LoadQW{..} | Self::LoadNumber{..} | Self::LoadStr{..}, 
			Self::LoadQW{..} | Self::LoadNumber{..} | Self::LoadStr{..}) => {
				let self_bytes = match self {
					Self::LoadQW{bytes} => bytes,
					Self::LoadNumber{number} => &number.to_ne_bytes(),
					Self::LoadStr{string} => &(string.as_ptr() as usize).to_ne_bytes(),
					_ => unreachable!(),
				};
				let other_bytes = match other {
					Self::LoadQW{bytes} => bytes,
					Self::LoadNumber{number} => &number.to_ne_bytes(),
					Self::LoadStr{string} => &(string.as_ptr() as usize).to_ne_bytes(),
					_ => unreachable!(),
				};
				self_bytes == other_bytes
			}
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
			(Self::CallGameFunction {has_return: kind0, args: args0, ptr: ptr0}, 
			 Self::CallGameFunction {has_return: kind1, args: args1, ptr: ptr1}) => kind0 == kind1 && args0 == args1 && ptr0 == ptr1,
			_ => false,
		}
	}
}

impl Op {
	fn decode(bytes: &mut &[u8]) -> Option<Self> {
		let (value, len) = match bytes.get(0)? {
			0x00 => (Self::ReturnVoid, 1),
			0x01 => (Self::ReturnValue, 1),
			// LoadQW
			// LoadNumber
			// LoadId
			0x03 => {
				let bytes = &mut bytes.get(1..)?;
				let value = get_u64_bytes(bytes)?;
				(Op::LoadQW{
					bytes: value,
				}, 1 + size_of::<u64>())
			}
			// LoadStr
			0x04 => {
				let bytes = &mut bytes.get(1..)?;
				use std::ptr::NonNull;
				let value = unsafe{NTStrPtr::from_ptr(NonNull::new_unchecked(std::ptr::with_exposed_provenance_mut(usize::from_ne_bytes(get_u64_bytes(bytes)?))))};
				(Op::LoadStr{
					string: value,
				}, 1 + size_of::<u64>())
			}
			// LoadTrue
			0x05 => (Op::LoadFalse, 1),
			// LoadFalse
			0x06 => (Op::LoadTrue, 1),
			// Dup
			0x07 => {
				let index = *bytes.get(1)? as usize;
				(Op::Dup{
					index
				}, 2)
			}
			0x08 => {
				unimplemented!();
				// let bytes = &mut bytes.get(1..)?;
				// let index = u16::from_ne_bytes(get_u16_bytes(bytes)?) as usize;
				// (Op::Dup{
				// 	index
				// }, 2)
			}
			0x09 => (Op::Pop   ,   1),
			0x10 => (Op::Add   ,   1),
			0x11 => (Op::Sub   ,   1),
			0x12 => (Op::Mul   ,   1),
			0x13 => (Op::Div   ,   1),
			0x14 => (Op::Rem   ,   1),
			0x15 => (Op::And   ,   1),
			0x16 => (Op::Or    ,   1),
			0x17 => (Op::Not   ,   1),
			0x18 => (Op::CmpEq ,   1),
			0x19 => (Op::CmpNeq,   1),
			0x1a => (Op::StrEq ,   1),
			0x1b => (Op::CmpG  ,   1),
			0x1c => (Op::CmpGe ,   1),
			0x1d => (Op::CmpL  ,   1),
			0x1e => (Op::CmpLe ,   1),
			0x1f => (Op::PrintStr, 1),
			// 0x20       | 0x21
			// 0b00100000 | 0b00100001
			// u8 index
			0x20 => {
				let index = *bytes.get(1)? as usize;
				(Op::LoadGlobal{index}, 2)
			}
			// u16 index
			0x21 => {unimplemented!()}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x22 => {
				let index = *bytes.get(1)? as usize;
				(Op::StoreGlobal{index}, 2)
			}
			// u16 index
			0x23 => {unimplemented!()}
			// 0x24          | 0x25
			// 0b00100100    | 0b00100101
			// offset as i16 | offset as isize
			0x24 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i16>())
			}
			0x25 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i64>())
			}
			// 0x26          | 0x27
			// 0b00100110    | 0b00100111
			// offset as i16 | offset as isize
			0x26 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i16>())
			}
			0x27 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i64>())
			}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x28 => {
				let index = *bytes.get(1)? as usize;
				(Op::LoadLocal{index}, 2)
			}
			// u16 index
			0x29 => {unimplemented!()}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x2a => {
				let index = *bytes.get(1)? as usize;
				(Op::StoreLocal{index}, 2)
			}
			// u16 index
			0x2b => {unimplemented!()}
			0x2e => {
				let next_byte = bytes.get(1)?;
				let kind = next_byte >> (size_of::<u8>() * 8 - 1) == 1;
				let args = (next_byte & (0b01111111)) as usize;

				let bytes = &mut bytes.get(2..)?;
				let ptr = unsafe{std::mem::transmute::<*const (), GameFnPtr>(
					std::ptr::with_exposed_provenance(
						usize::from_ne_bytes(get_usize_bytes(bytes)?)
					)
				)};
				(Op::CallGameFunction {
					has_return: kind,
					args,
					ptr,
				}, 1 + 1 + size_of::<usize>())
			}
			0x2f => {unimplemented!()}
			_ => return None,
		};
		*bytes = &bytes[len..];
		return Some(value);

		fn get_usize_bytes(bytes: &mut &[u8]) -> Option<[u8; size_of::<usize>()]> {
			if bytes.len() < size_of::<usize>() {
				return None;
			}
			Some(unsafe{*bytes.as_ptr().cast::<[u8; _]>()})
		}

		fn get_u64_bytes(bytes: &mut &[u8]) -> Option<[u8; size_of::<u64>()]> {
			if bytes.len() < size_of::<u64>() {
				return None;
			}
			Some(unsafe{*bytes.as_ptr().cast::<[u8; _]>()})
		}

		fn get_u16_bytes(bytes: &mut &[u8]) -> Option<[u8; size_of::<u16>()]> {
			if bytes.len() < size_of::<u16>() {
				return None;
			}
			Some(unsafe{*bytes.as_ptr().cast::<[u8; _]>()})
		}
	}

	fn calc_offset(from: isize, to: isize) -> isize {
		let offset_from_start = to - from;
		if offset_from_start < i16::MIN as isize || offset_from_start > i16::MAX as isize {
			todo!();
		}
		// opcode is 1 byte, offset fits in two bytes
		offset_from_start - 3
	}
}

pub struct Instructions{
	stream: Vec<u8>,
	on_fn_locations: Vec<Option<(/* start location in instruction stream */ usize, /* number of locals */ usize)>>,
	helper_fn_locations: Vec</* start location in instruction stream */ usize>,
	fn_labels: HashMap<usize, Arc<str>>,
	strings: HashSet<Arc<NTStr>>,
	jumps_count: usize,
	jumps_end: HashMap</* to */ usize, (/* from */ Vec<usize>, /* label */ usize)>,
	jumps_start: HashMap</* from */ usize, /* to */ usize>,
	// game_functions: HashSet<GameFunctionInfo>,
}

impl Instructions {
	pub fn new() -> Self {
		Self {
			stream: Vec::new(),
			on_fn_locations: Vec::new(),
			helper_fn_locations: Vec::new(),
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
		self.strings.clear();
		self.jumps_end.clear();
		self.jumps_start.clear();
	}

	pub fn push_op(&mut self, op: Op) {
		match op {
			Op::ReturnVoid  =>  self.stream.push(0x00),
			Op::ReturnValue => self.stream.push(0x01),
			Op::LoadQW{bytes}  => {
				self.stream.push(
					0x03
				);
				self.stream.extend_from_slice(&bytes);
			}
			Op::LoadNumber{number}  => {
				self.stream.push(
					0x03
				);
				self.stream.extend_from_slice(&number.to_ne_bytes());
			}
			Op::LoadStr{string}  => {
				self.stream.push(
					0x04
				);
				self.stream.extend_from_slice(&string.as_ptr().expose_provenance().to_ne_bytes());
			}
			Op::LoadFalse => self.stream.push(0x05),
			Op::LoadTrue  => self.stream.push(0x06),
			Op::Dup{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.stream.push(0x07);
				self.stream.push(index as u8);
			}
			Op::Pop       => self.stream.push(0x09),
			Op::Add       => self.stream.push(0x10),
			Op::Sub       => self.stream.push(0x11),
			Op::Mul       => self.stream.push(0x12),
			Op::Div       => self.stream.push(0x13),
			Op::Rem       => self.stream.push(0x14),
			Op::And       => self.stream.push(0x15),
			Op::Or        => self.stream.push(0x16),
			Op::Not       => self.stream.push(0x17),
			Op::CmpEq     => self.stream.push(0x18),
			Op::CmpNeq    => self.stream.push(0x19),
			Op::StrEq     => self.stream.push(0x1a),
			Op::CmpG      => self.stream.push(0x1b),
			Op::CmpGe     => self.stream.push(0x1c),
			Op::CmpL      => self.stream.push(0x1d),
			Op::CmpLe     => self.stream.push(0x1e),
			Op::PrintStr  => self.stream.push(0x1f),
			Op::LoadGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.stream.push(0x20);
				self.stream.push(index as u8);
			}
			Op::StoreGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.stream.push(0x22);
				self.stream.push(index as u8);
			}
			Op::Jmp{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.stream.push(0x24);
				let bytes = offset.to_ne_bytes();
				self.stream.push(bytes[0]);
				self.stream.push(bytes[1]);
				self.insert_jmp(self.stream.len(), offset);
			}
			Op::JmpIf{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.stream.push(0x26);
				let bytes = offset.to_ne_bytes();
				self.stream.push(bytes[0]);
				self.stream.push(bytes[1]);
				self.insert_jmp(self.stream.len(), offset);
			}
			Op::LoadLocal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.stream.push(0x28);
				self.stream.push(index as u8);
			}
			Op::StoreLocal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.stream.push(0x2a);
				self.stream.push(index as u8);
			}
			Op::CallGameFunction {
				has_return,
				args,
				ptr,
			} => {
				if args > (u8::MAX / 2) as usize {
					unimplemented!();
				}
				self.stream.push(0x2e);
				let next_byte = ((args as u8) & 0b01111111) | ((has_return as u8) << 7);
				self.stream.push(next_byte);
				self.stream.extend_from_slice(&unsafe{(ptr.void as usize).to_ne_bytes()});
			}
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

	pub fn insert_on_fn(&mut self, name: Arc<str>, index: usize, location: usize, locals_size: usize) {
		if self.on_fn_locations.len() <= index {
			self.on_fn_locations.resize(index + 1, None);
		}
		self.on_fn_locations[index] = Some((location, locals_size));
		self.fn_labels.insert(location, name);
	}

	pub fn begin_helper_fn(&mut self, name: Arc<str>) {
		self.helper_fn_locations.push(self.get_loc());
		self.fn_labels.insert(self.get_loc(), name);
	}

	pub fn assert_jumps_consistency(&self) {
		for (end, starts) in &self.jumps_end {
			for start in &starts.0 {
				assert_eq!(self.jumps_start.get(start).unwrap(), end);
			}
		}
	}

	/// SAFETY:
	/// `location` must be at the end of an instruction and the beginning of
	/// the next one
	/// Invalidates all instructions upto `locations`.
	///
	pub unsafe fn rewind_to(&mut self, location: usize) {
		self.stream.truncate(location);
	}

	pub fn get_loc(&self) -> usize {
		self.stream.len()
	}

	/// SAFETY: 
	/// location passed in must be the position returned by a previous call to push_op.
	///
	/// It will succeed if opcode matches and there is enough space to store a jump of that type
	pub unsafe fn try_patch_jump(&mut self, op: Op, location: usize) -> Option<()> {
		macro_rules! patch {
			($offset_type: ty, $new_offset: expr) => {{
				unsafe {
					*(self.stream.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<$offset_type>()]) = ($new_offset as $offset_type).to_ne_bytes();
				} 
				(location + 1 + size_of::<$offset_type>(), $new_offset as isize)
			}}
		}
		let (old_start, new_offset)  = match (self.stream.get(location)?, op) {
			// Jmp
			(0x24, Op::Jmp{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				patch!(i16, offset)
			}
			(0x25, Op::Jmp{offset}) => {
				patch!(isize, offset)
			}
			// JmpIf
			(0x26, Op::JmpIf{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				patch!(i16, offset)
			}
			(0x27, Op::JmpIf{offset}) => {
				patch!(isize, offset)
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
		let mut buf = &*self.stream;
		f.debug_list()
			.entries(
				std::iter::from_fn(|| {
					if buf.len() == 0 {
						None
					} else {
						Some(Op::decode(&mut buf)
							.expect("Invalid instruction"))
					}
				})
			)
			.finish()
	}
}

impl std::fmt::Display for Instructions {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let stream = &mut &*self.stream;
		let mut addr = unsafe{stream.as_ptr().byte_offset_from(self.stream.as_ptr())};
		if let Some(name) = self.fn_labels.get(&(addr as usize)) {
			writeln!(f, "{}:", name);
		}
		if let Some((froms, label)) = self.jumps_end.get(&(addr as usize)) && !froms.is_empty() {
			writeln!(f, "L_{}: ", label);
		}
		while let Some(ins) = Op::decode(stream) {
			let end_addr = unsafe{stream.as_ptr().byte_offset_from(self.stream.as_ptr())};
			write!(f, " 0x{:08x} ", addr)?;
			match ins {
				Op::ReturnVoid => write!(f, "ReturnVoid"),
				Op::ReturnValue => write!(f, "ReturnValue"),
				Op::LoadQW {
					bytes,
				} => write!(f, "Load {:?}", f64::from_ne_bytes(bytes)),
				Op::LoadStr {
					string,
				} => write!(f, "LoadStr {:?}", string.to_str()),
				Op::LoadNumber {
					number: _,
				} => unreachable!(),
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
					offset,
				} => write!(f, "Jmp L_{}", self.jumps_end.get(self.jumps_start.get(&(end_addr as usize)).unwrap()).unwrap().1),
				Op::JmpIf {
					offset,
				} => write!(f, "JmpIf L_{}", self.jumps_end.get(self.jumps_start.get(&(end_addr as usize)).unwrap()).unwrap().1),
				Op::LoadLocal {
					index,
				} => write!(f, "LoadLocal {}", index),
				Op::StoreLocal {
					index,
				} => write!(f, "StoreLocal {}", index),
				Op::CallGameFunction {
					has_return,
					args,
					ptr,
				} => write!(f, "CallGameFunction 0x{:016x}", &unsafe{std::mem::transmute::<GameFnPtr, *const ()>(ptr).addr()}),
			}?;
			addr = end_addr;
			// print labels: 
			write!(f, "\n")?;
			if let Some(name) = self.fn_labels.get(&(addr as usize)) {
				writeln!(f, "{}:", name);
			}
			if let Some((froms, label)) = self.jumps_end.get(&(addr as usize)) && !froms.is_empty() {
				writeln!(f, "L_{}: ", label);
			}
		}
		Ok(())
	}
}

pub struct Stack {
	stack: Vec<GrugValue>,
	rbp: usize,
}
impl Stack {
	pub fn new() -> Self {
		Self {
			stack: Vec::new(),
			rbp: 0,
		}
	}

	pub fn reset(mut self) -> Self {
		self.stack.clear();
		self.rbp = 0;
		self
	}

	pub unsafe fn run(&mut self, state: &GrugState, globals: &[Cell<GrugValue>], instructions: &Instructions, locals_size: usize, start_loc: usize) -> Option<GrugValue> {
		let string = format!("{}", instructions);
		let mut stream = &instructions.stream[start_loc..];
		self.stack.resize(self.rbp + locals_size, GrugValue{void: ()});
		while let Some(ins) = Op::decode(&mut stream) {
			match ins {
				Op::ReturnVoid         => {
					self.stack.truncate(self.rbp);
					if self.stack.len() == 0 {
						return Some(GrugValue{void: ()});
					} else {
						let ip = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						let rbp = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						self.rbp = rbp as usize;
						stream = &mut &*instructions.stream.get(ip as usize ..)?;
					}
				}
				Op::ReturnValue        => {
					let ret_val = self.stack.pop()?;
					self.stack.truncate(self.rbp);
					if self.stack.len() == 0 {
						return Some(ret_val);
					} else {
						self.stack.push(ret_val);
						let ip = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						let rbp = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						self.rbp = rbp as usize;
						stream = &mut &*instructions.stream.get(ip as usize ..)?;
					}
				}
				Op::LoadQW{bytes}      => self.stack.push(GrugValue::from_bytes(bytes)),
				Op::LoadStr{string}    => self.stack.push(GrugValue{string}),
				Op::LoadNumber{..}     => unreachable!(),
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
				Op::LoadLocal{index}   => {
					 let value = *self.stack.get(self.rbp + index)?;
					 self.stack.push(value);
				}
				Op::StoreLocal{index}  => {
					let value = self.stack.pop()?;
					*self.stack.get_mut(self.rbp + index)? = value;
				}
				Op::CallGameFunction {
					has_return,
					args,
					ptr,
				} => {
					match (has_return, args) {
						(false, 0) => (ptr.void_argless)(state),
						(false, n) => {
							(ptr.void)(state, self.stack.as_ptr().add(self.stack.len() - n));
							self.stack.truncate(self.stack.len() - n);
						}
						(true , 0) => {
							let value = (ptr.value_argless)(state);
							self.stack.push(value);
						}
						(true , n) => {
							let value = (ptr.value)(state, self.stack.as_ptr().add(self.stack.len() - n));
							self.stack.truncate(self.stack.len() - n);
							self.stack.push(value);
						}
					}
					if state.is_errorring.get() {
						return None
					}
				}
			}
		}
		println!("{}", string);
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::nt;
	use crate::state::GrugState;
	use crate::types::GameFnPtrValue;
	use super::super::interpreter::Interpreter;

	const MOD_API: &'static str /* ' */= r#"
	{
		"entities" : {
			"A" : {
				"description": "A",
				"on_functions": {
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
"#; // '
	static mut IDENTITY_ARG: f64 = 0.;
	extern "C" fn identity(_: &GrugState, arguments: *const GrugValue) -> GrugValue {
		unsafe{IDENTITY_ARG = (*arguments).number;}
		unsafe{*arguments}
	}

	extern "C" fn void_argless(_: &GrugState) {
	}
	extern "C" fn void_arg(_: &GrugState, _arguments: *const GrugValue) {
	}
	extern "C" fn value_argless(_: &GrugState) -> GrugValue {
		GrugValue{void: ()}
	}
	extern "C" fn value_arg(_: &GrugState, _arguments: *const GrugValue) -> GrugValue {
		GrugValue{void: ()}
	}

	fn get_state() -> GrugState {
		let mut state = GrugState::new_from_text(
			MOD_API,
			"doesn't matter",
			Default::default(),
			BytecodeBackend::new(),
		).unwrap();
		state.register_game_fn("identity", identity as GameFnPtrValue).unwrap();
		assert!(state.all_game_fns_registered());
		state
	}

	#[test]
	fn vm_test_state() {
		let state = get_state();

		let on_fib_id    = state.get_on_fn_id("A", "on_fib").unwrap();
		let on_double_id = state.get_on_fn_id("A", "on_double").unwrap();
		let _file = state.compile_grug_file_from_str("path/test-A.gru ", GRUG_FILE_TEXT).unwrap();
		let entity = state.create_entity(_file).unwrap();
		assert!(state.call_on_function(&entity, on_fib_id, &[GrugValue{number: 30.}]));
		unsafe{assert_eq!(*&raw const IDENTITY_ARG, 832040.);}
		
		assert!(state.call_on_function(&entity, on_double_id, &[GrugValue{number: 25.}]));
		unsafe{assert_eq!(*&raw const IDENTITY_ARG, 50.);}
	}
		
	#[test]
	fn vm_test_decoding() {
		let mut stream = Instructions::new();
		macro_rules! test_op {
			($op: expr) => {{
				stream.clear();
				let op = $op;
				stream.push_op(op);
				assert_eq!(Op::decode(&mut &*stream.stream), Some(op));
			}}
		}
		test_op!(Op::ReturnVoid);
		test_op!(Op::ReturnValue);
		for i in 0..10_usize {
			test_op!(Op::LoadQW{bytes: i.to_ne_bytes()});
			test_op!(Op::LoadNumber{number: i as f64});
			test_op!(Op::StoreGlobal{index: i});
			test_op!(Op::LoadGlobal{index: i});
			test_op!(Op::JmpIf{offset: i as isize});
			test_op!(Op::JmpIf{offset: -(i as isize)});
			test_op!(Op::Jmp{offset: i as isize});
			test_op!(Op::Jmp{offset: -(i as isize)});
			test_op!(Op::StoreLocal{index: i});
			test_op!(Op::LoadLocal{index: i});
			test_op!(Op::Dup{index: i});
			test_op!(Op::Dup{index: 10 * i});
		}
		test_op!(Op::LoadFalse);
		test_op!(Op::LoadTrue);
		test_op!(Op::Pop);
		test_op!(Op::Add);
		test_op!(Op::Sub);
		test_op!(Op::Mul);
		test_op!(Op::Div);
		test_op!(Op::Rem);
		test_op!(Op::And);
		test_op!(Op::Or);
		test_op!(Op::Not);
		test_op!(Op::CmpEq);
		test_op!(Op::CmpNeq);
		test_op!(Op::StrEq);
		test_op!(Op::CmpG);
		test_op!(Op::CmpGe);
		test_op!(Op::CmpL);
		test_op!(Op::CmpLe);
		test_op!(Op::CallGameFunction{
			has_return: false,
			args: 0,
			ptr: GameFnPtr{void_argless: void_argless},
		});
	}

	#[test]
	fn vm_test_patching() {
		let mut stream = Instructions::new();
		for i in 0..20 {
			stream.push_op(Op::Jmp{offset: 0});
			unsafe{stream.try_patch_jump(Op::Jmp{offset: i as isize * 10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.stream).unwrap(), Op::Jmp{offset: i as isize * 10});
			stream.clear();

			stream.push_op(Op::Jmp{offset: 0});
			unsafe{stream.try_patch_jump(Op::Jmp{offset: i as isize * -10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.stream).unwrap(), Op::Jmp{offset: i as isize * -10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * 10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.stream).unwrap(), Op::JmpIf{offset: i as isize * 10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * -10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.stream).unwrap(), Op::JmpIf{offset: i as isize * -10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * -1000}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.stream).unwrap(), Op::JmpIf{offset: i as isize * -1000});
			stream.clear();
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
			let cond_start = stream.get_loc() as isize;
			stream.push_op(Op::LoadGlobal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc() as isize;
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
			let block_end_loc = stream.get_loc() as isize;
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc() as isize;
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::ReturnValue);

			unsafe{stream.try_patch_jump(Op::JmpIf{offset: (block_continue_loc - block_begin_jump - 3)}, block_begin_jump as usize).unwrap()};
			unsafe{stream.try_patch_jump(Op::Jmp{offset: (cond_start - block_end_loc - 3)}, block_end_loc as usize).unwrap()};
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
			let cond_start = stream.get_loc() as isize;
			stream.push_op(Op::LoadLocal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc() as isize;
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
			let block_end_loc = stream.get_loc() as isize;
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc() as isize;
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::ReturnValue);

			unsafe{stream.try_patch_jump(Op::JmpIf{offset: (block_continue_loc - block_begin_jump - 3)}, block_begin_jump as usize).unwrap()};
			unsafe{stream.try_patch_jump(Op::Jmp{offset: (cond_start - block_end_loc - 3)}, block_end_loc as usize).unwrap()};
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
