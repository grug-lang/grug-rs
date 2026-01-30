use crate::types::{GlobalVariable, OnFunction, HelperFunction, GrugScriptId, GrugEntity, GrugOnFnId, GrugValue};
use crate::state::GrugState;
use crate::error::RuntimeError;

use std::ptr::NonNull;

#[derive(Debug)]
pub struct GrugFile {
	pub(crate) global_variables: Vec<GlobalVariable>,
	pub(crate) on_functions: Vec<Option<OnFunction>>,
	pub(crate) helper_functions: Vec<HelperFunction>,
}

pub mod interpreter {
	use crate::types::{GrugValue, GlobalVariable, GrugId, Argument, Statement, Expr, ExprType, LiteralExpr, UnaryOperator, GrugType, BinaryOperator, Variable, GrugOnFnId, GrugScriptId, GrugEntity};
	use super::{GrugFile, Backend};
	use crate::error::{RuntimeError, ON_FN_TIME_LIMIT, MAX_RECURSION_LIMIT};
	use crate::state::GrugState;
	use crate::cachemap::CacheMap;
	use crate::xar::ErasedXar;

	use std::ptr::NonNull;
	use std::sync::Arc;
	use std::cell::{Cell, RefCell};
	use std::collections::HashMap;
	use std::ffi::CStr;
	use std::time::{Duration, Instant};
	use std::alloc::Layout;

	pub struct GrugEntityData {
		pub(crate) global_variables: HashMap<Arc<str>, Cell<GrugValue>>,
	}

	impl GrugEntityData {
		pub(crate) fn get_global_variable(&self, name: &str) -> Option<&Cell<GrugValue>> {
			self.global_variables.get(name)
		}
	}

	#[derive(Debug)]
	struct CompiledFile {
		file: GrugFile,
		entities: RefCell<Vec<NonNull<GrugEntity>>>,
		data: ErasedXar,
	}

	impl CompiledFile {
		fn new(file: GrugFile) -> Self {
			Self {
				file,
				entities: RefCell::new(Vec::new()),
				data: ErasedXar::new(Layout::new::<GrugEntityData>()),
			}
		}
	}
	
	pub struct Interpreter {
		files: CacheMap<GrugScriptId, CompiledFile>,
		file_id_map: CacheMap<String, GrugScriptId>,
		next_id: Cell<u64>,
	}
	
	struct CallStack {
		local_variables: Vec<Vec<HashMap<Arc<str>, GrugValue>>>,
	}

	impl CallStack {
		fn new() -> Self {
			Self {
				local_variables: Vec::new(),
			}
		}
		
		fn pop_scope(&mut self) {
			self.local_variables.last_mut()
				.expect("must already have a stack frame").pop()
				.expect("must have scope");
		}

		fn add_local_variable(&mut self, name: Arc<str>, value: GrugValue) {
			assert!(self.local_variables.last_mut()
				.expect("must have stack frame").last_mut()
				.expect("last frame must have scope").insert(name, value)
				.is_none(), "variable already exists");
		}

		fn pop_stack_frame(&mut self) {
			self.local_variables.pop().expect("must have stack frame");
		}

		fn push_scope(&mut self) {
			self.local_variables.last_mut()
				.expect("must already have a stack frame")
				.push(HashMap::new());
		}

		fn push_stack_frame(&mut self) {
			self.local_variables.push(Vec::new());
		}

		fn get_local_variable(&mut self, name: &str) -> Option<&mut GrugValue> {
			for scope in self.local_variables.last_mut()?{
				if let Some(val) = scope.get_mut(name) {
					return Some(val)
				}
			}
			None
		}
	}

	enum GrugControlFlow {
		Return(GrugValue),
		Break,
		Continue,
		None,
	}

	// should be moved into backend later
	impl Interpreter {
		pub fn new() -> Self {
			Self {
				files: CacheMap::new(),
				file_id_map: CacheMap::new(),
				next_id: Cell::new(0),
			}
		}

		fn get_next_script_id(&self) -> GrugScriptId {
			let id = self.next_id.get();
			self.next_id.set(id + 1);
			GrugId::new(id)
		}

		fn run_function(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, arguments: &[Argument], values: &[GrugValue], statements: &[Statement]) -> Result<GrugValue, RuntimeError> {
			if call_stack.local_variables.len() > MAX_RECURSION_LIMIT {
				return Err(RuntimeError::StackOverflow)
			}
			if arguments.len() != values.len() {
				panic!("argument count mismatch")
			}
			call_stack.push_stack_frame();
			call_stack.push_scope();

			for (argument, value) in arguments.iter().zip(values) {
				call_stack.add_local_variable(Arc::clone(&argument.name), *value);
			}
			let value = self.run_statements(call_stack, state, file, entity, statements)?;
			let value = match value {
				GrugControlFlow::Return(value) => value,
				GrugControlFlow::None          => GrugValue{void: ()},
				GrugControlFlow::Break         => unreachable!(),
				GrugControlFlow::Continue      => unreachable!(),
			};

			call_stack.pop_scope();
			call_stack.pop_stack_frame();
			Ok(value)
		}

		fn run_statements(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, statements: &[Statement]) -> Result<GrugControlFlow, RuntimeError> {
			call_stack.push_scope();
			let mut ret_val = GrugControlFlow::None;
			'outer: for statement in statements {
				match statement {
					Statement::Variable(Variable{
						name,
						ty,
						assignment_expr,
					}) => {
						let assignment_expr = self.run_expr(call_stack, state, file, entity, assignment_expr)?;
						if let Some(_) = ty {
							call_stack.add_local_variable(Arc::clone(name), assignment_expr);
						} else {
							if let Some(var) = call_stack.get_local_variable(&**name) {
								*var = assignment_expr;
							} else if let Some(var) = entity.get_global_variable(&**name) {
								var.set(assignment_expr);
							} else {
								panic!("variable not found");
							}
						}
					},
					Statement::CallStatement {
						expr
					} => {
						self.run_expr(call_stack, state, file, entity, expr)?;
					},
					Statement::IfStatement{
						condition,
						if_statements,
						else_if_statements,
						else_statements,
					} => {
						let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool};
						// if statement
						if condition != 0 {
							let control_flow = self.run_statements(call_stack, state, file, entity, if_statements)?;
							if let GrugControlFlow::None = control_flow {
								continue;
							} else {
								ret_val = control_flow;
								break 'outer;
							} 
						} else {
							// else if statements
							for (condition, else_if_statements) in else_if_statements {
								let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool};
								if condition != 0 {
									let control_flow = self.run_statements(call_stack, state, file, entity, else_if_statements)?;
									if let GrugControlFlow::None = control_flow {
										// go to the next outer statment if any else if
										// condition was true and there was no return
										continue 'outer;
									} else {
										ret_val = control_flow;
										break 'outer;
									} 
								}
							}
							// else statements
							if let Some(else_statements) = else_statements {
								let control_flow = self.run_statements(call_stack, state, file, entity, else_statements)?;
								if let GrugControlFlow::None = control_flow {
									continue;
								} else {
									ret_val = control_flow;
									break 'outer;
								} 
							}
						}
					},
					Statement::ReturnStatement{
						expr,
					} => {
						if let Some(expr) = expr {
							ret_val = GrugControlFlow::Return(self.run_expr(call_stack, state, file, entity, expr)?);
						} else {
							ret_val = GrugControlFlow::Return(GrugValue{void: ()});
						}
						break 'outer;
					},
					Statement::WhileStatement{
						condition,
						statements,
					} => {
						loop {
							let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool};
							if condition == 0 {
								break;
							}
							match self.run_statements(call_stack, state, file, entity, statements)? {
								GrugControlFlow::Return(value) => {
									ret_val = GrugControlFlow::Return(value);
									break 'outer;
								}
								GrugControlFlow::Continue => (),
								GrugControlFlow::Break    => break,
								GrugControlFlow::None     => (),
							}
						}
					},
					Statement::Comment{
						value: _,
					} => (),
					Statement::BreakStatement => {
						ret_val = GrugControlFlow::Break;
						break 'outer;
					},
					Statement::ContinueStatement => {
						ret_val = GrugControlFlow::Continue;
						break 'outer;
					},
					Statement::EmptyLineStatement => (),
				}
			}
			call_stack.pop_scope();
			Ok(ret_val)
		}

		fn run_expr(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, expr: &Expr) -> Result<GrugValue, RuntimeError> {
			if Instant::elapsed(&state.call_start_time.get()) > Duration::from_millis(ON_FN_TIME_LIMIT) {
				return Err(RuntimeError::ExceededTimeLimit);
			}
			Ok(match &expr.ty {
				ExprType::LiteralExpr{
					expr,
					line: _,
					col: _,
				} => {
					match expr {
						LiteralExpr::TrueExpr => GrugValue{bool: 1},
						LiteralExpr::FalseExpr => GrugValue{bool: 0},
						LiteralExpr::StringExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::ResourceExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::EntityExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::NumberExpr {
							value,
							string: _,
						} => GrugValue{number: *value},
						LiteralExpr::IdentifierExpr{
							name,
						} => {
							if let Some(var) = call_stack.get_local_variable(name) {
								*var
							} else {
								entity.get_global_variable(name)
									.expect("could not find variable")
									.get()
							}
						},
					}
				},
				ExprType::UnaryExpr{
					operator,
					expr,
				} => {
					let mut value = self.run_expr(call_stack, state, file, entity, &expr)?;
					match (operator, &expr.result_ty) {
						(UnaryOperator::Not, Some(GrugType::Bool)) => unsafe{value.bool = (value.bool == 0) as u8},
						(UnaryOperator::Minus, Some(GrugType::Number)) => unsafe{value.number = -value.number},
						_ => unreachable!(),
					}
					value
				}
				ExprType::BinaryExpr{
					operands,
					operator,
				} => {
					let first_value = self.run_expr(call_stack, state, file, entity, &operands.0)?; 
					let mut second_value = || self.run_expr(call_stack, state, file, entity, &operands.1);
					debug_assert!(GrugType::match_non_exact(operands.0.result_ty.as_ref().unwrap(), operands.1.result_ty.as_ref().unwrap()));
					// debug_assert!(operands.0.result_ty == operands.1.result_ty || matches!((&operands.0.result_ty, &operands.1.result_ty), (Some(GrugType::Id{custom_name: None}), Some(GrugType::Id{..})) | (Some(GrugType::Id{..}), Some(GrugType::Id{custom_name: None}))));
					match (operator, &operands.0.result_ty) {
						(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{first_value.bool | second_value()?.bool}},
						(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(first_value.bool != 0 && second_value()?.bool != 0) as u8}},
						(BinaryOperator::DoubleEquals,   Some(ty)              )  => {
							let value = match ty {
								GrugType::Bool => !unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)},
								GrugType::Number => unsafe{first_value.number == second_value()?.number},
								GrugType::Id{..} => unsafe{first_value.id == second_value()?.id},
								GrugType::String => {
									unsafe {CStr::from_ptr(first_value.string)}.eq(unsafe{CStr::from_ptr(second_value()?.string)})
								},
								_ => unreachable!(),
							};
							GrugValue{bool: value as u8}
						},
						(BinaryOperator::NotEquals,      Some(ty)              )  => {
							let value = match ty {
								GrugType::Bool => unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)}
								GrugType::Number => unsafe{first_value.number != second_value()?.number}
								GrugType::Id{..} => unsafe{first_value.id != second_value()?.id}
								GrugType::String => {
									!unsafe {CStr::from_ptr(first_value.string)}.eq(unsafe{CStr::from_ptr(second_value()?.string)})
								}
								_ => unreachable!(),
							};
							GrugValue{bool: value as u8}
						},
						(BinaryOperator::Greater,        Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number > second_value()?.number} as u8},
						(BinaryOperator::GreaterEquals,  Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number >= second_value()?.number} as u8},
						(BinaryOperator::Less,           Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number < second_value()?.number} as u8},
						(BinaryOperator::LessEquals,     Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number <= second_value()?.number} as u8},
						(BinaryOperator::Plus,           Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number + second_value()?.number}},
						(BinaryOperator::Minus,          Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number - second_value()?.number}},
						(BinaryOperator::Multiply,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number * second_value()?.number}},
						(BinaryOperator::Division,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number / second_value()?.number}},
						(BinaryOperator::Remainder,      Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number % second_value()?.number}},
						_ => unreachable!(),
					}
				}
				ExprType::CallExpr{
					function_name,
					arguments,
					line: _,
					col: _,
				} if function_name.starts_with("helper_") => {
					let values = arguments.iter().map(|argument| self.run_expr(call_stack, state, file, entity, argument)).collect::<Result<Vec<_>, _>>()?;
					for helper_fn in &file.file.helper_functions {
						if helper_fn.name != *function_name {
							continue;
						}
						return Ok(self.run_function(call_stack, state, file, entity, &*helper_fn.arguments, &values, &*helper_fn.body_statements)?);
					}
					unreachable!("helper function not found");
				}
				ExprType::CallExpr{
					function_name,
					arguments,
					line: _,
					col: _,
				} => {
					let values = arguments.iter().map(|argument| self.run_expr(call_stack, state, file, entity, argument)).collect::<Result<Vec<_>, _>>()?;
					let game_fn = state.game_functions.get(&**function_name).expect("can't find game function");
					let return_ty = &state.mod_api.game_functions().get(function_name).unwrap().return_ty;
					let ret_val = match (values.len(), return_ty) {
						(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
						(0, _             ) => unsafe{(game_fn.value_argless)()},
						(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
						(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
					};
					if let Some(_) = state.error.get() {
						return Err(RuntimeError::GameFunctionError);
					}
					ret_val
				}
				ExprType::ParenthesizedExpr{
					expr,
					line: _,
					col: _,
				} => {
					self.run_expr(call_stack, state, file, entity, expr)?
				}
			})
		}

		pub(crate) fn init_global_exprs(&self, state: &GrugState, entity: &mut GrugEntityData, expr: &Expr) -> Result<GrugValue, RuntimeError> {
			if Instant::elapsed(&state.call_start_time.get()) > Duration::from_millis(ON_FN_TIME_LIMIT) {
				return Err(RuntimeError::ExceededTimeLimit);
			}
			Ok(match &expr.ty {
				ExprType::LiteralExpr{
					expr,
					line: _,
					col: _,
				} => {
					match expr {
						LiteralExpr::TrueExpr => GrugValue{bool: 1},
						LiteralExpr::FalseExpr => GrugValue{bool: 0},
						LiteralExpr::StringExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::ResourceExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::EntityExpr{
							value
						} => GrugValue{string: value.as_ptr().cast()},
						LiteralExpr::NumberExpr {
							value,
							string: _,
						} => GrugValue{number: *value},
						LiteralExpr::IdentifierExpr {
							name,
						} => {
							entity.global_variables.get(name)
								.expect("variable not found")
								.get()
						}
					}
				},
				ExprType::UnaryExpr{
					operator,
					expr,
				} => {
					let mut value = self.init_global_exprs(state, entity, &expr)?;
					match (operator, &expr.result_ty) {
						(UnaryOperator::Not, Some(GrugType::Bool)) => unsafe{value.bool = (value.bool == 0) as u8},
						(UnaryOperator::Minus, Some(GrugType::Number)) => unsafe{value.number = -value.number},
						_ => unreachable!(),
					}
					value
				}
				ExprType::BinaryExpr{
					operands,
					operator,
				} => {
					let first_value = self.init_global_exprs(state, entity, &operands.0)?; 
					let mut second_value = || self.init_global_exprs(state, entity, &operands.1);
					debug_assert!(GrugType::match_non_exact(operands.0.result_ty.as_ref().unwrap(), operands.1.result_ty.as_ref().unwrap()));
					// debug_assert!(operands.0.result_ty == operands.1.result_ty || matches!((&operands.0.result_ty, &operands.1.result_ty), (Some(GrugType::Id{custom_name: None}), Some(GrugType::Id{..})) | (Some(GrugType::Id{..}), Some(GrugType::Id{custom_name: None}))));
					match (operator, &operands.0.result_ty) {
						(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{first_value.bool | second_value()?.bool}},
						(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(first_value.bool != 0 && second_value()?.bool != 0) as u8}},
						(BinaryOperator::DoubleEquals,   Some(ty)              )  => {
							let value = match ty {
								GrugType::Bool => !unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)},
								GrugType::Number => unsafe{first_value.number == second_value()?.number},
								GrugType::Id{..} => unsafe{first_value.id == second_value()?.id},
								GrugType::String => {
									unsafe {CStr::from_ptr(first_value.string)}.eq(unsafe{CStr::from_ptr(second_value()?.string)})
								},
								_ => unreachable!(),
							};
							GrugValue{bool: value as u8}
						},
						(BinaryOperator::NotEquals,      Some(ty)              )  => {
							let value = match ty {
								GrugType::Bool => unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)}
								GrugType::Number => unsafe{first_value.number != second_value()?.number}
								GrugType::Id{..} => unsafe{first_value.id != second_value()?.id}
								GrugType::String => {
									!unsafe {CStr::from_ptr(first_value.string)}.eq(unsafe{CStr::from_ptr(second_value()?.string)})
								}
								_ => unreachable!(),
							};
							GrugValue{bool: value as u8}
						},
						(BinaryOperator::Greater,        Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number > second_value()?.number} as u8},
						(BinaryOperator::GreaterEquals,  Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number >= second_value()?.number} as u8},
						(BinaryOperator::Less,           Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number < second_value()?.number} as u8},
						(BinaryOperator::LessEquals,     Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number <= second_value()?.number} as u8},
						(BinaryOperator::Plus,           Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number + second_value()?.number}},
						(BinaryOperator::Minus,          Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number - second_value()?.number}},
						(BinaryOperator::Multiply,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number * second_value()?.number}},
						(BinaryOperator::Division,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number / second_value()?.number}},
						(BinaryOperator::Remainder,      Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number % second_value()?.number}},
						_ => unreachable!(),
					}
				}
				ExprType::CallExpr{
					function_name,
					arguments,
					line: _,
					col: _,
				} => {
					debug_assert!(!function_name.starts_with("on_") && !function_name.starts_with("helper_"));
					let values = arguments.iter().map(|argument| self.init_global_exprs(state, entity, argument)).collect::<Result<Vec<_>, _>>()?;
					let game_fn = state.game_functions.get(&**function_name).expect("can't find game function");
					let return_ty = &state.mod_api.game_functions().get(function_name).unwrap().return_ty;
					let ret_val = match (values.len(), return_ty) {
						(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
						(0, _             ) => unsafe{(game_fn.value_argless)()},
						(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
						(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
					};
					if let Some(_) = state.error.get() {
						return Err(RuntimeError::GameFunctionError)
					}
					ret_val
				}
				ExprType::ParenthesizedExpr{
					expr,
					line: _,
					col: _,
				} => {
					self.init_global_exprs(state, entity, expr)?
				}
			})
		}

		fn init_global_variables(&self, state: &GrugState, entity: &mut GrugEntityData, globals: &[GlobalVariable]) -> Result<(), RuntimeError> {
			state.call_start_time.set(Instant::now());
			globals.iter().map(|variable| {
				let value = self.init_global_exprs(state, entity, &variable.assignment_expr)?;
				entity.global_variables.insert(Arc::clone(&variable.name), Cell::new(value));
				Ok(())
			}).collect::<Result<Vec<_>, _>>()?;
			Ok(())
		}
	}

	unsafe impl Backend for Interpreter {
		fn insert_file(&self, path: &str, file: GrugFile) -> GrugScriptId {
			match self.file_id_map.get(path) {
				Some(id) => {	
					let _compiled_file = self.files.get(id)
						.expect("id exists in file_id_map so it must exist in files");
					
					todo!();
				},
				None => {
					let next_id = self.get_next_script_id();
					self.file_id_map.try_insert(String::from(path), next_id).unwrap();
					self.files.try_insert(next_id, CompiledFile::new(file)).unwrap();
					next_id
				}
			}
		}

		fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> Result<(), RuntimeError> {
			let file = self.files.get(&entity.file_id)
				.expect("file already compiled");

			let mut data = GrugEntityData {
				global_variables: HashMap::from([(Arc::from("me"), Cell::new(GrugValue{id:entity.id}))]),
			};
			self.init_global_variables(state, &mut data, &file.file.global_variables)?;

			let data = unsafe{file.data.insert(data)};
			file.entities.borrow_mut().push(NonNull::from_ref(entity));
			entity.members.set(data.as_ptr());

			Ok(())
		}

		fn clear_entities(&mut self) {
			self.files.iter_mut().for_each(|(_, file)| {
				file.entities.get_mut().clear();
			});
		}

		fn destroy_entity_data(&self, entity: &GrugEntity) -> bool {
			let file = self.files.get(&entity.file_id)
				.expect("file compiled");
			let Some((i, _)) = file.entities.borrow().iter().enumerate().find(|(_, en)| std::ptr::eq(en.as_ptr().cast_const(), entity)) else {
				// not found
				return false;
			};
			file.entities.borrow_mut().swap_remove(i);
			return true;
		}

		unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError> {
			let file = &self.files.get(&entity.file_id)
				.expect("file already created");

			let Some(on_function) = &file.file.on_functions[on_fn_id as usize] else {
				panic!("function not available");
			};

			let values = if on_function.arguments.len() == 0 {
				&[]
			} else {
				unsafe{std::slice::from_raw_parts(values, on_function.arguments.len())}
			};

			state.call_start_time.set(Instant::now());
			self.run_function(
				&mut CallStack::new(),
				state,
				file,
				unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
				&on_function.arguments, 
				values,
				&on_function.body_statements
			)?;
			Ok(())
		}

		fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError> {
			let file = &self.files.get(&entity.file_id)
				.expect("file already created");

			let Some(on_function) = &file.file.on_functions[on_fn_id as usize] else {
				panic!("function not available");
			};

			state.call_start_time.set(Instant::now());
			self.run_function(
				&mut CallStack::new(),
				state,
				file,
				unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
				&on_function.arguments, 
				values,
				&on_function.body_statements
			)?;
			Ok(())
		}
	}
}
pub use interpreter::Interpreter;

pub unsafe trait Backend {
	/// The AST of a typechecked grug file is provided to let the backend do
	/// further transforms and lower to bytecode or even machine code
	/// 
	/// Each path must be associated with a single GrugScriptId. If the same
	/// path is provided again, the old file must be replaced and the member
	/// data of any entities created from the old file must be regenerated.
	/// Importantly, the GrugScriptId associated with the new file must be the
	/// same as the only associated with the old file.
	///
	fn insert_file(&self, path: &str, file: GrugFile) -> GrugScriptId;
	/// Initialize the member data of the newly created entity. When this
	/// function is called, the member field of `entity` points to garbage and
	/// must not be deinitialized. The GrugScriptId to be used is obtained from
	/// the file_id member of `entity`. 
	///
	/// `entity` is pinned until it is deinitialized by a call to
	/// `destroy_entity_data` or `insert_file` with the same path as its
	/// current GrugScriptId. The reference must be stored as a raw pointer
	/// within self so that it can be used during `destroy_entity_data` to
	/// check for pointer equality. 
	/// It is safe to use that pointer as a &GrugEntity in the meantime.
	fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> Result<(), RuntimeError>;
	/// Deinitialize all the data associated with all entities. The pointers
	/// stored during `init_entity` must be used to get access to the entity data.
	/// The entities can only be accessed as a &GrugEntity even self is available with an exclusive reference
	fn clear_entities(&mut self);
	/// Deinitialize the data associated with `entity`. 
	///
	/// # IMPORTANT!!!!
	/// Before deinitializing the data, ensure that the address of `entity`
	/// matches the address of a pointer stored during a previous call to
	/// `init_entity`. If `entity` does not match any stored pointer, the data
	/// MUST NOT be deinitialized and the return value MUST BE `false`.
	///
	///	If true is returned, the data should be deinitialized and the pointer
	///	MUST be removed from storage.
	///
	/// It is safe to never deinitialize the data and return false everytime. 
	fn destroy_entity_data(&self, entity: &GrugEntity) -> bool;

	/// Run the on function with id `on_fn_id` of the script associated with `entity`.
	/// The id of an on_ function is based on its order within mod_Api.json. 
	///
	/// GrugFile.on_function stores the on_ function data based on the order
	/// within mod_api.json. The backend is responsible for preserving the
	/// mapping from id to on_ function code after all required transformations
	///
	/// # SAFETY: `values` must point to an array of GrugValues of at least as
	/// many elements as the number of arguments to the on_ function
	///
	/// If the number of arguments is 0, then `values` is allowed to be null
	unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError>;
	/// Run the on function with id `on_fn_id` of the script associated with `entity`.
	/// The id of an on_ function is based on its order within mod_Api.json. 
	///
	/// GrugFile.on_function stores the on_ function data based on the order
	/// within mod_api.json. The backend is responsible for preserving the
	/// mapping from id to on_ function code after all required transformations
	///
	/// # Panics: The length of `values` must exactly match the number of
	/// expected arguments to the on_ function
	fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError>;
}

pub struct ErasedBackend {
	pub data: NonNull<()>,
	pub vtable: &'static BackendVTable,
}

pub struct BackendVTable {
	/// SAFETY: `path` must be a utf-8 buffer that is valid to read for atleast `path_len`
	pub(crate) insert_file         : unsafe fn(data: NonNull<()>, path: *const u8, path_len: usize, file: GrugFile) -> GrugScriptId,
	pub(crate) init_entity         : fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> Result<(), RuntimeError>,
	pub(crate) clear_entities      : fn(data: NonNull<()>),
	pub(crate) destroy_entity_data : fn(data: NonNull<()>, entity: &GrugEntity) -> bool,
	/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
	pub(crate) call_on_function_raw: unsafe fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError>,
	pub(crate) call_on_function    : fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError>,
	// destroys the resources owned by the backend
	pub(crate) drop                : fn(data: NonNull<()>),
}

impl ErasedBackend {
	pub fn insert_file(&self, path: &str, file: GrugFile) -> GrugScriptId {
		unsafe{(self.vtable.insert_file)(self.data, path.as_ptr(), path.len(), file)}
	}
	pub fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> Result<(), RuntimeError> {
		(self.vtable.init_entity)(self.data, state, entity)
	}
	pub fn clear_entities(&mut self) {
		(self.vtable.clear_entities)(self.data)
	}
	pub fn destroy_entity_data(&self, entity: &GrugEntity) -> bool {
		(self.vtable.destroy_entity_data)(self.data, entity)
	}
	pub unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError>{
		unsafe{(self.vtable.call_on_function_raw)(self.data, state, entity, on_fn_id, values)}
	}
	pub fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError> {
		(self.vtable.call_on_function)(self.data, state, entity, on_fn_id, values)
	}
}

impl Drop for ErasedBackend {
	fn drop (&mut self) {
		(self.vtable.drop)(self.data)
	}
}

impl<T: Backend> From<T> for ErasedBackend {
	fn from(other: T) -> Self {
		unsafe fn insert_file<T: Backend>(data: NonNull<()>, path: *const u8, path_len: usize, file: GrugFile) -> GrugScriptId {
			T::insert_file(
				unsafe{data.cast::<T>().as_ref()},
				unsafe{std::str::from_utf8_unchecked(std::slice::from_raw_parts(path, path_len))},
				file
			)
		}
		fn init_entity<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> Result<(), RuntimeError> {
			T::init_entity(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity
			)
		}

		fn clear_entities<T: Backend>(data: NonNull<()>) {
			T::clear_entities(
				unsafe{data.cast::<T>().as_mut()},
			)
		}
		fn destroy_entity_data<T: Backend>(data: NonNull<()>, entity: &GrugEntity) -> bool {
			T::destroy_entity_data(
				unsafe{data.cast::<T>().as_ref()},
				entity
			)
		}
		/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
		unsafe fn call_on_function_raw<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError> {
			unsafe{T::call_on_function_raw(
				data.cast::<T>().as_ref(),
				state, 
				entity,
				on_fn_id,
				values
			)}
		}
		fn call_on_function<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError> {
			T::call_on_function(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity,
				on_fn_id,
				values
			)
		}
		// destroys the resources owned by the backend
		fn drop<T: Backend>(data: NonNull<()>) {
			_ = unsafe{Box::from_raw(data.cast::<T>().as_ptr())};
		}

		Self {
			data: unsafe{NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>()},
			vtable: &BackendVTable {
				insert_file         : insert_file::<T>,
				init_entity         : init_entity::<T>,
				clear_entities      : clear_entities::<T>,
				destroy_entity_data : destroy_entity_data::<T>,
				call_on_function_raw: call_on_function_raw::<T>,
				call_on_function    : call_on_function::<T>,
				drop                : drop::<T>,
			}
		}
	}
}
