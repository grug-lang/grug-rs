use crate::mod_api::{ModApi, get_mod_api, ModApiError};
use crate::error::GrugError;
use crate::backend::{GrugEntity, GrugFile, UninitGrugEntity};
use crate::types::{GlobalStatement, GrugValue, Expr, ExprType, LiteralExpr, UnaryOperator, BinaryOperator, GrugType, Argument, Statement, GlobalVariable, OnFunction, GrugId};

use std::cell::Cell;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::{Arc, atomic::{AtomicU64, Ordering}};
use std::time::{Instant, Duration};
use std::ffi::CStr;
use std::fmt::Write;

#[repr(C)]
pub union GameFnPtr {
	void: GameFnPtrVoid,
	void_argless: GameFnPtrVoidArgless,
	value: GameFnPtrValue,
	value_argless: GameFnPtrValueArgless,
}

mod from_impls {
	use super::*;
	impl From<GameFnPtrVoid> for GameFnPtr {
		fn from (value: GameFnPtrVoid) -> Self {
			Self {
				void: value,
			}
		}
	}

	impl From<GameFnPtrVoidArgless> for GameFnPtr {
		fn from (value: GameFnPtrVoidArgless) -> Self {
			Self {
				void_argless: value,
			}
		}
	}

	impl From<GameFnPtrValue> for GameFnPtr {
		fn from (value: GameFnPtrValue) -> Self {
			Self {
				value: value,
			}
		}
	}

	impl From<GameFnPtrValueArgless> for GameFnPtr {
		fn from (value: GameFnPtrValueArgless) -> Self {
			Self {
				value_argless: value,
			}
		}
	}
}

type GameFnPtrVoid = extern "C" fn (args: *const GrugValue);
type GameFnPtrVoidArgless = extern "C" fn ();
type GameFnPtrValue = extern "C" fn (args: *const GrugValue) -> GrugValue;
type GameFnPtrValueArgless = extern "C" fn () -> GrugValue;

pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
	pub(crate) next_id: AtomicU64,
	pub(crate) game_functions: HashMap<&'static str, GameFnPtr>,
	
	// should be moved into backend later
	call_start_time: Cell<Instant>,
	error: Cell<Option<&'static str>>,
	pub handled_error: Cell<bool>,
}

pub struct CallStack {
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

const ON_FN_TIME_LIMIT: u64 = 100; // ms
// const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

const MAX_RECURSION_LIMIT: usize = 100;

impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D, game_functions: HashMap<&'static str, GameFnPtr>) -> Result<Self, GrugError<'a>> {
		let mod_api_text = std::fs::read_to_string(mod_api_path).unwrap();
		let mod_api = get_mod_api(&mod_api_text)?;

		for game_fn_name in mod_api.game_functions().keys() {
			if let None = game_functions.get(&**game_fn_name) {
				Err(ModApiError::GameFnNotProvided{
					game_fn_name: String::from(&**game_fn_name),
				})?;
			}
		}
		
		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_id: AtomicU64::new(0),
			game_functions,
			call_start_time: Cell::new(Instant::now()),
			error: Cell::new(None),
			handled_error: Cell::new(false),
		})
	}

	pub fn create_entity(&self, file: &Arc<GrugFile>) -> Result<GrugEntity, RuntimeError> {
		let me_id = self.get_id();
		let mut temp_entity = UninitGrugEntity {
			id: me_id, 
			global_variables: HashMap::from([(Arc::from("me"), Cell::new(GrugValue{id:me_id}))]),
			file: Arc::clone(file),
		};
		self.init_global_variables(&mut temp_entity, &file.global_variables)?;
		
		Ok(GrugEntity {
			id: temp_entity.id,
			global_variables: temp_entity.global_variables,
			file: temp_entity.file,
		})
	}

	pub fn get_id(&self) -> GrugId {
		GrugId::new(self.next_id.fetch_add(1, Ordering::Relaxed))
	}

	pub unsafe fn set_next_id(&self, next_id: u64) {
		self.next_id.store(next_id, Ordering::Relaxed);
	}

	fn init_global_variables(&self, entity: &mut UninitGrugEntity, globals: &[GlobalVariable]) -> Result<(), RuntimeError> {
		self.call_start_time.set(Instant::now());
		globals.iter().map(|variable| {
			let value = self.init_global_exprs(entity, &variable.assignment_expr)?;
			entity.global_variables.insert(Arc::clone(&variable.name), Cell::new(value));
			Ok(())
		}).collect::<Result<Vec<_>, _>>()?;
		Ok(())
	}

	pub fn clear_error(&self) {
		self.error.set(None);
		self.handled_error.set(false);
	}

	pub fn set_error(&self, error: &'static str) {
		self.error.set(Some(error));
		self.handled_error.set(false);
	}

	pub fn set_handled_error(&self) {
		self.handled_error.set(true);
	}
}

enum GrugControlFlow {
	Return(GrugValue),
	Break,
	Continue,
	None,
}

// should be moved into backend later
impl GrugState {
	pub unsafe fn call_on_function_raw(&self, entity: &GrugEntity, function_name: &str, values: *const GrugValue) -> Result<(), RuntimeError> {
		let file = Arc::clone(&entity.file);
		for on_function in &file.on_functions {
			if &*on_function.name != function_name {
				continue;
			}
			let values = if on_function.arguments.len() == 0 {
				&[]
			} else {
				unsafe{std::slice::from_raw_parts(values, on_function.arguments.len())}
			};
			self.call_start_time.set(Instant::now());
			self.run_function(
				&mut CallStack::new(),
				entity, 
				&on_function.arguments, 
				values,
				&on_function.body_statements
			)?;
		}
		Ok(())
	}

	pub fn call_on_function(&self, entity: &GrugEntity, function_name: &str, values: &[GrugValue]) -> Result<(), RuntimeError> {
		let file = Arc::clone(&entity.file);
		for on_function in &file.on_functions {
			if &*on_function.name != function_name {
				continue;
			}
			self.call_start_time.set(Instant::now());
			self.run_function(
				&mut CallStack::new(), 
				entity, 
				&on_function.arguments, 
				values, 
				&on_function.body_statements
			)?;
			break;
		}
		Ok(())
	}

	fn run_function(&self, call_stack: &mut CallStack, entity: &GrugEntity, arguments: &[Argument], values: &[GrugValue], statements: &[Statement]) -> Result<GrugValue, RuntimeError> {
		if call_stack.local_variables.len() > MAX_RECURSION_LIMIT {
			return Err(RuntimeError::StackOverflow)
		}
		if arguments.len() != values.len() {
			return Err(RuntimeError::FunctionArgumentCountMismatch {
				expected: arguments.len(),
				got: values.len(),
			});
		}
		call_stack.push_stack_frame();
		call_stack.push_scope();

		for (argument, value) in arguments.iter().zip(values) {
			call_stack.add_local_variable(Arc::clone(&argument.name), *value);
		}
		let value = self.run_statements(call_stack, entity, statements)?;
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

	fn run_statements(&self, call_stack: &mut CallStack, entity: &GrugEntity, statements: &[Statement]) -> Result<GrugControlFlow, RuntimeError> {
		call_stack.push_scope();
		let mut ret_val = GrugControlFlow::None;
		'outer: for statement in statements {
			match statement {
				Statement::VariableStatement{
					name,
					ty,
					assignment_expr,
				} => {
					let assignment_expr = self.run_expr(call_stack, entity, assignment_expr)?;
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
					self.run_expr(call_stack, entity, expr)?;
				},
				Statement::IfStatement{
					condition,
					if_statements,
					else_if_statements,
					else_statements,
				} => {
					let condition = unsafe{self.run_expr(call_stack, entity, condition)?.bool};
					// if statement
					if condition != 0 {
						let control_flow = self.run_statements(call_stack, entity, if_statements)?;
						if let GrugControlFlow::None = control_flow {
							continue;
						} else {
							ret_val = control_flow;
							break 'outer;
						} 
					} else {
						// else if statements
						for (condition, else_if_statements) in else_if_statements {
							let condition = unsafe{self.run_expr(call_stack, entity, condition)?.bool};
							if condition != 0 {
								let control_flow = self.run_statements(call_stack, entity, else_if_statements)?;
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
							let control_flow = self.run_statements(call_stack, entity, else_statements)?;
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
						ret_val = GrugControlFlow::Return(self.run_expr(call_stack, entity, expr)?);
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
						let condition = unsafe{self.run_expr(call_stack, entity, condition)?.bool};
						if condition == 0 {
							break;
						}
						match self.run_statements(call_stack, entity, statements)? {
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

	fn init_global_exprs(&self, entity: &mut UninitGrugEntity, expr: &Expr) -> Result<GrugValue, RuntimeError> {
		if Instant::elapsed(&self.call_start_time.get()) > Duration::from_millis(ON_FN_TIME_LIMIT) {
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
				let mut value = self.init_global_exprs(entity, &expr)?;
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
				let first_value = self.init_global_exprs(entity, &operands.0)?; 
				let mut second_value = || self.init_global_exprs(entity, &operands.1);
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
				let values = arguments.iter().map(|argument| self.init_global_exprs(entity, argument)).collect::<Result<Vec<_>, _>>()?;
				let game_fn = self.game_functions.get(&**function_name).expect("can't find game function");
				let return_ty = &self.mod_api.game_functions().get(function_name).unwrap().return_ty;
				let ret_val = match (values.len(), return_ty) {
					(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
					(0, _             ) => unsafe{(game_fn.value_argless)()},
					(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
					(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
				};
				if let Some(err) = self.error.get() {
					return Err(RuntimeError::GameFunctionError{
						message: err,
					})
				}
				ret_val
			}
			ExprType::ParenthesizedExpr{
				expr,
				line: _,
				col: _,
			} => {
				self.init_global_exprs(entity, expr)?
			}
		})
	}

	fn run_expr(&self, call_stack: &mut CallStack, entity: &GrugEntity, expr: &Expr) -> Result<GrugValue, RuntimeError> {
		if Instant::elapsed(&self.call_start_time.get()) > Duration::from_millis(ON_FN_TIME_LIMIT) {
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
				let mut value = self.run_expr(call_stack, entity, &expr)?;
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
				let first_value = self.run_expr(call_stack, entity, &operands.0)?; 
				let mut second_value = || self.run_expr(call_stack, entity, &operands.1);
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
				let file = Arc::clone(&entity.file);
				let values = arguments.iter().map(|argument| self.run_expr(call_stack, entity, argument)).collect::<Result<Vec<_>, _>>()?;
				for helper_fn in &file.helper_functions {
					if helper_fn.name != *function_name {
						continue;
					}
					return Ok(self.run_function(call_stack, entity, &*helper_fn.arguments, &values, &*helper_fn.body_statements)?);
				}
				unreachable!("helper function not found");
			}
			ExprType::CallExpr{
				function_name,
				arguments,
				line: _,
				col: _,
			} => {
				let values = arguments.iter().map(|argument| self.run_expr(call_stack, entity, argument)).collect::<Result<Vec<_>, _>>()?;
				let game_fn = self.game_functions.get(&**function_name).expect("can't find game function");
				let return_ty = &self.mod_api.game_functions().get(function_name).unwrap().return_ty;
				let ret_val = match (values.len(), return_ty) {
					(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
					(0, _             ) => unsafe{(game_fn.value_argless)()},
					(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
					(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
				};
				if let Some(err) = self.error.get() {
					return Err(RuntimeError::GameFunctionError{
						message: err,
					})
				}
				ret_val
			}
			ExprType::ParenthesizedExpr{
				expr,
				line: _,
				col: _,
			} => {
				self.run_expr(call_stack, entity, expr)?
			}
		})
	}
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
	ExceededTimeLimit,
	StackOverflow,
	GameFunctionError{
		message: &'static str,
	},
	FunctionArgumentCountMismatch {
		expected: usize,
		got: usize,
	},
}

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::ExceededTimeLimit => write!(f, "Took longer than {} milliseconds to run", ON_FN_TIME_LIMIT),
			Self::StackOverflow => write!(f, "Stack overflow, so check for accidental infinite recursion"),
			Self::GameFunctionError{message} => write!(f, "{}", message),
			Self::FunctionArgumentCountMismatch {
				expected: _,
				got: _,
			} => write!(f, "{:?}", self),
		}
	}
}

