use crate::mod_api::{ModApi, get_mod_api, ModApiError};
use crate::error::GrugError;
use crate::backend::{GrugEntity, GrugFile};
use crate::types::{GlobalStatement, GrugValue, Expr, ExprType, LiteralExpr, UnaryOperator, BinaryOperator, GrugType, Argument, Statement};

use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Instant, Duration};

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
	pub(crate) next_id: u64,
	pub(crate) game_functions: HashMap<&'static str, GameFnPtr>,

	// should be moved into backend later
	local_variables: Vec<Vec<HashMap<Arc<str>, GrugValue>>>,
	current_fn_call_depth: usize,
	call_start_time: Instant,
}

// const ON_FN_TIME_LIMIT: u64 = 10; // ms
const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D, game_functions: HashMap<&'static str, GameFnPtr>) -> Result<Self, GrugError<'a>> {
		let mod_api_text = std::fs::read_to_string(mod_api_path).unwrap();
		let mod_api = get_mod_api(&mod_api_text)?;

		for (game_fn_name, game_fn) in mod_api.game_functions() {
			if let None = game_functions.get(&**game_fn_name) {
				Err(ModApiError::GameFnNotProvided{
					game_fn_name: String::from(&**game_fn_name),
				})?;
			}
		}
		
		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_id: 0,
			game_functions,
			local_variables: Vec::new(),
			current_fn_call_depth: 0,
			call_start_time: Instant::now()
		})
	}

	pub fn create_entity(&mut self, file: &Arc<GrugFile>) -> Result<GrugEntity, RuntimeError> {
		let me_id = self.get_id();
		let mut global_variables = self.init_global_variables(&file.global_variables)?;
		global_variables.insert(Arc::from("me"), GrugValue{id: me_id});
		Ok(GrugEntity {
			id: me_id,
			global_variables,
			file: Arc::clone(file),
		})
	}

	pub fn get_id(&mut self) -> u64 {
		let next_id = self.next_id;
		self.next_id += 1;
		next_id
	}

	pub unsafe fn set_next_id(&mut self, next_id: u64) {
		self.next_id = next_id;
	}

	fn init_global_variables(&mut self, globals: &[GlobalStatement]) -> Result<HashMap<Arc<str>, GrugValue>, RuntimeError> {
		self.current_fn_call_depth += 1;
		self.call_start_time = Instant::now();
		let ret_val = globals.iter().map(|statement| {
			let GlobalStatement::GlobalVariableStatement{name, assignment_expr, ..} = statement else {
				unreachable!();
			};
			let value = self.init_global_exprs(assignment_expr)?;
			Ok((Arc::clone(name), value))
		}).collect::<Result<_, _>>();
		self.current_fn_call_depth -= 1;
		ret_val
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
	pub unsafe fn call_on_function_raw(&mut self, entity: &mut GrugEntity, function_name: &str, values: *const GrugValue) -> Result<(), RuntimeError> {
		let file = Arc::clone(&entity.file);
		for on_function in &file.on_functions {
			let GlobalStatement::GlobalOnFunction{name, arguments, body_statements, calls_helper_fn, has_while_loop} = on_function else {
				unreachable!();
			};
			if &**name != function_name {
				continue;
			}
			debug_assert!(self.local_variables.len() == 0);
			let values = if arguments.len() == 0 {
				&[]
			} else {
				unsafe{std::slice::from_raw_parts(values, arguments.len())}
			};
			self.run_function(
				entity, 
				arguments, 
				values,
				body_statements
			)?;
		}
		Ok(())
	}

	pub fn call_on_function(&mut self, entity: &mut GrugEntity, function_name: &str, values: &[GrugValue]) -> Result<(), RuntimeError> {
		let file = Arc::clone(&entity.file);
		for on_function in &file.on_functions {
			let GlobalStatement::GlobalOnFunction{name, arguments, body_statements, calls_helper_fn, has_while_loop} = on_function else {
				unreachable!();
			};
			if &**name != function_name {
				continue;
			}
			debug_assert!(self.local_variables.len() == 0);
			self.run_function(entity, arguments, values, body_statements)?;
			break;
		}
		Ok(())
	}

	fn run_function(&mut self, entity: &mut GrugEntity, arguments: &[Argument], values: &[GrugValue], statements: &[Statement]) -> Result<GrugValue, RuntimeError> {
		if arguments.len() != values.len() {
			return Err(RuntimeError::FunctionArgumentCountMismatch {
				expected: arguments.len(),
				got: values.len(),
			});
		}
		self.push_stack_frame();
		self.push_scope();

		for (argument, value) in arguments.iter().zip(values) {
			self.add_local_variable(Arc::clone(&argument.name), *value);
		}
		let value = self.run_statements(entity, statements)?;
		let value = match value {
			GrugControlFlow::Return(value) => value,
			GrugControlFlow::None          => GrugValue{void: ()},
			GrugControlFlow::Break         => unreachable!(),
			GrugControlFlow::Continue      => unreachable!(),
		};

		self.pop_scope();
		self.pop_stack_frame();
		Ok(value)
	}

	fn run_statements(&mut self, entity: &mut GrugEntity, statements: &[Statement]) -> Result<GrugControlFlow, RuntimeError> {
		self.push_scope();
		let mut ret_val = GrugControlFlow::None;
		'outer: for statement in statements {
			match statement {
				Statement::VariableStatement{
					name,
					ty,
					assignment_expr,
				} => {
					let assignment_expr = self.run_expr(entity, assignment_expr)?;
					if let Some(ty) = ty {
						self.add_local_variable(Arc::clone(name), assignment_expr);
					} else {
						*(if let Some(var) = self.get_local_variable(&**name) {
							var
						} else if let Some(var) = entity.get_global_variable(&**name) {
							var
						} else {
							panic!("variable not found");
						}) = assignment_expr;
					}
				},
				Statement::CallStatement {
					expr
				} => {
					self.run_expr(entity, expr)?;
				},
				Statement::IfStatement{
					condition,
					if_statements,
					else_if_statements,
					else_statements,
				} => {
					let condition = unsafe{self.run_expr(entity, condition)?.bool};
					// if statement
					if condition != 0 {
						let control_flow = self.run_statements(entity, if_statements)?;
						if let GrugControlFlow::None = control_flow {
							continue;
						} else {
							ret_val = control_flow;
							break 'outer;
						} 
					} else {
						// else if statements
						for (condition, else_if_statements) in else_if_statements {
							let condition = unsafe{self.run_expr(entity, condition)?.bool};
							if condition != 0 {
								let control_flow = self.run_statements(entity, else_if_statements)?;
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
						let control_flow = self.run_statements(entity, else_statements)?;
						if let GrugControlFlow::None = control_flow {
							continue;
						} else {
							ret_val = control_flow;
							break 'outer;
						} 
					}
				},
				Statement::ReturnStatement{
					expr,
				} => {
					if let Some(expr) = expr {
						ret_val = GrugControlFlow::Return(self.run_expr(entity, expr)?);
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
						let condition = unsafe{self.run_expr(entity, condition)?.bool};
						if condition == 0 {
							break;
						}
						match self.run_statements(entity, statements)? {
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
					value,
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
		self.pop_scope();
		Ok(ret_val)
	}

	fn init_global_exprs(&mut self, expr: &Expr) -> Result<GrugValue, RuntimeError> {
		if Instant::elapsed(&self.call_start_time) > Duration::from_millis(ON_FN_TIME_LIMIT) {
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
						value,
					} => todo!("strings need to be null terminated"),
					LiteralExpr::NumberExpr {
						value,
					} => GrugValue{number: *value},
					_ => unreachable!(),
				}
			},
			ExprType::UnaryExpr{
				operator,
				expr,
			} => {
				let mut value = self.init_global_exprs(&expr)?;
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
				let values = (self.init_global_exprs(&operands.0)?, self.init_global_exprs(&operands.1)?);
				debug_assert!(operands.0.result_ty == operands.1.result_ty);
				match (operator, &operands.0.result_ty) {
					(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{values.0.bool | values.1.bool}},
					(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(values.0.bool == 0 && values.1.bool == 0) as u8}},
					(BinaryOperator::DoubleEquals,   _                     )  => unimplemented!(),
					(BinaryOperator::NotEquals,      _                     )  => unimplemented!(),
					(BinaryOperator::Greater,        Some(GrugType::Number))  => GrugValue{bool: unsafe{values.0.number > values.1.number} as u8},
					(BinaryOperator::GreaterEquals,  Some(GrugType::Number))  => GrugValue{bool: unsafe{values.0.number >= values.1.number} as u8},
					(BinaryOperator::Less,           Some(GrugType::Number))  => GrugValue{bool: unsafe{values.0.number < values.1.number} as u8},
					(BinaryOperator::LessEquals,     Some(GrugType::Number))  => GrugValue{bool: unsafe{values.0.number <= values.1.number} as u8},
					(BinaryOperator::Plus,           Some(GrugType::Number))  => GrugValue{number: unsafe{values.0.number + values.1.number}},
					(BinaryOperator::Minus,          Some(GrugType::Number))  => GrugValue{number: unsafe{values.0.number - values.1.number}},
					(BinaryOperator::Multiply,       Some(GrugType::Number))  => GrugValue{number: unsafe{values.0.number * values.1.number}},
					(BinaryOperator::Division,       Some(GrugType::Number))  => GrugValue{number: unsafe{values.0.number / values.1.number}},
					(BinaryOperator::Remainder,      Some(GrugType::Number))  => GrugValue{number: unsafe{values.0.number % values.1.number}},
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
				let values = arguments.iter().map(|argument| self.init_global_exprs(argument)).collect::<Result<Vec<_>, _>>()?;
				let game_fn = self.game_functions.get(&**function_name).expect("can't find game function");
				let return_ty = &self.mod_api.game_functions().get(function_name).unwrap().return_ty;
				match (values.len(), return_ty) {
					// (0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
					(0, _             ) => unsafe{(game_fn.value_argless)()},
					// (_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
					(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
					_ => unreachable!(),
				}
			}
			ExprType::ParenthesizedExpr{
				expr,
				line: _,
				col: _,
			} => {
				self.init_global_exprs(expr)?
			}
		})
	}

	fn run_expr(&mut self, entity: &mut GrugEntity, expr: &Expr) -> Result<GrugValue, RuntimeError> {
		if Instant::elapsed(&self.call_start_time) > Duration::from_millis(ON_FN_TIME_LIMIT) {
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
						value,
					} => todo!("strings need to be null terminated"),
					LiteralExpr::NumberExpr {
						value,
					} => GrugValue{number: *value},
					LiteralExpr::ResourceExpr{
						value,
					} => unimplemented!(),
					LiteralExpr::EntityExpr{
						value,
					} => unimplemented!(),
					LiteralExpr::IdentifierExpr{
						name,
					} => {
						*self.get_local_variable(name).or_else(|| entity.get_global_variable(name))
							.expect("could not find global variable")
					},
				}
			},
			ExprType::UnaryExpr{
				operator,
				expr,
			} => {
				let mut value = self.run_expr(entity, &expr)?;
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
				let first_value = self.run_expr(entity, &operands.0)?; 
				let mut second_value = || self.run_expr(entity, &operands.1);
				debug_assert!(operands.0.result_ty == operands.1.result_ty);
				match (operator, &operands.0.result_ty) {
					(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{first_value.bool | second_value()?.bool}},
					(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(first_value.bool != 0 && second_value()?.bool != 0) as u8}},
					(BinaryOperator::DoubleEquals,   _                     )  => unimplemented!(),
					(BinaryOperator::NotEquals,      _                     )  => unimplemented!(),
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
				let values = arguments.iter().map(|argument| self.run_expr(entity, argument)).collect::<Result<Vec<_>, _>>()?;
				for helper_fn in &file.helper_functions {
					let GlobalStatement::GlobalHelperFunction{
						name,
						arguments,
						body_statements,
						return_ty,
						..
					} = helper_fn else {
						unreachable!();
					};
					if name != function_name {
						continue;
					}
					return Ok(self.run_function(entity, arguments, &values, body_statements)?);
				}
				unreachable!("helper function not found");
			}
			ExprType::CallExpr{
				function_name,
				arguments,
				line: _,
				col: _,
			} => {
				let values = arguments.iter().map(|argument| self.run_expr(entity, argument)).collect::<Result<Vec<_>, _>>()?;
				let game_fn = self.game_functions.get(&**function_name).expect("can't find game function");
				let return_ty = &self.mod_api.game_functions().get(function_name).unwrap().return_ty;
				match (values.len(), return_ty) {
					(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
					(0, _             ) => unsafe{(game_fn.value_argless)()},
					(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_ptr()); GrugValue{void: ()}},
					(_, _             ) => unsafe{(game_fn.value)(values.as_ptr())},
				}
			}
			ExprType::ParenthesizedExpr{
				expr,
				line: _,
				col: _,
			} => {
				self.run_expr(entity, expr)?
			}
		})
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

#[derive(Debug)]
pub enum RuntimeError {
	ExceededTimeLimit,
	FunctionArgumentCountMismatch {
		expected: usize,
		got: usize,
	},
}
