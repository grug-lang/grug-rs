use crate::mod_api::{ModApi, get_mod_api};
use crate::error::GrugError;
use crate::backend::{GrugEntity, GrugFile};
use crate::types::{GlobalStatement, GrugValue, Expr, ExprType, LiteralExpr, UnaryOperator, BinaryOperator, GrugType};

use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Instant, Duration};

#[repr(C)]
union GameFnPtr {
	void: GameFnPtrVoid,
	void_argless: GameFnPtrVoidArgless,
	value: GameFnPtrValue,
	value_argless: GameFnPtrValueArgless,
}

type GameFnPtrVoid = extern "C" fn (args: *mut GrugValue);
type GameFnPtrVoidArgless = extern "C" fn ();
type GameFnPtrValue = extern "C" fn (args: *mut GrugValue) -> GrugValue;
type GameFnPtrValueArgless = extern "C" fn () -> GrugValue;

pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
	pub(crate) next_id: u64,
	pub(crate) game_functions: HashMap<Arc<str>, GameFnPtr>,

	// should be moved into backend later
	local_variables: Vec<Vec<HashMap<Arc<str>, GrugValue>>>,
	current_fn_call_depth: usize,
	call_start_time: Instant,
}

const ON_FN_TIME_LIMIT: u64 = 10; // ms

impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D) -> Result<Self, GrugError<'a>> {
		let mod_api_text = std::fs::read_to_string(mod_api_path).unwrap();
		let mod_api = get_mod_api(&mod_api_text)?;
		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_id: 0,
			game_functions: HashMap::new(),
			local_variables: Vec::new(),
			current_fn_call_depth: 0,
			call_start_time: Instant::now()
		})
	}

	pub fn create_entity(&mut self, file: &GrugFile) -> Result<GrugEntity, RuntimeError> {
		let me_id = self.get_id();
		let globals = self.init_global_variables(&file.global_variables)?;
		todo!();
	}

	pub fn get_id(&mut self) -> u64 {
		let next_id = self.next_id;
		self.next_id += 1;
		next_id
	}

	fn init_global_variables(&mut self, globals: &[GlobalStatement]) -> Result<HashMap<Arc<str>, GrugValue>, RuntimeError> {
		self.current_fn_call_depth += 1;
		self.call_start_time = Instant::now();
		let ret_val = globals.iter().map(|statement| {
			let GlobalStatement::GlobalVariableStatement{name, assignment_expr, ..} = statement else {
				unreachable!();
			};
			let value = self.run_expr(&mut GrugEntity::dummy(), assignment_expr)?;
			Ok((Arc::clone(name), value))
		}).collect::<Result<_, _>>();
		self.current_fn_call_depth -= 1;
		ret_val
	}
}

// should be moved into backend later
impl GrugState {
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
				let values = (self.run_expr(entity, &operands.0)?, self.run_expr(entity, &operands.1)?);
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
				let mut values = arguments.iter().map(|argument| self.run_expr(entity, argument)).collect::<Result<Vec<_>, _>>()?;
				let game_fn = self.game_functions.get(function_name).expect("can't find game function");
				let return_ty = &self.mod_api.game_functions().get(function_name).unwrap().return_ty;
				match (values.len(), return_ty) {
					(0, GrugType::Void) => unsafe{(game_fn.void_argless)(); GrugValue{void: ()}},
					(0, _             ) => unsafe{(game_fn.value_argless)()},
					(_, GrugType::Void) => unsafe{(game_fn.void)(values.as_mut_ptr()); GrugValue{void: ()}},
					(_, _             ) => unsafe{(game_fn.value)(values.as_mut_ptr())},
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
	
	fn get_local_variable(&mut self, name: &str) -> Option<&mut GrugValue> {
		for scope in self.local_variables.last_mut()?{
			if let Some(val) = scope.get_mut(name) {
				return Some(val)
			}
		}
		None
	}
}

enum RuntimeError {
	ExceededTimeLimit,
}
