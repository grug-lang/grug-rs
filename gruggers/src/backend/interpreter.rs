#![allow(warnings)]
use crate::types::{GrugValue, GrugEntity, GrugFileId};
use crate::ast::{
	Argument, Statement, Expr, ExprData, MemberVariable,
	OnFunction, HelperFunction,
	// UnaryOperator, BinaryOperator, 
	GrugType, GrugAst,
};
use crate::xar::ErasedXar;
use crate::arena::Arena;
use crate::backend::Backend;
use crate::ntstring::{NTStr, NTStrPtr};

// use gruggers_core::runtime_error::{RuntimeError, ON_FN_TIME_LIMIT, MAX_RECURSION_LIMIT};
use gruggers_core::state::State;

use std::ptr::NonNull;
use std::sync::Arc;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::time::Instant;
use std::alloc::Layout;

use allocator_api2::boxed::Box;
use allocator_api2::vec::Vec;

fn copy_into_arena<'arena>(ast: &GrugAst<'_>, arena: &'arena Arena) -> GrugAst<'arena> {
	let mut members = Vec::with_capacity_in(ast.members.len(), arena);
	for member in ast.members {
		let name = copy_string(member.name, arena);
		let ty = copy_type(member.ty, arena);
		let assignment_expr = copy_expr(&member.assignment_expr, arena);
		members.push(MemberVariable {
			name,
			ty, 
			assignment_expr,
		});
	}

	let mut on_functions = Vec::with_capacity_in(ast.on_functions.len(), arena);
	for on_function in ast.on_functions {
		let Some(on_function) = on_function else {on_functions.push(None); continue;};
		let name = copy_string(on_function.name, arena);
		let mut arguments = Vec::with_capacity_in(on_function.arguments.len(), arena);
		for argument in on_function.arguments {
			arguments.push(Argument {
				name: copy_string(argument.name, arena),
				ty: copy_type(argument.ty, arena),
			});
		}
		
		let body_statements = copy_statements(on_function.body_statements, arena);
		on_functions.push(Some(&*Box::leak(Box::new_in(OnFunction{
			name, 
			arguments: arguments.leak(),
			body_statements,
		}, arena))));
	}

	let mut helper_functions = Vec::with_capacity_in(ast.helper_functions.len(), arena);
	for helper_function in ast.helper_functions {
		let name = copy_string(helper_function.name, arena);
		let return_type = copy_type(helper_function.return_type, arena);
		let mut arguments = Vec::with_capacity_in(helper_function.arguments.len(), arena);
		for argument in helper_function.arguments {
			arguments.push(Argument {
				name: copy_string(argument.name, arena),
				ty: copy_type(argument.ty, arena),
			});
		}
		
		let body_statements = copy_statements(helper_function.body_statements, arena);
		helper_functions.push(HelperFunction{
			name, 
			return_type,
			arguments: arguments.leak(),
			body_statements,
		});
	}

	GrugAst {
		members: members.leak(),
		on_functions: on_functions.leak(),
		helper_functions: helper_functions.leak(),
	}
}

fn copy_statements<'arena>(stmts: &[Statement<'_>], arena: &'arena Arena) -> &'arena mut [Statement<'arena>] {
	let mut vec = Vec::with_capacity_in(stmts.len(), arena);
	for stmt in stmts {
		let stmt = match stmt {
			Statement::Variable {
				name,
				ty,
				assignment_expr,
			} => Statement::Variable {
				name: copy_string(*name, arena),
				ty: ty.map(|ty| &*Box::leak(Box::new_in(copy_type(*ty, arena), arena))),
				assignment_expr : copy_expr(assignment_expr, arena),
			},
			Statement::Call(expr) => Statement::Call(copy_expr(expr, arena)),
			Statement::If {
				condition,
				is_chained,
				if_block,
				else_block,
			} => Statement::If {
				condition: copy_expr(condition, arena),
				is_chained: *is_chained,
				if_block: copy_statements(if_block, arena),
				else_block: copy_statements(else_block, arena),
			},
			Statement::While {
				condition,
				block,
			} => Statement::While {
				condition: copy_expr(condition, arena),
				block: copy_statements(block, arena),
			},
			Statement::Return {
				expr,
			} => Statement::Return {
				expr: expr.as_ref().map(|expr| Box::leak(Box::new_in(copy_expr(expr, arena), arena))),
			},
			Statement::Comment(string) => Statement::Comment(copy_string(*string, arena)),
			Statement::Break => Statement::Break,
			Statement::Continue => Statement::Continue,
			Statement::EmptyLine => Statement::EmptyLine,
		};
		vec.push(stmt);
	}
	vec.leak()
}

fn copy_expr<'arena>(expr: &Expr<'_>, arena: &'arena Arena) -> Expr<'arena> {
	let result_type = expr.result_type.map(|res| &*Box::leak(Box::new_in(copy_type(*res, arena), arena)));
	let data = match &expr.data {
		ExprData::True  => ExprData::True,
		ExprData::False => ExprData::False,
		ExprData::String(string) => ExprData::String(copy_string(*string, arena)),
		ExprData::Resource(string) => ExprData::Resource(copy_string(*string, arena)),
		ExprData::Entity(string) => ExprData::Entity(copy_string(*string, arena)),
		ExprData::Identifier(string) => ExprData::Identifier(copy_string(*string, arena)),
		ExprData::Number(number, string) => ExprData::Number(*number, copy_string(*string, arena)),
		ExprData::Unary {
			op,
			expr,
		} => {
			ExprData::Unary {
				op: *op,
				expr: Box::leak(Box::new_in(copy_expr(expr, arena), arena)),
			}
		},
		ExprData::Binary {
			op,
			left,
			right,
		} => {
			ExprData::Binary {
				op: *op,
				left: Box::leak(Box::new_in(copy_expr(left, arena), arena)),
				right: Box::leak(Box::new_in(copy_expr(right, arena), arena)),
			}
		},
		ExprData::Call {
			name,
			args,
			ptr,
		} => {
			ExprData::Call {
				name: copy_string(*name, arena),
				args: copy_exprs(args, arena),
				ptr: *ptr,
			}
		},
		ExprData::Parenthesized(expr) => ExprData::Parenthesized(Box::leak(Box::new_in(copy_expr(expr, arena), arena))),
	};
	Expr {
		result_type,
		data,
	}
}

fn copy_exprs<'arena>(exprs: &[Expr<'_>], arena: &'arena Arena) -> &'arena mut [Expr<'arena>] {
	let mut vec = Vec::with_capacity_in(exprs.len(), arena);
	exprs.iter().for_each(|expr| vec.push(copy_expr(expr, arena)));
	vec.leak()
}

fn copy_type<'arena>(ty: GrugType<'_>, arena: &'arena Arena) -> GrugType<'arena> {
	match ty {
		GrugType::Void => GrugType::Void,
		GrugType::Bool => GrugType::Bool,
		GrugType::Number => GrugType::Number,
		GrugType::String => GrugType::String,
		GrugType::Id{custom_name: None} => GrugType::Id{custom_name: None},
		GrugType::Entity{entity_type: None} => GrugType::Entity{entity_type: None},
		GrugType::Resource{extension} => GrugType::Resource{extension: copy_string(extension, arena)},
		GrugType::Id{custom_name: Some(custom_name)} => GrugType::Id{custom_name: Some(copy_string(custom_name, arena))},
		GrugType::Entity{entity_type: Some(entity_type)} => GrugType::Entity{entity_type: Some(copy_string(entity_type, arena))},
	}
}

fn copy_string<'arena>(string: NTStrPtr<'_>, arena: &'arena Arena) -> NTStrPtr<'arena> {
	Box::leak(NTStr::box_from_str_in(string.to_str(), arena)).as_ntstrptr()
}

struct GrugEntityData {
	pub(crate) global_variables: HashMap<Arc<str>, Cell<GrugValue>>,
}

impl GrugEntityData {
	pub(crate) fn get_global_variable(&self, name: &str) -> Option<&Cell<GrugValue>> {
		self.global_variables.get(name)
	}
}

struct CompiledFile {
	file: GrugAst<'static>,
	entities: RefCell<Vec<NonNull<GrugEntity>>>,
	data: ErasedXar,
	arena: Arena,
}

impl CompiledFile {
	fn new(file: GrugAst) -> Self {
		let arena = Arena::new();
		let file = unsafe{std::mem::transmute::<GrugAst<'_>, GrugAst<'static>>(copy_into_arena(&file, &arena))};
		Self {
			file: file,
			entities: RefCell::new(Vec::new()),
			data: ErasedXar::new(Layout::new::<GrugEntityData>()),
			arena,
		}
	}
}

pub struct Interpreter {
	files: RefCell<Vec<CompiledFile>>,
}

// struct CallStack {
// 	local_variables: Vec<Vec<HashMap<Arc<str>, GrugValue>>>,
// }

// impl CallStack {
// 	fn new() -> Self {
// 		Self {
// 			local_variables: Vec::new(),
// 		}
// 	}
	
// 	fn pop_scope(&mut self) {
// 		self.local_variables.last_mut()
// 			.expect("must already have a stack frame").pop()
// 			.expect("must have scope");
// 	}

// 	fn add_local_variable(&mut self, name: Arc<str>, value: GrugValue) {
// 		assert!(self.local_variables.last_mut()
// 			.expect("must have stack frame").last_mut()
// 			.expect("last frame must have scope").insert(name, value)
// 			.is_none(), "variable already exists");
// 	}

// 	fn pop_stack_frame(&mut self) {
// 		self.local_variables.pop().expect("must have stack frame");
// 	}

// 	fn push_scope(&mut self) {
// 		self.local_variables.last_mut()
// 			.expect("must already have a stack frame")
// 			.push(HashMap::new());
// 	}

// 	fn push_stack_frame(&mut self) {
// 		self.local_variables.push(Vec::new());
// 	}

// 	fn get_local_variable(&mut self, name: &str) -> Option<&mut GrugValue> {
// 		for scope in self.local_variables.last_mut()?{
// 			if let Some(val) = scope.get_mut(name) {
// 				return Some(val)
// 			}
// 		}
// 		None
// 	}
// }

// enum GrugControlFlow {
// 	Return(GrugValue),
// 	Break,
// 	Continue,
// 	None,
// }

// impl Interpreter {
// 	pub fn new() -> Self {
// 		Self {
// 			files: CacheMap::new(),
// 		}
// 	}

// 	fn run_function(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, arguments: &[Argument], values: &[GrugValue], statements: &[Statement]) -> Option<GrugValue> {
// 		if call_stack.local_variables.len() > MAX_RECURSION_LIMIT {
// 			state.set_runtime_error(RuntimeError::StackOverflow);
// 			return None
// 		}
// 		if arguments.len() != values.len() {
// 			panic!("argument count mismatch")
// 		}
// 		call_stack.push_stack_frame();
// 		call_stack.push_scope();

// 		for (argument, value) in arguments.iter().zip(values) {
// 			call_stack.add_local_variable(Arc::clone(&argument.name), *value);
// 		}
// 		let value = self.run_statements(call_stack, state, file, entity, statements)?;
// 		let value = match value {
// 			GrugControlFlow::Return(value) => value,
// 			GrugControlFlow::None          => GrugValue{void: ()},
// 			GrugControlFlow::Break         => unreachable!(),
// 			GrugControlFlow::Continue      => unreachable!(),
// 		};

// 		call_stack.pop_scope();
// 		call_stack.pop_stack_frame();
// 		Some(value)
// 	}

// 	fn run_statements(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, statements: &[Statement]) -> Option<GrugControlFlow> {
// 		call_stack.push_scope();
// 		let mut ret_val = GrugControlFlow::None;
// 		'outer: for statement in statements {
// 			match statement {
// 				Statement::Variable{
// 					name,
// 					ty,
// 					assignment_expr,
// 				} => {
// 					let assignment_expr = self.run_expr(call_stack, state, file, entity, assignment_expr)?;
// 					if let Some(_) = ty {
// 						call_stack.add_local_variable(Arc::clone(name), assignment_expr);
// 					} else {
// 						if let Some(var) = call_stack.get_local_variable(&**name) {
// 							*var = assignment_expr;
// 						} else if let Some(var) = entity.get_global_variable(&**name) {
// 							var.set(assignment_expr);
// 						} else {
// 							panic!("variable not found");
// 						}
// 					}
// 				},
// 				Statement::CallStatement {
// 					expr
// 				} => {
// 					self.run_expr(call_stack, state, file, entity, expr)?;
// 				},
// 				Statement::IfStatement{
// 					condition,
// 					is_chained: _,
// 					if_statements,
// 					else_statements,
// 				} => {
// 					let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool} != 0;
// 					// if statement
// 					if condition {
// 						let control_flow = self.run_statements(call_stack, state, file, entity, if_statements)?;
// 						if let GrugControlFlow::None = control_flow {
// 							continue;
// 						} else {
// 							ret_val = control_flow;
// 							break 'outer;
// 						} 
// 					} else {
// 						// else statements
// 						let control_flow = self.run_statements(call_stack, state, file, entity, else_statements)?;
// 						if let GrugControlFlow::None = control_flow {
// 							continue;
// 						} else {
// 							ret_val = control_flow;
// 							break 'outer;
// 						} 
// 					}
// 				},
// 				Statement::ReturnStatement{
// 					expr,
// 				} => {
// 					if let Some(expr) = expr {
// 						ret_val = GrugControlFlow::Return(self.run_expr(call_stack, state, file, entity, expr)?);
// 					} else {
// 						ret_val = GrugControlFlow::Return(GrugValue{void: ()});
// 					}
// 					break 'outer;
// 				},
// 				Statement::WhileStatement{
// 					condition,
// 					statements,
// 				} => {
// 					loop {
// 						let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool};
// 						if condition == 0 {
// 							break;
// 						}
// 						match self.run_statements(call_stack, state, file, entity, statements)? {
// 							GrugControlFlow::Return(value) => {
// 								ret_val = GrugControlFlow::Return(value);
// 								break 'outer;
// 							}
// 							GrugControlFlow::Continue => (),
// 							GrugControlFlow::Break    => break,
// 							GrugControlFlow::None     => (),
// 						}
// 					}
// 				},
// 				Statement::Comment{
// 					value: _,
// 				} => (),
// 				Statement::BreakStatement => {
// 					ret_val = GrugControlFlow::Break;
// 					break 'outer;
// 				},
// 				Statement::ContinueStatement => {
// 					ret_val = GrugControlFlow::Continue;
// 					break 'outer;
// 				},
// 				Statement::EmptyLineStatement => (),
// 			}
// 		}
// 		call_stack.pop_scope();
// 		Some(ret_val)
// 	}

// 	fn run_expr(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, expr: &Expr) -> Option<GrugValue> {
// 		if Instant::elapsed(&state.call_start_time.get()) > Duration::from_millis(ON_FN_TIME_LIMIT) {
// 			state.set_runtime_error(RuntimeError::ExceededTimeLimit);
// 			return None;
// 		}
// 		Some(match &expr.ty {
// 			ExprType::LiteralExpr{
// 				expr,
// 				line: _,
// 				col: _,
// 			} => {
// 				match expr {
// 					LiteralExpr::TrueExpr => GrugValue{bool: 1},
// 					LiteralExpr::FalseExpr => GrugValue{bool: 0},
// 					LiteralExpr::StringExpr{
// 						value
// 					} => unsafe{GrugValue{string: value.as_ntstrptr().detach_lifetime()}},
// 					LiteralExpr::ResourceExpr{
// 						value
// 					} => unsafe{GrugValue{string: value.as_ntstrptr().detach_lifetime()}},
// 					LiteralExpr::EntityExpr{
// 						value
// 					} => unsafe{GrugValue{string: value.as_ntstrptr().detach_lifetime()}},
// 					LiteralExpr::NumberExpr {
// 						value,
// 						string: _,
// 					} => GrugValue{number: *value},
// 					LiteralExpr::IdentifierExpr{
// 						name,
// 					} => {
// 						if let Some(var) = call_stack.get_local_variable(name) {
// 							*var
// 						} else {
// 							entity.get_global_variable(name)
// 								.expect("could not find variable")
// 								.get()
// 						}
// 					},
// 				}
// 			},
// 			ExprType::UnaryExpr{
// 				operator,
// 				expr,
// 			} => {
// 				let mut value = self.run_expr(call_stack, state, file, entity, &expr)?;
// 				match (operator, &expr.result_ty) {
// 					(UnaryOperator::Not, Some(GrugType::Bool)) => unsafe{value.bool = (value.bool == 0) as u8},
// 					(UnaryOperator::Minus, Some(GrugType::Number)) => unsafe{value.number = -value.number},
// 					_ => unreachable!(),
// 				}
// 				value
// 			}
// 			ExprType::BinaryExpr{
// 				operands,
// 				operator,
// 			} => {
// 				let first_value = self.run_expr(call_stack, state, file, entity, &operands.0)?; 
// 				let mut second_value = || self.run_expr(call_stack, state, file, entity, &operands.1);
// 				// debug_assert!(operands.0.result_ty == operands.1.result_ty || matches!((&operands.0.result_ty, &operands.1.result_ty), (Some(GrugType::Id{custom_name: None}), Some(GrugType::Id{..})) | (Some(GrugType::Id{..}), Some(GrugType::Id{custom_name: None}))));
// 				match (operator, &operands.0.result_ty) {
// 					(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{first_value.bool | second_value()?.bool}},
// 					(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(first_value.bool != 0 && second_value()?.bool != 0) as u8}},
// 					(BinaryOperator::DoubleEquals,   Some(ty)              )  => {
// 						let value = match ty {
// 							GrugType::Bool => !unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)},
// 							GrugType::Number => unsafe{first_value.number == second_value()?.number},
// 							GrugType::Id{..} => unsafe{first_value.id == second_value()?.id},
// 							GrugType::String => {
// 								unsafe{first_value.string.to_str() == second_value()?.string.to_str()}
// 							},
// 							_ => unreachable!(),
// 						};
// 						GrugValue{bool: value as u8}
// 					},
// 					(BinaryOperator::NotEquals,      Some(ty)              )  => {
// 						let value = match ty {
// 							GrugType::Bool => unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)}
// 							GrugType::Number => unsafe{first_value.number != second_value()?.number}
// 							GrugType::Id{..} => unsafe{first_value.id != second_value()?.id}
// 							GrugType::String => {
// 								unsafe{first_value.string.to_str() != second_value()?.string.to_str()}
// 							}
// 							_ => unreachable!(),
// 						};
// 						GrugValue{bool: value as u8}
// 					},
// 					(BinaryOperator::Greater,        Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number > second_value()?.number} as u8},
// 					(BinaryOperator::GreaterEquals,  Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number >= second_value()?.number} as u8},
// 					(BinaryOperator::Less,           Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number < second_value()?.number} as u8},
// 					(BinaryOperator::LessEquals,     Some(GrugType::Number))  => GrugValue{bool: unsafe{first_value.number <= second_value()?.number} as u8},
// 					(BinaryOperator::Plus,           Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number + second_value()?.number}},
// 					(BinaryOperator::Minus,          Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number - second_value()?.number}},
// 					(BinaryOperator::Multiply,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number * second_value()?.number}},
// 					(BinaryOperator::Division,       Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number / second_value()?.number}},
// 					(BinaryOperator::Remainder,      Some(GrugType::Number))  => GrugValue{number: unsafe{first_value.number % second_value()?.number}},
// 					_ => unreachable!(),
// 				}
// 			}
// 			ExprType::CallExpr{
// 				function_name,
// 				arguments,
// 				line: _,
// 				col: _,
// 			} if function_name.starts_with("helper_") => {
// 				let values = arguments.iter().map(|argument| self.run_expr(call_stack, state, file, entity, argument)).collect::<Option<Vec<_>>>()?;
// 				for helper_fn in &file.file.helper_functions {
// 					if helper_fn.name != *function_name {
// 						continue;
// 					}
// 					return Some(self.run_function(call_stack, state, file, entity, &*helper_fn.arguments, &values, &*helper_fn.body_statements)?);
// 				}
// 				unreachable!("helper function not found");
// 			}
// 			ExprType::CallExpr{
// 				function_name,
// 				arguments,
// 				line: _,
// 				col: _,
// 			} => {
// 				let values = arguments.iter().map(|argument| self.run_expr(call_stack, state, file, entity, argument)).collect::<Option<Vec<_>>>()?;
// 				let game_fn = state.game_functions.get(&**function_name).expect("can't find game function");
// 				let return_ty = &state.mod_api.game_functions().get(function_name).unwrap().return_ty;
// 				let ret_val = match (values.len(), return_ty) {
// 					(0, GrugType::Void) => unsafe{(game_fn.void_argless)(state, ); GrugValue{void: ()}},
// 					(0, _             ) => unsafe{(game_fn.value_argless)(state, )},
// 					(_, GrugType::Void) => unsafe{(game_fn.void)(state, values.as_ptr()); GrugValue{void: ()}},
// 					(_, _             ) => unsafe{(game_fn.value)(state, values.as_ptr())},
// 				};
// 				if state.is_errorring.get() {
// 					return None;
// 				}
// 				ret_val
// 			}
// 			ExprType::ParenthesizedExpr{
// 				expr,
// 				line: _,
// 				col: _,
// 			} => {
// 				self.run_expr(call_stack, state, file, entity, expr)?
// 			}
// 		})
// 	}

// 	fn init_global_variables(&self, state: &GrugState, file: &CompiledFile, entity: &mut GrugEntityData) -> Option<()> {
// 		state.call_start_time.set(Instant::now());
// 		file.file.global_variables.iter().map(|variable| {
// 			let value = self.run_expr(
// 				&mut CallStack::new(), 
// 				state, 
// 				file,
// 				entity, 
// 				&variable.assignment_expr
// 			)?;
// 			entity.global_variables.insert(Arc::clone(&variable.name), Cell::new(value));
// 			Some(())
// 		}).collect::<Option<Vec<_>>>()?;
// 		Some(())
// 	}
// }

impl Backend for Interpreter {
	fn insert_file(&self, id: GrugFileId, file: GrugAst) {
		todo!();
		// match self.files.get(&id.0) {
		// 	Some(_file) => {
		// 		todo!();
		// 	}
		// 	None => {
		// 		self.files.try_insert(id, CompiledFile::new(file));
		// 	}
		// }
	}

	fn init_entity<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity) -> bool {
		todo!();
		// let file = self.files.get(&entity.file_id)
		// 	.expect("file already compiled");

		// let mut data = GrugEntityData {
		// 	global_variables: HashMap::from([(Arc::from("me"), Cell::new(GrugValue{id:entity.id}))]),
		// };
		// if self.init_global_variables(state, file, &mut data).is_none() {
		// 	return false;
		// }

		// let data = unsafe{file.data.insert(data)};
		// file.entities.borrow_mut().push(NonNull::from_ref(entity));
		// entity.members.set(data.as_ptr());

		// true
	}

	fn clear_entities(&mut self) {
		todo!();
		// self.files.iter_mut().for_each(|(_, file)| {
		// 	file.entities.get_mut().clear();
		// });
	}

	fn destroy_entity_data(&self, entity: &GrugEntity) {
		todo!();
		// let file = self.files.get(&entity.file_id)
		// 	.expect("file compiled");
		// let Some((i, _)) = file.entities.borrow().iter().enumerate().find(|(_, en)| std::ptr::eq(en.as_ptr().cast_const(), entity)) else {
		// 	// not found
		// 	return false;
		// };
		// file.entities.borrow_mut().swap_remove(i);
		// return true;
	}

	unsafe fn call_on_function_raw<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		todo!();
		// let file = &self.files.get(&entity.file_id)
		// 	.expect("file already created");

		// let Some(on_function) = &file.file.on_functions[on_fn_index] else {
		// 	return false;
		// };

		// let values = if on_function.arguments.len() == 0 {
		// 	&[]
		// } else {
		// 	unsafe{std::slice::from_raw_parts(values, on_function.arguments.len())}
		// };

		// state.call_start_time.set(Instant::now());
		// self.run_function(
		// 	&mut CallStack::new(),
		// 	state,
		// 	file,
		// 	unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
		// 	&on_function.arguments, 
		// 	values,
		// 	&on_function.body_statements
		// ).is_some()
	}

	fn call_on_function<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		todo!();
		// let file = &self.files.get(&entity.file_id)
		// 	.expect("file already created");

		// let Some(on_function) = &file.file.on_functions[on_fn_index] else {
		// 	return false;
		// };

		// state.call_start_time.set(Instant::now());
		// self.run_function(
		// 	&mut CallStack::new(),
		// 	state,
		// 	file,
		// 	unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
		// 	&on_function.arguments, 
		// 	values,
		// 	&on_function.body_statements
		// ).is_some()
	}
}
