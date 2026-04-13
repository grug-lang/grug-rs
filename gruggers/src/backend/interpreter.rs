use crate::types::{GrugValue, GrugEntity, GrugFileId};
use crate::ast::{
	Argument, Statement, Expr, ExprData, MemberVariable, OnFunction,
	HelperFunction, UnaryOperator, BinaryOperator, GrugType, GrugAst,
};
use crate::xar::Xar;
use crate::arena::Arena;
use crate::backend::Backend;
use crate::ntstring::{NTStr, NTStrPtr};

use gruggers_core::runtime_error::{RuntimeError, ON_FN_TIME_LIMIT, MAX_RECURSION_LIMIT};
use gruggers_core::state::State;

use std::ptr::NonNull;
use std::pin::Pin;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::time::{Instant, Duration};

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
			span: member.span,
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
			span: on_function.span,
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
			span: helper_function.span
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
			} => {
				let mut ifs = Vec::new();
				let mut condition = condition;
				let mut is_chained = is_chained;
				let mut if_block = if_block;
				let mut else_block = else_block;
				while *is_chained {
					ifs.push((
						copy_expr(condition, arena),
						*is_chained,
						copy_statements(if_block, arena),
					));
					(condition, is_chained, if_block, else_block) = match else_block {
						[Statement::If{condition, is_chained, if_block, else_block}] => (condition, is_chained, if_block, else_block),
						_ => panic!("invalid ast"),
					};
				}
				let mut current = Statement::If {
					condition: copy_expr(condition, arena),
					is_chained: *is_chained,
					if_block: copy_statements(if_block, arena),
					else_block: copy_statements(else_block, arena),
				};
				for (condition, is_chained, if_block) in ifs.into_iter().rev() {
					current = Statement::If {
						condition,
						is_chained,
						if_block,
						else_block: std::slice::from_mut(Box::leak(Box::new_in(current, arena))),
					};
				}
				current
			}
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
	pub(crate) global_variables: HashMap<&'static str, Cell<GrugValue>>,
}

impl GrugEntityData {
	pub(crate) fn get_global_variable(&self, name: &str) -> Option<&Cell<GrugValue>> {
		self.global_variables.get(name)
	}
}

struct CompiledFile {
	file: GrugAst<'static>,
	entities: RefCell<std::vec::Vec<NonNull<GrugEntity>>>,
	data: Xar<GrugEntityData>,
	_arena: Arena,
}

impl CompiledFile {
	fn new(file: GrugAst) -> Self {
		let arena = Arena::new();
		let file = unsafe{std::mem::transmute::<GrugAst<'_>, GrugAst<'static>>(copy_into_arena(&file, &arena))};
		Self {
			file: file,
			entities: RefCell::new(std::vec::Vec::new()),
			data: Xar::new(),
			_arena: arena,
		}
	}
}

pub struct Interpreter {
	files: RefCell<Vec<CompiledFile>>,
}

struct CallStack {
	start_time: Instant,
	local_variables: Vec<Vec<HashMap<&'static str, GrugValue>>>,
}

impl CallStack {
	fn new() -> Self {
		Self {
			start_time: Instant::now(),
			local_variables: Vec::new(),
		}
	}
	
	fn pop_scope(&mut self) {
		self.local_variables.last_mut()
			.expect("must already have a stack frame").pop()
			.expect("must have scope");
	}

	fn add_local_variable(&mut self, name: &'static str, value: GrugValue) {
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

impl Interpreter {
	pub fn new() -> Self {
		Self {
			files: RefCell::new(Vec::new()),
		}
	}

	fn run_function<GrugState: State>(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, arguments: &'static [Argument], values: &[GrugValue], statements: &'static [Statement]) -> Option<GrugValue> {
		if call_stack.local_variables.len() > MAX_RECURSION_LIMIT {
			state.set_runtime_error(RuntimeError::StackOverflow);
			return None
		}
		if arguments.len() != values.len() {
			panic!("argument count mismatch")
		}
		call_stack.push_stack_frame();
		call_stack.push_scope();

		for (argument, value) in arguments.iter().zip(values) {
			call_stack.add_local_variable(argument.name.to_str(), *value);
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
		Some(value)
	}

	fn run_statements<GrugState: State>(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, statements: &'static [Statement]) -> Option<GrugControlFlow> {
		call_stack.push_scope();
		let mut ret_val = GrugControlFlow::None;
		'outer: for statement in statements {
			match statement {
				Statement::Variable{
					name,
					ty,
					assignment_expr,
				} => {
					let name = name.to_str();
					let assignment_expr = self.run_expr(call_stack, state, file, entity, assignment_expr)?;
					if let Some(_) = ty {
						call_stack.add_local_variable(name, assignment_expr);
					} else {
						if let Some(var) = call_stack.get_local_variable(name) {
							*var = assignment_expr;
						} else if let Some(var) = entity.get_global_variable(name) {
							var.set(assignment_expr);
						} else {
							panic!("variable not found");
						}
					}
				},
				Statement::Call(expr) => {
					self.run_expr(call_stack, state, file, entity, expr)?;
				},
				Statement::If{
					condition,
					is_chained,
					if_block,
					else_block,
				} => {
					let mut condition = condition;
					let mut is_chained = is_chained;
					let mut if_block = if_block;
					let mut else_block = else_block;
					loop {
						// if block
						if unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool} != 0 {
							let control_flow = self.run_statements(call_stack, state, file, entity, if_block)?;
							if let GrugControlFlow::None = control_flow {
								break;
							} else {
								ret_val = control_flow;
								break 'outer;
							} 
						} else {
							// else block
							if *is_chained {
								(condition, is_chained, if_block, else_block) = match else_block {
									[Statement::If{condition, is_chained, if_block, else_block}] => (condition, is_chained, if_block, else_block),
									_ => panic!("invalid ast"),
								};
								continue;
							} else {
								let control_flow = self.run_statements(call_stack, state, file, entity, else_block)?;
								if let GrugControlFlow::None = control_flow {
									break;
								} else {
									ret_val = control_flow;
									break 'outer;
								} 
							}
						}
					}
				},
				Statement::Return{
					expr,
				} => {
					if let Some(expr) = expr {
						ret_val = GrugControlFlow::Return(self.run_expr(call_stack, state, file, entity, expr)?);
					} else {
						ret_val = GrugControlFlow::Return(GrugValue{void: ()});
					}
					break 'outer;
				},
				Statement::While{
					condition,
					block,
				} => {
					loop {
						let condition = unsafe{self.run_expr(call_stack, state, file, entity, condition)?.bool};
						if condition == 0 {
							break;
						}
						match self.run_statements(call_stack, state, file, entity, block)? {
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
				Statement::Comment(_) => (),
				Statement::Break => {
					ret_val = GrugControlFlow::Break;
					break 'outer;
				},
				Statement::Continue => {
					ret_val = GrugControlFlow::Continue;
					break 'outer;
				},
				Statement::EmptyLine => (),
			}
		}
		call_stack.pop_scope();
		Some(ret_val)
	}

	fn run_expr<GrugState: State>(&self, call_stack: &mut CallStack, state: &GrugState, file: &CompiledFile, entity: &GrugEntityData, expr: &'static Expr) -> Option<GrugValue> {
		if call_stack.start_time.elapsed() > Duration::from_millis(ON_FN_TIME_LIMIT) {
			state.set_runtime_error(RuntimeError::ExceededTimeLimit);
			return None;
		}
		Some(match &expr.data {
			ExprData::True => GrugValue{bool: 1},
			ExprData::False => GrugValue{bool: 0},
			ExprData::String(value) => GrugValue{string: *value},
			ExprData::Resource(value) => GrugValue{string: *value},
			ExprData::Entity(value) => GrugValue{string: *value},
			ExprData::Number (value, _) => GrugValue{number: *value},
			ExprData::Identifier(name) => {
				let name = name.to_str();
				if let Some(var) = call_stack.get_local_variable(name) {
					*var
				} else {
					entity.get_global_variable(name)
						.expect("could not find variable")
						.get()
				}
			},
			ExprData::Unary{
				op,
				expr,
			} => {
				let mut value = self.run_expr(call_stack, state, file, entity, &expr)?;
				match (op, &expr.result_type) {
					(UnaryOperator::Not, Some(GrugType::Bool)) => unsafe{value.bool = (value.bool == 0) as u8},
					(UnaryOperator::Minus, Some(GrugType::Number)) => unsafe{value.number = -value.number},
					_ => unreachable!(),
				}
				value
			}
			ExprData::Binary{
				op,
				left,
				right
			} => {
				let first_value = self.run_expr(call_stack, state, file, entity, &left)?; 
				let mut second_value = || self.run_expr(call_stack, state, file, entity, &right);
				// debug_assert!(left.result_ty == right.result_ty || matches!((&left.result_ty, &right.result_ty), (Some(GrugType::Id{custom_name: None}), Some(GrugType::Id{..})) | (Some(GrugType::Id{..}), Some(GrugType::Id{custom_name: None}))));
				match (op, &left.result_type) {
					(BinaryOperator::Or,             Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{first_value.bool | second_value()?.bool}},
					(BinaryOperator::And,            Some(GrugType::Bool  ))  => GrugValue{bool: unsafe{(first_value.bool != 0 && second_value()?.bool != 0) as u8}},
					(BinaryOperator::DoubleEquals,   Some(ty)              )  => {
						let value = match ty {
							GrugType::Bool => !unsafe{(first_value.bool == 0) ^ (second_value()?.bool == 0)},
							GrugType::Number => unsafe{first_value.number == second_value()?.number},
							GrugType::Id{..} => unsafe{first_value.id == second_value()?.id},
							GrugType::String => {
								unsafe{first_value.string.to_str() == second_value()?.string.to_str()}
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
								unsafe{first_value.string.to_str() != second_value()?.string.to_str()}
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
			ExprData::Call{
				name,
				args,
				ptr: None
			} => {
				let name = name.to_str();
				let values = args.iter().map(|argument| self.run_expr(call_stack, state, file, entity, argument)).collect::<Option<Vec<_>>>()?;
				for helper_fn in file.file.helper_functions {
					if helper_fn.name.to_str() != name {
						continue;
					}
					return Some(self.run_function(call_stack, state, file, entity, &*helper_fn.arguments, &values, &*helper_fn.body_statements)?);
				}
				unreachable!("helper function not found");
			}
			ExprData::Call{
				name: _,
				args,
				ptr: Some(ptr),
			} => {
				let ptr = unsafe{ptr.as_ptr()};
				let values = args.iter().map(|arg| self.run_expr(call_stack, state, file, entity, arg)).collect::<Option<Vec<_>>>()?;
				let ret_val = ptr(state, values.as_ptr());
				let ret_val = if expr.result_type == Some(&GrugType::Void) {GrugValue{void: ()}} else {ret_val};
				if state.is_errorring() {
					return None;
				}
				ret_val
			}
			ExprData::Parenthesized(expr) => {
				self.run_expr(call_stack, state, file, entity, expr)?
			}
		})
	}

	fn init_global_variables<GrugState: State>(&self, state: &GrugState, file: &CompiledFile, entity: &mut GrugEntityData) -> Option<()> {
		file.file.members.iter().map(|variable| {
			let value = self.run_expr(
				&mut CallStack::new(), 
				state, 
				file,
				entity, 
				&variable.assignment_expr
			)?;
			entity.global_variables.insert(variable.name.to_str(), Cell::new(value));
			Some(())
		}).collect::<Option<Vec<_>>>()?;
		Some(())
	}
}

impl Backend for Interpreter {
	fn insert_file<GrugState: State>(&self, state: &GrugState, id: GrugFileId, file: GrugAst) {
		let mut compiled_file = CompiledFile::new(file);
		let mut files = self.files.borrow_mut();
		if let Some(old_file) = files.get_mut(id.0 as usize) {
			let mut old_entities = std::mem::replace(&mut *old_file.entities.borrow_mut(), std::vec::Vec::new());
			old_entities.extract_if(.., |old_entity| {
				let mut data = GrugEntityData {
					global_variables: HashMap::from([("me", Cell::new(GrugValue{id:unsafe{(*old_entity.as_ptr()).id}}))]),
				};
				if self.init_global_variables(state, &compiled_file, &mut data).is_none() {
					return true;
				}
				let data = compiled_file.data.insert(data);
				unsafe{(*old_entity.as_ptr()).members.set(data.as_ptr().cast())};
				false
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
		let file = self.files.borrow();
		let file = file.get(entity.file_id.0 as usize)
			.expect("file already compiled");

		let mut data = GrugEntityData {
			global_variables: HashMap::from([("me", Cell::new(GrugValue{id:entity.id}))]),
		};
		if self.init_global_variables(state, file, &mut data).is_none() {
			return false;
		}

		let data = file.data.insert(data);
		file.entities.borrow_mut().push(NonNull::from_ref(Pin::get_ref(entity)));
		entity.members.set(data.as_ptr().cast());

		true
	}

	fn clear_entities(&mut self) {
		self.files.borrow_mut().iter_mut().for_each(|file| {
			file.entities.get_mut().clear();
			file.data.clear();
		});
	}

	fn destroy_entity_data(&self, entity: &GrugEntity) {
		let file = self.files.borrow();
		let file = file.get(entity.file_id.0 as usize)
			.expect("file compiled");
		file.entities.borrow_mut().extract_if(.., |en| std::ptr::eq(en.as_ptr().cast_const(), entity)).for_each(|_| {});
	}

	unsafe fn call_on_function_raw<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		let file = &self.files.borrow();
		let file = file.get(entity.file_id.0 as usize)
			.expect("file already created");

		let Some(on_function) = &file.file.on_functions[on_fn_index] else {
			return false;
		};

		let values = if on_function.arguments.len() == 0 {
			&[]
		} else {
			unsafe{std::slice::from_raw_parts(values, on_function.arguments.len())}
		};

		self.run_function(
			&mut CallStack::new(),
			state,
			file,
			unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
			&on_function.arguments, 
			values,
			&on_function.body_statements
		).is_some()
	}

	fn call_on_function<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		let file = &self.files.borrow();
		let file = file.get(entity.file_id.0 as usize)
			.expect("file already created");

		let Some(on_function) = &file.file.on_functions[on_fn_index] else {
			return false;
		};

		self.run_function(
			&mut CallStack::new(),
			state,
			file,
			unsafe{entity.members.get().cast::<GrugEntityData>().as_ref()}, 
			&on_function.arguments, 
			values,
			&on_function.body_statements
		).is_some()
	}
}
