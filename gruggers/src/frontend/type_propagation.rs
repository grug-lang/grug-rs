use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ffi::OsStr;
use std::path::PathBuf;

use crate::error::{Error, ErrorKind, SourceSpan};
use crate::types::GameFnPtr;
use crate::ntstring::{NTStr, NTStrPtr};
use crate::ast::{
	GrugType, UnaryOperator, BinaryOperator,
	ExprData, HelperFunction, Statement, Expr,
	Parameter,
};
use crate::frontend::GlobalStatement;
use crate::nt;
use crate::arena::Arena;
use crate::frontend::parser::Ast;
use crate::mod_api::{ModApiEntity, ModApiHostFn};

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

pub(super) struct TypePropogator<'mod_api, 'arena> {
	file_text: &'arena str,
	file_path: &'arena OsStr,
	entity: &'mod_api ModApiEntity<'mod_api>,
	game_fns: &'mod_api HashMap<&'mod_api NTStr, ModApiHostFn<'mod_api>>,
	game_fn_ptrs: &'arena HashMap<&'static str, GameFnPtr>,
	resources: Vec<&'arena OsStr, &'arena Arena>,
	current_mod_name: &'arena OsStr,
	mods_dir_path: &'mod_api OsStr,
	global_variables: HashMap<&'arena str, GrugType<'arena>>,
	local_variables: Vec<HashMap<&'arena str, GrugType<'arena>>>,
	num_while_loops_deep: usize,
	current_fn_name: Option<&'arena str>,
	arena: &'arena Arena,
}

impl<'mod_api: 'arena, 'arena> TypePropogator<'mod_api, 'arena> {
	pub fn new (
		entity: &'mod_api ModApiEntity, 
		game_fns: &'mod_api HashMap<&'mod_api NTStr, ModApiHostFn>, 
		game_fn_ptrs: &'arena HashMap<&'static str, GameFnPtr>, 
		mod_name: &'arena OsStr, 
		mods_dir_path: &'mod_api OsStr, 
		file_text: &'arena str,
		file_path: &'arena OsStr,
		arena: &'arena Arena,
	) -> Self {
		Self {
			file_text,
			file_path,
			entity,
			game_fns,
			game_fn_ptrs,
			current_mod_name: mod_name,
			resources: Vec::new_in(arena),
			mods_dir_path,
			global_variables: HashMap::new(),
			local_variables: Vec::new(),
			num_while_loops_deep: 0,
			current_fn_name: None,
			arena,
		}
	}

	#[track_caller]
	fn new_type_propagator_error<T>(&self, span: SourceSpan, args: std::fmt::Arguments) -> Result<T, Error> {
		Err(Error::new(
			ErrorKind::TYPE_CHECKER_ERROR,
			self.current_fn_name.unwrap_or("member scope"),
			self.file_path,
			self.file_text, 
			span,
			args
		))
	}

	pub fn fill_result_types(
		entity: &'mod_api ModApiEntity, 
		game_fns: &'mod_api HashMap<&'mod_api NTStr, ModApiHostFn>, 
		game_fn_ptrs: &'arena HashMap<&'static str, GameFnPtr>, 
		mod_name: &'arena OsStr, 
		mods_dir_path: &'mod_api OsStr, 
		file_text: &'arena str,
		file_path: &'arena OsStr,
		entity_type: &str, 
		ast: &mut Ast<'arena>,
		arena: &'arena Arena,
	) -> Result<&'arena [&'arena OsStr], Error> {
		let mut type_propagator = Self::new(
			entity, 
			game_fns,
			game_fn_ptrs,
			mod_name,
			mods_dir_path,
			file_text, 
			file_path,
			arena,
		);

		type_propagator.global_variables.insert(
			nt!("me"), 
			GrugType::Id{custom_name: Some(type_propagator.arena.copy_str_into_nt(entity_type).as_ntstrptr())}
		);

		let variables = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::Variable(x) => Some(x), _ => None});
		for variable in variables {
			type_propagator.check_global_expr(&variable.assignment_expr, variable.name.to_str())?;
			let result_ty = type_propagator.fill_expr(&ast.local_fn_signatures, &ast.export_fn_signatures, &mut variable.assignment_expr)?;

			if let ExprData::Identifier(name) = &variable.assignment_expr.data 
				&& name.to_str() == "me" {
				return type_propagator.new_type_propagator_error(
					variable.assignment_expr.span,
					format_args!("Global variables can't be assigned 'me'")
				);
			}
			if !(variable.ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && result_ty != variable.ty {
				return type_propagator.new_type_propagator_error(
					variable.assignment_expr.span,
					format_args!("Can't assign {} to '{}', which has type {}", result_ty, variable.name, variable.ty)
				);
			}
			type_propagator.add_global_variable(variable.name.to_str(), result_ty, variable.span)?;
		}

		let mut previous_on_fn_index = 0;
		// on functions need to be iterated separately because grug_tests expects a certain order to the on_functions
		let mut on_functions = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::OnFunction(x) => Some(x), _ => None})
			.collect::<Vec<_>>();
		for (on_fn_name, mod_api_on_fn) in 
			type_propagator.entity.export_fns.iter()
		{
			let Some((current_index, current_on_fn)) = 
				on_functions.iter_mut().enumerate()
				.find(|(_, on_fn)| on_fn.name.to_ntstr() == &**on_fn_name) else 
			{
				continue;
			};

			// These should only be set inside type_propagator.fill_statements
			debug_assert!(type_propagator.local_variables.is_empty());
			debug_assert!(type_propagator.num_while_loops_deep == 0);
			debug_assert!(type_propagator.current_fn_name.is_none());

			if previous_on_fn_index > current_index {
				type_propagator.current_fn_name = Some(on_fn_name);
				return type_propagator.new_type_propagator_error(
					current_on_fn.span,
					format_args!("The function '{}' needs to be moved before or after a different export function, according to the entity '{}' in mod_api.json", current_on_fn.name.to_str(), entity_type)
				);
			}
			previous_on_fn_index = current_index;

			type_propagator.current_fn_name = Some(current_on_fn.name.to_str());
			
			if mod_api_on_fn.parameters.len() > current_on_fn.parameters.len() {
				let param = &mod_api_on_fn.parameters[current_on_fn.parameters.len()];
				return type_propagator.new_type_propagator_error(
					current_on_fn.span,
					format_args!("Function '{}' expected the parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				);
			} else if mod_api_on_fn.parameters.len() < current_on_fn.parameters.len() {
				let param = &current_on_fn.parameters[mod_api_on_fn.parameters.len()];
				return type_propagator.new_type_propagator_error(
					param.name_span,
					format_args!("Function '{}' got an unexpected extra parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				);
			}
			for (param, arg) in mod_api_on_fn.parameters.iter().zip(current_on_fn.parameters.iter()) {
				if param.name != arg.name {
					return type_propagator.new_type_propagator_error(
						arg.name_span,
						format_args!("Function '{}' its '{}' parameter was supposed to be named '{}'", current_on_fn.name.to_str(), arg.name.to_str(), param.name.to_str())
					);
				}
				if param.ty != arg.ty {
					return type_propagator.new_type_propagator_error(
						arg.type_span,
						format_args!("Function '{}' its '{}' parameter was supposed to have the type {}, but got {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty, arg.ty)
					);
				}
			}
			type_propagator.push_scope();
			for param in current_on_fn.parameters {
				type_propagator.add_local_variable(param.name.to_str(), param.ty, param.name_span)?;
			}
			type_propagator.fill_statements(&ast.local_fn_signatures, &ast.export_fn_signatures, current_on_fn.body_statements, &GrugType::Void)?;
			type_propagator.pop_scope();

			debug_assert!(type_propagator.current_fn_name == Some(current_on_fn.name.to_str()));
			type_propagator.current_fn_name = None;
		}
		let entity_on_functions = &type_propagator.entity.export_fns;
		for on_fn in on_functions {
			let on_fn_name = on_fn.name.to_ntstr();
			if !entity_on_functions.iter().any(|(name, _)| *name == on_fn_name) {
				type_propagator.current_fn_name = Some(on_fn_name.as_str());
				return type_propagator.new_type_propagator_error(
					on_fn.span,
					format_args!("The function '{}' was not declared by entity '{}' in mod_api.json", on_fn_name, entity_type)
				);
			}
		}

		for statement in &mut ast.global_statements {
			match statement {
				GlobalStatement::Variable(_) => (),
				GlobalStatement::OnFunction(_) => (),
				GlobalStatement::EmptyLine => (),
				GlobalStatement::HelperFunction(HelperFunction{
					name,
					parameters,
					body_statements,
					return_type,
					span,
				}) => {
					debug_assert!(type_propagator.local_variables.is_empty());
					debug_assert!(type_propagator.num_while_loops_deep == 0);
					debug_assert!(type_propagator.current_fn_name.is_none());

					let name = name.to_str();
					type_propagator.current_fn_name = Some(name);
					type_propagator.push_scope();
					for param in *parameters {
						type_propagator.add_local_variable(param.name.to_str(), param.ty, param.name_span)?;
					}
					type_propagator.fill_statements(&ast.local_fn_signatures, &ast.export_fn_signatures, body_statements, return_type)?;

					if *return_type != GrugType::Void && !matches!(body_statements.last(), Some(Statement::Return{..})) {
						return type_propagator.new_type_propagator_error(
							*span,
							format_args!("Function '{}' is supposed to return {} as its last line", name, return_type)
						);
					}

					type_propagator.pop_scope();

					debug_assert!(type_propagator.current_fn_name == Some(name));
					type_propagator.current_fn_name = None;
				}
				GlobalStatement::Comment{..} => (),
			}
		}
		Ok(type_propagator.resources.leak())
	}
	
	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_statements(&mut self, helper_fns: &[(&str, (GrugType<'arena>, &[Parameter<'arena>]))], export_fns: &[(&str, &[Parameter<'arena>])], statements: &mut [Statement<'arena>], expected_return_type: &GrugType<'arena>) -> Result<(), Error> {
		self.push_scope();
		for statement in statements {
			match statement {
				Statement::Variable{
					name,
					ty,
					assignment_expr,
					name_span,
				} => {
					let result_ty = self.fill_expr(helper_fns, export_fns, assignment_expr)?;
					
					if let Some(ty) = ty {
						self.add_local_variable(name.to_str(), **ty, *name_span)?;
						if !(**ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && **ty != result_ty {
							return self.new_type_propagator_error(
								assignment_expr.span,
								format_args!("Can't assign {} to '{}', which has type {}", result_ty, name, ty)
							);
						}
					} else {
						let ty = if let Some(ty) = self.get_global_variable_type(name.to_str()) {
							if matches!(ty, GrugType::Id {..}) {
								return self.new_type_propagator_error(
									assignment_expr.span,
									format_args!("Global id variables can't be reassigned")
								);
							}
							ty
						} else if let Some(ty) = self.get_local_variable_type(name.to_str()) {
							ty
						} else {
							return self.new_type_propagator_error(
								*name_span,
								format_args!("Can't assign to the variable '{}', since it does not exist", name)
							);
						};

						if !(ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && ty != result_ty {
							return self.new_type_propagator_error(
								assignment_expr.span,
								format_args!("Can't assign {} to '{}', which has type {}", result_ty, name, ty)
							);
						}
					}
				}
				Statement::Call(expr) => {
					self.fill_expr(helper_fns, export_fns, expr)?;
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
					loop {
						let cond_type = self.fill_expr(helper_fns, export_fns, condition)?;
						if cond_type != GrugType::Bool {
							return self.new_type_propagator_error(
								condition.span,
								format_args!("If condition must be bool but got '{}'", cond_type)
							);
						}
						self.fill_statements(helper_fns, export_fns, if_block, expected_return_type)?;
						if !else_block.is_empty() {
							if *is_chained {
								debug_assert!(else_block.len() == 1);
								let [statement] = else_block else {unreachable!()};
								(condition, is_chained, if_block, else_block) = match statement {
									Statement::If{condition, is_chained, if_block, else_block} => (condition, is_chained, if_block, else_block),
									_ => unreachable!(),
								};
								continue;
							} else {
								self.fill_statements(helper_fns, export_fns, else_block, expected_return_type)?;
							}
						}
						break;
					}
					// TODO: Maybe this should be looked at again
					// [https://github.com/grug-lang/grug/issues/116]
				}
				Statement::While {
					condition,
					block,
				} => {
					let cond_type = self.fill_expr(helper_fns, export_fns, condition)?;
					if cond_type != GrugType::Bool {
						return self.new_type_propagator_error(
							condition.span,
							format_args!("While condition must be bool but got '{}'", cond_type)
						);
					}
					self.num_while_loops_deep += 1;
					self.fill_statements(helper_fns, export_fns, block, expected_return_type)?;
					self.num_while_loops_deep -= 1;

				}
				Statement::Return {
					return_span,
					expr,
				} => {
					let (return_ty, span) = expr.as_mut()
						.map(|expr| Ok((self.fill_expr(helper_fns, export_fns, expr)?, expr.span)))
						.unwrap_or(Ok((GrugType::Void, *return_span)))?;
					if *expected_return_type != (GrugType::Id{custom_name: None}) && *expected_return_type != return_ty {
						if return_ty == GrugType::Void {
							return self.new_type_propagator_error(
								span,
								format_args!("Function '{}' is supposed to return a value of type {}", self.current_fn_name.unwrap(), expected_return_type)
							);
						} else if *expected_return_type == GrugType::Void {
							return self.new_type_propagator_error(
								span,
								format_args!("Function '{}' wasn't supposed to return any value", self.current_fn_name.unwrap())
							);
						} else {
							return self.new_type_propagator_error(
								span,
								format_args!("Function '{}' is supposed to return {}, not {}", self.current_fn_name.unwrap(), expected_return_type, return_ty)
							);
						}
					}
				}
				Statement::Break(span) => {
					if self.num_while_loops_deep == 0 {
						return self.new_type_propagator_error(
							*span,
							format_args!("There is a break statement that isn't inside of a while loop")
						);
					}
				}
				Statement::Continue(span) => {
					if self.num_while_loops_deep == 0 {
						return self.new_type_propagator_error(
							*span,
							format_args!("There is a continue statement that isn't inside of a while loop")
						);
					}
				}
				_ => (),
			}
		}
		self.pop_scope();
		Ok(())
	}

	// Check that the global variable's assigned value doesn't contain a call_to a helper function nor identifier
	fn check_global_expr(&mut self, assignment_expr: &Expr<'_>, name: &str) -> Result<(), Error> {
		match &assignment_expr.data {
			ExprData::Entity(_) => unreachable!(),
			ExprData::Resource(_) => unreachable!(),
			ExprData::True          |
			ExprData::False         |
			ExprData::String(_)     | 
			ExprData::Identifier(_) |
			ExprData::Number(_, _)  => (),
			ExprData::Unary{
				op: _,
				expr,
				op_span: _,
			} => self.check_global_expr(expr, name)?,
			ExprData::Binary{
				left,
				right,
				op: _,
				op_span: _,
			} => {
				self.check_global_expr(left, name)?;
				self.check_global_expr(right, name)?;
			},
			ExprData::Call{
				reciever: None,
				name: fn_name,
				args,
				ptr : _,
				name_span: _,
			} => {
				let fn_name = fn_name.to_str();
				if fn_name.starts_with("_") {
					return self.new_type_propagator_error(
						assignment_expr.span,
						format_args!("The global variable '{}' isn't allowed to call local functions", name)
					);
				}
				args.iter().map(|argument| self.check_global_expr(argument, name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprData::Call{
				reciever: Some(_),
				name: _,
				args: _,
				ptr : _,
				name_span: _,
			} => {
				unimplemented!();
			},
			ExprData::Parenthesized(expr) => self.check_global_expr(expr, name)?,
		}
		Ok(())
	}

	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_expr(&mut self, helper_fns: &[(&str, (GrugType<'arena>, &[Parameter<'arena>]))], export_fns: &[(&str, &[Parameter<'arena>])], assignment_expr: &mut Expr<'arena>) -> Result<GrugType<'arena>, Error> {
		// MUST be None before type propogation
		assert!(assignment_expr.result_type.is_none());
		let result_ty = match &mut assignment_expr.data {
			ExprData::True => GrugType::Bool,
			ExprData::False => GrugType::Bool,
			ExprData::String{..} => GrugType::String,
			ExprData::Resource{..} => GrugType::Resource{extension: nt!("").as_ntstrptr()},
			ExprData::Entity{..} => GrugType::Entity{entity_type: None},
			ExprData::Identifier(name) => {
				let Some(ty) = self.get_variable_type(name.to_str()) else {
					return self.new_type_propagator_error(
						assignment_expr.span,
						format_args!("The variable '{}' does not exist", name.to_str())
					);
				};
				ty
			},
			ExprData::Number{
				..
			} => GrugType::Number,
			ExprData::Unary{
				op,
				expr,
				op_span,
			} => {
				if let Expr{data: ExprData::Unary{op: next_op, ..}, ..} = expr && next_op == op {
					return self.new_type_propagator_error(
						*op_span,
						format_args!("Found '{0}' directly next to another '{0}', which can be simplified by just removing both of them", op)
					);
				}
				let result_ty = self.fill_expr(helper_fns, export_fns, expr)?;
				match (op, &result_ty) {
					(UnaryOperator::Not, GrugType::Bool) => (),
					(UnaryOperator::Not, got) => return self.new_type_propagator_error(
						*op_span,
						format_args!("Found 'not' before {}, but it can only be put before a bool", got)
					),
					(UnaryOperator::Minus, GrugType::Number) => (),
					(UnaryOperator::Minus, got) => return self.new_type_propagator_error(
						*op_span,
						format_args!("Found '-' before {}, but it can only be put before a number", got)
					),
					// _ => (),
				};
				result_ty
			},
			ExprData::Binary{
				left,
				right,
				op,
				op_span,
			} => {
				let result_0 = self.fill_expr(helper_fns, export_fns, left)?;
				let result_1 = self.fill_expr(helper_fns, export_fns, right)?;
				match (&result_0, &result_1, *op) {
					(GrugType::String, GrugType::String, BinaryOperator::DoubleEquals) | 
					(GrugType::String, GrugType::String, BinaryOperator::NotEquals) => (),
					(GrugType::String, GrugType::String, BinaryOperator::Plus) => {
						return self.new_type_propagator_error(
							*op_span,
							format_args!("cannot add strings with '+'")
						);
					},
					(GrugType::String, GrugType::String, _) => {
						return self.new_type_propagator_error(
							*op_span,
							format_args!("You can't use the '{}' operator on strings", op)
						);
					},
					_ => (),
				}
				if !GrugType::match_non_exact(&result_0, &result_1) {
					return self.new_type_propagator_error(
						*op_span,
						format_args!("The left and right operand of a binary expression ('{}') must have the same type, but got {} and {}", op, result_0, result_1)
					);
				}

				match op {
					BinaryOperator::Or | BinaryOperator::And => {
						if result_0 != GrugType::Bool {
							return self.new_type_propagator_error(
								*op_span,
								format_args!("'{}' operator expects bool", op)
							);
						}
						GrugType::Bool
					}
					BinaryOperator::DoubleEquals | BinaryOperator::NotEquals => {
						GrugType::Bool
					},
					BinaryOperator::Greater | BinaryOperator::GreaterEquals | 
					BinaryOperator::Less | BinaryOperator::LessEquals => {
						if result_0 != GrugType::Number {
							return self.new_type_propagator_error(
								*op_span,
								format_args!("'{}' operator expects number", op)
							);
						}
						GrugType::Bool
					},
					BinaryOperator::Plus | BinaryOperator::Minus |
					BinaryOperator::Multiply | BinaryOperator::Division => {
						if result_0 != GrugType::Number {
							return self.new_type_propagator_error(
								*op_span,
								format_args!("'{}' operator expects number", op)
							);
						}
						result_0
					},
				}
			},
			ExprData::Call{
				reciever: None,
				name: fn_name,
				args,
				ptr ,
				name_span,
			} => {
				let fn_name = fn_name.to_str();
				if let Some((_, (return_ty, sig_arguments))) = helper_fns.iter().find(|(name, _)| *name == fn_name) {
					self.check_arguments(helper_fns, export_fns, fn_name, *name_span, sig_arguments, args)?;
					*return_ty
				} else if let Some(game_fn) = self.game_fns.get(fn_name) {
					self.check_arguments(helper_fns, export_fns, fn_name, *name_span, game_fn.parameters, args)?;
					if let Some(game_fn_ptr) = self.game_fn_ptrs.get(fn_name) {
						*ptr = Some(*game_fn_ptr);
						game_fn.return_ty
					} else {
						panic!("This error is not triggerred by grug_tests");
						// return self.new_type_propagator_error(
						// 	*name_span,
						// 	format_args!("Game function {} was not registered", fn_name)
						// );
					}
				} else if fn_name.starts_with("_") {
					return self.new_type_propagator_error(
						*name_span,
						format_args!("The local function '{}' was not defined by this grug file", fn_name)
					);
				} else if export_fns.iter().any(|(name, _)| *name == fn_name) {
					return self.new_type_propagator_error(
						*name_span,
						format_args!("Mods aren't allowed to call their own export functions")
					);
				} else {
					return self.new_type_propagator_error(
						*name_span,
						format_args!("The game function '{}' was not declared by mod_api.json", fn_name)
					);
				}
			},
			ExprData::Call{
				reciever: Some(_),
				name: _,
				args: _,
				ptr : _,
				name_span: _,
			} => {
				unimplemented!()
			}
			ExprData::Parenthesized(expr) => {
				self.fill_expr(helper_fns, export_fns, expr)?
			},
		};
		assignment_expr.result_type = Some(Box::leak(Box::new_in(result_ty, self.arena)));
		Ok(result_ty)
	}

	fn check_arguments(&mut self, 
		helper_fns: &[(&str, (GrugType<'arena>, &[Parameter<'arena>]))], 
		export_fns: &[(&str, &[Parameter<'arena>])], 
		function_name: &str, 
		name_span: SourceSpan, 
		signature: &[Parameter<'_>], 
		arguments: &mut [Expr<'arena>], 
	) -> Result<(), Error> {
		if signature.len() > arguments.len() {
			let param = signature[arguments.len()];
			return self.new_type_propagator_error(
				name_span,
				format_args!("Function call '{}' expected the argument '{}' with type {}", function_name, param.name.to_str(), param.ty)
			);
		} else if signature.len() < arguments.len() {
			let arg = &mut arguments[signature.len()];
			let got_type = self.fill_expr(helper_fns, export_fns, arg)?;
			return self.new_type_propagator_error(
				arg.span,
				format_args!("Function call '{}' got an unexpected extra argument with type {}", function_name, got_type)
			);
		}
		for (param, arg) in signature.iter().zip(arguments) {
			let arg_result_ty = self.fill_expr(helper_fns, export_fns, arg)?;
			// If argument is resource
			if let GrugType::Resource{extension} = param.ty 
				&& let ExprData::Resource(ref mut value) = arg.data {
				*value = self.validate_and_fix_resource_string(value.to_str(), extension.to_str(), arg.span)?.as_ntstrptr();
			// If argument is entity
			} else if let GrugType::Entity{entity_type: _} = param.ty 
				&& let ExprData::Entity(ref mut value) = arg.data {
				self.validate_and_fix_entity_string(value, arg.span)?;
			// argument is string but resource is expected
			} else if let GrugType::Resource{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return self.new_type_propagator_error(
					arg.span,
					format_args!("The host function '{}' expects a resource string, so put an 'r' in front of string \"{}\"", function_name, string)
				);
			// argument is string but entity is expected
			} else if let GrugType::Entity{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return self.new_type_propagator_error(
					arg.span,
					format_args!("The host function '{}' expects an entity string, so put an 'e' in front of string \"{}\"", function_name, string)
				);
			// if argument is void
			} else if &arg_result_ty == &GrugType::Void {
				return self.new_type_propagator_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got a function call that doesn't return anything", function_name, param.ty, param.name)
				);
			// id type coersion to id
			} else if let GrugType::Id{custom_name: None} = param.ty && let GrugType::Id{custom_name: _} = arg_result_ty {
				arg.result_type = Some(&GrugType::Id{custom_name: None});
			// mismatch
			} else if param.ty != arg_result_ty {
				return self.new_type_propagator_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got {}", function_name, param.ty, param.name, arg_result_ty)
				);
			}
		}
		Ok(())
	}

	fn validate_and_fix_resource_string(&mut self, value: &str, extension: &str, span: SourceSpan) -> Result<&'arena NTStr, Error> {
		if value.is_empty() {
			return self.new_type_propagator_error(
				span,
				format_args!("Resources can't be empty strings")
			);
		} else if value.starts_with("/") {
			return self.new_type_propagator_error(
				span,
				format_args!("Remove the leading slash from the resource \"{}\"", value)
			);
		} else if value.ends_with("/") {
			return self.new_type_propagator_error(
				span,
				format_args!("Remove the trailing slash from the resource \"{}\"", value)
			);
		} else if value.contains("\\") {
			return self.new_type_propagator_error(
				span,
				format_args!("Replace the '\\' with '/' in the resource \"{}\"", value)
			);
		} else if value.contains("//") {
			return self.new_type_propagator_error(
				span,
				format_args!("Replace the '//' with '/' in the resource \"{}\"", value)
			);
		} else if value == ".." || value.starts_with("../") 
		       || value.ends_with("/..") || value.contains("/../") {
			return self.new_type_propagator_error(
				span,
				format_args!("Remove the '..' from the resource \"{}\"", value)
			);
		} else if value == "." || value.starts_with("./") 
		       || value.ends_with("/.") || value.contains("/./") {
			return self.new_type_propagator_error(
				span,
				format_args!("Remove the '.' from the resource \"{}\"", value)
			);
		} else if value.ends_with(".") {
			return self.new_type_propagator_error(
				span,
				format_args!("resource name \"{}\" cannot end with .", value)
			);
		} else if value.ends_with(extension) {

		} else {
			return self.new_type_propagator_error(
				span,
				format_args!("The resource '{}' was supposed to have the extension '{}'", value, extension)
			);
		}
		// fix string
		let mut string = PathBuf::from(self.current_mod_name);
		string.push(value);
		let resource_str = Box::leak(NTStr::box_from_str_in(&format!("{}", string.display()), self.arena));

		// check if resource exists
		let mut full_path = PathBuf::from(self.mods_dir_path);
		full_path.push(resource_str.as_str());
		// we can't do `Ok(true) == std::fs::exists(&full_path)` because std::io::Error is not PartialEq
		if !std::fs::exists(&full_path).is_ok_and(std::convert::identity) {
			self.new_type_propagator_error(
				span,
				format_args!("resource '{}' does not exist", value)
			)
		} else {
			self.resources.push(resource_str.as_str().as_ref());
			Ok(resource_str)
		}
	}

	fn validate_and_fix_entity_string(&mut self, entity_string_old: &mut NTStrPtr<'arena>, span: SourceSpan) -> Result<(), Error> {
		let entity_string = entity_string_old.to_str();
		// Validate string
		if entity_string.is_empty() {
			return self.new_type_propagator_error(
				span,
				format_args!("Entities can't be empty strings")
			);
		}

		let (mod_name, entity_name) = if let Some((mod_name, entity_name)) = entity_string.split_once(":") {
			if mod_name.is_empty() {
				return self.new_type_propagator_error(
					span,
					format_args!("Entity '{}' is missing a mod name", entity_string)
				);
			}
			if entity_name.is_empty() {
				return self.new_type_propagator_error(
					span,
					format_args!("Entity '{}' missing entity name", entity_string)
				);
			}
			if mod_name == self.current_mod_name {
				return self.new_type_propagator_error(
					span,
					format_args!("Entity string ('{}') cannot refer to its own mod", entity_string)
				);
			}
			(mod_name, entity_name)
		} else {
			("", entity_string)
		};

		if let Some(ch) = mod_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return self.new_type_propagator_error(
				span,
				format_args!("Entity '{}' its mod name contains the invalid character '{}'", entity_string, ch)
			);
		}
		if let Some(ch) = entity_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return self.new_type_propagator_error(
				span,
				format_args!("Entity '{}' its entity name contains the invalid character '{}'", entity_string, ch)
			);
		}

		// Fix string
		// TODO: If the mod name is non utf8, this may cause problems
		// Cross mod entities are supported, so we actually need to handle this properly
		if mod_name.is_empty() {
			*entity_string_old = Box::leak(NTStr::box_from_str_in(&format!("{}:{}", self.current_mod_name.display(), entity_name), self.arena)).as_ntstrptr()
		}
		Ok(())
	}

	fn get_variable_type(&self, var_name: &str) -> Option<GrugType<'arena>> {
		if let var@Some(_) = self.get_local_variable_type(var_name) {
			var
		} else {
			self.get_global_variable_type(var_name)
		}
	}

	fn push_scope(&mut self) {
		self.local_variables.push(HashMap::new());
	}

	fn pop_scope(&mut self) {
		self.local_variables.pop().unwrap();
	}

	fn get_local_variable_type(&self, var_name: &str) -> Option<GrugType<'arena>> {
		for scope in self.local_variables.iter().rev() {
			if let var@Some(_) = scope.get(var_name) {
				return var.cloned();
			}
		}
		None
	}

	fn get_global_variable_type(&self, var_name: &str) -> Option<GrugType<'arena>> {
		self.global_variables.get(var_name).cloned()
	}

	fn add_local_variable(&mut self, name: &'arena str, ty: GrugType<'arena>, name_span: SourceSpan) -> Result<(), Error> {
		if self.get_global_variable_type(name).is_some() {
			return self.new_type_propagator_error(
				name_span,
				format_args!("The local variable '{}' shadows an earlier global variable", name),
			);
		}
		if self.get_local_variable_type(name).is_some() {
			return self.new_type_propagator_error(
				name_span,
				format_args!("The local variable '{}' shadows an earlier local variable", name),
			);
		}
		let result = self.local_variables.last_mut().expect("There is no local scope to push onto").insert(name, ty).is_none();
		debug_assert!(result);
		Ok(())
	}

	fn add_global_variable(&mut self, name: &'arena str, ty: GrugType<'arena>, name_span: SourceSpan) -> Result<(), Error> {
		match self.global_variables.entry(name) {
			Entry::Occupied(_) => return self.new_type_propagator_error(
				name_span,
				format_args!("The global variable '{}' shadows an earlier global variable", name),
			),
			Entry::Vacant(x) => {x.insert(ty);},
		}
		Ok(())
	}
}
