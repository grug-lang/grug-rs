use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ffi::OsStr;
use std::path::PathBuf;

use crate::error::{Error, ErrorKind, SourceSpan};
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
use crate::mod_api::{ModApiEntity, ModApi};

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

pub(super) struct TypePropogator<'mod_api, 'arena: 'temp, 'temp> {
	file_text: &'arena str,
	file_path: &'arena OsStr,
	entity: &'mod_api ModApiEntity<'mod_api>,
	mod_api: &'mod_api ModApi,
	current_mod_name: &'arena OsStr,
	mods_dir_path: &'mod_api OsStr,
	local_fns: &'arena [(&'arena str, (GrugType<'arena>, &'arena [Parameter<'arena>]))],
	export_fns: &'arena [(&'arena str, &'arena [Parameter<'arena>])],
	resources: Vec<&'arena OsStr, &'arena Arena>,
	global_variables: HashMap<&'arena str, GrugType<'arena>>,
	local_variables: Vec<HashMap<&'arena str, GrugType<'arena>>>,
	num_while_loops_deep: usize,
	current_fn_name: Option<&'arena str>,
	arena: &'arena Arena,
	temp_arena: &'temp Arena,
}

impl<'mod_api: 'arena, 'arena: 'temp, 'temp> TypePropogator<'mod_api, 'arena, 'temp> {
	// TODO: This should only be called with fill_result_types
	pub fn new (
		file_text: &'arena str,
		file_path: &'arena OsStr,
		entity: &'mod_api ModApiEntity, 
		mod_api: &'mod_api ModApi,
		mod_name: &'arena OsStr, 
		mods_dir_path: &'mod_api OsStr, 
		local_fns: &'arena [(&'arena str, (GrugType<'arena>, &'arena [Parameter<'arena>]))],
		export_fns: &'arena [(&'arena str, &'arena [Parameter<'arena>])],
		arena: &'arena Arena,
		temp_arena: &'temp Arena,
	) -> Self {
		Self {
			file_text,
			file_path,
			entity,
			mod_api,
			current_mod_name: mod_name,
			mods_dir_path,
			local_fns,
			export_fns,
			resources: Vec::new_in(arena),
			global_variables: HashMap::new(),
			local_variables: Vec::new(),
			num_while_loops_deep: 0,
			current_fn_name: None,
			arena,
			temp_arena
		}
	}

	#[track_caller]
	fn new_error<T>(&self, span: SourceSpan, args: std::fmt::Arguments) -> Result<T, Error> {
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
		mod_api: &'mod_api ModApi,
		mod_name: &'arena OsStr, 
		mods_dir_path: &'mod_api OsStr, 
		file_text: &'arena str,
		file_path: &'arena OsStr,
		entity_type: &str, 
		// We take this Ast by value because we fill in inconsistent data
		// temporarily which is not fixed if an error is returned.
		//
		// If we took a &mut Ast, the caller would be able to observe this
		// inconsitent state by just ignoring the error
		mut ast: Ast<'arena>,
		// This is the arena that contains the longlived data used by the ast
		arena: &'arena Arena,
		// This is the arena that contains the short lived data used only during type checking. 
		// It may be cleared as soon as this function returns
		temp_arena: &'temp Arena,
	) -> Result<(Ast<'arena>, &'arena [&'arena OsStr]), Error> {
		let mut type_propagator = Self::new(
			file_text, 
			file_path,
			entity, 
			mod_api,
			mod_name,
			mods_dir_path,
			&ast.local_fn_signatures,
			&ast.export_fn_signatures,
			arena,
			temp_arena,
		);

		type_propagator.global_variables.insert(
			nt!("me"), 
			GrugType::Id{
				name: type_propagator.arena.copy_str_into_nt(entity_type).as_ntstrptr(),
				generics: &[]
			}
		);

		let variables = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::Variable(x) => Some(x), _ => None});
		for variable in variables {
			type_propagator.check_global_expr(&variable.assignment_expr, variable.name.to_str())?;

			type_propagator.verify_generics(variable.ty, variable.type_span)?;
			let result_ty = type_propagator.fill_complete_expr(&mut variable.assignment_expr, Some(variable.ty))?;

			if let ExprData::Identifier(name) = &variable.assignment_expr.data 
				&& name.to_str() == "me" {
				return type_propagator.new_error(
					variable.assignment_expr.span,
					format_args!("Global variables can't be assigned 'me'")
				);
			}
			if variable.ty != result_ty {
				return type_propagator.new_error(
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
				return type_propagator.new_error(
					current_on_fn.span,
					format_args!("The function '{}' needs to be moved before or after a different export function, according to the entity '{}' in mod_api.json", current_on_fn.name.to_str(), entity_type)
				);
			}
			previous_on_fn_index = current_index;

			type_propagator.current_fn_name = Some(current_on_fn.name.to_str());
			
			if mod_api_on_fn.parameters.len() > current_on_fn.parameters.len() {
				let param = &mod_api_on_fn.parameters[current_on_fn.parameters.len()];
				return type_propagator.new_error(
					current_on_fn.span,
					format_args!("Function '{}' expected the parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				);
			} else if mod_api_on_fn.parameters.len() < current_on_fn.parameters.len() {
				let param = &current_on_fn.parameters[mod_api_on_fn.parameters.len()];
				return type_propagator.new_error(
					param.name_span,
					format_args!("Function '{}' got an unexpected extra parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				);
			}
			for (param, arg) in mod_api_on_fn.parameters.iter().zip(current_on_fn.parameters.iter()) {
				type_propagator.verify_generics(arg.ty, arg.type_span)?;
				if param.name != arg.name {
					return type_propagator.new_error(
						arg.name_span,
						format_args!("Function '{}' its '{}' parameter was supposed to be named '{}'", current_on_fn.name.to_str(), arg.name.to_str(), param.name.to_str())
					);
				}
				if param.ty != arg.ty {
					return type_propagator.new_error(
						arg.type_span,
						format_args!("Function '{}' its '{}' parameter was supposed to have the type {}, but got {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty, arg.ty)
					);
				}
			}
			type_propagator.push_scope();
			for param in current_on_fn.parameters {
				type_propagator.add_local_variable(param.name.to_str(), param.ty, param.name_span)?;
			}
			type_propagator.fill_statements(current_on_fn.body_statements, &GrugType::Void)?;
			type_propagator.pop_scope();

			debug_assert!(type_propagator.current_fn_name == Some(current_on_fn.name.to_str()));
			type_propagator.current_fn_name = None;
		}
		let entity_on_functions = &type_propagator.entity.export_fns;
		for on_fn in on_functions {
			let on_fn_name = on_fn.name.to_ntstr();
			if !entity_on_functions.iter().any(|(name, _)| *name == on_fn_name) {
				type_propagator.current_fn_name = Some(on_fn_name.as_str());
				return type_propagator.new_error(
					on_fn.span,
					format_args!("The function '{}' was not declared by entity '{}' in mod_api.json", on_fn_name, entity_type)
				);
			}
		}

		for statement in &mut *ast.global_statements {
			match statement {
				GlobalStatement::Variable(_) => (),
				GlobalStatement::OnFunction(_) => (),
				GlobalStatement::EmptyLine => (),
				GlobalStatement::HelperFunction(HelperFunction{
					name,
					parameters,
					body_statements,
					return_type,
					return_type_span: _,
					span,
				}) => {
					debug_assert!(type_propagator.local_variables.is_empty());
					debug_assert!(type_propagator.num_while_loops_deep == 0);
					debug_assert!(type_propagator.current_fn_name.is_none());

					let name = name.to_str();
					type_propagator.current_fn_name = Some(name);
					type_propagator.push_scope();
					for param in *parameters {
						type_propagator.verify_generics(param.ty, param.type_span)?;
						type_propagator.add_local_variable(param.name.to_str(), param.ty, param.name_span)?;
					}
					type_propagator.fill_statements(body_statements, return_type)?;

					if *return_type != GrugType::Void && !matches!(body_statements.last(), Some(Statement::Return{..})) {
						return type_propagator.new_error(
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
		Ok((ast, type_propagator.resources.leak()))
	}

	fn verify_generics(&self, ty: GrugType, err_span: SourceSpan) -> Result<(), Error> {
		// Check the number of generic parameters
		if let GrugType::Id{name, generics} = ty {
			if let Some(class) = self.mod_api.classes().get(name.to_str()) {
				if generics.len() != class.generics.len() {
					return self.new_error(
						err_span,
						format_args!("type {} has {} generics, but was given {}", name, class.generics.len(), generics.len()),
					)
				}
				for generic in generics {
					self.verify_generics(*generic, err_span)?;
				}
				Ok(())
			} else if !generics.is_empty() {
				self.new_error(
					err_span,
					format_args!("type {} has {} generics, but was given {}", name, 0, generics.len()),
				)
			} else {
				Ok(())
			}
		} else {
			Ok(())
		}
	}
	
	fn fill_statements(&mut self, statements: &mut [Statement<'arena>], expected_return_type: &GrugType<'arena>) -> Result<(), Error> {
		self.push_scope();
		for statement in statements {
			match statement {
				Statement::Variable{
					name,
					ty,
					type_span,
					assignment_expr,
					name_span,
				} => {
					
					if let Some(ty) = ty {
						self.verify_generics(**ty, *type_span)?;
						let result_ty = self.fill_complete_expr(assignment_expr, Some(**ty))?;
						self.add_local_variable(name.to_str(), **ty, *name_span)?;
						if **ty != result_ty {
							return self.new_error(
								assignment_expr.span,
								format_args!("Can't assign {} to '{}', which has type {}", result_ty, name, ty)
							);
						}
					} else {
						let result_ty = self.fill_complete_expr(assignment_expr, ty.copied())?;
						let ty = if let Some(ty) = self.get_global_variable_type(name.to_str()) {
							if matches!(ty, GrugType::Id {..}) {
								return self.new_error(
									assignment_expr.span,
									format_args!("Global id variables can't be reassigned")
								);
							}
							ty
						} else if let Some(ty) = self.get_local_variable_type(name.to_str()) {
							ty
						} else {
							return self.new_error(
								*name_span,
								format_args!("Can't assign to the variable '{}', since it does not exist", name)
							);
						};

						if ty != result_ty {
							return self.new_error(
								assignment_expr.span,
								format_args!("Can't assign {} to '{}', which has type {}", result_ty, name, ty)
							);
						}
					}
				}
				Statement::Call(expr) => {
					self.fill_complete_expr(expr, None)?;
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
						let cond_type = self.fill_complete_expr(condition, Some(GrugType::Bool))?;
						if cond_type != GrugType::Bool {
							return self.new_error(
								condition.span,
								format_args!("If condition must be bool but got '{}'", cond_type)
							);
						}
						self.fill_statements(if_block, expected_return_type)?;
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
								self.fill_statements(else_block, expected_return_type)?;
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
					let cond_type = self.fill_complete_expr(condition, Some(GrugType::Bool))?;
					if cond_type != GrugType::Bool {
						return self.new_error(
							condition.span,
							format_args!("While condition must be bool but got '{}'", cond_type)
						);
					}
					self.num_while_loops_deep += 1;
					self.fill_statements(block, expected_return_type)?;
					self.num_while_loops_deep -= 1;
				}
				Statement::Return {
					return_span,
					expr,
				} => {
					let (return_ty, span) = expr.as_mut()
						.map(|expr| Ok((self.fill_complete_expr(expr, Some(*expected_return_type))?, expr.span)))
						.unwrap_or(Ok((GrugType::Void, *return_span)))?;
					if *expected_return_type != return_ty {
						if return_ty == GrugType::Void {
							return self.new_error(
								span,
								format_args!("Function '{}' is supposed to return a value of type {}", self.current_fn_name.unwrap(), expected_return_type)
							);
						} else if *expected_return_type == GrugType::Void {
							return self.new_error(
								span,
								format_args!("Function '{}' wasn't supposed to return any value", self.current_fn_name.unwrap())
							);
						} else {
							return self.new_error(
								span,
								format_args!("Function '{}' is supposed to return {}, not {}", self.current_fn_name.unwrap(), expected_return_type, return_ty)
							);
						}
					}
				}
				Statement::Break(span) => {
					if self.num_while_loops_deep == 0 {
						return self.new_error(
							*span,
							format_args!("There is a break statement that isn't inside of a while loop")
						);
					}
				}
				Statement::Continue(span) => {
					if self.num_while_loops_deep == 0 {
						return self.new_error(
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

	// Check that the global variable's assigned value doesn't contain a call to a helper function nor identifier
	fn check_global_expr(&mut self, assignment_expr: &Expr<'_>, name: &str) -> Result<(), Error> {
		match &assignment_expr.data {
			ExprData::Entity(_)     |
			ExprData::Resource(_)   |
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
				receiver: None,
				name: fn_name,
				args,
				ptr : _,
				name_span: _,
			} => {
				let fn_name = fn_name.to_str();
				if fn_name.starts_with("_") {
					return self.new_error(
						assignment_expr.span,
						format_args!("The global variable '{}' isn't allowed to call local functions", name)
					);
				}
				args.iter().map(|argument| self.check_global_expr(argument, name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprData::Call{
				receiver: Some(_),
				name: _,
				args,
				ptr : _,
				name_span: _,
			} => {
				args.iter().map(|argument| self.check_global_expr(argument, name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprData::Parenthesized(expr) => self.check_global_expr(expr, name)?,
		}
		Ok(())
	}

	fn convert_mod_api_type<'a>(mod_api_type: GrugType<'mod_api>, replacements: &[GrugType<'a>], arena: &'a Arena) -> GrugType<'a> where 
		'mod_api: 'a
	{
		match mod_api_type {
			GrugType::Id {
				name,
				generics,
			} => GrugType::Id {
				name,
				generics: arena.slice_from_iter(generics.iter().map(|generic| Self::convert_mod_api_type(*generic, replacements, arena))),
			},
			// This refers to the index within the generics array of the current host function, not the index within the typing context
			GrugType::Existential{idx} => {
				replacements[idx]
			}
			_ => mod_api_type,
		}
	}

	/// Type inference in grug is limited to host function calls, 
	/// Variables do not have type inference (yet).
	///
	/// This means that each complete expression can be type checked independently (but in order).
	///
	/// This function creates a typing context for such an expression and typechecks it.
	/// 
	/// The basic flow of this function is as follows,
	///
	/// 1. Create a new typing context,
	/// 2. Walk the expression tree once
	/// 	- for each call to a generic host function, create new existential
	/// 	types for the generics used by that host function.
	/// 	- Emit constraints for the expressions
	/// 		- for function calls, add a constraint between the expected
	/// 		type of the parameter (which may or may not be generic) and the
	/// 		actual type of the expression, (which may or may not be generic)
	/// 	- For each constraint check if it is consistent with the preexisting constraints
	/// 		- Return an error if not
	/// 3. Recursively substitute all existentials with their actual types in the type context.
	/// 4. Walk the expression tree a second time in the exact same order
	/// 	- Create the new existentials again, but this time, substitute the
	/// 	calculated types from the previous steps as soon as the
	/// 	existentials are created
	fn fill_complete_expr(&mut self, expr: &mut Expr<'arena>, expected_type: Option<GrugType<'arena>>) -> Result<GrugType<'arena>, Error> {
		let mut ty_ctx = TyCtx::new(self.temp_arena);
		// First run through the expression, We do not have a list of substitutions.
		//
		// the `fill_expr` function writes the result of the expression into
		// the `result_type` field of all inner expressions The first time
		// through, we give it the `'temp` arena. 
		// 
		// This would require that `'temp` outlives `'arena'` which would
		// defeat the point of the temporary arena So we temporarily truncate
		// the lifetime of the expression to be `'temp` just for this call
		//
		// This would be unsound if the final AST ever contains an allocation
		// into the temporary arena but we ensure this never happens
		//
		// 1. If this entire function succeeds, the second call to fill_expr
		//    will replace all pointers into the `'temp` arena with pointers
		//    into the `'arena` arena, and callers never need to care about this.
		//
		// 2. If there is an error in any inner function, because we don't ever
		//    catch an error in the type propagator, we will return the error
		//    out of `fill_result_types`, and the caller will never see the
		//    inconsistent state of the AST. 
		//    The destructor of the AST will see the pointers into the `'temp`
		//    arena, but the arena will last at least as long as the call to
		//    `fill_result_types`
		let expr_type = self.fill_expr(&mut ty_ctx, None, unsafe{std::mem::transmute::<&mut Expr<'arena>, &mut Expr<'temp>>(expr)}, self.temp_arena)?;
		if let Some(expected_type) = expected_type {
			ty_ctx.add_constraint(expected_type, expr_type).map_err(|err| err.into_err(expr.span, self))?;
		}
		let substitutions = ty_ctx.substitute(self.arena);
		// Clear the typing context for the second pass. 
		// This time, the type context is only used to keep track of the number
		// of existentials that have been created
		
		self.fill_expr(&mut TyCtx::new(self.arena), Some(substitutions), expr, self.arena)
	}

	fn fill_expr<'a>(&mut self, ty_ctx: &mut TyCtx<'a>, substitutions: Option<&[GrugType<'arena>]>, assignment_expr: &mut Expr<'a>, arena: &'a Arena) -> Result<GrugType<'a>, Error> where
		'arena: 'a,
	{
		let result_ty = match &mut assignment_expr.data {
			ExprData::True => GrugType::Bool,
			ExprData::False => GrugType::Bool,
			ExprData::String{..} => GrugType::String,
			ExprData::Resource{..} => GrugType::Resource{extension: nt!("").as_ntstrptr()},
			ExprData::Entity{..} => GrugType::Entity{entity_type: None},
			ExprData::Identifier(name) => {
				let Some(ty) = self.get_variable_type(name.to_str()) else {
					return self.new_error(
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
					return self.new_error(
						*op_span,
						format_args!("Found '{0}' directly next to another '{0}', which can be simplified by just removing both of them", op)
					);
				}
				let result_ty = self.fill_expr(ty_ctx, substitutions, expr, arena)?;
				match (op, &result_ty) {
					(UnaryOperator::Not, GrugType::Bool) => (),
					(UnaryOperator::Not, GrugType::Existential{idx}) => {
						ty_ctx.add_constraint(GrugType::Bool, GrugType::Existential{idx: *idx}).map_err(|err| err.into_err(expr.span, self))?;
					}
					(UnaryOperator::Not, got) => return self.new_error(
						*op_span,
						format_args!("Found 'not' before {}, but it can only be put before a bool", got)
					),
					(UnaryOperator::Minus, GrugType::Number) => (),
					(UnaryOperator::Minus, GrugType::Existential{idx}) => {
						ty_ctx.add_constraint(GrugType::Number, GrugType::Existential{idx: *idx}).map_err(|err| err.into_err(expr.span, self))?
					}
					(UnaryOperator::Minus, got) => return self.new_error(
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
				let result_0 = self.fill_expr(ty_ctx, substitutions, left, arena)?;
				let result_1 = self.fill_expr(ty_ctx, substitutions, right, arena)?;
				match (&result_0, &result_1, *op) {
					(GrugType::String, GrugType::String, BinaryOperator::DoubleEquals) | 
					(GrugType::String, GrugType::String, BinaryOperator::NotEquals) => (),
					(GrugType::String, GrugType::String, BinaryOperator::Plus) => {
						return self.new_error(
							*op_span,
							format_args!("cannot add strings with '+'")
						);
					},
					(GrugType::String, GrugType::String, _) => {
						return self.new_error(
							*op_span,
							format_args!("You can't use the '{}' operator on strings", op)
						);
					},
					_ => (),
				}
				if result_0 != result_1 {
					return self.new_error(
						*op_span,
						format_args!("The left and right operand of a binary expression ('{}') must have the same type, but got {} and {}", op, result_0, result_1)
					);
				}

				match op {
					BinaryOperator::Or | BinaryOperator::And => {
						match result_0 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Bool, GrugType::Existential{idx}).map_err(|err| err.into_err(left.span, self))?,
							GrugType::Bool => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects bool", op)
								);
							}
						}
						match result_1 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Bool, GrugType::Existential{idx}).map_err(|err| err.into_err(right.span, self))?,
							GrugType::Bool => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects bool", op)
								);
							}
						}
						GrugType::Bool
					}
					BinaryOperator::DoubleEquals | BinaryOperator::NotEquals => {
						ty_ctx.add_constraint(result_0, result_1).map_err(|err| err.into_err(right.span, self))?;
						GrugType::Bool
					},
					BinaryOperator::Greater | BinaryOperator::GreaterEquals | 
					BinaryOperator::Less | BinaryOperator::LessEquals => {
						match result_0 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Number, GrugType::Existential{idx}).map_err(|err| err.into_err(right.span, self))?,
							GrugType::Number => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects number", op)
								);
							}
						}
						match result_1 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Number, GrugType::Existential{idx}).map_err(|err| err.into_err(right.span, self))?,
							GrugType::Number => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects number", op)
								);
							}
						}
						GrugType::Bool
					},
					BinaryOperator::Plus | BinaryOperator::Minus |
					BinaryOperator::Multiply | BinaryOperator::Division => {
						match result_0 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Number, GrugType::Existential{idx}).map_err(|err| err.into_err(right.span, self))?,
							GrugType::Number => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects number", op)
								);
							}
						}
						match result_1 {
							GrugType::Existential{idx} => ty_ctx.add_constraint(GrugType::Number, GrugType::Existential{idx}).map_err(|err| err.into_err(right.span, self))?,
							GrugType::Number => (),
							_ => {
								return self.new_error(
									*op_span,
									format_args!("'{}' operator expects number", op)
								);
							}
						}
						result_0
					},
				}
			},
			ExprData::Call{
				receiver: None,
				name: fn_name,
				args,
				ptr ,
				name_span,
			} => {
				let fn_name = fn_name.to_str();
				if let Some((_, (return_ty, sig_arguments))) = self.local_fns.iter().find(|(name, _)| *name == fn_name) {
					self.fill_arguments(fn_name, ty_ctx, substitutions, *name_span, sig_arguments, args, arena)?;
					*return_ty
				} else if let Some(host_fn) = self.mod_api.host_fns().get(fn_name) {
					// Create the actual types to represent generics
					let generics = if let Some(substitutions) = substitutions {
						// for the second time through, replace the existentials as they are created
						arena.slice_from_iter(host_fn.generics.iter().map(|generic| {
							let GrugType::Existential{idx} = ty_ctx.create_existential(*generic) else {unreachable!()};
							substitutions[idx]
						}))
					} else {
						// The first time through, just create the existentials
						arena.slice_from_iter(host_fn.generics.iter().map(|generic| {
							ty_ctx.create_existential(*generic)
						}))
					};

					// substitute generic arguments in host fn parameters with actual types (existentials the first time through)
					let parameters = arena.slice_from_iter(host_fn.parameters.iter().map(|param| {
						Parameter {
							ty: Self::convert_mod_api_type(param.ty, generics, arena),
							..*param
						}
					}));
					
					self.fill_arguments(fn_name, ty_ctx, substitutions, *name_span, parameters, args, arena)?;
					if substitutions.is_some() {
						if let Some(game_fn_ptr) = host_fn.fn_ptr {
							*ptr = Some(game_fn_ptr);
						} else {
							panic!("Game function {} was not registered (Note: This error is not triggerred by grug_tests)", fn_name);
							// return self.new_error(
							// 	*name_span,
							// 	format_args!("Game function {} was not registered", fn_name)
							// );
						}
					}
					Self::convert_mod_api_type(host_fn.return_ty, generics, arena)
				} else if fn_name.starts_with("_") {
					return self.new_error(
						*name_span,
						format_args!("The local function '{}' was not defined by this grug file", fn_name)
					);
				} else if self.export_fns.iter().any(|(name, _)| *name == fn_name) {
					return self.new_error(
						*name_span,
						format_args!("Mods aren't allowed to call their own export functions")
					);
				} else {
					return self.new_error(
						*name_span,
						format_args!("The game function '{}' was not declared by mod_api.json", fn_name)
					);
				}
			},
			ExprData::Call{
				receiver: Some(receiver),
				name,
				args,
				ptr,
				name_span,
			} => {
				let name = name.to_str();
				match &receiver.data {
					ExprData::Call {
						receiver: Some(_),
						..
					} => {
						return self.new_error(
							receiver.span,
							format_args!("Method chaining is not allowed")
						);
					}
					ExprData::Call {
						receiver: None,
						..
					} => {
						return self.new_error(
							receiver.span,
							format_args!("Cannot call method on the result of a function call")
						);
					}
					_ => (),
				};
				let receiver_type = self.fill_expr(ty_ctx, substitutions, receiver, arena)?;
				let receiver_name = match receiver_type {
					GrugType::Id{name, ..} => name.to_str(),
					GrugType::Existential{idx} => {
						let ty = ty_ctx.get_current_type(idx);
						match ty {
							Some(GrugType::Id{name, ..}) => name.to_str(),
							None => return self.new_error(
								receiver.span,
								format_args!("Unable to infer type of method receiver"),
							),
							Some(ty) => return self.new_error(
								receiver.span,
								format_args!("Cannot call method on '{}' type", ty)
							),
						}
					}
					ty => return self.new_error(
						receiver.span,
						format_args!("Cannot call method on '{}' type", ty)
					)
				};
				let Some(class) = self.mod_api.classes().get(receiver_name) else {
					return self.new_error(
						receiver.span,
						format_args!("Type '{}' does not have any methods", receiver_name)
					);
				};
				let Some((_, host_fn)) = class.methods.iter().find(|(fn_name, _)| fn_name.as_str() == name) else {
					return self.new_error(
						receiver.span,
						format_args!("Cannot find method '{}' on type '{}'", name, receiver_name)
					);
				};

				// Create the actual types to represent generics
				let generics = if let Some(substitutions) = substitutions {
					// for the second time through, replace the existentials as they are created
					arena.slice_from_iter(host_fn.generics.iter().map(|generic| {
						let GrugType::Existential{idx} = ty_ctx.create_existential(*generic) else {unreachable!()};
						substitutions[idx]
					}))
				} else {
					// The first time through, just create the existentials
					arena.slice_from_iter(host_fn.generics.iter().map(|generic| {
						ty_ctx.create_existential(*generic)
					}))
				};

				// substitute generic arguments in host fn parameters with actual types (existentials the first time through)
				let parameters = arena.slice_from_iter(host_fn.parameters.iter().map(|param| {
					Parameter {
						ty: Self::convert_mod_api_type(param.ty, generics, arena),
						..*param
					}
				}));

				// do the same for the method receiver, and add a constraint between that and the actual type of the receiver
				let mod_api_receiver_type = Self::convert_mod_api_type(class.ty, generics, arena);
				ty_ctx.add_constraint(mod_api_receiver_type, receiver_type).map_err(|err| err.into_err(receiver.span, self))?;
				
				self.fill_arguments(name, ty_ctx, substitutions, *name_span, parameters, args, arena)?;
				if let Some(fn_ptr) = host_fn.fn_ptr {
					*ptr = Some(fn_ptr);
				} else {
					// unregistered method
					panic!("This error is not triggerred by grug_tests");
					// return self.new_error(
					// 	*name_span,
					// 	format_args!("Game function {} was not registered", fn_name)
					// );
				}
				Self::convert_mod_api_type(host_fn.return_ty, generics, arena)
			}
			ExprData::Parenthesized(expr) => {
				self.fill_expr(ty_ctx, substitutions, expr, arena)?
			},
		};
		assignment_expr.result_type = Some(arena.alloc_into(result_ty));
		Ok(result_ty)
	}

	fn fill_arguments<'a>(&mut self, 
		function_name: &str, 
		ty_ctx: &mut TyCtx<'a>,
		substitutions: Option<&[GrugType<'arena>]>,
		name_span: SourceSpan, 
		signature: &[Parameter<'a>], 
		arguments: &mut [Expr<'a>], 
		arena: &'a Arena,
	) -> Result<(), Error> where
		'arena: 'a
	{
		if signature.len() > arguments.len() {
			let param = signature[arguments.len()];
			return self.new_error(
				name_span,
				format_args!("Function call '{}' expected the argument '{}' with type {}", function_name, param.name.to_str(), param.ty)
			);
		} else if signature.len() < arguments.len() {
			let arg = &mut arguments[signature.len()];
			let got_type = self.fill_expr(ty_ctx, substitutions, arg, arena)?;
			return self.new_error(
				arg.span,
				format_args!("Function call '{}' got an unexpected extra argument with type {}", function_name, got_type)
			);
		}
		for (param, arg) in signature.iter().zip(arguments) {
			let arg_result_ty = self.fill_expr(ty_ctx, substitutions, arg, arena)?;
			// If argument is resource
			if let GrugType::Resource{extension} = param.ty 
				&& let ExprData::Resource(ref mut value) = arg.data {
				*value = self.validate_and_fix_resource_string(value.to_str(), extension.to_str(), arg.span, arena)?.as_ntstrptr();
			// If argument is entity
			} else if let GrugType::Entity{entity_type: _} = param.ty 
				&& let ExprData::Entity(ref mut value) = arg.data {
				self.validate_and_fix_entity_string(value, arg.span, arena)?;
			// argument is a literal string but resource is expected
			} else if let GrugType::Resource{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return self.new_error(
					arg.span,
					format_args!("The host function '{}' expects a resource string, so put an 'r' in front of string \"{}\"", function_name, string)
				);
			// argument is a literal string but entity is expected
			} else if let GrugType::Entity{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return self.new_error(
					arg.span,
					format_args!("The host function '{}' expects an entity string, so put an 'e' in front of string \"{}\"", function_name, string)
				);
			// if argument is void
			} else if &arg_result_ty == &GrugType::Void {
				return self.new_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got a function call that doesn't return anything", function_name, param.ty, param.name)
				);
			// If the shape of the argument type matches the shape of the expected parameter including generics
			} else if arg_result_ty.matches(&param.ty) {
				ty_ctx.add_constraint(param.ty, arg_result_ty).map_err(|err| err.into_err(arg.span, self))?;
			// mismatch
			} else {
				return self.new_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got {}", function_name, param.ty, param.name, arg_result_ty)
				);
			}
		}
		Ok(())
	}

	fn validate_and_fix_resource_string<'a>(&mut self, value: &str, extension: &str, span: SourceSpan, arena: &'a Arena) -> Result<&'a NTStr, Error> {
		if value.is_empty() {
			return self.new_error(
				span,
				format_args!("Resources can't be empty strings")
			);
		} else if value.starts_with("/") {
			return self.new_error(
				span,
				format_args!("Remove the leading slash from the resource \"{}\"", value)
			);
		} else if value.ends_with("/") {
			return self.new_error(
				span,
				format_args!("Remove the trailing slash from the resource \"{}\"", value)
			);
		} else if value.contains("\\") {
			return self.new_error(
				span,
				format_args!("Replace the '\\' with '/' in the resource \"{}\"", value)
			);
		} else if value.contains("//") {
			return self.new_error(
				span,
				format_args!("Replace the '//' with '/' in the resource \"{}\"", value)
			);
		} else if value == ".." || value.starts_with("../") 
		       || value.ends_with("/..") || value.contains("/../") {
			return self.new_error(
				span,
				format_args!("Remove the '..' from the resource \"{}\"", value)
			);
		} else if value == "." || value.starts_with("./") 
		       || value.ends_with("/.") || value.contains("/./") {
			return self.new_error(
				span,
				format_args!("Remove the '.' from the resource \"{}\"", value)
			);
		} else if value.ends_with(".") {
			return self.new_error(
				span,
				format_args!("resource name \"{}\" cannot end with .", value)
			);
		} else if value.ends_with(extension) {

		} else {
			return self.new_error(
				span,
				format_args!("The resource '{}' was supposed to have the extension '{}'", value, extension)
			);
		}
		// fix string
		let mut string = PathBuf::from(self.current_mod_name);
		string.push(value);
		let resource_str = Box::leak(NTStr::box_from_str_in(&format!("{}", string.display()), arena));

		// check if resource exists
		let mut full_path = PathBuf::from(self.mods_dir_path);
		full_path.push(resource_str.as_str());
		// we can't do `Ok(true) == std::fs::exists(&full_path)` because std::io::Error is not PartialEq
		if !std::fs::exists(&full_path).is_ok_and(std::convert::identity) {
			self.new_error(
				span,
				format_args!("resource '{}' does not exist", value)
			)
		} else {
			self.resources.push(self.arena.copy_str_into(resource_str).as_ref());
			Ok(resource_str)
		}
	}

	fn validate_and_fix_entity_string<'a>(&mut self, entity_string_old: &mut NTStrPtr<'a>, span: SourceSpan, arena: &'a Arena) -> Result<(), Error> {
		let entity_string = entity_string_old.to_str();
		// Validate string
		if entity_string.is_empty() {
			return self.new_error(
				span,
				format_args!("Entities can't be empty strings")
			);
		}

		let (mod_name, entity_name) = if let Some((mod_name, entity_name)) = entity_string.split_once(":") {
			if mod_name.is_empty() {
				return self.new_error(
					span,
					format_args!("Entity '{}' is missing a mod name", entity_string)
				);
			}
			if entity_name.is_empty() {
				return self.new_error(
					span,
					format_args!("Entity '{}' missing entity name", entity_string)
				);
			}
			if mod_name == self.current_mod_name {
				return self.new_error(
					span,
					format_args!("Entity string ('{}') cannot refer to its own mod", entity_string)
				);
			}
			(mod_name, entity_name)
		} else {
			("", entity_string)
		};

		if let Some(ch) = mod_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return self.new_error(
				span,
				format_args!("Entity '{}' its mod name contains the invalid character '{}'", entity_string, ch)
			);
		}
		if let Some(ch) = entity_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return self.new_error(
				span,
				format_args!("Entity '{}' its entity name contains the invalid character '{}'", entity_string, ch)
			);
		}

		// Fix string
		// TODO: If the mod name is non utf8, this may cause problems
		// Cross mod entities are supported, so we actually need to handle this properly
		if mod_name.is_empty() {
			*entity_string_old = Box::leak(NTStr::box_from_str_in(&format!("{}:{}", self.current_mod_name.display(), entity_name), arena)).as_ntstrptr()
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
			return self.new_error(
				name_span,
				format_args!("The local variable '{}' shadows an earlier global variable", name),
			);
		}
		if self.get_local_variable_type(name).is_some() {
			return self.new_error(
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
			Entry::Occupied(_) => return self.new_error(
				name_span,
				format_args!("The global variable '{}' shadows an earlier global variable", name),
			),
			Entry::Vacant(x) => {x.insert(ty);},
		}
		Ok(())
	}
}

struct TyCtx<'a> {
	existentials: Vec<&'a str, &'a Arena>,
	substitutions: Vec<GrugType<'a>, &'a Arena>,
	constraints: Vec<(GrugType<'a>, GrugType<'a>), &'a Arena>,
}

#[derive(Clone, Copy)]
enum TypeInferenceError<'a> {
	Mismatch {
		left: GrugType<'a>,
		right: GrugType<'a>,
	},
	Resource,
	Entity,
}

impl<'a> TypeInferenceError<'a> {
	fn into_err(self, err_span: SourceSpan, context: &TypePropogator) -> Error {
		match self {
			Self::Mismatch{left, right} => context.new_error::<std::convert::Infallible>(
				err_span,
				format_args!("Expected {} but got {}", TypeDiff::new(left, right), TypeDiff::new(right, left)),
			),
			Self::Resource => context.new_error::<std::convert::Infallible>(
				err_span,
				format_args!("cannot use resource strings in generics"),
			),
			Self::Entity   => context.new_error::<std::convert::Infallible>(
				err_span,
				format_args!("cannot use entity strings in generics"),
			),
		}.unwrap_err()
	}
}

impl<'a> TyCtx<'a> {
	fn new(temp_arena: &'a Arena) -> Self {
		Self {
			existentials: Vec::new_in(temp_arena),
			substitutions: Vec::new_in(temp_arena),
			constraints: Vec::new_in(temp_arena),
		}
	}

	// Returns the first currently known replacement type for an existential
	fn get_current_type(&self, idx: usize) -> Option<GrugType<'a>> {
		fn get_current_type_inner<'a>(substitutions: &[GrugType<'a>], stack: StackLL<usize>) -> Option<GrugType<'a>> {
			match substitutions[stack.current] {
				GrugType::Existential{idx} => {
					let mut current = Some(&stack);
					while let Some(cur) = current {
						if **cur == idx {
							return None
						}
						current = cur.parent;
					}
					get_current_type_inner(substitutions, StackLL{current: idx, parent: Some(&stack)})
				},
				ty => Some(ty)
			}
		}
		get_current_type_inner(&self.substitutions, StackLL{current: idx, parent: None})
	}

	fn create_existential(&mut self, name: &'a str) -> GrugType<'static> {
		let new_existential = GrugType::Existential { idx: self.existentials.len() };
		self.existentials.push(name);
		self.substitutions.push(new_existential);
		new_existential
	}

	fn add_constraint(&mut self, left: GrugType<'a>, right: GrugType<'a>) -> Result<(), TypeInferenceError<'a>> {
		self.constraints.push((left, right));
		while let Some(constraint) = self.constraints.pop() {
			match constraint {
				(GrugType::Void, GrugType::Void) => (),
				(GrugType::Bool, GrugType::Bool) => (),
				(GrugType::Number, GrugType::Number) => (),
				(GrugType::String, GrugType::String) => (),
				(_, GrugType::Resource{..}) | 
				(GrugType::Resource{..}, _) => return Err(TypeInferenceError::Resource),
				(_, GrugType::Entity{..}) | 
				(GrugType::Entity{..}, _) => return Err(TypeInferenceError::Entity),
				(left@GrugType::Id{name: left_name, ..}, right@GrugType::Id{name: right_name, ..}) if left_name != right_name => {
					return Err(TypeInferenceError::Mismatch{left, right});
				}
				(GrugType::Id{generics: left_generics, ..}, GrugType::Id{generics: right_generics, ..}) => {
					assert_eq!(left_generics.len(), right_generics.len(), "You forgot to verify the number of generics on types");
					for (left, right) in left_generics.iter().zip(right_generics) {
						self.constraints.push((*left, *right));
					}
				}
				// An existential is always equal to itself
				(GrugType::Existential{idx: left_idx}, GrugType::Existential{idx: right_idx}) if left_idx == right_idx => (),
				// At least one side is an existential
				(GrugType::Existential{idx}, other) |
				(other, GrugType::Existential{idx}) => {
					// TODO, recursive `occurs` check
					// self.occurs_in(StackLL{current: idx, parent: None}, other)?;
					self.constraints.push((other, self.substitutions[idx]));
					self.substitutions[idx] = other;
				}
				(left, right) => {
					return Err(TypeInferenceError::Mismatch{left, right});
				}
			}
		}
		Ok(())
	}

	fn copy_type_into<'arena>(&self, ty: GrugType<'a>, parent_existentials: StackLL<usize>, arena: &'arena Arena) -> GrugType<'arena> {
		match ty {
			GrugType::Void => GrugType::Void,
			GrugType::Bool => GrugType::Bool,
			GrugType::Number => GrugType::Number,
			GrugType::String => GrugType::String,
			GrugType::Entity{entity_type: None} => GrugType::Entity{entity_type: None},
			GrugType::Resource{extension} => GrugType::Resource{extension: arena.copy_str_into_nt(extension.to_str()).as_ntstrptr()},
			GrugType::Entity{entity_type: Some(entity_type)} => GrugType::Entity{entity_type: Some(arena.copy_str_into_nt(entity_type.to_str()).as_ntstrptr())},
			GrugType::Existential{idx} => {
				let mut current = Some(&parent_existentials);
				while let Some(cur) = current {
					if **cur == idx {
						panic!("Infinitely recursive type found during unification");
					}
					current = cur.parent;
				}
				let return_type = self.copy_type_into(
					self.substitutions[idx], 
					StackLL{current: idx, parent: Some(&parent_existentials)}, 
					arena
				);
				return_type
			}
			GrugType::Id{name, generics} => GrugType::Id{
				name: arena.copy_str_into_nt(name.to_str()).as_ntstrptr(),
				generics: arena.slice_from_iter(generics.iter().map(|ty| self.copy_type_into(*ty, parent_existentials, arena))),
			},
		}
	}

	fn substitute<'arena>(&mut self, arena: &'arena Arena) -> &'arena [GrugType<'arena>] {
		// Copy all types into the permanent arena
		arena.slice_from_iter(self.substitutions.iter().enumerate().map(|(i, ty)| {
			self.copy_type_into(*ty, StackLL{current: i, parent: None}, arena)
		}))
	}
}

#[derive(Clone, Copy)]
struct StackLL<'a, T> {
	current: T,
	parent: Option<&'a StackLL<'a, T>>,
}

impl<'a, T> std::ops::Deref for StackLL<'a, T> {
	type Target = T;
	fn deref(&self) -> &T {
		&self.current
	}
}

/// Its Display implementation only shows the parts of the types that are different
struct TypeDiff<'a> {
	expected: GrugType<'a>,
	got     : GrugType<'a>,
}

impl<'a> TypeDiff<'a> {
	fn new(expected: GrugType<'a>, got: GrugType<'a>) -> Self {
		Self {
			expected,
			got
		}
	}
}

impl<'a> std::fmt::Display for TypeDiff<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		fn get_type_name(ty: GrugType<'_>) -> &str {
			match ty {
				GrugType::Void => "void",
				GrugType::Bool => "bool",
				GrugType::Number => "number",
				GrugType::String => "string",
				GrugType::Id {
					name,
					generics: _,
				} => name.to_str(),
				GrugType::Resource{..} => "resource",
				GrugType::Entity{..} => "entity",
				GrugType::Existential {..} => "_",
			}
		}

		f.write_str(get_type_name(self.expected))?;
		match (self.expected, self.got) {
			(
				left@GrugType::Id{name: name_left, generics: generics_left}, 
				right@GrugType::Id{name: name_right, generics: generics_right}
			) if name_left == name_right && !left.matches(&right) => {
				f.write_str("[")?;
				for (i, (expected, got)) in generics_left.iter().copied().zip(generics_right.iter().copied()).enumerate() {
					Self {
						expected,
						got
					}.fmt(f)?;
					if i != generics_left.len() - 1 {
						f.write_str(", ")?;
					}
				}
				f.write_str("]")?;
			}
			_ => (),
		}
		Ok(())
	}
}
