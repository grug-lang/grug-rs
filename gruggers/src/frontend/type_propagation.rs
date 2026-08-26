use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ffi::OsStr;
use std::path::PathBuf;

use crate::error::{Error, ErrorKind, SourceSpan};
use crate::ntstring::{NTStr, NTStrPtr};
use crate::ast::{
	Type, UnaryOperator, BinaryOperator,
	ExprData, HelperFunction, Statement, Expr,
	Parameter,
};
use crate::frontend::GlobalStatement;
use crate::nt;
use crate::arena::Arena;
use crate::frontend::parser::Ast;
use crate::mod_api::{ModApiEntity, ModApi, Trait, Generic};
use crate::type_storage::TypeStorage;

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

pub(super) struct TypePropagator<'mod_api, 'arena: 'temp, 'temp> {
	file_text: &'arena str,
	file_path: &'arena OsStr,
	entity: &'mod_api ModApiEntity<'mod_api>,
	mod_api: &'mod_api ModApi,
	current_mod_name: &'arena OsStr,
	mods_dir_path: &'mod_api OsStr,
	local_fns: &'arena [(&'arena str, (Type<'arena>, &'arena [Parameter<'arena>]))],
	export_fns: &'arena [(&'arena str, &'arena [Parameter<'arena>])],
	resources: Vec<&'arena OsStr, &'arena Arena>,
	global_variables: HashMap<&'arena str, Type<'arena>>,
	local_variables: Vec<HashMap<&'arena str, Type<'arena>>>,
	num_while_loops_deep: usize,
	current_fn_name: Option<&'arena str>,
	arena: &'arena Arena,
	temp_arena: &'temp Arena,
	type_storage: &'temp mut TypeStorage,
}

struct TypeMismatch<'a> {
	span: SourceSpan,
	diff: TypeDiff<'a>,
}

enum TypeInferenceError<'a> {
	Error(Error),
	Mismatch(TypeMismatch<'a>),
}

impl<'a> From<TypeMismatch<'a>> for TypeInferenceError<'a> {
	fn from(other: TypeMismatch<'a>) -> Self {
		Self::Mismatch(other)
	}
}

impl<'a> From<Error> for TypeInferenceError<'a> {
	fn from(other: Error) -> Self {
		Self::Error(other)
	}
}

impl<'mod_api: 'arena, 'arena: 'temp, 'temp> TypePropagator<'mod_api, 'arena, 'temp> {
	// TODO: This should only be called within fill_result_types
	pub fn new (
		file_text: &'arena str,
		file_path: &'arena OsStr,
		entity: &'mod_api ModApiEntity, 
		mod_api: &'mod_api ModApi,
		mod_name: &'arena OsStr, 
		mods_dir_path: &'mod_api OsStr, 
		local_fns: &'arena [(&'arena str, (Type<'arena>, &'arena [Parameter<'arena>]))],
		export_fns: &'arena [(&'arena str, &'arena [Parameter<'arena>])],
		arena: &'arena Arena,
		temp_arena: &'temp Arena,
		type_storage: &'temp mut TypeStorage,
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
			temp_arena,
			type_storage,
		}
	}

	#[track_caller]
	fn new_error(&self, span: SourceSpan, args: std::fmt::Arguments) -> Error {
		Error::new(
			ErrorKind::TYPE_CHECKER_ERROR,
			self.current_fn_name.unwrap_or("member scope"),
			self.file_path,
			self.file_text, 
			span,
			args
		)
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
		type_storage: &'temp mut TypeStorage,
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
			type_storage,
		);

		type_propagator.global_variables.insert(
			nt!("me"), 
			Type::Id{
				name: type_propagator.arena.copy_str_into_nt(entity_type).as_ntstrptr(),
				generics: &[]
			}
		);

		let variables = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::Variable(x) => Some(x), _ => None});
		for variable in variables {
			type_propagator.verify_generics(variable.ty, variable.type_span)?;
			type_propagator.check_global_expr(&variable.assignment_expr, variable.name.to_str())?;

			let result_ty = type_propagator.fill_complete_expr(&mut variable.assignment_expr, Some(variable.ty)).map_err(|err| match err {
				TypeInferenceError::Error(err) => err,
				TypeInferenceError::Mismatch(mismatch) => type_propagator.new_error(
					mismatch.span,
					format_args!("Can't assign {} to '{}', which has type {}", mismatch.diff.swapped(), variable.name, mismatch.diff)
				)
			})?;

			if let ExprData::Identifier(name) = &variable.assignment_expr.data 
				&& name.to_str() == "me" {
				return Err(type_propagator.new_error(
					variable.assignment_expr.span,
					format_args!("Global variables can't be assigned 'me'")
				));
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
				return Err(type_propagator.new_error(
					current_on_fn.span,
					format_args!("The function '{}' needs to be moved before or after a different export function, according to the entity '{}' in mod_api.json", current_on_fn.name.to_str(), entity_type)
				));
			}
			previous_on_fn_index = current_index;

			type_propagator.current_fn_name = Some(current_on_fn.name.to_str());
			
			if mod_api_on_fn.parameters.len() > current_on_fn.parameters.len() {
				let param = &mod_api_on_fn.parameters[current_on_fn.parameters.len()];
				return Err(type_propagator.new_error(
					current_on_fn.span,
					format_args!("Function '{}' expected the parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				));
			} else if mod_api_on_fn.parameters.len() < current_on_fn.parameters.len() {
				let param = &current_on_fn.parameters[mod_api_on_fn.parameters.len()];
				return Err(type_propagator.new_error(
					param.name_span,
					format_args!("Function '{}' got an unexpected extra parameter '{}' with type {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty)
				));
			}
			for (param, arg) in mod_api_on_fn.parameters.iter().zip(current_on_fn.parameters.iter()) {
				type_propagator.verify_generics(arg.ty, arg.type_span)?;
				if param.name != arg.name {
					return Err(type_propagator.new_error(
						arg.name_span,
						format_args!("Function '{}' its '{}' parameter was supposed to be named '{}'", current_on_fn.name.to_str(), arg.name.to_str(), param.name.to_str())
					));
				}
				if param.ty != arg.ty {
					return Err(type_propagator.new_error(
						arg.type_span,
						format_args!("Function '{}' its '{}' parameter was supposed to have the type {}, but got {}", current_on_fn.name.to_str(), param.name.to_str(), param.ty, arg.ty)
					));
				}
			}
			type_propagator.push_scope();
			for param in current_on_fn.parameters {
				type_propagator.add_local_variable(param.name.to_str(), param.ty, param.name_span)?;
			}
			type_propagator.fill_statements(current_on_fn.body_statements, &Type::Void)?;
			type_propagator.pop_scope();

			debug_assert!(type_propagator.current_fn_name == Some(current_on_fn.name.to_str()));
			type_propagator.current_fn_name = None;
		}
		let entity_on_functions = &type_propagator.entity.export_fns;
		for on_fn in on_functions {
			let on_fn_name = on_fn.name.to_ntstr();
			if !entity_on_functions.iter().any(|(name, _)| *name == on_fn_name) {
				type_propagator.current_fn_name = Some(on_fn_name.as_str());
				return Err(type_propagator.new_error(
					on_fn.span,
					format_args!("The function '{}' was not declared by entity '{}' in mod_api.json", on_fn_name, entity_type)
				));
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

					if *return_type != Type::Void && !matches!(body_statements.last(), Some(Statement::Return{..})) {
						return Err(type_propagator.new_error(
							*span,
							format_args!("Function '{}' is supposed to return {} as its last line", name, return_type)
						));
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

	fn verify_generics(&self, ty: Type, err_span: SourceSpan) -> Result<(), Error> {
		// Check the number of generic parameters
		if let Type::Id{name, generics} = ty {
			if let Some(class) = self.mod_api.classes().get(name.to_str()) {
				if generics.len() != class.generics.len() {
					return Err(self.new_error(
						err_span,
						format_args!("type {} has {} generics, but was given {}", name, class.generics.len(), generics.len()),
					))
				}
				for generic in generics {
					self.verify_generics(*generic, err_span)?;
				}
				Ok(())
			} else if !generics.is_empty() {
				Err(self.new_error(
					err_span,
					format_args!("type {} has {} generics, but was given {}", name, 0, generics.len()),
				))
			} else {
				Ok(())
			}
		} else {
			Ok(())
		}
	}
	
	fn fill_statements(&mut self, statements: &mut [Statement<'arena>], expected_return_type: &Type<'arena>) -> Result<(), Error> {
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
						self.fill_complete_expr(assignment_expr, Some(**ty)).map_err(|err| match err {
							TypeInferenceError::Error(err) => err,
							TypeInferenceError::Mismatch(mismatch) => self.new_error(
								mismatch.span,
								format_args!("Can't assign {} to '{}', which has type {}", mismatch.diff.swapped(), name, mismatch.diff)
							)
						})?;
						self.add_local_variable(name.to_str(), **ty, *name_span)?;
					} else {
						let ty = if let Some(ty) = self.get_global_variable_type(name.to_str()) {
							if matches!(ty, Type::Id {..}) {
								return Err(self.new_error(
									assignment_expr.span,
									format_args!("Global id variables can't be reassigned")
								));
							}
							ty
						} else if let Some(ty) = self.get_local_variable_type(name.to_str()) {
							ty
						} else {
							return Err(self.new_error(
								*name_span,
								format_args!("Can't assign to the variable '{}', since it does not exist", name)
							));
						};

						self.fill_complete_expr(assignment_expr, Some(ty)).map_err(|err| match err {
							TypeInferenceError::Error(err) => err,
							TypeInferenceError::Mismatch(mismatch) => self.new_error(
								mismatch.span,
								format_args!("Can't assign {} to '{}', which has type {}", mismatch.diff.swapped(), name, mismatch.diff)
							)
						})?;
					}
				}
				Statement::Call(expr) => {
					self.fill_complete_expr(expr, None).map_err(|err| match err {
						TypeInferenceError::Error(err) => err,
						TypeInferenceError::Mismatch(_) => unreachable!(),
					})?;
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
						self.fill_complete_expr(condition, Some(Type::Bool)).map_err(|err| match err {
							TypeInferenceError::Error(err) => err,
							TypeInferenceError::Mismatch(mismatch) => self.new_error(
								mismatch.span,
								format_args!("If condition must be bool but got '{}'", mismatch.diff.swapped())
							)
						})?;
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
					self.fill_complete_expr(condition, Some(Type::Bool)).map_err(|err| match err {
						TypeInferenceError::Error(err) => err,
						TypeInferenceError::Mismatch(mismatch) => self.new_error(
							mismatch.span,
							format_args!("While condition must be bool but got '{}'", mismatch.diff.swapped())
						)
					})?;
					self.num_while_loops_deep += 1;
					self.fill_statements(block, expected_return_type)?;
					self.num_while_loops_deep -= 1;
				}
				Statement::Return {
					return_span,
					expr,
				} => {
					if let Some(expr) = expr {
						self.fill_complete_expr(expr, Some(*expected_return_type)).map_err(|err| match err {
							TypeInferenceError::Error(err) => err,
							TypeInferenceError::Mismatch(mismatch) if mismatch.diff.print == Type::Void => self.new_error(
								mismatch.span,
								format_args!("Function '{}' wasn't supposed to return any value but it returned {}", self.current_fn_name.unwrap(), mismatch.diff.swapped())
							),
							TypeInferenceError::Mismatch(mismatch) => self.new_error(
								mismatch.span,
								format_args!("Function '{}' is supposed to return {}, not {}", self.current_fn_name.unwrap(), mismatch.diff, mismatch.diff.swapped())
							),
						})?;
					} else {
						if *expected_return_type != Type::Void {
							return Err(self.new_error(
								*return_span,
								format_args!("Function '{}' is supposed to return a value of type {}", self.current_fn_name.unwrap(), expected_return_type)
							));
						}
					}
				}
				Statement::Break(span) => {
					if self.num_while_loops_deep == 0 {
						return Err(self.new_error(
							*span,
							format_args!("There is a break statement that isn't inside of a while loop")
						));
					}
				}
				Statement::Continue(span) => {
					if self.num_while_loops_deep == 0 {
						return Err(self.new_error(
							*span,
							format_args!("There is a continue statement that isn't inside of a while loop")
						));
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
				generics: _,
			} => {
				let fn_name = fn_name.to_str();
				if fn_name.starts_with("_") {
					return Err(self.new_error(
						assignment_expr.span,
						format_args!("The global variable '{}' isn't allowed to call local functions", name)
					));
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
				generics: _,
			} => {
				args.iter().map(|argument| self.check_global_expr(argument, name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprData::Parenthesized(expr) => self.check_global_expr(expr, name)?,
		}
		Ok(())
	}

	fn convert_mod_api_type<'a>(mod_api_type: Type<'mod_api>, replacements: &[Type<'a>], arena: &'a Arena) -> Type<'a> where 
		'mod_api: 'a
	{
		match mod_api_type {
			Type::Id {
				name,
				generics,
			} => Type::Id {
				name,
				generics: arena.slice_from_iter(generics.iter().map(|generic| Self::convert_mod_api_type(*generic, replacements, arena))),
			},
			// This refers to the index within the generics array of the current host function, not the index within the typing context
			Type::Existential{idx} => {
				replacements[idx]
			}
			_ => mod_api_type,
		}
	}

	/// Type inference in grug is limited to host function calls, 
	/// Variables do not have type inference (yet).
	///
	/// This means that each complete expression can be type checked
	/// independently (but in order).
	///
	/// This function creates a typing context for such an expression and
	/// typechecks it.
	/// 
	/// The basic flow of this function is as follows:
	///
	/// 1. Create a new typing context,
	/// 2. Walk the expression tree once
	/// 	- For each call to a generic host function, create new existential
	/// 	  types for the generics used by that host function.
	/// 	- Emit constraints for the expressions.
	/// 		- For function calls, add a constraint between the expected
	/// 		  type of the parameter (which may or may not be generic) and the
	/// 		  actual type of the expression (which may or may not be generic).
	/// 	- For each constraint check if it is consistent with the preexisting constraints.
	/// 		- Return an error if not.
	/// 3. Recursively substitute all existentials with their actual types in the type context.
	/// 4. Walk the expression tree a second time in the exact same order.
	/// 	- Create the new existentials again, but this time, substitute the
	/// 	  calculated types from the previous steps as soon as the
	/// 	  existentials are created.
	///
	/// see
	/// (this)[https://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/]
	/// blog post for an explanation of how constraints work
	fn fill_complete_expr(&mut self, expr: &mut Expr<'arena>, expected_type: Option<Type<'arena>>) -> Result<Type<'arena>, TypeInferenceError<'temp>> {
		let mut ty_ctx = TyCtx::new(self.current_fn_name.unwrap_or("member scope"), self.file_path, self.file_text, self.temp_arena);
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
			ty_ctx.add_constraint(expr.span, expected_type, expr_type)?;
		}
		let substitutions = ty_ctx.substitute(&mut self.type_storage, self.arena)?;
		let substitutions = self.type_storage.insert_type_list(substitutions);
		// Clear the typing context for the second pass. 
		// This time, the type context is only used to keep track of the number
		// of existentials that have been created
		
		Ok(self.fill_expr(&mut TyCtx::new(self.current_fn_name.unwrap_or("member scope"), self.file_path, self.file_text, self.arena), Some(substitutions), expr, self.arena)?)
	}

	fn fill_expr<'a>(&mut self, ty_ctx: &mut TyCtx<'a, 'arena>, substitutions: Option<&[Type<'static>]>, assignment_expr: &mut Expr<'a>, arena: &'a Arena) -> Result<Type<'a>, Error> where
		'arena: 'a,
	{
		let result_ty = match &mut assignment_expr.data {
			ExprData::True => Type::Bool,
			ExprData::False => Type::Bool,
			ExprData::String{..} => Type::String,
			ExprData::Resource{..} => Type::Resource{extension: nt!("").as_ntstrptr()},
			ExprData::Entity{..} => Type::Entity{entity_type: None},
			ExprData::Identifier(name) => {
				let Some(ty) = self.get_variable_type(name.to_str()) else {
					return Err(self.new_error(
						assignment_expr.span,
						format_args!("The variable '{}' does not exist", name.to_str())
					));
				};
				ty
			},
			ExprData::Number{
				..
			} => Type::Number,
			ExprData::Unary{
				op,
				expr,
				op_span,
			} => {
				if let Expr{data: ExprData::Unary{op: next_op, ..}, ..} = expr && next_op == op {
					return Err(self.new_error(
						*op_span,
						format_args!("Found '{0}' directly next to another '{0}', which can be simplified by just removing both of them", op)
					));
				}
				let result_ty = self.fill_expr(ty_ctx, substitutions, expr, arena)?;
				let expected = match op {
					UnaryOperator::Not   => Type::Bool,
					UnaryOperator::Minus => Type::Number,
				};
				ty_ctx.add_constraint(*op_span, expected, result_ty).map_err(|err| self.new_error(
					err.span,
					format_args!("Found '{}' before {}, but it can only be put before a {}", op, err.diff.swapped(), expected)
				))?;
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
				ty_ctx.add_constraint(*op_span, result_0, result_1).map_err(|err| self.new_error(
					err.span,
					format_args!("The left and right operand of a binary expression ('{}') must have the same type, but got {} and {}", op, err.diff, err.diff.swapped())
				))?;
				// If the types are strings, we want to find out here
				let result_0 = if let Some(ty) = ty_ctx.get_current_type(result_0) {ty} else {result_0};
				let result_1 = if let Some(ty) = ty_ctx.get_current_type(result_1) {ty} else {result_1};

				match (&result_0, &result_1, *op) {
					(Type::String, Type::String, BinaryOperator::DoubleEquals) | 
					(Type::String, Type::String, BinaryOperator::NotEquals) => (),
					(Type::String, Type::String, BinaryOperator::Plus) => {
						return Err(self.new_error(
							*op_span,
							format_args!("cannot add strings with '+'")
						));
					},
					(Type::String, Type::String, _) => {
						return Err(self.new_error(
							*op_span,
							format_args!("You can't use the '{}' operator on strings", op)
						));
					},
					_ => (),
				}

				let (expected_type, result_type) = match op {
					BinaryOperator::Or | BinaryOperator::And => (Type::Bool, Type::Bool),
					BinaryOperator::DoubleEquals | BinaryOperator::NotEquals => (result_0, Type::Bool),
					BinaryOperator::Greater  | BinaryOperator::GreaterEquals | 
					BinaryOperator::Less     | BinaryOperator::LessEquals    => (Type::Number, Type::Bool),
					BinaryOperator::Plus     | BinaryOperator::Minus         |
					BinaryOperator::Multiply | BinaryOperator::Division      => (Type::Number, Type::Number),
				};
				// Make sure both the left and right expressions have the expected type
				for (expr_result, expr_span) in [(result_0, left.span), (result_1, right.span)] {
					ty_ctx.add_constraint(expr_span, expected_type, expr_result).map_err(|err| self.new_error(
						*op_span,
						format_args!("'{}' operator expects {} but got {}", op, expected_type, err.diff.swapped())
					))?;
				}
				result_type
			},
			ExprData::Call{
				receiver: None,
				name,
				args,
				ptr ,
				name_span,
				generics: final_generics,
			} => {
				let name = name.to_str();
				if let Some((_, (return_ty, sig_arguments))) = self.local_fns.iter().find(|(fn_name, _)| *fn_name == name) {
					self.fill_arguments(name, ty_ctx, substitutions, *name_span, sig_arguments, args, arena)?;
					*return_ty
				} else if let Some(host_fn) = self.mod_api.host_fns().get(name) {
					// Create the actual types to represent generics
					let generics = if let Some(substitutions) = substitutions {
						// for the second time through, replace the existentials as they are created, and also verify traits
						let mut generics = Vec::with_capacity_in(host_fn.generics.len(), arena);
						for generic in host_fn.generics {
							let Type::Existential{idx} = ty_ctx.create_existential(name, *name_span) else {unreachable!()};
							let actual_ty = substitutions[idx];
							ty_ctx.verify_traits(actual_ty, generic.traits(), *name_span, name)?;
							generics.push(actual_ty)
						}
						generics.leak()
					} else {
						// The first time through, just create the existentials
						arena.slice_from_iter(host_fn.generics.iter().map(|_| {
							ty_ctx.create_existential(name, *name_span)
						}))
					};

					// substitute generic arguments in host fn parameters with actual types (existentials the first time through)
					let parameters = arena.slice_from_iter(host_fn.parameters.iter().map(|param| {
						Parameter {
							ty: Self::convert_mod_api_type(param.ty, generics, arena),
							..*param
						}
					}));
					
					self.fill_arguments(name, ty_ctx, substitutions, *name_span, parameters, args, arena)?;

					// only fill in the host function pointer the second time
					// through.
					if substitutions.is_some() {
						*final_generics = self.type_storage.insert_type_list(generics);
						if let Some(host_fn_ptr) = host_fn.fn_ptr {
							*ptr = Some(host_fn_ptr);
						} else if let Some(fn_registerer) = host_fn.registerer {
							let result = unsafe{fn_registerer(generics.as_ptr())};
							if let Some(result) = result {
								*ptr = Some(result);
							} else {
								return Err(self.new_error(
									*name_span,
									format_args!("generic function '{}' failed instantiation for types {}", name, TypeListDisplay(generics))
								));
							}
						} else {
							panic!("function {} was not registered (Note: This error is not triggerred by grug_tests)", name);
						}
					}
					Self::convert_mod_api_type(host_fn.return_ty, generics, arena)
				} else if name.starts_with("_") {
					return Err(self.new_error(
						*name_span,
						format_args!("The local function '{}' was not defined by this grug file", name)
					));
				} else if self.export_fns.iter().any(|(fn_name, _)| *fn_name == name) {
					return Err(self.new_error(
						*name_span,
						format_args!("Mods aren't allowed to call their own export functions")
					));
				} else {
					return Err(self.new_error(
						*name_span,
						format_args!("The game function '{}' was not declared by mod_api.json", name)
					));
				}
			},
			ExprData::Call{
				receiver: Some(receiver),
				name,
				args,
				ptr,
				name_span,
				generics: final_generics,
			} => {
				let name = name.to_str();
				let receiver_type = self.fill_expr(ty_ctx, substitutions, receiver, arena)?;
				// We want to at least know the first level of the type is known
				let receiver_type = if let Some(ty) = ty_ctx.get_current_type(receiver_type) {ty} else {
					return Err(self.new_error(
						receiver.span,
						format_args!("Unable to infer type of method receiver"),
					));
				};
				
				let receiver_name = match receiver_type {
					Type::Id{name, ..} => name.to_str(),
					ty => return Err(self.new_error(
						receiver.span,
						format_args!("Cannot call method on '{}' type", ty)
					))
				};
				let Some(class) = self.mod_api.classes().get(receiver_name) else {
					return Err(self.new_error(
						receiver.span,
						format_args!("Type '{}' does not have any methods", receiver_name)
					));
				};
				let Some((_, host_fn)) = class.methods.iter().find(|(fn_name, _)| fn_name.as_str() == name) else {
					return Err(self.new_error(
						receiver.span,
						format_args!("Cannot find method '{}' on type '{}'", name, receiver_name)
					));
				};

				// Create the actual types to represent generics
				let generics = if let Some(substitutions) = substitutions {
					// for the second time through, replace the existentials as they are created, and also verify traits
					let mut generics = Vec::with_capacity_in(host_fn.generics.len(), arena);
					for generic in host_fn.generics {
						let Type::Existential{idx} = ty_ctx.create_existential(name, *name_span) else {unreachable!()};
						let actual_ty = substitutions[idx];
						ty_ctx.verify_traits(actual_ty, generic.traits(), *name_span, name)?;
						generics.push(actual_ty)
					}
					generics.leak()
				} else {
					// The first time through, just create the existentials
					arena.slice_from_iter(host_fn.generics.iter().map(|_| {
						ty_ctx.create_existential(name, *name_span)
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
				// TODO: Fix the error message here (i actually don't know if this can even error)
				ty_ctx.add_constraint(receiver.span, mod_api_receiver_type, receiver_type).map_err(|err| self.new_error(
					err.span,
					format_args!("Expected {} but got {}", err.diff, err.diff.swapped())
				))?;
				
				self.fill_arguments(name, ty_ctx, substitutions, *name_span, parameters, args, arena)?;

				// only fill in the host function pointer the second time
				// through.
				if substitutions.is_some() {
					*final_generics = self.type_storage.insert_type_list(generics);
					// non generic functions directly use the function
					// from the host function data
					if let Some(host_fn_ptr) = host_fn.fn_ptr {
						*ptr = Some(host_fn_ptr);
					} else if let Some(fn_registerer) = host_fn.registerer {
						let result = unsafe{fn_registerer(generics.as_ptr())};
						if let Some(result) = result {
							*ptr = Some(result);
						} else {
							return Err(self.new_error(
								*name_span,
								format_args!("generic method '{}.{}' failed instantiation for types {}", receiver_name, name, TypeListDisplay(generics))
							));
						}
					} else {
						panic!("method {}.{} was not registered (Note: This error is not triggerred by grug_tests)", receiver_name, name);
					}
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
		ty_ctx: &mut TyCtx<'a, 'arena>,
		substitutions: Option<&[Type<'static>]>,
		name_span: SourceSpan, 
		signature: &[Parameter<'a>], 
		arguments: &mut [Expr<'a>], 
		arena: &'a Arena,
	) -> Result<(), Error> where
		'arena: 'a
	{
		if signature.len() > arguments.len() {
			let param = signature[arguments.len()];
			return Err(self.new_error(
				name_span,
				format_args!("Function call '{}' expected the argument '{}' with type {}", function_name, param.name.to_str(), param.ty)
			));
		} else if signature.len() < arguments.len() {
			let arg = &mut arguments[signature.len()];
			let got_type = self.fill_expr(ty_ctx, substitutions, arg, arena)?;
			return Err(self.new_error(
				arg.span,
				format_args!("Function call '{}' got an unexpected extra argument with type {}", function_name, got_type)
			));
		}
		for (param, arg) in signature.iter().zip(arguments) {
			let arg_result_ty = self.fill_expr(ty_ctx, substitutions, arg, arena)?;
			// If argument is resource
			if let Type::Resource{extension} = param.ty 
				&& let ExprData::Resource(ref mut value) = arg.data {
				if let Some(_) = substitutions {
					*value = self.validate_and_fix_resource_string(value.to_str(), extension.to_str(), arg.span, arena)?.as_ntstrptr();
				}
			// If argument is entity
			} else if let Type::Entity{entity_type: _} = param.ty 
				&& let ExprData::Entity(ref mut value) = arg.data {
				if let Some(_) = substitutions {
					self.validate_and_fix_entity_string(value, arg.span, arena)?;
				}
			// argument is a literal string but resource is expected
			} else if let Type::Resource{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return Err(self.new_error(
					arg.span,
					format_args!("The host function '{}' expects a resource string, so put an 'r' in front of string \"{}\"", function_name, string)
				));
			// argument is a literal string but entity is expected
			} else if let Type::Entity{..} = param.ty 
				&& let ExprData::String(string) = arg.data {
				return Err(self.new_error(
					arg.span,
					format_args!("The host function '{}' expects an entity string, so put an 'e' in front of string \"{}\"", function_name, string)
				));
			// if argument is void
			} else if &arg_result_ty == &Type::Void {
				return Err(self.new_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got a function call that doesn't return anything", function_name, param.ty, param.name)
				));
			// If the shape of the argument type matches the shape of the expected parameter including generics
			} else if arg_result_ty.matches(&param.ty) {
				ty_ctx.add_constraint(arg.span, param.ty, arg_result_ty).map_err(|err| self.new_error(
					err.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got {}", function_name, err.diff, param.name, err.diff.swapped())
				))?;
			// mismatch
			} else {
				let type_diff = TypeDiff::new(param.ty, arg_result_ty);
				return Err(self.new_error(
					arg.span,
					format_args!("Function call '{}' expected the type {} for argument '{}', but got {}", function_name, type_diff, param.name, type_diff.swapped())
				));
			}
		}
		Ok(())
	}

	fn validate_and_fix_resource_string<'a>(&mut self, value: &str, extension: &str, span: SourceSpan, arena: &'a Arena) -> Result<&'a NTStr, Error> {
		if value.is_empty() {
			return Err(self.new_error(
				span,
				format_args!("Resources can't be empty strings")
			));
		} else if value.starts_with("/") {
			return Err(self.new_error(
				span,
				format_args!("Remove the leading slash from the resource \"{}\"", value)
			));
		} else if value.ends_with("/") {
			return Err(self.new_error(
				span,
				format_args!("Remove the trailing slash from the resource \"{}\"", value)
			));
		} else if value.contains("\\") {
			return Err(self.new_error(
				span,
				format_args!("Replace the '\\' with '/' in the resource \"{}\"", value)
			));
		} else if value.contains("//") {
			return Err(self.new_error(
				span,
				format_args!("Replace the '//' with '/' in the resource \"{}\"", value)
			));
		} else if value == ".." || value.starts_with("../") 
		       || value.ends_with("/..") || value.contains("/../") {
			return Err(self.new_error(
				span,
				format_args!("Remove the '..' from the resource \"{}\"", value)
			));
		} else if value == "." || value.starts_with("./") 
		       || value.ends_with("/.") || value.contains("/./") {
			return Err(self.new_error(
				span,
				format_args!("Remove the '.' from the resource \"{}\"", value)
			));
		} else if value.ends_with(".") {
			return Err(self.new_error(
				span,
				format_args!("resource name \"{}\" cannot end with .", value)
			));
		} else if value.ends_with(extension) {

		} else {
			return Err(self.new_error(
				span,
				format_args!("The resource '{}' was supposed to have the extension '{}'", value, extension)
			));
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
			Err(self.new_error(
				span,
				format_args!("resource '{}' does not exist", value)
			))
		} else {
			self.resources.push(self.arena.copy_str_into(resource_str).as_ref());
			Ok(resource_str)
		}
	}

	fn validate_and_fix_entity_string<'a>(&mut self, entity_string_old: &mut NTStrPtr<'a>, span: SourceSpan, arena: &'a Arena) -> Result<(), Error> {
		let entity_string = entity_string_old.to_str();
		// Validate string
		if entity_string.is_empty() {
			return Err(self.new_error(
				span,
				format_args!("Entities can't be empty strings")
			));
		}

		let (mod_name, entity_name) = if let Some((mod_name, entity_name)) = entity_string.split_once(":") {
			if mod_name.is_empty() {
				return Err(self.new_error(
					span,
					format_args!("Entity '{}' is missing a mod name", entity_string)
				));
			}
			if entity_name.is_empty() {
				return Err(self.new_error(
					span,
					format_args!("Entity '{}' missing entity name", entity_string)
				));
			}
			if mod_name == self.current_mod_name {
				return Err(self.new_error(
					span,
					format_args!("Entity string ('{}') cannot refer to its own mod", entity_string)
				));
			}
			(mod_name, entity_name)
		} else {
			("", entity_string)
		};

		if let Some(ch) = mod_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return Err(self.new_error(
				span,
				format_args!("Entity '{}' its mod name contains the invalid character '{}'", entity_string, ch)
			));
		}
		if let Some(ch) = entity_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return Err(self.new_error(
				span,
				format_args!("Entity '{}' its entity name contains the invalid character '{}'", entity_string, ch)
			));
		}

		// Fix string
		// TODO: If the mod name is non utf8, this may cause problems
		// Cross mod entities are supported, so we actually need to handle this properly
		if mod_name.is_empty() {
			*entity_string_old = Box::leak(NTStr::box_from_str_in(&format!("{}:{}", self.current_mod_name.display(), entity_name), arena)).as_ntstrptr()
		}
		Ok(())
	}

	fn get_variable_type(&self, var_name: &str) -> Option<Type<'arena>> {
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

	fn get_local_variable_type(&self, var_name: &str) -> Option<Type<'arena>> {
		for scope in self.local_variables.iter().rev() {
			if let var@Some(_) = scope.get(var_name) {
				return var.cloned();
			}
		}
		None
	}

	fn get_global_variable_type(&self, var_name: &str) -> Option<Type<'arena>> {
		self.global_variables.get(var_name).cloned()
	}

	fn add_local_variable(&mut self, name: &'arena str, ty: Type<'arena>, name_span: SourceSpan) -> Result<(), Error> {
		if self.get_global_variable_type(name).is_some() {
			return Err(self.new_error(
				name_span,
				format_args!("The local variable '{}' shadows an earlier global variable", name),
			));
		}
		if self.get_local_variable_type(name).is_some() {
			return Err(self.new_error(
				name_span,
				format_args!("The local variable '{}' shadows an earlier local variable", name),
			));
		}
		let result = self.local_variables.last_mut().expect("There is no local scope to push onto").insert(name, ty).is_none();
		debug_assert!(result);
		Ok(())
	}

	fn add_global_variable(&mut self, name: &'arena str, ty: Type<'arena>, name_span: SourceSpan) -> Result<(), Error> {
		match self.global_variables.entry(name) {
			Entry::Occupied(_) => return Err(self.new_error(
				name_span,
				format_args!("The global variable '{}' shadows an earlier global variable", name),
			)),
			Entry::Vacant(x) => {x.insert(ty);},
		}
		Ok(())
	}
}

#[derive(Clone, Copy)]
struct ExistentialData<'a> {
	// span of the name of the function that declared the existential
	function_name_span: SourceSpan,
	// name of the function that declared the existential
	function_name: &'a str,
}

struct TyCtx<'a, 'err> {
	function_name: &'err str,
	file_path: &'err OsStr,
	file_text: &'err str,
	/// The names of each existential that has been created in this context so
	/// far, The index of the existential refers to the index into these
	/// vectors that contains the data about that existential
	existentials: Vec<ExistentialData<'a>,
		&'a Arena
	>,
	/// The current type that the existential at each index should be replaced
	/// with
	substitutions: Vec<Type<'a>, &'a Arena>,
	/// A list of constraints that still need to be evaluated
	constraints: Vec<(Type<'a>, Type<'a>), &'a Arena>,
	/// The arena that all data is allocated in while typechecking
	temp_arena: &'a Arena,
}

impl<'a, 'err> TyCtx<'a, 'err> {
	fn new(
		function_name: &'err str,
		file_path: &'err OsStr,
		file_text: &'err str,
		temp_arena: &'a Arena
	) -> Self {
		Self {
			function_name,
			file_path,
			file_text,
			existentials: Vec::new_in(temp_arena),
			substitutions: Vec::new_in(temp_arena),
			constraints: Vec::new_in(temp_arena),
			temp_arena,
		}
	}

	#[track_caller]
	fn new_error(&self, span: SourceSpan, args: std::fmt::Arguments) -> Error {
		Error::new(
			ErrorKind::TYPE_CHECKER_ERROR,
			self.function_name,
			self.file_path,
			self.file_text, 
			span,
			args
		)
	}

	// Returns the first currently known replacement type for an existential
	fn get_current_type(&self, ty: Type<'a>) -> Option<Type<'a>> {
		fn get_current_type_inner<'a>(substitutions: &[Type<'a>], stack: StackLL<usize>) -> Option<Type<'a>> {
			match substitutions[stack.current] {
				Type::Existential{idx} => {
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
		match ty {
			Type::Existential{idx} => get_current_type_inner(&self.substitutions, StackLL{current: idx, parent: None}),
			_ => Some(ty)
		}
	}

	fn create_existential(&mut self, function_name: &'a str, function_name_span: SourceSpan) -> Type<'static> {
		let new_existential = Type::Existential { idx: self.existentials.len() };
		self.existentials.push(ExistentialData{
			function_name_span, 
			function_name,
		});
		self.substitutions.push(new_existential);
		new_existential
	}

	fn add_constraint(&mut self, err_span: SourceSpan, left: Type<'a>, right: Type<'a>) -> Result<(), TypeMismatch<'a>> {
		self.constraints.push((left, right));
		// This ensures recursive calls only handle the constraints relevant to them
		while let Some(constraint) = self.constraints.pop() {
			match constraint {
				(Type::Void, Type::Void) => (),
				(Type::Bool, Type::Bool) => (),
				(Type::Number, Type::Number) => (),
				(Type::String, Type::String) => (),
				(_, Type::Resource{..}) | 
				(Type::Resource{..}, _) => unreachable!("resource strings never appear in generics"),
				(_, Type::Entity{..}) | 
				(Type::Entity{..}, _) => unreachable!("entity strings never appear in generics"),
				(Type::Id{name: left_name, ..}, Type::Id{name: right_name, ..}) if left_name != right_name => {
					return Err(TypeMismatch{
						span: err_span,
						// SAFETY: This is an error case, so we never send this type to the backend
						// The occurs check also ensures types are never recursive
						diff: unsafe{TypeDiff::new(self.copy_type_into(left, self.temp_arena), self.copy_type_into(right, self.temp_arena))}
					});
				}
				// This part is *not* recursive. The error should contain the original types
				(Type::Id{generics: left_generics, ..}, Type::Id{generics: right_generics, ..}) => {
					assert_eq!(left_generics.len(), right_generics.len(), "You forgot to verify the number of generics on types");
					for (left, right) in left_generics.iter().zip(right_generics) {
						self.constraints.push((*left, *right));
					}
				}
				// An existential is always equal to it
				(Type::Existential{idx: left_idx}, Type::Existential{idx: right_idx}) if left_idx == right_idx => (),
				// At least one side is an existential
				// This part *is* recursive. The error should contain the new types
				(Type::Existential{idx}, other) |
				(other, Type::Existential{idx}) => {
					// TODO, recursive `occurs` check
					let old_substitution = self.substitutions[idx];
					if let Type::Existential{idx: found_idx} = old_substitution && found_idx == idx {
						self.substitutions[idx] = other;
					}
					self.constraints.push((other, old_substitution));
				}
				_ => {
					return Err(TypeMismatch{
						span: err_span,
						diff: unsafe{TypeDiff::new(self.copy_type_into(left, self.temp_arena), self.copy_type_into(right, self.temp_arena))}
					});
				}
			}
		}
		Ok(())
	}

	// # Safety:
	// If not all types are fully concrete, then the returned type will have
	// existentials, which should not be passed to the backend
	//
	// # Note: 
	// If there are any non-trivial loops, this will result in a stack overflow
	unsafe fn copy_type_into<'arena>(&self, ty: Type<'a>, arena: &'arena Arena) -> Type<'arena> {
		match ty {
			Type::Resource{extension} => Type::Resource{extension: arena.copy_str_into_nt(extension.to_str()).as_ntstrptr()},
			Type::Entity{entity_type: Some(entity_type)} => Type::Entity{entity_type: Some(arena.copy_str_into_nt(entity_type.to_str()).as_ntstrptr())},
			Type::Existential{idx} => {
				if let Type::Existential{idx} = self.substitutions[idx]{
					return Type::Existential{idx};
				}
				let return_type = unsafe{self.copy_type_into(
					self.substitutions[idx], 
					arena
				)};
				return_type
			}
			Type::Id{name, generics} => Type::Id{
				name: arena.copy_str_into_nt(name.to_str()).as_ntstrptr(),
				generics: arena.slice_from_iter(generics.iter().map(|ty| unsafe{self.copy_type_into(*ty, arena)})),
			},
			Type::Void => Type::Void,
			Type::Bool => Type::Bool,
			Type::Number => Type::Number,
			Type::String => Type::String,
			Type::Entity{entity_type: None} => Type::Entity{entity_type: None},
		}
	}

	fn check_consistency(&self, ty: Type, parent_existentials: StackLL<usize>) -> Result<(), Error> {
		match ty {
			Type::Existential{idx} => {
				if idx == *parent_existentials {
					let data = self.existentials[idx];
					return Err(self.new_error(
						data.function_name_span,
						format_args!("unable to infer generics in function '{}'", data.function_name)
					));
				}
				let mut current = Some(&parent_existentials);
				while let Some(cur) = current {
					if **cur == idx {
						let data = self.existentials[idx];
						return Err(self.new_error(
							data.function_name_span,
							format_args!("Infinitely recursive type found during type inference of function `{}`", data.function_name)
						));
					}
					current = cur.parent;
				}
				self.check_consistency(self.substitutions[idx], StackLL{current: idx, parent: Some(&parent_existentials)})?;
			}
			Type::Id{name: _, generics} => for generic in generics {
				self.check_consistency(*generic, parent_existentials)?
			}
			_ => (),
		}
		Ok(())
	}

	fn substitute<'arena>(&mut self, type_storage: &mut TypeStorage, arena: &'arena Arena) -> Result<&'arena [Type<'static>], Error> {
		// Copy all types into the permanent arena
		for i in 0..self.substitutions.len() {
			self.check_consistency(self.substitutions[i], StackLL{current: i, parent: None})?;
		}
		Ok(arena.slice_from_iter(self.substitutions.iter().map(|ty| {
			// SAFETY: Consistency check has been performed on all existentials
			unsafe {type_storage.insert_type(self.copy_type_into(*ty, arena))}
		})))
	}

	fn verify_traits(&self, ty: Type, traits: &[&Trait], err_span: SourceSpan, function_name: &str) -> Result<(), Error> {
		fn type_matches_implementor(ty: Type, imp_ty: Type, generics: &[Generic]) -> bool {
			match (ty, imp_ty) {
				(Type::Id{name: ty_name, generics: ty_generics}, Type::Id{name: imp_name, generics: imp_generics}) if 
					ty_name == imp_name => {
						debug_assert_eq!(ty_generics.len(), imp_generics.len());
						ty_generics.iter().zip(imp_generics).all(|(ty, imp)| type_matches_implementor(*ty, *imp, generics))
					}
				(ty, Type::Existential{idx}) => {
					generics.get(idx).expect("existential should always point to a valid generic")
						.traits().into_iter().all(|tr| {
							tr.implementors.into_iter().any(|imp| type_matches_implementor(ty, imp.ty, imp.generics))
						})
				}
				(Type::Void, Type::Void) => true,
				(Type::Bool, Type::Bool) => true,
				(Type::Number, Type::Number) => true,
				(Type::String, Type::String) => true,
				(Type::Resource{..}, _) |
				(_, Type::Resource{..}) => unreachable!("resource strings cannot be used in generics"),
				(Type::Entity{..}, _) |
				(_, Type::Entity{..}) => unreachable!("entity strings cannot be used in generics"),
				_ => false
			}
		}
		for tr in traits {
			if !tr.implementors.into_iter().any(|imp| type_matches_implementor(ty, imp.ty, imp.generics)) {
				return Err(self.new_error(err_span, format_args!("host function '{}' expects type '{}' to implement constraint '{}' but it doesn't", function_name, ty, tr.name)));
			}
		} 
		Ok(())
	}
}

/// A linked list whose elements live on the stack
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
#[derive(Clone, Copy)]
struct TypeDiff<'a> {
	print: Type<'a>,
	diff : Type<'a>,
}

impl<'a> TypeDiff<'a> {
	fn new(print: Type<'a>, diff: Type<'a>) -> Self {
		Self {
			print,
			diff
		}
	}

	fn swapped(self) -> Self {
		Self {
			print: self.diff,
			diff : self.print,
		}
	}
}

impl<'a> std::fmt::Display for TypeDiff<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		fn get_type_name(ty: Type<'_>) -> &str {
			match ty {
				Type::Void => "void",
				Type::Bool => "bool",
				Type::Number => "number",
				Type::String => "string",
				Type::Id {
					name,
					generics: _,
				} => name.to_str(),
				Type::Resource{..} => "resource",
				Type::Entity{..} => "entity",
				Type::Existential {..} => unreachable!(),
			}
		}

		if self.print.matches(&self.diff) {
			return f.write_str("_")
		}
		f.write_str(get_type_name(self.print))?;
		match (self.print, self.diff) {
			(
				Type::Id{name: name_left, generics: generics_left}, 
				Type::Id{name: name_right, generics: generics_right}
			) if name_left == name_right => {
				f.write_str("[")?;
				for (i, (print, diff)) in generics_left.iter().copied().zip(generics_right.iter().copied()).enumerate() {
					Self {
						print,
						diff
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

pub(crate) struct TypeListDisplay<'a>(pub(crate) &'a [Type<'a>]);

impl<'a> std::fmt::Display for TypeListDisplay<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_str("[")?;
		for (i, ty) in self.0.iter().enumerate() {
			write!(f, "{}", ty)?;
			if i != self.0.len() - 1 {
				f.write_str(", ")?;
			}
		}
		f.write_str("]")?;
		Ok(())
	}
}
