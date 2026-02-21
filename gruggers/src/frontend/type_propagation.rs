use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;
use crate::ntstring::NTStr;
use crate::ast::{
	GrugType, UnaryOperator, BinaryOperator, GlobalStatement,
	ExprData, LiteralExprData, HelperFunction, Statement, Expr,

	c_expr, c_argument, c_statement,
};
use crate::nt;
use crate::arena::Arena;
use crate::frontend::parser::AST;
use crate::mod_api::{ModApiEntity, ModApiGameFn};

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

pub(super) struct TypePropogator<'a> {
	entity: &'a ModApiEntity,
	game_fns: &'a HashMap<Arc<str>, ModApiGameFn>,
	current_mod_name: String,
	global_variables: HashMap<&'a str, GrugType<'a>>,
	local_variables: Vec<HashMap<&'a str, GrugType<'a>>>,
	num_while_loops_deep: usize,
	current_fn_name: Option<&'a str>,
}

#[derive(Debug)]
pub enum TypePropogatorError {
	// grug_assert(grug_entity, "The entity '%s' was not declared by mod_api.json", file_entity_type);
	EntityDoesNotExist{
		entity_name: Arc<str>,
	},
	// grug_assert(!get_global_variable(name), "The global variable '%s' shadows an earlier global variable with the same name, so change the name of one of them", name);
	GlobalVariableShadowed {
		name: Arc<str>,
	},
	// grug_assert(!starts_with(expr->call.fn_name, "helper_"), "The global variable '%s' isn't allowed to call helper functions", name);
	GlobalCantCallHelperFn {
		global_name: Arc<str>,
	},
	// grug_assert(var, "The variable '%s' does not exist", expr->literal.string);
	VariableDoesNotExist {
		name: Arc<str>,
	},
	// grug_assert(expr->unary.operator != expr->unary.expr->unary.operator, "Found '%s' directly next to another '%s', which can be simplified by just removing both of them", get_token_type_str[expr->unary.operator], get_token_type_str[expr->unary.expr->unary.operator]);
	AdjacentUnaryOperators {
		operator: UnaryOperator,
	},
	// grug_assert(expr->result_type == type_bool, "Found 'not' before %s, but it can only be put before a bool", expr->result_type_name);
	NotOperatorNotBeforeBool{
		got: GrugType<'static>,
	},
	// grug_assert(expr->result_type == type_i32 || expr->result_type == type_f32, "Found '-' before %s, but it can only be put before an i32 or f32", expr->result_type_name);
	MinusOperatorNotBeforeNumber {
		got: GrugType<'static>,
	},
	// grug_assert(binary_expr.operator == EQUALS_TOKEN || binary_expr.operator == NOT_EQUALS_TOKEN, "You can't use the %s operator on a string", get_token_type_str[binary_expr.operator]);
	CannotCompareStrings {
		// This can only be BinaryOperator::DoubleEquals | BinaryOperator::NotEquals
		operator: BinaryOperator,
	},
	// grug_error("The left and right operand of a binary expression ('%s') must have the same type, but got %s and %s", get_token_type_str[binary_expr.operator], binary_expr.left_expr->result_type_name, binary_expr.right_expr->result_type_name);
	BinaryOperatorTypeMismatch {
		operator: BinaryOperator,
		left: GrugType<'static>,
		right: GrugType<'static>,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_bool, "'%s' operator expects bool", get_token_type_str[binary_expr.operator]);
	LogicalOperatorExpectsBool {
		// Must be 'or' or 'and' operators
		operator: BinaryOperator,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_i32 || binary_expr.left_expr->result_type == type_f32, "'%s' operator expects i32 or f32", get_token_type_str[binary_expr.operator]);
	ComparisonOperatorExpectsNumber {
		// Must be '>', '>=', '<', or '<=' operators
		operator: BinaryOperator,
		got_type: GrugType<'static>,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_i32 || binary_expr.left_expr->result_type == type_f32, "'%s' operator expects i32 or f32", get_token_type_str[binary_expr.operator]);
	ArithmeticOperatorExpectsNumber {
		// Must be '+', '-', '*', or '/' operators
		operator: BinaryOperator,
		got_type: GrugType<'static>,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_i32, "'%%' operator expects i32");
	RemainderOperatorExpectsNumber {
		got_ty: GrugType<'static>,
	},
	// grug_error("Mods aren't allowed to call their own on_ functions, but '%s' was called", name);
	CallOnFnWithinOnFn {
		on_fn_name: Arc<str>,
	},
	// grug_error("The function '%s' does not exist", name);
	FunctionDoesNotExist {
		function_name: Arc<str>,
	},
	// grug_assert(call_expr.argument_count >= param_count, "Function call '%s' expected the argument '%s' with type %s", name, params[call_expr.argument_count].name, params[call_expr.argument_count].type_name);
	TooFewArguments{
		function_name: Arc<str>,
		expected_name: Arc<str>,
		expected_type: GrugType<'static>,
	},
	// grug_assert(call_expr.argument_count <= param_count, "Function call '%s' got an unexpected extra argument with type %s", name, call_expr.arguments[param_count].result_type_name);
	TooManyArguments{
		function_name: Arc<str>,
		got_type: GrugType<'static>,
	},
	ResourceValidationError(ResourceValidationError),
	EntityValidationError(EntityValidationError),
	// grug_assert(arg->result_type != type_void, "Function call '%s' expected the type %s for argument '%s', but got a function call that doesn't return anything", name, param.type_name, param.name);
	VoidArgumentInFunctionCall {
		function_name: Arc<str>,
		signature_type: GrugType<'static>,
		parameter_name: Arc<str>
	},
	// grug_error("Function call '%s' expected the type %s for argument '%s', but got %s", name, param.type_name, param.name, arg->result_type_name);
	FunctionArgumentMismatch {
		function_name: Arc<str>,
		expected_type: GrugType<'static>,
		got_type: GrugType<'static>,
		parameter_name: Arc<str>
	},
	// grug_assert(entity_on_fn, "The function '%s' was not declared by entity '%s' in mod_api.json", on_fns[fn_index].fn_name, file_entity_type);
	OnFnDoesNotExist {
		function_name: Arc<str>,
		entity_name: Arc<str>,
	},
	// grug_assert(arg_count >= param_count, "Function '%s' expected the parameter '%s' with type %s", name, params[arg_count].name, params[arg_count].type_name);
	TooFewParameters{
		function_name: Arc<str>,
		expected_name: Arc<str>,
		expected_type: GrugType<'static>,
	},
	// grug_assert(arg_count <= param_count, "Function '%s' got an unexpected extra parameter '%s' with type %s", name, args[param_count].name, args[param_count].type_name);
	TooManyParameters{
		function_name: Arc<str>,
		parameter_name: Arc<str>,
		parameter_type: GrugType<'static>,
	},
	// grug_assert(streq(arg_name, param.name), "Function '%s' its '%s' parameter was supposed to be named '%s'", name, arg_name, param.name);
	OnFnParameterNameMismatch{
		function_name: Arc<str>,
		got_name: Arc<str>,
		expected_name: Arc<str>,
	},
	//grug_error("Function '%s' its '%s' parameter was supposed to have the type %s, but got %s", name, param.name, param.type_name, arg_type_name);
	OnFnParameterTypeMismatch{
		function_name: Arc<str>,
		parameter_name: Arc<str>,
		got_type: GrugType<'static>,
		expected_type: GrugType<'static>,
	},
	// grug_assert(!streq(global->assignment_expr.literal.string, "me"), "Global variables can't be assigned 'me'");
	GlobalCantBeAssignedMe {
		name: Arc<str>,
	},
	// grug_error("Can't assign %s to '%s', which has type %s", global->assignment_expr.result_type_name, global->name, global->type_name);
	VariableTypeMismatch {
		name: Arc<str>,
		got_type: GrugType<'static>,
		expected_type: GrugType<'static>,
	},
	// grug_assert(!get_local_variable(name), "The local variable '%s' shadows an earlier local variable with the same name, so change the name of one of them", name);
	LocalVariableShadowedByGlobal {
		name: Arc<str>,
	},
	// grug_assert(!get_global_variable(name), "The local variable '%s' shadows an earlier global variable with the same name, so change the name of one of them", name);
	LocalVariableShadowedByLocal {
		name: Arc<str>,
	},
	// grug_assert(var, "Can't assign to the variable '%s', since it does not exist", variable_statement.name);
	CantAssignBecauseVariableDoesntExist {
		name: Arc<str>,
	},
	// "If condition must be bool but got '%s'", 
	IfConditionTypeMismatch {
		got_type: GrugType<'static>,
	},
	// "While condition must be bool but got '%s'", 
	WhileConditionTypeMismatch {
		got_type: GrugType<'static>,
	},
	// TODO: This needs location information 
	// "There is a break statement that isn't inside of a while loop" 
	BreakStatementOutsideWhileLoop,
	// TODO: This needs location information 
	// "There is a break statement that isn't inside of a while loop" 
	ContinueStatementOutsideWhileLoop,

	// TODO: This needs more information, like the name of the global
	// grug_assert(!compiled_init_globals_fn, "Global id variables can't be reassigned");
	GlobalIdsCantBeReassigned,
	// grug_assert(!var, "The variable '%s' already exists", variable_statement.name);
	VariableAlreadyExists {
		variable_name: Arc<str>
	},
	// grug_assert(fn_return_type != type_void, "Function '%s' wasn't supposed to return any value", filled_fn_name);
	// grug_error("Function '%s' is supposed to return %s, not %s", filled_fn_name, fn_return_type_name, statement.return_statement.value->result_type_name);
	// grug_assert(fn_return_type == type_void, "Function '%s' is supposed to return a value of type %s", filled_fn_name, fn_return_type_name);
	MismatchedReturnType {
		function_name: Arc<str>,
		expected_type: GrugType<'static>,
		got_type: GrugType<'static>,
	},
	// grug_assert(last_statement.type == RETURN_STATEMENT, "Function '%s' is supposed to return %s as its last line", filled_fn_name, fn_return_type_name);
	LastStatementNotReturn {
		function_name: Arc<str>,
		expected_return_type: GrugType<'static>,
	},
	// grug_assert(previous_on_fn_index <= on_fn_index, "The function '%s' needs to be moved before/after a different on_ function, according to the entity '%s' in mod_api.json", on_fn->fn_name, grug_entity->name);
	OutOfOrderOnFn {
		entity_name: Arc<str>,
		on_fn_name: Arc<str>,
	}
}

impl std::fmt::Display for TypePropogatorError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::EntityDoesNotExist{
				entity_name,
			} => write!(f, "The entity '{}' was not declared by mod_api.json", entity_name),
			Self::GlobalVariableShadowed {
				name,
			} => write!(f, "The global variable '{}' shadows an earlier global variable with the same name, so change the name of one of them", name),
			Self::GlobalCantCallHelperFn {
				global_name,
			} => write!(f, "The global variable '{}' isn't allowed to call helper functions", global_name),
			Self::VariableDoesNotExist {
				name,
			} => write!(f, "The variable '{}' does not exist", name),
			Self::AdjacentUnaryOperators {
				operator,
			} => write!(f, "Found '{0}' directly next to another '{0}', which can be simplified by just removing both of them", operator),
			Self::NotOperatorNotBeforeBool{
				got,
			} => write!(f, "Found 'not' before {}, but it can only be put before a bool", got),
			Self::MinusOperatorNotBeforeNumber {
				got,
			} => write!(f, "Found '-' before {}, but it can only be put before a number", got),
			Self::CannotCompareStrings {
				operator,
			} => write!(f, "You can't use the {} operator on a string", operator),
			Self::BinaryOperatorTypeMismatch {
				operator,
				left: GrugType::String,
				right: _,
			} => write!(f, "You can't use the {} operator on a string", operator),
			Self::BinaryOperatorTypeMismatch {
				operator,
				left: _,
				right: GrugType::String,
			} => write!(f, "You can't use the {} operator on a string", operator),
			Self::BinaryOperatorTypeMismatch {
				operator,
				left,
				right,
			} => write!(f, "The left and right operand of a binary expression ('{}') must have the same type, but got {} and {}", operator, left, right),
			Self::LogicalOperatorExpectsBool {
				operator,
			} => write!(f, "'{}' operator expects bool", operator),
			Self::ComparisonOperatorExpectsNumber {
				operator,
				got_type: _,
			} => write!(f, "'{}' operator expects number", operator),
			Self::ArithmeticOperatorExpectsNumber {
				got_type: GrugType::String,
				operator,
			} => write!(f, "You can't use the {} operator on a string", operator),
			Self::ArithmeticOperatorExpectsNumber {
				operator,
				got_type: _,
			} => write!(f, "'{}' operator expects number", operator),
			Self::RemainderOperatorExpectsNumber {
				got_ty: _,
			} => write!(f, "'%' operator expects number"),
			Self::CallOnFnWithinOnFn {
				on_fn_name,
			} => write!(f, "Mods aren't allowed to call their own on_ functions, but '{}' was called", on_fn_name),
			Self::FunctionDoesNotExist {
				function_name,
			} if function_name.starts_with("helper_") => write!(f, "The helper function '{}' was not defined by this grug file", function_name),
			Self::FunctionDoesNotExist {
				function_name,
			} => write!(f, "The game function '{}' was not declared by mod_api.json", function_name),
			Self::TooFewArguments{
				function_name,
				expected_name,
				expected_type,
			} => write!(f, "Function call '{}' expected the argument '{}' with type {}", function_name, expected_name, expected_type),
			Self::TooManyArguments{
				function_name,
				got_type,
			} => write!(f, "Function call '{}' got an unexpected extra argument with type {}", function_name, got_type),
			Self::ResourceValidationError(error) => write!(f, "{}", error),
			Self::EntityValidationError(error) => write!(f, "{}", error),
			Self::VoidArgumentInFunctionCall {
				function_name,
				signature_type,
				parameter_name
			} => write!(f, "Function call '{}' expected the type {} for argument '{}', but got a function call that doesn't return anything", function_name, signature_type, parameter_name),
			Self::FunctionArgumentMismatch {
				function_name,
				expected_type,
				got_type,
				parameter_name
			} => write!(f, "Function call '{}' expected the type {} for argument '{}', but got {}", function_name, expected_type, parameter_name, got_type),
			Self::OnFnDoesNotExist {
				function_name,
				entity_name,
			} => write!(f, "The function '{}' was not declared by entity '{}' in mod_api.json", function_name, entity_name),
			Self::TooFewParameters{
				function_name,
				expected_name,
				expected_type,
			} => write!(f, "Function '{}' expected the parameter '{}' with type {}", function_name, expected_name, expected_type),
			Self::TooManyParameters{
				function_name,
				parameter_name,
				parameter_type,
			} => write!(f, "Function '{}' got an unexpected extra parameter '{}' with type {}", function_name, parameter_name, parameter_type),
			Self::OnFnParameterNameMismatch{
				function_name,
				got_name,
				expected_name,
			} => write!(f, "Function '{}' its '{}' parameter was supposed to be named '{}'", function_name, got_name, expected_name),
			Self::OnFnParameterTypeMismatch{
				function_name,
				parameter_name,
				got_type,
				expected_type,
			} => write!(f, "Function '{}' its '{}' parameter was supposed to have the type {}, but got {}", function_name, parameter_name, expected_type, got_type),
			Self::GlobalCantBeAssignedMe {
				name: _,
			} => write!(f, "Global variables can't be assigned 'me'"),
			Self::VariableTypeMismatch {
				name,
				got_type,
				expected_type,
			} => write!(f, "Can't assign {} to '{}', which has type {}", got_type, name, expected_type),
			Self::LocalVariableShadowedByGlobal {
				name,
			} => write!(f, "The local variable '{}' shadows an earlier global  variable with the same name, so change the name of one of them", name),
			Self::LocalVariableShadowedByLocal {
				name,
			} => write!(f, "The local variable '{}' shadows an earlier local variable with the same name, so change the name of one of them", name),
			Self::CantAssignBecauseVariableDoesntExist {
				name,
			} => write!(f, "Can't assign to the variable '{}', since it does not exist", name),
			Self::IfConditionTypeMismatch {
				got_type,
			} => write!(f, "If condition must be bool but got '{}'", got_type),
			Self::WhileConditionTypeMismatch {
				got_type,
			} => write!(f, "While condition must be bool but got '{}'", got_type),
			Self::BreakStatementOutsideWhileLoop => write!(f, "There is a break statement that isn't inside of a while loop"),
			Self::ContinueStatementOutsideWhileLoop => write!(f, "There is a continue statement that isn't inside of a while loop"),
			Self::GlobalIdsCantBeReassigned => write!(f, "Global id variables can't be reassigned"),
			// grug_assert(!var, "The variable '%s' already exists", variable_statement.name);
			Self::VariableAlreadyExists {
				variable_name
			} => write!(f, "The variable '{}' already exists", variable_name),
			// grug_assert(fn_return_type == type_void, "Function '%s' is supposed to return a value of type %s", filled_fn_name, fn_return_type_name);
			Self::MismatchedReturnType {
				function_name,
				expected_type,
				got_type: GrugType::Void,
			} => write!(f, "Function '{}' is supposed to return a value of type {}", function_name, expected_type),
			// grug_assert(fn_return_type != type_void, "Function '%s' wasn't supposed to return any value", filled_fn_name);
			Self::MismatchedReturnType {
				function_name,
				expected_type: GrugType::Void,
				got_type: _,
			} => write!(f, "Function '{}' wasn't supposed to return any value", function_name),
			// grug_error("Function '%s' is supposed to return %s, not %s", filled_fn_name, fn_return_type_name, statement.return_statement.value->result_type_name);
			Self::MismatchedReturnType {
				function_name,
				expected_type,
				got_type,
			} => write!(f, "Function '{}' is supposed to return {}, not {}", function_name, expected_type, got_type),
			// grug_assert(last_statement.type == RETURN_STATEMENT, "Function '%s' is supposed to return %s as its last line", filled_fn_name, fn_return_type_name);
			Self::LastStatementNotReturn {
				function_name,
				expected_return_type,
			} => write!(f, "Function '{}' is supposed to return {} as its last line", function_name, expected_return_type),
			// grug_assert(previous_on_fn_index <= on_fn_index, "The function '%s' needs to be moved before/after a different on_ function, according to the entity '%s' in mod_api.json", on_fn->fn_name, grug_entity->name);
			Self::OutOfOrderOnFn {
				entity_name,
				on_fn_name,
			} => write!(f, "The function '{}' needs to be moved before/after a different on_ function, according to the entity '{}' in mod_api.json", on_fn_name, entity_name),
		}
	}
}

#[derive(Debug)]
pub enum ResourceValidationError {
	// grug_assert(string[0] != '\0', "Resources can't be empty strings");
	// TODO: This needs to display the actual argument
	EmptyResource {},
	// grug_assert(string[0] != '/', "Remove the leading slash from the resource \"%s\"", string);
	LeadingForwardSlash {
		value: Arc<NTStr>
	},
	//grug_assert(string[string_len - 1] != '/', "Remove the trailing slash from the resource \"%s\"", string);
	TrailingForwardSlash {
		value: Arc<NTStr>
	},
	// grug_assert(!strchr(string, '\\'), "Replace the '\\' with '/' in the resource \"%s\"", string);
	ContainsBackslash {
		value: Arc<NTStr>
	},
	// grug_assert(!strstr(string, "//"), "Replace the '//' with '/' in the resource \"%s\"", string);
	ContainsDoubleForwardSlash {
		value: Arc<NTStr>
	},
	// TODO: This error needs a better message
	// grug_assert(string_len != 1 && string[1] != '/', "Remove the '.' from the resource \"%s\"", string);
	BeginsWithDotWithoutSlash {
		value: Arc<NTStr>
	},
	// TODO: This error needs a better message
	// grug_assert(dot[1] != '/' && dot[1] != '\0', "Remove the '.' from the resource \"%s\"", string);
	ContainsSlashDotInMiddle {
		value: Arc<NTStr>
	},
	// TODO: This error needs a better message
	// grug_assert(string_len != 2 && string[2] != '/', "Remove the '..' from the resource \"%s\"", string);
	BeginsWithDotDotWithoutSlash {
		value: Arc<NTStr>
	},
	// TODO: This error needs a better message
	// grug_assert(dotdot[2] != '/' && dotdot[2] != '\0', "Remove the '..' from the resource \"%s\"", string);
	// " 
	ContainsSlashDotDotInMiddle {
		value: Arc<NTStr>
	},
	// grug_assert(ends_with(string, resource_extension), "The resource '%s' was supposed to have the extension '%s'", string, resource_extension);
	ExtensionMismatch {
		expected: Arc<str>,
		value: Arc<NTStr>
	},
	// raise TypePropagationError(f'resource name "{string}" cannot end with .')
	EndsWithDot  {
		value: Arc<NTStr>
	}
}

impl std::fmt::Display for ResourceValidationError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::EmptyResource{} => write!(f, "Resources can't be empty strings"),
			// grug_assert(string[0] != '/', "Remove the leading slash from the resource \"%s\"", string);
			Self::LeadingForwardSlash {
				value
			} => write!(f, "Remove the leading slash from the resource \"{}\"", value),
			//grug_assert(string[string_len - 1] != '/', "Remove the trailing slash from the resource \"%s\"", string);
			Self::TrailingForwardSlash {
				value
			} => write!(f, "Remove the trailing slash from the resource \"{}\"", value),
			// grug_assert(!strchr(string, '\\'), "Replace the '\\' with '/' in the resource \"%s\"", string);
			Self::ContainsBackslash {
				value
			} => write!(f, "Replace the '\\' with '/' in the resource \"{}\"", value),
			// grug_assert(!strstr(string, "//"), "Replace the '//' with '/' in the resource \"%s\"", string);
			Self::ContainsDoubleForwardSlash {
				value
			} => write!(f, "Replace the '//' with '/' in the resource \"{}\"", value),
			// TODO: This error needs a better message
			// grug_assert(string_len != 1 && string[1] != '/', "Remove the '.' from the resource \"%s\"", string);
			Self::BeginsWithDotWithoutSlash {
				value
			} => write!(f, "Remove the '.' from the resource \"{}\"", value),
			// TODO: This error needs a better message
			// grug_assert(dot[1] != '/' && dot[1] != '\0', "Remove the '.' from the resource \"%s\"", string);
			Self::ContainsSlashDotInMiddle {
				value
			} => write!(f, "Remove the '.' from the resource \"{}\"", value),
			// TODO: This error needs a better message
			// grug_assert(string_len != 2 && string[2] != '/', "Remove the '..' from the resource \"%s\"", string);
			Self::BeginsWithDotDotWithoutSlash {
				value
			} => write!(f, "Remove the '..' from the resource \"{}\"", value),
			// TODO: This error needs a better message
			// grug_assert(dotdot[2] != '/' && dotdot[2] != '\0', "Remove the '..' from the resource \"%s\"", string);
			// " 
			Self::ContainsSlashDotDotInMiddle {
				value
			} => write!(f, "Remove the '..' from the resource \"{}\"", value),
			// grug_assert(ends_with(string, resource_extension), "The resource '%s' was supposed to have the extension '%s'", string, resource_extension);
			Self::ExtensionMismatch {
				expected,
				value
			} => write!(f, "The resource '{}' was supposed to have the extension '{}'", value, expected),
			// raise TypePropagationError(f'resource name "{string}" cannot end with .')
			Self::EndsWithDot {
				value
			} => write!(f, "resource name \"{}\" cannot end with .", value),
		}
	}
}

#[derive(Debug)]
pub enum EntityValidationError {
	EntityCantBeEmpty,
	// grug_assert(len > 0, "Entity '%s' is missing a mod name", string);
	EntityMissingModName {
		entity_string: Arc<NTStr>,
	},
	// grug_assert(*entity_name != '\0', "Entity '%s' specifies the mod name '%s', but it is missing an entity name after the ':'", string, mod_name);
	EntityMissingEntityName {
		mod_name: String,
		entity_string: Arc<NTStr>,
	},
	// grug_assert(!streq(mod_name, mod), "Entity '%s' its mod name '%s' is invalid, since the file it is in refers to its own mod; just change it to '%s'", string, mod_name, entity_name);
	ModNameIsCurrentMod {
		full_entity_string: Arc<NTStr>,
		mod_name: String,
		entity_name: String,
	},
	// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its mod name contains the invalid character '%c'", string, c);
	ModNameHasInvalidCharacter {
		entity_name: Arc<NTStr>, 
		invalid_char: char
	},
	// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its entity name contains the invalid character '%c'", string, c);
	EntityNameHasInvalidCharacter {
		entity_name: Arc<NTStr>, 
		invalid_char: char
	},
}

impl std::fmt::Display for EntityValidationError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::EntityCantBeEmpty => write!(f, "Entities can't be empty strings"),
			// grug_assert(len > 0, "Entity '%s' is missing a mod name", string);
			Self::EntityMissingModName {
				entity_string,
			} => write!(f, "Entity '{}' is missing a mod name", entity_string),
			// grug_assert(*entity_name != '\0', "Entity '%s' specifies the mod name '%s', but it is missing an entity name after the ':'", string, mod_name);
			Self::EntityMissingEntityName {
				mod_name,
				entity_string,
			} => write!(f, "Entity '{}' specifies the mod name '{}', but it is missing an entity name after the ':'", entity_string, mod_name),
			// grug_assert(!streq(mod_name, mod), "Entity '%s' its mod name '%s' is invalid, since the file it is in refers to its own mod; just change it to '%s'", string, mod_name, entity_name);
			Self::ModNameIsCurrentMod {
				full_entity_string,
				mod_name,
				entity_name,
			} => write!(f, "Entity '{}' its mod name '{}' is invalid, since the file it is in refers to its own mod; just change it to '{}'", full_entity_string, mod_name, entity_name),
			// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its mod name contains the invalid character '%c'", string, c);
			Self::ModNameHasInvalidCharacter {
				entity_name, 
				invalid_char
			} => write!(f, "Entity '{}' its mod name contains the invalid character '{}'", entity_name, invalid_char),
			// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its entity name contains the invalid character '%c'", string, c);
			Self::EntityNameHasInvalidCharacter {
				entity_name, 
				invalid_char
			} => write!(f, "Entity '{}' its entity name contains the invalid character '{}'", entity_name, invalid_char),
		}
	}
}

impl From<ResourceValidationError> for TypePropogatorError {
	fn from (other: ResourceValidationError) -> Self {
		Self::ResourceValidationError(other)
	}
}

impl From<EntityValidationError> for TypePropogatorError {
	fn from (other: EntityValidationError) -> Self {
		Self::EntityValidationError(other)
	}
}

impl<'a> TypePropogator<'a> {
	pub fn new (entity: &'a ModApiEntity, game_fns: &'a HashMap<Arc<str>, ModApiGameFn>, mod_name: String) -> Self {
		Self {
			entity,
			game_fns,
			current_mod_name: mod_name,
			global_variables: HashMap::new(),
			local_variables: Vec::new(),
			num_while_loops_deep: 0,
			current_fn_name: None,
		}
	}

	pub fn fill_result_types(&mut self, entity_name: &str, ast: &mut AST<'a>, arena: &'a Arena) -> Result<(), TypePropogatorError> {
		self.add_global_variable(nt!("me"), GrugType::Id{custom_name: Some(Box::leak(NTStr::box_from_str_in(entity_name, arena)))})?;

		let variables = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::Variable(x) => Some(x), _ => None})
			.collect::<Vec<_>>();
		for variable in variables {
			self.check_global_expr(&variable.assignment_expr, variable.name.as_str())?;
			let result_ty = self.fill_expr(&*ast.helper_fn_signatures, &mut variable.assignment_expr.into(), arena)?;

			if let ExprData::Literal(LiteralExprData::Identifier(name)) = &variable.assignment_expr.data 
				&& name.as_str() == "me" {
				// grug_assert(!streq(global->assignment_expr.literal.string, "me"), "Global variables can't be assigned 'me'");
				return Err(TypePropogatorError::GlobalCantBeAssignedMe {
					name: Arc::from(name.as_str()),
				});
			}
			if !(variable.ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && result_ty != variable.ty {
				return Err(TypePropogatorError::VariableTypeMismatch {
					name: Arc::from(variable.name.as_str()),
					got_type: result_ty.clone(),
					expected_type: variable.ty.clone(),
				});
			}
			self.add_global_variable(variable.name.as_str(), result_ty)?;
		}

		let mut previous_on_fn_index = 0;
		// on functions need to be iterated separately because grug_tests expects a certain order to the on_functions
		let mut on_functions = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::OnFunction(x) => Some(x), _ => None})
			.collect::<Vec<_>>();
		for (on_fn_name, mod_api_on_fn) in 
			self.entity.on_fns.iter()
		{
			let Some((current_index, current_on_fn)) = 
				on_functions.iter_mut().enumerate()
				.find(|(_, on_fn)| on_fn.name.as_str() == &**on_fn_name) else 
			{
				continue;
			};
			if previous_on_fn_index > current_index {
				return Err(TypePropogatorError::OutOfOrderOnFn {
					entity_name: Arc::from(entity_name),
					on_fn_name: Arc::from(current_on_fn.name.as_str()),
				});
			}
			previous_on_fn_index = current_index;
			
			if mod_api_on_fn.arguments.len() > current_on_fn.arguments.len() {
				return Err(TypePropogatorError::TooFewParameters{
					function_name: Arc::from(current_on_fn.name.as_str()),
					expected_name: Arc::from(mod_api_on_fn.arguments[current_on_fn.arguments.len()].name.as_str()),
					expected_type: mod_api_on_fn.arguments[current_on_fn.arguments.len()].ty.clone(),
				});
			} else if mod_api_on_fn.arguments.len() < current_on_fn.arguments.len() {
				return Err(TypePropogatorError::TooManyParameters{
					function_name: Arc::from(current_on_fn.name.as_str()),
					parameter_name: Arc::from(current_on_fn.arguments[mod_api_on_fn.arguments.len()].name.to_str()),
					parameter_type: current_on_fn.arguments[mod_api_on_fn.arguments.len()].ty.into(),
				});
			}
			for (param, arg) in mod_api_on_fn.arguments.iter().zip(current_on_fn.arguments.iter()) {
				if param.name != arg.name.to_ntstr() {
					return Err(TypePropogatorError::OnFnParameterNameMismatch {
						function_name: Arc::from(current_on_fn.name.as_str()),
						got_name: Arc::from(arg.name.to_str()),
						expected_name: Arc::from(param.name.as_str()),
					});
				}
				if param.ty != arg.ty.into() {
					return Err(TypePropogatorError::OnFnParameterTypeMismatch {
						function_name: Arc::from(current_on_fn.name.as_str()),
						parameter_name: Arc::from(param.name.as_str()),
						got_type: arg.ty.into(),
						expected_type: param.ty.into(),
					});
				}
			}
			// These should only be set inside self.fill_statements
			debug_assert!(self.local_variables.is_empty());
			debug_assert!(self.num_while_loops_deep == 0);
			debug_assert!(self.current_fn_name.is_none());

			self.current_fn_name = Some(current_on_fn.name.as_str());
			self.push_scope();
			for arg in current_on_fn.arguments {
				self.add_local_variable(arg.name.to_str(), arg.ty.into())?;
			}
			self.fill_statements(&ast.helper_fn_signatures, &mut current_on_fn.body_statements, &GrugType::Void, arena)?;
			self.pop_scope();

			debug_assert!(self.current_fn_name.as_deref() == Some(current_on_fn.name.as_str()));
			self.current_fn_name = None;
		}
		let entity_on_functions = &self.entity.on_fns;
		for on_fn in on_functions {
			if !entity_on_functions.iter().any(|(name, _)| **name == *on_fn.name.as_str()) {
				return Err(TypePropogatorError::OnFnDoesNotExist {
					function_name: Arc::from(on_fn.name.as_str()),
					entity_name: Arc::from(entity_name),
				});
			}
		}

		for statement in &mut ast.global_statements {
			match statement {
				GlobalStatement::Variable(_) => (),
				GlobalStatement::OnFunction(_) => (),
				GlobalStatement::EmptyLine => (),
				GlobalStatement::HelperFunction(HelperFunction{
					name,
					arguments,
					body_statements,
					return_type,
				}) => {
					debug_assert!(self.local_variables.is_empty());
					debug_assert!(self.num_while_loops_deep == 0);
					debug_assert!(self.current_fn_name.is_none());

					self.current_fn_name = Some(name);
					self.push_scope();
					for arg in *arguments {
						self.add_local_variable(arg.name.to_str(), arg.ty.into())?;
					}
					self.fill_statements(&ast.helper_fn_signatures, body_statements, return_type, arena)?;

					if *return_type != GrugType::Void && !matches!(body_statements.last().map(|&x| x.into()), Some(Statement::Return{..})) {
						return Err(TypePropogatorError::LastStatementNotReturn {
							function_name: Arc::from(self.current_fn_name.unwrap()),
							expected_return_type: return_type.clone(),
						});
					}

					self.pop_scope();

					debug_assert!(self.current_fn_name == Some(name));
					self.current_fn_name = None;
				}
				GlobalStatement::Comment{..} => (),
			}
		}
		Ok(())
	}
	
	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_statements(&mut self, helper_fns: &[(&NTStr, (GrugType, &[c_argument<'a>]))], statements: &mut [c_statement], expected_return_type: &GrugType, arena: &'a Arena) -> Result<(), TypePropogatorError> {
		self.push_scope();
		for statement in statements {
			let mut r_statement = (*statement).into();
			match &mut r_statement {
				Statement::Variable{
					name,
					ty,
					assignment_expr
				} => {
					let result_ty = self.fill_expr(helper_fns, assignment_expr, arena)?;
					
					if let Some(ty) = ty {
						if self.get_variable_type(name).is_some() {
							return Err(TypePropogatorError::VariableAlreadyExists{
								variable_name: Arc::from(name.as_str()),
							});
						}
						if !(*ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && *ty != result_ty {
						// if *ty == (GrugType::Id{custom_name: None}) || GrugType::match_non_exact(ty, &result_ty) {
							return Err(TypePropogatorError::VariableTypeMismatch {
								name: Arc::from(name.as_str()),
								got_type: result_ty,
								expected_type: *ty,
							});
						} else {
							self.add_local_variable(name.as_str(), ty.clone())?
						}
					} else {
						let ty = if let Some(ty) = self.get_global_variable_type(name) {
							if matches!(ty, GrugType::Id {..}) {
								return Err(TypePropogatorError::GlobalIdsCantBeReassigned);
							}
							ty
						} else if let Some(ty) = self.get_local_variable_type(name) {
							ty
						} else {
							return Err(TypePropogatorError::CantAssignBecauseVariableDoesntExist {
								name: Arc::from(name.as_str()),
							});
						};

						if !(ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && ty != result_ty {
						// if result_ty != var.ty {
							return Err(TypePropogatorError::VariableTypeMismatch {
								name: Arc::from(name.as_str()),
								got_type: result_ty.clone(),
								expected_type: ty.clone(),
							});
						}
					}
				}
				Statement::Call(expr) => {
					self.fill_expr(helper_fns, expr, arena)?;
				}
				Statement::If {
					condition,
					is_chained: _,
					if_block,
					else_block,
				} => {
					let cond_type = self.fill_expr(helper_fns, condition, arena)?;
					if cond_type != GrugType::Bool {
						return Err(TypePropogatorError::IfConditionTypeMismatch {
							got_type: cond_type
						});
					}
					self.fill_statements(helper_fns, if_block, expected_return_type, arena)?;
					if !else_block.is_empty() {
						self.fill_statements(helper_fns, else_block, expected_return_type, arena)?;
					}
					// TODO: Maybe this should be looked at again
					// [https://github.com/grug-lang/grug/issues/116]
				}
				Statement::While {
					condition,
					block,
				} => {
					let cond_type = self.fill_expr(helper_fns, condition, arena)?;
					if cond_type != GrugType::Bool {
						return Err(TypePropogatorError::WhileConditionTypeMismatch {
							got_type: cond_type
						});
					}
					self.num_while_loops_deep += 1;
					self.fill_statements(helper_fns, block, expected_return_type, arena)?;
					self.num_while_loops_deep -= 1;

				}
				Statement::Return {
					expr,
				} => {
					// result_ty MUST be filled
					let return_ty = expr.as_mut()
						.map(|expr| self.fill_expr(helper_fns, expr, arena))
						.unwrap_or(Ok(GrugType::Void))?;
					if *expected_return_type != (GrugType::Id{custom_name: None}) && *expected_return_type != return_ty {
						return Err(TypePropogatorError::MismatchedReturnType{
							function_name: Arc::from(self.current_fn_name.unwrap()),
							expected_type: expected_return_type.clone(),
							got_type: return_ty.clone(),
						})
					}
				}
				Statement::Break => {
					if self.num_while_loops_deep == 0 {
						return Err(TypePropogatorError::BreakStatementOutsideWhileLoop);
					}
				}
				Statement::Continue => {
					if self.num_while_loops_deep == 0 {
						return Err(TypePropogatorError::ContinueStatementOutsideWhileLoop);
					}
				}
				_ => (),
			}
			*statement = r_statement.into();
		}
		self.pop_scope();
		Ok(())
	}

	// Check that the global variable's assigned value doesn't contain a call_to a helper function nor identifier
	fn check_global_expr(&mut self, assignment_expr: &Expr<'_>, name: &str) -> Result<(), TypePropogatorError> {
		match assignment_expr.data {
			ExprData::Literal(LiteralExprData::Entity(_)) => unreachable!(),
			ExprData::Literal(LiteralExprData::Resource(_)) => unreachable!(),
			ExprData::Literal(_) => (),
			ExprData::Unary{
				op: _,
				expr,
			} => self.check_global_expr(&(*expr).into(), name)?,
			ExprData::Binary{
				left,
				right,
				op: _,
			} => {
				self.check_global_expr(&(*left).into(), name)?;
				self.check_global_expr(&(*right).into(), name)?;
			},
			ExprData::Call{
				name: fn_name,
				args,
			} => {
				if fn_name.starts_with("helper_") {
					Err(TypePropogatorError::GlobalCantCallHelperFn{
						global_name: Arc::from(name),
					})?;
				}
				args.iter().map(|argument| self.check_global_expr(&(*argument).into(), name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprData::Parenthesized(expr) => self.check_global_expr(&(*expr).into(), name)?,
		}
		Ok(())
	}

	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_expr(&mut self, helper_fns: &[(&NTStr, (GrugType, &[c_argument<'a>]))], assignment_expr: &mut c_expr<'a>, arena: &'a Arena) -> Result<GrugType, TypePropogatorError> {
		let new_expr: Expr = (*assignment_expr).into();
		// MUST be None before type propogation
		assert!(new_expr.result_type.is_none());
		let result_ty = match &mut new_expr.data {
			ExprData::Literal(expr) => {
				match expr {
					LiteralExprData::True => GrugType::Bool,
					LiteralExprData::False => GrugType::Bool,
					LiteralExprData::String{
						..
					} => GrugType::String,
					LiteralExprData::Resource{..} | LiteralExprData::Entity{..} => {
						panic!("Cannot encounter resource or entity string when filling expression");
					}
					LiteralExprData::Identifier(name) => {
						let ty = self.get_variable_type(&*name).ok_or_else(|| TypePropogatorError::VariableDoesNotExist{
							name: Arc::from(name.as_str()),
						})?;
						ty
					},
					LiteralExprData::Number{
						..
					} => GrugType::Number,
				}
			},
			ExprData::Unary{
				op: operator,
				expr,
			} => {
				*expr = {
					let r_expr = (**expr).into();
					if let Expr{data: ExprData::Unary{op: next_operator, ..}, ..} = r_expr && next_operator == operator {
						return Err(TypePropogatorError::AdjacentUnaryOperators{
							operator
						});
					}
					r_expr.into()
				};
				let result_ty = self.fill_expr(helper_fns, expr, arena)?;
				match (operator, &result_ty) {
					(UnaryOperator::Not, GrugType::Bool) => (),
					(UnaryOperator::Not, got) => return Err(TypePropogatorError::NotOperatorNotBeforeBool{
						got,
					}),
					(UnaryOperator::Minus, GrugType::Number) => (),
					(UnaryOperator::Minus, got) => return Err(TypePropogatorError::MinusOperatorNotBeforeNumber{
						got,
					}),
					// _ => (),
				};
				result_ty
			},
			ExprData::Binary{
				left,
				right,
				op,
			} => {
				let result_0 = self.fill_expr(helper_fns, &mut left, arena)?;
				let result_1 = self.fill_expr(helper_fns, &mut right, arena)?;
				match (&result_1, op) {
					(GrugType::String, BinaryOperator::DoubleEquals) | 
					(GrugType::String, BinaryOperator::NotEquals) => (),
					(GrugType::String, _) => {
						return Err(TypePropogatorError::CannotCompareStrings{
							operator: op
						});
					},
					_ => (),
				}
				if !GrugType::match_non_exact(&result_0, &result_1) {
					return Err(TypePropogatorError::BinaryOperatorTypeMismatch{
						operator: op,
						left: result_0,
						right: result_1,
					});
				}

				match op {
					BinaryOperator::Or | BinaryOperator::And => {
						if result_0 != GrugType::Bool {
							return Err(TypePropogatorError::LogicalOperatorExpectsBool {
								operator: op,
							});
						}
						GrugType::Bool
					}
					BinaryOperator::DoubleEquals | BinaryOperator::NotEquals => {
						GrugType::Bool
					},
					BinaryOperator::Greater | BinaryOperator::GreaterEquals | 
					BinaryOperator::Less | BinaryOperator::LessEquals => {
						if result_0 != GrugType::Number {
							return Err(TypePropogatorError::ComparisonOperatorExpectsNumber {
								operator: op,
								got_type: result_0,
							});
						}
						GrugType::Bool
					},
					BinaryOperator::Plus | BinaryOperator::Minus |
					BinaryOperator::Multiply | BinaryOperator::Division => {
						if result_0 != GrugType::Number {
							return Err(TypePropogatorError::ArithmeticOperatorExpectsNumber {
								operator: op,
								got_type: result_0,
							});
						}
						result_0
					},
					BinaryOperator::Remainder => {
						if result_0 != GrugType::Number {
							return Err(TypePropogatorError::RemainderOperatorExpectsNumber {
								got_ty: result_0,
							});
						}
						result_0
					},
				}
			},
			ExprData::Call{
				name: fn_name,
				args,
			} => {
				// TODO: Move this line to within check_arguments
				args.iter_mut().map(|argument| self.fill_expr(helper_fns, argument, arena)).collect::<Result<Vec<_>, _>>()?;
				if let Some((_, (return_ty, sig_arguments))) = helper_fns.iter().find(|(name, _)| *name == fn_name.as_str()) {
					self.check_arguments(fn_name, sig_arguments, args, arena)?;
					return_ty.clone()
				} else if let Some(game_fn) = self.game_fns.get(fn_name) {
					self.check_arguments(fn_name, &game_fn.arguments, args, arena)?;
					game_fn.return_ty
				} else if fn_name.starts_with("on_") {
					return Err(TypePropogatorError::CallOnFnWithinOnFn {
						on_fn_name: Arc::from(fn_name)
					});
				} else {
					return Err(TypePropogatorError::FunctionDoesNotExist {
						function_name: Arc::from(fn_name)
					});
				}
			},
			ExprData::Parenthesized(expr) => {
				self.fill_expr(helper_fns, expr, arena)?
			},
		};
		new_expr.result_ty = Some(result_ty);
		*assignment_expr = new_expr.into();
		Ok(result_ty)
	}

	fn check_arguments(&mut self, function_name: &NTStr, signature: &[c_argument<'_>], arguments: &mut [c_expr<'a>], arena: &'a Arena) -> Result<(), TypePropogatorError> {
		debug_assert!(arguments.iter().all(|arg| arg.into().result_type.is_some()));
		if signature.len() > arguments.len() {
			return Err(TypePropogatorError::TooFewArguments{
				function_name: Arc::from(function_name.as_str()),
				expected_name: Arc::from(&signature[arguments.len()].name.as_str()),
				expected_type: signature[arguments.len()].ty,
			});
		} else if signature.len() < arguments.len() {
			return Err(TypePropogatorError::TooManyArguments{
				function_name: Arc::from(function_name),
				got_type: arguments[signature.len()].result_type.unwrap(),
			});
		}
		for (param, arg) in signature.iter().zip(arguments) {
			if let GrugType::Resource{extension} = param.into().ty 
				&& let ExprData::Literal(LiteralExprData::String(mut value)) = arg.into().data {
				self.validate_resource_string(value, extension.as_str())?;
				value = self.fix_resource_string(value);
				*arg = {
					Expr {
						data: ExprData::Literal(LiteralExprData::String(value)),
						result_type: Some(GrugType::Resource{
							extension,
						}),
					}.into()
				};
			} else if let GrugType::Entity{entity_type: ty} = param.into().ty 
				&& let ExprData::Literal(LiteralExprData::String(mut value)) = arg.into().ty {
				self.validate_entity_string(value)?;
				if let Some(fixed_entity) = self.fix_entity_string(value, arena) {value = fixed_entity}
				*arg = {
					Expr {
						data: ExprData::Literal(LiteralExprData::String(value)),
						result_type: Some(GrugType::Entity{
							entity_type: ty,
						}),
					}.into()
				};
			} else if arg.result_ty.as_ref().unwrap() == &GrugType::Void {
				return Err(TypePropogatorError::VoidArgumentInFunctionCall{
					function_name: Arc::clone(function_name),
					signature_type: param.ty.clone(),
					parameter_name: Arc::clone(&param.name),
				});
			} else if let GrugType::Id{custom_name: None} = param.ty && let Some(GrugType::Id{custom_name: _}) = arg.result_ty {
				arg.result_ty = Some(GrugType::Id{custom_name: None});
			} else if Some(&param.ty) != arg.result_ty.as_ref() {
				return Err(TypePropogatorError::FunctionArgumentMismatch {
					function_name: Arc::clone(function_name),
					expected_type: param.ty.clone(),
					got_type: arg.result_ty.as_ref().unwrap().clone(),
					parameter_name: Arc::clone(&param.name),
				});
			}
		}
		Ok(())
	}

	fn validate_resource_string(&mut self, value: &mut Arc<NTStr>, extension: &Arc<str>) -> Result<(), ResourceValidationError> {
		if value.is_empty() {
			Err(ResourceValidationError::EmptyResource{ })
		} else if value.starts_with("/") {
			Err(ResourceValidationError::LeadingForwardSlash {
				value: Arc::clone(value),
			})
		} else if value.ends_with("/") {
			Err(ResourceValidationError::TrailingForwardSlash {
				value: Arc::clone(value),
			})
		} else if value.contains("\\") {
			Err(ResourceValidationError::ContainsBackslash {
				value: Arc::clone(value),
			})
		} else if value.contains("//") {
			Err(ResourceValidationError::ContainsDoubleForwardSlash {
				value: Arc::clone(value),
			})
		} else if &***value == ".." || value.starts_with("../") {
			Err(ResourceValidationError::BeginsWithDotDotWithoutSlash {
				value: Arc::clone(value),
			})
		} else if value.ends_with("/..") || value.contains("/../") {
			Err(ResourceValidationError::ContainsSlashDotDotInMiddle {
				value: Arc::clone(value),
			})
		} else if &***value == "." || value.starts_with("./") {
			Err(ResourceValidationError::BeginsWithDotWithoutSlash {
				value: Arc::clone(value),
			})
		} else if value.ends_with("/.") || value.contains("/./") {
			Err(ResourceValidationError::ContainsSlashDotInMiddle {
				value: Arc::clone(value),
			})
		} else if value.ends_with(".") {
			Err(ResourceValidationError::EndsWithDot {
				value: Arc::clone(value),
			})
		} else if value.ends_with(&**extension) {
			Ok(())
		} else {
			Err(ResourceValidationError::ExtensionMismatch {
				expected: Arc::clone(extension),
				value: Arc::clone(value),
			})
		}
	}

	fn fix_resource_string(&mut self, value: &NTStr, arena: &'a Arena) -> &'a NTStr {
		Box::leak(NTStr::box_from_str_in(&format!("{}/{}", self.current_mod_name, value), arena))
	}

	fn validate_entity_string(&mut self, entity_string: &mut Arc<NTStr>) -> Result<(), EntityValidationError> {
		if entity_string.is_empty() {
			return Err(EntityValidationError::EntityCantBeEmpty);
		}

		let (mod_name, entity_name) = if let Some((mod_name, entity_name)) = entity_string.split_once(":") {
			if mod_name.is_empty() {
				return Err(EntityValidationError::EntityMissingModName {
					entity_string: Arc::clone(entity_string),
				});
			}
			if entity_name.is_empty() {
				return Err(EntityValidationError::EntityMissingEntityName {
					mod_name: String::from(mod_name),
					entity_string: Arc::clone(entity_string),
				});
			}
			if mod_name == self.current_mod_name {
				return Err(EntityValidationError::ModNameIsCurrentMod {
					full_entity_string: Arc::clone(entity_string),
					mod_name: String::from(mod_name),
					entity_name: String::from(entity_name),
				});
				// grug_assert(!streq(mod_name, mod), "Entity '%s' its mod name '%s' is invalid, since the file it is in refers to its own mod; just change it to '%s'", string, mod_name, entity_name);
			}
			(mod_name, entity_name)
		} else {
			("", &***entity_string)
		};

		if let Some(ch) = mod_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return Err(EntityValidationError::ModNameHasInvalidCharacter{
				entity_name: Arc::clone(entity_string),
				invalid_char: ch,
			});
		}
		if let Some(ch) = entity_name.chars().find(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')) {
			return Err(EntityValidationError::EntityNameHasInvalidCharacter{
				entity_name: Arc::clone(entity_string),
				invalid_char: ch,
			});
		}
		Ok(())
	}

	fn fix_entity_string(&mut self, value: &NTStr, arena: &'a Arena) -> Option<&'a NTStr> {
		if value.split_once(":").is_none() {
			Some(Box::leak(NTStr::box_from_str_in(&format!("{}:{}", self.current_mod_name, value), arena)))
		} else {
			None
		}
	}

	fn get_variable_type(&self, var_name: &str) -> Option<GrugType> {
		// TODO: also do local variables
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

	fn get_local_variable_type(&self, var_name: &str) -> Option<GrugType> {
		for scope in self.local_variables.iter().rev() {
			if let var@Some(_) = scope.get(var_name) {
				return var.cloned();
			}
		}
		None
	}

	fn get_global_variable_type(&self, var_name: &str) -> Option<GrugType> {
		self.global_variables.get(var_name).cloned()
	}

	fn add_local_variable(&mut self, name: &'a str, ty: GrugType<'a>) -> Result<(), TypePropogatorError> {
		if self.get_global_variable_type(&name).is_some() {
			return Err(TypePropogatorError::LocalVariableShadowedByGlobal{
				name,
			});
		}
		match self.local_variables.last_mut().expect("There is no local scope to push onto").entry(Arc::clone(&name)) {
			Entry::Occupied(_) => return Err(TypePropogatorError::LocalVariableShadowedByLocal{
				name,
			})?,
			Entry::Vacant(x) => {x.insert(ty);},
		}
		Ok(())
	}

	fn add_global_variable(&mut self, name: &'a str, ty: GrugType<'a>) -> Result<(), TypePropogatorError> {
		match self.global_variables.entry(Arc::clone(&name)) {
			Entry::Occupied(_) => return Err(TypePropogatorError::GlobalVariableShadowed{
				name,
			})?,
			Entry::Vacant(x) => {x.insert(ty);},
		}
		Ok(())
	}
}
