use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;
use crate::ntstring::NTStr;
use crate::types::*;
use crate::state::GrugState;
use crate::frontend::parser::AST;

type GrugIDType = *mut ();

#[allow(dead_code)]
enum GrugValue {
	_Bool(bool),
	_I32(i32),
	_F32(f32),
	_String(String),
	_Id(GrugIDType),
	_Resource(String),
	_Entity(GrugIDType),
	Uninitialized,
}
struct Variable {
	name: Arc<str>,
	ty: GrugType,
	value: GrugValue,
}

pub(super) struct TypePropogator<'a> {
	grug_state: &'a GrugState,
	current_mod_name: String,
	global_variables: HashMap<Arc<str>, Variable>,
	local_variables: Vec<HashMap<Arc<str>, Variable>>,
	num_while_loops_deep: usize,
	current_fn_calls_helper_fn: bool,
	current_fn_has_while_loop: bool,
	current_fn_name: Option<Arc<str>>,
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
		line: usize,
		col: usize,
	},
	// grug_assert(var, "The variable '%s' does not exist", expr->literal.string);
	VariableDoesNotExist {
		name: Arc<str>,
		line: usize, 
		col: usize,
	},
	// grug_assert(expr->unary.operator != expr->unary.expr->unary.operator, "Found '%s' directly next to another '%s', which can be simplified by just removing both of them", get_token_type_str[expr->unary.operator], get_token_type_str[expr->unary.expr->unary.operator]);
	AdjacentUnaryOperators {
		operator: UnaryOperator,
	},
	// grug_assert(expr->result_type == type_bool, "Found 'not' before %s, but it can only be put before a bool", expr->result_type_name);
	NotOperatorNotBeforeBool{
		got: GrugType,
	},
	// grug_assert(expr->result_type == type_i32 || expr->result_type == type_f32, "Found '-' before %s, but it can only be put before an i32 or f32", expr->result_type_name);
	MinusOperatorNotBeforeNumber {
		got: GrugType,
	},
	// grug_assert(binary_expr.operator == EQUALS_TOKEN || binary_expr.operator == NOT_EQUALS_TOKEN, "You can't use the %s operator on a string", get_token_type_str[binary_expr.operator]);
	CannotCompareStrings {
		// This can only be BinaryOperator::DoubleEquals | BinaryOperator::NotEquals
		operator: BinaryOperator,
	},
	// grug_error("The left and right operand of a binary expression ('%s') must have the same type, but got %s and %s", get_token_type_str[binary_expr.operator], binary_expr.left_expr->result_type_name, binary_expr.right_expr->result_type_name);
	BinaryOperatorTypeMismatch {
		operator: BinaryOperator,
		left: GrugType,
		right: GrugType,
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
		got_type: GrugType,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_i32 || binary_expr.left_expr->result_type == type_f32, "'%s' operator expects i32 or f32", get_token_type_str[binary_expr.operator]);
	ArithmeticOperatorExpectsNumber {
		// Must be '+', '-', '*', or '/' operators
		operator: BinaryOperator,
		got_type: GrugType,
	},
	// grug_assert(binary_expr.left_expr->result_type == type_i32, "'%%' operator expects i32");
	RemainderOperatorExpectsNumber {
		got_ty: GrugType,
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
		expected_type: GrugType,
	},
	// grug_assert(call_expr.argument_count <= param_count, "Function call '%s' got an unexpected extra argument with type %s", name, call_expr.arguments[param_count].result_type_name);
	TooManyArguments{
		function_name: Arc<str>,
		got_type: GrugType,
	},
	ResourceValidationError(ResourceValidationError),
	EntityValidationError(EntityValidationError),
	// grug_assert(arg->result_type != type_void, "Function call '%s' expected the type %s for argument '%s', but got a function call that doesn't return anything", name, param.type_name, param.name);
	VoidArgumentInFunctionCall {
		function_name: Arc<str>,
		signature_type: GrugType,
		parameter_name: Arc<str>
	},
	// grug_error("Function call '%s' expected the type %s for argument '%s', but got %s", name, param.type_name, param.name, arg->result_type_name);
	FunctionArgumentMismatch {
		function_name: Arc<str>,
		expected_type: GrugType,
		got_type: GrugType,
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
		expected_type: GrugType,
	},
	// grug_assert(arg_count <= param_count, "Function '%s' got an unexpected extra parameter '%s' with type %s", name, args[param_count].name, args[param_count].type_name);
	TooManyParameters{
		function_name: Arc<str>,
		parameter_name: Arc<str>,
		parameter_type: GrugType,
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
		got_type: GrugType,
		expected_type: GrugType,
	},
	// grug_assert(!streq(global->assignment_expr.literal.string, "me"), "Global variables can't be assigned 'me'");
	GlobalCantBeAssignedMe {
		name: Arc<str>,
	},
	// grug_error("Can't assign %s to '%s', which has type %s", global->assignment_expr.result_type_name, global->name, global->type_name);
	VariableTypeMismatch {
		name: Arc<str>,
		got_type: GrugType,
		expected_type: GrugType,
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
		got_type: GrugType,
	},
	// "While condition must be bool but got '%s'", 
	WhileConditionTypeMismatch {
		got_type: GrugType,
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
		expected_type: GrugType,
		got_type: GrugType,
	},
	// grug_assert(last_statement.type == RETURN_STATEMENT, "Function '%s' is supposed to return %s as its last line", filled_fn_name, fn_return_type_name);
	LastStatementNotReturn {
		function_name: Arc<str>,
		expected_return_type: GrugType,
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
				line: _,
				col: _,
			} => write!(f, "The global variable '{}' isn't allowed to call helper functions", global_name),
			Self::VariableDoesNotExist {
				name,
				line: _, 
				col: _,
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
	pub fn new (state: &'a GrugState, mod_name: String) -> Self {
		Self {
			grug_state: state,
			current_mod_name: mod_name,
			global_variables: HashMap::new(),
			local_variables: Vec::new(),
			num_while_loops_deep: 0,
			current_fn_calls_helper_fn: false,
			current_fn_has_while_loop: false,
			current_fn_name: None,
		}
	}

	pub fn fill_result_types(&mut self, entity_name: &str, ast: &mut AST) -> Result<(), TypePropogatorError> {
		let entity_name = Arc::from(entity_name);
		let entity = self.grug_state.mod_api.entities().get(&*entity_name).ok_or_else(|| TypePropogatorError::EntityDoesNotExist{
			entity_name: Arc::clone(&entity_name),
		})?;
		
		self.add_global_variable(Arc::from("me"), GrugType::Id{custom_name: Some(Arc::clone(&entity_name))})?;

		let variables = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::Variable(x) => Some(x), _ => None})
			.collect::<Vec<_>>();
		for variable in variables {
			self.check_global_expr(&variable.assignment_expr, &variable.name)?;
			let result_ty = self.fill_expr(&ast.helper_fn_signatures, &mut variable.assignment_expr)?;
			
			// global expr cant call helper function and this has been checked
			debug_assert!(!self.current_fn_calls_helper_fn);

			if let ExprType::LiteralExpr{expr: LiteralExpr::IdentifierExpr{name, ..}, ..} = &variable.assignment_expr.ty 
				&& &**name == "me" {
				// grug_assert(!streq(global->assignment_expr.literal.string, "me"), "Global variables can't be assigned 'me'");
				return Err(TypePropogatorError::GlobalCantBeAssignedMe {
					name: Arc::clone(name),
				});
			}
			if variable.ty != (GrugType::Id{custom_name: None}) && result_ty != variable.ty {
			// if &result_ty != ty {
				return Err(TypePropogatorError::VariableTypeMismatch {
					name: Arc::clone(&variable.name),
					got_type: result_ty.clone(),
					expected_type: variable.ty.clone(),
				});
			}
			self.add_global_variable(Arc::clone(&variable.name), result_ty)?;
		}

		let mut previous_on_fn_index = 0;
		// on functions need to be iterated separately because grug_tests expects a certain order to the on_functions
		let mut on_functions = ast.global_statements
			.iter_mut().filter_map(|st| match st {GlobalStatement::OnFunction(x) => Some(x), _ => None})
			.collect::<Vec<_>>();
		for (on_fn_index, (on_fn_name, mod_api_on_fn)) in 
			self.grug_state.mod_api.entities().get(&*entity_name)
			.expect("already verified that entity exists").on_fns.iter().enumerate() 
		{
			let Some((current_index, current_on_fn)) = 
				on_functions.iter_mut().enumerate()
				.find(|(i, on_fn)| &*on_fn.name == &**on_fn_name) else 
			{
				continue;
			};
			if previous_on_fn_index > current_index {
				return Err(TypePropogatorError::OutOfOrderOnFn {
					entity_name: Arc::from(entity_name),
					on_fn_name: Arc::clone(&current_on_fn.name),
				});
			}
			previous_on_fn_index = current_index;
			
			if mod_api_on_fn.arguments.len() > current_on_fn.arguments.len() {
				return Err(TypePropogatorError::TooFewParameters{
					function_name: Arc::clone(&current_on_fn.name),
					expected_name: Arc::clone(&mod_api_on_fn.arguments[current_on_fn.arguments.len()].name),
					expected_type: mod_api_on_fn.arguments[current_on_fn.arguments.len()].ty.clone(),
				});
			} else if mod_api_on_fn.arguments.len() < current_on_fn.arguments.len() {
				return Err(TypePropogatorError::TooManyParameters{
					function_name: Arc::clone(&current_on_fn.name),
					parameter_name: Arc::clone(&current_on_fn.arguments[mod_api_on_fn.arguments.len()].name),
					parameter_type: current_on_fn.arguments[mod_api_on_fn.arguments.len()].ty.clone(),
				});
			}
			for (param, arg) in mod_api_on_fn.arguments.iter().zip(current_on_fn.arguments.iter()) {
				if param.name != arg.name {
					return Err(TypePropogatorError::OnFnParameterNameMismatch {
						function_name: Arc::clone(&current_on_fn.name),
						got_name: Arc::clone(&arg.name),
						expected_name: Arc::clone(&param.name),
					});
				}
				if param.ty != arg.ty {
					return Err(TypePropogatorError::OnFnParameterTypeMismatch {
						function_name: Arc::clone(&current_on_fn.name),
						parameter_name: Arc::clone(&param.name),
						got_type: arg.ty.clone(),
						expected_type: param.ty.clone(),
					});
				}
			}
			// These should only be set inside self.fill_statements
			debug_assert!(self.local_variables.len() == 0);
			debug_assert!(self.num_while_loops_deep == 0);
			debug_assert!(self.current_fn_calls_helper_fn == false);
			debug_assert!(self.current_fn_has_while_loop == false);
			debug_assert!(self.current_fn_name == None);

			self.current_fn_name = Some(Arc::clone(&current_on_fn.name));
			self.push_scope();
			for arg in &current_on_fn.arguments {
				self.add_local_variable(Arc::clone(&arg.name), arg.ty.clone())?;
			}
			self.fill_statements(&ast.helper_fn_signatures, &mut current_on_fn.body_statements, &GrugType::Void)?;
			self.pop_scope();

			debug_assert!(self.current_fn_name.as_ref() == Some(&current_on_fn.name));
			self.current_fn_name = None;

			current_on_fn.calls_helper_fn = self.current_fn_calls_helper_fn;
			current_on_fn.has_while_loop = self.current_fn_has_while_loop;
			self.current_fn_calls_helper_fn = false;
			self.current_fn_has_while_loop  = false;
		}
		let entity_on_functions = &self.grug_state.mod_api.entities().get(&*entity_name)
			.expect("already checked that entity exists").on_fns;
		for on_fn in on_functions {
			if entity_on_functions.iter().find(|(name, entity_on_fn)| &**name == &*on_fn.name).is_none() {
				return Err(TypePropogatorError::OnFnDoesNotExist {
					function_name: Arc::clone(&on_fn.name),
					entity_name: Arc::clone(&entity.name),
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
					calls_helper_fn,
					has_while_loop,
					return_ty,
				}) => {
					debug_assert!(self.local_variables.len() == 0);
					debug_assert!(self.num_while_loops_deep == 0);
					debug_assert!(self.current_fn_calls_helper_fn == false);
					debug_assert!(self.current_fn_has_while_loop == false);
					debug_assert!(self.current_fn_name == None);

					self.current_fn_name = Some(Arc::clone(name));
					self.push_scope();
					for arg in arguments {
						self.add_local_variable(Arc::clone(&arg.name), arg.ty.clone())?;
					}
					self.fill_statements(&ast.helper_fn_signatures, body_statements, return_ty)?;
					self.pop_scope();

					debug_assert!(self.current_fn_name.as_ref() == Some(name));
					self.current_fn_name = None;

					*calls_helper_fn = self.current_fn_calls_helper_fn;
					*has_while_loop = self.current_fn_has_while_loop;
					self.current_fn_calls_helper_fn = false;
					self.current_fn_has_while_loop  = false;
				}
				GlobalStatement::Comment{..} => (),
			}
		}
		Ok(())
	}
	
	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_statements(&mut self, helper_fns: &HashMap<Arc<str>, (GrugType, Vec<Argument>)>, statements: &mut [Statement], expected_return_type: &GrugType) -> Result<(), TypePropogatorError> {
		self.push_scope();
		for statement in &mut *statements {
			match statement {
				Statement::VariableStatement {
					name,
					ty,
					assignment_expr
				} => {
					let result_ty = self.fill_expr(helper_fns, assignment_expr)?;
					
					if let Some(ty) = ty {
						if self.get_variable(name).is_some() {
							return Err(TypePropogatorError::VariableAlreadyExists{
								variable_name: Arc::clone(name),
							});
						}
						if !(*ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && *ty != result_ty {
						// if *ty == (GrugType::Id{custom_name: None}) || GrugType::match_non_exact(ty, &result_ty) {
							return Err(TypePropogatorError::VariableTypeMismatch {
								name: Arc::clone(name),
								got_type: result_ty.clone(),
								expected_type: ty.clone(),
							});
						} else {
							self.add_local_variable(Arc::clone(name), ty.clone())?
						}
					} else {
						let var = if let Some(var) = self.get_global_variable(name) {
							if matches!(var.ty, GrugType::Id {..}) {
								return Err(TypePropogatorError::GlobalIdsCantBeReassigned);
							}
							var
						} else if let Some(var) = self.get_local_variable(name) {
							var
						} else {
							return Err(TypePropogatorError::CantAssignBecauseVariableDoesntExist {
								name: Arc::clone(name),
							});
						};

						if !(var.ty == GrugType::Id{custom_name: None} && matches!(result_ty, GrugType::Id{..})) && var.ty != result_ty {
						// if result_ty != var.ty {
							return Err(TypePropogatorError::VariableTypeMismatch {
								name: Arc::clone(name),
								got_type: result_ty.clone(),
								expected_type: var.ty.clone(),
							});
						}
					}
				}
				Statement::CallStatement {
					expr
				} => {
					self.fill_expr(helper_fns, expr)?;
				}
				Statement::IfStatement {
					condition,
					if_statements,
					else_if_statements,
					else_statements,
				} => {
					let cond_type = self.fill_expr(helper_fns, condition)?;
					if cond_type != GrugType::Bool {
						return Err(TypePropogatorError::IfConditionTypeMismatch {
							got_type: cond_type
						});
					}
					self.fill_statements(helper_fns, if_statements, expected_return_type)?;
					for (condition, else_if_statements) in else_if_statements {
						let cond_type = self.fill_expr(helper_fns, condition)?;
						if cond_type != GrugType::Bool {
							return Err(TypePropogatorError::IfConditionTypeMismatch {
								got_type: cond_type
							});
						}
						self.fill_statements(helper_fns, else_if_statements, expected_return_type)?;
					}
					if let Some(else_statements) = else_statements {
						self.fill_statements(helper_fns, else_statements, expected_return_type)?;
					}
					// TODO: Maybe this should be looked at again
					// [https://github.com/grug-lang/grug/issues/116]
				}
				Statement::WhileStatement {
					condition,
					statements,
				} => {
					let cond_type = self.fill_expr(helper_fns, condition)?;
					if cond_type != GrugType::Bool {
						return Err(TypePropogatorError::WhileConditionTypeMismatch {
							got_type: cond_type
						});
					}
					self.num_while_loops_deep += 1;
					self.fill_statements(helper_fns, statements, expected_return_type)?;
					self.num_while_loops_deep -= 1;
					self.current_fn_has_while_loop = true;

				}
				Statement::ReturnStatement {
					expr,
				} => {
					// result_ty MUST be filled
					let return_ty = expr.as_mut()
						.map(|expr| self.fill_expr(helper_fns, expr))
						.unwrap_or(Ok(GrugType::Void))?;
					if *expected_return_type != (GrugType::Id{custom_name: None}) && *expected_return_type != return_ty {
						return Err(TypePropogatorError::MismatchedReturnType{
							function_name: Arc::clone(&self.current_fn_name.as_ref().unwrap()),
							expected_type: expected_return_type.clone(),
							got_type: return_ty.clone(),
						})
					}
				}
				Statement::BreakStatement => {
					if self.num_while_loops_deep == 0 {
						return Err(TypePropogatorError::BreakStatementOutsideWhileLoop);
					}
				}
				Statement::ContinueStatement => {
					if self.num_while_loops_deep == 0 {
						return Err(TypePropogatorError::ContinueStatementOutsideWhileLoop);
					}
				}
				_ => (),
			}
		}
		if *expected_return_type != GrugType::Void && !matches!(statements.last(), Some(Statement::ReturnStatement{..})) {
			return Err(TypePropogatorError::LastStatementNotReturn {
				function_name: Arc::clone(&self.current_fn_name.as_ref().unwrap()),
				expected_return_type: expected_return_type.clone(),
			});
		}
		self.pop_scope();
		Ok(())
	}

	// Check that the global variable's assigned value doesn't contain a call_to a helper function nor identifier
	fn check_global_expr(&mut self, assignment_expr: &Expr, name: &Arc<str>) -> Result<(), TypePropogatorError> {
		match assignment_expr.ty {
			ExprType::LiteralExpr{expr: LiteralExpr::EntityExpr{..}, ..} => unreachable!(),
			ExprType::LiteralExpr{expr: LiteralExpr::ResourceExpr{..}, ..} => unreachable!(),
			ExprType::LiteralExpr{..} => (),
			ExprType::UnaryExpr{
				operator: _,
				ref expr,
			} => self.check_global_expr(&*expr, name)?,
			ExprType::BinaryExpr{
				ref operands,
				operator: _,
			} => {
				self.check_global_expr(&operands.0, name)?;
				self.check_global_expr(&operands.1, name)?;
			},
			ExprType::CallExpr{
				ref function_name,
				ref arguments,
				line,
				col,
			} => {
				if function_name.starts_with("helper_") {
					Err(TypePropogatorError::GlobalCantCallHelperFn{
						global_name: Arc::clone(name),
						line,
						col,
					})?;
				}
				arguments.iter().map(|argument| self.check_global_expr(argument, name))
					.collect::<Result<Vec<_>, _>>()?;
			},
			ExprType::ParenthesizedExpr{
				ref expr,
				..
			} => self.check_global_expr(&*expr, name)?,
		}
		Ok(())
	}

	// out parameter self.current_on_fn_calls_helper_fn
	fn fill_expr(&mut self, helper_fns: &HashMap<Arc<str>, (GrugType, Vec<Argument>)>, assignment_expr: &mut Expr) -> Result<GrugType, TypePropogatorError> {
		// MUST be None before type propogation
		assert!(matches!(assignment_expr.result_ty, None));
		let result_ty = match assignment_expr.ty {
			ExprType::LiteralExpr{
				ref mut expr,
				line,
				col,
			} => {
				match expr {
					LiteralExpr::TrueExpr => GrugType::Bool,
					LiteralExpr::FalseExpr => GrugType::Bool,
					LiteralExpr::StringExpr{
						..
					} => GrugType::String,
					LiteralExpr::ResourceExpr{
						..
					} => {
						// TODO: Figure out why this is
						panic!("This is supposed to be unreachable but i don't remember why");
					}
					LiteralExpr::EntityExpr{
						..
					} => {
						// TODO: Figure out why this is
						panic!("This is supposed to be unreachable but i don't remember why");
					}
					LiteralExpr::IdentifierExpr{
						name
					} => {
						let var = self.get_variable(&*name).ok_or_else(|| TypePropogatorError::VariableDoesNotExist{
							name: Arc::clone(name),
							line,
							col,
						})?;
						var.ty.clone()
					},
					LiteralExpr::NumberExpr{
						..
					} => GrugType::Number,
				}
			},
			ExprType::UnaryExpr{
				operator,
				ref mut expr,
			} => {
				if let ExprType::UnaryExpr{operator: next_operator, ..} = expr.ty && next_operator == operator {
					return Err(TypePropogatorError::AdjacentUnaryOperators{
						operator
					});
				}
				let result_ty = self.fill_expr(helper_fns, expr)?;
				match (operator, &result_ty) {
					(UnaryOperator::Not, GrugType::Bool) => (),
					(UnaryOperator::Not, got@_) => return Err(TypePropogatorError::NotOperatorNotBeforeBool{
						got: got.clone(),
					}),
					(UnaryOperator::Minus, GrugType::Number) => (),
					(UnaryOperator::Minus, got@_) => return Err(TypePropogatorError::MinusOperatorNotBeforeNumber{
						got: got.clone(),
					}),
					// _ => (),
				};
				result_ty
			},
			ExprType::BinaryExpr{
				ref mut operands,
				operator,
			} => {
				let result_0 = self.fill_expr(helper_fns, &mut operands.0)?;
				let result_1 = self.fill_expr(helper_fns, &mut operands.1)?;
				match (&result_1, operator) {
					(GrugType::String, BinaryOperator::DoubleEquals) | 
					(GrugType::String, BinaryOperator::NotEquals) => {
						return Err(TypePropogatorError::CannotCompareStrings{
							operator
						});
					},
					_ => (), 
				}
				if !GrugType::match_non_exact(&result_0, &result_1) {
					return Err(TypePropogatorError::BinaryOperatorTypeMismatch{
						operator,
						left: result_0,
						right: result_1,
					});
				}

				match operator {
					BinaryOperator::Or | BinaryOperator::And => {
						if result_0 != GrugType::Bool {
							return Err(TypePropogatorError::LogicalOperatorExpectsBool {
								operator,
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
								operator,
								got_type: result_0,
							});
						}
						GrugType::Bool
					},
					BinaryOperator::Plus | BinaryOperator::Minus |
					BinaryOperator::Multiply | BinaryOperator::Division => {
						if result_0 != GrugType::Number {
							return Err(TypePropogatorError::ArithmeticOperatorExpectsNumber {
								operator,
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
			ExprType::CallExpr{
				ref mut function_name,
				ref mut arguments,
				line: _,
				col: _,
			} => {
				// TODO: Move this line to within check_arguments
				arguments.iter_mut().map(|argument| self.fill_expr(helper_fns, argument)).collect::<Result<Vec<_>, _>>()?;
				if function_name.starts_with("helper_") {
					self.current_fn_calls_helper_fn = true;
				}
				if let Some((return_ty, sig_arguments)) = helper_fns.get(function_name) {
					self.check_arguments(function_name, sig_arguments, arguments)?;
					return_ty.clone()
				} else if let Some(game_fn) = self.grug_state.mod_api.game_functions().get(function_name) {
					self.check_arguments(function_name, &game_fn.arguments, arguments)?;
					game_fn.return_ty.clone()
				} else if function_name.starts_with("on_") {
					return Err(TypePropogatorError::CallOnFnWithinOnFn {
						on_fn_name: Arc::clone(function_name)
					});
				} else {
					return Err(TypePropogatorError::FunctionDoesNotExist {
						function_name: Arc::clone(function_name)
					});
				}
			},
			ExprType::ParenthesizedExpr{
				ref mut expr,
				line: _,
				col: _,
			} => {
				self.fill_expr(helper_fns, expr)?
			},
		};
		assignment_expr.result_ty = Some(result_ty.clone());
		Ok(result_ty)
	}

	fn check_arguments(&mut self, function_name: &Arc<str>, signature: &[Argument], arguments: &mut [Expr]) -> Result<(), TypePropogatorError> {
		debug_assert!(arguments.iter().all(|arg| arg.result_ty.is_some()));
		if signature.len() > arguments.len() {
			return Err(TypePropogatorError::TooFewArguments{
				function_name: Arc::clone(function_name),
				expected_name: Arc::clone(&signature[arguments.len()].name),
				expected_type: signature[arguments.len()].ty.clone(),
			});
		} else if signature.len() < arguments.len() {
			return Err(TypePropogatorError::TooManyArguments{
				function_name: Arc::clone(function_name),
				got_type: arguments[signature.len()].result_ty.clone().unwrap(),
			});
		}
		for (param, arg) in signature.iter().zip(arguments) {
			if let GrugType::Resource{ref extension} = param.ty 
				&& let ExprType::LiteralExpr{expr: LiteralExpr::StringExpr{ref mut value}, line, col} = arg.ty {
				self.validate_resource_string(value, extension)?;
				*value = self.fix_resource_string(value);
				arg.result_ty = Some(GrugType::Resource{
					extension: Arc::clone(extension)
				});
			} else if let GrugType::Entity{ref ty} = param.ty 
				&& let ExprType::LiteralExpr{expr: LiteralExpr::StringExpr{ref mut value}, line, col} = arg.ty {
				self.validate_entity_string(value)?;
				if let Some(fixed_entity) = self.fix_entity_string(value) {*value = fixed_entity}
				arg.result_ty = Some(GrugType::Entity{
					ty: ty.clone()
				});
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
					function_name: Arc::clone(&function_name),
					expected_type: param.ty.clone(),
					got_type: arg.result_ty.as_ref().unwrap().clone(),
					parameter_name: Arc::clone(&param.name),
				});
			}
		}
		Ok(())
	}

	fn validate_resource_string(&mut self, value: &mut Arc<NTStr>, extension: &Arc<str>) -> Result<(), ResourceValidationError> {
		if value.len() == 0 {
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
		} else if value.starts_with("..") {
			Err(ResourceValidationError::BeginsWithDotDotWithoutSlash {
				value: Arc::clone(value),
			})
		} else if value.contains("/..") {
			Err(ResourceValidationError::ContainsSlashDotDotInMiddle {
				value: Arc::clone(value),
			})
		} else if value.starts_with(".") {
			Err(ResourceValidationError::BeginsWithDotWithoutSlash {
				value: Arc::clone(value),
			})
		} else if value.contains("/.") {
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

	fn fix_resource_string(&mut self, value: &NTStr) -> Arc<NTStr> {
		NTStr::arc_from_str(&*format!("{}/{}", self.current_mod_name, value))
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

		if let Some(ch) = mod_name.chars().filter(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')).next() {
			return Err(EntityValidationError::ModNameHasInvalidCharacter{
				entity_name: Arc::clone(entity_string),
				invalid_char: ch,
			});
		}
		if let Some(ch) = entity_name.chars().filter(|ch| !(ch.is_ascii_lowercase() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')).next() {
			return Err(EntityValidationError::EntityNameHasInvalidCharacter{
				entity_name: Arc::clone(entity_string),
				invalid_char: ch,
			});
		}
		Ok(())
	}

	fn fix_entity_string(&mut self, value: &NTStr) -> Option<Arc<NTStr>> {
		if let None = value.split_once(":") {
			Some(NTStr::arc_from_str(&*format!("{}:{}", self.current_mod_name, value)))
		} else {
			None
		}
	}

	fn get_variable(&self, var_name: &str) -> Option<&Variable> {
		// TODO: also do local variables
		if let var@Some(_) = self.get_local_variable(var_name) {
			var
		} else {
			self.get_global_variable(var_name)
		}
	}

	fn push_scope(&mut self) {
		self.local_variables.push(HashMap::new());
	}

	fn pop_scope(&mut self) {
		self.local_variables.pop().unwrap();
	}

	fn get_local_variable(&self, var_name: &str) -> Option<&Variable> {
		for scope in self.local_variables.iter().rev() {
			if let var@Some(_) = scope.get(var_name) {
				return var;
			}
		}
		None
	}

	fn get_global_variable(&self, var_name: &str) -> Option<&Variable> {
		self.global_variables.get(var_name)
	}

	fn add_local_variable(&mut self, name: Arc<str>, ty: GrugType) -> Result<(), TypePropogatorError> {
		if let Some(_) = self.get_global_variable(&name) {
			return Err(TypePropogatorError::LocalVariableShadowedByGlobal{
				name,
			});
		}
		match self.local_variables.last_mut().expect("There is no local scope to push onto").entry(Arc::clone(&name)) {
			Entry::Occupied(_) => return Err(TypePropogatorError::LocalVariableShadowedByLocal{
				name,
			})?,
			Entry::Vacant(x) => {x.insert(Variable{
				name,
				ty,
				value: GrugValue::Uninitialized,
			});},
		}
		Ok(())
	}

	fn add_global_variable(&mut self, name: Arc<str>, ty: GrugType) -> Result<(), TypePropogatorError> {
		match self.global_variables.entry(Arc::clone(&name)) {
			Entry::Occupied(_) => return Err(TypePropogatorError::GlobalVariableShadowed{
				name,
			})?,
			Entry::Vacant(x) => {x.insert(Variable{
				name,
				ty,
				value: GrugValue::Uninitialized,
			});},
		}
		Ok(())
	}
}
