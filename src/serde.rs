use crate::error::GrugError;
use crate::frontend::*;
use crate::types::*;
use json::{JsonValue, object};

pub fn dump_file_to_json<'a> (grug_path: &'a str, output_path: &'a str) -> Result<(), GrugError<'a>> {
	let file_text = std::fs::read_to_string(&grug_path).unwrap();

	let tokens = tokenizer::tokenize(&file_text)?;

	let mut ast = parser::parse(&*tokens)?;
	
	Ok(())
}

fn ast_to_json(ast: &[GlobalStatement]) -> String {
	// let mut json_ast = Vec::new();
	ast.iter().map(|statement| match statement {
		GlobalStatement::GlobalVariableStatement{
			name,
			ty,
			assignment_expr,
		} => {
			object! {
				"kind": "global_variable",
				"name": &**name, 
				"type": serialize_type(ty),
				"assignment_expr": serialize_expr(assignment_expr),
			}
		},
		GlobalStatement::GlobalOnFunction{
			name,
			arguments,
			body_statements,
			calls_helper_fn: _,
			has_while_loop: _,
		} => {
			object! {
				"kind": "global_on_function",
				"name": &**name,
				"arguments": arguments.iter().map(serialize_argument).collect::<Vec<_>>(),
				"body_statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
			}
		},
		// GlobalStatement::GlobalHelperFunction{
		// 	name: Arc<str>,
		// 	arguments: Vec<Argument>,
		// 	body_statements: Vec<Statement>,
		// 	calls_helper_fn: bool,
		// 	has_while_loop: bool,
		// 	return_ty: GrugType,
		// } => {

		// },
		// GlobalStatement::GlobalComment{
		// 	value: Arc<str>,
		// } => {

		// },
		// GlobalStatement::GlobalEmptyLine,
		_ => todo!(),
	}).collect::<Vec<_>>();
	todo!();
}

fn serialize_type(ty: &GrugType) -> JsonValue {
	match ty {
		GrugType::Void => "void".into(),
		GrugType::Bool => "bool".into(),
		GrugType::Number => "number".into(),
		GrugType::String => "string".into(),
		GrugType::Id {
			custom_name: None
		} => "id".into(),
		GrugType::Id{
			custom_name: Some(name),
		} => (&**name).into(),
		GrugType::Resource {
			extension,
		} => unreachable!(),
		GrugType::Entity {
			ty,
		} => unreachable!(),
	}
}

fn serialize_expr(expr: &Expr) -> JsonValue {
	match &expr.ty {
		ExprType::LiteralExpr{
			expr,
			line: _,
			col: _,
		} => {
			object! {
				"type": "literal",
				"expr": serializer_literal_expr(expr),
			}
		},
		ExprType::UnaryExpr{
			operator,
			expr,
		} => object! {
			"type": "unary",
			"operator": serialize_unary_operator(operator),
			"expr": serialize_expr(expr),
		},
		ExprType::BinaryExpr{
			operands,
			operator,
		} => object! {
			"type": "binary",
			"operator": serialize_binary_operator(operator),
			"left": serialize_expr(&operands.0),
			"right": serialize_expr(&operands.1),
		},
		ExprType::CallExpr{
			function_name,
			arguments,
			line: _,
			col: _,
		} => object! {
			"type": "call",
			"function_name": &**function_name,
			"arguments": arguments.iter().map(serialize_expr).collect::<Vec<_>>(),
		},
		ExprType::ParenthesizedExpr{
			expr,
			line: _,
			col: _,
		} => object! {
			"type": "parenthesized",
			"expr": serialize_expr(expr),
		},
	}
}

fn serializer_literal_expr(expr: &LiteralExpr) -> JsonValue {
	match expr {
		LiteralExpr::TrueExpr => object! {
			"type": "boolean",
			"value": "true",
		},
		LiteralExpr::FalseExpr => object! {
			"type": "boolean",
			"value": "false",
		},
		LiteralExpr::StringExpr{
			value,
		} => object! { 
			"type": "string",
			"value": &**value,
		},
		LiteralExpr::ResourceExpr{
			value,
		} => object! { 
			"type": "resource",
			"value": &**value,
		},
		LiteralExpr::EntityExpr{
			value,
		} => object! { 
			"type": "entity",
			"value": &**value,
		},
		LiteralExpr::IdentifierExpr{
			name
		} => object! { 
			"type": "identitfier",
			"value": &**name,
		},
		LiteralExpr::NumberExpr {
			value
		} => object! { 
			"type": "number",
			"value": *value,
		},
	}
}

fn serialize_unary_operator(operator: &UnaryOperator) -> JsonValue {
	match operator {
		UnaryOperator::Not => "not".into(),
		UnaryOperator::Minus => "minus".into(),
	}
}

fn serialize_binary_operator(operator: &BinaryOperator) -> JsonValue {
	match operator {
		BinaryOperator::Or => "or".into(),
		BinaryOperator::And => "and".into(),
		BinaryOperator::DoubleEquals => "double_equals".into(),
		BinaryOperator::NotEquals => "not_equals".into(),
		BinaryOperator::Greater => "greater".into(),
		BinaryOperator::GreaterEquals => "greater_equals".into(),
		BinaryOperator::Less => "less".into(),
		BinaryOperator::LessEquals => "less_equals".into(),
		BinaryOperator::Plus => "plus".into(),
		BinaryOperator::Minus => "minus".into(),
		BinaryOperator::Multiply => "mul".into(),
		BinaryOperator::Division => "div".into(),
		BinaryOperator::Remainder => "remainder".into(),
	}
}

fn serialize_argument(argument: &Argument) -> JsonValue {
	object! {
		"name": &*argument.name,
		"type": serialize_type(&argument.ty),
	}
}

fn serialize_statement(statement: &Statement) -> JsonValue {
	match statement {
		Statement::VariableStatement{
			name,
			ty: Some(ty),
			assignment_expr,
		} => object! {
			"kind": "variable",
			"type": serialize_type(ty),
			"assignment_expr": serialize_expr(assignment_expr),
		},
		Statement::VariableStatement{
			name,
			ty: None,
			assignment_expr,
		} => object! {
			"kind": "variable",
			"assignment_expr": serialize_expr(assignment_expr),
		},
		Statement::CallStatement {
			expr
		} => object! {
			"kind": "call",
			"expr": serialize_expr(expr),
		},
		// Statement::IfStatement{
		// 	condition: Expr,
		// 	if_statements: Vec<Statement>,
		// 	else_statements: Vec<Statement>,
		// },
		// Statement::ReturnStatement{
		// 	expr: Option<Expr>,
		// },
		// Statement::WhileStatement{
		// 	condition: Expr,
		// 	statements: Vec<Statement>,
		// },
		// Statement::Comment{
		// 	value: Arc<str>,
		// },
		// Statement::BreakStatement,
		// Statement::ContinueStatement,
		// Statement::EmptyLineStatement,
		_ => todo!(),
	}
}
