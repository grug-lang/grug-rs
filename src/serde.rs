use crate::error::GrugError;
use crate::frontend::*;

pub fn dump_file_to_json<'a> (grug_path: &'a str, output_path: &'a str) -> Result<(), GrugError<'a>> {
	let file_text = std::fs::read_to_string(grug_path).unwrap();

	let tokens = tokenizer::tokenize(&file_text)?;

	let ast = parser::parse(&tokens)?;
	
	let string = ast_to_json(&ast.global_statements);

	std::fs::write(output_path, &string).unwrap();

	Ok(())
}

pub fn generate_file_from_json<'a> (json_path: &'a str, output_path: &'a str) -> Result<(), GrugError<'a>> {
	let file_text = std::fs::read_to_string(json_path).unwrap();
	// TODO: This should maybe have a proper error
	let json_value = json::parse(&file_text).unwrap();
	let file_text = json_to_text(&json_value).unwrap();
	std::fs::write(output_path, file_text).unwrap();
	Ok(())
}

mod ser {
	use crate::types::*;
	use json::{JsonValue, object};
	pub(super) fn ast_to_json(ast: &[GlobalStatement]) -> String {
		// let mut json_ast = Vec::new();
		let ast = ast.iter().map(|statement| match statement {
			GlobalStatement::Variable(GlobalVariable{
				name,
				ty,
				assignment_expr,
			}) => {
				object! {
					"kind": "global_variable",
					"name": &**name, 
					"type": serialize_type(ty),
					"assignment_expr": serialize_expr(assignment_expr),
				}
			},
			GlobalStatement::OnFunction(OnFunction{
				name,
				arguments,
				body_statements,
				calls_helper_fn: _,
				has_while_loop: _,
			}) => {
				object! {
					"kind": "on_function",
					"name": &**name,
					"arguments": arguments.iter().map(serialize_argument).collect::<Vec<_>>(),
					"body_statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				}
			},
			GlobalStatement::HelperFunction(HelperFunction{
				name,
				arguments,
				body_statements,
				calls_helper_fn: _,
				has_while_loop: _,
				return_ty: GrugType::Void,
			}) => {
				object! {
					"kind": "helper_function",
					"name": &**name,
					"arguments": arguments.iter().map(serialize_argument).collect::<Vec<_>>(),
					"body_statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				}
			},
			GlobalStatement::HelperFunction(HelperFunction{
				name,
				arguments,
				body_statements,
				calls_helper_fn: _,
				has_while_loop: _,
				return_ty,
			}) => {
				object! {
					"kind": "helper_function",
					"name": &**name,
					"arguments": arguments.iter().map(serialize_argument).collect::<Vec<_>>(),
					"body_statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
					"return_type": serialize_type(return_ty),
				}
			},
			GlobalStatement::Comment{
				value,
			} => {
				object! {
					"kind": "comment",
					"value": &**value,
				}
			},
			GlobalStatement::EmptyLine => {
				object! {
					"kind": "empty_line",
				}
			},
		}).collect::<Vec<_>>();
		json::stringify_pretty(ast, 4)
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
				extension: _,
			} => unreachable!(),
			GrugType::Entity {
				ty: _,
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
				"value": &***value,
			},
			LiteralExpr::ResourceExpr{
				value,
			} => object! { 
				"type": "resource",
				"value": &***value,
			},
			LiteralExpr::EntityExpr{
				value,
			} => object! { 
				"type": "entity",
				"value": &***value,
			},
			LiteralExpr::IdentifierExpr{
				name
			} => object! { 
				"type": "identifier",
				"value": &**name,
			},
			LiteralExpr::NumberExpr {
				value,
				string,
			} => object! { 
				"type": "number",
				"value": *value,
				"string": &**string,
			},
		}
	}

	fn serialize_unary_operator(operator: &UnaryOperator) -> JsonValue {
		match operator {
			UnaryOperator::Not => "not ".into(),
			UnaryOperator::Minus => "-".into(),
		}
	}

	fn serialize_binary_operator(operator: &BinaryOperator) -> JsonValue {
		match operator {
			BinaryOperator::Or => "or".into(),
			BinaryOperator::And => "and".into(),
			BinaryOperator::DoubleEquals => "==".into(),
			BinaryOperator::NotEquals => "!=".into(),
			BinaryOperator::Greater => ">".into(),
			BinaryOperator::GreaterEquals => ">=".into(),
			BinaryOperator::Less => "<".into(),
			BinaryOperator::LessEquals => "<=".into(),
			BinaryOperator::Plus => "+".into(),
			BinaryOperator::Minus => "-".into(),
			BinaryOperator::Multiply => "*".into(),
			BinaryOperator::Division => "/".into(),
			BinaryOperator::Remainder => "%".into(),
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
				"name": &**name,
				"type": serialize_type(ty),
				"assignment_expr": serialize_expr(assignment_expr),
			},
			Statement::VariableStatement{
				name,
				ty: None,
				assignment_expr,
			} => object! {
				"name": &**name,
				"kind": "variable",
				"assignment_expr": serialize_expr(assignment_expr),
			},
			Statement::CallStatement {
				expr
			} => object! {
				"kind": "call",
				"expr": serialize_expr(expr),
			},
			Statement::IfStatement{
				condition,
				if_statements,
				else_if_statements,
				else_statements: None,
			} => object! {
				"kind": "if",
				"cond": serialize_expr(condition),
				"if_block": if_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				"else_if_statements": else_if_statements.iter().map(|(condition, statements)| {
					object! {
						"cond": serialize_expr(condition),
						"block": statements.iter().map(serialize_statement).collect::<Vec<_>>(),
					}
				}).collect::<Vec<_>>(),
			},
			Statement::IfStatement{
				condition,
				if_statements,
				else_if_statements,
				else_statements: Some(else_statements),
			} => object! {
				"kind": "if",
				"cond": serialize_expr(condition),
				"if_block": if_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				"else_if_statements": else_if_statements.iter().map(|(condition, statements)| {
					object! {
						"cond": serialize_expr(condition),
						"block": statements.iter().map(serialize_statement).collect::<Vec<_>>(),
					}
				}).collect::<Vec<_>>(),
				"else_block": else_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
			},
			Statement::ReturnStatement{
				expr: None,
			} => object! {
				"kind": "return",
			},
			Statement::ReturnStatement{
				expr: Some(expr),
			} => object! {
				"kind": "return",
				"expr": serialize_expr(expr),
			},
			Statement::WhileStatement{
				condition,
				statements,
			} => object! {
				"kind": "while",
				"cond": serialize_expr(condition),
				"statements": statements.iter().map(serialize_statement).collect::<Vec<_>>(),
			},
			Statement::Comment{
				value,
			} => object! {
				"kind": "comment",
				"value": &**value,
			},
			Statement::BreakStatement => object!{"kind": "break"},
			Statement::ContinueStatement => object!{"kind": "continue"},
			Statement::EmptyLineStatement => object!{"kind": "empty_line"},
		}
	}
}
use ser::*;

mod de {
	use json::{JsonValue, object::Object};

	#[derive(Debug)]
	pub enum JsonDeserializeError {
		RootNotArray,
		FieldMissing {
			parent_context: String,
			field_name: &'static str,
		},
		UnexpectedExpressionKind,
		GlobalStatementNotObject,
		GlobalStatementKindNotString,
		GlobalVariableNameNotString,
		GlobalVariableTypeNotString,
		ExpressionNotObject,
		ExpressionKindNotString,
		LiteralExpressionNotObject,
		LiteralExpressionTypeNotString,
		LiteralExpressionValueNotString,
		LiteralExpressionStringNotString,
		UnknownLiteralType,
		UnaryExpressionOperatorNotString,
		BinaryExpressionOperatorNotString,
		CallExpressionFunctionNameNotString,
		CallExpressionArgumentsNotArray,
		OnFunctionNameNotString,
		ArgumentsNotArray,
		ArgumentNotObject,
		ArgumentNameNotString,
		ArgumentTypeNotString,
		StatementsNotArray,
		StatementNotObject,
		StatementKindNotString,
		LocalVariableNameNotString,
		LocalVariableTypeNotString,
		ElseIfStatementsNotArray,
		ElseIfBlockNotObject,
		ElseBlockNotArray,
		CommentValueNotString,
		HelperFunctionNameNotString,
		HelperFunctionReturnTypeNotString,
	}

	pub fn json_to_text(input: &JsonValue) -> Result<String, JsonDeserializeError> {
		if let JsonValue::Array(input) = input {
			let mut output = String::new();
			for (i, statement) in input.iter().enumerate() {
				apply_global_statement(statement, 0, &mut output)?;
				if i < input.len() - 1 {
					output.push_str("\n");
				}
			}
			Ok(output)
		} else {
			Err(JsonDeserializeError::RootNotArray)
		}
	}

	fn apply_global_statement(input: &JsonValue, indentation: usize, output: &mut String) -> Result<(), JsonDeserializeError> {
		if let JsonValue::Object(global_statement) = input {
			let Some(kind) = get_object_field(global_statement, "kind", "global_statement")?.as_str() else {
				return Err(JsonDeserializeError::GlobalStatementKindNotString)
			};
			match kind {
				"global_variable" => {
					let Some(name) = get_object_field(global_statement, "name", "global_variable")?.as_str() else {
						return Err(JsonDeserializeError::GlobalVariableNameNotString)
					};

					output.push_str(name);
					output.push_str(": ");

					let Some(ty) = get_object_field(global_statement, "type", "global_variable")?.as_str() else {
						return Err(JsonDeserializeError::GlobalVariableTypeNotString)
					};

					output.push_str(ty);
					output.push_str(" = ");

					let assignment_expr = get_object_field(global_statement, "assignment_expr", "global_variable")?;
					apply_expr(assignment_expr, output)?;
					Ok(())
				}
				"on_function" => {
					let Some(name) = get_object_field(global_statement, "name", "global_on_function")?.as_str() else {
						return Err(JsonDeserializeError::OnFunctionNameNotString)
					};
					output.push_str(name);
					output.push_str("(");
					let arguments = get_object_field(global_statement, "arguments", "global_on_function")?;
					let body_statements = get_object_field(global_statement, "body_statements", "global_on_function")?;
					apply_arguments(arguments, output)?;
					output.push_str(") ");
					apply_statements(body_statements, indentation + 1, output)?;
					Ok(())
				}
				"helper_function" => {
					let Some(name) = get_object_field(global_statement, "name", "global_helper")?.as_str() else {
						return Err(JsonDeserializeError::HelperFunctionNameNotString)
					};
					output.push_str(name);
					output.push_str("(");
					let arguments = get_object_field(global_statement, "arguments", "global_helper")?;
					let body_statements = get_object_field(global_statement, "body_statements", "global_helper")?;
					apply_arguments(arguments, output)?;
					output.push_str(") ");

					if let Ok(ty) = get_object_field(global_statement, "return_type", "global_helper_function") {
						let Some(ty) = ty.as_str() else {
							return Err(JsonDeserializeError::HelperFunctionReturnTypeNotString);
						};
						output.push_str(ty);
						output.push_str(" ");
					}
					
					apply_statements(body_statements, indentation + 1, output)?;
					Ok(())
				}
				"comment" => {
					let Some(value) = get_object_field(global_statement, "value", "comment")?.as_str() else {
						return Err(JsonDeserializeError::CommentValueNotString);
					};
					output.push_str("# ");
					output.push_str(value);
					Ok(())
				}
				"empty_line" => {
					Ok(())
				}
				_ => unreachable!(),
			}
		} else {
			Err(JsonDeserializeError::GlobalStatementNotObject)
		}
	}

	fn apply_arguments(arguments: &JsonValue, output: &mut String) -> Result<(), JsonDeserializeError> {
		let JsonValue::Array(arguments) = arguments else {
			return Err(JsonDeserializeError::ArgumentsNotArray)
		};
		for (i, argument) in arguments.iter().enumerate() {
			let JsonValue::Object(argument) = argument else {
				return Err(JsonDeserializeError::ArgumentNotObject)
			};
			let Some(name) = get_object_field(argument, "name", "argument")?.as_str() else {
				return Err(JsonDeserializeError::ArgumentNameNotString)
			};
			let Some(ty) = get_object_field(argument, "type", "argument")?.as_str() else {
				return Err(JsonDeserializeError::ArgumentTypeNotString)
			};
			output.push_str(name);
			output.push_str(": ");
			output.push_str(ty);
			if i < arguments.len() - 1 {
				output.push_str(", ");
			}
		}
		Ok(())
	}
	
	fn apply_statements(statements: &JsonValue, indentation: usize, output: &mut String) -> Result<(), JsonDeserializeError> {
		let JsonValue::Array(statements) = statements else {
			return Err(JsonDeserializeError::StatementsNotArray)
		};
		output.push_str("{\n");
		for statement in statements {
			let JsonValue::Object(statement) = statement else {
				return Err(JsonDeserializeError::StatementNotObject);
			};
			let Some(kind) = get_object_field(statement, "kind", "statement")?.as_str() else {
				return Err(JsonDeserializeError::StatementKindNotString);
			};
			match kind {
				"variable" => {
					apply_indentation(indentation, output);
					let Some(name) = get_object_field(statement, "name", "variable")?.as_str() else {
						return Err(JsonDeserializeError::LocalVariableNameNotString)
					};

					output.push_str(name);

					if let Ok(ty) = get_object_field(statement, "type", "variable") {
						let Some(ty) = ty.as_str() else {
							return Err(JsonDeserializeError::LocalVariableTypeNotString);
						};
						output.push_str(": ");
						output.push_str(ty);
					}

					output.push_str(" = ");

					let assignment_expr = get_object_field(statement, "assignment_expr", "variable")?;
					apply_expr(assignment_expr, output)?;
				}
				"call" => {
					apply_indentation(indentation, output);
					let call = get_object_field(statement, "expr", "call_statement")?;
					apply_expr(call, output)?;
				}
				"if" => {
					apply_indentation(indentation, output);
					output.push_str("if ");
					apply_expr(get_object_field(statement, "cond", "if")?, output)?;
					output.push_str(" ");
					apply_statements(get_object_field(statement, "if_block", "if")?, indentation + 1, output)?;

					let JsonValue::Array(else_if_blocks) = get_object_field(statement, "else_if_statements", "if")? else {
						return Err(JsonDeserializeError::ElseIfStatementsNotArray);
					};
					for else_if in else_if_blocks {
						let JsonValue::Object(else_if) = else_if else {
							return Err(JsonDeserializeError::ElseIfBlockNotObject);
						};
						output.push_str(" else if ");
						apply_expr(get_object_field(else_if, "cond", "else_if")?, output)?;
						output.push_str(" ");
						apply_statements(get_object_field(else_if, "block", "if")?, indentation + 1, output)?;
					}
					if let Ok(else_block) = get_object_field(statement, "else_block", "if") {
						let value@JsonValue::Array(else_block) = else_block else {
							return Err(JsonDeserializeError::ElseBlockNotArray);
						};
						if !else_block.is_empty() {
							output.push_str(" else ");
							apply_statements(value, indentation + 1, output)?;
						}
					}
				}
				"while" => {
					apply_indentation(indentation, output);
					output.push_str("while ");
					apply_expr(get_object_field(statement, "cond", "while")?, output)?;
					output.push_str(" ");
					apply_statements(get_object_field(statement, "statements", "while")?, indentation + 1, output)?;
				}
				"break" => {
					apply_indentation(indentation, output);
					output.push_str("break");
				}
				"continue" => {
					apply_indentation(indentation, output);
					output.push_str("continue");
				}
				"comment" => {
					apply_indentation(indentation, output);
					let Some(value) = get_object_field(statement, "value", "comment")?.as_str() else {
						return Err(JsonDeserializeError::CommentValueNotString);
					};
					output.push_str("# ");
					output.push_str(value);
				}
				"return" => {
					apply_indentation(indentation, output);
					output.push_str("return");
					if let Ok(expr) = get_object_field(statement, "expr", "return") {
						output.push_str(" ");
						apply_expr(expr, output)?;
					}
				}
				"empty_line" => (),
				value => unreachable!("{}", value),
			}
			output.push_str("\n");
		}
		apply_indentation(indentation - 1, output);
		output.push_str("}");
		Ok(())
	}

	fn apply_indentation(indentation: usize, output: &mut String) {
		for _ in 0..(indentation * crate::frontend::SPACES_PER_INDENT) {
			output.push_str(" ");
		}
	}

	fn apply_expr(input: &JsonValue, output: &mut String) -> Result<(), JsonDeserializeError> {
		let JsonValue::Object(input) = input else {
			return Err(JsonDeserializeError::ExpressionNotObject)
		};
		let Some(ty) = get_object_field(input, "type", "expression")?.as_str() else {
			return Err(JsonDeserializeError::ExpressionKindNotString);
		};
		match ty {
			"literal" => {
				let JsonValue::Object(expr) = get_object_field(input, "expr", "literal_expression")? else {
					return Err(JsonDeserializeError::LiteralExpressionNotObject);
				};
				let Some(ty) = get_object_field(expr, "type", "literal_expression")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionTypeNotString);
				};
				match ty {
					"string" | "entity" | "resource" => {
						let Some(value) = get_object_field(expr, "value", "literal_expression")?.as_str() else {
							return Err(JsonDeserializeError::LiteralExpressionValueNotString);
						};
						output.push_str("\"");
						output.push_str(value);
						output.push_str("\"");
					}
					"boolean" | "identifier" => {
						let Some(value) = get_object_field(expr, "value", "literal_expression")?.as_str() else {
							return Err(JsonDeserializeError::LiteralExpressionValueNotString);
						};
						output.push_str(value);
					}
					"number" => {
						let Some(string) = get_object_field(expr, "string", "literal_expression")?.as_str() else {
							return Err(JsonDeserializeError::LiteralExpressionStringNotString);
						};
						use std::fmt::Write;
						write!(output, "{}", string).unwrap();
					}
					_ => {
						return Err(JsonDeserializeError::UnknownLiteralType)
					}
				}
				Ok(())
			}
			"unary" => {
				let Some(op) = get_object_field(input, "operator", "unary_expression")?.as_str() else {
					return Err(JsonDeserializeError::UnaryExpressionOperatorNotString);
				};
				output.push_str(op);
				let expr = get_object_field(input, "expr", "unary_expression")?;
				apply_expr(expr, output)
			}
			"binary" => {
				let Some(op) = get_object_field(input, "operator", "binary_expression")?.as_str() else {
					return Err(JsonDeserializeError::BinaryExpressionOperatorNotString);
				};
				let left = get_object_field(input, "left", "binary_expression")?;
				let right = get_object_field(input, "right", "binary_expression")?;
				apply_expr(left, output)?;
				output.push_str(" ");
				output.push_str(op);
				output.push_str(" ");
				apply_expr(right, output)
			}
			"call" => {
				let Some(function_name) = get_object_field(input, "function_name", "call_expression")?.as_str() else {
					return Err(JsonDeserializeError::CallExpressionFunctionNameNotString);
				};
				let JsonValue::Array(arguments) = get_object_field(input, "arguments", "call_expression")? else {
					return Err(JsonDeserializeError::CallExpressionArgumentsNotArray);
				};
				output.push_str(function_name);
				output.push_str("(");
				for (i, argument) in arguments.iter().enumerate() {
					apply_expr(argument, output)?;
					
					if i < arguments.len() - 1 {
						output.push_str(", ")
					}
				}
				output.push_str(")");
				Ok(())
			}
			"parenthesized" => {
				let expr = get_object_field(input, "expr", "parenthesized_expression")?;
				output.push_str("(");
				apply_expr(expr, output)?;
				output.push_str(")");
				Ok(())
			}
			_ => Err(JsonDeserializeError::UnexpectedExpressionKind),
		}
	}

	fn get_object_field<'a>(input: &'a Object, field: &'static str, parent_context: &str) -> Result<&'a JsonValue, JsonDeserializeError> {
		match &input[field] {
			JsonValue::Null => Err(JsonDeserializeError::FieldMissing{
				parent_context: String::from(parent_context),
				field_name: field,
			}),
			value => Ok(value),
		}
	}
}
use de::*;
