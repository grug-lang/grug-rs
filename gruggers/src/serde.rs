//! Contains functions to convert a grug file to/from its json representation.
//!
//! Grug files can be losslessly converted to and from json. This allows for
//! easy ast transformations for upgrades and downgrades
use crate::error::Error;
use crate::frontend::*;
use crate::arena::Arena;

use std::ffi::OsStr;

/// Converts a grug file into its json ast if it is valid. 
///
/// `file path` is only required for error reporting. A dummy file name can be
/// provided
pub fn dump_file_to_json (grug_text: &str, file_path: impl AsRef<OsStr>) -> Result<String, Error> {
	let arena = Arena::new();

	let tokens = tokenizer::tokenize(grug_text, &arena, "")?;

	let ast = parser::parse(&tokens, &arena, grug_text, file_path.as_ref())?;
	
	let json = json::stringify_pretty(ast_to_json(&ast.global_statements), 4);
	Ok(json)
}

/// Converts a grug json ast into a grug file if there are no errors.
pub fn generate_file_from_json (input_json: &str) -> Result<String, Error> {
	let json_value = json::parse(input_json).unwrap();
	Ok(json_to_text(&json_value).unwrap())
}

mod ser {
	use crate::ast::*;
	use crate::frontend::GlobalStatement;
	use json::{JsonValue, object};
	pub(super) fn ast_to_json(ast: &[GlobalStatement<'_>]) -> JsonValue {
		ast.iter().map(|statement| match statement {
			GlobalStatement::Variable(MemberVariable{
				name,
				ty,
				assignment_expr,
				span: _
			}) => {
				object! {
					"type": "GLOBAL_VARIABLE",
					"name": name.to_str(), 
					"variable_type": serialize_type(ty),
					"assignment": serialize_expr(assignment_expr),
				}
			},
			GlobalStatement::OnFunction(OnFunction{
				name,
				parameters,
				body_statements,
				span: _
			}) => {
				let mut object = object! {
					"type": "GLOBAL_ON_FN",
					"name": name.to_str(),
					"statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				};
				if !parameters.is_empty() {
					// TODO: rename this to "parameters"
					object["arguments"] = parameters.iter().map(serialize_parameter).collect::<Vec<_>>().into();
				}
				object
			},
			GlobalStatement::HelperFunction(HelperFunction{
				name,
				parameters,
				body_statements,
				return_type,
				span: _
			}) => {
				let mut object = object! {
					"type": "GLOBAL_HELPER_FN",
					"name": name.to_str(),
					"statements": body_statements.iter().map(serialize_statement).collect::<Vec<_>>(),
				};
				if !parameters.is_empty() {
					// TODO: rename this to "parameters"
					object["arguments"] = parameters.iter().map(serialize_parameter).collect::<Vec<_>>().into();
				}
				if *return_type != GrugType::Void {
					// TODO: rename this to "parameters"
					object["return_type"] = serialize_type(return_type);
				}
				object
			},
			GlobalStatement::Comment{
				value,
			} => {
				object! {
					"type": "GLOBAL_COMMENT",
					"comment": value.to_str(),
				}
			},
			GlobalStatement::EmptyLine => {
				object! {
					"type": "GLOBAL_EMPTY_LINE",
				}
			},
		}).collect::<Vec<_>>().into()
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
			} => name.to_str().into(),
			GrugType::Resource {
				..
			} => unreachable!(),
			GrugType::Entity {
				..
			} => unreachable!(),
		}
	}

	fn serialize_expr(expr: &Expr) -> JsonValue {
		match &expr.data {
			ExprData::True => object! {
				"type": "TRUE_EXPR",
			},
			ExprData::False => object! {
				"type": "FALSE_EXPR",
			},
			ExprData::String(value) => object! { 
				"type": "STRING_EXPR",
				"str": value.to_str(),
			},
			ExprData::Resource(value) => object! { 
				"type": "RESOURCE_EXPR",
				"str": value.to_str(),
			},
			ExprData::Entity(value) => object! { 
				"type": "ENTITY_EXPR",
				"str": value.to_str(),
			},
			ExprData::Identifier(name) => object! { 
				"type": "IDENTIFIER_EXPR",
				"str": name.to_str(),
			},
			ExprData::Number (_, string) => object! { 
				"type": "NUMBER_EXPR",
				"value": string.to_str(),
			},
			ExprData::Unary{
				op,
				expr,
				op_span: _,
			} => object! {
				"type": "UNARY_EXPR",
				"operator": serialize_unary_operator(op),
				"expr": serialize_expr(expr),
			},
			ExprData::Binary{
				left,
				right,
				op,
				op_span: _,
			} => {
				let ty = if matches!(op, BinaryOperator::Or | BinaryOperator::And) {
					"LOGICAL_EXPR"
				} else {
					"BINARY_EXPR"
				};
				object! {
					"type": ty,
					"operator": serialize_binary_operator(op),
					"left_expr": serialize_expr(left),
					"right_expr": serialize_expr(right),
				}
			}
			ExprData::Call{
				name,
				args,
				ptr: _, 
				name_span: _,
			} => {
				let mut object = object! {
					"type": "CALL_EXPR",
					"name": name.to_str(),
				};
				if !args.is_empty() {
					object["arguments"] = args.iter().map(serialize_expr).collect::<Vec<_>>().into();
				}
				object
			}
			ExprData::Parenthesized(expr) => object! {
				"type": "PARENTHESIZED_EXPR",
				"expr": serialize_expr(expr),
			},
		}
	}

	fn serialize_unary_operator(operator: &UnaryOperator) -> JsonValue {
		match operator {
			UnaryOperator::Not => "NOT_TOKEN".into(),
			UnaryOperator::Minus => "MINUS_TOKEN".into(),
		}
	}

	fn serialize_binary_operator(operator: &BinaryOperator) -> JsonValue {
		match operator {
			BinaryOperator::Or => "OR_TOKEN".into(),
			BinaryOperator::And => "AND_TOKEN".into(),
			BinaryOperator::DoubleEquals => "EQUALS_TOKEN".into(),
			BinaryOperator::NotEquals => "NOT_EQUALS_TOKEN".into(),
			BinaryOperator::Greater => "GREATER_TOKEN".into(),
			BinaryOperator::GreaterEquals => "GREATER_OR_EQUAL_TOKEN".into(),
			BinaryOperator::Less => "LESS_TOKEN".into(),
			BinaryOperator::LessEquals => "LESS_OR_EQUAL_TOKEN".into(),
			BinaryOperator::Plus => "PLUS_TOKEN".into(),
			BinaryOperator::Minus => "MINUS_TOKEN".into(),
			BinaryOperator::Multiply => "MULTIPLICATION_TOKEN".into(),
			BinaryOperator::Division => "DIVISION_TOKEN".into(),
		}
	}

	fn serialize_parameter(argument: &Parameter) -> JsonValue {
		object! {
			"name": argument.name.to_str(),
			"type": serialize_type(&argument.ty),
		}
	}

	fn serialize_statement(statement: &Statement) -> JsonValue {
		match statement {
			Statement::Variable{
				name,
				ty,
				assignment_expr,
				name_span: _,
			} => {
				let mut object = object! {
					"type": "VARIABLE_STATEMENT",
					"name": name.to_str(),
					"assignment": serialize_expr(assignment_expr),
				};
				if let Some(ty) = ty {
					object["variable_type"] = serialize_type(ty);
				}
				object
			}
			Statement::Call(expr) => {
				let expr = serialize_expr(expr);
				let name = expr["name"].as_str().expect("name is always a string");
				
				let mut object = object! {
					"type": "CALL_STATEMENT",
					"name": name,
				};
				if let JsonValue::Array(arguments) = &expr["arguments"] {
					object["arguments"] = JsonValue::from(&**arguments);
				}
				object
			}
			Statement::If{
				condition,
				is_chained: _,
				if_block,
				else_block,
			} => {
				let mut object = object! {
					"type": "IF_STATEMENT",
					"condition": serialize_expr(condition),
				};
				if !if_block.is_empty() {
					object["if_statements"] = if_block.iter().map(serialize_statement).collect::<Vec<_>>().into();
				}
				if !else_block.is_empty() {
					object["else_statements"] = else_block.iter().map(serialize_statement).collect::<Vec<_>>().into();
				}
				object
			}
			Statement::Return{
				return_span: _,
				expr: None,
			} => object! {
				"type": "RETURN_STATEMENT",
			},
			Statement::Return{
				return_span: _,
				expr: Some(expr),
			} => object! {
				"type": "RETURN_STATEMENT",
				"expr": serialize_expr(expr),
			},
			Statement::While{
				condition,
				block,
			} => object! {
				"type": "WHILE_STATEMENT",
				"condition": serialize_expr(condition),
				"statements": block.iter().map(serialize_statement).collect::<Vec<_>>(),
			},
			Statement::Comment{comment_span: _, value} => object! {
				"type": "COMMENT_STATEMENT",
				"comment": value.to_str(),
			},
			Statement::Break(_) => object!{"type": "BREAK_STATEMENT"},
			Statement::Continue(_) => object!{"type": "CONTINUE_STATEMENT"},
			Statement::EmptyLine => object!{"type": "EMPTY_LINE_STATEMENT"},
		}
	}
}
use ser::*;

mod de {
	use json::{JsonValue, object::Object};

	#[derive(Debug)]
	pub enum JsonDeserializeError {
		RootNotArray,
		#[allow(dead_code)]
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
		LiteralExpressionValueNotString,
		LiteralExpressionStringNotString,
		UnaryExpressionOperatorNotString,
		BinaryExpressionOperatorNotString,
		CallExpressionFunctionNameNotString,
		CallExpressionArgumentsNotArray,
		OnFunctionNameNotString,
		ParametersNotArray,
		ParameterNotObject,
		ParameterNameNotString,
		ParameterTypeNotString,
		StatementsNotArray,
		StatementNotObject,
		StatementKindNotString,
		LocalVariableNameNotString,
		LocalVariableTypeNotString,
		ElseBlockNotArray,
		CommentValueNotString,
		HelperFunctionNameNotString,
		HelperFunctionReturnTypeNotString,
		InvalidGlobalStatementType,
		UnrecognizedOperator,
	}

	pub fn json_to_text(input: &JsonValue) -> Result<String, JsonDeserializeError> {
		if let JsonValue::Array(input) = input {
			let mut output = String::new();
			for statement in input.iter() {
				apply_global_statement(statement, 0, &mut output)?;
				output.push_str("\n");
			}
			Ok(output)
		} else {
			Err(JsonDeserializeError::RootNotArray)
		}
	}

	fn apply_global_statement(input: &JsonValue, indentation: usize, output: &mut String) -> Result<(), JsonDeserializeError> {
		if let JsonValue::Object(global_statement) = input {
			let Some(kind) = get_object_field(global_statement, "type", "global_statement")?.as_str() else {
				return Err(JsonDeserializeError::GlobalStatementKindNotString)
			};
			match kind {
				"GLOBAL_VARIABLE" => {
					let Some(name) = get_object_field(global_statement, "name", "GLOBAL_VARIABLE")?.as_str() else {
						return Err(JsonDeserializeError::GlobalVariableNameNotString)
					};

					output.push_str(name);
					output.push_str(": ");

					let Some(ty) = get_object_field(global_statement, "variable_type", "GLOBAL_VARIABLE")?.as_str() else {
						return Err(JsonDeserializeError::GlobalVariableTypeNotString)
					};

					output.push_str(ty);
					output.push_str(" = ");

					let assignment_expr = get_object_field(global_statement, "assignment", "GLOBAL_VARIABLE")?;
					apply_expr(assignment_expr, output)?;
					Ok(())
				}
				"GLOBAL_ON_FN" => {
					let Some(name) = get_object_field(global_statement, "name", "GLOBAL_ON_FN")?.as_str() else {
						return Err(JsonDeserializeError::OnFunctionNameNotString)
					};
					output.push_str("export ");
					output.push_str(name);
					output.push_str("(");
					if let Ok(parameters) = get_object_field(global_statement, "arguments", "GLOBAL_ON_FN") {
						apply_parameters(parameters, output)?;
					}
					output.push_str(") ");

					let body_statements = get_object_field(global_statement, "statements", "GLOBAL_ON_FN")?;
					apply_statements(body_statements, indentation + 1, output)?;
					Ok(())
				}
				"GLOBAL_HELPER_FN" => {
					let Some(name) = get_object_field(global_statement, "name", "GLOBAL_HELPER_FN")?.as_str() else {
						return Err(JsonDeserializeError::HelperFunctionNameNotString)
					};
					output.push_str("local ");
					output.push_str(name);
					output.push_str("(");
					if let Ok(parameters) = get_object_field(global_statement, "arguments", "GLOBAL_HELPER_FN") {
						apply_parameters(parameters, output)?;
					}
					output.push_str(") ");

					if let Ok(ty) = get_object_field(global_statement, "return_type", "GLOBAL_HELPER_FN") {
						let Some(ty) = ty.as_str() else {
							return Err(JsonDeserializeError::HelperFunctionReturnTypeNotString);
						};
						output.push_str(ty);
						output.push_str(" ");
					}
					let body_statements = get_object_field(global_statement, "statements", "GLOBAL_HELPER_FN")?;
					
					apply_statements(body_statements, indentation + 1, output)?;
					Ok(())
				}
				"GLOBAL_COMMENT" => {
					let Some(value) = get_object_field(global_statement, "comment", "GLOBAL_COMMENT")?.as_str() else {
						return Err(JsonDeserializeError::CommentValueNotString);
					};
					output.push_str("# ");
					output.push_str(value);
					Ok(())
				}
				"GLOBAL_EMPTY_LINE" => {
					Ok(())
				}
				_ => Err(JsonDeserializeError::InvalidGlobalStatementType)
			}
		} else {
			Err(JsonDeserializeError::GlobalStatementNotObject)
		}
	}

	fn apply_parameters(parameters: &JsonValue, output: &mut String) -> Result<(), JsonDeserializeError> {
		let JsonValue::Array(parameters) = parameters else {
			return Err(JsonDeserializeError::ParametersNotArray)
		};
		for (i, parameter) in parameters.iter().enumerate() {
			let JsonValue::Object(parameter) = parameter else {
				return Err(JsonDeserializeError::ParameterNotObject)
			};
			let Some(name) = get_object_field(parameter, "name", "argument")?.as_str() else {
				return Err(JsonDeserializeError::ParameterNameNotString)
			};
			let Some(ty) = get_object_field(parameter, "type", "argument")?.as_str() else {
				return Err(JsonDeserializeError::ParameterTypeNotString)
			};
			output.push_str(name);
			output.push_str(": ");
			output.push_str(ty);
			if i < parameters.len() - 1 {
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
			let Some(kind) = get_object_field(statement, "type", "statement")?.as_str() else {
				return Err(JsonDeserializeError::StatementKindNotString);
			};
			match kind {
				"VARIABLE_STATEMENT" => {
					apply_indentation(indentation, output);
					let Some(name) = get_object_field(statement, "name", "VARIABLE_STATEMENT")?.as_str() else {
						return Err(JsonDeserializeError::LocalVariableNameNotString)
					};

					output.push_str(name);

					if let Ok(ty) = get_object_field(statement, "variable_type", "VARIABLE_STATEMENT") {
						let Some(ty) = ty.as_str() else {
							return Err(JsonDeserializeError::LocalVariableTypeNotString);
						};
						output.push_str(": ");
						output.push_str(ty);
					}

					output.push_str(" = ");

					let assignment_expr = get_object_field(statement, "assignment", "variable")?;
					apply_expr(assignment_expr, output)?;
				}
				"CALL_STATEMENT" => {
					apply_indentation(indentation, output);
					let Some(name) = get_object_field(statement, "name", "CALL_STATEMENT")?.as_str() else {
						return Err(JsonDeserializeError::CallExpressionFunctionNameNotString);
					};
					output.push_str(name);
					output.push_str("(");

					if let Ok(arguments) = get_object_field(statement, "arguments", "CALL_STATEMENT") {
						let JsonValue::Array(arguments) = arguments else {
							return Err(JsonDeserializeError::CallExpressionArgumentsNotArray);
						};
						for (i, argument) in arguments.iter().enumerate() {
							apply_expr(argument, output)?;
							
							if i < arguments.len() - 1 {
								output.push_str(", ")
							}
						}
					}
					output.push_str(")");
				}
				"IF_STATEMENT" => {
					apply_indentation(indentation, output);
					let mut statement = statement;
					'outer: loop {
						output.push_str("if ");
						apply_expr(get_object_field(statement, "condition", "IF_STATEMENT")?, output)?;
						output.push_str(" ");

						if let Ok(if_statements) = get_object_field(statement, "if_statements", "IF_STATEMENT") {
							apply_statements(if_statements, indentation + 1, output)?;
						} else {
							apply_statements(&JsonValue::Array(vec![]), indentation + 1, output)?;
						}
						if let Ok(else_block) = get_object_field(statement, "else_statements", "IF_STATEMENT") {
							output.push_str(" else ");
							let else_block_array@JsonValue::Array(else_block) = else_block else {
								return Err(JsonDeserializeError::ElseBlockNotArray);
							};
							// TODO: Fix this upstream in grug_tests
							// single if statement in else block means chained if blocks
							'chained: {
								if let &[JsonValue::Object(ref first_statement)] = else_block.as_slice() {
									let Some(kind) = get_object_field(first_statement, "type", "statement")?.as_str() else {
										return Err(JsonDeserializeError::StatementKindNotString);
									};
									if kind != "IF_STATEMENT" {
										break 'chained;
									}
									statement = first_statement;
									continue 'outer;
								} 
							}
							apply_statements(else_block_array, indentation + 1, output)?;
						}
						break;
					}
				}
				"WHILE_STATEMENT" => {
					apply_indentation(indentation, output);
					output.push_str("while ");
					apply_expr(get_object_field(statement, "condition", "WHILE_STATEMENT")?, output)?;
					output.push_str(" ");
					apply_statements(get_object_field(statement, "statements", "WHILE_STATEMENT")?, indentation + 1, output)?;
				}
				"BREAK_STATEMENT" => {
					apply_indentation(indentation, output);
					output.push_str("break");
				}
				"CONTINUE_STATEMENT" => {
					apply_indentation(indentation, output);
					output.push_str("continue");
				}
				"COMMENT_STATEMENT" => {
					apply_indentation(indentation, output);
					let Some(value) = get_object_field(statement, "comment", "COMMENT_STATEMENT")?.as_str() else {
						return Err(JsonDeserializeError::CommentValueNotString);
					};
					output.push_str("# ");
					output.push_str(value);
				}
				"RETURN_STATEMENT" => {
					apply_indentation(indentation, output);
					output.push_str("return");
					if let Ok(expr) = get_object_field(statement, "expr", "RETURN_STATEMENT") {
						output.push_str(" ");
						apply_expr(expr, output)?;
					}
				}
				"EMPTY_LINE_STATEMENT" => (),
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
			"STRING_EXPR" => {
				let Some(value) = get_object_field(input, "str", "LITERAL_EXPRESSION")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionValueNotString);
				};
				output.push_str("\"");
				output.push_str(value);
				output.push_str("\"");
				Ok(())
			}
			"ENTITY_EXPR" => {
				let Some(value) = get_object_field(input, "str", "LITERAL_EXPRESSION")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionValueNotString);
				};
				output.push_str("e\"");
				output.push_str(value);
				output.push_str("\"");
				Ok(())
			}
			"RESOURCE_EXPR" => {
				let Some(value) = get_object_field(input, "str", "LITERAL_EXPRESSION")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionValueNotString);
				};
				output.push_str("r\"");
				output.push_str(value);
				output.push_str("\"");
				Ok(())
			}
			"TRUE_EXPR" => {
				output.push_str("true"); 
				Ok(())
			}
			"FALSE_EXPR" => {
				output.push_str("false"); 
				Ok(())
			}
			"IDENTIFIER_EXPR" => {
				let Some(value) = get_object_field(input, "str", "IDENTIFIER_EXPR")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionValueNotString);
				};
				output.push_str(value);
				Ok(())
			}
			"NUMBER_EXPR" => {
				let Some(string) = get_object_field(input, "value", "LITERAL_EXPRESSION")?.as_str() else {
					return Err(JsonDeserializeError::LiteralExpressionStringNotString);
				};
				output.push_str(string);
				Ok(())
			}
			"UNARY_EXPR" => {
				let Some(op) = get_object_field(input, "operator", "UNARY_EXPR")?.as_str() else {
					return Err(JsonDeserializeError::UnaryExpressionOperatorNotString);
				};
				apply_operator(op, output)?;
				apply_expr(get_object_field(input, "expr", "UNARY_EXPR")?, output)
			}
			"BINARY_EXPR" | "LOGICAL_EXPR" => {
				let Some(op) = get_object_field(input, "operator", "BINARY_EXPR")?.as_str() else {
					return Err(JsonDeserializeError::BinaryExpressionOperatorNotString);
				};
				let left = get_object_field(input, "left_expr", "BINARY_EXPR")?;
				let right = get_object_field(input, "right_expr", "BINARY_EXPR")?;
				apply_expr(left, output)?;
				output.push_str(" ");
				apply_operator(op, output)?;
				output.push_str(" ");
				apply_expr(right, output)
			}
			"CALL_EXPR" => {
				let Some(name) = get_object_field(input, "name", "CALL_EXPR")?.as_str() else {
					return Err(JsonDeserializeError::CallExpressionFunctionNameNotString);
				};
				
				output.push_str(name);
				output.push_str("(");
				if let Ok(arguments) = get_object_field(input, "arguments", "CALL_EXPR") {
					let JsonValue::Array(arguments) = arguments else {
						return Err(JsonDeserializeError::CallExpressionArgumentsNotArray);
					};
					for (i, argument) in arguments.iter().enumerate() {
						apply_expr(argument, output)?;
						
						if i < arguments.len() - 1 {
							output.push_str(", ")
						}
					}
				}
				output.push_str(")");
				Ok(())
			}
			"PARENTHESIZED_EXPR" => {
				let expr = get_object_field(input, "expr", "PARENTHESIZED_EXPR")?;
				output.push_str("(");
				apply_expr(expr, output)?;
				output.push_str(")");
				Ok(())
			}
			_ => Err(JsonDeserializeError::UnexpectedExpressionKind),
		}
	}

	fn apply_operator(input: &str, output: &mut String) -> Result<(), JsonDeserializeError> {
		let string = match input {
			"NOT_TOKEN" => "not ",
			"AND_TOKEN" => "and",
			"OR_TOKEN" => "or",
			"PLUS_TOKEN" => "+",
			"MINUS_TOKEN" => "-",
			"MULTIPLICATION_TOKEN" => "*",
			"DIVISION_TOKEN" => "/",
			"EQUALS_TOKEN" => "==",
			"NOT_EQUALS_TOKEN" => "!=",
			"GREATER_OR_EQUAL_TOKEN" => ">=",
			"GREATER_TOKEN" => ">",
			"LESS_OR_EQUAL_TOKEN" => "<=",
			"LESS_TOKEN" => "<",
			_ => Err(JsonDeserializeError::UnrecognizedOperator)?,
		};
		output.push_str(string);
		Ok(())
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
