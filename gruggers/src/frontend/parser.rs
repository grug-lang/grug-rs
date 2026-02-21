use super::tokenizer::{Token, TokenType};
use crate::ast::{
	GlobalStatement, GrugType, HelperFunction, Statement, OnFunction, Argument,
	MemberVariable, Expr, ExprData, LiteralExprData, UnaryOperator,
	BinaryOperator, 

	c_argument, c_statement, c_expr
};
use crate::ntstring::NTStr;
use crate::arena::Arena;
use std::sync::Arc;

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

#[allow(unused)]
#[derive(Debug)]
pub enum ParserError {
	// grug_error("Unexpected token '%s' on line %zu", token.str, get_token_line_number(i));
	UnexpectedToken {
		token_value: String,
		line: usize,
		col: usize,
	},
	GotWrongToken {
		expected: TokenType,
		got: TokenType,
		line: usize,
		col: usize,
	},
	// TODO: This is a bad error message
	// "token_index 1 was out of bounds in peek_token()"
	OutOfTokensError,
	GlobalAfterOnFunctions {
		token_value: String,
		line: usize,
		col: usize,
	},
	ExpectedNewLine {
		token_value: String,
		line: usize, 
		col: usize,
	},
	// "Unexpected empty line, on line %s"
	NewlineNotAllowed {
		line: usize, 
		col: usize,
	},
	// "The global variable 'me' has to have its name changed to something else, since grug already declares that variable"
	// TODO: This error message should also return line information
	GlobalNamedMe {
		#[allow(unused)]
		line: usize,
		#[allow(unused)]
		col: usize,
	},
	ExpectedSpace {
		got: TokenType,
		line: usize,
		col: usize, 
	},
	MissingType {
		line: usize,
		col: usize,
	},
	GlobalCantBeResource {
		name: String,
		line: usize,
		col: usize,
	},
	GlobalCantBeEntity {
		name: String,
		line: usize,
		col: usize,
	},
	GlobalMissingInitializer {
		name: String,
		line: usize,
		col: usize,			
	},
	ExceededMaxParsingDepth,
	ExpectedPrimaryExpression {
		got_token: TokenType,
		line: usize,
		col: usize,
	},
	OnFunctionAfterHelperFunctions{
		name: String,
		line: usize,
		col: usize,
	},
	// ("%s() can't be empty", name)
	EmptyFunction{
		name: String,
		line: usize,
		col: usize,
	},
	ArgumentCantBeResource {
		name: String,
		line: usize,
		col: usize,
	},
	// "The argument '%s' can't have 'entity' as its type"
	ArgumentCantBeEntity {
		name: String,
		line: usize,
		col: usize,
	},
	HelperFnReturnTypeCantBeResource {
		fn_name: String,
		line: usize,
		col: usize,
	},
	HelperFnReturnTypeCantBeEntity {
		fn_name: String,
		line: usize,
		col: usize,
	},
	IndentationMismatch{
		expected_spaces: usize,
		got_spaces: usize,
		line: usize,
		col: usize,
	},
	ExpectedIndentation{
		got: String,
		line: usize,
		col: usize,
	},
	ExpectedStatement{
		prev_token: String,
		line: usize,
		col: usize,
	},
	ExpectedStatementToken{
		got_token: TokenType,
		line: usize,
		col: usize,
	},
	// "The local variable 'me' has to have its name changed to something else, since grug already declares that variable"
	LocalNamedMe {
		line: usize,
		col: usize,
	},
	VariableCantBeResource {
		name: String,
		line: usize,
		col: usize,
	},
	VariableCantBeEntity {
		name: String,
		line: usize,
		col: usize,
	},
	MissingVariableAssignment{
		name: String,
		line: usize,
		col: usize,
	},
	ReassigningMe {
		line: usize,
		col: usize,
	},
	// grug_error("%s() is defined before the first time it gets called", fn.fn_name);
	HelperFnDefinedBeforeCall {
		helper_fn_name: String
	},
	AlreadyDefinedHelperFunction {
		helper_fn_name: Arc<str>,
	},
	// grug_error("The f32 %s is too big", str);
	FloatTooBig {
		value: String,
	},
	// grug_error("The f32 %s is too close to zero", str);
	FloatTooSmall {
		value: String,
	},
	// grug_assert(!get_helper_fn(name), "The function '%s' was defined several times in the same file", name);
	AlreadyDefinedOnFn {
		on_fn_name: Arc<str>, 
	}
}

impl std::fmt::Display for ParserError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			// grug_error("Unexpected token '%s' on line %zu", token.str, get_token_line_number(i));
			Self::UnexpectedToken {
				token_value,
				line,
				col: _,
			} => write!(f, "Unexpected token '{}' on line {}", token_value, line),
			Self::GotWrongToken{
				expected,
				got,
				line, 
				..
			} => write!(f, "Expected token type {}, but got {} on line {}", expected, got, line),
			Self::GlobalNamedMe {
				line: _, 
				col: _, 
			} => write!(f, "The global variable 'me' has to have its name changed to something else, since grug already declares that variable"),
			Self::LocalNamedMe {
				line: _, 
				col: _, 
			} => write!(f, "The local variable 'me' has to have its name changed to something else, since grug already declares that variable"),
			Self::OutOfTokensError => 
				write!(f, "token_index 1 was out of bounds in peek_token()"),
			Self::EmptyFunction{
				name,
				..
			} => write!(f, "{}() can't be empty", name),
			// "Unexpected empty line, on line %s"
			Self::NewlineNotAllowed {
				line, 
				..
			} => write!(f, "Unexpected empty line, on line {}", line),
			// "The argument '%s' can't have 'entity' as its type"
			Self::ArgumentCantBeEntity {
				name,
				line: _,
				col: _,
			} => write!(f, "The argument '{}' can't have 'entity' as its type", name),
			// grug_error("The f32 %s is too big", str);
			Self::FloatTooBig {
				value,
			} => write!(f, "The number {} is too big", value),
			// grug_error("The f32 %s is too close to zero", str);
			Self::FloatTooSmall {
				value,
			} => write!(f, "The number {} is too close to zero", value),
			Self::GlobalAfterOnFunctions {
				token_value,
				line: _,
				col: _,
			} => write!(f, "Move the global variable '{}' so it is above the on_ functions", token_value),
			Self::GlobalCantBeEntity {
				name,
				line: _,
				col: _,
			} => write!(f, "The global variable '{}' can't have 'entity' as its type", name),
			Self::GlobalCantBeResource {
				name,
				line: _,
				col: _,
			} => write!(f, "The global variable '{}' can't have 'resource' as its type", name),
			Self::GlobalMissingInitializer {
				name,
				line,
				col: _,			
			} => write!(f, "The global variable '{}' was not assigned a value on line {}", name, line),
			Self::HelperFnDefinedBeforeCall {
				helper_fn_name
			} => write!(f, "{}() is defined before the first time it gets called", helper_fn_name),
			Self::OnFunctionAfterHelperFunctions {
				name,
				line: _,
				col: _,
			} => write!(f, "{}() must be defined before all helper_ functions", name),
			Self::AlreadyDefinedHelperFunction {
				helper_fn_name,
			} => write!(f, "The function '{}' was defined several times in the same file", helper_fn_name),
			Self::ExpectedPrimaryExpression {
				got_token,
				line,
				col: _,
			} => write!(f, "Expected a primary expression token, but got token type {} on line {}", got_token, line),
			Self::ExceededMaxParsingDepth => write!(f, "There is a function that contains more than {} levels of nested expressions", MAX_PARSING_DEPTH),
			Self::ReassigningMe {
				line: _,
				col: _,
			} => write!(f, "Assigning a new value to the entity's 'me' variable is not allowed"),
			Self::ExpectedNewLine {
				token_value: _,
				line,
				col: _,
			} => write!(f, "Expected an empty line, on line {}", line),
			// grug_assert(!get_helper_fn(name), "The function '%s' was defined several times in the same file", name);
			Self::AlreadyDefinedOnFn {
				on_fn_name
			} => write!(f, "The function '{}' was defined several times in the same file", on_fn_name),
			Self::ArgumentCantBeResource {
				name,
				line: _,
				col: _,
			} => write!(f, "The argument '{}' can't have 'resource' as its type", name),
			Self::HelperFnReturnTypeCantBeResource {
				fn_name,
				line: _,
				col: _,
			} => write!(f, "The function '{}' can't have 'resource' as its return type", fn_name),
			Self::HelperFnReturnTypeCantBeEntity {
				fn_name,
				line: _,
				col: _,
			} => write!(f, "The function '{}' can't have 'entity' as its return type", fn_name),
			Self::VariableCantBeResource {
				name,
				line: _,
				col: _,
			} => write!(f, "The variable '{}' can't have 'resource' as its type", name),
			Self::VariableCantBeEntity {
				name,
				line: _,
				col: _,
			} => write!(f, "The variable '{}' can't have 'entity' as its type", name),
			Self::ExpectedStatementToken{
				got_token,
				line,
				col: _,
			} => write!(f, "Expected a statement token, but got token type {} on line {}", got_token, line),
			Self::MissingVariableAssignment{
				name,
				line,
				col: _,
			} => write!(f, "The variable '{}' was not assigned a value on line {}", name, line),
			Self::ExpectedStatement{
				prev_token,
				line,
				col: _,
			} => write!(f, "Expected '(', or ':', or ' =' after the word '{}' on line {}", prev_token, line),
			_ => write!(f, "{:?}", self),
		}
	}
}

const MAX_PARSING_DEPTH: usize = 100;

pub(crate) struct AST<'a> {
	pub(crate) global_statements: Vec<GlobalStatement<'a>, &'a Arena>,
	pub(crate) called_helper_fns: Vec<&'a NTStr, &'a Arena>, 
	pub(crate) helper_fn_signatures: Vec<(&'a NTStr, (GrugType<'a>, &'a [c_argument<'a>])), &'a Arena>,
	pub(crate) on_fn_signatures: Vec<(&'a NTStr, &'a [c_argument<'a>]), &'a Arena>,
}

pub(crate) fn parse<'a>(tokens: &'_ [Token], arena: &'a Arena) -> Result<AST<'a>, ParserError> {
	let mut ast = AST::new_in(arena);
	let mut seen_helper_fn = false;

	let mut seen_on_fn = false;
	let mut newline_allowed = false;
	let mut newline_seen = false;
	let mut newline_required = false;
	let mut just_seen_global = false;
	let mut last_newline_location = (0, 0);

	let mut tokens = tokens.iter();

	while let Ok(token) = peek_next_token(&tokens) {
		if assert_next_token_types(&tokens, &[TokenType::Word, TokenType::Colon]).is_ok() {
			if seen_on_fn {
				return Err(ParserError::GlobalAfterOnFunctions {
					token_value: token.value.to_string(),
					line: token.line,
					col: token.col,
				});
			}
			if newline_required && !just_seen_global {
				return Err(ParserError::GlobalAfterOnFunctions {
					token_value: token.value.to_string(),
					line: token.line,
					col: token.col,
				});
			}

			let global_variable = ast.parse_global_variable(&mut tokens, &arena)?;
			ast.global_statements.push(GlobalStatement::Variable(global_variable));
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;

			newline_allowed = true;
			newline_required = true;
			just_seen_global = true;
		} else if let Ok([name_token, ..]) = assert_next_token_types(&tokens, &[TokenType::Word, TokenType::OpenParenthesis]) && name_token.value.starts_with("on_") {
			// Cannot have global function after helper function
			if seen_helper_fn {
				return Err(ParserError::OnFunctionAfterHelperFunctions{
					name: name_token.value.to_string(),
					line: name_token.line,
					col: name_token.col,
				});
			}
			// expect newline after each item
			if newline_required {
				return Err(ParserError::ExpectedNewLine{
					token_value: name_token.value.to_string(),
					line: name_token.line,
					col: name_token.col,
				});
			}

			let on_fn = ast.parse_on_fn(&mut tokens, arena)?;

			if ast.on_fn_signatures.iter().find(|(name, _)| *name == &*on_fn.name).is_some() {
				return Err(ParserError::AlreadyDefinedOnFn{
					on_fn_name: Arc::from(on_fn.name.as_str()),
				});
			}
			
			ast.on_fn_signatures.push((Box::leak(Box::from(&*on_fn.name)), on_fn.arguments));
			ast.global_statements.push(GlobalStatement::OnFunction(on_fn));

			seen_on_fn = true;

			newline_allowed = true;
			newline_seen = false;
			newline_required = true;

			just_seen_global = false;
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else if let Ok([name_token, ..]) = assert_next_token_types(&tokens, &[TokenType::Word, TokenType::OpenParenthesis]) && name_token.value.starts_with("helper_") {
			// expect newline after each item
			if newline_required {
				return Err(ParserError::ExpectedNewLine{
					token_value: name_token.value.to_string(),
					line: name_token.line,
					col: name_token.col,
				});
			}

			let helper_fn = ast.parse_helper_fn(&mut tokens, arena)?;
			seen_helper_fn = true;

			if ast.helper_fn_signatures.iter().find(|(name, _)| *name == &*helper_fn.name).is_some() {
				return Err(ParserError::AlreadyDefinedHelperFunction{
					helper_fn_name: Arc::from(helper_fn.name.as_str()),
				});
			}

			ast.helper_fn_signatures.push((Box::leak(Box::from(&*helper_fn.name)), (helper_fn.return_type, helper_fn.arguments.clone())));
			ast.global_statements.push(GlobalStatement::HelperFunction(helper_fn));

			newline_allowed = true;
			newline_seen = false;
			newline_required = true;

			just_seen_global = false;
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else if let Ok([token]) = consume_next_token_types(&mut tokens, &[TokenType::NewLine]) {
			if !newline_allowed {
				return Err(ParserError::NewlineNotAllowed{
					line: token.line,
					col: token.col,
				});
			}

			// Disallow consecutive empty lines
			newline_allowed = false;
			newline_seen = true;
			newline_required = false;
			last_newline_location = (token.line, token.col);
			
			ast.global_statements.push(GlobalStatement::EmptyLine);
		} else if let Ok([comment_token]) = consume_next_token_types(&mut tokens, &[TokenType::Comment]) {
			newline_allowed = true;

			ast.global_statements.push(GlobalStatement::Comment{
				value: Box::leak(NTStr::box_from_str_in(comment_token.value, arena)),
			});
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else {
			return Err(ParserError::UnexpectedToken{
				token_value: String::from(token.value),
				line: token.line,
				col: token.col,
			});
		}
	}

	if !newline_allowed && newline_seen {
		return Err(ParserError::NewlineNotAllowed{
			// a newline has been seen so the line number will be incremented by one
			// but we want the line number of the previous line
			line: last_newline_location.0,
			col: last_newline_location.1,
		});
	}

	Ok(ast)
}

impl<'arena> AST<'arena> {
	fn new_in(arena: &'arena Arena) -> Self {
		Self {
			global_statements: Vec::new_in(arena),
			called_helper_fns: Vec::new_in(arena),
			helper_fn_signatures: Vec::new_in(arena),
			on_fn_signatures: Vec::new_in(arena),
		}
	}

	// helper_fn -> "on_" + name + "(" + arguments? + ")" + type + statements 
	fn parse_helper_fn<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<HelperFunction<'a>, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		if self.called_helper_fns.iter().find(|val| val.as_str() == fn_name).is_none() {
			return Err(ParserError::HelperFnDefinedBeforeCall {
				helper_fn_name: fn_name.into(),
			});
		}

		// This should never fail because this is checked before calling parse_helper_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let args = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_arguments(tokens, arena)?
		} else {
			Vec::new().leak()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;

		// return type
		let return_type = if let Ok([_, type_token]) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Word]) {
			match self.parse_type(type_token, arena)? {
				GrugType::Resource{..} => return Err(ParserError::HelperFnReturnTypeCantBeResource{
					fn_name: fn_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				GrugType::Entity{..}   => return Err(ParserError::HelperFnReturnTypeCantBeEntity{
					fn_name: fn_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				x => x,
			}
		} else {
			GrugType::Void
		};
		
		let body_statements = self.parse_statements(tokens, 0, 1, arena)?;

		if body_statements.iter().all(|&x| matches!(x.into(), Statement::Comment{..} | Statement::EmptyLine)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
				line: name_token.line, 
				col: name_token.col,
			});
		}

		Ok(HelperFunction{
			name: Box::leak(NTStr::box_from_str_in(fn_name, arena)),
			arguments: args,
			body_statements,
			return_type
		})
	}

	// on_fn -> "on_" + name + "(" + arguments? + ")" + statements 
	fn parse_on_fn<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<OnFunction<'a>, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		// This should never fail because this is checked before calling parse_on_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let args = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_arguments(tokens, arena)?
		} else {
			Vec::new().leak()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
		
		let body_statements = self.parse_statements(tokens, 0, 1, arena)?;

		if body_statements.iter().all(|&x| matches!(x.into(), Statement::Comment{..} | Statement::EmptyLine)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
				line: name_token.line, 
				col: name_token.col,
			});
		}

		Ok(OnFunction{
			name: Box::leak(NTStr::box_from_str_in(fn_name, arena)),
			arguments: args,
			body_statements,
		})
	}

	// arguments -> argument + ("," + argument)*;
	fn parse_arguments<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<&'a [c_argument<'a>], ParserError> {
		let mut arguments = Vec::new_in(arena);
		loop {
			// parse_arg
			let name_token = get_next_token(tokens)?;
			let arg_name = name_token.value;
			consume_next_token_types(tokens, &[TokenType::Colon, TokenType::Space])?;

			let arg_type = self.parse_type(get_next_token(tokens)?, arena)?;

			match arg_type {
				GrugType::Resource{..} => return Err(ParserError::ArgumentCantBeResource{
					name: arg_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				GrugType::Entity{..}   => return Err(ParserError::ArgumentCantBeEntity{
					name: arg_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				_ => (),
			}
			arguments.push(Argument{
				name: Box::leak(NTStr::box_from_str_in(arg_name, arena)),
				ty: arg_type,
			}.into());
			
			if consume_next_token_types(tokens, &[TokenType::Comma]).is_err() {
				break;
			}
			
			consume_space(tokens)?;
		}
		Ok(arguments.leak())
	}

	// TODO: Get the grammar for statements
	// This parser consumes a space before consuming the curly braces
	fn parse_statements<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<&'a mut [c_statement<'a>], ParserError> {
		assert_parsing_depth(parsing_depth)?;
		consume_next_token_types(tokens, &[TokenType::Space, TokenType::OpenBrace, TokenType::NewLine])?;

		let mut newline_allowed = false;
		let mut newline_seen = false;

		let mut statements = Vec::new_in(arena);

		while !is_end_of_block(tokens, indentation)? {
			// newlines
			if let Ok([token]) = consume_next_token_types(tokens, &[TokenType::NewLine]) {
				if !newline_allowed {
					return Err(ParserError::NewlineNotAllowed{
						line: token.line,
						col: token.col,
					});
				}
				// cannot have consecutive newlines
				newline_allowed = false;
				newline_seen = true;

				statements.push(Statement::EmptyLine.into());
			} else {
				newline_allowed = true;
				newline_seen = false;
				consume_indentation(tokens, indentation)?;

				statements.push(self.parse_statement(tokens, parsing_depth + 1, indentation, arena)?.into());
				consume_next_token_types(tokens, &[TokenType::NewLine])?;
			}
		}

		if !newline_allowed && newline_seen {
			let [next_token] = peek_next_tokens(tokens)?;
			return Err(ParserError::NewlineNotAllowed{
				// a newline has been seen so the line number will be incremented by one
				// but we want the line number of the previous line
				line: next_token.line - 1,
				col: next_token.col,
			});
		}

		if indentation != 1 {
			consume_indentation(tokens, indentation - 1)?;
		}
		consume_next_token_types(tokens, &[TokenType::CloseBrace])?;

		Ok(statements.leak())
	}

	// stmt -> variable_stmt | if_stmt | return_stmt | while_stmt | ;
	fn parse_statement<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError> {
		let next_tokens = peek_next_tokens::<2>(tokens)?;
		match next_tokens[0].ty {
			TokenType::Word => {
				match next_tokens[1].ty {
					TokenType::OpenParenthesis => {
						Ok(Statement::Call(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?))
					}
					TokenType::Colon | TokenType::Space => {
						self.parse_local_variable(tokens, parsing_depth + 1, arena)
					}
					_ => {
						Err(ParserError::ExpectedStatement{
							prev_token: next_tokens[0].value.to_string(),
							line: next_tokens[0].line,
							col: next_tokens[0].col,
						})
					}
				}
			}
			TokenType::If => {
				// if condition and block
				consume_next_token_types(tokens, &[TokenType::If, TokenType::Space])?;

				let condition = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
				let if_block = self.parse_statements(tokens, parsing_depth + 1, indentation + 1, arena)?;

				// else block 
				
				let is_chained;
				let else_block;

				if consume_next_token_types(tokens, &[TokenType::Space, TokenType::Else]).is_ok() {
					let [space_token, if_token] = peek_next_tokens(tokens)?;
					if TokenType::Space == space_token.ty && TokenType::If == if_token.ty {
						is_chained = true;
						consume_next_token_types(tokens, &[TokenType::Space]).unwrap();
						else_block = std::slice::from_mut(Box::leak(Box::new_in(self.parse_statement(tokens, parsing_depth + 1, indentation, arena)?.into(), arena)));
					} else {
						is_chained = false;
						else_block = self.parse_statements(tokens, parsing_depth, indentation + 1, arena)?;
					}
				} else {
					is_chained = false;
					else_block = Vec::new().leak();
				}
				Ok(Statement::If{
					condition, 
					is_chained,
					if_block,
					else_block,
				})
			}
			TokenType::Return => {
				tokens.next();
				let expr = if let TokenType::NewLine = next_tokens[1].ty {
					None
				} else {
					consume_space(tokens)?;
					Some(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?)
				};
				Ok(Statement::Return{ expr })
			}
			TokenType::While => {
				assert_parsing_depth(parsing_depth)?;
				consume_next_token_types(tokens, &[TokenType::While, TokenType::Space])?;

				let condition = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
				let block = self.parse_statements(tokens, parsing_depth + 1, indentation + 1, arena)?;

				Ok(Statement::While{
					condition,
					block,
				})
			}
			TokenType::Break => {
				tokens.next();
				Ok(Statement::Break)
			}
			TokenType::Continue => {
				tokens.next();
				Ok(Statement::Continue)
			}
			TokenType::NewLine => {
				tokens.next();
				Ok(Statement::EmptyLine)
			}
			TokenType::Comment => {
				tokens.next();
				Ok(Statement::Comment(Box::leak(NTStr::box_from_str_in(next_tokens[1].value, arena))))
			}
			got_token => {
				Err(ParserError::ExpectedStatementToken{
					got_token,
					line: next_tokens[0].line,
					col: next_tokens[0].col,
				})
			},
		}
	}

	// local_variable -> word + (":" + type)? + "=" + " " + expr
	fn parse_local_variable<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let name_token = get_next_token(tokens)?;
		let local_name = name_token.value; 
		let mut ty = None;

		if consume_next_token_types(tokens, &[TokenType::Colon]).is_ok() {
			if local_name == "me" {
				return Err(ParserError::LocalNamedMe{
					line: name_token.line,
					col: name_token.col,
				});
			}
			consume_space(tokens)?;
			ty = Some(self.parse_type(get_next_token(tokens)?, arena)?);

			match ty {
				Some(GrugType::Resource{..}) => return Err(ParserError::VariableCantBeResource{
					name: local_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				Some(GrugType::Entity{..})   => return Err(ParserError::VariableCantBeEntity{
					name: local_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				_ => (),
			}
		}
		// TODO: This error should just be folded into ExpectedSpace but it has
		// to be different to match the required error message
		consume_space(tokens).map_err(|x| match x {
			ParserError::ExpectedSpace{line, col, ..} => ParserError::MissingVariableAssignment{
				name: local_name.to_string(),
				line,
				col
			},
			_ => unreachable!(),
		})?;

		// TODO: This Me error should be folded into the other Me error within
		// the branch above but it has to be separate to match the required error message
		if local_name == "me" {
			return Err(ParserError::ReassigningMe{
				line: name_token.line,
				col: name_token.col,
			});
		}

		consume_next_token_types(tokens, &[TokenType::Equal])?;

		consume_space(tokens)?;
		let assignment_expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
		Ok(Statement::Variable{
			name: Box::leak(NTStr::box_from_str_in(local_name, arena)),
			ty,
			assignment_expr,
		})
	}

	// global -> word + ":" + type + " =" + expr;
	fn parse_global_variable<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<MemberVariable<'a>, ParserError> {
		let name_token = get_next_token(tokens)?;
		let global_name = name_token.value; 

		if global_name == "me" {
			return Err(ParserError::GlobalNamedMe{
				line: name_token.line,
				col: name_token.col,
			});
		}
		consume_next_token_types(tokens, &[TokenType::Colon])?;
		consume_space(tokens)?;

		let global_type = self.parse_type(get_next_token(tokens)?, arena)?;
		match global_type {
			GrugType::Resource{..} => return Err(ParserError::GlobalCantBeResource{
				name: global_name.to_string(),
				line: name_token.line,
				col: name_token.line,
			}),
			GrugType::Entity{..}   => return Err(ParserError::GlobalCantBeEntity{
				name: global_name.to_string(),
				line: name_token.line,
				col: name_token.line,
			}),
			_ => (),
		}

		if peek_next_token(tokens)?.ty != TokenType::Space {
			return Err(ParserError::GlobalMissingInitializer{
				name: global_name.to_string(),
				line: name_token.line,
				col: name_token.col,
			});
		}

		consume_space(tokens)?;
		consume_next_token_types(tokens, &[TokenType::Equal])?;

		consume_space(tokens)?;
		
		let assignment_expr = self.parse_expression(tokens, 0, 0., arena)?;
		
		Ok(MemberVariable{
			name: Box::leak(NTStr::box_from_str_in(global_name, arena)),
			ty: global_type,
			assignment_expr: assignment_expr.into(),
		})
	}

	// Recursive descent parsing adapted from the implementation in grug:
	// https://github.com/grug-lang/grug
	// fn parse_expression<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
	// 	assert_parsing_depth(parsing_depth)?;
	// 	self.parse_or(tokens, parsing_depth + 1)
	// }

	fn parse_expression<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, min_precedence: f32, arena: &'a Arena) -> Result<c_expr<'a>, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut current: Expr = {
			let Token{ty, line, col, value} = get_next_token(tokens)?;
			match ty {
				TokenType::OpenParenthesis => {
					let expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
					let close_paren = &consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?[0];

					Expr{
						data: ExprData::Parenthesized(Box::leak(Box::new_in(expr.into(), arena))),
						result_type: None,
					}
				}
				TokenType::True => {
					Expr{
						data: ExprData::Literal(LiteralExprData::True),
						result_type: None,
					}
				}
				TokenType::False => {
					Expr{
						data: ExprData::Literal(LiteralExprData::False),
						result_type: None,
					}
				}
				TokenType::String => {
					Expr{
						data: ExprData::Literal(LiteralExprData::String(Box::leak(NTStr::box_from_str_in(value, arena)))),
						result_type: None,
					}
				}
				TokenType::Word => {
					let value: &'a NTStr  = Box::leak(NTStr::box_from_str_in(value, arena));
					// a word token can actually be a function call
					if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::OpenParenthesis]) {
						if value.as_str().starts_with("helper_") {
							if self.called_helper_fns.iter().find(|name| **name == &*value).is_none() {
								self.called_helper_fns.push(value);
							}
						}
						// immediate ")" | (expr + ("," + " " + expr)*) + ")"
						
						if let Ok([close_paren_token]) = consume_next_token_types(tokens, &[TokenType::CloseParenthesis]) {
							Expr{
								data: ExprData::Call {
									name: value,
									args: Vec::new().leak(),
								},
								result_type: None,
							}
						} else {
							let mut arguments = Vec::new_in(arena);
							loop {
								arguments.push(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?.into());
								if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Comma, TokenType::Space]) {
									
								} else {
									let [close_paren_token] = consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
									break Expr {
										data: ExprData::Call {
											name: value,
											args: arguments.leak(),
										},
										result_type: None,
									};
								}
							}
						}
					} else {
						Expr{
							data: ExprData::Literal(LiteralExprData::Identifier(value)),
							result_type: None,
						}
					}
				}
				TokenType::Int32 => {
					Expr{
						data: ExprData::Literal(
							LiteralExprData::Number(
								value.parse::<i64>().unwrap() as f64,
								Box::leak(NTStr::box_from_str_in(value, arena)),
							)
						),
						result_type: None,
					}
				}
				TokenType::Float32 => {
					let number = value.parse::<f64>().unwrap();
					if number > f64::MAX {
						return Err(ParserError::FloatTooBig{
							value: String::from(*value),
						});
					} else if number != 0. && number < f64::MIN_POSITIVE {
						return Err(ParserError::FloatTooSmall{
							value: String::from(*value),
						});
					}

					Expr{
						data: ExprData::Literal(
							LiteralExprData::Number(
								number,
								Box::leak(NTStr::box_from_str_in(value, arena)),
							)
						),
						result_type: None,
					}
				}
				TokenType::Minus | TokenType::Not => {
					let unary_op = match ty {
						TokenType::Minus => UnaryOperator::Minus,
						TokenType::Not => {consume_space(tokens)?; UnaryOperator::Not},
						_ => unreachable!(),
					};
					let ((), r_bp) = Self::get_prefix_precedence(unary_op);
					let expr = self.parse_expression(tokens, parsing_depth + 1, r_bp, arena)?;
					Expr {
						result_type: None,
						data: ExprData::Unary{
							op: unary_op,
							expr: Box::leak(Box::new_in(expr.into(), arena)),
						},
					}
				}
				_ =>  {
					return Err(ParserError::ExpectedPrimaryExpression{
						got_token: *ty,
						line: *line, 
						col: *col,
					})
				}
			}
		};
		while let Ok([space, op]) = peek_next_tokens(tokens) {
			let TokenType::Space = space.ty else {
				break;
			};
			let bin_op = match op.ty {
				TokenType::Or => {
					BinaryOperator::Or
				}
				TokenType::And => {
					BinaryOperator::And
				}
				TokenType::DoubleEquals => {
					BinaryOperator::DoubleEquals
				}
				TokenType::NotEquals => {
					BinaryOperator::NotEquals
				}
				TokenType::Greater => {
					BinaryOperator::Greater
				}
				TokenType::GreaterEquals => {
					BinaryOperator::GreaterEquals
				}
				TokenType::Less => {
					BinaryOperator::Less
				}
				TokenType::LessEquals => {
					BinaryOperator::LessEquals
				}
				TokenType::Plus => {
					BinaryOperator::Plus
				}
				TokenType::Minus => {
					BinaryOperator::Minus
				}
				TokenType::Star => {
					BinaryOperator::Multiply
				}
				TokenType::ForwardSlash => {
					BinaryOperator::Division
				}
				TokenType::Percent => {
					BinaryOperator::Remainder
				}
				_ => break,
			};
			let (l_bp, r_bp) = Self::get_infix_precedence(bin_op);
			if l_bp < min_precedence {
				break;
			}
			consume_space(tokens)?;
			_ = get_next_token(tokens)?;
			consume_space(tokens)?;
			let next = self.parse_expression(tokens, parsing_depth + 1, r_bp, arena)?;

			current = Expr {
				result_type: None,
				data: ExprData::Binary {
					op: bin_op,
					left : Box::leak(Box::new_in(current.into(), arena)),
					right: Box::leak(Box::new_in(next   .into(), arena)),
				}
			};
		}
		Ok(current.into())
	}

	fn get_prefix_precedence(op: UnaryOperator) -> ((), f32) {
		match op {
			UnaryOperator::Minus => ((), 7.0),
			UnaryOperator::Not   => ((), 8.0),
		}
	}

	fn get_infix_precedence(op: BinaryOperator) -> (f32, f32) {
		match op {
			BinaryOperator::Or            => (1.0, 1.1),
			BinaryOperator::And           => (2.0, 2.1),
			BinaryOperator::DoubleEquals  => (3.0, 3.1),
			BinaryOperator::NotEquals     => (3.0, 3.1),
			BinaryOperator::Greater       => (4.0, 4.1),
			BinaryOperator::GreaterEquals => (4.0, 4.1),
			BinaryOperator::Less          => (4.0, 4.1),
			BinaryOperator::LessEquals    => (4.0, 4.1),
			BinaryOperator::Plus          => (5.0, 5.1),
			BinaryOperator::Minus         => (5.0, 5.1),
			BinaryOperator::Multiply      => (6.0, 6.1),
			BinaryOperator::Division      => (6.0, 6.1),
			BinaryOperator::Remainder     => (6.0, 6.1),
		}
	}
	
	fn parse_type<'a>(&mut self, type_token: &'a Token, arena: &'a Arena) -> Result<GrugType<'a>, ParserError> {
		if type_token.ty != TokenType::Word {
			return Err (ParserError::MissingType{
				line: type_token.line,
				col: type_token.col,
			});
		}
		Ok(match type_token.value {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"   => GrugType::Number,
			"string"   => GrugType::String,
			"resource" => GrugType::Resource{
				extension: Box::leak(NTStr::box_from_str_in("", arena)),
			},
			"id"       => GrugType::Id {custom_name: None},
			"entity"   => GrugType::Entity {
				entity_type: None,
			},
			type_name => {
				GrugType::Id {
					custom_name: Some(Box::leak(NTStr::box_from_str_in(type_name, arena))),
				}
			}
		})
	}
}

fn is_end_of_block(tokens: &mut std::slice::Iter<Token>, indentation: usize) -> Result<bool, ParserError> {
	use super::SPACES_PER_INDENT;

	assert!(indentation != 0);
	let next_token = peek_next_token(tokens)?;
	match next_token.ty {
		TokenType::CloseBrace => Ok(true),
		TokenType::NewLine => Ok(false),
		TokenType::Indentation => {
			// TODO: I don't understand this?
			//
			// 	    fn on_something() {
			// 	    	if (boolean) {
			//				some_game_fn()
			// 	  ->	}
			// 	    }
			// 	There would be an indentation token at the arrow with 4 spaces
			// 	(indentation is going from 2 to 1) and this branch detects that
			//
			// 	Would'nt it be better to check for the close braces directly
			Ok(next_token.value.len() == (indentation - 1) * SPACES_PER_INDENT)
		}
		_ => Err(ParserError::ExpectedIndentation {
			got: next_token.value.to_string(),
			line: next_token.line,
			col: next_token.col,
		})
	}
}

// Checks if the passed in parsing_depth is allowed
fn assert_parsing_depth(parsing_depth: usize) -> Result<(), ParserError> {
	if parsing_depth > MAX_PARSING_DEPTH {
		Err(ParserError::ExceededMaxParsingDepth)
	} else {
		Ok(())
	}
}

// checks whether the next few tokens match the expected tokens without consuming the input
fn assert_next_token_types<'a>(tokens: &std::slice::Iter<'a, Token<'a>>, expected: &[TokenType]) -> Result<&'a [Token<'a>], ParserError> {
	if tokens.len() < expected.len() {
		return Err(ParserError::OutOfTokensError);
	}
	for (Token{ty: got_ty, line, col, ..}, expected_ty) in tokens.clone().zip(expected) {
		if got_ty != expected_ty {
			return Err(ParserError::GotWrongToken{
				expected: *expected_ty,
				got: *got_ty,
				line: *line,
				col: *col,
			});
		}
	}
	Ok(&tokens.as_slice()[..expected.len()])
}

// consumes the next few tokens if they match the given types, otherwise leaves the input unchanged
fn consume_next_token_types<'a, const N: usize>(tokens: &'_ mut std::slice::Iter<'a, Token<'a>>, expected: &'_ [TokenType; N]) -> Result<&'a [Token<'a>; N], ParserError> {
	assert_next_token_types(tokens, expected)?;
	let ret_val = &tokens.as_slice()[..expected.len()];
	*tokens = tokens.as_slice()[expected.len()..].iter();
	Ok(unsafe{&*(ret_val as *const [Token] as *const [Token; N])})
}

fn get_next_token<'a> (tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
	tokens.next().ok_or(ParserError::OutOfTokensError)
}

fn peek_next_token<'a> (tokens: &std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
	tokens.as_slice().first().ok_or(ParserError::OutOfTokensError)
}

fn peek_next_tokens<'a, const N: usize> (tokens: &std::slice::Iter<'a, Token<'a>>) -> Result<&'a [Token<'a>; N], ParserError> {
	Ok(unsafe{&*(tokens.as_slice().get(..N).ok_or(ParserError::OutOfTokensError)? as *const _ as * const _)})
}

fn consume_space<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
	Ok(&consume_next_token_types(tokens, &[TokenType::Space]).map_err(|err| match err {
		ParserError::GotWrongToken{got, line, col, ..} => ParserError::ExpectedSpace{got, line, col},
		_ => unreachable!(),
	})?[0])
}

fn consume_indentation<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>, indentation: usize) -> Result<&'a Token<'a>, ParserError> {
	use super::SPACES_PER_INDENT;

	let token = &consume_next_token_types(tokens, &[TokenType::Indentation])?[0];
	let spaces = token.value.len();
	if spaces != indentation * SPACES_PER_INDENT {
		return Err(ParserError::IndentationMismatch{
			expected_spaces: indentation * SPACES_PER_INDENT,
			got_spaces: spaces,
			line: token.line,
			col: token.col,
		});
	}
	Ok(token)
}
