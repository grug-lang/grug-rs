use super::tokenizer::{Token, TokenType};
use crate::ast::{
	GrugType, HelperFunction, Statement, OnFunction, Parameter,
	MemberVariable, Expr, ExprData, UnaryOperator,
	BinaryOperator, 
};
use super::GlobalStatement;
use crate::ntstring::NTStr;
use crate::arena::Arena;

use gruggers_core::error::SourceSpan;

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
	},
	GotWrongToken {
		expected: TokenType,
		got: TokenType,
		line: usize,
	},
	// TODO: This is a bad error message
	// "token_index 1 was out of bounds in peek_token()"
	OutOfTokensError,
	GlobalAfterOnFunctions {
		token_value: String,
	},
	ExpectedNewLine {
		line: usize, 
	},
	// "Unexpected empty line, on line %s"
	NewlineNotAllowed {
		line: usize, 
	},
	// "The global variable 'me' has to have its name changed to something else, since grug already declares that variable"
	// TODO: This error message should also return line information
	GlobalNamedMe,
	ExpectedSpace {
		got: TokenType,
		line: usize,
	},
	GlobalCantBeResource {
		name: String,
	},
	GlobalCantBeEntity {
		name: String,
	},
	GlobalMissingInitializer {
		name: String,
		line: usize,
	},
	ExceededMaxParsingDepth,
	ExpectedPrimaryExpression {
		got_token: TokenType,
		line: usize,
	},
	OnFunctionAfterHelperFunctions{
		name: String,
	},
	// ("%s() can't be empty", name)
	EmptyFunction{
		name: String,
	},
	ArgumentCantBeResource {
		name: String,
	},
	// "The argument '%s' can't have 'entity' as its type"
	ArgumentCantBeEntity {
		name: String,
	},
	HelperFnReturnTypeCantBeResource {
		fn_name: String,
	},
	HelperFnReturnTypeCantBeEntity {
		fn_name: String,
	},
	IndentationMismatch{
		expected_spaces: usize,
		got_spaces: usize,
		line: usize,
	},
	ExpectedIndentation{
		got: String,
		line: usize,
	},
	ExpectedStatement{
		prev_token: String,
		line: usize,
	},
	ExpectedStatementToken{
		got_token: TokenType,
		line: usize,
	},
	// "The local variable 'me' has to have its name changed to something else, since grug already declares that variable"
	LocalNamedMe,
	VariableCantBeResource {
		name: String,
	},
	VariableCantBeEntity {
		name: String,
	},
	MissingVariableAssignment{
		name: String,
		line: usize,
	},
	ReassigningMe,
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
			} => write!(f, "Unexpected token '{}' on line {}", token_value, line),
			Self::GotWrongToken{
				expected: _,
				got: TokenType::OpenParenthesis,
				line, 
			} => write!(f, "Unexpected '(' after non-identifier at line {}", line),
			Self::GotWrongToken{
				expected,
				got,
				line, 
			} => write!(f, "Expected token type {}, but got {} on line {}", expected, got, line),
			Self::GlobalNamedMe => write!(f, "The global variable 'me' has to have its name changed to something else, since grug already declares that variable"),
			Self::LocalNamedMe => write!(f, "The local variable 'me' has to have its name changed to something else, since grug already declares that variable"),
			Self::OutOfTokensError => 
				write!(f, "token_index 1 was out of bounds in peek_token()"),
			Self::EmptyFunction{
				name,
			} => write!(f, "{}() can't be empty", name),
			// "Unexpected empty line, on line %s"
			Self::NewlineNotAllowed {
				line, 
			} => write!(f, "Unexpected empty line, on line {}", line),
			// "The argument '%s' can't have 'entity' as its type"
			Self::ArgumentCantBeEntity {
				name,
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
			} => write!(f, "Move the global variable '{}' so it is above the on_ functions", token_value),
			Self::GlobalCantBeEntity {
				name,
			} => write!(f, "The global variable '{}' can't have 'entity' as its type", name),
			Self::GlobalCantBeResource {
				name,
			} => write!(f, "The global variable '{}' can't have 'resource' as its type", name),
			Self::GlobalMissingInitializer {
				name,
				line,
			} => write!(f, "The global variable '{}' was not assigned a value on line {}", name, line),
			Self::HelperFnDefinedBeforeCall {
				helper_fn_name
			} => write!(f, "{}() is defined before the first time it gets called", helper_fn_name),
			Self::OnFunctionAfterHelperFunctions {
				name,
			} => write!(f, "{}() must be defined before all helper_ functions", name),
			Self::AlreadyDefinedHelperFunction {
				helper_fn_name,
			} => write!(f, "The function '{}' was defined several times in the same file", helper_fn_name),
			Self::ExpectedPrimaryExpression {
				got_token,
				line,
			} => write!(f, "Expected a primary expression token, but got token type {} on line {}", got_token, line),
			Self::ExceededMaxParsingDepth => write!(f, "There is a function that contains more than {} levels of nested expressions", MAX_PARSING_DEPTH),
			Self::ReassigningMe => write!(f, "Assigning a new value to the entity's 'me' variable is not allowed"),
			Self::ExpectedNewLine {
				line,
			} => write!(f, "Expected an empty line, on line {}", line),
			// grug_assert(!get_helper_fn(name), "The function '%s' was defined several times in the same file", name);
			Self::AlreadyDefinedOnFn {
				on_fn_name
			} => write!(f, "The function '{}' was defined several times in the same file", on_fn_name),
			Self::ArgumentCantBeResource {
				name,
			} => write!(f, "The argument '{}' can't have 'resource' as its type", name),
			Self::HelperFnReturnTypeCantBeResource {
				fn_name,
			} => write!(f, "The function '{}' can't have 'resource' as its return type", fn_name),
			Self::HelperFnReturnTypeCantBeEntity {
				fn_name,
			} => write!(f, "The function '{}' can't have 'entity' as its return type", fn_name),
			Self::VariableCantBeResource {
				name,
			} => write!(f, "The variable '{}' can't have 'resource' as its type", name),
			Self::VariableCantBeEntity {
				name,
			} => write!(f, "The variable '{}' can't have 'entity' as its type", name),
			Self::ExpectedStatementToken{
				got_token,
				line,
			} => write!(f, "Expected a statement token, but got token type {} on line {}", got_token, line),
			Self::MissingVariableAssignment{
				name,
				line,
			} => write!(f, "The variable '{}' was not assigned a value on line {}", name, line),
			Self::ExpectedStatement{
				prev_token,
				line,
			} => write!(f, "Expected '(', or ':', or ' =' after the word '{}' on line {}", prev_token, line),
			Self::ExpectedIndentation{
				got,
				line,
			} => write!(f, "Expected indentation, newline, or '}}', but got '{}' on line {}", got, line),
			Self::IndentationMismatch{
				expected_spaces,
				got_spaces,
				line,
			} => write!(f, "Expected {} spaces, but got {} spaces on line {}", expected_spaces, got_spaces, line),
			Self::ExpectedSpace{
				got,
				line,
			} => write!(f, "Expected space (' '), but got {} at line {}", got, line),
		}
	}
}

const MAX_PARSING_DEPTH: usize = 100;

pub(crate) struct AST<'arena> {
	pub(crate) global_statements: Vec<GlobalStatement<'arena>, &'arena Arena>,
	pub(crate) called_helper_fns: Vec<&'arena str, &'arena Arena>, 
	pub(crate) helper_fn_signatures: Vec<(&'arena str, (GrugType<'arena>, &'arena [Parameter<'arena>])), &'arena Arena>,
	pub(crate) on_fn_signatures: Vec<(&'arena str, &'arena [Parameter<'arena>]), &'arena Arena>,
}

pub(crate) fn parse<'a>(tokens: &'a [Token], arena: &'a Arena) -> Result<AST<'a>, ParserError> {
	let mut ast = AST::new_in(arena);
	let mut seen_helper_fn = false;

	let mut seen_on_fn = false;
	let mut newline_allowed = false;
	let mut newline_seen = false;
	let mut newline_required = false;
	let mut just_seen_global = false;
	let mut last_newline_token_span = SourceSpan{offset: 0, line: 0};

	let mut tokens = tokens.iter();

	while let Ok(token) = peek_next_token(&tokens) {
		if assert_next_token_types(&tokens, &[TokenType::Word, TokenType::Colon]).is_ok() {
			if seen_on_fn {
				return Err(ParserError::GlobalAfterOnFunctions {
					token_value: token.value.to_string(),
				});
			}
			if newline_required && !just_seen_global {
				return Err(ParserError::GlobalAfterOnFunctions {
					token_value: token.value.to_string(),
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
				});
			}
			// expect newline after each item
			if newline_required {
				return Err(ParserError::ExpectedNewLine{
					line: name_token.span.line,
				});
			}

			let on_fn = ast.parse_on_fn(&mut tokens, arena)?;

			let on_fn_name = on_fn.name.to_str();
			if ast.on_fn_signatures.iter().find(|(name, _)| *name == on_fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedOnFn{
					on_fn_name: Arc::from(on_fn.name.to_str()),
				});
			}
			
			ast.on_fn_signatures.push((on_fn.name.to_str(), on_fn.parameters));
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
					line: name_token.span.line,
				});
			}

			let helper_fn = ast.parse_helper_fn(&mut tokens, arena)?;
			seen_helper_fn = true;

			let helper_fn_name = helper_fn.name.to_str();
			if ast.helper_fn_signatures.iter().find(|(name, _)| *name == helper_fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedHelperFunction{
					helper_fn_name: Arc::from(helper_fn.name.to_str()),
				});
			}

			ast.helper_fn_signatures.push(((helper_fn.name.to_str()), (helper_fn.return_type, helper_fn.parameters)));
			ast.global_statements.push(GlobalStatement::HelperFunction(helper_fn));

			newline_allowed = true;
			newline_seen = false;
			newline_required = true;

			just_seen_global = false;
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else if let Ok([token]) = consume_next_token_types(&mut tokens, &[TokenType::NewLine]) {
			if !newline_allowed {
				return Err(ParserError::NewlineNotAllowed{
					line: token.span.line,
				});
			}

			// Disallow consecutive empty lines
			newline_allowed = false;
			newline_seen = true;
			newline_required = false;
			last_newline_token_span = token.span;
			
			ast.global_statements.push(GlobalStatement::EmptyLine);
		} else if let Ok([comment_token]) = consume_next_token_types(&mut tokens, &[TokenType::Comment]) {
			newline_allowed = true;

			ast.global_statements.push(GlobalStatement::Comment{
				value: Box::leak(NTStr::box_from_str_in(comment_token.value, arena)).as_ntstrptr(),
			});
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else {
			return Err(ParserError::UnexpectedToken{
				token_value: String::from(token.value),
				line: token.span.line,
			});
		}
	}

	if !newline_allowed && newline_seen {
		return Err(ParserError::NewlineNotAllowed{
			// a newline has been seen so the line number will be incremented by one
			// but we want the line number of the previous line
			line: last_newline_token_span.line,
		});
	}

	Ok(ast)
}

impl<'a> AST<'a> {
	fn new_in(arena: &'a Arena) -> Self {
		Self {
			global_statements: Vec::new_in(arena),
			called_helper_fns: Vec::new_in(arena),
			helper_fn_signatures: Vec::new_in(arena),
			on_fn_signatures: Vec::new_in(arena),
		}
	}

	// helper_fn -> "on_" + name + "(" + arguments? + ")" + type + statements 
	fn parse_helper_fn(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<HelperFunction<'a>, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		if self.called_helper_fns.iter().find(|val| **val == fn_name).is_none() {
			return Err(ParserError::HelperFnDefinedBeforeCall {
				helper_fn_name: fn_name.into(),
			});
		}

		// This should never fail because this is checked before calling parse_helper_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let parameters = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_parameters(tokens, arena)?
		} else {
			Vec::new_in(arena).leak()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;

		// return type
		let return_type = if let Ok([_, type_token]) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Word]) {
			match self.parse_type(type_token, arena)? {
				GrugType::Resource{..} => return Err(ParserError::HelperFnReturnTypeCantBeResource{
					fn_name: fn_name.to_string(),
				}),
				GrugType::Entity{..}   => return Err(ParserError::HelperFnReturnTypeCantBeEntity{
					fn_name: fn_name.to_string(),
				}),
				x => x,
			}
		} else {
			GrugType::Void
		};
		
		let body_statements = self.parse_statements(tokens, 0, 1, arena)?;

		if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLine)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
			});
		}

		Ok(HelperFunction{
			name: Box::leak(NTStr::box_from_str_in(fn_name, arena)).as_ntstrptr(),
			parameters,
			body_statements,
			return_type,
			span: name_token.span,
		})
	}

	// on_fn -> "on_" + name + "(" + arguments? + ")" + statements 
	fn parse_on_fn(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<OnFunction<'a>, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		// This should never fail because this is checked before calling parse_on_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let parameters = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_parameters(tokens, arena)?
		} else {
			Vec::new_in(arena).leak()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
		
		let body_statements = self.parse_statements(tokens, 0, 1, arena)?;

		if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLine)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
			});
		}

		Ok(OnFunction{
			name: Box::leak(NTStr::box_from_str_in(fn_name, arena)).as_ntstrptr(),
			parameters,
			body_statements,
			span: name_token.span
		})
	}

	// parameters -> parameter + ("," + parameter)*;
	fn parse_parameters(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<&'a [Parameter<'a>], ParserError> {
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
				}),
				GrugType::Entity{..}   => return Err(ParserError::ArgumentCantBeEntity{
					name: arg_name.to_string(),
				}),
				_ => (),
			}
			arguments.push(Parameter{
				name: Box::leak(NTStr::box_from_str_in(arg_name, arena)).as_ntstrptr(),
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
	fn parse_statements(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<&'a mut [Statement<'a>], ParserError> {
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
						line: token.span.line,
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
				line: next_token.span.line - 1,
			});
		}

		if indentation != 1 {
			consume_indentation(tokens, indentation - 1)?;
		}
		consume_next_token_types(tokens, &[TokenType::CloseBrace])?;

		Ok(statements.leak())
	}

	// stmt -> variable_stmt | if_stmt | return_stmt | while_stmt | ;
	fn parse_statement(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError> {
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
							line: next_tokens[0].span.line,
						})
					}
				}
			}
			TokenType::If => {
				// if condition and block
				let mut ifs = Vec::new();
				loop {
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
							ifs.push((
								condition,
								is_chained,
								if_block,
								&mut [] as &mut [Statement],
							));
							continue;
						} else {
							is_chained = false;
							else_block = self.parse_statements(tokens, parsing_depth, indentation + 1, arena)?;
						}
					} else {
						is_chained = false;
						else_block = &mut [];
					}
					ifs.push((
						condition,
						is_chained,
						if_block,
						else_block,
					));
					break;
				}
				let mut current = ifs.pop().expect("We have parsed at least a single if statement");
				for statement in ifs.into_iter().rev() {
					let else_block = std::slice::from_mut(Box::leak(Box::new_in(
						Statement::If{
							condition: current.0,
							is_chained: current.1,
							if_block: current.2,
							else_block: current.3,
						}, arena,
					)));
					current = statement;
					current.3 = else_block;
				}
				Ok(Statement::If{
					condition: current.0,
					is_chained: current.1,
					if_block: current.2,
					else_block: current.3,
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
				Ok(Statement::Return{ expr: expr.map(|expr| Box::leak(Box::new_in(expr, arena))) })
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
				Ok(Statement::Comment(Box::leak(NTStr::box_from_str_in(next_tokens[0].value, arena)).as_ntstrptr()))
			}
			got_token => {
				Err(ParserError::ExpectedStatementToken{
					got_token,
					line: next_tokens[0].span.line,
				})
			},
		}
	}

	// local_variable -> word + (":" + type)? + "=" + " " + expr
	fn parse_local_variable(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let name_token = get_next_token(tokens)?;
		let local_name = name_token.value; 
		let mut ty = None;

		if consume_next_token_types(tokens, &[TokenType::Colon]).is_ok() {
			if local_name == "me" {
				return Err(ParserError::LocalNamedMe);
			}
			consume_space(tokens)?;
			ty = Some(self.parse_type(get_next_token(tokens)?, arena)?);

			match ty {
				Some(GrugType::Resource{..}) => return Err(ParserError::VariableCantBeResource{
					name: local_name.to_string(),
				}),
				Some(GrugType::Entity{..})   => return Err(ParserError::VariableCantBeEntity{
					name: local_name.to_string(),
				}),
				_ => (),
			}
		}
		// TODO: This error should just be folded into ExpectedSpace but it has
		// to be different to match the required error message
		consume_space(tokens).map_err(|x| match x {
			ParserError::ExpectedSpace{line, ..} => ParserError::MissingVariableAssignment{
				name: local_name.to_string(),
				line,
			},
			ParserError::OutOfTokensError => ParserError::OutOfTokensError,
			_ => unreachable!(),
		})?;

		// TODO: This Me error should be folded into the other Me error within
		// the branch above but it has to be separate to match the required error message
		if local_name == "me" {
			return Err(ParserError::ReassigningMe);
		}

		consume_next_token_types(tokens, &[TokenType::Equal])?;

		consume_space(tokens)?;
		let assignment_expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
		Ok(Statement::Variable{
			name: Box::leak(NTStr::box_from_str_in(local_name, arena)).as_ntstrptr(),
			ty: ty.map(|ty| &*Box::leak(Box::new_in(ty, arena))),
			assignment_expr,
		})
	}

	// global -> word + ":" + type + " =" + expr;
	fn parse_global_variable(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<MemberVariable<'a>, ParserError> {
		let name_token = get_next_token(tokens)?;
		let global_name = name_token.value; 

		if global_name == "me" {
			return Err(ParserError::GlobalNamedMe);
		}
		consume_next_token_types(tokens, &[TokenType::Colon])?;
		consume_space(tokens)?;

		let global_type = self.parse_type(get_next_token(tokens)?, arena)?;
		match global_type {
			GrugType::Resource{..} => return Err(ParserError::GlobalCantBeResource{
				name: global_name.to_string(),
			}),
			GrugType::Entity{..}   => return Err(ParserError::GlobalCantBeEntity{
				name: global_name.to_string(),
			}),
			_ => (),
		}

		if peek_next_token(tokens)?.ty != TokenType::Space {
			return Err(ParserError::GlobalMissingInitializer{
				name: global_name.to_string(),
				line: name_token.span.line,
			});
		}

		consume_space(tokens)?;
		consume_next_token_types(tokens, &[TokenType::Equal])?;

		consume_space(tokens)?;
		
		let assignment_expr = self.parse_expression(tokens, 0, 0., arena)?;
		
		Ok(MemberVariable{
			name: Box::leak(NTStr::box_from_str_in(global_name, arena)).as_ntstrptr(),
			ty: global_type,
			assignment_expr: assignment_expr.into(),
			span: name_token.span
		})
	}

	// Recursive descent parsing adapted from the implementation in grug:
	// https://github.com/grug-lang/grug
	// fn parse_expression<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
	// 	assert_parsing_depth(parsing_depth)?;
	// 	self.parse_or(tokens, parsing_depth + 1)
	// }

	fn parse_expression(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, min_precedence: f32, arena: &'a Arena) -> Result<Expr<'a>, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut current: Expr = {
			let Token{ty, span, value} = get_next_token(tokens)?;
			match ty {
				TokenType::OpenParenthesis => {
					let expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
					let _ = &consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?[0];

					Expr{
						data: ExprData::Parenthesized(Box::leak(Box::new_in(expr.into(), arena))),
						result_type: None,
					}
				}
				TokenType::True => {
					Expr{
						data: ExprData::True,
						result_type: None,
					}
				}
				TokenType::False => {
					Expr{
						data: ExprData::False,
						result_type: None,
					}
				}
				TokenType::String => {
					Expr{
						data: ExprData::String(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
					}
				}
				TokenType::Resource => {
					Expr{
						data: ExprData::Resource(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
					}
				}
				TokenType::Entity => {
					Expr{
						data: ExprData::Entity(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
					}
				}
				TokenType::Word => {
					let value: &'a NTStr  = Box::leak(NTStr::box_from_str_in(value, arena));
					// a word token can actually be a function call
					if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::OpenParenthesis]) {
						if value.as_str().starts_with("helper_") {
							if self.called_helper_fns.iter().find(|name| **name == &*value.as_str()).is_none() {
								self.called_helper_fns.push(value);
							}
						}
						// immediate ")" | (expr + ("," + " " + expr)*) + ")"
						
						if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::CloseParenthesis]) {
							Expr{
								data: ExprData::Call {
									name: value.as_ntstrptr(),
									args: Vec::new().leak(),
									ptr : None,
								},
								result_type: None,
							}
						} else {
							let mut arguments = Vec::new_in(arena);
							loop {
								arguments.push(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?.into());
								if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Comma, TokenType::Space]) {
									
								} else {
									let [_] = consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
									break Expr {
										data: ExprData::Call {
											name: value.as_ntstrptr(),
											args: arguments.leak(),
											ptr : None,
										},
										result_type: None,
									};
								}
							}
						}
					} else {
						Expr{
							data: ExprData::Identifier(value.as_ntstrptr()),
							result_type: None,
						}
					}
				}
				TokenType::Int32 => {
					Expr{
						data: ExprData::Number(
							value.parse::<i64>().unwrap_or(f64::MAX as i64) as f64,
							Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr(),
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
					} else if number == 0. && value.contains(['1', '2', '3', '4', '5', '6', '7', '8', '9']) {
						return Err(ParserError::FloatTooSmall{
							value: String::from(*value),
						});
					}

					Expr{
						data: ExprData::Number(
							number,
							Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr(),
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
						line: span.line, 
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
	
	fn parse_type(&mut self, type_token: &'a Token, arena: &'a Arena) -> Result<GrugType<'a>, ParserError> {
		if type_token.ty != TokenType::Word {
			// TODO: 
			panic!("unimplemented error condition");
		}
		Ok(match type_token.value {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"   => GrugType::Number,
			"string"   => GrugType::String,
			"resource" => GrugType::Resource{
				extension: Box::leak(NTStr::box_from_str_in("", arena)).as_ntstrptr(),
			},
			"id"       => GrugType::Id {custom_name: None},
			"entity"   => GrugType::Entity {
				entity_type: None,
			},
			type_name => {
				GrugType::Id {
					custom_name: Some(Box::leak(NTStr::box_from_str_in(type_name, arena)).as_ntstrptr()),
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
			line: next_token.span.line,
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
	for (Token{ty: got_ty, span, ..}, expected_ty) in tokens.clone().zip(expected) {
		if got_ty != expected_ty {
			return Err(ParserError::GotWrongToken{
				expected: *expected_ty,
				got: *got_ty,
				line: span.line,
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
		ParserError::GotWrongToken{got, line, ..} => ParserError::ExpectedSpace{got, line},
		ParserError::OutOfTokensError => ParserError::OutOfTokensError,
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
			line: token.span.line,
		});
	}
	Ok(token)
}
