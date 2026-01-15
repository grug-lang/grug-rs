use super::tokenizer::{Token, TokenType};
use crate::types::*;
use crate::ntstring::NTStr;
use std::collections::{HashSet, HashMap};
use std::sync::Arc;

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
	UnexpectedOpenParenthesis {
		got_ty: ExprType,
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

pub(crate) struct AST {
	pub(crate) global_statements: Vec<GlobalStatement>,
	pub(crate) called_helper_fns: HashSet<Arc<str>>, 
	pub(crate) helper_fn_signatures: HashMap<Arc<str>, (GrugType, Vec<Argument>)>,
	pub(crate) on_fn_signatures: HashMap<Arc<str>, Vec<Argument>>,
}

pub(crate) fn parse(tokens: &'_ [Token]) -> Result<AST, ParserError> {
	let mut ast = AST::new();
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

			let global_variable = ast.parse_global_variable(&mut tokens)?;
			ast.global_statements.push(global_variable);
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;

			newline_allowed = true;
			newline_required = true;
			just_seen_global = true;
		} else if let Ok(&[ref name_token, ..]) = assert_next_token_types(&tokens, &[TokenType::Word, TokenType::OpenParenthesis]) && name_token.value.starts_with("on_") {
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

			let on_fn = ast.parse_on_fn(&mut tokens)?;
			let (on_fn_name, on_fn_arguments) = if let GlobalStatement::OnFunction(OnFunction{name, arguments, ..}) = &on_fn {(Arc::clone(name), arguments)} else {unreachable!()};

			if ast.on_fn_signatures.get(&on_fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedOnFn{
					on_fn_name,
				});
			}
			
			ast.on_fn_signatures.insert(on_fn_name, on_fn_arguments.clone());
			ast.global_statements.push(on_fn);

			seen_on_fn = true;

			newline_allowed = true;
			newline_seen = false;
			newline_required = true;

			just_seen_global = false;
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else if let Ok(&[ref name_token, ..]) = assert_next_token_types(&tokens, &[TokenType::Word, TokenType::OpenParenthesis]) && name_token.value.starts_with("helper_") {
			// expect newline after each item
			if newline_required {
				return Err(ParserError::ExpectedNewLine{
					token_value: name_token.value.to_string(),
					line: name_token.line,
					col: name_token.col,
				});
			}

			let helper_fn = ast.parse_helper_fn(&mut tokens)?;
			let (helper_fn_name, return_ty, helper_fn_arguments) = if let GlobalStatement::HelperFunction(HelperFunction{name, return_ty, arguments, ..}) = &helper_fn {(Arc::clone(name), return_ty.clone(), arguments)} else {unreachable!()};
			seen_helper_fn = true;

			if ast.helper_fn_signatures.get(&helper_fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedHelperFunction{
					helper_fn_name,
				});
			}

			ast.helper_fn_signatures.insert(helper_fn_name, (return_ty, helper_fn_arguments.clone()));
			ast.global_statements.push(helper_fn);

			newline_allowed = true;
			newline_seen = false;
			newline_required = true;

			just_seen_global = false;
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
		} else if let Ok(&[ref token]) = consume_next_token_types(&mut tokens, &[TokenType::NewLine]) {
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
		} else if let Ok(&[ref comment_token]) = consume_next_token_types(&mut tokens, &[TokenType::Comment]) {
			newline_allowed = true;

			ast.global_statements.push(GlobalStatement::Comment{
				value: comment_token.value.into(),
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

impl AST {
	fn new() -> Self {
		Self {
			global_statements: Vec::new(),
			called_helper_fns: HashSet::new(),
			helper_fn_signatures: HashMap::new(),
			on_fn_signatures: HashMap::new(),
		}
	}

	// helper_fn -> "on_" + name + "(" + arguments? + ")" + type + statements 
	fn parse_helper_fn<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<GlobalStatement, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		if !self.called_helper_fns.contains(fn_name) {
			return Err(ParserError::HelperFnDefinedBeforeCall {
				helper_fn_name: fn_name.into(),
			});
		}

		// This should never fail because this is checked before calling parse_helper_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let args = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_arguments(tokens)?
		} else {
			Vec::new()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;

		// return type
		let return_ty = if let Ok(&[_, ref type_token]) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Word]) {
			match self.parse_type(type_token)? {
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
		
		let body_statements = self.parse_statements(tokens, 0, 1)?;

		if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLineStatement)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
				line: name_token.line, 
				col: name_token.col,
			});
		}

		Ok(GlobalStatement::HelperFunction(HelperFunction{
			name: fn_name.into(),
			arguments: args,
			body_statements,
			calls_helper_fn: false,
			has_while_loop: false,
			return_ty
		}))
	}

	// on_fn -> "on_" + name + "(" + arguments? + ")" + statements 
	fn parse_on_fn<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<GlobalStatement, ParserError> {
		let name_token = tokens.next().unwrap();
		let fn_name = name_token.value;

		// This should never fail because this is checked before calling parse_on_fn
		consume_next_token_types(tokens, &[TokenType::OpenParenthesis]).unwrap();

		let args = if assert_next_token_types(tokens, &[TokenType::Word]).is_ok() {
			self.parse_arguments(tokens)?
		} else {
			Vec::new()
		};
		consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
		
		let body_statements = self.parse_statements(tokens, 0, 1)?;

		if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLineStatement)) {
			return Err(ParserError::EmptyFunction{
				name: fn_name.to_string(),
				line: name_token.line, 
				col: name_token.col,
			});
		}

		Ok(GlobalStatement::OnFunction(OnFunction{
			name: fn_name.into(),
			arguments: args,
			body_statements,
			calls_helper_fn: false,
			has_while_loop: false,
		}))
	}

	// arguments -> argument + ("," + argument)*;
	fn parse_arguments<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<Vec<Argument>, ParserError> {
		let mut arguments = Vec::new();
		loop {
			// parse_arg
			let name_token = get_next_token(tokens)?;
			let arg_name = name_token.value;
			consume_next_token_types(tokens, &[TokenType::Colon, TokenType::Space])?;

			let arg_type = self.parse_type(get_next_token(tokens)?)?;

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
				name: arg_name.into(),
				ty: arg_type,
			});
			
			if consume_next_token_types(tokens, &[TokenType::Comma]).is_err() {
				break;
			}
			
			consume_space(tokens)?;
		}
		Ok(arguments)
	}

	// TODO: Get the grammar for statements
	// This parser consumes a space before consuming the curly braces
	fn parse_statements<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize) -> Result<Vec<Statement>, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		consume_next_token_types(tokens, &[TokenType::Space, TokenType::OpenBrace, TokenType::NewLine])?;

		let mut newline_allowed = false;
		let mut newline_seen = false;

		let mut statements = Vec::new();

		while !is_end_of_block(tokens, indentation)? {
			// newlines
			if let Ok(&[ref token]) = consume_next_token_types(tokens, &[TokenType::NewLine]) {
				if !newline_allowed {
					return Err(ParserError::NewlineNotAllowed{
						line: token.line,
						col: token.col,
					});
				}
				// cannot have consecutive newlines
				newline_allowed = false;
				newline_seen = true;

				statements.push(Statement::EmptyLineStatement);
			} else {
				newline_allowed = true;
				newline_seen = false;
				consume_indentation(tokens, indentation)?;

				statements.push(self.parse_statement(tokens, parsing_depth + 1, indentation)?);
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

		Ok(statements)
	}

	// stmt -> variable_stmt | if_stmt | return_stmt | while_stmt | ;
	fn parse_statement<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize) -> Result<Statement, ParserError> {
		let next_tokens = peek_next_tokens::<2>(tokens)?;
		match next_tokens[0].ty {
			TokenType::Word => {
				match next_tokens[1].ty {
					TokenType::OpenParenthesis => {
						Ok(Statement::CallStatement{
							expr: self.parse_call(tokens, parsing_depth + 1)?,
						})
					}
					TokenType::Colon | TokenType::Space => {
						self.parse_local_variable(tokens, parsing_depth + 1)
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
				let (condition, if_statements) = self.parse_if_statement(tokens, parsing_depth + 1, indentation)?;
				let mut else_if_statements = Vec::new();
				let mut else_statements = None;

				while let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Else]) {
					let [space_token, if_token] = peek_next_tokens(tokens)?;
					if TokenType::Space == space_token.ty && TokenType::If == if_token.ty {
						consume_next_token_types(tokens, &[TokenType::Space]).unwrap();
						else_if_statements.push(self.parse_if_statement(tokens, parsing_depth, indentation)?);
					} else {
						else_statements = Some(self.parse_statements(tokens, parsing_depth, indentation + 1)?);
						break;
					}
				}
				Ok(Statement::IfStatement{
					condition, 
					if_statements,
					else_if_statements,
					else_statements,
				})
			}
			TokenType::Return => {
				tokens.next();
				let expr = if let TokenType::NewLine = next_tokens[1].ty {
					None
				} else {
					consume_space(tokens)?;
					Some(self.parse_expression(tokens, parsing_depth + 1)?)
				};
				Ok(Statement::ReturnStatement{
					expr
				})
			}
			TokenType::While => {
				self.parse_while_statement(tokens, parsing_depth + 1, indentation)
			}
			TokenType::Break => {
				tokens.next();
				Ok(Statement::BreakStatement)
			}
			TokenType::Continue => {
				tokens.next();
				Ok(Statement::ContinueStatement)
			}
			TokenType::NewLine => {
				tokens.next();
				Ok(Statement::EmptyLineStatement)
			}
			TokenType::Comment => {
				tokens.next();
				Ok(Statement::Comment{
					value: next_tokens[0].value.into(),
				})
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
	
	// while_statement -> "while" + " " + expr + statements
	fn parse_while_statement<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize) -> Result<Statement, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		consume_next_token_types(tokens, &[TokenType::While, TokenType::Space])?;

		let condition = self.parse_expression(tokens, parsing_depth + 1)?;
		let statements = self.parse_statements(tokens, parsing_depth + 1, indentation + 1)?;

		Ok(Statement::WhileStatement{
			condition,
			statements,
		})
	}

	// if_stmt -> "if" + " " + expr + " " + statements + ("else" + (" " + if_stmt | statements))?
	fn parse_if_statement<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize) -> Result<(Expr, Vec<Statement>), ParserError> {
		assert_parsing_depth(parsing_depth)?;
		consume_next_token_types(tokens, &[TokenType::If, TokenType::Space])?;

		let condition = self.parse_expression(tokens, parsing_depth + 1)?;
		let statements = self.parse_statements(tokens, parsing_depth + 1, indentation + 1)?;

		Ok((condition, statements))
	}

	// local_variable -> word + (":" + type)? + "=" + " " + expr
	fn parse_local_variable<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Statement, ParserError> {
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
			ty = Some(self.parse_type(get_next_token(tokens)?)?);

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
		let assignment_expr = self.parse_expression(tokens, parsing_depth + 1)?;
		Ok(Statement::VariableStatement{
			name: local_name.into(),
			ty,
			assignment_expr,
		})
	}

	// global -> word + ":" + type + " =" + expr;
	fn parse_global_variable<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<GlobalStatement, ParserError> {
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

		let global_type = self.parse_type(get_next_token(tokens)?)?;
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
		
		let assignment_expr = self.parse_expression(tokens, 0)?;
		
		return Ok(GlobalStatement::Variable(GlobalVariable{
			name: global_name.into(),
			ty: global_type,
			assignment_expr,
		}));
	}

	// Recursive descent parsing adapted from the implementation in grug:
	// https://github.com/grug-lang/grug
	fn parse_expression<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		return self.parse_or(tokens, parsing_depth + 1);
	}

	// or -> and + " " + "||" + and

	fn parse_or<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_and(tokens, parsing_depth + 1)?;

		while let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Or]) {
			let left_expr = expr;
			consume_space(tokens)?;
			let right_expr = self.parse_and(tokens, parsing_depth + 1)?;
			expr = Expr {
				ty: ExprType::BinaryExpr{
					operands: Box::new((left_expr, right_expr)),
					operator: BinaryOperator::Or,
				},
				result_ty: None,
			};
		}
		return Ok(expr);
	}
	
	// and -> equality + " " + "||" + equality

	fn parse_and<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_equality(tokens, parsing_depth + 1)?;

		while let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::And]) {
			let left_expr = expr;
			consume_space(tokens)?;
			let right_expr = self.parse_equality(tokens, parsing_depth + 1)?;
			expr = Expr {
				ty: ExprType::BinaryExpr{
					operands: Box::new((left_expr, right_expr)),
					operator: BinaryOperator::And,
				},
				result_ty: None,
			};
		}
		return Ok(expr);
	}
	
	// equality -> comparison + " " + ("==" | "!=") + comparison

	fn parse_equality<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_comparison(tokens, parsing_depth + 1)?;

		loop {
			if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::DoubleEquals]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_comparison(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::DoubleEquals,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::NotEquals]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_comparison(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::NotEquals,
					},
					result_ty: None,
				};
			} else {
				break
			}
		}
		return Ok(expr);
	}

	fn parse_comparison<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_term(tokens, parsing_depth + 1)?;

		loop {
			if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Greater]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_term(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Greater,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::GreaterEquals]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_term(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::GreaterEquals,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Less]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_term(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Less,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::LessEquals]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_term(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::LessEquals,
					},
					result_ty: None,
				};
			} else {
				break
			}
		}
		return Ok(expr);
	}

	fn parse_term<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_factor(tokens, parsing_depth + 1)?;

		loop {
			if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Plus]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_factor(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Plus,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Minus]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_factor(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Minus,
					},
					result_ty: None,
				};
			} else {
				break
			}
		}
		return Ok(expr);
	}

	fn parse_factor<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let mut expr = self.parse_unary(tokens, parsing_depth + 1)?;

		loop {
			if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Star]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_unary(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Multiply,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::ForwardSlash]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_unary(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Division,
					},
					result_ty: None,
				};
			} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Percent]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = self.parse_unary(tokens, parsing_depth + 1)?;
				expr = Expr {
					ty: ExprType::BinaryExpr{
						operands: Box::new((left_expr, right_expr)),
						operator: BinaryOperator::Division,
					},
					result_ty: None,
				};
			} else {
				break
			}
		}
		return Ok(expr);
	}

	fn parse_unary<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;

		if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Minus]) {
			Ok(Expr {
				ty: ExprType::UnaryExpr{
					operator: UnaryOperator::Minus,
					expr: Box::new(self.parse_unary(tokens, parsing_depth + 1)?),
				},
				result_ty: None,
			})
		} else if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Not]) {
			consume_space(tokens)?;
			Ok(Expr {
				ty: ExprType::UnaryExpr{
					operator: UnaryOperator::Not,
					expr: Box::new(self.parse_unary(tokens, parsing_depth + 1)?),
				},
				result_ty: None,
			})
		} else {
			self.parse_call(tokens, parsing_depth + 1)
		}
	}

	// call -> primary | word + "(" + (expr + ("," + " " + expr)*) ? + ")"
	fn parse_call<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		let expr = self.parse_primary(tokens, parsing_depth + 1)?;

		// Not a function call
		if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::OpenParenthesis]) {
			// functions can only be identifiers for now
			match expr.ty {
				ExprType::LiteralExpr {
					expr: LiteralExpr::IdentifierExpr{
						name,
					},
					..
				} => {
					// TODO: Add called helper_functions tracking
					if name.starts_with("helper_") {
						self.called_helper_fns.insert(Arc::clone(&name));
					}
					if let Ok(&[ref close_paren_token]) = consume_next_token_types(tokens, &[TokenType::CloseParenthesis]) {
						return Ok(Expr{
							ty: ExprType::CallExpr {
								function_name: name.into(),
								arguments: Vec::new(),
								line: close_paren_token.line,
								col: close_paren_token.col,
							},
							result_ty: None,
						});
					}

					let mut arguments = Vec::new();

					loop {
						arguments.push(self.parse_expression(tokens, parsing_depth + 1)?);
						if !consume_next_token_types(tokens, &[TokenType::Comma]).is_ok() {
							let [close_paren_token] = consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
							return Ok(Expr{
								ty: ExprType::CallExpr {
									function_name: name.into(),
									arguments: arguments,
									line: close_paren_token.line,
									col: close_paren_token.col,
								},
								result_ty: None,
							});
						}
						consume_space(tokens)?;
					}
				}
				_ => {
					let (line, col) = expr.ty.get_last_known_location();
					return Err(ParserError::UnexpectedOpenParenthesis{
						got_ty: expr.ty,
						line, 
						col,
					})
				}
			}
		} else {
			Ok(expr)
		}

	}

	// primary -> "(" + expr + ")" | "true" | "false" | string | word | i32 | f32;
	fn parse_primary<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize) -> Result<Expr, ParserError> {
		assert_parsing_depth(parsing_depth)?;
		
		match get_next_token(tokens)? {
			Token{ty: TokenType::OpenParenthesis, ..} => {
				let expr = self.parse_expression(tokens, parsing_depth + 1)?;
				let close_paren = &consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?[0];

				return Ok(Expr{
					ty: ExprType::ParenthesizedExpr{
						expr: Box::new(expr),
						line: close_paren.line,
						col: close_paren.col,
					},
					result_ty: None,
				});
			}
			Token{ty: TokenType::True, line, col, ..} => {
				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::TrueExpr,
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty: TokenType::False, line, col, ..} => {
				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::FalseExpr,
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty: TokenType::String, line, col, value} => {
				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::StringExpr {
							value: NTStr::arc_from_str(*value),
						},
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty: TokenType::Word, line, col, value} => {
				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::IdentifierExpr {
							name: (*value).into(),
						},
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty: TokenType::Int32, line, col, value} => {
				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::NumberExpr {
							value: value.parse::<i64>().unwrap() as f64,
							string: Arc::from(*value),
						},
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty: TokenType::Float32, line, col, value} => {
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

				Ok(Expr{
					ty: ExprType::LiteralExpr{
						expr: LiteralExpr::NumberExpr {
							value: number,
							string: Arc::from(*value),
						},
						line: *line,
						col: *col
					},
					result_ty: None,
				})
			}
			Token{ty, line, col,..} =>  {
				Err(ParserError::ExpectedPrimaryExpression{
					got_token: *ty,
					line: *line, 
					col: *col,
				})
			}
		}
	}
	
	fn parse_type<'a>(&mut self, type_token: &Token) -> Result<GrugType, ParserError> {
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
				extension: "".into(),
			},
			"id"       => GrugType::Id {custom_name: None},
			"entity"   => GrugType::Entity {
				ty: None,
			},
			type_name => {
				GrugType::Id {
					custom_name: Some(type_name.into()),
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
	for (Token{ty: got_ty, line, col, ..}, expected_ty) in tokens.clone().zip(expected.into_iter()) {
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
	*tokens = tokens.as_slice()[expected.len()..].into_iter();
	Ok(unsafe{&*(ret_val as *const [Token] as *const [Token; N])})
}

fn get_next_token<'a> (tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
	tokens.next().ok_or(ParserError::OutOfTokensError)
}

fn peek_next_token<'a> (tokens: &std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
	tokens.as_slice().get(0).ok_or(ParserError::OutOfTokensError)
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
