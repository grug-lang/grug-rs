use super::tokenizer::{Token, TokenType};
use crate::ast::{
	GrugType, HelperFunction, Statement, OnFunction, Parameter,
	MemberVariable, Expr, ExprData, UnaryOperator,
	BinaryOperator, 
};
use super::GlobalStatement;
use crate::ntstring::NTStr;
use crate::arena::Arena;

use gruggers_core::error::{GrugError, ErrorKind, SourceSpan};

use std::ffi::OsStr;

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;

#[allow(unused)]
#[derive(Debug)]
pub enum ParserError<'a> {
	GrugError(GrugError<Arena>),
	// grug_error("Unexpected token '%s' on line %zu", token.str, get_token_line_number(i));
	UnexpectedToken {
		token: Token<'a>,
	},
	UnexpectedEof {
		expected: TokenType,
	},
	GotWrongToken {
		expected: TokenType,
		got: Token<'a>,
	},
	ExpectedSpace {
		got: Token<'a>
	},
	// TODO: This is a bad error message
	// "token_index 1 was out of bounds in peek_token()"
	OutOfTokensError,
	ExceededMaxParsingDepth,
	IndentationMismatch{
		expected_spaces: usize,
		token: Token<'a>,
	},
	ExpectedIndentation{
		got: Token<'a>,
	},
	ExpectedStatement{
		prev_token: String,
		line: usize,
	},
}

impl<'a> ParserError<'a> {
	fn into_grug_error(self, ast: &Ast) -> GrugError<Arena> {
		match self {
			Self::GrugError(err) => err,
			// grug_error("Unexpected token '%s' on line %zu", token.str, get_token_line_number(i));
			Self::UnexpectedToken {
				token,
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				token.span,
				format_args!("Unexpected token '{}' on line {}", token.value, token.span.line),
			),
			Self::UnexpectedEof {
				expected,
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				ast.last_token_span,
				format_args!("Expected {} but got end of file", expected),
			),
			Self::GotWrongToken {
				expected,
				got,
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				got.span,
				format_args!("Expected {} but got {}", expected, got.ty),
			),
			Self::ExpectedSpace {
				got
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				got.span,
				format_args!("Expected space (' '), but got {} at line {}", got.ty, got.span.line),
			),
			// TODO: This is a bad error message
			// "token_index 1 was out of bounds in peek_token()"
			Self::OutOfTokensError => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				ast.last_token_span,
				format_args!("unexpected end of file"),
			),
			Self::ExceededMaxParsingDepth => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				ast.last_token_span,
				format_args!("There is a function that contains more than {} levels of nested expressions", MAX_PARSING_DEPTH),
			),
			Self::IndentationMismatch{
				expected_spaces,
				token,
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				token.span,
				format_args!("Expected {} spaces, but got {} spaces", expected_spaces, token.value.len())
			),
			Self::ExpectedIndentation{
				got,
			} => GrugError::new_error(
				ErrorKind::PARSER_ERROR,
				ast.current_function,
				ast.file_path,
				ast.file_text,
				got.span,
				format_args!("Expected indentation, line break, or '}}' but got '{}'", got.value),
			),
			err => unimplemented!("{:?}", err)
		}
	}
}

const MAX_PARSING_DEPTH: usize = 100;

pub(crate) struct Ast<'arena> {
	// needed for error reporting
	pub(crate) file_text: &'arena str,
	// needed for error reporting
	pub(crate) file_path: &'arena OsStr,
	// needed to report `out of tokens errors`
	pub(crate) last_token_span: SourceSpan,
	// needed to report error location
	pub(crate) current_function: &'arena str,
	pub(crate) global_statements: Vec<GlobalStatement<'arena>, &'arena Arena>,
	pub(crate) called_helper_fns: Vec<&'arena str, &'arena Arena>, 
	pub(crate) helper_fn_signatures: Vec<(&'arena str, (GrugType<'arena>, &'arena [Parameter<'arena>])), &'arena Arena>,
	pub(crate) on_fn_signatures: Vec<(&'arena str, &'arena [Parameter<'arena>]), &'arena Arena>,
}

pub(crate) fn parse<'a>(tokens: &'a [Token], arena: &'a Arena, file_text: &'a str, file_path: &'a OsStr) -> Result<Ast<'a>, GrugError<Arena>> {
	let final_token = tokens.last().map(|token| token.span).unwrap_or(SourceSpan{offset: 0, line: 0});
	let mut ast = Ast::new_in(final_token, file_text, file_path, arena);
	let mut seen_helper_fn = false;

	let mut seen_on_fn = false;
	let mut newline_allowed = false;
	let mut newline_seen = false;
	let mut newline_required = false;
	let mut last_newline_token_span = SourceSpan{offset: 0, line: 0};

	let mut tokens = tokens.iter();

	let result = (|ast: &mut Ast<'a>| -> Result<(), ParserError<'a>> {
		while let Ok(token) = peek_next_token(&tokens) {
			if let Ok([name_token, _]) = consume_next_token_types(&mut tokens, &[TokenType::Word, TokenType::Colon]) {
				if seen_on_fn {
					return ast.new_parse_error(
						name_token.span,
						format_args!("Cannot declare member variables after on_ functions")
					);
				}

				let global_name = name_token.value; 

				if global_name == "me" {
					return ast.new_parse_error(
						name_token.span,
						format_args!("variable cannot be named 'me'")
					);
				}
				consume_space(&mut tokens)?;

				let type_token = get_next_token(&mut tokens)?;
				let global_type = ast.parse_type(type_token, arena)?;
				match global_type {
					GrugType::Resource{..} => {
						return ast.new_parse_error(
							type_token.span,
							format_args!("The global variable '{}' can't have 'resource' as its type", global_name)
						);
					},
					GrugType::Entity{..} => {
						return ast.new_parse_error(
							type_token.span,
							format_args!("The global variable '{}' can't have 'entity' as its type", global_name)
						);
					},
					_ => (),
				}

				// TODO: I think this will error on this line
				// `x: number =25`
				//
				// The error message is not going to be helpful in that case
				match peek_next_token(&tokens)? {
					Token{ty: TokenType::Space, ..} => (),
					Token{span, ..} => return ast.new_parse_error(
						*span,
						format_args!("The global variable '{}' was not assigned a value", global_name)
					),
				}

				consume_space(&mut tokens)?;
				consume_next_token_types(&mut tokens, &[TokenType::Equal])?;

				consume_space(&mut tokens)?;
				
				let assignment_expr = ast.parse_expression(&mut tokens, 0, 0., arena)?;
				
				ast.global_statements.push(GlobalStatement::Variable(MemberVariable{
					name: Box::leak(NTStr::box_from_str_in(global_name, arena)).as_ntstrptr(),
					ty: global_type,
					assignment_expr,
					span: name_token.span
				}));
			let global_variable = ast.parse_global_variable(&mut tokens, &arena)?;
			ast.global_statements.push(GlobalStatement::Variable(global_variable));
			consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;

			newline_allowed = true;
			newline_required = true;
			just_seen_global = true;
		// on_fn -> "export" + " " + name + "(" + arguments? + ")" + statements 
		} else if let Ok(_) = consume_next_token_types(&mut tokens, &[TokenType::Export]) {
			// Require space after Export Token
			consume_space(&mut tokens)?;

			let [name_token] = consume_next_token_types(&mut tokens, &[TokenType::Word])?;

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

			let fn_name = name_token.value;

			consume_next_token_types(&mut tokens, &[TokenType::OpenParenthesis])?;

			let parameters = if assert_next_token_types(&mut tokens, &[TokenType::Word]).is_ok() {
				ast.parse_parameters(&mut tokens, arena)?
			} else {
				&[]
			};
			consume_next_token_types(&mut tokens, &[TokenType::CloseParenthesis])?;
			
			let body_statements = ast.parse_statements(&mut tokens, 0, 1, arena)?;

			if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLine)) {
				return Err(ParserError::EmptyFunction{
					name: fn_name.to_string(),
				});
			}

			let on_fn = OnFunction{
				name: Box::leak(NTStr::box_from_str_in(fn_name, arena)).as_ntstrptr(),
				parameters,
				body_statements,
				span: name_token.span
			};

			let fn_name = on_fn.name.to_str();
			if ast.on_fn_signatures.iter().find(|(name, _)| *name == fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedOnFn{
					fn_name: Arc::from(fn_name),
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
		// helper_fn -> "local" + " " + name + "(" + arguments? + ")" + type + statements 
		} else if let Ok(_) = consume_next_token_types(&mut tokens, &[TokenType::Local]) {
			consume_space(&mut tokens)?;
			let [name_token] = consume_next_token_types(&mut tokens, &[TokenType::Word])?;
			// expect newline after each item
			if newline_required {
				return Err(ParserError::ExpectedNewLine{
					line: name_token.span.line,
				});
			}

			let fn_name = name_token.value;

			if ast.called_helper_fns.iter().find(|val| **val == fn_name).is_none() {
				return Err(ParserError::HelperFnDefinedBeforeCall {
					fn_name: fn_name.into(),
				});
			}

			// This should never fail because this is checked before calling parse_helper_fn
			consume_next_token_types(&mut tokens, &[TokenType::OpenParenthesis]).unwrap();

			let parameters = if assert_next_token_types(&mut tokens, &[TokenType::Word]).is_ok() {
				ast.parse_parameters(&mut tokens, arena)?
			} else {
				&[]
			};
			consume_next_token_types(&mut tokens, &[TokenType::CloseParenthesis])?;

			// return type
			let return_type = if let Ok([_, type_token]) = consume_next_token_types(&mut tokens, &[TokenType::Space, TokenType::Word]) {
				match ast.parse_type(type_token, arena)? {
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
			
			let body_statements = ast.parse_statements(&mut tokens, 0, 1, arena)?;

			if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLine)) {
				return Err(ParserError::EmptyFunction{
					name: fn_name.to_string(),
				});
			}

			let helper_fn = HelperFunction{
				name: Box::leak(NTStr::box_from_str_in(fn_name, arena)).as_ntstrptr(),
				parameters,
				body_statements,
				return_type,
				span: name_token.span,
			};

			seen_helper_fn = true;

			if ast.helper_fn_signatures.iter().find(|(name, _)| *name == fn_name).is_some() {
				return Err(ParserError::AlreadyDefinedHelperFunction{
					fn_name: Arc::from(helper_fn.name.to_str()),
				});
			}
				newline_allowed = true;
				newline_seen = false;
				newline_required = true;

				consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
			// helper_fn -> "local" + " " + name + "(" + arguments? + ")" + type + statements 
			} else if let Ok([name_token]) = assert_next_token_types(&tokens, &[TokenType::Word]) && name_token.value.starts_with("helper_") {
				let [name_token] = consume_next_token_types(&mut tokens, &[TokenType::Word]).unwrap();
				let fn_name = name_token.value;
				// expect newline after each item
				if newline_required {
					return ast.new_parse_error(
						name_token.span,
						format_args!("Expected an empty line")
					);
				}

				ast.current_function = fn_name;

				if !ast.called_helper_fns.contains(&fn_name) {
					return ast.new_parse_error(
						name_token.span,
						format_args!("{}() is defined before the first time it gets called", fn_name)
					);
				}

				consume_next_token_types(&mut tokens, &[TokenType::OpenParenthesis])?;

				let parameters = if assert_next_token_types(&tokens, &[TokenType::Word]).is_ok() {
					ast.parse_parameters(&mut tokens, arena)?
				} else {
					&[]
				};
				consume_next_token_types(&mut tokens, &[TokenType::CloseParenthesis])?;

				// return type
				let return_type = if let Ok([_, type_token]) = consume_next_token_types(&mut tokens, &[TokenType::Space, TokenType::Word]) {
					match ast.parse_type(type_token, arena)? {
						GrugType::Resource{..} => {
							return ast.new_parse_error(
								type_token.span,
								format_args!("The function '{}' can't have 'resource' as its return type", fn_name)
							);
						},
						GrugType::Entity{..} => {
							return ast.new_parse_error(
								type_token.span,
								format_args!("The function '{}' can't have 'entity' as its return type", fn_name)
							);
						},
						x => x,
					}
				} else {
					GrugType::Void
				};
				
				let body_statements = ast.parse_statements(&mut tokens, 0, 1, arena)?;

				if body_statements.iter().all(|x| matches!(x, Statement::Comment{..} | Statement::EmptyLine)) {
					return ast.new_parse_error(
						name_token.span,
						format_args!("{}() can't be empty", fn_name),
					);
				}

				let helper_fn = HelperFunction{
					name: Box::leak(NTStr::box_from_str_in(fn_name, arena)).as_ntstrptr(),
					parameters,
					body_statements,
					return_type,
					span: name_token.span,
				};

				seen_helper_fn = true;

				if ast.helper_fn_signatures.iter().any(|(name, _)| *name == fn_name) {
					return ast.new_parse_error(
						name_token.span,
						format_args!("The function '{}' was defined several times in the same file", fn_name),
					);
				}
				ast.current_function = "member scope";

				ast.helper_fn_signatures.push((fn_name, (helper_fn.return_type, helper_fn.parameters)));
				ast.global_statements.push(GlobalStatement::HelperFunction(helper_fn));

				newline_allowed = true;
				newline_seen = false;
				newline_required = true;

				consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;
			} else if let Ok([token]) = consume_next_token_types(&mut tokens, &[TokenType::NewLine]) {
				if !newline_allowed {
					return ast.new_parse_error(
						token.span,
						format_args!("Unexpected empty line")
					);
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
				Err(ParserError::UnexpectedToken{
					token: *token
				})?
			}
		}

		if !newline_allowed && newline_seen {
			// a newline has been seen so the line number will be incremented by one
			// but we want the line number of the previous line
			return ast.new_parse_error(
				last_newline_token_span,
				format_args!("Unexpected empty line")
			);
		}
		Ok(())
	})(&mut ast);
	match result {
		Ok(()) => Ok(ast),
		Err(err) => Err(err.into_grug_error(&ast))
	}
}

impl<'a> Ast<'a> {
	fn new_in(last_token_span: SourceSpan, file_text: &'a str, file_path: &'a OsStr, arena: &'a Arena) -> Self {
		Self {
			file_text,
			file_path,
			last_token_span,
			current_function: "member scope",
			global_statements: Vec::new_in(arena),
			called_helper_fns: Vec::new_in(arena),
			helper_fn_signatures: Vec::new_in(arena),
			on_fn_signatures: Vec::new_in(arena),
		}
	}

	#[track_caller]
	#[inline]
	fn new_parse_error<T>(&self, span: SourceSpan, args: std::fmt::Arguments) -> Result<T, ParserError<'static>> {
		Err(ParserError::GrugError(GrugError::new_error(
			ErrorKind::PARSER_ERROR,
			self.current_function,
			self.file_path,
			self.file_text,
			span,
			args
		)))
	}

	// parameters -> parameter + ("," + parameter)*;
	fn parse_parameters(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, arena: &'a Arena) -> Result<&'a [Parameter<'a>], ParserError<'a>> {
		let mut arguments = Vec::new_in(arena);
		loop {
			// parse_arg
			let name_token = get_next_token(tokens)?;
			let arg_name = name_token.value;
			consume_next_token_types(tokens, &[TokenType::Colon, TokenType::Space])?;

			let type_token = get_next_token(tokens)?;
			let param_type = self.parse_type(type_token, arena)?;

			match param_type {
				GrugType::Resource{..} => {
					return self.new_parse_error(
						type_token.span,
						format_args!("The argument '{}' can't have 'resource' as its type", arg_name)
					);
				},
				GrugType::Entity{..} => {
					return self.new_parse_error(
						type_token.span,
						format_args!("The argument '{}' can't have 'entity' as its type", arg_name)
					);
				},
				_ => (),
			}
			arguments.push(Parameter{
				name: Box::leak(NTStr::box_from_str_in(arg_name, arena)).as_ntstrptr(),
				ty: param_type,
				name_span: name_token.span,
				type_span: type_token.span
			});
			
			if consume_next_token_types(tokens, &[TokenType::Comma]).is_err() {
				break;
			}
			
			consume_space(tokens)?;
		}
		Ok(arguments.leak())
	}

	// TODO: Get the grammar for statements
	// This parser consumes a space before consuming the curly braces
	fn parse_statements(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<&'a mut [Statement<'a>], ParserError<'a>> {
		assert_parsing_depth(parsing_depth)?;
		let &[_, _, mut last_new_line] = consume_next_token_types(tokens, &[TokenType::Space, TokenType::OpenBrace, TokenType::NewLine])?;

		let mut newline_allowed = false;
		let mut newline_seen = false;

		let mut statements = Vec::new_in(arena);

		while !is_end_of_block(tokens, indentation)? {
			// newlines
			if let Ok([indentation_token, _]) = consume_next_token_types(tokens, &[TokenType::Indentation, TokenType::NewLine]) {
				return self.new_parse_error(
					indentation_token.span,
					format_args!("Empty line cannot have indentation")
				);
			} else if let Ok([token]) = consume_next_token_types(tokens, &[TokenType::NewLine]) {
				last_new_line = *token;
				if !newline_allowed {
					return self.new_parse_error(
						token.span,
						format_args!("Unexpected empty line")
					);
				}
				// cannot have consecutive newlines
				newline_allowed = false;
				newline_seen = true;

				statements.push(Statement::EmptyLine);
			} else {
				newline_allowed = true;
				newline_seen = false;
				consume_indentation(tokens, indentation)?;

				statements.push(self.parse_statement(tokens, parsing_depth + 1, indentation, arena)?);
				consume_next_token_types(tokens, &[TokenType::NewLine])?;
			}
		}

		if !newline_allowed && newline_seen {
			// a newline has been seen so the line number will be incremented by one
			// but we want the line number of the previous line
			return self.new_parse_error(
				last_new_line.span,
				format_args!("Unexpected empty line")
			);
		}

		if indentation != 1 {
			consume_indentation(tokens, indentation - 1)?;
		}
		consume_next_token_types(tokens, &[TokenType::CloseBrace])?;

		Ok(statements.leak())
	}

	// stmt -> variable_stmt | if_stmt | return_stmt | while_stmt | ;
	fn parse_statement(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError<'a>> {
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
						self.new_parse_error(
							next_tokens[1].span,
							format_args!("Expected '(', or ':', or ' =' after the word '{}' on line {}", next_tokens[0].value, next_tokens[0].span.line),
						)
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
				Ok(Statement::Return{ 
					return_span: next_tokens[0].span,
					expr: expr.map(|expr| Box::leak(Box::new_in(expr, arena)))
				})
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
				Ok(Statement::Break(next_tokens[0].span))
			}
			TokenType::Continue => {
				tokens.next();
				Ok(Statement::Continue(next_tokens[0].span))
			}
			TokenType::Comment => {
				tokens.next();
				Ok(Statement::Comment{
					comment_span: next_tokens[0].span,
					value: Box::leak(NTStr::box_from_str_in(next_tokens[0].value, arena)).as_ntstrptr()
				})
			}
			got_token => {
				self.new_parse_error(
					next_tokens[0].span,
					format_args!("Expected a statement token, but got {} on line {}", got_token, next_tokens[0].span.line)
				)
			},
		}
	}

	// local_variable -> word + (":" + type)? + "=" + " " + expr
	fn parse_local_variable(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, arena: &'a Arena) -> Result<Statement<'a>, ParserError<'a>> {
		assert_parsing_depth(parsing_depth)?;
		let name_token = get_next_token(tokens)?;
		let local_name = name_token.value; 
		let mut ty = None;

		if consume_next_token_types(tokens, &[TokenType::Colon]).is_ok() {
			if local_name == "me" {
				return self.new_parse_error(
					name_token.span,
					format_args!("variable cannot be named 'me'"),
				);
			}
			consume_space(tokens)?;
			let type_token = get_next_token(tokens)?;
			ty = Some(self.parse_type(type_token, arena)?);

			match ty {
				Some(GrugType::Resource{..}) => {
					return self.new_parse_error(
						type_token.span,
						format_args!("The variable '{}' can't have 'resource' as its type", local_name)
					);
				},
				Some(GrugType::Entity{..}) => {
					return self.new_parse_error(
						type_token.span,
						format_args!("The variable '{}' can't have 'entity' as its type", local_name)
					);
				},
				_ => (),
			}
		}
		// TODO: This error should just be folded into ExpectedSpace but it has
		// to be different to match the required error message
		match consume_space(tokens) {
			Ok(_) => (),
			Err(ParserError::ExpectedSpace{got}) => return self.new_parse_error(
				got.span,
				format_args!("Variable '{}' was not assigned a value", local_name),
			),
			Err(ParserError::OutOfTokensError) => return Err(ParserError::OutOfTokensError),
			_ => unreachable!(),
		}

		// TODO: This Me error should be folded into the other Me error within
		// the branch above but it has to be separate to match the required error message
		if local_name == "me" {
			return self.new_parse_error(
				name_token.span,
				format_args!("Assigning a new value to the entity's 'me' variable is not allowed"),
			);
		}

		consume_next_token_types(tokens, &[TokenType::Equal])?;

		consume_space(tokens)?;
		let assignment_expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
		Ok(Statement::Variable{
			name: Box::leak(NTStr::box_from_str_in(local_name, arena)).as_ntstrptr(),
			ty: ty.map(|ty| &*Box::leak(Box::new_in(ty, arena))),
			assignment_expr,
			name_span: name_token.span,
		})
	}

	fn parse_expression(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, min_precedence: f32, arena: &'a Arena) -> Result<Expr<'a>, ParserError<'a>> {
		assert_parsing_depth(parsing_depth)?;
		let mut current: Expr = {
			let Token{ty, span, value} = get_next_token(tokens)?;
			match ty {
				TokenType::OpenParenthesis => {
					let expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
					let _ = &consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?[0];

					Expr{
						data: ExprData::Parenthesized(Box::leak(Box::new_in(expr, arena))),
						result_type: None,
						span: *span,
					}
				}
				TokenType::True => {
					Expr{
						data: ExprData::True,
						result_type: None,
						span: *span,
					}
				}
				TokenType::False => {
					Expr{
						data: ExprData::False,
						result_type: None,
						span: *span,
					}
				}
				TokenType::String => {
					Expr{
						data: ExprData::String(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
						span: *span,
					}
				}
				TokenType::Resource => {
					Expr{
						data: ExprData::Resource(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
						span: *span,
					}
				}
				TokenType::Entity => {
					Expr{
						data: ExprData::Entity(Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr()),
						result_type: None,
						span: *span,
					}
				}
				TokenType::Word => {
					let value: &'a NTStr  = Box::leak(NTStr::box_from_str_in(value, arena));
					// a word token can actually be a function call
					if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::OpenParenthesis]) {
						if value.as_str().starts_with("helper_")
							&& !self.called_helper_fns.contains(&value.as_str())
						{
							self.called_helper_fns.push(value);
						}
						
						// immediate ")" | (expr + ("," + " " + expr)*) + ")"
						
						if let Ok([_]) = consume_next_token_types(tokens, &[TokenType::CloseParenthesis]) {
							Expr{
								data: ExprData::Call {
									name: value.as_ntstrptr(),
									args: Vec::new().leak(),
									ptr : None,
									name_span: *span,
								},
								result_type: None,
								span: *span,
							}
						} else {
							let mut arguments = Vec::new_in(arena);
							loop {
								arguments.push(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?);
								if let Ok([_, _]) = consume_next_token_types(tokens, &[TokenType::Comma, TokenType::Space]) {
									
								} else {
									let [_] = consume_next_token_types(tokens, &[TokenType::CloseParenthesis])?;
									break Expr {
										data: ExprData::Call {
											name: value.as_ntstrptr(),
											args: arguments.leak(),
											ptr : None,
											name_span: *span
										},
										result_type: None,
										span: *span,
									};
								}
							}
						}
					} else {
						Expr{
							data: ExprData::Identifier(value.as_ntstrptr()),
							result_type: None,
							span: *span,
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
						span: *span,
					}
				}
				TokenType::Float32 => {
					let number = value.parse::<f64>().unwrap();
					if number > f64::MAX {
						return self.new_parse_error(
							*span,
							format_args!("The number {} is too big", value)
						);
					} else if (number != 0. && number < f64::MIN_POSITIVE) 
						   || (number == 0. && value.contains(['1', '2', '3', '4', '5', '6', '7', '8', '9'])) {
						return self.new_parse_error(
							*span,
							format_args!("The number {} is too close to zero", value)
						);
					}

					Expr{
						data: ExprData::Number(
							number,
							Box::leak(NTStr::box_from_str_in(value, arena)).as_ntstrptr(),
						),
						result_type: None,
						span: *span,
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
							expr: Box::leak(Box::new_in(expr, arena)),
							op_span: *span,
						},
						span: *span,
					}
				}
				_ =>  {
					return self.new_parse_error(
						*span,
						format_args!("Expected a primary expression token but got {}", ty)
					);
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
				span: current.span,
				result_type: None,
				data: ExprData::Binary {
					op: bin_op,
					left : Box::leak(Box::new_in(current, arena)),
					right: Box::leak(Box::new_in(next   , arena)),
					op_span: op.span,
				},
			};
		}
		Ok(current)
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
	
	fn parse_type(&mut self, type_token: &'a Token, arena: &'a Arena) -> Result<GrugType<'a>, ParserError<'a>> {
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

fn is_end_of_block<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>, indentation: usize) -> Result<bool, ParserError<'a>> {
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
			got: *next_token,
		})
	}
}

// Checks if the passed in parsing_depth is allowed
#[track_caller]
fn assert_parsing_depth(parsing_depth: usize) -> Result<(), ParserError<'static>> {
	if parsing_depth > MAX_PARSING_DEPTH {
		Err(ParserError::ExceededMaxParsingDepth)
	} else {
		Ok(())
	}
}

// checks whether the next few tokens match the expected tokens without consuming the input
#[track_caller]
fn assert_next_token_types<'a, const N: usize>(tokens: &std::slice::Iter<'a, Token<'a>>, expected: &[TokenType; N]) -> Result<&'a [Token<'a>;N], ParserError<'a>> {
	if tokens.len() < expected.len() {
		return Err(ParserError::UnexpectedEof{expected: expected[tokens.len()]});
	}
	for (got, expected) in tokens.clone().zip(expected) {
		if got.ty != *expected {
			return Err(ParserError::GotWrongToken{
				expected: *expected,
				got: *got,
			});
		}
	}
	Ok(unsafe{&*(tokens.as_slice().as_ptr() as *const [Token; N])})
}

// consumes the next few tokens if they match the given types, otherwise leaves the input unchanged
#[track_caller]
fn consume_next_token_types<'a, const N: usize>(tokens: &mut std::slice::Iter<'a, Token<'a>>, expected: &'_ [TokenType; N]) -> Result<&'a [Token<'a>; N], ParserError<'a>> {
	let ret_val = assert_next_token_types(tokens, expected)?;
	*tokens = tokens.as_slice()[expected.len()..].iter();
	Ok(ret_val)
}

#[track_caller]
fn get_next_token<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError<'static>> {
	tokens.next().ok_or(ParserError::OutOfTokensError)
}

#[track_caller]
fn peek_next_token<'a>(tokens: &std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError<'static>> {
	tokens.as_slice().first().ok_or(ParserError::OutOfTokensError)
}

#[track_caller]
fn peek_next_tokens<'a, const N: usize> (tokens: &std::slice::Iter<'a, Token<'a>>) -> Result<&'a [Token<'a>; N], ParserError<'static>> {
	Ok(unsafe{&*(tokens.as_slice().get(..N).ok_or(ParserError::OutOfTokensError)? as *const _ as * const _)})
}

#[track_caller]
fn consume_space<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError<'a>> {
	let token = get_next_token(tokens)?;
	
	if token.ty != TokenType::Space {
		return Err(ParserError::ExpectedSpace{got: *token});
	}
	Ok(token)
}

#[track_caller]
fn consume_indentation<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>, indentation: usize) -> Result<&'a Token<'a>, ParserError<'a>> {
	use super::SPACES_PER_INDENT;
	
	let [token] = consume_next_token_types(tokens, &[TokenType::Indentation])?;
	let spaces = token.value.len();
	if spaces != indentation * SPACES_PER_INDENT {
		return Err(ParserError::IndentationMismatch{
			expected_spaces: indentation * SPACES_PER_INDENT,
			token: *token,
		});
	}
	Ok(token)
}

