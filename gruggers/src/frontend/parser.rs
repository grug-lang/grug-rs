use super::GlobalStatement;
use super::tokenizer::{Token, TokenType, TokensSlice, TokensArray};
use crate::arena::Arena;
use crate::VARIABLE_INFERENCE;
use crate::ast::{
    BinaryOperator, Expr, ExprData, HelperFunction, MemberVariable, OnFunction, Parameter,
    Statement, Type, UnaryOperator,
};

use crate::error::{Error, ErrorKind, SourceSpan};

use std::ffi::OsStr;

use allocator_api2::vec::Vec;

#[allow(unused)]
#[derive(Debug)]
/// Lightweight error for structural parsing error
enum ParserError<'a> {
    GrugError(Error),
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
        got: Token<'a>,
    },
    OutOfTokensError,
    ExceededMaxParsingDepth,
    IndentationMismatch {
        expected_spaces: usize,
        token: Token<'a>,
    },
    ExpectedIndentation {
        got: Token<'a>,
    },
}

impl<'a> ParserError<'a> {
    fn into_grug_error(self, parser: &Parser) -> Error {
        match self {
            Self::GrugError(err) => err,
            // grug_error("Unexpected token '%s' on line %zu", token.str, get_token_line_number(i));
            Self::UnexpectedToken { token } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                token.span,
                format_args!(
                    "Unexpected token '{}' on line {}",
                    token.value, token.span.line
                ),
            ),
            Self::UnexpectedEof { expected } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                parser.last_token_span,
                format_args!("Expected {} but got end of file", expected),
            ),
            Self::GotWrongToken { expected, got } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                got.span,
                format_args!("Expected {} but got {}", expected, got.ty),
            ),
            Self::ExpectedSpace { got } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                got.span,
                format_args!(
                    "Expected space (' '), but got {} at line {}",
                    got.ty, got.span.line
                ),
            ),
            Self::OutOfTokensError => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                parser.last_token_span,
                format_args!("unexpected end of file"),
            ),
            Self::ExceededMaxParsingDepth => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                parser.last_token_span,
                format_args!(
                    "There is a function that contains more than {} levels of nested expressions",
                    MAX_PARSING_DEPTH
                ),
            ),
            Self::IndentationMismatch {
                expected_spaces,
                token,
            } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                token.span,
                format_args!(
                    "Expected {} spaces, but got {} spaces",
                    expected_spaces,
                    token.value.len()
                ),
            ),
            Self::ExpectedIndentation { got } => Error::new(
                ErrorKind::PARSER_ERROR,
                parser.current_function,
                parser.file_path,
                parser.file_text,
                got.span,
                format_args!(
                    "Expected indentation, line break, or '}}' but got '{}'",
                    got.value
                ),
            ),
        }
    }
}

const MAX_PARSING_DEPTH: usize = 100;

pub(crate) struct Ast<'arena> {
    pub global_statements: Vec<GlobalStatement<'arena>, &'arena Arena>,
    pub local_fn_signatures: &'arena [(&'arena str, (Type<'arena>, &'arena [Parameter<'arena>]))],
    pub export_fn_signatures: &'arena [(&'arena str, &'arena [Parameter<'arena>])],
}

struct Parser<'arena> {
    // needed for error reporting
    pub(crate) file_text: &'arena str,
    // needed for error reporting
    pub(crate) file_path: &'arena OsStr,
    // needed to report `out of tokens errors`
    pub(crate) last_token_span: SourceSpan,
    // needed to report error location
    pub(crate) current_function: &'arena str,
    pub(crate) global_statements: Vec<GlobalStatement<'arena>, &'arena Arena>,
    pub(crate) called_local_functions: Vec<&'arena str, &'arena Arena>,
    pub(crate) local_fn_signatures:
        Vec<(&'arena str, (Type<'arena>, &'arena [Parameter<'arena>])), &'arena Arena>,
    pub(crate) export_fn_signatures: Vec<(&'arena str, &'arena [Parameter<'arena>]), &'arena Arena>,
}

pub(crate) fn parse<'a>(
    mut tokens: TokensSlice<'a>,
    arena: &'a Arena,
    file_text: &'a str,
    file_path: &'a OsStr,
) -> Result<Ast<'a>, Error> {
    let final_token_span = *tokens.spans()
        .last()
        .unwrap_or(&SourceSpan { offset: 0, line: 1 });
    let mut parser = Parser::new_in(final_token_span, file_text, file_path, arena);
    let mut seen_helper_fn = false;

    let mut seen_on_fn = false;
    let mut newline_allowed = false;
    let mut newline_seen = false;
    let mut newline_required = false;
    let mut last_newline_token_span = SourceSpan { offset: 0, line: 0 };

    let result = (|parser: &mut Parser<'a>| -> Result<(), ParserError<'a>> {
		while let Ok(token) = peek_tokens(&mut tokens) {
            if let Ok(name_token) =
                consume_tokens(&mut tokens, &[TokenType::Word])
            {
				let ([_], [global_name], [name_token_span]) = name_token.as_arrays();
                if seen_on_fn {
                    return parser.new_error(
                        *name_token_span,
                        format_args!("Cannot declare member variables after on_ functions"),
                    );
                }
				consume_tokens(&mut tokens, &[TokenType::Colon])?;

                if *global_name == "me" {
                    return parser.new_error(
                        *name_token_span,
                        format_args!("variable cannot be named 'me'"),
                    );
                }
                consume_space(&mut tokens)?;

                let (global_type, global_type_span) = parser.parse_type(&mut tokens, arena)?;
                if type_contains_resource(global_type) {
                    return parser.new_error(
                        global_type_span,
                        format_args!(
                            "The global variable '{}' can't contain 'resource' in its type",
                            global_name
                        ),
                    );
                }
                if type_contains_entity(global_type) {
                    return parser.new_error(
                        global_type_span,
                        format_args!(
                            "The global variable '{}' can't contain 'entity' in its type",
                            global_name
                        ),
                    );
                }

                // TODO: I think this will error on this line
                // `x: number =25`
                //
                // The error message is not going to be helpful in that case
                match consume_tokens(&mut tokens, &[TokenType::Space]) {
					Ok(_) => (),
					Err(ParserError::GotWrongToken { got, .. }) => {
                        return parser.new_error(
                            got.span,
                            format_args!(
                                "The global variable '{}' was not assigned a value",
                                global_name
                            ),
                        );
					}
					Err(err) => return Err(err)
                };

                consume_tokens(&mut tokens, &[TokenType::Equal])?;

                consume_space(&mut tokens)?;

                let assignment_expr = parser.parse_expression(&mut tokens, 0, 0., arena)?;

                parser
                    .global_statements
                    .push(GlobalStatement::Variable(MemberVariable {
						name: global_name,
                        ty: global_type,
                        type_span: global_type_span,
                        assignment_expr,
                        span: *name_token_span,
                    }));

                consume_tokens(&mut tokens, &[TokenType::NewLine])?;

                newline_allowed = true;
                newline_required = true;
            // export fn -> "export" + " " + name + "(" + arguments? + ")" + statements
            } else if let Ok(_) =
                consume_tokens(&mut tokens, &[TokenType::Export, TokenType::Space])
            {
                let ([_], [fn_name], [name_token_span]) = consume_tokens(&mut tokens, &[TokenType::Word])?.as_arrays();

                // expect newline after each item
                if newline_required {
                    return parser
                        .new_error(*name_token_span, format_args!("Expected an empty line"));
                }

                parser.current_function = fn_name;

                // Cannot have global function after helper function
                if seen_helper_fn {
                    return parser.new_error(
                        *name_token_span,
                        format_args!("{}() must be defined before all local functions", fn_name),
                    );
                }
                consume_tokens(&mut tokens, &[TokenType::OpenParenthesis])?;

                let parameters = if assert_tokens(&tokens, &[TokenType::Word]).is_ok() {
                    parser.parse_parameters(&mut tokens, arena)?
                } else {
                    &[]
                };
                consume_tokens(&mut tokens, &[TokenType::CloseParenthesis])?;

                let body_statements = parser.parse_statements(&mut tokens, 0, 1, arena)?;

                if body_statements
                    .iter()
                    .all(|x| matches!(x, Statement::Comment { .. } | Statement::EmptyLine))
                {
                    return parser.new_error(
                        *name_token_span,
                        format_args!("{}() can't be empty", fn_name),
                    );
                }

                let on_fn = OnFunction {
					name: fn_name,
                    parameters,
                    body_statements,
                    span: *name_token_span,
                };

                if parser
                    .export_fn_signatures
                    .iter()
                    .any(|(name, _)| name == fn_name)
                {
                    return parser.new_error(
                        *name_token_span,
                        format_args!(
                            "The function '{}' was defined several times in the same file",
                            fn_name
                        ),
                    );
                }
                parser.current_function = "member scope";

                parser
                    .export_fn_signatures
                    .push((fn_name, on_fn.parameters));
                parser
                    .global_statements
                    .push(GlobalStatement::OnFunction(on_fn));

                seen_on_fn = true;

                newline_allowed = true;
                newline_seen = false;
                newline_required = true;

                consume_tokens(&mut tokens, &[TokenType::NewLine])?;
            // local fn -> "local" + " " + name + "(" + arguments? + ")" + type + statements
            } else if let Ok(_) =
                consume_tokens(&mut tokens, &[TokenType::Local, TokenType::Space])
            {
                let ([_], [fn_name], [name_token_span]) = consume_tokens(&mut tokens, &[TokenType::Word])?.as_arrays();

				if !fn_name.starts_with("_") {
                    parser.current_function = fn_name;
                    return parser.new_error(
                        *name_token_span,
                        format_args!("Local function name must begin with '_'"),
                    );
                }
                // expect newline after each item
                if newline_required {
                    return parser
                        .new_error(*name_token_span, format_args!("Expected an empty line"));
                }

                parser.current_function = fn_name;

                if !parser.called_local_functions.contains(&fn_name) {
                    return parser.new_error(
                        *name_token_span,
                        format_args!(
                            "{}() is defined before the first time it gets called",
                            fn_name
                        ),
                    );
                }

                consume_tokens(&mut tokens, &[TokenType::OpenParenthesis])?;

                let parameters = if assert_tokens(&tokens, &[TokenType::Word]).is_ok() {
                    parser.parse_parameters(&mut tokens, arena)?
                } else {
                    &[]
                };
                consume_tokens(&mut tokens, &[TokenType::CloseParenthesis])?;

                // return type
                let (return_type, return_type_span) = if let Ok(_) =
                    assert_tokens(&tokens, &[TokenType::Space, TokenType::Word])
                {
                    consume_space(&mut tokens).unwrap();
                    let (return_type, return_type_span) = parser.parse_type(&mut tokens, arena)?;
                    if type_contains_resource(return_type) {
                        return parser.new_error(
                            return_type_span,
                            format_args!(
                                "The function '{}' can't contain 'resource' in its return type",
                                fn_name
                            ),
                        );
                    }
                    if type_contains_entity(return_type) {
                        return parser.new_error(
                            return_type_span,
                            format_args!(
                                "The function '{}' can't contain 'entity' in its return type",
                                fn_name
                            ),
                        );
                    }
                    (return_type, return_type_span)
                } else {
                    (Type::Void, *name_token_span)
                };

                let body_statements = parser.parse_statements(&mut tokens, 0, 1, arena)?;

                if body_statements
                    .iter()
                    .all(|x| matches!(x, Statement::Comment { .. } | Statement::EmptyLine))
                {
                    return parser.new_error(
                        *name_token_span,
                        format_args!("{}() can't be empty", fn_name),
                    );
                }

                let helper_fn = HelperFunction {
					name: fn_name,
                    parameters,
                    body_statements,
                    return_type,
                    return_type_span,
                    span: *name_token_span,
                };

                seen_helper_fn = true;

                if parser
                    .local_fn_signatures
                    .iter()
                    .any(|(name, _)| name == fn_name)
                {
                    return parser.new_error(
                        *name_token_span,
                        format_args!(
                            "The function '{}' was defined several times in the same file",
                            fn_name
                        ),
                    );
                }
                parser.current_function = "member scope";

                parser
                    .local_fn_signatures
                    .push((fn_name, (helper_fn.return_type, helper_fn.parameters)));
                parser
                    .global_statements
                    .push(GlobalStatement::HelperFunction(helper_fn));

                newline_allowed = true;
                newline_seen = false;
                newline_required = true;

                consume_tokens(&mut tokens, &[TokenType::NewLine])?;
            } else if let Ok(token) = consume_tokens(&mut tokens, &[TokenType::NewLine])
            {
                if !newline_allowed {
                    return parser.new_error(token.spans[0], format_args!("Unexpected empty line"));
                }

                // Disallow consecutive empty lines
                newline_allowed = false;
                newline_seen = true;
                newline_required = false;
                last_newline_token_span = token.spans[0];

                parser.global_statements.push(GlobalStatement::EmptyLine);
            } else if let Ok(token) =
                consume_tokens(&mut tokens, &[TokenType::Comment])
            {
				let (_, [value], _) = token.as_arrays();
                newline_allowed = true;

                parser.global_statements.push(GlobalStatement::Comment {
					value,
                });
                consume_tokens(&mut tokens, &[TokenType::NewLine])?;
            } else {
				let ([ty], [value], [span]) = token.as_arrays();
                Err(ParserError::UnexpectedToken { token: Token{ty: *ty, value: *value, span: *span} })?
            }
        }

        if !newline_allowed && newline_seen {
            // a newline has been seen so the line number will be incremented by one
            // but we want the line number of the previous line
            return parser.new_error(
                last_newline_token_span,
                format_args!("Unexpected empty line"),
            );
        }
        Ok(())
    })(&mut parser);
    match result {
        Ok(()) => Ok(Ast {
            global_statements: parser.global_statements,
            local_fn_signatures: parser.local_fn_signatures.leak(),
            export_fn_signatures: parser.export_fn_signatures.leak(),
        }),
        Err(err) => Err(err.into_grug_error(&parser)),
    }
}

impl<'a> Parser<'a> {
    fn new_in(
        last_token_span: SourceSpan,
        file_text: &'a str,
        file_path: &'a OsStr,
        arena: &'a Arena,
    ) -> Self {
        Self {
            file_text,
            file_path,
            last_token_span,
            current_function: "member scope",
            global_statements: Vec::new_in(arena),
            called_local_functions: Vec::new_in(arena),
            local_fn_signatures: Vec::new_in(arena),
            export_fn_signatures: Vec::new_in(arena),
        }
    }

    #[track_caller]
    #[inline]
    fn new_error<T>(
        &self,
        span: SourceSpan,
        args: std::fmt::Arguments,
    ) -> Result<T, ParserError<'static>> {
        Err(ParserError::GrugError(Error::new(
            ErrorKind::PARSER_ERROR,
            self.current_function,
            self.file_path,
            self.file_text,
            span,
            args,
        )))
    }

    // parameters -> parameter + ("," + parameter)*;
    fn parse_parameters(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        arena: &'a Arena,
    ) -> Result<&'a [Parameter<'a>], ParserError<'a>> {
        let mut arguments = Vec::new_in(arena);
        loop {
            // parse_arg
            let ([_], [arg_name], [name_token_span]) = consume_tokens(tokens, &[TokenType::Word])?.as_arrays();
            consume_tokens(tokens, &[TokenType::Colon, TokenType::Space])?;

            let (param_type, type_span) = self.parse_type(tokens, arena)?;

            if type_contains_resource(param_type) {
                return self.new_error(
                    type_span,
                    format_args!(
                        "The argument '{}' can't contain 'resource' in its type",
                        arg_name
                    ),
                );
            }
            if type_contains_entity(param_type) {
                return self.new_error(
                    type_span,
                    format_args!(
                        "The argument '{}' can't contain 'entity' in its type",
                        arg_name
                    ),
                );
            }
            arguments.push(Parameter {
				name: arg_name,
                ty: param_type,
                name_span: *name_token_span,
                type_span,
            });

            if consume_tokens(tokens, &[TokenType::Comma]).is_err() {
                break;
            }

            consume_space(tokens)?;
        }
        Ok(arguments.leak())
    }

    fn parse_statements(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        parsing_depth: usize,
        indentation: usize,
        arena: &'a Arena,
    ) -> Result<&'a mut [Statement<'a>], ParserError<'a>> {
        assert_parsing_depth(parsing_depth)?;
        let (_, _, &[_, _, mut last_new_line_span]) = consume_tokens(
            tokens,
            &[TokenType::Space, TokenType::OpenBrace, TokenType::NewLine],
        )?.as_arrays();

        let mut newline_allowed = false;
        let mut newline_seen = false;

        let mut statements = Vec::new_in(arena);

        while !is_end_of_block(tokens, indentation)? {
            // newlines
            if let Ok(tokens) =
                consume_tokens(tokens, &[TokenType::Indentation, TokenType::NewLine])
            {
				let [indentation_token_span, _] = tokens.spans;
                return self.new_error(
                    *indentation_token_span,
                    format_args!("Empty line cannot have indentation"),
                );
            } else if let Ok(token) = consume_tokens(tokens, &[TokenType::NewLine]) {
				[last_new_line_span] = *token.spans;
                if !newline_allowed {
                    return self.new_error(last_new_line_span, format_args!("Unexpected empty line"));
                }
                // cannot have consecutive newlines
                newline_allowed = false;
                newline_seen = true;

                statements.push(Statement::EmptyLine);
            } else {
                newline_allowed = true;
                newline_seen = false;
                consume_indentation(tokens, indentation)?;

                statements.push(self.parse_statement(
                    tokens,
                    parsing_depth + 1,
                    indentation,
                    arena,
                )?);
                consume_tokens(tokens, &[TokenType::NewLine])?;
            }
        }

        if !newline_allowed && newline_seen {
            // a newline has been seen so the line number will be incremented by one
            // but we want the line number of the previous line
            return self.new_error(last_new_line_span, format_args!("Unexpected empty line"));
        }

        if indentation != 1 {
            consume_indentation(tokens, indentation - 1)?;
        }
        consume_tokens(tokens, &[TokenType::CloseBrace])?;

        Ok(statements.leak())
    }

    // stmt -> variable_stmt | if_stmt | return_stmt | while_stmt | ;
    fn parse_statement(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        parsing_depth: usize,
        indentation: usize,
        arena: &'a Arena,
    ) -> Result<Statement<'a>, ParserError<'a>> {
        let (tys, values, spans) = peek_tokens::<2>(tokens)?.as_arrays();
        match tys[0] {
            TokenType::Word => match tys[1] {
                TokenType::OpenParenthesis | TokenType::Dot => Ok(Statement::Call(
                    self.parse_expression(tokens, parsing_depth + 1, 0., arena)?,
                )),
                TokenType::Colon | TokenType::Space => {
                    self.parse_local_variable(tokens, parsing_depth + 1, arena)
                }
                _ => self.new_error(
					spans[1],
                    format_args!(
                        "Expected '(', or ':', or ' =' after the word '{}' on line {}",
                        values[0], spans[0].line
                    ),
                ),
            },
            TokenType::If => {
                // if condition and block
                let mut ifs = Vec::new();
                loop {
                    consume_tokens(tokens, &[TokenType::If, TokenType::Space])?;

                    let condition = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
                    let if_block =
                        self.parse_statements(tokens, parsing_depth + 1, indentation + 1, arena)?;

                    // else block

                    let is_chained;
                    let else_block;

                    if consume_tokens(tokens, &[TokenType::Space, TokenType::Else])
                        .is_ok()
                    {
                        let (tys, _, _) = peek_tokens(tokens)?.as_arrays();
						if let &[TokenType::Space, TokenType::If] = tys {
                            is_chained = true;
                            consume_tokens(tokens, &[TokenType::Space]).unwrap();
                            ifs.push((
                                condition,
                                is_chained,
                                if_block,
                                &mut [] as &mut [Statement],
                            ));
                            continue;
                        } else {
                            is_chained = false;
                            else_block = self.parse_statements(
                                tokens,
                                parsing_depth,
                                indentation + 1,
                                arena,
                            )?;
                        }
                    } else {
                        is_chained = false;
                        else_block = &mut [];
                    }
                    ifs.push((condition, is_chained, if_block, else_block));
                    break;
                }
                let mut current = ifs
                    .pop()
                    .expect("We have parsed at least a single if statement");
                for statement in ifs.into_iter().rev() {
                    let else_block = std::slice::from_mut(arena.alloc_into(
                        Statement::If {
                            condition: current.0,
                            is_chained: current.1,
                            if_block: current.2,
                            else_block: current.3,
                        },
                    ));
                    current = statement;
                    current.3 = else_block;
                }
                Ok(Statement::If {
                    condition: current.0,
                    is_chained: current.1,
                    if_block: current.2,
                    else_block: current.3,
                })
            }
            TokenType::Return => {
				get_tokens::<1>(tokens)?;
                let expr = if let TokenType::NewLine = tys[1] {
                    None
                } else {
                    consume_space(tokens)?;
                    Some(arena.alloc_into(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?))
                };
                Ok(Statement::Return {
                    return_span: spans[0],
                    expr
                })
            }
            TokenType::While => {
                assert_parsing_depth(parsing_depth)?;
                consume_tokens(tokens, &[TokenType::While, TokenType::Space])?;

                let condition = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
                let block =
                    self.parse_statements(tokens, parsing_depth + 1, indentation + 1, arena)?;

                Ok(Statement::While { condition, block })
            }
            TokenType::Break => {
				get_tokens::<1>(tokens)?;
                Ok(Statement::Break(spans[0]))
            }
            TokenType::Continue => {
				get_tokens::<1>(tokens)?;
                Ok(Statement::Continue(spans[0]))
            }
            TokenType::Comment => {
				get_tokens::<1>(tokens)?;
                Ok(Statement::Comment {
                    comment_span: spans[0],
					value: values[0],
                })
            }
            got_token => self.new_error(
                spans[0],
                format_args!(
                    "Expected a statement token, but got {} on line {}",
                    got_token, spans[0].line
                ),
            ),
        }
    }

    // local_variable -> word + (":" + type)? + "=" + " " + expr
    fn parse_local_variable(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        parsing_depth: usize,
        arena: &'a Arena,
    ) -> Result<Statement<'a>, ParserError<'a>> {
        assert_parsing_depth(parsing_depth)?;
		let ([_], [local_name], [name_token_span]) = consume_tokens(tokens, &[TokenType::Word])?.as_arrays();

        let (ty, type_span) = if consume_tokens(tokens, &[TokenType::Colon]).is_ok() {
            if *local_name == "me" {
                return self.new_error(
                    *name_token_span,
                    format_args!("variable cannot be named 'me'"),
                );
            }
            consume_space(tokens)?;
            let (ty, type_span) = self.parse_type(tokens, arena)?;

            if type_contains_resource(ty) {
                return self.new_error(
                    type_span,
                    format_args!(
                        "The variable '{}' can't contain 'resource' in its type",
                        local_name
                    ),
                );
            }
            if type_contains_entity(ty) {
                return self.new_error(
                    type_span,
                    format_args!(
                        "The variable '{}' can't contain 'entity' in its type",
                        local_name
                    ),
                );
            }
            (Some(&*arena.alloc_into(ty)), type_span)
        } else {
            (None, *name_token_span)
        };
        // TODO: This error should just be folded into ExpectedSpace but it has
        // to be different to match the required error message
		match consume_tokens(tokens, &[TokenType::Space]) {
			Ok(_) => (),
			Err(ParserError::GotWrongToken { got, .. }) => {
				return self.new_error(
					got.span,
					format_args!(
						"Variable '{}' was not assigned a value",
						local_name
					),
				);
			}
			Err(err) => return Err(err)
		};

        if *local_name == "me" {
            return self.new_error(
                *name_token_span,
                // TODO: "Cannot assign to 'me'"
                format_args!("Assigning a new value to the entity's 'me' variable is not allowed"),
            );
        }

        consume_tokens(tokens, &[TokenType::Equal])?;

        consume_space(tokens)?;
        let assignment_expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
        Ok(Statement::Variable {
			name: local_name,
            ty,
            type_span,
            assignment_expr,
            name_span: *name_token_span,
        })
    }

    fn parse_expression(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        parsing_depth: usize,
        min_precedence: f32,
        arena: &'a Arena,
    ) -> Result<Expr<'a>, ParserError<'a>> {
        assert_parsing_depth(parsing_depth)?;
        let mut current: Expr = {
            let ([ty], [value], [span]) = get_tokens(tokens)?.as_arrays();
            match ty {
                TokenType::OpenParenthesis => {
                    let expr = self.parse_expression(tokens, parsing_depth + 1, 0., arena)?;
                    let _ = &consume_tokens(tokens, &[TokenType::CloseParenthesis])?;

                    Expr {
                        data: ExprData::Parenthesized(arena.alloc_into(expr)),
                        result_type: None,
                        span: *span,
                    }
                }
                TokenType::True => Expr {
                    data: ExprData::True,
                    result_type: None,
                    span: *span,
                },
                TokenType::False => Expr {
                    data: ExprData::False,
                    result_type: None,
                    span: *span,
                },
                TokenType::String => Expr {
                    data: ExprData::String(value),
                    result_type: None,
                    span: *span,
                },
                TokenType::Resource => Expr {
                    data: ExprData::Resource(value),
                    result_type: None,
                    span: *span,
                },
                TokenType::Entity => Expr {
                    data: ExprData::Entity(value),
                    result_type: None,
                    span: *span,
                },
                TokenType::Word => {
                    // a word token can actually be a function call
                    if let Ok(_) = consume_tokens(tokens, &[TokenType::OpenParenthesis])
                    {
                        if value.starts_with("_")
                            && !self.called_local_functions.contains(value)
                        {
                            self.called_local_functions.push(value);
                        }

                        // immediate ")" | (expr + ("," + " " + expr)*) + ")"

                        if let Ok(_) =
                            consume_tokens(tokens, &[TokenType::CloseParenthesis])
                        {
                            Expr {
                                data: ExprData::Call {
                                    receiver: None,
                                    name: value,
                                    args: Vec::new().leak(),
                                    ptr: None,
                                    name_span: *span,
                                    generics: &[],
                                },
                                result_type: None,
                                span: *span,
                            }
                        } else {
                            let mut arguments = Vec::new_in(arena);
                            loop {
                                arguments.push(self.parse_expression(
                                    tokens,
                                    parsing_depth + 1,
                                    0.,
                                    arena,
                                )?);
                                if let Ok(_) = consume_tokens(
                                    tokens,
                                    &[TokenType::Comma, TokenType::Space],
                                ) {
                                } else {
                                    let _ = consume_tokens(
                                        tokens,
                                        &[TokenType::CloseParenthesis],
                                    )?;
                                    break Expr {
                                        data: ExprData::Call {
                                            receiver: None,
                                            name: value,
                                            args: arguments.leak(),
                                            ptr: None,
                                            name_span: *span,
                                            generics: &[],
                                        },
                                        result_type: None,
                                        span: *span,
                                    };
                                }
                            }
                        }
                    } else {
                        Expr {
                            data: ExprData::Identifier(value),
                            result_type: None,
                            span: *span,
                        }
                    }
                }
                TokenType::Int32 => Expr {
                    data: ExprData::Number(
                        value.parse::<i64>().unwrap_or(f64::MAX as i64) as f64,
						value,
                    ),
                    result_type: None,
                    span: *span,
                },
                TokenType::Float32 => {
                    let number = value.parse::<f64>().unwrap();
                    if number > f64::MAX {
                        return self
                            .new_error(*span, format_args!("The number {} is too big", value));
                    } else if (number != 0. && number < f64::MIN_POSITIVE)
                        || (number == 0.
                            && value.contains(['1', '2', '3', '4', '5', '6', '7', '8', '9']))
                    {
                        return self.new_error(
                            *span,
                            format_args!("The number {} is too close to zero", value),
                        );
                    }

                    Expr {
                        data: ExprData::Number(
                            number,
							value,
                        ),
                        result_type: None,
                        span: *span,
                    }
                }
                TokenType::Minus | TokenType::Not => {
                    let unary_op = match ty {
                        TokenType::Minus => UnaryOperator::Minus,
                        TokenType::Not => {
                            consume_space(tokens)?;
                            UnaryOperator::Not
                        }
                        _ => unreachable!(), // grcov-excl-line
                    };
                    let ((), r_bp) = Self::get_prefix_precedence(unary_op);
                    let expr = self.parse_expression(tokens, parsing_depth + 1, r_bp, arena)?;
                    Expr {
                        result_type: None,
                        data: ExprData::Unary {
                            op: unary_op,
                            expr: arena.alloc_into(expr),
                            op_span: *span,
                        },
                        span: *span,
                    }
                }
                _ => {
                    return self.new_error(
                        *span,
                        format_args!("Expected a primary expression token but got {}", ty),
                    );
                }
            }
        };
        while let Ok(toks) = peek_tokens(tokens) {
            // Actually a method call
            // TODO: Consume the dot and word token separately
            // TODO: add a test to verify that a call can be a receiver
            // `_return_vec().push(25)`
			if let [TokenType::Dot, TokenType::Word] = toks.tys {
				let (_, [_, name], [_, name_span]) = toks.as_arrays();
				get_tokens::<2>(tokens)?;
                let receiver = current;
                // a word token can actually be a function call
				match consume_tokens(tokens, &[TokenType::OpenParenthesis]) {
					Ok(_) => {
						// immediate ")" | (expr + ("," + " " + expr)*) + ")"
						current = if let Ok(_) = consume_tokens(tokens, &[TokenType::CloseParenthesis]) {
							Expr {
								span: receiver.span,
								data: ExprData::Call {
									receiver: Some(arena.alloc_into(receiver)),
									name, 
									args: Vec::new().leak(),
									ptr: None,
									name_span: *name_span,
									generics: &[],
								},
								result_type: None,
							}
						} else {
							let mut arguments = Vec::new_in(arena);
							loop {
								arguments.push(self.parse_expression(tokens, parsing_depth + 1, 0., arena)?);
								if let Ok(_) = consume_tokens(tokens, &[TokenType::Comma, TokenType::Space]) {
									// empty 
								} else {
									consume_tokens(tokens, &[TokenType::CloseParenthesis])?;
									break;
								}
							}
							Expr {
								span: receiver.span,
								data: ExprData::Call {
									receiver: Some(arena.alloc_into(receiver)),
									name, 
									args: arguments.leak(),
									ptr: None,
									name_span: *name_span,
									generics: &[],
								},
								result_type: None,
							}
						};
					} 
					Err(ParserError::GotWrongToken { got, .. }) => {
						// Reserved for struct field accesses
						return self.new_error(got.span, format_args!("Method call expected '('"));
					}
					Err(err) => return Err(err),
                }
            } else if let [TokenType::Space, _] = toks.tys {
				let bin_op = match toks.tys[1] {
					TokenType::Or => BinaryOperator::Or,
					TokenType::And => BinaryOperator::And,
					TokenType::DoubleEquals => BinaryOperator::DoubleEquals,
					TokenType::NotEquals => BinaryOperator::NotEquals,
					TokenType::Greater => BinaryOperator::Greater,
					TokenType::GreaterEquals => BinaryOperator::GreaterEquals,
					TokenType::Less => BinaryOperator::Less,
					TokenType::LessEquals => BinaryOperator::LessEquals,
					TokenType::Plus => BinaryOperator::Plus,
					TokenType::Minus => BinaryOperator::Minus,
					TokenType::Star => BinaryOperator::Multiply,
					TokenType::ForwardSlash => BinaryOperator::Division,
					_ => break,
				};
				let (l_bp, r_bp) = Self::get_infix_precedence(bin_op);
				if l_bp < min_precedence {
					break;
				}
				// consume space and op
				_ = get_tokens::<2>(tokens)?;
				consume_space(tokens)?;
				let next = self.parse_expression(tokens, parsing_depth + 1, r_bp, arena)?;

				current = Expr {
					span: current.span,
					result_type: None,
					data: ExprData::Binary {
						op: bin_op,
						left: arena.alloc_into(current),
						right: arena.alloc_into(next),
						op_span: toks.spans[1],
					},
				};
			} else {
				break;
			}
        }
        Ok(current)
    }

    fn get_prefix_precedence(op: UnaryOperator) -> ((), f32) {
        match op {
            UnaryOperator::Minus => ((), 7.0),
            UnaryOperator::Not => ((), 8.0),
        }
    }

    fn get_infix_precedence(op: BinaryOperator) -> (f32, f32) {
        match op {
            BinaryOperator::Or => (1.0, 1.1),
            BinaryOperator::And => (2.0, 2.1),
            BinaryOperator::DoubleEquals => (3.0, 3.1),
            BinaryOperator::NotEquals => (3.0, 3.1),
            BinaryOperator::Greater => (4.0, 4.1),
            BinaryOperator::GreaterEquals => (4.0, 4.1),
            BinaryOperator::Less => (4.0, 4.1),
            BinaryOperator::LessEquals => (4.0, 4.1),
            BinaryOperator::Plus => (5.0, 5.1),
            BinaryOperator::Minus => (5.0, 5.1),
            BinaryOperator::Multiply => (6.0, 6.1),
            BinaryOperator::Division => (6.0, 6.1),
        }
    }

    fn parse_type(
        &mut self,
        tokens: &mut TokensSlice<'a>,
        arena: &'a Arena,
    ) -> Result<(Type<'a>, SourceSpan), ParserError<'a>> {
        let (_, [value], [span]) = consume_tokens(tokens, &[TokenType::Word])?.as_arrays();
        Ok((
            match *value {
				// grcov-excl-start: VARIABLE_INFERENCE is off
				"_" if VARIABLE_INFERENCE => Type::Existential {
					// TODO: Is this okay, should we create a unique idx for
					// each inferred variable in a function?
					idx: 0,
				},
				// grcov-excl-stop
				"_" => {
					return self.new_error(
						*span,
						format_args!("`_` is not a valid type name"),
					);
				}
                "bool" => Type::Bool,
                "number" => Type::Number,
                "string" => Type::String,
                "resource" => Type::Resource {
                    extension: "",
                    optional: false,
                },
                "entity" => Type::Entity { entity_type: None },
                type_name => {
                    let generics = if let Ok(_) =
                        consume_tokens(tokens, &[TokenType::OpenBracket])
                    {
                        let mut generics = Vec::new_in(arena);
                        generics.push(self.parse_type(tokens, arena)?.0);

                        while let Ok(_) = consume_tokens(tokens, &[TokenType::Comma]) {
                            consume_space(tokens)?;
                            generics.push(self.parse_type(tokens, arena)?.0);
                        }
                        consume_tokens(tokens, &[TokenType::CloseBracket])?;
                        &*generics.leak()
                    } else {
                        &[]
                    };
                    Type::Id {
                        name: type_name,
                        generics,
                    }
                }
            },
            *span,
        ))
    }
}

fn type_contains_resource(ty: Type) -> bool {
    match ty {
        Type::Resource { .. } => true,
        Type::Id { generics, name: _ } => generics.iter().copied().any(type_contains_resource),
        _ => false,
    }
}

fn type_contains_entity(ty: Type) -> bool {
    match ty {
        Type::Entity { .. } => true,
        Type::Id { generics, name: _ } => generics.iter().copied().any(type_contains_entity),
        _ => false,
    }
}

fn is_end_of_block<'a>(
    tokens: &mut TokensSlice<'a>,
    indentation: usize,
) -> Result<bool, ParserError<'a>> {
    use super::SPACES_PER_INDENT;

    assert!(indentation != 0);
    let ([ty], [value], [span]) = peek_tokens(tokens)?.as_arrays();
    match ty {
        TokenType::CloseBrace => Ok(true),
        TokenType::NewLine => Ok(false),
        TokenType::Indentation => {
            Ok(value.len() == (indentation - 1) * SPACES_PER_INDENT)
        }
        _ => Err(ParserError::ExpectedIndentation { got: Token{ty: *ty, value: *value, span: *span }}),
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
#[inline]
fn assert_tokens<'a, const N: usize>(
    tokens: &TokensSlice<'a>,
    expected: &[TokenType; N],
) -> Result<TokensArray<'a, N>, ParserError<'a>> {
	let Some(tokens) = tokens.as_array() else {
        return Err(ParserError::UnexpectedEof {
            expected: expected[tokens.len()],
        });
	};
    for (i, (got, expected)) in tokens.tys.iter().zip(expected).enumerate() {
        if got != expected {
            return Err(ParserError::GotWrongToken {
                expected: *expected,
                got: Token {
					ty: *got,
					value: tokens.values[i],
					span: tokens.spans[i],
				},
            });
        }
    }
	Ok(tokens)
}

// consumes the next few tokens if they match the given types, otherwise leaves the input unchanged
#[track_caller]
#[inline]
fn consume_tokens<'a, const N: usize>(
    tokens: &mut TokensSlice<'a>,
    expected: &'_ [TokenType; N],
) -> Result<TokensArray<'a, N>, ParserError<'a>> {
    _ = assert_tokens::<N>(tokens, expected)?;
	Ok(tokens.take_array().unwrap())
}

#[track_caller]
fn get_tokens<'a, const N: usize>(
    tokens: &mut TokensSlice<'a>
) -> Result<TokensArray<'a, N>, ParserError<'static>> {
	tokens.take_array().ok_or(ParserError::OutOfTokensError)
}

#[track_caller]
fn peek_tokens<'a, const N: usize>(
    tokens: &TokensSlice<'a>
) -> Result<TokensArray<'a, N>, ParserError<'static>> {
	tokens.as_array().ok_or(ParserError::OutOfTokensError)
}

#[track_caller]
#[inline]
fn consume_space<'a>(
    tokens: &mut TokensSlice<'a>
) -> Result<TokensArray<'a, 1>, ParserError<'a>> {
    match consume_tokens(tokens, &[TokenType::Space]) {
		Err(ParserError::GotWrongToken { got, .. }) => Err(ParserError::ExpectedSpace { got }),
		Err(err) => Err(err),
		Ok(tok) => Ok(tok)
	}
}

#[track_caller]
fn consume_indentation<'a>(
    tokens: &mut TokensSlice<'a>,
    indentation: usize,
) -> Result<TokensArray<'a, 1>, ParserError<'a>> {
    use super::SPACES_PER_INDENT;

    match consume_tokens(tokens, &[TokenType::Indentation]) {
		Err(err) => Err(err),
		Ok(TokensArray {tys: [ty], values: [value], spans: [span]}) if value.len() != indentation * SPACES_PER_INDENT => {
			return Err(ParserError::IndentationMismatch {
				expected_spaces: indentation * SPACES_PER_INDENT,
				token: Token { ty: *ty, value: *value, span: *span },
			});
		}
		Ok(tok) => Ok(tok)
	}
}
