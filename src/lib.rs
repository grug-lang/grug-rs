// #![allow(warnings)]

mod frontend {
	const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;

	#[derive(Debug)]
	enum GrugType {
		Void,
		Bool,
		I32,
		F32,
		String,
		Id,
		Resource,
		Entity,
	}

	pub fn compile_grug_file<'a>(path: &'a str, mod_name: &'a str) -> Result<(), GrugError<'a>> {
		if !path.contains('/') {
			return Err(GrugError::FileNameError(FileNameError::FilePathDoesNotContainForwardSlash{path}))
		}
		let entity_type = get_entity_type(path)?;

		let file_text = std::fs::read_to_string(&path).unwrap();

		let tokens = tokenizer::tokenize(&file_text)?;

		let ast = parser::parse(&*tokens)?;

		return Ok(());
	}

	fn get_entity_type(path: &str) -> Result<&str, FileNameError> {
		let (_, entity_type) = path.rsplit_once("-").ok_or(
				FileNameError::EntityMissing{path}
			)?;
		let (entity_type, _) = entity_type.rsplit_once(".").ok_or(
				FileNameError::MissingPeriodInFileName{path}
			)?;
		if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
			return Err(FileNameError::EntityLenExceedsMaxLen{path, entity_len: entity_type.len()});
		}
		if entity_type.len() == 0 {
			return Err(FileNameError::EntityMissing{path});
		}
		check_custom_id_is_pascal(entity_type)
	}

	fn check_custom_id_is_pascal(entity_type: &str) -> Result<&str, FileNameError> {
		let mut chars = entity_type.chars();
		let Some(first) = chars.next() else {
			return Err(FileNameError::EntityNotPascalCase1{entity_type});
		};
		for ch in chars {
			if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_digit(10)) {
                return Err(FileNameError::EntityNotPascalCase2{entity_type, wrong_char: ch});
			}
		}
		Ok(entity_type)
	}

	#[derive(Debug)]
	pub enum FileNameError<'a> {
		FilePathDoesNotContainForwardSlash{
			path: &'a str
		},
		MissingPeriodInFileName {
			path: &'a str
		},
		EntityLenExceedsMaxLen {
			path: &'a str,
			entity_len: usize,
		},
		EntityMissing {
			path: &'a str
		},
		EntityNotPascalCase1 {
			entity_type: &'a str,
		},
		EntityNotPascalCase2 {
			entity_type: &'a str,
			wrong_char: char,
		}
	}

	impl<'a> std::fmt::Display for FileNameError<'a> {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::FilePathDoesNotContainForwardSlash{
					path
				} => write!(f, "The grug file path {}, does not contain a '/' character", path),
				Self::MissingPeriodInFileName {
					path
				} => write!(f, "'{}' is missing a period in its filename", path),
				Self::EntityLenExceedsMaxLen {
					path,
					entity_len,
				} => write!(f, 
					"There are more than {MAX_FILE_ENTITY_TYPE_LENGTH} characters \n\
                	in the entity type of '{path}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH"
				),
				Self::EntityMissing {
					path
				} => write!(f, 
					"'{}' is missing an entity type in its name;\n\
					use a dash to specify it, like 'ak47-gun.grug'",
					path
				),
				Self::EntityNotPascalCase1 {
					entity_type,
				} => write!(f, "'{entity_type}' seems like a custom ID type, but isn't in PascalCase"),
				Self::EntityNotPascalCase2 {
					entity_type,
					wrong_char,
				} => write!(f,
					"'{entity_type}' seems like a custom ID type, but it contains '{wrong_char}', \n\
                    which isn't uppercase/lowercase/a digit"
				),
			}
		}
	}

	#[derive(Debug)]
	pub enum GrugError<'a> {
		FileNameError(FileNameError<'a>),
		TokenizerError(TokenizerError),
		ParserError(ParserError),
	}

	impl<'a> From<FileNameError<'a>> for GrugError<'a> {
		fn from (from: FileNameError<'a>) -> Self {
			Self::FileNameError(from)
		}
	}

	impl<'a> From<TokenizerError> for GrugError<'a> {
		fn from (from: TokenizerError) -> Self {
			Self::TokenizerError(from)
		}
	}

	impl<'a> From<ParserError> for GrugError<'a> {
		fn from (from: ParserError) -> Self {
			Self::ParserError(from)
		}
	}

	impl<'a> std::fmt::Display for GrugError<'a> {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::FileNameError(error) => write!(f, "{}", error),
				err => write!(f, "{:?}", err),
			}
		}
	}

	mod tokenizer {
		#[derive(Debug)]
		pub struct Token<'a> {
			pub(super) ty: TokenType,
			pub(super) value: &'a str,
			pub(super) line: usize,
			pub(super) col: usize,
		}

		#[derive(Debug, PartialEq, Clone, Copy)]
		pub enum TokenType {
			OpenParenthesis,
			CloseParenthesis,
			OpenBrace,
			CloseBrace,
			Plus,
			Minus,
			Star,
			ForwardSlash,
			Percent,
			Comma,
			Colon,
			NewLine,
			DoubleEquals,
			NotEquals,
			Equal,
			GreaterEquals,
			Greater,
			LessEquals,
			Less,
			And,
			Or,
			Not,
			True,
			False,
			If,
			Else,
			While,
			Break,
			Return,
			Continue,
			Space,
			Indentation,
			String,
			Word,
			Int32,
			Float32,
			Comment,
		}

		#[derive(Debug)]
		pub enum TokenizerError{
			SpacesPerIndentError {
				actual_spaces: usize,
				spaces_per_indent: usize,
				line_num: usize,
				col_num: usize,
			},
			UnclosedString {
				start_line: usize,
				start_col: usize,
			},
			MultiplePeriodsInNumber {
				line: usize,
				col: usize,
			},
			FloatTrailingPeriod {
				line: usize,
				col: usize,
			},
			NoSpaceAfterComment {
				line: usize,
				col: usize,
			},
			UnrecognizedCharacter {
				ch: char,
				line: usize,
				col: usize,
			}
		}

		const SPACES_PER_INDENT: usize = 4;

		pub fn tokenize(file_text: &'_ str) -> Result<Vec<Token>, TokenizerError> {
			let mut tokens = Vec::new();
			let mut cur_line = 1;
			let mut last_new_line = 0;

			let file_text = file_text.as_bytes();
			let mut i = 0;

			while i < file_text.len() {
				macro_rules! token_match {
					($tag: literal => $expr: expr$(, $extra_expr: expr)?) => {
						let lit_len = $tag.len();
						if i + lit_len <= file_text.len() && &file_text[i..(i+lit_len)] == &*$tag {
							// SAFETY: string is guaranteed to be utf8 because it tests equal to tag which is utf8 despite being a byte array
							tokens.push(Token{
								ty: $expr, 
								value: unsafe{str::from_utf8_unchecked(&file_text[i..(i+lit_len)])},
								line: cur_line,
								col: i - last_new_line,
							});
							i += lit_len;
							$($extra_expr;)?
							continue;
						}
					}
				}
				macro_rules! token_match_word {
					($tag: literal => $expr: expr$(, $extra_expr: expr)?) => {
						let lit_len = $tag.len();
						if i + lit_len <= file_text.len() && &file_text[i..(i+lit_len)] == &*$tag && (i + lit_len == file_text.len() || !is_word_char(file_text[i+lit_len] as char)) {
							// SAFETY: string is guaranteed to be utf8 because it tests equal to tag which is utf8 despite being a byte array
							tokens.push(Token{
								ty: $expr, 
								value: unsafe{str::from_utf8_unchecked(&file_text[i..(i+lit_len)])},
								line: cur_line,
								col: i - last_new_line,
							});
							i += lit_len;
							$($extra_expr;)?
							continue;
						}
					}
				}
				token_match!(b"(" => TokenType::OpenParenthesis);
				token_match!(b")" => TokenType::CloseParenthesis);
				token_match!(b"{" => TokenType::OpenBrace);
				token_match!(b"}" => TokenType::CloseBrace);
				token_match!(b"+" => TokenType::Plus);
				token_match!(b"-" => TokenType::Minus);
				token_match!(b"*" => TokenType::Star);
				token_match!(b"/" => TokenType::ForwardSlash);
				token_match!(b"%" => TokenType::Percent);
				token_match!(b"," => TokenType::Comma);
				token_match!(b":" => TokenType::Colon);
				token_match!(b"\n" => TokenType::NewLine, {cur_line += 1; last_new_line = i});
				token_match!(b"\r\n" => TokenType::NewLine, {cur_line += 1; last_new_line = i});
				token_match!(b"==" => TokenType::DoubleEquals);
				token_match!(b"!=" => TokenType::NotEquals);
				token_match!(b"=" => TokenType::Equal);
				token_match!(b">=" => TokenType::GreaterEquals);
				token_match!(b">" => TokenType::Greater);
				token_match!(b"<=" => TokenType::LessEquals);
				token_match!(b"<" => TokenType::Less);
				token_match_word!(b"and" => TokenType::And);
				token_match_word!(b"or" => TokenType::Or);
				token_match_word!(b"not" => TokenType::Not);
				token_match_word!(b"true" => TokenType::True);
				token_match_word!(b"false" => TokenType::False);
				token_match_word!(b"if" => TokenType::If);
				token_match_word!(b"while" => TokenType::Else);
				token_match_word!(b"while" => TokenType::While);
				token_match_word!(b"break" => TokenType::Break);
				token_match_word!(b"return" => TokenType::Return);
				token_match_word!(b"continue" => TokenType::Continue);

				// Spaces
				let lit_len = b" ".len();
				if &file_text[i..(i+lit_len)] == &*b" " {
					let old_i = i;
					while i < file_text.len() && file_text[i] == b' ' {
						i += 1;
					}
					let num_spaces = i - old_i;
					if num_spaces == 1 {
							// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
						tokens.push(Token{
							ty: TokenType::Space, 
							value: unsafe{str::from_utf8_unchecked(&file_text[old_i..i])},
							line: cur_line,
							col: old_i - last_new_line,
						});
						continue;
					}
					if num_spaces % SPACES_PER_INDENT != 0 {
						return Err(TokenizerError::SpacesPerIndentError{
							actual_spaces: num_spaces,
							spaces_per_indent: SPACES_PER_INDENT,
							line_num: cur_line,
							col_num: old_i - last_new_line,
						});
					}

					// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
					tokens.push(Token{
						ty: TokenType::Indentation, 
						value: unsafe{str::from_utf8_unchecked(&file_text[old_i..i])},
						line: cur_line,
						col: old_i - last_new_line,
					});
					continue;
				}
					
				// TODO: Does grug allow tabs for indentation and if it does, should each tab be a separate token
				// token_match!(b"\t" => TokenType::Indentation);

				// Strings
				if file_text[i] == b'"' {
					let quote_start_index = i;
					i += 1;
					let start_index = i;
					let start_line = cur_line;
					let start_col = quote_start_index - last_new_line;

					// TODO: Handle Escaped strings
					// This requires changing Token::value to Cow<'_, str>
					while i < file_text.len() && file_text[i] != b'"' {
						if file_text[i] == b'\n' {
							cur_line += 1;
							last_new_line = i + 1;
						}
						i += 1;
					}
					if i >= file_text.len() {
						return Err(TokenizerError::UnclosedString{
							start_line,
							start_col,
						});
					}
					tokens.push(Token{
						ty: TokenType::String,
						// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
						value: unsafe{str::from_utf8_unchecked(&file_text[start_index..(i-1)])},
						line: start_line,
						col: start_col,
					});
					i += 1;
				}

				// TODO: Handle unicode strings
				// Words
				if (file_text[i] as char).is_ascii_alphabetic() || file_text[i] == b'_' {
					let start = i;
					while i < file_text.len() && (file_text[i] as char).is_ascii_alphanumeric() || file_text[i] == b'_'{
						i += 1
					}
					// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
					tokens.push(Token{
						ty: TokenType::Word, 
						value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
						line: cur_line,
						col: start - last_new_line,
					});
					continue;
				}

				// Numbers
				if (file_text[i] as char).is_ascii_digit() {
					let start = i;
					let mut seen_period = false;
					i += 1;
					while i < file_text.len() && (file_text[i] as char).is_ascii_digit() || file_text[i] == b'.' {
						if file_text[i] == b'.'{
							if seen_period {
								return Err(TokenizerError::MultiplePeriodsInNumber{
									line: cur_line,
									col: i - last_new_line,
								});
							}
							seen_period = true;
						}
						i += 1;
					}

					if seen_period {
						if file_text[i - 1] == b'.' {
							// TODO: I think floats with trailing periods
							// should be allowed but i can understand why
							// they're not
							return Err(TokenizerError::FloatTrailingPeriod {
								line: cur_line,
								col: i - 1 - last_new_line,
							});
						}
						// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
						tokens.push(Token{
							ty: TokenType::Float32, 
							value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
							line: cur_line,
							col: start - last_new_line,
						});
					}
					else {
						// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
						tokens.push(Token{
							ty: TokenType::Int32, 
							value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
							line: cur_line,
							col: start - last_new_line,
						});
					}
					continue;
				}

				// Comments
				if file_text[i] == b'#' {
					let old_i = i;
					i += 1;
					if i >= file_text.len() || file_text[i] != b' ' {
						return Err(TokenizerError::NoSpaceAfterComment{
							line: cur_line,
							col: i - last_new_line,
						});
					}
					i += 1;
					let start = i;
					while i < file_text.len() && file_text[i] != b'\r' && file_text[i] != b'\n' {
						i += 1;
					}
					// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
					tokens.push(Token{
						ty: TokenType::Comment, 
						value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
						line: cur_line,
						col: old_i - last_new_line
					});
					continue;
				}

				return Err(TokenizerError::UnrecognizedCharacter{
					ch: file_text[i] as char,
					line: cur_line,
					col: i - last_new_line,
				});
			}
			
			Ok(tokens)
		}

		fn is_word_char(ch: char) -> bool {
			ch.is_ascii_alphanumeric() || ch == '_'
		}
	}
	use tokenizer::*;

	mod parser {
		use std::collections::{HashSet, HashMap};

		mod types {
			use crate::frontend::GrugType;
			pub struct VariableStatement {
				name: String,
				ty: Option<GrugType>,
				type_name: Option<String>,
				assignment_expr: Expr,
			}

			pub struct GlobalVariableStatement {
				name: String,
				ty: GrugType,
				assigment_expr: Expr,
			}

			#[derive(Debug)]
			pub enum LiteralExpr {
				TrueExpr,
				FalseExpr,
				StringExpr{
					value: String,
				},
				ResourceExpr{
					value: String,
				},
				EntityExpr{
					value: String,
				},
				IdentifierExpr{
					name: String
				},
				I32Expr{
					value: i32,
				},
				F32Expr{
					value: f32,
				},
			}

			#[derive(Debug)]
			pub enum ExprType {
				LiteralExpr{
					expr: LiteralExpr,
					line: usize,
					col: usize,
				},
				UnaryExpr{
					operator: UnaryOperator,
					expr: Box<Expr>,
				},
				BinaryExpr{
					operands: Box<(Expr, Expr)>,
					operator: BinaryOperator,
				},
				// LogicalExpr(Box<Expr>),
				CallExpr{
					function_name: String,
					arguments: Vec<Expr>,
					line: usize,
					col: usize,
				},
				ParenthesizedExpr{
					expr: Box<Expr>,
					line: usize,
					col: usize,
				},
			}

			impl ExprType {
				fn get_last_known_location(&self) -> (usize, usize) {
					let mut current = self;
					loop {
						match current {
							Self::LiteralExpr{
								line,
								col,
								..
							} => return (*line, *col),
							Self::UnaryExpr{
								expr,
								..
							} => current = &expr.ty,
							Self::BinaryExpr{
								operands,
								..
							} => current = &operands.1.ty,
							Self::CallExpr{
								line, col,
								..
							} => return (*line, *col),
							Self::ParenthesizedExpr {
								line, col, 
								..
							} => return (*line, *col),
						}
					}
				}
			}
			
			#[derive(Debug)]
			pub enum UnaryOperator {
				Not,
				Minus,
			}

			#[derive(Debug)]
			pub enum BinaryOperator {
				Or,
				And,
				DoubleEquals,
				NotEquals,
				Greater,
				GreaterEquals,
				Less,
				LessEquals,
				Plus,
				Minus,
				Multiply,
				Division,
				Remainder,
			}
			
			#[derive(Debug)]
			pub struct Expr {
				pub(super) ty: ExprType,
				// Can be None before Type checking but MUST be Some after
				pub(super) result_ty: Option<GrugType>,
			}
		}

		pub use types::*;

		use super::tokenizer::{Token, TokenType};
		use super::GrugType;
		#[derive(Debug)]
		pub enum ParserError {
			UnexpectedToken {
				expected: TokenType,
				got: TokenType,
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			OutOfTokensError,
			GlobalAfterOnFunctions {
				token_value: String,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			ExpectedNewLine {
				#[allow(unused)]
				token_value: String,
				#[allow(unused)]
				line: usize, 
				#[allow(unused)]
				col: usize,
			},
			GlobalNamedMe {
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			ExpectedSpace {
				got: TokenType,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize, 
			},
			MissingType {
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			GlobalCantBeResource {
				name: String,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			GlobalCantBeEntity {
				name: String,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,
			},
			GlobalMissingInitializer {
				name: String,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,			
			},
			UnexpectedOpenParenthesis {
				got_ty: ExprType,
				#[allow(unused)]
				line: usize,
				#[allow(unused)]
				col: usize,			
			},
			ExceededMaxParsingDepth,
		}

		const MAX_PARSING_DEPTH: usize = 100;

		pub struct Ast;
		pub fn parse(tokens: &'_ [Token]) -> Result<Ast, ParserError> {
			// let global_statements = [];
			// let helper_fns = {};
			// let on_fns = {};
			// let statements = [];
			// let arguments = [];
			// let parsing_depth = 0;
			// let loop_depth = 0;
			// let parsing_depth = 0;
			// let indentation = 0;
			// let called_helper_fn_names = set();
			// let global_statements = [];

			let mut ast = Vec::new();

			let mut seen_on_fn = false;
			let mut seen_newline = false;
			let mut newline_allowed = false;
			let mut newline_required = false;
			let mut just_seen_global = false;

			let mut idx = 0;

			let mut tokens = tokens.iter();

			while let Some(token) = tokens.next() {
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

				    ast.push(parse_global_variable(&mut tokens)?);
				    consume_next_token_types(&mut tokens, &[TokenType::NewLine])?;

				    newline_allowed = true;
				    newline_required = true;
				    just_seen_global = true;
				}
			}

			Ok(Ast)
		}

		// global -> word + ":" + type + " =" + expr;
		fn parse_global_variable<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<VariableStatement, ParserError> {
			let name_token = &tokens.as_slice()[0];
			let global_name = name_token.value; 

			if global_name == "me" {
				return Err(ParserError::GlobalNamedMe{
					line: name_token.line,
					col: name_token.col,
				});
			}
			consume_next_token_types(tokens, &[TokenType::Colon])?;
			consume_space(tokens)?;

			let global_type = parse_type(tokens)?;
			match global_type {
				GrugType::Resource => return Err(ParserError::GlobalCantBeResource{
					name: global_name.to_string(),
					line: name_token.line,
					col: name_token.line,
				}),
				GrugType::Entity   => return Err(ParserError::GlobalCantBeEntity{
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
			
			let assignment_expr = parse_expression(tokens, 0)?;
			
			todo!();
		}

		// Recursive descent parsing adapted from the implementation in grug:
		// https://github.com/grug-lang/grug
		fn parse_expression(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			return parse_or(tokens, parsing_depth + 1);
		}

		// or -> and + " " + "||" + and

		fn parse_or(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_and(tokens, parsing_depth + 1)?;

			while let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Or]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = parse_and(tokens, parsing_depth + 1)?;
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

		fn parse_and(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_equality(tokens, parsing_depth + 1)?;

			while let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::And]) {
				let left_expr = expr;
				consume_space(tokens)?;
				let right_expr = parse_equality(tokens, parsing_depth + 1)?;
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

		fn parse_equality(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_comparison(tokens, parsing_depth + 1)?;

			loop {
				if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::DoubleEquals]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_comparison(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::DoubleEquals,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::NotEquals]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_comparison(tokens, parsing_depth + 1)?;
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

		fn parse_comparison(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_term(tokens, parsing_depth + 1)?;

			loop {
				if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Greater]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_term(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::Greater,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::GreaterEquals]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_term(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::GreaterEquals,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Less]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_term(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::Less,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::LessEquals]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_term(tokens, parsing_depth + 1)?;
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

		fn parse_term(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_factor(tokens, parsing_depth + 1)?;

			loop {
				if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Plus]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_factor(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::Plus,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Minus]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_factor(tokens, parsing_depth + 1)?;
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

		fn parse_factor(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let mut expr = parse_unary(tokens, parsing_depth + 1)?;

			loop {
				if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Star]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_unary(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::Multiply,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::ForwardSlash]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_unary(tokens, parsing_depth + 1)?;
					expr = Expr {
						ty: ExprType::BinaryExpr{
							operands: Box::new((left_expr, right_expr)),
							operator: BinaryOperator::Division,
						},
						result_ty: None,
					};
				} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Percent]) {
					let left_expr = expr;
					consume_space(tokens)?;
					let right_expr = parse_unary(tokens, parsing_depth + 1)?;
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

		fn parse_unary(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;

			if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Minus]) {
				Ok(Expr {
					ty: ExprType::UnaryExpr{
						operator: UnaryOperator::Minus,
						expr: Box::new(parse_unary(tokens, parsing_depth + 1)?),
					},
					result_ty: None,
				})
			} else if let Ok(()) = consume_next_token_types(tokens, &[TokenType::Not]) {
				consume_space(tokens)?;
				Ok(Expr {
					ty: ExprType::UnaryExpr{
						operator: UnaryOperator::Not,
						expr: Box::new(parse_unary(tokens, parsing_depth + 1)?),
					},
					result_ty: None,
				})
			} else {
				parse_call(tokens, parsing_depth + 1)
			}
		}

		fn parse_call(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
			let expr = parse_primary(tokens, parsing_depth + 1)?;

			// Not a function call
			if matches!(peek_next_token(tokens), Ok(Token{ty: TokenType::OpenParenthesis,..})) {
				return Ok(expr);
			}
			

			// functions can only be identifiers for now
			if !matches!(expr.ty, ExprType::LiteralExpr{expr: LiteralExpr::IdentifierExpr{..}, ..}) {
				let (line, col) = expr.get_last_known_location();
				return Err(ParserError::UnexpectedOpenParenthesis{
					got_ty: expr.ty,
					line, 
					col,
				})
			}
			todo!();
		}

		fn parse_primary(tokens: &mut std::slice::Iter<Token>, parsing_depth: usize) -> Result<Expr, ParserError> {
			assert_parsing_depth(parsing_depth + 1)?;
		}


		// Checks if the passed in parsing_depth is allowed
		fn assert_parsing_depth(parsing_depth: usize) -> Result<(), ParserError> {
			if parsing_depth > MAX_PARSING_DEPTH {
				Err(ParserError::ExceededMaxParsingDepth)
			} else {
				Ok(())
			}
		}
		
		fn parse_type<'a>(tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<GrugType, ParserError> {
			let next_token = get_next_token(tokens)?;
			if next_token.ty != TokenType::Word {
				return Err (ParserError::MissingType{
					line: next_token.line,
					col: next_token.col,
				});
			}
			Ok(match next_token.value {
				"void"     => GrugType::Void,
				"bool"     => GrugType::Bool,
				"i32"      => GrugType::I32,
				"f32"      => GrugType::F32,
				"string"   => GrugType::String,
				"resource" => GrugType::Resource,
				"entity"   => GrugType::Entity,
				_       => GrugType::Id,
			})
		}

		// checks whether the next few tokens match the expected tokens without consuming the input
		fn assert_next_token_types(tokens: &std::slice::Iter<Token>, expected: &[TokenType]) -> Result<(), ParserError> {
			if tokens.len() < expected.len() {
				return Err(ParserError::OutOfTokensError);
			}
			for (Token{ty: got_ty, line, col, ..}, expected_ty) in tokens.clone().zip(expected.into_iter()) {
				if got_ty != expected_ty {
					return Err(ParserError::UnexpectedToken{
						expected: *expected_ty,
						got: *got_ty,
						line: *line,
						col: *col,
					});
				}
			}
			Ok(())
		}

		// consumes the next few tokens if they match the given types, otherwise leaves the input unchanged
		fn consume_next_token_types(tokens: &mut std::slice::Iter<Token>, expected: &[TokenType]) -> Result<(), ParserError> {
			assert_next_token_types(tokens, expected)?;
			*tokens = tokens.as_slice()[expected.len()..].into_iter();
			Ok(())
		}

		fn get_next_token<'a> (tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
			tokens.next().ok_or(ParserError::OutOfTokensError)
		}

		fn peek_next_token<'a> (tokens: &mut std::slice::Iter<'a, Token<'a>>) -> Result<&'a Token<'a>, ParserError> {
			tokens.as_slice().get(0).ok_or(ParserError::OutOfTokensError)
		}

		fn consume_space(tokens: &mut std::slice::Iter<Token>) -> Result<(), ParserError> {
			consume_next_token_types(tokens, &[TokenType::Space]).map_err(|err| match err {
				ParserError::UnexpectedToken{got, line, col, ..} => ParserError::ExpectedSpace{got, line, col},
				_ => unreachable!(),
			})
		}
	}
	use parser::*;
}

mod bindings {
	use std::ffi::{c_char, c_float, CStr, CString};
	use std::mem::ManuallyDrop;
	use std::io::Write;

	use super::frontend;

	#[repr(C)]
	#[allow(non_camel_case_types)]
	pub enum grug_type {
		grug_type_i32,
		grug_type_f32,
		grug_type_id,
	}

	#[repr(C)]
	#[allow(non_camel_case_types)]
	pub union grug_value_union {
		i32: i32,
		float: c_float,
		id: u64
	}

	#[repr(C)]
	#[allow(non_camel_case_types)]
	pub struct grug_value {
		ty: grug_type,
		value: grug_value_union
	}

	pub extern "C" fn compile_grug_file(path: *const c_char, mod_name: *const c_char) -> *const c_char {
		let path = unsafe{CStr::from_ptr(path)}.to_str().unwrap();
		let mod_name = unsafe{CStr::from_ptr(mod_name)}.to_str().unwrap();

		match frontend::compile_grug_file(path, mod_name) {
			Ok(()) => return std::ptr::null(),
			Err(err) => ManuallyDrop::new(CString::new(format!("{}", err)).unwrap()).as_ptr() as *const c_char,
		}
	}
	pub extern "C" fn init_globals_fn_dispatcher (path: *const c_char) {
		println!("init_globals_fn_dispatcher called with {}", unsafe{CStr::from_ptr(path)}.to_str().unwrap());
	}
	pub extern "C" fn on_fn_dispatcher (fn_name: *const c_char, grug_file_path: *const c_char, value: *mut grug_value, values_count: c_size_t) {
		println!(
			"on_fn_dispatcher: {} {}", 
			unsafe{CStr::from_ptr(fn_name)}.to_str().unwrap(),
			unsafe{CStr::from_ptr(grug_file_path)}.to_str().unwrap(),
		);
	}
	pub extern "C" fn dump_file_to_json (input_grug_path: *const c_char, output_json_path: *const c_char) -> bool {
		false
	}
	pub extern "C" fn generate_file_from_json (input_json_path: *const c_char, output_grug_path: *const c_char) -> bool {
		false
	}
	pub extern "C" fn game_fn_error (msg: *const c_char) {
		println!("game_fn_error called with {}", unsafe{CStr::from_ptr(msg)}.to_str().unwrap());
	}

	#[allow(non_camel_case_types)]
	pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(*const c_char, *const c_char) -> *const c_char;
	#[allow(non_camel_case_types)]
	pub type init_globals_fn_dispatcher_t = extern "C" fn (*const c_char);
	#[allow(non_camel_case_types)]
	pub type on_fn_dispatcher_t = extern "C" fn (*const c_char, *const c_char, *mut grug_value, c_size_t);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = extern "C" fn (*const c_char, *const c_char) -> bool;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = extern "C" fn (*const c_char, *const c_char) -> bool;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (*const c_char);

	#[link(name="tests", kind="dylib")]
	unsafe extern "C" {
		pub fn grug_tests_run(
			tests_dir_path_: *const c_char, 
			compile_grug_file: compile_grug_file_t,
			init_globals_fn_dispatcher_: init_globals_fn_dispatcher_t,
			on_fn_dispatcher_: on_fn_dispatcher_t,
			dump_file_to_json_: dump_file_to_json_t,
			generate_file_from_json_: generate_file_from_json_t,
			game_fn_error_: game_fn_error_t,
			whitelisted_test_: *const c_char
		);
	}
}

#[cfg(test)]
mod test {
	use super::bindings::*;

	#[test] 
	fn test () {
		let args = std::env::args().collect::<Vec<_>>();

		// if args.len() < 2 {
		// 	println!("Test Usage: cargo test -- <path/to/grug-tests/>");
		// 	panic!();
		// }
		// let grug_path = &*args[1];
		// println!("{}", grug_path);

		// let 
		
		unsafe {
			grug_tests_run(
				c"src/grug-tests/tests/".as_ptr(),
				compile_grug_file,
				init_globals_fn_dispatcher,
				on_fn_dispatcher,
				dump_file_to_json,
				generate_file_from_json,
				game_fn_error,
				std::ptr::null(),
			)
		}
	}
}
