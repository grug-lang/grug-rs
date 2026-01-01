// This is to ensure that any results that come 
// from parsing are not ignored
#![deny(unused_must_use)]
#![deny(unused_mut)]
// #![deny(warnings)]
const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
const SPACES_PER_INDENT: usize = 4;

mod types {
	use std::sync::Arc;
	// TODO Unnest some of these enums

	#[derive(Debug, Clone, PartialEq)]
	pub enum GrugType {
		Void,
		Bool,
		Number,
		String,
		Id{
			custom_name: Option<Arc<str>>,
		},
		Resource {
			extension: Arc<str>,
		},
		Entity {
			ty: Option<Arc<str>>,
		},
	}

	impl std::fmt::Display for GrugType {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::Void => write!(f, "void"),
				Self::Bool => write!(f, "bool"),
				Self::Number => write!(f, "number"),
				Self::String => write!(f, "string"),
				Self::Id{
					custom_name: None,
				} => write!(f, "id"),
				Self::Id{
					custom_name: Some(custom_name),
				} => write!(f, "{}", custom_name),
				Self::Resource {
					extension: _,
				} => write!(f, "resource"),
				Self::Entity {
					ty: Some(name),
				} => write!(f, "{}", name),
				Self::Entity {
					ty: None,
				} => write!(f, "entity"),
			}
		}
	}

	#[derive(Debug)]
	pub enum LiteralExpr {
		TrueExpr,
		FalseExpr,
		StringExpr{
			value: Arc<str>,
		},
		ResourceExpr{
			value: Arc<str>,
		},
		EntityExpr{
			value: Arc<str>,
		},
		IdentifierExpr{
			name: Arc<str>
		},
		NumberExpr {
			value: f64,
		}
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
		CallExpr{
			function_name: Arc<str>,
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
		pub fn get_last_known_location(&self) -> (usize, usize) {
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
	
	#[derive(Debug, Clone, Copy)]
	pub enum UnaryOperator {
		Not,
		Minus,
	}

	impl std::fmt::Display for UnaryOperator {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::Not => write!(f, "NOT_TOKEN"),
				Self::Minus => write!(f, "MINUS_TOKEN"),
			}
		}
	}

	#[derive(Debug, Clone, Copy)]
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

	impl std::fmt::Display for BinaryOperator {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::Or => write!(f, "OR_TOKEN"),
				Self::And => write!(f, "AND_TOKEN"),
				Self::DoubleEquals => write!(f, "EQUALS_TOKEN"),
				Self::NotEquals => write!(f, "NOT_EQUALS_TOKEN"),
				Self::Greater => write!(f, "GREATER_TOKEN"),
				Self::GreaterEquals => write!(f, "GREATER_OR_EQUAL_TOKEN"),
				Self::Less => write!(f, "LESS_TOKEN"),
				Self::LessEquals => write!(f, "LESS_OR_EQUAL_TOKEN"),
				Self::Plus => write!(f, "PLUS_TOKEN"),
				Self::Minus => write!(f, "MINUS_TOKEN"),
				Self::Multiply => write!(f, "MULTIPLICATION_TOKEN"),
				Self::Division => write!(f, "DIVISION_TOKEN"),
				Self::Remainder => write!(f, "REMAINDER_TOKEN"),
			}
		}
	}
	
	#[derive(Debug)]
	pub struct Expr {
		pub(super) ty: ExprType,
		// Can be None before Type checking but MUST be Some after
		pub(super) result_ty: Option<GrugType>,
	}

	#[derive(Debug)]
	pub enum GlobalStatement {
		GlobalVariableStatement{
			name: Arc<str>,
			ty: GrugType,
			assignment_expr: Expr,
		},
		GlobalOnFunction{
			name: Arc<str>,
			arguments: Vec<Argument>,
			body_statements: Vec<Statement>,
			calls_helper_fn: bool,
			has_while_loop: bool,
		},
		GlobalHelperFunction{
			name: Arc<str>,
			arguments: Vec<Argument>,
			body_statements: Vec<Statement>,
			calls_helper_fn: bool,
			has_while_loop: bool,
			return_ty: GrugType,
		},
		GlobalComment{
			value: Arc<str>,
		},
		GlobalEmptyLine,
	}

	#[derive(Debug, Clone)]
	pub struct Argument {
		pub(super) name: Arc<str>,
		pub(super) ty: GrugType,
	}

	// TODO: remove Statement suffix from these variants
	// TODO: Statements needs location information
	#[derive(Debug)]
	pub enum Statement {
		VariableStatement{
			name: Arc<str>,
			ty: Option<GrugType>,
			assignment_expr: Expr,
		},
		CallStatement {
			expr: Expr
		},
		IfStatement{
			condition: Expr,
			if_statements: Vec<Statement>,
			else_statements: Vec<Statement>,
		},
		ReturnStatement{
			expr: Option<Expr>,
		},
		WhileStatement{
			condition: Expr,
			statements: Vec<Statement>,
		},
		Comment{
			value: Arc<str>,
		},
		BreakStatement,
		ContinueStatement,
		EmptyLineStatement,
	}
}

pub fn compile_grug_file<'a>(path: &'a str, mod_name: &'a str) -> Result<(), GrugError<'a>> {
	if !path.contains('/') {
		return Err(GrugError::FileNameError(FileNameError::FilePathDoesNotContainForwardSlash{path}))
	}
	let entity_type = get_entity_type(path)?;

	let file_text = std::fs::read_to_string(&path).unwrap();

	let tokens = tokenizer::tokenize(&file_text)?;

	let mut ast = parser::parse(&*tokens)?;
	
	TypePropogator::new(mod_name.into()).fill_result_types(entity_type, &mut ast)?;

	return Ok(());
}

fn get_entity_type(path: &str) -> Result<&str, FileNameError<'_>> {
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

fn check_custom_id_is_pascal(entity_type: &str) -> Result<&str, FileNameError<'_>> {
	let mut chars = entity_type.chars();
	let Some(_) = chars.next() else {
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
				entity_len: _,
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
	ModApiError(ModApiError),
	TypePropogatorError(TypePropogatorError),
}

impl<'a> From<FileNameError<'a>> for GrugError<'a> {
	fn from (from: FileNameError<'a>) -> Self {
		// this extra single quote is needed to prevent a vim plugin from
		// mishandling quotes in the rest of the file
		// '
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

impl<'a> From<ModApiError> for GrugError<'a> {
	fn from(other: ModApiError) -> Self {
		Self::ModApiError(other)
	}
}

impl<'a> From<TypePropogatorError> for GrugError<'a> {
	fn from(other: TypePropogatorError) -> Self {
		Self::TypePropogatorError(other)
	}
}

impl<'a> std::fmt::Display for GrugError<'a> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::TokenizerError(error) => write!(f, "{}", error),
			Self::FileNameError(error) => write!(f, "{}", error),
			Self::ParserError(error) => write!(f, "{}", error),
			Self::TypePropogatorError(error) => write!(f, "{}", error),
			err => write!(f, "{:?}", err),
		}
	}
}

pub mod tokenizer {
	use super::SPACES_PER_INDENT;

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

	impl std::fmt::Display for TokenType {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
			match self {
				Self::OpenParenthesis => write!(f, "OPEN_PARENTHESIS_TOKEN"),
				Self::CloseParenthesis => write!(f, "CLOSE_PARENTHESIS_TOKEN"),
				Self::OpenBrace => write!(f, "OPEN_BRACE_TOKEN"),
				Self::CloseBrace => write!(f, "CLOSE_PARENTHESIS_TOKEN"),
				Self::Plus => write!(f, "PLUS_TOKEN"),
				Self::Minus => write!(f, "MINUS_TOKEN"),
				Self::Star => write!(f, "MULTIPLICATION_TOKEN"),
				Self::ForwardSlash => write!(f, "DIVISION_TOKEN"),
				Self::Percent => write!(f, "REMAINDER_TOKEN"),
				Self::Comma => write!(f, "COMMA_TOKEN"),
				Self::Colon => write!(f, "COLON_TOKEN"),
				Self::NewLine => write!(f, "NEWLINE_TOKEN"),
				Self::DoubleEquals => write!(f, "EQUALS_TOKEN"),
				Self::NotEquals => write!(f, "NOT_EQUALS_TOKEN"),
				Self::Equal => write!(f, "ASSIGNMENT_TOKEN"),
				Self::GreaterEquals => write!(f, "GREATER_OR_EQUAL_TOKEN"),
				Self::Greater => write!(f, "GREATER_TOKEN"),
				Self::LessEquals => write!(f, "LESS_OR_EQUAL_TOKEN"),
				Self::Less => write!(f, "LESS_TOKEN"),
				Self::And => write!(f, "AND_TOKEN"),
				Self::Or => write!(f, "OR_TOKEN"),
				Self::Not => write!(f, "NOT_TOKEN"),
				Self::True => write!(f, "TRUE_TOKEN"),
				Self::False => write!(f, "FALSE_TOKEN"),
				Self::If => write!(f, "IF_TOKEN"),
				Self::Else => write!(f, "ELSE_TOKEN"),
				Self::While => write!(f, "WHILE_TOKEN"),
				Self::Break => write!(f, "BREAK_TOKEN"),
				Self::Return => write!(f, "RETURN_TOKEN"),
				Self::Continue => write!(f, "CONTINUE_TOKEN"),
				Self::Space => write!(f, "SPACE_TOKEN"),
				Self::Indentation => write!(f, "INDENTATION_TOKEN"),
				Self::String => write!(f, "STRING_TOKEN"),
				Self::Word => write!(f, "WORD_TOKEN"),
				Self::Int32 => write!(f, "I32_TOKEN"),
				Self::Float32 => write!(f, "F32_TOKEN"),
				Self::Comment => write!(f, "COMMENT_TOKEN"),
				// _ => write!(f, "{:?}", self),
			}
		}
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
		// "Missing digit after decimal point in '%s'"
		FloatTrailingPeriod {
			parsed_string: String,
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

	impl std::fmt::Display for TokenizerError {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				Self::FloatTrailingPeriod {
					parsed_string,
					line: _,
					col: _,
				} => write!(f, "Missing digit after decimal point in '{}'", parsed_string),
				err => write!(f, "{:?}", err),
			}
		}
	}

	pub fn tokenize(file_text: &str) -> Result<Vec<Token<'_>>, TokenizerError> {
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
			token_match_word!(b"else" => TokenType::Else);
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
					value: unsafe{str::from_utf8_unchecked(&file_text[start_index..(i)])},
					line: start_line,
					col: start_col,
				});
				i += 1;
				continue;
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
							parsed_string: String::from(unsafe{str::from_utf8_unchecked(&file_text[start..i])}),
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

pub mod parser {
	use super::tokenizer::{Token, TokenType};
	use super::types::*;
	use super::MOD_API;
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
			got_token: String,
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
					line, 
					col, 
				} => write!(f, "The global variable 'me' has to have its name changed to something else, since grug already declares that variable"),
				Self::LocalNamedMe {
					line, 
					col, 
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
				_ => write!(f, "{:?}", self),
			}
		}
	}

	const MAX_PARSING_DEPTH: usize = 100;

	pub(super) struct AST {
		pub(super) global_statements: Vec<GlobalStatement>,
		pub(super) called_helper_fns: HashSet<Arc<str>>, 
		pub(super) helper_fn_signatures: HashMap<Arc<str>, (GrugType, Vec<Argument>)>,
	}

	pub fn parse(tokens: &'_ [Token]) -> Result<AST, ParserError> {
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
				let (helper_fn_name, return_ty, helper_fn_arguments) = if let GlobalStatement::GlobalHelperFunction{name, return_ty, arguments, ..} = &helper_fn {(Arc::clone(name), return_ty.clone(), arguments)} else {unreachable!()};
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
				
				ast.global_statements.push(GlobalStatement::GlobalEmptyLine);
			} else if let Ok(&[ref comment_token]) = consume_next_token_types(&mut tokens, &[TokenType::Comment]) {
				newline_allowed = true;

				ast.global_statements.push(GlobalStatement::GlobalComment{
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

			Ok(GlobalStatement::GlobalHelperFunction{
				name: fn_name.into(),
				arguments: args,
				body_statements,
				calls_helper_fn: false,
				has_while_loop: false,
				return_ty
			})
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

			Ok(GlobalStatement::GlobalOnFunction{
				name: fn_name.into(),
				arguments: args,
				body_statements,
				calls_helper_fn: false,
				has_while_loop: false,
			})
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
								got_token: next_tokens[0].value.to_string(),
								line: next_tokens[0].line,
								col: next_tokens[0].col,
							})
						}
					}
				}
				TokenType::If => {
					self.parse_if_statement(tokens, parsing_depth + 1, indentation)
				}
				TokenType::Return => {
					tokens.next();
					let expr = if let Ok(_) = consume_next_token_types(tokens, &[TokenType::NewLine]) {
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
		fn parse_if_statement<'a>(&mut self, tokens: &mut std::slice::Iter<'a, Token<'a>>, parsing_depth: usize, indentation: usize) -> Result<Statement, ParserError> {
			assert_parsing_depth(parsing_depth)?;
			consume_next_token_types(tokens, &[TokenType::If, TokenType::Space])?;

			let condition = self.parse_expression(tokens, parsing_depth + 1)?;
			let if_statements = self.parse_statements(tokens, parsing_depth + 1, indentation + 1)?;
			let else_statements;

			if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::Else]) {
				if let Ok(_) = consume_next_token_types(tokens, &[TokenType::Space, TokenType::If]) {
					else_statements = vec![self.parse_if_statement(tokens, parsing_depth, indentation)?];
				} else {
					else_statements = self.parse_statements(tokens, parsing_depth, indentation + 1)?;
				}
			} else {
				else_statements = Vec::new();
			}

			Ok(Statement::IfStatement{
				condition, 
				if_statements,
				else_statements,
			})
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
			
			return Ok(GlobalStatement::GlobalVariableStatement{
				name: global_name.into(),
				ty: global_type,
				assignment_expr,
			});
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
								value: (*value).into(),
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
}
use parser::*;

pub mod mod_api {
	use super::types::*;
	use std::collections::HashMap;
	use std::sync::Arc;
	use std::sync::OnceLock;

	#[derive(Debug)]
	pub struct ModApi {
		entities: HashMap<Arc<str>, ModApiEntity>,
		game_functions: HashMap<Arc<str>, ModApiGameFn>,
	}

	impl ModApi {
		pub fn entities(&self) -> &HashMap<Arc<str>, ModApiEntity> {
			&self.entities
		}
		pub fn game_functions(&self) -> &HashMap<Arc<str>, ModApiGameFn> {
			&self.game_functions
		}
	}

	#[derive(Debug)]
	pub struct ModApiEntity {
		pub(super) name: Arc<str>,
		pub(super) description: Option<String>,
		pub(super) on_fns: HashMap<Arc<str>, ModApiOnFn>,
	}
	
	#[derive(Debug)]
	pub struct ModApiOnFn {
		pub(super) name: Arc<str>, 
		pub(super) description: Option<String>,
		pub(super) arguments: Vec<Argument>,
	}

	#[derive(Debug)]
	pub struct ModApiGameFn {
		pub(super) name: Arc<str>,
		pub(super) description: Option<String>,
		pub(super) return_ty: GrugType,
		pub(super) arguments: Vec<Argument>,
	}

	// TODO: Add Display impl for all variants
	#[derive(Debug)]
	pub enum ModApiError{
		JsonError(json::Error),
		EntitiesNotObject,
		OnFunctionsNotObject{
			entity_name: String,
		},
		OnFnsArgumentsNotArray{
			entity_name: String,
			on_fn_name: String,
		},
		OnFnArgumentMissingName{
			entity_name: String,
			on_fn_name: String,
		},
		OnFnArgumentMissingType{
			entity_name: String,
			on_fn_name: String,
		},
		OnFnArgumentResource {
			entity_name: String,
			on_fn_name: String,
			argument_name: String,
		},
		OnFnArgumentEntity {
			entity_name: String,
			on_fn_name: String,
			argument_name: String,
		},
		GameFnsNotObject,
		OnFnArgumentVoid {
			entity_name: String,
			on_fn_name: String,
			argument_name: String,
		},
		GameFnArgumentsNotArray{
			game_fn_name: String,
		},
		GameFnArgumentMissingName{
			game_fn_name: String,
		},
		GameFnArgumentMissingType{
			game_fn_name: String,
			argument_name: String,
		},
		GameFnArgumentVoid{
			game_fn_name: String,
			argument_name: String,
		},
		GameFnResourceMissingExtension{
			game_fn_name: String,
			argument_name: String,
		},
		GameFnEntityMissingType{
			game_fn_name: String,
			argument_name: String,
		},
		GameFnReturnsEntity{
			game_fn_name: String,
		},
		GameFnReturnsResource{
			game_fn_name: String,
		},
	}
	
	impl From<json::Error> for ModApiError {
		fn from(other: json::Error) -> ModApiError {
			ModApiError::JsonError(other)
		}
	}

	pub fn get_mod_api(mod_api_text: &str) -> Result<ModApi, ModApiError> {
		use std::collections::HashSet;

		let mod_api_json = json::parse(mod_api_text)?;
		// "entities" object
		let entities = &mod_api_json["entities"];
		if !entities.is_object() {
			return Err(ModApiError::EntitiesNotObject);
		}

		let existing_entities = entities.entries().map(|(entity_name, _)| entity_name).collect::<HashSet<_>>();

		let entities = entities.entries().map(|(entity_name, entity_values)| {
			// optional "description" string
			let description = entity_values["description"].as_str().map(str::to_string);

			// optional "on_fns" object
			let on_fns = &entity_values["on_functions"];
			if !on_fns.is_object() && !on_fns.is_null() {
				return Err(ModApiError::OnFunctionsNotObject{
					entity_name: entity_name.to_string(),
				});
			}
			let on_fns = on_fns.entries().map(|(fn_name, fn_values)| {
				// optional "description" string
				let description = fn_values["description"].as_str().map(str::to_string);
				
				// optional "arguments" object
				let arguments = &fn_values["arguments"];
				if !arguments.is_array() && !arguments.is_null(){
					return Err(ModApiError::OnFnsArgumentsNotArray{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
					});
				}
				let arguments = arguments.members().map(|argument_values| {
					// optional "name" string
					let argument_name = argument_values["name"].as_str().ok_or(ModApiError::OnFnArgumentMissingName{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
					})?;
					// "type" string
					let ty = argument_values["type"].as_str().ok_or(ModApiError::OnFnArgumentMissingType{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
					})?;
					let ty = match ty {
						// arguments can't be void
						"void"     => Err(ModApiError::OnFnArgumentVoid{
							entity_name: entity_name.to_string(),
							on_fn_name: fn_name.to_string(),
							argument_name: argument_name.to_string(),
						})?,
						"bool"     => GrugType::Bool,
						"number"   => GrugType::Number,
						"string"   => GrugType::String,
						"id"       => GrugType::Id{custom_name: None},
						"resource" => Err(ModApiError::OnFnArgumentResource{
							entity_name: entity_name.to_string(),
							on_fn_name: fn_name.to_string(),
							argument_name: argument_name.to_string(),
						})?,
						"entity" => Err(ModApiError::OnFnArgumentEntity{
							entity_name: entity_name.to_string(),
							on_fn_name: fn_name.to_string(),
							argument_name: argument_name.to_string(),
						})?,
						type_name => {
							let extra_value = type_name.into();
							GrugType::Id {
								custom_name: Some(extra_value),
							}
						}
					};
					Ok(Argument{
						name: argument_name.into(),
						ty,
					})
				}).collect::<Result<Vec<_>, ModApiError>>()?;
				let fn_name = Arc::from(fn_name);
				Ok((Arc::clone(&fn_name), ModApiOnFn{
					name: fn_name,
					description,
					arguments
				}))
			}).collect::<Result<HashMap<_, _>, _>>()?;
			let entity_name = Arc::from(entity_name);
			Ok((Arc::clone(&entity_name), ModApiEntity{
				name: entity_name,
				description,
				on_fns
			}))
		}).collect::<Result<HashMap<_, _>, ModApiError>>()?;
		
		// "game_functions" object
		let game_functions = &mod_api_json["game_functions"];
		if !game_functions.is_object() {
			return Err(ModApiError::GameFnsNotObject);
		}

		let game_functions = game_functions.entries().map(|(fn_name, game_fn_values)| {
			// optional "description" string
			let description = game_fn_values["description"].as_str().map(str::to_string);

			// optional "arguments" object
			let arguments = &game_fn_values["arguments"];
			if !arguments.is_array() && !arguments.is_null(){
				return Err(ModApiError::GameFnArgumentsNotArray{
					game_fn_name: fn_name.to_string(),
				});
			}
			let arguments = arguments.members().map(|argument_values| {
				// "name" string
				let argument_name = argument_values["name"].as_str().ok_or(ModApiError::GameFnArgumentMissingName{
					game_fn_name: fn_name.to_string(),
				})?;
				// "type" string
				let ty = argument_values["type"].as_str().ok_or(ModApiError::GameFnArgumentMissingType{
					game_fn_name: fn_name.to_string(),
					argument_name: argument_name.to_string(),
				})?;
				let ty = match ty {
					// arguments can't be void
					"void"     => Err(ModApiError::GameFnArgumentVoid{
						game_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?,
					"bool"     => GrugType::Bool,
					"number"      => GrugType::Number,
					"string"   => GrugType::String,
					"id"       => GrugType::Id{custom_name: None},
					"entity"   => {
						let entity_type: Arc<str> = argument_values["entity_type"].as_str().ok_or(ModApiError::GameFnEntityMissingType{
							game_fn_name: fn_name.to_string(),
							argument_name: argument_name.to_string(),
						})?.into();
						GrugType::Entity {
							ty: (&*entity_type != "").then(|| entity_type)
						}
					},
					"resource" => {
						let extension = argument_values["resource_extension"].as_str().ok_or(ModApiError::GameFnResourceMissingExtension{
							game_fn_name: fn_name.to_string(),
							argument_name: argument_name.to_string(),
						})?.into();
						GrugType::Resource {
							extension
						}
					}
					type_name => {
						let extra_value = type_name.into();
						GrugType::Id {
							custom_name: Some(extra_value),
						}
					}
				};
				Ok(Argument{
					name: argument_name.into(),
					ty,
				})
			}).collect::<Result<Vec<_>, ModApiError>>()?;

			// optional "return_type" string
			let return_ty = game_fn_values["return_type"].as_str().unwrap_or("void");
			let return_ty = match return_ty {
				"void"     => GrugType::Void,
				"bool"     => GrugType::Bool,
				"number"      => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity" => Err(ModApiError::GameFnReturnsEntity{
					game_fn_name: fn_name.to_string(),
				})?,
				"resource" => Err(ModApiError::GameFnReturnsResource{
					game_fn_name: fn_name.to_string(),
				})?,
				type_name => {
					let extra_value = type_name.into();
					GrugType::Id {
						custom_name: Some(extra_value),
					}
				}
			};
			let fn_name = Arc::from(fn_name);
			Ok((Arc::clone(&fn_name), ModApiGameFn{
				name: fn_name,
				return_ty,
				description,
				arguments
			}))
		}).collect::<Result<HashMap<_, _>, ModApiError>>()?;

		Ok(ModApi{
			entities,
			game_functions
		})
	}

	pub static MOD_API: OnceLock<ModApi> = std::sync::OnceLock::new();
}
use mod_api::*;

pub mod type_propogation {
	use std::collections::HashMap;
	use std::collections::hash_map::Entry;
	use std::sync::Arc;
	use super::types::*;
	use super::parser::AST;
	use super::mod_api::MOD_API;

	type GrugIDType = *mut ();

	enum GrugValue {
		Bool(bool),
		I32(i32),
		F32(f32),
		String(String),
		Id(GrugIDType),
		Resource(String),
		Entity(GrugIDType),
		Uninitialized,
	}
	struct Variable {
		name: Arc<str>,
		ty: GrugType,
		value: GrugValue,
	}

	pub(super) struct TypePropogator {
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
		// grug_assert(entity_on_fn, "The function '%s' was not was not declared by entity '%s' in mod_api.json", on_fns[fn_index].fn_name, file_entity_type);
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
					line,
					col,
				} => write!(f, "The global variable '{}' isn't allowed to call helper functions", global_name),
				Self::VariableDoesNotExist {
					name,
					line, 
					col,
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
					left,
					right,
				} => write!(f, "The left and right operand of a binary expression ('{}') must have the same type, but got {} and {}", operator, left, right),
				Self::LogicalOperatorExpectsBool {
					operator,
				} => write!(f, "'{}' operator expects bool", operator),
				Self::ComparisonOperatorExpectsNumber {
					operator,
					got_type,
				} => write!(f, "'{}' operator expects number", operator),
				Self::ArithmeticOperatorExpectsNumber {
					got_type: GrugType::String,
					operator,
				} => write!(f, "You can't use the {} operator on a string", operator),
				Self::ArithmeticOperatorExpectsNumber {
					operator,
					got_type,
				} => write!(f, "'{}' operator expects number", operator),
				Self::RemainderOperatorExpectsNumber {
					got_ty,
				} => write!(f, "'%' operator expects number"),
				Self::CallOnFnWithinOnFn {
					on_fn_name,
				} => write!(f, "Mods aren't allowed to call their own on_ functions, but '{}' was called", on_fn_name),
				Self::FunctionDoesNotExist {
					function_name,
				} => write!(f, "The function '{}' does not exist", function_name),
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
				} => write!(f, "The function '{}' was not was not declared by entity '{}' in mod_api.json", function_name, entity_name),
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
					name,
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
			value: Arc<str>
		},
		//grug_assert(string[string_len - 1] != '/', "Remove the trailing slash from the resource \"%s\"", string);
		TrailingForwardSlash {
			value: Arc<str>
		},
		// grug_assert(!strchr(string, '\\'), "Replace the '\\' with '/' in the resource \"%s\"", string);
		ContainsBackslash {
			value: Arc<str>
		},
		// grug_assert(!strstr(string, "//"), "Replace the '//' with '/' in the resource \"%s\"", string);
		ContainsDoubleForwardSlash {
			value: Arc<str>
		},
		// TODO: This error needs a better message
		// grug_assert(string_len != 1 && string[1] != '/', "Remove the '.' from the resource \"%s\"", string);
		BeginsWithDotWithoutSlash {
			value: Arc<str>
		},
		// TODO: This error needs a better message
		// grug_assert(dot[1] != '/' && dot[1] != '\0', "Remove the '.' from the resource \"%s\"", string);
		ContainsSlashDotInMiddle {
			value: Arc<str>
		},
		// TODO: This error needs a better message
		// grug_assert(string_len != 2 && string[2] != '/', "Remove the '..' from the resource \"%s\"", string);
		BeginsWithDotDotWithoutSlash {
			value: Arc<str>
		},
		// TODO: This error needs a better message
		// grug_assert(dotdot[2] != '/' && dotdot[2] != '\0', "Remove the '..' from the resource \"%s\"", string);
		// " 
		ContainsSlashDotDotInMiddle {
			value: Arc<str>
		},
		ResourceExtensionMismatch {
			expected: Arc<str>,
			value: Arc<str>
		}
	}

	impl std::fmt::Display for ResourceValidationError {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
			match self {
				_ => write!(f, "{:?}", self)
			}
		}
	}

	#[derive(Debug)]
	pub enum EntityValidationError {
		EntityCantBeEmpty,
		// grug_assert(len > 0, "Entity '%s' is missing a mod name", string);
		EntityMissingModName {
			entity_string: Arc<str>,
		},
		// grug_assert(*entity_name != '\0', "Entity '%s' specifies the mod name '%s', but it is missing an entity name after the ':'", string, mod_name);
		EntityMissingEntityName {
			mod_name: String,
			entity_string: Arc<str>,
		},
		// grug_assert(!streq(mod_name, mod), "Entity '%s' its mod name '%s' is invalid, since the file it is in refers to its own mod; just change it to '%s'", string, mod_name, entity_name);
		ModNameIsCurrentMod {
			full_entity_string: Arc<str>,
			mod_name: String,
			entity_name: String,
		},
		// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its mod name contains the invalid character '%c'", string, c);
		ModNameHasInvalidCharacter {
			entity_name: Arc<str>, 
			invalid_char: char
		},
		// grug_assert(islower(c) || isdigit(c) || c == '_' || c == '-', "Entity '%s' its entity name contains the invalid character '%c'", string, c);
		EntityNameHasInvalidCharacter {
			entity_name: Arc<str>, 
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
				_ => write!(f, "{:?}", self),
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
	
	impl TypePropogator {
		pub fn new (mod_name: String) -> Self {
			Self {
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
			let entity = MOD_API.wait().entities().get(&*entity_name).ok_or_else(|| TypePropogatorError::EntityDoesNotExist{
				entity_name: Arc::clone(&entity_name),
			})?;
			
			self.add_global_variable(Arc::from("me"), GrugType::Id{custom_name: Some(entity_name.into())})?;

			for statement in &mut ast.global_statements {
				match statement {
					GlobalStatement::GlobalVariableStatement{
						name,
						ty,
						assignment_expr,
					} => {
						self.check_global_expr(assignment_expr, &*name)?;
						let result_ty = self.fill_expr(&ast.helper_fn_signatures, assignment_expr)?;
						
						// global expr cant call helper function and this has been checked
						debug_assert!(!self.current_fn_calls_helper_fn);

						if let ExprType::LiteralExpr{expr: LiteralExpr::IdentifierExpr{name, ..}, ..} = &assignment_expr.ty 
							&& &**name == "me" {
							// grug_assert(!streq(global->assignment_expr.literal.string, "me"), "Global variables can't be assigned 'me'");
							return Err(TypePropogatorError::GlobalCantBeAssignedMe {
								name: Arc::clone(name),
							});
						}
						if &result_ty != ty {
							return Err(TypePropogatorError::VariableTypeMismatch {
								name: Arc::clone(name),
								got_type: result_ty.clone(),
								expected_type: ty.clone(),
							});
						}
						self.add_global_variable(Arc::clone(name), result_ty)?;
					}
					GlobalStatement::GlobalOnFunction{
						name,
						arguments,
						body_statements,
						calls_helper_fn,
						has_while_loop,
					} => {
						let grug_on_fn = entity.on_fns.get(&**name).ok_or_else(|| TypePropogatorError::OnFnDoesNotExist{
							function_name: Arc::clone(name),
							entity_name: Arc::clone(&entity.name),
						})?;
						if grug_on_fn.arguments.len() > arguments.len() {
							return Err(TypePropogatorError::TooFewParameters{
								function_name: Arc::clone(name),
								expected_name: Arc::clone(&grug_on_fn.arguments[arguments.len()].name),
								expected_type: grug_on_fn.arguments[arguments.len()].ty.clone(),
							});
						} else if grug_on_fn.arguments.len() < arguments.len() {
							return Err(TypePropogatorError::TooManyParameters{
								function_name: Arc::clone(name),
								parameter_name: Arc::clone(&arguments[grug_on_fn.arguments.len()].name),
								parameter_type: arguments[grug_on_fn.arguments.len()].ty.clone(),
							});
						}
						for (param, arg) in grug_on_fn.arguments.iter().zip(arguments.iter()) {
							if param.name != arg.name {
								return Err(TypePropogatorError::OnFnParameterNameMismatch {
									function_name: Arc::clone(name),
									got_name: Arc::clone(&arg.name),
									expected_name: Arc::clone(&param.name),
								});
							}
							if param.ty != arg.ty {
								return Err(TypePropogatorError::OnFnParameterTypeMismatch {
									function_name: Arc::clone(name),
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

						self.current_fn_name = Some(Arc::clone(name));
						for arg in arguments {
							self.add_local_variable(Arc::clone(&arg.name), arg.ty.clone())?;
						}
						self.fill_statements(&ast.helper_fn_signatures, body_statements, &GrugType::Void)?;

						debug_assert!(self.current_fn_name.as_ref() == Some(name));
						self.current_fn_name = None;

						*calls_helper_fn = self.current_fn_calls_helper_fn;
						*has_while_loop = self.current_fn_has_while_loop;
						self.current_fn_calls_helper_fn = false;
						self.current_fn_has_while_loop  = false;
					}
					GlobalStatement::GlobalEmptyLine => (),
					GlobalStatement::GlobalHelperFunction {
						name,
						arguments,
						body_statements,
						calls_helper_fn,
						has_while_loop,
						return_ty,
					} => {
						debug_assert!(self.local_variables.len() == 0);
						debug_assert!(self.num_while_loops_deep == 0);
						debug_assert!(self.current_fn_calls_helper_fn == false);
						debug_assert!(self.current_fn_has_while_loop == false);
						debug_assert!(self.current_fn_name == None);

						self.current_fn_name = Some(Arc::clone(name));
						for arg in arguments {
							self.add_local_variable(Arc::clone(&arg.name), arg.ty.clone())?;
						}
						self.fill_statements(&ast.helper_fn_signatures, body_statements, return_ty)?;

						debug_assert!(self.current_fn_name.as_ref() == Some(name));
						self.current_fn_name = None;

						*calls_helper_fn = self.current_fn_calls_helper_fn;
						*has_while_loop = self.current_fn_has_while_loop;
						self.current_fn_calls_helper_fn = false;
						self.current_fn_has_while_loop  = false;
					}
					_ => todo!(),
				}
			}
			Ok(())
		}
		
		// out parameter self.current_on_fn_calls_helper_fn
		fn fill_statements(&mut self, helper_fns: &HashMap<Arc<str>, (GrugType, Vec<Argument>)>, statements: &mut [Statement], expected_return_type: &GrugType) -> Result<(), TypePropogatorError> {
			self.push_scope();
			let mut last_statement = None;
			for statement in statements {
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
							if &result_ty == ty {
								self.add_local_variable(Arc::clone(name), ty.clone())?
							} else {
								return Err(TypePropogatorError::VariableTypeMismatch {
									name: Arc::clone(name),
									got_type: result_ty.clone(),
									expected_type: ty.clone(),
								});
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

							if result_ty != var.ty {
								return Err(TypePropogatorError::VariableTypeMismatch {
									name: Arc::clone(name),
									got_type: result_ty.clone(),
									expected_type: var.ty.clone(),
								});
							}
						}
						last_statement = Some(statement);
					}
					Statement::CallStatement {
						expr
					} => {
						self.fill_expr(helper_fns, expr)?;
						last_statement = Some(statement);
					}
					Statement::IfStatement {
						condition,
						if_statements,
						else_statements,
					} => {
						let cond_type = self.fill_expr(helper_fns, condition)?;
						if cond_type != GrugType::Bool {
							return Err(TypePropogatorError::IfConditionTypeMismatch {
								got_type: cond_type
							});
						}
						self.fill_statements(helper_fns, if_statements, expected_return_type)?;
						self.fill_statements(helper_fns, else_statements, expected_return_type)?;
						// TODO: Maybe this should be looked at again
						// [https://github.com/grug-lang/grug/issues/116]
						last_statement = Some(statement);
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

						last_statement = Some(statement);
					}
					Statement::ReturnStatement {
						expr,
					} => {
						// result_ty MUST be filled
						let return_ty = expr.as_mut()
							.map(|expr| self.fill_expr(helper_fns, expr))
							.unwrap_or(Ok(GrugType::Void))?;
						if *expected_return_type != return_ty {
							return Err(TypePropogatorError::MismatchedReturnType{
								function_name: Arc::clone(&self.current_fn_name.as_ref().unwrap()),
								expected_type: expected_return_type.clone(),
								got_type: return_ty.clone(),
							})
						}
						last_statement = Some(statement);
					}
					Statement::BreakStatement => {
						if self.num_while_loops_deep == 0 {
							return Err(TypePropogatorError::BreakStatementOutsideWhileLoop);
						}
						last_statement = Some(statement);
					}
					Statement::ContinueStatement => {
						if self.num_while_loops_deep == 0 {
							return Err(TypePropogatorError::ContinueStatementOutsideWhileLoop);
						}
						last_statement = Some(statement);
					}
					_ => (),
				}
			}
			if *expected_return_type != GrugType::Void && !matches!(last_statement, Some(Statement::ReturnStatement{..})) {
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
					if let ExprType::UnaryExpr{..} = expr.ty {
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
					if result_0 != result_1 {
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
					line,
					col,
				} => {
					// TODO: Move this line to within check_arguments
					arguments.iter_mut().map(|argument| self.fill_expr(helper_fns, argument)).collect::<Result<Vec<_>, _>>()?;
					if function_name.starts_with("helper_") {
						self.current_fn_calls_helper_fn = true;
					}
					if let Some((return_ty, sig_arguments)) = helper_fns.get(function_name) {
						self.check_arguments(function_name, sig_arguments, arguments)?;
						return_ty.clone()
					} else if let Some(game_fn) = MOD_API.get().unwrap().game_functions().get(function_name) {
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
					line,
					col,
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
					&& let ExprType::LiteralExpr{expr: LiteralExpr::StringExpr{ref value}, ..} = arg.ty {
					self.validate_resource_string(value, extension)?;
					arg.result_ty = Some(GrugType::Resource{
						extension: Arc::clone(extension)
					});
				} else if let GrugType::Entity{ref ty} = param.ty 
					&& let ExprType::LiteralExpr{expr: LiteralExpr::StringExpr{ref value}, ..} = arg.ty {
					self.validate_entity_string(value)?;
					arg.result_ty = Some(GrugType::Entity{
						ty: ty.clone()
					});
				} else if arg.result_ty.as_ref().unwrap() == &GrugType::Void {
					return Err(TypePropogatorError::VoidArgumentInFunctionCall{
						function_name: Arc::clone(function_name),
						signature_type: param.ty.clone(),
						parameter_name: Arc::clone(&param.name),
					});
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

		fn validate_resource_string(&mut self, value: &Arc<str>, extension: &Arc<str>) -> Result<(), ResourceValidationError> {
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
			} else if value.starts_with(".") && !value.starts_with("./") {
				Err(ResourceValidationError::BeginsWithDotWithoutSlash {
					value: Arc::clone(value),
				})
			} else if (value.contains("/.") && !value.contains("/./")) || value.ends_with("/.") {
				Err(ResourceValidationError::ContainsSlashDotInMiddle {
					value: Arc::clone(value),
				})
			} else if value.starts_with("..") && !value.starts_with("../") {
				Err(ResourceValidationError::BeginsWithDotDotWithoutSlash {
					value: Arc::clone(value),
				})
			} else if (value.contains("/..") && !value.contains("/../")) || value.ends_with("/..") {
				Err(ResourceValidationError::ContainsSlashDotDotInMiddle {
					value: Arc::clone(value),
				})
			} else if value.ends_with(&**extension) {
				Ok(())
			} else {
				Err(ResourceValidationError::ResourceExtensionMismatch {
					expected: Arc::clone(extension),
					value: Arc::clone(value),
				})
			}
		}

		fn validate_entity_string(&mut self, entity_string: &Arc<str>) -> Result<(), EntityValidationError> {
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
				("", &**entity_string)
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
}
use type_propogation::*;

