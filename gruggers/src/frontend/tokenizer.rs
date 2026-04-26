use super::SPACES_PER_INDENT;
use allocator_api2::vec::Vec;
use crate::arena::Arena;
use gruggers_core::error::SourceSpan;
use gruggers_core::error::{ErrorKind, grug_error};

use std::ffi::OsStr;

#[derive(Debug)]
pub struct Token<'a> {
	pub(super) ty: TokenType,
	pub(super) value: &'a str,
	pub(super) span: SourceSpan,
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
	Export,
	Local,
	Space,
	Indentation,
	String,
	Entity,
	Resource,
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
			Self::CloseBrace => write!(f, "CLOSE_BRACE_TOKEN"),
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
			Self::Export => write!(f, "EXPORT_TOKEN"),
			Self::Local => write!(f, "LOCAL_TOKEN"),
			Self::Space => write!(f, "SPACE_TOKEN"),
			Self::Indentation => write!(f, "INDENTATION_TOKEN"),
			Self::String => write!(f, "STRING_TOKEN"),
			Self::Word => write!(f, "WORD_TOKEN"),
			Self::Int32 => write!(f, "NUMBER_TOKEN"),
			Self::Float32 => write!(f, "NUMBER_TOKEN"),
			Self::Comment => write!(f, "COMMENT_TOKEN"),
			Self::Resource => write!(f, "RESOURCE_TOKEN"),
			Self::Entity => write!(f, "ENTITY_TOKEN"),
		}
	}
}

pub const TOKENIZER_ERROR: u8 = 0x2;

fn tokenizer_error_kind() -> ErrorKind {
	ErrorKind::COMPILE_ERROR.add_component(TOKENIZER_ERROR)
}

#[derive(Debug)]
pub enum TokenizerError{
	BasicError(grug_error<Arena>),
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
	UnrecognizedCharacter {
		ch: char,
		line: usize,
		col: usize,
	},
	// grug_assert(!isspace(grug_text[i - 1]), "A comment has trailing whitespace on line %zu", get_character_line_number(i));
	CommentTrailingWhitespace {
		line: usize,
		col: usize,
	},
	EmptyComment {
		line: usize,
		col: usize,
	},
	LineBreakInString {
		line: usize,
		col: usize,
	},
	NullByte {
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
			Self::CommentTrailingWhitespace {
				line,
				col: _,
			} => write!(f, "A comment has trailing whitespace on line {}", line),
			Self::UnclosedString {
				start_line,
				start_col: _,
			} => write!(f, "Unclosed \" on line {}", start_line),
			Self::MultiplePeriodsInNumber {
				line,
				col: _,
			} => write!(f, "Encountered two '.' periods in a number on line {}", line),
			Self::SpacesPerIndentError {
				actual_spaces,
				spaces_per_indent,
				line_num,
				col_num: _,
			} => write!(f, "Encountered {} spaces, while indentation expects multiples of {} spaces, on line {}", actual_spaces, spaces_per_indent, line_num),
			Self::EmptyComment {
				line,
				col: _
			} => write!(f, "Expected the comment to contain some text on line {}", line),
			Self::UnrecognizedCharacter {
				ch,
				line,
				col: _
			} => write!(f, "Unrecognized character '{}' on line {}", ch, line),
			Self::LineBreakInString {
				line,
				col: _
			} => write!(f, "Unexpected line break in string on line {}", line),
			Self::NullByte {
				line,
				col: _
			} => write!(f, "Unexpected null byte on line {}", line),
			Self::BasicError(error) => write!(f, "{}", error),
		}
	}
}

pub fn tokenize<'a, 'b, P: AsRef<OsStr>>(file_text: &'b str, arena: &'a Arena, file_path: P) -> Result<Vec<Token<'b>, &'a Arena>, TokenizerError> {
	let file_path = file_path.as_ref();
	let mut tokens = Vec::new_in(arena);
	let mut cur_line = 1;
	let mut last_new_line = 0;

	let file_text = file_text.as_bytes();
	let mut i = 0;

	'outer: while i < file_text.len() {
		macro_rules! token_match {
			($tag: literal => $expr: expr$(, $extra_expr: expr)?) => {
				let lit_len = $tag.len();
				if i + lit_len <= file_text.len() && &file_text[i..(i+lit_len)] == &*$tag {
					// SAFETY: string is guaranteed to be utf8 because it tests equal to tag which is utf8 despite being a byte array
					tokens.push(Token{
						ty: $expr, 
						value: unsafe{str::from_utf8_unchecked(&file_text[i..(i+lit_len)])},
						span: SourceSpan{offset: i, line: cur_line},
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
						span: SourceSpan{offset: i, line: cur_line},
					});
					i += lit_len;
					$($extra_expr;)?
					continue 'outer;
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
		token_match_word!(b"export" => TokenType::Export);
		token_match_word!(b"local" => TokenType::Local);

		// Spaces
		let lit_len = b" ".len();
		if file_text[i..(i+lit_len)] == *b" " {
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
					span: SourceSpan{offset: old_i, line: cur_line},
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
				span: SourceSpan{offset: old_i, line: cur_line},
			});
			continue;
		}
			
		// TODO: Does grug allow tabs for indentation and if it does, should each tab be a separate token
		// token_match!(b"\t" => TokenType::Indentation);

		// Entitiy strings, resource strings, and basic strings
		// Strings
		for (start, ty) in [(&b"r\""[..], TokenType::Resource), (&b"e\""[..], TokenType::Entity), (&b"\""[..], TokenType::String)] {
			if file_text[i..].starts_with(start) {
				let quote_start_index = i;
				i += start.len();
				let start_index = i;
				let start_line = cur_line;
				let start_col = quote_start_index - last_new_line;

				// TODO: Handle Escaped strings
				// This requires changing Token::value to Cow<'_, str>
				// Just allocate a new string
				while i < file_text.len() && file_text[i] != b'"' {
					if file_text[i] == b'\0' {
						return Err(TokenizerError::NullByte {line: cur_line, col: i - last_new_line});
					}
					if i + 2 < file_text.len() && (&file_text[i..=(i+1)] == &[b'\\', b'\r'] || &file_text[i..=(i+1)] == &[b'\\', b'\n']) {
						return Err(TokenizerError::LineBreakInString{
							line: cur_line,
							col: i - last_new_line,
						});
					}
					if file_text[i] == b'\n' {
						cur_line += 1;
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
					ty,
					// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
					value: unsafe{str::from_utf8_unchecked(&file_text[start_index..(i)])},
					span: SourceSpan{offset: quote_start_index, line: start_line},
				});
				i += 1;
				continue 'outer;
			}
		}

		// TODO: Handle unicode strings
		// Words
		if (file_text[i] as char).is_ascii_alphabetic() || file_text[i] == b'_' {
			let start = i;
			while i < file_text.len() && ((file_text[i] as char).is_ascii_alphanumeric() || file_text[i] == b'_'){
				i += 1
			}
			// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
			tokens.push(Token{
				ty: TokenType::Word, 
				value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
				span: SourceSpan{offset: start, line: cur_line},
			});
			continue;
		}

		// Numbers
		if (file_text[i] as char).is_ascii_digit() {
			let start = i;
			let mut seen_period = false;
			i += 1;
			while i < file_text.len() && ((file_text[i] as char).is_ascii_digit() || file_text[i] == b'.') {
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
					// NOTE: I think floats with trailing periods
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
					span: SourceSpan{offset: start, line: cur_line},
				});
			}
			else {
				// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
				tokens.push(Token{
					ty: TokenType::Int32, 
					value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
					span: SourceSpan{offset: start, line: cur_line},
				});
			}
			continue;
		}

		// Comments
		if file_text[i] == b'#' {
			let old_i = i;
			i += 1;
			if i >= file_text.len() || file_text[i] != b' ' {
				return Err(TokenizerError::BasicError(grug_error::new_error(
					tokenizer_error_kind(), 
					"member scope", 
					file_path, 
					// SAFETY: file_text is from a &str
					unsafe{std::str::from_utf8_unchecked(file_text)},
					SourceSpan{offset: i, line: cur_line},
					format_args!("Expected a single space after the '#' on line {}", cur_line), 
				)));
			}
			i += 1;
			let start = i;
			while i < file_text.len() && file_text[i] != b'\r' && file_text[i] != b'\n' {
				if file_text[i] == b'\0' {
					return Err(TokenizerError::NullByte {line: cur_line, col: i - last_new_line});
				}
				i += 1;
			}
			
			if (i - start) == 0 {
				return Err(TokenizerError::EmptyComment{
					line: cur_line,
					col: i - last_new_line,
				});
			} else if (file_text[i - 1] as char).is_ascii_whitespace() {
				return Err(TokenizerError::CommentTrailingWhitespace{
					line: cur_line,
					col: i - last_new_line,
				});
			}
			cur_line += 1;

			// SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
			tokens.push(Token{
				ty: TokenType::Comment, 
				value: unsafe{str::from_utf8_unchecked(&file_text[start..i])},
				span: SourceSpan{offset: old_i, line: cur_line - 1},
			});
			continue;
		}

		// SAFETY: file_text is from a &str
		let file_text = unsafe{std::str::from_utf8_unchecked(file_text)};
		return Err(TokenizerError::BasicError(grug_error::new_error(
			tokenizer_error_kind(), 
			"member scope", 
			file_path, 
			file_text,
			SourceSpan{offset: i, line: cur_line},
			format_args!("Unrecognized character '{}' on line {}", file_text[i..].chars().next().expect("There is atleast one more character"), cur_line), 
		)));
	}
	
	Ok(tokens)
}

fn is_word_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || ch == '_'
}
