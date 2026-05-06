use super::SPACES_PER_INDENT;
use allocator_api2::vec::Vec;
use crate::arena::Arena;
use gruggers_core::error::{ErrorKind, GrugError, SourceSpan};

use std::ffi::OsStr;

#[derive(Debug, Clone, Copy)]
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
			Self::OpenParenthesis => write!(f, "'('"),
			Self::CloseParenthesis => write!(f, "')'"),
			Self::OpenBrace => write!(f, "'{{'"),
			Self::CloseBrace => write!(f, "'}}'"),
			Self::Plus => write!(f, "'+'"),
			Self::Minus => write!(f, "'-'"),
			Self::Star => write!(f, "'*'"),
			Self::ForwardSlash => write!(f, "'/'"),
			Self::Percent => write!(f, "'%'"),
			Self::Comma => write!(f, "','"),
			Self::Colon => write!(f, "':'"),
			Self::NewLine => write!(f, "line break ('\\n')"),
			Self::DoubleEquals => write!(f, "'=='"),
			Self::NotEquals => write!(f, "'!='"),
			Self::Equal => write!(f, "'='"),
			Self::GreaterEquals => write!(f, "'>='"),
			Self::Greater => write!(f, "'>'"),
			Self::LessEquals => write!(f, "'<='"),
			Self::Less => write!(f, "'<'"),
			Self::And => write!(f, "'and'"),
			Self::Or => write!(f, "'or'"),
			Self::Not => write!(f, "'not'"),
			Self::True => write!(f, "'true'"),
			Self::False => write!(f, "'false'"),
			Self::If => write!(f, "'if'"),
			Self::Else => write!(f, "'else'"),
			Self::While => write!(f, "'while'"),
			Self::Break => write!(f, "'break'"),
			Self::Return => write!(f, "'return'"),
			Self::Continue => write!(f, "'continue'"),
			Self::Export => write!(f, "'export'"),
			Self::Local => write!(f, "'local'"),
			Self::Space => write!(f, "space (' ')"),
			Self::Indentation => write!(f, "indentation"),
			Self::String => write!(f, "string"),
			Self::Word => write!(f, "word"),
			Self::Int32 => write!(f, "number"),
			Self::Float32 => write!(f, "number"),
			Self::Comment => write!(f, "comment"),
			Self::Resource => write!(f, "resource string"),
			Self::Entity => write!(f, "entity string"),
		}
	}
}

pub fn tokenize<'a, 'b, P: AsRef<OsStr>>(file_text: &'b str, arena: &'a Arena, file_path: P) -> Result<Vec<Token<'b>, &'a Arena>, GrugError<Arena>> {
	let file_path = file_path.as_ref();
	let mut tokens = Vec::new_in(arena);
	let mut cur_line = 1;

	let file_text_str = file_text;
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
		token_match!(b"\n" => TokenType::NewLine, {cur_line += 1;});
		token_match!(b"\r\n" => TokenType::NewLine, {cur_line += 1;});
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
				return Err(GrugError::new_error(
					ErrorKind::TOKENIZER_ERROR, 
					"member scope", 
					file_path, 
					file_text_str,
					SourceSpan{offset: i - num_spaces, line: cur_line},
					format_args!("Expected multiple of {} spaces but found {} spaces", SPACES_PER_INDENT, num_spaces)
				));
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

				// TODO: Handle Escaped strings
				//
				// Just allocate the new string in the arena, you don't even need Cow
				while i < file_text.len() && file_text[i] != b'"' {
					if file_text[i] == b'\0' {
						return Err(GrugError::new_error(
							ErrorKind::TOKENIZER_ERROR, 
							"member scope", 
							file_path, 
							file_text_str,
							SourceSpan{offset: i, line: cur_line},
							format_args!("Unexpected null byte on line {}", cur_line)
						));
					}
					if i + 2 < file_text.len() && (&file_text[i..=(i+2)] == b"\\\r\n" || &file_text[i..=(i+1)] == b"\\\n") {
						return Err(GrugError::new_error(
							ErrorKind::TOKENIZER_ERROR, 
							"member scope", 
							file_path, 
							file_text_str,
							SourceSpan{offset: i, line: cur_line},
							format_args!("Unexpected line break in string on line {}", cur_line), 
						));
					}
					if file_text[i] == b'\n' {
						cur_line += 1;
					}
					i += 1;
				}
				if i >= file_text.len() {
					return Err(GrugError::new_error(
						ErrorKind::TOKENIZER_ERROR, 
						"member scope", 
						file_path, 
						file_text_str,
						SourceSpan{offset: quote_start_index, line: start_line},
						format_args!("Unclosed \" on line {}", start_line), 
					));
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
						return Err(GrugError::new_error(
							ErrorKind::TOKENIZER_ERROR, 
							"member scope", 
							file_path, 
							file_text_str,
							SourceSpan{offset: i, line: cur_line},
							format_args!("Encountered two '.' periods in a number on line {}", cur_line), 
						));
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
					return Err(GrugError::new_error(
						ErrorKind::TOKENIZER_ERROR, 
						"member scope", 
						file_path, 
						file_text_str,
						SourceSpan{offset: i, line: cur_line},
						format_args!("Missing digit after decimal point in '{}'", 
							&file_text_str[start..i]
						), 
					));
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
				return Err(GrugError::new_error(
					ErrorKind::TOKENIZER_ERROR, 
					"member scope", 
					file_path, 
					file_text_str,
					SourceSpan{offset: i, line: cur_line},
					format_args!("Expected space (' ') after '#'"), 
				));
			}
			i += 1;
			let start = i;
			while i < file_text.len() && file_text[i] != b'\r' && file_text[i] != b'\n' {
				if file_text[i] == b'\0' {
					return Err(GrugError::new_error(
						ErrorKind::TOKENIZER_ERROR, 
						"member scope", 
						file_path, 
						file_text_str,
						SourceSpan{offset: i, line: cur_line},
						format_args!("Unexpected null byte on line {}", cur_line)
					));
				}
				i += 1;
			}
			
			if (i - start) == 0 {
				return Err(GrugError::new_error(
					ErrorKind::TOKENIZER_ERROR, 
					"member scope", 
					file_path, 
					file_text_str,
					SourceSpan{offset: i - 1, line: cur_line},
					format_args!("Expected comment to contain some text")
				));
			} else if (file_text[i - 1] as char).is_ascii_whitespace() {
				return Err(GrugError::new_error(
					ErrorKind::TOKENIZER_ERROR, 
					"member scope", 
					file_path, 
					file_text_str,
					SourceSpan{offset: i, line: cur_line},
					format_args!("A comment has trailing whitespace on line {}", cur_line)
				));
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

		return Err(GrugError::new_error(
			ErrorKind::TOKENIZER_ERROR, 
			"member scope", 
			file_path, 
			file_text_str,
			SourceSpan{offset: i, line: cur_line},
			format_args!("Unrecognized character '{}'", file_text_str[i..].chars().next().expect("There is atleast one more character")), 
		));
	}
	
	Ok(tokens)
}

fn is_word_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || ch == '_'
}

