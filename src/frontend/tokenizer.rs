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
			Self::Space => write!(f, "SPACE_TOKEN"),
			Self::Indentation => write!(f, "INDENTATION_TOKEN"),
			Self::String => write!(f, "STRING_TOKEN"),
			Self::Word => write!(f, "WORD_TOKEN"),
			Self::Int32 => write!(f, "NUMBER_TOKEN"),
			Self::Float32 => write!(f, "NUMBER_TOKEN"),
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
	},
	// grug_assert(!isspace(grug_text[i - 1]), "A comment has trailing whitespace on line %zu", get_character_line_number(i));
	CommentTrailingWhitespace {
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
			Self::NoSpaceAfterComment {
				line,
				col: _,
			} => write!(f, "Expected a single space after the '#' on line {}", line),
			Self::CommentTrailingWhitespace {
				line,
				col: _,
			} => write!(f, "A comment has trailing whitespace on line {}", line),
			Self::UnclosedString {
				start_line,
				start_col: _,
			} => write!(f, "Unclosed \" on line {}", start_line),
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
			if (file_text[i - 1] as char).is_ascii_whitespace() {
				return Err(TokenizerError::CommentTrailingWhitespace{
					line: cur_line,
					col: i - last_new_line,
				});
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
