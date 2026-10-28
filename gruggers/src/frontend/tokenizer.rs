use super::SPACES_PER_INDENT;
use crate::arena::Arena;
use crate::error::{Error, ErrorKind, SourceSpan};
use allocator_api2::vec::Vec;
use allocator_api2::alloc::Allocator;

use std::ptr::NonNull;
use std::mem::{size_of, transmute};
use std::alloc::Layout;
use std::ffi::OsStr;

#[derive(Debug, Clone, Copy, PartialEq)]
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
    OpenBracket,
    CloseBracket,
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Comma,
    Colon,
    Dot,
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

const _: () = const {assert!(size_of::<TokenType>() == size_of::<Option<TokenType>>())};

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::OpenParenthesis => write!(f, "'('"),
            Self::CloseParenthesis => write!(f, "')'"),
            Self::OpenBrace => write!(f, "'{{'"),
            Self::CloseBrace => write!(f, "'}}'"),
            Self::OpenBracket => write!(f, "'['"),
            Self::CloseBracket => write!(f, "']'"),
            Self::Plus => write!(f, "'+'"),
            Self::Minus => write!(f, "'-'"),
            Self::Star => write!(f, "'*'"),
            Self::ForwardSlash => write!(f, "'/'"),
            Self::Comma => write!(f, "','"),
            Self::Colon => write!(f, "':'"),
            Self::Dot => write!(f, "'.'"),
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

pub fn tokenize<'a, P: AsRef<OsStr>>(
    file_text: &'a str,
    arena: &'a Arena,
    file_path: P,
) -> Result<Vec<Token<'a>, &'a Arena>, Error> {
    let file_path = file_path.as_ref();
    let mut tokens = Vec::new_in(arena);
    let mut cur_line = 1;

    macro_rules! new_tokenizer_error{
		(($offset: expr, $line: expr) => $format_str: literal $(, $args: expr)*) => {
			Error::new(ErrorKind::TOKENIZER_ERROR, "", file_path, file_text, SourceSpan{offset: $offset, line: $line}, format_args!($format_str $(,$args)*))
		};
		($span: expr => $format_str: literal $(, $args: expr)*) => {
			Error::new(ErrorKind::TOKENIZER_ERROR, "", file_path, file_text, $span, format_args!($format_str $(,$args)*))
		}
	}

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
        token_match!(b"[" => TokenType::OpenBracket);
        token_match!(b"]" => TokenType::CloseBracket);
        token_match!(b"+" => TokenType::Plus);
        token_match!(b"-" => TokenType::Minus);
        token_match!(b"*" => TokenType::Star);
        token_match!(b"/" => TokenType::ForwardSlash);
        token_match!(b"," => TokenType::Comma);
        token_match!(b":" => TokenType::Colon);
        token_match!(b"." => TokenType::Dot);
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
        if file_text[i..(i + lit_len)] == *b" " {
            let old_i = i;
            while i < file_text.len() && file_text[i] == b' ' {
                i += 1;
            }
            let num_spaces = i - old_i;
            if num_spaces == 1 {
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
                tokens.push(Token {
                    ty: TokenType::Space,
                    value: unsafe { str::from_utf8_unchecked(&file_text[old_i..i]) },
                    span: SourceSpan {
                        offset: old_i,
                        line: cur_line,
                    },
                });
                continue;
            }
            if num_spaces % SPACES_PER_INDENT != 0 {
                return Err(new_tokenizer_error!(
                    (i - num_spaces, cur_line) =>
                    "Expected multiple of {} spaces but found {} spaces", SPACES_PER_INDENT, num_spaces
                ));
            }

            // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
            tokens.push(Token {
                ty: TokenType::Indentation,
                value: unsafe { str::from_utf8_unchecked(&file_text[old_i..i]) },
                span: SourceSpan {
                    offset: old_i,
                    line: cur_line,
                },
            });
            continue;
        }

        // Entitiy strings, resource strings, and basic strings
        // Strings
        for (start, ty) in [
            (&b"r\""[..], TokenType::Resource),
            (&b"e\""[..], TokenType::Entity),
            (&b"\""[..], TokenType::String),
        ] {
            if file_text[i..].starts_with(start) {
                let quote_start_index = i;
                i += start.len();
                let start_index = i;
                let start_line = cur_line;

                let mut is_escaped = false;

                let mut allocated = Vec::new_in(arena);
				let mut finished = false;

				let mut chars = file_text_str[i..].chars();
				while let Some(char) = chars.next() {
					match char {
						'"' if !is_escaped => {
							finished = true;
							break;
						},
						't' if is_escaped => allocated.push(b'\t'),
						'n' if is_escaped => allocated.push(b'\n'),
						'r' if is_escaped => allocated.push(b'\r'),
						'\\' if !is_escaped && ty == TokenType::String => {
							is_escaped = true;
						}
						'\r' | '\n' if is_escaped => {
							return Err(new_tokenizer_error!(
								(file_text_str.len() - chars.as_str().len() - 2, cur_line) =>
								"Unexpected line break in string on line {}", cur_line
							));
						}
						'\n' => {
							cur_line += 1;
							allocated.push(b'\n');
						}
						x => {
							is_escaped = false;
							let mut bytes = [0;4];
							let char_utf8 = x.encode_utf8(&mut bytes).as_bytes();
							allocated.extend_from_slice(char_utf8);
						}
					}
				}
				if !finished {
                    return Err(new_tokenizer_error!(
                        (quote_start_index, start_line) =>
                        "Unclosed \" on line {}", start_line
                    ));
				}

                let value = if !allocated.is_empty() {
                    unsafe { str::from_utf8_unchecked(allocated.leak()) }
                } else {
                    unsafe { str::from_utf8_unchecked(&file_text[start_index..(i)]) }
                };
                tokens.push(Token {
                    ty,
                    // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
                    value,
                    span: SourceSpan {
                        offset: quote_start_index,
                        line: start_line,
                    },
                });
				i = file_text.len() - chars.as_str().len();
                continue 'outer;
            }
        }

        // TODO: Handle unicode strings
        // Words
        if (file_text[i] as char).is_ascii_alphabetic() || file_text[i] == b'_' {
            let start = i;
            while i < file_text.len()
                && ((file_text[i] as char).is_ascii_alphanumeric() || file_text[i] == b'_')
            {
                i += 1
            }
            // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
            tokens.push(Token {
                ty: TokenType::Word,
                value: unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
                span: SourceSpan {
                    offset: start,
                    line: cur_line,
                },
            });
            continue;
        }

        // Numbers
        if (file_text[i] as char).is_ascii_digit() {
            let start = i;
            let mut seen_period = false;
            i += 1;
            while i < file_text.len()
                && ((file_text[i] as char).is_ascii_digit() || file_text[i] == b'.')
            {
                if file_text[i] == b'.' {
                    if seen_period {
                        return Err(new_tokenizer_error!(
                            (i, cur_line) =>
                            "Encountered two '.' periods in a number on line {}", cur_line
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
                    return Err(new_tokenizer_error!(
                        (i, cur_line) =>
                        "Missing digit after decimal point in '{}'", &file_text_str[start..i]
                    ));
                }
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
                tokens.push(Token {
                    ty: TokenType::Float32,
                    value: unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
                    span: SourceSpan {
                        offset: start,
                        line: cur_line,
                    },
                });
            } else {
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
                tokens.push(Token {
                    ty: TokenType::Int32,
                    value: unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
                    span: SourceSpan {
                        offset: start,
                        line: cur_line,
                    },
                });
            }
            continue;
        }

        // Comments
        if file_text[i] == b'#' {
            let old_i = i;
            i += 1;
            if i >= file_text.len() || file_text[i] != b' ' {
                return Err(new_tokenizer_error!(
                    (i, cur_line) =>
                    "Expected space (' ') after '#'"
                ));
            }
            i += 1;
            let start = i;
            while i < file_text.len() && file_text[i] != b'\r' && file_text[i] != b'\n' {
                i += 1;
            }

            if (i - start) == 0 {
                return Err(new_tokenizer_error!(
                    (i - 1, cur_line) =>
                    "Expected comment to contain some text"
                ));
            } else if (file_text[i - 1] as char).is_ascii_whitespace() {
                return Err(new_tokenizer_error!(
                    (i, cur_line) =>
                    "A comment has trailing whitespace on line {}", cur_line
                ));
            }

            // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
            tokens.push(Token {
                ty: TokenType::Comment,
                value: unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
                span: SourceSpan {
                    offset: old_i,
                    line: cur_line,
                },
            });
            continue;
        }

        return Err(new_tokenizer_error!(
            (i, cur_line) =>
            "Unrecognized character '{}'", file_text_str[i..].chars().next().expect("There is atleast one more character")
        ));
    }

    Ok(tokens)
}

pub fn tokenize_2<'a, P: AsRef<OsStr>>(
    file_text: &'a str,
    arena: &'a Arena,
    file_path: P,
) -> Result<Tokens<'a>, Error> {
    let file_path = file_path.as_ref();
    let mut tokens = Tokens::new_in(arena);
    let mut cur_line = 1;

    macro_rules! new_tokenizer_error{
		(($offset: expr, $line: expr) => $format_str: literal $(, $args: expr)*) => {
			Error::new(ErrorKind::TOKENIZER_ERROR, "", file_path, file_text, SourceSpan{offset: $offset, line: $line}, format_args!($format_str $(,$args)*))
		};
		($span: expr => $format_str: literal $(, $args: expr)*) => {
			Error::new(ErrorKind::TOKENIZER_ERROR, "", file_path, file_text, $span, format_args!($format_str $(,$args)*))
		}
	}

    let file_text_str = file_text;
    let file_text = file_text.as_bytes();
    let mut i = 0;

    'outer: while i < file_text.len() {
        macro_rules! token_match {
			($tag: literal => $expr: expr$(, $extra_expr: expr)?) => {
				let lit_len = $tag.len();
				if i + lit_len <= file_text.len() && &file_text[i..(i+lit_len)] == &*$tag {
					// SAFETY: string is guaranteed to be utf8 because it tests equal to tag which is utf8 despite being a byte array
					tokens.push(
						$expr, 
						unsafe{str::from_utf8_unchecked(&file_text[i..(i+lit_len)])}, 
						SourceSpan{offset: i, line: cur_line}
					);
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
					unsafe{println!("{:?}", (lit_len, str::from_utf8_unchecked(&file_text[i..(i+lit_len)])));}
					tokens.push(
						$expr, 
						unsafe{str::from_utf8_unchecked(&file_text[i..(i+lit_len)])}, 
						SourceSpan{offset: i, line: cur_line}
					);
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
        token_match!(b"[" => TokenType::OpenBracket);
        token_match!(b"]" => TokenType::CloseBracket);
        token_match!(b"+" => TokenType::Plus);
        token_match!(b"-" => TokenType::Minus);
        token_match!(b"*" => TokenType::Star);
        token_match!(b"/" => TokenType::ForwardSlash);
        token_match!(b"," => TokenType::Comma);
        token_match!(b":" => TokenType::Colon);
        token_match!(b"." => TokenType::Dot);
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
        if file_text[i..(i + lit_len)] == *b" " {
            let old_i = i;
            while i < file_text.len() && file_text[i] == b' ' {
                i += 1;
            }
            let num_spaces = i - old_i;
            if num_spaces == 1 {
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
				tokens.push(
					TokenType::Space, 
					unsafe{str::from_utf8_unchecked(&file_text[old_i..i])}, 
					SourceSpan{offset: old_i, line: cur_line}
				);
                continue;
            }
            if num_spaces % SPACES_PER_INDENT != 0 {
                return Err(new_tokenizer_error!(
                    (i - num_spaces, cur_line) =>
                    "Expected multiple of {} spaces but found {} spaces", SPACES_PER_INDENT, num_spaces
                ));
            }

            // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
			tokens.push(
				TokenType::Indentation, 
				unsafe{str::from_utf8_unchecked(&file_text[old_i..i])}, 
				SourceSpan{offset: old_i, line: cur_line}
			);
            continue;
        }

        // Entitiy strings, resource strings, and basic strings
        // Strings
        for (start, ty) in [
            (&b"r\""[..], TokenType::Resource),
            (&b"e\""[..], TokenType::Entity),
            (&b"\""[..], TokenType::String),
        ] {
            if file_text[i..].starts_with(start) {
                let quote_start_index = i;
                i += start.len();
                let start_index = i;
                let start_line = cur_line;

                let mut is_escaped = false;

                let mut allocated = Vec::new_in(arena);
				let mut finished = false;

				let mut chars = file_text_str[i..].chars();
				while let Some(char) = chars.next() {
					match char {
						'"' if !is_escaped => {
							finished = true;
							break;
						},
						't' if is_escaped => allocated.push(b'\t'),
						'n' if is_escaped => allocated.push(b'\n'),
						'r' if is_escaped => allocated.push(b'\r'),
						'\\' if !is_escaped && ty == TokenType::String => {
							is_escaped = true;
						}
						'\r' | '\n' if is_escaped => {
							return Err(new_tokenizer_error!(
								(file_text_str.len() - chars.as_str().len() - 2, cur_line) =>
								"Unexpected line break in string on line {}", cur_line
							));
						}
						'\0' => {
							return Err(new_tokenizer_error!(
								(file_text_str.len() - chars.as_str().len(), cur_line) =>
								"Unexpected null byte on line {}", cur_line
							));
						}
						'\n' => {
							cur_line += 1;
							allocated.push(b'\n');
						}
						x => {
							is_escaped = false;
							let mut bytes = [0;4];
							let char_utf8 = x.encode_utf8(&mut bytes).as_bytes();
							allocated.extend_from_slice(char_utf8);
						}
					}
				}
				if !finished {
                    return Err(new_tokenizer_error!(
                        (quote_start_index, start_line) =>
                        "Unclosed \" on line {}", start_line
                    ));
				}

                let value = if !allocated.is_empty() {
                    unsafe { str::from_utf8_unchecked(allocated.leak()) }
                } else {
                    unsafe { str::from_utf8_unchecked(&file_text[start_index..(i)]) }
                };
				tokens.push(
					ty, 
					value, 
					SourceSpan{offset: quote_start_index, line: start_line}
				);
				i = file_text.len() - chars.as_str().len();
                continue 'outer;
            }
        }

        // TODO: Handle unicode strings
        // Words
        if (file_text[i] as char).is_ascii_alphabetic() || file_text[i] == b'_' {
            let start = i;
            while i < file_text.len()
                && ((file_text[i] as char).is_ascii_alphanumeric() || file_text[i] == b'_')
            {
                i += 1
            }
			tokens.push(
				TokenType::Word, 
                unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
				SourceSpan{offset: start, line: cur_line}
			);
            continue;
        }

        // Numbers
        if (file_text[i] as char).is_ascii_digit() {
            let start = i;
            let mut seen_period = false;
            i += 1;
            while i < file_text.len()
                && ((file_text[i] as char).is_ascii_digit() || file_text[i] == b'.')
            {
                if file_text[i] == b'.' {
                    if seen_period {
                        return Err(new_tokenizer_error!(
                            (i, cur_line) =>
                            "Encountered two '.' periods in a number on line {}", cur_line
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
                    return Err(new_tokenizer_error!(
                        (i, cur_line) =>
                        "Missing digit after decimal point in '{}'", &file_text_str[start..i]
                    ));
                }
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
				tokens.push(
					TokenType::Float32, 
					unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
					SourceSpan{offset: start, line: cur_line}
				);
            } else {
                // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
				tokens.push(
					TokenType::Int32, 
					unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
					SourceSpan{offset: start, line: cur_line}
				);
            }
            continue;
        }

        // Comments
        if file_text[i] == b'#' {
            let old_i = i;
            i += 1;
            if i >= file_text.len() || file_text[i] != b' ' {
                return Err(new_tokenizer_error!(
                    (i, cur_line) =>
                    "Expected space (' ') after '#'"
                ));
            }
            i += 1;
            let start = i;
            while i < file_text.len() && file_text[i] != b'\r' && file_text[i] != b'\n' {
                if file_text[i] == b'\0' {
                    return Err(new_tokenizer_error!(
                        (i, cur_line) =>
                        "Unexpected null byte on line {}", cur_line
                    ));
                }
                i += 1;
            }

            if (i - start) == 0 {
                return Err(new_tokenizer_error!(
                    (i - 1, cur_line) =>
                    "Expected comment to contain some text"
                ));
            } else if (file_text[i - 1] as char).is_ascii_whitespace() {
                return Err(new_tokenizer_error!(
                    (i, cur_line) =>
                    "A comment has trailing whitespace on line {}", cur_line
                ));
            }

            // SAFETY: string starting at current index is guaranteed to be utf8 it matches a valid utf8 byte
			tokens.push(
				TokenType::Comment, 
				unsafe { str::from_utf8_unchecked(&file_text[start..i]) },
				SourceSpan{offset: old_i, line: cur_line}
			);
            continue;
        }

        return Err(new_tokenizer_error!(
            (i, cur_line) =>
            "Unrecognized character '{}'", file_text_str[i..].chars().next().expect("There is atleast one more character")
        ));
    }

    Ok(tokens)
}

fn is_word_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

#[expect(unused)]
pub struct Tokens<'a> {
	buf: NonNull<u8>,
	len: usize,
	cap: usize,
	arena: &'a Arena
}

impl<'a> Tokens<'a> {
	pub const fn new_in(arena: &'a Arena) -> Self {
		Self {
			buf: NonNull::dangling(),
			len: 0,
			cap: 0,
			arena,
		}
	}

	pub fn leak(self) -> TokensSlice<'a> {
		TokensSlice {
			len: self.len,
			tys: self.tys_ptr(),
			values: self.values_ptr(),
			spans: self.spans_ptr(),
		}
	}

	pub fn as_slice(&self) -> TokensSlice {
		TokensSlice {
			len: self.len,
			tys: self.tys_ptr(),
			values: self.values_ptr(),
			spans: self.spans_ptr(),
		}
	}

	fn grow(&mut self) {
		let new_cap = if self.cap == 0 { 8 } else { self.cap * 2 };
		let new_layout = Self::layout_for_cap(new_cap);
		let old_layout = Self::layout_for_cap(self.cap);

		let old_values_off = self.values_off();
		let old_spans_off = self.spans_off();

		let new_ptr = unsafe{self.arena.grow(self.buf, old_layout, new_layout)
			.expect("could not reallocate buffer").cast()};
		self.buf = new_ptr;
		self.cap = new_cap;
		
		unsafe{self.buf.add(old_spans_off).cast().copy_to(self.spans_ptr(), self.len)};
		unsafe{self.buf.add(old_values_off).cast().copy_to(self.values_ptr(), self.len)};
		// Set rest of types array to None
		unsafe{self.buf.add(self.len).write_bytes(transmute::<Option<TokenType>, u8>(None), self.cap - self.len)};
	}

	const fn layout_for_cap(cap: usize) -> Layout {
		if cap == 0 {
			Layout::new::<()>()
		} else {
			match Layout::from_size_align(
				(64 + cap) * size_of::<Option<TokenType>>() + 
				cap * size_of::<&str>() + 
				cap * size_of::<SourceSpan>(),
				8
			) {
				Ok(layout) => layout,
				Err(_) => panic!("invalid layout"),
			}
		}
	}

	const fn tys_ptr(&self) -> NonNull<Option<TokenType>> {
		self.buf.cast()
	}

	const fn values_off(&self) -> usize {
		if self.cap == 0 {
			0
		} else {
			(64 + self.cap) * size_of::<Option<TokenType>>()
		}
	}

	const fn spans_off(&self) -> usize {
		self.values_off() + self.cap * size_of::<&str>()
	}

	const fn values_ptr(&self) -> NonNull<&'a str> {
		unsafe{self.buf.add(self.values_off()).cast()}
	}

	const fn spans_ptr(&self) -> NonNull<SourceSpan> {
		unsafe{self.buf.add(self.spans_off()).cast()}
	}

	pub fn push(&mut self, ty: TokenType, value: &'a str, span: SourceSpan) {
		if self.len >= self.cap {
			self.grow();
		}
		unsafe{self.tys_ptr().add(self.len).write(Some(ty))}
		unsafe{self.values_ptr().add(self.len).write(value)}
		unsafe{self.spans_ptr().add(self.len).write(span)}
		self.len += 1;
		// println!("values after push {:?}", (self.as_slice().types(), self.as_slice().values(), self.as_slice().spans()))
	}
}

pub struct TokensSlice<'a> {
	len: usize,
	// if len != 0, tys is guaranteed to contain 64 values past the end of slice that are `None`
	// This is to allow SIMD optimizations for array comparisons
	tys: NonNull<Option<TokenType>>,
	values: NonNull<&'a str>,
	spans: NonNull<SourceSpan>,
}

impl<'a> Iterator for TokensSlice<'a> {
	type Item = Token<'a>;
	fn next(&mut self) -> Option<Self::Item> {
		if self.len == 0 { None }
		else {
			let tok = Token {
				ty   : unsafe{self.tys.cast().read()},
				value: unsafe{self.values    .read()},
				span : unsafe{self.spans     .read()},
			};
			self.len -= 1;
			self.tys    = unsafe{self.tys.add(1)};
			self.values = unsafe{self.values.add(1)};
			self.spans  = unsafe{self.spans.add(1)};
			Some(tok)
		}
	}
}

impl<'a> TokensSlice<'a> {
	#[inline(always)]
	pub fn as_array<const N: usize>(&self) -> Option<TokensArray<'a, N>> {
		if self.len >= N {
			if N > 0 {
				// This hopefully indicates that a SIMD overload is okay.
				unsafe {
					assert!(self.tys.add(self.len).cast::<[Option<TokenType>; 64]>().read() == [None;64]);
				}
			} 
			Some(TokensArray {
				tys   : unsafe{self.tys   .cast().as_ref()},
				values: unsafe{self.values.cast().as_ref()},
				spans : unsafe{self.spans .cast().as_ref()},
			})
		} else {
			None
		}
	}

	pub fn take_array<const N: usize>(&mut self) -> Option<TokensArray<'a, N>> {
		let arr = self.as_array()?;
		self.len -= N;
		self.tys    = unsafe{self.tys.add(N)};
		self.values = unsafe{self.values.add(N)};
		self.spans  = unsafe{self.spans.add(N)};
		Some(arr)
	}

	pub fn types(&self) -> &[TokenType] {
		unsafe{std::slice::from_raw_parts(self.tys.cast().as_ptr(), self.len)}
	}
	pub fn values(&self) -> &[&str] {
		unsafe{std::slice::from_raw_parts(self.values.as_ptr(), self.len)}
	}
	pub fn spans(&self) -> &[SourceSpan] {
		unsafe{std::slice::from_raw_parts(self.spans.as_ptr(), self.len)}
	}
}

pub struct TokensArray<'a, const N: usize> {
	pub tys:    &'a [TokenType ;N],
	pub values: &'a [&'a str   ;N],
	pub spans:  &'a [SourceSpan;N],
}
