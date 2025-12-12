// #![allow(warnings)]

mod frontend {
	const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;

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
		use std::marker::PhantomData;

		#[derive(Debug)]
		pub struct Token<'a> {
			ty: TokenType,
			value: &'a str,
			line: usize,
			col: usize,
		}

		#[derive(Debug)]
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
			GreaterEqual,
			Greater,
			LessEqual,
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
				token_match!(b"\n" => TokenType::NewLine, {cur_line += 1; last_new_line = i + 1});
				token_match!(b"\r\n" => TokenType::NewLine, {cur_line += 1; last_new_line = i + 2});
				token_match!(b"==" => TokenType::DoubleEquals);
				token_match!(b"!=" => TokenType::NotEquals);
				token_match!(b"=" => TokenType::Equal);
				token_match!(b">=" => TokenType::GreaterEqual);
				token_match!(b">" => TokenType::Greater);
				token_match!(b"<=" => TokenType::LessEqual);
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
		use std::marker::PhantomData;
		use std::collections::{HashSet, HashMap};

		use super::tokenizer::Token;
		#[derive(Debug)]
		pub enum ParserError{}

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

			let mut seen_on_fn = false;
			let mut seen_newline = false;
			let mut newline_allowed = false;
			let mut newline_required = false;
			let mut just_seen_global = false;

			let mut idx = 0;

			Ok(Ast)
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
				c"../grug-tests/tests/".as_ptr(),
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
