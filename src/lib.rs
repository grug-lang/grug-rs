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
				_ => todo!(),
			}
		}
	}

	mod tokenizer {
		use std::marker::PhantomData;

		pub struct Token;
		pub enum TokenizerError{}

		pub fn tokenize(file_text: &'_ str) -> Result<Vec<Token>, TokenizerError> {
			Ok(vec![])
		}
	}
	use tokenizer::*;

	mod parser {
		use std::marker::PhantomData;

		use super::tokenizer::Token;
		pub enum ParserError{}

		pub struct Ast;
		pub fn parse(tokens: &'_ [Token]) -> Result<Ast, ParserError> {
			Ok(Ast)
		}
	}
	use parser::*;
}

mod bindings {
	use std::ffi::{c_char, c_float, CStr, OsStr};
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
			Err(err) => ManuallyDrop::new(format!("{}", err)).as_ptr() as *const c_char,
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
