// #![allow(warnings)]

mod frontend {
	const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;

	pub fn compile_grug_file(path: &str, mod_name: &str) -> Result<(), String> {
		if !path.contains('/') {
			return Err(format!("The grug file path {}, does not contain a '/' character", path));
		}
		let entity_type = get_entity_type(path)?;

		let file_text = std::fs::read_to_string(&path).map_err(|x| x.to_string())?;

		let tokens = tokenizer::tokenize(&file_text)?;

		let ast = parser::parse(&*tokens)?;

		return Ok(());
	}

	fn get_entity_type(path: &str) -> Result<&str, String> {
		let (_, entity_type) = path.rsplit_once("-").ok_or(
				format!(
					"'{}' is missing an entity type in its name;\n\
					use a dash to specify it, like 'ak47-gun.grug'",
					path
				)
			)?;
		let (entity_type, _) = entity_type.rsplit_once(".").ok_or(
				format!( "'{}' is missing a period in its filename", path)
			)?;
		if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
			return Err(
				format!(
                	"There are more than {MAX_FILE_ENTITY_TYPE_LENGTH} characters \n\
                	in the entity type of '{path}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH"
				)
			);
		}
		if entity_type.len() == 0 {
			return Err(format!(
				"'{}' is missing an entity type in its name;\n\
				use a dash to specify it, like 'ak47-gun.grug'",
				path
			));
		}
		check_custom_id_is_pascal(entity_type)
	}

	fn check_custom_id_is_pascal(entity_type: &str) -> Result<&str, String> {
		let mut chars = entity_type.chars();
		let Some(first) = chars.next() else {
			return Err(format!(
                "'{entity_type}' seems like a custom ID type, but isn't in PascalCase"
			));
		};
		for ch in chars {
			if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_digit(10)) {
                return Err(format!(
                    "'{entity_type}' seems like a custom ID type, but it contains '{ch}', \n\
                    which isn't uppercase/lowercase/a digit"
                ));
			}
		}
		Ok(entity_type)
	}

	// enum FileNameError<'a> {
	// 	FilePathDoesNotContainForwardSlash{
	// 		path: &'a str
	// 	},
	// 	MissingPeriodInFileName {
	// 		path: &'a str
	// 	},
	// 	EntityLenExceedsMaxLen {
	// 		path: &'a str,
	// 		entity_len: &'a str,
	// 	},

	// }

	// pub enum GrugError {
	// 	FileNameError(FileNameError),
	// 	TokenizerError(TokenizerError),
	// 	ParserError(ParserError),
	// }

	mod tokenizer {
		pub struct Token;

		pub fn tokenize(file_text: &str) -> Result<Vec<Token>, String> {
			Ok(vec![])
		}
	}

	mod parser {
		use super::tokenizer::Token;

		pub struct Ast;
		pub fn parse(tokens: &[Token]) -> Result<Ast, String> {
			Ok(Ast)
		}
	}
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
			Err(err_string) => ManuallyDrop::new(err_string).as_ptr() as *const c_char,
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
