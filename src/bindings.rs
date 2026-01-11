use std::ffi::c_float;
#[cfg(test)]
pub mod test_bindings {
	use super::*;
	use crate::state::GrugState;
	use crate::types::{GrugValue};
	use crate::frontend;
	use crate::serde;
	use std::ffi::{c_char, CStr, CString};
	use std::sync::OnceLock;
	use std::mem::ManuallyDrop;

	pub static GLOBAL_TEST_STATE: OnceLock<GrugState> = OnceLock::new();
	pub extern "C" fn compile_grug_file(path: *const c_char) -> *const c_char {
		let path = unsafe{CStr::from_ptr(path)}.to_str().unwrap();

		match frontend::compile_grug_file(GLOBAL_TEST_STATE.get().unwrap(), path) {
			Ok(()) => return std::ptr::null(),
			Err(err) => ManuallyDrop::new(CString::new(format!("{}", err)).unwrap()).as_ptr() as *const c_char,
		}
	}
	pub extern "C" fn init_globals_fn_dispatcher () {
		println!("init_globals_fn_dispatcher called");
	}
	#[allow(unused_variables)]
	pub extern "C" fn on_fn_dispatcher (fn_name: *const c_char, value: *mut GrugValue) {
		println!(
			"on_fn_dispatcher: {}", 
			unsafe{CStr::from_ptr(fn_name)}.to_str().unwrap(),
		);
	}
	#[allow(unused_variables)]
	pub extern "C" fn dump_file_to_json (input_grug_path: *const c_char, output_json_path: *const c_char) -> i32 {
		let grug_path = unsafe{CStr::from_ptr(input_grug_path)}.to_str().unwrap();
		let json_path = unsafe{CStr::from_ptr(output_json_path)}.to_str().unwrap();

		match serde::dump_file_to_json(grug_path, json_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn generate_file_from_json (input_json_path: *const c_char, output_grug_path: *const c_char) -> i32 {
		let input_json_path = unsafe{CStr::from_ptr(input_json_path)}.to_str().unwrap();
		let output_grug_path = unsafe{CStr::from_ptr(output_grug_path)}.to_str().unwrap();

		match serde::generate_file_from_json(input_json_path, output_grug_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn game_fn_error (msg: *const c_char) {
		println!("game_fn_error called with {}", unsafe{CStr::from_ptr(msg)}.to_str().unwrap());
	}

	#[allow(non_camel_case_types)]
	pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(*const c_char) -> *const c_char;
	#[allow(non_camel_case_types)]
	pub type init_globals_fn_dispatcher_t = extern "C" fn ();
	#[allow(non_camel_case_types)]
	pub type on_fn_dispatcher_t = extern "C" fn (*const c_char, *mut GrugValue);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = extern "C" fn (*const c_char, *const c_char) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = extern "C" fn (*const c_char, *const c_char) -> i32;
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
pub use test_bindings::*;
