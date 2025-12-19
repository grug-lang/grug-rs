use std::ffi::{c_char, c_float, CStr, CString};
use std::mem::ManuallyDrop;

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
#[allow(unused_variables)]
pub extern "C" fn on_fn_dispatcher (fn_name: *const c_char, grug_file_path: *const c_char, value: *mut grug_value, values_count: c_size_t) {
	println!(
		"on_fn_dispatcher: {} {}", 
		unsafe{CStr::from_ptr(fn_name)}.to_str().unwrap(),
		unsafe{CStr::from_ptr(grug_file_path)}.to_str().unwrap(),
	);
}
#[allow(unused_variables)]
pub extern "C" fn dump_file_to_json (input_grug_path: *const c_char, output_json_path: *const c_char) -> bool {
	false
}
#[allow(unused_variables)]
pub extern "C" fn generate_file_from_json (input_json_path: *const c_char, output_grug_path: *const c_char) -> bool {
	false
}
#[allow(unused_variables)]
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
#[cfg(test)]
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
