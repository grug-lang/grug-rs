#![deny(warnings)]
use std::ffi::CString;
use std::mem::ManuallyDrop;
use grug_rs::state::GrugState;

mod test_bindings {
	use grug_rs::state::{GrugState, RuntimeError};
	use grug_rs::backend::{GrugFile, GrugEntity};
	use grug_rs::types::{GrugValue};
	use grug_rs::frontend;
	use grug_rs::serde;
	use std::ffi::{c_char, CStr, CString};
	use std::sync::{Mutex, Arc,};
	use std::mem::ManuallyDrop;

	pub static GLOBAL_TEST_STATE: Mutex<Option<GrugState>> = Mutex::new(None);
	pub static CURRENT_GRUG_FILE: Mutex<Option<Arc<GrugFile>>> = Mutex::new(None);
	pub static CURRENT_GRUG_ENTITY: Mutex<Option<GrugEntity>> = Mutex::new(None);
	pub static CURRENT_PATH: Mutex<Option<&str>> = Mutex::new(None);
	pub static ERROR_FUNCTION_MESSAGE: Mutex<Option<&str>> = Mutex::new(None);
	pub extern "C" fn compile_grug_file(path: *const c_char) -> *const c_char {
		let path = unsafe{CStr::from_ptr(path)}.to_str().unwrap();
		*CURRENT_PATH.lock().unwrap() = Some(path);
		let ret_val = match frontend::compile_grug_file(GLOBAL_TEST_STATE.lock().unwrap().as_ref().unwrap(), path) {
			Ok(file) => {
				*CURRENT_GRUG_FILE.lock().unwrap() = Some(Arc::new(file));
				std::ptr::null()
			},
			Err(err) => ManuallyDrop::new(CString::new(format!("{}", err)).unwrap()).as_ptr() as *const c_char,
		};
		ret_val
	}
	pub extern "C" fn init_globals_fn_dispatcher () {
		unsafe{GLOBAL_TEST_STATE.lock().unwrap().as_mut().unwrap().set_next_id(42)};
		*CURRENT_GRUG_ENTITY.lock().unwrap() = Some(
			GLOBAL_TEST_STATE
				.lock().unwrap()
				.as_mut().unwrap()
				.create_entity(CURRENT_GRUG_FILE.lock().unwrap().as_ref().unwrap())
				.expect("runtime error")
		)
	}
	#[allow(unused_variables)]
	pub extern "C" fn on_fn_dispatcher (fn_name: *const c_char, values: *const GrugValue) {
		*ERROR_FUNCTION_MESSAGE.lock().unwrap() = None;
		let fn_name = unsafe{CStr::from_ptr(fn_name)}.to_str().unwrap();
		
		let (kind, msg) = match unsafe{GLOBAL_TEST_STATE.lock().unwrap().as_mut().unwrap()
			.call_on_function_raw(CURRENT_GRUG_ENTITY.lock().unwrap().as_mut().unwrap(), fn_name, values)}
		{
			Err(RuntimeError::StackOverflow) => (0, ManuallyDrop::new(CString::new(format!("{}", RuntimeError::StackOverflow)).unwrap()).as_ptr()),
			Err(RuntimeError::ExceededTimeLimit) => (1, ManuallyDrop::new(CString::new(format!("{}", RuntimeError::ExceededTimeLimit)).unwrap()).as_ptr()),
			Ok(_) => {
				if let Some(msg) = *ERROR_FUNCTION_MESSAGE.lock().unwrap() {
					(2, msg.as_ptr().cast())
				} else {
					return;
				}
			}
			Err(RuntimeError::FunctionArgumentCountMismatch {
				expected: _,
				got: _,
			}) => return,
		};
		unsafe{grug_tests_runtime_error_handler(
			msg, 
			kind,
			fn_name.as_ptr().cast(),
			CURRENT_PATH.lock().unwrap().unwrap().as_ptr().cast(),
		)};
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
		*ERROR_FUNCTION_MESSAGE.lock().unwrap() = Some(unsafe{CStr::from_ptr(msg).to_str().unwrap()});
	}

	#[allow(non_camel_case_types)]
	// pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(*const c_char) -> *const c_char;
	#[allow(non_camel_case_types)]
	pub type init_globals_fn_dispatcher_t = extern "C" fn ();
	#[allow(non_camel_case_types)]
	pub type on_fn_dispatcher_t = extern "C" fn (*const c_char, *const GrugValue);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = extern "C" fn (*const c_char, *const c_char) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = extern "C" fn (*const c_char, *const c_char) -> i32;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (*const c_char);

	#[link(name="tests", kind="dylib")]
	unsafe extern "C" {
		pub fn grug_tests_runtime_error_handler(
			reason: *const c_char,
			ty: i32,
			on_fn_name: *const c_char,
			on_fn_path: *const c_char,
		);
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
use test_bindings::*;

mod game_fn_bindings {
	use grug_rs::types::GrugValue;
	use grug_rs::state::GameFnPtr;
	use std::collections::HashMap;
	#[link(name = "tests", kind="dylib")]
	unsafe extern "C" {
		safe fn game_fn_nothing();
		safe fn game_fn_magic() -> GrugValue;
		safe fn game_fn_initialize(values: *const GrugValue);
		safe fn game_fn_initialize_bool(values: *const GrugValue);
		safe fn game_fn_identity(values: *const GrugValue) -> GrugValue;
		safe fn game_fn_max(values: *const GrugValue) -> GrugValue;
		safe fn game_fn_say(values: *const GrugValue);
		safe fn game_fn_sin(values: *const GrugValue) -> GrugValue;
		safe fn game_fn_cos(values: *const GrugValue) -> GrugValue;
        safe fn game_fn_mega(values: *const GrugValue);
        safe fn game_fn_get_false() -> GrugValue;
        safe fn game_fn_set_is_happy(values: *const GrugValue);
        safe fn game_fn_mega_f32(values: *const GrugValue);
        safe fn game_fn_mega_i32(values: *const GrugValue);
        safe fn game_fn_draw(values: *const GrugValue);
        safe fn game_fn_blocked_alrm();
        safe fn game_fn_spawn(values: *const GrugValue);
        safe fn game_fn_has_resource(values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_entity(values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_string(values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_opponent() -> GrugValue;
        safe fn game_fn_set_d(values: *const GrugValue);
        safe fn game_fn_set_opponent(values: *const GrugValue);
        safe fn game_fn_motherload(values: *const GrugValue);
        safe fn game_fn_motherload_subless(values: *const GrugValue);
        safe fn game_fn_offset_32_bit_f32(values: *const GrugValue);
        safe fn game_fn_offset_32_bit_i32(values: *const GrugValue);
        safe fn game_fn_offset_32_bit_string(values: *const GrugValue);
        safe fn game_fn_talk(values: *const GrugValue);
        safe fn game_fn_get_position(values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_position(values: *const GrugValue);
        safe fn game_fn_cause_game_fn_error();
        safe fn game_fn_call_on_b_fn();
        safe fn game_fn_store(values: *const GrugValue);
        safe fn game_fn_retrieve() -> GrugValue;
        safe fn game_fn_box_number(values: *const GrugValue) -> GrugValue;
	}
	pub fn get_game_functions () -> HashMap<&'static str, GameFnPtr> {
		HashMap::from([
			("nothing", (game_fn_nothing as extern "C" fn()).into()),
			("magic", (game_fn_magic as extern "C" fn() -> _).into()),
			("initialize", (game_fn_initialize as extern "C" fn(_)).into()),
			("initialize_bool", (game_fn_initialize_bool as extern "C" fn(_)).into()),
			("identity", (game_fn_identity as extern "C" fn(_) -> _).into()),
			("max", (game_fn_max as extern "C" fn(_) -> _).into()),
			("say", (game_fn_say as extern "C" fn(_)).into()),
			("sin", (game_fn_sin as extern "C" fn(_) -> _).into()),
			("cos", (game_fn_cos as extern "C" fn(_) -> _).into()),
			("mega", (game_fn_mega as extern "C" fn(_)).into()),
			("get_false", (game_fn_get_false as extern "C" fn() -> _).into()),
			("set_is_happy", (game_fn_set_is_happy as extern "C" fn(_)).into()),
			("mega_f32", (game_fn_mega_f32 as extern "C" fn(_)).into()),
			("mega_i32", (game_fn_mega_i32 as extern "C" fn(_)).into()),
			("draw", (game_fn_draw as extern "C" fn(_)).into()),
			("blocked_alrm", (game_fn_blocked_alrm as extern "C" fn()).into()),
			("spawn", (game_fn_spawn as extern "C" fn(_)).into()),
			("has_resource", (game_fn_has_resource as extern "C" fn(_) -> _).into()),
			("has_entity", (game_fn_has_entity as extern "C" fn(_) -> _).into()),
			("has_string", (game_fn_has_string as extern "C" fn(_) -> _).into()),
			("get_opponent", (game_fn_get_opponent as extern "C" fn() -> _).into()),
			("set_d", (game_fn_set_d as extern "C" fn(_)).into()),
			("set_opponent", (game_fn_set_opponent as extern "C" fn(_)).into()),
			("motherload", (game_fn_motherload as extern "C" fn(_)).into()),
			("motherload_subless", (game_fn_motherload_subless as extern "C" fn(_)).into()),
			("offset_32_bit_f32", (game_fn_offset_32_bit_f32 as extern "C" fn(_)).into()),
			("offset_32_bit_i32", (game_fn_offset_32_bit_i32 as extern "C" fn(_)).into()),
			("offset_32_bit_string", (game_fn_offset_32_bit_string as extern "C" fn(_)).into()),
			("talk", (game_fn_talk as extern "C" fn(_)).into()),
			("get_position", (game_fn_get_position as extern "C" fn(_) -> _).into()),
			("set_position", (game_fn_set_position as extern "C" fn(_)).into()),
			("cause_game_fn_error", (game_fn_cause_game_fn_error as extern "C" fn()).into()),
			("call_on_b_fn", (game_fn_call_on_b_fn as extern "C" fn()).into()),
			("store", (game_fn_store as extern "C" fn(_)).into()),
			("retrieve", (game_fn_retrieve as extern "C" fn() -> _).into()),
			("box_number", (game_fn_box_number as extern "C" fn(_) -> _).into()),
		])
	}
}
use game_fn_bindings::*;
use std::io::Write;

#[test]
fn main () {
	let mut args = std::env::args().collect::<Vec<_>>();

	let mut whitelisted_test = std::ptr::null();
	if args.len() == 3 {
		whitelisted_test = ManuallyDrop::new(CString::new(args.pop().unwrap()).unwrap()).as_ptr();
	} else if args.len() > 3 {
		eprintln!("usage: cargo test -- grug_tests <whitelisted_test>");
		std::process::exit(2);
	}
	// let grug_path = &*args[1];
	// println!("{}", grug_path);

	// let 

	let grug_tests_path = c"src/grug-tests/tests";

	let game_functions = get_game_functions();
	let state = GrugState::new("src/grug-tests/mod_api.json", grug_tests_path.to_str().unwrap(), game_functions).unwrap();
	// register_game_functions(&mut state);
		
	*GLOBAL_TEST_STATE.lock().unwrap() = Some(state);

	std::panic::set_hook(Box::new(|info| {
		_ = std::io::stdout().write_fmt(
			format_args!("{}: {}\n", info.location().unwrap(), info.payload_as_str().unwrap_or("No info"))
		);
		std::process::exit(2);
	}));
	unsafe {
		grug_tests_run(
			grug_tests_path.as_ptr(),
			compile_grug_file,
			init_globals_fn_dispatcher,
			on_fn_dispatcher,
			dump_file_to_json,
			generate_file_from_json,
			game_fn_error,
			whitelisted_test,
		)
	}
	_ = std::panic::take_hook();
}
