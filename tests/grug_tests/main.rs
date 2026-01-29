#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::state::GrugState;
use gruggers::ntstring::NTStr;
use gruggers::nt;

mod test_bindings {
	use gruggers::state::GrugState;
	use gruggers::error::RuntimeError;
	use gruggers::types::{GrugValue, GrugScriptId, GrugEntityHandle};
	use gruggers::ntstring::{NTStrPtr, NTStr};
	use gruggers::serde;
	use std::path::PathBuf;

	pub static mut GLOBAL_TEST_STATE: Option<GrugState> = None;
	pub static mut CURRENT_GRUG_ENTITY: Option<GrugEntityHandle<'static>> = None;
	pub static mut CURRENT_SCRIPT_ID: Option<GrugScriptId> = None;
	pub static mut CURRENT_PATH: Option<&NTStr> = None;

	pub extern "C" fn compile_grug_file(path: NTStrPtr<'static>) -> Option<NTStrPtr<'static>> {
		unsafe {
			GLOBAL_TEST_STATE.as_mut().unwrap().clear_entities();
			let path = path.as_ntstr();
			CURRENT_PATH = Some(path);
			match GLOBAL_TEST_STATE.as_mut().unwrap().compile_grug_file(path) {
				Ok(id) => {
					CURRENT_SCRIPT_ID = Some(id);
					None
				},
				Err(err) => {
					let mut string = format!("{}", err);
					string.push('\0');
					Some(NTStr::from_str(String::leak(string)).unwrap().into())
				}
			}
		}
	}
	pub extern "C" fn init_globals_fn_dispatcher () {
		unsafe {
			GLOBAL_TEST_STATE.as_ref().unwrap().clear_error();
			GLOBAL_TEST_STATE.as_ref().unwrap().set_next_id(42);
			CURRENT_GRUG_ENTITY = Some(
				GLOBAL_TEST_STATE
					.as_ref().unwrap()
					.create_entity(CURRENT_SCRIPT_ID.unwrap())
					.expect("runtime error")
			)
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn on_fn_dispatcher (fn_name: NTStrPtr<'static>, values: *const GrugValue) {
		unsafe {
			let fn_name = fn_name.as_ntstr();
			let entity_type = PathBuf::from(CURRENT_PATH.unwrap().as_str());
			let entity_type = entity_type
				.file_prefix().unwrap()
				.to_str().unwrap()
				.split_once("-").unwrap()
				.1;
			let fn_id = GLOBAL_TEST_STATE.as_ref().unwrap().get_on_fn_id(entity_type, fn_name).unwrap();
			
			let (kind, msg) = match GLOBAL_TEST_STATE.as_ref().unwrap()
				.call_on_function_raw(&CURRENT_GRUG_ENTITY.as_ref().unwrap(), fn_id, values)
			{
				Err(RuntimeError::StackOverflow) => (0, (format!("{}\0", RuntimeError::StackOverflow))),
				Err(RuntimeError::ExceededTimeLimit) => (1, (format!("{}\0", RuntimeError::ExceededTimeLimit))),
				Err(err@RuntimeError::GameFunctionError{..}) => (2, String::from(GLOBAL_TEST_STATE.as_ref().unwrap().get_error().unwrap())),
				Ok(_) => return,
			};
			if !GLOBAL_TEST_STATE.as_ref().unwrap().handled_error.get() {
				GLOBAL_TEST_STATE.as_ref().unwrap().set_handled_error();
				grug_tests_runtime_error_handler(
					NTStr::from_str(String::leak(msg)).unwrap().as_ntstrptr(),
					kind,
					fn_name.as_ntstrptr(),
					CURRENT_PATH.as_ref().unwrap().as_ntstrptr(),
				);
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn dump_file_to_json (input_grug_path: NTStrPtr<'static>, output_json_path: NTStrPtr<'static>) -> i32 {
		let grug_path = input_grug_path.as_ntstr();
		let json_path = output_json_path.as_ntstr();

		match serde::dump_file_to_json(grug_path, json_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn generate_file_from_json (input_json_path: NTStrPtr<'static>, output_grug_path: NTStrPtr<'static>) -> i32 {
		let output_grug_path = output_grug_path.as_ntstr();
		let input_json_path = input_json_path.as_ntstr();

		match serde::generate_file_from_json(input_json_path, output_grug_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn game_fn_error (msg: NTStrPtr<'static>) {
		unsafe {
			GLOBAL_TEST_STATE.as_ref().unwrap().set_error(msg.as_ntstr())
		}
	}

	#[allow(non_camel_case_types)]
	// pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(NTStrPtr<'static>) -> Option<NTStrPtr<'static>>;
	#[allow(non_camel_case_types)]
	pub type init_globals_fn_dispatcher_t = extern "C" fn ();
	#[allow(non_camel_case_types)]
	pub type on_fn_dispatcher_t = extern "C" fn (NTStrPtr<'static>, *const GrugValue);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = extern "C" fn (NTStrPtr<'static>, NTStrPtr<'static>) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = extern "C" fn (NTStrPtr<'static>, NTStrPtr<'static>) -> i32;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (NTStrPtr<'static>);

	#[link(name="tests", kind="dylib")]
	unsafe extern "C" {
		pub fn grug_tests_runtime_error_handler(
			reason: NTStrPtr<'static>,
			ty: i32,
			on_fn_name: NTStrPtr<'static>,
			on_fn_path: NTStrPtr<'static>,
		);
		pub fn grug_tests_run(
			tests_dir_path_: NTStrPtr<'static>, 
			compile_grug_file: compile_grug_file_t,
			init_globals_fn_dispatcher_: init_globals_fn_dispatcher_t,
			on_fn_dispatcher_: on_fn_dispatcher_t,
			dump_file_to_json_: dump_file_to_json_t,
			generate_file_from_json_: generate_file_from_json_t,
			game_fn_error_: game_fn_error_t,
			whitelisted_test_: Option<NTStrPtr<'static>>
		);
	}
}
use test_bindings::*;

mod game_fn_bindings {
	use gruggers::types::GrugValue;
	use gruggers::state::GrugState;
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
	pub fn register_game_functions(state: &mut GrugState) {
		_ = state.register_game_fn("nothing", game_fn_nothing as extern "C" fn());
		_ = state.register_game_fn("magic", game_fn_magic as extern "C" fn() -> _);
		_ = state.register_game_fn("initialize", game_fn_initialize as extern "C" fn(_));
		_ = state.register_game_fn("initialize_bool", game_fn_initialize_bool as extern "C" fn(_));
		_ = state.register_game_fn("identity", game_fn_identity as extern "C" fn(_) -> _);
		_ = state.register_game_fn("max", game_fn_max as extern "C" fn(_) -> _);
		_ = state.register_game_fn("say", game_fn_say as extern "C" fn(_));
		_ = state.register_game_fn("sin", game_fn_sin as extern "C" fn(_) -> _);
		_ = state.register_game_fn("cos", game_fn_cos as extern "C" fn(_) -> _);
		_ = state.register_game_fn("mega", game_fn_mega as extern "C" fn(_));
		_ = state.register_game_fn("get_false", game_fn_get_false as extern "C" fn() -> _);
		_ = state.register_game_fn("set_is_happy", game_fn_set_is_happy as extern "C" fn(_));
		_ = state.register_game_fn("mega_f32", game_fn_mega_f32 as extern "C" fn(_));
		_ = state.register_game_fn("mega_i32", game_fn_mega_i32 as extern "C" fn(_));
		_ = state.register_game_fn("draw", game_fn_draw as extern "C" fn(_));
		_ = state.register_game_fn("blocked_alrm", game_fn_blocked_alrm as extern "C" fn());
		_ = state.register_game_fn("spawn", game_fn_spawn as extern "C" fn(_));
		_ = state.register_game_fn("has_resource", game_fn_has_resource as extern "C" fn(_) -> _);
		_ = state.register_game_fn("has_entity", game_fn_has_entity as extern "C" fn(_) -> _);
		_ = state.register_game_fn("has_string", game_fn_has_string as extern "C" fn(_) -> _);
		_ = state.register_game_fn("get_opponent", game_fn_get_opponent as extern "C" fn() -> _);
		_ = state.register_game_fn("set_d", game_fn_set_d as extern "C" fn(_));
		_ = state.register_game_fn("set_opponent", game_fn_set_opponent as extern "C" fn(_));
		_ = state.register_game_fn("motherload", game_fn_motherload as extern "C" fn(_));
		_ = state.register_game_fn("motherload_subless", game_fn_motherload_subless as extern "C" fn(_));
		_ = state.register_game_fn("offset_32_bit_f32", game_fn_offset_32_bit_f32 as extern "C" fn(_));
		_ = state.register_game_fn("offset_32_bit_i32", game_fn_offset_32_bit_i32 as extern "C" fn(_));
		_ = state.register_game_fn("offset_32_bit_string", game_fn_offset_32_bit_string as extern "C" fn(_));
		_ = state.register_game_fn("talk", game_fn_talk as extern "C" fn(_));
		_ = state.register_game_fn("get_position", game_fn_get_position as extern "C" fn(_) -> _);
		_ = state.register_game_fn("set_position", game_fn_set_position as extern "C" fn(_));
		_ = state.register_game_fn("cause_game_fn_error", game_fn_cause_game_fn_error as extern "C" fn());
		_ = state.register_game_fn("call_on_b_fn", game_fn_call_on_b_fn as extern "C" fn());
		_ = state.register_game_fn("store", game_fn_store as extern "C" fn(_));
		_ = state.register_game_fn("retrieve", game_fn_retrieve as extern "C" fn() -> _);
		_ = state.register_game_fn("box_number", game_fn_box_number as extern "C" fn(_) -> _);
	}
}
use game_fn_bindings::*;
use std::io::Write;

#[test]
fn main () {
	let mut args = std::env::args().collect::<Vec<_>>();

	let mut whitelisted_test = None;
	if args.len() == 3 {
		let mut test = args.pop().unwrap();
		test.push('\0');
		whitelisted_test = unsafe{Some(NTStr::from_str_unchecked(String::leak(test)).as_ntstrptr())};
	} else if args.len() > 3 {
		eprintln!("usage: cargo test -- grug_tests <whitelisted_test>");
		std::process::exit(2);
	}

	let grug_tests_path = nt!("src/grug-tests/tests");

	let mut state = GrugState::new("src/grug-tests/mod_api.json", grug_tests_path.as_str()).unwrap();
	// register_game_functions(&mut state);
	register_game_functions(&mut state);
	// let game_functions = get_game_functions();
	assert!(state.all_game_fns_registered());
		
	unsafe {
		GLOBAL_TEST_STATE = Some(state);
	}

	std::panic::set_hook(Box::new(|info| {
		_ = std::io::stdout().write_fmt(
			format_args!("{}: {}\n", info.location().unwrap(), info.payload_as_str().unwrap_or("No info"))
		);
		std::process::exit(2);
	}));
	unsafe {
		grug_tests_run(
			grug_tests_path.as_ntstrptr(),
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
