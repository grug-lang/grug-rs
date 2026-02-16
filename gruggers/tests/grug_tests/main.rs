#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::ntstring::NTStr;
use gruggers::nt;

mod test_bindings {
	use gruggers::state::{GrugInitSettings, GrugState};
	use gruggers::backend::bytecode::BytecodeBackend;
	use gruggers::error::RuntimeError;
	use gruggers::types::{GrugValue, GrugScriptId, GrugEntityHandle};
	use gruggers::ntstring::{NTStrPtr, NTStr};
	use gruggers::serde;
	use std::path::PathBuf;
	use gruggers::nt;

	pub static mut CURRENT_GRUG_ENTITY: Option<GrugEntityHandle<'static>> = None;
	pub static mut CURRENT_SCRIPT_ID: Option<GrugScriptId> = None;
	pub static mut CURRENT_PATH: Option<&NTStr> = None;

	pub extern "C" fn create_grug_state<'a>(_mod_api_path: NTStrPtr<'a>, _mods_dir_path: NTStrPtr<'a>) -> Box<GrugState> {
		let mods_dir_path = nt!("src/grug-tests/tests");
		let mod_api_path = nt!("src/grug-tests/mod_api.json");

		let mut state = GrugInitSettings::new()
			.set_mod_api_path(mod_api_path.as_str())
			.set_mods_dir(mods_dir_path.as_str())
			.set_runtime_error_handler(|kind, msg, fn_name, script_path| {
				let mut msg = String::from(msg);
				msg.push('\0');
				let mut fn_name = String::from(fn_name);
				fn_name.push('\0');
				let mut script_path = String::from(script_path);
				script_path.push('\0');
				unsafe{
					grug_tests_runtime_error_handler (
					NTStrPtr::from_str_unchecked(&msg),
					kind as i32,
					NTStrPtr::from_str_unchecked(&fn_name),
					NTStrPtr::from_str_unchecked(&script_path),
				)};
			})
			.set_backend(BytecodeBackend::new())
			.build_state().unwrap();
		// register_game_functions(&mut state);
		super::game_fn_bindings::register_game_functions(&mut state);
		// let game_functions = get_game_functions();
		assert!(state.all_game_fns_registered());
		Box::new(state)
	}

	pub extern "C" fn destroy_grug_state<'a>(_state: Box<GrugState>) { }

	pub extern "C" fn compile_grug_file(state: &mut GrugState, path: NTStrPtr<'static>) -> Option<NTStrPtr<'static>> {
		unsafe {
			state.clear_entities();
			let path = path.to_ntstr();
			CURRENT_PATH = Some(path);
			match state.compile_grug_file(path) {
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

	pub extern "C" fn init_globals_fn_dispatcher (state: &'static GrugState, ) {
		unsafe {
			state.clear_error();
			state.set_next_entity_id(42);
			CURRENT_GRUG_ENTITY = Some(
				state
					.create_entity(CURRENT_SCRIPT_ID.unwrap())
					.expect("runtime error")
			)
		}
	}

	#[allow(unused_variables)]
	pub extern "C" fn on_fn_dispatcher<'a> (state: &GrugState, fn_name: NTStrPtr<'a>, values: *const GrugValue) {
		unsafe {
			let fn_name = fn_name.to_ntstr();
			let entity_type = PathBuf::from(CURRENT_PATH.unwrap().as_str());
			let entity_type = entity_type
				.file_prefix().unwrap()
				.to_str().unwrap()
				.split_once("-").unwrap()
				.1;
			let fn_id = state.get_on_fn_id(entity_type, fn_name).unwrap();
			
			_ = state
				.call_on_function_raw(&CURRENT_GRUG_ENTITY.as_ref().unwrap(), fn_id, values);
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn dump_file_to_json<'a> (_state: &GrugState, input_grug_path: NTStrPtr<'a>, output_json_path: NTStrPtr<'a>) -> i32 {
		let grug_path = input_grug_path.to_ntstr();
		let json_path = output_json_path.to_ntstr();

		match serde::dump_file_to_json(grug_path, json_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn generate_file_from_json<'a> (_state: &GrugState, input_json_path: NTStrPtr<'a>, output_grug_path: NTStrPtr<'a>) -> i32 {
		let output_grug_path = output_grug_path.to_ntstr();
		let input_json_path = input_json_path.to_ntstr();

		match serde::generate_file_from_json(input_json_path, output_grug_path) {
			Ok(()) => 0,
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}
	#[allow(unused_variables)]
	pub extern "C" fn game_fn_error (state: &GrugState, msg: NTStrPtr<'static>) {
		state.set_runtime_error(RuntimeError::GameFunctionError{message: msg.to_str()});
	}

	#[allow(non_camel_case_types)]
	// pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type create_grug_state_t = for<'a> extern "C" fn(NTStrPtr<'a>, NTStrPtr<'a>) -> Box<GrugState>;
	#[allow(non_camel_case_types)]
	pub type destroy_grug_state_t = extern "C" fn(Box<GrugState>);
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(&mut GrugState, NTStrPtr<'static>) -> Option<NTStrPtr<'static>>;
	#[allow(non_camel_case_types)]
	pub type init_globals_fn_dispatcher_t = extern "C" fn (&'static GrugState);
	#[allow(non_camel_case_types)]
	pub type on_fn_dispatcher_t = for<'a> extern "C" fn (&GrugState, NTStrPtr<'a>, *const GrugValue);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = for<'a> extern "C" fn (&GrugState, NTStrPtr<'a>, NTStrPtr<'a>) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = for<'a> extern "C" fn (&GrugState, NTStrPtr<'a>, NTStrPtr<'a>) -> i32;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (&GrugState, NTStrPtr<'static>);

	#[link(name="tests", kind="dylib")]
	unsafe extern "C" {
		pub fn grug_tests_runtime_error_handler<'a>(
			reason: NTStrPtr<'a>,
			ty: i32,
			on_fn_name: NTStrPtr<'a>,
			on_fn_path: NTStrPtr<'a>,
		);
		#[allow(improper_ctypes)]
		pub fn grug_tests_run(
			tests_dir_path_: NTStrPtr<'static>, 
			mod_api_path: NTStrPtr<'static>, 
			create_grug_state: create_grug_state_t,
			destroy_grug_state: destroy_grug_state_t,
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
	#[allow(improper_ctypes)]
	unsafe extern "C" {
		safe fn game_fn_nothing              <'a>(state: &'a GrugState, );
		safe fn game_fn_magic                <'a>(state: &'a GrugState, ) -> GrugValue;
		safe fn game_fn_initialize           <'a>(state: &'a GrugState, values: *const GrugValue);
		safe fn game_fn_initialize_bool      <'a>(state: &'a GrugState, values: *const GrugValue);
		safe fn game_fn_identity             <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_max                  <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_say                  <'a>(state: &'a GrugState, values: *const GrugValue);
		safe fn game_fn_sin                  <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_cos                  <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_mega                 <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_get_false            <'a>(state: &'a GrugState, ) -> GrugValue;
        safe fn game_fn_set_is_happy         <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_mega_f32             <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_mega_i32             <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_draw                 <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_blocked_alrm         <'a>(state: &'a GrugState, );
        safe fn game_fn_spawn                <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_has_resource         <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_entity           <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_string           <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_opponent         <'a>(state: &'a GrugState, ) -> GrugValue;
        safe fn game_fn_set_d                <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_get_os               <'a>(state: &'a GrugState);
        safe fn game_fn_set_opponent         <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_motherload           <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_motherload_subless   <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_offset_32_bit_f32    <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_offset_32_bit_i32    <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_offset_32_bit_string <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_talk                 <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_get_position         <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_position         <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_cause_game_fn_error  <'a>(state: &'a GrugState, );
        safe fn game_fn_call_on_b_fn         <'a>(state: &'a GrugState, );
        safe fn game_fn_store                <'a>(state: &'a GrugState, values: *const GrugValue);
        safe fn game_fn_retrieve             <'a>(state: &'a GrugState, ) -> GrugValue;
        safe fn game_fn_box_number           <'a>(state: &'a GrugState, values: *const GrugValue) -> GrugValue;
	}
	pub fn register_game_functions(state: &mut GrugState) {
		_ = state.register_game_fn("nothing",              game_fn_nothing              as for<'a>extern "C" fn(&'a GrugState, ));
		_ = state.register_game_fn("magic",                game_fn_magic                as for<'a>extern "C" fn(&'a GrugState, ) -> _);
		_ = state.register_game_fn("initialize",           game_fn_initialize           as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("initialize_bool",      game_fn_initialize_bool      as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("identity",             game_fn_identity             as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("max",                  game_fn_max                  as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("say",                  game_fn_say                  as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("sin",                  game_fn_sin                  as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("cos",                  game_fn_cos                  as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("mega",                 game_fn_mega                 as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("get_false",            game_fn_get_false            as for<'a>extern "C" fn(&'a GrugState, ) -> _);
		_ = state.register_game_fn("set_is_happy",         game_fn_set_is_happy         as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("mega_f32",             game_fn_mega_f32             as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("mega_i32",             game_fn_mega_i32             as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("draw",                 game_fn_draw                 as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("blocked_alrm",         game_fn_blocked_alrm         as for<'a>extern "C" fn(&'a GrugState, ));
		_ = state.register_game_fn("spawn",                game_fn_spawn                as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("has_resource",         game_fn_has_resource         as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("has_entity",           game_fn_has_entity           as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("has_string",           game_fn_has_string           as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("get_opponent",         game_fn_get_opponent         as for<'a>extern "C" fn(&'a GrugState, ) -> _);
		_ = state.register_game_fn("set_d",                game_fn_set_d                as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("get_os",               game_fn_get_os               as for<'a>extern "C" fn(&'a GrugState, ));
		_ = state.register_game_fn("set_opponent",         game_fn_set_opponent         as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("motherload",           game_fn_motherload           as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("motherload_subless",   game_fn_motherload_subless   as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("offset_32_bit_f32",    game_fn_offset_32_bit_f32    as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("offset_32_bit_i32",    game_fn_offset_32_bit_i32    as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("offset_32_bit_string", game_fn_offset_32_bit_string as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("talk",                 game_fn_talk                 as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("get_position",         game_fn_get_position         as for<'a>extern "C" fn(&'a GrugState, _) -> _);
		_ = state.register_game_fn("set_position",         game_fn_set_position         as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("cause_game_fn_error",  game_fn_cause_game_fn_error  as for<'a>extern "C" fn(&'a GrugState, ));
		_ = state.register_game_fn("call_on_b_fn",         game_fn_call_on_b_fn         as for<'a>extern "C" fn(&'a GrugState, ));
		_ = state.register_game_fn("store",                game_fn_store                as for<'a>extern "C" fn(&'a GrugState, _));
		_ = state.register_game_fn("retrieve",             game_fn_retrieve             as for<'a>extern "C" fn(&'a GrugState, ) -> _);
		_ = state.register_game_fn("box_number",           game_fn_box_number           as for<'a>extern "C" fn(&'a GrugState, _) -> _);
	}
}
use std::io::Write;

#[test]
fn grug_tests () {
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
	let mod_api_path = nt!("src/grug-tests/mod_api.json");

	std::panic::set_hook(Box::new(|info| {
		_ = std::io::stdout().write_fmt(
			format_args!("{}: {}\n", info.location().unwrap(), info.payload_as_str().unwrap_or("No info"))
		);
		std::process::exit(2);
	}));
	unsafe {
		grug_tests_run(
			grug_tests_path.as_ntstrptr(),
			mod_api_path.as_ntstrptr(),
			create_grug_state,
			destroy_grug_state,
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
