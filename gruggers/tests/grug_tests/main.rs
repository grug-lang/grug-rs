#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::ntstring::NTStr;
use gruggers::nt;

mod test_bindings {
	use gruggers::state::{GrugInitSettings, GrugState, State, GrugEntityHandle, Files};
	use gruggers::backend::BytecodeBackend;
	use gruggers_core::runtime_error::RuntimeError;
	use gruggers::types::{GrugValue, GrugFileId};
	use gruggers::ntstring::{NTStrPtr, NTStr, NTBytes};
	use gruggers::serde;

	use std::path::Path;
	use std::ffi::OsStr;

	static mut CURRENT_ENTITY: Option<GrugEntityHandle<'static>> = None;

	pub extern "C" fn create_grug_state<'a>(mod_api_path: NTBytes<'a>, mods_dir_path: NTBytes<'a>) -> Option<Box<(GrugState, Files)>> {
		let mut state = GrugInitSettings::new()
			.set_mod_api_path(unsafe{OsStr::from_encoded_bytes_unchecked(mod_api_path.to_bytes())})
			.set_mods_dir(unsafe{OsStr::from_encoded_bytes_unchecked(mods_dir_path.to_bytes())})
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
			.build_state().ok()?;
		super::game_fn_bindings::register_game_functions(&mut state).ok()?;
		let files = state.compile_all_files();
		// let files = Vec::new();
		Some(Box::new((state, files)))
	}

	pub extern "C" fn destroy_grug_state<'a>(_state: Box<(GrugState, Files)>) { }

	pub extern "C" fn compile_grug_file((_state, files): &(GrugState, Files), path: NTBytes<'static>, err_out: &mut Option<NTStrPtr<'static>>) -> GrugFileId {
		let path = path.to_bytes();

		fn compare_paths_normalized(first: &[u8], second: &[u8]) -> bool {
			first.iter().zip(second).all(|(first, second)| first == second || ((*first == b'/' || *first == b'\\') && (*second == b'/' || *second == b'\\')))
		}

		for file in files.files() {
			if compare_paths_normalized(file.path().as_os_str().as_encoded_bytes(), path){
				match &file.result() {
					Ok(id) => {
						*err_out = None;
						return *id;
					},
					Err(err) => {
						let mut string = format!("{}", err);
						string.push('\0');
						*err_out = Some(NTStr::try_from_str(String::leak(string)).unwrap().as_ntstrptr());
						return GrugFileId::new(u64::MAX);
					}
				}
			}
		}
		panic!("file not found: {}", unsafe{OsStr::from_encoded_bytes_unchecked(path).display()});
		// match state.compile_grug_file(unsafe{OsStr::from_encoded_bytes_unchecked(path)}) {
		// 	Ok(id) => {
		// 		*err_out = None;
		// 		return id;
		// 	}
		// 	Err(err) => {
		// 		let mut string = format!("{}", err);
		// 		string.push('\0');
		// 		*err_out = Some(NTStr::try_from_str(String::leak(string)).unwrap().as_ntstrptr());
		// 		return GrugFileId::new(u64::MAX);
		// 	}
		// }
	}

	pub extern "C" fn init_globals ((state, _): &(GrugState, Files), file_id: GrugFileId) {
		unsafe{state.set_next_entity_id(42)};
		unsafe{&mut * &raw mut CURRENT_ENTITY}.take().map(|entity| state.destroy_entity(entity));
		unsafe{CURRENT_ENTITY = std::mem::transmute::<Option<GrugEntityHandle<'_>>, Option<GrugEntityHandle<'static>>>(state.
			create_entity(file_id))};
	}

	#[allow(unused_variables)]
	pub extern "C" fn call_export_fn<'a> ((state, _): &(GrugState, Files), file_id: GrugFileId, fn_name: NTStrPtr<'a>, args: *const GrugValue, args_count: usize) {
		state.clear_error();
		unsafe{state.set_next_entity_id(42)};

		let fn_name = fn_name.to_ntstr();
		let entity_type = Path::new(state.get_script_path(file_id)
			.expect("input file_id is invalid"))
			.file_prefix().unwrap()
			.to_str().unwrap()
			.split_once("-").unwrap()
			.1;

		let fn_id = state.get_export_fn_id(entity_type, fn_name).unwrap();
		let entity = unsafe{(&*&raw const CURRENT_ENTITY).as_ref()}
			.expect("called init_globals already");
		let args = if args_count == 0 {&[]} else {unsafe{std::slice::from_raw_parts(args, args_count)}};
		_ = state
			.call_on_function(&*entity, fn_id, args);
	}

	#[allow(unused_variables)]
	pub extern "C" fn dump_file_to_json<'a> (_state: &(GrugState, Files), input_grug_file: NTStrPtr<'a>, output_buffer: *mut u8, output_buffer_len: usize) -> i32 {
		let grug_file = input_grug_file.to_ntstr();

		match serde::dump_file_to_json(grug_file, "") {
			Ok(string) => {
				if string.len() + 1 >= output_buffer_len {
					return 1;
				} else {
					unsafe{string.as_ptr().copy_to(output_buffer, string.len())};
					unsafe{output_buffer.add(string.len()).write(b'\0')};
				}
				0
			},
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}

	pub extern "C" fn generate_file_from_json<'a> (_state: &(GrugState, Files), input_json: NTStrPtr<'a>, output_buffer: *mut u8, output_buffer_len: usize) -> i32 {
		let input_json = input_json.to_ntstr();

		match serde::generate_file_from_json(input_json) {
			Ok(string) => {
				if string.len() + 1 >= output_buffer_len {
					return 1;
				} else {
					unsafe{string.as_ptr().copy_to(output_buffer, string.len())};
					unsafe{output_buffer.add(string.len()).write(b'\0')};
				}
				0
			},
			Err(err) => {
				eprintln!("{}", err);
				1
			}
		}
	}

	pub extern "C" fn game_fn_error ((state, _): &(GrugState, Files), msg: NTStrPtr<'static>) {
		state.set_runtime_error(RuntimeError::GameFunctionError{message: msg.to_str()});
	}

	#[allow(non_camel_case_types)]
	// pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type create_grug_state_t = for<'a> extern "C" fn(NTBytes<'a>, NTBytes<'a>) -> Option<Box<(GrugState, Files)>>;
	#[allow(non_camel_case_types)]
	pub type destroy_grug_state_t = extern "C" fn(Box<(GrugState, Files)>);
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = extern "C" fn(&(GrugState, Files), NTBytes<'static>, &mut Option<NTStrPtr<'static>>) -> GrugFileId;
	#[allow(non_camel_case_types)]
	pub type init_globals_t = extern "C" fn (&'_ (GrugState, Files), GrugFileId);
	#[allow(non_camel_case_types)]
	pub type call_export_fn_t = for<'a> extern "C" fn (&(GrugState, Files), GrugFileId, NTStrPtr<'a>, *const GrugValue, usize);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = for<'a> extern "C" fn (&(GrugState, Files), NTStrPtr<'a>, *mut u8, usize) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = for<'a> extern "C" fn (&(GrugState, Files), NTStrPtr<'a>, *mut u8, usize) -> i32;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (&(GrugState, Files), NTStrPtr<'static>);

	#[repr(C)]
	pub struct GrugStateVTable {
		create_grug_state: create_grug_state_t,
		destroy_grug_state: destroy_grug_state_t,
		compile_grug_file: compile_grug_file_t,
		init_globals: init_globals_t,
		call_export_fn: call_export_fn_t,
		dump_file_to_json: dump_file_to_json_t,
		generate_file_from_json: generate_file_from_json_t,
		game_fn_error: game_fn_error_t,
	}

	pub const STATE_VTABLE: GrugStateVTable = GrugStateVTable {
		create_grug_state,
		destroy_grug_state,
		compile_grug_file,
		init_globals,
		call_export_fn,
		dump_file_to_json,
		generate_file_from_json,
		game_fn_error,
	};

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
			vtable: GrugStateVTable,
			whitelisted_test_: Option<NTStrPtr<'static>>
		);
	}
}
use test_bindings::*;

mod game_fn_bindings {
	use gruggers::types::GrugValue;
	use gruggers::state::GrugState;
	use gruggers::error::Error;
	#[link(name = "tests", kind="dylib")]
	#[allow(improper_ctypes)]
	unsafe extern "C" {
		safe fn game_fn_nothing              <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_magic                <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_initialize           <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_initialize_bool      <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_identity             <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_max                  <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_say                  <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_sin                  <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
		safe fn game_fn_cos                  <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_mega                 <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_false            <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_is_happy         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_mega_f32             <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_mega_i32             <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_draw                 <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_blocked_alrm         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_spawn                <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_spawn_d              <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_resource         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_entity           <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_has_string           <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_opponent         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_d                <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_os               <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_opponent         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_motherload           <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_motherload_subless   <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_offset_32_bit_f32    <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_offset_32_bit_i32    <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_offset_32_bit_string <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_talk                 <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_get_position         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_set_position         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_cause_game_fn_error  <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_call_on_b_fn         <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_store                <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_retrieve             <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_box_number           <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
        safe fn game_fn_print_csv            <'a>(data: &'static (), state: &'a GrugState, values: *const GrugValue) -> GrugValue;
	}
	pub fn register_game_functions(state: &mut GrugState) -> Result<(), Error> { unsafe {
		state.register_host_fn_raw("nothing",              game_fn_nothing             , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("magic",                game_fn_magic               , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("initialize",           game_fn_initialize          , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("initialize_bool",      game_fn_initialize_bool     , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("identity",             game_fn_identity            , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("max",                  game_fn_max                 , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("say",                  game_fn_say                 , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("sin",                  game_fn_sin                 , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("cos",                  game_fn_cos                 , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("mega",                 game_fn_mega                , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("get_false",            game_fn_get_false           , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("set_is_happy",         game_fn_set_is_happy        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("mega_f32",             game_fn_mega_f32            , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("mega_i32",             game_fn_mega_i32            , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("draw",                 game_fn_draw                , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("blocked_alrm",         game_fn_blocked_alrm        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("spawn",                game_fn_spawn               , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("spawn_d",              game_fn_spawn_d             , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("has_resource",         game_fn_has_resource        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("has_entity",           game_fn_has_entity          , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("has_string",           game_fn_has_string          , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("get_opponent",         game_fn_get_opponent        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("set_d",                game_fn_set_d               , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("get_os",               game_fn_get_os              , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("set_opponent",         game_fn_set_opponent        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("motherload",           game_fn_motherload          , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("motherload_subless",   game_fn_motherload_subless  , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("offset_32_bit_f32",    game_fn_offset_32_bit_f32   , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("offset_32_bit_i32",    game_fn_offset_32_bit_i32   , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("offset_32_bit_string", game_fn_offset_32_bit_string, std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("talk",                 game_fn_talk                , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("get_position",         game_fn_get_position        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("set_position",         game_fn_set_position        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("cause_game_fn_error",  game_fn_cause_game_fn_error , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("call_on_b_fn",         game_fn_call_on_b_fn        , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("store",                game_fn_store               , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("retrieve",             game_fn_retrieve            , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("box_number",           game_fn_box_number          , std::ptr::NonNull::dangling(), None)?; 
		state.register_host_fn_raw("print_csv",            game_fn_print_csv           , std::ptr::NonNull::dangling(), None)?; 
		Ok(())
	}}
}
use std::io::Write;

#[test]
fn grug_tests () {
	let mut args = std::env::args().collect::<Vec<_>>();

	let mut whitelisted_test = None;
	if args.len() >= 3 {
		let mut test = args.remove(2);
		if !test.starts_with("--") {
			test.push('\0');
			whitelisted_test = unsafe{Some(NTStr::from_str_unchecked(String::leak(test)).as_ntstrptr())};
		}
	};

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
			STATE_VTABLE,
			whitelisted_test,
		)
	}
	_ = std::panic::take_hook();
}
