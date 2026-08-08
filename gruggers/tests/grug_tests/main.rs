#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::ntstring::NTStr;
use gruggers::nt;

mod test_bindings {
	use gruggers::state::{GrugInitSettings, GrugState, State, GrugEntityHandle};
	use gruggers::backend::{BytecodeBackend, StubBackend};
	use gruggers_core::runtime_error::RuntimeError;
	use gruggers::types::{Value, FileId};
	use gruggers::ntstring::{NTStrPtr, NTBytes, NTStr};
	use gruggers::serde;
	use gruggers::nt;
	use gruggers::arena::Arena;

	use std::path::Path;
	use std::ffi::OsStr;

	type CState = (GrugState, Arena);

	pub extern "C" fn parse_mod_api(mod_api_path: NTBytes) -> Option<NTBytes<'static>>{
		let state = GrugInitSettings::new()
			.set_mod_api_path(unsafe{OsStr::from_encoded_bytes_unchecked(mod_api_path.to_bytes())})
			.set_mods_dir(unsafe{OsStr::from_encoded_bytes_unchecked(b".")})
			.set_backend(StubBackend)
			.build_state();
		match state {
			Ok(_) => None,
			Err(err) => {
				let return_str: &NTStr = (&*format!("{}\0", err).leak()).try_into().unwrap();
				
				Some(return_str.into())
			}
		}
	}

	pub extern "C" fn create_grug_state<'a>(mod_api_path: NTBytes<'a>, mods_dir_path: NTBytes<'a>, _unsafe_mode: bool) -> Option<Box<CState>> {
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
			.build_state().map_err(|err| {println!("{}", err); err}).ok()?;
		super::game_fn_bindings::register_game_functions(&mut state).map_err(|err| {println!("{}", err); err}).ok()?;
		state.all_host_fns_registered().map_err(|err| {println!("{}", err); err}).ok()?;
		Some(Box::new((state, Arena::new())))
	}

	pub extern "C" fn destroy_grug_state<'a>(_state: Box<CState>) { }

	pub extern "C" fn compile_grug_file<'a>(cstate: &'a CState, path: NTBytes<'_>, err_out: &'_ mut Option<NTStrPtr<'a>>) -> FileId {
		let path = path.to_bytes();
		let (state, arena) = cstate;
		
		// SAFETY: Bite me
		match state.compile_grug_file(unsafe{OsStr::from_encoded_bytes_unchecked(path)}) {
			Ok(id) => {
				*err_out = None;
				return id;
			}
			Err(err) => {
				*err_out = Some(arena.copy_str_into_nt(err.inner().error_string.to_str()).as_ntstrptr());
				return FileId::new(u64::MAX);
			}
		}
	}

	// This is actually a noop
	pub extern "C" fn destroy_grug_file(_: &CState, _file_id: FileId) {}

	pub extern "C" fn create_entity<'a>((state, _): &'a CState, file_id: FileId, err_out: &'_ mut Option<NTStrPtr<'a>>) -> Option<GrugEntityHandle<'a>> {
		unsafe{state.set_next_entity_id(42)};
		match state.create_entity(file_id) {
			Some(entity) => {*err_out = None; Some(entity)},
			None => {
				*err_out = Some(nt!("Some error occurred while creating entity").as_ntstrptr());
				None
			}
		}
	}

	pub extern "C" fn destroy_entity<'a>((state, _): &'a CState, handle: GrugEntityHandle<'a>) {
		state.destroy_entity(handle);
	}

	pub extern "C" fn update<'a>((state, arena): &'a CState, err_out: &mut Option<NTStrPtr<'a>>) {
		std::thread::sleep(std::time::Duration::from_micros(1));
		match state.update_files() {
			(_, updated_files) => {
				// for each file in the `updated_files`, find it in `files`,
				// copy its contents over to the `files`'s arena and replace it
				for updated_file in updated_files.files() {
					if let Err(err) = updated_file.result() {
						*err_out = Some(arena.copy_str_into_nt(err.error_string.to_str()).as_ntstrptr());
						return;
					}
				}
			},
		}
		*err_out = None;
	}

	#[allow(unused_variables)]
	pub extern "C" fn call_export_fn<'a> ((state, _): &CState, entity: GrugEntityHandle<'a>, fn_name: NTStrPtr<'a>, args: *const Value, args_count: usize) {
		state.clear_error();
		unsafe{state.set_next_entity_id(42)};

		let fn_name = fn_name.to_ntstr();
		let entity_type = Path::new(state.get_script_path_rel(entity.file_id)
			.expect("input file_id is invalid"))
			.file_prefix().unwrap()
			.to_str().unwrap()
			.split_once("-").unwrap()
			.1;

		let fn_id = state.get_export_fn_id(entity_type, fn_name).unwrap();
		let args = if args_count == 0 {&[]} else {unsafe{std::slice::from_raw_parts(args, args_count)}};
		_ = state
			.call_export_fn(&*entity, fn_id, args);
	}

	#[allow(unused_variables)]
	pub extern "C" fn dump_file_to_json<'a> (_state: &CState, input_grug_file: NTStrPtr<'a>, output_buffer: *mut u8, output_buffer_len: usize) -> i32 {
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

	pub extern "C" fn generate_file_from_json<'a> (_state: &CState, input_json: NTStrPtr<'a>, output_buffer: *mut u8, output_buffer_len: usize) -> i32 {
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

	pub extern "C" fn game_fn_error ((state, _): &CState, msg: NTStrPtr<'static>) {
		state.set_runtime_error(RuntimeError::GameFunctionError{message: msg.to_str()});
	}

	#[allow(non_camel_case_types)]
	// pub type c_size_t = u64;
	#[allow(non_camel_case_types)]
	pub type parse_mod_api_t = for<'a> extern "C" fn(NTBytes<'a>) -> Option<NTBytes<'static>>;
	#[allow(non_camel_case_types)]
	pub type create_grug_state_t = for<'a> extern "C" fn(NTBytes<'a>, NTBytes<'a>, bool) -> Option<Box<CState>>;
	#[allow(non_camel_case_types)]
	pub type destroy_grug_state_t = extern "C" fn(Box<CState>);
	#[allow(non_camel_case_types)]
	pub type compile_grug_file_t = for<'a> extern "C" fn(&'a CState, NTBytes<'_>, &mut Option<NTStrPtr<'a>>) -> FileId;
	#[allow(non_camel_case_types)]
	pub type destroy_grug_file_t = extern "C" fn(&'_ CState, FileId);
	#[allow(non_camel_case_types)]
	pub type create_entity_t = for<'a> extern "C" fn(&'a CState, FileId, &mut Option<NTStrPtr<'a>>) -> Option<GrugEntityHandle<'a>>;
	#[allow(non_camel_case_types)]
	pub type destroy_entity_t = for<'a> extern "C" fn(&'a CState, GrugEntityHandle<'a>);
	#[allow(non_camel_case_types)]
	pub type update_t = for<'a> extern "C" fn (&'a CState, &mut Option<NTStrPtr<'a>>);
	#[allow(non_camel_case_types)]
	pub type call_export_fn_t = for<'a> extern "C" fn (&'a CState, GrugEntityHandle<'a>, NTStrPtr<'_>, *const Value, usize);
	#[allow(non_camel_case_types)]
	pub type dump_file_to_json_t = extern "C" fn (&CState, NTStrPtr<'_>, *mut u8, usize) -> i32;
	#[allow(non_camel_case_types)]
	pub type generate_file_from_json_t = extern "C" fn (&CState, NTStrPtr<'_>, *mut u8, usize) -> i32;
	#[allow(non_camel_case_types)]
	pub type game_fn_error_t = extern "C" fn (&CState, NTStrPtr<'static>);

	#[repr(C)]
	pub struct GrugStateVTable {
		parse_mod_api: parse_mod_api_t,
		create_grug_state: create_grug_state_t,
		destroy_grug_state: destroy_grug_state_t,
		compile_grug_file: compile_grug_file_t,
		destroy_grug_file: destroy_grug_file_t,
		create_entity: create_entity_t,
		destroy_entity: destroy_entity_t,
		update: update_t,
		call_export_fn: call_export_fn_t,
		dump_file_to_json: dump_file_to_json_t,
		generate_file_from_json: generate_file_from_json_t,
		game_fn_error: game_fn_error_t,
	}

	pub const STATE_VTABLE: GrugStateVTable = GrugStateVTable {
		parse_mod_api,
		create_grug_state,
		destroy_grug_state,
		compile_grug_file,
		destroy_grug_file,
		create_entity,
		destroy_entity,
		update,
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
	use gruggers::types::{Value, HostFnWithState};
	use gruggers::ast::Type;
	use gruggers::state::GrugState;
	use gruggers::error::Error;
	#[link(name = "tests", kind="dylib")]
	#[allow(improper_ctypes)]
	unsafe extern "C" {
		safe fn game_fn_nothing                       <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_magic                         <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_initialize                    <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_initialize_bool               <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_identity                      <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_max                           <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_say                           <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_sin                           <'a>(state: &'a GrugState, values: *const Value) -> Value;
		safe fn game_fn_cos                           <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_mega                          <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_get_false                     <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_set_is_happy                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_mega_f32                      <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_mega_i32                      <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_draw                          <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_assert_state_is_not_null      <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_blocked_alrm                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_spawn                         <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_spawn_d                       <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_has_resource                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_has_entity                    <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_has_string                    <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_get_opponent                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_set_d                         <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_get_os                        <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_set_opponent                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_motherload                    <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_motherload_subless            <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_offset_32_bit_f32             <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_offset_32_bit_i32             <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_offset_32_bit_string          <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_talk                          <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_get_position                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_set_position                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_cause_game_fn_error           <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_call_on_b_fn                  <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_call_on_b_fn_number           <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_store                         <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_retrieve                      <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_box_number                    <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_print_csv                     <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_vec_number_new                <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_vec_number_push               <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_vec_number_pop                <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_vec_number_insert             <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_utils                         <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_Utils_assert_state_is_not_null<'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_Utils_cause_game_fn_error     <'a>(state: &'a GrugState, values: *const Value) -> Value;
        safe fn game_fn_Utils_call_on_b_fn            <'a>(state: &'a GrugState, values: *const Value) -> Value;

		safe fn reg_game_fn_vec_new                           (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_vec_push                          (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_vec_pop                           (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_vec_insert                        (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;

		safe fn reg_game_fn_box                               (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_box_get                           (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_box_set                           (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		
		safe fn reg_game_fn_default                           (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;

		safe fn reg_game_fn_dict                              (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_dict_from_vec                     (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_dict_put                          (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;

		safe fn reg_game_fn_cause_game_fn_error_generic       (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_Utils_cause_game_fn_error_generic (types: &[Type;1]) -> Option<HostFnWithState<GrugState>>;

		safe fn reg_game_fn_make_pair                         (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_pair_first                        (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;
		safe fn reg_game_fn_pair_second                       (types: &[Type;2]) -> Option<HostFnWithState<GrugState>>;
	}
	pub fn register_game_functions(state: &mut GrugState) -> Result<(), Error> { unsafe {
		state.register_host_fn("nothing",                  game_fn_nothing             )?; 
		state.register_host_fn("magic",                    game_fn_magic               )?; 
		state.register_host_fn("initialize",               game_fn_initialize          )?; 
		state.register_host_fn("initialize_bool",          game_fn_initialize_bool     )?; 
		state.register_host_fn("identity",                 game_fn_identity            )?; 
		state.register_host_fn("max",                      game_fn_max                 )?; 
		state.register_host_fn("say",                      game_fn_say                 )?; 
		state.register_host_fn("sin",                      game_fn_sin                 )?; 
		state.register_host_fn("cos",                      game_fn_cos                 )?; 
		state.register_host_fn("mega",                     game_fn_mega                )?; 
		state.register_host_fn("get_false",                game_fn_get_false           )?; 
		state.register_host_fn("set_is_happy",             game_fn_set_is_happy        )?; 
		state.register_host_fn("mega_f32",                 game_fn_mega_f32            )?; 
		state.register_host_fn("mega_i32",                 game_fn_mega_i32            )?; 
		state.register_host_fn("draw",                     game_fn_draw                )?; 
		state.register_host_fn("assert_state_is_not_null", game_fn_assert_state_is_not_null)?; 
		state.register_host_fn("blocked_alrm",             game_fn_blocked_alrm        )?; 
		state.register_host_fn("spawn",                    game_fn_spawn               )?; 
		state.register_host_fn("spawn_d",                  game_fn_spawn_d             )?; 
		state.register_host_fn("has_resource",             game_fn_has_resource        )?; 
		state.register_host_fn("has_entity",               game_fn_has_entity          )?; 
		state.register_host_fn("has_string",               game_fn_has_string          )?; 
		state.register_host_fn("get_opponent",             game_fn_get_opponent        )?; 
		state.register_host_fn("set_d",                    game_fn_set_d               )?; 
		state.register_host_fn("get_os",                   game_fn_get_os              )?; 
		state.register_host_fn("set_opponent",             game_fn_set_opponent        )?; 
		state.register_host_fn("motherload",               game_fn_motherload          )?; 
		state.register_host_fn("motherload_subless",       game_fn_motherload_subless  )?; 
		state.register_host_fn("offset_32_bit_f32",        game_fn_offset_32_bit_f32   )?; 
		state.register_host_fn("offset_32_bit_i32",        game_fn_offset_32_bit_i32   )?; 
		state.register_host_fn("offset_32_bit_string",     game_fn_offset_32_bit_string)?; 
		state.register_host_fn("talk",                     game_fn_talk                )?; 
		state.register_host_fn("get_position",             game_fn_get_position        )?; 
		state.register_host_fn("set_position",             game_fn_set_position        )?; 
		state.register_host_fn("cause_game_fn_error",      game_fn_cause_game_fn_error )?; 
		state.register_host_fn("call_on_b_fn",             game_fn_call_on_b_fn        )?; 
		state.register_host_fn("call_on_b_fn_number",      game_fn_call_on_b_fn_number )?; 
		state.register_host_fn("store",                    game_fn_store               )?; 
		state.register_host_fn("retrieve",                 game_fn_retrieve            )?; 
		state.register_host_fn("box_number",               game_fn_box_number          )?; 
		state.register_host_fn("print_csv",                game_fn_print_csv           )?; 
		state.register_host_fn("vec_number_new",           game_fn_vec_number_new      )?; 
		state.register_host_fn("utils",                    game_fn_utils               )?; 
		state.register_method("VecNumber", "push",   game_fn_vec_number_push     )?; 
		state.register_method("VecNumber", "pop",    game_fn_vec_number_pop      )?; 
		state.register_method("VecNumber", "insert", game_fn_vec_number_insert   )?; 

		state.register_method("Utils", "assert_state_is_not_null", game_fn_Utils_assert_state_is_not_null)?; 
		state.register_method("Utils", "cause_game_fn_error",      game_fn_Utils_cause_game_fn_error     )?; 
		state.register_method("Utils", "call_on_b_fn",             game_fn_Utils_call_on_b_fn           )?; 
		state.register_generic_method("Utils", "cause_game_fn_error_generic", reg_game_fn_Utils_cause_game_fn_error_generic)?; 

		state.register_generic_fn("vec", reg_game_fn_vec_new)?;
		state.register_generic_method("Vec", "push"  , reg_game_fn_vec_push  )?; 
		state.register_generic_method("Vec", "pop"   , reg_game_fn_vec_pop   )?; 
		state.register_generic_method("Vec", "insert", reg_game_fn_vec_insert)?; 

		state.register_generic_fn("box", reg_game_fn_box)?;
		state.register_generic_method("Box", "set"   , reg_game_fn_box_set   )?; 
		state.register_generic_method("Box", "get"   , reg_game_fn_box_get   )?; 

		state.register_generic_fn("make_pair",                   reg_game_fn_make_pair)?; 
		state.register_generic_method("Pair", "first" ,          reg_game_fn_pair_first)?; 
		state.register_generic_method("Pair", "second",          reg_game_fn_pair_second)?; 

		state.register_generic_fn("dict",                        reg_game_fn_dict)?; 
		state.register_generic_fn("dict_from_vec",               reg_game_fn_dict_from_vec)?; 
		state.register_generic_method("Dict", "put"   , reg_game_fn_dict_put)?; 

		state.register_generic_fn("default", reg_game_fn_default)?; 
		state.register_generic_fn("cause_game_fn_error_generic", reg_game_fn_cause_game_fn_error_generic)?; 

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
