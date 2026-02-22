// #![deny(warnings)]
#![allow(improper_ctypes)]
mod test_bindings {
	use gruggers::ntstring::{NTStrPtr, NTStr};
	use gruggers::state::{GrugState, GrugInitSettings};
	use gruggers::backend::BytecodeBackend;
	use gruggers::types::{GrugEntityHandle, GrugEntity, GrugScriptId, GrugOnFnId, GrugValue};

	use super::game_functions::*;
	
	#[repr(C)]
	pub struct GrugVTable {
		create_grug_state: extern "C" fn(NTStrPtr<'_>, NTStrPtr<'_>) -> Box<GrugState>,
		destroy_grug_state: extern "C" fn(Box<GrugState>),

		compile_grug_file: extern "C" fn(&GrugState, NTStrPtr<'_>) -> GrugScriptId,
		create_entity: for<'a> extern "C" fn(&'a GrugState, GrugScriptId) -> GrugEntityHandle<'a>,
		get_on_fn_id: extern "C" fn(&GrugState, NTStrPtr<'_>, NTStrPtr<'_>) -> GrugOnFnId,
		call_entity_on_fn: extern "C" fn(&GrugState, &GrugEntity, GrugOnFnId, *const GrugValue, usize),
		destroy_entity: for<'a> extern "C" fn(&'a GrugState, GrugEntityHandle<'a>),
	}

	extern "C" fn create_grug_state(mod_api_path: NTStrPtr<'_>, mods_dir_path: NTStrPtr<'_>) -> Box<GrugState> {
		let mut state = GrugInitSettings::new()
			.set_mods_dir(mods_dir_path.to_str())
			.set_mod_api_path(mod_api_path.to_str())
			.set_backend(BytecodeBackend::new())
			.set_runtime_error_handler(|code, reason, fn_name, script_path| {
				let reason      = NTStr::arc_from_str(reason);
				let fn_name     = NTStr::arc_from_str(fn_name);
				let script_path = NTStr::arc_from_str(script_path);
				unsafe {
					runtime_error_handler(
						reason.as_ntstrptr(),
						code,
						fn_name.as_ntstrptr(),
						script_path.as_ntstrptr(),
					);
				}
			})
			.build_state()
			.unwrap();
		register_game_functions(&mut state);
		assert!(state.all_game_fns_registered());
		Box::new(state)
	}

	extern "C" fn destroy_grug_state(_: Box<GrugState>) {}

	extern "C" fn compile_grug_file(state: &GrugState, script_path: NTStrPtr<'_>) -> GrugScriptId {
		state.compile_grug_file(script_path.to_str()).unwrap()
	}

	extern "C" fn get_on_fn_id(state: &GrugState, entity_name: NTStrPtr<'_>, on_fn_name: NTStrPtr<'_>) -> GrugOnFnId {
		state.get_on_fn_id(entity_name.to_str(), on_fn_name.to_str())
			.unwrap()
	}

	extern "C" fn create_entity(state: &GrugState, script_id: GrugScriptId) -> GrugEntityHandle<'_> {
		state.create_entity(script_id).unwrap()
	}

	extern "C" fn destroy_entity(state: &GrugState, handle: GrugEntityHandle<'_>) {
		state.destroy_entity(handle)
	}

	extern "C" fn call_entity_on_fn(state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue, values_len: usize) {
		let values = unsafe{if values.is_null() {&[]} else {std::slice::from_raw_parts(values, values_len)}};
		assert!(state.call_on_function(
			entity, 
			on_fn_id, 
			values,
		));
	}

	pub const GRUG_VTABLE: GrugVTable = GrugVTable{
		create_grug_state,
		destroy_grug_state,

		compile_grug_file,
		create_entity,
		get_on_fn_id,
		call_entity_on_fn,
		destroy_entity,
	};
	
	
	#[link(name = "bench", kind="dylib")]
	unsafe extern "C" {
		fn runtime_error_handler<'a>(
			reason: NTStrPtr<'a>,
			error_kind: u32,
			on_fn_name: NTStrPtr<'a>,
			script_path: NTStrPtr<'a>,
		);

		pub fn grug_bench_run<'a>(
			mod_api_path: NTStrPtr<'a>,
			mods_dir_path: NTStrPtr<'a>,
			grug_vtable: &'static GrugVTable,
		);
	}
}
use test_bindings::*;

mod game_functions {
	use gruggers::state::GrugState;
	use gruggers::types::GrugValue;

	#[link(name = "bench", kind="dylib")]
	unsafe extern "C" {
		safe fn game_fn_print_number<'a>(state: &'a GrugState, arguments: *const GrugValue);
		safe fn game_fn_get_1       <'a>(state: &'a GrugState, arguments: *const GrugValue) -> GrugValue;
		safe fn game_fn_get_mass    <'a>(state: &'a GrugState, arguments: *const GrugValue) -> GrugValue;
		safe fn game_fn_get_number  <'a>(state: &'a GrugState                             ) -> GrugValue;
		safe fn game_fn_x           <'a>(state: &'a GrugState, arguments: *const GrugValue) -> GrugValue;
		safe fn game_fn_y           <'a>(state: &'a GrugState, arguments: *const GrugValue) -> GrugValue;
		safe fn game_fn_sqrt        <'a>(state: &'a GrugState, arguments: *const GrugValue) -> GrugValue;
		safe fn game_fn_set_x       <'a>(state: &'a GrugState, arguments: *const GrugValue);
		safe fn game_fn_set_y       <'a>(state: &'a GrugState, arguments: *const GrugValue);
	}

	pub fn register_game_functions(state: &mut GrugState) {
		state.register_game_fn("print_number", game_fn_print_number as for<'a> extern "C" fn(&'a _, _)     ).unwrap();
		state.register_game_fn("get_1"       , game_fn_get_1        as for<'a> extern "C" fn(&'a _, _) -> _).unwrap();
		state.register_game_fn("get_mass"    , game_fn_get_mass     as for<'a> extern "C" fn(&'a _, _) -> _).unwrap();
		state.register_game_fn("get_number"  , game_fn_get_number   as for<'a> extern "C" fn(&'a _,  ) -> _).unwrap();
		state.register_game_fn("x"           , game_fn_x            as for<'a> extern "C" fn(&'a _, _) -> _).unwrap();
		state.register_game_fn("y"           , game_fn_y            as for<'a> extern "C" fn(&'a _, _) -> _).unwrap();
		state.register_game_fn("sqrt"        , game_fn_sqrt         as for<'a> extern "C" fn(&'a _, _) -> _).unwrap();
		state.register_game_fn("set_x"       , game_fn_set_x        as for<'a> extern "C" fn(&'a _, _)     ).unwrap();
		state.register_game_fn("set_y"       , game_fn_set_y        as for<'a> extern "C" fn(&'a _, _)     ).unwrap();
	}
}

use gruggers::nt;
#[test]
fn grug_bench () {
	use std::io::Write;
	std::panic::set_hook(Box::new(|info| {
		_ = std::io::stdout().write_fmt(
			format_args!("{}: {}\n", info.location().unwrap(), info.payload_as_str().unwrap_or("No info"))
		);
		std::process::exit(2);
	}));
	unsafe {
		grug_bench_run(
			nt!("src/grug-bench/mod_api.json").as_ntstrptr(),
			nt!("src/grug-bench/mods").as_ntstrptr(),
			&GRUG_VTABLE
		);
	}
}

