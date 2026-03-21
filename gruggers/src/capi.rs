#![allow(improper_ctypes_definitions)]
use crate::state::{GrugInitSettings, GrugState, GrugEntityHandle, EventFnEntry};
use crate::ntstring::NTStrPtr;
use crate::types::{GrugScriptId, GrugOnFnId, GrugEntity, GrugValue, GameFnPtrState};

#[unsafe(no_mangle)]
pub extern "C" fn grug_init(settings: GrugInitSettings) -> Option<Box<GrugState>> {
	Some(Box::new(settings.build_state().ok()?))
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit(_: Option<Box<GrugState>>) {}

#[unsafe(no_mangle)]
pub extern "C" fn grug_register_game_fn(state: &mut GrugState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrState<GrugState>) -> bool {
	state.register_game_fn(game_fn_name.to_str(), func).is_ok()
}

const INVALID_GRUG_SCRIPT_ID: GrugScriptId = GrugScriptId::new(u64::MAX);

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_file(state: &GrugState, file_path: NTStrPtr<'_>) -> GrugScriptId {
	state.compile_grug_file(file_path.to_str()).unwrap_or_else(|err| {println!("{}", err); INVALID_GRUG_SCRIPT_ID})
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(state: &GrugState, file_id: GrugScriptId) -> Option<GrugEntityHandle<'_>> {
	state.create_entity(file_id)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit_entity(state: &GrugState, handle: GrugEntityHandle<'_>) {
	state.destroy_entity(handle)
}

const INVALID_GRUG_ON_FN_ID: GrugOnFnId = u64::MAX;

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_ids(state: &GrugState) -> &[EventFnEntry<'_>] {
	state.get_on_functions()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_id(state: &GrugState, entity_type: NTStrPtr<'_>, on_fn_name: NTStrPtr<'_>) -> GrugOnFnId {
	state.get_on_fn_id(entity_type.to_str(), on_fn_name.to_str()).unwrap_or(INVALID_GRUG_ON_FN_ID)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_call_on_function(state: &GrugState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue, values_len: usize) -> bool {
	unsafe{state.call_on_function(entity, on_fn_id, std::slice::from_raw_parts(values, values_len))}
}

#[unsafe(no_mangle)]
pub fn grug_all_game_fns_registered(state: &GrugState) -> bool {
	state.all_game_fns_registered().is_ok()
}
