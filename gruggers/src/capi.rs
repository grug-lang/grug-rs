#![allow(improper_ctypes_definitions)]
use crate::state::{GrugInitSettings, GrugState};
use crate::ntstring::NTStrPtr;
use crate::types::{GameFnPtrVoidArgless, GameFnPtrVoid, GameFnPtrValue, GameFnPtrValueArgless, GrugScriptId, GrugEntityHandle};

#[unsafe(no_mangle)]
pub extern "C" fn grug_init(settings: GrugInitSettings) -> Option<Box<GrugState>> {
	Some(Box::new(settings.build_state().ok()?))
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_register_game_fn_void_argless(state: &mut GrugState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrVoidArgless<GrugState>) -> bool {
	state.register_game_fn(game_fn_name.to_str(), func).is_ok()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_register_game_fn_void(state: &mut GrugState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrVoid<GrugState>) -> bool {
	state.register_game_fn(game_fn_name.to_str(), func).is_ok()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_register_game_fn_value_argless(state: &mut GrugState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrValueArgless<GrugState>) -> bool {
	state.register_game_fn(game_fn_name.to_str(), func).is_ok()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_register_game_fn_value(state: &mut GrugState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrValue<GrugState>) -> bool {
	state.register_game_fn(game_fn_name.to_str(), func).is_ok()
}

const INVALID_GRUG_SCRIPT_ID: GrugScriptId = GrugScriptId::new(u64::MAX);

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_file(state: &GrugState, file_path: NTStrPtr<'_>) -> GrugScriptId {
	state.compile_grug_file(file_path.to_str()).unwrap_or_else(|_| INVALID_GRUG_SCRIPT_ID)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(state: &GrugState, file_id: GrugScriptId) -> Option<GrugEntityHandle<'_>> {
	state.create_entity(file_id)
}
