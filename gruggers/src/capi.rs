//! Provides a public c api for use when compiling as a static library. 
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::state::{EventFnEntry, GrugEntityHandle, GrugInitSettings, GrugState, Files, FileInfo};
use crate::ntstring::NTStrPtr;
use crate::types::{GrugFileId, GrugOnFnId, GrugEntity, GrugValue, GameFnPtrState, INVALID_GRUG_SCRIPT_ID};
use crate::error::{Error, GrugError};

use std::ffi::OsString;
use std::cell::UnsafeCell;
use std::mem::MaybeUninit;

type CState = (GrugState, /* last error */ UnsafeCell<Option<Error>>, /* info from last compile */ UnsafeCell<Files>, /* resources from last compile */ UnsafeCell<Vec<OsString>>);

#[unsafe(no_mangle)]
pub extern "C" fn grug_default_settings() -> GrugInitSettings<'static> {
	GrugInitSettings::new()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_init(settings: GrugInitSettings, out_err: &mut MaybeUninit<GrugError<'static>>) -> Option<Box<CState>> {
	match settings.build_state() {
		Ok(state) => Some(Box::new((state, UnsafeCell::new(None), UnsafeCell::new(Files::empty()), UnsafeCell::new(vec![])))),
		Err(err) => {
			unsafe{*out_err.as_mut_ptr() = err.leak()};
			None
		}
	}
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit(_: Option<Box<CState>>) {}

/// # SAFETY
/// same as [`CState::register_host_fn`]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_host_fn<'a>(state: &'a mut CState, game_fn_name: NTStrPtr<'static>, func: GameFnPtrState<GrugState>) -> Option<&'a GrugError<'a>> {
	// SAFETY: This function is exposed to C and is inherently unsafe
	if let Err(err) = unsafe{state.0.register_host_fn(game_fn_name.to_str(), func)} {
		Some(state.1.get_mut().insert(err).inner())
	} else {
		None
	}
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_all_files(state: &CState) -> &[FileInfo<'_>] {
	let files = unsafe{&mut *state.2.get()};
	*files = state.0.compile_all_files();
	files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_update(state: &CState) -> &[FileInfo<'_>] {
	let files = unsafe{&mut *state.2.get()};
	let resources = unsafe{&mut *state.3.get()};
	(*resources, *files) = state.0.update_files();
	files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_file(state: &CState, file_path: NTStrPtr<'_>) -> GrugFileId {
	
	match state.0.compile_grug_file(file_path.to_str()) {
		Ok(id) => id,
		Err(err) => {
			unsafe{*state.1.get() = Some(err)};
			INVALID_GRUG_SCRIPT_ID
		}
	}
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(state: &CState, file_id: GrugFileId) -> Option<GrugEntityHandle<'_>> {
	state.0.create_entity(file_id)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit_entity(state: &CState, handle: GrugEntityHandle<'_>) {
	state.0.destroy_entity(handle)
}

const INVALID_GRUG_ON_FN_ID: GrugOnFnId = u64::MAX;

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_ids(state: &CState) -> &[EventFnEntry<'_>] {
	state.0.get_on_functions()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_id(state: &CState, entity_type: NTStrPtr<'_>, on_fn_name: NTStrPtr<'_>) -> GrugOnFnId {
	match state.0.get_export_fn_id(entity_type.to_str(), on_fn_name.to_str()) {
		Ok(id) => id,
		Err(err) => {
			unsafe{*state.1.get() = Some(err)};
			INVALID_GRUG_ON_FN_ID
		}
	}
}

/// # Safety
/// `values` must point to a buffer that contains at least `values_len`
/// elements
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_call_on_function(state: &CState, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue, values_len: usize) -> bool {
	unsafe{state.0.call_on_function(entity, on_fn_id, std::slice::from_raw_parts(values, values_len))}
}

#[unsafe(no_mangle)]
/// returns 1 if all host functions mentioned in the mod api have been
/// registered
///
/// returns 0 otherwise
pub extern "C" fn grug_all_host_fns_registered(state: &CState) -> bool {
	let Err(err) = state.0.all_host_fns_registered() else {
		return true;
	};
	unsafe{*state.1.get() = Some(err)};
	false
}

/// 
#[unsafe(no_mangle)]
pub extern "C" fn grug_get_error<'a>(state: &'a CState) -> Option<&'a GrugError<'a>> {
	Some(unsafe{&*state.1.get()}.as_ref()?.inner())
}
