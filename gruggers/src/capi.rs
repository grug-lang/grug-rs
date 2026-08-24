//! Provides a public c api for use when compiling as a static library. 
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::state::{ExportFnEntry, GrugEntityHandle, GrugInitSettings, GrugState, Files, FileInfo, State};
use crate::ntstring::NTStrPtr;
use crate::types::{FileId, ExportFnId, GrugEntity, Value, HostFnWithState, HostFnRegErased, INVALID_GRUG_FILE_ID};
use crate::error::{Error, GrugError};

use gruggers_core::runtime_error::RuntimeError;

use std::ffi::OsString;
use std::cell::UnsafeCell;
use std::mem::MaybeUninit;

// TODO: Create an actual struct for these
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
/// same as [`GrugState::register_host_fn`]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_host_fn<'a>(state: &'a mut CState, fn_name: NTStrPtr, func: HostFnWithState<GrugState>) -> Option<&'a GrugError<'a>> {
	// SAFETY: This function is exposed to C and is inherently unsafe
	if let Err(err) = unsafe{state.0.register_host_fn(fn_name.to_str(), func)} {
		Some(state.1.get_mut().insert(err).inner())
	} else {
		None
	}
}

/// # SAFETY
/// same as [`GrugState::register_method`]
pub unsafe extern "C" fn grug_register_method<'a>(state: &'a mut CState, class_name: NTStrPtr, fn_name: NTStrPtr, func: HostFnWithState<GrugState>) -> Option<&'a GrugError<'a>> {
	// SAFETY: This function is exposed to C and is inherently unsafe
	if let Err(err) = unsafe{state.0.register_method(class_name.to_str(), fn_name.to_str(), func)} {
		Some(state.1.get_mut().insert(err).inner())
	} else {
		None
	}
}

/// # SAFETY
/// same as [`GrugState::register_generic_fn`]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_generic_fn<'a>(state: &'a mut CState, fn_name: NTStrPtr, func: HostFnRegErased) -> Option<&'a GrugError<'a>> {
	// SAFETY: This function is exposed to C and is inherently unsafe
	if let Err(err) = unsafe{state.0.register_generic_fn_internal_unsafe(None, fn_name.to_str(), func)} {
		Some(state.1.get_mut().insert(err).inner())
	} else {
		None
	}
}

/// # SAFETY
/// same as [`GrugState::register_generic_method`]
pub unsafe extern "C" fn grug_register_generic_method<'a>(state: &'a mut CState, class_name: NTStrPtr, fn_name: NTStrPtr, func: HostFnRegErased) -> Option<&'a GrugError<'a>> {
	// SAFETY: This function is exposed to C and is inherently unsafe
	if let Err(err) = unsafe{state.0.register_generic_fn_internal_unsafe(Some(class_name.to_str()), fn_name.to_str(), func)} {
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
pub extern "C" fn grug_compile_file(state: &CState, file_path: NTStrPtr<'_>) -> FileId {
	match state.0.compile_grug_file(file_path.to_str()) {
		Ok(id) => id,
		Err(err) => {
			unsafe{*state.1.get() = Some(err)};
			INVALID_GRUG_FILE_ID
		}
	}
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_set_next_entity_id(state: &CState, next_id: u64) {
    unsafe { state.0.set_next_entity_id(next_id) };
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(state: &CState, file_id: FileId) -> Option<GrugEntityHandle<'_>> {
	state.0.create_entity(file_id)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit_entity(state: &CState, handle: GrugEntityHandle<'_>) {
	state.0.destroy_entity(handle)
}

const INVALID_GRUG_EXPORT_FN_ID: ExportFnId = ExportFnId(u64::MAX);

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_fn_ids(state: &CState) -> &[ExportFnEntry<'_>] {
	state.0.get_export_fns()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_id(state: &CState, entity_type: NTStrPtr<'_>, on_fn_name: NTStrPtr<'_>) -> ExportFnId {
	match state.0.get_export_fn_id(entity_type.to_str(), on_fn_name.to_str()) {
		Ok(id) => id,
		Err(err) => {
			unsafe{*state.1.get() = Some(err)};
			INVALID_GRUG_EXPORT_FN_ID
		}
	}
}

/// # Safety
/// `values` must point to a buffer that contains at least `values_len`
/// elements
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_call_export_fn(state: &CState, entity: &GrugEntity, on_fn_id: ExportFnId, values: *const Value, values_len: usize) -> bool {
	unsafe{state.0.call_export_fn(entity, on_fn_id, std::slice::from_raw_parts(values, values_len))}
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_set_runtime_error(state: &CState, message: NTStrPtr) {
	state.0.set_runtime_error(RuntimeError::GameFunctionError{message: message.to_str()});
}

#[unsafe(no_mangle)]
/// returns 1 if all host functions mentioned in the mod api have been
/// registered
///
/// returns 0 otherwise
pub extern "C" fn grug_all_host_fns_registered(state: &mut CState) -> Option<&GrugError<'_>> {
	let Err(err) = state.0.all_host_fns_registered() else {
		return None
	};
	Some(state.1.get_mut().insert(err).inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_error<'a>(state: &'a CState) -> Option<&'a GrugError<'a>> {
	Some(unsafe{&*state.1.get()}.as_ref()?.inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_entity_get_data<'a>(_: &CState, entity: Option<&'a GrugEntity>) -> Option<&'a GrugEntity> {
	entity
}
