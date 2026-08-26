//! Provides a public c api for use when compiling as a static library.
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::state::{ExportFnEntry, GrugEntityHandle, GrugInitSettings, GrugState, Files, FileInfo, State};
use crate::ntstring::{NTBytes, NTStrPtr};
use crate::types::{FileId, ExportFnId, GrugEntity, Value, HostFnWithState, HostFnRegErased, INVALID_GRUG_FILE_ID};
use crate::error::{Error, GrugError};
use crate::own_ptr::OwnPtr;
use crate::arena::Arena;

use gruggers_core::runtime_error::RuntimeError;

use std::cell::UnsafeCell;
use std::mem::MaybeUninit;

pub struct Resources {
	inner: OwnPtr<'static, [NTBytes<'static>]>,
	_arena: Arena,
}

impl Resources {
	pub fn empty() -> Self {
		Self {
			inner: (Box::new([]) as Box<[_]>).into(),
			_arena: Arena::new(),
		}
	}

	pub fn paths<'a>(&'a self) -> &'a [NTBytes<'a>] {
		&*self.inner
	}
}

// TODO: Create an actual struct for these
type CState = (GrugState, /* last error */ UnsafeCell<Option<Error>>, /* info from last compile */ UnsafeCell<Files>, /* resources from last compile */ UnsafeCell<Resources>);

#[repr(C)]
pub struct CGrugRuntimeErrorHandler {
    pub user_data: *mut std::ffi::c_void,
    pub drop_fn: Option<extern "C" fn(*mut std::ffi::c_void)>,
    pub handler_fn: Option<extern "C" fn(
        data: *mut std::ffi::c_void,
        err_kind: u32,
        reason_str: *mut std::ffi::c_char,
        reason_len: usize,
        export_fn_name: *mut std::ffi::c_char,
        export_fn_name_len: usize,
        script_path: *mut std::ffi::c_char,
        script_path_len: usize,
    )>,
}

#[repr(C)]
pub struct CGrugBackend {
    pub obj: *mut std::ffi::c_void,
    pub vtable: *mut std::ffi::c_void,
}

#[repr(C)]
pub struct CGrugInitSettings {
    pub mod_api_path: *const u8,
    pub mod_api_path_len: usize,
    pub mods_dir_path: *const u8,
    pub mods_dir_path_len: usize,
    pub runtime_error_handler: CGrugRuntimeErrorHandler,
    pub backend: CGrugBackend,
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_default_settings() -> CGrugInitSettings {
    CGrugInitSettings {
        mod_api_path: std::ptr::null(),
        mod_api_path_len: 0,
        mods_dir_path: std::ptr::null(),
        mods_dir_path_len: 0,
        runtime_error_handler: CGrugRuntimeErrorHandler {
            user_data: std::ptr::null_mut(),
            drop_fn: None,
            handler_fn: None,
        },
        backend: CGrugBackend {
            obj: std::ptr::null_mut(),
            vtable: std::ptr::null_mut(),
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_init(
    c_settings: CGrugInitSettings,
    out_err: &mut MaybeUninit<GrugError<'static>>
) -> Option<Box<CState>> {
    // Because C passed the exact lengths, we don't need to leak or duplicate the strings!
    // The lifetimes of these OsStrs only need to survive until build_state() completes.
    let mod_api_path = unsafe {
        std::ffi::OsStr::from_encoded_bytes_unchecked(std::slice::from_raw_parts(
            c_settings.mod_api_path,
            c_settings.mod_api_path_len,
        ))
    };
    let mods_dir_path = unsafe {
        std::ffi::OsStr::from_encoded_bytes_unchecked(std::slice::from_raw_parts(
            c_settings.mods_dir_path,
            c_settings.mods_dir_path_len,
        ))
    };

    let mut rust_settings = GrugInitSettings::new()
        .set_mod_api_path(mod_api_path)
        .set_mods_dir(mods_dir_path);

    // Safely wrap the C function pointer into a Rust closure
    if let Some(c_handler_fn) = c_settings.runtime_error_handler.handler_fn {
        let c_user_data = c_settings.runtime_error_handler.user_data as usize;

        rust_settings = rust_settings.set_runtime_error_handler(
            move |err_kind, reason_str, export_fn_name, script_path| {
                let user_data_ptr = c_user_data as *mut std::ffi::c_void;
                c_handler_fn(
                    user_data_ptr,
                    err_kind,
                    reason_str.as_ptr() as *mut std::ffi::c_char,
                    reason_str.len(),
                    export_fn_name.as_ptr() as *mut std::ffi::c_char,
                    export_fn_name.len(),
                    script_path.as_ptr() as *mut std::ffi::c_char,
                    script_path.len(),
                );
            }
        );
    }

    match rust_settings.build_state() {
        Ok(state) => Some(Box::new((
            state,
            UnsafeCell::new(None),
            UnsafeCell::new(Files::empty()),
            UnsafeCell::new(Resources::empty()),
        ))),
        Err(err) => {
            unsafe { out_err.as_mut_ptr().write(err.leak()) };
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
#[unsafe(no_mangle)]
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
#[unsafe(no_mangle)]
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
	state.0.clear_error();
	let files = unsafe{&mut *state.2.get()};
	let resources = unsafe{&mut *state.3.get()};

	let (updated_resource_paths, updated_files) = state.0.update_files();
	*files = updated_files;

	let arena = Arena::new();
	let paths: Box<[NTBytes<'_>]> = updated_resource_paths.iter()
		.map(|path| unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(path.as_encoded_bytes()))})
		.collect();
	// SAFETY: `paths` borrows from `arena`, which we move into `Resources`
	// right alongside it, so the borrowed data stays valid for as long as
	// `paths` is reachable. Same pattern as `Files`/`FileInfo`.
	let paths: OwnPtr<'static, [NTBytes<'static>]> = unsafe{std::mem::transmute::<
		OwnPtr<'_, [NTBytes<'_>]>,
		OwnPtr<'static, [NTBytes<'static>]>
	>(paths.into())};
	*resources = Resources { inner: paths, _arena: arena };

	files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_updated_resources(state: &CState) -> &[NTBytes<'_>] {
	unsafe{&*state.3.get()}.paths()
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
	state.0.clear_error();
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
	state.0.clear_error();
	unsafe{state.0.call_export_fn(entity, on_fn_id, std::slice::from_raw_parts(values, values_len))}
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_set_runtime_error(state: &CState, message: NTStrPtr) {
	state.0.set_runtime_error(RuntimeError::GameFunctionError{message: message.to_str().to_string()});
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
