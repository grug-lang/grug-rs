//! Provides a public c api for use when compiling as a static library.
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::error::{Error, GrugError};
use crate::ntstring::{NTBytes, NTStrPtr};
use crate::state::{
    ExportFnEntry, FileInfo, Files, GrugEntityHandle, GrugInitSettings, GrugState, ResourcePaths,
};
use crate::types::{ExportFnId, FileId, GrugEntity, HostFnWithState, INVALID_GRUG_FILE_ID, Value};

use std::cell::UnsafeCell;
use std::mem::MaybeUninit;

// TODO: Create an actual struct for these
type CState = (
    GrugState,
    /* last error */ UnsafeCell<Option<Error>>,
    /* info from last compile */ UnsafeCell<Files>,
    /* resources from last update */ UnsafeCell<ResourcePaths>,
);

#[repr(C)]
pub struct CGrugRuntimeErrorHandler {
    pub user_data: *mut std::ffi::c_void,
    pub drop_fn: Option<extern "C" fn(*mut std::ffi::c_void)>,
    pub handler_fn: Option<
        extern "C" fn(
            data: *mut std::ffi::c_void,
            err_kind: u32,
            reason_str: *mut std::ffi::c_char,
            reason_len: usize,
            export_fn_name: *mut std::ffi::c_char,
            export_fn_name_len: usize,
            script_path: *mut std::ffi::c_char,
            script_path_len: usize,
        ),
    >,
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
    out_err: &mut MaybeUninit<GrugError<'static>>,
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
            move |error: &gruggers_core::runtime_error::RuntimeError| {
                let user_data_ptr = c_user_data as *mut std::ffi::c_void;
                let error_message = error.error_message.to_str();
                let export_fn_name = error.export_fn_name.to_str();
                let script_path = error.script_path.to_bytes();
                c_handler_fn(
                    user_data_ptr,
                    error.kind as u32,
                    error_message.as_ptr() as *mut std::ffi::c_char,
                    error_message.len(),
                    export_fn_name.as_ptr() as *mut std::ffi::c_char,
                    export_fn_name.len(),
                    script_path.as_ptr() as *mut std::ffi::c_char,
                    script_path.len(),
                );
            },
        );
    }

    match rust_settings.build_state() {
        Ok(state) => Some(Box::new((
            state,
            UnsafeCell::new(None),
            UnsafeCell::new(Files::empty()),
            UnsafeCell::new(ResourcePaths::empty()),
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
pub unsafe extern "C" fn grug_register_host_fn<'a>(
    state: &'a mut CState,
    fn_name: NTStrPtr,
    func: HostFnWithState<0, GrugState>,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe { state.0.register_host_fn(fn_name.to_str(), func) } {
        Some(state.1.get_mut().insert(err).inner())
    } else {
        None
    }
}

/// # SAFETY
/// same as [`GrugState::register_method`]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_method<'a>(
    state: &'a mut CState,
    class_name: NTStrPtr,
    fn_name: NTStrPtr,
    func: HostFnWithState<0, GrugState>,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe {
        state
            .0
            .register_method(class_name.to_str(), fn_name.to_str(), func)
    } {
        Some(state.1.get_mut().insert(err).inner())
    } else {
        None
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_all_files(state: &CState) -> &[FileInfo<'_>] {
    let files = unsafe { &mut *state.2.get() };
    *files = state.0.compile_all_files();
    files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_update(state: &CState) -> &[FileInfo<'_>] {
    state.0.clear_error();
    let files = unsafe { &mut *state.2.get() };
    let resources = unsafe { &mut *state.3.get() };
    (*resources, *files) = state.0.update_files();
    files.files()
}

/// Get the paths (relative to the mods directory) of every non-`.grug` file
/// that was detected as changed by the most recent call to [`grug_update`].
/// This includes files that are never referenced by a `resource` string
/// inside any `.grug` script (such an auto-discovered texture, `.lang`
/// file, or JSON data), since resource strings no longer register a file
/// watch; they are only used to validate that a resource exists at compile
/// time.
///
/// The returned slice is only valid until the next call to [`grug_update`].
#[unsafe(no_mangle)]
pub extern "C" fn grug_get_updated_resources(state: &CState) -> &[NTBytes<'_>] {
    unsafe { &*state.3.get() }.paths()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_file(state: &CState, file_path: NTStrPtr<'_>) -> FileId {
    match state.0.compile_grug_file(file_path.to_str()) {
        Ok(id) => id,
        Err(err) => {
            unsafe { *state.1.get() = Some(err) };
            INVALID_GRUG_FILE_ID
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_set_next_entity_id(state: &CState, next_id: u64) {
    unsafe { state.0.set_next_entity_id(next_id) };
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(
    state: &CState,
    file_id: FileId,
) -> Option<GrugEntityHandle<'_>> {
    state.0.clear_error();
    state.0.create_entity(file_id)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit_entity(state: &CState, handle: GrugEntityHandle<'_>) -> bool {
    state.0.destroy_entity(handle)
}

const INVALID_GRUG_EXPORT_FN_ID: ExportFnId = ExportFnId(u64::MAX);

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_fn_ids(state: &CState) -> &[ExportFnEntry<'_>] {
    state.0.get_export_fns()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_id(
    state: &CState,
    entity_type: NTStrPtr<'_>,
    on_fn_name: NTStrPtr<'_>,
) -> ExportFnId {
    match state
        .0
        .get_export_fn_id(entity_type.to_str(), on_fn_name.to_str())
    {
        Ok(id) => id,
        Err(err) => {
            unsafe { *state.1.get() = Some(err) };
            INVALID_GRUG_EXPORT_FN_ID
        }
    }
}

/// # Safety
/// `values` must point to a buffer that contains at least `values_len`
/// elements
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_call_export_fn(
    state: &CState,
    entity: &GrugEntity,
    on_fn_id: ExportFnId,
    values: *const Value,
    values_len: usize,
) -> bool {
    state.0.clear_error();
    unsafe {
        state.0.call_export_fn(
            entity,
            on_fn_id,
            std::slice::from_raw_parts(values, values_len),
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_set_runtime_error(state: &CState, message: NTStrPtr) {
    state.0.set_host_fn_error(message.to_str());
}

#[unsafe(no_mangle)]
/// returns 1 if all host functions mentioned in the mod api have been
/// registered
///
/// returns 0 otherwise
pub extern "C" fn grug_all_host_fns_registered(state: &mut CState) -> Option<&GrugError<'_>> {
    let Err(err) = state.0.all_host_fns_registered() else {
        return None;
    };
    Some(state.1.get_mut().insert(err).inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_error<'a>(state: &'a CState) -> Option<&'a GrugError<'a>> {
    Some(unsafe { &*state.1.get() }.as_ref()?.inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_entity_get_data<'a>(
    _: &CState,
    entity: Option<&'a GrugEntity>,
) -> Option<&'a GrugEntity> {
    entity
}
