//! Provides a public c api for use when compiling as a static library.
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::error::{Error, ErrorKind, GrugError, SourceSpan};
use crate::ntstring::{NTOsStrPtr, NTStrPtr};
use crate::serde;
use crate::state::{
    ExportFnEntry, FileInfo, Files, GrugEntityHandle, GrugInitSettings, GrugState, ResourcePaths,
};
use crate::types::{
    ErasedHostFn, ErasedRegFn, ExportFnId, FileId, GrugEntity, INVALID_GRUG_FILE_ID, Value,
};

use std::cell::UnsafeCell;
use std::mem::MaybeUninit;

// TODO: Create an actual struct for these
type CState = (
    GrugState,
    /* last error */ UnsafeCell<Option<Error>>,
    /* info from last compile */ UnsafeCell<Files>,
    /* resources from last update */ UnsafeCell<ResourcePaths>,
);

#[unsafe(no_mangle)]
pub extern "C" fn grug_default_settings() -> GrugInitSettings<'static> {
    GrugInitSettings::default()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_init(
    settings: GrugInitSettings,
    out_err: &mut MaybeUninit<GrugError<'static>>,
) -> Option<Box<CState>> {
    match settings.build_state() {
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
/// same as [`GrugState::register_host_fn`], except the number of generic
/// paramters is not checked
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_host_fn<'a>(
    state: &'a mut CState,
    fn_name: NTStrPtr,
    func: ErasedHostFn,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe {
        state
            .0
            .register_host_fn_internal(None, fn_name.to_str(), func, None)
    } {
        Some(state.1.get_mut().insert(err).inner())
    } else {
        None
    }
}

/// # SAFETY
/// same as [`GrugState::register_method`], except the number of generic
/// paramters is not checked
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_host_method<'a>(
    state: &'a mut CState,
    class_name: NTStrPtr,
    fn_name: NTStrPtr,
    func: ErasedHostFn,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe {
        state
            .0
            .register_host_fn_internal(Some(class_name.to_str()), fn_name.to_str(), func, None)
    } {
        Some(state.1.get_mut().insert(err).inner())
    } else {
        None
    }
}

/// # SAFETY
/// same as [`GrugState::register_reg_fn`], except the number of generic
/// paramters is not checked
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_reg_fn<'a>(
    state: &'a mut CState,
    fn_name: NTStrPtr,
    func: ErasedRegFn,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe {
        state
            .0
            .register_reg_fn_internal(None, fn_name.to_str(), func, None)
    } {
        Some(state.1.get_mut().insert(err).inner())
    } else {
        None
    }
}

/// # SAFETY
/// same as [`GrugState::register_method`], except the number of generic
/// paramters is not checked
#[unsafe(no_mangle)]
pub unsafe extern "C" fn grug_register_reg_method<'a>(
    state: &'a mut CState,
    class_name: NTStrPtr,
    fn_name: NTStrPtr,
    func: ErasedRegFn,
) -> Option<&'a GrugError<'a>> {
    // SAFETY: This function is exposed to C and is inherently unsafe
    if let Err(err) = unsafe {
        state
            .0
            .register_reg_fn_internal(Some(class_name.to_str()), fn_name.to_str(), func, None)
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
pub extern "C" fn grug_get_updated_resources(state: &CState) -> &[NTOsStrPtr<'_>] {
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

/// Writes `text` and a null terminator into `out`, or the needed size into `out_len`.
fn write_text(out: *mut u8, out_len: &mut usize, text: &str) -> bool {
    let needed = text.len() + 1;
    if out.is_null() || *out_len < needed {
        *out_len = needed;
        return false;
    }
    // SAFETY: The caller promises that `out` holds `*out_len` writable bytes, and `needed` is
    // within that capacity.
    unsafe {
        std::ptr::copy_nonoverlapping(text.as_ptr(), out, text.len());
        out.add(text.len()).write(b'\0');
    }
    *out_len = needed;
    true
}

/// Converts grug source text into its json ast.
///
/// `out_json_len` is the capacity of `out_json` on entry and the bytes needed, terminator included,
/// on return. A null or undersized buffer only asks for that size.
#[unsafe(no_mangle)]
pub extern "C" fn grug_to_json(
    state: &CState,
    file_path: NTStrPtr<'_>,
    grug_text: NTStrPtr<'_>,
    out_json: *mut u8,
    out_json_len: &mut usize,
) -> bool {
    unsafe { *state.1.get() = None };
    match serde::dump_file_to_json(grug_text.to_str(), file_path.to_str()) {
        Ok(json) => write_text(out_json, out_json_len, &json),
        Err(err) => {
            unsafe { *state.1.get() = Some(err) };
            false
        }
    }
}

/// Converts a json grug ast back into grug source text. The buffer rules are those of
/// [`grug_to_json`].
#[unsafe(no_mangle)]
pub extern "C" fn json_to_grug(
    state: &CState,
    json_text: NTStrPtr<'_>,
    out_grug: *mut u8,
    out_grug_len: &mut usize,
) -> bool {
    unsafe { *state.1.get() = None };
    match serde::generate_file_from_json(json_text.to_str()) {
        Ok(grug) if grug.contains('\0') => {
            unsafe {
                *state.1.get() = Some(Error::new(
                    ErrorKind::COMPILE_ERROR,
                    "",
                    "".as_ref(),
                    "",
                    SourceSpan { offset: 0, line: 0 },
                    format_args!("the generated grug source contains a null byte"),
                ))
            };
            false
        }
        Ok(grug) => write_text(out_grug, out_grug_len, &grug),
        Err(err) => {
            unsafe { *state.1.get() = Some(err) };
            false
        }
    }
}

/// # Safety
/// There is no memory safety issue here.
/// But this may cause older entities to be replaced
/// by newer ones with no warning if the ids start overlapping
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
pub extern "C" fn grug_set_runtime_error(state: &CState, message: NTStrPtr) {
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
