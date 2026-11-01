//! Provides a public c api for use when compiling as a static library.
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
#![allow(improper_ctypes_definitions)]
use crate::error::{Error, GrugError};
use crate::frontend::CompileSettings;
use crate::ntstring::{NTOsStrPtr, NTStrPtr};
use crate::state::{
    ExportFnEntry, FileInfo, Files, GrugEntityHandle, GrugInitSettings, GrugState, ResourcePaths,
};
use crate::types::{
    ErasedHostFn, ErasedRegFn, ExportFnId, FileId, GrugEntity, INVALID_GRUG_FILE_ID, Value,
};

use std::cell::UnsafeCell;
use std::ffi::{OsStr, c_void};
use std::marker::PhantomData;
use std::mem::MaybeUninit;

// TODO: Create an actual struct for these
type CState = (
    GrugState,
    /* last error */ UnsafeCell<Option<Error>>,
    /* info from last compile */ UnsafeCell<Files>,
    /* resources from last update */ UnsafeCell<ResourcePaths>,
);

/// Decides the mode of one mod's files, as `grug.h` declares it.
///
/// `mod_name` is `mod_name_len` bytes and need not be null terminated, and it
/// is the first directory component of a file's path relative to the mods
/// directory, exactly as `grug_file_info`'s `mod_name` reports it, or empty for
/// a file directly within the mods directory. An answer of true compiles that
/// mod's files in [`Mode::Unsafe`](crate::types::Mode::Unsafe).
///
/// It is called on the thread that called the compile function, once per file
/// that call installs, and must give the same answer for the same name for the
/// length of that call.
pub type GrugIsUnsafeModFn =
    extern "C" fn(user_data: *const c_void, mod_name: *const u8, mod_name_len: usize) -> bool;

/// The mode of every file one compile call compiles, as `grug.h` declares it,
/// which is what [`CompileSettings`] wraps for Rust.
///
/// grug's mode is per file and fixed when the file compiles, so a call that
/// compiles many files takes the whole decision instead of one mode. The
/// decision is by mod, so a host answers for the mod of each file it compiles
/// rather than for each path, and a file that is hot reloaded or added later is
/// in its mod's mode without the host naming it again.
#[repr(C)]
pub struct GrugCompileSettings<'a> {
    /// Answers the mode of the mod of each file the call compiles. Null means
    /// safe for every file, which is what a host that grants nothing passes.
    is_unsafe_mod: Option<GrugIsUnsafeModFn>,
    /// Passed back to `is_unsafe_mod`. Unused, and may be null, when it is
    /// null.
    user_data: *const c_void,
    _marker: PhantomData<&'a ()>,
}

impl GrugCompileSettings<'_> {
    /// Runs `compile` with these settings as [`CompileSettings`] reads them.
    ///
    /// The callback is borrowed for the length of that call, so the caller must
    /// keep it, and what `user_data` points to, alive and usable until the
    /// compile call returns.
    fn with<T>(&self, compile: impl FnOnce(CompileSettings<'_>) -> T) -> T {
        let Some(is_unsafe_mod) = self.is_unsafe_mod else {
            return compile(CompileSettings::safe());
        };
        let user_data = self.user_data;
        let decide = |mod_name: &OsStr| {
            let mod_name = mod_name.as_encoded_bytes();
            is_unsafe_mod(user_data, mod_name.as_ptr(), mod_name.len())
        };
        compile(CompileSettings::new(&decide))
    }
}

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
pub extern "C" fn grug_compile_all_files<'a, 'b>(
    state: &'a CState,
    settings: GrugCompileSettings<'b>,
) -> &'a [FileInfo<'a>] {
    let files = unsafe { &mut *state.2.get() };
    *files = settings.with(|settings| state.0.compile_all_files(settings));
    files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_update<'a, 'b>(
    state: &'a CState,
    settings: GrugCompileSettings<'b>,
) -> &'a [FileInfo<'a>] {
    state.0.clear_error();
    let files = unsafe { &mut *state.2.get() };
    let resources = unsafe { &mut *state.3.get() };
    (*resources, *files) = settings.with(|settings| state.0.update_files(settings));
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
pub extern "C" fn grug_compile_file(
    state: &CState,
    file_path: NTStrPtr<'_>,
    settings: GrugCompileSettings<'_>,
) -> FileId {
    match settings.with(|settings| state.0.compile_grug_file(file_path.to_str(), settings)) {
        Ok(id) => id,
        Err(err) => {
            unsafe { *state.1.get() = Some(err) };
            INVALID_GRUG_FILE_ID
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
