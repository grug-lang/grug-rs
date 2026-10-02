//! Provides a public c api for use when compiling as a static library.
//!
//! These functions have the same safety requirements as the equivalent
//! functions in state.rs
//!
//! Entries that take a shared reference to a state lock it for the duration
//! of the call, so the C API may be called from more than one thread, and
//! the state's own storage (the last error, `Files` and `ResourcePaths`) is
//! written under the same lock. The registration functions take a mutable
//! reference, so their caller already has exclusive access.
#![allow(improper_ctypes_definitions)]
use crate::error::{Error, GrugError};
use crate::ntstring::{NTOsStrPtr, NTStrPtr};
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
    let _guard = state.0.enter();
    let files = unsafe { &mut *state.2.get() };
    *files = state.0.compile_all_files();
    files.files()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_update(state: &CState) -> &[FileInfo<'_>] {
    let _guard = state.0.enter();
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
    let _guard = state.0.enter();
    unsafe { &*state.3.get() }.paths()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_compile_file(state: &CState, file_path: NTStrPtr<'_>) -> FileId {
    let _guard = state.0.enter();
    match state.0.compile_grug_file(file_path.to_str()) {
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
    let _guard = state.0.enter();
    unsafe { state.0.set_next_entity_id(next_id) };
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_create_entity(
    state: &CState,
    file_id: FileId,
) -> Option<GrugEntityHandle<'_>> {
    let _guard = state.0.enter();
    state.0.clear_error();
    state.0.create_entity(file_id)
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_deinit_entity(state: &CState, handle: GrugEntityHandle<'_>) -> bool {
    let _guard = state.0.enter();
    state.0.destroy_entity(handle)
}

const INVALID_GRUG_EXPORT_FN_ID: ExportFnId = ExportFnId(u64::MAX);

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_fn_ids(state: &CState) -> &[ExportFnEntry<'_>] {
    let _guard = state.0.enter();
    state.0.get_export_fns()
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_on_fn_id(
    state: &CState,
    entity_type: NTStrPtr<'_>,
    on_fn_name: NTStrPtr<'_>,
) -> ExportFnId {
    let _guard = state.0.enter();
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
    let _guard = state.0.enter();
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
    let _guard = state.0.enter();
    state.0.set_host_fn_error(message.to_str());
}

#[unsafe(no_mangle)]
/// returns 1 if all host functions mentioned in the mod api have been
/// registered
///
/// returns 0 otherwise
pub extern "C" fn grug_all_host_fns_registered(state: &mut CState) -> Option<&GrugError<'_>> {
    let _guard = state.0.enter();
    let Err(err) = state.0.all_host_fns_registered() else {
        return None;
    };
    Some(state.1.get_mut().insert(err).inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_get_error<'a>(state: &'a CState) -> Option<&'a GrugError<'a>> {
    let _guard = state.0.enter();
    Some(unsafe { &*state.1.get() }.as_ref()?.inner())
}

#[unsafe(no_mangle)]
pub extern "C" fn grug_entity_get_data<'a>(
    _: &CState,
    entity: Option<&'a GrugEntity>,
) -> Option<&'a GrugEntity> {
    entity
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::BytecodeBackend;
    use crate::nt;
    use crate::state::test_support::{MOD_API, SCRIPT};
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    fn init_test_state(name: &str) -> (PathBuf, Box<CState>) {
        let dir = std::env::temp_dir().join(format!("gruggers-capi-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("race").join("code")).unwrap();
        std::fs::write(dir.join("mod_api.json"), MOD_API).unwrap();
        std::fs::write(dir.join("race").join("code").join("race-Test.grug"), SCRIPT).unwrap();

        let mod_api_path = dir.join("mod_api.json");
        let settings = GrugInitSettings::new()
            .set_mod_api_path(&mod_api_path)
            .set_mods_dir(&dir)
            .set_backend(BytecodeBackend::new())
            .set_runtime_error_handler(|_| {})
            .set_poll_interval(Duration::from_secs(3600));
        let mut err = MaybeUninit::uninit();
        let state = grug_init(settings, &mut err).expect("state builds");
        (dir, state)
    }

    /// The shape the JNI layer uses to reach the VM: one `CState` pointer
    /// handed to several threads. The C entries serialize themselves, so the
    /// calls have to finish instead of racing.
    #[test]
    fn the_c_api_can_be_entered_from_two_threads() {
        let (dir, state) = init_test_state("concurrent-export");
        let file_id = grug_compile_file(&state, nt!("race/code/race-Test.grug").as_ntstrptr());
        assert_ne!(file_id, INVALID_GRUG_FILE_ID);
        let handle = grug_create_entity(&state, file_id).expect("entity is created");
        let fn_id = grug_get_on_fn_id(&state, nt!("Test").as_ntstrptr(), nt!("run").as_ntstrptr());

        let state_ptr = &*state as *const CState as usize;
        let entity_ptr = &*handle as *const GrugEntity as usize;
        let calls = AtomicUsize::new(0);
        std::thread::scope(|scope| {
            for _ in 0..4 {
                scope.spawn(|| {
                    // SAFETY: the entry points serialize their calls, and the
                    // entity outlives this scope by construction. Zero
                    // arguments still need a non-null pointer, like `&[]`
                    // gives.
                    let state = state_ptr as *const CState;
                    let entity = entity_ptr as *const GrugEntity;
                    let no_values = std::ptr::NonNull::<Value>::dangling().as_ptr();
                    for _ in 0..100 {
                        assert!(unsafe {
                            grug_call_export_fn(&*state, &*entity, fn_id, no_values, 0)
                        });
                        calls.fetch_add(1, Ordering::Relaxed);
                    }
                });
            }
        });
        assert_eq!(calls.load(Ordering::Relaxed), 400);

        grug_deinit(Some(state));
        let _ = std::fs::remove_dir_all(&dir);
    }
}
