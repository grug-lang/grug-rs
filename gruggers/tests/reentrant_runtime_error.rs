//! A runtime error handler may re-enter the VM while it runs.
//!
//! The regression test for #25. `raise_runtime_error_inner` used to hold the backend's only
//! `error_arena` borrowed across `state.handle_runtime_error`, so a handler that called back into
//! the VM and provoked a second error panicked with `RefCell already borrowed`, and the unwind
//! reached the `extern "C"` handler trampoline, which cannot unwind, so the process aborted. The
//! error also borrowed the shared call stack, which a nested call pushes and pops.
//!
//! The handler below re-enters the state when the first error is reported, reads the outer error
//! again after the nested call, and counts both reports.
//!
//! A second test covers the other half: a handler that re-enters while a host function error is
//! being reported must not stop the outer invocation from unwinding. `call_on_function` used to
//! clear the error flag for good on entry, so a successful nested call made the outer script run
//! past its error and return success.

use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use gruggers::ast::Type;
use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::types::{ExportFnId, GrugEntity, Value};

/// A loop long enough that the 100 ms time limit ends it, so the errors need no host function and
/// no recursion.
const SCRIPT: &str = r#"export run() {
    x: number = 0
    while x < 1000000000 {
        x = x + 1
    }
}
"#;

const MOD_API: &str = r#"{
    "classes": {},
    "entities": {
        "Test": {
            "description": "a test entity",
            "export_functions": [
                { "name": "run", "description": "runs" }
            ]
        }
    },
    "host_functions": {}
}"#;

/// The pointers the handler needs to call back into the state, filled in once the state exists.
struct Reentry {
    state: AtomicPtr<GrugState>,
    entity: AtomicPtr<GrugEntity>,
    fn_id: AtomicU64,
    entered: AtomicBool,
}

fn write_mods(dir: &Path) {
    std::fs::create_dir_all(dir.join("race").join("code")).unwrap();
    std::fs::write(dir.join("mod_api.json"), MOD_API).unwrap();
    std::fs::write(dir.join("race").join("code").join("race-Test.grug"), SCRIPT).unwrap();
}

#[test]
fn a_handler_can_reenter_the_vm_and_raise_a_second_error() {
    let dir = std::env::temp_dir().join("grug-reentrant-error");
    let _ = std::fs::remove_dir_all(&dir);
    write_mods(&dir);

    let reentry = Arc::new(Reentry {
        state: AtomicPtr::new(std::ptr::null_mut()),
        entity: AtomicPtr::new(std::ptr::null_mut()),
        fn_id: AtomicU64::new(0),
        entered: AtomicBool::new(false),
    });
    let reports = Arc::new(Mutex::new(Vec::new()));

    let handler_reentry = Arc::clone(&reentry);
    let handler_reports = Arc::clone(&reports);
    let mod_api_path = dir.join("mod_api.json");
    let state = GrugInitSettings::new()
        .set_mod_api_path(&mod_api_path)
        .set_mods_dir(&dir)
        .set_runtime_error_handler(move |error| {
            let message = error.error_string.to_str().to_string();
            let frames = error.call_stack.len();
            handler_reports.lock().unwrap().push(message.clone());

            if handler_reentry.entered.swap(true, Ordering::SeqCst) {
                return;
            }
            // SAFETY: the pointers are filled in before any script runs, and a host runtime error
            // handler calling back into the state is the API's reentrancy.
            unsafe {
                let state = handler_reentry.state.load(Ordering::SeqCst);
                let entity = handler_reentry.entity.load(Ordering::SeqCst);
                let fn_id = ExportFnId(handler_reentry.fn_id.load(Ordering::SeqCst));
                let _ = (*state).call_export_fn(&*entity, fn_id, &[]);
            }

            // The nested error must not have reused this error's arena or call stack.
            assert_eq!(
                message,
                error.error_string.to_str(),
                "the nested raise changed the outer error"
            );
            assert_eq!(
                frames,
                error.call_stack.len(),
                "the nested raise changed the outer call stack"
            );
        })
        .set_poll_interval(Duration::from_secs(3600))
        .build_state()
        .expect("state builds");

    reentry.state.store(
        &state as *const GrugState as *mut GrugState,
        Ordering::SeqCst,
    );

    let file_id = state
        .compile_grug_file("race/code/race-Test.grug")
        .expect("script compiles");
    let handle = state.create_entity(file_id).expect("entity is created");
    let fn_id = state
        .get_export_fn_id("Test", "run")
        .expect("export fn exists");
    reentry.entity.store(
        &*handle as *const GrugEntity as *mut GrugEntity,
        Ordering::SeqCst,
    );
    reentry.fn_id.store(fn_id.0, Ordering::SeqCst);

    let _ = state.call_export_fn(&handle, fn_id, &[]);

    let reports = reports.lock().unwrap();
    assert_eq!(
        reports.len(),
        2,
        "the outer and nested errors must both be reported: {reports:?}"
    );
}

/// The error is raised two grug frames deep, matching a handler re-entering while nested calls
/// are already on the stack. `_helper` and `run` each call a marker after `boom`, and neither may
/// run once `boom` has reported its error.
const UNWIND_SCRIPT: &str = r#"export run() {
    _helper()
    after_helper()
}

export nested() {
    nested_ran()
}

local _helper() {
    boom()
    after_boom()
}
"#;

const UNWIND_MOD_API: &str = r#"{
    "classes": {},
    "entities": {
        "Test": {
            "description": "a test entity",
            "export_functions": [
                { "name": "run", "description": "runs" },
                { "name": "nested", "description": "nested" }
            ]
        }
    },
    "host_functions": {
        "boom": { "description": "raises", "parameters": [] },
        "after_boom": { "description": "marks", "parameters": [] },
        "after_helper": { "description": "marks", "parameters": [] },
        "nested_ran": { "description": "marks", "parameters": [] }
    }
}"#;

/// Markers the host functions below set. The two tests in this file run in parallel but never
/// touch the same statics.
static AFTER_BOOM: AtomicBool = AtomicBool::new(false);
static AFTER_HELPER: AtomicBool = AtomicBool::new(false);
static NESTED_RAN: AtomicBool = AtomicBool::new(false);
static NESTED_CALL_OK: AtomicBool = AtomicBool::new(false);

extern "C" fn boom(state: &GrugState, _: *const Value, _: &[Type; 0]) -> Value {
    state.set_host_fn_error("boom");
    Value { void: () }
}

extern "C" fn after_boom(_: &GrugState, _: *const Value, _: &[Type; 0]) -> Value {
    AFTER_BOOM.store(true, Ordering::SeqCst);
    Value { void: () }
}

extern "C" fn after_helper(_: &GrugState, _: *const Value, _: &[Type; 0]) -> Value {
    AFTER_HELPER.store(true, Ordering::SeqCst);
    Value { void: () }
}

extern "C" fn nested_ran(_: &GrugState, _: *const Value, _: &[Type; 0]) -> Value {
    NESTED_RAN.store(true, Ordering::SeqCst);
    Value { void: () }
}

fn write_unwind_mods(dir: &Path) {
    std::fs::create_dir_all(dir.join("unwind").join("code")).unwrap();
    std::fs::write(dir.join("mod_api.json"), UNWIND_MOD_API).unwrap();
    std::fs::write(
        dir.join("unwind").join("code").join("unwind-Test.grug"),
        UNWIND_SCRIPT,
    )
    .unwrap();
}

#[test]
fn a_handler_that_reenters_does_not_stop_the_outer_unwind() {
    let dir = std::env::temp_dir().join("grug-reentrant-unwind");
    let _ = std::fs::remove_dir_all(&dir);
    write_unwind_mods(&dir);

    let reentry = Arc::new(Reentry {
        state: AtomicPtr::new(std::ptr::null_mut()),
        entity: AtomicPtr::new(std::ptr::null_mut()),
        fn_id: AtomicU64::new(0),
        entered: AtomicBool::new(false),
    });
    let reports = Arc::new(Mutex::new(Vec::new()));

    let handler_reentry = Arc::clone(&reentry);
    let handler_reports = Arc::clone(&reports);
    let mod_api_path = dir.join("mod_api.json");
    let mut state = GrugInitSettings::new()
        .set_mod_api_path(&mod_api_path)
        .set_mods_dir(&dir)
        .set_runtime_error_handler(move |error| {
            handler_reports
                .lock()
                .unwrap()
                .push(error.error_string.to_str().to_string());

            if handler_reentry.entered.swap(true, Ordering::SeqCst) {
                return;
            }
            // SAFETY: the pointers are filled in before any script runs, and a host runtime error
            // handler calling back into the state is the API's reentrancy.
            unsafe {
                let state = handler_reentry.state.load(Ordering::SeqCst);
                let entity = handler_reentry.entity.load(Ordering::SeqCst);
                let fn_id = ExportFnId(handler_reentry.fn_id.load(Ordering::SeqCst));
                NESTED_CALL_OK.store(
                    (*state).call_export_fn(&*entity, fn_id, &[]),
                    Ordering::SeqCst,
                );
            }
        })
        .set_poll_interval(Duration::from_secs(3600))
        .build_state()
        .expect("state builds");

    unsafe {
        state.register_host_fn("boom", boom).unwrap();
        state.register_host_fn("after_boom", after_boom).unwrap();
        state
            .register_host_fn("after_helper", after_helper)
            .unwrap();
        state.register_host_fn("nested_ran", nested_ran).unwrap();
    }
    state.all_host_fns_registered().unwrap();

    let file_id = state
        .compile_grug_file("unwind/code/unwind-Test.grug")
        .expect("script compiles");
    let handle = state.create_entity(file_id).expect("entity is created");
    let run_id = state.get_export_fn_id("Test", "run").expect("run exists");
    let nested_id = state
        .get_export_fn_id("Test", "nested")
        .expect("nested exists");

    reentry.state.store(
        &state as *const GrugState as *mut GrugState,
        Ordering::SeqCst,
    );
    reentry.entity.store(
        &*handle as *const GrugEntity as *mut GrugEntity,
        Ordering::SeqCst,
    );
    reentry.fn_id.store(nested_id.0, Ordering::SeqCst);

    let completed = state.call_export_fn(&handle, run_id, &[]);

    assert!(
        !completed,
        "the outer invocation must still unwind after the handler returns"
    );
    assert!(
        NESTED_CALL_OK.load(Ordering::SeqCst),
        "the nested invocation must succeed"
    );
    assert!(
        NESTED_RAN.load(Ordering::SeqCst),
        "the nested invocation must run"
    );
    assert!(
        !AFTER_BOOM.load(Ordering::SeqCst),
        "the helper must not resume after its error"
    );
    assert!(
        !AFTER_HELPER.load(Ordering::SeqCst),
        "the caller must not resume after the helper's error"
    );

    let reports = reports.lock().unwrap();
    assert_eq!(
        reports.len(),
        1,
        "only the outer error is reported: {reports:?}"
    );
}
