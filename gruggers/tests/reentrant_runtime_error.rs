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

use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::types::{ExportFnId, GrugEntity};

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
