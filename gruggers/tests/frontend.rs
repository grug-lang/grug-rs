//! Tests for compiler behavior that grug-tests doesn't cover, because it is
//! specific to gruggers or to its API.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{LazyLock, Mutex, MutexGuard, PoisonError};

use gruggers::CompileSettings;
use gruggers::backend::BytecodeBackend;
use gruggers::error::Error;
use gruggers::state::{GrugInitSettings, GrugState};

const MOD_API_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/grug-tests/mod_api.json");

/// An existing mods directory for tests that compile a string, so they don't
/// have to create one. The file watcher scans it, so it has to stay small
const STR_MODS_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests");

/// A mods directory that is deleted when the test using it ends
struct ModsDir(PathBuf);

impl ModsDir {
    fn new(test_name: &str) -> Self {
        // Tests run in parallel and can share a name, so every directory gets
        // its own number too
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "gruggers-{}-{}-{}",
            std::process::id(),
            test_name,
            id
        ));
        std::fs::create_dir_all(path.join("m")).unwrap();
        Self(path)
    }

    fn write(&self, relative_path: &str, text: &str) {
        let path = self.0.join(relative_path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
}

impl Drop for ModsDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// A state whose host functions haven't been registered
fn new_unregistered_state(mods_dir: &Path) -> GrugState {
    GrugInitSettings::new()
        .set_mod_api_path(MOD_API_PATH)
        .set_mods_dir(mods_dir.as_os_str())
        .set_backend(BytecodeBackend::new())
        .build_state()
        .unwrap()
}

fn new_state(mods_dir: &Path) -> GrugState {
    let mut state = new_unregistered_state(mods_dir);
    // SAFETY: None of the scripts in these tests call a host function
    unsafe { state.register_dummies() };
    state
}

/// A state that rescans the mods directory often enough that a test can wait for one scan
fn new_state_with_poll_interval(mods_dir: &Path, interval: std::time::Duration) -> GrugState {
    let mut state = GrugInitSettings::new()
        .set_mod_api_path(MOD_API_PATH)
        .set_mods_dir(mods_dir.as_os_str())
        .set_poll_interval(interval)
        .set_backend(BytecodeBackend::new())
        .build_state()
        .unwrap();
    // SAFETY: None of the scripts in these tests call a host function
    unsafe { state.register_dummies() };
    state
}

fn compile_error(state: &GrugState, path: impl AsRef<OsStr>, text: &str) -> Error {
    state
        .compile_grug_file_from_str(path, text, CompileSettings::safe())
        .expect_err("expected a compile error")
}

/// The state shared by the tests that only compile a string, since creating
/// a state takes milliseconds
fn shared_state() -> MutexGuard<'static, GrugState> {
    static STATE: LazyLock<Mutex<GrugState>> =
        LazyLock::new(|| Mutex::new(new_state(Path::new(STR_MODS_DIR))));
    // A test that fails while holding the lock leaves the state usable
    STATE.lock().unwrap_or_else(PoisonError::into_inner)
}

/// The full error string of compiling `text` as the file at `path`
fn compile_error_string(path: impl AsRef<OsStr>, text: &str) -> String {
    compile_error(&shared_state(), path, text)
        .inner()
        .error_string
        .to_str()
        .to_owned()
}

/// The error messages of the files that `compile_all_files` failed to compile
fn compile_all_errors(state: &GrugState) -> (usize, Vec<String>) {
    let files = state.compile_all_files(CompileSettings::safe());
    let mut ok_count = 0;
    let mut errors = Vec::new();
    for file in files.files() {
        match file.result() {
            Ok(_) => ok_count += 1,
            Err(err) => errors.push(err.error_message.to_str().to_owned()),
        }
    }
    (ok_count, errors)
}

// TODO: Move to grug-tests once the implementations agree on what counts as
// a level of nesting, and where the error is reported
#[test]
fn expressions_cant_be_nested_too_deeply() {
    let text = format!(
        "export a() {{\n    x: number = {}1{}\n}}\n",
        "(".repeat(101),
        ")".repeat(101)
    );
    // Parsing 100 levels of nesting can overflow the 2 MiB stack that tests
    // get by default, in an unoptimized build with coverage instrumentation
    let error = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || compile_error_string("m/x-D.grug", &text))
        .unwrap()
        .join()
        .unwrap();
    assert_eq!(
        error,
        "  in a (m/x-D.grug:3:2)\nError: There is a function that contains more than 100 levels of nested expressions\n3 $ }"
    );
}

// TODO: Move to grug-tests if grug-lang/grug-tests#64, which proposes these
// escape sequences, is accepted
// compile_grug_file_from_str canonicalizes the path of a file that compiles,
// which needs the file to exist
#[test]
fn strings_can_contain_escape_sequences() {
    let mods_dir = ModsDir::new("escapes");
    let text = "x: string = \"a\\tb\\nc\\rd\\\\e\\\"f\"\n";
    mods_dir.write("m/x-A.grug", text);
    let state = new_state(&mods_dir.0);
    state.compile_grug_file_from_str("m/x-A.grug", text, CompileSettings::safe()).unwrap();
}

// grug-tests covers "\n", but can't cover "\r\n", since grug-for-python reads
// "\r\n" as "\n"
#[test]
fn strings_cant_escape_a_windows_line_break() {
    assert_eq!(
        compile_error_string("m/x-A.grug", "x: string = \"a\\\r\n\"\n"),
        "  in (m/x-A.grug:1:15)\nError: Unexpected line break in string on line 1\n1 $ x: string = \"a\\"
    );
}

// grug-tests can't cover this, since a file name this long exceeds the 255 byte
// limit of most file systems
#[test]
fn entity_type_cant_be_too_long() {
    let path = format!("m/x-{}.grug", "A".repeat(421));
    assert_eq!(
        compile_error_string(&path, "x: number = 1\n"),
        format!(
            "Error: There are more than 421 characters \nin the entity type of '{0}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH\n$  {0}",
            path
        )
    );
}

#[cfg(unix)]
#[test]
fn entity_type_must_be_utf8() {
    use std::os::unix::ffi::OsStrExt;

    let path = OsStr::from_bytes(b"m/x-\xff.grug");
    assert_eq!(
        compile_error_string(path, "x: number = 1\n"),
        "Error: '\u{FFFD}' is not valid utf8\n$  \u{FFFD}"
    );
}

#[test]
fn compiling_a_missing_file_is_an_io_error() {
    let err = shared_state()
        .compile_grug_file("m/missing-A.grug", CompileSettings::safe())
        .expect_err("expected an IO error");
    // The OS error in between differs per platform
    let error_string = err.inner().error_string.to_str();
    assert!(error_string.starts_with("Error: IO error: "), "{}", error_string);
    assert!(error_string.ends_with("\n$ m/missing-A.grug"), "{}", error_string);
}

#[test]
fn recompiling_a_file_reinitializes_its_entities() {
    let mods_dir = ModsDir::new("recompile");
    mods_dir.write("m/x-A.grug", "x: number = 1\n");
    let state = new_state(&mods_dir.0);
    let file = state.compile_grug_file("m/x-A.grug", CompileSettings::safe()).unwrap();
    state.create_entity(file).unwrap();

    mods_dir.write("m/x-A.grug", "x: number = 2\n");
    assert_eq!(state.compile_grug_file("m/x-A.grug", CompileSettings::safe()).unwrap(), file);
}

#[test]
fn compile_all_files_compiles_every_grug_file() {
    let mods_dir = ModsDir::new("compile-all");
    mods_dir.write("m/ok-A.grug", "x: number = 1\n");
    mods_dir.write("m/nested/ok-A.grug", "x: number = 2\n");
    mods_dir.write("m/broken-A.grug", "x: number =");
    mods_dir.write("m/readme.txt", "not a grug file");
    let state = new_state(&mods_dir.0);

    let (ok_count, errors) = compile_all_errors(&state);
    assert_eq!(ok_count, 2);
    assert_eq!(errors, ["unexpected end of file"]);

    // Compiling everything again reinitializes the entities of each file
    let file = state.compile_grug_file("m/ok-A.grug", CompileSettings::safe()).unwrap();
    state.create_entity(file).unwrap();
    assert_eq!(compile_all_errors(&state).0, 2);
}

#[test]
fn compile_all_files_accepts_a_mods_dir_ending_in_a_slash() {
    let mods_dir = ModsDir::new("trailing-slash");
    mods_dir.write("m/ok-A.grug", "x: number = 1\n");
    let mut path = mods_dir.0.clone().into_os_string();
    path.push("/");
    let state = new_state(Path::new(&path));
    assert_eq!(compile_all_errors(&state), (1, Vec::new()));
}

#[cfg(unix)]
#[test]
fn compile_all_files_reports_files_it_cant_open() {
    use std::os::unix::fs::PermissionsExt;

    let mods_dir = ModsDir::new("unreadable");
    mods_dir.write("m/locked-A.grug", "x: number = 1\n");
    let locked = mods_dir.0.join("m/locked-A.grug");
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();
    // Root can open the file regardless of its permissions
    if std::fs::read(&locked).is_ok() {
        return;
    }
    let state = new_state(&mods_dir.0);
    let (ok_count, errors) = compile_all_errors(&state);
    assert_eq!(ok_count, 0);
    assert_eq!(errors.len(), 1);
}

// all_host_fns_registered is optional, so compiling has to check this too
#[test]
fn called_host_fn_must_be_registered() {
    let state = new_unregistered_state(Path::new(STR_MODS_DIR));
    let err = compile_error(&state, "m/x-D.grug", "export a() {\n    say(\"hi\")\n}\n");
    assert_eq!(
        err.inner().error_string.to_str(),
        "  in a (m/x-D.grug:2:5)\nError: The host function 'say' was not registered\n2 $ say(\"hi\")"
    );
}

/// A file that is gone when its compile is installed is an IO error, not a panic.
///
/// Installing a successful compile canonicalizes the file's path, so a `.grug` file deleted after
/// its text was read panicked out of `get_or_insert_script_id`, and a panic that crosses
/// `grug_update`'s `extern "C"` frame aborts the host process (#908). Deleting the file here is
/// the same deletion a hot reload races with, without the race.
#[test]
fn compiling_a_deleted_file_from_a_string_is_an_io_error() {
    let mods_dir = ModsDir::new("deleted-from-string");
    mods_dir.write("m/x-A.grug", "x: number = 1\n");
    let state = new_state(&mods_dir.0);
    state.compile_grug_file("m/x-A.grug", CompileSettings::safe()).unwrap();
    std::fs::remove_file(mods_dir.0.join("m/x-A.grug")).unwrap();

    let err = state
        .compile_grug_file_from_str("m/x-A.grug", "x: number = 2\n", CompileSettings::safe())
        .expect_err("expected an IO error for a file that is gone");
    // The OS error in between differs per platform
    let error_string = err.inner().error_string.to_str();
    assert!(
        error_string.starts_with("Error: IO error: "),
        "{}",
        error_string
    );
    assert!(
        error_string.ends_with("\n$ m/x-A.grug"),
        "{}",
        error_string
    );
}

/// Creates a FIFO, which a reader blocks on until a writer opens it or writes to it.
///
/// `std` cannot create one, and the only tests that need it are on Linux, where the poller is
/// tested too.
#[cfg(target_os = "linux")]
fn create_fifo(path: &Path) {
    let status = std::process::Command::new("mkfifo")
        .arg(path)
        .status()
        .expect("mkfifo should be installed");
    assert!(status.success(), "mkfifo failed");
}

/// The result of a compile, without the file id, so a test can carry it out of a thread.
#[cfg(target_os = "linux")]
fn compile_result_without_id(
    file: &gruggers::state::FileInfo<'_>,
) -> Result<(), String> {
    file.result()
        .map(|_| ())
        .map_err(|err| err.error_message.to_str().to_owned())
}

/// A `.grug` file deleted after the compiler thread read it and before the state installed it is a
/// deletion, not a panic.
///
/// The FIFO beside the victim is in the same chunk of compile requests as it, and the compiler
/// opens every file of a chunk before it reads any of them, so the victim is open once the FIFO's
/// read is what blocks. Deleting the victim then is the race a hot reload lost, arranged so that
/// it happens every time instead of one run in many.
#[cfg(target_os = "linux")]
#[test]
fn a_file_deleted_before_its_compile_is_installed_is_not_an_error() {
    let mods_dir = ModsDir::new("deleted-before-install");
    let state = new_state_with_poll_interval(&mods_dir.0, std::time::Duration::from_millis(5));
    // Both files appear after the watcher's baseline, so its next walk reports them as new
    mods_dir.write("m/x-A.grug", "x: number = 1\n");
    let fifo = mods_dir.0.join("m/z-Block.grug");
    create_fifo(&fifo);
    // Reading and writing keeps the compiler's open from blocking on a writer, so what the test
    // controls is the FIFO's read, which happens after every file of the chunk is open.
    let writer = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&fifo)
        .unwrap();
    // Nothing else wakes the test before the poller does, and the assertion below is what catches
    // a wait that was too short.
    std::thread::sleep(std::time::Duration::from_millis(500));

    let update = std::thread::spawn(move || {
        let (_, files) = state.update_files(CompileSettings::safe());
        files
            .files()
            .iter()
            .map(compile_result_without_id)
            .collect::<Vec<_>>()
    });
    std::thread::sleep(std::time::Duration::from_millis(500));
    std::fs::remove_file(mods_dir.0.join("m/x-A.grug")).unwrap();
    // Ends the block, so the compiler compiles both files and the state installs them
    drop(writer);

    let reported = update
        .join()
        .expect("update_files panicked on a file that was gone when its compile was installed");
    // The victim was read before it was deleted, so its compile succeeded and it is the install
    // that has to drop it. Reporting it as an error would mean it was never read.
    assert_eq!(
        reported.len(),
        1,
        "expected only the FIFO to be reported, got {:?}",
        reported
    );
    assert!(reported[0].is_err());
}

/// The same deletion during the walk `compile_all_files` does at startup is not a panic either.
#[cfg(target_os = "linux")]
#[test]
fn a_file_deleted_during_the_startup_walk_is_not_an_error() {
    let mods_dir = ModsDir::new("deleted-during-startup-walk");
    mods_dir.write("m/x-A.grug", "x: number = 1\n");
    let fifo = mods_dir.0.join("m/z-Block.grug");
    create_fifo(&fifo);
    let writer = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&fifo)
        .unwrap();

    let state = new_state(&mods_dir.0);
    let compile = std::thread::spawn(move || {
        let files = state.compile_all_files(CompileSettings::safe());
        files
            .files()
            .iter()
            .map(compile_result_without_id)
            .collect::<Vec<_>>()
    });
    std::thread::sleep(std::time::Duration::from_millis(500));
    std::fs::remove_file(mods_dir.0.join("m/x-A.grug")).unwrap();
    drop(writer);

    let reported = compile
        .join()
        .expect("compile_all_files panicked on a file that was gone when its compile was installed");
    assert_eq!(
        reported.len(),
        1,
        "expected only the FIFO to be reported, got {:?}",
        reported
    );
    assert!(reported[0].is_err());
}

/// A `.grug` symlink whose target is gone is reported as an IO error, not as a deletion.
///
/// The startup walk stats an entry without following it, so a symlink is not a path that vanished
/// while the walk listed it: opening it is what fails, and that is a real error the host has to see.
#[cfg(unix)]
#[test]
fn a_grug_symlink_to_a_gone_file_is_an_io_error_at_startup() {
    let mods_dir = ModsDir::new("dangling-symlink");
    std::os::unix::fs::symlink("nowhere.grug", mods_dir.0.join("m/gone-A.grug")).unwrap();
    let state = new_state(&mods_dir.0);
    let (ok_count, errors) = compile_all_errors(&state);
    assert_eq!(ok_count, 0);
    assert_eq!(errors.len(), 1);
    assert!(errors[0].starts_with("IO error: "), "{}", errors[0]);
}

/// grug's mode is per file, and a file's mode is its mod's: two files that
/// recurse deeper than the recursion limit are stopped in the mod the host left
/// safe and finish in the mod it granted, in the same call that compiles both.
///
/// This is what a host granting one mod unsafe mode asks for, and it is the
/// whole of the per-file input: the same file text, the same state and the same
/// compile call, differing only in the mod the decision names.
#[test]
fn a_files_mode_is_its_mods_mode() {
    let mods_dir = ModsDir::new("mode-per-mod");
    // Deeper than the recursion limit, so safe mode stops it and unsafe mode
    // runs all 5000 frames and returns
    let deep = "export a() {\n    _deep(5000)\n}\n\nlocal _deep(n: number) {\n    if n == 0 {\n        return\n    }\n    _deep(n - 1)\n}\n";
    mods_dir.write("safe/x-D.grug", deep);
    mods_dir.write("granted/x-D.grug", deep);
    let state = new_state(&mods_dir.0);

    let granted = OsStr::new("granted");
    let decide = move |mod_name: &OsStr| mod_name == granted;
    let settings = CompileSettings::new(&decide);
    let files = state.compile_all_files(settings);
    let mut ran = Vec::new();
    for file in files.files() {
        let id = file.result().expect("both files compile");
        let path = state.get_script_path_rel(id).unwrap().to_str().unwrap().to_owned();
        let entity = state.create_entity(id).expect("the entity is created");
        state.clear_error();
        let fn_id = state.get_export_fn_id("D", "a").unwrap();
        // False means a runtime error stopped the export function
        let finished = state.call_export_fn(&entity, fn_id, &[]);
        ran.push((path, finished));
        state.destroy_entity(entity);
    }

    assert_eq!(
        ran,
        [
            ("granted/x-D.grug".to_owned(), true),
            ("safe/x-D.grug".to_owned(), false),
        ]
    );
}

