//! Tests for compiler behavior that grug-tests doesn't cover, because it is
//! specific to gruggers or to its API.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

use gruggers::backend::BytecodeBackend;
use gruggers::state::{GrugInitSettings, GrugState};

const MOD_API_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/grug-tests/mod_api.json");

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

fn new_state(mods_dir: &Path) -> GrugState {
    let mut state = GrugInitSettings::new()
        .set_mod_api_path(MOD_API_PATH)
        .set_mods_dir(mods_dir.as_os_str())
        .set_poll_interval(Duration::from_millis(20))
        .set_backend(BytecodeBackend::new())
        .build_state()
        .unwrap();
    // SAFETY: None of the scripts in these tests call a host function
    unsafe { state.register_dummies() };
    state
}

fn compile_error(path: &str, text: &str) -> String {
    let mods_dir = ModsDir::new("compile-error");
    let state = new_state(&mods_dir.0);
    let err = state
        .compile_grug_file_from_str(path, text)
        .expect_err("expected a compile error");
    err.inner().error_message.to_str().to_owned()
}

/// The error messages of the files that `compile_all_files` failed to compile
fn compile_all_errors(state: &GrugState) -> (usize, Vec<String>) {
    let files = state.compile_all_files();
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
    assert_eq!(
        compile_error("m/x-D.grug", &text),
        "There is a function that contains more than 100 levels of nested expressions"
    );
}

// TODO: Move to grug-tests if grug-lang/grug-tests#64, which proposes these
// escape sequences, is accepted
#[test]
fn strings_can_contain_escape_sequences() {
    let mods_dir = ModsDir::new("escapes");
    let text = "x: string = \"a\\tb\\nc\\rd\\\\e\\\"f\"\n";
    mods_dir.write("m/x-A.grug", text);
    let state = new_state(&mods_dir.0);
    state.compile_grug_file_from_str("m/x-A.grug", text).unwrap();
}

// grug-tests covers "\n", but can't cover "\r\n", since grug-for-python reads
// "\r\n" as "\n"
#[test]
fn strings_cant_escape_a_windows_line_break() {
    assert_eq!(
        compile_error("m/x-A.grug", "x: string = \"a\\\r\n\"\n"),
        "Unexpected line break in string on line 1"
    );
}

// grug-tests can't cover this, since a file name this long exceeds the 255 byte
// limit of most file systems
#[test]
fn entity_type_cant_be_too_long() {
    let path = format!("m/x-{}.grug", "A".repeat(421));
    assert!(compile_error(&path, "x: number = 1\n").contains("exceeding MAX_FILE_ENTITY_TYPE_LENGTH"));
}

#[cfg(unix)]
#[test]
fn entity_type_must_be_utf8() {
    use std::os::unix::ffi::OsStrExt;

    let mods_dir = ModsDir::new("non-utf8");
    let state = new_state(&mods_dir.0);
    let path = std::ffi::OsStr::from_bytes(b"m/x-\xff.grug");
    let err = state
        .compile_grug_file_from_str(path, "x: number = 1\n")
        .expect_err("expected a compile error");
    assert!(err.inner().error_message.to_str().ends_with("is not valid utf8"));
}

#[test]
fn compiling_a_missing_file_is_an_io_error() {
    let mods_dir = ModsDir::new("missing-file");
    let state = new_state(&mods_dir.0);
    let err = state
        .compile_grug_file("m/missing-A.grug")
        .expect_err("expected an IO error");
    assert!(err.inner().error_message.to_str().starts_with("IO error: "));
}

#[test]
fn recompiling_a_file_reinitializes_its_entities() {
    let mods_dir = ModsDir::new("recompile");
    mods_dir.write("m/x-A.grug", "x: number = 1\n");
    let state = new_state(&mods_dir.0);
    let file = state.compile_grug_file("m/x-A.grug").unwrap();
    state.create_entity(file).unwrap();

    mods_dir.write("m/x-A.grug", "x: number = 2\n");
    assert_eq!(state.compile_grug_file("m/x-A.grug").unwrap(), file);
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
    let file = state.compile_grug_file("m/ok-A.grug").unwrap();
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

#[test]
fn update_files_compiles_new_files() {
    let mods_dir = ModsDir::new("update-new-file");
    mods_dir.write("m/old-A.grug", "x: number = 1\n");
    let state = new_state(&mods_dir.0);
    compile_all_errors(&state);

    mods_dir.write("m/new-A.grug", "x: number = 2\n");
    // Give the file watcher, which polls every 20ms, time to see the new file
    std::thread::sleep(Duration::from_millis(200));
    let (_, updated) = state.update_files();
    let results: Vec<_> = updated.files().iter().map(|file| file.result().is_ok()).collect();
    assert_eq!(results, [true]);
}
