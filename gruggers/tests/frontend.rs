//! Tests for compiler behavior that grug-tests doesn't cover, because it is
//! specific to gruggers or to its API.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

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

fn compile_error(state: &GrugState, path: impl AsRef<OsStr>, text: &str) -> Error {
    state
        .compile_grug_file_from_str(path, text)
        .expect_err("expected a compile error")
}

/// The full error string of compiling `text` as the file at `path`
fn compile_error_string(path: impl AsRef<OsStr>, text: &str) -> String {
    let state = new_state(Path::new(STR_MODS_DIR));
    compile_error(&state, path, text)
        .inner()
        .error_string
        .to_str()
        .to_owned()
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
    state.compile_grug_file_from_str("m/x-A.grug", text).unwrap();
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
    let state = new_state(Path::new(STR_MODS_DIR));
    let err = state
        .compile_grug_file("m/missing-A.grug")
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
