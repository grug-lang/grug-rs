use gruggers::ast::Type;
use gruggers::backend::BytecodeBackend;
use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::types::Value;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn mod_api(optional: &str) -> String {
    format!(
        r#"{{
	"entities": {{
		"D": {{
			"description": "a test entity",
			"export_functions": [
				{{
					"name": "run",
					"description": "runs"
				}}
			]
		}}
	}},
	"classes": {{}},
	"host_functions": {{
		"probe": {{
			"description": "probes a path",
			"parameters": [
				{{
					"name": "path",
					"type": {{
						"name": "resource",
						"resource_extension": ".txt"{optional}
					}}
				}}
			]
		}}
	}}
}}"#
    )
}

fn workspace() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "grug_optional_resource_{}_{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("mods/mymod/code")).unwrap();
    fs::write(
        dir.join("mods/mymod/code/probe-D.grug"),
        "export run() {\n    probe(r\"does/not/exist.txt\")\n}\n",
    )
    .unwrap();
    fs::write(dir.join("mod_api.json"), mod_api("")).unwrap();
    dir
}

extern "C" fn probe(
    _state: &GrugState,
    _values: *const Value,
    _generics: &'static [Type<'static>; 0],
) -> Value {
    Value { void: () }
}

fn build_state(dir: &Path, api: &str) -> Result<GrugState, gruggers::error::Error> {
    let mod_api_path = dir.join("mod_api.json");
    fs::write(&mod_api_path, api).unwrap();
    let mods_dir = dir.join("mods");
    let mut state = GrugInitSettings::new()
        .set_mod_api_path(mod_api_path.as_os_str())
        .set_mods_dir(mods_dir.as_os_str())
        .set_backend(BytecodeBackend::new())
        .build_state()?;
    unsafe { state.register_host_fn("probe", probe) }.expect("probe should register");
    Ok(state)
}

fn compile_probe(state: &GrugState) -> Result<(), String> {
    // Paths given to compile_grug_file are relative to the mods directory.
    state
        .compile_grug_file("mymod/code/probe-D.grug")
        .map(|_| ())
        .map_err(|err| err.to_string())
}

#[test]
fn an_optional_resource_compiles_even_when_the_path_does_not_exist() {
    let dir = workspace();
    let state = build_state(&dir, &mod_api(r#", "optional": true"#)).expect("mod_api should parse");
    compile_probe(&state).expect("an optional resource should not have to exist");
}

#[test]
fn a_resource_is_still_required_to_exist_by_default() {
    let dir = workspace();
    let state = build_state(&dir, &mod_api("")).expect("mod_api should parse");
    let error = compile_probe(&state).expect_err("a required resource must exist");
    assert!(error.contains("does not exist"), "{error}");
}

#[test]
fn optional_must_be_a_boolean() {
    let dir = workspace();
    let error = match build_state(&dir, &mod_api(r#", "optional": "yes""#)) {
        Ok(_) => panic!("a non-boolean optional should be rejected"),
        Err(error) => error,
    };
    assert!(error.to_string().contains("is not a boolean"), "{error}");
}
