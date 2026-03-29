use std::path::PathBuf;

use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::backend::StubBackend;

const BIN_NAME: &'static str = "grugc";

type Result<T> = core::result::Result<T, Error>;
type Error = Box<dyn std::error::Error>;

fn main() -> Result<()>{
	let cwd = String::from(std::env::current_dir()?.to_string_lossy());
	let args = std::env::args().collect::<Vec<_>>();
	let Some(file_to_compile) = args.get(1) else {
		println!("Usage: {BIN_NAME} <path-to-file>");
		std::process::exit(2);
	};

	let mod_api_path = match get_mod_api_path() {
		Ok(m) => m,
		Err(err) => {
			println!("error finding mod_api: {}", err);
			std::process::exit(2);
		}
	};
	let mod_api_path = mod_api_path.to_string_lossy();

	let mut grug_state = GrugInitSettings::new()
		.set_mod_api_path(&mod_api_path)
		.set_mods_dir("")
		.set_backend(StubBackend)
		.build_state().unwrap();

	// SAFETY: We never call a script created from this compiler
	unsafe{grug_state.register_dummies()}

	let file_text = match std::fs::read_to_string(&file_to_compile) {
		Ok(file) => file,
		Err(err) => {
			println!("Could not open file {} : {}", file_to_compile, err);
			std::process::exit(2);
		}
	};

	match grug_state.compile_grug_file_from_str(&file_to_compile, &file_text) {
		Ok(file_id) => {
			println!("No errors: {}", file_to_compile);
		}
		Err(err) => {
			println!("Error: {}: {}", file_to_compile, err);
		}
	}
	
	Ok(())
}

fn get_mod_api_path() -> Result<PathBuf> {
	let mut cwd = std::env::current_dir()?;
	loop {
		cwd.push("mod_api.json");
		if let Ok(true) = std::fs::exists(&cwd) {return Ok(cwd)};
		if !cwd.pop() {Err("Could not find mod_api in current or parent directory")?;}
		if !cwd.pop() {Err("Could not find mod_api in current or parent directory")?;}
	}
}
