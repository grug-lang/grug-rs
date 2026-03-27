use std::path::PathBuf;

use gruggers::state::{GrugInitSettings, GrugState};

type Result<T> = core::result::Result<T, Error>;
type Error = Box<dyn std::error::Error>;

fn main() -> Result<()>{
	let cwd = std::env::current_dir()?;
	println!("cwd: {:?}", cwd);
	let args = std::env::args().collect::<Vec<_>>();
	println!("args: {:?}", args);
	let file_to_compile = args.get(1);
	println!("file_to_compile: {:?}", file_to_compile);
	let mod_api_path = get_mod_api_path()?;
	let mod_api_path = mod_api_path.to_string_lossy();
	println!("mod_api_path: {:?}", mod_api_path);

	let grug_state = GrugInitSettings::new()
		.set_mod_api_path(&mod_api_path)
		.set_mods_dir("")
		.build_state().unwrap();
	
	

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
