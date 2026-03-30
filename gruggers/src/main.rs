use std::path::Path;

use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::backend::StubBackend;

const BIN_NAME: &'static str = "grugc";

type Result<T> = core::result::Result<T, Error>;
type Error = Box<dyn std::error::Error>;

#[derive(Debug)]
struct CliArgs {
	files_to_compile: Vec<String>,
	mod_api_path: Option<String>,
}

fn main() -> Result<()>{
	let args = match parse_args() {
		Ok(args) => args,
		Err(err) => {
			println!("{}", err);
			print_basic_usage();
			std::process::exit(1);
		}
	};

	let mod_api_path = match args.mod_api_path {
		Some(path) => path,
		None => search_mod_api_path()?,
	};

	let mut grug_state = GrugInitSettings::new()
		.set_mod_api_path(&mod_api_path)
		.set_mods_dir("")
		.set_backend(StubBackend)
		.build_state().unwrap();

	// SAFETY: We never call a script created from this compiler
	unsafe{grug_state.register_dummies()}

	for file_to_compile in args.files_to_compile {
		compile_files(&grug_state, &file_to_compile)?;
		println!("No errors in {}", file_to_compile);
	}
	
	Ok(())
}

fn compile_files<P: AsRef<Path>>(state: &GrugState, path: P) -> Result<()> {
	let Ok(metadata) = std::fs::metadata(&path) else {
		println!("Could not open path {}", path.as_ref().display());
		std::process::exit(2);
	};
	
	if metadata.is_dir() {
		for dir_entry in std::fs::read_dir(path)? {
			compile_files(state, &dir_entry?.path())?;
		}
	} else if let Some(extension) = path.as_ref().extension() && extension == "grug" {
		let file_text = std::fs::read_to_string(&path)?;
		match state.compile_grug_file_from_str(&path.as_ref().to_string_lossy(), &file_text) {
			Ok(_) => (),
			Err(err) => {
				println!("Error in {}: {}", path.as_ref().display(), err);
			}
		}
	}
	Ok(())
}

fn search_mod_api_path() -> Result<String> {
	let mut cwd = std::env::current_dir()?;
	loop {
		cwd.push("mod_api.json");
		if let Ok(true) = std::fs::exists(&cwd) {return Ok(String::from(cwd.to_string_lossy()))};
		if !cwd.pop() {Err("Could not find mod_api in current or parent directory")?;}
		if !cwd.pop() {Err("Could not find mod_api in current or parent directory")?;}
	}
}

fn parse_args() -> Result<CliArgs> {
	let mut args = std::env::args();
	// First arg is the name of the executable
	args.next().unwrap();

	let mut files_to_compile = Vec::new();
	let mut mod_api_path = None;
	
	while let Some(next_arg) = args.next() {
		if next_arg == "-m" {
			if mod_api_path.is_some() {
				Err("Mod api path can only appear once in the arguments")?;
			}
			let Some(actual_path) = args.next() else {
				return Err(Box::from("Expected path to mod api after '-m'")).into();
			};
			mod_api_path = Some(String::from(actual_path));
		} else if next_arg == "-i" {
			let Some(actual_path) = args.next() else {
				return Err(Box::from("Expected path to file after '-i'")).into();
			};
			files_to_compile.push(String::from(actual_path));
		} else if next_arg.starts_with("-") {
			return Err(Box::from("Unexpected switch in arguments")).into();
		} else {
			files_to_compile.push(String::from(next_arg));
		}
	}
	if files_to_compile.is_empty() {
		return Err(Box::from("Expected at least one file to compile")).into();
	}
	Ok(CliArgs {
		files_to_compile,
		mod_api_path,
	})
}

fn print_basic_usage() {
	println!("Usage: {BIN_NAME} ([-i] <path-to-grug-file>)* [-m <path-to-mod-api-json>]");
}
