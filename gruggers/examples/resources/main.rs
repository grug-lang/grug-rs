#![deny(warnings)]
use gruggers::state::{GrugInitSettings, GrugState};

use std::time::Duration;

mod game_fns {
	use super::*;
	pub fn print_string<'a>(_state: &'a GrugState, input: &str) {
		println!("{}", input);
	}
	pub fn print_file<'a>(_state: &'a GrugState, input_path: &str) {
		let mut path = std::path::PathBuf::from(_state.mods_dir_path());
		path.push(input_path);

		print!("{}", std::fs::read_to_string(path).unwrap());
	}
}
use game_fns::*;

fn main () {
	let mut state = GrugInitSettings::new()
		.set_mods_dir("gruggers/examples/resources/mods")
		.set_mod_api_path("gruggers/examples/resources/mod_api.json")
		.build_state().unwrap();
	state.register_host_fn("print_string", print_string).unwrap();
	state.register_host_fn("print_file", print_file).unwrap();
	state.all_host_fns_registered().unwrap();

	let files = state.compile_all_files();
	let id = *files.files()[0].result().as_ref().unwrap();
	let dog = state.create_entity(id).unwrap();
	let on_bark_id = state.get_export_fn_id("Dog", "bark").unwrap();

	loop {
		let (resources, files) = state.update_files();
		for resource in resources {print!("{}, ", resource.display())};
		println!("");
		for file in files.files() {if let Err(err) = file.result() {println!("{}, ", err)}};
		println!("{:?}", state.update_files());
		if !state.call_on_function(&*dog, on_bark_id, &[]) {panic!()};
		std::thread::sleep(Duration::from_secs(1));
	}
}
