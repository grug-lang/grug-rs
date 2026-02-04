#![deny(warnings)]
use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::types::GrugValue;
use gruggers::nt;

use std::time::Duration;

mod game_fns {
	use super::*;
	pub extern "C" fn print_string<'a>(_state: &'a GrugState, arguments: *const GrugValue) {
		unsafe {
			let string = (*arguments).string.to_str();
			println!("{}", string);
		}
	}
}
use game_fns::*;

fn main () {
	let mut state = GrugInitSettings::new()
		.set_mods_dir("examples/minimal/mods")
		.set_mod_api_path("examples/minimal/mod_api.json")
		.build_state().unwrap();
	state.register_game_fn("print_string", print_string as for<'a> extern "C" fn(&'a GrugState, _)).unwrap();
	assert!(state.all_game_fns_registered());

	let id = state.compile_grug_file("goldie/first-Dog.grug").unwrap();
	let dog = state.create_entity(id).unwrap();
	let on_bark_id = state.get_on_fn_id("Dog", "on_bark").unwrap();

	loop {
		if state.call_on_function(&*dog, on_bark_id, &[GrugValue{string: nt!("woof").as_ntstrptr()}]) {panic!()};
		if state.call_on_function(&*dog, on_bark_id, &[GrugValue{string: nt!("arg").as_ntstrptr()}]) {panic!()};
		std::thread::sleep(Duration::from_secs(1));
	}
}
