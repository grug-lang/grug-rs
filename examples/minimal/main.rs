#![deny(warnings)]
use gruggers::state::GrugInitSettings;
use gruggers::types::GrugValue;

use std::ffi::CStr;
use std::time::Duration;

mod game_fns {
	use super::*;
	pub extern "C" fn print_string(arguments: *const GrugValue) {
		unsafe {
			let string = CStr::from_ptr((*arguments).string).to_str().unwrap();
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
	state.register_game_fn("print_string", print_string as extern "C" fn(_)).unwrap();
	assert!(state.all_game_fns_registered());

	let id = state.compile_grug_file("goldie/first-Dog.grug").unwrap();
	let dog = state.create_entity(id).unwrap();
	let on_bark_id = state.get_on_fn_id("Dog", "on_bark").unwrap();

	loop {
		if state.call_on_function(&*dog, on_bark_id, &[GrugValue{string: c"woof".as_ptr().cast()}]) {panic!()};
		if state.call_on_function(&*dog, on_bark_id, &[GrugValue{string: c"arf".as_ptr().cast()}]) {panic!()};
		std::thread::sleep(Duration::from_secs(1));
	}
}
