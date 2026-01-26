use gruggers::state::GrugState;
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
	let mut state = GrugState::new("examples/minimal/mod_api.json", "examples/minimal/mods").unwrap();
	state.register_game_fn("print_string", print_string as extern "C" fn(_));
	assert!(state.all_game_fns_registered());

	let id = state.compile_grug_file("goldie/first-Dog.grug").unwrap();
	let dog = state.create_entity(id).unwrap();
	let on_bark_id = state.get_on_fn_id("Dog", "on_bark").unwrap();

	loop {
		state.call_on_function(dog, on_bark_id, &[GrugValue{string: c"woof".as_ptr().cast()}]).unwrap();
		state.call_on_function(dog, on_bark_id, &[GrugValue{string: c"arf".as_ptr().cast()}]).unwrap();
		std::thread::sleep(Duration::from_secs(1));
	}
}
