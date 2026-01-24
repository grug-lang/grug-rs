use gruggers::*;
use gruggers::state::GrugState;
use gruggers::error::GrugError;
use gruggers::types::GrugValue;

use std::ffi::CStr;
use std::time::Duration;

mod game_fns {
	use super::*;
	pub extern "C" fn print_number(arguments: *const GrugValue) {
		unsafe {
			let number = (*arguments).number;
			println!("{}", number);
		}
	}
	pub extern "C" fn print_string(arguments: *const GrugValue) {
		unsafe {
			let string = CStr::from_ptr((*arguments).string).to_str().unwrap();
			println!("{}", string);
		}
	}
}
use game_fns::*;

fn main () {
	let mut state = GrugState::new("examples/fib_naive/mod_api.json", "examples/fib_naive/mods").unwrap();
	state.register_game_fn("print_string", print_string as extern "C" fn(_));
	state.register_game_fn("print_number", print_number as extern "C" fn(_));
	assert!(state.all_game_fns_registered());

	state.compile_grug_file("fib_script/entity-Fib.grug").unwrap();
	let script = state.create_entity("fib_script/entity-Fib.grug").unwrap();

	println!("Naive implementation");
	for i in 0.. {
		let Ok(_) = state.call_on_function(script, "on_fib_naive", &[GrugValue{number:i as f64}]) else {
			break
		};
	}
	
	println!("iterative implementation");
	for i in (0..50) {
		print!("{i} : ");
		let Ok(_) = state.call_on_function(script, "on_fib_iterative", &[GrugValue{number:i as f64}]) else {
			break
		};
	}
}
