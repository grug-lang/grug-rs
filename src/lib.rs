// #![deny(warnings)]

pub mod bindings;
pub mod frontend;
pub mod backend;

pub mod mod_api;
pub mod types;
pub mod serde;
pub mod error;
pub mod state;

#[cfg(test)]
mod test {
	use super::bindings::*;
	use std::ffi::CString;
	use std::mem::ManuallyDrop;
	use crate::mod_api::*;
	use crate::state::GrugState;

	#[test] 
	fn grug_tests () {
		let mut args = std::env::args().collect::<Vec<_>>();

		let mut whitelisted_test = std::ptr::null();
		if args.len() == 3 {
			whitelisted_test = ManuallyDrop::new(CString::new(args.pop().unwrap()).unwrap()).as_ptr();
		} else if args.len() > 3 {
			eprintln!("usage: cargo test -- grug_tests <whitelisted_test>");
			std::process::exit(2);
		}
		// let grug_path = &*args[1];
		// println!("{}", grug_path);

		// let 

		let grug_tests_path = c"src/grug-tests/tests";
		let mod_api_text = std::fs::read_to_string("src/grug-tests/mod_api.json").unwrap();

		let state = GrugState::new("src/grug-tests/mod_api.json", grug_tests_path.to_str().unwrap()).unwrap();
		// register_game_functions(&mut state);
			
		_ = GLOBAL_TEST_STATE.set(state);

		unsafe {
			grug_tests_run(
				grug_tests_path.as_ptr(),
				compile_grug_file,
				init_globals_fn_dispatcher,
				on_fn_dispatcher,
				dump_file_to_json,
				generate_file_from_json,
				game_fn_error,
				whitelisted_test,
			)
		}
	}
}

// TODO: implementation errors like too many statements in block should not be grug errors
// TODO: Better Error handling
