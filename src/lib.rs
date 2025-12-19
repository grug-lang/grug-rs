// #![allow(warnings)]

pub mod bindings;
pub mod frontend;

#[cfg(test)]
mod test {
	use super::bindings::*;
	use std::ffi::CString;
	use std::mem::ManuallyDrop;
	use crate::frontend::mod_api::*;

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

		let grug_tests_path = c"src/grug-tests/tests/";

		let mod_api_text = std::fs::read_to_string("src/grug-tests/mod_api.json").unwrap();

		let mod_api = get_mod_api(&mod_api_text).unwrap();
		// panic!();
		panic!("{:#?}", mod_api);
		
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
