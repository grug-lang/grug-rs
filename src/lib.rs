// #![allow(warnings)]

mod bindings;
mod frontend;

#[cfg(test)]
mod test {
	use super::bindings::*;

	#[test] 
	fn test () {
		let args = std::env::args().collect::<Vec<_>>();

		// if args.len() < 2 {
		// 	println!("Test Usage: cargo test -- <path/to/grug-tests/>");
		// 	panic!();
		// }
		// let grug_path = &*args[1];
		// println!("{}", grug_path);

		// let 
		
		unsafe {
			grug_tests_run(
				c"src/grug-tests/tests/".as_ptr(),
				compile_grug_file,
				init_globals_fn_dispatcher,
				on_fn_dispatcher,
				dump_file_to_json,
				generate_file_from_json,
				game_fn_error,
				std::ptr::null(),
			)
		}
	}
}

// TODO: implementation errors like too many statements in block should not be grug errors
