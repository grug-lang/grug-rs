// #![deny(warnings)]
#![allow(clippy::single_char_add_str, clippy::bool_comparison)]

pub mod frontend;
pub mod backend;

pub mod mod_api;
pub mod types;
pub mod serde;
pub mod error;
pub mod state;

mod ntstring;

mod cachemap;


// Ideal Usage 
// 
// let state = GrugState::new(
// 		"mod_api_path",
// 		"mods_path",
// );
// for func in funcs {
// 	state.register_fn(func.name, func.ptr);
// }
// if !state.all_game_fns_registered() {
// 	return 0;
// }
// state.compile_grug_file("test/hello-A.grug");
// let id = state.create_entity("test/hello-A.grug")
// id.call_on_fn("on_b", grug_args![25, 30, 32]);

// TODO: implementation errors like 'too many statements in block' should not be grug errors
// TODO: Better Error handling
