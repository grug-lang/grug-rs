#![no_main]

use libfuzzer_sys::fuzz_target;
use gruggers::state::GrugState;
use gruggers::backend::BytecodeBackend;

const MOD_API: &str = r#"{
	"entities": {
		"A": {
			"on_functions": {
				
			}
		}
	},
	"game_functions": {
		"test": {
			"return_type": "boolean",
			"arguments": []
		}
	}
}"#;

fuzz_target!(|data: &[u8]| {
	let data = match std::str::from_utf8(data) {
		Ok(data) => data,
		Err(err) => return,
	};
	let state = GrugState::new_from_text(MOD_API, "mods", Default::default(), BytecodeBackend::new()).unwrap();
	state.compile_grug_file_from_str("test-A.grug", data);
});
