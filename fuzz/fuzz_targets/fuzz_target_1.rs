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

fuzz_target!(|data: &str| {
	let state = GrugState::new_from_text(MOD_API, "mods", Default::default(), BytecodeBackend::new()).unwrap();
	state.compile_grug_file_from_str("test/test-A.grug", data);
});
