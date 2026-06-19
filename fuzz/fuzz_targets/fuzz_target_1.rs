#![no_main]

use libfuzzer_sys::fuzz_target;
use gruggers::state::GrugState;
use gruggers::backend::BytecodeBackend;

use std::cell::RefCell;

const MOD_API: &str = r#"{
	"entities": {
		"A": {
			"export_functions": [ ]
		}
	},
	"classes": {},
	"host_functions": {
		"test": {
			"return_type": "boolean",
			"parameters": []
		}
	}
}"#;

thread_local! {
	static STATE: RefCell<Option<GrugState>> = RefCell::new(None);
}

fuzz_target!(|data: &str| {
	STATE.with_borrow_mut(|state: &mut Option<GrugState>| {
		let state = state.get_or_insert_with(|| GrugState::new_from_text(MOD_API, "./", Default::default(), BytecodeBackend::new()).unwrap());
		_ = state.compile_grug_file_from_str("test/test-A.grug", data);
	});
});

