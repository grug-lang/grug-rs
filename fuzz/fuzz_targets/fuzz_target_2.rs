#![no_main]

use libfuzzer_sys::fuzz_target;

use allocator_api2::alloc::Global;
use gruggers_core::ntstring::NTStr;

fuzz_target!(|data: &str| {
	let str = NTStr::box_from_str_in(data, Global);
	let str = str.as_ntstrptr();
	assert_eq!(str.const_len(), str.len(), "{}", data);
});

