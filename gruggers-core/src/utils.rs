use crate::ntstring::{NTBytes, NTStr};
use allocator_api2::alloc::Allocator;
use allocator_api2::boxed::Box;

pub fn copy_str_nt<'a>(str: &'_ NTStr, a: &'a impl Allocator) -> &'a NTStr {
	let bytes = copy_bytes(str.as_str_with_null().as_bytes(), a);
	// bytes is directly copied from str which is utf8, contains no internal
	// null bytes and has a null terminator
	unsafe{NTStr::from_str_unchecked(std::str::from_utf8_unchecked(bytes))}
}

pub fn copy_str_as_ntstr<'a>(str: &'_ str, a: &'a impl Allocator) -> &'a NTStr {
	assert!(!str.as_bytes().contains(&b'\0'));
	let mut slice = Box::<[u8], _>::new_uninit_slice_in(str.len() + 1, a);
	// SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
	unsafe{slice.as_mut_ptr().cast::<u8>().copy_from(str.as_ptr(), str.len())};
	// SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
	unsafe{slice.as_mut_ptr().cast::<u8>().add(str.len()).write(b'\0')};
	// SAFETY: 
	// 	- Slice is fully initialized in the above line
	// 	- slice only contains utf8 because it comes from a str
	// 	- slice only contains a single null byte at the end
	// 		- no internal null bytes because of assert
	// 		- one null byte at end explicitly added
	unsafe{NTStr::from_str_unchecked(std::str::from_utf8_unchecked(Box::leak(slice.assume_init())))}
}

pub fn copy_bytes_as_nt<'a>(str: &'_ [u8], a: &'a impl Allocator) -> NTBytes<'a> {
	assert!(!str.contains(&b'\0'));
	let mut slice = Box::<[u8], _>::new_uninit_slice_in(str.len() + 1, a);
	// SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
	unsafe{slice.as_mut_ptr().cast::<u8>().copy_from(str.as_ptr(), str.len())};
	// SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
	unsafe{slice.as_mut_ptr().cast::<u8>().add(str.len()).write(b'\0')};
	// SAFETY: 
	// 	- Slice is fully initialized in the above line
	// 	- slice only contains a single null byte at the end
	// 		- no internal null bytes because of assert
	// 		- one null byte at end explicitly added
	unsafe{NTBytes::from_bytes_unchecked(Box::leak(slice.assume_init()))}
}

pub fn copy_bytes_nt<'a>(str: NTBytes<'_>, a: &'a impl Allocator) -> NTBytes<'a> {
	let bytes = copy_bytes(str.to_bytes_with_null(), a);
	// bytes is directly copied from str which contains no internal
	// null bytes and has a null terminator
	unsafe{NTBytes::from_bytes_unchecked(bytes)}
}

pub fn copy_str<'a>(str: &'_ str, a: &'a impl Allocator) -> &'a str {
	let bytes = copy_bytes(str.as_bytes(), a);
	// bytes is directly copied from str which is utf8
	unsafe{std::str::from_utf8_unchecked(bytes)}
}

pub fn copy_bytes<'a>(str: &'_ [u8], a: &'a impl Allocator) -> &'a [u8] {
	let mut slice = Box::<[u8], _>::new_uninit_slice_in(str.len(), a);
	// SAFETY: `slice` was just allocated within `a` with length `str.len`
	unsafe{slice.as_mut_ptr().cast::<u8>().copy_from(str.as_ptr(), str.len())};
	// SAFETY: Slice is fully initialized in the above line
	Box::leak(unsafe{slice.assume_init()})
}
