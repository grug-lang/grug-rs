use std::alloc::Layout;
use std::mem::MaybeUninit;
use crate::ntstring::{NTOsStr, NTStr};
use std::alloc::Allocator;
use std::ffi::OsStr;

pub fn copy_str_as_ntstr<'a>(str: &'_ str, a: &'a impl Allocator) -> &'a NTStr {
    let slice = copy_bytes_as_nt(str.as_bytes(), a);
    // SAFETY:
    // 	- slice only contains utf8 because it comes from a str
    // 	- slice only contains a single null byte at the end
    // 		- no internal null bytes because of assert
    // 		- one null byte at end explicitly added
    unsafe { NTStr::from_str_unchecked(std::str::from_utf8_unchecked(slice)) }
}

pub fn copy_osstr_as_ntosstr<'a>(str: &'_ OsStr, a: &'a impl Allocator) -> &'a NTOsStr {
    let slice = copy_bytes_as_nt(str.as_encoded_bytes(), a);
    // SAFETY:
    // 	- slice is valid as an osstr, because it comes from an osstr, and only a null byte was added.
    // 	- slice only contains a single null byte at the end
    // 		- no internal null bytes because of assert
    // 		- one null byte at end explicitly added
    unsafe { NTOsStr::from_osstr_unchecked(OsStr::from_encoded_bytes_unchecked(slice)) }
}

pub fn copy_str<'a>(str: &'_ str, a: &'a impl Allocator) -> &'a str {
    let bytes = copy_bytes(str.as_bytes(), a);
    // bytes is directly copied from str which is utf8
    unsafe { std::str::from_utf8_unchecked(bytes) }
}

pub fn copy_bytes<'a>(str: &'_ [u8], a: &'a impl Allocator) -> &'a [u8] {
	let mut slice = box_new_uninit_slice(str.len(), a);
    // SAFETY: `slice` was just allocated within `a` with length `str.len`
    unsafe {
        slice
            .as_mut_ptr()
            .cast::<u8>()
            .copy_from(str.as_ptr(), str.len())
    };
    // SAFETY: Slice is fully initialized in the above line
    Box::leak(unsafe { slice.assume_init() })
}

pub fn copy_bytes_as_nt<'a>(str: &'_ [u8], a: &'a impl Allocator) -> &'a [u8] {
    assert!(!str.contains(&b'\0'));
	let mut slice = box_new_uninit_slice(str.len() + 1, a);
    // SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
    unsafe {
        slice
            .as_mut_ptr()
			.cast::<u8>()
            .copy_from(str.as_ptr(), str.len())
    };
    // SAFETY: `slice` was just allocated within `a` with length `str.len + 1`
    unsafe { slice.as_mut_ptr().cast::<u8>().add(str.len()).write(b'\0') };
    // SAFETY:
    // 	- Slice is fully initialized in the above line
    unsafe { Box::leak(slice.assume_init()) }
}

pub fn box_new_uninit_slice<T, A: Allocator>(len: usize, a: A) -> Box<[MaybeUninit<T>], A> {
	let ptr = a.allocate(Layout::array::<T>(len).unwrap()).unwrap().cast::<MaybeUninit<T>>();
	// allocate may return extra capacity, but we want a box with the exact requested length
	let ptr = std::ptr::slice_from_raw_parts_mut(ptr.as_ptr(), len);
	unsafe{Box::from_raw_in(ptr, a)}
}
