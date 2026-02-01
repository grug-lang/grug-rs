use std::sync::Arc;
use std::ops::Deref;
use std::ffi::{CStr, c_char};
use std::ptr::NonNull;
use std::marker::PhantomData;

/// represents a utf-8 string with a single null byte at the end.
#[repr(transparent)]
pub struct NTStr(str);

impl NTStr {
	pub fn arc_from_str (value: &str) -> Arc<Self> {
		let arc = Arc::into_raw(Arc::<[u8]>::new_uninit_slice(value.len() + 1));

		unsafe{
			std::ptr::copy(value.as_ptr(), arc.cast_mut().cast(), value.len());
			arc.cast_mut().cast::<u8>().add(value.len()).write(b'\0');
			std::mem::transmute(Arc::from_raw(arc).assume_init())
		}
	}

	// Does not include the null byte
	pub fn len(&self) -> usize {
		self.0.len() - 1
	}

	pub fn as_str(&self) -> &str {
		&self.0[..(self.0.len() - 1)]
	}

	pub fn as_str_with_null(&self) -> &str {
		&self.0
	}

	/// The last byte of `value` MUST be a null byte and there must be no other null byte in between
	pub unsafe fn from_str_unchecked(value: &str) -> &Self {
		unsafe {std::mem::transmute::<&str, &NTStr>(value)}
	}
	
	pub fn from_str(value: &str) -> Option<&NTStr> {
		if *value.as_bytes().last()? == b'\0' {
			for byte in &value.as_bytes()[0..value.len()-1] {
				if *byte == b'\0' {return None}
			}
			// SAFETY: last byte (if it exists) is null
			unsafe{Some(Self::from_str_unchecked(value))}
		} else {
			None
		}
	}

	pub fn as_ntstrptr(&self) -> NTStrPtr<'_> {
		// SAFETY There is a null byte at the self.len()
		unsafe{NTStrPtr::from_ptr(NonNull::from_ref(&self.0).cast::<i8>())}
	}
}

impl std::fmt::Debug for NTStr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.deref().fmt(f)
	}
}

impl std::fmt::Display for NTStr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.deref().fmt(f)
	}
}

impl<'a> TryFrom<&'a str> for &'a NTStr {
	type Error = ();
	fn try_from(value: &str) -> Result<&NTStr, Self::Error> {
		NTStr::from_str(value).ok_or(())
	}
}

impl std::ops::Deref for NTStr {
	type Target = str;
	fn deref(&self) -> &Self::Target {
		self.as_str()
	}
}

impl AsRef<str> for NTStr {
	fn as_ref(&self) -> &str {
		&self.0[..(self.0.len() - 1)]
	}
}

impl AsRef<CStr> for NTStr {
	fn as_ref(&self) -> &CStr {
		// SAFETY: There is a single null byte at the end
		unsafe{CStr::from_bytes_with_nul_unchecked(self.0.as_bytes())}
	}
}

impl<'a> From<&'a NTStr> for String {
	fn from(other: &'a NTStr) -> String {
		String::from(other.as_str_with_null())
	}
}


#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn ntstr_test() {
		let str = "hello\0";
		let ntstr = <&NTStr>::try_from(str).unwrap();
		let str2 = &**ntstr;
		assert_eq!(str2, "hello");
	}
}

/// Represents a null terminated UTF-8 string as a single pointer (unlike CStr which uses two)
/// Requires that there is atleast one null byte between the pointer and the end of the buffer it points to
/// This pointer must be valid to read until the null byte
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct NTStrPtr<'a>(NonNull<c_char>, PhantomData<&'a ()>);
const _: () = const {assert!(std::mem::size_of::<NTStrPtr>() ==  std::mem::size_of::<Option<NTStrPtr>>())};

impl<'a> NTStrPtr<'a> {
	pub fn as_ptr(self) -> *const u8 {
		self.0.cast::<u8>().as_ptr().cast_const()
	}
	pub unsafe fn from_ptr (ptr: NonNull<c_char>) -> Self {
		Self(ptr, PhantomData)
	}

	pub fn to_cstr(self) -> &'a CStr {
		unsafe{CStr::from_ptr(self.0.as_ptr().cast_const())}
	}

	pub fn to_ntstr(self) -> &'a NTStr {
		let mut i = 0;
		loop {
			// SAFETY: buffer is okay to read upto the next null byte
			if unsafe{self.0.cast::<u8>().add(i).read()} == b'\0' {
				// buffer is okay to read upto len = i
				let slice = unsafe{std::slice::from_raw_parts(self.0.cast::<u8>().as_ptr(), i + 1)};
				// SAFETY: NTStrPtr points to a utf8 encoded buffer
				let slice = unsafe{std::str::from_utf8_unchecked(slice)};
				// SAFETY: Last byte is null
				return unsafe{NTStr::from_str_unchecked(slice)};
			}
			i += 1;
		}
	}

	pub fn to_str(self) -> &'a str {
		self.to_ntstr().as_str_with_null()
	}
	
	/// SAFETY: There must be at least one null byte within the str
	pub unsafe fn from_str_unchecked(value: &'a str) -> Self {
		unsafe{Self::from_ptr(NonNull::from_ref(value).cast::<c_char>())}
	}
	
	/// Expects a single null byte at the end of the string and no null bytes
	/// in the rest of the string
	pub fn from_str(value: &'a str) -> Option<Self> {
		Some(NTStr::from_str(value)?.as_ntstrptr())
	}
}

impl<'a> PartialEq for NTStrPtr<'a> {
	fn eq(&self, other: &Self) -> bool {
		if self.0 == other.0 {
			true
		} else {
			let mut i = 0;
			loop {
				if unsafe{self.0.cast::<u8>().add(i).read()} == b'\0' && unsafe{other.0.cast::<u8>().add(i).read()} == b'\0' {break true}
				if unsafe{self.0.cast::<u8>().add(i).read()} == b'\0' {break false}
				if unsafe{other.0.cast::<u8>().add(i).read()} == b'\0' {break false}
				if unsafe{self.0.cast::<u8>().add(i).read()} != unsafe{other.0.cast::<u8>().add(i).read()} {
					break false;
				}
				i += 1;
			}
		}
	}
}

impl<'a> From<&'a NTStr> for NTStrPtr<'a> {
	fn from (other: &'a NTStr) -> Self {
		other.as_ntstrptr()
	}
}

impl<'a> std::fmt::Debug for NTStrPtr<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.to_str().fmt(f)
	}
}

#[macro_export]
macro_rules! nt {
	($lit: literal) => {
		{
			const {
				let bytes = $lit.as_bytes();
				let mut i = 0;
				while i < bytes.len() {
					assert!(bytes[i] != b'\0');
					i += 1;
				}
			};
			unsafe{$crate::ntstring::NTStr::from_str_unchecked(concat!($lit, "\0"))}
		}
	}
}
