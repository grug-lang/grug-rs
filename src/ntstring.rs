use std::sync::Arc;
use std::ops::{DerefMut, Deref};
use std::ffi::CStr;

#[repr(transparent)]
pub struct NTStr(str);

impl NTStr {
	pub fn arc_from_str (value: &str) -> Arc<Self> {
		let mut arc = Arc::into_raw(Arc::<[u8]>::new_uninit_slice(value.len() + 1));

		unsafe{
			std::ptr::copy(value.as_ptr(), arc.cast_mut().cast(), value.len());
			arc.cast_mut().cast::<u8>().add(value.len()).write(b'\0');
			std::mem::transmute(Arc::from_raw(arc).assume_init())
		}
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
		if *value.as_bytes().last().ok_or(())? == b'\0' {
			Ok(unsafe {std::mem::transmute(value)})
		}
		else {
			Err(())
		}
	}
}

impl std::ops::Deref for NTStr {
	type Target = str;
	fn deref(&self) -> &Self::Target {
		&self.0[..(self.0.len() - 1)]
	}
}

impl std::ops::DerefMut for NTStr {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl AsRef<str> for NTStr {
	fn as_ref(&self) -> &str {
		&self.0[..(self.0.len() - 1)]
	}
}

impl AsRef<CStr> for NTStr {
	fn as_ref(&self) -> &CStr {
		unsafe{CStr::from_bytes_with_nul_unchecked(self.0.as_bytes())}
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
