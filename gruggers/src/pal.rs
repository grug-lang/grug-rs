#[cfg(target_os="windows")]
pub mod windows {
	#![allow(non_camel_case_types)]
	#![allow(non_snake_case)]

	use std::ffi::{c_void, c_int};

	pub type HANDLE = *mut c_void;
	pub type LPVOID = *mut c_void;
	pub type SIZE_T = usize;
	pub type DWORD = u32;
	pub type WORD = u16;
	pub type DWORD_PTR = *mut DWORD;
	pub type BOOL = c_int;

	pub const INVALID_HANDLE_VALUE: HANDLE = std::ptr::with_exposed_provenance_mut(-1_isize as usize);

	pub const TRUE : BOOL = 1;
	// pub const FALSE: BOOL = 0;

	pub struct OwnedHandle(pub HANDLE);
	unsafe impl Send for OwnedHandle {}
	unsafe impl Sync for OwnedHandle {}
	impl OwnedHandle {
		/// SAFETY: `handle` must be a valid handle
		pub unsafe fn new(handle: HANDLE) -> Self {
			Self(handle)
		}
	}
	impl Drop for OwnedHandle {
		fn drop(&mut self) {
			unsafe {CloseHandle(self.0)};
		}
	}

	pub type ULONG_PTR = usize;


	#[link(name = "kernel32")]
	unsafe extern "system" {
		pub fn VirtualAllocEx (
			hProcess: HANDLE,
			lpAddress: LPVOID,
			dwSize: SIZE_T,
			flAllocationType: DWORD,
			flProtect : DWORD,
		) -> *mut c_void;

		pub fn VirtualProtectEx (
			process: HANDLE,
			Address: LPVOID,
			Size: DWORD,
			newProtect: DWORD,
			oldProtect: &mut DWORD,
		) -> BOOL;

		pub fn VirtualFreeEx (
			process: HANDLE,
			Address: LPVOID,
			Size: DWORD,
			FreeType: DWORD,
		) -> BOOL;

		pub fn GetCurrentProcess() -> HANDLE;

		pub fn CloseHandle(
			object: HANDLE
		) -> BOOL;
	}

	pub const MEM_COMMIT: DWORD = 0x00001000;
	pub const MEM_RESERVE: DWORD = 0x00002000;

	// pub const MEM_DECOMMIT: DWORD = 0x00004000;
	pub const MEM_RELEASE: DWORD = 0x00008000;

	pub const PAGE_READ_WRITE: DWORD = 0x04;
	pub const PAGE_NOACCESS: DWORD = 0x01;
}
