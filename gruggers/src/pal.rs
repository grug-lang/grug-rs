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

	pub type ULONG = u32;
	pub type ULONG_PTR = usize;
	#[derive(Clone, Copy, Eq, PartialEq)]
	#[repr(transparent)]
	pub struct NTSTATUS(u32);
	impl NTSTATUS {
		const TOP_NIBBLE: u32 = 0xF0000000;

		pub const PENDING: Self = Self(0x00000103);
		pub fn is_success(&self) -> bool {
			((self.0 & Self::TOP_NIBBLE) >> 28) < 0x4
		}
	}
	
	pub type LargeInteger = i64;

	pub type ApcIoRoutine = extern "C" fn (*mut c_void, *mut IoStatusBlock, ULONG);
	
	#[repr(C)]
	pub struct IoStatusBlock {
		pub status: IoStatusBlockStatus,
		pub information: ULONG_PTR,
	}
	impl IoStatusBlock{
		pub fn empty() -> Self {
			Self {
				status: IoStatusBlockStatus {status: NTSTATUS(0)},
				information: 0,
			}
		}
	}

	pub union IoStatusBlockStatus {
		pub status: NTSTATUS,
		pub pointer: *mut c_void,
	}
	
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

		pub fn CreateFileA(
			file_name: *const u8,
			desired_access: DWORD,
			share_mode: DWORD,
			security_attributes: Option<&mut ()>,
			creation_disposition: DWORD,
			flags_and_attributes: DWORD,
			template_file: HANDLE,
		) -> HANDLE;

		/// Apc routine should only be provided if the file is opened for asynchronous reading
		pub fn NtReadFile(
			file_handle: HANDLE,
			event: HANDLE,
			apc_routine: Option<ApcIoRoutine>,
			apc_context: *mut c_void,
			io_status_block: *mut IoStatusBlock,
			buffer: *mut u8,
			length: ULONG,
			offset: Option<&LargeInteger>,
			key: Option<&ULONG>
		) -> NTSTATUS;

		pub fn WaitForMultipleObjects(
			count: DWORD,
			handles: *mut HANDLE,
			wait_all: BOOL,
			milliseconds: DWORD,
		) -> DWORD;

		pub fn GetCurrentProcess() -> HANDLE;

		pub fn CloseHandle(
			object: HANDLE
		) -> BOOL;
	}

	pub const INFINITE: DWORD = 0xFFFFFFFF;

	pub const MEM_COMMIT: DWORD = 0x00001000;
	pub const MEM_RESERVE: DWORD = 0x00002000;

	// pub const MEM_DECOMMIT: DWORD = 0x00004000;
	pub const MEM_RELEASE: DWORD = 0x00008000;

	pub const PAGE_READ_WRITE: DWORD = 0x04;
	pub const PAGE_NOACCESS: DWORD = 0x01;

	
	pub struct AccessMask;
	impl AccessMask {
		// https://learn.microsoft.com/en-us/windows/win32/secauthz/access-mask
		// pub const SYNCHRONIZE     : DWORD = 1 << 20;

		// pub const GENERIC_ALL     : DWORD = 1 << 28;
		// pub const GENERIC_EXECUTE : DWORD = 1 << 29;
		// pub const GENERIC_WRITE   : DWORD = 1 << 30;
		pub const GENERIC_READ    : DWORD = 1 << 31;
	}

	pub struct ShareMode;
	impl ShareMode {
		// pub const NO_SHARING       : DWORD = 0x0;
		pub const FILE_SHARE_READ  : DWORD = 0x1;
		// pub const FILE_SHARE_WRITE : DWORD = 0x2;
		// pub const FILE_SHARE_DELETE: DWORD = 0x4;
	}

	pub struct CreateDisposition;
	impl CreateDisposition {
		// pub const CREATE_NEW       : DWORD = 1;
		// pub const CREATE_ALWAYS    : DWORD = 2;
		pub const OPEN_EXISTING    : DWORD = 3;
		// pub const OPEN_ALWAYS      : DWORD = 4;
		// pub const TRUNCATE_EXISTING: DWORD = 5;
	}

	pub struct FlagsAndAttributes;
	impl FlagsAndAttributes {
		pub const FILE_ATTRIBUTE_NORMAL     : DWORD = 0x80;
		pub const FILE_FLAG_BACKUP_SEMANTICS: DWORD = 0x02000000;
		pub const FILE_FLAG_NO_BUFFERING    : DWORD = 0x20000000;
		pub const FILE_FLAG_OVERLAPPED      : DWORD = 0x40000000;
	}

}
