// api
// 
// pub        fn page_size() -> usize {
// pub unsafe fn page_alloc(base_addr: Option<NonNull<u8>>, size: usize, disp: Disposition, protection: Protection) -> Result<NonNull<u8>, Error>;
// pub unsafe fn page_protect(ptr: NonNull<u8>, size: usize, protection: Protection) -> Result<Protection, Error>;
// pub unsafe fn page_free(ptr: NonNull<u8>, size: usize) -> Result<(), Error>;

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Protection(usize);
impl Protection {
	pub const NO_ACCESS : Self = Self(0b00000000);
	pub const READ      : Self = Self(0b00000001);
	pub const WRITE     : Self = Self(0b00000010);
	pub const EXEC      : Self = Self(0b00000100);
	pub const GUARD     : Self = Self(0b00001000);
}

impl std::ops::BitOrAssign for Protection {
	fn bitor_assign(&mut self, other: Self) {
		*self = *self | other;
	}
}

impl std::ops::BitOr for Protection {
	type Output = Self;
	fn bitor(self, other: Self) -> Self {
		Self(self.0 | other.0)
	}
}

impl std::ops::BitAnd for Protection {
	type Output = Self;
	fn bitand(self, other: Self) -> Self {
		Self(self.0 & other.0)
	}
}

impl std::ops::BitAndAssign for Protection {
	fn bitand_assign(&mut self, other: Self) {
		*self = *self | other;
	}
}

pub enum Disposition {
	Commit,
	Decommit,
	Reserve,
	Release,
}

pub use inner::*;
#[cfg(target_os = "windows")]
pub mod inner {
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
	use super::*;
	use std::ptr::{NonNull, null_mut};

	use inner::*;
	use page_alloc::*;

	type Error = &'static str;

	fn disp_to_ulong(disp: Disposition) -> ULONG {
		match disp {
			Disposition::Commit => MEM_COMMIT,
			Disposition::Decommit => MEM_DECOMMIT,
			Disposition::Reserve => MEM_RESERVE,
			Disposition::Release => MEM_RELEASE,
		}
	}

	fn ulong_to_prot(prot: ULONG) -> Result<Protection, &'static str> {
		let first_part = prot & (0xFF);

		let mut val = match first_part {
			PAGE_NOACCESS          => Protection::NO_ACCESS,
			PAGE_READONLY          => Protection::READ,
			PAGE_READWRITE         => Protection::READ | Protection::WRITE,
			PAGE_EXECUTE           => Protection::EXEC,
			PAGE_EXECUTE_READ      => Protection::EXEC | Protection::READ,
			PAGE_EXECUTE_READWRITE => Protection::EXEC | Protection::READ | Protection::WRITE,
			_ => return Err("unknown protection constant")
		};

		if prot & PAGE_GUARD == PAGE_GUARD { val |= Protection::GUARD };
		Ok(val)
	}

	fn prot_to_ulong(prot: Protection) -> Result<ULONG, &'static str> {
		let first_part = prot & (Protection::READ | Protection::WRITE | Protection::EXEC);

		let mut val = if first_part == Protection::NO_ACCESS { PAGE_NOACCESS }
		else if first_part == Protection::READ { PAGE_READONLY }
		else if first_part == Protection::EXEC { PAGE_EXECUTE }
		else if first_part == Protection::READ | Protection::EXEC { PAGE_EXECUTE_READ }
		else if first_part == Protection::READ | Protection::EXEC | Protection::WRITE { PAGE_EXECUTE_READWRITE }
		else if first_part == Protection::READ | Protection::WRITE { PAGE_READWRITE }
		else { return Err("invalid protection constant"); };

		if prot & Protection::GUARD == Protection::GUARD { val |= PAGE_GUARD };
		Ok(val)
	}
	
	pub fn page_size() -> usize {
		*PAGE_SIZE as usize
	}
	pub unsafe fn page_alloc(base_addr: Option<NonNull<()>>, size: usize, disp: Disposition, prot: Protection) -> Result<NonNull<[u8]>, Error> {
		let disp = disp_to_ulong(disp);
		let prot = prot_to_ulong(prot)?;
		virtual_alloc(
			base_addr.map(NonNull::as_ptr).unwrap_or_else(null_mut),
			size,
			disp,
			prot,
		).map_err(NTSTATUS::to_str)
	}
	pub unsafe fn page_protect(ptr: NonNull<u8>, size: usize, new_prot: Protection) -> Result<Protection, Error> {
		let new_prot = prot_to_ulong(new_prot)?;
		unsafe {virtual_protect(
			ptr,
			size,
			new_prot,
		)}.map_err(NTSTATUS::to_str).map(|prot| ulong_to_prot(prot)).flatten()
	}
	
	pub unsafe fn page_free(ptr: NonNull<u8>, size: usize, disp: Disposition) -> Result<(), Error> {
		let disp = disp_to_ulong(disp);
		unsafe{virtual_free(ptr, size, disp).map_err(NTSTATUS::to_str)}
	}

	mod inner {
		use std::ffi::{c_int, c_void};

		pub type HANDLE = *mut c_void;
		pub type LPVOID = *mut c_void;
		pub type SIZE_T = usize;
		pub type DWORD = u32;
		pub type WORD = u16;
		pub type DWORD_PTR = *mut DWORD;
		pub type BOOL = c_int;

		#[expect(dead_code)]
		pub const INVALID_HANDLE_VALUE: HANDLE =
			std::ptr::with_exposed_provenance_mut(-1_isize as usize);

		#[expect(dead_code)]
		pub const TRUE: BOOL = 1;
		#[expect(dead_code)]
		pub const FALSE: BOOL = 0;

		pub struct OwnedHandle(pub HANDLE);
		unsafe impl Send for OwnedHandle {}
		unsafe impl Sync for OwnedHandle {}
		impl OwnedHandle {
			/// SAFETY: `handle` must be a valid handle
			#[expect(dead_code)]
			pub unsafe fn new(handle: HANDLE) -> Self {
				Self(handle)
			}
		}
		impl Drop for OwnedHandle {
			fn drop(&mut self) {
				unsafe { CloseHandle(self.0) };
			}
		}

		pub type ULONG = u32;
		pub type LONG = i32;
		pub type ULONG_PTR = usize;
		#[derive(Clone, Copy, Eq, PartialEq, Debug)]
		#[repr(transparent)]
		pub struct NTSTATUS(u32);
		impl NTSTATUS {
			const TOP_NIBBLE: u32 = 0xF0000000;

			#[expect(dead_code)]
			pub const ERR_EOF: Self = Self(0xc0000011);
			#[expect(dead_code)]
			pub const PENDING: Self = Self(0x00000103);
			// pub const SUCCESS: Self = Self(0x00000000);
			pub fn is_success(&self) -> bool {
				((self.0 & Self::TOP_NIBBLE) >> 28) < 0x4
			}

			pub fn severity(self) -> u8 {
				(self.0 >> 30) as u8
			}

			pub fn to_str(self) -> &'static str {
				match self.0 {
					_ if self.severity() == 0 => "unknown success",
					_ if self.severity() == 1 => "unknown information",
					_ if self.severity() == 2 => "unknown warning",
					_ if self.severity() == 3 => "unknown error",
					_ => "unknown"
				}
			}
		}

		#[expect(dead_code)]
		pub type LargeInteger = i64;

		#[expect(dead_code)]
		pub type ApcIoRoutine = extern "C" fn(*mut c_void, *mut IoStatusBlock, ULONG);

		#[repr(C)]
		#[derive(Clone, Copy)]
		pub struct IoStatusBlock {
			pub status: IoStatusBlockStatus,
			pub information: ULONG_PTR,
		}

		impl std::fmt::Debug for IoStatusBlock {
			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.debug_struct("IoStatusBlock")
					.field("status", &unsafe { self.status.status })
					.field("information", &self.information)
					.finish()
			}
		}

		impl IoStatusBlock {
			#[expect(dead_code)]
			pub fn empty() -> Self {
				Self {
					status: IoStatusBlockStatus {
						status: NTSTATUS(0),
					},
					information: 0,
				}
			}
		}

		#[repr(C)]
		#[derive(Clone, Copy)]
		pub union IoStatusBlockStatus {
			pub status: NTSTATUS,
			pub pointer: *mut c_void,
		}

		#[link(name = "kernel32")]
		unsafe extern "system" {
			pub safe fn GetCurrentProcess() -> HANDLE;
			pub fn CloseHandle(object: HANDLE) -> BOOL;
			#[expect(dead_code)]
			pub fn AddVectoredExceptionHandler(
				first: ULONG,
				handler: VectoredExceptionHandler,
			) -> HANDLE;
		}

		pub type VectoredExceptionHandler = extern "C" fn (&mut ExceptionInfo) -> LONG;
		#[repr(C)]
		#[derive(Debug)]
		pub struct ExceptionInfo<'a> {
			pub exception: &'a mut ExceptionRecord<'a>,
			pub ctx: &'a mut (),
		}

		#[repr(C)]
		#[derive(Debug)]
		pub struct ExceptionRecord<'a> {
			pub code: DWORD,
			pub flags: DWORD,
			pub record: &'a mut ExceptionRecord<'a>,
			pub address: *mut (),
			pub num_params: DWORD,
			pub info: [ULONG_PTR; EXCEPTION_MAXIMUM_PARAMETERS],
		}
		pub const EXCEPTION_MAXIMUM_PARAMETERS : usize = 20;
		#[expect(dead_code)]
		pub const EXCEPTION_CONTINUE_SEARCH    : LONG = 0x00000000;
		#[expect(dead_code)]
		pub const EXCEPTION_CONTINUE_EXCECUTION: LONG = -1;

		#[expect(dead_code)]
		pub const INFINITE: DWORD = 0xFFFFFFFF;

		pub const MEM_COMMIT: DWORD = 0x00001000;
		pub const MEM_RESERVE: DWORD = 0x00002000;

		pub const MEM_DECOMMIT: DWORD = 0x00004000;
		pub const MEM_RELEASE: DWORD = 0x00008000;

		pub const PAGE_NOACCESS          : DWORD = 0x01 ;
		pub const PAGE_READONLY          : DWORD = 0x02 ;
		pub const PAGE_READWRITE         : DWORD = 0x04 ;
		pub const PAGE_EXECUTE           : DWORD = 0x10 ;
		pub const PAGE_EXECUTE_READ      : DWORD = 0x20 ;
		pub const PAGE_EXECUTE_READWRITE : DWORD = 0x40 ;
		pub const PAGE_GUARD             : DWORD = 0x100;

		#[expect(dead_code)]
		pub struct AccessMask;
		impl AccessMask {
			// https://learn.microsoft.com/en-us/windows/win32/secauthz/access-mask
			// pub const SYNCHRONIZE: DWORD = 1 << 20;

			// pub const GENERIC_ALL     : DWORD = 1 << 28;
			// pub const GENERIC_EXECUTE : DWORD = 1 << 29;
			// pub const GENERIC_WRITE   : DWORD = 1 << 30;
			// pub const GENERIC_READ: DWORD = 1 << 31;
		}

		#[expect(dead_code)]
		pub struct ShareMode;
		impl ShareMode {
			// pub const NO_SHARING       : DWORD = 0x0;
			// pub const FILE_SHARE_READ: DWORD = 0x1;
			// pub const FILE_SHARE_WRITE : DWORD = 0x2;
			// pub const FILE_SHARE_DELETE: DWORD = 0x4;
		}

		#[expect(dead_code)]
		pub struct CreateDisposition;
		impl CreateDisposition {
			// pub const CREATE_NEW       : DWORD = 1;
			// pub const CREATE_ALWAYS    : DWORD = 2;
			// pub const OPEN_EXISTING: DWORD = 3;
			// pub const OPEN_ALWAYS: DWORD = 4;
			// pub const TRUNCATE_EXISTING: DWORD = 5;
		}

		#[expect(dead_code)]
		pub struct FlagsAndAttributes;
		impl FlagsAndAttributes {
			// pub const FILE_ATTRIBUTE_NORMAL: DWORD = 0x00000080;
			// pub const FILE_FLAG_BACKUP_SEMANTICS: DWORD = 0x02000000;
			// pub const FILE_FLAG_NO_BUFFERING            : DWORD = 0x20000000;
			// pub const FILE_NO_INTERMEDIATE_BUFFERING    : DWORD = 0x00000008;
			// pub const FILE_FLAG_OVERLAPPED: DWORD = 0x40000000;
		}
	}
	mod page_alloc {
		#![allow(non_snake_case)]
		// directly use VirtualAlloc and VirtualFree on windows
		use super::*;
		use std::ptr::NonNull;

		pub static PAGE_SIZE: std::sync::LazyLock<u32> =
			std::sync::LazyLock::new(|| page_size());

		#[link(name = "ntdll", kind="dylib")]
		unsafe extern "system" {
			fn NtAllocateVirtualMemory(
				proc_handle: HANDLE,
				base_addr: &mut *mut (),
				zero_bits: ULONG_PTR,
				region_size: &mut SIZE_T,
				allocation_type: ULONG,
				protect: ULONG,
			) -> NTSTATUS;
			fn NtFreeVirtualMemory(
				proc_handle: HANDLE,
				base_addr: &mut *mut (),
				region_size: &mut SIZE_T,
				free_type: ULONG,
			) -> NTSTATUS;
			fn NtProtectVirtualMemory(
				proc_handle: HANDLE,
				base_addr: &mut *mut (),
				region_size: &mut SIZE_T,
				new_prot: ULONG,
				old_prot: &mut ULONG,
			) -> NTSTATUS;
		}

		pub fn page_size() -> u32 {
			#[repr(C)]
			struct DUMMYSTRUCTNAME {
				ProcessorArchitecture: WORD,
				Reserved: WORD,
			}
			#[repr(C)]
			struct SYSTEM_INFO {
				dummy: DUMMYSTRUCTNAME,
				dwPageSize: DWORD,
				lpMinimumApplicationAddress: LPVOID,
				lpMaximumApplicationAddress: LPVOID,
				dwActiveProcessorMask: DWORD_PTR,
				dwNumberOfProcessors: DWORD,
				dwProcessorType: DWORD,
				dwAllocationGranularity: DWORD,
				wProcessorLevel: WORD,
				wProcessorRevision: WORD,
			}
			#[link(name = "kernel32")]
			unsafe extern "system" {
				fn GetSystemInfo(SystemInfo: *mut SYSTEM_INFO);
			}
			let mut sys_info = std::mem::MaybeUninit::uninit();
			unsafe {
				GetSystemInfo(sys_info.as_mut_ptr());
			}
			unsafe { sys_info.assume_init().dwPageSize }
		}

		pub fn virtual_alloc(mut base_addr: *mut (), mut region_size: usize, alloc_type: ULONG, prot: ULONG) -> Result<NonNull<[u8]>, NTSTATUS> {
			let result = unsafe{NtAllocateVirtualMemory(
				GetCurrentProcess(),
				&mut base_addr,
				0,
				&mut region_size,
				alloc_type,
				prot
			)};
			if result.is_success() {
				unsafe{Ok(NonNull::new_unchecked(std::ptr::slice_from_raw_parts_mut(base_addr.cast(), region_size)))}
			} else {
				Err(result)
			}
		}

		pub unsafe fn virtual_free(base_addr: NonNull<u8>, mut region_size: usize, free_type: ULONG) -> Result<(), NTSTATUS> {
			let mut base_addr = base_addr.as_ptr().cast();
			let result = unsafe{NtFreeVirtualMemory(
				GetCurrentProcess(),
				&mut base_addr,
				&mut region_size,
				free_type,
			)};
			if result.is_success() {
				Ok(())
			} else {
				Err(result)
			}
		}

		pub unsafe fn virtual_protect(base_addr: NonNull<u8>, mut region_size: usize, new_prot: ULONG) -> Result<ULONG, NTSTATUS> {
			let mut base_addr = base_addr.as_ptr().cast();
			let mut old_prot = 0;
			let result = unsafe{NtProtectVirtualMemory(
				GetCurrentProcess(),
				&mut base_addr,
				&mut region_size,
				new_prot,
				&mut old_prot
			)};
			if result.is_success() {
				Ok(old_prot)
			} else {
				Err(result)
			}
		}
	}
}
