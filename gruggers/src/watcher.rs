pub use watcher::*;
pub use std::ffi::{OsStr, OsString};

#[allow(warnings)]
pub fn poll_watcher(mods_dir: impl AsRef<OsStr>, mut f: impl FnMut(Result<OsString, std::io::Error>) + Send + 'static) -> Result<(), std::io::Error>{
	use std::collections::HashMap;
	let mods_dir = OsString::from(mods_dir.as_ref());
	let mods_dir_len = if mods_dir.as_encoded_bytes().last().is_some_and(|x| *x != b'\\' && *x != b'/') {mods_dir.len() + 1} else {mods_dir.len()};

	let mut files = HashMap::new();
	
	let mut dirs_to_check = vec![mods_dir.clone()];
	
	while let Some(dir) = dirs_to_check.pop() {
		for entry in std::fs::read_dir(dir)? {
			let entry = entry?;
			let path = entry.path();
			let metadata = entry.metadata()?;
			if metadata.file_type().is_dir() {dirs_to_check.push(path.clone().into_os_string());}
			files.insert(path, metadata.modified()?);
		}
	}
	std::thread::spawn(move || {
		fn is_newer_than(first: std::time::SystemTime, second: std::time::SystemTime) -> bool {
			match first.duration_since(second) {
				Ok(diff) if diff != std::time::Duration::ZERO => true,
				_ => false
			}
		}
		loop {
			// Replacement for try blocks
			match (|| {
				let mut dirs_to_check = vec![mods_dir.clone()];
				while let Some(dir) = dirs_to_check.pop() {
					for entry in std::fs::read_dir(dir)? {
						let entry = entry?;
						let entry_path = entry.path();
						let metadata = entry.metadata()?;
						let m_time = metadata.modified()?;
						if metadata.file_type().is_dir() {dirs_to_check.push(entry_path.clone().into_os_string());}
						
						match files.get_mut(&entry_path) {
							Some(old_m_time) if is_newer_than(m_time, *old_m_time) => {
								let rel_path = &entry_path.as_os_str().as_encoded_bytes()[mods_dir_len..];
								let rel_path = unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)};
								f(Ok(OsString::from(rel_path)));
								*old_m_time = m_time;
							}
							None => {
								files.insert(entry_path, m_time);
							}
							_ => (),
						}
					}
				}
				
				Ok(())
			})() {
				Ok(()) => (),
				Err(err) => f(Err(err)),
			}
			
			std::thread::sleep(std::time::Duration::from_secs(1));
		}
	});
	Ok(())
}

#[cfg(target_os="windows")]
mod watcher {
	#![allow(unused)]
	#![allow(non_camel_case_types)]
	use std::ffi::{c_void, c_int, OsStr, OsString};
	use std::mem::MaybeUninit;
	use std::os::windows::ffi::OsStringExt;
	use crate::ntstring::{NTStr, NTStrPtr};
	use allocator_api2::alloc::Global;

	struct OwnedHandle(HANDLE);
	unsafe impl Send for OwnedHandle {}
	unsafe impl Sync for OwnedHandle {}
	impl OwnedHandle {
		/// SAFETY: `handle` must be a valid handle
		unsafe fn new(handle: HANDLE) -> Self {
			Self(handle)
		}
	}
	impl Drop for OwnedHandle {
		fn drop(&mut self) {
			unsafe {CloseHandle(self.0)};
		}
	}

	type HANDLE = *mut c_void;
	const INVALID_HANDLE_VALUE: HANDLE = std::ptr::with_exposed_provenance_mut(-1_isize as usize);

	const TRUE : BOOL = 1;
	const FALSE: BOOL = 0;

	type DWORD = u32;
	type ULONG_PTR = usize;
	type BOOL = c_int;
	type OverlappedCompletionRoutine = extern "C" fn(DWORD, DWORD, &mut Overlapped);

	struct AccessMask;
	impl AccessMask {
		// https://learn.microsoft.com/en-us/windows/win32/secauthz/access-mask
		const SYNCHRONIZE     : DWORD = 1 << 20;

		const GENERIC_ALL     : DWORD = 1 << 28;
		const GENERIC_EXECUTE : DWORD = 1 << 29;
		const GENERIC_WRITE   : DWORD = 1 << 30;
		const GENERIC_READ    : DWORD = 1 << 31;
	}

	struct ShareMode;
	impl ShareMode {
		const NO_SHARING       : DWORD = 0x0;
		const FILE_SHARE_READ  : DWORD = 0x1;
		const FILE_SHARE_WRITE : DWORD = 0x2;
		const FILE_SHARE_DELETE: DWORD = 0x4;
	}

	struct CreateDisposition;
	impl CreateDisposition {
		const CREATE_NEW       : DWORD = 1;
		const CREATE_ALWAYS    : DWORD = 2;
		const OPEN_EXISTING    : DWORD = 3;
		const OPEN_ALWAYS      : DWORD = 4;
		const TRUNCATE_EXISTING: DWORD = 5;
	}

	struct FlagsAndAttributes;
	impl FlagsAndAttributes {
		const FILE_ATTRIBUTE_NORMAL     : DWORD = 0x80;
		const FILE_FLAG_BACKUP_SEMANTICS: DWORD = 0x02000000;
		const FILE_FLAG_OVERLAPPED      : DWORD = 0x40000000;
	}

	struct NotifyFilter;
	impl NotifyFilter {
		const CHANGE_FILE_NAME  : DWORD = 0x00000001;
		const CHANGE_DIR_NAME   : DWORD = 0x00000002;
		const CHANGE_ATTRIBUTES : DWORD = 0x00000004;
		const CHANGE_SIZE       : DWORD = 0x00000008;
		const CHANGE_LAST_WRITE : DWORD = 0x00000010;
		const CHANGE_LAST_ACCESS: DWORD = 0x00000020;
		const CHANGE_CREATION   : DWORD = 0x00000040;
		const CHANGE_SECURITY   : DWORD = 0x00000100;
	}

	#[repr(C)]
	struct Overlapped {
		internal: ULONG_PTR,
		internal_high: ULONG_PTR,
		offset: [DWORD;2],
		event: HANDLE,
	}

	#[repr(u32)]
	#[derive(Debug)]
	pub enum FileAction {
		Added = 0x1,
		Removed = 0x2,
		Modified = 0x3,
		RenamedOldName = 0x4,
		RenamedNewName = 0x5,
	}

	#[repr(C)]
	pub struct FileNotifyInformation {
		next_entry_offset: DWORD,
		pub action: FileAction,
		file_name_len: DWORD,
		file_name: [u16],
	}

	impl std::fmt::Debug for FileNotifyInformation {
		fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
			f.debug_struct("FileNotifyInformation")
				.field("action", &self.action)
				.field("file_name", &self.file_name())
				.finish()
		}
	}

	impl FileNotifyInformation {
		pub fn file_name(&self) -> OsString {
			OsString::from_wide(&self.file_name)
		}
	}

	pub struct DirChanges {
		buffer: Box<[MaybeUninit<DWORD>]>,
	}
	unsafe impl Send for DirChanges {}
	unsafe impl Sync for DirChanges {}

	impl DirChanges {
		/// SAFETY: buffer must be initialized with a call to ReadDirectoryChangesW
		unsafe fn new(buffer: Box<[MaybeUninit<u32>]>) -> Self {
			Self {buffer}
		}

		fn iter(&self) -> DirChangesIter<'_> {
			self.into_iter()
		}
	}

	impl<'a> IntoIterator for &'a DirChanges {
		type IntoIter = DirChangesIter<'a>;
		type Item = &'a FileNotifyInformation;
		fn into_iter(self) -> Self::IntoIter {
			DirChangesIter {
				_buffer: &self.buffer,
				current: self.buffer.as_ptr().cast::<u32>(),
			}
		}
	}

	pub struct DirChangesIter<'a> {
		_buffer: &'a [MaybeUninit<DWORD>],
		current: *const DWORD,
	}

	impl<'a> Iterator for DirChangesIter<'a> {
		type Item = &'a FileNotifyInformation;
		fn next(&mut self) -> Option<Self::Item> {
			use std::ptr;
			use std::mem::transmute;
			if self.current.is_null() {return None;}

			// SAFETY: 
			// 	alignment: FileNotifyInformation is 4 byte aligned and
			// 	ReadDirectoryChangesW guarantees FileNotifyInformation is aligned
			// 	to 4 bytes
			// 	len: len of 0 is always valid for slices
			let current = unsafe{transmute::<*const [DWORD], &FileNotifyInformation>(ptr::slice_from_raw_parts(self.current, 0))};
			let next_offset = current.next_entry_offset as usize;
			// SAFETY: 
			// 	alignment: FileNotifyInformation is 4 byte aligned and
			// 	ReadDirectoryChangesW guarantees FileNotifyInformation is aligned
			// 	to 4 bytes
			// 	len: len is guaranteed by ReadDirectoryChangesW
			let current = unsafe{transmute::<*const [DWORD], &FileNotifyInformation>(ptr::slice_from_raw_parts(self.current, current.file_name_len as usize / 2))};

			self.current = if next_offset == 0 {std::ptr::null()} else {unsafe{self.current.byte_add(next_offset)}};
			Some(current)
		}
	}

	#[link(name = "kernel32", kind="dylib")]
	unsafe extern "C" {
		fn CreateFileA(
			file_name: *const u8,
			desired_access: DWORD,
			share_mode: DWORD,
			security_attributes: Option<&mut ()>,
			creation_disposition: DWORD,
			flags_and_attributes: DWORD,
			template_file: HANDLE,
		) -> HANDLE;
		fn ReadDirectoryChangesW(
			directory: HANDLE,
			buffer: *mut MaybeUninit<u32>,
			buffer_len: DWORD,
			watch_subtree: BOOL,
			notify_filter: DWORD,
			bytes_returned: Option<&mut DWORD>,
			overlapped: Option<&mut Overlapped>,
			completion_routine: Option<OverlappedCompletionRoutine>
		) -> BOOL;
		fn CloseHandle(
			object: HANDLE
		) -> BOOL;
	}

	/// SAFETY: path should be null terminated
	unsafe fn open_dir (path: &[u8]) -> Result<OwnedHandle, std::io::Error> {
		let handle = unsafe {
			CreateFileA(
				path.as_ptr(),
				AccessMask::GENERIC_READ,
				ShareMode::FILE_SHARE_READ,
				None,
				CreateDisposition::OPEN_EXISTING,
				FlagsAndAttributes::FILE_ATTRIBUTE_NORMAL |
				FlagsAndAttributes::FILE_FLAG_OVERLAPPED |
				FlagsAndAttributes::FILE_FLAG_BACKUP_SEMANTICS,
				std::ptr::null_mut(),
			)
		};
		if handle == INVALID_HANDLE_VALUE {
			return Err(std::io::Error::last_os_error());
		}
		// SAFETY: CreateFileA with FILE_FLAG_BACKUP_SEMANTICS flag creates a directory handle
		Ok(unsafe{OwnedHandle::new(handle)})
	}

	fn read_changes(handle: &OwnedHandle) -> Result<DirChanges, std::io::Error> {
		use std::mem::size_of;
		let mut buffer = Vec::with_capacity(1024 * 16);
		loop {
			let mut bytes_returned = 0;
			let ret_val = unsafe{ReadDirectoryChangesW (
				handle.0,
				buffer.as_mut_ptr(),
				(buffer.capacity() * size_of::<DWORD>()) as u32,
				TRUE,
				NotifyFilter::CHANGE_FILE_NAME | NotifyFilter::CHANGE_DIR_NAME | NotifyFilter::CHANGE_LAST_WRITE,
				Some(&mut bytes_returned),
				None,
				None,
			)};
			if ret_val == 0 {
				return Err(std::io::Error::last_os_error());
			}
			if bytes_returned != 0 {
				unsafe{buffer.set_len((bytes_returned as usize - 1) / size_of::<DWORD>() + 1)};
				let buffer: Box<[MaybeUninit<DWORD>]> = buffer.into();
				return Ok(unsafe{DirChanges::new(buffer)});
			}
			buffer.reserve(buffer.len())
		}
	}

	pub fn watch_changes(path: impl AsRef<OsStr>, mut f: impl FnMut(Result<OsString, std::io::Error>) + Send + 'static) -> Result<(), std::io::Error>{
		let mut path = Vec::from(path.as_ref().as_encoded_bytes());
		path.push(b'\0');
		let handle = unsafe{open_dir(&path)?};
		std::thread::spawn(move || {
			loop {
				match read_changes(&handle) {
					Ok(changes) => {
						changes.iter().for_each(|change| f(Ok(change.file_name())));
					},
					Err(err) => f(Err(err)),
				}
			}
		});
		Ok(())
	}
}

