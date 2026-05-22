#[cfg(target_os="windows")]
mod windows {
	use crate::pal::windows::*;
	use crate::arena::Arena;
	use std::fs::File;
	use std::ffi::{OsStr, c_void};
	use std::os::windows::io::{FromRawHandle, AsRawHandle};

	use allocator_api2::vec::Vec;

	/// # SAFETY
	/// The returned file should only be used for async reads
	///
	/// The std File functions will panic if given a file opened for async reads
	pub fn open_file_async_for_read(path: &impl AsRef<OsStr>) -> std::io::Result<File> {
		let mut path = Vec::from(path.as_ref().as_encoded_bytes());
		path.push(b'\0');

		let file = unsafe{CreateFileA(
			path.as_ptr(),
			AccessMask::GENERIC_READ | AccessMask::SYNCHRONIZE,
			ShareMode::FILE_SHARE_READ,
			None,
			CreateDisposition::OPEN_ALWAYS,
			FlagsAndAttributes::FILE_FLAG_OVERLAPPED,
			std::ptr::null_mut(),
		)};

		if file == INVALID_HANDLE_VALUE {
			return Err(std::io::Error::last_os_error());
		}
		Ok(unsafe{File::from_raw_handle(file)})
	}

	/// All files passed to this functions should be opened for async reads without shared write permissions
	pub fn read_files_async<'a>(files: &[File], arena: &'a Arena) -> std::io::Result<&'a [&'a [u8]]> {
		let mut iosbs = Vec::new_in(arena);
		iosbs.extend(files.iter().map(|_| IoStatusBlock::empty()));
		let iosbs = iosbs.leak();

		let mut files_data = Vec::new_in(arena);

		let mut file_handles = Vec::new_in(arena);
		file_handles.extend(files.iter().map(File::as_raw_handle));

		for (i, file) in files.iter().enumerate() {
			// get file size,
			let size = file.metadata()?.len();
			// allocate space,
			let (buf, _, cap) = Vec::with_capacity_in(size as usize, arena).into_raw_parts();

			// start read
			let nt_status = unsafe{NtReadFile(
				file.as_raw_handle(),
				std::ptr::null_mut(),
				None,
				std::ptr::null_mut(),
				iosbs.as_mut_ptr().add(i),
				buf, 
				cap as DWORD,
				Some(&0_i64),
				None
			)};
			if nt_status != NTSTATUS::PENDING {
				panic!("status not pending: {:x?}", nt_status);
			}

			files_data.push(std::ptr::slice_from_raw_parts_mut(buf, cap));
		}
		
		// wait for files 64 at a time
		for chunk in file_handles.chunks_mut(64) {
			let result = unsafe{WaitForMultipleObjectsEx(chunk.len() as DWORD, chunk.as_mut_ptr(), TRUE, INFINITE, FALSE)};
			const WAIT_OBJECT_0: DWORD = 0;
			const WAIT_ABANDONED_0 : DWORD = 0x00000080;
			if !(result < WAIT_OBJECT_0 + chunk.len() as DWORD && result >= WAIT_OBJECT_0) {
				panic!("Wait abandoned");
			}
		}

		let files_data = files_data.leak();
		for (iosb, file_data) in iosbs.iter_mut().zip(&mut *files_data) {
			if unsafe{!iosb.status.status.is_success()} {
				panic!("read failed");
			}
			let new_ptr = unsafe{std::slice::from_raw_parts_mut(file_data.cast::<u8>(), iosb.information as usize)};
			*file_data = new_ptr;
		}
		
		Ok(unsafe{std::mem::transmute::<&mut [*mut [u8]], &'a mut [&'a [u8]]>(files_data)})
	}
}
#[cfg(target_os="windows")]
pub use windows::*;

