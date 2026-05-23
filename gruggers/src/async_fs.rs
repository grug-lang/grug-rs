#[cfg(target_os="windows")]
mod windows {
	use crate::pal::windows::*;
	use crate::arena::Arena;
	use crate::error::{Error, ErrorKind, SourceSpan};
	use std::fs::File;
	use std::ffi::{OsStr, c_void};
	use std::os::windows::io::{FromRawHandle, AsRawHandle};

	use allocator_api2::vec::Vec;

	/// # SAFETY
	/// The returned file should only be used for async reads
	///
	/// The std File functions will panic if given a file opened for async reads
	pub fn open_file_async_for_read(path: &impl AsRef<OsStr>) -> Result<File, Error> {
		let path = path.as_ref();
		let mut path_nt = Vec::from(path.as_encoded_bytes());
		path_nt.push(b'\0');

		let file = unsafe{CreateFileA(
			path_nt.as_ptr(),
			AccessMask::GENERIC_READ | AccessMask::SYNCHRONIZE,
			ShareMode::FILE_SHARE_READ,
			None,
			CreateDisposition::OPEN_ALWAYS,
			FlagsAndAttributes::FILE_FLAG_OVERLAPPED,
			std::ptr::null_mut(),
		)};

		if file == INVALID_HANDLE_VALUE {
			return Err(Error::from_io_error(std::io::Error::last_os_error(), path));
		}
		Ok(unsafe{File::from_raw_handle(file)})
	}

	/// All files passed to this functions should be opened for async reads without shared write permissions
	// TODO: get the file paths as input so the errors can point to the correct files
	pub fn read_files_async<'a>(files: &[File], arena: &'a Arena) -> Vec<Result<&'a [u8], Error>, &'a Arena> {
		let mut iosbs = Vec::with_capacity_in(files.len(), arena);
		iosbs.extend(files.iter().map(|_| IoStatusBlock::empty()));
		let iosbs = iosbs.leak();

		let mut files_data = Vec::with_capacity_in(files.len(), arena);

		let mut file_handles = Vec::with_capacity_in(files.len(), arena);
		file_handles.extend(files.iter().map(File::as_raw_handle));

		let mut num_apcs_queued: usize = 0;
		// SAFETY: This must explicitly be a usize because `apc` expects it to
		// be a usize. Using type inference defaults to i32 which is incorrect
		let mut num_apcs_finished: usize = 0;

		for (i, file) in files.iter().enumerate() {
			// get file size,
			let size = file.metadata().expect("metadata always succeeds on windows").len();
			// allocate space,
			let cap = if size > 0 {((((size - 1) / 4096) + 1) * 4096) as usize} else {0};
			let buf = arena.alloc(std::alloc::Layout::from_size_align(cap, 4096).unwrap()).unwrap();

			// increment num_apcs_finished on completion
			extern "C" fn apc(data: *mut c_void, _: *mut IoStatusBlock, _: ULONG) {
				// SAFETY: 
				// This apc will only be queued on this current thread
				// We do not exit the outer function until all apcs are signalled.
				// this function is only ever called with `num_apcs_finished` as the context, which is a usize
				//
				// (TODO: Figure out if this is in fact thread safe)
				unsafe{*data.cast::<usize>() += 1};
			}

			// start read
			let nt_status = unsafe{NtReadFile(
				file.as_raw_handle(),
				std::ptr::null_mut(),
				Some(apc),
				(&raw mut num_apcs_finished).cast(),
				iosbs.as_mut_ptr().add(i),
				buf.cast::<u8>().as_ptr(), 
				buf.len() as DWORD,
				Some(&0_i64),
				None
			)};

			// NtReadFile always returns STATUS_PENDING is the file is opened for async reads
			if nt_status != NTSTATUS::PENDING {
				files_data.push(Err(Error::new(
					ErrorKind::IO_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("IO Error (status is not STATUS_PENDING): {:?}", nt_status),
				)));
			} else {
				num_apcs_queued += 1;
				files_data.push(Ok(std::ptr::slice_from_raw_parts_mut(buf.as_ptr().cast::<u8>(), cap)));
			}
		}
		
		// wait for all apcs to complete
		while num_apcs_queued != num_apcs_finished {
			// sleep for a maximum of 500ms to read files
			let result = unsafe{SleepEx(500, TRUE)};
			const WAIT_IO_COMPLETION: DWORD = 0xC0;
			if result == 0 {
				panic!("File read took more than 500ms");
			} else if result != WAIT_IO_COMPLETION {
				// This can't be replaced with an error because we don't know
				// which files it affects (TODO: verify)
				panic!("Woken for invalid reason: {:x?}", result);
			}
		}

		for (iosb, file_data) in iosbs.iter_mut().zip(&mut files_data) {
			let Ok(file_data) = file_data else {continue};
			if unsafe{!iosb.status.status.is_success()} {
				// TODO: replace with error
				panic!("read failed: {:x?}", unsafe{iosb.status.status});
			}
			let new_ptr = std::ptr::slice_from_raw_parts_mut(file_data.cast::<u8>(), iosb.information as usize);
			*file_data = new_ptr;
		}

		let mut ret_val = Vec::with_capacity_in(files.len(), arena);
		ret_val.extend(files_data.into_iter().map(|data: Result<*mut [u8], Error>| {
			data.map(|ptr| unsafe{&*ptr})
		}));
		
		ret_val
	}
}
#[cfg(target_os="windows")]
pub use windows::*;

