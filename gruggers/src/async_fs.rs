use std::fs::File;
use std::ptr::NonNull;
use std::ffi::OsStr;

use crate::ntstring::{NTStr, NTStrError};
use crate::error::{Error, ErrorKind, SourceSpan};
use crate::arena::Arena;

/// Allocates a buffer for the file aligned to 4096 bytes with space for a
/// null terminator
fn allocate_buffer_for_file<'a>(file: &File, arena: &'a Arena) -> NonNull<[u8]> {
	// get file size, add 1 byte for null byte
	let size = file.metadata().expect("metadata always succeeds on windows").len() + 1;
	// allocate space rounded up to the nearest multiple of 4096 bytes
	let cap = if size > 0 {((((size - 1) / 4096) + 1) * 4096) as usize} else {0};
	arena.alloc(std::alloc::Layout::from_size_align(cap, 4096).unwrap()).unwrap()
}

pub fn verify_file_data<'a>(data: &'a [u8], file_path: &OsStr) -> Result<&'a NTStr, Error> {
	let str = std::str::from_utf8(data).map_err(|err| {
		// SAFETY: err.valid_up_to returns the length of the
		// portion of the string that is valid utf8
		let str = unsafe{std::str::from_utf8_unchecked(&data[..err.valid_up_to()])};
		Error::new(
			ErrorKind::UTF8_ERROR,
			"",
			file_path, 
			str,
			// Find the actual line number 
			SourceSpan{offset: err.valid_up_to(), line: SourceSpan::get_line_from_offset(err.valid_up_to(), str)},
			format_args!("File is not valid utf8: {}", err),
		)
	})?;
	let nt_str = NTStr::try_from_str(str).map_err(|err| {
		match err {
			NTStrError::UnexpectedNullByte{location} => {
				let line = SourceSpan::get_line_from_offset(location, str);
				Error::new(
					ErrorKind::UNEXPECTED_NULL_BYTE,
					"",
					file_path, 
					str,
					// Find the actual line number 
					SourceSpan{offset: location, line},
					format_args!("Unexpected null byte on line {}", line),
				)
			}
			err => panic!("{:?}", err),
		}
	})?;
	Ok(nt_str)
}
#[cfg(target_os="windows")]
mod windows {
	use crate::pal::windows::*;
	use crate::arena::Arena;
	use crate::error::{Error, ErrorKind, SourceSpan};
	use crate::ntstring::NTStr;

	use std::ptr::NonNull;
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
	/// The returned slice of bytes must be null terminated
	// TODO: get the file paths as input so the errors can point to the correct files
	pub fn read_files_async<'a, 'b>(files: impl IntoIterator<Item=(&'b File, &'b OsStr)>, arena: &'a Arena) -> Vec<Result<&'a NTStr, Error>, &'a Arena> {
		let files = {
			let mut temp = Vec::new_in(arena);
			temp.extend(files);
			temp
		};
		let mut iosbs = Vec::with_capacity_in(files.len(), arena);
		iosbs.extend(files.iter().map(|_| IoStatusBlock::empty()));
		let iosbs = iosbs.leak();

		let mut files_data = Vec::with_capacity_in(files.len(), arena);

		let mut file_handles = Vec::with_capacity_in(files.len(), arena);
		file_handles.extend(files.iter().map(|(file, _)| *file).map(File::as_raw_handle));

		let mut num_apcs_queued: usize = 0;
		// SAFETY: This must explicitly be a usize because `apc` expects it to
		// be a usize. Using type inference defaults to i32 which is incorrect
		let mut num_apcs_finished: usize = 0;

		for (i, (file, file_path)) in files.iter().enumerate() {
			let buf = super::allocate_buffer_for_file(file, arena);

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
					file_path,
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("IO Error (status is not STATUS_PENDING): {:?}", nt_status),
				)));
			} else {
				num_apcs_queued += 1;
				files_data.push(Ok((buf, file_path)));
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
			let Ok((file_text, file_path)) = file_data else {continue};
			// Empty files will throw an EOF error, but that is expected
			if unsafe{!iosb.status.status.is_success() && iosb.status.status != NTSTATUS::ERR_EOF} {
				*file_data = Err(Error::new(
					ErrorKind::IO_ERROR,
					"",
					// We don't know which file this error is for
					file_path,
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("IO Error (status is not Success): {:x?}", unsafe{iosb.status.status}),
				));
				continue;
			}
			// Add a null byte to the end of the file;
			unsafe{*file_text.cast::<u8>().add(iosb.information as usize).as_ptr() = b'\0'};
			let new_ptr = unsafe{NonNull::new_unchecked(std::ptr::slice_from_raw_parts_mut(file_text.cast::<u8>().as_ptr(), iosb.information as usize + 1))};
			*file_text = new_ptr;
		}

		let mut ret_val = Vec::with_capacity_in(files.len(), arena);
		ret_val.extend(files_data.into_iter().map(|data| {
			data.map(|(ptr, path)| {
				let slice = unsafe{&*ptr.as_ptr()};
				super::verify_file_data(slice, path)
			}).flatten()
		}));
		
		ret_val
	}
}
#[cfg(target_os="windows")]
pub use windows::*;

/// File reads are not actually async in the fallback path
#[cfg(not(target_os = "windows"))]
mod fallback {
	use crate::arena::Arena;
	use crate::error::Error;
	use std::fs::File;
	use std::ffi::OsStr;
	use std::io::Read;

	use allocator_api2::vec::Vec;

	pub fn open_file_async_for_read(path: &impl AsRef<OsStr>) -> Result<File, Error> {
		let path = path.as_ref();
		File::open(path).map_err(|err| Error::from_io_error(err, path))
	}

	pub fn read_files_async<'a, 'b>(files: impl IntoIterator<Item=&'b File>, arena: &'a Arena) -> Vec<Result<&'a NTStr, Error>, &'a Arena> {
		let mut files = {
			let mut temp = Vec::new_in(arena);
			temp.extend(files);
			temp
		};
		let mut files_data = Vec::with_capacity_in(files.len(), arena);
		for file in files.iter_mut() {
			let size = file.metadata().expect("metadata always succeeds on windows").len() + 1;
			let buf = super::allocate_buffer_for_file(file, arena);
			// Should actually be unnecessary but the Read interface requires
			// that the input buffer is fully initialized
			unsafe{buf.cast::<u8>().write_bytes(0, buf.len())};
			// SAFETY: buffer is fully initialized
			let buf = unsafe{std::slice::from_raw_parts_mut(buf.cast::<u8>().as_ptr(), cap)};

			match file.read(buf) {
				Ok(size) => files_data.push(super::verify_file_data(&buf[..size + 1])),
				Err(err) => files_data.push(Err(Error::from_io_error(err, "file_unknown (ig)".as_ref())))
			}
		}
		
		files_data
	}
}
#[cfg(not(target_os = "windows"))]
pub use fallback::*;
