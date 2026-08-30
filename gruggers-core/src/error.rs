use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;
use crate::ntstring::{NTStrPtr, NTStr, NTBytes, copy_box_nt_bytes_in};
use std::io::Write;
use std::ffi::OsStr;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct SourceSpan {
	pub offset: usize,
	pub line: usize,
}

impl SourceSpan {
	/// Get the line and column number of the start of a source span in the
	/// source code if it is within bounds
	///
	/// # Panics
	/// if the source offset is out of bounds of the text
	// TODO: Make this SIMD Optimized
	pub fn get_col(self, text: &str) -> usize {
		// This is for if the error is in the first line. The loop wont find a
		// '\n' so `column` will not get reinitialized
		let mut column = self.offset + 1;
		let text = text.as_bytes();
		for (i, ch) in text.get(..self.offset).expect("span within source code bounds").iter().rev().enumerate() {
			if *ch == b'\n' {
				column = i + 1;
				break;
			}
		}
		column
	}

	/// If the line embedded within self is incorrect, use this function to get
	/// the actual line number
	/// # Panics
	/// if the source offset is out of bounds of the text
	pub fn get_line_from_offset(offset: usize, text: &str) -> usize {
		let text = text.as_bytes();
		// count the number of b'\n' from 0..self.offset
		text.get(..offset).expect("span within source code bounds").iter().filter(|byte| **byte == b'\n').count() + 1
	}

	/// Get the full source line that contains the start of the source span in
	/// the source code if it is within bounds
	///
	/// # Panics
	/// if the source offset is out of bounds of the text
	pub fn get_source_line(self, text: &str) -> &str {
		let text = text.as_bytes();
		
		let mut line_start = 0;
		for (i, ch) in text.get(..self.offset).expect("span within source code bounds").iter().rev().enumerate() {
			if *ch == b'\n' {
				line_start = self.offset - i;
				break;
			}
		}
		let mut line_end = text.len();
		for (i, ch) in text.get(self.offset..).expect("span within source code bounds").iter().enumerate() {
			if *ch == b'\n' || *ch == b'\r'{
				line_end = self.offset + i;
				break;
			}
		}
		// SAFETY: 
		// 		line_start is either the start of the input or right after a b'\n'
		// 		line_end is either the end of the input or right before a b'\n'
		unsafe{std::str::from_utf8_unchecked(&text[line_start..line_end])}
	}
}

/// Short code that represents an error from grug
///
/// Least significant byte is used to encode the top level error kind
/// (No Error, Initialization Error, Compile error, and runtime error)
/// Remaining bytes can be used to add specific codes for specific errors
///
/// Each byte is considered a separate field. So to add a sub error kind, set
/// the next available byte to a non zero value
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ErrorKind([u8;4]);
const _: () = const {assert!(std::mem::size_of::<ErrorKind>() == 4)};

impl ErrorKind {
	pub const NONE:                        Self = Self([0x0, 0, 0, 0]);
	pub const INIT_ERROR:                  Self = Self([0x1, 0, 0, 0]);
	pub const COMPILE_ERROR:               Self = Self([0x2, 0, 0, 0]);
	pub const RUNTIME_ERROR:               Self = Self([0x3, 0, 0, 0]);

	pub const MOD_API_ERROR:               Self = Self::INIT_ERROR.add_component(0x1);
	pub const FUNCTION_REGISTRATION_ERROR: Self = Self::INIT_ERROR.add_component(0x2);

	pub const MOD_API_IO_ERROR:            Self = Self::MOD_API_ERROR.add_component(0x1);
	pub const MOD_API_JSON_ERROR:          Self = Self::MOD_API_ERROR.add_component(0x2);

	pub const IO_ERROR:                    Self = Self::COMPILE_ERROR.add_component(0x1);
	pub const FILE_NAME_ERROR:             Self = Self::COMPILE_ERROR.add_component(0x2);
	pub const UNEXPECTED_NULL_BYTE:        Self = Self::COMPILE_ERROR.add_component(0x3);
	pub const UTF8_ERROR:                  Self = Self::COMPILE_ERROR.add_component(0x4);
	pub const TOKENIZER_ERROR:             Self = Self::COMPILE_ERROR.add_component(0x5);
	pub const PARSER_ERROR:                Self = Self::COMPILE_ERROR.add_component(0x6);
	pub const TYPE_CHECKER_ERROR:          Self = Self::COMPILE_ERROR.add_component(0x7);

	pub const EMPTY_FILE:                  Self = Self::FILE_NAME_ERROR.add_component(0x1);

	pub const fn add_component(mut self, other: u8) -> Self {
		let mut i = 0;
		while i < self.0.len() {
			if self.0[i] == 0 {
				self.0[i] = other;
				return self;
			}
			i += 1;
		}
		panic!("");
	}

	pub const fn matches(&self, other: &Self) -> bool {
		let mut i = 0;
		while i < self.0.len() {
			if self.0[i] == 0 || other.0[i] == 0{
				return true;
			} else if self.0[i] != other.0[i] {
				return false;
			}
			i += 1;
		}
		true
	}

	pub const fn as_u32(self) -> u32 {
		u32::from_ne_bytes(self.0)
	}
}

impl Eq for ErrorKind { }

/// Contains all data associated with a compile time error in grug
/// 
/// In order to maintain c compatibility, all string fields are represented as
/// null terminated pointers. 
/// 
/// This error API does not allow for an owned GrugError within safe rust. 
///
/// Downstream crates should provide owned versions of the error using an
/// allocator that frees all memory owned by these strings on drop.
#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct GrugError<'a> {
	/// A unique integer identifier for the error that represents the
	/// kind of error that occurred and which specific error
	pub error_kind: ErrorKind,
	/// name of the function the error occurred in. If the error is the member
	/// scope, this string is `member scope`. 
	///
	/// This field may be an empty string if a function name is not meaningful
	/// for the error kind
	pub function_name: NTStrPtr<'a>,
	/// Path to the file with the error
	pub file_path: NTBytes<'a>,
	/// Source line that contains the error
	pub source_line: &'static str,
	/// Location of the error. This span may point to (0, 0) if the error is
	/// not within a file
	pub span: SourceSpan,
	/// Single line error message
	pub error_message: NTStrPtr<'a>,
	/// A string that can be directly printed to the screen. The format of the
	/// error depends on the exact error kind.
	pub error_string: NTStrPtr<'a>,
}

impl<'a> std::fmt::Debug for GrugError<'a> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		f.debug_struct("Error")
			.field("errorkind", &self.error_kind)
			.field("function_name", &self.function_name)
			.field("file_path", &self.file_path_as_os_str().display())
			.field("source_line", &self.source_line)
			.field("line", &self.span.line)
			.field("offset", &self.span.offset)
			.field("error_message", &self.error_message)
			.field("error_string", &self.error_string)
			.finish_non_exhaustive()
	}
}

impl<'a> GrugError<'a> {
	/// The path to the file with the error
	pub fn file_path_as_os_str(&self) -> &OsStr {unsafe{OsStr::from_encoded_bytes_unchecked(self.file_path.to_bytes())}}
}

impl<'a> GrugError<'a> {
	#[track_caller]
	pub fn new_error_in<A: Allocator>(error_kind: ErrorKind, function_name: &str, file_path: &OsStr, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments, alloc: &'a A) -> Self {
		// println!("{:?}", std::panic::Location::caller());
		let line = err_span.line;
		let column = err_span.get_col(source_text);
		let source_line = err_span.get_source_line(source_text).trim_start();

		let mut err_string = Vec::new_in(&alloc);
		if error_kind.matches(&ErrorKind::FILE_NAME_ERROR) {
			// TODO: There should only be a single space between the $ and
			// file name
			write!(err_string, 
				"Error: {error_message}\n\
				$  {}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::UTF8_ERROR) || error_kind.matches(&ErrorKind::UNEXPECTED_NULL_BYTE) {
			write!(err_string, 
				"  in ({}:{line}:{column})\n\
				Error: {error_message}\n\
				{line} $ {source_line}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::IO_ERROR) {
			write!(err_string, 
				"Error: {error_message}\n\
				$ {}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::TOKENIZER_ERROR) {
			write!(err_string, 
				"  in ({}:{line}:{column})\n\
				Error: {error_message}\n\
				{line} $ {source_line}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::MOD_API_ERROR) {
			write!(err_string, 
				"  in mod_api ({})\n\
				Error: {error_message}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::INIT_ERROR) {
			write!(err_string, 
				"  while initializing state
				Error: {error_message}\0"
			).expect("writing into a vec should never fail");
		} else {
			write!(err_string, 
				"  in {function_name} ({}:{line}:{column})\n\
				Error: {error_message}\n\
				{line} $ {source_line}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		}

		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let function_name = unsafe{Box::leak(NTStr::box_from_str_in(function_name, &alloc)).as_ntstrptr().detach_lifetime()};

		// Copy source_line into an allocator and return a reference to the new string
		// Equivalent to Box::leak(Box::from(str)) except the box is allocated in a custom allocator
		// cannot use box_from_str_in because that doesn't allow null bytes
		// within the string
		let source_line = {
			let mut slice = Box::<[u8], _>::new_uninit_slice_in(source_line.len(), &alloc);
			// SAFETY: `slice` was just allocated within `alloc` with length `souce_line.len()`
			unsafe{slice.as_mut_ptr().cast::<u8>().copy_from(source_line.as_ptr(), source_line.len())};
			// SAFETY: Slice is fully initialized in the above line
			let slice = Box::leak(unsafe{slice.assume_init()});

			// - SAFETY: [u8] to str is valid because the slice is guaranteed to
			// be utf8 because it was copied from a str
			//
			// - SAFETY: lifetime transmute is safe because we never give out a
			// `'static` pointer to this string from safe code
			unsafe{std::mem::transmute::<&mut [u8], &'static str>(slice)}
		};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let file_path = unsafe{NTBytes::from_bytes_unchecked(Box::leak(copy_box_nt_bytes_in(file_path.as_encoded_bytes(), &alloc))).detach_lifetime()};

		// let source = unsafe{Box::leak(NTStr::box_from_str_in(source_text, &alloc)).as_ntstrptr().detach_lifetime()};


		// SAFETY: err_string only contains utf8 strings, and is null terminated
		// The string can last as long as the allocator exists, and we never give
		// out a 'static pointer to the string
		let error_string = unsafe{
			NTStr::from_str_unchecked(
				str::from_utf8_unchecked(err_string.leak())
			).as_ntstrptr().detach_lifetime()
		};

		let mut err_message = Vec::new_in(&alloc);
		write!(err_message, "{}\0", error_message)
			.expect("writing into a vec should never fail");

		// SAFETY: err_string only contains utf8 strings, and is null terminated
		// The string can last as long as the allocator exists, and we never give
		// out a 'static pointer to the string
		let error_message = unsafe{
			NTStr::from_str_unchecked(
				str::from_utf8_unchecked(err_message.leak())
			).as_ntstrptr().detach_lifetime()
		};


		Self {
			error_kind,
			function_name,
			file_path,
			source_line,
			span: err_span,
			error_message,
			error_string,
		}
	}

	pub fn copy_into<'b, A: Allocator>(&self, alloc: &'b A) -> GrugError<'b> {
		GrugError {
			error_kind: self.error_kind,
			// SAFETY: We never give out a `'static` pointer to this string from safe code
			function_name: unsafe{Box::leak(NTStr::box_from_str_in(self.function_name.to_str(), alloc)).as_ntstrptr().detach_lifetime()},

			// SAFETY: We never give out a `'static` pointer to this string from safe code
			file_path: unsafe{NTBytes::from_bytes_unchecked(Box::leak(copy_box_nt_bytes_in(self.file_path.to_bytes(), &alloc))).detach_lifetime()},

			// Copy source_line into an allocator and return a reference to the new string
			// Equivalent to Box::leak(Box::from(str)) except the box is allocated in a custom allocator
			// cannot use box_from_str_in because that doesn't allow null bytes
			// within the string
			source_line: {
				let mut slice = Box::<[u8], _>::new_uninit_slice_in(self.source_line.len(), &alloc);
				// SAFETY: `slice` was just allocated within `alloc` with length `souce_line.len()`
				unsafe{slice.as_mut_ptr().cast::<u8>().copy_from(self.source_line.as_ptr(), self.source_line.len())};
				// SAFETY: Slice is fully initialized in the above line
				let slice = Box::leak(unsafe{slice.assume_init()});

				// - SAFETY: [u8] to str is valid because the slice is guaranteed to
				// be utf8 because it was copied from a str
				//
				// - SAFETY: lifetime transmute is safe because we never give out a
				// `'static` pointer to this string from safe code
				unsafe{std::mem::transmute::<&mut [u8], &'static str>(slice)}
			},
			span: self.span,
			// SAFETY: We never give out a `'static` pointer to this string from safe code
			error_message: unsafe{Box::leak(NTStr::box_from_str_in(self.error_message.to_str(), alloc)).as_ntstrptr().detach_lifetime()},
			// SAFETY: We never give out a `'static` pointer to this string from safe code
			error_string: unsafe{Box::leak(NTStr::box_from_str_in(self.error_string.to_str(), alloc)).as_ntstrptr().detach_lifetime()},
		}
	}
}

impl<'a> std::fmt::Display for GrugError<'a> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		f.write_str(self.error_string.to_str())
	}
}

