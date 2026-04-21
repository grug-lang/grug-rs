use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;
use crate::ntstring::{NTStrPtr, NTStr, NTBytes, copy_box_nt_bytes_in};
use std::io::Write;
use std::ffi::OsStr;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
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
		let mut last_new_line = 0;
		let text = text.as_bytes();
		for (i, ch) in text.get(..self.offset).expect("span within source code bounds").iter().enumerate() {
			if *ch == b'\n' {
				last_new_line = i;
			}
		}
		self.offset - last_new_line
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
		let line_end = text.len();
		for (i, ch) in text.get(self.offset..).expect("span within source code bounds").iter().enumerate() {
			if *ch == b'\n' {
				line_start = self.offset + i;
				break;
			}
		}
		// SAFETY: 
		// 		line_start is either the start of the input or right after a b'\n'
		// 		line_end is either the end of the input or right before a b'\n'
		return unsafe{std::str::from_utf8_unchecked(&text[line_start..line_end])};
	}
}

/// Short code that represents an error from grug
///
/// Remaining bits can be used to add specific codes for specific errors
#[repr(align(4))]
#[derive(Clone, Copy, Debug)]
pub struct ErrorKind(pub [u8;4]);
const _: () = const {assert!(std::mem::size_of::<ErrorKind>() == 4)};

impl From<u8> for ErrorKind {
	fn from(other: u8) -> Self {
		Self([other, 0, 0, 0])
	}
}

impl ErrorKind {
	pub const NONE: Self = Self([0x0, 0, 0, 0]);
	pub const INIT_ERROR: Self = Self([0x1, 0, 0, 0]);
	pub const COMPILE_ERROR: Self = Self([0x2, 0, 0, 0]);
	pub const RUNTIME_ERROR: Self = Self([0x3, 0, 0, 0]);

	pub fn add_component(mut self, other: u8) -> Self {
		let mut i = 0;
		while i < self.0.len() {
			if self.0[i] == 0 {
				self.0[i] = other;
				return self;
			}
			i += 1;
		}
		panic!("components: {:?}", self.0);
	}
}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[repr(C)]
pub struct grug_error<A> {
	/// Represents the kind of error that occurred and which specific error
	error_kind: ErrorKind,
	/// name of the function the error occurred in. If the error is the member
	/// scope, this string is `member scope`, 
	function_name: NTStrPtr<'static>,
	/// Path to the file with the error
	file_path: NTBytes<'static>,
	/// Source line that contains the error
	source_line: NTStrPtr<'static>,
	/// line number of the error
	line: usize,
	/// column number of the error
	column: usize,
	/// Single line error message
	error_message: NTStrPtr<'static>,
	/// A string that can be directly printed to the screen
	error_string: NTStrPtr<'static>,
	/// All the other fields of the error are allocated in this allocator To
	/// prevent leaks, this allocator needs to be able to free all memory it
	/// owns on drop
	///
	/// This is best done with an arena allocator
	/// 
	/// Keep in mind though, that the allocator is itself allocated in the global allocator
	allocator: Box<A>,
}

impl<A> std::fmt::Debug for grug_error<A> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		f.debug_struct("Error")
			.field("errorkind", &self.error_kind)
			.field("function_name", &self.function_name)
			.field("file_path", &self.file_path)
			.field("source_line", &self.source_line)
			.field("line", &self.line)
			.field("column", &self.column)
			.field("error_message", &self.error_message)
			.finish_non_exhaustive()
	}
}

impl<A: Allocator> grug_error<A> {
	pub fn new_error(error_kind: ErrorKind, function_name: &str, file_path: &OsStr, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments) -> Self where
		A: Default,
	{
		let alloc = A::default();
		Self::new_error_in(error_kind, function_name, file_path, source_text, err_span, error_message, alloc)
	}

	pub fn new_error_in(error_kind: ErrorKind, function_name: &str, file_path: &OsStr, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments, alloc: A) -> Self {
		let line = err_span.line;
		let column = err_span.get_col(source_text);
		let source_line = err_span.get_source_line(source_text);

		let mut err_string = Vec::new_in(&alloc);
		write!(err_string, 
			"  in {function_name} ({}:{line}:{column})\n\
			Error: {error_message}\n\
			{line} $ {source_line}\0",
			file_path.display()
		).expect("writing into a vec should never fail");

		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let function_name = unsafe{Box::leak(NTStr::box_from_str_in(function_name, &alloc)).as_ntstrptr().detach_lifetime()};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let source_line = unsafe{Box::leak(NTStr::box_from_str_in(source_line, &alloc)).as_ntstrptr().detach_lifetime()};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let file_path = unsafe{NTBytes::from_bytes_unchecked(Box::leak(copy_box_nt_bytes_in(file_path.as_encoded_bytes(), &alloc))).detach_lifetime()};


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
			line,
			column,
			error_message,
			error_string,
			allocator: Box::new(alloc),
		}
	}
}

impl<A> std::fmt::Display for grug_error<A> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		// TODO: This should be changed to self.error_string later
		f.write_str(self.error_message.to_str())
	}
}

