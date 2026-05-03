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
	pub const NONE:               Self = Self([0x0, 0, 0, 0]);
	pub const INIT_ERROR:         Self = Self([0x1, 0, 0, 0]);
	pub const COMPILE_ERROR:      Self = Self([0x2, 0, 0, 0]);
	pub const RUNTIME_ERROR:      Self = Self([0x3, 0, 0, 0]);

	pub const MOD_API_ERROR:      Self = Self::INIT_ERROR.add_component(0x1);

	pub const MOD_API_IO_ERROR:   Self = Self::MOD_API_ERROR.add_component(0x1);
	pub const MOD_API_JSON_ERROR: Self = Self::MOD_API_ERROR.add_component(0x2);

	pub const FILE_NAME_ERROR:    Self = Self::COMPILE_ERROR.add_component(0x1);
	pub const TOKENIZER_ERROR:    Self = Self::COMPILE_ERROR.add_component(0x2);
	pub const PARSER_ERROR:       Self = Self::COMPILE_ERROR.add_component(0x3);
	pub const TYPE_CHECKER_ERROR: Self = Self::COMPILE_ERROR.add_component(0x4);

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
		return true;
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
#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[repr(C)]
pub struct GrugError<A> {
	/// A unique integer identifier for the error that represents the
	/// kind of error that occurred and which specific error
	pub error_kind: ErrorKind,
	/// name of the function the error occurred in. If the error is the member
	/// scope, this string is `member scope`, 
	function_name: NTStrPtr<'static>,
	/// Path to the file with the error
	file_path: NTBytes<'static>,
	/// Source line that contains the error
	source_line: NTStrPtr<'static>,
	/// Location of the error
	span: SourceSpan,
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

impl<A> std::fmt::Debug for GrugError<A> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		f.debug_struct("Error")
			.field("errorkind", &self.error_kind)
			.field("function_name", &self.function_name)
			.field("file_path", &self.file_path().display())
			.field("source_line", &self.source_line)
			.field("line", &self.span.line)
			.field("offset", &self.span.offset)
			.field("error_message", &self.error_message)
			.finish_non_exhaustive()
	}
}

impl<A> GrugError<A> {
	/// The name of the function the error occurred in. returns `member scope`
	/// if the error was not in a function 
	pub fn function_name(&self) -> &str {self.function_name.to_str()}
	/// The path to the file with the error
	pub fn file_path(&self) -> &OsStr {unsafe{OsStr::from_encoded_bytes_unchecked(self.file_path.to_bytes())}}
	/// The source line that contains the error
	pub fn source_line(&self) -> &str {self.source_line.to_str()}
	/// The location that the error occurred at
	pub fn span(&self) -> SourceSpan {self.span}
	/// A single line message that describes the error
	pub fn error_message(&self) -> &str {self.error_message.to_str()}
	/// A string that can be directly printed to the screen
	pub fn error_string(&self) -> &str {self.error_string.to_str()}
}

impl<A: Allocator> GrugError<A> {
	#[track_caller]
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
		if error_kind.matches(&ErrorKind::FILE_NAME_ERROR) {
			write!(err_string, 
				"Error: {error_message}\n\
				  {}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		} else if error_kind.matches(&ErrorKind::TOKENIZER_ERROR) {
			write!(err_string, 
				"  in ({}:{line}:{column})\n\
				Error: {error_message}\n\
				{line} $ {source_line}\0",
				file_path.display()
			).expect("writing into a vec should never fail");
		}

		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let function_name = unsafe{Box::leak(NTStr::box_from_str_in(function_name, &alloc)).as_ntstrptr().detach_lifetime()};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let source_line = unsafe{Box::leak(NTStr::box_from_str_in(source_line, &alloc)).as_ntstrptr().detach_lifetime()};
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
			allocator: Box::new(alloc),
		}
	}
}

impl<A> std::fmt::Display for GrugError<A> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		// TODO: This should be changed to self.error_string later
		// TODO: Each different top level error kind should have a different format
		f.write_str(self.error_message.to_str())
	}
}

