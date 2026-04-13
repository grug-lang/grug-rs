use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;
use crate::ntstring::{NTStrPtr, NTStr};
use std::io::Write;

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

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[repr(C)]
pub struct grug_error<A> {
	/// name of the function the error occurred in. If the error is the member
	/// scope, this string is `member scope`, 
	function_name: NTStrPtr<'static>,
	/// Path to the file with the error
	file_path: NTStrPtr<'static>,
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

impl<A: Allocator> grug_error<A> {
	pub fn new_error_in(function_name: &str, file_path: &str, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments, alloc: A) -> Self {
		let line = err_span.line;
		let column = err_span.get_col(source_text);
		let source_line = err_span.get_source_line(source_text);
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let function_name = unsafe{NTStr::box_from_str_in(function_name, &alloc).as_ntstrptr().detach_lifetime()};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let source_line = unsafe{NTStr::box_from_str_in(source_line, &alloc).as_ntstrptr().detach_lifetime()};
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let file_path = unsafe{NTStr::box_from_str_in(file_path, &alloc).as_ntstrptr().detach_lifetime()};

		let mut err_string = Vec::new_in(&alloc);
		write!(err_string, 
			   "  in {function_name} ({file_path}:{line}:{column})\
				Error: {error_message}\
				{line} $ {source_line}\0"
		).expect("writing into a vec should never fail");

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
		f.write_str(self.error_string.to_str())
	}
}

