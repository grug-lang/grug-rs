//! Defines types necessary for handling runtime errors

use std::ffi::OsStr;
use std::io::Write;
use crate::error::SourceSpan;
use crate::ntstring::{NTStrPtr, NTBytes, NTStr};
use crate::utils::{copy_str, copy_str_as_ntstr, copy_bytes_as_nt};
use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;

/// This is the maximum time allowed to execute an on function.
/// Backends are allowed to take longer than this time to throw an error.
pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// pub const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

/// This is the maximum allowed depth of function calls when executing an on function.
/// Backends are allowed to go further than this limit because of optimizations.
pub const MAX_RECURSION_LIMIT: usize = 100;

/// Indicates the kind of runtime error that has occurred
#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorKind {
	/// The grug code recursed too many times
	StackOverflow,
	/// The grug code took too long to execute
	TimeLimitExceeded,
	/// A host function triggered an arbitrary error
	HostFnError,
}

/// Contains all data associated with a runtime error in grug
/// 
/// In order to maintain c compatibility, all string fields are represented as
/// null terminated pointers. 
/// 
/// This error API does not allow for an owned [`RuntimeError`] within safe rust. 
///
/// Downstream crates could provide owned versions of the error using an
/// allocator that frees all memory owned by these strings on drop. However,
/// they should seriously consider if this is necessary. 
///
/// The `call_stack` field stores a pointer to the entire file text where the
/// frame occurred to make error reporting easier. By default, they are just
/// pointers into already allocated strings, so they do not cause any memory
/// allocations. A naive copy could create a new allocation for each of these
/// strings, which could lead to arbitrarily high memory usage.
#[repr(C)]
pub struct RuntimeError<'a> {
	/// The kind of runtime error 
	pub kind: RuntimeErrorKind,
	/// The state of the callstack when the error occurred
	/// This is a best effort guess at the state which may or may not be
	/// deformed due to optimizations
	pub call_stack: &'a [StackFrame<'a>],
	/// The last export function that was called before the error occurred
	pub export_fn_name: NTStrPtr<'a>,
	/// The script path of the last export function that was called.
	/// Must be valid to convert to an OsStr.
	pub script_path: NTBytes<'a>,
	/// The location the error occurred at
	pub err_span: SourceSpan,
	/// The source line where the error occurred
	pub source_line: &'a str,
	/// A message string that explains the error
	pub error_message: NTStrPtr<'a>,
	/// A string that can be directly printed to the screen. The format of the
	/// error depends on the exact error kind.
	pub error_string: NTStrPtr<'a>,
}

impl<'a> RuntimeError<'a> {
	/// The call_stack must have already been allocated within `a`
	#[track_caller]
	pub fn new_error_in<A: Allocator>(
		kind: RuntimeErrorKind, 
		call_stack: &'a [StackFrame<'a>], 
		export_fn_name: &str,
		script_path: &OsStr,
		err_span: SourceSpan, 
		source_text: &str, 
		error_message: &str, 
		a: &'a A
	) -> Self {
		let source_line = copy_str(err_span.get_source_line(source_text), a).trim();

		let error_message = {
			let mut err_message_vec = Vec::new_in(a);
			write!(err_message_vec, "{}\0", error_message).expect("Writing to a vec can never fail");
			// SAFETY: The string is from a formatter which only outputs valid
			// utf8
			NTStr::try_from_str(unsafe{std::str::from_utf8_unchecked(err_message_vec.leak())})
				.expect("null byte found in error message")
				.as_ntstrptr()
		};

		let error_string = {
			let mut error_string = Vec::new_in(a);
			for stack_frame in call_stack {
				match stack_frame.file_path {
					Some(file_path) => {
						write!(error_string, 
							"    called from {} ({}:{}:{})\n", 
							stack_frame.fn_name.to_str(), 
							unsafe{OsStr::from_encoded_bytes_unchecked(file_path.to_bytes()).display()}, 
							stack_frame.span.line, 
							stack_frame.span.get_col(stack_frame.file_text.to_str()),
						).expect("Writing into a Vec can never fail");
					}
					None => {
						write!(error_string, 
							"    called from {}\n", 
							stack_frame.fn_name.to_str(), 
						).expect("Writing into a Vec can never fail");
					}
				}
			}
			write!(error_string, "Runtime Error: {}\n", error_message)
				.expect("Writing into a Vec can never fail");

			write!(error_string, "{} $ {}\0", err_span.line, source_line)
				.expect("Writing into a Vec can never fail");
			// SAFETY: The string is from a formatter which only outputs valid
			// utf8
			NTStr::try_from_str(unsafe{std::str::from_utf8_unchecked(error_string.leak())})
				.expect("null byte found in error message")
				.as_ntstrptr()
		};
		println!("{}", error_string);

		let export_fn_name = copy_str_as_ntstr(export_fn_name, a).as_ntstrptr();
		let script_path = copy_bytes_as_nt(script_path.as_encoded_bytes(), a);
		
		Self {
			kind,
			call_stack,
			export_fn_name,
			script_path,
			err_span,
			source_line,
			error_message,
			error_string,
		}
	}

	/// Get the script_path field as an OsStr instead of an NTBytes.
	pub fn script_path_as_osstr(&self) -> &'a OsStr {
		// SAFETY: self.script_path is valid to convert to an OsStr
		unsafe{OsStr::from_encoded_bytes_unchecked(self.script_path.to_bytes())}
	}
}

/// Provides information about a single stack frame in a grug execution
#[repr(C)]
#[derive(Copy, Clone)]
pub struct StackFrame<'a> {
	/// The name of the function being executed
	pub fn_name: NTStrPtr<'a>,
	/// The path to the file relative to the mods directory
	///
	/// file_path == None (null in c) indicates that this is a host fn frame.
	/// if file_path is none, the span and file text is the span within the last grug script.
	pub file_path: Option<NTBytes<'a>>,
	/// The location in code where another function was called
	pub span: SourceSpan,
	/// The entire text of the file where the frame occurred
	pub file_text: NTStrPtr<'a>,
}

impl<'a> std::fmt::Debug for StackFrame<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_struct("StackFrame")
			.field("fn_name", &self.fn_name.to_str())
			.field("file_path", &self.file_path.map(|file_path| unsafe{OsStr::from_encoded_bytes_unchecked(file_path.to_bytes())}))
			.field("span", &self.span)
			.field("file_text", &self.file_text.to_str())
			.finish()
	}
}
