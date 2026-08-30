//! Defines types necessary for handling runtime errors

use std::ffi::OsStr;
use std::io::Write;
use crate::error::SourceSpan;
use crate::ntstring::{NTStrPtr, NTBytes, NTStr};
use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box;
/// Enum that represents all possible runtime errors
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RuntimeError<'a> {
	/// Execution of a grug_script takes longer than allowed.
	ExceededTimeLimit = 0,
	/// Indicates potentially unbounded recursion
	StackOverflow,
	/// A game function called the `set_runtime_error` function on the state
	/// with the given `message`
	GameFunctionError{
		message: &'a str,
	},
}

impl<'a> RuntimeError<'a> {
	/// Return the code defined by grug.h for a runtime error kind
	pub fn code(self) -> u32 {
		match self {
			Self::StackOverflow         => 0,
			Self::ExceededTimeLimit     => 1,
			Self::GameFunctionError{..} => 2,
		}
	}
}

/// This is the maximum time allowed to execute an on function.
/// Backends are allowed to take longer than this time to throw an error.
pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// pub const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

/// This is the maximum allowed depth of function calls when executing an on function.
/// Backends are allowed to go further than this limit because of optimizations.
pub const MAX_RECURSION_LIMIT: usize = 100;

impl<'a> std::fmt::Display for RuntimeError<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::ExceededTimeLimit => write!(f, "Took longer than {} milliseconds to run", ON_FN_TIME_LIMIT),
			Self::StackOverflow => write!(f, "Stack overflow, so check for accidental infinite recursion"),
			Self::GameFunctionError{message} => write!(f, "{}", message),
		}
	}
}

#[repr(u32)]
pub enum RuntimeErrorKind {
	TimeLimitExceeded,
	StackOverflow,
	HostFnError,
}

#[repr(C)]
pub struct RuntimeError2<'a> {
	/// The kind of runtime error 
	pub kind: RuntimeErrorKind,
	/// The state of the callstack when the error occurred
	/// This is a best effort guess at the state which may or may not be
	/// deformed due to optimizations
	pub call_stack: &'a [StackFrame<'a>],
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

impl<'a> RuntimeError2<'a> {
	/// The call_stack must have already been allocated within `a`
	#[track_caller]
	pub fn new_error_in<A: Allocator>(kind: RuntimeErrorKind, call_stack: &'a [StackFrame<'a>], err_span: SourceSpan, source_text: &str, error_message: std::fmt::Arguments, a: &'a A) -> Self {
		// Copy source_line into an allocator and return a reference to the new string
		// Equivalent to Box::leak(Box::from(str)) except the box is allocated in a custom allocator
		// cannot use box_from_str_in because that doesn't allow null bytes
		// within the string
		let source_line = {
			let source_line = err_span.get_source_line(source_text);
			let mut slice = Box::<[u8], _>::new_uninit_slice_in(source_line.len(), &a);
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
		let error_message = {
			let mut err_message_vec = Vec::new_in(a);
			write!(err_message_vec, "{}\0", error_message).expect("Writing to a vec can never fail");
			// SAFETY: The string is from a formatter which only outputs valid
			// utf8
			NTStr::try_from_str(unsafe{std::str::from_utf8_unchecked(err_message_vec.leak())})
				.expect("null byte found in error message")
		};

		let error_string = {
			let mut error_string = Vec::new_in(a);
			for stack_frame in call_stack {
				match stack_frame {
					StackFrame::GrugCall{ function_name, file_path, span, file_text } => {
						write!(error_string, 
							"    called from {} ({}:{}:{})\n", 
							function_name.to_str(), 
							unsafe{OsStr::from_encoded_bytes_unchecked(file_path.to_bytes()).display()}, 
							span.line, 
							span.get_col(file_text.to_str()),
						).expect("Writing into a Vec can never fail");
					}
					StackFrame::HostCall { function_name } => {
						write!(error_string, 
							"    called from {}\n", 
							function_name.to_str(), 
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
		};
		
		Self {
			kind,
			call_stack,
			err_span,
			source_line,
			error_message: error_message.as_ntstrptr(),
			error_string: error_string.as_ntstrptr(),
		}
	}
}

#[repr(C, u32)]
#[derive(Copy, Clone, Debug)]
pub enum StackFrame<'a> {
	/// The stack frame is from a grug export or local function. 
	GrugCall {
		function_name: NTStrPtr<'a>,
		file_path: NTBytes<'a>,
		span: SourceSpan,
		file_text: NTStrPtr<'a>
	},
	/// The stack frame belongs to a host function
	HostCall {
		function_name: NTStrPtr<'a>,
	}
}
