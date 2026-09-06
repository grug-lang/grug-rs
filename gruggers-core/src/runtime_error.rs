//! Defines types necessary for handling runtime errors

use std::ffi::OsStr;
use std::io::Write;
use crate::error::SourceSpan;
use crate::ntstring::{NTStrPtr, NTBytes, NTStr};
use crate::utils::{copy_str, copy_bytes_nt, copy_str_nt};
use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;
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
#[derive(Clone, Copy, PartialEq, Eq)]
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
		let source_line = copy_str(err_span.get_source_line(source_text), a);

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
				match stack_frame {
					StackFrame::GrugCall{ function_name, file_path, span } => {
						write!(error_string, 
							"    called from {} ({}:{})\n", 
							function_name.to_str(), 
							unsafe{OsStr::from_encoded_bytes_unchecked(file_path.to_bytes()).display()}, 
							span.line, 
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
				.as_ntstrptr()
		};
		
		Self {
			kind,
			call_stack,
			err_span,
			source_line,
			error_message,
			error_string,
		}
	}

	pub fn copy_into<'b, A: Allocator>(&self, alloc: &'b A) -> RuntimeError2<'b> {
		let mut new_call_stack = Vec::with_capacity_in(self.call_stack.len(), alloc);
		for stack_frame in self.call_stack {
			new_call_stack.push(stack_frame.copy_into(alloc));
		}
		RuntimeError2 {
			kind: self.kind,
			call_stack: new_call_stack.leak(),
			source_line: copy_str(self.source_line, alloc),
			err_span: self.err_span,
			error_message: copy_str_nt(self.error_message.to_ntstr(), alloc).as_ntstrptr(),
			error_string: copy_str_nt(self.error_string.to_ntstr(), alloc).as_ntstrptr(),
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
	},
	/// The stack frame belongs to a host function
	HostCall {
		function_name: NTStrPtr<'a>,
	}
}

impl<'a> StackFrame<'a> {
	fn copy_into<'b>(&self, a: &'b impl Allocator) -> StackFrame<'b> {
		match *self {
			Self::GrugCall { function_name, file_path, span } => {
				StackFrame::GrugCall {
					function_name: copy_str_nt(function_name.to_ntstr(), a).as_ntstrptr(),
					file_path: copy_bytes_nt(file_path, a),
					span,
				}
			}
			Self::HostCall { function_name } => {
				StackFrame::HostCall {
					function_name: copy_str_nt(function_name.to_ntstr(), a).as_ntstrptr()
				}
			}
		}
	}
}
