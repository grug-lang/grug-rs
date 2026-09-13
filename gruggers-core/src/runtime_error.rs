//! Defines types necessary for handling runtime errors

use std::ffi::OsStr;
use std::io::Write;
use crate::error::SourceSpan;
use crate::ntstring::{NTStrPtr, NTBytes, NTStr};
use crate::utils::{copy_str, copy_bytes_nt, copy_str_nt, copy_str_as_ntstr, copy_bytes_as_nt};
use allocator_api2::alloc::Allocator;
use allocator_api2::vec::Vec;

/// This is the maximum time allowed to execute an on function.
/// Backends are allowed to take longer than this time to throw an error.
pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// pub const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

/// This is the maximum allowed depth of function calls when executing an on function.
/// Backends are allowed to go further than this limit because of optimizations.
pub const MAX_RECURSION_LIMIT: usize = 100;

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorKind {
	StackOverflow,
	TimeLimitExceeded,
	HostFnError,
}

#[repr(C)]
pub struct RuntimeError<'a> {
	/// The kind of runtime error 
	pub kind: RuntimeErrorKind,
	/// The state of the callstack when the error occurred
	/// This is a best effort guess at the state which may or may not be
	/// deformed due to optimizations
	pub call_stack: &'a [&'a StackFrame<'a>],
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
		call_stack: &'a [&'a StackFrame<'a>], 
		export_fn_name: &str,
		script_path: &OsStr,
		err_span: SourceSpan, 
		source_text: &str, 
		error_message: &str, 
		a: &'a A
	) -> Self {
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

	pub fn copy_into<'b, A: Allocator>(&self, alloc: &'b A) -> RuntimeError<'b> {
		// call stack is a slice of references. So we first have to copy the
		// values into an array, then create a new array of references to the
		// first array
		let mut new_call_stack_storage = Vec::with_capacity_in(self.call_stack.len(), alloc);
		for stack_frame in self.call_stack {
			new_call_stack_storage.push(stack_frame.copy_into(alloc));
		}
		let new_call_stack_storage = &*new_call_stack_storage.leak();

		let mut new_call_stack = Vec::with_capacity_in(self.call_stack.len(), alloc);
		for stack_frame in new_call_stack_storage {
			new_call_stack.push(stack_frame);
		}

		RuntimeError {
			kind: self.kind,
			call_stack: new_call_stack.leak(),
			export_fn_name: copy_str_nt(self.export_fn_name.to_ntstr(), alloc).as_ntstrptr(),
			script_path: copy_bytes_nt(self.script_path, alloc),
			source_line: copy_str(self.source_line, alloc),
			err_span: self.err_span,
			error_message: copy_str_nt(self.error_message.to_ntstr(), alloc).as_ntstrptr(),
			error_string: copy_str_nt(self.error_string.to_ntstr(), alloc).as_ntstrptr(),
		}
	}

	pub fn script_path_as_osstr(&self) -> &'a OsStr {
		// SAFETY: self.script_path is valid to convert to an OsStr
		unsafe{OsStr::from_encoded_bytes_unchecked(self.script_path.to_bytes())}
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
