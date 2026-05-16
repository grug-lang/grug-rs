pub use gruggers_core::error::*;

use crate::arena::Arena;
use std::ffi::OsStr;

/// Owned wrapper around [`GrugError`] that carries its
/// allocator within itself
///
/// The inner error can be obtained with [`Self::inner`]
pub struct Error {
	inner_error: GrugError<'static>,
	/// The fields of the error are allocated in this arena
	_arena: Arena
}

impl std::fmt::Debug for Error {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.inner_error.fmt(f)
	}
}

impl std::fmt::Display for Error {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.inner_error.fmt(f)
	}
}

impl Error {
	/// Provides a safe reference to the inner error
	pub fn inner<'a>(&'a self) -> &'a GrugError<'a> {
		&self.inner_error
	}

	/// Create a new grug error within an arena
	pub(crate) fn new(error_kind: ErrorKind, function_name: &str, file_path: &OsStr, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments) -> Self {
		let arena = Arena::new();
		let error = GrugError::new_error_in(error_kind, function_name, file_path, source_text, err_span, error_message, &arena);
		// Transmute to a static lifetime
		// SAFETY: We never give out a 'static reference to the inner error
		let error = unsafe{std::mem::transmute::<GrugError, GrugError<'static>>(error)};
		Self {
			inner_error: error,
			_arena: arena
		}
	}
}
