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

	/// Leaks the arena and returns a &'static [`GrugError`]
	///
	/// The only known use case of this is the [`grug_init`] function in the c
	/// api. That function has no place to store an [`Error`], so it has to
	/// leak the memory to return an error to the caller.
	pub fn leak<'a>(self) -> GrugError<'static> {
		let inner = unsafe{std::mem::transmute::<GrugError, GrugError<'static>>(*self.inner())};
		std::mem::forget(self);
		inner
	}

	/// Create a new grug error within an arena
	#[track_caller]
	pub(crate) fn new(error_kind: ErrorKind, function_name: &str, file_path: &OsStr, source_text: &str, err_span: SourceSpan, error_message: std::fmt::Arguments) -> Self {
		println!("{:?}", std::panic::Location::caller());
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

	#[track_caller]
	pub(crate) fn from_io_error(err: std::io::Error, file_path: &OsStr) -> Self {
		Self::new(
			ErrorKind::IO_ERROR,
			"",
			file_path,
			"",
			SourceSpan {offset: 0, line: 0},
			format_args!("IO error: {}", err),
		)
	}
}
