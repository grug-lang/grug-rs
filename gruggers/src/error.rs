use crate::frontend::FileError;
use crate::frontend::tokenizer::TokenizerError;
use crate::frontend::parser::ParserError;
use crate::frontend::type_propagation::TypePropogatorError;
use crate::mod_api::ModApiError;
use crate::arena::Arena;
use crate::ntstring::{NTStrPtr, NTStr};
use crate::nt;
pub use gruggers_core::runtime_error::RuntimeError;

use allocator_api2::vec::Vec;

use std::marker::PhantomData;
use std::io::Write;

#[derive(Debug)]
pub enum GrugError {
	FileError(FileError),
	TokenizerError(TokenizerError),
	ParserError(ParserError),
	ModApiError(ModApiError),
	TypePropogatorError(TypePropogatorError),
}

impl From<FileError> for GrugError {
	fn from (from: FileError) -> Self {
		// this extra single quote is needed to prevent a vim plugin from
		// mishandling quotes in the rest of the file
		// '
		Self::FileError(from)
	}
}

impl From<TokenizerError> for GrugError {
	fn from (from: TokenizerError) -> Self {
		Self::TokenizerError(from)
	}
}

impl From<ParserError> for GrugError {
	fn from (from: ParserError) -> Self {
		Self::ParserError(from)
	}
}

impl From<ModApiError> for GrugError {
	fn from(other: ModApiError) -> Self {
		Self::ModApiError(other)
	}
}

impl From<TypePropogatorError> for GrugError {
	fn from(other: TypePropogatorError) -> Self {
		Self::TypePropogatorError(other)
	}
}

impl std::fmt::Display for GrugError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::TokenizerError(error) => write!(f, "{}", error),
			Self::FileError(error) => write!(f, "{}", error),
			Self::ParserError(error) => write!(f, "{}", error),
			Self::TypePropogatorError(error) => write!(f, "{}", error),
			err => write!(f, "{:?}", err),
		}
	}
}


#[allow(dead_code)]
pub struct SourceSpan {
	pub offset: usize,
	pub len: usize,
}

impl SourceSpan {
	pub fn get_loc_in(&self, text: &str) -> Option<(usize, usize)> {
		let mut lines = 0;
		let mut last_new_line = 0;
		let text = text.as_bytes();
		for (i, ch) in text.get(..self.offset)?.iter().enumerate() {
			if *ch == b'\n' {
				lines += 1;
				last_new_line = i;
			}
		}
		Some((lines, self.offset - last_new_line))
	}
}

#[allow(dead_code)]
struct Diagnostic<'a>(PhantomData<&'a ()>);

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub struct grug_error {
	allocator: Arena,
	error_string: NTStrPtr<'static>,
	severity: NTStrPtr<'static>,
	file_name: NTStrPtr<'static>,
	file_text: NTStrPtr<'static>,
	error_span: SourceSpan,
	error_message: NTStrPtr<'static>,
	_diagnostics: &'static [Diagnostic<'static>],
}

impl std::fmt::Display for grug_error {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		f.write_str(self.error_string.to_str())
	}
}

impl grug_error {
	#[allow(dead_code)]
	pub(crate) fn new_error(error_message: std::fmt::Arguments, file_name: &str, file_text: &str, span: SourceSpan) -> Self {
		let arena = Arena::new();
		// SAFETY: We never give out a `'static` pointer to this string from safe code
		let (line, col) = span.get_loc_in(file_text)
			.expect("Source span offset greater than file size");
		let file_text = unsafe{NTStr::box_from_str_in(file_text, &arena).as_ntstrptr().detach_lifetime()};
		let file_name = unsafe{NTStr::box_from_str_in(file_name, &arena).as_ntstrptr().detach_lifetime()};
		let severity = nt!("Error");
		let mut err_string = Vec::new_in(&arena);
		write!(err_string, "{} {}:{}:{}: {}\0", severity.as_str(), file_name, line, col, error_message)
			.expect("writing into a vec should never fail");
		// SAFETY: err_string only contains utf8 strings, and is null terminated
		// The string can last as long as the arena exists, and we never give
		// out a 'static pointer to the string
		let error_string = unsafe{
			NTStr::from_str_unchecked(
				str::from_utf8_unchecked(err_string.leak())
			).as_ntstrptr().detach_lifetime()
		};

		let mut err_message = Vec::new_in(&arena);
		write!(err_message, "{}\0", error_message)
			.expect("writing into a vec should never fail");
		let error_message = unsafe{
			NTStr::from_str_unchecked(
				str::from_utf8_unchecked(err_message.leak())
			).as_ntstrptr().detach_lifetime()
		};


		Self {
			allocator: arena,
			error_string,
			severity: severity.as_ntstrptr(),
			file_name,
			file_text,
			error_span: span,
			error_message,
			_diagnostics: &[],
		}
	}
}

const _: () = const{
	// The C interop with Rust assumes that slice pointers have a layout like this
	// #[repr(C)]
	// struct Slice<T> {
	// 		data: NonNull<T>,
	// 		len : usize,
	// }
	// 
	// The rust compiler currently does not guarantee the layout of slice pointer.
	// These assertions ensure that if the assumption is broken, we get a
	// compile error instead of random crashes
	let x: &[Diagnostic] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
};
