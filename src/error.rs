use crate::frontend::FileError;
use crate::frontend::tokenizer::TokenizerError;
use crate::frontend::parser::ParserError;
use crate::frontend::type_propagation::TypePropogatorError;
use crate::mod_api::ModApiError;

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

pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

pub const MAX_RECURSION_LIMIT: usize = 100;

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RuntimeError {
	ExceededTimeLimit,
	StackOverflow,
	// GameFunctionError,
	GameFunctionError{
		message: &'static str,
	},
}

impl RuntimeError {
	pub fn into_code(self) -> u32 {
		match self {
			Self::StackOverflow         => 0,
			Self::ExceededTimeLimit     => 1,
			Self::GameFunctionError{..} => 2,
		}
	}
}

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::ExceededTimeLimit => write!(f, "Took longer than {} milliseconds to run", ON_FN_TIME_LIMIT),
			Self::StackOverflow => write!(f, "Stack overflow, so check for accidental infinite recursion"),
			Self::GameFunctionError{message} => write!(f, "{}", message),
		}
	}
}
