use crate::frontend::FileNameError;
use crate::frontend::tokenizer::TokenizerError;
use crate::frontend::parser::ParserError;
use crate::frontend::type_propagation::TypePropogatorError;
use crate::mod_api::ModApiError;

#[derive(Debug)]
pub enum GrugError<'a> {
	FileNameError(FileNameError<'a>),
	TokenizerError(TokenizerError),
	ParserError(ParserError),
	ModApiError(ModApiError),
	TypePropogatorError(TypePropogatorError),
}

impl<'a> From<FileNameError<'a>> for GrugError<'a> {
	fn from (from: FileNameError<'a>) -> Self {
		// this extra single quote is needed to prevent a vim plugin from
		// mishandling quotes in the rest of the file
		// '
		Self::FileNameError(from)
	}
}

impl<'a> From<TokenizerError> for GrugError<'a> {
	fn from (from: TokenizerError) -> Self {
		Self::TokenizerError(from)
	}
}

impl<'a> From<ParserError> for GrugError<'a> {
	fn from (from: ParserError) -> Self {
		Self::ParserError(from)
	}
}

impl<'a> From<ModApiError> for GrugError<'a> {
	fn from(other: ModApiError) -> Self {
		Self::ModApiError(other)
	}
}

impl<'a> From<TypePropogatorError> for GrugError<'a> {
	fn from(other: TypePropogatorError) -> Self {
		Self::TypePropogatorError(other)
	}
}

impl<'a> std::fmt::Display for GrugError<'a> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::TokenizerError(error) => write!(f, "{}", error),
			Self::FileNameError(error) => write!(f, "{}", error),
			Self::ParserError(error) => write!(f, "{}", error),
			Self::TypePropogatorError(error) => write!(f, "{}", error),
			err => write!(f, "{:?}", err),
		}
	}
}

pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

pub const MAX_RECURSION_LIMIT: usize = 100;

#[derive(Debug, Clone)]
pub enum RuntimeError {
	ExceededTimeLimit,
	StackOverflow,
	GameFunctionError{
		message: &'static str,
	},
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
