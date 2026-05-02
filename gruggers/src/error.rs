use crate::frontend::type_propagation::TypePropogatorError;
use crate::mod_api::ModApiError;
pub use gruggers_core::runtime_error::RuntimeError;
use gruggers_core::error::grug_error;
use crate::arena::Arena;

#[derive(Debug)]
pub enum GrugError {
	GrugError(grug_error<Arena>),
	ModApiError(ModApiError),
	TypePropogatorError(TypePropogatorError),
}

impl From<grug_error<Arena>> for GrugError {
	fn from (from: grug_error<Arena>) -> Self {
		Self::GrugError(from)
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
			Self::TypePropogatorError(error) => write!(f, "{}", error),
			Self::GrugError(error) => write!(f, "{}", error),
			Self::ModApiError(error) => write!(f, "{:?}", error),
		}
	}
}
