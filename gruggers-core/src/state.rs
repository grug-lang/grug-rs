//! Defines the [`State`] trait
use crate::runtime_error::RuntimeError;
use crate::backend::ErasedBackend;

pub struct DummyState;
impl State for DummyState {
	fn set_runtime_error(&self, _error: RuntimeError) {
		unreachable!();
	}
	fn is_errorring(&self) -> bool {
		unreachable!();
	}
}

// This is the interface that a state needs to follow so a backend can use it
/// Defines the interface a state needs to implement to be used by a backend.
///
/// This is not enough for a full implementation of a state, but the other
/// parts of the interface are not needed by the backend
pub trait State: Sized {
	/// Indicate to the state that an error has occurred. Used by the backend
	/// for `RuntimeError::ExceededTimeLimit` and
	/// `RuntimeError::StackOverflow`, and by the game for
	/// `RuntimeError::GameFunctionError`.
	fn set_runtime_error(&self, error: RuntimeError);
	/// Indicates if a runtime error has occurred in a game function or a
	/// recursive grug_function
	///
	/// This should be reset automatically when a new on_function is called
	fn is_errorring(&self) -> bool;
}

// This check ensures that c code can safely zero the backend field in GrugInitSettings
const _: () = unsafe{const {std::mem::forget(std::mem::MaybeUninit::<Option<ErasedBackend<DummyState>>>::zeroed().assume_init())}};
const _: () = const {assert!(std::mem::size_of::<Option<ErasedBackend<DummyState>>>() == std::mem::size_of::<ErasedBackend<DummyState>>())};

