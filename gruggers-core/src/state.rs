//! Defines the [`State`] trait
use crate::runtime_error::RuntimeError;

pub(crate) struct DummyState;
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
