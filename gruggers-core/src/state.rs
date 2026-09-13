//! Defines the [`State`] trait
use crate::runtime_error::RuntimeError;

/// Defines the interface a state needs to implement to be used by a backend.
///
/// This is not enough for a full implementation of a state, but the other
/// parts of the interface are not needed by the backend
///
/// The handle_runtime_error function is passed to the backend during
/// construction (can be a function pointer passed in at runtime or can be
/// passed in as through a generic parameter). The backend is expected to store
/// the function as a function pointer.
pub trait State: Sized {
	/// Tell the state to call the runtime error handler with this runtime error struct.
	fn handle_runtime_error(&self, error: &RuntimeError);
}
