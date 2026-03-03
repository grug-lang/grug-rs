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
pub trait State: Sized {
	fn set_runtime_error(&self, error: RuntimeError);
	fn is_errorring(&self) -> bool;
}
