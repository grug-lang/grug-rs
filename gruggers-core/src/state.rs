use crate::runtime_error::RuntimeError;
use std::marker::PhantomData;

pub(crate) struct DummyState;
impl State for DummyState {
	fn set_runtime_error(&self, _error: RuntimeError) {
		unreachable!();
	}
}

// This is the interface that a state needs to follow so a backend can use it
pub trait State: Sized {
	fn set_runtime_error(&self, error: RuntimeError);
}
