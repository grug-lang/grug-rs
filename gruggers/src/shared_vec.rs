use std::cell::UnsafeCell;
use allocator_api2::vec::Vec;
use allocator_api2::alloc::{Allocator, Global};
/// A Vec where you can push and pop with a shared reference. 
///
/// This obviously means it is unsafe to get a reference to any element, and it is not Sync.
pub struct SharedVec<T, A: Allocator = Global>(UnsafeCell<Vec<T, A>>);

// SAFETY: No thread local data is used
unsafe impl<T: Send, A: Allocator + Send> Send for SharedVec<T, A> {}

impl<T> SharedVec<T, Global> {
	pub const fn new() -> Self {
		Self(UnsafeCell::new(Vec::new()))
	}
}

#[allow(unused)]
impl<T, A: Allocator> SharedVec<T, A> {
	pub fn push(&self, val: T) {
		// We never give out a reference to the inner values.
		unsafe{&mut *self.0.get()}.push(val)
	}

	pub fn pop(&self) -> Option<T> {
		unsafe{&mut *self.0.get()}.pop()
	}

	/// Returns the entire vec as a slice. 
	/// It is UB to push or pop any data while this slice is active
	pub unsafe fn as_slice_unsafe(&self) -> &[T] {
		unsafe{&*self.0.get()}.as_slice()
	}

	pub fn leak<'a> (self) -> &'a [T] where A: 'a {
		self.0.into_inner().leak()
	}

	pub fn len<'a> (&self) -> usize {
		unsafe{&*self.0.get()}.len()
	}
	pub fn truncate(&self, len: usize) {
		unsafe{&mut *self.0.get()}.truncate(len);
	}
}
