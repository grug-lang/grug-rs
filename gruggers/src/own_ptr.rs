//! Provides the `OwnPtr<'a, T>` type which is a pointer that owns its pointee but 
//! not the memory it is allocated in. 
//!
//! # Usecase 
//!
//! Any memory allocated in an arena will automatically be freed when the arena
//! goes out of scope. As such, the destructors of [`Box`] and [`Vec`] are partially
//! unnecessary when allocated in an arena. 
//!
//! They are especially problematic in this kind of pattern, where a struct
//! contains several fields that are all allocated in the same allocator.
//!
//! ```rs
//! struct Foo {
//! 	bar: Box<str, &'static Arena>,
//! 	bax: Box<[u8], &'static Arena>,
//! 	_arena: Arena,
//! }
//! ```
//!
//! The allocator field of the boxes is a reference to the `_arena` field of `Foo`, and
//! hence this use of the 'static lifetime is unsound.
//!
//! The equivalent version using `OwnPtr` is not unsound because there is no
//! such reference to the arena.  The memory is automatically freed when the
//! `_arena` field is dropped
//!
//! ```rs
//! struct Foo {
//! 	bar: OwnPtr<'static, str>,
//! 	bax: OwnPtr<'static, [u8]>,
//! 	_arena: Arena,
//! }
//! ```

use std::ptr::NonNull;
use std::marker::PhantomData;
use allocator_api2::boxed::Box as Box2;
use allocator_api2::alloc::Allocator;

/// `OwnPtr<T>` behaves a lot like [`Box<T>`] except it does not own the memory it
/// points to. Hence, it also has a lifetime `'a`.
pub struct OwnPtr<'a, T: ?Sized>(NonNull<T>, PhantomData<&'a ()>);
// SAFETY: OwnPtr is Send and Sync for the same reason Box is.
unsafe impl<'a, T: Send + ?Sized> Send for OwnPtr<'a, T> {}
unsafe impl<'a, T: Sync + ?Sized> Sync for OwnPtr<'a, T> {}

impl<'a, T: ?Sized> OwnPtr<'a, T> {
	/// Creates an OwnPtr from a raw pointer
	///
	/// # SAFETY
	///
	/// `ptr` must be non-null and must point to an allocated and initialized T
	pub unsafe fn from_ptr(ptr: *mut T) -> Self {
		Self(unsafe{NonNull::new_unchecked(ptr)}, PhantomData)
	}
}

impl<'a, T: ?Sized> std::ops::Deref for OwnPtr<'a, T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		// SAFETY: self.0 always points to a valid T
		unsafe{self.0.as_ref()}
	}
}

impl<'a, T: ?Sized> std::ops::DerefMut for OwnPtr<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		// SAFETY: self.0 always points to a valid T
		unsafe{self.0.as_mut()}
	}
}

impl<'a, T> IntoIterator for OwnPtr<'a, [T]> {
	type IntoIter = IntoIter<'a, T>;
	type Item = T;
	fn into_iter(self) -> Self::IntoIter {
		let ret_val = IntoIter{
			start: self.0.cast::<T>(),
			// SAFETY: self.0 + self.0.len() does not wrap
			end: unsafe{self.0.cast::<T>().add(self.0.len())},
			marker: PhantomData
		};
		std::mem::forget(self);
		ret_val
	}
}

impl<'a, T: ?Sized> Drop for OwnPtr<'a, T> {
	fn drop (&mut self) {
		// SAFETY: self.0 is valid and needs to be dropped
		unsafe{std::ptr::drop_in_place(self.0.as_ptr())};
	}
}

impl<'a, T: ?Sized, A: Allocator + 'a> From<Box2<T, A>> for OwnPtr<'a, T> {
	fn from(other: Box2<T, A>) -> Self {
		// SAFETY: Box2::into_raw points to a valid T
		unsafe{Self::from_ptr(Box2::into_raw(other))}
	}
}

pub struct IntoIter<'a, T>{
	start: NonNull<T>,
	end: NonNull<T>,
	marker: PhantomData<&'a ()>
}

impl<'a, T> Iterator for IntoIter<'a, T> {
	type Item = T;
	fn next(&mut self) -> Option<Self::Item> {
		self.nth(0)
	}

	fn nth(&mut self, n: usize) -> Option<Self::Item> {
		if (unsafe{self.end.offset_from(self.start)} as usize) <= n {
			// SAFETY: self.start..(self.end) is valid to read and needs to be dropped
			unsafe{std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(self.start.as_ptr(), self.len()))};
			self.start = self.end;
			None
		} else {
			// SAFETY: self.start..(self.start + n) is valid to read and needs to be dropped
			unsafe{std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(self.start.as_ptr(), n))};
			// SAFETY: self.start + 1 does not wrap if self.start < self.end
			self.start = unsafe{self.start.add(n + 1)};
			Some(unsafe{self.start.sub(1).read()})
		}
	}

	fn last(mut self) -> Option<Self::Item> {
		if self.start == self.end {
			None
		} else {
			self.nth(self.len() - 1)
		}
	}

	fn count(self) -> usize {
		self.len()
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = self.len();
		(len, Some(len))
	}
}

impl<'a, T> DoubleEndedIterator for IntoIter<'a, T> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.nth_back(0)
	}

	fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
		if (unsafe{self.end.offset_from(self.start)} as usize) <= n {
			// SAFETY: self.start..(self.end) is valid to read and needs to be dropped
			unsafe{std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(self.start.as_ptr(), self.len()))};
			self.end = self.start;
			None
		} else {
			// SAFETY: (self.end - n)..self.end is valid to read and needs to be dropped
			unsafe{std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(self.end.sub(n).as_ptr(), n))};
			// SAFETY: self.start + 1 does not wrap if self.start < self.end
			self.end = unsafe{self.end.sub(n)};
			Some(unsafe{self.end.sub(1).read()})
		}
	}
}

impl<'a, T> Drop for IntoIter<'a, T> {
	fn drop (&mut self) {
		// SAFETY: self.start..(self.start + self.len()) is valid and needs to be dropped
		unsafe{std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(self.start.as_ptr(), self.len()))}
	}
}

impl<'a, T> ExactSizeIterator for IntoIter<'a, T> {
	fn len(&self) -> usize {
		unsafe{self.end.offset_from(self.start) as usize}
	}
}
