use std::ptr::NonNull;
use std::marker::PhantomData;
use std::mem::{size_of, ManuallyDrop};
use std::alloc::{alloc, Layout, handle_alloc_error, dealloc};
use std::cell::Cell;

/// A growable exponential array 
/// Insertion returns a pinned pointer to a value.
/// The values inside the Xar are not dropped unless delete is called with a pointer to the item
/// This also includes the destructor. 
/// The destructor simply deallocates the memory and does not drop the values in it.
pub struct Xar<T> {
	committed: Cell<usize>,
	inner: NonNull<XarInner<T>>,
}
// # SAFETY
// Sending ownership to a different thread requiries that there are no 
// current references into the Xar
unsafe impl<T: Send> Send for Xar<T> {}


#[repr(C)]
union XarStorage<T> {
	value: ManuallyDrop<T>,
	free_list: Option<NonNull<Self>>,
}

impl<T> XarStorage<T> {
	fn value(value: T) -> Self {
		Self {
			value: ManuallyDrop::new(value),
		}
	}

	fn free_list(free_list: Option<NonNull<Self>>) -> Self {
		Self {
			free_list
		}
	}
}

struct XarInner<T> {
	free_list: Cell<Option<NonNull<XarStorage<T>>>>,
	chunks: [Cell<Option<NonNull<XarStorage<T>>>>; 62],
	// TODO:
	// allocator: A
}

impl<T> Xar<T> {
	const _ASSERT_1: () = assert!(size_of::<XarHandle<'static, T>>() == size_of::<XarHandle<'static, Option<T>>>());
	const FIRST_SIZE: usize = const {
		let size = size_of::<XarStorage<T>>();
		let size = if 64 / size > 8 {64 / size} else {8};
		size
	};
	pub fn new() -> Self {
		Self {
			committed: Cell::new(0),
			inner: Self::create_inner(),
		}
	}

	pub fn insert(&self, value: T) -> XarHandle<'_, T> {
		if size_of::<T>() == 0 {
			return XarHandle::new(NonNull::dangling());
		}
		let inner = unsafe{&*self.inner.as_ptr()};
		if let Some(free) = inner.free_list.get() {
			unsafe{
				let next = (*free.as_ptr()).free_list;
				*free.as_ptr() = XarStorage::value(value);
				inner.free_list.set(next);
				XarHandle::new(free)
			}
		} else {
			let location = Self::calc_location(self.committed.get());
			let chunk = unsafe{&(*self.inner.as_ptr()).chunks[location.0]};
			let chunk = if let Some(chunk) = chunk.get() {
				chunk
			} else {
				let ret_val = Self::alloc_chunk(location.0);
				chunk.set(Some(ret_val));
				ret_val
			};
			unsafe {
				chunk.add(location.1).write(XarStorage::value(value));
			}
			self.committed.update(|x| x + 1);
			unsafe {
				XarHandle::new(chunk.add(location.1))
			}
		}
	}

	fn alloc_chunk(bucket_idx: usize) -> NonNull<XarStorage<T>> {
		let chunk_size = Self::chunk_size(bucket_idx);

		let layout = Layout::array::<XarStorage<T>>(chunk_size)
			.expect("layout invalid");
		let ptr = unsafe{alloc(layout)}.cast::<XarStorage<T>>();
		if ptr.is_null() {
			handle_alloc_error(layout);
		}
		unsafe{NonNull::new_unchecked(ptr)}
	}

	fn chunk_size(bucket_idx: usize) -> usize {
		if bucket_idx == 0 || bucket_idx == 1 {
			Self::FIRST_SIZE
		} else {
			let mut bucket_size = Self::FIRST_SIZE * 2;
			(2..bucket_idx).for_each(|_| bucket_size *= 2);
			bucket_size
		}
	}

	/// # SAFETY
	/// handle must be from the current Xar
	pub unsafe fn delete(&self, handle: XarHandle<T>) {
		if size_of::<T>() == 0 {
			return;
		}
		unsafe{
			std::ptr::drop_in_place(handle.0.as_ptr());
		}
		let free_list = unsafe{&(*self.inner.as_ptr()).free_list};
		unsafe{*handle.0.as_ptr() = XarStorage::free_list(free_list.get())};
		free_list.set(Some(handle.0));
	}

	fn calc_location(idx: usize) -> (usize, usize) {
		if idx < Self::FIRST_SIZE {
			(0, idx)
		} else if idx < Self::FIRST_SIZE * 2 {
			(1, idx - Self::FIRST_SIZE)
		} else {
			let mut bucket = 2;
			let mut bucket_max = Self::FIRST_SIZE * 4;
			while bucket_max <= idx {
				bucket += 1;
				bucket_max *= 2;
			}
			(bucket, idx - bucket_max / 2)
		}
	}

	fn create_inner() -> NonNull<XarInner<T>> {
		if size_of::<T>() == 0 {
			return NonNull::dangling();
		}
		let layout = Layout::new::<XarInner<T>>();
		let ptr = unsafe{alloc(layout).cast::<XarInner<T>>()};
		if ptr.is_null() {
			handle_alloc_error(layout);
		}
		// SAFETY: NonNull and aligned
		unsafe {
			ptr.write(XarInner{
				free_list: Cell::new(None),
				chunks: std::array::from_fn(|_| Cell::new(None)),
			});
		}
		// SAFETY: NonNull
		unsafe{NonNull::new_unchecked(ptr)}
	}

	pub fn clear(&mut self) {
		if size_of::<T>() != 0 {
			*self.committed.get_mut() = 0;
			unsafe{*(*self.inner.as_ptr()).free_list.get_mut() = None};
		}
	}
}

impl<T> Drop for Xar<T> {
	fn drop (&mut self) {
		if size_of::<T>() != 0 {
			return;
		}
		let inner = unsafe{&mut (*self.inner.as_ptr())};
		for (i, chunk) in inner.chunks.iter_mut().filter_map(|x| x.get_mut().as_mut()).enumerate() {
			let chunk_size = Self::chunk_size(i);
			let layout = Layout::array::<XarStorage<T>>(chunk_size).unwrap();
			unsafe{dealloc(chunk.as_ptr().cast::<u8>(), layout)};
		}
	}
}

#[repr(transparent)]
pub struct XarHandle<'a, T> (NonNull<XarStorage<T>>, PhantomData<&'a ()>);

impl<'a, T: std::fmt::Debug> std::fmt::Debug for XarHandle<'a, T> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_struct("XarHandle")
			.field("location", &self.0)
			.field("value", &**self)
			.finish()
	}
}

impl<'a, T> XarHandle<'a, T> {
	fn new(ptr: NonNull<XarStorage<T>>) -> Self {
		Self(ptr, PhantomData)
	}

	/// # SAFETY 
	/// XarHandle is not Clone or Copy.
	/// This is to ensure that there is only ever one pointer to a slot which 
	/// statically ensures that a slot can only be dropped once
	/// 
	/// Sometimes though, it may be necessary to store this pointer in multiple places. 
	/// This means that it is now possible to have a double drop of a slot. 
	///
	/// The caller must ensure that only a single one of these values is passed to delete
	pub unsafe fn clone(&self) -> Self {
		Self(self.0, self.1)
	}
}

impl<'a, T> std::ops::Deref for XarHandle<'a, T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		unsafe{& (*self.0.as_ptr()).value}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn xar_test_1 () {
		assert_eq!(Xar::<usize>::FIRST_SIZE, 8);
		assert_eq!(Xar::<usize>::calc_location(0), (0, 0));
		assert_eq!(Xar::<usize>::calc_location(7), (0, 7));
		assert_eq!(Xar::<usize>::calc_location(8), (1, 0));

		assert_eq!(Xar::<usize>::calc_location(15), (1, 7));
		assert_eq!(Xar::<usize>::calc_location(16), (2, 0));
		assert_eq!(Xar::<usize>::calc_location(31), (2, 15));
		assert_eq!(Xar::<usize>::calc_location(32), (3, 0));
	}

	#[test]
	fn xar_test_2 () {
		let x = Xar::new();
		let x_1 = x.insert(25);
		let x_2 = x.insert(26);
		let x_3 = x.insert(27);
		let x_4 = x.insert(28);
		assert_eq!(25, *x_1);
		assert_eq!(26, *x_2);
		assert_eq!(27, *x_3);
		assert_eq!(28, *x_4);
		eprintln!("{:?}", x_1);
	}
}
