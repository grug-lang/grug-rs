use std::ptr::NonNull;
use std::marker::PhantomData;
use std::mem::{size_of, ManuallyDrop};
use std::alloc::{alloc, Layout, handle_alloc_error, dealloc};
use std::cell::Cell;

mod xar {
	use super::*;

	// TODO: Test get_mut of XarHandle
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
		pub const FIRST_SIZE: usize = const {
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
			if size_of::<T>() == 0 {
				return;
			}
			let inner = unsafe{&mut (*self.inner.as_ptr())};
			for (i, chunk) in inner.chunks.iter_mut().filter_map(|x| x.get_mut().as_mut()).enumerate() {
				let chunk_size = Self::chunk_size(i);
				let layout = Layout::array::<XarStorage<T>>(chunk_size).unwrap();
				unsafe{dealloc(chunk.as_ptr().cast::<u8>(), layout)};
			}
			let layout = Layout::new::<XarInner<T>>();
			unsafe{dealloc(self.inner.as_ptr().cast::<u8>(), layout)};
		}
	}
	
	/// An unsafe pointer to a slot in a Xar
	/// 
	/// All access to the underlying data is unsafe.
	/// Users are encouraged to create a newtype around this handle to select
	/// either shared or unique semantics based on what they require.
	///
	/// The value in the slot is leaked if the XarHandle to it is dropped. 
	/// The only way to drop the value is to pass it back to the owning Xar
	/// which will also free the slot
	#[repr(transparent)]
	pub struct XarHandle<'a, T> (NonNull<XarStorage<T>>, PhantomData<&'a ()>);
	impl<'a, T> Clone for XarHandle<'a, T> {
		fn clone(&self) -> Self {
			Self::new(self.0)
		}
	}
	impl<'a, T> Copy for XarHandle<'a, T> {}

	impl<'a, T> XarHandle<'a, T> {
		fn new(ptr: NonNull<XarStorage<T>>) -> Self {
			Self(ptr, PhantomData)
		}

		/// SAFETY: The pointer MUST be derived from a XarHandle of the same type
		pub unsafe fn from_ptr(ptr: NonNull<T>) -> Self {
			Self(ptr.cast(), PhantomData)
		}

		pub unsafe fn get_ref(self) -> &'a T {
			unsafe{& (*self.0.as_ptr()).value}
		}

		pub unsafe fn get_mut(self) -> &'a mut T {
			unsafe{&mut (*self.0.as_ptr()).value}
		}

		/// # SAFETY 
		/// Some of the safety guarantees of Xar require the returned handles to be
		/// tied to its own lifetime. This makes storing these handles problematic
		/// sometimes. 
		///
		/// Take care when using the returned handle to ensure that the value is
		/// only used for as long as it is allowed to 
		pub unsafe fn detach_lifetime(self) -> XarHandle<'static, T> {
			unsafe{
				std::mem::transmute::<Self, XarHandle<'static, T>>(self)
			}
		}
	}

	impl<'a, T: std::fmt::Debug> std::fmt::Debug for XarHandle<'a, T> {
		fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
			f.debug_struct("XarHandle")
				.field("ptr", &self.0)
				.finish()
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
			assert_eq!(25, unsafe{*x_1.get_ref()});
			assert_eq!(26, unsafe{*x_2.get_ref()});
			assert_eq!(27, unsafe{*x_3.get_ref()});
			assert_eq!(28, unsafe{*x_4.get_ref()});

			*unsafe{x_1.get_mut()} += 5;
			*unsafe{x_2.get_mut()} += 5;
			*unsafe{x_3.get_mut()} += 5;
			*unsafe{x_4.get_mut()} += 5;

			assert_eq!(30, unsafe{*x_1.get_ref()});
			assert_eq!(31, unsafe{*x_2.get_ref()});
			assert_eq!(32, unsafe{*x_3.get_ref()});
			assert_eq!(33, unsafe{*x_4.get_ref()});
			eprintln!("{:?}", x_1);
		}

		#[test]
		fn xar_test_3 () {
			let x = Xar::new();
			let mut vec = Vec::new();
			for i in 0..1000 {
				unsafe{vec.push(x.insert(i).get_mut())};
			}
			for i in 0..1000 {
				*vec[i] *= 2;
			}
			for i in 0..1000 {
				assert_eq!(*vec[i], 2 * i);
			}
		}
	}
}
pub use xar::*;

mod erased_xar {
	use super::*;
	/// A growable exponential array 
	/// Insertion returns a pinned pointer to a value.
	/// The values inside the Xar are not dropped unless delete is called with a pointer to the item
	/// This also includes the destructor. 
	/// The destructor simply deallocates the memory and does not drop the values in it.
	pub struct ErasedXar {
		committed: Cell<usize>,
		inner: NonNull<XarInner>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy)]
	pub struct ErasedPtr<'a>(NonNull<()>, PhantomData<&'a ()>);
	const _: () = assert!(size_of::<ErasedPtr<'static>>() == size_of::<Option<ErasedPtr<'static>>>());

	impl<'a> ErasedPtr<'a> {
		pub fn from_ptr(ptr: NonNull<()>) -> Self {
			Self(ptr, PhantomData)
		}

		pub unsafe fn as_ref<T>(self) -> &'a T {
			unsafe{&*self.0.cast::<T>().as_ptr()}
		}

		/// ErasedPtr must be valid to write for bytes.len bytes
		pub unsafe fn write_bytes(self, bytes: &[u8]) {
			unsafe{self.0.as_ptr().cast::<u8>().copy_from(bytes.as_ptr(), bytes.len())}
		}
		
		/// ErasedPtr must be aligned to T and must have atleast enough space
		/// for a [T] with `len` elements
		pub unsafe fn write_slice<T: Clone>(self, len: usize, value: T) -> *mut [T] {
			for i in 1..len {
				unsafe{self.0.cast::<T>().add(i).write(value.clone())};
			}
			if len > 0 {
				unsafe{self.0.cast::<T>().write(value)}
			}
			std::ptr::slice_from_raw_parts_mut(self.0.as_ptr().cast::<T>(), len)
		}

		/// ErasedPtr must be aligned to T and must have atleast enough space
		/// for a T
		pub unsafe fn write_value<T>(self, value: T) {
			unsafe{self.0.cast::<T>().write(value)};
		}

		pub unsafe fn detach_lifetime(self) -> ErasedPtr<'static> {
			unsafe{std::mem::transmute::<Self, ErasedPtr<'static>>(self)}
		}

		/// Reads the value as is
		pub unsafe fn read<T>(self) -> T {
			unsafe{self.0.as_ptr().cast::<T>().read()}
		}

		pub unsafe fn byte_add(self, bytes: usize) -> Self {
			unsafe{Self(self.0.byte_add(bytes), PhantomData)}
		}

		/// Drops the value at the pointee
		pub unsafe fn drop_in_place<T>(self) {
			unsafe{self.0.as_ptr().cast::<T>().drop_in_place()}
		}

		pub fn as_ptr(self) -> NonNull<()> {
			self.0
		}
	}

	struct XarInner {
		item_layout: Layout,
		free_list: Cell<Option<ErasedPtr<'static>>>,
		chunks: [Cell<Option<ErasedPtr<'static>>>; 61],
		// TODO:
		// allocator: A
	}

	impl ErasedXar {
		pub fn new(item_layout: Layout) -> Self {
			Self {
				committed: Cell::new(0),
				inner: Self::create_inner(item_layout),
			}
		}

		/// SAFETY: Layout of T must match the layout used to create the ErasedXar
		pub unsafe fn insert<T>(&self, value: T) -> ErasedPtr<'_> {
			debug_assert!(Layout::new::<T>() == unsafe{(*self.inner.as_ptr()).item_layout});
			let ret_val = self.get_slot();
			unsafe{ret_val.write_value(value)};
			ret_val
		}

		pub fn get_slot(&self) -> ErasedPtr<'_> {
			if self.item_size() == 0 {
				return ErasedPtr::from_ptr(NonNull::dangling());
			}
			let inner = unsafe{&*self.inner.as_ptr()};
			if let Some(free) = inner.free_list.get() {
				unsafe{
					let next = free.read::<Option<ErasedPtr>>();
					inner.free_list.set(next);
					free
				}
			} else {
				let location = self.calc_location(self.committed.get());
				let chunk = unsafe{&(*self.inner.as_ptr()).chunks[location.0]};
				let chunk = if let Some(chunk) = chunk.get() {
					chunk
				} else {
					let ret_val = self.alloc_chunk(location.0);
					unsafe{chunk.set(Some(ret_val.detach_lifetime()))};
					ret_val
				};
				let ret_val = unsafe {
					chunk.byte_add(location.1 * self.item_size())
				};
				self.committed.update(|x| x + 1);
				ret_val
			}
		}

		fn item_size(&self) -> usize {
			unsafe{(*self.inner.as_ptr()).item_layout.size()}
		}

		fn item_align(&self) -> usize {
			unsafe{(*self.inner.as_ptr()).item_layout.align()}
		}

		fn first_chunk_size(&self) -> usize {
			let chunk_size = self.item_size();
			let chunk_size = if 64 / chunk_size > 8 {64 / chunk_size} else {8};
			chunk_size
		}

		fn alloc_chunk(&self, bucket_idx: usize) -> ErasedPtr<'_> {
			let layout = self.chunk_layout(bucket_idx);
			let ptr = unsafe{alloc(layout)}.cast::<()>();
			if ptr.is_null() {
				handle_alloc_error(layout);
			}
			ErasedPtr::from_ptr(unsafe{NonNull::new_unchecked(ptr)})
		}

		fn chunk_size(&self, bucket_idx: usize) -> usize {
			if bucket_idx == 0 || bucket_idx == 1 {
				self.first_chunk_size()
			} else {
				let mut bucket_size = self.first_chunk_size() * 2;
				(2..bucket_idx).for_each(|_| bucket_size *= 2);
				bucket_size
			}
		}

		fn chunk_layout(&self, bucket_idx: usize) -> Layout {
			Layout::from_size_align(self.chunk_size(bucket_idx) * self.item_size(), self.item_align())
				.expect("invalid layout")
		}

		/// # SAFETY
		/// handle must be from the current Xar
		pub unsafe fn delete_with<F: FnOnce(ErasedPtr)>(&self, handle: ErasedPtr, f: F) {
			f(handle);
			if self.item_size() == 0 {
				return;
			}
			
			let free_list = unsafe{&(*self.inner.as_ptr()).free_list};
			unsafe{handle.write_value(free_list.get());}
			unsafe{free_list.set(Some(handle.detach_lifetime()))};
		}

		/// # SAFETY
		/// handle must be from the current Xar
		pub unsafe fn delete(&self, handle: ErasedPtr) {
			unsafe{self.delete_with(handle, |_| {})}
		}

		fn calc_location(&self, idx: usize) -> (usize, usize) {
			if idx < self.first_chunk_size() {
				(0, idx)
			} else if idx < self.first_chunk_size() * 2 {
				(1, idx - self.first_chunk_size())
			} else {
				let mut bucket = 2;
				let mut bucket_max = self.first_chunk_size() * 4;
				while bucket_max <= idx {
					bucket += 1;
					bucket_max *= 2;
				}
				(bucket, idx - bucket_max / 2)
			}
		}

		fn create_inner(mut item_layout: Layout) -> NonNull<XarInner> {
			let align = std::cmp::max(item_layout.align(), align_of::<Option<ErasedPtr>>());
			let size = std::cmp::max(item_layout.size(), size_of::<Option<ErasedPtr>>());
			item_layout = Layout::from_size_align(size, align)
				.expect("invalid layout");

			let layout = Layout::new::<XarInner>();
			let ptr = unsafe{alloc(layout).cast::<XarInner>()};
			if ptr.is_null() {
				handle_alloc_error(layout);
			}
			// SAFETY: NonNull and aligned
			unsafe {
				ptr.write(XarInner{
					item_layout,
					free_list: Cell::new(None),
					chunks: std::array::from_fn(|_| Cell::new(None)),
				});
			}
			// SAFETY: NonNull
			unsafe{NonNull::new_unchecked(ptr)}
		}

		pub fn clear(&mut self) {
			if self.item_size() != 0 {
				*self.committed.get_mut() = 0;
				unsafe{*(*self.inner.as_ptr()).free_list.get_mut() = None};
			}
		}
	}

	impl Drop for ErasedXar {
		fn drop (&mut self) {
			if self.item_size() == 0 {
				return;
			}
			let inner = unsafe{&mut (*self.inner.as_ptr())};
			for (i, chunk) in inner.chunks.iter_mut().filter_map(|x| x.get_mut().as_mut()).enumerate() {
				let layout = self.chunk_layout(i);
				unsafe{dealloc(chunk.0.as_ptr().cast::<u8>(), layout)};
			}
			let layout = Layout::new::<XarInner>();
			unsafe{dealloc(self.inner.as_ptr().cast::<u8>(), layout)};
		}
	}

	mod trait_impls {
		use super::*;

		use std::fmt::{Debug, Formatter};

		impl Debug for ErasedXar {
			fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
				f.debug_struct("ErasedXar")
					.field("committed", &self.committed.get())
					.field("layout", unsafe{&(*self.inner.as_ptr()).item_layout})
					.finish()
			}
		}

		impl<'a> std::fmt::Debug for ErasedPtr<'a> {
			fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
				f.debug_struct("ErasedPtr")
					.field("ptr", &self.0)
					.finish()
			}
		}
	}

	#[cfg(test)]
	mod test {
		use super::*;
		#[test]
		fn xar_test_1 () {
			let x = ErasedXar::new(Layout::new::<usize>());
			assert_eq!(x.first_chunk_size(), 8);
			assert_eq!(x.calc_location(0), (0, 0));
			assert_eq!(x.calc_location(7), (0, 7));
			assert_eq!(x.calc_location(8), (1, 0));

			assert_eq!(x.calc_location(15), (1, 7));
			assert_eq!(x.calc_location(16), (2, 0));
			assert_eq!(x.calc_location(31), (2, 15));
			assert_eq!(x.calc_location(32), (3, 0));
		}

		#[test]
		fn xar_test_2 () {
			let x = ErasedXar::new(Layout::new::<usize>());
			let x_1 = x.get_slot();
			let x_2 = x.get_slot();
			let x_3 = x.get_slot();
			let x_4 = x.get_slot();
			unsafe{
				x_1.write_value::<usize>(25);
				x_2.write_value::<usize>(26);
				x_3.write_value::<usize>(27);
				x_4.write_value::<usize>(28);
				assert_eq!(25, *x_1.as_ref::<usize>());
				assert_eq!(26, *x_2.as_ref::<usize>());
				assert_eq!(27, *x_3.as_ref::<usize>());
				assert_eq!(28, *x_4.as_ref::<usize>());
				eprintln!("{:?}", x_1.as_ref::<usize>());
			}
		}
	}
}
pub use erased_xar::*;

