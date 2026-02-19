mod page_alloc {
	// directly use VirtualAlloc and VirtualFree on windows
	#[cfg(all(not(miri), windows))]
	pub mod windows {
		#![allow(non_camel_case_types)]
		#![allow(non_snake_case)]
		pub struct PageAllocator;

		use std::ptr::NonNull;
		use core::ffi::{c_void, c_int};
		use allocator_api2::alloc::AllocError;

		type HANDLE = *mut c_void;
		type LPVOID = *mut c_void;
		type SIZE_T = usize;
		type DWORD = u32;
		type WORD = u16;
		type DWORD_PTR = *mut DWORD;
		type BOOL = c_int;


		#[link(name = "kernel32")]
		unsafe extern "system" {
			fn VirtualAllocEx (
				hProcess: HANDLE,
				lpAddress: LPVOID,
				dwSize: SIZE_T,
				flAllocationType: DWORD,
				flProtect : DWORD,
			) -> *mut c_void;

			fn VirtualFreeEx (
				process: HANDLE,
				Address: LPVOID,
				Size: DWORD,
				FreeType: DWORD,
			) -> BOOL;

			fn GetCurrentProcess() -> HANDLE;
		}

		const MEM_COMMIT: DWORD = 0x00001000;
		const MEM_RESERVE: DWORD = 0x00002000;

		const MEM_DECOMMIT: DWORD = 0x00004000;
		const MEM_RELEASE: DWORD = 0x00008000;

		const PAGE_READ_WRITE: DWORD = 0x04;
		const PAGE_NO_ACCESS: DWORD = 0x01;

		pub static PAGE_SIZE: std::sync::LazyLock<u32> = std::sync::LazyLock::new(|| PageAllocator::page_size());

		impl PageAllocator {
			pub fn page_size () -> u32 {
				#[repr(C)]
				struct DUMMYSTRUCTNAME {
					ProcessorArchitecture: WORD,
					Reserved: WORD,
				}
				#[repr(C)]
				struct SYSTEM_INFO {
					dummy: DUMMYSTRUCTNAME,
					dwPageSize: DWORD,
					lpMinimumApplicationAddress: LPVOID,
					lpMaximumApplicationAddress: LPVOID,
					dwActiveProcessorMask: DWORD_PTR,
					dwNumberOfProcessors: DWORD,
					dwProcessorType: DWORD,
					dwAllocationGranularity: DWORD,
					wProcessorLevel: WORD,
					wProcessorRevision: WORD,
				}
				unsafe extern "system" {
					fn GetSystemInfo(SystemInfo: *mut SYSTEM_INFO);
				}
				let mut sys_info = std::mem::MaybeUninit::uninit();
				unsafe {
					GetSystemInfo(sys_info.as_mut_ptr());
				}
				unsafe {
					sys_info.assume_init().dwPageSize
				}
			}
			pub fn alloc_pages(num_pages: usize) -> Result<NonNull<[u8]>, AllocError> {
				let ptr = unsafe {
					VirtualAllocEx(
						GetCurrentProcess(),
						std::ptr::null_mut(),
						num_pages * (*PAGE_SIZE as usize),
						MEM_COMMIT | MEM_RESERVE,
						PAGE_READ_WRITE,
					)
				};
				// TODO: replace with NonNull::new().ok_or();
				if ptr.is_null() {
					Err(AllocError)
				} else {
					unsafe {
						Ok(NonNull::new_unchecked(std::ptr::slice_from_raw_parts_mut(ptr.cast(), num_pages * (*PAGE_SIZE as usize))))
					}
				}
			}
			pub fn reserve_pages(num_pages: usize) -> Result<NonNull<[u8]>, AllocError> {
				let ptr = unsafe {
					VirtualAllocEx(
						GetCurrentProcess(),
						std::ptr::null_mut(),
						num_pages * (*PAGE_SIZE as usize),
						MEM_RESERVE,
						PAGE_NO_ACCESS,
					)
				};
				if ptr.is_null() {
					Err(AllocError)
				} else {
					unsafe {
						Ok(NonNull::new_unchecked(std::ptr::slice_from_raw_parts_mut(ptr.cast(), num_pages * (*PAGE_SIZE as usize))))
					}
				}
			}
			pub unsafe fn commit_pages(start_ptr: NonNull<u8>, num_pages: usize) -> Result<(), AllocError> {
				let ptr = unsafe {
					VirtualAllocEx(
						GetCurrentProcess(),
						start_ptr.as_ptr().cast(),
						num_pages * (*PAGE_SIZE as usize),
						MEM_COMMIT,
						PAGE_READ_WRITE,
					)
				};
				if ptr.is_null() {
					Err(AllocError)
				} else {
					Ok(())
				}
			}
			pub unsafe fn free_pages(start_ptr: NonNull<u8>, _num_pages: usize) -> Result<(), AllocError>{
				if unsafe {
					VirtualFreeEx (
						GetCurrentProcess(),
						start_ptr.as_ptr().cast(),
						0,
						MEM_RELEASE,
					)
				} == 0 {
					Err(AllocError)
				} else {
					Ok(())
				}
			}
			pub unsafe fn decommit_pages(start_ptr: NonNull<u8>, num_pages: usize) -> Result<(), AllocError>{
				if unsafe {
					VirtualFreeEx (
						GetCurrentProcess(),
						start_ptr.as_ptr().cast(),
						(num_pages as u32) * *PAGE_SIZE,
						MEM_DECOMMIT,
					)
				} == 0 {
					Err(AllocError)
				} else {
					Ok(())
				}
			}
		}

		#[cfg(test)]
		mod tests {
			use super::*;

			#[test]
			fn page_alloc_test() {
				unsafe {
					let ptr_1 = PageAllocator::alloc_pages(2)
						.expect("Allocating Pages Failed");
					PageAllocator::decommit_pages(ptr_1.cast(), 2)
						.expect("Decommitting Pages Failed");
					PageAllocator::free_pages(ptr_1.cast(), 2)
						.expect("Freeing Pages Failed");

					let ptr_2 = PageAllocator::reserve_pages(2)
						.expect("Reserving pages failed");
					PageAllocator::free_pages(ptr_2.cast(), 2)
						.expect("Freeing Pages Failed");
					PageAllocator::commit_pages(ptr_2.cast(), 2)
						.expect_err("");
				}
			}
		}
	}
	#[cfg(all(not(miri), windows))]
	pub use windows::*;

	// use normal allocator with miri and as a fallback
	#[cfg(any(miri, not(windows)))]
	pub mod otherwise {
		use std::ptr::NonNull;
		use std::alloc::Layout;

		use allocator_api2::alloc::AllocError;

		pub struct PageAllocator;

		pub static PAGE_SIZE: std::sync::LazyLock<u32> = std::sync::LazyLock::new(|| PageAllocator::page_size());

		impl PageAllocator {
			pub const fn page_size () -> u32 {
				4096
			}
			pub fn alloc_pages(num_pages: usize) -> Result<NonNull<[u8]>, AllocError> {
				if num_pages == 0 {
					unsafe{return Ok(NonNull::new_unchecked(std::ptr::slice_from_raw_parts_mut(NonNull::dangling().as_ptr(), 0)))};
				}
				let layout = Layout::from_size_align(num_pages * Self::page_size() as usize, 4096).map_err(|_| AllocError)?;
				let ptr = unsafe{std::alloc::alloc(layout)};
				let ptr = std::ptr::slice_from_raw_parts_mut(ptr, num_pages * Self::page_size() as usize);
				NonNull::new(ptr).ok_or(AllocError)
			}
			pub unsafe fn free_pages(start_ptr: NonNull<u8>, num_pages: usize) -> Result<(), AllocError>{
				if num_pages == 0 {
					return Ok(());
				}
				let layout = Layout::from_size_align(num_pages * Self::page_size() as usize, 4096).map_err(|_| AllocError)?;
				unsafe{std::alloc::dealloc(start_ptr.as_ptr(), layout)};
				Ok(())
			}
		}

		#[cfg(test)]
		mod tests {
			use super::*;

			#[test]
			fn page_alloc_test() {
				unsafe {
					let ptr_1 = PageAllocator::alloc_pages(2)
						.expect("Allocating Pages Failed");
					PageAllocator::free_pages(ptr_1.cast(), 2)
						.expect("Freeing Pages Failed");
				}
			}
		}
	}
	#[cfg(any(miri, not(windows)))]
	pub use otherwise::*;
}

mod arena {
	use std::alloc::Layout;
	use std::ptr::NonNull;
	use std::cell::Cell;
	use super::page_alloc::{PageAllocator, PAGE_SIZE};

	use allocator_api2::alloc::{Allocator, AllocError};

	pub struct Arena {
		// current points to the block where the next allocation will be attempted
		current: Cell<*mut ArenaHeader>,
	}

	struct ArenaHeader {
		// start is stored implicitly
		/* start  : *mut u8, */
		current: *mut u8,
		end    : *mut u8,
		prev   : *mut ArenaHeader,
	}

	impl ArenaHeader {
		/// SAFETY: location must point to the start of a block allocated from PageAllocator::alloc_pages
		/// SAFETY: size_bytes is the total size of the allocation created in bytes
		/// prev may be null if there is no previous
		unsafe fn write_into(location: *mut Self, prev: *mut Self, size_bytes: usize) {
			unsafe {
				let current = location.cast::<u8>().add(std::mem::size_of::<Self>());
				let end = location.cast::<u8>().add(size_bytes);
				*location = Self {
					current,
					end,
					prev,
				}
			}
		}

		fn alloc(&mut self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			let align_offset = self.current.align_offset(layout.align());
			let space_required = align_offset + layout.size();

			if space_required > self.remaining_space() {
				Err(AllocError)
			} else {
				let ret_val = unsafe {NonNull::new_unchecked(
					std::ptr::slice_from_raw_parts_mut(
						self.current.add(align_offset),
						layout.size(),
					)
				)};
				self.current = unsafe{self.current.add(space_required)};
				Ok(ret_val)
			}
		}

		// Returns a pointer with the same address as self but with provenance over the entire block
		fn start(&self) -> *mut u8 {
			self.current.with_addr((self as *const Self).addr() + std::mem::size_of::<Self>())
		}

		fn remaining_space(&self) -> usize {
			// SAFETY: end is always >= current
			unsafe {
				self.end.cast_const().offset_from_unsigned(self.current.cast_const())
			}
		}

		#[allow(unused)]
		fn total_space(&self) -> usize {
			// SAFETY: end is always >= start
			unsafe {
				self.end.offset_from_unsigned(self.start())
			}
		}

		// number of pages taken by the current block
		fn cur_block_size(&self) -> usize {
			let st = self.current.with_addr((self as *const Self).addr());
			(unsafe {
				self.end.offset_from_unsigned(st)
			}) / (*PAGE_SIZE as usize)
		}

		/// SAFETY: All pointers into this block are invalidated after this call
		/// This function cannot even take &mut self because self is allocated
		/// into the memory which is freed here
		/// ptr must point to the start of a block allocated from PageAllocator::alloc_pages
		unsafe fn free(ptr: *mut Self) {
			// SAFETY: precondition states that ptr must be valid to pass into
			// free_pages which means it must be non-null
			let result = unsafe {
				PageAllocator::free_pages(NonNull::new_unchecked(ptr.cast()), (&*ptr).cur_block_size()).is_ok()
			};
			debug_assert!(result);
		}
	}

	impl Arena {
		pub fn new () -> Self {
			Self {
				current: Cell::new(std::ptr::null_mut()),
			}
		}

		fn alloc_new_block(&self, min_size_bytes: usize) -> &mut ArenaHeader {
			// at least 1 page is allocated
			let page_size = *PAGE_SIZE as usize;
			let mut num_pages = if self.current.get().is_null() {1} else {
				unsafe { (&*self.current.get()).cur_block_size() * 2}
			};
			while num_pages * page_size < min_size_bytes {
				num_pages *= 2;
			}
			let block = PageAllocator::alloc_pages(num_pages)
				.expect("Could not allocate pages");
			debug_assert!(ptr_is_aligned_to(block.as_ptr() as *mut u8, 4096));
			
			// SAFETY: Block was just successfully allocated and the start of a
			// block is where an ArenaHeader should be written to 
			unsafe {
				ArenaHeader::write_into(
					block.as_ptr() as *mut ArenaHeader, 
					self.current.get(), 
					block.len(),
				);
			}
			self.current.set(block.as_ptr().cast());
			// SAFETY: Just properly allocated and wrote to self.current
			unsafe {
				&mut *(self.current.get().cast())
			}
		}

		pub fn alloc(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			let current = match self.current_block() {
				None => {
					self.alloc_new_block(layout.size() * 2)
				}
				Some(x) => x,
			};
			Ok(match current.alloc(layout) {
				Err(_) => {
					let current = self.alloc_new_block(layout.size() * 2);
					current.alloc(layout)
						.expect("Just allocated enough space to fit layout")
				}
				Ok(x) => x,
			})
		}

		pub fn realloc(&self, old_ptr: *mut u8, old_layout: Layout, new_layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			let ptr = self.alloc(new_layout)?;
			// ptr from self.alloc is valid to write to for length new_layout.size()
			// old_ptr is valid to read from for length old_layout.size()
			if !old_ptr.is_null() { unsafe {
				old_ptr.copy_from_nonoverlapping(ptr.as_ptr().cast(), std::cmp::min(old_layout.size(), new_layout.size()));
			} }
			Ok(ptr)
		}

		pub fn alloc_zeroed(&mut self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			let ptr = self.alloc(layout)?;
			unsafe{ (ptr.as_ptr() as *mut u8).write_bytes(0, layout.size()) };
			Ok(ptr)
		}

		pub fn realloc_zeroed(&mut self, old_ptr: *mut u8, old_layout: Layout, new_layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			let ptr = self.alloc_zeroed(new_layout)?;
			unsafe{ (ptr.as_ptr() as *mut u8).write_bytes(0, new_layout.size()) };
			unsafe {
				old_ptr.copy_from_nonoverlapping(ptr.as_ptr().cast(), std::cmp::min(old_layout.size(), new_layout.size()));
			}
			Ok(ptr)
		}

		fn current_block(&self) -> Option<&mut ArenaHeader> {
			// SAFETY: self.current is always written to before being assigned 
			unsafe {
				self.current.get().as_mut()
			}
		}

		/// Resets the memory allocated into this arena
		/// Does not free all memory requested from OS, the largest block will still be held.
		///
		/// use `Self::free` to free all held memory
		/// SAFETY: All pointers into this arena will be invalidated
		pub fn clear(&mut self) {
			if let Some(first_block) = self.current_block() {
				// SAFETY: dereferencing self.current is safe because if it is non_null, it is initialized
				let mut current = first_block.prev;
				first_block.prev = std::ptr::null_mut();
				first_block.current = first_block.start();

				while !current.is_null() {
					// SAFETY: dereferencing current is safe because if it is non-null, it is initialized
					let prev = unsafe {(*current).prev};
					// SAFETY: precondition - all pointer are invalidated
					// SAFETY: current is non-null so it is the start of a
					// block recieved from PageAllocator::alloc_pages
					unsafe { ArenaHeader::free(current) };
					current = prev;
				}
			}
		}

		/// Deallocates all memory held by this arena
		/// SAFETY: Same as `Self::clear` 
		pub fn free(self) {
			// SAFETY: dereferencing self.current is safe because if it is non_null, it is initialized
			let mut current = self.current.get();
			self.current.set(std::ptr::null_mut());
			while !current.is_null() {
				// SAFETY: dereferencing current is safe because if it is non-null, it is initialized
				let prev = unsafe {(*current).prev};
				// SAFETY: precondition - all pointer are invalidated
				// SAFETY: current is non-null so it is the start of a
				// block recieved from PageAllocator::alloc_pages
				unsafe { ArenaHeader::free(current) };
				current = prev;
			}
		}
	}

	unsafe impl Allocator for Arena {
		fn allocate (&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
			self.alloc(layout)
		}
		unsafe fn deallocate (&self, _ptr: NonNull<u8>, _layout: Layout) {}
		// unsafe fn realloc (&mut self, old_ptr: *mut u8, old_layout: Layout, new_layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
		// 	self.realloc(old_ptr, old_layout, new_layout)
		// }
	}

	fn ptr_is_aligned_to<T>(ptr: *mut T, align: usize) -> bool {
		ptr.addr() % align == 0
	}

	#[cfg(test)]
	mod test {
		use super::*;
		#[test]
		fn arena_test () {
			let x = Arena::new();
			assert!(x.current.get() == std::ptr::null_mut());
			x.free(); 

			let y = Arena::new();
			y.alloc(Layout::new::<[usize;25]>()).unwrap();
			assert_eq!(
				y.current_block()
					.unwrap()
					.total_space(),
				(*PAGE_SIZE as usize) - std::mem::size_of::<ArenaHeader>()
			);
			
			y.alloc(Layout::from_size_align(4096, 1).unwrap()).unwrap();
			assert_eq!(
				y.current_block()
					.unwrap()
					.total_space(),
				(*PAGE_SIZE as usize) * 2 - std::mem::size_of::<ArenaHeader>()
			);

			y.alloc(Layout::from_size_align(4096, 1).unwrap()).unwrap();
			assert_eq!(
				y.current_block()
					.unwrap()
					.total_space(),
				(*PAGE_SIZE as usize) * 4 - std::mem::size_of::<ArenaHeader>()
			);
			
			y.free();
		}
	}
}

pub use page_alloc::PageAllocator;
pub use arena::Arena;
