use std::ptr::NonNull;
use std::mem::MaybeUninit;
use std::arch::asm;

use crate::pal::{page_alloc, page_free, Protection, Disposition};

#[allow(dead_code)]
pub struct Stack {
	memory: NonNull<u8>,
	normal_size: usize,
	guard_size: usize,
}

impl Stack {
	pub fn new() -> Self {
		let normal_size = 1024 * 1024;
		let guard_size = normal_size / 2;
		let pages = unsafe{page_alloc(None, normal_size + guard_size, Disposition::Reserve, Protection::READ | Protection::WRITE).unwrap()};
		unsafe{page_alloc(Some(pages.cast()), guard_size, Disposition::Commit, Protection::READ | Protection::WRITE | Protection::GUARD).unwrap()};
		unsafe{page_alloc(Some(pages.cast().byte_add(guard_size)), normal_size, Disposition::Commit, Protection::READ | Protection::WRITE).unwrap()};
		Self {
			memory: pages.cast(),
			normal_size,
			guard_size,
		}
	}

	#[cfg(all(target_arch="x86_64", target_os="windows"))]
	pub fn run_in<F: FnOnce() -> O, O>(&mut self, f: F) -> O {
		let f = MaybeUninit::new(f);
		let o = MaybeUninit::uninit();
		let stack_begin = unsafe{self.memory.as_ptr().add(self.normal_size - 8)};
		extern "C" fn wrapper<F: FnOnce() -> O, O>(input: *mut F, out: *mut O) {
			unsafe{out.write((input.read())())}
		}

		unsafe {asm!{
			"mov [{stack_begin}], rsp",
			"mov rsp, {stack_begin}",
			"call {}",
			"pop rsp",
			sym wrapper::<F, O>,
			stack_begin = in(reg) stack_begin,
			in("rcx") f.as_ptr(),
			in("rdx") o.as_ptr(),
			clobber_abi("C")
		}}
		unsafe{o.assume_init()}
	}
}

impl Default for Stack {
	fn default() -> Self {Self::new()}
}

impl Drop for Stack {
	fn drop(&mut self) {
		unsafe{page_free(self.memory, self.normal_size + self.guard_size, Disposition::Release).unwrap()};
	}
}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn basic_test() {
		let mut stack = Stack::new();
		let mut x = 0;
		stack.run_in(|| x = 1);
		assert_eq!(x, 1);
	}
}
