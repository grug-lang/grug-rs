use std::ptr::NonNull;

use crate::pal::{page_alloc, page_protect, page_free, page_size, Protection, Disposition};

pub trait Instruction {
	fn encode (&self, code: &mut Code);
}

pub struct Code {
	memory: NonNull<u8>,
	code_pages: usize,
	data_pages: usize,
	code_current: usize,
	data_current: usize,
}

impl Drop for Code {
	fn drop (&mut self) {
		unsafe{Self::dealloc(self.memory, self.data_pages, self.code_pages)};
	}
}

impl Code {
	pub const fn new() -> Self {
		Self {
			memory: NonNull::dangling(),
			code_pages: 0,
			data_pages: 0,
			code_current: 0,
			data_current: 0,
		}
	}

	pub unsafe fn get_fn_ptr_at<T: Copy>(&mut self, offset: usize) -> T {
		*unsafe {std::mem::transmute::<&NonNull<u8>, &T>(&self.memory.add(self.data_pages * page_size() + offset))}
	}

	pub fn push_ins<Ins: Instruction>(&mut self, ins: Ins) -> usize {
		let start = self.code_current;
		ins.encode(self);
		return self.code_current - start;
	}

	pub fn new_alloc(data_pages: usize, code_pages: usize) -> NonNull<u8> {
		let page_size = page_size();
		let new_memory = unsafe{page_alloc(None, data_pages + code_pages, Disposition::Commit, Protection::NO_ACCESS).unwrap().cast::<u8>()};
		if data_pages != 0 {unsafe{page_protect(new_memory, data_pages, Protection::READ | Protection::WRITE).unwrap()};};
		if code_pages != 0 {unsafe{page_protect(new_memory.add(data_pages * page_size), code_pages, Protection::READ | Protection::WRITE | Protection::EXEC).unwrap()};};
		new_memory
	}

	pub unsafe fn copy_data_to(&self, new_memory: NonNull<u8>, new_data_pages: usize, new_code_pages: usize) {
		let page_size = page_size();
		debug_assert!(self.data_pages * page_size >= self.data_current);
		let start_offset = self.data_pages * page_size - self.data_current;
		let end_offset = self.data_pages * page_size + self.code_current;
		
		debug_assert!(new_data_pages * page_size >= self.data_current);
		let len = end_offset - start_offset;
		let new_start_offset = new_data_pages * page_size - self.data_current;

		debug_assert!(self.code_current <= new_code_pages * page_size);
		unsafe{new_memory.add(new_start_offset).copy_from_nonoverlapping(self.memory.add(start_offset), len)};
	}

	pub unsafe fn dealloc(old_memory: NonNull<u8>, data_pages: usize, code_pages: usize) {
		if data_pages + code_pages != 0 {
			unsafe{page_free(old_memory, data_pages + code_pages, Disposition::Release).unwrap()};
		}
	}

	pub fn grow_code(&mut self) {
		let code_pages = if self.code_pages == 0 {1} else {self.code_pages * 2};

		// allocate new memory
		let new_memory = Self::new_alloc(self.data_pages, code_pages);

		// copy data over
		unsafe{self.copy_data_to(new_memory, self.data_pages, code_pages)};
		
		// Deallocate old memory
		let old_memory = self.memory;
		let old_code_pages = self.code_pages;

		self.memory = new_memory;
		self.code_pages = code_pages;

		unsafe{Self::dealloc(old_memory, self.data_pages, old_code_pages)};
	}

	pub fn grow_data(&mut self) {
		let data_pages = if self.data_pages == 0 {1} else {self.data_pages * 2};

		// allocate new memory
		let new_memory = Self::new_alloc(data_pages, self.code_pages);

		// copy data over
		unsafe{self.copy_data_to(new_memory, data_pages, self.code_pages)};
		
		// Deallocate old memory
		let old_memory = self.memory;
		let old_data_pages = self.data_pages;

		self.memory = new_memory;
		self.data_pages = data_pages;

		unsafe{Self::dealloc(old_memory, old_data_pages, self.code_pages)};
	}

	pub fn insert_data(&mut self, data: &[u8], align: usize) -> isize {
		assert!(align.count_ones() == 1);
		let page_size = page_size();
		
		let aligned_offset = ((self.data_current + data.len() - 1) & (!(align - 1))) + 1;
		while self.code_pages * page_size < aligned_offset {
			self.grow_data()
		}
		unsafe{self.memory
			.add(self.data_pages * page_size - aligned_offset)
			.as_ptr()
			.copy_from_nonoverlapping(data.as_ptr(), data.len())
		};
		self.data_current = aligned_offset;
		return -(aligned_offset as isize);
	}

	pub fn insert_code(&mut self, data: &[u8]) {
		let page_size = page_size();
		
		while self.code_pages * page_size < self.code_current + data.len() {
			self.grow_code()
		}
		unsafe{self.memory
			.add(self.data_pages * page_size + self.code_current)
			.as_ptr()
			.copy_from_nonoverlapping(data.as_ptr(), data.len())
		};
		self.code_current += data.len();
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::asm::*;
	#[test]
	fn return_const() {
		let mut code = Code::new();
		code.push_ins(Mov::ImmReg(ImmReg::Imm16{dst: Reg16::Ax, data: 392}));
		code.push_ins(Ret);
		let value = unsafe{code.get_fn_ptr_at::<extern "C" fn () -> u16>(0)}();
		assert_eq!(value, 392);
	}

	#[test]
	fn return_input() {
		let mut code = Code::new();
		code.push_ins(Mov::RegReg(RegReg::R64{src: Reg64::Rcx, dst: Reg64::Rax}));
		code.push_ins(Ret);
		let value = unsafe{code.get_fn_ptr_at::<extern "system" fn (u64) -> u64>(0)}(201);
		assert_eq!(value, 201);
	}

	#[test]
	fn push_pop() {
		let mut code = Code::new();
		code.push_ins(Push::Imm32s(172));
		code.push_ins(Mov::RegMem(RegMem{swap: true, reg: Reg::Reg64(Reg64::Rcx), mem: Memory::Sib{ scale: Scale::S1, index: Index64::NoIndex, base: Base64::Rsp, disp: 0 }}));
		code.push_ins(Pop::Reg64(Reg64::Rax));
		code.push_ins(Ret);
		let value = unsafe{code.get_fn_ptr_at::<extern "system" fn () -> u64>(0)}();
		assert_eq!(value, 172);
	}

	#[test]
	fn push_pop_2() {
		let mut code = Code::new();
		code.push_ins(Push::Imm32s(172));
		code.push_ins(Mov::RegMem(RegMem{swap: true, reg: Reg::Reg64(Reg64::Rcx), mem: Memory::Sib{ scale: Scale::S1, index: Index64::NoIndex, base: Base64::Rsp, disp: 0 }}));
		code.push_ins(Pop::Reg64(Reg64::Rax));
		code.push_ins(Add(BinOp::ImmReg(ImmReg::Imm32s{dst: Reg64::Rcx, data: -32})));
		code.push_ins(Add(BinOp::RegReg(RegReg::R64{src: Reg64::Rcx, dst: Reg64::Rax})));
		code.push_ins(Ret);
		let value = unsafe{code.get_fn_ptr_at::<extern "system" fn () -> u64>(0)}();
		assert_eq!(value, 172 * 2 - 32);
	}

// 	#[test]
// 	fn add_1() {
// 		let mut code = Code::new();
// 		code.push_ins(Add::ModRm(ModRm::RegReg(RegReg::R64{src: Reg64::Rcx, dst: Reg64::Rdx})));
// 		code.push_ins(Mov::ModRm(ModRm::RegReg(RegReg::R64{src: Reg64::Rdx, dst: Reg64::Rax})));
// 		code.push_ins(Ins::Ret);
// 		let value = unsafe{code.get_fn_ptr_at::<extern "system" fn (u64, u64) -> u64>(0)}(201, 500);
// 		assert_eq!(value, 201 + 500);
// 	}
}
