use crate::code::{Code, Instruction};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Int3;
pub struct Ret;

impl Instruction for Int3 {
	fn encode (&self, code: &mut Code) {
		code.insert_code(&[0xcc]);
	}
}

impl Instruction for Ret {
	fn encode (&self, code: &mut Code) {
		code.insert_code(&[0xc3]);
	}
}

impl Instruction for Mov {
	fn encode (&self, code: &mut Code) {
		const MOV_IMM_TO_REG_VDS: u8 = 0xc6;
		const MOV_IMM_TO_REG_VQP: u8 = 0xb0;
		const MOV_MOD_RM: u8 = 0x88;
		match self {
			Mov::ImmMem(imm_mem) => {
				if imm_mem.reg_size() == 2 { code.insert_code(&[Prefix::OPERAND_SIZE]) }
				(imm_mem.imm.rex() | imm_mem.memory.rex()).insert_into(code);
				let opcode = MOV_IMM_TO_REG_VDS | if imm_mem.reg_size() != 1 { OpField::W } else {0};
				code.insert_code(&[opcode]);
				imm_mem.memory.encode(0, code);
			}
			Mov::ImmReg(imm) => {
				imm.encode_rex(Rex::R).insert_into(code);
				let reg = imm.encode_reg();
				match imm {
					ImmReg::Imm8{dst, data} => {
						let opcode = MOV_IMM_TO_REG_VQP | reg;
						code.insert_code(&[opcode, *data as u8]);
					}
					ImmReg::Imm16{dst, data} => {
						let opcode = MOV_IMM_TO_REG_VQP | reg | 0x08;
						code.insert_code(&[Prefix::OPERAND_SIZE, opcode]);
						code.insert_code(&(*data as i16).to_ne_bytes());
					}
					ImmReg::Imm32{dst, data} => {
						let opcode = MOV_IMM_TO_REG_VQP | reg | 0x08;
						code.insert_code(&[opcode]);
						code.insert_code(&data.to_ne_bytes());
					}
					ImmReg::Imm32s{dst, data} => {
						let opcode = MOV_IMM_TO_REG_VDS | OpField::W;
						code.insert_code(&[opcode, 0b11000000 | reg]);
						code.insert_code(&data.to_ne_bytes());
					}
				}
			}
			Mov::Imm64(Imm64{dst, data}) => {
				let rex = Rex::W | dst.encode_rex(Rex::R);
				let reg = dst.encode_reg();

				let opcode = MOV_IMM_TO_REG_VQP | reg | 0x08;
				rex.insert_into(code);
				code.insert_code(&[opcode]);
				code.insert_code(&data.to_ne_bytes());
			}
			Mov::RegReg(reg_reg) => {
				if reg_reg.reg_size() == 2 {
					code.insert_code(&[Prefix::OPERAND_SIZE]);
				}
				reg_reg.rex().insert_into(code);
				let opcode = MOV_MOD_RM | 
					if reg_reg.reg_size() != 1 {OpField::W} else {0};
				code.insert_code(&[opcode]);
				reg_reg.encode(code);
			}
			Mov::RegMem(reg_mem) => {
				if reg_mem.reg_size() == 2 {
					code.insert_code(&[Prefix::OPERAND_SIZE]);
				}
				reg_mem.rex().insert_into(code);
				let opcode = MOV_MOD_RM | 
					if reg_mem.reg_size() != 1 {OpField::W} else {0} |
					if reg_mem.swap {OpField::D} else {0};
				code.insert_code(&[opcode]);
				reg_mem.encode(code);
			}
		}
	}
}

impl Instruction for Push {
	fn encode (&self, code: &mut Code) {
		const PUSH_IMM: u8 = 0x68;
		const PUSH_REG: u8 = 0x50;
		const PUSH_MEM: u8 = 0xFF;
		match *self {
			Self::Imm32s(val) => {
				Rex::W.insert_into(code);
				if i8::MIN as i32 <= val && i8::MAX as i32 >= val {
					code.insert_code(&[PUSH_IMM | OpField::S]);
					code.insert_code(&(val as i8).to_ne_bytes());
				} else {
					code.insert_code(&[PUSH_IMM]);
					code.insert_code(&val.to_ne_bytes());
				}
			},
			Self::Imm32(val) => {
				if i8::MIN as i32 <= val && i8::MAX as i32 >= val {
					code.insert_code(&[PUSH_IMM | OpField::S]);
					code.insert_code(&(val as i8).to_ne_bytes());
				} else {
					code.insert_code(&[PUSH_IMM]);
					code.insert_code(&val.to_ne_bytes());
				}
			},
			Self::Memory32(mem) => { 
				let rex = mem.rex();
				rex.insert_into(code);
				code.insert_code(&[PUSH_MEM]);
				mem.encode(6, code);
			},
			Self::Memory64(mem) => { 
				let rex = mem.rex() | Rex::W;
				rex.insert_into(code);
				code.insert_code(&[PUSH_MEM]);
				mem.encode(6, code);
			},
			Self::Reg32(reg) => { 
				code.insert_code(&[PUSH_REG | reg.encode_reg()]);
			},
			Self::Reg64(reg) => { 
				Rex::W.insert_into(code);
				code.insert_code(&[PUSH_REG | reg.encode_reg()]);
			},
		}
	}
}

impl Instruction for Pop {
	fn encode (&self, code: &mut Code) {
		const POP_REG: u8 = 0x58;
		const POP_MEM: u8 = 0x8F | OpField::W;
		match *self {
			Self::Memory32(mem) => { 
				let rex = mem.rex();
				rex.insert_into(code);
				code.insert_code(&[POP_MEM]);
				mem.encode(0, code);
			},
			Self::Memory64(mem) => { 
				let rex = mem.rex() | Rex::W;
				rex.insert_into(code);
				code.insert_code(&[POP_MEM]);
				mem.encode(0, code);
			},
			Self::Reg32(reg) => { 
				code.insert_code(&[POP_REG | reg.encode_reg()]);
			},
			Self::Reg64(reg) => { 
				Rex::W.insert_into(code);
				code.insert_code(&[POP_REG | reg.encode_reg()]);
			},
		}
	}
}

fn encode_binop(sub_op: u8, operands: BinOp, code: &mut Code) {
	const BASE_IMM_OPCODE: u8 = 0x80;

	match operands {
		BinOp::ImmReg(imm_reg) => {
			imm_reg.encode_rex(Rex::B).insert_into(code);
			if imm_reg.reg_size() == 2 { code.insert_code(&[Prefix::OPERAND_SIZE]) };
			let opcode = BASE_IMM_OPCODE | if imm_reg.reg_size() != 1 {OpField::W} else {0};
			code.insert_code(&[opcode]);
			imm_reg.encode_mod_rm(sub_op, code);
		},
		BinOp::ImmMem(imm_mem) => {
			if imm_mem.reg_size() == 2 { code.insert_code(&[Prefix::OPERAND_SIZE]) }
			(imm_mem.imm.rex() | imm_mem.memory.rex()).insert_into(code);
			let opcode = BASE_IMM_OPCODE | if imm_mem.reg_size() != 1 { OpField::W } else {0};
			code.insert_code(&[opcode]);
			imm_mem.memory.encode(sub_op, code);
		}
		BinOp::RegReg(reg_reg) => {
			reg_reg.rex().insert_into(code);
			if reg_reg.reg_size() == 2 { code.insert_code(&[Prefix::OPERAND_SIZE]) };
			let opcode = sub_op | if reg_reg.reg_size() != 1 {OpField::W} else {0};
			code.insert_code(&[opcode]);
			reg_reg.encode(code);
		}
		BinOp::RegMem(reg_mem) => {
			if reg_mem.reg_size() == 2 {
				code.insert_code(&[Prefix::OPERAND_SIZE]);
			}
			reg_mem.rex().insert_into(code);
			let opcode = sub_op | 
				if reg_mem.reg_size() != 1 {OpField::W} else {0} |
				if reg_mem.swap {OpField::D} else {0};
			code.insert_code(&[opcode]);
			reg_mem.encode(code);
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mov {
	ImmReg(ImmReg),
	ImmMem(ImmMem),
	Imm64(Imm64),
	RegReg(RegReg),
	RegMem(RegMem),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Push {
	Imm32s(i32),
	Imm32(i32),
	Memory32(Memory),
	Memory64(Memory),
	Reg32(Reg32),
	Reg64(Reg64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pop {
	Memory32(Memory),
	Memory64(Memory),
	Reg32(Reg32),
	Reg64(Reg64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
	ImmReg(ImmReg),
	ImmMem(ImmMem),
	RegReg(RegReg),
	RegMem(RegMem),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Add(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Or(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Adc(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Sbb(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct And(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Sub(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Xor(pub BinOp);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Cmp(pub BinOp);

impl Instruction for Add { fn encode (&self, code: &mut Code) { encode_binop(0x00, self.0, code) } }
impl Instruction for Or  { fn encode (&self, code: &mut Code) { encode_binop(0x01, self.0, code) } }
impl Instruction for Adc { fn encode (&self, code: &mut Code) { encode_binop(0x02, self.0, code) } }
impl Instruction for Sbb { fn encode (&self, code: &mut Code) { encode_binop(0x03, self.0, code) } }
impl Instruction for And { fn encode (&self, code: &mut Code) { encode_binop(0x04, self.0, code) } }
impl Instruction for Sub { fn encode (&self, code: &mut Code) { encode_binop(0x05, self.0, code) } }
impl Instruction for Xor { fn encode (&self, code: &mut Code) { encode_binop(0x06, self.0, code) } }
impl Instruction for Cmp { fn encode (&self, code: &mut Code) { encode_binop(0x07, self.0, code) } }


pub struct Prefix;
impl Prefix {
	pub const OPERAND_SIZE: u8 = 0x66;
	pub const ADDRESS_SIZE: u8 = 0x67;
}

pub struct OpField;
impl OpField {
	pub const NONE: u8 = 0b00000000;
	pub const W:    u8 = 0b00000001;
	pub const D:    u8 = 0b00000010;
	pub const S:    u8 = 0b00000010;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Rex(u8);
impl Rex {
	const NO_REX : Self = Self(0b00000000);
	const NONE   : Self = Self(0b01000000);
	const R      : Self = Self(0b01001000);
	const W      : Self = Self(0b01000100);
	const X      : Self = Self(0b01000010);
	const B      : Self = Self(0b01000001);

	fn insert_into(self, code: &mut Code) {
		if self != Self::NO_REX {
			code.insert_code(&[self.0]);
		}
	}
}

impl std::ops::BitOr for Rex {
	type Output = Self;
	fn bitor(self, other: Self) -> Self {
		Self(self.0 | other.0)
	}
}

impl std::ops::BitOrAssign for Rex {
	fn bitor_assign(&mut self, other: Self) {
		*self = *self | other
	}
}

impl std::ops::BitAnd for Rex {
	type Output = Self;
	fn bitand(self, other: Self) -> Self {
		Self(self.0 & other.0)
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegReg {
	R8{src: Reg8, dst: Reg8},
	R16{src: Reg16, dst: Reg16},
	R32{src: Reg32, dst: Reg32},
	R64{src: Reg64, dst: Reg64},
}

impl RegReg {
	pub const fn reg_size(self) -> usize {
		match self {
			Self::R8 {..} => 1,
			Self::R16 {..} => 2,
			Self::R32 {..} => 4,
			Self::R64 {..} => 8,
		}
	}

	pub fn rex(self) -> Rex {
		match self {
			Self::R8{src, dst} => {
				src.encode_rex(Rex::B) |
				dst.encode_rex(Rex::R)
			}
			Self::R16{src, dst} => {
				src.encode_rex(Rex::B) |
				dst.encode_rex(Rex::R)
			}
			Self::R32{src, dst} => {
				src.encode_rex(Rex::B) |
				dst.encode_rex(Rex::R)
			}
			Self::R64{src, dst} => {
				src.encode_rex(Rex::B) |
				dst.encode_rex(Rex::R)
			}
		}
	}

	pub fn encode(self, code: &mut Code) {
		const MOD_RM_REG_REG: u8 = 0b11000000;
		match self {
			Self::R8{src, dst} => {
				let src = src.encode_reg();
				let dst = dst.encode_reg();
				code.insert_code(&[MOD_RM_REG_REG | src << 3 | dst]);
			}
			Self::R16{src, dst} => {
				let src = src.encode_reg();
				let dst = dst.encode_reg();
				code.insert_code(&[MOD_RM_REG_REG | src << 3 | dst]);
			}
			Self::R32{src, dst} => {
				let src = src.encode_reg();
				let dst = dst.encode_reg();
				code.insert_code(&[MOD_RM_REG_REG | src << 3 | dst]);
			}
			Self::R64{src, dst} => {
				let src = src.encode_reg();
				let dst = dst.encode_reg();
				code.insert_code(&[MOD_RM_REG_REG | src << 3 | dst]);
			}
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Memory {
	Sib {
		scale: Scale,
		index: Index64, 
		base: Base64,
		disp: i32,
	},
	RipRel {
		disp: i32
	},
}

impl Memory {
	fn rex(self) -> Rex {
		match self {
			Self::Sib {scale: _, index, base, disp: _} => {
				base.encode_rex(Rex::B) |
				index.encode_rex(Rex::X)
			}
			Self::RipRel{..} => Rex::NO_REX
		}
	}

	pub fn encode(self, reg: u8, code: &mut Code) {
		const MOD_RM_DISP32 : u8 = 0b10000000;
		const MOD_RM_DISP8  : u8 = 0b01000000;
		const MOD_RM_NODISP : u8 = 0b00000000;
		const MOD_RM_RIPREL : u8 = 0b00000000;
		match self {
			Self::Sib { scale, index, base, disp } => {
				// mod_rm byte only
				if index == Index64::NoIndex {
					let rsp = Reg64::Rsp.encode_reg();
					let src = base.encode_reg();
					let dst = reg;
					let no_idx = Index64::NoIndex.encode_reg();

					// displacement only can only be done with 
					// NODISP, SIB, IDX = rsp, base = rbp with 32 bit displacement
					if base == Base64::NoBase {
						code.insert_code(&[
							MOD_RM_NODISP | dst << 3 | rsp, 
							Scale::S1 as u8 | no_idx << 3 | src
						]);
						code.insert_code(&disp.to_ne_bytes());
					// 32 bit displacement
					} else if disp < i8::MIN as i32 || disp > i8::MAX as i32 {
						code.insert_code(&[MOD_RM_DISP32 | dst << 3 | src]);
						// base == Rsp or R12 actually does SIB, so we have to
						// do the equivalent in SIB
						if base == Base64::Rsp || base == Base64::R12 {
							code.insert_code(&[Scale::S1 as u8 | no_idx << 3 | src]);
						}
						code.insert_code(&disp.to_ne_bytes());
					// 8 bit displacement
					} else if disp != 0 {
						code.insert_code(&[MOD_RM_DISP8 | dst << 3 | src]);
						// base == Rsp or R12 actually does SIB, so we have to
						// do the equivalent in SIB
						if base == Base64::Rsp || base == Base64::R12 {
							code.insert_code(&[Scale::S1 as u8 | no_idx << 3 | src]);
						}
						code.insert_code(&(disp as i8).to_ne_bytes());
					// actual 0 displacement
					} else {
						if base == Base64::Rbp || base == Base64::R13 {
							code.insert_code(&[MOD_RM_DISP8 | dst << 3 | src, 0]);
						} else {
							code.insert_code(&[MOD_RM_NODISP | dst << 3 | src]);
							// base == Rsp or R12 actually does SIB, so we have to
							// do the equivalent in SIB
							if base == Base64::Rsp || base == Base64::R12 {
								code.insert_code(&[Scale::S1 as u8 | no_idx << 3 | src]);
							}
						}
					}
				} else {
					let rsp = Reg64::Rsp.encode_reg();
					let dst = reg;
					let idx = index.encode_reg();
					let bas = base.encode_reg();
					
					// No base is actually done with MOD_RM_NODISP with base = rbp with a 32 bit displacement
					if base == Base64::NoBase {
						code.insert_code(&[
							MOD_RM_NODISP | dst << 3 | rsp, 
							scale as u8 | idx << 3 | bas
						]);
						code.insert_code(&disp.to_ne_bytes());
					// 32 bit displacement
					} else if disp < i8::MIN as i32 || disp > i8::MAX as i32 {
						code.insert_code(&[
							MOD_RM_DISP32 | dst << 3 | rsp, 
							scale as u8 | idx << 3 | bas
						]);
						code.insert_code(&disp.to_ne_bytes());
					// 8 bit displacement
					} else if disp != 0 {
						code.insert_code(&[
							MOD_RM_DISP8 | dst << 3 | rsp, 
							scale as u8 | idx << 3 | bas
						]);
						code.insert_code(&(disp as i8).to_ne_bytes());
					// 0 displacement
					} else {
						// 0 displacement with Rbp or R13 base is not possible with the straightforward encoding
						// we use 8 bit displacement with a displacement of 0 instead
						if base == Base64::Rbp || base == Base64::R13 {
							code.insert_code(&[
								MOD_RM_DISP8 | dst << 3 | rsp, 
								scale as u8 | idx << 3 | bas, 
								0
							]);
						} else {
							code.insert_code(&[
								MOD_RM_NODISP | dst << 3 | rsp, 
								scale as u8 | idx << 3 | bas
							]);
						}
					}
				}
			}
			Self::RipRel { disp } => {
				let dst = reg;
				code.insert_code(&[MOD_RM_RIPREL | dst << 3 | 0b101]);
				code.insert_code(&disp.to_ne_bytes());
			}
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RegMem {
	pub swap: bool,
	pub reg: Reg,
	pub mem: Memory,
}

impl RegMem {
	pub const fn reg_size(self) -> usize {
		self.reg.reg_size()
	}

	pub fn rex(self) -> Rex {
		self.reg.encode_rex(Rex::R) | 
		self.mem.rex()
	}

	pub fn encode(self, code: &mut Code) {
		self.mem.encode(self.reg.encode_reg(), code)
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum Scale {
	S1 = 0b00000000,
	S2 = 0b01000000,
	S4 = 0b10000000,
	S8 = 0b11000000
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ImmMem {
	memory: Memory,
	imm: ImmVal,
}

impl ImmMem {
	pub const fn reg_size(self) -> usize {
		self.imm.reg_size()
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ImmVal {
	Imm8  (i8),
	Imm16 (i16),
	Imm32 (i32),
	Imm32s(i32),
}

impl ImmVal {
	pub const fn reg_size(self) -> usize {
		match self {
			Self::Imm8  (_) => 1,
			Self::Imm16 (_) => 2,
			Self::Imm32 (_) => 4,
			Self::Imm32s(_) => 8,
		}
	}

	pub const fn rex(self) -> Rex {
		match self {
			Self::Imm8  { .. } => Rex::NO_REX,
			Self::Imm16 { .. } => Rex::NO_REX,
			Self::Imm32 { .. } => Rex::NO_REX,
			Self::Imm32s{ .. } => Rex::W,
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ImmReg {
	Imm8 {
		dst: Reg8,
		data: i8,
	},
	Imm16 {
		dst: Reg16,
		data: i16,
	},
	Imm32 {
		dst: Reg32,
		data: i32,
	},
	Imm32s {
		dst: Reg64,
		data: i32,
	},
}

impl ImmReg {
	pub fn encode_rex(self, field: Rex) -> Rex {
		match self {
			Self::Imm8  {dst, .. } => dst.encode_rex(field),
			Self::Imm16 {dst, .. } => dst.encode_rex(field),
			Self::Imm32 {dst, .. } => dst.encode_rex(field),
			Self::Imm32s{dst, .. } => Rex::W | dst.encode_rex(field),
		}
	}

	pub fn encode_reg(self) -> u8 {
		match self {
			Self::Imm8  {dst, .. } => dst.encode_reg(),
			Self::Imm16 {dst, .. } => dst.encode_reg(),
			Self::Imm32 {dst, .. } => dst.encode_reg(),
			Self::Imm32s{dst, .. } => dst.encode_reg(),
		}
	}

	pub const fn reg_size(self) -> usize {
		match self {
			Self::Imm8{..}   => 1,
			Self::Imm16{..}  => 2,
			Self::Imm32{..}  => 4,
			Self::Imm32s{..} => 8,
		}
	}
	
	pub fn encode_mod_rm(self, reg: u8, code: &mut Code) {
		const MOD_RM_REG_REG: u8 = 0b11000000;
		match self {
			Self::Imm8  {dst, data} => {
				code.insert_code(&[MOD_RM_REG_REG | reg << 3 | dst.encode_reg(), data as u8])
			}
			Self::Imm16 {dst, data} => {
				code.insert_code(&[MOD_RM_REG_REG | reg << 3 | dst.encode_reg()]);
				code.insert_code(&data.to_ne_bytes());
			}
			Self::Imm32 {dst, data} => {
				code.insert_code(&[MOD_RM_REG_REG | reg << 3 | dst.encode_reg()]);
				code.insert_code(&data.to_ne_bytes());
			}
			Self::Imm32s{dst, data} => {
				code.insert_code(&[MOD_RM_REG_REG | reg << 3 | dst.encode_reg()]);
				code.insert_code(&data.to_ne_bytes());
			}
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Imm64 {
	pub dst: Reg64,
	pub data: i64,
}

impl Imm64 {
	pub const fn reg_size(self) -> usize {
		8
	}
}

pub use register::*;
mod register {
	use super::*;

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Reg8 {
		Al,
		Cl,
		Dl,
		Bl,
		Spl,
		Bpl,
		Sil,
		Dil,
		R8l,
		R9l,
		R10l,
		R11l,
		R12l,
		R13l,
		R14l,
		R15l,
	}

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Reg16 {
		Ax,
		Cx,
		Dx,
		Bx,
		Sp,
		Bp,
		Si,
		Di,
		R8w,
		R9w,
		R10w,
		R11w,
		R12w,
		R13w,
		R14w,
		R15w,
	}

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Reg32 {
		Eax ,
		Ecx ,
		Edx ,
		Ebx ,
		Esp ,
		Ebp ,
		Esi ,
		Edi ,
		R8d ,
		R9d ,
		R10d,
		R11d,
		R12d,
		R13d,
		R14d,
		R15d,
	}

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Reg64 {
		Rax,
		Rcx,
		Rdx,
		Rbx,
		Rsp,
		Rbp,
		Rsi,
		Rdi,
		R8 ,
		R9 ,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
	}

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Reg {
		Reg8(Reg8),
		Reg16(Reg16),
		Reg32(Reg32),
		Reg64(Reg64),
	}

	impl Reg {
		pub const fn reg_size(self) -> usize {
			match self {
				Self::Reg8(_) => 1,
				Self::Reg16(_) => 2,
				Self::Reg32(_) => 4,
				Self::Reg64(_) => 8,
			}
		}
	}

	pub trait Register: Copy {
		/// returns the encoding for the register and whether it requires the
		/// corresponding rex bit needs to be set
		fn encode_rex (self, rex_field: Rex) -> Rex;
		fn encode_reg(self) -> u8;
	}

	impl Reg8 {
		pub fn always_requires_rex(self) -> bool {
			match self {
				Self::Spl | Self::Bpl | Self::Sil | Self::Dil => true,
				_ => false,
			}
		}
	}

	impl Register for Reg {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Reg8 (reg) => reg.encode_rex(rex_field),
				Self::Reg16(reg) => reg.encode_rex(rex_field),
				Self::Reg32(reg) => reg.encode_rex(rex_field),
				Self::Reg64(reg) => reg.encode_rex(rex_field),
			}
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Reg8 (reg) => reg.encode_reg(),
				Self::Reg16(reg) => reg.encode_reg(),
				Self::Reg32(reg) => reg.encode_reg(),
				Self::Reg64(reg) => reg.encode_reg(),
			}
		}
	}

	impl Register for Reg8 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Al   => Rex::NO_REX,
				Self::Cl   => Rex::NO_REX,
				Self::Dl   => Rex::NO_REX,
				Self::Bl   => Rex::NO_REX,
				Self::Spl  => Rex::NONE,
				Self::Bpl  => Rex::NONE,
				Self::Sil  => Rex::NONE,
				Self::Dil  => Rex::NONE,
				Self::R8l  => rex_field,
				Self::R9l  => rex_field,
				Self::R10l => rex_field,
				Self::R11l => rex_field,
				Self::R12l => rex_field,
				Self::R13l => rex_field,
				Self::R14l => rex_field,
				Self::R15l => rex_field,
			}
		}

		fn encode_reg(self) -> u8 {
			match self {
				Self::Al   => 0b000,
				Self::Cl   => 0b001,
				Self::Dl   => 0b010,
				Self::Bl   => 0b011,
				Self::Spl  => 0b100,
				Self::Bpl  => 0b101,
				Self::Sil  => 0b110,
				Self::Dil  => 0b111,
				Self::R8l  => 0b000,
				Self::R9l  => 0b001,
				Self::R10l => 0b010,
				Self::R11l => 0b011,
				Self::R12l => 0b100,
				Self::R13l => 0b101,
				Self::R14l => 0b110,
				Self::R15l => 0b111,
			}
		}
	}

	impl Register for Reg16 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Ax   => Rex::NO_REX,
				Self::Cx   => Rex::NO_REX,
				Self::Dx   => Rex::NO_REX,
				Self::Bx   => Rex::NO_REX,
				Self::Sp   => Rex::NO_REX,
				Self::Bp   => Rex::NO_REX,
				Self::Si   => Rex::NO_REX,
				Self::Di   => Rex::NO_REX,
				Self::R8w  => rex_field,
				Self::R9w  => rex_field,
				Self::R10w => rex_field,
				Self::R11w => rex_field,
				Self::R12w => rex_field,
				Self::R13w => rex_field,
				Self::R14w => rex_field,
				Self::R15w => rex_field,
			}
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Ax    => 0b000,
				Self::Cx    => 0b001,
				Self::Dx    => 0b010,
				Self::Bx    => 0b011,
				Self::Sp    => 0b100,
				Self::Bp    => 0b101,
				Self::Si    => 0b110,
				Self::Di    => 0b111,
				Self::R8w   => 0b000,
				Self::R9w   => 0b001,
				Self::R10w  => 0b010,
				Self::R11w  => 0b011,
				Self::R12w  => 0b100,
				Self::R13w  => 0b101,
				Self::R14w  => 0b110,
				Self::R15w  => 0b111,
			}
		}
	}

	impl Register for Reg32 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Eax  => Rex::NO_REX,
				Self::Ecx  => Rex::NO_REX,
				Self::Edx  => Rex::NO_REX,
				Self::Ebx  => Rex::NO_REX,
				Self::Esp  => Rex::NO_REX,
				Self::Ebp  => Rex::NO_REX,
				Self::Esi  => Rex::NO_REX,
				Self::Edi  => Rex::NO_REX,
				Self::R8d  => rex_field,
				Self::R9d  => rex_field,
				Self::R10d => rex_field,
				Self::R11d => rex_field,
				Self::R12d => rex_field,
				Self::R13d => rex_field,
				Self::R14d => rex_field,
				Self::R15d => rex_field,
			}
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Eax  => 0b000,
				Self::Ecx  => 0b001,
				Self::Edx  => 0b010,
				Self::Ebx  => 0b011,
				Self::Esp  => 0b100,
				Self::Ebp  => 0b101,
				Self::Esi  => 0b110,
				Self::Edi  => 0b111,
				Self::R8d  => 0b000,
				Self::R9d  => 0b001,
				Self::R10d => 0b010,
				Self::R11d => 0b011,
				Self::R12d => 0b100,
				Self::R13d => 0b101,
				Self::R14d => 0b110,
				Self::R15d => 0b111,
			}
		}
	}

	impl Register for Reg64 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Rax => Rex::NO_REX,
				Self::Rcx => Rex::NO_REX,
				Self::Rdx => Rex::NO_REX,
				Self::Rbx => Rex::NO_REX,
				Self::Rsp => Rex::NO_REX,
				Self::Rbp => Rex::NO_REX,
				Self::Rsi => Rex::NO_REX,
				Self::Rdi => Rex::NO_REX,
				Self::R8  => rex_field,
				Self::R9  => rex_field,
				Self::R10 => rex_field,
				Self::R11 => rex_field,
				Self::R12 => rex_field,
				Self::R13 => rex_field,
				Self::R14 => rex_field,
				Self::R15 => rex_field,
			}
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Rax => 0b000,
				Self::Rcx => 0b001,
				Self::Rdx => 0b010,
				Self::Rbx => 0b011,
				Self::Rsp => 0b100,
				Self::Rbp => 0b101,
				Self::Rsi => 0b110,
				Self::Rdi => 0b111,
				Self::R8  => 0b000,
				Self::R9  => 0b001,
				Self::R10 => 0b010,
				Self::R11 => 0b011,
				Self::R12 => 0b100,
				Self::R13 => 0b101,
				Self::R14 => 0b110,
				Self::R15 => 0b111,
			}
		}
	}

	impl Register for Index64 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Rax     => Rex::NO_REX,
				Self::Rcx     => Rex::NO_REX,
				Self::Rdx     => Rex::NO_REX,
				Self::Rbx     => Rex::NO_REX,
				Self::NoIndex => Rex::NO_REX,
				Self::Rbp     => Rex::NO_REX,
				Self::Rsi     => Rex::NO_REX,
				Self::Rdi     => Rex::NO_REX,
				Self::R8      => rex_field,
				Self::R9      => rex_field,
				Self::R10     => rex_field,
				Self::R11     => rex_field,
				Self::R12     => rex_field,
				Self::R13     => rex_field,
				Self::R14     => rex_field,
				Self::R15     => rex_field,
			}                    
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Rax     => 0b000,
				Self::Rcx     => 0b001,
				Self::Rdx     => 0b010,
				Self::Rbx     => 0b011,
				Self::NoIndex => 0b100,
				Self::Rbp     => 0b101,
				Self::Rsi     => 0b110,
				Self::Rdi     => 0b111,
				Self::R8      => 0b000,
				Self::R9      => 0b001,
				Self::R10     => 0b010,
				Self::R11     => 0b011,
				Self::R12     => 0b100,
				Self::R13     => 0b101,
				Self::R14     => 0b110,
				Self::R15     => 0b111,
			}
		}
	}

	impl Register for Base64 {
		fn encode_rex(self, rex_field: Rex) -> Rex {
			match self {
				Self::Rax    => Rex::NO_REX,
				Self::Rcx    => Rex::NO_REX,
				Self::Rdx    => Rex::NO_REX,
				Self::Rbx    => Rex::NO_REX,
				Self::Rsp    => Rex::NO_REX,
				Self::NoBase => Rex::NO_REX,
				Self::Rbp    => Rex::NO_REX,
				Self::Rsi    => Rex::NO_REX,
				Self::Rdi    => Rex::NO_REX,
				Self::R8     => rex_field,
				Self::R9     => rex_field,
				Self::R10    => rex_field,
				Self::R11    => rex_field,
				Self::R12    => rex_field,
				Self::R13    => rex_field,
				Self::R14    => rex_field,
				Self::R15    => rex_field,
			}
		}
		fn encode_reg(self) -> u8 {
			match self {
				Self::Rax    => 0b000,
				Self::Rcx    => 0b001,
				Self::Rdx    => 0b010,
				Self::Rbx    => 0b011,
				Self::Rsp    => 0b100,
				Self::NoBase => 0b100,
				Self::Rbp    => 0b101,
				Self::Rsi    => 0b110,
				Self::Rdi    => 0b111,
				Self::R8     => 0b000,
				Self::R9     => 0b001,
				Self::R10    => 0b010,
				Self::R11    => 0b011,
				Self::R12    => 0b100,
				Self::R13    => 0b101,
				Self::R14    => 0b110,
				Self::R15    => 0b111,
			}
		}
	}

	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Index64 {
		Rax,
		Rcx,
		Rdx,
		Rbx,
		NoIndex,
		Rbp,
		Rsi,
		Rdi,
		R8,
		R9,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
	}
	#[derive(Clone, Copy, Debug, PartialEq)]
	pub enum Base64 {
		NoBase,
		Rax,
		Rcx,
		Rdx,
		Rbx,
		Rsp,
		Rbp,
		Rsi,
		Rdi,
		R8,
		R9,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
	}
}
