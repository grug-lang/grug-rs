use crate::types::{GrugValue, GrugId};
use crate::ntstring::NTStrPtr;

use std::cell::Cell;
use std::mem::size_of;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Op {
	// 0x00
	ReturnVoid,
	// 0x01
	ReturnValue,
	// All the four following types have the same instruction
	// representation. The variants just exist to make encoding easier
	// 0x03
	LoadQW {
		bytes: [u8; 8],
	},
	// 0x03
	LoadNumber {
		number: f64,
	},
	// 0x03
	LoadId {
		id: GrugId,
	},
	// 0x03
	LoadStr {
		string: NTStrPtr<'static>,
	},
	// 0x04
	LoadFalse,
	// 0x05
	LoadTrue,
	// 0x06        | 0x07
	// 0b00000110  | 0b00000111
	// index as u8 | index as u16
	Dup{
		index: usize,
	},
	// 0x08
	Pop,
	// 0b00010000
	// 0x10
	Add,
	// 0x11
	Sub,
	// 0x12
	Mul,
	// 0x13
	Div,
	// 0x14
	Rem,
	// 0x15
	And,
	// 0x16
	Or,
	// 0x17
	Not,
	// 0x18
	CmpEq,
	// 0x19
	CmpNeq,
	// 0x1a
	StrEq,
	// 0x1b
	CmpG,
	// 0x1c
	CmpGe,
	// 0x1d
	CmpL,
	// 0x1e
	CmpLe,
	// 0x1f
	PrintStr,
	// 0x20          | 0x21
	// 0b00100000    | 0b00100001
	// index as u8   | index as u16
	LoadGlobal {
		index: usize,
	},
	// 0x22          | 0x23
	// 0b00100010    | 0b00100011
	// index as u8   | index as u16
	StoreGlobal {
		index: usize,
	},
	// 0x24          | 0x25
	// 0b00100100    | 0b00100101
	// offset as i16 | offset as isize
	Jmp {
		offset: isize,
	},
	// 0x26          | 0x27
	// 0b00100110    | 0b00100111
	// offset as i16 | offset as isize
	JmpIf {
		offset: isize,
	},
	// 0x28          | 0x29
	// 0b00101000    | 0b00101001
	LoadLocal {
		index: usize,
	},
	// 0x2a          | 0x2b
	// 0b00101010    | 0b00101011
	StoreLocal {
		index: usize,
	},
	// 0x2c          | 0x2d
	// 0b00101100    | 0b00101101
	// CallHelper {
	// 	args: usize,
	// 	helper_index: usize
	// },
	// 0x2e          | 0x2f
	// 0b00101110    | 0b00101111
	// CallGameFunction {
	// 	args: usize,
	// 	helper_index: usize
	// },
}

impl PartialEq for Op {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::ReturnVoid, Self::ReturnVoid)       => true,
			(Self::ReturnValue, Self::ReturnValue)     => true,
			(Self::LoadQW{..} | Self::LoadNumber{..} | Self::LoadId{..} | Self::LoadStr{..}, 
			Self::LoadQW{..} | Self::LoadNumber{..} | Self::LoadId{..} | Self::LoadStr{..}) => {
				let self_bytes = match self {
					Self::LoadQW{bytes} => bytes,
					Self::LoadNumber{number} => &number.to_ne_bytes(),
					Self::LoadId{id} => &id.to_inner().to_ne_bytes(),
					Self::LoadStr{string} => &(string.as_ptr() as usize).to_ne_bytes(),
					_ => unreachable!(),
				};
				let other_bytes = match other {
					Self::LoadQW{bytes} => bytes,
					Self::LoadNumber{number} => &number.to_ne_bytes(),
					Self::LoadId{id} => &id.to_inner().to_ne_bytes(),
					Self::LoadStr{string} => &(string.as_ptr() as usize).to_ne_bytes(),
					_ => unreachable!(),
				};
				self_bytes == other_bytes
			}
			(Self::LoadFalse, Self::LoadFalse)  => true,
			(Self::LoadTrue , Self::LoadTrue )  => true,
			(Self::Dup{index: i0}  , Self::Dup{index: i1})  => i0 == i1,
			(Self::Pop      , Self::Pop      )  => true,
			(Self::Add      , Self::Add      )  => true,
			(Self::Sub      , Self::Sub      )  => true,
			(Self::Mul      , Self::Mul      )  => true,
			(Self::Div      , Self::Div      )  => true,
			(Self::Rem      , Self::Rem      )  => true,
			(Self::And      , Self::And      )  => true,
			(Self::Or       , Self::Or       )  => true,
			(Self::Not      , Self::Not      )  => true,
			(Self::CmpEq    , Self::CmpEq    )  => true,
			(Self::CmpNeq   , Self::CmpNeq   )  => true,
			(Self::StrEq    , Self::StrEq    )  => true,
			(Self::CmpG     , Self::CmpG     )  => true,
			(Self::CmpGe    , Self::CmpGe    )  => true,
			(Self::CmpL     , Self::CmpL     )  => true,
			(Self::CmpLe    , Self::CmpLe    )  => true,
			(Self::PrintStr , Self::PrintStr )  => true,
			(Self::LoadGlobal {index: o1} , Self::LoadGlobal {index: o2}) => o1 == o2,
			(Self::StoreGlobal{index: o1} , Self::StoreGlobal{index: o2}) => o1 == o2,
			(Self::Jmp  {offset: o1} , Self::Jmp  {offset: o2}) => o1 == o2,
			(Self::JmpIf{offset: o1} , Self::JmpIf{offset: o2}) => o1 == o2,
			(Self::LoadLocal  {index: o1} , Self::LoadLocal  {index: o2}) => o1 == o2,
			(Self::StoreLocal {index: o1} , Self::StoreLocal {index: o2}) => o1 == o2,
			_ => false,
		}
	}
}

impl Op {
	fn decode(bytes: &mut &[u8]) -> Option<Self> {
		let (value, len) = match bytes.get(0)? {
			0x00 => (Self::ReturnVoid, 1),
			0x01 => (Self::ReturnValue, 1),
			// LoadQW
			// LoadNumber
			// LoadId
			// LoadStr
			0x03 => {
				let bytes = &mut bytes.get(1..)?;
				let value = get_u64_bytes(bytes)?;
				(Op::LoadQW{
					bytes: value,
				}, 1 + size_of::<u64>())
			}
			// LoadTrue
			0x04 => (Op::LoadFalse, 1),
			// LoadFalse
			0x05 => (Op::LoadTrue, 1),
			0x06 => {
				let index = *bytes.get(1)? as usize;
				(Op::Dup{
					index
				}, 2)
			}
			0x07 => {
				let bytes = &mut bytes.get(1..)?;
				let index = u16::from_ne_bytes(get_u16_bytes(bytes)?) as usize;
				(Op::Dup{
					index
				}, 2)
			}
			0x08 => (Op::Pop   ,   1),
			0x10 => (Op::Add   ,   1),
			0x11 => (Op::Sub   ,   1),
			0x12 => (Op::Mul   ,   1),
			0x13 => (Op::Div   ,   1),
			0x14 => (Op::Rem   ,   1),
			0x15 => (Op::And   ,   1),
			0x16 => (Op::Or    ,   1),
			0x17 => (Op::Not   ,   1),
			0x18 => (Op::CmpEq ,   1),
			0x19 => (Op::CmpNeq,   1),
			0x1a => (Op::StrEq ,   1),
			0x1b => (Op::CmpG  ,   1),
			0x1c => (Op::CmpGe ,   1),
			0x1d => (Op::CmpL  ,   1),
			0x1e => (Op::CmpLe ,   1),
			0x1f => (Op::PrintStr, 1),
			// 0x20       | 0x21
			// 0b00100000 | 0b00100001
			// u8 index
			0x20 => {
				let index = *bytes.get(1)? as usize;
				(Op::LoadGlobal{index}, 2)
			}
			// u16 index
			0x21 => {unimplemented!()}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x22 => {
				let index = *bytes.get(1)? as usize;
				(Op::StoreGlobal{index}, 2)
			}
			// u16 index
			0x23 => {unimplemented!()}
			// 0x24          | 0x25
			// 0b00100100    | 0b00100101
			// offset as i16 | offset as isize
			0x24 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i16>())
			}
			0x25 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i64>())
			}
			// 0x26          | 0x27
			// 0b00100110    | 0b00100111
			// offset as i16 | offset as isize
			0x26 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i16>())
			}
			0x27 => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i64>())
			}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x28 => {
				let index = *bytes.get(1)? as usize;
				(Op::LoadLocal{index}, 2)
			}
			// u16 index
			0x29 => {unimplemented!()}
			// 0x22       | 0x23
			// 0b00100010 | 0b00100011
			// u8 index
			0x2a => {
				let index = *bytes.get(1)? as usize;
				(Op::StoreLocal{index}, 2)
			}
			// u16 index
			0x2b => {unimplemented!()}
			_ => return None,
		};
		*bytes = &bytes[len..];
		return Some(value);

		fn get_u64_bytes(bytes: &mut &[u8]) -> Option<[u8; size_of::<u64>()]> {
			if bytes.len() < size_of::<u64>() {
				return None;
			}
			Some(unsafe{*bytes.as_ptr().cast::<[u8; _]>()})
		}

		fn get_u16_bytes(bytes: &mut &[u8]) -> Option<[u8; size_of::<u16>()]> {
			if bytes.len() < size_of::<u16>() {
				return None;
			}
			Some(unsafe{*bytes.as_ptr().cast::<[u8; _]>()})
		}
	}
}

#[repr(transparent)]
pub struct Instructions(Vec<u8>);

impl std::fmt::Debug for Instructions {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut buf = &*self.0;
		f.debug_list()
			.entries(
				std::iter::from_fn(|| {
					if buf.len() == 0 {
						None
					} else {
						Some(Op::decode(&mut buf)
							.expect("Invalid instruction"))
					}
				})
			)
			.finish()
	}
}

pub struct Stack {
	stack: Vec<GrugValue>,
	rbp: usize,
}
impl Stack {
	pub fn new() -> Self {
		Self {
			stack: Vec::new(),
			rbp: 0,
		}
	}

	pub fn reset(mut self) -> Self {
		self.stack.clear();
		self.rbp = 0;
		self
	}

	pub unsafe fn run(&mut self, globals: &[Cell<GrugValue>], instructions: &Instructions, _start_loc: usize) -> Option<GrugValue> {
		self.stack.resize(self.stack.len() + 64, GrugValue{void: ()});
		let mut stream = &*instructions.0;
		while let Some(ins) = Op::decode(&mut stream) {
			match ins {
				Op::ReturnVoid         => {
					self.stack.truncate(self.rbp);
					if self.stack.len() == 0 {
						return Some(GrugValue{void: ()});
					} else {
						let ip = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						let rbp = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						self.rbp = rbp as usize;
						stream = &mut &*instructions.0.get(ip as usize ..)?;
					}
				}
				Op::ReturnValue        => {
					let ret_val = self.stack.pop()?;
					self.stack.truncate(self.rbp);
					if self.stack.len() == 0 {
						return Some(ret_val);
					} else {
						self.stack.push(ret_val);
						let ip = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						let rbp = u64::from_ne_bytes(self.stack.pop()?.as_bytes());
						self.rbp = rbp as usize;
						stream = &mut &*instructions.0.get(ip as usize ..)?;
					}
				}
				Op::LoadQW{bytes}      => self.stack.push(GrugValue::from_bytes(bytes)),
				Op::LoadNumber{..}     | 
				Op::LoadStr{..}        | 
				Op::LoadId{..}         => unreachable!(),
				Op::LoadFalse          => self.stack.push(GrugValue{bool: 0}),
				Op::LoadTrue           => self.stack.push(GrugValue{bool: 1}),
				Op::Dup{index}         => {
					self.stack.push(*self.stack.get(self.stack.len() - 1 - index)?)
				}
				Op::Pop                => {self.stack.pop()?;}
				Op::Add                |
				Op::Sub                |
				Op::Mul                |
				Op::Div                |
				Op::Rem                => {
					let second = unsafe{self.stack.pop()?.number};
					let first = unsafe{self.stack.pop()?.number};
					let value = match ins {
						Op::Add => first + second,
						Op::Sub => first - second,
						Op::Mul => first * second,
						Op::Div => first / second,
						Op::Rem => first % second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{number: value});
				}
				Op::And                |
				Op::Or                 => {
					let second = unsafe{self.stack.pop()?.bool};
					let first = unsafe{self.stack.pop()?.bool};
					let value = match ins {
						Op::And => (first != 0) && (second != 0),
						Op::Or  => (first != 0) || (second != 0),
						_ => unreachable!(),
					} as u8;
					self.stack.push(GrugValue{bool: value});
				}
				Op::Not                => {
					let value = unsafe{self.stack.pop()?.bool};
					self.stack.push(GrugValue{bool: (value == 0) as u8});
				}
				Op::CmpEq | Op::CmpNeq => {
					let second = self.stack.pop()?.as_bytes();
					let first = self.stack.pop()?.as_bytes();
					let value = match ins {
						Op::CmpEq  => first == second,
						Op::CmpNeq => first != second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				Op::StrEq              => {
					let second = unsafe{self.stack.pop()?.string};
					let first = unsafe{self.stack.pop()?.string};
					self.stack.push(GrugValue{bool: (first == second) as u8});
				}
				Op::CmpG  | Op::CmpGe  |
				Op::CmpL  | Op::CmpLe  => {
					let second = unsafe{self.stack.pop()?.number};
					let first = unsafe{self.stack.pop()?.number};
					let value = match ins {
						Op::CmpG  => first >  second,
						Op::CmpGe => first >= second,
						Op::CmpL  => first <  second,
						Op::CmpLe => first <= second,
						_ => unreachable!(),
					};
					self.stack.push(GrugValue{bool: value as u8});
				}
				Op::PrintStr           => {
					use std::ptr::NonNull;
					let str = unsafe{
						NTStrPtr::from_ptr(
							NonNull::new_unchecked(
								std::ptr::with_exposed_provenance_mut(
									usize::from_ne_bytes(self.stack.pop()?.as_bytes())
								)
							)
						).to_str()
					};
					// let str = unsafe{NTStrPtr::from_ptr(NonNull::new_unchecked(self.stack.pop()?.string.cast_mut())).to_str()};
					println!("{}", str);
					// std::io::stdout().write_fmt(format_args!("\n{}", str)).unwrap();
				}
				Op::LoadGlobal{index}  => {
					self.stack.push(globals.get(index)?.get());
				}
				Op::StoreGlobal{index} => {
					globals.get(index)?.set(self.stack.pop()?);
				}
				Op::Jmp{offset}        => {
					stream = unsafe{
						std::slice::from_raw_parts(
							stream.as_ptr().offset(offset), 
							(stream.len() as isize - offset) as usize
						)
					}
				}
				Op::JmpIf{offset}      => {
					if unsafe{self.stack.pop()?.bool} != 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								stream.as_ptr().offset(offset), 
								(stream.len() as isize - offset) as usize
							)
						}
					}
				}
				Op::LoadLocal{index}   => {
					 let value = *self.stack.get(self.rbp + index)?;
					 self.stack.push(value);
				}
				Op::StoreLocal{index}  => {
					let value = self.stack.pop()?;
					*self.stack.get_mut(self.rbp + index)? = value;
				}
			}
		}
		None
	}
}
impl Instructions {
	pub fn new() -> Self {
		Self(Vec::new())
	}

	pub fn clear(&mut self) { self.0.clear() }

	pub fn push_op(&mut self, op: Op) {
		match op {
			Op::ReturnVoid  =>  self.0.push(0x00),
			Op::ReturnValue => self.0.push(0x01),
			Op::LoadQW{bytes}  => {
				self.0.push(
					0x03
				);
				self.0.extend_from_slice(&bytes);
			}
			Op::LoadNumber{number}  => {
				self.0.push(
					0x03
				);
				self.0.extend_from_slice(&number.to_ne_bytes());
			}
			Op::LoadId{id}  => {
				self.0.push(
					0x03
				);
				self.0.extend_from_slice(&id.to_inner().to_ne_bytes());
			}
			Op::LoadStr{string}  => {
				self.0.push(
					0x03
				);
				self.0.extend_from_slice(&string.as_ptr().expose_provenance().to_ne_bytes());
			}
			Op::LoadFalse => self.0.push(0x04),
			Op::LoadTrue  => self.0.push(0x05),
			Op::Dup{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x06);
				self.0.push(index as u8);
			}
			Op::Pop       => self.0.push(0x08),
			Op::Add       => self.0.push(0x10),
			Op::Sub       => self.0.push(0x11),
			Op::Mul       => self.0.push(0x12),
			Op::Div       => self.0.push(0x13),
			Op::Rem       => self.0.push(0x14),
			Op::And       => self.0.push(0x15),
			Op::Or        => self.0.push(0x16),
			Op::Not       => self.0.push(0x17),
			Op::CmpEq     => self.0.push(0x18),
			Op::CmpNeq    => self.0.push(0x19),
			Op::StrEq     => self.0.push(0x1a),
			Op::CmpG      => self.0.push(0x1b),
			Op::CmpGe     => self.0.push(0x1c),
			Op::CmpL      => self.0.push(0x1d),
			Op::CmpLe     => self.0.push(0x1e),
			Op::PrintStr  => self.0.push(0x1f),
			Op::LoadGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x20);
				self.0.push(index as u8);
			}
			Op::StoreGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x22);
				self.0.push(index as u8);
			}
			Op::Jmp{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.0.push(0x24);
				let bytes = offset.to_ne_bytes();
				self.0.push(bytes[0]);
				self.0.push(bytes[1]);
			}
			Op::JmpIf{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.0.push(0x26);
				let bytes = offset.to_ne_bytes();
				self.0.push(bytes[0]);
				self.0.push(bytes[1]);
			}
			Op::LoadLocal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x28);
				self.0.push(index as u8);
			}
			Op::StoreLocal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x2a);
				self.0.push(index as u8);
			}
		}
	}

	pub fn get_loc(&self) -> usize {
		self.0.len()
	}

	/// SAFETY: 
	/// location passed in must be the position returned by a previous call to push_op.
	///
	/// It will succeed if opcode matches and there is enough space to store a jump of that type
	pub unsafe fn try_patch_jump(&mut self, op: Op, location: usize) -> Option<()> {
		match (self.0.get(location)?, op) {
			// Jmp
			(0x24, Op::Jmp{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<i16>()]) = (offset as i16).to_ne_bytes();
				} 
				Some(())
			}
			(0x25, Op::Jmp{offset}) => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<isize>()]) = offset.to_ne_bytes();
				} 
				Some(())
			}
			// JmpIf
			(0x26, Op::JmpIf{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<i16>()]) = (offset as i16).to_ne_bytes();
				} 
				Some(())
			}
			(0x27, Op::JmpIf{offset}) => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<isize>()]) = offset.to_ne_bytes();
				} 
				Some(())
			}
			_ => None,
		}
	}
}
#[cfg(test)]
mod test {
	use super::*;
	use crate::nt;
	#[test]
	fn vm_test_decoding() {
		let mut stream = Instructions(Vec::new());
		macro_rules! test_op {
			($op: expr) => {{
				stream.clear();
				let op = $op;
				stream.push_op(op);
				assert_eq!(Op::decode(&mut &*stream.0), Some(op));
			}}
		}
		test_op!(Op::ReturnVoid);
		test_op!(Op::ReturnValue);
		for i in 0..10_usize {
			test_op!(Op::LoadQW{bytes: i.to_ne_bytes()});
			test_op!(Op::LoadNumber{number: i as f64});
			test_op!(Op::StoreGlobal{index: i});
			test_op!(Op::LoadGlobal{index: i});
			test_op!(Op::JmpIf{offset: i as isize});
			test_op!(Op::JmpIf{offset: -(i as isize)});
			test_op!(Op::Jmp{offset: i as isize});
			test_op!(Op::Jmp{offset: -(i as isize)});
			test_op!(Op::StoreLocal{index: i});
			test_op!(Op::LoadLocal{index: i});
			test_op!(Op::Dup{index: i});
			test_op!(Op::Dup{index: 10 * i});
		}
		test_op!(Op::LoadFalse);
		test_op!(Op::LoadTrue);
		test_op!(Op::Pop);
		test_op!(Op::Add);
		test_op!(Op::Sub);
		test_op!(Op::Mul);
		test_op!(Op::Div);
		test_op!(Op::Rem);
		test_op!(Op::And);
		test_op!(Op::Or);
		test_op!(Op::Not);
		test_op!(Op::CmpEq);
		test_op!(Op::CmpNeq);
		test_op!(Op::StrEq);
		test_op!(Op::CmpG);
		test_op!(Op::CmpGe);
		test_op!(Op::CmpL);
		test_op!(Op::CmpLe);
	}

	#[test]
	fn vm_test_patching() {
		let mut stream = Instructions(Vec::new());
		for i in 0..20 {
			stream.push_op(Op::Jmp{offset: 0});
			unsafe{stream.try_patch_jump(Op::Jmp{offset: i as isize * 10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.0).unwrap(), Op::Jmp{offset: i as isize * 10});
			stream.clear();

			stream.push_op(Op::Jmp{offset: 0});
			unsafe{stream.try_patch_jump(Op::Jmp{offset: i as isize * -10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.0).unwrap(), Op::Jmp{offset: i as isize * -10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * 10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.0).unwrap(), Op::JmpIf{offset: i as isize * 10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * -10}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.0).unwrap(), Op::JmpIf{offset: i as isize * -10});
			stream.clear();

			stream.push_op(Op::JmpIf{offset: 0});
			unsafe{stream.try_patch_jump(Op::JmpIf{offset: i as isize * -1000}, 0).unwrap()};
			assert_eq!(Op::decode(&mut &*stream.0).unwrap(), Op::JmpIf{offset: i as isize * -1000});
			stream.clear();
		}
	}

	#[test]
	fn vm_test_0() {
		let stream = Instructions(Vec::new());
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&[], &stream, 0).is_none()});
	}

	#[test]
	fn vm_test_1() {
		let mut stream = Instructions(Vec::new());
		stream.push_op(Op::ReturnVoid);
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&[], &stream, 0).is_some()});
	}

	#[test]
	fn vm_test_2() {
		let mut stream = Instructions(Vec::new());
		stream.push_op(Op::LoadNumber{number:25.});
		stream.push_op(Op::ReturnValue);
		let mut vm = Stack::new();
		assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == 25.)});
	}

	#[test]
	fn vm_test_3() {
		let mut stream = Instructions(Vec::new());
		let mut vm = Stack::new();

		for i in 0..5 {
			for j in 1..5 {
				let i = i as f64;
				let j = j as f64;
				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Add);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == i + j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Sub);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == i - j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Mul);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == i * j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Div);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == i / j)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::Rem);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.number == i % j)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_4() {
		let mut stream = Instructions(Vec::new());
		let mut vm = Stack::new();

		for i in 0..1 {
			for j in 0..1 {
				let i = i != 0;
				let j = j != 0;
				if i {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::And);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i && j) as u8)});
				stream.clear();

				if i {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {_ = stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::Or);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i || j) as u8)});
				stream.clear();

				if i {stream.push_op(Op::LoadTrue)} else {stream.push_op(Op::LoadFalse)};
				if j {stream.push_op(Op::LoadTrue)} else {stream.push_op(Op::LoadFalse)};
				stream.push_op(Op::Or);
				stream.push_op(Op::ReturnValue);
			}
		}
	}

	#[test]
	fn vm_test_5() {
		let mut stream = Instructions(Vec::new());
		let mut vm = Stack::new();

		for i in 0..10 {
			for j in 0..10 {
				let i = i as f64;
				let j = j as f64;
				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpEq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i == j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpNeq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i != j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpG);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i > j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpGe);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i >= j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpL);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i < j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadNumber{number:i});
				stream.push_op(Op::LoadNumber{number:j});
				stream.push_op(Op::CmpLe);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i <= j) as u8)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_6() {
		let mut stream = Instructions(Vec::new());
		let mut vm = Stack::new();

		let strings = [
			nt!("a"),
			nt!("b"),
			nt!("c"),
			nt!("d"),
			nt!("e"),
			nt!("f"),
			nt!("g"),
			nt!("aa"),
			nt!("ba"),
			nt!("ca"),
			nt!("da"),
			nt!("ea"),
			nt!("fa"),
			nt!("ga"),
		];

		for i in &strings {
			for j in &strings {
				let i = i.as_ntstrptr();
				let j = j.as_ntstrptr();
				stream.push_op(Op::LoadStr{string:i});
				stream.push_op(Op::LoadStr{string:j});
				stream.push_op(Op::StrEq);
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some_and(|x| x.bool == (i == j) as u8)});
				stream.clear();

				stream.push_op(Op::LoadStr{string:i});
				stream.push_op(Op::LoadStr{string:j});
				stream.push_op(Op::PrintStr);
				stream.push_op(Op::PrintStr);
				stream.push_op(Op::ReturnVoid);
				assert!(unsafe{vm.run(&[], &stream, 0).is_some()});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_7() {
		let mut stream = Instructions(Vec::new());
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			for j in 0..10 {
				let i = i as f64;
				let j = j as f64;
				stream.push_op(Op::LoadNumber {number: i});
				stream.push_op(Op::StoreGlobal{index: 0});
				stream.push_op(Op::LoadNumber {number: j});
				stream.push_op(Op::StoreGlobal{index: 1});
				stream.push_op(Op::LoadGlobal {index: 0});
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| x.number == i)});
				stream.clear();

				stream.push_op(Op::LoadNumber {number: i});
				stream.push_op(Op::StoreGlobal{index: 0});
				stream.push_op(Op::LoadNumber {number: j});
				stream.push_op(Op::StoreGlobal{index: 1});
				stream.push_op(Op::LoadGlobal {index: 0});
				stream.push_op(Op::LoadGlobal {index: 1});
				stream.push_op(Op::Add);
				stream.push_op(Op::StoreGlobal{index: 2});
				stream.push_op(Op::LoadGlobal {index: 2});
				stream.push_op(Op::ReturnValue);
				assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| x.number == i + j)});
				stream.clear();
			}
		}
	}

	#[test]
	fn vm_test_8() {
		let mut stream = Instructions(Vec::new());
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreGlobal{index: 0});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreGlobal{index: 1});
			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::StoreGlobal{index: 2});
			let cond_start = stream.get_loc() as isize;
			stream.push_op(Op::LoadGlobal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc() as isize;
			stream.push_op(Op::JmpIf{offset: 0});
			stream.push_op(Op::LoadGlobal{index: 1});
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::StoreGlobal{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::StoreGlobal{index: 0});
			stream.push_op(Op::LoadGlobal{index: 2});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::Sub);
			stream.push_op(Op::StoreGlobal{index: 2});
			let block_end_loc = stream.get_loc() as isize;
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc() as isize;
			stream.push_op(Op::LoadGlobal{index: 0});
			stream.push_op(Op::ReturnValue);

			unsafe{stream.try_patch_jump(Op::JmpIf{offset: (block_continue_loc - block_begin_jump - 3)}, block_begin_jump as usize).unwrap()};
			unsafe{stream.try_patch_jump(Op::Jmp{offset: (cond_start - block_end_loc - 3)}, block_end_loc as usize).unwrap()};
			let fib = {
				let mut a = 1.;
				let mut b = 1.;
				let mut i = i as f64;
				while i > 0. {
					let _b0 = b;
					let _a0 = a;
					let _a1 = a;
					b = _a1;
					let _0 = _b0 + _a0;
					a = _0;
					let _i = i;
					let _1 = 1.;
					i = _i - _1;
				}
				a
			};
			assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| {assert_eq!(x.number, fib); true})});
			// panic!("{:#?}", stream);
			stream.clear();
		}
	}

	#[test]
	fn vm_test_9() {
		let mut stream = Instructions(Vec::new());
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreLocal{index: 0});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::StoreLocal{index: 1});
			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::StoreLocal{index: 2});
			let cond_start = stream.get_loc() as isize;
			stream.push_op(Op::LoadLocal{index: 2});
			stream.push_op(Op::LoadNumber{number: 0.});
			stream.push_op(Op::CmpLe);
			let block_begin_jump = stream.get_loc() as isize;
			stream.push_op(Op::JmpIf{offset: 0});
			stream.push_op(Op::LoadLocal{index: 1});
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::StoreLocal{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::StoreLocal{index: 0});
			stream.push_op(Op::LoadLocal{index: 2});
			stream.push_op(Op::LoadNumber{number: 1.});
			stream.push_op(Op::Sub);
			stream.push_op(Op::StoreLocal{index: 2});
			let block_end_loc = stream.get_loc() as isize;
			stream.push_op(Op::Jmp{offset: 0});
			let block_continue_loc = stream.get_loc() as isize;
			stream.push_op(Op::LoadLocal{index: 0});
			stream.push_op(Op::ReturnValue);

			unsafe{stream.try_patch_jump(Op::JmpIf{offset: (block_continue_loc - block_begin_jump - 3)}, block_begin_jump as usize).unwrap()};
			unsafe{stream.try_patch_jump(Op::Jmp{offset: (cond_start - block_end_loc - 3)}, block_end_loc as usize).unwrap()};
			let fib = {
				let mut a = 1.;
				let mut b = 1.;
				let mut i = i as f64;
				while i > 0. {
					let _b0 = b;
					let _a0 = a;
					let _a1 = a;
					b = _a1;
					let _0 = _b0 + _a0;
					a = _0;
					let _i = i;
					let _1 = 1.;
					i = _i - _1;
				}
				a
			};
			assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| {assert_eq!(x.number, fib); true})});
			// panic!("{:#?}", stream);
			stream.clear();
		}
	}

	#[test]
	fn vm_test_10() {
		let mut stream = Instructions(Vec::new());
		let globals = [const {Cell::new(GrugValue{void: ()})}; 10];
		let mut vm = Stack::new();

		for i in 0..10 {
			stream.push_op(Op::LoadNumber{number: 0.});
			for j in 0..i {
				stream.push_op(Op::LoadNumber{number: (i - j) as f64});
			}
			for _ in 0..(i-1) {
				stream.push_op(Op::Pop);
			}
			stream.push_op(Op::ReturnValue);
			
			assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| {assert_eq!(x.number, i as f64); true})});
			stream.clear();

			stream.push_op(Op::LoadNumber{number: i as f64});
			stream.push_op(Op::LoadNumber{number: i as f64 - 1.});
			stream.push_op(Op::Dup{index: 1});
			stream.push_op(Op::Add);
			stream.push_op(Op::Add);
			stream.push_op(Op::ReturnValue);
			assert!(unsafe{vm.run(&globals, &stream, 0).is_some_and(|x| {assert_eq!(x.number, 3. * i as f64 - 1.); true})});
			stream.clear();
		}
	}
}
