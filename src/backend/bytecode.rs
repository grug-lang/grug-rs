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
	// 0x06
	Add,
	// 0x07
	Sub,
	// 0x08
	Mul,
	// 0x09
	Div,
	// 0x0A
	Rem,
	// 0x0B
	And,
	// 0x0C
	Or,
	// 0x0D
	Not,
	// 0x0E
	CmpEq,
	// 0x0F
	CmpNeq,
	// 0x10
	StrEq,
	// 0x11
	CmpG,
	// 0x12
	CmpGe,
	// 0x13
	CmpL,
	// 0x14
	CmpLe,
	// 0x15
	PrintStr,
	// 0x16          | 0x17
	// 0b00010110    | 0b00010111
	// index as u8   | index as u16
	LoadGlobal {
		index: usize,
	},
	// 0x18          | 0x19
	// 0b00011000    | 0b00011001
	// index as u8   | index as u16
	StoreGlobal {
		index: usize,
	},
	// 0x1a          | 0x1b
	// 0b00011010    | 0b00011011
	// offset as i16 | offset as isize
	Jmp {
		offset: isize,
	},
	// 0x1c          | 0x1d
	// 0b00011100    | 0b00011101
	// offset as i16 | offset as isize
	JmpIf {
		offset: isize,
	},
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
			(Self::LoadTrue, Self::LoadTrue)    => true,
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
			0x06 => (Op::Add   ,   1),
			0x07 => (Op::Sub   ,   1),
			0x08 => (Op::Mul   ,   1),
			0x09 => (Op::Div   ,   1),
			0x0a => (Op::Rem   ,   1),
			0x0b => (Op::And   ,   1),
			0x0c => (Op::Or    ,   1),
			0x0d => (Op::Not   ,   1),
			0x0E => (Op::CmpEq ,   1),
			0x0F => (Op::CmpNeq,   1),
			0x10 => (Op::StrEq ,   1),
			0x11 => (Op::CmpG  ,   1),
			0x12 => (Op::CmpGe ,   1),
			0x13 => (Op::CmpL  ,   1),
			0x14 => (Op::CmpLe ,   1),
			0x15 => (Op::PrintStr, 1),
			// 0x16       | 0x17
			// 0b00010110 | 0b00010111
			// u8 index
			0x16 => {
				let index = *bytes.get(1)? as usize;
				(Op::LoadGlobal{index}, 2)
			}
			// u16 index
			0x17 => {unimplemented!()}
			// 0x18       | 0x19
			// 0b00011000 | 0b00011001
			// u8 index
			0x18 => {
				let index = *bytes.get(1)? as usize;
				(Op::StoreGlobal{index}, 2)
			}
			// u16 index
			0x19 => {unimplemented!()}
			// 0x1a          | 0x1b
			// 0b00011010    | 0b00011011
			// offset as i16 | offset as isize
			0x1a => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i16>())
			}
			0x1b => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::Jmp{offset}, 1 + size_of::<i64>())
			}
			// 0x1c          | 0x1d
			// 0b00011100    | 0b00011101
			// offset as i16 | offset as isize
			0x1c => {
				let bytes = &mut bytes.get(1..)?;
				let offset = i16::from_ne_bytes(get_u16_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i16>())
			}
			0x1d => {
				let bytes = &mut bytes.get(1..)?;
				let offset = u64::from_ne_bytes(get_u64_bytes(bytes)?) as isize;
				(Op::JmpIf{offset}, 1 + size_of::<i64>())
			}
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
	// rbp: usize,
}
impl Stack {
	pub fn new() -> Self {
		Self {
			stack: Vec::new(),
			// rbp: 0,
		}
	}

	pub unsafe fn run(&mut self, globals: &[Cell<GrugValue>], instructions: &Instructions, _start_loc: usize) -> Option<GrugValue> {
		let mut stream = &*instructions.0;
		while let Some(ins) = Op::decode(&mut stream) {
			match ins {
				Op::ReturnVoid         => return Some(GrugValue{void: ()}),
				Op::ReturnValue        => return self.stack.pop(),
				Op::LoadQW{bytes}      => self.stack.push(GrugValue::from_bytes(bytes)),
				Op::LoadNumber{..}     | 
				Op::LoadStr{..}        | 
				Op::LoadId{..}         => unreachable!(),
				Op::LoadFalse          => self.stack.push(GrugValue{bool: 0}),
				Op::LoadTrue           => self.stack.push(GrugValue{bool: 1}),
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
				Op::Jmp{offset} => {
					stream = unsafe{
						std::slice::from_raw_parts(
							stream.as_ptr().offset(offset), 
							(stream.len() as isize - offset) as usize
						)
					}
				}
				Op::JmpIf{offset} => {
					if unsafe{self.stack.pop()?.bool} != 0 {
						stream = unsafe{
							std::slice::from_raw_parts(
								stream.as_ptr().offset(offset), 
								(stream.len() as isize - offset) as usize
							)
						}
					}
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
			Op::LoadFalse =>  self.0.push(0x04),
			Op::LoadTrue  =>  self.0.push(0x05),
			Op::Add       =>  self.0.push(0x06),
			Op::Sub       =>  self.0.push(0x07),
			Op::Mul       =>  self.0.push(0x08),
			Op::Div       =>  self.0.push(0x09),
			Op::Rem       =>  self.0.push(0x0a),
			Op::And       =>  self.0.push(0x0b),
			Op::Or        =>  self.0.push(0x0c),
			Op::Not       =>  self.0.push(0x0d),
			Op::CmpEq     =>  self.0.push(0x0e),
			Op::CmpNeq    =>  self.0.push(0x0f),
			Op::StrEq     =>  self.0.push(0x10),
			Op::CmpG      =>  self.0.push(0x11),
			Op::CmpGe     =>  self.0.push(0x12),
			Op::CmpL      =>  self.0.push(0x13),
			Op::CmpLe     =>  self.0.push(0x14),
			Op::PrintStr  =>  self.0.push(0x15),
			Op::LoadGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x16);
				self.0.push(index as u8);
			}
			Op::StoreGlobal{index} => {
				if index > u8::MAX as usize {
					unimplemented!();
				}
				self.0.push(0x18);
				self.0.push(index as u8);
			}
			Op::Jmp{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.0.push(0x1a);
				let bytes = offset.to_ne_bytes();
				self.0.push(bytes[0]);
				self.0.push(bytes[1]);
			}
			Op::JmpIf{offset} => {
				if offset > i16::MAX as isize || offset < i16::MIN as isize {
					unimplemented!();
				}
				self.0.push(0x1c);
				let bytes = offset.to_ne_bytes();
				self.0.push(bytes[0]);
				self.0.push(bytes[1]);
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
			(0x1a, Op::Jmp{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<i16>()]) = (offset as i16).to_ne_bytes();
				} 
				Some(())
			}
			(0x1b, Op::Jmp{offset}) => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<isize>()]) = offset.to_ne_bytes();
				} 
				Some(())
			}
			// JmpIf
			(0x1c, Op::JmpIf{offset}) if offset >= i16::MIN as isize && offset <= i16::MAX as isize => {
				unsafe {
					*(self.0.get_mut((location + 1)..)? as *mut [u8] as *mut [u8; size_of::<i16>()]) = (offset as i16).to_ne_bytes();
				} 
				Some(())
			}
			(0x1d, Op::JmpIf{offset}) => {
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
		}
		test_op!(Op::LoadFalse);
		test_op!(Op::LoadTrue);
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
}
