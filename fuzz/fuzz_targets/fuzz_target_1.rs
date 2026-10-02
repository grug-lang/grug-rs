#![no_main]

use libfuzzer_sys::{fuzz_target, Corpus};
use arbitrary::{Arbitrary, Unstructured, Error};
use allocator_api2::vec::Vec;
use gruggers::state::GrugState;
use gruggers::backend::BytecodeBackend;
use gruggers::error::ErrorKind;
use gruggers::arena::Arena;

use std::sync::Mutex;
use std::cell::LazyCell;
use std::time::Duration;
use std::io::Write;
use std::mem::ManuallyDrop;
use core::ops::ControlFlow;

static ARENAS: Mutex<Vec<Arena>> = Mutex::new(Vec::new());

struct TokenizedString {
	arena: ManuallyDrop<Arena>,
	str: &'static str,
}

impl std::fmt::Debug for TokenizedString {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		std::fmt::Debug::fmt(self.str, f)
	}
}

impl Drop for TokenizedString {
	fn drop(&mut self) {
		let mut arena = unsafe{ManuallyDrop::take(&mut self.arena)};
		arena.clear();
		ARENAS.lock().unwrap().push(arena);
	}
}

#[derive(Clone, Copy, Debug)]
enum TokenKind {
	OpenParenthesis,
	CloseParenthesis,
	OpenBrace,
	CloseBrace,
	OpenBracket,
	CloseBracket,
	Plus,
	Minus,
	Star,
	ForwardSlash,
	Comma,
	Colon,
	Dot,
	NewLine,
	DoubleEquals,
	NotEquals,
	Equal,
	GreaterEquals,
	Greater,
	LessEquals,
	Less,
	And,
	Or,
	Not,
	True,
	False,
	If,
	Else,
	While,
	Break,
	Return,
	Continue,
	Export,
	Local,
	Space,
	Indentation,
	String,
	Entity,
	Resource,
	Word,
	Int32,
	Float32,
	Comment,
}

impl<'a> Arbitrary<'a> for TokenKind {
	fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<Self> {
		if u.is_empty() {
			return Err(Error::NotEnoughData)
		}
		Ok(*u.choose(&[
			 TokenKind::String,
			 TokenKind::OpenParenthesis,
			 TokenKind::CloseParenthesis,
			 TokenKind::OpenBrace,
			 TokenKind::CloseBrace,
			 TokenKind::OpenBracket,
			 TokenKind::CloseBracket,
			 TokenKind::Plus,
			 TokenKind::Minus,
			 TokenKind::Star,
			 TokenKind::ForwardSlash,
			 TokenKind::Comma,
			 TokenKind::Colon,
			 TokenKind::Dot,
			 TokenKind::NewLine,
			 TokenKind::DoubleEquals,
			 TokenKind::NotEquals,
			 TokenKind::Equal,
			 TokenKind::GreaterEquals,
			 TokenKind::Greater,
			 TokenKind::LessEquals,
			 TokenKind::Less,
			 TokenKind::And,
			 TokenKind::Or,
			 TokenKind::Not,
			 TokenKind::True,
			 TokenKind::False,
			 TokenKind::If,
			 TokenKind::Else,
			 TokenKind::While,
			 TokenKind::Break,
			 TokenKind::Return,
			 TokenKind::Continue,
			 TokenKind::Export,
			 TokenKind::Local,
			 TokenKind::Space,
			 TokenKind::Indentation,
			 TokenKind::Entity,
			 TokenKind::Resource,
			 TokenKind::Word,
			 TokenKind::Int32,
			 TokenKind::Float32,
			 TokenKind::Comment,
		])?)
	}
}

impl<'a> Arbitrary<'a> for TokenizedString {
	fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<Self> {
		// fn arbitrary_string_in_arena
		let arena = ARENAS.lock().unwrap().pop().unwrap_or_default();
		let mut str = Vec::new_in(&arena);
		u.arbitrary_loop(
			None,
			None,
			|u| {
				match TokenKind::arbitrary(u)? {
					TokenKind::OpenParenthesis  => str.extend_from_slice(b"("),
					TokenKind::CloseParenthesis => str.extend_from_slice(b")"),
					TokenKind::OpenBrace        => str.extend_from_slice(b"{"),
					TokenKind::CloseBrace       => str.extend_from_slice(b"}"),
					TokenKind::OpenBracket      => str.extend_from_slice(b"["),
					TokenKind::CloseBracket     => str.extend_from_slice(b"]"),
					TokenKind::Plus             => str.extend_from_slice(b"+"),
					TokenKind::Minus            => str.extend_from_slice(b"-"),
					TokenKind::Star             => str.extend_from_slice(b"*"),
					TokenKind::ForwardSlash     => str.extend_from_slice(b"/"),
					TokenKind::Comma            => str.extend_from_slice(b","),
					TokenKind::Colon            => str.extend_from_slice(b":"),
					TokenKind::Dot              => str.extend_from_slice(b"."),
					TokenKind::NewLine          => str.extend_from_slice(b"\n"),
					TokenKind::DoubleEquals     => str.extend_from_slice(b"=="),
					TokenKind::NotEquals        => str.extend_from_slice(b"!="),
					TokenKind::Equal            => str.extend_from_slice(b"="),
					TokenKind::GreaterEquals    => str.extend_from_slice(b">="),
					TokenKind::Greater          => str.extend_from_slice(b">"),
					TokenKind::LessEquals       => str.extend_from_slice(b"<="),
					TokenKind::Less             => str.extend_from_slice(b"<"),
					TokenKind::And              => str.extend_from_slice(b"and"),
					TokenKind::Or               => str.extend_from_slice(b"or"),
					TokenKind::Not              => str.extend_from_slice(b"not"),
					TokenKind::True             => str.extend_from_slice(b"true"),
					TokenKind::False            => str.extend_from_slice(b"false"),
					TokenKind::If               => str.extend_from_slice(b"if"),
					TokenKind::Else             => str.extend_from_slice(b"else"),
					TokenKind::While            => str.extend_from_slice(b"while"),
					TokenKind::Break            => str.extend_from_slice(b"break"),
					TokenKind::Return           => str.extend_from_slice(b"return"),
					TokenKind::Continue         => str.extend_from_slice(b"continue"),
					TokenKind::Export           => str.extend_from_slice(b"export"),
					TokenKind::Local            => str.extend_from_slice(b"local"),
					TokenKind::Space            => str.extend_from_slice(b" "),
					TokenKind::Indentation      => {
						(0..(u.int_in_range::<usize>(0..=200)?)).for_each(|_| {
							str.extend_from_slice(b"    ");
						});
					}
					TokenKind::String           => write!(str, "\"{}\"", String::arbitrary(u)?).unwrap(),
					TokenKind::Entity           => write!(str, "e\"{}\"", String::arbitrary(u)?).unwrap(),
					TokenKind::Resource         => write!(str, "r\"{}\"", String::arbitrary(u)?).unwrap(),
					TokenKind::Word             => write!(str, "{}", String::arbitrary(u)?).unwrap(),
					TokenKind::Int32            => write!(str, "{}", i32::arbitrary(u)?).unwrap(),
					TokenKind::Float32          => write!(str, "{}", f32::arbitrary(u)?).unwrap(),
					TokenKind::Comment          => write!(str, "// {}", String::arbitrary(u)?).unwrap(),
				}
				Ok(ControlFlow::Continue(()))
			}
		)?;
		let str = unsafe{std::mem::transmute::<&str, &'static str>(std::str::from_utf8_unchecked(str.leak()))};
		Ok(Self {
			arena: ManuallyDrop::new(arena),
			str,
		})
	}
}

const MOD_API: &str = r#"{
	"entities": {
		"A": {
			"description": "foo",
			"export_functions": [ ]
		}
	},
	"classes": {},
	"host_functions": {
		"test": {
			"description": "foo",
			"return_type": {
				"name": "boolean"
			},
			"parameters": []
		}
	}
}"#;

thread_local! {
	static STATE: LazyCell<GrugState> = LazyCell::new(|| {
		GrugState::new_from_text(
			MOD_API, 
			"./", 
			Default::default(), 
			Duration::from_millis(1000), 
			BytecodeBackend::new()
		).unwrap()
	});
}
fuzz_target!(
	|data: TokenizedString| -> Corpus {
		match std::panic::catch_unwind(|| {
			STATE.with(|state: &LazyCell<GrugState>| {
				let result = state.compile_grug_file_from_str("fuzz/fuzz_targets/test/test-A.grug", data.str);
				if let Err(err) = result {
					if err.inner().error_kind.matches(&ErrorKind::TOKENIZER_ERROR) {
						Corpus::Reject
					} else {
						Corpus::Keep
					}
				} else {
					Corpus::Keep
				}
			})
		}) {
			Ok(val) => val,
			Err(err) => {
				std::mem::forget(err); 
				Corpus::Keep
			}
		}
	}
);
