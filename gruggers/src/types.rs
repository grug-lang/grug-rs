use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use crate::xar::XarHandle;
use crate::ntstring::NTStrPtr;
use crate::state::GrugState;

// TODO Unnest some of these enums

#[repr(C)]
#[derive(Clone, Copy)]
pub union GameFnPtr {
	pub void: GameFnPtrVoid,
	pub void_argless: GameFnPtrVoidArgless,
	pub value: GameFnPtrValue,
	pub value_argless: GameFnPtrValueArgless,
}

impl std::fmt::Debug for GameFnPtr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		unsafe{self.void.fmt(f)}
	}
}

impl PartialEq for GameFnPtr {
	fn eq(&self, other: &Self) -> bool {
		const _: () = const{assert!(size_of::<GameFnPtr>() == size_of::<usize>())};
		unsafe{std::ptr::fn_addr_eq(self.void, other.void)}
		// unsafe{std::mem::transmute::<Self, usize>(*self) == std::mem::transmute::<Self, usize>(*other)}
	}
}

mod from_impls {
	use super::*;
	impl From<GameFnPtrVoid> for GameFnPtr {
		fn from (value: GameFnPtrVoid) -> Self {
			Self {
				void: value,
			}
		}
	}

	impl From<GameFnPtrVoidArgless> for GameFnPtr {
		fn from (value: GameFnPtrVoidArgless) -> Self {
			Self {
				void_argless: value,
			}
		}
	}

	impl From<GameFnPtrValue> for GameFnPtr {
		fn from (value: GameFnPtrValue) -> Self {
			Self {
				value,
			}
		}
	}

	impl From<GameFnPtrValueArgless> for GameFnPtr {
		fn from (value: GameFnPtrValueArgless) -> Self {
			Self {
				value_argless: value,
			}
		}
	}
}

pub type GameFnPtrVoid = extern "C" fn (state: &GrugState, args: *const GrugValue);
pub type GameFnPtrVoidArgless = extern "C" fn (state: &GrugState);
pub type GameFnPtrValue = extern "C" fn (state: &GrugState, args: *const GrugValue) -> GrugValue;
pub type GameFnPtrValueArgless = extern "C" fn (state: &GrugState) -> GrugValue;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct GrugId(u64);
pub type GrugScriptId = GrugId;

impl std::fmt::Display for GrugId {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl GrugId {
	pub fn new(id: u64) -> Self {
		Self(id)
	}

	pub fn to_inner(self) -> u64 {
		self.0
	}
}

pub type GrugOnFnId = u64;

#[derive(Clone, Copy)]
#[repr(C)]
pub union GrugValue {
	pub number: c_double,
	pub bool: u8,
	pub id: GrugId,
	pub string: NTStrPtr<'static>,
	pub void: (),
}

impl GrugValue {
	pub fn from_bytes(bytes: [u8;8]) -> Self {
		const _: () = const {assert!(std::mem::size_of::<GrugValue>() == std::mem::size_of::<[u8;8]>())};
		unsafe{std::mem::transmute::<[u8;8], Self>(bytes)}
	}
	pub fn as_bytes(self) -> [u8;8] {
		const _: () = const {assert!(std::mem::size_of::<GrugValue>() == std::mem::size_of::<[u8;8]>())};
		unsafe{std::mem::transmute::<Self, [u8;8]>(self)}
	}
}

/// SAFETY: GrugValue is !Send and !Sync because of the *mut c_char within it
/// This is just a pointer to a null terminated c string, which is thread safe
unsafe impl Send for GrugValue {}
unsafe impl Sync for GrugValue {}

/// A pointer to a grug entity. Only allows shared access to the data and does
/// not allow copying or cloning. Lifetime of shared borrows are limited to the lifetime of self
#[repr(transparent)]
pub struct GrugEntityHandle<'a>(XarHandle<'a, GrugEntity>);

impl<'a> GrugEntityHandle<'a> {
	/// SAFETY: inner can only be deleted by deleting the returned value
	/// The returned value is allowed to create a shared reference to the data at any time 
	pub(crate) unsafe fn new(inner: XarHandle<'a, GrugEntity>) -> Self {
		Self(inner)
	}

	pub(crate) fn into_inner(self) -> XarHandle<'a, GrugEntity> {
		self.0
	}
}

impl<'a> AsRef<GrugEntity> for GrugEntityHandle<'a> {
	fn as_ref(&self) -> &GrugEntity {
		unsafe{self.0.get_ref()}
	}
}

impl<'a> std::ops::Deref for GrugEntityHandle<'a> {
	type Target = GrugEntity;
	fn deref(&self) -> &Self::Target {
		unsafe{self.0.get_ref()}
	}
}

#[derive(Debug)]
pub struct GrugEntity {
	pub id: GrugId,
	pub file_id: GrugScriptId,
	pub members: Cell<NonNull<()>>,
}

impl GrugEntity {
	/// SAFETY: The members of the returned entity are uninitialized
	/// This data must be initialized before it is actually used as an entity
	pub unsafe fn new_uninit(id: GrugId, file_id: GrugScriptId) -> Self {
		Self {
			id,
			file_id,
			members: Cell::new(NonNull::dangling())
		}
	}
}
