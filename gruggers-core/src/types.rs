//! Defines the types shared by all implementations of grug.h
use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use crate::ntstring::NTStrPtr;
use crate::state::State;

// TODO: Remove the "Grug" prefix from these types

/// A function pointer to a game function
/// Game functions have one of the following 4 signatures
/// ```
/// extern "C" fn (&GrugState);
/// extern "C" fn (&GrugState, *const GrugValue);
/// extern "C" fn (&GrugState) -> GrugValue;
/// extern "C" fn (&GrugState, *const GrugValue) -> GrugValue;
/// ```
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct GameFnPtr(NonNull<()>);

impl GameFnPtr {
	pub const unsafe fn void<GrugState: State>(self) -> GameFnPtrVoid<GrugState> {
		unsafe{std::mem::transmute(self.0)}
	}
	pub const unsafe fn void_argless<GrugState: State>(self) -> GameFnPtrVoidArgless<GrugState> {
		unsafe{std::mem::transmute(self.0)}
	}
	pub const unsafe fn value<GrugState: State>(self) -> GameFnPtrValue<GrugState> {
		unsafe{std::mem::transmute(self.0)}
	}
	pub const unsafe fn value_argless<GrugState: State>(self) -> GameFnPtrValueArgless<GrugState> {
		unsafe{std::mem::transmute(self.0)}
	}

	pub const fn from_void<GrugState: State>(value: GameFnPtrVoid<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute(value)})
	}
	pub const fn from_void_argless<GrugState: State>(value: GameFnPtrVoidArgless<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute(value)})
	}
	pub const fn from_value<GrugState: State>(value: GameFnPtrValue<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute(value)})
	}
	pub const fn from_value_argless<GrugState: State>(value: GameFnPtrValueArgless<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute(value)})
	}
}

impl std::fmt::Debug for GameFnPtr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl PartialEq for GameFnPtr {
	fn eq(&self, other: &Self) -> bool {
		const _: () = const{assert!(size_of::<GameFnPtr>() == size_of::<usize>())};
		std::ptr::eq(self.0.as_ptr(), other.0.as_ptr())
		// unsafe{std::mem::transmute::<Self, usize>(*self) == std::mem::transmute::<Self, usize>(*other)}
	}
}

mod from_impls {
	use super::*;
	impl<GrugState: State> From<GameFnPtrVoid<GrugState>> for GameFnPtr {
		fn from (value: GameFnPtrVoid<GrugState>) -> Self {
			Self(unsafe{std::mem::transmute(value)})
		}
	}

	impl<GrugState: State> From<GameFnPtrVoidArgless<GrugState>> for GameFnPtr {
		fn from (value: GameFnPtrVoidArgless<GrugState>) -> Self {
			Self(unsafe{std::mem::transmute(value)})
		}
	}

	impl<GrugState: State> From<GameFnPtrValue<GrugState>> for GameFnPtr {
		fn from (value: GameFnPtrValue<GrugState>) -> Self {
			Self(unsafe{std::mem::transmute(value)})
		}
	}

	impl<GrugState: State> From<GameFnPtrValueArgless<GrugState>> for GameFnPtr {
		fn from (value: GameFnPtrValueArgless<GrugState>) -> Self {
			Self(unsafe{std::mem::transmute(value)})
		}
	}
}

/// Alias for `extern "C" fn (&GrugState, *const GrugValue)`
pub type GameFnPtrVoid<GrugState> = extern "C" fn (state: &GrugState, args: *const GrugValue);
/// Alias for `extern "C" fn (&GrugState)`
pub type GameFnPtrVoidArgless<GrugState> = extern "C" fn (state: &GrugState);
/// Alias for `extern "C" fn (&GrugState, *const GrugValue) -> GrugValue`
pub type GameFnPtrValue<GrugState> = extern "C" fn (state: &GrugState, args: *const GrugValue) -> GrugValue;
/// Alias for `extern "C" fn (&GrugState) -> GrugValue`
pub type GameFnPtrValueArgless<GrugState> = extern "C" fn (state: &GrugState) -> GrugValue;

/// Represents a handle to an object owned by grug
/// Can refer to grug entities, grug files, on functions, or game objects
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct GrugId(pub u64);

// TODO: Rename this to GrugFileId
/// An id that uniquely refers to a script path. 
pub type GrugScriptId = GrugId;

impl std::fmt::Display for GrugId {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl GrugId {
	pub const fn new(id: u64) -> Self {
		Self(id)
	}

	pub const fn to_inner(self) -> u64 {
		self.0
	}
}

/// Uniquely refers to a particular on function from a particular entity from
/// the mod_api. 
/// Two different entities will have unique OnFnIds for all their on functions
pub type GrugOnFnId = u64;

// TODO: Provide the ability to disable some of these fields and change the size of the fields
// TODO: Should this be parametrised by the lifetime?. This could be useful for
// game functions to make sure they don't store the string in a static without copying it out.
/// In memory representation of a grug value. This is untagged because the
/// typechecker ensures all types are valid.
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

/// Entity data owned by the state. Entity members are stored by the backend
/// and a pointer to it is stored in `members`
#[derive(Debug)]
pub struct GrugEntity {
	/// id of the `me` member variable in a grug_script
	pub id: GrugId,
	/// File id of file this entity is created from 
	pub file_id: GrugScriptId,
	/// Pointer to the entity's members stored by the backend
	pub members: Cell<NonNull<()>>,
}

impl GrugEntity {
	/// SAFETY: The `members` field of the returned entity are uninitialized
	/// This data must be initialized by the backend before it is actually used
	/// as an entity
	pub unsafe fn new_uninit(id: GrugId, file_id: GrugScriptId) -> Self {
		Self {
			id,
			file_id,
			members: Cell::new(NonNull::dangling())
		}
	}
}
