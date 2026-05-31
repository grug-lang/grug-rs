//! Defines the types shared by all implementations of grug.h
use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use std::marker::PhantomPinned;
use crate::ntstring::NTStrPtr;
use crate::state::State;
use crate::ast::GrugType;

// TODO: Remove the "Grug" prefix from these types

/// A function pointer to a game function
/// Game functions have one the following signature
/// ```text
/// extern "C" fn (&GrugState, *const GrugValue) -> GrugValue;
/// ```
///
/// This is the type erased version of [`GameFnPtrState`] for use in the AST.
/// 
/// Conversion to and from [`GameFnPtrState`] is done using [`Self::as_ptr`] and [`Self::from_ptr`]
/// 
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct GameFnPtr(NonNull<()>);
// SAFETY: GameFnPtr is always just a function pointer
unsafe impl Send for GameFnPtr {}
unsafe impl Sync for GameFnPtr {}
/// A Game fn pointer for a specific kind of state. Each implementor of
/// [`State`] should register its own version of [`GameFnPtrState`].
///
/// [`GameFnPtr`] can be cast to use any state but it is UB to cast to any
/// state other than the current state the pointer was recieved from.
/// 
/// When Backends are running an export function, [`GameFnPtrState`] should be
/// cast to the same kind of state used in `call_on_function`.
pub type GameFnPtrState<GrugState> = extern "C" fn (&'static (), &GrugState, *const GrugValue) -> GrugValue;

pub struct HostFnStruct<F, Inputs, Output, State>(F, std::marker::PhantomData<(Inputs, Output, State)>);
#[diagnostic::on_unimplemented(
	message = "{Self} does not contain a valid host function",
	label = "this function",
	note = "the function signature should be of the form fn(&GrugState, ...impl FromGrugValue) -> impl IntoGrugValue",
	note = "the remaining arguments need to implement FromGrugValue"
)]
pub trait IntoHostFn<GrugState: State> {
	const PARAMETERS: &'static [GrugType<'static>];
	const RETURN_TYPE: GrugType<'static>;
	fn into_host_fn(self) -> impl Fn(&GrugState, *const GrugValue) -> GrugValue;
}

pub trait IntoGrugValue {
	const TYPE: GrugType<'static>;
	fn into(self) -> GrugValue;
}
pub trait FromGrugValue {
	const TYPE: GrugType<'static>;
	unsafe fn from(ptr: &mut *const GrugValue) -> Self;
}
mod trait_impls {
	#![allow(non_snake_case)]
	use super::*;
	use crate::ntstring::NTStr;
	impl IntoGrugValue for () {
		const TYPE: GrugType<'static> = GrugType::Void;
		fn into(self) -> GrugValue {
			GrugValue{void: ()}
		}
	}
	impl IntoGrugValue for &'static NTStr {
		const TYPE: GrugType<'static> = GrugType::String;
		fn into(self) -> GrugValue {
			GrugValue{string: self.as_ntstrptr()}
		}
	}
	impl IntoGrugValue for f64 {
		const TYPE: GrugType<'static> = GrugType::Number;
		fn into(self) -> GrugValue {
			GrugValue{number: self}
		}
	}
	impl IntoGrugValue for f32 {
		const TYPE: GrugType<'static> = GrugType::Number;
		fn into(self) -> GrugValue {
			GrugValue{number: self as f64}
		}
	}
	impl IntoGrugValue for bool {
		const TYPE: GrugType<'static> = GrugType::Bool;
		fn into(self) -> GrugValue {
			GrugValue{bool: self as u8}
		}
	}

	impl<'a> FromGrugValue for &'a str {
		const TYPE: GrugType<'static> = GrugType::String;
		unsafe fn from(ptr: &mut *const GrugValue) -> Self {
			let value = unsafe{ptr.read().string.to_str()};
			*ptr = unsafe{ptr.add(1)};
			value
		}
	}
	impl<'a> FromGrugValue for &'a NTStr {
		const TYPE: GrugType<'static> = GrugType::String;
		unsafe fn from(ptr: &mut *const GrugValue) -> Self {
			let value = unsafe{ptr.read().string.to_ntstr()};
			*ptr = unsafe{ptr.add(1)};
			value
		}
	}
	impl<'a> FromGrugValue for f64 {
		const TYPE: GrugType<'static> = GrugType::Number;
		unsafe fn from(ptr: &mut *const GrugValue) -> Self {
			let value = unsafe{ptr.read().number};
			*ptr = unsafe{ptr.add(1)};
			value
		}
	}
	impl<'a> FromGrugValue for bool {
		const TYPE: GrugType<'static> = GrugType::Bool;
		unsafe fn from(ptr: &mut *const GrugValue) -> Self {
			let value = unsafe{ptr.read().bool} != 0;
			*ptr = unsafe{ptr.add(1)};
			value
		}
	}

	macro_rules! impl_host_fn_traits {
		($($inputs: ident),*, -> O) => {
			#[diagnostic::do_not_recommend]
			impl<F: Fn(&GrugState, $($inputs),*) -> O, $($inputs: FromGrugValue,)* GrugState: State, O: IntoGrugValue> From<F> for HostFnStruct<F, ($($inputs,)*), O, GrugState>{
				fn from (other: F) -> Self {
					Self(other, std::marker::PhantomData)
				}
			}
			#[diagnostic::do_not_recommend]
			impl<F: Fn(&GrugState, $($inputs),*) -> O, $($inputs: FromGrugValue,)* GrugState: State, O: IntoGrugValue> IntoHostFn<GrugState> for HostFnStruct<F, ($($inputs, )*), O, GrugState> {
				const PARAMETERS: &'static [GrugType<'static>] = &[];
				const RETURN_TYPE: GrugType<'static> = O::TYPE;
				fn into_host_fn(self) -> impl Fn(&GrugState, *const GrugValue) -> GrugValue {
					move |state, mut _args| {
						$(
							let $inputs = unsafe{$inputs::from(&mut _args)};
						)*
						self.0(state, $($inputs),*).into()
					}
				}
			}
		}
	}

	impl_host_fn_traits!(, -> O);
	impl_host_fn_traits!(I0, -> O);
	impl_host_fn_traits!(I0, I1, -> O);
	impl_host_fn_traits!(I0, I1, I2, -> O);
	impl_host_fn_traits!(I0, I1, I2, I3, -> O);
	impl_host_fn_traits!(I0, I1, I2, I3, I4, -> O);
	impl_host_fn_traits!(I0, I1, I2, I3, I4, I5, -> O);
	impl_host_fn_traits!(I0, I1, I2, I3, I4, I5, I6, -> O);
	impl_host_fn_traits!(I0, I1, I2, I3, I4, I5, I6, I7, -> O);
}

impl GameFnPtr {
	/// Casts `self` to a [`GameFnPtrState`] for the input state
	/// 
	/// # Safety
	/// The input type must be compatible with the type used to construct
	/// `self`
	pub const unsafe fn as_ptr<GrugState: State>(self) -> GameFnPtrState<GrugState> {
		unsafe{std::mem::transmute::<NonNull<()>, GameFnPtrState<GrugState>>(self.0)}
	}

	/// Type erases a [`GameFnPtrState`]
	pub const fn from_ptr<GrugState: State>(value: GameFnPtrState<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute::<GameFnPtrState<GrugState>, NonNull<()>>(value)})
	}

	/// converts the pointer into a usize without exposing provenance
	pub fn as_usize(self) -> usize {
		self.0.as_ptr().addr()
	}
}

impl std::fmt::Debug for GameFnPtr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

/// Represents a handle to an object owned by grug
/// Can refer to grug entities, grug files, on functions, or game objects
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct GrugId(pub u64);

/// An id that uniquely refers to a script path. 
pub type GrugFileId = GrugId;
pub const INVALID_GRUG_SCRIPT_ID: GrugFileId = GrugFileId::new(u64::MAX);

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
	pub file_id: GrugFileId,
	/// Pointer to the entity's members stored by the backend
	pub members: Cell<NonNull<()>>,
	pub _marker: PhantomPinned,
}

impl GrugEntity {
	/// # SAFETY 
	/// The `members` field of the returned entity are uninitialized
	/// This data must be initialized by the backend before it is actually used
	/// as an entity
	pub unsafe fn new_uninit(id: GrugId, file_id: GrugFileId) -> Self {
		Self {
			id,
			file_id,
			members: Cell::new(NonNull::dangling()),
			_marker: PhantomPinned,
		}
	}
}
