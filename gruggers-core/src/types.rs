//! Defines the types shared by all implementations of grug.h
use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use std::marker::PhantomPinned;
use crate::ntstring::NTStrPtr;
use crate::state::State;
use crate::ast::Type;


/// A function pointer to a function that provides specialized versions of
/// generic host functions
///
/// This function is called after type inference has determined all relevant
/// generic types to obtain the actual host function pointer for the function
/// call.
///
/// Grug implementations are allowed to cache the results of these function
/// calls, so providers must ensure these functions are pure. 
///
/// The argument is a pointer to an array of grug types. These types indicate
/// the generic parameters associated with this specific host function call
///
/// The number of types provided is determined by the mod_api.
///
/// For normal host functions, it is the number of elements in the
/// "used_generics" field of the host_function
///
/// For methods, it is the number of elements in the "used_generics"
/// field of the class and the method combined.
pub type HostFnReg<const N: usize, State> = for<'a> extern "C" fn (&'a [Type<'a>; N]) -> Option<HostFnWithState<State>>;

/// Type erased version of HostFnReg
///
/// This is the version of HostFnReg that grug.h exports. 
/// 
/// [`HostFnReg`] can be transmuted into [`HostFnRegErased`] and it is sound
/// to call as long as the number of elements provided to the function is the
/// same as the generic `N` in [`HostFnReg`]
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct HostFnRegErased(for<'a> unsafe extern "C" fn (*const Type<'a>) -> Option<HostFn>);

impl<const N: usize, GrugState: State> From<HostFnReg<N, GrugState>> for HostFnRegErased {
	fn from(other: HostFnReg<N, GrugState>) -> HostFnRegErased {
		// HostFnReg has the exact same ABI as HostFnRegErased.
		//
		// HostFnRegErased is unsafe. The requirement for calling it is that
		// the number of generics expected by HostFnReg (N).
		//
		// As long as the unsafe precondition is met, the call is safe
		unsafe{std::mem::transmute::<HostFnReg<N, GrugState>, HostFnRegErased>(other)}
	}
}
impl From<for<'a> unsafe extern "C" fn (*const Type<'a>) -> Option<HostFn>> for HostFnRegErased {
	fn from(other: for<'a> unsafe extern "C" fn (*const Type<'a>) -> Option<HostFn>) -> HostFnRegErased {
		Self(other)
	}
}


impl std::ops::Deref for HostFnRegErased {
	type Target = unsafe extern "C" fn (*const Type) -> Option<HostFn>;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// A function pointer to a game function
/// Game functions have one the following signature
/// ```text
/// extern "C" fn (&GrugState, *const Value) -> Value;
/// ```
///
/// This is the type erased version of [`HostFnWithState`] for use in the AST.
/// 
/// Conversion to and from [`HostFnWithState`] is done using [`Self::as_ptr`] and [`Self::from_ptr`]
/// 
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct HostFn(NonNull<()>);
// SAFETY: HostFn is always just a function pointer
unsafe impl Send for HostFn {}
unsafe impl Sync for HostFn {}
/// A Game fn pointer for a specific kind of state. Each implementor of
/// [`State`] should register its own version of [`HostFnWithState`].
///
/// [`HostFn`] can be cast to use any state but it is UB to cast to any
/// state other than the current state the pointer was recieved from.
/// 
/// When Backends are running an export function, [`HostFnWithState`] should be
/// cast to the same kind of state used in `call_on_function`.
pub type HostFnWithState<GrugState> = extern "C" fn (&GrugState, *const Value) -> Value;

impl HostFn {
	/// Casts `self` to a [`HostFnWithState`] for the input state
	/// 
	/// # Safety
	/// The input type must be compatible with the type used to construct
	/// `self`
	pub const unsafe fn as_ptr<GrugState: State>(self) -> HostFnWithState<GrugState> {
		unsafe{std::mem::transmute::<NonNull<()>, HostFnWithState<GrugState>>(self.0)}
	}

	/// Type erases a [`HostFnWithState`]
	pub const fn from_ptr<GrugState: State>(value: HostFnWithState<GrugState>) -> Self {
		Self(unsafe{std::mem::transmute::<HostFnWithState<GrugState>, NonNull<()>>(value)})
	}

	/// converts the pointer into a usize without exposing provenance
	pub fn as_usize(self) -> usize {
		self.0.as_ptr().addr()
	}
}

impl std::fmt::Debug for HostFn {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

/// Represents a handle to an object owned by grug
/// Can refer to grug entities, grug files, on functions, or game objects
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Id(pub u64);

/// An id that uniquely refers to a script path. 
pub type FileId = Id;
pub const INVALID_GRUG_SCRIPT_ID: FileId = FileId::new(u64::MAX);

impl std::fmt::Display for Id {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl Id {
	pub const fn new(id: u64) -> Self {
		Self(id)
	}

	pub const fn to_inner(self) -> u64 {
		self.0
	}
}

/// Uniquely refers to a particular on function from a particular entity from
/// the mod_api. 
/// Two different entities will have unique ExportFnIds for all their on functions
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(transparent)]
pub struct ExportFnId(pub u64);

// TODO: Provide the ability to disable some of these fields and change the size of the fields
// TODO: Should this be parametrised by the lifetime?. This could be useful for
// game functions to make sure they don't store the string in a static without copying it out.
/// In memory representation of a grug value. This is untagged because the
/// typechecker ensures all types are valid.
#[derive(Clone, Copy)]
#[repr(C)]
pub union Value {
	pub number: c_double,
	pub bool: u8,
	pub id: Id,
	pub custom_type: *mut (),
	pub bytes: [u8; 8],
	pub string: NTStrPtr<'static>,
	pub void: (),
}

/// Entity data owned by the state. Entity members are stored by the backend
/// and a pointer to it is stored in `members`
#[derive(Debug)]
pub struct GrugEntity {
	/// id of the `me` member variable in a grug_script
	pub id: Id,
	/// File id of file this entity is created from 
	pub file_id: FileId,
	/// Pointer to the entity's members stored by the backend
	pub members: Cell<NonNull<()>>,
	pub _marker: PhantomPinned,
}

impl GrugEntity {
	/// # SAFETY 
	/// The `members` field of the returned entity are uninitialized
	/// This data must be initialized by the backend before it is actually used
	/// as an entity
	pub unsafe fn new_uninit(id: Id, file_id: FileId) -> Self {
		Self {
			id,
			file_id,
			members: Cell::new(NonNull::dangling()),
			_marker: PhantomPinned,
		}
	}
}

pub struct GrugStr {
	inner: NonNull<u8>,
}
#[repr(C)]
struct GrugStrInner {
	ref_count: Cell<usize>,
	len: usize,
	str: [u8;0],
}

impl std::ops::Deref for GrugStr {
	type Target = str;
	fn deref(&self) -> &str {
		let len = unsafe{self.inner.cast::<usize>().sub(1).read()};
		unsafe{std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.inner.as_ptr(), len))}
	}
}

impl GrugStr {
	pub fn from_str(input: &str) -> Self {
		let alloc = std::alloc::Layout::new::<GrugStrInner>().extend(std::alloc::Layout::array::<u8>(input.len() + 1).unwrap())
			.expect("Could not create layout").0;
		let ptr = unsafe{std::alloc::alloc(alloc).cast::<GrugStrInner>()};
		if ptr.is_null() {
			panic!("Could not allocate memory");
		}
		unsafe{ptr.write(GrugStrInner{ref_count: Cell::new(1), len: input.len(), str: []})}
		let ret_ptr = unsafe{&raw mut (*ptr).str as *mut u8};
		unsafe{ret_ptr.copy_from(input.as_ptr(), input.len())};
		unsafe{ret_ptr.add(input.len()).write(b'\0')};
		Self {
			inner: unsafe{NonNull::new_unchecked(ret_ptr)},
		}
	}
}
