//! Defines the types shared by all implementations of grug.h
use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use std::ffi::c_void;
use crate::ntstring::NTStrPtr;
use crate::state::State;
use crate::ast::Type;

/// A function pointer to a game function
/// Game functions have one the following signature
/// ```text
/// extern "C" fn (&GrugState, *const Value) -> Value;
/// ```
///
/// This is the type erased version of [`HostFnWithState`] for use in the AST.
/// 
/// Conversion from [`HostFnWithState`] is done using [`Self::from_ptr`]
/// 
#[derive(Clone, Copy, Hash, Eq)]
#[repr(transparent)]
pub struct HostFn(ErasedHostFnPtr);

impl PartialEq for HostFn {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}
impl std::ops::Deref for HostFn {
    type Target = ErasedHostFnPtr;
    fn deref(&self) -> &ErasedHostFnPtr {
        &self.0
    }
}

/// SAFETY: This function should only be called with the same state type and
/// number of generics it was originally created for
type ErasedHostFnPtr = unsafe extern "C" fn (*const c_void, *const Value, *const Type<'static>) -> Value;
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
pub type HostFnWithState<const N: usize, GrugState> = extern "C" fn (&GrugState, *const Value, generics: &'static [Type<'static>; N]) -> Value;

impl HostFn {
    /// Type erases a [`HostFnWithState`]
    pub const fn from_erased_ptr(value: ErasedHostFnPtr) -> Self {
        Self(value)
    }
    /// Type erases a [`HostFnWithState`]
    pub const fn from_ptr<const N: usize, GrugState: State>(value: HostFnWithState<N, GrugState>) -> Self {
        Self(unsafe{std::mem::transmute::<HostFnWithState<N, GrugState>, ErasedHostFnPtr>(value)})
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
/// This id is used to indicate that a particular grug file had a compilation error
pub const INVALID_GRUG_FILE_ID: FileId = FileId::new(u64::MAX);

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Id {
    /// Create a new id with the the input value
    pub const fn new(id: u64) -> Self {
        Self(id)
    }

    /// Conversts the id to its inner value
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
    /// As a number
    pub number: c_double,
    /// As a boolean
    pub bool: u8,
    /// As an Id
    pub id: Id,
    /// As a raw pointer
    pub custom_type: *mut (),
    /// As raw bytes
    pub bytes: [u8; 8],
    /// As a string pointer
    pub string: NTStrPtr<'static>,
    /// As empty data
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
        }
    }
}
