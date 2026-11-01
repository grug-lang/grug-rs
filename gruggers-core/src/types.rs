//! Defines the types shared by all implementations of grug.h
use crate::ast::Type;
use crate::ntstring::NTStrPtr;
use crate::state::State;
use std::cell::Cell;
use std::ffi::c_double;
use std::ffi::c_void;
use std::ptr::NonNull;

/// A Type erased version of [`RegFn`]
pub type ErasedRegFn = unsafe extern "C" fn(*const Type<'static>) -> Option<ErasedHostFn>;

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
/// The number of types provided is the number of generics for the function in
/// the mod_api. For methods, if the method does not define any generics, then
/// the parent's generics are inherited.
///
/// Returning `None` (NULL in C) means there is no specialized version for these
/// generics, so the host function pointer registered for the same function is
/// called instead. If there is none, the call is a compile error.
pub type RegFn<const N: usize, State> =
    extern "C" fn(&[Type<'static>; N]) -> Option<HostFn<N, State>>;

/// Type erases a registration function pointer
pub fn erase_reg_fn<const N: usize, GrugState: State>(reg_fn: RegFn<N, GrugState>) -> ErasedRegFn {
    unsafe { std::mem::transmute::<RegFn<N, GrugState>, ErasedRegFn>(reg_fn) }
}

/// A type erased function pointer to a host function
/// Host functions have the following signature
/// ```text
/// extern "C" fn (&GrugState, *const Value, &[Type<'static>;N]) -> Value;
/// ```
///
/// This is the type erased version of [`HostFn`] for use in the AST.
///
/// Conversion from [`HostFn`] is done using [`erase_host_fn`].
pub type ErasedHostFn =
    unsafe extern "C" fn(*const c_void, *const Value, *const Type<'static>) -> Value;

/// Type erases a host function pointer
pub fn erase_host_fn<const N: usize, GrugState: State>(
    host_fn: HostFn<N, GrugState>,
) -> ErasedHostFn {
    unsafe { std::mem::transmute::<HostFn<N, GrugState>, ErasedHostFn>(host_fn) }
}

/// A Host fn pointer for a specific kind of state with a specific number of
/// generics. Each implementor of [`State`] should register its own version of
/// [`HostFn`].
///
/// [`HostFn`] can be cast to use any state but it is UB to cast to any
/// state other than the current state the pointer was recieved from.
pub type HostFn<const N: usize, GrugState> =
    extern "C" fn(&GrugState, *const Value, generics: &'static [Type<'static>; N]) -> Value;

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

/// The mode a compiled file's code runs in.
///
/// The mode is decided when the file compiles and travels with the compiled
/// file, so a host decides it per file and never per state: two files of the
/// same state can run in different modes, and a file that is recompiled with
/// another mode runs in that one from then on. Switching a whole mod's mode is
/// therefore a recompile of its files, not a state-wide switch.
///
/// Safe mode is the sandbox, and it is what every caller gets unless it asks
/// for [`Mode::Unsafe`]:
///
/// - an on function that runs longer than [`ON_FN_TIME_LIMIT`]
///   (100 ms) is stopped with a time limit error,
/// - grug code that recurses deeper than [`MAX_RECURSION_LIMIT`] (100
///   call frames) is stopped with a stack overflow error,
/// - a host function error unwinds the grug code that called the host
///   function.
///
/// Unsafe mode skips exactly those three checks and nothing else, which is what
/// a host grants a mod for speed: the checks cost a compare and a masked
/// counter per instruction, and a file that is correct without them needs them
/// neither. Everything a caller observes is otherwise unchanged:
///
/// - the same file compiles to the same program, and a compile error is the
///   same error in both modes,
/// - grug's values, arithmetic, entity and resource semantics and the mod API
///   are the same in both modes,
/// - the file's member variables and its export functions behave the same way
///   while the file stays within the limits, so a correct program produces
///   exactly the same values in both modes,
/// - a host function error is still reported to the host's runtime error
///   handler in unsafe mode; what unsafe mode drops is the unwinding, so the
///   calling script carries on with the value the host function returned
///   instead of being abandoned. The host can therefore still tell that the
///   error happened, and a script that ignores what its host function returned
///   after an error goes on with a value it was never meant to receive.
///
/// What unsafe mode drops is bounded: a script that recursed too deeply can
/// exhaust the process stack and one that never returns can hang the process,
/// which is the point of unsafe mode and why a host must only grant it to code
/// the user trusts.
///
/// [`ON_FN_TIME_LIMIT`]: crate::runtime_error::ON_FN_TIME_LIMIT
/// [`MAX_RECURSION_LIMIT`]: crate::runtime_error::MAX_RECURSION_LIMIT
/// [`ON_FN_TIME_LIMIT`]: crate::runtime_error::ON_FN_TIME_LIMIT
/// [`MAX_RECURSION_LIMIT`]: crate::runtime_error::MAX_RECURSION_LIMIT
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Mode {
    /// The sandbox: the time limit, the recursion limit and host function
    /// errors are enforced.
    #[default]
    Safe = 0,
    /// The time limit, the recursion limit and host function errors do not stop
    /// the script. See the type's documentation for what else does not change.
    Unsafe = 1,
}

impl Mode {
    /// Whether this is [`Mode::Safe`], where every check a host relies on is
    /// enforced.
    pub const fn is_safe(self) -> bool {
        matches!(self, Mode::Safe)
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
