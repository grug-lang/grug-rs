//! Defines traits and types for working with backends from a state
use crate::types::{GrugScriptId, GrugEntity, GrugValue};
use crate::ast::GrugAst;
use crate::state::{State, DummyState};
use crate::runtime_error::RuntimeError;
use crate::ntstring::{NTStrPtr, NTStr};

use std::ptr::NonNull;

/// Interface of backends
pub trait Backend {
	/// The AST of a typechecked grug file is provided to let the backend do
	/// further transforms and lower to bytecode or even machine code
	/// 
	/// The script ids are guaranteed to be in contiguous ascending order.
	///
	/// If the same script id is returned again, then it means the old script
	/// associated with the id should be destroyed and replaced with this one. 
	///
	/// The entity data of all entities created from the old script should be
	/// regenerated
	fn insert_file(&self, id: GrugScriptId, file: GrugAst<'_>);
	/// Initialize the member data of the newly created entity. When this
	/// function is called, the member field of `entity` points to garbage and
	/// must not be deinitialized. The GrugScriptId to be used is obtained from
	/// the file_id member of `entity`. 
	///
	/// Returns false if there was a runtime error during execution
	#[must_use]
	fn init_entity<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity) -> bool;
	/// Deinitialize all the data associated with all entities. The pointers
	/// stored during `init_entity` must be used to get access to the entity data.
	/// The entities can only be accessed as a &GrugEntity even self is available with an exclusive reference
	fn clear_entities(&mut self);
	/// Deinitialize the data associated with `entity`. 
	fn destroy_entity_data(&self, entity: &GrugEntity);

	/// Run the on function at index `on_fn_index` of the script associated
	/// with `entity`.
	///
	/// # SAFETY: `values` must point to an array of GrugValues of at least as
	/// many elements as the number of arguments to the on_ function
	///
	/// If the number of arguments is 0, then `values` is allowed to be null
	#[must_use]
	unsafe fn call_on_function_raw<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool;
	/// Run the on function at index `on_fn_index` of the script associated
	/// with `entity`.
	///
	/// # Panics: The length of `values` must exactly match the number of
	/// expected arguments to the on_ function
	#[must_use]
	fn call_on_function<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool;
}

// This check ensures that c code can safely zero the backend field in GrugInitSettings
const _: () = unsafe{const {std::mem::forget(std::mem::MaybeUninit::<Option<ErasedBackend<DummyState>>>::zeroed().assume_init())}};
const _: () = const {assert!(std::mem::size_of::<Option<ErasedBackend<DummyState>>>() == std::mem::size_of::<ErasedBackend<DummyState>>())};

/// C-api compatible version of `&dyn [Backend]`
#[repr(C)]
pub struct ErasedBackend<GrugState: State + 'static> {
	pub data: NonNull<()>,
	pub vtable: &'static BackendVTable<GrugState>,
}

/// C-api compatible version of the vtable of [`&dyn Backend`]
#[repr(C)]
pub struct BackendVTable<GrugState: State> {
	#[allow(improper_ctypes_definitions)]
	/// See [`Backend::insert_file`]
	pub(crate) insert_file         : extern "C" fn(data: NonNull<()>, id: GrugScriptId, file: GrugAst<'_>),
	/// See [`Backend::init_entity`]
	pub(crate) init_entity         : extern "C" fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> bool,
	/// See [`Backend::clear_entities`]
	pub(crate) clear_entities      : extern "C" fn(data: NonNull<()>),
	/// See [`Backend::destroy_entity_data`]
	pub(crate) destroy_entity_data : extern "C" fn(data: NonNull<()>, entity: &GrugEntity),
	/// See [`Backend::call_on_function_raw`]
	///
	/// SAFETY: `values` must point to a buffer of at least as many values as `on_fn_index` expects
	pub(crate) call_on_function_raw: for<'a> unsafe extern "C" fn(data: NonNull<()>, state: &'a GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool,
	/// See [`Backend::call_on_function`]
	pub(crate) call_on_function    : for<'a> fn(data: NonNull<()>, state: &'a GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool,
	/// destroys the resources owned by the backend
	pub(crate) drop                : extern "C" fn(data: NonNull<()>),
}

impl<GrugState: State> ErasedBackend<GrugState> {
	/// See [`Backend::insert_file`]
	pub fn insert_file(&self, id: GrugScriptId, file: GrugAst<'_>) {
		(self.vtable.insert_file)(self.data, id, file)
	}
	/// See [`Backend::init_entity`]
	pub fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> bool {
		(self.vtable.init_entity)(self.data, state, entity)
	}
	/// See [`Backend::clear_entities`]
	pub fn clear_entities(&mut self) {
		(self.vtable.clear_entities)(self.data)
	}
	/// See [`Backend::destroy_entity_data`]
	pub fn destroy_entity_data(&self, entity: &GrugEntity) {
		(self.vtable.destroy_entity_data)(self.data, entity)
	}
	/// See [`Backend::call_on_function_raw`]
	///
	/// SAFETY: `values` must point to a buffer of at least as many values as `on_fn_index` expects
	pub unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		unsafe{(self.vtable.call_on_function_raw)(self.data, state, entity, on_fn_index, values)}
	}
	/// See [`Backend::call_on_function`]
	pub fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		(self.vtable.call_on_function)(self.data, state, entity, on_fn_index, values)
	}
}

impl<GrugState: State> Drop for ErasedBackend<GrugState> {
	fn drop (&mut self) {
		(self.vtable.drop)(self.data)
	}
}

impl<T: Backend, GrugState: State> From<T> for ErasedBackend<GrugState> {
	fn from(other: T) -> Self {
		#[allow(improper_ctypes_definitions)]
		extern "C" fn insert_file<T: Backend, GrugState: State>(data: NonNull<()>, id: GrugScriptId, file: GrugAst<'_>) {
			T::insert_file(
				unsafe{data.cast::<T>().as_ref()},
				id,
				file
			)
		}

		extern "C" fn init_entity<T: Backend, GrugState: State>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> bool {
			T::init_entity::<GrugState>(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity
			)
		}

		extern "C" fn clear_entities<T: Backend, GrugState: State>(data: NonNull<()>) {
			T::clear_entities(
				unsafe{data.cast::<T>().as_mut()},
			)
		}

		extern "C" fn destroy_entity_data<T: Backend, GrugState: State>(data: NonNull<()>, entity: &GrugEntity) {
			T::destroy_entity_data(
				unsafe{data.cast::<T>().as_ref()},
				entity
			)
		}
		/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
		unsafe extern "C" fn call_on_function_raw<T: Backend, GrugState: State>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
			unsafe{T::call_on_function_raw::<GrugState>(
				data.cast::<T>().as_ref(),
				state, 
				entity,
				on_fn_index,
				values
			)}
		}
		fn call_on_function<T: Backend, GrugState: State>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
			T::call_on_function::<GrugState>(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity,
				on_fn_index,
				values
			)
		}
		// destroys the resources owned by the backend
		extern "C" fn drop<T: Backend, GrugState: State>(data: NonNull<()>) {
			_ = unsafe{Box::from_raw(data.cast::<T>().as_ptr())};
		}

		Self {
			data: unsafe{NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>()},
			vtable: &BackendVTable {
				insert_file         : insert_file::<T, GrugState>,
				init_entity         : init_entity::<T, GrugState>,
				clear_entities      : clear_entities::<T, GrugState>,
				destroy_entity_data : destroy_entity_data::<T, GrugState>,
				call_on_function_raw: call_on_function_raw::<T, GrugState>,
				call_on_function    : call_on_function::<T, GrugState>,
				drop                : drop::<T, GrugState>,
			}
		}
	}
}

#[repr(transparent)]
pub struct CState(NonNull<()>);
impl State for CState {
	fn set_runtime_error(&self, _error: RuntimeError) {
		panic!("This is an error within gruggers_core");
	}
	fn is_errorring(&self) -> bool {
		panic!("This is an error within gruggers_core");
	}
}

struct CStateWithHandler {
	state: NonNull<CState>,
	set_runtime_error: for<'a> extern "C" fn (NonNull<CState>, u32, Option<NTStrPtr<'a>>),
	is_errorring: extern "C" fn (NonNull<CState>) -> bool,
}

impl State for CStateWithHandler {
	fn set_runtime_error(&self, error: RuntimeError) {
		match error {
			RuntimeError::StackOverflow |
			RuntimeError::ExceededTimeLimit => (self.set_runtime_error)(self.state, error.code(), None),
			RuntimeError::GameFunctionError{message} => {
				let string;
				let message = if let Some(message) = NTStr::from_str(message) {
					message
				} else {
					string = format!("{}\n", message);
					NTStr::from_str(&string).unwrap()
				};
				(self.set_runtime_error)(self.state, error.code(), Some(message.as_ntstrptr()));
			}
		}
	}
	fn is_errorring(&self) -> bool {
		(self.is_errorring)(self.state)
	}
}

pub struct CBackend<B: Backend> {
	set_runtime_error: for<'a> extern "C" fn (NonNull<CState>, u32, Option<NTStrPtr<'a>>),
	is_errorring: extern "C" fn (NonNull<CState>) -> bool,
	backend: B,
}

impl<B: Backend> From<CBackend<B>> for ErasedBackend<CState> {
	fn from(other: CBackend<B>) -> Self {
		#[allow(improper_ctypes_definitions)]
		extern "C" fn insert_file<B: Backend>(data: NonNull<()>, id: GrugScriptId, file: GrugAst<'_>) {
			B::insert_file(
				unsafe{&data.cast::<CBackend<B>>().as_ref().backend},
				id,
				file
			)
		}

		extern "C" fn init_entity<B: Backend>(data: NonNull<()>, state: &CState, entity: &GrugEntity) -> bool {
			B::init_entity::<CStateWithHandler>(
				unsafe{&data.cast::<CBackend<B>>().as_ref().backend},
				&CStateWithHandler{
					state: NonNull::from_ref(state), 
					set_runtime_error: unsafe{data.cast::<CBackend<B>>().as_ref().set_runtime_error},
					is_errorring: unsafe{data.cast::<CBackend<B>>().as_ref().is_errorring},
				}, 
				entity
			)
		}

		extern "C" fn clear_entities<B: Backend>(data: NonNull<()>) {
			B::clear_entities(
				unsafe{&mut data.cast::<CBackend<B>>().as_mut().backend},
			)
		}

		extern "C" fn destroy_entity_data<B: Backend>(data: NonNull<()>, entity: &GrugEntity) {
			B::destroy_entity_data(
				unsafe{&data.cast::<CBackend<B>>().as_ref().backend},
				entity
			)
		}
		/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
		unsafe extern "C" fn call_on_function_raw<B: Backend>(data: NonNull<()>, state: &CState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
			unsafe{B::call_on_function_raw::<CStateWithHandler>(
				&data.cast::<CBackend<B>>().as_ref().backend,
				&CStateWithHandler{
					state: NonNull::from_ref(state), 
					set_runtime_error: data.cast::<CBackend<B>>().as_ref().set_runtime_error,
					is_errorring: data.cast::<CBackend<B>>().as_ref().is_errorring,
				}, 
				entity,
				on_fn_index,
				values
			)}
		}
		fn call_on_function<B: Backend>(data: NonNull<()>, state: &CState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
			B::call_on_function::<CStateWithHandler>(
				unsafe{&data.cast::<CBackend<B>>().as_ref().backend},
				&CStateWithHandler{
					state: NonNull::from_ref(state), 
					set_runtime_error: unsafe{data.cast::<CBackend<B>>().as_ref().set_runtime_error},
					is_errorring: unsafe{data.cast::<CBackend<B>>().as_ref().is_errorring},
				}, 
				entity,
				on_fn_index,
				values
			)
		}
		// destroys the resources owned by the backend
		extern "C" fn drop<B: Backend>(data: NonNull<()>) {
			_ = unsafe{Box::from_raw(data.cast::<CBackend<B>>().as_ptr())};
		}

		Self {
			data: unsafe{NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>()},
			vtable: &BackendVTable {
				insert_file         : insert_file::<B>,
				init_entity         : init_entity::<B>,
				clear_entities      : clear_entities::<B>,
				destroy_entity_data : destroy_entity_data::<B>,
				call_on_function_raw: call_on_function_raw::<B>,
				call_on_function    : call_on_function::<B>,
				drop                : drop::<B>,
			}
		}
	}
}

pub fn erased_c_backend<B: Backend>(
	backend: B, 
	set_runtime_error: extern "C" fn (NonNull<CState>, u32, Option<NTStrPtr<'_>>),
	is_errorring: extern "C" fn (NonNull<CState>) -> bool,
) -> ErasedBackend<CState> {
	ErasedBackend::from(CBackend{
		backend,
		set_runtime_error,
		is_errorring,
	})
}

/// Exports an extern "C" function to create a backend.
///
/// This can be used to create a dll to replace the backend from a game that uses grug
#[macro_export]
macro_rules! export_backend {
	($backend: expr) => {
		#[unsafe(no_mangle)]
		pub extern "C" fn create_backend(
			set_runtime_error: extern "C" fn (::core::ptr::NonNull<$crate::backend::CState>, ::core::primitive::u32, ::core::option::Option<$crate::ntstring::NTStrPtr<'_>>),
			is_errorring: extern "C" fn (::core::ptr::NonNull<$crate::backend::CState>) -> ::core::primitive::bool,
		) -> $crate::backend::ErasedBackend<$crate::backend::CState> {
			$crate::backend::erased_c_backend($backend, set_runtime_error, is_errorring)
		}
	}
}
