use crate::types::{GrugScriptId, GrugEntity, GrugValue};
use crate::ast::GrugAst;
use crate::state::GrugState;

use std::ptr::NonNull;

// #[derive(Debug)]
// pub struct GrugAst {
// 	pub(crate) global_variables: Vec<GlobalVariable>,
// 	pub(crate) on_functions: Vec<Option<OnFunction>>,
// 	pub(crate) helper_functions: Vec<HelperFunction>,
// }

// pub mod interpreter;
// pub use interpreter::Interpreter;

pub mod bytecode;
pub use bytecode::BytecodeBackend;

pub unsafe trait Backend {
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
	fn insert_file(&self, state: &GrugState, id: GrugScriptId, file: GrugAst<'_>);
	/// Initialize the member data of the newly created entity. When this
	/// function is called, the member field of `entity` points to garbage and
	/// must not be deinitialized. The GrugScriptId to be used is obtained from
	/// the file_id member of `entity`. 
	///
	/// `entity` is pinned until it is deinitialized by a call to
	/// `destroy_entity_data` or `insert_file` with the same path as its
	/// current GrugScriptId. The reference must be stored as a raw pointer
	/// within self so that it can be used during `destroy_entity_data` to
	/// check for pointer equality. 
	/// It is safe to use that pointer as a &GrugEntity in the meantime.
	// TODO: This should pass in a pinned shared reference to strengthen the guarantee
	#[must_use]
	fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> bool;
	/// Deinitialize all the data associated with all entities. The pointers
	/// stored during `init_entity` must be used to get access to the entity data.
	/// The entities can only be accessed as a &GrugEntity even self is available with an exclusive reference
	fn clear_entities(&mut self);
	/// Deinitialize the data associated with `entity`. 
	///
	/// # IMPORTANT!!!!
	/// Before deinitializing the data, ensure that the address of `entity`
	/// matches the address of a pointer stored during a previous call to
	/// `init_entity`. If `entity` does not match any stored pointer, the data
	/// MUST NOT be deinitialized and the return value MUST BE `false`.
	///
	///	If true is returned, the data should be deinitialized and the pointer
	///	MUST be removed from storage.
	///
	/// It is safe to never deinitialize the data and return false everytime. 
	#[must_use]
	fn destroy_entity_data(&self, entity: &GrugEntity) -> bool;

	/// Run the on function at index `on_fn_index` of the script associated
	/// with `entity`.
	///
	/// # SAFETY: `values` must point to an array of GrugValues of at least as
	/// many elements as the number of arguments to the on_ function
	///
	/// If the number of arguments is 0, then `values` is allowed to be null
	#[must_use]
	unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool;
	/// Run the on function at index `on_fn_index` of the script associated
	/// with `entity`.
	///
	/// # Panics: The length of `values` must exactly match the number of
	/// expected arguments to the on_ function
	#[must_use]
	fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool;
}

// This check ensures that c code can safely zero the backend field in GrugInitSettings
const _: () = unsafe{const {std::mem::forget(std::mem::MaybeUninit::<Option<ErasedBackend>>::zeroed().assume_init())}};
const _: () = const {assert!(std::mem::size_of::<Option<ErasedBackend>>() == std::mem::size_of::<ErasedBackend>())};

#[repr(C)]
pub struct ErasedBackend {
	pub data: NonNull<()>,
	pub vtable: &'static BackendVTable,
}

#[repr(C)]
pub struct BackendVTable {
	/// SAFETY: `path` must be a utf-8 buffer that is valid to read for atleast `path_len`
	pub(crate) insert_file         : unsafe fn(data: NonNull<()>, state: &GrugState, id: GrugScriptId, file: GrugAst<'_>),
	pub(crate) init_entity         : extern "C" fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> bool,
	pub(crate) clear_entities      : extern "C" fn(data: NonNull<()>),
	pub(crate) destroy_entity_data : extern "C" fn(data: NonNull<()>, entity: &GrugEntity) -> bool,
	/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
	pub(crate) call_on_function_raw: unsafe extern "C" fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool,
	pub(crate) call_on_function    : fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool,
	// destroys the resources owned by the backend
	pub(crate) drop                : extern "C" fn(data: NonNull<()>),
}

impl ErasedBackend {
	pub fn insert_file(&self, state: &GrugState, id: GrugScriptId, file: GrugAst<'_>) {
		unsafe{(self.vtable.insert_file)(self.data, state, id, file)}
	}
	pub fn init_entity<'a>(&self, state: &'a GrugState, entity: &GrugEntity) -> bool {
		(self.vtable.init_entity)(self.data, state, entity)
	}
	pub fn clear_entities(&mut self) {
		(self.vtable.clear_entities)(self.data)
	}
	pub fn destroy_entity_data(&self, entity: &GrugEntity) -> bool {
		(self.vtable.destroy_entity_data)(self.data, entity)
	}
	pub unsafe fn call_on_function_raw(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
		unsafe{(self.vtable.call_on_function_raw)(self.data, state, entity, on_fn_index, values)}
	}
	pub fn call_on_function(&self, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
		(self.vtable.call_on_function)(self.data, state, entity, on_fn_index, values)
	}
}

impl Drop for ErasedBackend {
	fn drop (&mut self) {
		(self.vtable.drop)(self.data)
	}
}

impl<T: Backend> From<T> for ErasedBackend {
	fn from(other: T) -> Self {
		unsafe fn insert_file<T: Backend>(data: NonNull<()>, state: &GrugState, id: GrugScriptId, file: GrugAst<'_>) {
			T::insert_file(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				id,
				file
			)
		}

		extern "C" fn init_entity<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> bool {
			T::init_entity(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity
			)
		}

		extern "C" fn clear_entities<T: Backend>(data: NonNull<()>) {
			T::clear_entities(
				unsafe{data.cast::<T>().as_mut()},
			)
		}

		extern "C" fn destroy_entity_data<T: Backend>(data: NonNull<()>, entity: &GrugEntity) -> bool {
			T::destroy_entity_data(
				unsafe{data.cast::<T>().as_ref()},
				entity
			)
		}
		/// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
		unsafe extern "C" fn call_on_function_raw<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: *const GrugValue) -> bool {
			unsafe{T::call_on_function_raw(
				data.cast::<T>().as_ref(),
				state, 
				entity,
				on_fn_index,
				values
			)}
		}
		fn call_on_function<T: Backend>(data: NonNull<()>, state: &GrugState, entity: &GrugEntity, on_fn_index: usize, values: &[GrugValue]) -> bool {
			T::call_on_function(
				unsafe{data.cast::<T>().as_ref()},
				state, 
				entity,
				on_fn_index,
				values
			)
		}
		// destroys the resources owned by the backend
		extern "C" fn drop<T: Backend>(data: NonNull<()>) {
			_ = unsafe{Box::from_raw(data.cast::<T>().as_ptr())};
		}

		Self {
			data: unsafe{NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>()},
			vtable: &BackendVTable {
				insert_file         : insert_file::<T>,
				init_entity         : init_entity::<T>,
				clear_entities      : clear_entities::<T>,
				destroy_entity_data : destroy_entity_data::<T>,
				call_on_function_raw: call_on_function_raw::<T>,
				call_on_function    : call_on_function::<T>,
				drop                : drop::<T>,
			}
		}
	}
}

