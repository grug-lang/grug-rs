//! Defines traits and types for working with backends from a state
use crate::ast::GrugAst;
use crate::ntstring::NTStrPtr;
use crate::runtime_error::RuntimeError;
use crate::state::State;
use crate::types::{FileId, GrugEntity, Value};

use std::ptr::NonNull;

/// Interface of backends
///
/// An implementor of the backend is expected to store the state's runtime error
/// handler function in its constructor. That function should be called when a runtime error is required
pub trait Backend {
    /// The AST of a typechecked grug file is provided to let the backend do
    /// further transforms and lower to bytecode or even machine code
    ///
    /// The script ids are guaranteed to be in contiguous ascending order.
    ///
    /// If the same script id is returned again, then it means the old script
    /// associated with the id should be destroyed and replaced with this one.
    ///
    /// The bindings are expected to call [`Backend::init_entity`] on all entities that
    /// belongs to the old script.
    ///
    /// The entity member of all entities created from the old script should be
    /// destroyed. This means that any entity that was created from this file
    /// is temporarily invalid until the bindings call init_entity on them again.
    fn insert_file(&self, id: FileId, file: &GrugAst<'_>);
    /// Initialize the member data of the entity. When this
    /// function is called, the member field of `entity` points to garbage and
    /// must not be deinitialized. The FileId to be used is obtained from
    /// the file_id member of `entity`.
    ///
    /// Returns `Some` if there was a runtime error during execution
    #[must_use]
    fn init_entity<GrugState: State>(&self, state: &GrugState, entity: &GrugEntity) -> bool;
    /// Deinitialize all the data associated with all entities.
    fn clear_entities(&mut self);
    /// Deinitialize the data associated with `entity`.
    ///
    /// # Safety:
    /// This function must not be called on an uninitialized entity.
    /// An entity is considered uninitialized if
    /// 	- It has just been created and [`Backend::init_entity`] hasn't been called on it.
    /// 	- The file an entity belongs to has be reloaded and [`Backend::init_entity`]
    /// 	hasn't been called on it.
    /// 	- This function has been called on the entity
    unsafe fn destroy_entity_data(&self, entity: &GrugEntity);
    /// Run the on function at index `on_fn_index` of the script associated
    /// with `entity`.
    ///
    /// # SAFETY
    /// `values` must point to an array of GrugValues of at least as
    /// many elements as the number of arguments to the on_ function
    ///
    /// If the number of arguments is 0, then `values` is allowed to be null
    #[must_use]
    unsafe fn call_on_function_raw<GrugState: State>(
        &self,
        state: &GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: *const Value,
    ) -> bool;
    /// Run the on function at index `on_fn_index` of the script associated
    /// with `entity`.
    ///
    /// # Panics: The length of `values` must exactly match the number of
    /// expected arguments to the on_ function
    #[must_use]
    fn call_on_function<GrugState: State>(
        &self,
        state: &GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: &[Value],
    ) -> bool;
    /// Calls the state's runtime error handler with the error information from
    /// the current grug script execution.
    ///
    /// Once this function is called, all currently execution grug scripts must
    /// unwind as soon as control is returned to them.  This unwinding state
    /// should persist until the next call to  [`Backend::init_entity`],
    /// [`Backend::call_on_function_raw`], or [`Backend::call_on_function`]. The backend is
    /// responsible for maintaining this unwinding state.
    fn raise_runtime_error<GrugState: State>(&self, state: &GrugState, message: &str);
}

/// C-api compatible version of `&dyn [Backend]`
#[repr(C)]
pub struct ErasedBackend<GrugState: State + 'static> {
    /// A pointer to a heap allocation which owns the data required by the backend
    pub data: NonNull<()>,
    /// A pointer to a statically allocated (or leaked) vtable which contains
    /// all the functions required to be implemented by backends.
    pub vtable: &'static BackendVTable<GrugState>,
}

/// C-api compatible version of the vtable of [`&dyn Backend`]
#[repr(C)]
pub struct BackendVTable<GrugState: State> {
    #[allow(improper_ctypes_definitions)]
    /// See [`Backend::insert_file`]
    pub(crate) insert_file: extern "C" fn(data: NonNull<()>, id: FileId, file: &GrugAst<'_>),
    /// See [`Backend::init_entity`]
    pub(crate) init_entity:
        extern "C" fn(data: NonNull<()>, state: &GrugState, entity: &GrugEntity) -> bool,
    /// See [`Backend::clear_entities`]
    pub(crate) clear_entities: extern "C" fn(data: NonNull<()>),
    /// See [`Backend::destroy_entity_data`]
    pub(crate) destroy_entity_data: unsafe extern "C" fn(data: NonNull<()>, entity: &GrugEntity),
    /// See [`Backend::call_on_function_raw`]
    ///
    /// SAFETY: `values` must point to a buffer of at least as many values as `on_fn_index` expects
    pub(crate) call_on_function_raw: for<'a> unsafe extern "C" fn(
        data: NonNull<()>,
        state: &'a GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: *const Value,
    ) -> bool,
    /// See [`Backend::call_on_function`]
    pub(crate) call_on_function: for<'a> fn(
        data: NonNull<()>,
        state: &'a GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: &[Value],
    ) -> bool,
    pub(crate) raise_runtime_error: for<'a> extern "C" fn(
        data: NonNull<()>,
        state: &'a GrugState,
        message: NonNull<u8>,
        message_len: usize,
    ),
    /// destroys the resources owned by the backend
    pub(crate) drop: extern "C" fn(data: NonNull<()>),
}

impl<GrugState: State> ErasedBackend<GrugState> {
    /// See [`Backend::insert_file`]
    #[inline]
    pub fn insert_file(&self, id: FileId, file: &GrugAst<'_>) {
        (self.vtable.insert_file)(self.data, id, file)
    }
    /// See [`Backend::init_entity`]
    #[inline]
    pub fn init_entity(&self, state: &GrugState, entity: &GrugEntity) -> bool {
        (self.vtable.init_entity)(self.data, state, entity)
    }
    /// See [`Backend::clear_entities`]
    #[inline]
    pub fn clear_entities(&mut self) {
        (self.vtable.clear_entities)(self.data)
    }
    /// See [`Backend::destroy_entity_data`]
    #[inline]
    pub unsafe fn destroy_entity_data(&self, entity: &GrugEntity) {
        unsafe { (self.vtable.destroy_entity_data)(self.data, entity) }
    }
    /// See [`Backend::call_on_function_raw`]
    ///
    /// # SAFETY
    /// `values` must point to a buffer of at least as many values as `on_fn_index` expects
    #[inline]
    pub unsafe fn call_on_function_raw(
        &self,
        state: &GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: *const Value,
    ) -> bool {
        unsafe { (self.vtable.call_on_function_raw)(self.data, state, entity, on_fn_index, values) }
    }
    /// See [`Backend::call_on_function`]
    #[inline]
    pub fn call_on_function(
        &self,
        state: &GrugState,
        entity: &GrugEntity,
        on_fn_index: usize,
        values: &[Value],
    ) -> bool {
        (self.vtable.call_on_function)(self.data, state, entity, on_fn_index, values)
    }
    /// See [`Backend::raise_runtime_error`]
    #[inline]
    pub fn raise_runtime_error(&self, state: &GrugState, message: &str) {
        (self.vtable.raise_runtime_error)(
            self.data,
            state,
            NonNull::from_ref(message).cast(),
            message.len(),
        )
    }
}

impl<GrugState: State> Drop for ErasedBackend<GrugState> {
    fn drop(&mut self) {
        (self.vtable.drop)(self.data)
    }
}

impl<T: Backend, GrugState: State> From<T> for ErasedBackend<GrugState> {
    fn from(other: T) -> Self {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn insert_file<T: Backend>(data: NonNull<()>, id: FileId, file: &GrugAst<'_>) {
            T::insert_file(unsafe { data.cast::<T>().as_ref() }, id, file)
        }

        extern "C" fn init_entity<T: Backend, GrugState: State>(
            data: NonNull<()>,
            state: &GrugState,
            entity: &GrugEntity,
        ) -> bool {
            T::init_entity::<GrugState>(unsafe { data.cast::<T>().as_ref() }, state, entity)
        }

        extern "C" fn clear_entities<T: Backend>(data: NonNull<()>) {
            T::clear_entities(unsafe { data.cast::<T>().as_mut() })
        }

        unsafe extern "C" fn destroy_entity_data<T: Backend>(
            data: NonNull<()>,
            entity: &GrugEntity,
        ) {
            unsafe { T::destroy_entity_data(data.cast::<T>().as_ref(), entity) }
        }
        /// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
        unsafe extern "C" fn call_on_function_raw<T: Backend, GrugState: State>(
            data: NonNull<()>,
            state: &GrugState,
            entity: &GrugEntity,
            on_fn_index: usize,
            values: *const Value,
        ) -> bool {
            unsafe {
                T::call_on_function_raw::<GrugState>(
                    data.cast::<T>().as_ref(),
                    state,
                    entity,
                    on_fn_index,
                    values,
                )
            }
        }
        fn call_on_function<T: Backend, GrugState: State>(
            data: NonNull<()>,
            state: &GrugState,
            entity: &GrugEntity,
            on_fn_index: usize,
            values: &[Value],
        ) -> bool {
            T::call_on_function::<GrugState>(
                unsafe { data.cast::<T>().as_ref() },
                state,
                entity,
                on_fn_index,
                values,
            )
        }
        extern "C" fn raise_runtime_error<T: Backend, GrugState: State>(
            data: NonNull<()>,
            state: &GrugState,
            message: NonNull<u8>,
            message_len: usize,
        ) {
            let message = if message_len == 0 {
                unsafe {
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                        message.as_ptr().cast_const(),
                        message_len,
                    ))
                }
            } else {
                unsafe { NTStrPtr::from_ptr(message.cast()).to_str() }
            };
            T::raise_runtime_error::<GrugState>(
                unsafe { data.cast::<T>().as_ref() },
                state,
                message,
            )
        }
        // destroys the resources owned by the backend
        extern "C" fn drop<T>(data: NonNull<()>) {
            _ = unsafe { Box::from_raw(data.cast::<T>().as_ptr()) };
        }

        Self {
            data: unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>() },
            vtable: &BackendVTable {
                insert_file: insert_file::<T>,
                init_entity: init_entity::<T, GrugState>,
                clear_entities: clear_entities::<T>,
                destroy_entity_data: destroy_entity_data::<T>,
                call_on_function_raw: call_on_function_raw::<T, GrugState>,
                call_on_function: call_on_function::<T, GrugState>,
                raise_runtime_error: raise_runtime_error::<T, GrugState>,
                drop: drop::<T>,
            },
        }
    }
}

/// A representation of a [`State`] provided by c when a backend is used in a c library.
/// This is the pointer that is passed in to any function exported by the backend.
#[repr(transparent)]
pub struct CState(());
impl State for CState {
    fn handle_runtime_error(&self, _error: &RuntimeError) {
        panic!("This is an error within gruggers_core");
    }
}

struct CStateWithHandler {
    state: NonNull<CState>,
    handle_runtime_error: for<'a> extern "C" fn(NonNull<CState>, &RuntimeError),
}

impl State for CStateWithHandler {
    fn handle_runtime_error(&self, error: &RuntimeError) {
        (self.handle_runtime_error)(self.state, error)
    }
}

/// An version of a backend that is exported to a c function. The
/// `set_runtime_error` and `is_errorring` functions provided by c are not API
/// compatible with the ones expected by [`State`], so these functions have to be
/// adapted for use by rust backends
pub struct CBackend<B: Backend> {
    handle_runtime_error: for<'a> extern "C" fn(NonNull<CState>, &RuntimeError),
    backend: B,
}

impl<B: Backend> From<CBackend<B>> for ErasedBackend<CState> {
    fn from(other: CBackend<B>) -> Self {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn insert_file<B: Backend>(data: NonNull<()>, id: FileId, file: &GrugAst<'_>) {
            B::insert_file(
                unsafe { &data.cast::<CBackend<B>>().as_ref().backend },
                id,
                file,
            )
        }

        extern "C" fn init_entity<B: Backend>(
            data: NonNull<()>,
            state: &CState,
            entity: &GrugEntity,
        ) -> bool {
            B::init_entity::<CStateWithHandler>(
                unsafe { &data.cast::<CBackend<B>>().as_ref().backend },
                &CStateWithHandler {
                    state: NonNull::from_ref(state),
                    handle_runtime_error: unsafe {
                        data.cast::<CBackend<B>>().as_ref().handle_runtime_error
                    },
                },
                entity,
            )
        }

        extern "C" fn clear_entities<B: Backend>(data: NonNull<()>) {
            B::clear_entities(unsafe { &mut data.cast::<CBackend<B>>().as_mut().backend })
        }

        unsafe extern "C" fn destroy_entity_data<B: Backend>(
            data: NonNull<()>,
            entity: &GrugEntity,
        ) {
            unsafe { B::destroy_entity_data(&data.cast::<CBackend<B>>().as_ref().backend, entity) }
        }
        /// SAFETY: `values` must point to a buffer of at least as many values as on_fn_id expects
        unsafe extern "C" fn call_on_function_raw<B: Backend>(
            data: NonNull<()>,
            state: &CState,
            entity: &GrugEntity,
            on_fn_index: usize,
            values: *const Value,
        ) -> bool {
            unsafe {
                B::call_on_function_raw::<CStateWithHandler>(
                    &data.cast::<CBackend<B>>().as_ref().backend,
                    &CStateWithHandler {
                        state: NonNull::from_ref(state),
                        handle_runtime_error: data
                            .cast::<CBackend<B>>()
                            .as_ref()
                            .handle_runtime_error,
                    },
                    entity,
                    on_fn_index,
                    values,
                )
            }
        }
        fn call_on_function<B: Backend>(
            data: NonNull<()>,
            state: &CState,
            entity: &GrugEntity,
            on_fn_index: usize,
            values: &[Value],
        ) -> bool {
            B::call_on_function::<CStateWithHandler>(
                unsafe { &data.cast::<CBackend<B>>().as_ref().backend },
                &CStateWithHandler {
                    state: NonNull::from_ref(state),
                    handle_runtime_error: unsafe {
                        data.cast::<CBackend<B>>().as_ref().handle_runtime_error
                    },
                },
                entity,
                on_fn_index,
                values,
            )
        }
        extern "C" fn raise_runtime_error<B: Backend>(
            data: NonNull<()>,
            state: &CState,
            message: NonNull<u8>,
            message_len: usize,
        ) {
            let message = if message_len == 0 {
                unsafe {
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                        message.as_ptr().cast_const(),
                        message_len,
                    ))
                }
            } else {
                unsafe { NTStrPtr::from_ptr(message.cast()).to_str() }
            };
            B::raise_runtime_error::<CStateWithHandler>(
                unsafe { data.cast::<B>().as_ref() },
                &CStateWithHandler {
                    state: NonNull::from_ref(state),
                    handle_runtime_error: unsafe {
                        data.cast::<CBackend<B>>().as_ref().handle_runtime_error
                    },
                },
                message,
            )
        }
        // destroys the resources owned by the backend
        extern "C" fn drop<B: Backend>(data: NonNull<()>) {
            _ = unsafe { Box::from_raw(data.cast::<CBackend<B>>().as_ptr()) };
        }

        Self {
            data: unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(other))).cast::<()>() },
            vtable: &BackendVTable {
                insert_file: insert_file::<B>,
                init_entity: init_entity::<B>,
                clear_entities: clear_entities::<B>,
                destroy_entity_data: destroy_entity_data::<B>,
                call_on_function_raw: call_on_function_raw::<B>,
                call_on_function: call_on_function::<B>,
                raise_runtime_error: raise_runtime_error::<B>,
                drop: drop::<B>,
            },
        }
    }
}

/// Create an ErasedBackend for use from c from an input backend.
/// This function is intended to be used in an exported `create_backend` function.
pub fn erased_c_backend<B: Backend>(
    backend: B,
    handle_runtime_error: extern "C" fn(NonNull<CState>, &RuntimeError),
) -> ErasedBackend<CState> {
    ErasedBackend::from(CBackend {
        backend,
        handle_runtime_error,
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
            handle_runtime_error: extern "C" fn(
                ::core::ptr::NonNull<$crate::backend::CState>,
                &$crate::runtime_error::RuntimeError<'_>,
            ),
        ) -> $crate::backend::ErasedBackend<$crate::backend::CState> {
            $crate::backend::erased_c_backend($backend, handle_runtime_error)
        }
    };
}
