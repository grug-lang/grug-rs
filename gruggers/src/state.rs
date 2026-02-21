use crate::mod_api::{ModApi, get_mod_api, get_mod_api_from_text};
use crate::error::{GrugError, RuntimeError};
use crate::backend::{Backend, ErasedBackend, BytecodeBackend};
use crate::types::{GrugValue, GrugId, GameFnPtr, GrugOnFnId, GrugScriptId, GrugEntity, GrugEntityHandle};
use crate::xar::Xar;

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::cell::{Cell, RefCell, Ref};
use std::path::{Path, PathBuf};
use std::collections::{HashMap, hash_map::Entry};
use std::sync::{atomic::{AtomicU64, Ordering}, Arc};
use std::time::Instant;

#[repr(C)]
pub struct RuntimeErrorHandler {
	data: NonNull<()>,
	drop: Option<extern "C" fn(data: Option<NonNull<()>>)>,
	func: Option<for<'b> extern "C" fn(
		data: NonNull<()>, 
		err_kind: u32, 
		reason: NonNull<u8>,
		reason_len: usize, 
		on_fn_name: NonNull<u8>, 
		on_fn_name_len: usize,
		script_path: NonNull<u8>,
		script_path_len: usize,
	)>,
}

impl RuntimeErrorHandler {
	pub const fn new_default () -> Self {
		Self {
			data: NonNull::dangling(),
			drop: None, 
			func: None
		}
	}

	fn handle_error(&self, kind: RuntimeError, message: &str, on_fn_name: &str, script_path: &str) {
		if let Some(func) = self.func {
			func(
				self.data,
				kind.into_code(),
				NonNull::from_ref(message).cast::<u8>(),
				message.len(),
				NonNull::from_ref(on_fn_name).cast::<u8>(),
				on_fn_name.len(),
				NonNull::from_ref(script_path).cast::<u8>(),
				script_path.len(),
			)
		} 
	}
}

impl Default for RuntimeErrorHandler {
	fn default() -> Self {
		Self::new_default()
	}
}

impl<F: for<'b> Fn(u32, &'b str, &'b str, &'b str)> From<F> for RuntimeErrorHandler {
	fn from(f: F) -> Self {
		let f = unsafe{NonNull::new_unchecked(Box::into_raw(Box::new(f)))}.cast::<()>();
		extern "C" fn handler<F: for<'a> Fn(u32, &'a str, &'a str, &'a str)> (
			data: NonNull<()>, 
			err_kind: u32, 
			reason: NonNull<u8>,
			reason_len: usize, 
			on_fn_name: NonNull<u8>, 
			on_fn_name_len: usize,
			script_path: NonNull<u8>,
			script_path_len: usize,
		) {
			unsafe{(data.cast::<F>().as_ref())(
				err_kind,
				std::str::from_utf8_unchecked(std::slice::from_raw_parts(reason.as_ptr(), reason_len)),
				std::str::from_utf8_unchecked(std::slice::from_raw_parts(on_fn_name.as_ptr(), on_fn_name_len)),
				std::str::from_utf8_unchecked(std::slice::from_raw_parts(script_path.as_ptr(), script_path_len)),
			)};
		}
		extern "C" fn drop<F>(data: Option<NonNull<()>>) {
			data.map(|x| unsafe{Box::from_raw(x.cast::<F>().as_ptr())});
		}
		Self {
			data: f,
			drop: Some(drop::<F> as extern "C" fn(_)),
			func: Some(handler::<F> as extern "C" fn (_, _, _, _, _, _, _, _)),
		}
	}
}

#[repr(C)]
pub struct GrugInitSettings<'a> {
	_marker: PhantomData<&'a ()>,
	mod_api_path: Option<NonNull<u8>>,
	mod_api_path_len: usize,
	mods_dir_path: Option<NonNull<u8>>,
	mods_dir_path_len: usize,
	runtime_error_handler: RuntimeErrorHandler,

	backend: Option<ErasedBackend>,

	// custom allocation support (currently ignored)
	// TODO: Is this even a good idea
	// This allocator has to be a malloc/free style allocator (no arenas, only
	// page allocators and default c style allocators)
	// Might as well use those directly.
	//
	// But then again, if the game knows all memory is allocated by this
	// allocator, it can deallocate memory allocated by grug
	//
	// alloc_data: Option<GrugAllocator>,
}

impl<'a> GrugInitSettings<'a> {
	pub const fn new() -> Self {
		Self {
			_marker: PhantomData,
			mod_api_path: None,
			mod_api_path_len: 0,
			mods_dir_path: None,
			mods_dir_path_len: 0,
			runtime_error_handler: RuntimeErrorHandler::new_default(),
			backend: None,
		}
	}

	pub fn set_mods_dir(mut self, dir: &'a str) -> Self {
		self.mods_dir_path = Some(NonNull::from_ref(dir).cast::<u8>());
		self.mods_dir_path_len = dir.len();
		self
	}

	pub fn set_mod_api_path(mut self, mod_api: &'a str) -> Self {
		self.mod_api_path = Some(NonNull::from_ref(mod_api).cast::<u8>());
		self.mod_api_path_len = mod_api.len();
		self
	}

	pub fn set_backend<B: Backend>(mut self, backend: B) -> Self {
		self.backend = Some(backend.into());
		self
	}

	pub fn set_runtime_error_handler<F: for<'b> Fn(u32, &'b str, &'b str, &'b str)> (mut self, f: F) -> Self {
		self.runtime_error_handler = f.into();
		self
	}

	pub fn build_state(self) -> Result<GrugState, GrugError> {
		let mod_api_path = unsafe{Self::maybe_nt_or_length(self.mod_api_path, self.mod_api_path_len)}
			.unwrap_or("./mod_api.json");
		let mods_dir_path = unsafe{Self::maybe_nt_or_length(self.mods_dir_path, self.mods_dir_path_len)}
			.unwrap_or("./mods");

		GrugState::new(mod_api_path, mods_dir_path, self.runtime_error_handler, self.backend.unwrap_or_else(|| BytecodeBackend::new().into()))
	}

	unsafe fn maybe_nt_or_length(ptr: Option<NonNull<u8>>, len: usize) -> Option<&'a str> {
		// null terminated
		if let Some(ptr) = ptr {
			if len == 0 {
				let mut i = 0;
				loop {
					if unsafe{ptr.add(i).read()} == b'\0' {
						return Some(
							unsafe{std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.as_ptr(), i))}
						)
					}
					i += 1;
				}
			} else {
				Some(
					unsafe{std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.as_ptr(), len))}
				)
			}
		} else {None}
	}
}

impl Default for GrugInitSettings<'static> {
	fn default () -> Self {
		Self::new()
	}
}

pub fn default_runtime_error_handler(_err_kind: u32, reason: &str, on_fn_name: &str, script_path: &str) {
	println!("Runtime Error: {} in function {} in script {}", reason, on_fn_name, script_path);
	std::process::exit(1);
}

pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
	next_entity_id: AtomicU64,
	pub(crate) game_functions: HashMap<&'static str, GameFnPtr>,
	pub(crate) runtime_error_handler: RuntimeErrorHandler,

	pub(crate) entities: Xar<GrugEntity>,
	
	pub(crate) on_functions: Vec<OnFnEntry>,
	pub(crate) path_to_script_ids: RefCell<HashMap<String, GrugScriptId>>,
	next_script_id: AtomicU64,

	pub(crate) backend: ErasedBackend,
	// pub(crate) backend: Interpreter,
	pub(crate) current_script: Cell<Option<GrugScriptId>>,
	pub(crate) current_on_fn_id: Cell<Option<GrugOnFnId>>,
	pub(crate) call_start_time: Cell<Instant>,
	pub(crate) is_errorring: Cell<bool>,
}

impl GrugState {
	fn new<J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D, handler: RuntimeErrorHandler, backend: ErasedBackend) -> Result<Self, GrugError> {
		let mod_api = get_mod_api(&mod_api_path)?;

		let mut on_fns = Vec::new();
		let init_globals = Arc::from("init_globals");
		for (entity_type, entity) in mod_api.entities() {
			on_fns.push(OnFnEntry {
				entity_type: Arc::clone(entity_type),
				on_fn_name : Arc::clone(&init_globals),
				index      : 0,
			});
			for (i, (on_fn_name, _)) in entity.on_fns.iter().enumerate() {
				on_fns.push(OnFnEntry{
					entity_type: Arc::clone(entity_type),
					on_fn_name : Arc::clone(on_fn_name),
					index      : i,
				});
			}
		}

		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_entity_id: AtomicU64::new(0),
			game_functions: HashMap::new(),
			runtime_error_handler: handler,
			entities: Xar::new(),
			on_functions: on_fns,
			path_to_script_ids: RefCell::new(HashMap::new()),
			next_script_id: AtomicU64::new(0),
			backend,
			current_script: Cell::new(None),
			current_on_fn_id: Cell::new(None),
			call_start_time: Cell::new(Instant::now()),
			is_errorring: Cell::new(false),
		})
	}

	pub fn new_from_text<D: AsRef<Path>> (mod_api_text: &str, mods_dir_path: D, handler: RuntimeErrorHandler, backend: impl Into<ErasedBackend>) -> Result<Self, GrugError> {
		let mod_api = get_mod_api_from_text(mod_api_text)?;

		let mut on_fns = Vec::new();
		let init_globals = Arc::from("init_globals");
		for (entity_type, entity) in mod_api.entities() {
			on_fns.push(OnFnEntry {
				entity_type: Arc::clone(entity_type),
				on_fn_name : Arc::clone(&init_globals),
				index      : 0,
			});
			for (i, (on_fn_name, _)) in entity.on_fns.iter().enumerate() {
				on_fns.push(OnFnEntry{
					entity_type: Arc::clone(entity_type),
					on_fn_name : Arc::clone(on_fn_name),
					index      : i,
				});
			}
		}

		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_entity_id: AtomicU64::new(0),
			game_functions: HashMap::new(),
			runtime_error_handler: handler,
			entities: Xar::new(),
			on_functions: on_fns,
			path_to_script_ids: RefCell::new(HashMap::new()),
			next_script_id: AtomicU64::new(0),
			backend: backend.into(),
			current_script: Cell::new(None),
			current_on_fn_id: Cell::new(None),
			call_start_time: Cell::new(Instant::now()),
			is_errorring: Cell::new(false),
		})
	}

	pub fn get_on_fn_id(&self, entity_type: &str, on_fn_name: &str) -> Result<GrugOnFnId, StateError> {
		if !self.mod_api.entities().contains_key(entity_type) {
			return Err(StateError::UnknownEntityType{
				entity_type: String::from(entity_type),
			});
		}
		for (i, on_fn_entry) in self.on_functions.iter().enumerate() {
			if &*on_fn_entry.entity_type == entity_type && &*on_fn_entry.on_fn_name == on_fn_name {
				return Ok(i as u64)
			}
		}
		Err(StateError::UnknownOnFunction {
			entity_type: String::from(entity_type),
			on_function_name : String::from(on_fn_name ),
		})
	}
	
	pub fn get_on_fn_name(&self, on_fn_id: GrugOnFnId) -> Option<&str> {
		return self.on_functions.get(on_fn_id as usize).map(|entry| &*entry.on_fn_name)
	}

	pub fn get_entity_on_functions(&self, entity_type: &str) -> Result<&[OnFnEntry], StateError> {
		if !self.mod_api.entities().contains_key(entity_type) {
			return Err(StateError::UnknownEntityType{
				entity_type: String::from(entity_type),
			});
		}
		let mut start = 0;
		while start != self.on_functions.len() && &*self.on_functions[start].entity_type != entity_type {
			start += 1;
		}
		let mut end = start;
		while end != self.on_functions.len() && &*self.on_functions[end].entity_type == entity_type {
			end += 1;
		}
		Ok(&self.on_functions[start..end])
	}

	pub fn register_game_fn<F: Into<GameFnPtr>>(&mut self, name: &'static str, ptr: F) -> Result<(), StateError> {
		if !self.mod_api.game_functions().contains_key(name) {
			Err(StateError::UnknownGameFunction{
				game_function_name: name,
			})
		} else {
			match self.game_functions.entry(name) {
				Entry::Occupied(_) => Err(StateError::ReregisteringGameFunction{
					game_function_name: name,
				}),
				Entry::Vacant(x) => {
					x.insert(ptr.into());
					Ok(())
				}
			}
		}
	}
	
	// This should only happen during an error so its okay if its slow
	pub fn get_script_path(&self, script_id: GrugScriptId) -> Option<&str> {
		let string = Ref::filter_map(self.path_to_script_ids.borrow(), |inner|
			inner.iter().find(|(_, v)| **v == script_id).map(|x| x.0)
		).ok()?;
		// SAFETY: a path is never replaced once it is inserted into the map;
		let string: &str = unsafe{&*(&**string as *const str)};
		Some(string)
	}

	pub fn all_game_fns_registered(&self) -> bool {
		for game_fn_name in self.mod_api.game_functions().keys() {
			if !self.game_functions.contains_key(&**game_fn_name) {
				return false;
				// Err(ModApiError::GameFnNotProvided{
				// 	game_fn_name: String::from(&**game_fn_name),
				// })?;
			}
		}
		return true;
	}

	pub(crate) fn get_next_script_id(&self) -> GrugScriptId {
		GrugId::new(self.next_script_id.fetch_add(1, Ordering::Relaxed))
	}

	pub fn get_next_entity_id(&self) -> GrugId {
		GrugId::new(self.next_entity_id.fetch_add(1, Ordering::Relaxed))
	}

	/// # Safety
	/// There is no memory safety issue here. 
	/// But this may cause older entities to be replaced 
	/// by newer ones with no warning if the ids start overlapping
	pub unsafe fn set_next_entity_id(&self, next_id: u64) {
		self.next_entity_id.store(next_id, Ordering::Relaxed);
	}

	pub fn create_entity(&self, file_id: GrugScriptId) -> Option<GrugEntityHandle<'_>> {
		let old_script   = self.current_script  .get();
		let old_on_fn_id = self.current_on_fn_id.get();
		self.current_script  .set(Some(file_id));
		self.current_on_fn_id.set(Some(0));

		let entity = self.entities.insert(unsafe{GrugEntity::new_uninit(self.get_next_entity_id(), file_id)});
		let entity = unsafe{GrugEntityHandle::new(entity)};
		let success = self.backend.init_entity(self, &entity);

		self.current_script  .set(old_script);
		self.current_on_fn_id.set(old_on_fn_id);

		if success {
			Some(entity)
		} else {
			unsafe{self.entities.delete(entity.into_inner());}
			None
		}
	}

	pub fn destroy_entity<'a>(&'a self, entity: GrugEntityHandle<'a>) {
		if self.backend.destroy_entity_data(&*entity) {
			// SAFETY: an entity stored within self.files must have come from
			// this same state because of the return value of the above
			// function. GrugEntityHandle contains the only other handle to the entity storage
			unsafe{self.entities.delete(entity.into_inner())};
		}
	}

	pub fn set_runtime_error(&self, error: RuntimeError) {
		self.is_errorring.set(true);
		let Some(current_script) = self.current_script.get() else {
			return
		};
		let Some(current_on_fn_id) = self.current_on_fn_id.get() else {
			return
		};
		let current_on_fn_name = self.get_on_fn_name(current_on_fn_id).unwrap();
		let message = format!("{}", error);
		self.runtime_error_handler.handle_error(
			error, 
			&message,
			current_on_fn_name,
			self.get_script_path(current_script).unwrap(),
		);
	}

	pub fn clear_entities(&mut self) {
		self.backend.clear_entities();
		self.entities.clear();
	}

	pub fn clear_error(&self) {
		self.is_errorring.set(false);
	}

	fn get_on_fn_index(&self, id: GrugOnFnId) -> usize {
		self.on_functions[id as usize].index
	}
}

// should be moved into backend later
impl GrugState {
	/// # SAFETY 
	/// `values` must point to an array of values with length equal to
	/// the number of arguments expected by `function_name`. If there are no arguments, 
	/// `values` may be null
	#[must_use]
	pub unsafe fn call_on_function_raw(&self, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> bool {
		let old_script   = self.current_script  .get();
		let old_on_fn_id = self.current_on_fn_id.get();
		self.current_script  .set(Some(entity.file_id));
		self.current_on_fn_id.set(Some(on_fn_id));

		let ret_val = unsafe {
			self.backend.call_on_function_raw(self, entity, self.get_on_fn_index(on_fn_id), values)
		};

		self.current_script  .set(old_script);
		self.current_on_fn_id.set(old_on_fn_id);

		ret_val
	}

	#[must_use]
	pub fn call_on_function(&self, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> bool {
		let old_script   = self.current_script  .get();
		let old_on_fn_id = self.current_on_fn_id.get();
		self.current_script  .set(Some(entity.file_id));
		self.current_on_fn_id.set(Some(on_fn_id));

		let ret_val = self.backend.call_on_function(self, entity, self.get_on_fn_index(on_fn_id), values);

		self.current_script  .set(old_script);
		self.current_on_fn_id.set(old_on_fn_id);

		ret_val
	}
}

#[derive(Debug)]
pub enum StateError {
	UnknownEntityType{
		entity_type: String
	},
	UnknownOnFunction {
		entity_type: String,
		on_function_name: String,
	},
	UnknownGameFunction {
		game_function_name: &'static str,
	},
	ReregisteringGameFunction {
		game_function_name: &'static str,
	}
}

impl std::fmt::Display for StateError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::UnknownEntityType{
				entity_type,
			} => write!(f, "mod api does not define an entity named {}", entity_type),
			Self::UnknownOnFunction {
				entity_type,
				on_function_name,
			} => write!(f, "'{}' does not contain an on_function named '{}'", entity_type, on_function_name),
			Self::UnknownGameFunction {
				game_function_name,
			} => write!(f, "Game function named '{}' is not found in mod_api.json", game_function_name),
			Self::ReregisteringGameFunction {
				game_function_name,
			} => write!(f, "Game function named '{}' has already been registered", game_function_name),
		}
	}
}

pub struct OnFnEntry {
	pub entity_type: Arc<str>,
	pub on_fn_name : Arc<str>,
	pub index      : usize,
}
