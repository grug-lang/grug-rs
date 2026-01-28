use crate::mod_api::{ModApi, get_mod_api};
use crate::error::{GrugError, RuntimeError};
use crate::backend::Backend;
use crate::types::{GrugValue, GrugId, GameFnPtr, GrugOnFnId, GrugScriptId, GrugEntity, GrugEntityHandle};
use crate::xar::Xar;

use std::cell::Cell;
use std::path::{Path, PathBuf};
use std::collections::{HashMap, hash_map::Entry};
use std::sync::{atomic::{AtomicU64, Ordering}};
use std::time::Instant;

#[repr(C)]
pub struct GrugInitSettings<'a> {
	pub mod_api_path: &'a str,
	pub mods_dir_path: &'a str,
	pub runtime_error_handler: fn(reason: String, err_kind: u32, on_fn_name: String, script_path: String),
	
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

impl Default for GrugInitSettings<'static> {
	fn default () -> Self {
		Self {
			mod_api_path: "./mod_api.json",
			mods_dir_path: "./mods",
			runtime_error_handler: default_runtime_error_handler,
		}
	}
}

pub fn default_runtime_error_handler(reason: String, _err_kind: u32, on_fn_name: String, script_path: String) {
	println!("Runtime Error: {} in function {} in script {}", reason, on_fn_name, script_path);
	std::process::exit(1);
}

pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
	pub(crate) next_id: AtomicU64,
	pub(crate) game_functions: HashMap<&'static str, GameFnPtr>,

	pub(crate) entities: Xar<GrugEntity>,
	
	pub(crate) backend: Backend,
	// should be moved into backend later
	pub(crate)call_start_time: Cell<Instant>,
	pub(crate)error: Cell<Option<&'static str>>,
	pub handled_error: Cell<bool>,
}

impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D) -> Result<Self, GrugError<'a>> {
		let mod_api = get_mod_api(&mod_api_path)?;

		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_id: AtomicU64::new(0),
			game_functions: HashMap::new(),
			entities: Xar::new(),
			backend: Backend::new(),
			call_start_time: Cell::new(Instant::now()),
			error: Cell::new(None),
			handled_error: Cell::new(false),
		})
	}

	pub fn get_on_fn_id(&self, entity_type: &str, on_fn_name: &str) -> Result<GrugOnFnId, StateError> {
		Ok(self.mod_api.entities().get(entity_type)
			.ok_or(StateError::UnknownEntityType{
				entity_type: String::from(entity_type)
			})?
			.get_on_fn(on_fn_name)
			.ok_or(StateError::UnknownOnFunction{
				entity_type: String::from(entity_type), 
				on_function_name: String::from(on_fn_name)}
			)?
			.0 as u64)
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

	pub fn get_id(&self) -> GrugId {
		GrugId::new(self.next_id.fetch_add(1, Ordering::Relaxed))
	}

	/// # Safety
	/// There is no memory safety issue here. 
	/// But this may cause older entities to be replaced 
	/// by newer ones with no warning if the ids start overlapping
	pub unsafe fn set_next_id(&self, next_id: u64) {
		self.next_id.store(next_id, Ordering::Relaxed);
	}

	pub fn create_entity(&self, file_id: GrugScriptId) -> Result<GrugEntityHandle<'_>, RuntimeError> {
		let entity = self.entities.insert(unsafe{GrugEntity::new_uninit(self.get_id(), file_id)});
		let entity = unsafe{GrugEntityHandle::new(entity)};
		self.backend.init_entity(self, &entity)?;
		Ok(entity)
	}

	pub fn destroy_entity<'a>(&'a self, entity: GrugEntityHandle<'a>) {
		if unsafe{self.backend.destroy_entity_data(&*entity)} {
			// SAFETY: an entity stored within self.files must have come from
			// this same state because of the return value of the above
			// function. GrugEntityHandle contains the only other handle to the entity storage
			unsafe{self.entities.delete(entity.into_inner())};
		}
	}

	pub fn clear_entities(&mut self) {
		self.backend.clear_entities();
		self.entities.clear();
	}

	pub fn clear_error(&self) {
		self.error.set(None);
		self.handled_error.set(false);
	}

	pub fn set_error(&self, error: &'static str) {
		self.error.set(Some(error));
		self.handled_error.set(false);
	}

	pub fn set_handled_error(&self) {
		self.handled_error.set(true);
	}
}

// should be moved into backend later
impl GrugState {
	/// # SAFETY 
	/// `values` must point to an array of values with length equal to
	/// the number of arguments expected by `function_name`. If there are no arguments, 
	/// `values` may be null
	pub unsafe fn call_on_function_raw(&self, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: *const GrugValue) -> Result<(), RuntimeError> {
		unsafe {
			self.backend.call_on_function_raw(self, entity, on_fn_id, values)
		}
	}

	pub fn call_on_function(&self, entity: &GrugEntity, on_fn_id: GrugOnFnId, values: &[GrugValue]) -> Result<(), RuntimeError> {
		self.backend.call_on_function(self, entity, on_fn_id, values)
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
