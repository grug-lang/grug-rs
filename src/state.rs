use crate::mod_api::{ModApi, get_mod_api};
use crate::error::GrugError;
use crate::backend::{Backend, RuntimeError};
use crate::types::{GrugValue, GrugId, GameFnPtr};

use std::cell::Cell;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::{atomic::{AtomicU64, Ordering}};
use std::time::Instant;

pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
	pub(crate) next_id: AtomicU64,
	pub(crate) game_functions: HashMap<&'static str, GameFnPtr>,
	
	pub(crate) backend: Backend,
	// should be moved into backend later
	pub(crate)call_start_time: Cell<Instant>,
	pub(crate)error: Cell<Option<&'static str>>,
	pub handled_error: Cell<bool>,
}
impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D) -> Result<Self, GrugError<'a>> {
		let mod_api_text = std::fs::read_to_string(mod_api_path).unwrap();
		let mod_api = get_mod_api(&mod_api_text)?;

		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
			next_id: AtomicU64::new(0),
			game_functions: HashMap::new(),
			backend: Backend::new(),
			call_start_time: Cell::new(Instant::now()),
			error: Cell::new(None),
			handled_error: Cell::new(false),
		})
	}

	pub fn register_game_fn<F: Into<GameFnPtr>>(&mut self, name: &'static str, ptr: F) {
		self.game_functions.insert(name, ptr.into());
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

	pub fn create_entity(&self, file_path: &str) -> Result<GrugId, RuntimeError> {
		self.backend.create_entity(self, file_path)
	}

	pub fn clear_entities(&mut self) {
		self.backend.clear_entities();
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
	pub unsafe fn call_on_function_raw(&self, entity: GrugId, function_name: &str, values: *const GrugValue) -> Result<(), RuntimeError> {
		unsafe {
			self.backend.call_on_function_raw(self, entity, function_name, values)
		}
	}

	pub fn call_on_function(&self, entity: GrugId, function_name: &str, values: &[GrugValue]) -> Result<(), RuntimeError> {
		self.backend.call_on_function(self, entity, function_name, values)
	}
}
