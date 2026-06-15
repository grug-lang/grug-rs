//! Doing anything with grug first requires you to create a [`GrugState`].
//!
//! # Using [`GrugState`]
//! The first thing to do is to create a GrugInitSettings. This contains
//! configuration data for the state. Important configuration parameters
//! include mod_api path, the mods directory path, the runtime error handler,
//! and an explicit backend. 
//!
//! GrugInitSettings can be initialized directly from c code as a normal struct. 
//! It can be zeroed to set all options to the defaults.
//! 
//! Once configuration is complete, the GrugState can be built with
//! [`GrugInitSettings::build_state`]. If there is any error when creating the
//! state, an [`Error`] with kind INIT_ERROR will be returned. 
//!
//! If there is no error, you can then begin registering the host functions.
//! Host functions are functions that grug code can call to perform any action
//! not directly built into grug (which includes almost every single useful
//! operation). All host functions mentioned in the mod_api should be registered
//! exactly once at this stage. 
//!
//! It is not an error to not register a host function, but if a script
//! encounters that function, The compilation threads will panic. It is always
//! an error to register a function not mentioned in the mod_api or registering
//! a function more than once.
//!
//! [`GrugState::all_host_fns_registered`] can be used to verify that all host
//! functions are registered.
//! 
//! At this point, you can begin compiling the grug files.
//! [`GrugState::compile_all_files`] can be used to compile all files within
//! the mods directory at once. This is the most efficient way to compile
//! files, because the frontend can work on multiple files at the same time.
//!
//! You can also use [`GrugState::compile_grug_file`] and
//! [`GrugState::compile_grug_file_from_str`] to compile files individually.
//!
//! At this point, the state will begin automatically monitoring for changes to
//! the mods directory. To handle the changes, call [`GrugState::update_files`]
//! once at the top of the game loop or message loop. This returns all the
//! scripts that were recompiled and any resources that need to be reloaded.

use crate::xar::XarHandle;
use crate::mod_api::{ModApi, get_mod_api, get_mod_api_from_text};
use crate::error::{Error, ErrorKind, SourceSpan};
use crate::backend::{Backend, ErasedBackend, BytecodeBackend};
use crate::types::{GrugValue, GrugId, GameFnPtr, GrugOnFnId, GrugFileId, GrugEntity, INVALID_GRUG_SCRIPT_ID};
use crate::xar::Xar;
use crate::ntstring::{NTStrPtr};
use crate::arena::Arena;
use crate::own_ptr::OwnPtr;
use crate::nt;
use crate::watcher::{watch_changes};

use gruggers_core::runtime_error::RuntimeError;
pub use gruggers_core::state::State;
pub use gruggers_core::ast::GrugAst;

use std::path::{Path, PathBuf};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::pin::Pin;
use std::cell::{Cell, RefCell, Ref};
use std::collections::{HashMap, hash_map::Entry, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::ffi::{OsString, OsStr};
use std::sync::mpsc::{Receiver, Sender, channel};
use std::sync::{RwLock, Arc};

// /// Called by the 
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

const _: () = const {
	assert!(std::mem::size_of::<RuntimeErrorHandler>() == std::mem::size_of::<Option<RuntimeErrorHandler>>());
};

impl RuntimeErrorHandler {
	/// 
	pub const fn new_default () -> Self {
		Self {
			data: NonNull::dangling(),
			drop: None, 
			func: None
		}
	}

	fn handle_error(&self, kind: RuntimeError, message: &str, on_fn_name: &str, script_path: &OsStr) {
		if let Some(func) = self.func {
			func(
				self.data,
				kind.code(),
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
	runtime_error_handler: Option<RuntimeErrorHandler>,

	backend: Option<ErasedBackend<GrugState>>,
}

const _: () = const {
	unsafe{std::mem::forget(std::mem::MaybeUninit::<GrugInitSettings<'static>>::zeroed().assume_init())};
};

impl<'a> GrugInitSettings<'a> {
	pub const fn new() -> Self {
		Self {
			_marker: PhantomData,
			mod_api_path: None,
			mod_api_path_len: 0,
			mods_dir_path: None,
			mods_dir_path_len: 0,
			runtime_error_handler: None,
			backend: None,
		}
	}

	pub fn set_mods_dir<P: AsRef<OsStr> + ?Sized>(mut self, dir: &'a P) -> Self {
		let dir = dir.as_ref();
		if dir.is_empty() {
			self.mods_dir_path = None;
			self.mods_dir_path_len = 0;
		} else {
			self.mods_dir_path = Some(NonNull::from_ref(dir).cast::<u8>());
			self.mods_dir_path_len = dir.len();
		}
		self
	}

	pub fn set_mod_api_path<P: AsRef<OsStr> + ?Sized>(mut self, mod_api: &'a P) -> Self {
		let mod_api = mod_api.as_ref();
		if mod_api.is_empty() {
			self.mod_api_path = None;
			self.mod_api_path_len = 0;
		} else {
			self.mod_api_path = Some(NonNull::from_ref(mod_api).cast::<u8>());
			self.mod_api_path_len = mod_api.len();
		}
		self
	}

	pub fn set_backend<B: Backend>(mut self, backend: B) -> Self {
		self.backend = Some(backend.into());
		self
	}

	pub fn set_runtime_error_handler<F: for<'b> Fn(u32, &'b str, &'b str, &'b str)> (mut self, f: F) -> Self {
		self.runtime_error_handler = Some(f.into());
		self
	}

	pub fn build_state(self) -> Result<GrugState, Error> {
		let mod_api_path = unsafe{Self::maybe_nt_or_length(self.mod_api_path, self.mod_api_path_len)}
			.unwrap_or("./mod_api.json");
		let mods_dir_path = unsafe{Self::maybe_nt_or_length(self.mods_dir_path, self.mods_dir_path_len)}
			.unwrap_or("./mods");

		GrugState::new(
			mod_api_path,
			mods_dir_path,
			self.runtime_error_handler.unwrap_or_else(RuntimeErrorHandler::new_default), 
			self.backend.unwrap_or_else(|| BytecodeBackend::new().into())
		)
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
	pub(crate) mod_api: Arc<ModApi>,
	pub(crate) mods_dir_path: OsString,
	next_entity_id: AtomicU64,
	pub(crate) host_fn_ptrs: Arc<RwLock<HashMap<&'static str, GameFnPtr>>>,
	pub(crate) method_fn_ptrs: Arc<RwLock<HashMap<&'static str, HashMap<&'static str, GameFnPtr>>>>,
	pub(crate) runtime_error_handler: RuntimeErrorHandler,

	pub(crate) entities: Xar<GrugEntity>,
	pub(crate) resources: RefCell<HashSet<OsString>>,
	/// Send an arena and a slice of filepaths to compile (allocated within the arena)
	pub(crate) compiler_senders: Vec<Sender<(Arena, &'static [&'static OsStr])>>,
	/// Receive the arena and a slice of ASTs and the corresponding filepaths,
	/// and a list of resources used by these files
	/// (all allocated within the same arena)
	pub(crate) compiler_receiver: Receiver<(Arena, OwnPtr<'static, [(Result<GrugAst<'static>, Error>, &'static OsStr)]>, &'static [&'static OsStr])>,
	/// SAFETY: The strings within the `export_functions` field is allocated within
	/// `mod_api`. So any reference given out to this field must have the 'self
	/// lifetime
	/// If a later change makes mod_api mutable, these need to be allocated separately
	export_functions: Vec<ExportFnEntry<'static>>,
	pub(crate) path_to_script_ids: RefCell<HashMap<OsString, (OsString, GrugFileId)>>,
	next_script_id: AtomicU64,

	pub(crate) backend: ErasedBackend<Self>,
	// for use when compiling
	pub(crate) arenas : RefCell<Vec<Arena>>,
	// pub(crate) backend: Interpreter,
	pub(crate) current_script: Cell<Option<GrugFileId>>,
	pub(crate) current_export_fn_id: Cell<Option<GrugOnFnId>>,
	pub(crate) is_errorring: Cell<bool>,

	pub(crate) changes: Receiver<Result<OsString, std::io::Error>>,
}

impl State for GrugState {
	fn set_runtime_error(&self, error: RuntimeError) {
		self.is_errorring.set(true);
		let Some(current_script) = self.current_script.get() else {
			return
		};
		let Some(current_export_fn_id) = self.current_export_fn_id.get() else {
			return
		};
		let current_on_fn_name = self.get_export_fn_name(current_export_fn_id).unwrap();
		let message = format!("{}", error);
		self.runtime_error_handler.handle_error(
			error, 
			&message,
			current_on_fn_name,
			self.get_script_path_rel(current_script).unwrap(),
		);
	}

	fn is_errorring(&self) -> bool {
		self.is_errorring.get()
	}
}

impl GrugState {
	fn new (mod_api_path: impl AsRef<OsStr>, mods_dir_path: impl AsRef<OsStr>, handler: RuntimeErrorHandler, backend: ErasedBackend<Self>) -> Result<Self, Error> {
		let mod_api = get_mod_api(mod_api_path.as_ref())?;
		Self::new_inner(mod_api, mods_dir_path, handler, backend)
	}

	pub fn new_from_text (mod_api_text: &str, mods_dir_path: impl AsRef<OsStr>, handler: RuntimeErrorHandler, backend: impl Into<ErasedBackend<Self>>) -> Result<Self, Error> {
		let mod_api = get_mod_api_from_text("<Mod API Source>", mod_api_text)?;
		Self::new_inner(mod_api, mods_dir_path, handler, backend.into())
	}

	fn new_inner (mod_api: ModApi, mods_dir_path: impl AsRef<OsStr>, handler: RuntimeErrorHandler, backend: ErasedBackend<Self>) -> Result<Self, Error> {
		let mut on_fns = Vec::new();
		let init_globals = nt!("init_globals");
		let mods_dir_path = PathBuf::from(mods_dir_path.as_ref());
		
		for (entity_type, entity) in mod_api.entities() {
			on_fns.push(ExportFnEntry {
				// SAFETY: All EventFnEntries we give out have a 'self
				// lifetime, which is the same as the 'mod_api lifetime they
				// actually have
				entity_type   : unsafe{entity_type.as_ntstrptr().detach_lifetime()},
				event_fn_name : unsafe{init_globals.as_ntstrptr().detach_lifetime()},
				index      : 0,
			});
			for (i, (event_fn_name, _)) in entity.export_fns.iter().enumerate() {
				on_fns.push(ExportFnEntry{
					// SAFETY: All EventFnEntries we give out have a 'self
					// lifetime, which is the same as the 'mod_api lifetime they
					// actually have
					entity_type   : unsafe{entity_type.as_ntstrptr().detach_lifetime()},
					event_fn_name : unsafe{event_fn_name.as_ntstrptr().detach_lifetime()},
					index         : i,
				});
			}
		}

		let mod_api = Arc::new(mod_api);
		let host_fn_ptrs = Arc::new(RwLock::new(HashMap::new()));
		let method_fn_ptrs = Arc::new(RwLock::new(HashMap::new()));

		let (sender, reciever) = channel();
		watch_changes(&mods_dir_path, move |changes| sender.send(changes).is_ok()).unwrap();
		let num_threads = {
			let available_threads = std::thread::available_parallelism().map(|x| x.get()).unwrap_or(1);
			if available_threads <= 2 {1} else {available_threads - 2}
		};
		let (snd, rcv) = channel();
		// Create compiler threads
		let compiler_senders = (0..num_threads).map(|_| {
			let (per_thread_send, per_thread_rcv) = channel();
			std::thread::spawn(Self::compiler_thread_fn(
				per_thread_rcv, 
				snd.clone(), 
				mods_dir_path.clone(), 
				Arc::clone(&mod_api), 
				Arc::clone(&host_fn_ptrs),
				Arc::clone(&method_fn_ptrs)
			));
			per_thread_send
		}).collect::<Vec<_>>();

		Ok(Self {
			mod_api,
			mods_dir_path: mods_dir_path.into(),
			next_entity_id: AtomicU64::new(0),
			host_fn_ptrs,
			method_fn_ptrs,
			runtime_error_handler: handler,
			resources: RefCell::new(HashSet::new()),
			entities: Xar::new(),
			compiler_senders,
			compiler_receiver: rcv,
			export_functions: on_fns,
			path_to_script_ids: RefCell::new(HashMap::new()),
			next_script_id: AtomicU64::new(0),
			arenas: RefCell::new(Vec::new()),
			backend,
			current_script: Cell::new(None),
			current_export_fn_id: Cell::new(None),
			is_errorring: Cell::new(false),
			changes: reciever,
		})
	}

	pub(crate) fn get_or_insert_script_id(&self, path: &Path) -> GrugFileId {
		let mut canonicalized = PathBuf::from(self.mods_dir_path.clone());
		canonicalized.push(path);
		let canonicalized = canonicalized.canonicalize().expect("error while canonicalizing");

		let mut path_to_script_ids = self.path_to_script_ids.borrow_mut();
		match path_to_script_ids.get(canonicalized.as_os_str()) {
			Some((_, id)) => *id,
			None => {
				let id = self.get_next_script_id();
				assert!(path_to_script_ids.insert(canonicalized.into_os_string(), (OsString::from(path), id)).is_none());
				id
			}
		}
	}

	pub fn mods_dir_path(&self) -> &OsStr {
		self.mods_dir_path.as_ref()
	}

	pub fn get_export_fn_id(&self, entity_type: &str, fn_name: &str) -> Result<GrugOnFnId, Error> {
		if !self.mod_api.entities().contains_key(entity_type) {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("mod api does not define an entity named {}", entity_type),
			));
		}
		for (i, on_fn_entry) in self.export_functions.iter().enumerate() {
			if on_fn_entry.entity_type() == entity_type && on_fn_entry.event_fn_name() == fn_name {
				return Ok(i as u64)
			}
		}
		return Err(Error::new(
			ErrorKind::INIT_ERROR,
			"",
			"".as_ref(),
			"",
			SourceSpan{offset: 0, line: 0},
			format_args!("'{}' does not export a function named '{}'", entity_type, fn_name),
		));
	}
	
	pub fn get_export_fn_name(&self, fn_id: GrugOnFnId) -> Option<&str> {
		self.export_functions.get(fn_id as usize).map(|entry| entry.event_fn_name())
	}

	pub fn get_export_fns(&self) -> &[ExportFnEntry<'_>] {
		&self.export_functions
	}

	pub fn get_entity_export_functions(&self, entity_type: &str) -> Result<&[ExportFnEntry<'_>], Error> {
		if !self.mod_api.entities().contains_key(entity_type) {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("mod api does not define an entity named {}", entity_type),
			));
		}
		let mut start = 0;
		while start != self.export_functions.len() && self.export_functions[start].entity_type() != entity_type {
			start += 1;
		}
		let mut end = start;
		while end != self.export_functions.len() && self.export_functions[end].entity_type() == entity_type {
			end += 1;
		}
		Ok(&self.export_functions[start..end])
	}

	pub unsafe fn register_host_fn(&mut self, name: &'static str, func: extern "C" fn (&GrugState, *const GrugValue) -> GrugValue) -> Result<(), Error> {
		if !self.mod_api.host_fns().contains_key(name) {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Host function named '{}' is not found in mod_api.json", name),
			));
		} else {
			match self.host_fn_ptrs.write().unwrap().entry(name) {
				Entry::Occupied(_) => return Err(Error::new(
					ErrorKind::INIT_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function named '{}' has already been registered", name),
				)),
				Entry::Vacant(x) => {
					x.insert(GameFnPtr::from_ptr(func));
					Ok(())
				}
			}
		}
	}

	pub unsafe fn register_method(&mut self, class_name: &'static str, function_name: &'static str, func: extern "C" fn (&GrugState, *const GrugValue) -> GrugValue) -> Result<(), Error> {
		let Some(class) = self.mod_api.classes().get(class_name) else {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Class with name '{}' is not found in mod_api.json", class_name),
			));
		};
		if class.methods.iter().find(|(name, _)| name.as_str() == function_name).is_none() {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Class with name '{}' does not contain method with name '{}'", class_name, function_name),
			));
		}
		
		else {
			match self.method_fn_ptrs.write().unwrap().entry(class_name) {
				Entry::Occupied(mut entry) => {
					match entry.get_mut().entry(function_name) {
						Entry::Occupied(_) => return Err(Error::new(
							ErrorKind::INIT_ERROR,
							"",
							"".as_ref(),
							"",
							SourceSpan{offset: 0, line: 0},
							format_args!("Host method named '{}' on class '{}' has already been registered", function_name, class_name),
						)),
						Entry::Vacant(x) => {
							x.insert(GameFnPtr::from_ptr(func));
							Ok(())
						}
					}
				}
				Entry::Vacant(x) => {
					x.insert(HashMap::from([(function_name, GameFnPtr::from_ptr(func))]));
					Ok(())
				}
			}
		}
	}

	/// Register a dummy function for each game function defined in the mod_api
	///
	/// # Safety
	///
	/// It is immediate UB to run any grug script created with this grug_state afterwards.
	///
	/// You are only allowed to compile scripts from this state.
	/// This function only exists to allow the cli compiler to function.
	pub unsafe fn register_dummies(&mut self) {
		extern "C" fn dummy_host_fn(_state: &GrugState, _arguments: *const GrugValue) -> GrugValue {
			GrugValue{void: ()}
		}

		let mut host_fn_ptrs = self.host_fn_ptrs.write().unwrap();
		for name in self.mod_api.host_fns().keys() {
			host_fn_ptrs.entry(Box::leak(Box::from(name.as_str()))).or_insert(GameFnPtr::from_ptr(dummy_host_fn));
		}
	}
	
	// This should only happen during an error so its okay if its slow
	pub fn get_script_path_rel(&self, script_id: GrugFileId) -> Option<&OsStr> {
		let string = Ref::filter_map(self.path_to_script_ids.borrow(), |inner|
			inner.values().find(|(_, v)| *v == script_id).map(|x| &*x.0)
		).ok()?;
		// SAFETY: a path is never replaced once it is inserted into the map;
		let string: &OsStr = unsafe{&*(&*string as *const OsStr)};
		Some(string)
	}

	pub fn all_host_fns_registered(&self) -> Result<(), Error> {
		// Check all normal host functions
		let host_fn_ptrs = self.host_fn_ptrs.read().unwrap();
		for game_fn_name in self.mod_api.host_fns().keys() {
			if !host_fn_ptrs.contains_key(game_fn_name.as_str()) {
				return Err(Error::new(
					ErrorKind::INIT_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("host function '{game_fn_name}' has not been registered"),
				));
			}
		}
		// check all methods
		let method_fn_ptrs = self.method_fn_ptrs.read().unwrap();
		for (class_name, class) in self.mod_api.classes() {
			let Some(method_fn_ptrs) = method_fn_ptrs.get(class_name.as_str()) else {
				return Err(Error::new(
					ErrorKind::INIT_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("methods for class '{class_name}' have not been registered"),
				));
			};
			for (method_name, _) in class.methods {
				if !method_fn_ptrs.contains_key(method_name.as_str()) {
					return Err(Error::new(
						ErrorKind::INIT_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("method '{method_name}' in class '{class_name}' has not been registered"),
					));
				}
			}
		}
		Ok(())
	}

	pub(crate) fn get_next_script_id(&self) -> GrugFileId {
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

	/// Create a new entity from the input file id
	pub fn create_entity(&self, file_id: GrugFileId) -> Option<GrugEntityHandle<'_>> {
		let old_script   = self.current_script  .get();
		let old_fn_id = self.current_export_fn_id.get();
		self.current_script  .set(Some(file_id));
		self.current_export_fn_id.set(Some(0));

		let entity = self.entities.insert(unsafe{GrugEntity::new_uninit(self.get_next_entity_id(), file_id)});
		let entity = unsafe{GrugEntityHandle::new(entity)};
		// SAFETY: Entity is created inside a Xar which does not move elements around. 
		let success = self.backend.init_entity(self, unsafe{Pin::new_unchecked(&entity)});

		self.current_script  .set(old_script);
		self.current_export_fn_id.set(old_fn_id);

		if success {
			Some(entity)
		} else {
			unsafe{self.entities.delete(entity.into_inner());}
			None
		}
	}

	/// Destroys the entity passed in _if_ the entity was allocated from self
	pub fn destroy_entity<'a>(&'a self, entity: GrugEntityHandle<'a>) {
		if self.entities.is_contained_within(entity.0) {
			self.backend.destroy_entity_data(&entity);
			// `self.entities.contained_within` returns true so this entity must exist within self
			unsafe{self.entities.delete(entity.into_inner())};
		}
	}

	/// Destroy all entities 
	pub fn clear_entities(&mut self) {
		self.backend.clear_entities();
		self.entities.clear();
	}

	/// Clear any currently active errors
	pub fn clear_error(&self) {
		self.is_errorring.set(false);
	}

	/// get the index of the export function within its entity
	fn get_export_fn_index(&self, id: GrugOnFnId) -> usize {
		self.export_functions[id as usize].index
	}
}

impl GrugState {
	/// # SAFETY 
	/// `values` must point to an array of values with length equal to
	/// the number of arguments expected by `function_name`. If there are no arguments, 
	/// `values` may be null
	#[must_use]
	pub unsafe fn call_on_function_raw(&self, entity: &GrugEntity, fn_id: GrugOnFnId, values: *const GrugValue) -> bool {
		let old_script   = self.current_script  .get();
		let old_fn_id = self.current_export_fn_id.get();
		self.current_script  .set(Some(entity.file_id));
		self.current_export_fn_id.set(Some(fn_id));

		let ret_val = unsafe {
			self.backend.call_on_function_raw(self, entity, self.get_export_fn_index(fn_id), values)
		};

		self.current_script  .set(old_script);
		self.current_export_fn_id.set(old_fn_id);

		ret_val
	}

	#[must_use]
	pub fn call_on_function(&self, entity: &GrugEntity, fn_id: GrugOnFnId, values: &[GrugValue]) -> bool {
		let old_script   = self.current_script  .get();
		let old_fn_id = self.current_export_fn_id.get();
		self.current_script  .set(Some(entity.file_id));
		self.current_export_fn_id.set(Some(fn_id));

		let ret_val = self.backend.call_on_function(self, entity, self.get_export_fn_index(fn_id), values);

		self.current_script  .set(old_script);
		self.current_export_fn_id.set(old_fn_id);

		ret_val
	}
}

// TODO: This should be moved to gruggers-core
pub struct ExportFnEntry<'a> {
	entity_type   : NTStrPtr<'a>,
	event_fn_name : NTStrPtr<'a>,
	pub index      : usize,
}

impl<'a> ExportFnEntry<'a> {
	/// Turns the null terminated string representing the entity name into a [`&str`]
	pub fn entity_type(&self) -> &str {
		self.entity_type.to_str()
	}
	/// Turns the null terminated string representing the event function name into a [`&str`]
	pub fn event_fn_name(&self) -> &str {
		self.event_fn_name.to_str()
	}
}

const _: () = const{
	// The C interop with Rust assumes that slice pointers have a layout like this
	// #[repr(C)]
	// struct Slice<T> {
	// 		data: NonNull<T>,
	// 		len : usize,
	// }
	// 
	// The rust compiler currently does not guarantee the layout of slice pointer.
	// These assertions ensure that if the assumption is broken, we get a
	// compile error instead of random crashes
	let x: &[ExportFnEntry] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
};

/// A pointer to a grug entity. Only allows shared access to the data and does
/// not allow copying or cloning. Lifetime of shared borrows are limited to the lifetime of self
#[repr(transparent)]
pub struct GrugEntityHandle<'a>(XarHandle<'a, GrugEntity>);

impl<'a> GrugEntityHandle<'a> {
	/// # SAFETY
	/// inner can only be deleted by deleting the returned value
	/// `GrugEntityHandle` is `Deref<Target> = GrugEntity`, so
	/// the returned value is allowed to create a shared reference to the data at any time 
	pub unsafe fn new(inner: XarHandle<'a, GrugEntity>) -> Self {
		Self(inner)
	}

	pub fn into_inner(self) -> XarHandle<'a, GrugEntity> {
		self.0
	}
}

impl<'a> AsRef<GrugEntity> for GrugEntityHandle<'a> {
	fn as_ref(&self) -> &GrugEntity {
		unsafe{self.0.get_ref()}
	}
}

impl<'a> std::ops::Deref for GrugEntityHandle<'a> {
	type Target = GrugEntity;
	fn deref(&self) -> &Self::Target {
		unsafe{self.0.get_ref()}
	}
}

mod files {
	use crate::own_ptr::OwnPtr;
	use crate::arena::Arena;
	use crate::ntstring::{NTBytes, NTStrPtr};
	use crate::types::GrugFileId;
	use crate::error::GrugError;
	use crate::state::INVALID_GRUG_SCRIPT_ID;

	use std::ffi::OsStr;
	use std::path::Path;
	use std::mem::MaybeUninit;

	pub struct Files {
		/// Fuck man, we just need 'unsafe already
		pub(crate) inner: OwnPtr<'static, [FileInfo<'static>]>,
		pub(crate) _arena: Arena,
	}

	impl std::fmt::Debug for Files {
		fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
			self.files().fmt(f)
		}
	}

	impl Files {
		pub fn empty() -> Self {
			Self {
				inner: (Box::new([]) as Box<[_]>).into(),
				_arena: Arena::new(),
			}
		}

		/// Get the list of files that were compiled or recompiled
		pub fn files<'a>(&'a self) -> &'a [FileInfo<'a>] {
			&*self.inner
		}
	}

	// Test struct for c api
	// Eventually replace FileInfo with this
	#[derive(Debug, Clone, Copy)]
	#[repr(C)]
	pub struct FileInfo<'a> {
		/// Full path to the file relative to the mods directory
		pub(crate) path: NTBytes<'a>,
		/// Filename component of the path
		pub(crate) file_name: NTBytes<'a>,
		/// first level directory within the mods directory
		// TODO: Check that mods directly within the mods directory don't
		// cause problems. This is technically disallowed by grug but grugc
		// uses this behavior
		pub(crate) mod_name: NTBytes<'a>,
		/// Portion of the filename between the '-' and '.'
		pub(crate) entity_type: NTStrPtr<'a>,
		/// Portion of the filename before the '-'
		pub(crate) entity_name: NTBytes<'a>,
		/// These two files are actually a Result<GrugFileId, GrugError<'a>>
		/// Err case is when file_id === INVALID_GRUG_SCRIPT_ID
		pub(crate) file_id: GrugFileId,
		pub(crate) error: MaybeUninit<GrugError<'a>>,
	}

	impl<'a> FileInfo<'a> {
		pub(crate) fn new_in(path: &OsStr, file_name: &OsStr, mod_name: &OsStr, entity_type: &str, entity_name: &OsStr, result: Result<GrugFileId, GrugError>, arena: &'a Arena) -> Self {
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let path = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(path.as_encoded_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let file_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(file_name.as_encoded_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let mod_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(mod_name.as_encoded_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice,
			// and the returned slice is utf8 encoded because it comes from a
			// str
			let entity_type = arena.copy_str_into_nt(entity_type).as_ntstrptr();
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let entity_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(entity_name.as_encoded_bytes()))};
			let (file_id, error) = match result {
				Ok(id) => (id, MaybeUninit::uninit()),
				Err(err) => (INVALID_GRUG_SCRIPT_ID, MaybeUninit::new(err.copy_into(arena)))
			};
			FileInfo {
				path,
				file_name,
				mod_name,
				entity_type,
				entity_name,
				file_id,
				error
			}
		}
		pub fn copy_into<'b>(&self, arena: &'b Arena) -> FileInfo<'b> {
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let path = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(self.path.to_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let file_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(self.file_name.to_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let mod_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(self.mod_name.to_bytes()))};
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice,
			// and the returned slice is utf8 encoded because it comes from a
			// str
			let entity_type = arena.copy_str_into_nt(self.entity_type.to_str()).as_ntstrptr();
			// Safety: `copy_bytes_into_nt` returns a null terminated byte slice
			let entity_name = unsafe{NTBytes::from_bytes_unchecked(arena.copy_bytes_into_nt(self.entity_name.to_bytes()))};
			let (file_id, error) = if self.file_id == INVALID_GRUG_SCRIPT_ID {
				// SAFETY: self.error is intialized if self.file_id == INVALID_GRUG_SCRIPT_ID
				(INVALID_GRUG_SCRIPT_ID, MaybeUninit::new(unsafe{self.error.assume_init()}.copy_into(arena)))
			} else {
				(self.file_id, MaybeUninit::uninit())
			};
			FileInfo {
				path,
				file_name,
				mod_name,
				entity_type,
				entity_name,
				file_id,
				error
			}
		}
		pub fn path (&self) -> &Path {
			OsStr::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(self.path.to_bytes())})
		}
		pub fn file_name (&self) -> &OsStr {
			unsafe{OsStr::from_encoded_bytes_unchecked(self.file_name.to_bytes())}
		}
		pub fn mod_name (&self) -> &OsStr {
			unsafe{OsStr::from_encoded_bytes_unchecked(self.mod_name.to_bytes())}
		}
		pub fn entity_type (&self) -> &str {
			self.entity_type.to_str()
		}
		pub fn entity_name (&self) -> &OsStr {
			unsafe{OsStr::from_encoded_bytes_unchecked(self.entity_name.to_bytes())}
		}
		pub fn result (&self) -> Result<GrugFileId, GrugError<'_>> {
			if self.file_id == INVALID_GRUG_SCRIPT_ID {unsafe{Err(*self.error.assume_init_ref())}}
			else {Ok(self.file_id)}
		}
	}

}
pub use files::*;
