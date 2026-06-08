use crate::state::{GrugState, Files, FileInfo};
use crate::arena::Arena;
use crate::types::GrugFileId;
use crate::ast::*;
use crate::ntstring::NTStrPtr;
use crate::error::{Error, ErrorKind, SourceSpan};
use crate::mod_api::ModApi;
use crate::own_ptr::OwnPtr;
use gruggers_core::types::GameFnPtr;

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box as Box2;

use std::ffi::{OsStr, OsString};
use std::path::PathBuf;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{RwLock, RwLockReadGuard, Arc};
use std::sync::mpsc::{Receiver, Sender};
// use std::path::Path;

const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod tokenizer;
pub mod parser;
mod type_propagation;
use type_propagation::TypePropogator;

// Compilation functions
impl GrugState {
	/// Send at most this many files to a thread for compilation
	const FILES_PER_THREAD: usize = 8;
	// All 'static fields are actually allocated within the arena
	/// Uses async file system apis on windows
	pub(crate) fn compiler_thread_fn(
		// these paths are relative
		receiver: Receiver<(Arena, &'static [&'static OsStr])>,
		sender: Sender<(
			Arena, 
			OwnPtr<'static, [(
				Result<GrugAst<'static>, Error>, 
				// derived from input paths
				// These paths are relative
				&'static OsStr
			)]>, 
			// Resources
			// These paths are relative
			&'static [&'static OsStr]
		)>,
		// path is absolute
		mods_dir_path: OsString,
		mod_api: Arc<ModApi>,
		host_fn_ptrs: Arc<RwLock<HashMap<&'static str, GameFnPtr>>>,
	) -> impl FnOnce() {
		use crate::async_fs::{open_file_async_for_read, read_files_async};
		let mods_dir_path = PathBuf::from(mods_dir_path);
		move || {
			for (arena, files) in receiver.iter() {
				let host_fn_ptrs = host_fn_ptrs.read().unwrap();
				let mut resources = Vec::new_in(&arena);
				// This is the actual lifetime of the data but it has to be erased to send across the channel
				fn combine_lifetimes<'a>(_: &'a Arena, input: &'static [&'static OsStr]) -> &'a [&'a OsStr] {input}
				let files = combine_lifetimes(&arena, files);

				let mut results = Vec::new_in(&arena);
				// read all files
				// split into files that can be opened and files that can't
				let mut ok_files = Vec::new_in(&arena);
				for file_path in files.iter().copied() {
					let mut abs_path = mods_dir_path.clone();
					abs_path.push(file_path);
					match open_file_async_for_read(&abs_path) {
						Ok(file) => {
							ok_files.push((file, file_path)); 
						},
						Err(err) => {
							results.push((Err(err), file_path));
						}
					}
				}
				// read the contents of files that can be read
				let ok_files_data = read_files_async(ok_files.iter().map(|(file, _)| file), &arena);
				// compile files one by one and collect errors, asts and resources
				results.extend(ok_files_data.into_iter().zip(&ok_files).map(|(data, (_, path))| {
					let path = *path;
					let data = match data {
						Ok(data) => data,
						Err(err) => return (Err(err), path),
					};

					// convert to utf8
					let file_text = match std::str::from_utf8(data).map_err(|err| {
						Error::new(
							ErrorKind::UTF8_ERROR,
							"",
							path, 
							// SAFETY: err.valid_up_to returns the length of the
							// portion of the string that is valid utf8
							unsafe{std::str::from_utf8_unchecked(&data[..err.valid_up_to()])},
							SourceSpan{offset: err.valid_up_to(), line: 0},
							format_args!("File is not valid utf8: {}", err),
						)
					}) {
						Ok(file_text) => file_text,
						Err(err) => return (Err(err), path),
					};
					// compile the file
					let (ast, current_resources) = match Self::compile_inner(
						path,
						file_text,
						mods_dir_path.as_ref(),
						&mod_api,
						&host_fn_ptrs,
						&arena
					) {
						Ok(data) => data,
						Err(err) => return (Err(err), path)
					};
					// add resource paths 
					resources.extend_from_slice(current_resources);
					// collect errors and resources
					(Ok(ast), path)
				}));
				drop(ok_files);


				let results = unsafe{std::mem::transmute::<
					OwnPtr<[(
						Result<
							GrugAst<'_>,
							Error
						>, 
						&OsStr
					)]>, 
					OwnPtr<'static, [(
						Result<
							GrugAst<'static>,
							Error
						>, 
						&'static OsStr
					)]>
				>(results.into_boxed_slice().into())};
				let resources = unsafe{std::mem::transmute::<&[&OsStr], &'static[&'static OsStr]>(resources.leak())};
				let Ok(()) = sender.send((arena, results, resources)) else {break;};
			}
		}
	}

	/// Compile a grug file at a relative path within the mods directory. Once
	/// compiled directly once, the file will be automatically hot reloaded by
	/// the state. 
	pub fn compile_grug_file(&self, path: impl AsRef<OsStr>) -> Result<GrugFileId, Error> {
		let path = path.as_ref();
		let mut path_buf = self.mods_dir_path.clone();
		path_buf.push("/");
		path_buf.push(path);
		
		let file_text = match std::fs::read_to_string(path_buf) {
			Ok(file_text) => file_text,
			Err(err) => return Err(Error::from_io_error(err, path))
		};

		self.compile_grug_file_from_str(path, &file_text)
	}

	/// Compile a grug file directly from a string. The path is required to
	/// identify the entity type and to uniquely identify a particular script
	/// for hot reloading.
	///
	/// If the path is the same as a path within the mods directory, when the
	/// actual file changes, the script will be hot reloaded. 
	///
	/// If a file has already been compiled with the same path, the script will
	/// be hot reloaded.
	pub fn compile_grug_file_from_str(&self, path: impl AsRef<OsStr>, file_text: &str) -> Result<GrugFileId, Error> {
		use super::frontend::*;
		let path = path.as_ref();

		let mut arena = self.arenas.borrow_mut().pop().unwrap_or_default();
		// immediately invoked closure so we get try {} finally {}
		let id = (|| {
			let host_fn_ptrs = self.host_fn_ptrs.read().unwrap();
			let (file, resources) = Self::compile_inner(path, file_text, &self.mods_dir_path, &self.mod_api, &host_fn_ptrs, &arena)?;
			let mut self_resources = self.resources.borrow_mut();
			for resource in resources {
				if !self_resources.contains(*resource) {self_resources.insert(OsString::from(resource));}
			}
			let mut path_to_script_ids = self.path_to_script_ids.borrow_mut();
			let id = match path_to_script_ids.get(path) {
				Some(id) => *id,
				None => {
					let id = self.get_next_script_id();
					assert!(path_to_script_ids.insert(OsString::from(path), id).is_none());
					id
				}
			};
			self.backend.insert_file(self, id, file);
			Ok(id)
		})();
		arena.clear();
		self.arenas.borrow_mut().push(arena);
		
		id
	}

	/// Compile all the files within the mods directory. Uses asynchronous file
	/// system apis if available on the current platform
	pub fn compile_all_files(&self) -> Files {
		// iterate over all files and get all valid paths
		let arena = self.arenas.borrow_mut().pop().unwrap_or_else(Arena::new);

		let mut file_paths = Vec::new_in(&arena);
		let mut files = std::vec::Vec::new();

		let mods_dir_len = if self.mods_dir_path.as_encoded_bytes().last().is_some_and(|x| *x != b'\\' && *x != b'/') {self.mods_dir_path.len() + 1} else {self.mods_dir_path.len()};
		// Iterate through every directory within the mods directory and
		// collect relative paths to all grug scripts
		for mod_dir in std::fs::read_dir(&self.mods_dir_path).expect("Could not read mods directory") {
			let Ok(mod_dir) = mod_dir else {
				panic!("unable to read directory: {:?}", mod_dir);
			};
			let mut entries_to_check = std::vec::Vec::from([mod_dir]);

			while let Some(next_entry) = entries_to_check.pop() {
				if next_entry.metadata().expect("could not read metadata").is_dir() {
					let next_entry_path = next_entry.path();
					for entry in std::fs::read_dir(&next_entry_path).expect("Could not read mods directory") {
						let Ok(entry) = entry else {
							panic!("unable to read entry: {:?}", entry);
						};
						entries_to_check.push(entry);
					}
				} else {
					let entry_path = next_entry.path();
					if let Some(extension) = entry_path.extension() && extension == "grug" {
						// get path relative to mods dir
						let rel_path = unsafe{OsStr::from_encoded_bytes_unchecked(&entry_path.as_os_str().as_encoded_bytes()[mods_dir_len..])};
						// copy path into arena
						let rel_path = arena.copy_osstr_into(rel_path);
						file_paths.push(rel_path);
					};
				}
			}
		}

		let mut next_thread = self.compiler_senders.iter().cycle();
		let sent_count = file_paths.len();
		let mut recv_count = 0;
		// Chunk into FILES_PER_THREAD sized blocks
		for chunk in file_paths.chunks(Self::FILES_PER_THREAD) {
			let cur_arena = self.arenas.borrow_mut().pop().unwrap_or_else(Arena::new);

			// We need to allocate the paths into the new arena to ensure panic safety
			//
			// If these strings were allocated in the outer arena (or if
			// they are a reference to the existing PathBufs), then if this
			// thread panics for any reason, the strings will be freed and
			// the compiler threads will access freed memory.
			let chunk = cur_arena.slice_from_iter(chunk.iter().map(|item| cur_arena.copy_osstr_into(item.as_ref())));
			// SAFETY: make sure we never use this slice outside the current loop iteration
			let chunk = unsafe{std::mem::transmute::<&[&OsStr], &'static [&'static OsStr]>(chunk)};
			// Send to threads
			next_thread.next().expect("at least one compiler thread").send((cur_arena, chunk)).expect("send succeeds");
		}

		// Send to backend while recieving
		while recv_count < sent_count {
			let (mut current_arena, results, resources) = self.compiler_receiver.recv().unwrap(); 
			recv_count += results.len();

			let mut path_to_script_ids = self.path_to_script_ids.borrow_mut();
			for (result, path) in results {
				// turn ok result into id, allocate error into outer arena
				let result = match result {
					Ok(ast) => {
						let id = match path_to_script_ids.get(path) {
							Some(id) => *id,
							None => {
								let id = self.get_next_script_id();
								assert!(path_to_script_ids.insert(OsString::from(path), id).is_none());
								id
							}
						};
						// Send to backend
						self.backend.insert_file(self, id, ast);
						Ok(id)
					}
					Err(err) => {
						let err = err.inner().copy_into(&arena);
						Err(err)
					}
				};
				// Create FileInfo from this result
				let path = <OsStr as AsRef<Path>>::as_ref(path);
				let mod_dir_path = path.parent().expect("must have at least component in path").components().next().unwrap().as_os_str();
				let info = FileInfo::new_in(
					path.as_os_str(),
					path.file_name().unwrap(),
					mod_dir_path,
					get_entity_type(path.as_os_str()).unwrap_or(""),
					path.file_prefix().unwrap(),
					result,
					&arena
				);
				files.push(info);
			}
			let mut self_resources = self.resources.borrow_mut();
			for resource in resources {
				if !self_resources.contains(*resource) {
					self_resources.insert(OsString::from(resource));
				}
			}
			// `results` and `resources` are allocated within current_arena, so it
			// is only safe to clear the current_arena now.
			current_arena.clear();
			self.arenas.borrow_mut().push(current_arena);
		}
		drop(file_paths);
		Files {
			inner: unsafe{std::mem::transmute::<OwnPtr<[FileInfo]>, OwnPtr<'static, [FileInfo]>>(files.into_boxed_slice().into())},
			_arena: arena,
		}
	}

	/// Check if there are any files in the mods directory that need to be hot reloaded. 
	/// Also returns any resources that need to be reloaded
	pub fn update_files(&self) -> (std::vec::Vec<OsString>, Files) {
		let arena = self.arenas.borrow_mut().pop().unwrap_or_else(Arena::new);
		let mut file_paths = Vec::new_in(&arena);
		let mut updated_resources = std::vec::Vec::new();
		
		let mut grug_files = std::vec::Vec::new();
		for change in self.changes.try_iter() {
			let file_name = change.expect("File IO error");
			if let Some(extension) = <OsStr as AsRef<Path>>::as_ref(&file_name).extension() && extension == "grug" {
				if !file_paths.contains(&&*file_name) {
					let rel_name = arena.copy_osstr_into(file_name.as_ref());
					file_paths.push(rel_name);
				}
			}
			if self.resources.borrow().contains(&file_name) {
				if !updated_resources.contains(&file_name) {
					updated_resources.push(file_name);
				}
			}
		}

		let mut next_thread = self.compiler_senders.iter().cycle();
		let sent_count = file_paths.len();
		let mut recv_count = 0;
		// Chunk into FILES_PER_THREAD sized blocks
		for chunk in file_paths.chunks(Self::FILES_PER_THREAD) {
			let cur_arena = self.arenas.borrow_mut().pop().unwrap_or_else(Arena::new);

			// We need to allocate the paths into the new arena to ensure panic safety
			//
			// If these strings were allocated in the outer arena (or if
			// they are a reference to the existing PathBufs), then if this
			// thread panics for any reason, the strings will be freed and
			// the compiler threads will access freed memory.
			let chunk = cur_arena.slice_from_iter(chunk.iter().map(|item| cur_arena.copy_osstr_into(item.as_ref())));
			// SAFETY: make sure we never use this slice outside the current loop iteration
			let chunk = unsafe{std::mem::transmute::<&[&OsStr], &'static [&'static OsStr]>(chunk)};
			// Send to threads
			next_thread.next().expect("at least one compiler thread").send((cur_arena, chunk)).expect("send succeeds");
		}

		// Send to backend while recieving
		while recv_count < sent_count {
			let (mut current_arena, results, resources) = self.compiler_receiver.recv().unwrap(); 
			recv_count += results.len();

			let mut path_to_script_ids = self.path_to_script_ids.borrow_mut();
			for (result, path) in results {
				// turn ok result into id, copy err into current arena
				let result = match result {
					Ok(ast) => {
						let id = match path_to_script_ids.get(path) {
							Some(id) => *id,
							None => {
								let id = self.get_next_script_id();
								assert!(path_to_script_ids.insert(OsString::from(path), id).is_none());
								id
							}
						};
						// Send to backend
						self.backend.insert_file(self, id, ast);
						Ok(id)
					}
					Err(err) => {
						let err = err.inner().copy_into(&arena);
						Err(err)
					}
				};
				// Create FileInfo from this result
				let path = <OsStr as AsRef<Path>>::as_ref(path);
				println!("{:?}", path);
				let mod_dir_path = path.parent().expect("must have at least component in path").components().next().unwrap().as_os_str();
				let info = FileInfo::new_in(
					path.as_os_str(),
					path.file_name().unwrap(),
					mod_dir_path,
					get_entity_type(path.as_os_str()).unwrap_or(""),
					path.file_prefix().unwrap(),
					result,
					&arena
				);
				grug_files.push(info);
			}
			let mut self_resources = self.resources.borrow_mut();
			for resource in resources {
				if !self_resources.contains(*resource) {
					self_resources.insert(OsString::from(resource));
				}
			}
			// `results` and `resources` are allocated within arena, so it
			// is only safe to clear the arena now.
			current_arena.clear();
			self.arenas.borrow_mut().push(current_arena);
		}
		drop(file_paths);
		let grug_files = Files {
			inner: unsafe{std::mem::transmute::<OwnPtr<[FileInfo]>, OwnPtr<'static, [FileInfo]>>(grug_files.into_boxed_slice().into())},
			_arena: arena,
		};
		(updated_resources, grug_files)
	}

	/// Merge threaded compilation and standalone compilation
	fn compile_inner<'arena>(
		path: &'arena OsStr, 
		file_text: &'arena str, 
		mods_dir_path: &'arena OsStr, 
		mod_api: &'arena ModApi, 
		host_fn_ptrs: &'arena RwLockReadGuard<HashMap<&'static str, GameFnPtr>>, 
		arena: &'arena Arena
	) -> Result<(GrugAst<'arena>, &'arena [&'arena OsStr]), Error> {
		let mod_name = get_mod_name(path);
		let entity_type = get_entity_type(path)?;

		// tokenize
		let tokens = tokenizer::tokenize(file_text, arena, path)?;
		// parse
		let mut ast = parser::parse(tokens.leak(), arena, file_text, path)?;

		// get mod api entity declaration
		let entity = mod_api.entities().get(entity_type).ok_or_else(|| 
			// TODO: This is not handled by grug_tests
			Error::new(
				ErrorKind::FILE_NAME_ERROR,
				"",
				path, 
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Entity '{}' is not registered in the mod_api.json", entity_type),
			)
		)?;
		// get mod_api host function declarations
		let mod_api_host_fns = mod_api.host_fns();
		
		// type check 
		let resources = TypePropogator::fill_result_types(
			entity, 
			mod_api_host_fns, 
			host_fn_ptrs,
			mod_name, 
			mods_dir_path, 
			file_text, 
			path,
			entity_type,
			&mut ast,
			arena,
		)?;

		// convert into GrugAst
		let mut member_variables = Vec::new_in(arena);
		let mut on_functions = Vec::new_in(arena);
		on_functions.extend((0..entity.export_fns.len()).map(|_| None));
		let mut helper_functions = Vec::new_in(arena);

		ast.global_statements.into_iter().for_each(|statement| {
			match statement {
				GlobalStatement::Variable(st@MemberVariable      {..}) => member_variables.push(st),
				GlobalStatement::OnFunction(st@OnFunction        {..}) => {
					let (i, _) = entity.get_export_fn(st.name.to_str()).unwrap();
					on_functions[i] = Some(&*Box2::leak(Box2::new_in(st, arena)));
				}
				GlobalStatement::HelperFunction(st@HelperFunction{..}) => helper_functions.push(st),
				_ => (),
			}
		});

		let file = GrugAst{
			members: member_variables.leak(),
			on_functions: on_functions.leak(),
			helper_functions: helper_functions.leak(),
		};
		Ok((file, resources))
	}
}

/// A top level statement in a grug file.
///
/// This is not passed through [`GrugAst`] but is instead supposed to be used
/// internally by a grug state implementation
#[derive(Debug)]
pub(crate) enum GlobalStatement<'a> {
	/// A member variable
	/// `x: number = 25`
	Variable(MemberVariable<'a>),
	/// An on function declaration
	/// ```text
	/// on_init(id: number) {
	///     set_max_health(50)
	///     set_unarmed_damage(2)
	///     set_weapon("sword.json")
	/// }
	/// ```
	OnFunction(OnFunction<'a>),
	/// A helper function declaration
	/// ```text
	/// helper_color(n: number) Color {
	///     if n == 0 {
	///         return color("blue")
	///     } else if n == 1 {
	///         return color("red")
	///     } else if n == 2 {
	///         return color("green")
	///     } else if n == 3 {
	///         return color("yellow")
	///     } else if n == 3 {
	///         return color("black")
	///     } 
	///     return game_fn_error("invalid color id")
	/// }
	/// ```
	HelperFunction(HelperFunction<'a>),
	/// A comment at the top level of a file
	/// ```text
	/// ## This is a global comment
	/// shared_number: number = 0
	/// ```
	Comment{
		value: NTStrPtr<'a>,
	},
	/// An Empty line at the top level of a script
	EmptyLine,
}

/// The mods directory contains a directory for each mod. Each mod may contain
/// several scripts. This function returns the name of the current mod
/// directory from the full path.
fn get_mod_name (path: &OsStr) -> &OsStr {
	let path = path.as_encoded_bytes();
	let mut slash_len = 0;
	for (i, ch) in path.iter().enumerate() {
		if *ch == b'/' || *ch == b'\\' {slash_len = i; break;}
	}
	// SAFETY: Next byte is b'/' which is valid utf8 or the length is 0.
	unsafe{OsStr::from_encoded_bytes_unchecked(&path[..slash_len])}
	
	// This restrict isn't checked in grug_tests and it gets in the way of
	// implementing the compiler in the simplest way
	// path.split_once('/').map(|x| x.0).ok_or(GrugError::FileError(FileError::FilePathDoesNotContainForwardSlash{path: String::from(path)}))
}

/// The filename of a grug script should be as follows <name>-<entity>.grug
///
/// Where <name> can be any utf8 string, and <entity> refers to the entity type
/// the file contains.
///
/// This function returns <entity> for a given file path (if <entity> exists)
fn get_entity_type(path: &OsStr) -> Result<&str, Error> {
	let mut dot_pos = None;
	let mut dash_pos = None;
	let path_bytes = path.as_encoded_bytes();
	let file_name = <OsStr as AsRef<Path>>::as_ref(path).file_name().unwrap_or("".as_ref());
	for (i, ch) in path_bytes.iter().enumerate().rev() {
		match (ch, dot_pos, dash_pos) {
			(b'.', None, None) => dot_pos = Some(i),
			(b'-', None, None) => 
				return Err(Error::new(
					ErrorKind::FILE_NAME_ERROR,
					"",
					path, 
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("'{}' is missing a period in its filename", file_name.display())
				)),
			(b'-', Some(_), None) => {dash_pos = Some(i); break;},
			_ => (),
		}
	}
	let (dot_pos, dash_pos) = match (dot_pos, dash_pos) {
		(Some(dot_pos), Some(dash_pos)) if dot_pos == dash_pos + 1 => {
			return Err(Error::new(
				ErrorKind::FILE_NAME_ERROR,
				"",
				path, 
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("'{}' is missing an entity type in its name", file_name.display())
			));
		}
		(Some(dot_pos), Some(dash_pos)) => (dot_pos, dash_pos),
		_ => {
			return Err(Error::new(
				ErrorKind::FILE_NAME_ERROR,
				"",
				path, 
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("'{}' is missing an entity type in its name", file_name.display())
			));
		}
	};
	// SAFETY: dash_pos is b'-' which is valid utf8, and dot_pos is b'.' which
	// is also utf8 so (dash_pos+1)..dot_pos will not truncate a utf8 codepoint
	let entity_type = unsafe{OsStr::from_encoded_bytes_unchecked(&path_bytes[(dash_pos + 1)..dot_pos])};
	if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
		return Err(Error::new(
			ErrorKind::FILE_NAME_ERROR,
			"",
			path, 
			"",
			SourceSpan{offset: 0, line: 0},
			format_args!("There are more than {} characters \n\
				in the entity type of '{}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH", 
				entity_type.len(), path.display()
			)
		));
	}
	check_custom_id_is_pascal(entity_type, path)
}

/// Entities and Id types in grug must be pascalCase.
fn check_custom_id_is_pascal<'a>(entity_type: &'a OsStr, path: &'_ OsStr) -> Result<&'a str, Error> {
	let entity_type = entity_type.to_str().ok_or_else(|| 
		Error::new(
			ErrorKind::FILE_NAME_ERROR,
			"",
			entity_type, 
			"",
			SourceSpan{offset: 0, line: 0},
			format_args!("'{}' is not valid utf8", 
				entity_type.display()
			)
		)
	)?;
	let mut chars = entity_type.chars();
	if let Some(first) = chars.next() && !first.is_uppercase() {
		return Err(Error::new(
			ErrorKind::FILE_NAME_ERROR,
			"",
			path, 
			"",
			SourceSpan{offset: 0, line: 0},
			format_args!("'{entity_type}' seems like a custom ID type, but it doesn't start in Uppercase")
		));
	}
	for ch in chars {
		if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_ascii_digit()) {
			return Err(Error::new(
				ErrorKind::FILE_NAME_ERROR,
				"",
				path, 
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("'{entity_type}' seems like a custom ID type, but it contains '{ch}', which isn't uppercase, lowercase, or a digit", )
			));
		}
	}
	Ok(entity_type)
}

