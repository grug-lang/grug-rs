use crate::state::{GrugState, FileInfo};
use crate::arena::Arena;
use crate::types::GrugFileId;
use crate::ast::*;
use crate::ntstring::NTStrPtr;
use crate::error::{Error, ErrorKind, SourceSpan};

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box as Box2;

use std::ffi::{OsStr, OsString};
use std::path::Path;
// use std::path::Path;

const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod tokenizer;

pub mod parser;

// Compilation functions
impl GrugState {
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
			Err(err) => return Err(Error::new(
				ErrorKind::IO_ERROR,
				"",
				path, 
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Unable to open file: {}", err)
			))
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

		let mod_name = get_mod_name(path);
		let entity_type = get_entity_type(path)?;

		let mut arena = self.arenas.borrow_mut().pop().unwrap_or_default();
		// immediately invoked closure so we get try {} finally {}
		let id = (|| {
			let tokens = tokenizer::tokenize(file_text, &arena, path)?;

			let mut ast = parser::parse(&tokens, &arena, file_text, path)?;

			let entity = self.mod_api.entities().get(entity_type).ok_or_else(|| 
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
			let game_functions = self.mod_api.host_fns();
			
			let resources = TypePropogator::new(
				entity, 
				game_functions, 
				&self.game_functions, 
				mod_name, 
				&self.mods_dir_path, 
				file_text, 
				path
			).fill_result_types(entity_type, &mut ast, &arena)?;
			self.resources.lock().expect("poisoned mutex").extend(resources);

			// let mod_api_entity = self.mod_api.entities.get(entity_type);
			let mut member_variables = Vec::new_in(&arena);
			let mut on_functions = Vec::new_in(&arena);
			on_functions.extend((0..entity.export_fns.len()).map(|_| None));
			let mut helper_functions = Vec::new_in(&arena);

			ast.global_statements.into_iter().for_each(|statement| {
				match statement {
					GlobalStatement::Variable(st@MemberVariable      {..}) => member_variables.push(st),
					GlobalStatement::OnFunction(st@OnFunction        {..}) => {
						let (i, _) = entity.get_export_fn(st.name.to_str()).unwrap();
						on_functions[i] = Some(&*Box2::leak(Box2::new_in(st, &arena)));
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
	
	/// Compile all the files within the mods directory. 
	pub fn compile_all_files(&self) -> std::vec::Vec<FileInfo> {
		let mut files = std::vec::Vec::new();
		let mods_dir_len = if self.mods_dir_path.as_encoded_bytes().last().is_some_and(|x| *x != b'\\' && *x != b'/') {self.mods_dir_path.len() + 1} else {self.mods_dir_path.len()};
		for mod_dir in std::fs::read_dir(&self.mods_dir_path).expect("Could not read mods directory") {
			let Ok(mod_dir) = mod_dir else {
				panic!("unable to read directory: {:?}", mod_dir);
			};
			let mod_dir_path = mod_dir.path();
			let mod_dir_path = unsafe{OsStr::from_encoded_bytes_unchecked(&mod_dir_path.as_os_str().as_encoded_bytes()[mods_dir_len..])};
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
					let entry_path = entry_path.as_os_str();
					let rel_path = entry_path.as_encoded_bytes();
					let rel_path = &rel_path[mods_dir_len..];
					let rel_path = <OsStr as AsRef<Path>>::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)});

					if let Some(extension) = Path::extension(rel_path) && extension == "grug" {
						let result = self.compile_grug_file(rel_path);
						let info = FileInfo {
							path: Box::from(rel_path),
							file_name: Box::from(rel_path.file_name().unwrap()),
							mod_name: Box::from(mod_dir_path),
							entity_type: Box::from(get_entity_type(rel_path.as_os_str()).unwrap_or("")),
							entity_name: Box::from(rel_path.file_prefix().unwrap()),
							result
						};
						files.push(info);
					};
				}
			}
		}
		files
	}

	/// Check if there are any files in the mods directory that need to be hot reloaded. 
	/// Also returns any resources that need to be reloaded
	pub fn update_files(&self) -> (std::vec::Vec<OsString>, std::vec::Vec<FileInfo>) {
		let mut resource_files = std::vec::Vec::new();
		let mut grug_files = std::vec::Vec::new();
		for change in self.changes.try_iter() {
			let file_name = change.expect("File IO error");
			{
				let file_name: &Path = file_name.as_ref();
				if let Some(extension) = Path::extension(file_name) && extension == "grug" {
					// This is actually not true of this library, although it is true for grug in general
					// the grug cli allows scripts to not have a parent directory, but it does not call this function
					let mod_dir_path = file_name.parent().expect("mod must have parent for successful compilation");
					let result = self.compile_grug_file(file_name);
					let info = FileInfo {
						path: Box::from(file_name),
						file_name: Box::from(file_name.file_name().unwrap()),
						mod_name: Box::from(mod_dir_path.as_os_str()),
						entity_type: Box::from(get_entity_type(file_name.as_os_str()).unwrap_or("")),
						entity_name: Box::from(file_name.file_prefix().unwrap()),
						result
					};
					grug_files.push(info);
				}
			}
			if self.resources.lock().unwrap().contains(&file_name) {
				resource_files.push(file_name);
			}
		}
		(resource_files, grug_files)
	}
	
	/// Compile all the files within the mods directory using async file system calls
	pub fn compile_all_files_async(&self) -> std::vec::Vec<FileInfo> {
		use crate::async_fs::{open_file_async_for_read, read_files_async};
		let mut arena = self.arenas.borrow_mut().pop().unwrap_or_else(Arena::new);

		let mut files = std::vec::Vec::new();

		let mods_dir_len = if self.mods_dir_path.as_encoded_bytes().last().is_some_and(|x| *x != b'\\' && *x != b'/') {self.mods_dir_path.len() + 1} else {self.mods_dir_path.len()};
		for mod_dir in std::fs::read_dir(&self.mods_dir_path).expect("Could not read mods directory") {
			arena.clear();
			let Ok(mod_dir) = mod_dir else {
				panic!("unable to read directory: {:?}", mod_dir);
			};
			let mod_dir_path = mod_dir.path();
			let mod_dir_path = unsafe{OsStr::from_encoded_bytes_unchecked(&mod_dir_path.as_os_str().as_encoded_bytes()[mods_dir_len..])};
			let mut mod_files = Vec::new_in(&arena);
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
					if let Some(extension) = Path::extension(&entry_path) && extension == "grug" {
						mod_files.push(entry_path);
					};
				}
			}

			// split into files that can be opened and files that can't
			let mut ok_files = Vec::new_in(&arena);
			let mut ok_files_paths = Vec::new_in(&arena);
			for file_path in mod_files {
				match open_file_async_for_read(&file_path) {
					Ok(file) => {
						ok_files.push(file); 
						ok_files_paths.push(file_path)
					},
					Err(err) => {
						// get the path relative to the mods directory
						let rel_path = &file_path.as_os_str().as_encoded_bytes()[mods_dir_len..];
						let rel_path = <OsStr as AsRef<Path>>::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)});
						
						let info = FileInfo {
							path: Box::from(rel_path),
							file_name: Box::from(rel_path.file_name().unwrap()),
							mod_name: Box::from(mod_dir_path),
							entity_type: Box::from(get_entity_type(rel_path.as_os_str()).unwrap_or("")),
							entity_name: Box::from(rel_path.file_prefix().unwrap()),
							result: Err(Error::new(
								ErrorKind::IO_ERROR,
								"",
								file_path.as_os_str(), 
								"",
								SourceSpan{offset: 0, line: 0},
								format_args!("Unable to open file: {}", err)
							)),
						};
						files.push(info);
					}
				}
			}
			// read the contents of files that can be read
			let ok_files_data = read_files_async(&ok_files, &arena).unwrap();
			// compile the files that can be read
			ok_files_data.into_iter().zip(&ok_files_paths).map(|(data, path)| {
				// get the path relative to the mods directory
				let rel_path = &path.as_os_str().as_encoded_bytes()[mods_dir_len..];
				let rel_path = <OsStr as AsRef<Path>>::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)});

				// convert to utf8
				let data = std::str::from_utf8(data).map_err(|err| {
					Error::new(
						ErrorKind::IO_ERROR,
						"",
						rel_path.as_os_str(), 
						"",
						SourceSpan{offset: err.valid_up_to(), line: 0},
						format_args!("File is not valid utf8: {}", err),
					)
				})?;
				self.compile_grug_file_from_str(rel_path, data)
			}).zip(&ok_files_paths).for_each(|(result, path)| {
				// get the path relative to the mods directory
				let rel_path = &path.as_os_str().as_encoded_bytes()[mods_dir_len..];
				let rel_path = <OsStr as AsRef<Path>>::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)});
				let info = FileInfo {
					path: Box::from(rel_path),
					file_name: Box::from(rel_path.file_name().unwrap()),
					mod_name: Box::from(mod_dir_path),
					entity_type: Box::from(get_entity_type(rel_path.as_os_str()).unwrap_or("")),
					entity_name: Box::from(rel_path.file_prefix().unwrap()),
					result
				};
				files.push(info)
			});
		}
		arena.clear();
		self.arenas.borrow_mut().push(arena);
		files
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
	// TODO: This only triggers if entity_type is empty, which should never happen?
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

pub mod type_propagation;
use type_propagation::*;
