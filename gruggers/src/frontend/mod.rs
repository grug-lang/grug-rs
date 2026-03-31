// This is to ensure that any results that come 
// from parsing are not ignored
#![deny(unused_must_use)]
#![deny(unused_mut)]
// #![deny(warnings)]
use crate::error::GrugError;
use crate::state::{GrugState, FileInfo};
// use crate::backend::GrugAst;
use crate::types::GrugFileId;
use crate::ast::*;
use crate::arena::Arena;
use crate::ntstring::NTStrPtr;

use allocator_api2::vec::Vec;
use allocator_api2::boxed::Box as Box2;

use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::sync::Arc;
// use std::path::Path;

const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod tokenizer;

pub mod parser;

impl GrugState {
	// Path is relative to mods directory
	pub fn compile_grug_file(&self, path: impl AsRef<OsStr>) -> Result<GrugFileId, GrugError> {
		let mut path_buf = self.mods_dir_path.clone();
		path_buf.push("\\");
		path_buf.push(path.as_ref());
		let file_text = std::fs::read_to_string(path_buf).unwrap();

		self.compile_grug_file_from_str(path, &file_text)
	}

	// Path is relative to mods directory or an absolute path
	pub fn compile_grug_file_from_str(&self, path: impl AsRef<OsStr>, file_text: &str) -> Result<GrugFileId, GrugError> {
		let path = path.as_ref();

		let mod_name = get_mod_name(path);
		let entity_type = get_entity_type(path)?;

		let mut arena = self.arenas.borrow_mut().pop().unwrap_or_else(|| Arena::new());
		// immediately invoked closure so we get try {} finally {}
		let id = (|| {
			let tokens = tokenizer::tokenize(file_text, &arena)?;

			let mut ast = parser::parse(&tokens, &arena)?;

			let entity = self.mod_api.entities().get(entity_type).ok_or_else(|| TypePropogatorError::EntityDoesNotExist{
				entity_name: Arc::from(entity_type),
			})?;
			let game_functions = self.mod_api.game_functions();
			
			TypePropogator::new(entity, game_functions, &self.game_functions, mod_name.into()).fill_result_types(entity_type, &mut ast, &arena)?;

			// let mod_api_entity = self.mod_api.entities.get(entity_type);
			let mut member_variables = Vec::new_in(&arena);
			let mut on_functions = Vec::new_in(&arena);
			on_functions.extend((0..entity.on_fns.len()).map(|_| None));
			let mut helper_functions = Vec::new_in(&arena);

			ast.global_statements.into_iter().for_each(|statement| {
				match statement {
					GlobalStatement::Variable(st@MemberVariable      {..}) => member_variables.push(st.into()),
					GlobalStatement::OnFunction(st@OnFunction        {..}) => {
						let (i, _) = entity.get_on_fn(st.name.to_str()).unwrap();
						on_functions[i] = Some(&*Box2::leak(Box2::new_in(st.into(), &arena)));
					}
					GlobalStatement::HelperFunction(st@HelperFunction{..}) => helper_functions.push(st.into()),
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
	
	pub fn compile_all_files(&self) -> std::vec::Vec<FileInfo> {
		let mut files = std::vec::Vec::new();
		for mod_dir in std::fs::read_dir(&self.mods_dir_path).expect("Could not read mods directory") {
			let Ok(mod_dir) = mod_dir else {
				panic!("unable to read directory: {:?}", mod_dir);
			};
			let mod_dir_path = mod_dir.path();
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
					// I fucking hate this
					let mut entry_path = next_entry.path();
					let entry_path = entry_path.as_mut_os_str();
					let rel_path = unsafe{std::mem::transmute::<&mut OsStr, &mut [u8]>(entry_path)};
					let rel_path = &mut rel_path[self.mods_dir_path.len()..];
					rel_path.iter_mut().for_each(|byte| if *byte == b'\\' {*byte = b'/';});
					let rel_path = <OsStr as AsRef<Path>>::as_ref(unsafe{OsStr::from_encoded_bytes_unchecked(rel_path)});

					if let Some(extension) = Path::extension(rel_path.as_ref()) && extension == "grug" {
						let result = self.compile_grug_file(rel_path);
						let info = FileInfo {
							path: Box::from(rel_path),
							file_name: Box::from(rel_path.file_name().unwrap()),
							mod_name: Box::from(mod_dir_path.as_os_str()),
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
	return unsafe{OsStr::from_encoded_bytes_unchecked(&path[..slash_len])};
	
	// This restrict isn't checked in grug_tests and it gets in the way of
	// implementing the compiler in the simplest way
	// path.split_once('/').map(|x| x.0).ok_or(GrugError::FileError(FileError::FilePathDoesNotContainForwardSlash{path: String::from(path)}))
}

fn get_entity_type(path: &OsStr) -> Result<&str, FileError> {
	let mut dot_pos = None;
	let mut dash_pos = None;
	let path_bytes = path.as_encoded_bytes();
	for (i, ch) in path_bytes.iter().enumerate().rev() {
		match (ch, dot_pos, dash_pos) {
			(b'.', None, None) => dot_pos = Some(i),
			(b'-', None, None) => Err(FileError::MissingPeriodInFileName{path: OsString::from(path)})?,
			(b'-', Some(_), None) => {dash_pos = Some(i); break;},
			_ => (),
		}
	}
	let (Some(dot_pos), Some(dash_pos)) = (dot_pos, dash_pos) else {
		Err(FileError::EntityMissing{path: OsString::from(path)})?
	};
	// SAFETY: dash_pos is b'-' which is valid utf8, and dot_pos is b'.' which
	// is also utf8 so (dash_pos+1)..dot_pos will not truncate a utf8 codepoint
	let entity_type = unsafe{OsStr::from_encoded_bytes_unchecked(&path_bytes[(dash_pos + 1)..dot_pos])};
	if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
		return Err(FileError::EntityLenExceedsMaxLen{path: OsString::from(path), entity_len: entity_type.len()});
	}
	if entity_type.is_empty() {
		return Err(FileError::EntityMissing{path: OsString::from(path)});
	}
	check_custom_id_is_pascal(entity_type)
}

fn check_custom_id_is_pascal(entity_type: &OsStr) -> Result<&str, FileError> {
	let entity_type = entity_type.to_str().ok_or_else(|| FileError::EntityNotUtf8{entity_type: OsString::from(entity_type)})?;
	let mut chars = entity_type.chars();
	let Some(_) = chars.next() else {
		return Err(FileError::EntityNotPascalCase1{entity_type: String::from(entity_type)});
	};
	for ch in chars {
		if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_ascii_digit()) {
			return Err(FileError::EntityNotPascalCase2{entity_type: String::from(entity_type), wrong_char: ch});
		}
	}
	Ok(entity_type)
}

#[derive(Debug)]
pub enum FileError {
	FilePathDoesNotContainForwardSlash{
		path: String
	},
	MissingPeriodInFileName {
		path: OsString,
	},
	EntityLenExceedsMaxLen {
		path: OsString,
		entity_len: usize,
	},
	EntityMissing {
		path: OsString,
	},
	EntityNotUtf8 {
		entity_type: OsString,
	},
	EntityNotPascalCase1 {
		entity_type: String,
	},
	EntityNotPascalCase2 {
		entity_type: String,
		wrong_char: char,
	}
}

impl std::fmt::Display for FileError {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::FilePathDoesNotContainForwardSlash{
				path
			} => write!(f, "The grug file path {}, does not contain a '/' character", path),
			Self::MissingPeriodInFileName {
				path
			} => write!(f, "'{}' is missing a period in its filename", path.display()),
			Self::EntityLenExceedsMaxLen {
				path,
				entity_len: _,
			} => write!(f, 
				"There are more than {MAX_FILE_ENTITY_TYPE_LENGTH} characters \n\
				in the entity type of '{}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH",
				path.display()
			),
			Self::EntityMissing {
				path
			} => write!(f, 
				"'{}' is missing an entity type in its name;\n\
				use a dash to specify it, like 'ak47-gun.grug'",
				path.display()
			),
			Self::EntityNotUtf8 {
				entity_type
			} => write!(f, "'{}' is not valid utf8", entity_type.display()),
			Self::EntityNotPascalCase1 {
				entity_type,
			} => write!(f, "'{entity_type}' seems like a custom ID type, but isn't in PascalCase"),
			Self::EntityNotPascalCase2 {
				entity_type,
				wrong_char,
			} => write!(f,
				"'{entity_type}' seems like a custom ID type, but it contains '{wrong_char}', \n\
				which isn't uppercase/lowercase/a digit"
			),
		}
	}
}

pub mod type_propagation;
use type_propagation::*;
