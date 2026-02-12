// This is to ensure that any results that come 
// from parsing are not ignored
#![deny(unused_must_use)]
#![deny(unused_mut)]
// #![deny(warnings)]
use crate::error::GrugError;
use crate::state::GrugState;
use crate::backend::GrugAst;
use crate::types::{GlobalStatement, GlobalVariable, OnFunction, HelperFunction, GrugScriptId};

use std::sync::Arc;
const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod tokenizer;

pub mod parser;

impl GrugState {
	pub fn compile_grug_file(&self, path: &str) -> Result<GrugScriptId, GrugError> {
		let mut path_buf = self.mods_dir_path.clone();
		path_buf.push(path);
		let file_text = std::fs::read_to_string(path_buf).unwrap();

		self.compile_grug_file_from_str(path, &file_text)
	}

	pub fn compile_grug_file_from_str(&self, path: &str, file_text: &str) -> Result<GrugScriptId, GrugError> {
		let mod_name = get_mod_name(path)?;
		let entity_type = get_entity_type(path)?;

		let tokens = tokenizer::tokenize(file_text)?;

		let mut ast = parser::parse(&tokens)?;

		let entity = self.mod_api.entities().get(&*entity_type).ok_or_else(|| TypePropogatorError::EntityDoesNotExist{
			entity_name: Arc::from(entity_type),
		})?;
		let game_functions = self.mod_api.game_functions();
		
		TypePropogator::new(entity, game_functions, mod_name.into()).fill_result_types(entity_type, &mut ast)?;

		// let mod_api_entity = self.mod_api.entities.get(entity_type);
		let mut global_variables = Vec::new();
		let mut on_functions = (0..entity.on_fns.len()).map(|_| None).collect::<Vec<_>>();
		let mut helper_functions = Vec::new();

		ast.global_statements.into_iter().for_each(|statement| {
			match statement {
				GlobalStatement::Variable(st@GlobalVariable      {..}) => global_variables.push(st),
				GlobalStatement::OnFunction(st@OnFunction        {..}) => {
					let (i, _) = entity.get_on_fn(&st.name).unwrap();
					on_functions[i] = Some(st);
				}
				GlobalStatement::HelperFunction(st@HelperFunction{..}) => helper_functions.push(st),
				_ => (),
			}
		});

		let file = GrugAst{
			global_variables,
			on_functions,
			helper_functions,
		};
		let mut path_to_script_ids = self.path_to_script_ids.borrow_mut();
		let id = match path_to_script_ids.get(path) {
			Some(id) => *id,
			None => {
				let id = self.get_next_script_id();
				assert!(path_to_script_ids.insert(String::from(path), id).is_none());
				id
			}
		};
		self.backend.insert_file(self, id, file);
		Ok(id)
	}
}

fn get_mod_name (path: &str) -> Result<&str, GrugError> {
	path.split_once('/').map(|x| x.0).ok_or(GrugError::FileError(FileError::FilePathDoesNotContainForwardSlash{path: String::from(path)}))
}

fn get_entity_type(path: &str) -> Result<&str, FileError> {
	let (_, entity_type) = path.rsplit_once("-").ok_or(
			FileError::EntityMissing{path: String::from(path)}
		)?;
	let (entity_type, _) = entity_type.rsplit_once(".").ok_or(
			FileError::MissingPeriodInFileName{path: String::from(path)}
		)?;
	if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
		return Err(FileError::EntityLenExceedsMaxLen{path: String::from(path), entity_len: entity_type.len()});
	}
	if entity_type.is_empty() {
		return Err(FileError::EntityMissing{path: String::from(path)});
	}
	check_custom_id_is_pascal(entity_type)
}

fn check_custom_id_is_pascal(entity_type: &str) -> Result<&str, FileError> {
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
		path: String
	},
	EntityLenExceedsMaxLen {
		path: String,
		entity_len: usize,
	},
	EntityMissing {
		path: String
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
			} => write!(f, "'{}' is missing a period in its filename", path),
			Self::EntityLenExceedsMaxLen {
				path,
				entity_len: _,
			} => write!(f, 
				"There are more than {MAX_FILE_ENTITY_TYPE_LENGTH} characters \n\
				in the entity type of '{path}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH"
			),
			Self::EntityMissing {
				path
			} => write!(f, 
				"'{}' is missing an entity type in its name;\n\
				use a dash to specify it, like 'ak47-gun.grug'",
				path
			),
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

