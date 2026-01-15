// This is to ensure that any results that come 
// from parsing are not ignored
#![deny(unused_must_use)]
#![deny(unused_mut)]
// #![deny(warnings)]
use crate::error::GrugError;
use crate::state::GrugState;
use crate::backend::GrugFile;
use crate::types::{GlobalStatement, GlobalVariable, OnFunction, HelperFunction};
const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod tokenizer;

pub mod parser;

pub fn compile_grug_file<'a>(state: &GrugState, path: &'a str) -> Result<GrugFile, GrugError<'a>> {
	let mod_name = get_mod_name(path)?;
	let entity_type = get_entity_type(path)?;

	let mut path_buf = state.mods_dir_path.clone();
	path_buf.push(path);
	let file_text = std::fs::read_to_string(path_buf).unwrap();

	let tokens = tokenizer::tokenize(&file_text)?;

	let mut ast = parser::parse(&*tokens)?;
	
	TypePropogator::new(state, mod_name.into()).fill_result_types(entity_type, &mut ast)?;

	let mut global_variables = Vec::new();
	let mut on_functions = Vec::new();
	let mut helper_functions = Vec::new();

	ast.global_statements.into_iter().for_each(|statement| {
		match statement {
			GlobalStatement::Variable(st@GlobalVariable      {..}) => global_variables.push(st),
			GlobalStatement::OnFunction(st@OnFunction        {..}) => on_functions.push(st),
			GlobalStatement::HelperFunction(st@HelperFunction{..}) => helper_functions.push(st),
			_ => (),
		}
	});

	return Ok(GrugFile{
		global_variables,
		on_functions,
		helper_functions,
	});
}

fn get_mod_name<'a> (path: &'a str) -> Result<&'a str, GrugError<'a>> {
	path.split_once('/').map(|x| x.0).ok_or(GrugError::FileNameError(FileNameError::FilePathDoesNotContainForwardSlash{path}))
}

fn get_entity_type(path: &str) -> Result<&str, FileNameError<'_>> {
	let (_, entity_type) = path.rsplit_once("-").ok_or(
			FileNameError::EntityMissing{path}
		)?;
	let (entity_type, _) = entity_type.rsplit_once(".").ok_or(
			FileNameError::MissingPeriodInFileName{path}
		)?;
	if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
		return Err(FileNameError::EntityLenExceedsMaxLen{path, entity_len: entity_type.len()});
	}
	if entity_type.len() == 0 {
		return Err(FileNameError::EntityMissing{path});
	}
	check_custom_id_is_pascal(entity_type)
}

fn check_custom_id_is_pascal(entity_type: &str) -> Result<&str, FileNameError<'_>> {
	let mut chars = entity_type.chars();
	let Some(_) = chars.next() else {
		return Err(FileNameError::EntityNotPascalCase1{entity_type});
	};
	for ch in chars {
		if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_digit(10)) {
			return Err(FileNameError::EntityNotPascalCase2{entity_type, wrong_char: ch});
		}
	}
	Ok(entity_type)
}

#[derive(Debug)]
pub enum FileNameError<'a> {
	FilePathDoesNotContainForwardSlash{
		path: &'a str
	},
	MissingPeriodInFileName {
		path: &'a str
	},
	EntityLenExceedsMaxLen {
		path: &'a str,
		entity_len: usize,
	},
	EntityMissing {
		path: &'a str
	},
	EntityNotPascalCase1 {
		entity_type: &'a str,
	},
	EntityNotPascalCase2 {
		entity_type: &'a str,
		wrong_char: char,
	}
}

impl<'a> std::fmt::Display for FileNameError<'a> {
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

