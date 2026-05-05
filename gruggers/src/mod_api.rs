use std::collections::HashMap;
use std::path::Path;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, GrugType};
use crate::arena::Arena;

use gruggers_core::error::{ErrorKind, GrugError, SourceSpan};

use allocator_api2::boxed::Box;
use allocator_api2::vec::Vec;

use json::JsonValue;

// the 'static fields within `ModApi` are allocated within `_arena`. Any
// reference to them must have a 'self lifetime
pub(crate) struct ModApi {
	entities: HashMap<&'static NTStr, ModApiEntity<'static>>,
	game_functions: HashMap<&'static NTStr, ModApiGameFn<'static>>,
	_arena: Arena,
}

impl ModApi {
	pub fn entities<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiEntity<'a>> {
		&self.entities
	}
	pub fn game_functions<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiGameFn<'a>> {
		&self.game_functions
	}
}

#[derive(Debug)]
pub(crate) struct ModApiEntity<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option<&'a str>,
	pub(crate) on_fns: &'a [(&'a NTStr, ModApiOnFn<'a>)],
}

impl<'a> ModApiEntity<'a> {
	pub(crate) fn get_on_fn(&self, name: &str) -> Option<(usize, &ModApiOnFn<'_>)> {
		self.on_fns.iter().enumerate().find_map(|(i, (fn_name, func))| (name == fn_name.as_str()).then_some((i, func)))
	}
}

#[derive(Debug)]
pub(crate) struct ModApiOnFn<'a> {
	#[allow(dead_code)]
	pub(super) description: Option<&'a str>,
	pub(super) parameters: &'a [Parameter<'a>],
}

#[derive(Debug)]
pub(crate) struct ModApiGameFn<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option<&'a str>,
	pub(crate) return_ty: GrugType<'a>,
	pub(crate) parameters: &'a [Parameter<'a>],
}

pub(crate) fn get_mod_api(mod_api_path: impl AsRef<Path>) -> Result<ModApi, GrugError<Arena>> {
	let mod_api_path = mod_api_path.as_ref();
	let mod_api_text = std::fs::read_to_string(mod_api_path).map_err(|err| 
		GrugError::new_error(
			ErrorKind::MOD_API_IO_ERROR,
			"",
			mod_api_path.as_ref(), 
			"",
			SourceSpan{offset: 0, line: 0},
			format_args!("IO Error: {}", err),
		)
	)?;
	get_mod_api_from_text(mod_api_path, &mod_api_text)
}

pub(crate) fn get_mod_api_from_text(mod_api_path: impl AsRef<Path>, mod_api_text: &str) -> Result<ModApi, GrugError<Arena>> {
	let arena = Arena::new();

	macro_rules! mod_api_err{
		(root => $fmt: literal $(, $args: expr)*) => {
			GrugError::new_error(
				ErrorKind::MOD_API_JSON_ERROR,
				"",
				mod_api_path.as_ref().as_ref(), 
				mod_api_text,
				SourceSpan{offset: 0, line: 0},
				format_args!($fmt $(, $args)*),
			)
		};
		($source: expr => $fmt: literal) => {
			GrugError::new_error(
				ErrorKind::MOD_API_JSON_ERROR,
				$source,
				mod_api_path.as_ref().as_ref(), 
				mod_api_text,
				SourceSpan{offset: 0, line: 0},
				format_args!($fmt),
			)
		}
	}

	let mod_api_json = json::parse(mod_api_text).map_err(|err| 
		mod_api_err!(root => "{}", err)
	)?;
	let JsonValue::Object(mod_api_root) = mod_api_json else {
		return Err(mod_api_err!(root => "root is not object"));
	};
	// "entities" object
	let entities = match mod_api_root.get("entities") {
		None => return Err(mod_api_err!(root => "root.entities does not exist")),
		Some(entities) => {
			let JsonValue::Object(entities) = entities else {
				return Err(mod_api_err!(root => "root.entities is not an object"));
			};
			entities
		}
	};

	let entities = entities.iter().map(|(entity_name, entity_values)| {
		let JsonValue::Object(entity_values) = entity_values else {
			return Err(mod_api_err!(entity_name => "root.entities.{entity_name} is not an object"));
		};
		// optional "description" string
		let description = match entity_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.description is not a string"));
				};
				Some(Box::leak(NTStr::box_from_str_in(str, &arena)).as_str())
			}
		};

		// optional "on_fns" object
		let on_fns = match &entity_values.get("on_functions") {
			None => &vec![],
			Some(on_fns) => {
				let JsonValue::Array(on_fns) = on_fns else {
					return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions is not an array"));
				};
				on_fns
			}
		};
		let on_fns = on_fns.iter().enumerate().map(|(i, function)| {
			let JsonValue::Object(function) = function else {
				return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}] is not an object"));
			};
			// "name" string
			let fn_name = match function.get("name") {
				None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].name missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].name is not a string"));
					};
					&*Box::leak(NTStr::box_from_str_in(str, &arena))
				}
			};
			// optional "description" string
			let description = match function.get("description") {
				None => None,
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].description is not a string"));
					};
					Some(Box::leak(NTStr::box_from_str_in(str, &arena)).as_str())
				}
			};
			
			// optional "arguments" object
			let parameters = match &function.get("arguments") {
				None => &vec![],
				Some(parameters) => {
					let JsonValue::Array(parameters) = parameters else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].arguments is not an array"));
					};
					parameters
				}
			};
			let parameters = parameters.iter().enumerate().map(|(j, param_values)| {
				let JsonValue::Object(param_values) = param_values else {
					return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].arguments[{j}] is not an object"))
				};
				// "name" string
				let param_name = match param_values.get("name") {
					None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].arguments[{j}].name missing")),
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].arguments[{j}].name is not a string"));
						};
						&*Box::leak(NTStr::box_from_str_in(str, &arena))
					}
				};
				// "type" string
				let ty = match param_values.get("type") {
					None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].arguments[{j}].type missing")),
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[{i}].arguments[{j}].type is not a string"));
						};
						Box::leak(NTStr::box_from_str_in(str, &arena)).as_str()
					}
				};
				let ty = match ty {
					// arguments can't be void
					"void"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is void")),
					"bool"     => GrugType::Bool,
					"number"   => GrugType::Number,
					"string"   => GrugType::String,
					"id"       => GrugType::Id{custom_name: None},
					"resource"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is resource")),
					"entity"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.on_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is entity")),
					type_name => {
						let extra_value = Box::leak(NTStr::box_from_str_in(type_name, &arena));
						GrugType::Id {
							custom_name: Some(extra_value.as_ntstrptr()),
						}
					}
				};
				Ok(Parameter{
					name: unsafe{param_name.as_ntstrptr().detach_lifetime()},
					ty,
					// This span should never be used
					// TODO: Maybe this should be the span within mod_api?
					name_span: SourceSpan{offset: 0, line: 0},
					type_span: SourceSpan{offset: 0, line: 0},
				})
			}).collect::<Result<Vec<_>, _>>()?;
			// SAFETY: we don't give out a 'static refernce to this string
			let parameters = {
				let mut temp = Vec::new_in(&arena);
				temp.extend(parameters);
				temp.leak()
			};
			Ok((fn_name, ModApiOnFn{
				description,
				parameters,
			}))
		}).collect::<Result<Vec<_>, _>>()?;
		let on_fns = {
			let mut temp = Vec::new_in(&arena);
			temp.extend(on_fns);
			temp.leak()
		};
		let entity_name = unsafe{
			std::mem::transmute::<&NTStr, &'static NTStr>(
				Box::leak(NTStr::box_from_str_in(entity_name, &arena))
			)
		};
		Ok((entity_name, ModApiEntity{
			description,
			on_fns
		}))
	}).collect::<Result<HashMap<_, _>, _>>()?;
	
	
	// "game_functions" object
	let game_functions = match mod_api_root.get("game_functions") {
		None => return Err(mod_api_err!(root => "root.game_functions does not exist")),
		Some(game_functions) => {
			let JsonValue::Object(game_functions) = game_functions else {
				return Err(mod_api_err!(root => "root.game_functions is not an object"));
			};
			game_functions
		}
	};

	let game_functions = game_functions.iter().map(|(fn_name, game_fn_values)| {
		let JsonValue::Object(game_fn_values) = game_fn_values else {
			return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name} is not an object"));
		};
		// optional "description" string
		let description = match game_fn_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(mod_api_err!(fn_name => "root.game_fn_values.{fn_name}.description is not a string"));
				};
				Some(Box::leak(NTStr::box_from_str_in(str, &arena)).as_str())
			}
		};
		
		// optional "arguments" object
		let parameters = match &game_fn_values.get("arguments") {
			None => &vec![],
			Some(parameters) => {
				let JsonValue::Array(parameters) = parameters else {
					return Err(mod_api_err!(fn_name => "root.game_fn_values.{fn_name}.arguments is not an array"));
				};
				parameters
			}
		};

		let parameters = parameters.iter().enumerate().map(|(i, param_values)| {
			let JsonValue::Object(param_values) = param_values else {
				return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[{i}] is not an object"));
			};
			// "name" string
			let param_name = match param_values.get("name") {
				None => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[{i}].name is missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments.name is not a string"));
					};
					&*Box::leak(NTStr::box_from_str_in(str, &arena))
				}
			};
			// "type" string
			let ty = match param_values.get("type") {
				None => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].type is missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].type is not a string"));
					};
					Box::leak(NTStr::box_from_str_in(str, &arena)).as_str()
				}
			};
			let ty = match ty {
				// arguments can't be void
				"void"     => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].type is void")),
				"bool"     => GrugType::Bool,
				"number"      => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity"   => {
					// "entity_type" string
					let entity_type = match param_values.get("entity_type") {
						None => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].entity_type is missing")),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].entity_type is not a string"));
							};
							&*Box::leak(NTStr::box_from_str_in(str, &arena))
						}
					};
					GrugType::Entity {
						entity_type: (!entity_type.is_empty()).then(|| {
							let entity_type = Box::leak(NTStr::box_from_str_in(entity_type, &arena));
							entity_type.as_ntstrptr()
						})
					}
				},
				"resource" => {
					// "resource_extension" string
					let extension = match param_values.get("resource_extension") {
						None => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].resource_extension is missing")),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.arguments[\"{param_name}\"].resource_extension is not a string"));
							};
							&*Box::leak(NTStr::box_from_str_in(str, &arena))
						}
					};
					let extension = Box::leak(NTStr::box_from_str_in(extension, &arena)).as_ntstrptr();
					GrugType::Resource {
						extension
					}
				}
				type_name => {
					let extra_value = Box::leak(NTStr::box_from_str_in(type_name, &arena)).as_ntstrptr();
					GrugType::Id {
						custom_name: Some(extra_value),
					}
				}
			};
			Ok(Parameter{
				name: unsafe{param_name.as_ntstrptr().detach_lifetime()},
				ty,
				name_span: SourceSpan{offset: 0, line: 0},
				type_span: SourceSpan{offset: 0, line: 0},
			})
		}).collect::<Result<Vec<_>, _>>()?;
		let parameters = {
			let mut temp = Vec::new_in(&arena);
			temp.extend(parameters);
			temp.leak()
		};

		// optional "return_type" string
		let return_ty = match game_fn_values.get("return_type") {
			None => "void",
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.return_type is not a string"));
				};
				&*Box::leak(NTStr::box_from_str_in(str, &arena))
			}
		};
		let return_ty = match return_ty {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"      => GrugType::Number,
			"string"   => GrugType::String,
			"id"       => GrugType::Id{custom_name: None},
			"entity"     => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.return_type is entity")),
			"resource"     => return Err(mod_api_err!(fn_name => "root.game_functions.{fn_name}.return_type is resource")),
			type_name => {
				let extra_value = Box::leak(NTStr::box_from_str_in(type_name, &arena)).as_ntstrptr();
				GrugType::Id {
					custom_name: Some(extra_value),
				}
			}
		};
		// SAFETY: we don't give out a 'static refernce to this string
		let fn_name = unsafe{
			std::mem::transmute::<&NTStr, &'static NTStr>(
				Box::leak(NTStr::box_from_str_in(fn_name, &arena))
			)
		};
		Ok((fn_name, ModApiGameFn{
			return_ty,
			description,
			parameters
		}))
	}).collect::<Result<HashMap<_, _>, _>>()?;

	Ok(ModApi{
		entities: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiEntity<'_>>, HashMap<&'static NTStr, ModApiEntity<'static>>>(entities)},
		game_functions: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiGameFn<'_>>, HashMap<&'static NTStr, ModApiGameFn<'static>>>(game_functions)},
		_arena: arena,
	})
}
