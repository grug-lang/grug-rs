use std::collections::HashMap;
use std::path::Path;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, GrugType};
use crate::arena::Arena;

use crate::error::{ErrorKind, Error, SourceSpan};

use allocator_api2::vec::Vec;

use json::JsonValue;

// the 'static fields within `ModApi` are allocated within `_arena`. Any
// reference to them must have a 'self lifetime
pub(crate) struct ModApi {
	entities: HashMap<&'static NTStr, ModApiEntity<'static>>,
	classes: HashMap<&'static NTStr, ModApiClass<'static>>,
	host_fns: HashMap<&'static NTStr, ModApiHostFn<'static>>,
	_arena: Arena,
}

// We don't have a public api that uses the `_arena` field which is the only !Send
// field
unsafe impl Send for ModApi {}
// We don't have a public api that uses the `_arena` field which is the only !Sync
// field
unsafe impl Sync for ModApi {}

impl ModApi {
	pub(crate) fn entities<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiEntity<'a>> {
		&self.entities
	}
	pub(crate) fn classes<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiClass<'a>> {
		&self.classes
	}
	pub(crate) fn host_fns<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiHostFn<'a>> {
		&self.host_fns
	}
}

pub(crate) struct ModApiClass<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option< &'a str>,
	pub(crate) methods: &'a [(&'a NTStr, ModApiHostFn<'a>)],
}

#[derive(Debug)]
pub(crate) struct ModApiEntity<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option<&'a str>,
	pub(crate) export_fns: &'a [(&'a NTStr, ModApiExportFn<'a>)],
}

impl<'a> ModApiEntity<'a> {
	pub(crate) fn get_export_fn(&self, name: &str) -> Option<(usize, &ModApiExportFn<'_>)> {
		self.export_fns.iter().enumerate().find_map(|(i, (fn_name, func))| (name == fn_name.as_str()).then_some((i, func)))
	}
}

#[derive(Debug)]
pub(crate) struct ModApiExportFn<'a> {
	#[allow(dead_code)]
	pub(super) description: Option<&'a str>,
	pub(super) parameters: &'a [Parameter<'a>],
}

#[derive(Debug)]
pub(crate) struct ModApiHostFn<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option<&'a str>,
	pub(crate) return_ty: GrugType<'a>,
	pub(crate) parameters: &'a [Parameter<'a>],
}

pub(crate) fn get_mod_api(mod_api_path: impl AsRef<Path>) -> Result<ModApi, Error> {
	let mod_api_path = mod_api_path.as_ref();
	let mod_api_text = std::fs::read_to_string(mod_api_path).map_err(|err| 
		Error::new(
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

pub(crate) fn get_mod_api_from_text(mod_api_path: impl AsRef<Path>, mod_api_text: &str) -> Result<ModApi, Error> {
	let arena = Arena::new();

	macro_rules! mod_api_err{
		(root => $fmt: literal $(, $args: expr)*) => {
			Error::new(
				ErrorKind::MOD_API_JSON_ERROR,
				"",
				mod_api_path.as_ref().as_ref(), 
				mod_api_text,
				SourceSpan{offset: 0, line: 0},
				format_args!($fmt $(, $args)*),
			)
		};
		($source: expr => $fmt: literal) => {
			Error::new(
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
				Some(arena.copy_str_into(str))
			}
		};

		// optional "export_functions" object
		let export_fns = match &entity_values.get("export_functions") {
			None => &vec![],
			Some(export_fns) => {
				let JsonValue::Array(export_fns) = export_fns else {
					return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions is not an array"));
				};
				export_fns
			}
		};
		let export_fns = export_fns.iter().enumerate().map(|(i, function)| {
			let JsonValue::Object(function) = function else {
				return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}] is not an object"));
			};
			// "name" string
			let fn_name = match function.get("name") {
				None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].name missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].name is not a string"));
					};
					arena.copy_str_into_nt(str)
				}
			};
			// optional "description" string
			let description = match function.get("description") {
				None => None,
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].description is not a string"));
					};
					Some(arena.copy_str_into(str))
				}
			};
			
			// optional "arguments" object
			let parameters = match &function.get("arguments") {
				None => &vec![],
				Some(parameters) => {
					let JsonValue::Array(parameters) = parameters else {
						return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].arguments is not an array"));
					};
					parameters
				}
			};
			let parameters = parameters.iter().enumerate().map(|(j, param_values)| {
				let JsonValue::Object(param_values) = param_values else {
					return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].arguments[{j}] is not an object"))
				};
				// "name" string
				let param_name = match param_values.get("name") {
					None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].arguments[{j}].name missing")), Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].arguments[{j}].name is not a string"));
						};
						arena.copy_str_into_nt(str)
					}
				};
				// "type" string
				let ty = match param_values.get("type") {
					None => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].arguments[{j}].type missing")),
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[{i}].arguments[{j}].type is not a string"));
						};
						str
					}
				};
				let ty = match ty {
					// arguments can't be void
					"void"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is void")),
					"bool"     => GrugType::Bool,
					"number"   => GrugType::Number,
					"string"   => GrugType::String,
					"id"       => GrugType::Id{custom_name: None},
					"resource"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is resource")),
					"entity"     => return Err(mod_api_err!(entity_name => "root.entities.{entity_name}.export_functions[\"{fn_name}\"].arguments[\"{param_name}\"].type is entity")),
					type_name => {
						let extra_value = arena.copy_str_into_nt(type_name).as_ntstrptr();
						GrugType::Id {
							custom_name: Some(extra_value),
						}
					}
				};
				Ok(Parameter{
					name: param_name.as_ntstrptr(),
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
			Ok((fn_name, ModApiExportFn{
				description,
				parameters,
			}))
		}).collect::<Result<Vec<_>, _>>()?;
		let export_fns = {
			let mut temp = Vec::new_in(&arena);
			temp.extend(export_fns);
			temp.leak()
		};
		let entity_name = arena.copy_str_into_nt(entity_name);
		Ok((entity_name, ModApiEntity{
			description,
			export_fns
		}))
	}).collect::<Result<HashMap<_, _>, _>>()?;
	
	// "classes" object
	let classes = match mod_api_root.get("classes") {
		None => return Err(mod_api_err!(root => "root.classes does not exist")),
		Some(classes) => {
			let JsonValue::Object(classes) = classes else {
				return Err(mod_api_err!(root => "root.classes is not an object"));
			};
			classes
		}
	};
	let classes = classes.iter().map(|(class_name, class_values)| {
		let JsonValue::Object(class_values) = class_values else {
			return Err(mod_api_err!(class_name => "root.classes.{class_name} is not an object"));
		};
		// optional "description" string
		let description = match class_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(mod_api_err!(class_name => "root.classes.{class_name}.description is not a string"));
				};
				Some(arena.copy_str_into(str))
			}
		};
		// optional "methods" array
		let methods = match &class_values.get("methods") {
			None => &vec![],
			Some(parameters) => {
				let JsonValue::Array(parameters) = parameters else {
					return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods is not an array"));
				};
				parameters
			}
		};
		let methods = methods.iter().enumerate().map(|(i, method_values)| {
			let JsonValue::Object(method_values) = method_values else {
				return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[{i}] is not an object"));
			};
			// "name" string
			let method_name = match method_values.get("name") {
				None => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[{i}].name is missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[{i}].name is not a string"));
					};
					arena.copy_str_into_nt(str)
				}
			};

			// optional "description" string
			let description = match method_values.get("description") {
				None => None,
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].description is not a string"));
					};
					Some(arena.copy_str_into(str))
				}
			};
			
			// optional "arguments" object
			let parameters = match &method_values.get("arguments") {
				None => &vec![],
				Some(parameters) => {
					let JsonValue::Array(parameters) = parameters else {
						return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments is not an array"));
					};
					parameters
				}
			};

			let parameters = parameters.iter().enumerate().map(|(i, param_values)| {
				let JsonValue::Object(param_values) = param_values else {
					return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[{i}] is not an object"));
				};
				// "name" string
				let param_name = match param_values.get("name") {
					None => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[{i}].name is missing")),
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[{i}].name is not a string"));
						};
						arena.copy_str_into_nt(str)
					}
				};
				// "type" string
				let ty = match param_values.get("type") {
					None => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].type is missing")),
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].type is not a string"));
						};
						str
					}
				};
				let ty = match ty {
					// arguments can't be void
					"void"     => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].type is void")),
					"bool"     => GrugType::Bool,
					"number"      => GrugType::Number,
					"string"   => GrugType::String,
					"id"       => GrugType::Id{custom_name: None},
					"entity"   => {
						// "entity_type" string
						match param_values.get("entity_type") {
							None => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].entity_type is missing")),
							Some(str) => {
								let Some(str) = str.as_str() else {
									return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].entity_type is not a string"));
								};
								GrugType::Entity {
									entity_type: (!str.is_empty()).then(|| {
										arena.copy_str_into_nt(str).as_ntstrptr()
									})
								}
							}
						}
					},
					"resource" => {
						// "resource_extension" string
						match param_values.get("resource_extension") {
							None => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].resource_extension is missing")),
							Some(str) => {
								let Some(str) = str.as_str() else {
									return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].arguments[\"{param_name}\"].resource_extension is not a string"));
								};
								GrugType::Resource {
									extension: arena.copy_str_into_nt(str).as_ntstrptr()
								}
							}
						}
					}
					// TODO: This should be checked against existing IDs
					type_name => {
						let extra_value = arena.copy_str_into_nt(type_name).as_ntstrptr();
						GrugType::Id {
							custom_name: Some(extra_value),
						}
					}
				};
				Ok(Parameter{
					name: param_name.as_ntstrptr(),
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
			let return_ty = match method_values.get("return_type") {
				None => "void",
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].return_type is not a string"));
					};
					arena.copy_str_into_nt(str)
				}
			};
			let return_ty = match return_ty {
				"void"     => GrugType::Void,
				"bool"     => GrugType::Bool,
				"number"      => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity"     => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].return_type is an entity")),
				"resource"     => return Err(mod_api_err!(class_name => "root.classes.{class_name}.methods[\"{method_name}\"].return_type is an resource")),
				type_name => {
					let extra_value = arena.copy_str_into_nt(type_name).as_ntstrptr();
					GrugType::Id {
						custom_name: Some(extra_value),
					}
				}
			};
			// SAFETY: we don't give out a 'static refernce to this string
			Ok((method_name, ModApiHostFn{
				return_ty,
				description,
				parameters
			}))
		}).collect::<Result<Vec<_>, _>>()?;
		let class_name = arena.copy_str_into_nt(class_name);
		Ok((class_name, ModApiClass {
			description,
			methods: methods.leak(),
		}))
	}).collect::<Result<HashMap<_, _>, _>>()?;
	
	// "host_functions" object
	let host_fns = match mod_api_root.get("host_functions") {
		None => return Err(mod_api_err!(root => "root.host_functions does not exist")),
		Some(host_fns) => {
			let JsonValue::Object(host_fns) = host_fns else {
				return Err(mod_api_err!(root => "root.host_functions is not an object"));
			};
			host_fns
		}
	};

	let host_fns = host_fns.iter().map(|(fn_name, game_fn_values)| {
		let JsonValue::Object(game_fn_values) = game_fn_values else {
			return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name} is not an object"));
		};
		// optional "description" string
		let description = match game_fn_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(mod_api_err!(fn_name => "root.game_fn_values.{fn_name}.description is not a string"));
				};
				Some(arena.copy_str_into(str))
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
				return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[{i}] is not an object"));
			};
			// "name" string
			let param_name = match param_values.get("name") {
				None => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[{i}].name is missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments.name is not a string"));
					};
					arena.copy_str_into_nt(str)
				}
			};
			// "type" string
			let ty = match param_values.get("type") {
				None => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].type is missing")),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].type is not a string"));
					};
					str
				}
			};
			let ty = match ty {
				// arguments can't be void
				"void"     => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].type is void")),
				"bool"     => GrugType::Bool,
				"number"      => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity"   => {
					// "entity_type" string
					match param_values.get("entity_type") {
						None => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].entity_type is missing")),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].entity_type is not a string"));
							};
							GrugType::Entity {
								entity_type: (!str.is_empty()).then(|| {
									arena.copy_str_into_nt(str).as_ntstrptr()
								})
							}
						}
					}
				},
				"resource" => {
					// "resource_extension" string
					match param_values.get("resource_extension") {
						None => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].resource_extension is missing")),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.arguments[\"{param_name}\"].resource_extension is not a string"));
							};
							GrugType::Resource {
								extension: arena.copy_str_into_nt(str).as_ntstrptr(),
							}
						}
					}
				}
				type_name => {
					GrugType::Id {
						custom_name: Some(arena.copy_str_into_nt(type_name).as_ntstrptr()),
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
					return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.return_type is not a string"));
				};
				str
			}
		};
		let return_ty = match return_ty {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"      => GrugType::Number,
			"string"   => GrugType::String,
			"id"       => GrugType::Id{custom_name: None},
			"entity"     => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.return_type is entity")),
			"resource"     => return Err(mod_api_err!(fn_name => "root.host_functions.{fn_name}.return_type is resource")),
			type_name => {
				GrugType::Id {
					custom_name: Some(arena.copy_str_into_nt(type_name).as_ntstrptr()),
				}
			}
		};
		// SAFETY: we don't give out a 'static refernce to this string
		let fn_name = arena.copy_str_into_nt(fn_name);
		Ok((fn_name, ModApiHostFn{
			return_ty,
			description,
			parameters
		}))
	}).collect::<Result<HashMap<_, _>, _>>()?;

	Ok(ModApi{
		entities: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiEntity<'_>>, HashMap<&'static NTStr, ModApiEntity<'static>>>(entities)},
		classes : unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiClass <'_>>, HashMap<&'static NTStr, ModApiClass <'static>>>(classes )},
		host_fns: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiHostFn<'_>>, HashMap<&'static NTStr, ModApiHostFn<'static>>>(host_fns)},
		_arena: arena,
	})
}
