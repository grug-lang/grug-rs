use std::collections::HashMap;
use std::path::Path;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, GrugType};
use crate::arena::Arena;
use crate::error::{ErrorKind, Error, SourceSpan, Result};
use crate::types::GameFnPtr;

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
	#[allow(dead_code)]
	pub(crate) fn classes<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiClass<'a>> {
		// SAFETY: Invariance of the methods field requires this transmute
		// This transmute brings it back to the actual lifetime
		unsafe{std::mem::transmute::<&'a HashMap<&'static NTStr, ModApiClass<'static>>, &'a HashMap<&'a NTStr, ModApiClass<'a>>>(&self.classes)}
	}
	pub(crate) fn host_fns<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiHostFn<'a>> {
		&self.host_fns
	}

	pub(crate) fn register_host_fn(&mut self, name: &str, ptr: GameFnPtr) -> Result<()> {
		let Some(host_fn_data) = self.host_fns.get_mut(name) else {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Host function named '{}' is not found in mod_api.json", name),
			));
		};
		match &mut host_fn_data.fn_ptr {
			Some(_) => {
				return Err(Error::new(
					ErrorKind::INIT_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function named '{}' has already been registered", name),
				));
			}
			x => *x = Some(ptr),
		}
		Ok(())
	}

	pub(crate) fn register_method_fn(&mut self, class_name: &str, fn_name: &str, ptr: GameFnPtr) -> Result<()> {
		let Some(class) = self.classes.get_mut(class_name) else {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Class with name '{}' is not found in mod_api.json", class_name),
			));
		};
		let Some((_, host_fn_data)) = class.methods.iter_mut().find(|(name, _)| name.as_str() == fn_name) else {
			return Err(Error::new(
				ErrorKind::INIT_ERROR,
				"",
				"".as_ref(),
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Class with name '{}' does not contain method with name '{}'", class_name, fn_name),
			));
		};
		match &mut host_fn_data.fn_ptr {
			Some(_) => {
				return Err(Error::new(
					ErrorKind::INIT_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host method named '{}' on class '{}' has already been registered", fn_name, class_name),
				));
			}
			x => *x = Some(ptr),
		}
		Ok(())
	}

	pub(crate) unsafe fn register_dummies(&mut self) {
		use crate::state::GrugState;
		use crate::types::GrugValue;
		extern "C" fn dummy_host_fn(_state: &GrugState, _arguments: *const GrugValue) -> GrugValue {
			GrugValue{void: ()}
		}
		for (_, host_fn) in &mut self.host_fns {
			host_fn.fn_ptr = const{Some(GameFnPtr::from_ptr(dummy_host_fn))};
		}
		for (_, class) in &mut self.classes {
			for (_, host_fn) in &mut *class.methods {
				host_fn.fn_ptr = const{Some(GameFnPtr::from_ptr(dummy_host_fn))};
			}
		}
	}
}

#[derive(Debug)]
pub(crate) struct ModApiClass<'a> {
	#[allow(dead_code)]
	pub(crate) description: Option< &'a str>,
	#[allow(dead_code)]
	pub(crate) methods: &'a mut [(&'a NTStr, ModApiHostFn<'a>)],
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
	pub(crate) fn_ptr: Option<GameFnPtr>,
	#[cfg(feature = "generics")]
	pub(crate) generics: &'a [&'a NTStr],
	#[cfg(feature = "generics")]
	pub(crate) registerer: Option<GameFnRegisterer>,
}

struct ModApiContext<'a> {
	path: &'a Path,
	text: &'a str,
}

impl<'a> ModApiContext<'a> {
	fn new(path: &'a Path, text: &'a str) -> Self {
		Self {
			path, 
			text,
		}
	}

	fn new_error(&self, location: &str, fmt: std::fmt::Arguments) -> Error {
		Error::new(
			ErrorKind::MOD_API_JSON_ERROR,
			location,
			self.path.as_ref(), 
			self.text,
			SourceSpan{offset: 0, line: 0},
			format_args!("{}", fmt),
		)
	}
}

pub(crate) fn get_mod_api(mod_api_path: impl AsRef<Path>) -> Result<ModApi> {
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

pub(crate) fn get_mod_api_from_text(mod_api_path: impl AsRef<Path>, mod_api_text: &str) -> Result<ModApi> {
	let context = ModApiContext::new(mod_api_path.as_ref(), mod_api_text);
	let arena = Arena::new();

	let mod_api_json = json::parse(mod_api_text).map_err(|err| 
		context.new_error("", format_args!("{err}"))
	)?;
	let JsonValue::Object(mod_api_root) = mod_api_json else {
		return Err(context.new_error("", format_args!("root is not object")));
	};
	// "entities" object
	let entities = match mod_api_root.get("entities") {
		None => return Err(context.new_error("", format_args!("root.entities does not exist"))),
		Some(entities) => {
			let JsonValue::Object(entities) = entities else {
				return Err(context.new_error("", format_args!("root.entities is not an object")));
			};
			entities
		}
	};

	macro_rules! parse_type{
		($root_object: expr, $location: expr, $prefix: expr, $key: literal $(, $args: expr)*) => {{
			// "type" string
			let ty = match $root_object.get($key) {
				None => "void",
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(context.new_error($location, format_args!(concat!($prefix, ".{} is not a string") $(, $args)*, $key)));
					};
					str
				}
			};
			let ty = match ty {
				"void"     => GrugType::Void,
				"bool"     => GrugType::Bool,
				"number"   => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity"   => {
					// "entity_type" string
					match $root_object.get("entity_type") {
						None => return Err(context.new_error($location, format_args!(concat!($prefix, ".entity_type is missing") $(, $args)*))),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(context.new_error($location, format_args!(concat!($prefix, ".entity_type is not a string") $(, $args)*)));
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
					match $root_object.get("resource_extension") {
						None => return Err(context.new_error($location, format_args!(concat!($prefix, ".resource_extension is missing") $(, $args)*))),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(context.new_error($location, format_args!(concat!($prefix, ".resource_extension is not a string") $(, $args)*)));
							};
							GrugType::Resource {
								extension: arena.copy_str_into_nt(str).as_ntstrptr(),
							}
						}
					}
				}
				type_name => {
					let extra_value = arena.copy_str_into_nt(type_name).as_ntstrptr();
					GrugType::Id {
						custom_name: Some(extra_value),
					}
				}
			};
			ty
		}}
	}

	macro_rules! parse_host_fn{
		($root_object: expr, $prefix: literal, $host_fn_key: literal $(, $args: expr)*) => {{
			let host_fns = match $root_object.get($host_fn_key) {
				None => return Err(context.new_error("", format_args!(concat!($prefix, ".{} does not exist") $(, $args)*, $host_fn_key))),
				Some(host_fns) => {
					let JsonValue::Object(host_fns) = host_fns else {
						return Err(context.new_error("", format_args!(concat!($prefix, ".{} is not an object") $(, $args)*, $host_fn_key)));
					};
					host_fns
				}
			};
			host_fns.iter().map(|(fn_name, game_fn_values)| {
				let JsonValue::Object(game_fn_values) = game_fn_values else {
					return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{} is not an object") $(, $args)*, $host_fn_key, fn_name)));
				};
				// optional "description" string
				let description = match game_fn_values.get("description") {
					None => None,
					Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.description is not a string") $(, $args)*, $host_fn_key, fn_name)));
						};
						Some(arena.copy_str_into(str))
					}
				};
				
				// optional "parameters" object
				let parameters = match &game_fn_values.get("parameters") {
					None => &vec![],
					Some(parameters) => {
						let JsonValue::Array(parameters) = parameters else {
							return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.parameters is not an array") $(, $args)*, $host_fn_key, fn_name)));
						};
						parameters
					}
				};

				let parameters = parameters.iter().enumerate().map(|(i, param_values)| {
					let JsonValue::Object(param_values) = param_values else {
						return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.parameters[{}] is not an object") $(, $args)*, $host_fn_key, fn_name, i)));
					};
					// "name" string
					let param_name = match param_values.get("name") {
						None => return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.parameters[{}].name is missing") $(, $args)*, $host_fn_key, fn_name, i))),
						Some(str) => {
							let Some(str) = str.as_str() else {
								return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.parameters.name is not a string") $(, $args)*, $host_fn_key, fn_name)));
							};
							arena.copy_str_into_nt(str)
						}
					};
					let ty = parse_type!(param_values, fn_name, concat!($prefix, ".{}.{}.parameters[\"{}\"]"), "type" $(, $args)*, $host_fn_key, fn_name, param_name);
					match &ty {
						GrugType::Void => return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.parameters[\"{}\"].type is void") $(, $args)*, $host_fn_key, fn_name, param_name))),
						_ => (),
					}
					Ok(Parameter{
						name: unsafe{param_name.as_ntstrptr().detach_lifetime()},
						ty,
						name_span: SourceSpan{offset: 0, line: 0},
						type_span: SourceSpan{offset: 0, line: 0},
					})
				}).collect::<Result<Vec<_>>>()?;
				let parameters = {
					let mut temp = Vec::new_in(&arena);
					temp.extend(parameters);
					temp.leak()
				};

				let return_ty = parse_type!(game_fn_values, fn_name, concat!($prefix, ".{}.{}"), "return_type" $(, $args)*, $host_fn_key, fn_name);
				match &return_ty {
					GrugType::Entity{..}   => return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.return_type is entity") $(, $args)*, $host_fn_key, fn_name))),
					GrugType::Resource{..} => return Err(context.new_error(fn_name, format_args!(concat!($prefix, ".{}.{}.return_type is resource") $(, $args)*, $host_fn_key, fn_name))),
					_ => (),
				}
				// SAFETY: we don't give out a 'static refernce to this string
				let fn_name = arena.copy_str_into_nt(fn_name);
				Ok((fn_name, ModApiHostFn{
					return_ty,
					description,
					parameters,
					fn_ptr: None,
				}))
			}).collect::<Result<HashMap<_, _>>>()
		}}
	}

	let entities = entities.iter().map(|(entity_name, entity_values)| {
		let JsonValue::Object(entity_values) = entity_values else {
			return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name} is not an object")));
		};
		// optional "description" string
		let description = match entity_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.description is not a string")));
				};
				Some(arena.copy_str_into(str))
			}
		};

		// optional "export_functions" object
		let export_fns = match &entity_values.get("export_functions") {
			None => &vec![],
			Some(export_fns) => {
				let JsonValue::Array(export_fns) = export_fns else {
					return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions is not an array")));
				};
				export_fns
			}
		};
		let export_fns = export_fns.iter().enumerate().map(|(i, function)| {
			let JsonValue::Object(function) = function else {
				return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[{i}] is not an object")));
			};
			// "name" string
			let fn_name = match function.get("name") {
				None => return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[{i}].name missing"))),
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[{i}].name is not a string")));
					};
					arena.copy_str_into_nt(str)
				}
			};
			// optional "description" string
			let description = match function.get("description") {
				None => None,
				Some(str) => {
					let Some(str) = str.as_str() else {
						return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].description is not a string")));
					};
					Some(arena.copy_str_into(str))
				}
			};
			
			// optional "parameters" object
			let parameters = match &function.get("parameters") {
				None => &vec![],
				Some(parameters) => {
					let JsonValue::Array(parameters) = parameters else {
						return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].parameters is not an array")));
					};
					parameters
				}
			};
			let parameters = parameters.iter().enumerate().map(|(j, param_values)| {
				let JsonValue::Object(param_values) = param_values else {
					return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].parameters[{j}] is not an object")))
				};
				// "name" string
				let param_name = match param_values.get("name") {
					None => return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[{i}].parameters[{j}].name missing"))), Some(str) => {
						let Some(str) = str.as_str() else {
							return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[{i}].parameters[{j}].name is not a string")));
						};
						arena.copy_str_into_nt(str)
					}
				};
				let ty = parse_type!(param_values, entity_name, "root.entities.{}.export_functions[\"{}\"].parameters[\"{}\"]", "type", entity_name, fn_name, param_name);
				match &ty {
					GrugType::Void => return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].parameters[\"{param_name}\"].type is void"))),
					GrugType::Resource{..} => return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].parameters[\"{param_name}\"].type is resource"))),
					GrugType::Entity{..} => return Err(context.new_error(entity_name, format_args!("root.entities.{entity_name}.export_functions[\"{fn_name}\"].parameters[\"{param_name}\"].type is entity"))),
					_ => (),
				}
				Ok(Parameter{
					name: param_name.as_ntstrptr(),
					ty,
					// This span should never be used
					// TODO: Maybe this should be the span within mod_api?
					name_span: SourceSpan{offset: 0, line: 0},
					type_span: SourceSpan{offset: 0, line: 0},
				})
			}).collect::<Result<Vec<_>>>()?;
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
		}).collect::<Result<Vec<_>>>()?;
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
	}).collect::<Result<HashMap<_, _>>>()?;
	
	// "classes" object
	let classes = match mod_api_root.get("classes") {
		None => return Err(context.new_error("", format_args!("root.classes does not exist"))),
		Some(classes) => {
			let JsonValue::Object(classes) = classes else {
				return Err(context.new_error("", format_args!("root.classes is not an object")));
			};
			classes
		}
	};
	let classes = classes.iter().map(|(class_name, class_values)| {
		let class_name = arena.copy_str_into_nt(class_name);
		let JsonValue::Object(class_values) = class_values else {
			return Err(context.new_error(class_name, format_args!("root.classes.{class_name} is not an object")));
		};
		// optional "description" string
		let description = match class_values.get("description") {
			None => None,
			Some(str) => {
				let Some(str) = str.as_str() else {
					return Err(context.new_error(class_name, format_args!("root.classes.{class_name}.description is not a string")));
				};
				Some(arena.copy_str_into(str))
			}
		};
		// optional "methods" object
		let methods = parse_host_fn!(&class_values, "root.classes.{}", "methods", class_name)?;
		let methods = {
			let mut temp = Vec::new_in(&arena);
			temp.extend(methods);
			temp.leak()
		};
		Ok((class_name, ModApiClass {
			description,
			methods: methods,
		}))
	}).collect::<Result<HashMap<_, _>>>()?;
	
	let host_fns = parse_host_fn!(&mod_api_root, "root", "host_functions")?;

	Ok(ModApi{
		entities: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiEntity<'_>>, HashMap<&'static NTStr, ModApiEntity<'static>>>(entities)},
		classes : unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiClass <'_>>, HashMap<&'static NTStr, ModApiClass <'static>>>(classes )},
		host_fns: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiHostFn<'_>>, HashMap<&'static NTStr, ModApiHostFn<'static>>>(host_fns)},
		_arena: arena,
	})
}
