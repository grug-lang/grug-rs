use std::collections::HashMap;
use std::path::Path;
use std::io::Write;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, GrugType};
use crate::arena::Arena;
use crate::error::{ErrorKind, Error, SourceSpan, Result};
use crate::types::{GameFnPtr, GameFnRegisterer};

use allocator_api2::vec::Vec;

use json::JsonValue;
use json::object::Object;

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
	pub(crate) ty: GrugType<'a>,
	pub(crate) methods: &'a mut [(&'a NTStr, ModApiHostFn<'a>)],
	pub(crate) generics: &'a [&'a NTStr],
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
	pub(crate) generics: &'a [&'a NTStr],
	pub(crate) parameters: &'a [Parameter<'a>],
	pub(crate) return_ty: GrugType<'a>,
	pub(crate) fn_ptr: Option<GameFnPtr>,
	pub(crate) _registerer: Option<GameFnRegisterer>,
}

struct ModApiContext<'a, 'error> {
	json_path: JsonPath<'a>,
	path: &'error Path,
	text: &'error str,
	arena: &'a Arena,
}

impl<'a, 'error> ModApiContext<'a, 'error> {
	fn new(path: &'error Path, text: &'error str, arena: &'a Arena) -> Self {
		Self {
			json_path: JsonPath(Vec::new_in(arena)),
			path, 
			text,
			arena,
		}
	}

	fn push_path(&mut self, value: JsonPathComponent<'a>) {self.json_path.0.push(value)}
	fn pop_path (&mut self)                               {self.json_path.0.pop().unwrap();}

	#[track_caller]
	fn new_error(&self, message: &str) -> Error {
		let mut location = Vec::new_in(self.arena);
		write!(location, "{}", &self.json_path).expect("writing into a vec can never fail");
		let location = unsafe{std::str::from_utf8_unchecked(location.leak())};
		Error::new(
			ErrorKind::MOD_API_JSON_ERROR,
			location,
			self.path.as_ref(), 
			self.text,
			SourceSpan{offset: 0, line: 0},
			format_args!("{} {}", location, message),
		)
	}

	// gets the key and pushes the path onto self
	fn get_key<'b>(&mut self, object: &'b Object, key: &'a str) -> Result<&'b JsonValue> {
		self.push_path(JsonPathComponent::ObjectKey(key));
		object.get(key).ok_or_else(|| self.new_error("does not exist"))
	}

	fn parse_type<'b>(&mut self, object: &'a JsonValue, used_generics: &'b[&'b NTStr], arena: &'b Arena) -> Result<GrugType<'b>> {
		let JsonValue::Object(object) = object else {
			return Err(self.new_error("is not an object"));
		};
		// "name" string
		let ty = match object.get("name") {
			None => return Ok(GrugType::Void),
			Some(str) => {
				self.push_path(JsonPathComponent::ObjectKey("name"));
				str.as_str().ok_or_else(|| self.new_error("is not a string"))?
			}
		};
		self.pop_path();

		let ty = match ty {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"   => GrugType::Number,
			"string"   => GrugType::String,
			"entity"   => {
				// "entity_type" string
				let entity_type = self.get_key(object, "entity_type")?.as_str().ok_or_else(|| self.new_error("is not a string"))?;
				self.pop_path();
				GrugType::Entity{
					entity_type: (!entity_type.is_empty()).then(|| {
						arena.copy_str_into_nt(entity_type).as_ntstrptr()
					})
				}
			},
			"resource" => {
				// "resource_extension" string
				let entity_type = self.get_key(object, "resource_extension")?.as_str().ok_or_else(|| self.new_error("is not a string"))?;
				self.pop_path();
				GrugType::Resource {
					extension: arena.copy_str_into_nt(entity_type).as_ntstrptr(),
				}
			}
			generic if generic.starts_with("$") => {
				for (i, name) in used_generics.iter().enumerate() {
					if &***name == generic {
						return Ok(GrugType::Existential{idx: i});
					}
				}
				self.push_path(JsonPathComponent::ObjectKey("name"));
				return Err(self.new_error("is an undeclared generic"));
			}
			type_name => {
				let name = arena.copy_str_into_nt(type_name).as_ntstrptr();
				// recursively parse types within generics
				let generics = if let Some(generics) = object.get("generics") {
					self.push_path(JsonPathComponent::ObjectKey("generics"));
					let mut temp = Vec::new_in(arena);
					let JsonValue::Array(generics) = generics else {
						return Err(self.new_error("is not an array"));
					};
					for (i, generic) in generics.iter().enumerate() {
						self.push_path(JsonPathComponent::ArrayIdx(i));
						temp.push(self.parse_type(generic, used_generics, arena)?);
						self.pop_path();
					}
					self.pop_path();
					&*temp.leak()
				} else {
					&[]
				};
				GrugType::Id {
					name,
					generics,
				}
			}
		};
		Ok(ty)
	}

	fn parse_parameters<'b>(&mut self, parameters: &'a JsonValue, generics: &'b [&'b NTStr], arena: &'b Arena) -> Result<&'b [Parameter<'b>]> {
		let JsonValue::Array(parameters) = parameters else {
			return Err(self.new_error("is not an array"));
		};
		let mut temp = Vec::new_in(arena);
		for (i, param_values) in parameters.iter().enumerate() {
			self.push_path(JsonPathComponent::ArrayIdx(i));
			let JsonValue::Object(param_values) = param_values else {
				return Err(self.new_error("is not an object"));
			};
			// required "name" string
			let param_name = self.get_key(param_values, "name")?.as_str().ok_or_else(|| self.new_error("is not a string"))?;
			self.pop_path();

			// replace [index] with ["<name>"] in path
			self.pop_path();
			self.push_path(JsonPathComponent::ArrayKey(param_name));

			let param_name = arena.copy_str_into_nt(param_name);
			
			// required "type" string
			let ty = self.get_key(param_values, "type")?;
			let ty = self.parse_type(ty, generics, arena)?;
			match &ty {
				GrugType::Void => return Err(self.new_error("cannot be void")),
				_ => (),
			}
			self.pop_path();

			self.pop_path();
			temp.push(Parameter{
				name: param_name.as_ntstrptr(),
				ty,
				name_span: SourceSpan{offset: 0, line: 0},
				type_span: SourceSpan{offset: 0, line: 0},
			});
		}
		Ok(temp.leak())
	}

	fn parse_host_fn<'b>(&mut self, host_fn_values: &'a JsonValue, parent_generics: &[&'b NTStr], arena: &'b Arena) -> Result<ModApiHostFn<'b>> {
		let JsonValue::Object(host_fn_values) = host_fn_values else {
			return Err(self.new_error("is not an object"));
		};
		// optional "description" string
		let description = host_fn_values.get("description").map(|inner| {
			self.push_path(JsonPathComponent::ObjectKey("description"));
			let description = arena.copy_str_into(inner.as_str().ok_or_else(|| self.new_error("is not a string"))?);
			self.pop_path();
			Ok(description)
		}).transpose()?;

		// optional "used_generics" key
		let mut used_generics = Vec::with_capacity_in(parent_generics.len(), arena);
		used_generics.extend(parent_generics);
		if let Some(generics) = host_fn_values.get("used_generics") {
			self.push_path(JsonPathComponent::ObjectKey("used_generics"));
			let JsonValue::Array(generics) = generics else {
				return Err(self.new_error("is not an array"));
			};
			for (i, generic) in generics.iter().enumerate() {
				self.push_path(JsonPathComponent::ArrayIdx(i));
				used_generics.push(arena.copy_str_into_nt(generic.as_str().ok_or_else(|| self.new_error("is not a string"))?));
				self.pop_path();
			}
			self.pop_path();
		}
		let generics = used_generics.leak();
		
		// optional "parameters" array 
		let parameters = if let Some(parameters) = host_fn_values.get("parameters") {
			self.push_path(JsonPathComponent::ObjectKey("parameters"));
			let parameters = self.parse_parameters(parameters, generics, arena)?;
			self.pop_path();
			parameters
		} else {
			&[]
		};

		let return_ty = if let Some(return_ty) = host_fn_values.get("return_type") {
			self.push_path(JsonPathComponent::ObjectKey("return_type"));
			let return_ty = self.parse_type(return_ty, generics, arena)?;
			match &return_ty {
				GrugType::Entity{..} => return Err(self.new_error("cannot be entity")),
				GrugType::Resource{..} => return Err(self.new_error("cannot be resource")),
				_ => (),
			}
			self.pop_path();
			return_ty
		} else {
			GrugType::Void
		};

		Ok(ModApiHostFn{
			description,
			return_ty,
			generics,
			parameters,
			fn_ptr: None,
			_registerer: None,
		})
	}
}

struct JsonPath<'a>(Vec<JsonPathComponent<'a>, &'a Arena>);

enum JsonPathComponent<'a> {
	ObjectKey(&'a str),
	ArrayIdx(usize),
	ArrayKey(&'a str),
}

impl<'a> std::fmt::Display for JsonPath<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_str("root")?;
		for component in &self.0 {match component {
			JsonPathComponent::ObjectKey(key) => write!(f, ".{}", key)?,
			JsonPathComponent::ArrayIdx(idx) => write!(f, "[{}]", idx)?,
			JsonPathComponent::ArrayKey(key) => write!(f, "[\"{}\"]", key)?,
		}}
		Ok(())
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
	let arena = Arena::new();
	let context: ModApiContext = ModApiContext::new(mod_api_path.as_ref(), mod_api_text, &arena);

	let mod_api_json = json::parse(mod_api_text).map_err(|err| {
		context.new_error(arena.fmt_into(format_args!("{err}")))
	})?;
	let JsonValue::Object(mod_api_root) = mod_api_json else {
		return Err(context.new_error("is not an object"));
	};

	// This is needed to get around the drop checker
	let mut context = context;

	let entities = context.get_key(&mod_api_root, "entities")?;
	let JsonValue::Object(entities) = entities else {
		return Err(context.new_error("is not an object"));
	};

	let entities = entities.iter().map(|(entity_name, entity_values)| {
		context.push_path(JsonPathComponent::ObjectKey(entity_name));
		let entity_name = arena.copy_str_into_nt(entity_name);
		let JsonValue::Object(entity_values) = entity_values else {
			return Err(context.new_error("is not an object"));
		};

		// optional "description" string
		let description = entity_values.get("description").map(|inner| {
			context.push_path(JsonPathComponent::ObjectKey("description"));
			let description = arena.copy_str_into(inner.as_str().ok_or_else(|| context.new_error("is not a string"))?);
			context.pop_path();
			Ok(description)
		}).transpose()?;

		let export_fns = if let Some(export_fns) = entity_values.get("export_functions") {
			context.push_path(JsonPathComponent::ObjectKey("export_functions"));
			let JsonValue::Array(export_fns) = export_fns else {
				return Err(context.new_error("is not an array"));
			};
			let mut temp = Vec::new_in(&arena);
			for (i, export_fn_values) in export_fns.iter().enumerate() {
				context.push_path(JsonPathComponent::ArrayIdx(i));
				let JsonValue::Object(export_fn_values) = export_fn_values else {
					return Err(context.new_error("is not an object"));
				};
				// required "name" string
				let name = context.get_key(export_fn_values, "name")?.as_str().ok_or_else(|| context.new_error("is not a string"))?;
				context.pop_path();

				context.pop_path();
				context.push_path(JsonPathComponent::ArrayKey(name));

				let name = arena.copy_str_into_nt(name);

				// optional "description" string
				let description = export_fn_values.get("description").map(|inner| {
					context.push_path(JsonPathComponent::ObjectKey("description"));
					let description = arena.copy_str_into(inner.as_str().ok_or_else(|| context.new_error("is not a string"))?);
					context.pop_path();
					Ok(description)
				}).transpose()?;
				
				// optional "parameters" array 
				let parameters = if let Some(parameters) = export_fn_values.get("parameters") {
					context.push_path(JsonPathComponent::ObjectKey("parameters"));
					let parameters = context.parse_parameters(parameters, &[], &arena)?;
					context.pop_path();
					parameters
				} else {
					&[]
				};

				context.pop_path();
				temp.push((name, ModApiExportFn{
					description,
					parameters,
				}))
			}

			context.pop_path();
			temp.leak()
		} else {
			&mut []
		};
		context.pop_path();
		Ok((entity_name, ModApiEntity{
			description,
			export_fns
		}))
	}).collect::<Result<HashMap<_, _>>>()?;
	context.pop_path();
	
	// "classes" object
	let classes = context.get_key(&mod_api_root, "classes")?;
	let JsonValue::Object(classes) = classes else {
		return Err(context.new_error("is not an object"));
	};
	let classes = classes.iter().map(|(class_name, class_values)| {
		context.push_path(JsonPathComponent::ObjectKey(class_name));
		let class_name = arena.copy_str_into_nt(class_name);
		let JsonValue::Object(class_values) = class_values else {
			return Err(context.new_error("is not an object"));
		};
		// optional "description" string
		let description = class_values.get("description").map(|inner| {
			context.push_path(JsonPathComponent::ObjectKey("description"));
			let description = arena.copy_str_into(inner.as_str().ok_or_else(|| context.new_error("is not a string"))?);
			context.pop_path();
			Ok(description)
		}).transpose()?;

		// optional "used_generics" key
		let mut used_generics = Vec::new_in(&arena);
		if let Some(generics) = class_values.get("used_generics") {
			context.push_path(JsonPathComponent::ObjectKey("used_generics"));
			let JsonValue::Array(generics) = generics else {
				return Err(context.new_error("is not an array"));
			};
			for (i, generic) in generics.iter().enumerate() {
				context.push_path(JsonPathComponent::ArrayIdx(i));
				used_generics.push(arena.copy_str_into_nt(generic.as_str().ok_or_else(|| context.new_error("is not a string"))?));
				context.pop_path();
			}
			context.pop_path();
		}
		let generics = used_generics.leak();

		let ty = GrugType::Id {
			name: class_name.as_ntstrptr(),
			generics: arena.slice_from_iter((0..(generics.len())).map(|i| {
				GrugType::Existential{idx: i}
			})),
		};

		// optional "methods" object
		let methods = if let Some(methods) = class_values.get("methods") {
			context.push_path(JsonPathComponent::ObjectKey("methods"));
			let JsonValue::Object(methods) = methods else {
				return Err(context.new_error("is not an object"));
			};
			let mut temp = Vec::new_in(&arena);
			for (method_name, method_values) in methods.iter() {
				context.push_path(JsonPathComponent::ObjectKey(method_name));
				let method_name = arena.copy_str_into_nt(method_name);
				temp.push((method_name, context.parse_host_fn(method_values, generics, &arena)?));
				context.pop_path();
			}
			context.pop_path();
			temp.leak()
		} else {
			&mut []
		};
		context.pop_path();
		Ok((class_name, ModApiClass {
			description,
			ty,
			generics,
			methods,
		}))
	}).collect::<Result<HashMap<_, _>>>()?;
	context.pop_path();
	
	let host_fns = context.get_key(&mod_api_root, "host_functions")?;
	let JsonValue::Object(host_fns) = host_fns else {
		return Err(context.new_error("is not an object"));
	};
	let host_fns = host_fns.iter().map(|(host_fn_name, host_fn_values)| {
		context.push_path(JsonPathComponent::ObjectKey(host_fn_name));
		let host_fn_name = arena.copy_str_into_nt(host_fn_name);
		let host_fn = context.parse_host_fn(host_fn_values, &[], &arena)?;
		context.pop_path();
		Ok((host_fn_name, host_fn))
	}).collect::<Result<HashMap<_, _>>>()?;
	context.pop_path();
	
	assert_eq!(0, context.json_path.0.len(), "{}", &context.json_path);
	drop(context);

	Ok(ModApi{
		entities: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiEntity<'_>>, HashMap<&'static NTStr, ModApiEntity<'static>>>(entities)},
		classes : unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiClass <'_>>, HashMap<&'static NTStr, ModApiClass <'static>>>(classes )},
		host_fns: unsafe{std::mem::transmute::<HashMap<&'_ NTStr, ModApiHostFn<'_>>, HashMap<&'static NTStr, ModApiHostFn<'static>>>(host_fns)},
		_arena: arena,
	})
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn mod_api_test_ok() {
		let text = r#"{
			"classes": {},
			"entities": {},
			"host_functions": {}
		}"#;
		get_mod_api_from_text("test", text).unwrap();
	}

	#[test]
	fn mod_api_test_err_1() {
		let text = r#"{
			"classes": {
				"Test": 42
			},
			"entities": {},
			"host_functions": {}
		}"#;
		match get_mod_api_from_text("test", text) {
			Ok(_) => panic!("expected failure"),
			Err(err) => assert_eq!(err.inner().error_message.to_str(), "root.classes.Test is not an object")
		}
	}
}
