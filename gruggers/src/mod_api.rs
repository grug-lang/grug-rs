use std::collections::HashMap;
use std::path::Path;
use std::io::Write;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, Type};
use crate::arena::Arena;
use crate::state::GrugState;
use crate::error::{ErrorKind, Error, SourceSpan, Result};
use crate::types::{HostFn, HostFnReg, HostFnRegErased};

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

	pub(crate) fn classes<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiClass<'a>> {
		// SAFETY: Invariance of the methods field requires this transmute
		// This transmute brings it back to the actual lifetime
		unsafe{std::mem::transmute::<&'a HashMap<&'static NTStr, ModApiClass<'static>>, &'a HashMap<&'a NTStr, ModApiClass<'a>>>(&self.classes)}
	}

	pub(crate) fn host_fns<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiHostFn<'a>> {
		&self.host_fns
	}

	pub(crate) fn register_fn(&mut self, class_name: Option<&str>, fn_name: &str, ptr: HostFn) -> Result<()> {
		if let Some(class_name) = class_name {
			let Some(class) = self.classes.get_mut(class_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Class with name '{}' is not found in mod_api.json", class_name),
				));
			};
			let Some((_, host_fn_data)) = class.methods.iter_mut().find(|(name, _)| name.as_str() == fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Class with name '{}' does not contain method with name '{}'", class_name, fn_name),
				));
			};
			if !host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host method '{}' on class '{}' is generic", class_name, fn_name),
				));
			}
			match &mut host_fn_data.fn_ptr {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Host method named '{}' on class '{}' has already been registered", fn_name, class_name),
					));
				}
				x => *x = Some(ptr),
			}
		} else {
			let Some(host_fn_data) = self.host_fns.get_mut(fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function named '{}' is not found in mod_api.json", fn_name),
				));
			};
			if !host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' is generic", fn_name),
				));
			}
			match &mut host_fn_data.fn_ptr {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Host function named '{}' has already been registered", fn_name),
					));
				}
				x => *x = Some(ptr),
			}
		}
		Ok(())
	}

	/// Registers a generic function and checks that the number of generics
	/// expected by the functions matches the number defined in the mod_api
	pub(crate) fn register_generic_fn<const N: usize>(&mut self, class_name: Option<&str>, fn_name: &str, ptr: HostFnReg<N, GrugState>) -> Result<()> {
		if let Some(class_name) = class_name {
			let Some(class) = self.classes.get_mut(class_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Class '{}' is not found in mod_api.json", class_name),
				));
			};
			let Some((_, host_fn_data)) = class.methods.iter_mut().find(|(name, _)| name.as_str() == fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Method {}.{} is not found in the mod_api.json", class_name, fn_name),
				));
			};
			if host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Method {}.{} is not generic", class_name, fn_name),
				));
			}
			if host_fn_data.generics.len() != N {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Method {}.{} has {} generics but the function provided expects {} generics", class_name, fn_name, host_fn_data.generics.len(), N),
				));
			}
			match &mut host_fn_data.registerer {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Method {}.{} has already been registered", fn_name, class_name),
					));
				}
				x => *x = Some(ptr.into()),
			}
		} else {
			let Some(host_fn_data) = self.host_fns.get_mut(fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' is not found in mod_api.json", fn_name),
				));
			};
			if host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' is not generic", fn_name),
				));
			}
			if host_fn_data.generics.len() != N {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' has {} generics but the function provided expects {} generics", fn_name, host_fn_data.generics.len(), N),
				));
			}
			match &mut host_fn_data.registerer {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Host function '{}' has already been registered", fn_name),
					));
				}
				x => *x = Some(ptr.into()),
			}
		}
		Ok(())
	}

	/// Registers a generic function and does not check that the number of
	/// generics expected by the functions matches the number defined in the
	/// mod_api. 
	///
	/// This is intended to be used directly by c code
	pub(crate) unsafe fn register_generic_fn_unchecked(&mut self, class_name: Option<&str>, fn_name: &str, ptr: HostFnRegErased) -> Result<()> {
		if let Some(class_name) = class_name {
			let Some(class) = self.classes.get_mut(class_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Class '{}' is not found in mod_api.json", class_name),
				));
			};
			let Some((_, host_fn_data)) = class.methods.iter_mut().find(|(name, _)| name.as_str() == fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Method {}.{} is not found in mod_api.json", class_name, fn_name),
				));
			};
			if host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Method {}.{} is not generic", class_name, fn_name),
				));
			}
			match &mut host_fn_data.registerer {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Method {}.{} has already been registered", fn_name, class_name),
					));
				}
				x => *x = Some(ptr),
			}
		} else {
			let Some(host_fn_data) = self.host_fns.get_mut(fn_name) else {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' is not found in mod_api.json", fn_name),
				));
			};
			if host_fn_data.generics.is_empty() {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host function '{}' is not generic", fn_name),
				));
			}
			match &mut host_fn_data.registerer {
				Some(_) => {
					return Err(Error::new(
						ErrorKind::FUNCTION_REGISTRATION_ERROR,
						"",
						"".as_ref(),
						"",
						SourceSpan{offset: 0, line: 0},
						format_args!("Host function '{}' has already been registered", fn_name),
					));
				}
				x => *x = Some(ptr),
			}
		}
		Ok(())
	}

	pub(crate) unsafe fn register_dummies(&mut self) {
		use crate::state::GrugState;
		use crate::types::Value;
		extern "C" fn dummy_host_fn(_state: &GrugState, _arguments: *const Value) -> Value {
			Value{void: ()}
		}
		unsafe extern "C" fn dummy_generic_fn<'a>(_: *const Type<'a>) -> Option<HostFn> {
			Some(HostFn::from_ptr(dummy_host_fn))
		}
		let dummy_generic_fn = (dummy_generic_fn as for<'a> unsafe extern "C" fn (*const Type<'a>) -> _).into();
		for (_, host_fn) in &mut self.host_fns {
			if host_fn.generics.is_empty() {
				host_fn.fn_ptr = const{Some(HostFn::from_ptr(dummy_host_fn))};
			} else {
				host_fn.registerer = Some(dummy_generic_fn);
			}
		}
		for (_, class) in &mut self.classes {
			for (_, host_fn) in &mut *class.methods {
				if host_fn.generics.is_empty() {
					host_fn.fn_ptr = const{Some(HostFn::from_ptr(dummy_host_fn))};
				} else {
					host_fn.registerer = Some(dummy_generic_fn);
				}
			}
		}
	}
}

#[derive(Debug)]
pub(crate) struct ModApiClass<'a> {
	#[expect(dead_code)]
	pub(crate) description: &'a str,
	pub(crate) ty: Type<'a>,
	pub(crate) methods: &'a mut [(&'a NTStr, ModApiHostFn<'a>)],
	pub(crate) generics: &'a [&'a NTStr],
}

#[derive(Debug)]
pub(crate) struct ModApiEntity<'a> {
	#[expect(dead_code)]
	pub(crate) description: &'a str,
	pub(crate) export_fns: &'a [(&'a NTStr, ModApiExportFn<'a>)],
}

impl<'a> ModApiEntity<'a> {
	pub(crate) fn get_export_fn(&self, name: &str) -> Option<(usize, &ModApiExportFn<'_>)> {
		self.export_fns.iter().enumerate().find_map(|(i, (fn_name, func))| (name == fn_name.as_str()).then_some((i, func)))
	}
}

#[derive(Debug)]
pub(crate) struct ModApiExportFn<'a> {
	#[expect(dead_code)]
	pub(super) description: &'a str,
	pub(super) parameters: &'a [Parameter<'a>],
}

#[derive(Debug)]
pub(crate) struct ModApiHostFn<'a> {
	#[expect(dead_code)]
	pub(crate) description: &'a str,
	pub(crate) generics: &'a [&'a NTStr],
	pub(crate) parameters: &'a [Parameter<'a>],
	pub(crate) return_ty: Type<'a>,
	pub(crate) fn_ptr: Option<HostFn>,
	pub(crate) registerer: Option<HostFnRegErased>,
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
			ErrorKind::MOD_API_ERROR,
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

	fn parse_type<'b>(&mut self, object: &'a JsonValue, used_generics: &'b[&'b NTStr], arena: &'b Arena) -> Result<Type<'b>> {
		let JsonValue::Object(object) = object else {
			return Err(self.new_error("is not an object"));
		};
		// "name" string
		let ty = self.get_key(object, "name")?;
		let ty = ty.as_str().ok_or_else(|| self.new_error("is not a string"))?;
		self.pop_path();

		let ty = match ty {
			"void"     => Type::Void,
			"bool"     => Type::Bool,
			"number"   => Type::Number,
			"string"   => Type::String,
			"entity"   => {
				// "entity_type" string
				let entity_type = self.get_key(object, "entity_type")?.as_str().ok_or_else(|| self.new_error("is not a string"))?;
				self.pop_path();
				Type::Entity{
					entity_type: (!entity_type.is_empty()).then(|| {
						arena.copy_str_into_nt(entity_type).as_ntstrptr()
					})
				}
			},
			"resource" => {
				// "resource_extension" string
				let entity_type = self.get_key(object, "resource_extension")?.as_str().ok_or_else(|| self.new_error("is not a string"))?;
				self.pop_path();
				Type::Resource {
					extension: arena.copy_str_into_nt(entity_type).as_ntstrptr(),
				}
			}
			generic if generic.starts_with("$") => {
				for (i, name) in used_generics.iter().enumerate() {
					if &***name == generic {
						return Ok(Type::Existential{idx: i});
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
				Type::Id {
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
				Type::Void => return Err(self.new_error("cannot be void")),
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
		// "description" string
		let description = self.get_key(host_fn_values, "description")?;
		let description = arena.copy_str_into(description.as_str().ok_or_else(|| self.new_error("is not a string"))?);
		self.pop_path();

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
				Type::Entity{..} => return Err(self.new_error("cannot be entity")),
				Type::Resource{..} => return Err(self.new_error("cannot be resource")),
				_ => (),
			}
			self.pop_path();
			return_ty
		} else {
			Type::Void
		};

		Ok(ModApiHostFn{
			description,
			return_ty,
			generics,
			parameters,
			fn_ptr: None,
			registerer: None,
		})
	}

	fn validate_function(&mut self, parameters: &[Parameter<'a>], return_type: Type<'a>, known_types: &[(&str, usize)]) -> Result<()> {
		self.push_path(JsonPathComponent::ObjectKey("parameters"));
		for parameter in parameters {
			self.push_path(JsonPathComponent::ArrayKey(parameter.name.to_str()));
			self.validate_type(parameter.ty, known_types)?;
			self.pop_path();
		}
		self.pop_path();
		self.push_path(JsonPathComponent::ObjectKey("return_type"));
		self.validate_type(return_type, known_types)?;
		self.pop_path();
		Ok(())
	}

	fn validate_type(&mut self, ty: Type<'a>, known_types: &[(&str, usize)]) -> Result<()> {
		match ty {
			Type::Id {
				name,
				generics
			} => {
				// If the type is found, then check if the number of generics
				// match and also recursively check types
				self.push_path(JsonPathComponent::ObjectKey("generics"));
				if let Some((_, num_generics)) = known_types.iter().find(|(ty_name, _)| *ty_name == name.to_str()) {
					if *num_generics != generics.len() {
						return Err(self.new_error(self.arena.fmt_into(format_args!(": {} was declared to have {} generics but here it has {}", name, num_generics, generics.len()))));
					}
					for (i, generic) in generics.iter().enumerate() {
						self.push_path(JsonPathComponent::ArrayIdx(i));
						self.validate_type(*generic, known_types)?;
						self.pop_path();
					}
				// TODO: Change the mod api format so this always throws an
				// error
				// if not found, number of generics MUST be 0
				} else {
					if generics.len() != 0 {
						return Err(self.new_error(self.arena.fmt_into(format_args!(": {} was not declared in \"classes\", so it cannot have generics", name))));
					}
				}
				self.pop_path();
			}
			_ => (),
		}
		Ok(())
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
	let mod_api_path = mod_api_path.as_ref();

	let mod_api_json = json::parse(mod_api_text).map_err(|err| {
		Error::new(
			ErrorKind::MOD_API_JSON_ERROR,
			"",
			mod_api_path.as_ref(),
			mod_api_text,
			SourceSpan{offset: 0, line: 0},
			format_args!("{err}"),
		)
	})?;

	let context = ModApiContext::new(mod_api_path, mod_api_text, &arena);

	let JsonValue::Object(mod_api_root) = mod_api_json else {
		return Err(context.new_error("is not an object"));
	};

	// This is needed to get around the drop check
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

		// "description" string
		let description = context.get_key(entity_values, "description")?;
		let description = arena.copy_str_into(description.as_str().ok_or_else(|| context.new_error("is not a string"))?);
		context.pop_path();

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

				// "description" string
				let description = context.get_key(export_fn_values, "description")?;
				let description = arena.copy_str_into(description.as_str().ok_or_else(|| context.new_error("is not a string"))?);
				context.pop_path();
				
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
		// "description" string
		let description = context.get_key(class_values, "description")?;
		let description = arena.copy_str_into(description.as_str().ok_or_else(|| context.new_error("is not a string"))?);
		context.pop_path();

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

		let ty = Type::Id {
			name: class_name.as_ntstrptr(),
			generics: arena.slice_from_iter((0..(generics.len())).map(|i| {
				Type::Existential{idx: i}
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

	let mut known_types = Vec::with_capacity_in(entities.len() + classes.len(), &arena);
	// Collect all entities as types with no generics
	context.push_path(JsonPathComponent::ObjectKey("entities"));
	for entity_name in entities.keys() {
		// Dont need to check for duplicates yet because there can't be any
		known_types.push((entity_name.as_str(), 0));
	}
	context.pop_path();

	context.push_path(JsonPathComponent::ObjectKey("classes"));
	// Collect all classes and the number of generics they declare
	for (class_name, class_data) in classes.iter() {
		context.push_path(JsonPathComponent::ObjectKey(class_name));
		if known_types.iter().find(|(type_name, _)| *type_name == class_name.as_str()).is_some() {
			return Err(context.new_error("class name already exists"));
		}
		known_types.push((class_name.as_str(), class_data.generics.len()));
		context.pop_path();
	}

	let known_types = known_types.leak();
	// Check every type within each class and its methods to make sure the
	// number of generics they use is correct
	for (class_name, class_data) in classes.iter() {
		context.push_path(JsonPathComponent::ObjectKey(class_name));
		context.push_path(JsonPathComponent::ObjectKey("methods"));
		for (method_name, host_fn) in &*class_data.methods {
			context.push_path(JsonPathComponent::ObjectKey(method_name));
			context.validate_function(host_fn.parameters, host_fn.return_ty, known_types)?;
			context.pop_path();
		}
		context.pop_path();
		context.pop_path();
	}
	context.pop_path();

	context.push_path(JsonPathComponent::ObjectKey("host_functions"));
	// Check every type within each host function and make sure the number of
	// generics they use is correct
	for (fn_name, host_fn) in host_fns.iter() {
		context.push_path(JsonPathComponent::ObjectKey(fn_name));
		context.validate_function(host_fn.parameters, host_fn.return_ty, known_types)?;
		context.pop_path();
	}
	context.pop_path();
	// Check every type within each entity and its export functions and make
	// sure the number of generics they use is correct
	context.push_path(JsonPathComponent::ObjectKey("entities"));
	for (entity_name, entity) in entities.iter() {
		context.push_path(JsonPathComponent::ObjectKey(entity_name));
		for (fn_name, export_fn) in entity.export_fns {
			context.push_path(JsonPathComponent::ObjectKey(fn_name));
			context.validate_function(export_fn.parameters, Type::Void, known_types)?;
			context.pop_path();
		}
		context.pop_path();
	}
	context.pop_path();
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
