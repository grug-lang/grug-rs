// TODO: Constraints on classes have to be verified for all functions that use
// them.
// TODO: Using the same name for a generic on a method as a generic on the
// class should actually add new constraints to that generic
// TODO: Methods on entities
use std::collections::HashMap;
use std::path::Path;
use std::io::Write;
use std::ptr::NonNull;
use std::cell::UnsafeCell;
use std::ffi::c_void;

use crate::ntstring::NTStr;
use crate::ast::{Parameter, Type};
use crate::arena::Arena;
use crate::state::GrugState;
use crate::error::{ErrorKind, Error, SourceSpan, Result};
use crate::types::{HostFn, HostFnWithState, HostFnReg, HostFnRegErased};
use crate::HAS_CONSTRAINTS;

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

	pub(crate) fn register_fn<const N: usize>(&mut self, class_name: Option<&str>, fn_name: &str, ptr: HostFnWithState<N, GrugState>) -> Result<()> {
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
					format_args!("Class '{}' does not contain method with name '{}'", class_name, fn_name),
				));
			};

			if !host_fn_data.generics.is_empty() && N == 0 {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host method '{}.{}' is supposed to be generic", class_name, fn_name),
				));
			}

			if host_fn_data.generics.len() != N {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host method '{}.{}' is supposed to have {} generics, but it has {}", class_name, fn_name, host_fn_data.generics.len(), N),
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
						format_args!("Host method '{}.{}' has already been registered", class_name, fn_name),
					));
				}
				x => *x = Some(HostFn::from_ptr(ptr)),
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

			if host_fn_data.generics.len() != N {
				return Err(Error::new(
					ErrorKind::FUNCTION_REGISTRATION_ERROR,
					"",
					"".as_ref(),
					"",
					SourceSpan{offset: 0, line: 0},
					format_args!("Host method '{}' is supposed to have {} generics, but it has {}", fn_name, host_fn_data.generics.len(), N),
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
						format_args!("Host function '{}' has already been registered", fn_name),
					));
				}
				x => *x = Some(HostFn::from_ptr(ptr)),
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
		use crate::types::Value;
		extern "C" fn dummy_host_fn(_state: *const c_void, _arguments: *const Value, _generics: *const Type) -> Value {
			Value{void: ()}
		}
		unsafe extern "C" fn dummy_generic_fn(_: *const Type<'static>) -> Option<HostFn> {
			Some(HostFn::from_erased_ptr(dummy_host_fn))
		}
		let dummy_generic_fn = (dummy_generic_fn as unsafe extern "C" fn (*const Type<'static>) -> _).into();
		for (_, host_fn) in &mut self.host_fns {
			if host_fn.generics.is_empty() {
				host_fn.fn_ptr = const{Some(HostFn::from_erased_ptr(dummy_host_fn))};
			} else {
				host_fn.registerer = Some(dummy_generic_fn);
			}
		}
		for (_, class) in &mut self.classes {
			for (_, host_fn) in &mut *class.methods {
				if host_fn.generics.is_empty() {
					host_fn.fn_ptr = const{Some(HostFn::from_erased_ptr(dummy_host_fn))};
				} else {
					host_fn.registerer = Some(dummy_generic_fn);
				}
			}
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TraitImplementor<'a> {
	pub(crate) generics: &'a[Generic<'a>],
	pub(crate) ty: Type<'a>
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Trait<'a> {
	pub(crate) name: &'a str,
	pub(crate) implementors: &'a [TraitImplementor<'a>]
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Generic<'a> {
	pub(crate) name: &'a NTStr,
	// Actually supposed to be &'a [&'a Trait<'a>],
	// but it has to be NonNull to allow mutation during construction
	traits: &'a [NonNull<Trait<'a>>],
}

impl<'a> Generic<'a> {
	pub(crate) fn traits(&self) -> &[&'a Trait<'a>] {
		// SAFETY: This is the actual type, but i have to use NonNull to allow
		// mutation during construction.
		
		// Note: This function is only safe to call after construction is
		// finished
		unsafe{std::mem::transmute::<&[NonNull<Trait<'a>>], &[&'a Trait<'a>]>(self.traits)}
	}
}

#[derive(Debug)]
pub(crate) struct ModApiClass<'a> {
	#[expect(dead_code)]
	pub(crate) description: &'a str,
	pub(crate) ty: Type<'a>,
	pub(crate) methods: &'a mut [(&'a NTStr, ModApiHostFn<'a>)],
	pub(crate) generics: &'a [Generic<'a>],
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
	pub(crate) generics: &'a [Generic<'a>],
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

	#[track_caller]
	fn new_fmt_error(&self, message: std::fmt::Arguments) -> Error {
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
	
	// gets the key as a str and pushes the path onto self
	fn get_str<'b>(&mut self, object: &'b Object, key: &'a str) -> Result<&'b str> {
		self.get_key(object, key)?.as_str().ok_or_else(|| self.new_error("is not a string"))
	}
	
	// gets the key and pushes the path onto self
	fn get_key<'b>(&mut self, object: &'b Object, key: &'a str) -> Result<&'b JsonValue> {
		self.push_path(JsonPathComponent::ObjectKey(key));
		object.get(key).ok_or_else(|| self.new_error("does not exist"))
	}

	fn parse_type<'b>(&mut self, object: &'a JsonValue, used_generics: &'_[Generic<'_>], arena: &'b Arena) -> Result<Type<'b>> {
		let JsonValue::Object(object) = object else {
			return Err(self.new_error("is not an object"));
		};
		// "name" string
		let ty = self.get_str(object, "name")?;
		self.pop_path();

		let ty = match ty {
			"void"     => Type::Void,
			"bool"     => Type::Bool,
			"number"   => Type::Number,
			"string"   => Type::String,
			"entity"   => {
				// "entity_type" string
				let entity_type = self.get_str(object, "entity_type")?;
				self.pop_path();
				Type::Entity{
					entity_type: (!entity_type.is_empty()).then(|| {
						arena.copy_str_into_nt(entity_type).as_ntstrptr()
					})
				}
			},
			"resource" => {
				// "resource_extension" string
				let extension = self.get_str(object, "resource_extension")?;
				self.pop_path();
				Type::Resource {
					extension: arena.copy_str_into_nt(extension).as_ntstrptr(),
				}
			}
			generic if generic.starts_with("$") => {
				for (i, used_generic) in used_generics.iter().enumerate() {
					if &**used_generic.name == generic {
						return Ok(Type::Existential{idx: i});
					}
				}
				self.push_path(JsonPathComponent::ObjectKey("name"));
				return Err(self.new_fmt_error(format_args!("is an undeclared generic (\"{}\")", generic)));
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

	fn parse_parameters<'b>(&mut self, parameters: &'a JsonValue, generics: &'_ [Generic<'_>], arena: &'b Arena) -> Result<&'b [Parameter<'b>]> {
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
			let param_name = self.get_str(param_values, "name")?;
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

	fn parse_used_generics<'b>(&mut self, used_generics_json: &'a JsonValue, traits: &HashMap<&str, &'b Trait<'b>>, arena: &'b Arena) -> Result<&'b[Generic<'b>]> {
		let JsonValue::Array(used_generics_json) = used_generics_json else {
			return Err(self.new_error("is not an array"));
		};
		let mut used_generics = Vec::with_capacity_in(used_generics_json.len(), arena);
		for (i, used_generic) in used_generics_json.iter().enumerate() {
			self.push_path(JsonPathComponent::ArrayIdx(i));
			let generic = if HAS_CONSTRAINTS {
				let JsonValue::Object(used_generic) = used_generic else {
					return Err(self.new_error("is not an object"));
				};
				let name = arena.copy_str_into_nt(self.get_str(used_generic, "name")?);
				if !name.starts_with("$") {
					return Err(self.new_error("must begin with '$'"));
				}
				self.pop_path(); // "name"

				let constraints = if let Some(json_constraints) = used_generic.get("constraints") {
					self.push_path(JsonPathComponent::ObjectKey("constraints"));
					let JsonValue::Array(json_constraints) = json_constraints else {
						return Err(self.new_error("is not an array"));
					};

					let mut constraints = Vec::with_capacity_in(json_constraints.len(), arena);

					for (i, constraint) in json_constraints.iter().enumerate() {
						self.push_path(JsonPathComponent::ArrayIdx(i));
						let Some(constraint) = constraint.as_str() else {
							return Err(self.new_error("is not a string"));
						};
						self.pop_path();
						self.push_path(JsonPathComponent::ArrayKey(constraint));
						if let Some(data) = traits.get(constraint) {
							constraints.push(NonNull::from_ref(*data));
							self.pop_path(); // array idx for constraint
						} else {
							return Err(self.new_error("is an unknown constraint"));
						}
					}
					self.pop_path(); 
					constraints.leak()
				} else {
					&mut []
				};
				Generic{name, traits: &*constraints}
			} else {
				let Some(used_generic) = used_generic.as_str() else {
					return Err(self.new_error("is not a string"));
				};
				let name = arena.copy_str_into_nt(used_generic);
				if !name.starts_with("$") {
					return Err(self.new_error("must begin with '$'"));
				}
				Generic{name, traits: &[]}
			};
			used_generics.push(generic);
			self.pop_path(); // used generics idx
		}
		Ok(used_generics.leak())
	}

	fn parse_host_fn<'b>(&mut self, host_fn_values: &'a JsonValue, parent_generics: &'_ [Generic<'b>], traits: &HashMap<&str, &'b Trait<'b>>, arena: &'b Arena) -> Result<ModApiHostFn<'b>> {
		let JsonValue::Object(host_fn_values) = host_fn_values else {
			return Err(self.new_error("is not an object"));
		};
		// "description" string
		let description = arena.copy_str_into(self.get_str(host_fn_values, "description")?);
		self.pop_path();

		// optional "used_generics" key
		let mut used_generics = Vec::with_capacity_in(parent_generics.len(), arena);
		used_generics.extend(parent_generics);
		if let Some(generics) = host_fn_values.get("used_generics") {
			self.push_path(JsonPathComponent::ObjectKey("used_generics"));
			used_generics.extend(self.parse_used_generics(generics, traits, arena)?);
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

	fn validate_function(&mut self, parameters: &[Parameter<'a>], return_type: Type<'a>, used_generics: &[Generic], known_types: &[(&str, &[Generic])]) -> Result<()> {
		self.push_path(JsonPathComponent::ObjectKey("parameters"));
		for parameter in parameters {
			self.push_path(JsonPathComponent::ArrayKey(parameter.name.to_str()));
			self.validate_type(parameter.ty, used_generics, known_types)?;
			self.pop_path();
		}
		self.pop_path();
		self.push_path(JsonPathComponent::ObjectKey("return_type"));
		self.validate_type(return_type, used_generics, known_types)?;
		self.pop_path();
		Ok(())
	}
	
	fn validate_type(&mut self, ty: Type<'a>, used_generics: &[Generic], known_types: &[(&str, &[Generic])]) -> Result<()> {
		// Checks if a particular types matches a particular trait
		fn type_matches_implementor(actual: (Type, &[Generic]), imp: (Type, &[Generic])) -> bool {
			match (actual.0, imp.0) {
				(Type::Id{name: ty_name, generics: act_generic_types}, Type::Id{name: imp_name, generics: imp_generic_types}) if 
					ty_name == imp_name => {
						act_generic_types.iter().zip(imp_generic_types).all(|(ty, imp_gen_type)| type_matches_implementor((*ty, actual.1), (*imp_gen_type, imp.1)))
					}
				(Type::Existential{idx: act_idx}, Type::Existential{idx: imp_idx}) => {
					let act_traits = actual.1[act_idx].traits();
					let imp_traits = imp   .1[imp_idx].traits();
					// make sure the actual existential has at least the
					// traits expected by the implementor
					imp_traits.iter().all(|&imp_trait| act_traits.iter().any(|&act_trait| std::ptr::eq(imp_trait, act_trait)))
				}
				(ty, Type::Existential{idx}) => {
					imp.1.get(idx).expect("existential should always point to a valid generic")
						.traits().into_iter().all(|tr| {
							tr.implementors.into_iter().any(|imp| type_matches_implementor((ty, actual.1), (imp.ty, imp.generics)))
						})
				}
				(Type::Void, Type::Void) => true,
				(Type::Bool, Type::Bool) => true,
				(Type::Number, Type::Number) => true,
				(Type::String, Type::String) => true,
				(Type::Resource{..}, _) |
				(_, Type::Resource{..}) => unreachable!("resource strings cannot be used in generics"),
				(Type::Entity{..}, _) |
				(_, Type::Entity{..}) => unreachable!("entity strings cannot be used in generics"),
				_ => false
			}
		}
		match ty {
			Type::Id {
				name,
				generics
			} => {
				// If the type is found, then check if the number of generics
				// match and also recursively check types
				self.push_path(JsonPathComponent::ObjectKey("generics"));
				if let Some((_, constraints)) = known_types.iter().find(|(ty_name, _)| *ty_name == name.to_str()) {
					if constraints.len() != generics.len() {
						return Err(self.new_error(self.arena.fmt_into(format_args!(": {} was declared to have {} generics but here it has {}", name, constraints.len(), generics.len()))));
					}
					for (i, (generic, constraint)) in generics.iter().zip(*constraints).enumerate() {
						self.push_path(JsonPathComponent::ArrayIdx(i));
						match *generic {
							Type::Resource{..} => return Err(self.new_error("resource strings cannot be used in generics")),
							Type::Entity{..}   => return Err(self.new_error("entity strings cannot be used in generics")),
							_ => {
								for tr in constraint.traits() {
									// If the type is already an existential,
									// then check if it already implements the
									// constraint. If not, then go for the
									// other check.
									let is_existential_that_implements_constraint = if let Type::Existential{idx} = generic {
										let act_traits = used_generics[*idx].traits();
										act_traits.iter().any(|&act_trait| std::ptr::eq(act_trait, *tr))
									} else {
										false
									};
									let is_type_that_matches_a_trait_implementor = 
										tr.implementors.into_iter().any(|imp| type_matches_implementor((*generic, used_generics), (imp.ty, imp.generics)));
									if !is_existential_that_implements_constraint && !is_type_that_matches_a_trait_implementor {
										if let Type::Existential{idx} = generic {
											return Err(self.new_error(self.arena.fmt_into(format_args!("type '{}' must implement constraint '{}'", used_generics[*idx].name, tr.name))));
										} else {
											return Err(self.new_error(self.arena.fmt_into(format_args!("type '{}' must implement constraint '{}'", generic, tr.name))));
										}
									}
								}
								self.validate_type(*generic, used_generics, known_types)?;
							}
						}
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

	// This is needed to get around the drop check.
	// context must be dropped before mod_api_root
	let mut context = context;

	let traits = if let Some(constraints) = mod_api_root.get("constraints") && HAS_CONSTRAINTS {
		context.push_path(JsonPathComponent::ObjectKey("constraints"));
		let JsonValue::Object(constraints) = constraints else {
			return Err(context.new_error("is not an object"));
		};
		let traits_storage = arena.slice_from_iter(constraints.iter().map(|(name, _)| 
			UnsafeCell::new(Trait{
				name: arena.copy_str_into_nt(name), 
				implementors: &[]
			})
		));
		
		let traits = constraints.iter().enumerate().map(|(i, (constraint_name, constraint_values))| {
			context.push_path(JsonPathComponent::ObjectKey(constraint_name));
			let JsonValue::Object(constraint_values) = constraint_values else {
				return Err(context.new_error("is not an object"));
			};
			
			// "description" string
			let _description = arena.copy_str_into(context.get_str(constraint_values, "description")?);
			context.pop_path();

			let implementors_json = context.get_key(constraint_values, "implementors")?;
			let JsonValue::Array(implementors_json) = implementors_json else {
				return Err(context.new_error("is not an array"));
			};
			let mut implementors = Vec::with_capacity_in(implementors_json.len(), &arena);
			for (i, implementor) in implementors_json.iter().enumerate() {
				context.push_path(JsonPathComponent::ArrayIdx(i));
				let JsonValue::Object(implementor) = implementor else {
					return Err(context.new_error("is not an object"));
				};
				// Optional "used_generics" key
				// This cannot use context.parse_used_generics because the
				// traits hashmap hasn't been created yet.
				let used_generics = if let Some(used_generics_json) = implementor.get("used_generics") {
					context.push_path(JsonPathComponent::ObjectKey("used_generics"));
					let JsonValue::Array(used_generics_json) = used_generics_json else {
						return Err(context.new_error("is not an array"));
					};
					let mut used_generics = Vec::with_capacity_in(used_generics_json.len(), &arena);
					for (i, used_generic) in used_generics_json.iter().enumerate() {
						context.push_path(JsonPathComponent::ArrayIdx(i));
						let JsonValue::Object(used_generic) = used_generic else {
							return Err(context.new_error("is not an object"));
						};
						let name = arena.copy_str_into_nt(context.get_str(used_generic, "name")?);
						context.pop_path();

						let constraints = if let Some(json_constraints) = used_generic.get("constraints") {
							context.push_path(JsonPathComponent::ObjectKey("constraints"));
							let JsonValue::Array(json_constraints) = json_constraints else {
								return Err(context.new_error("is not an array"));
							};
							let mut constraints = Vec::with_capacity_in(json_constraints.len(), &arena);

							'outer: for (i, constraint) in json_constraints.iter().enumerate() {
								context.push_path(JsonPathComponent::ArrayIdx(i));
								let Some(constraint) = constraint.as_str() else {
									return Err(context.new_error("is not a string"));
								};
								context.pop_path();
								context.push_path(JsonPathComponent::ArrayKey(constraint));
								for data in traits_storage.iter() {
									// SAFETY: This is a temporary reference and
									// nothing else holds a reference to the inner
									// value at this time
									if unsafe{(&*data.get()).name} == constraint {
										// SAFETY: Pointer from UnsafeCell is always
										// non null
										constraints.push(unsafe{NonNull::new_unchecked(data.get())});
										context.pop_path();
										continue 'outer;
									}
								}
								return Err(context.new_error("is an unknown constraint"));
							}
							context.pop_path();
							&*constraints.leak()
						} else {
							&[]
						};

						used_generics.push(Generic{name, traits: constraints});
						context.pop_path()
					}
					context.pop_path();
					used_generics.leak()
				} else {
					&mut []
				};

				let ty = context.get_key(implementor, "type")?;
				let ty = context.parse_type(ty, used_generics, &arena)?;
				context.pop_path();

				implementors.push(TraitImplementor{generics: used_generics, ty});
				context.pop_path()
			}
			context.pop_path();
			context.pop_path();
			let implementors = implementors.leak();

			let data = traits_storage[i].get();

			// Note(nikhil): It is important that each of these statement
			// dereferences data separately to prevent UB from aliasing
			// mutable references
			//
			// SAFETY: This is a temporary read reference and
			// nothing else holds an exclusive reference to the inner
			// value at this time
			//
			// This implementation relies on iteration order of hashmaps being
			// repeatable. This should be true as long as the order does not
			// change without mutation. This assertion just verifies that.
			assert!(unsafe{&*data}.name == constraint_name);

			// SAFETY: This is the only place where a mutable
			// reference is needed. 
			// A shared reference is put into the storage _after_ this
			// loop iteration, but there is no shared reference at the same time
			unsafe{&mut *data}.implementors = implementors;
			
			let data = unsafe{&*data};
			return Ok((data.name, data));
		}).collect::<Result<HashMap<_, _>>>()?;

		context.pop_path();
		traits
	} else {
		HashMap::new()
	};
	
	let entities = if let Some(entities) = mod_api_root.get("entities") {
		context.push_path(JsonPathComponent::ObjectKey("entities"));
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
			let description = arena.copy_str_into(context.get_str(entity_values, "description")?);
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
					let name = context.get_str(export_fn_values, "name")?;
					context.pop_path();

					context.pop_path();
					context.push_path(JsonPathComponent::ArrayKey(name));

					let name = arena.copy_str_into_nt(name);

					// "description" string
					let description = arena.copy_str_into(context.get_str(export_fn_values, "description")?);
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
		entities
	} else {
		HashMap::new()
	};
	assert_eq!(0, context.json_path.0.len(), "{}", &context.json_path);
	
	// "classes" object
	let classes = if let Some(classes) = mod_api_root.get("classes") {
		context.push_path(JsonPathComponent::ObjectKey("classes"));
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
			let description = arena.copy_str_into(context.get_str(class_values, "description")?);
			context.pop_path();
			
			// optional "used_generics" key
			let mut used_generics = Vec::new_in(&arena);
			if let Some(generics) = class_values.get("used_generics") {
				context.push_path(JsonPathComponent::ObjectKey("used_generics"));
				used_generics.extend(context.parse_used_generics(generics, &traits, &arena)?);
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
					temp.push((method_name, context.parse_host_fn(method_values, generics, &traits, &arena)?));
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
		classes
	} else {
		HashMap::new()
	};
	
	let host_fns = if let Some(host_fns) = mod_api_root.get("host_functions") {
		context.push_path(JsonPathComponent::ObjectKey("host_functions"));
		let JsonValue::Object(host_fns) = host_fns else {
			return Err(context.new_error("is not an object"));
		};
		let host_fns = host_fns.iter().map(|(host_fn_name, host_fn_values)| {
			context.push_path(JsonPathComponent::ObjectKey(host_fn_name));
			let host_fn_name = arena.copy_str_into_nt(host_fn_name);
			let host_fn = context.parse_host_fn(host_fn_values, &[], &traits, &arena)?;
			context.pop_path();
			Ok((host_fn_name, host_fn))
		}).collect::<Result<HashMap<_, _>>>()?;
		context.pop_path();
		host_fns
	} else {
		HashMap::new()
	};
	
	assert_eq!(0, context.json_path.0.len(), "{}", &context.json_path);

	let mut known_types = Vec::with_capacity_in(entities.len() + classes.len(), &arena);
	// Collect all entities as types with no generics
	context.push_path(JsonPathComponent::ObjectKey("entities"));
	for entity_name in entities.keys() {
		// Dont need to check for duplicates yet because there can't be any
		known_types.push((entity_name.as_str(), &[][..]));
	}
	context.pop_path();

	context.push_path(JsonPathComponent::ObjectKey("classes"));
	// Collect all classes and the number of generics they declare
	for (class_name, class_data) in classes.iter() {
		context.push_path(JsonPathComponent::ObjectKey(class_name));
		if known_types.iter().find(|(type_name, _)| *type_name == class_name.as_str()).is_some() {
			return Err(context.new_error("class name already exists"));
		}
		known_types.push((class_name.as_str(), class_data.generics));
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
			context.validate_function(host_fn.parameters, host_fn.return_ty, host_fn.generics, known_types)?;
			context.pop_path();
		}
		context.pop_path();
		context.pop_path();
	}
	context.pop_path();
	
	// Check every implementor of every constraint to make sure the number of
	// generics they use is correct
	for (trait_name, tr) in traits.iter() {
		context.push_path(JsonPathComponent::ObjectKey(trait_name));
		for (i, imp) in tr.implementors.iter().enumerate() {
			context.push_path(JsonPathComponent::ArrayIdx(i));
			context.validate_type(imp.ty, imp.generics, known_types)?;
			context.pop_path();
		}
		context.pop_path();
	}

	context.push_path(JsonPathComponent::ObjectKey("host_functions"));
	// Check every type within each host function and make sure the number of
	// generics they use is correct
	for (fn_name, host_fn) in host_fns.iter() {
		context.push_path(JsonPathComponent::ObjectKey(fn_name));
		context.validate_function(host_fn.parameters, host_fn.return_ty, host_fn.generics, known_types)?;
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
			context.validate_function(export_fn.parameters, Type::Void, &[], known_types)?;
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
