use crate::types::*;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct ModApi {
	entities: HashMap<Arc<str>, ModApiEntity>,
	game_functions: HashMap<Arc<str>, ModApiGameFn>,
}

impl ModApi {
	pub fn entities(&self) -> &HashMap<Arc<str>, ModApiEntity> {
		&self.entities
	}
	pub fn game_functions(&self) -> &HashMap<Arc<str>, ModApiGameFn> {
		&self.game_functions
	}
}

#[derive(Debug)]
pub struct ModApiEntity {
	pub(super) name: Arc<str>,
	pub(super) description: Option<String>,
	pub(super) on_fns: Vec<(Arc<str>, ModApiOnFn)>,
}

impl ModApiEntity {
	pub fn get_on_fn(&self, name: &str) -> Option<(usize, &ModApiOnFn)> {
		self.on_fns.iter().enumerate().find_map(|(i, (fn_name, func))| (name == &**fn_name).then(|| (i, func)))
	}
}

#[derive(Debug)]
pub struct ModApiOnFn {
	pub(super) name: Arc<str>, 
	pub(super) description: Option<String>,
	pub(super) arguments: Vec<Argument>,
}

#[derive(Debug)]
pub struct ModApiGameFn {
	pub(super) name: Arc<str>,
	pub(super) description: Option<String>,
	pub(super) return_ty: GrugType,
	pub(super) arguments: Vec<Argument>,
}

// TODO: Add Display impl for all variants
#[derive(Debug)]
pub enum ModApiError{
	JsonError(json::Error),
	EntitiesNotObject,
	OnFunctionsNotObject{
		entity_name: String,
	},
	OnFnsArgumentsNotArray{
		entity_name: String,
		on_fn_name: String,
	},
	OnFnArgumentMissingName{
		entity_name: String,
		on_fn_name: String,
	},
	OnFnArgumentMissingType{
		entity_name: String,
		on_fn_name: String,
	},
	OnFnArgumentResource {
		entity_name: String,
		on_fn_name: String,
		argument_name: String,
	},
	OnFnArgumentEntity {
		entity_name: String,
		on_fn_name: String,
		argument_name: String,
	},
	GameFnsNotObject,
	OnFnArgumentVoid {
		entity_name: String,
		on_fn_name: String,
		argument_name: String,
	},
	GameFnArgumentsNotArray{
		game_fn_name: String,
	},
	GameFnArgumentMissingName{
		game_fn_name: String,
	},
	GameFnArgumentMissingType{
		game_fn_name: String,
		argument_name: String,
	},
	GameFnArgumentVoid{
		game_fn_name: String,
		argument_name: String,
	},
	GameFnResourceMissingExtension{
		game_fn_name: String,
		argument_name: String,
	},
	GameFnEntityMissingType{
		game_fn_name: String,
		argument_name: String,
	},
	GameFnReturnsEntity{
		game_fn_name: String,
	},
	GameFnReturnsResource{
		game_fn_name: String,
	},
	GameFnNotProvided {
		game_fn_name: String,
	}
}

impl From<json::Error> for ModApiError {
	fn from(other: json::Error) -> ModApiError {
		ModApiError::JsonError(other)
	}
}

pub fn get_mod_api(mod_api_text: &str) -> Result<ModApi, ModApiError> {
	let mod_api_json = json::parse(mod_api_text)?;
	// "entities" object
	let entities = &mod_api_json["entities"];
	if !entities.is_object() {
		return Err(ModApiError::EntitiesNotObject);
	}

	let entities = entities.entries().map(|(entity_name, entity_values)| {
		// optional "description" string
		let description = entity_values["description"].as_str().map(str::to_string);

		// optional "on_fns" object
		let on_fns = &entity_values["on_functions"];
		if !on_fns.is_object() && !on_fns.is_null() {
			return Err(ModApiError::OnFunctionsNotObject{
				entity_name: entity_name.to_string(),
			});
		}
		let on_fns = on_fns.entries().map(|(fn_name, fn_values)| {
			// optional "description" string
			let description = fn_values["description"].as_str().map(str::to_string);
			
			// optional "arguments" object
			let arguments = &fn_values["arguments"];
			if !arguments.is_array() && !arguments.is_null(){
				return Err(ModApiError::OnFnsArgumentsNotArray{
					entity_name: entity_name.to_string(),
					on_fn_name: fn_name.to_string(),
				});
			}
			let arguments = arguments.members().map(|argument_values| {
				// optional "name" string
				let argument_name = argument_values["name"].as_str().ok_or(ModApiError::OnFnArgumentMissingName{
					entity_name: entity_name.to_string(),
					on_fn_name: fn_name.to_string(),
				})?;
				// "type" string
				let ty = argument_values["type"].as_str().ok_or(ModApiError::OnFnArgumentMissingType{
					entity_name: entity_name.to_string(),
					on_fn_name: fn_name.to_string(),
				})?;
				let ty = match ty {
					// arguments can't be void
					"void"     => Err(ModApiError::OnFnArgumentVoid{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?,
					"bool"     => GrugType::Bool,
					"number"   => GrugType::Number,
					"string"   => GrugType::String,
					"id"       => GrugType::Id{custom_name: None},
					"resource" => Err(ModApiError::OnFnArgumentResource{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?,
					"entity" => Err(ModApiError::OnFnArgumentEntity{
						entity_name: entity_name.to_string(),
						on_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?,
					type_name => {
						let extra_value = type_name.into();
						GrugType::Id {
							custom_name: Some(extra_value),
						}
					}
				};
				Ok(Argument{
					name: argument_name.into(),
					ty,
				})
			}).collect::<Result<Vec<_>, ModApiError>>()?;
			let fn_name = Arc::from(fn_name);
			Ok((Arc::clone(&fn_name), ModApiOnFn{
				name: fn_name,
				description,
				arguments,
			}))
		}).collect::<Result<Vec<_>, _>>()?;
		let entity_name = Arc::from(entity_name);
		Ok((Arc::clone(&entity_name), ModApiEntity{
			name: entity_name,
			description,
			on_fns
		}))
	}).collect::<Result<HashMap<_, _>, ModApiError>>()?;
	
	// "game_functions" object
	let game_functions = &mod_api_json["game_functions"];
	if !game_functions.is_object() {
		return Err(ModApiError::GameFnsNotObject);
	}

	let game_functions = game_functions.entries().map(|(fn_name, game_fn_values)| {
		// optional "description" string
		let description = game_fn_values["description"].as_str().map(str::to_string);

		// optional "arguments" object
		let arguments = &game_fn_values["arguments"];
		if !arguments.is_array() && !arguments.is_null(){
			return Err(ModApiError::GameFnArgumentsNotArray{
				game_fn_name: fn_name.to_string(),
			});
		}
		let arguments = arguments.members().map(|argument_values| {
			// "name" string
			let argument_name = argument_values["name"].as_str().ok_or(ModApiError::GameFnArgumentMissingName{
				game_fn_name: fn_name.to_string(),
			})?;
			// "type" string
			let ty = argument_values["type"].as_str().ok_or(ModApiError::GameFnArgumentMissingType{
				game_fn_name: fn_name.to_string(),
				argument_name: argument_name.to_string(),
			})?;
			let ty = match ty {
				// arguments can't be void
				"void"     => Err(ModApiError::GameFnArgumentVoid{
					game_fn_name: fn_name.to_string(),
					argument_name: argument_name.to_string(),
				})?,
				"bool"     => GrugType::Bool,
				"number"      => GrugType::Number,
				"string"   => GrugType::String,
				"id"       => GrugType::Id{custom_name: None},
				"entity"   => {
					let entity_type: Arc<str> = argument_values["entity_type"].as_str().ok_or(ModApiError::GameFnEntityMissingType{
						game_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?.into();
					GrugType::Entity {
						ty: (&*entity_type != "").then(|| entity_type)
					}
				},
				"resource" => {
					let extension = argument_values["resource_extension"].as_str().ok_or(ModApiError::GameFnResourceMissingExtension{
						game_fn_name: fn_name.to_string(),
						argument_name: argument_name.to_string(),
					})?.into();
					GrugType::Resource {
						extension
					}
				}
				type_name => {
					let extra_value = type_name.into();
					GrugType::Id {
						custom_name: Some(extra_value),
					}
				}
			};
			Ok(Argument{
				name: argument_name.into(),
				ty,
			})
		}).collect::<Result<Vec<_>, ModApiError>>()?;

		// optional "return_type" string
		let return_ty = game_fn_values["return_type"].as_str().unwrap_or("void");
		let return_ty = match return_ty {
			"void"     => GrugType::Void,
			"bool"     => GrugType::Bool,
			"number"      => GrugType::Number,
			"string"   => GrugType::String,
			"id"       => GrugType::Id{custom_name: None},
			"entity" => Err(ModApiError::GameFnReturnsEntity{
				game_fn_name: fn_name.to_string(),
			})?,
			"resource" => Err(ModApiError::GameFnReturnsResource{
				game_fn_name: fn_name.to_string(),
			})?,
			type_name => {
				let extra_value = type_name.into();
				GrugType::Id {
					custom_name: Some(extra_value),
				}
			}
		};
		let fn_name = Arc::from(fn_name);
		Ok((Arc::clone(&fn_name), ModApiGameFn{
			name: fn_name,
			return_ty,
			description,
			arguments
		}))
	}).collect::<Result<HashMap<_, _>, ModApiError>>()?;

	Ok(ModApi{
		entities,
		game_functions
	})
}

// pub static MOD_API: OnceLock<ModApi> = std::sync::OnceLock::new();
