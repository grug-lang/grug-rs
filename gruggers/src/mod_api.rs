use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::io::Write;
use std::path::Path;
use std::ptr::NonNull;

use crate::arena::Arena;
use crate::ast::{Parameter, Type};
use crate::error::{Error, ErrorKind, Result, SourceSpan};
use crate::frontend::type_propagation::StackLL;
use crate::ntstring::NTStr;
use crate::types::{ErasedHostFn, ErasedRegFn};

use allocator_api2::vec::Vec;

use json::JsonValue;
use json::object::Object;

// the 'static fields within `ModApi` are allocated within `_arena`. Any
// reference to them must have a 'self lifetime
pub(crate) struct ModApi {
    classes: HashMap<&'static NTStr, ModApiClass<'static>>,
    host_fns: HashMap<&'static NTStr, ModApiHostFn<'static>>,
    _arena: Arena,
    // Holds temporary strings, like the names in registration errors
    temp_arena: Arena,
}

// We don't have a public api that uses the `_arena` field which is the only !Send
// field
unsafe impl Send for ModApi {}
// We don't have a public api that uses the `_arena` field which is the only !Sync
// field
unsafe impl Sync for ModApi {}

impl ModApi {
    pub(crate) fn classes<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiClass<'a>> {
        // SAFETY: Invariance of the methods field requires this transmute
        // This transmute brings it back to the actual lifetime
        unsafe {
            std::mem::transmute::<
                &'a HashMap<&'static NTStr, ModApiClass<'static>>,
                &'a HashMap<&'a NTStr, ModApiClass<'a>>,
            >(&self.classes)
        }
    }

    pub(crate) fn host_fns<'a>(&'a self) -> &'a HashMap<&'a NTStr, ModApiHostFn<'a>> {
        &self.host_fns
    }

    fn new_reg_error(args: std::fmt::Arguments) -> Error {
        Error::new(
            ErrorKind::FUNCTION_REGISTRATION_ERROR,
            "",
            "".as_ref(),
            "",
            SourceSpan { offset: 0, line: 0 },
            args,
        )
    }

    pub(crate) fn register_host_fn(
        &mut self,
        class_name: Option<&str>,
        fn_name: &str,
        f: ErasedHostFn,
        gen_count: Option<usize>,
    ) -> Result<()> {
        let (host_fn, err_name) = self.find_host_fn_to_register(class_name, fn_name, gen_count)?;
        match &mut host_fn.fn_ptr {
            Some(_) => Err(Self::new_reg_error(format_args!(
                "{} has already been registered",
                err_name
            ))),
            ptr @ None => {
                *ptr = Some(f);
                Ok(())
            }
        }
    }

    pub(crate) fn register_reg_fn(
        &mut self,
        class_name: Option<&str>,
        fn_name: &str,
        f: ErasedRegFn,
        gen_count: Option<usize>,
    ) -> Result<()> {
        let (host_fn, err_name) = self.find_host_fn_to_register(class_name, fn_name, gen_count)?;
        match &mut host_fn.reg_fn {
            Some(_) => Err(Self::new_reg_error(format_args!(
                "{} has already been registered",
                err_name
            ))),
            ptr @ None => {
                *ptr = Some(f);
                Ok(())
            }
        }
    }

    // Finds the host function or method that a registration is for, along
    // with the name that registration errors should call it by
    fn find_host_fn_to_register(
        &mut self,
        class_name: Option<&str>,
        fn_name: &str,
        gen_count: Option<usize>,
    ) -> Result<(&mut ModApiHostFn<'static>, &str)> {
        self.temp_arena.clear();
        let ModApi {
            classes,
            host_fns,
            temp_arena,
            ..
        } = self;
        let (host_fn, err_name) = if let Some(class_name) = class_name {
            let Some(class) = classes.get_mut(class_name) else {
                return Err(Self::new_reg_error(format_args!(
                    "class or entity {} was not declared in the mod_api",
                    class_name
                )));
            };
            let class_is_entity = class.is_entity();
            let Some((_, host_fn)) = class
                .assoc_fns
                .iter_mut()
                .find(|(name, _)| name.as_str() == fn_name)
            else {
                return Err(Self::new_reg_error(format_args!(
                    "method {}.{} was not declared in the mod_api",
                    class_name, fn_name
                )));
            };

            let kind = if host_fn.is_static {
                "static method"
            } else if class_is_entity {
                "entity method"
            } else {
                "method"
            };
            (
                host_fn,
                temp_arena.fmt_into(format_args!("{} {}.{}", kind, class_name, fn_name)),
            )
        } else {
            let Some(host_fn) = host_fns.get_mut(fn_name) else {
                return Err(Self::new_reg_error(format_args!(
                    "host function {} was not declared in the mod_api",
                    fn_name
                )));
            };
            (
                host_fn,
                temp_arena.fmt_into(format_args!("host function {}", fn_name)),
            )
        };

        if let Some(gen_count) = gen_count
            && host_fn.generics.len() != gen_count
        {
            return Err(Self::new_reg_error(format_args!(
                "{} was not declared in the mod_api",
                err_name
            )));
        }
        Ok((host_fn, err_name))
    }

    pub(crate) unsafe fn register_dummies(&mut self) {
        use crate::types::Value;
        extern "C" fn dummy_host_fn(
            _state: *const c_void,
            _arguments: *const Value,
            _generics: *const Type,
        ) -> Value {
            Value { void: () }
        }
        for host_fn in self.host_fns.values_mut() {
            host_fn.fn_ptr = const { Some(dummy_host_fn) };
        }
        for class in self.classes.values_mut() {
            for (_, host_fn) in &mut *class.assoc_fns {
                host_fn.fn_ptr = const { Some(dummy_host_fn) };
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TraitImplementor<'a> {
    pub(crate) generics: &'a [Generic<'a>],
    pub(crate) ty: Type<'a>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Trait<'a> {
    pub(crate) name: &'a str,
    pub(crate) implementors: &'a [TraitImplementor<'a>],
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
        unsafe { std::mem::transmute::<&[NonNull<Trait<'a>>], &[&'a Trait<'a>]>(self.traits) }
    }
}

#[derive(Debug)]
pub(crate) struct ModApiClass<'a> {
    #[expect(dead_code)]
    pub(crate) description: &'a str,
    pub(crate) ty: Type<'a>,
    pub(crate) export_fns: Option<&'a mut [(&'a NTStr, ModApiExportFn<'a>)]>,
    pub(crate) assoc_fns: &'a mut [(&'a NTStr, ModApiHostFn<'a>)],
    pub(crate) generics: &'a [Generic<'a>],
}

impl<'a> ModApiClass<'a> {
    pub const fn is_entity(&self) -> bool {
        self.export_fns.is_some()
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
    pub(crate) is_static: bool,
    pub(crate) generics: &'a [Generic<'a>],
    pub(crate) parameters: &'a [Parameter<'a>],
    pub(crate) return_ty: Type<'a>,
    pub(crate) reg_fn: Option<ErasedRegFn>,
    pub(crate) fn_ptr: Option<ErasedHostFn>,
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

    fn push_path(&mut self, value: JsonPathComponent<'a>) {
        self.json_path.0.push(value)
    }
    fn pop_path(&mut self) {
        self.json_path.0.pop().unwrap();
    }

    fn new_error(&self, message: &str) -> Error {
        let mut location = Vec::new_in(self.arena);
        write!(location, "{}", self.json_path).expect("writing into a vec can never fail");
        let location = unsafe { std::str::from_utf8_unchecked(location.leak()) };
        Error::new(
            ErrorKind::MOD_API_ERROR,
            location,
            self.path.as_ref(),
            self.text,
            SourceSpan { offset: 0, line: 0 },
            format_args!("{} {}", location, message),
        )
    }

    #[track_caller]
    fn new_fmt_error(&self, message: std::fmt::Arguments) -> Error {
        let mut location = Vec::new_in(self.arena);
        write!(location, "{}", self.json_path).expect("writing into a vec can never fail");
        let location = unsafe { std::str::from_utf8_unchecked(location.leak()) };
        Error::new(
            ErrorKind::MOD_API_ERROR,
            location,
            self.path.as_ref(),
            self.text,
            SourceSpan { offset: 0, line: 0 },
            format_args!("{} {}", location, message),
        )
    }

    // gets the key as a str and pushes the path onto self
    fn get_str<'b>(&mut self, object: &'b Object, key: &'a str) -> Result<&'b str> {
        self.get_key(object, key)?
            .as_str()
            .ok_or_else(|| self.new_error("is not a string"))
    }

    // gets the key and pushes the path onto self
    fn get_key<'b>(&mut self, object: &'b Object, key: &'a str) -> Result<&'b JsonValue> {
        self.push_path(JsonPathComponent::ObjectKey(key));
        object
            .get(key)
            .ok_or_else(|| self.new_error("does not exist"))
    }

    fn parse_type<'b>(
        &mut self,
        object: &'a JsonValue,
        used_generics: &'_ [Generic<'_>],
        arena: &'b Arena,
    ) -> Result<Type<'b>> {
        let JsonValue::Object(object) = object else {
            return Err(self.new_error("is not an object"));
        };
        // "name" string
        let ty = self.get_str(object, "name")?;
        self.pop_path();

        if ty != "resource" && object.get("optional").is_some() {
            self.push_path(JsonPathComponent::ObjectKey("optional"));
            return Err(self.new_error("is only allowed on resource types"));
        }

        let ty = match ty {
            "void" => Type::Void,
            "bool" => Type::Bool,
            "number" => Type::Number,
            "string" => Type::String,
            "entity" => {
                // "entity_type" string
                let entity_type = self.get_str(object, "entity_type")?;
                self.pop_path();
                Type::Entity {
                    entity_type: (!entity_type.is_empty())
                        .then(|| arena.copy_str_into(entity_type)),
                }
            }
            "resource" => {
                // "resource_extension" string
                let extension = self.get_str(object, "resource_extension")?;
                self.pop_path();

                // optional "optional" bool, defaulting to false
                let optional = if let Some(optional) = object.get("optional") {
                    self.push_path(JsonPathComponent::ObjectKey("optional"));
                    let optional = optional
                        .as_bool()
                        .ok_or_else(|| self.new_error("is not a boolean"))?;
                    self.pop_path();
                    optional
                } else {
                    false
                };

                Type::Resource {
					extension: arena.copy_str_into(extension),
                    optional,
                }
            }
            generic if generic.starts_with("$") => {
                for (i, used_generic) in used_generics.iter().enumerate() {
                    if &**used_generic.name == generic {
                        return Ok(Type::Existential { idx: i });
                    }
                }
                self.push_path(JsonPathComponent::ObjectKey("name"));
                return Err(
                    self.new_fmt_error(format_args!("is an undeclared generic (\"{}\")", generic))
                );
            }
            type_name => {
                let name = arena.copy_str_into(type_name);
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
                Type::Id { name, generics }
            }
        };
        Ok(ty)
    }

    fn parse_parameters<'b>(
        &mut self,
        parameters: &'a JsonValue,
        generics: &'_ [Generic<'_>],
        arena: &'b Arena,
    ) -> Result<&'b [Parameter<'b>]> {
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

            let param_name = arena.copy_str_into(param_name);

            // required "type" string
            let ty = self.get_key(param_values, "type")?;
            let ty = self.parse_type(ty, generics, arena)?;
            if ty == Type::Void {
                return Err(self.new_error("cannot be void"));
            }
            self.pop_path();

            self.pop_path();
            temp.push(Parameter {
                name: param_name,
                ty,
                name_span: SourceSpan { offset: 0, line: 0 },
                type_span: SourceSpan { offset: 0, line: 0 },
            });
        }
        Ok(temp.leak())
    }

    fn parse_used_generics<'b>(
        &mut self,
        used_generics_json: &'a JsonValue,
        traits: &HashMap<&str, &'b Trait<'b>>,
        arena: &'b Arena,
    ) -> Result<&'b [Generic<'b>]> {
        let JsonValue::Array(used_generics_json) = used_generics_json else {
            return Err(self.new_error("is not an array"));
        };
        let mut used_generics = Vec::with_capacity_in(used_generics_json.len(), arena);
        for (i, used_generic) in used_generics_json.iter().enumerate() {
            self.push_path(JsonPathComponent::ArrayIdx(i));
            let generic = {
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
                Generic {
                    name,
                    traits: &*constraints,
                }
            };
            used_generics.push(generic);
            self.pop_path(); // used generics idx
        }
        Ok(used_generics.leak())
    }

    fn parse_host_fn<'b, const IS_STATIC: bool>(
        &mut self,
        host_fn_values: &'a JsonValue,
        parent_generics: &'b [Generic<'b>],
        traits: &HashMap<&str, &'b Trait<'b>>,
        arena: &'b Arena,
    ) -> Result<ModApiHostFn<'b>> {
        let JsonValue::Object(host_fn_values) = host_fn_values else {
            return Err(self.new_error("is not an object"));
        };
        // "description" string
        let description = arena.copy_str_into(self.get_str(host_fn_values, "description")?);
        self.pop_path();

        // optional "used_generics" key
        let generics = if let Some(generics) = host_fn_values.get("used_generics") {
            self.push_path(JsonPathComponent::ObjectKey("used_generics"));
            let used_generics = self.parse_used_generics(generics, traits, arena)?;
            self.pop_path();
            used_generics
        } else {
            parent_generics
        };

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
                Type::Entity { .. } => return Err(self.new_error("cannot be entity")),
                Type::Resource { .. } => return Err(self.new_error("cannot be resource")),
                _ => (),
            }
            self.pop_path();
            return_ty
        } else {
            Type::Void
        };

        Ok(ModApiHostFn {
            is_static: IS_STATIC,
            description,
            return_ty,
            generics,
            parameters,
            reg_fn: None,
            fn_ptr: None,
        })
    }

    fn validate_function(
        &mut self,
        parameters: &[Parameter<'a>],
        return_type: Type<'a>,
        used_generics: &[Generic],
        known_types: &[(&str, &[Generic])],
    ) -> Result<()> {
        self.push_path(JsonPathComponent::ObjectKey("parameters"));
        for parameter in parameters {
            self.push_path(JsonPathComponent::ArrayKey(parameter.name));
            self.validate_type(parameter.ty, used_generics, known_types)?;
            self.pop_path();
        }
        self.pop_path();
        self.push_path(JsonPathComponent::ObjectKey("return_type"));
        self.validate_type(return_type, used_generics, known_types)?;
        self.pop_path();
        Ok(())
    }

    fn validate_type(
        &mut self,
        ty: Type<'a>,
        used_generics: &[Generic],
        known_types: &[(&str, &[Generic])],
    ) -> Result<()> {
        // `checking` holds the (type, constraint) pairs whose check is in progress
        fn implements_trait<'b>(
            actual: (Type<'b>, &'b [Generic<'b>]),
            tr: &'b Trait<'b>,
            checking: Option<&StackLL<(Type<'b>, &'b Trait<'b>)>>,
        ) -> bool {
            // A bare generic implementor can make a check depend on itself,
            // and that path can't prove that the type implements the constraint
            if checking.is_some_and(|checking| {
                checking.any(|&(checked_ty, checked_tr)| {
                    checked_ty == actual.0 && std::ptr::eq(checked_tr, tr)
                })
            }) {
                return false;
            }
            let checking = StackLL {
                current: (actual.0, tr),
                parent: checking,
            };
            tr.implementors
                .iter()
                .any(|imp| type_matches_implementor(actual, (imp.ty, imp.generics), &checking))
        }
        // Checks if a particular types matches a particular trait
        fn type_matches_implementor<'b>(
            actual: (Type<'b>, &'b [Generic<'b>]),
            imp: (Type<'b>, &'b [Generic<'b>]),
            checking: &StackLL<(Type<'b>, &'b Trait<'b>)>,
        ) -> bool {
            match (actual.0, imp.0) {
                (
                    Type::Id {
                        name: ty_name,
                        generics: act_generic_types,
                    },
                    Type::Id {
                        name: imp_name,
                        generics: imp_generic_types,
                    },
                ) if ty_name == imp_name => {
                    act_generic_types
                        .iter()
                        .zip(imp_generic_types)
                        .all(|(ty, imp_gen_type)| {
                            type_matches_implementor(
                                (*ty, actual.1),
                                (*imp_gen_type, imp.1),
                                checking,
                            )
                        })
                }
                (Type::Existential { idx: act_idx }, Type::Existential { idx: imp_idx }) => {
                    let act_traits = actual.1[act_idx].traits();
                    let imp_traits = imp.1[imp_idx].traits();
                    // make sure the actual existential has at least the
                    // traits expected by the implementor
                    imp_traits.iter().all(|&imp_trait| {
                        act_traits
                            .iter()
                            .any(|&act_trait| std::ptr::eq(imp_trait, act_trait))
                    })
                }
                (ty, Type::Existential { idx }) => imp
                    .1
                    .get(idx)
                    .expect("existential should always point to a valid generic")
                    .traits()
                    .iter()
                    .all(|tr| implements_trait((ty, actual.1), tr, Some(checking))),
                (Type::Void, Type::Void)
                | (Type::Bool, Type::Bool)
                | (Type::Number, Type::Number)
                | (Type::String, Type::String) => true,
                // Resource and entity strings end up here, since no implementor
                // can be one, even when they are nested in a generic type
                _ => false,
            }
        }
        if let Type::Id { name, generics } = ty {
            // If the type is found, then check if the number of generics
            // match and also recursively check types
            self.push_path(JsonPathComponent::ObjectKey("generics"));
            if let Some((_, constraints)) = known_types
                .iter()
                .find(|(ty_name, _)| *ty_name == name)
            {
                if constraints.len() != generics.len() {
                    return Err(self.new_error(self.arena.fmt_into(format_args!(
                        ": {} was declared to have {} generics but here it has {}",
                        name,
                        constraints.len(),
                        generics.len()
                    ))));
                }
                for (i, (generic, constraint)) in generics.iter().zip(*constraints).enumerate() {
                    self.push_path(JsonPathComponent::ArrayIdx(i));
                    match *generic {
                        Type::Resource { .. } => {
                            return Err(
                                self.new_error("resource strings cannot be used in generics")
                            );
                        }
                        Type::Entity { .. } => {
                            return Err(self.new_error("entity strings cannot be used in generics"));
                        }
                        _ => {
                            for tr in constraint.traits() {
                                // If the type is already an existential,
                                // then check if it already implements the
                                // constraint. If not, then go for the
                                // other check.
                                let is_existential_that_implements_constraint =
                                    if let Type::Existential { idx } = generic {
                                        let act_traits = used_generics[*idx].traits();
                                        act_traits
                                            .iter()
                                            .any(|&act_trait| std::ptr::eq(act_trait, *tr))
                                    } else {
                                        false
                                    };
                                let is_type_that_matches_a_trait_implementor =
                                    implements_trait((*generic, used_generics), tr, None);
                                if !is_existential_that_implements_constraint
                                    && !is_type_that_matches_a_trait_implementor
                                {
                                    if let Type::Existential { idx } = generic {
                                        return Err(self.new_error(self.arena.fmt_into(
                                            format_args!(
                                                "type '{}' must implement constraint '{}'",
                                                used_generics[*idx].name, tr.name
                                            ),
                                        )));
                                    } else {
                                        return Err(self.new_error(self.arena.fmt_into(
                                            format_args!(
                                                "type '{}' must implement constraint '{}'",
                                                generic, tr.name
                                            ),
                                        )));
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
            } else if !generics.is_empty() {
				return Err(self.new_error(self.arena.fmt_into(format_args!(
					": {} was not declared in \"classes\", so it cannot have generics",
					name
				))));
			}
            
            self.pop_path();
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
        for component in &self.0 {
            match component {
                JsonPathComponent::ObjectKey(key) => write!(f, ".{}", key)?,
                JsonPathComponent::ArrayIdx(idx) => write!(f, "[{}]", idx)?,
                JsonPathComponent::ArrayKey(key) => write!(f, "[\"{}\"]", key)?,
            }
        }
        Ok(())
    }
}

pub(crate) fn get_mod_api(mod_api_path: impl AsRef<Path>) -> Result<ModApi> {
    let mod_api_path = mod_api_path.as_ref();
    let mod_api_text = std::fs::read_to_string(mod_api_path).map_err(|err| {
        Error::new(
            ErrorKind::MOD_API_IO_ERROR,
            "",
            mod_api_path.as_ref(),
            "",
            SourceSpan { offset: 0, line: 0 },
            format_args!("IO Error: {}", err),
        )
    })?;
    get_mod_api_from_text(mod_api_path, &mod_api_text)
}

pub(crate) fn get_mod_api_from_text(
    mod_api_path: impl AsRef<Path>,
    mod_api_text: &str,
) -> Result<ModApi> {
    let arena = Arena::new();
    let mod_api_path = mod_api_path.as_ref();

    let mod_api_json = json::parse(mod_api_text).map_err(|err| {
        Error::new(
            ErrorKind::MOD_API_JSON_ERROR,
            "",
            mod_api_path.as_ref(),
            mod_api_text,
            SourceSpan { offset: 0, line: 0 },
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

    let traits = if let Some(constraints) = mod_api_root.get("constraints") {
        context.push_path(JsonPathComponent::ObjectKey("constraints"));
        let JsonValue::Object(constraints) = constraints else {
            return Err(context.new_error("is not an object"));
        };
        let traits_storage = arena.slice_from_iter(constraints.iter().map(|(name, _)| {
            UnsafeCell::new(Trait {
                name: arena.copy_str_into_nt(name),
                implementors: &[],
            })
        }));

        let traits = constraints
            .iter()
            .enumerate()
            .map(|(i, (constraint_name, constraint_values))| {
                context.push_path(JsonPathComponent::ObjectKey(constraint_name));
                let JsonValue::Object(constraint_values) = constraint_values else {
                    return Err(context.new_error("is not an object"));
                };

                // "description" string
                let _description =
                    arena.copy_str_into(context.get_str(constraint_values, "description")?);
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
                    let used_generics = if let Some(used_generics_json) =
                        implementor.get("used_generics")
                    {
                        context.push_path(JsonPathComponent::ObjectKey("used_generics"));
                        let JsonValue::Array(used_generics_json) = used_generics_json else {
                            return Err(context.new_error("is not an array"));
                        };
                        let mut used_generics =
                            Vec::with_capacity_in(used_generics_json.len(), &arena);
                        for (i, used_generic) in used_generics_json.iter().enumerate() {
                            context.push_path(JsonPathComponent::ArrayIdx(i));
                            let JsonValue::Object(used_generic) = used_generic else {
                                return Err(context.new_error("is not an object"));
                            };
                            let name =
                                arena.copy_str_into_nt(context.get_str(used_generic, "name")?);
                            context.pop_path();

                            let constraints = if let Some(json_constraints) =
                                used_generic.get("constraints")
                            {
                                context.push_path(JsonPathComponent::ObjectKey("constraints"));
                                let JsonValue::Array(json_constraints) = json_constraints else {
                                    return Err(context.new_error("is not an array"));
                                };
                                let mut constraints =
                                    Vec::with_capacity_in(json_constraints.len(), &arena);

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
                                        if unsafe { (&*data.get()).name } == constraint {
                                            // SAFETY: Pointer from UnsafeCell is always
                                            // non null
                                            constraints.push(unsafe {
                                                NonNull::new_unchecked(data.get())
                                            });
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

                            used_generics.push(Generic {
                                name,
                                traits: constraints,
                            });
                            context.pop_path()
                        }
                        context.pop_path();
                        used_generics.leak()
                    } else {
                        &mut []
                    };

                    let ty = context.get_key(implementor, "type")?;
                    let ty = context.parse_type(ty, used_generics, &arena)?;
                    match ty {
                        Type::Resource { .. } => {
                            return Err(context.new_error("cannot be a resource string"));
                        }
                        Type::Entity { .. } => {
                            return Err(context.new_error("cannot be an entity string"));
                        }
                        _ => (),
                    }
                    context.pop_path();

                    implementors.push(TraitImplementor {
                        generics: used_generics,
                        ty,
                    });
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
                assert!(unsafe { &*data }.name == constraint_name);

                // SAFETY: This is the only place where a mutable
                // reference is needed.
                // A shared reference is put into the storage _after_ this
                // loop iteration, but there is no shared reference at the same time
                unsafe { &mut *data }.implementors = implementors;

                let data = unsafe { &*data };
                Ok((data.name, data))
            })
            .collect::<Result<HashMap<_, _>>>()?;

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

        let entities = entities
            .iter()
            .map(|(entity_name, entity_values)| {
                context.push_path(JsonPathComponent::ObjectKey(entity_name));
                let entity_name = arena.copy_str_into_nt(entity_name);
                let JsonValue::Object(entity_values) = entity_values else {
                    return Err(context.new_error("is not an object"));
                };

                // "description" string
                let description =
                    arena.copy_str_into(context.get_str(entity_values, "description")?);
                context.pop_path();

                let ty = Type::Id {
                    name: entity_name.as_str(),
                    generics: &[],
                };

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
                        let description =
                            arena.copy_str_into(context.get_str(export_fn_values, "description")?);
                        context.pop_path();

                        // optional "parameters" array
                        let parameters = if let Some(parameters) =
                            export_fn_values.get("parameters")
                        {
                            context.push_path(JsonPathComponent::ObjectKey("parameters"));
                            let parameters = context.parse_parameters(parameters, &[], &arena)?;
                            context.pop_path();
                            parameters
                        } else {
                            &[]
                        };

                        context.pop_path();
                        temp.push((
                            name,
                            ModApiExportFn {
                                description,
                                parameters,
                            },
                        ))
                    }

                    context.pop_path();
                    temp.leak()
                } else {
                    &mut []
                };

                let mut assoc_fns = Vec::new_in(&arena);
                // optional "methods" object
                if let Some(methods) = entity_values.get("methods") {
                    context.push_path(JsonPathComponent::ObjectKey("methods"));
                    let JsonValue::Object(methods) = methods else {
                        return Err(context.new_error("is not an object"));
                    };
                    for (method_name, method_values) in methods.iter() {
                        context.push_path(JsonPathComponent::ObjectKey(method_name));
                        let method_name = arena.copy_str_into_nt(method_name);
                        assoc_fns.push((
                            method_name,
                            context.parse_host_fn::<false>(method_values, &[], &traits, &arena)?,
                        ));

                        context.pop_path();
                    }
                    context.pop_path();
                }

                // TODO(nikhil): Should this be gated behind a feature flag?
                // optional "static_methods" object
                if let Some(static_methods) = entity_values.get("static_methods") {
                    context.push_path(JsonPathComponent::ObjectKey("static_methods"));
                    let JsonValue::Object(static_methods) = static_methods else {
                        return Err(context.new_error("is not an object"));
                    };
                    for (static_method_name, static_method_values) in static_methods.iter() {
                        context.push_path(JsonPathComponent::ObjectKey(static_method_name));
                        let static_method_name = arena.copy_str_into_nt(static_method_name);
                        // A static method can't share a name with a method, since
                        // registering either one only names the type and the
                        // function, which then wouldn't say which of the two it is
                        if assoc_fns
                            .iter()
                            .any(|(method_name, _)| *method_name == static_method_name)
                        {
                            return Err(context.new_error("is already declared as a method"));
                        }
                        assoc_fns.push((
                            static_method_name,
                            context.parse_host_fn::<true>(
                                static_method_values,
                                &[],
                                &traits,
                                &arena,
                            )?,
                        ));
                        context.pop_path();
                    }
                    context.pop_path();
                }
                let assoc_fns = assoc_fns.leak();

                context.pop_path();
                Ok((
                    entity_name,
                    ModApiClass {
                        description,
                        ty,
                        generics: &[],
                        export_fns: Some(export_fns),
                        assoc_fns,
                    },
                ))
            })
            .collect::<Result<HashMap<_, _>>>()?;
        context.pop_path();
        entities
    } else {
        HashMap::new()
    };
    assert_eq!(0, context.json_path.0.len(), "{}", context.json_path);

    // "classes" object
    let mut classes = if let Some(classes) = mod_api_root.get("classes") {
        context.push_path(JsonPathComponent::ObjectKey("classes"));
        let JsonValue::Object(classes) = classes else {
            return Err(context.new_error("is not an object"));
        };
        let classes = classes
            .iter()
            .map(|(class_name, class_values)| {
                context.push_path(JsonPathComponent::ObjectKey(class_name));
                if entities.contains_key(class_name) {
                    return Err(context.new_error("has already been declared as an entity"));
                }
                let class_name = arena.copy_str_into_nt(class_name);
                let JsonValue::Object(class_values) = class_values else {
                    return Err(context.new_error("is not an object"));
                };
                // "description" string
                let description =
                    arena.copy_str_into(context.get_str(class_values, "description")?);
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
                    name: class_name.as_str(),
                    generics: arena.slice_from_iter(
                        (0..(generics.len())).map(|i| Type::Existential { idx: i }),
                    ),
                };

                // optional "methods" object
                let mut assoc_fns = Vec::new_in(&arena);
                if let Some(methods) = class_values.get("methods") {
                    context.push_path(JsonPathComponent::ObjectKey("methods"));
                    let JsonValue::Object(methods) = methods else {
                        return Err(context.new_error("is not an object"));
                    };
                    for (method_name, method_values) in methods.iter() {
                        context.push_path(JsonPathComponent::ObjectKey(method_name));
                        let method_name = arena.copy_str_into_nt(method_name);
                        assoc_fns.push((
                            method_name,
                            context.parse_host_fn::<false>(
                                method_values,
                                generics,
                                &traits,
                                &arena,
                            )?,
                        ));
                        context.pop_path();
                    }
                    context.pop_path();
                };

                // optional "static_methods" object
                if let Some(static_methods) = class_values.get("static_methods") {
                    context.push_path(JsonPathComponent::ObjectKey("static_methods"));
                    let JsonValue::Object(static_methods) = static_methods else {
                        return Err(context.new_error("is not an object"));
                    };
                    for (static_method_name, static_method_values) in static_methods.iter() {
                        context.push_path(JsonPathComponent::ObjectKey(static_method_name));
                        let static_method_name = arena.copy_str_into_nt(static_method_name);
                        // A static method can't share a name with a method, since
                        // registering either one only names the type and the
                        // function, which then wouldn't say which of the two it is
                        if assoc_fns
                            .iter()
                            .any(|(method_name, _)| *method_name == static_method_name)
                        {
                            return Err(context.new_error("is already declared as a method"));
                        }
                        assoc_fns.push((
                            static_method_name,
                            context.parse_host_fn::<true>(
                                static_method_values,
                                generics,
                                &traits,
                                &arena,
                            )?,
                        ));
                        context.pop_path();
                    }
                    context.pop_path();
                };
                let assoc_fns = assoc_fns.leak();

                context.pop_path();
                Ok((
                    class_name,
                    ModApiClass {
                        description,
                        ty,
                        generics,
                        export_fns: None,
                        assoc_fns,
                    },
                ))
            })
            .collect::<Result<HashMap<_, _>>>()?;
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
        let host_fns = host_fns
            .iter()
            .map(|(host_fn_name, host_fn_values)| {
                context.push_path(JsonPathComponent::ObjectKey(host_fn_name));
                let host_fn_name = arena.copy_str_into_nt(host_fn_name);
                let host_fn =
                    context.parse_host_fn::<true>(host_fn_values, &[], &traits, &arena)?;
                context.pop_path();
                Ok((host_fn_name, host_fn))
            })
            .collect::<Result<HashMap<_, _>>>()?;
        context.pop_path();
        host_fns
    } else {
        HashMap::new()
    };

    assert_eq!(0, context.json_path.0.len(), "{}", context.json_path);

    classes.extend(entities);

    let mut known_types = Vec::with_capacity_in(classes.len(), &arena);

    // Collect all classes and the number of generics they declare. The names
    // are unique, since a class can't share its name with an entity
    for (class_name, class_data) in classes.iter() {
        known_types.push((class_name.as_str(), class_data.generics));
    }

    let known_types = known_types.leak();
    // Check every type within each class and its methods to make sure the
    // number of generics they use is correct
    for (class_name, class_data) in classes.iter() {
        if let Some(export_fns) = &class_data.export_fns {
            context.push_path(JsonPathComponent::ObjectKey("entities"));
            context.push_path(JsonPathComponent::ObjectKey(class_name));
            // Validate the export functions of the entities
            for (fn_name, export_fn) in export_fns.iter() {
                context.push_path(JsonPathComponent::ObjectKey(fn_name));
                context.validate_function(export_fn.parameters, Type::Void, &[], known_types)?;
                context.pop_path();
            }
        } else {
            context.push_path(JsonPathComponent::ObjectKey("classes"));
            context.push_path(JsonPathComponent::ObjectKey(class_name));
        }

        for (method_name, host_fn) in &*class_data.assoc_fns {
            if host_fn.is_static {
                context.push_path(JsonPathComponent::ObjectKey("static_methods"));
            } else {
                context.push_path(JsonPathComponent::ObjectKey("methods"));
            }
            context.push_path(JsonPathComponent::ObjectKey(method_name));
            context.validate_function(
                host_fn.parameters,
                host_fn.return_ty,
                host_fn.generics,
                known_types,
            )?;
            context.pop_path();
            context.pop_path();
        }
        context.pop_path();
        context.pop_path();
    }

    // Check every implementor of every constraint to make sure the number of
    // generics they use is correct
    context.push_path(JsonPathComponent::ObjectKey("constraints"));
    for (trait_name, tr) in traits.iter() {
        context.push_path(JsonPathComponent::ObjectKey(trait_name));
        for (i, imp) in tr.implementors.iter().enumerate() {
            context.push_path(JsonPathComponent::ArrayIdx(i));
            context.validate_type(imp.ty, imp.generics, known_types)?;
            context.pop_path();
        }
        context.pop_path();
    }
    context.pop_path();

    context.push_path(JsonPathComponent::ObjectKey("host_functions"));
    // Check every type within each host function and make sure the number of
    // generics they use is correct
    for (fn_name, host_fn) in host_fns.iter() {
        context.push_path(JsonPathComponent::ObjectKey(fn_name));
        context.validate_function(
            host_fn.parameters,
            host_fn.return_ty,
            host_fn.generics,
            known_types,
        )?;
        context.pop_path();
    }
    context.pop_path();
    assert_eq!(0, context.json_path.0.len(), "{}", context.json_path);
    drop(context);

    Ok(ModApi {
        classes: unsafe {
            std::mem::transmute::<
                HashMap<&'_ NTStr, ModApiClass<'_>>,
                HashMap<&'static NTStr, ModApiClass<'static>>,
            >(classes)
        },
        host_fns: unsafe {
            std::mem::transmute::<
                HashMap<&'_ NTStr, ModApiHostFn<'_>>,
                HashMap<&'static NTStr, ModApiHostFn<'static>>,
            >(host_fns)
        },
        _arena: arena,
        temp_arena: Arena::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Value;

    // Builds a mod_api.json from the bodies of its root objects
    fn mod_api_json(classes: &str, entities: &str, host_functions: &str) -> String {
        format!(
            r#"{{"classes": {{{classes}}}, "entities": {{{entities}}}, "host_functions": {{{host_functions}}}}}"#
        )
    }

    fn parse(classes: &str, entities: &str, host_functions: &str) -> ModApi {
        get_mod_api_from_text("test", &mod_api_json(classes, entities, host_functions)).unwrap()
    }

    fn parse_error(classes: &str, entities: &str, host_functions: &str) -> String {
        let err = get_mod_api_from_text("test", &mod_api_json(classes, entities, host_functions))
            .err()
            .expect("expected the mod_api to be rejected");
        err.inner().error_message.to_str().to_owned()
    }

    #[test]
    fn mod_api_test_ok() {
        parse("", "", "");
    }

    #[test]
    fn mod_api_test_err_1() {
        assert_eq!(
            parse_error(r#""Test": 42"#, "", ""),
            "root.classes.Test is not an object"
        );
    }

    #[test]
    fn missing_mod_api_file_is_an_io_error() {
        let err = get_mod_api("this/mod_api.json/does/not/exist")
            .err()
            .expect("expected reading the mod_api to fail");
        assert!(err.inner().error_message.to_str().starts_with("IO Error: "));
    }

    // TODO: Move to grug-tests once it's decided how mod_api.json treats type
    // names it doesn't declare, since grug-for-python reads "void" as one
    #[test]
    fn host_fn_parameter_cannot_be_void() {
        assert_eq!(
            parse_error(
                "",
                "",
                r#""f": {"description": "Foo", "parameters": [{"name": "x", "type": {"name": "void"}}]}"#
            ),
            "root.host_functions.f.parameters[\"x\"].type cannot be void"
        );
    }

    unsafe extern "C" fn dummy_host_fn(
        _state: *const c_void,
        _arguments: *const Value,
        _generics: *const Type<'static>,
    ) -> Value {
        Value { void: () }
    }

    unsafe extern "C" fn dummy_reg_fn(_generics: *const Type<'static>) -> Option<ErasedHostFn> {
        Some(dummy_host_fn)
    }

    // Box[$T] with a method and a static method, an entity E with a method,
    // and a host function f
    fn registration_mod_api() -> ModApi {
        parse(
            r#""Box": {
                "description": "Foo",
                "used_generics": [{"name": "$T"}],
                "methods": {"get": {"description": "Foo"}},
                "static_methods": {"new": {"description": "Foo"}}
            }"#,
            r#""E": {"description": "Foo", "methods": {"m": {"description": "Foo"}}}"#,
            r#""f": {"description": "Foo"}"#,
        )
    }

    fn register_twice(class_name: Option<&str>, fn_name: &str) -> String {
        let mut mod_api = registration_mod_api();
        mod_api.register_host_fn(class_name, fn_name, dummy_host_fn, None).unwrap();
        let err = mod_api
            .register_host_fn(class_name, fn_name, dummy_host_fn, None)
            .expect_err("expected the second registration to fail");
        err.inner().error_message.to_str().to_owned()
    }

    fn register_error(class_name: Option<&str>, fn_name: &str, gen_count: Option<usize>) -> String {
        let err = registration_mod_api()
            .register_host_fn(class_name, fn_name, dummy_host_fn, gen_count)
            .expect_err("expected the registration to fail");
        err.inner().error_message.to_str().to_owned()
    }

    #[test]
    fn host_fn_can_only_be_registered_once() {
        assert_eq!(register_twice(None, "f"), "host function f has already been registered");
    }

    #[test]
    fn method_can_only_be_registered_once() {
        assert_eq!(register_twice(Some("Box"), "get"), "method Box.get has already been registered");
    }

    #[test]
    fn static_method_can_only_be_registered_once() {
        assert_eq!(
            register_twice(Some("Box"), "new"),
            "static method Box.new has already been registered"
        );
    }

    #[test]
    fn entity_method_can_only_be_registered_once() {
        assert_eq!(register_twice(Some("E"), "m"), "entity method E.m has already been registered");
    }

    #[test]
    fn registered_host_fn_must_be_declared() {
        assert_eq!(
            register_error(None, "g", None),
            "host function g was not declared in the mod_api"
        );
    }

    #[test]
    fn registered_method_class_must_be_declared() {
        assert_eq!(
            register_error(Some("Nope"), "get", None),
            "class or entity Nope was not declared in the mod_api"
        );
    }

    #[test]
    fn registered_method_must_be_declared() {
        assert_eq!(
            register_error(Some("Box"), "nope", None),
            "method Box.nope was not declared in the mod_api"
        );
    }

    #[test]
    fn registered_generic_count_must_match() {
        assert_eq!(
            register_error(None, "f", Some(1)),
            "host function f was not declared in the mod_api"
        );
    }

    #[test]
    fn reg_fn_can_only_be_registered_once() {
        let mut mod_api = registration_mod_api();
        mod_api.register_reg_fn(None, "f", dummy_reg_fn, None).unwrap();
        let err = mod_api
            .register_reg_fn(None, "f", dummy_reg_fn, None)
            .expect_err("expected the second registration to fail");
        assert_eq!(
            err.inner().error_message.to_str(),
            "host function f has already been registered"
        );
        // The registered reg fn hands out a callable host fn
        let host_fn = unsafe { dummy_reg_fn(std::ptr::null()) }.unwrap();
        unsafe { host_fn(std::ptr::null(), std::ptr::null(), std::ptr::null()) };
    }

    #[test]
    fn register_dummies_registers_every_host_fn_and_method() {
        let mut mod_api = registration_mod_api();
        unsafe { mod_api.register_dummies() };
        let host_fns = mod_api
            .host_fns
            .values()
            .chain(mod_api.classes.values().flat_map(|class| class.assoc_fns.iter().map(|(_, host_fn)| host_fn)));
        for host_fn in host_fns {
            let fn_ptr = host_fn.fn_ptr.expect("every host fn should have a dummy");
            unsafe { fn_ptr(std::ptr::null(), std::ptr::null(), std::ptr::null()) };
        }
    }
}
