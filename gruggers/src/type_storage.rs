use std::collections::HashSet;
use std::sync::{RwLock, LazyLock};

use crate::arena::MTArena;
use crate::ntstring::NTStr;
use crate::ast::Type;

#[repr(transparent)]
#[derive(Eq, Clone, Copy)]
/// Override Hash and Eq to be shallow checks
struct TypeWrapper<'a>(Type<'a>);

impl<'a> PartialEq for TypeWrapper<'a> {
	fn eq(&self, other: &Self) -> bool {
		use Type::*;
		match (self.0, other.0) {
			(Void, Void) => true,
			(Bool, Bool) => true,
			(Number, Number) => true,
			(String, String) => true,
			(Id{name: name_1, generics: generics_1, ..}, Id{name: name_2, generics: generics_2, ..}) => name_1.ptr_eq(name_2) && std::ptr::eq(generics_1, generics_2),
			(
				Resource {
					extension: extension_1,
				}, 
				Resource {
					extension: extension_2,
				}, 
			) => extension_1.ptr_eq(extension_2),
			(
				Entity {
					entity_type: ty_1,
				}, 
				Entity {
					entity_type: ty_2,
				}, 
			) => match (ty_1, ty_2) {
				(None, None) => true,
				(Some(ty_1), Some(ty_2)) => ty_1.ptr_eq(ty_2),
				_ => false,
			}
			(Existential{idx: idx1}, Existential{idx: idx2}) => idx1 == idx2,
			_ => false,
		}
	}
}

impl<'a> std::hash::Hash for TypeWrapper<'a> {
	fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
		use Type::*;
		match self.0 {
			Id{name, generics, ..} => {
				"Type".hash(hasher);
				hasher.write_usize(name.as_ptr() as usize);
				hasher.write_usize(generics as *const _ as *const Type as usize);
			}
			Resource {
				extension,
			} => {
				"Type".hash(hasher);
				hasher.write_usize(extension.as_ptr() as usize);
			}
			Entity {
				entity_type: Some(ty),
			} => {
				"Type".hash(hasher);
				hasher.write_usize(ty.as_ptr() as usize);
			}
			_ => std::hash::Hash::hash(&self.0, hasher)
		}
	}
}

impl<'a> TypeWrapper<'a> {
	#[inline]
	fn cast_slice_from_type<'b>(this: &'b [Type<'a>]) -> &'b [Self] {
		// SAFETY: Self is repr transparent with Type, this is the whole point
		// of this wrapper
		unsafe {std::mem::transmute::<&[Type<'a>], &[Self]>(this)}
	}

	#[inline]
	fn cast_slice_to_type<'b>(this: &'b [Self]) -> &'b [Type<'a>] {
		// SAFETY: Self is repr transparent with Type, this is the whole point
		// of this wrapper
		unsafe {std::mem::transmute::<&[Self], &[Type<'a>]>(this)}
	}
}

pub struct TypeStorage {
	type_lists: HashSet<&'static [TypeWrapper<'static>]>,
	strings: HashSet<&'static NTStr>,
}

static GLOBAL_TYPE_ARENA: MTArena = MTArena::new();
static GLOBAL_TYPE_STORAGE: LazyLock<RwLock<TypeStorage>> = LazyLock::new(|| RwLock::new(TypeStorage::new()));

impl TypeStorage {
	pub fn new() -> Self {
		Self {
			type_lists: HashSet::new(),
			strings: HashSet::new(),
		}
	}

	pub fn insert_string(&mut self, str: &str) -> &'static NTStr {
		match self.strings.get(str) {
			Some(str) => return str,
			None => {
				{
					let global_storage = GLOBAL_TYPE_STORAGE.read().unwrap();
					match global_storage.strings.get(str) {
						Some(str) => {
							self.strings.insert(str);
							return str;
						}
						None => (),
					}
				}
				{
					let str = GLOBAL_TYPE_ARENA.copy_str_into_nt(str);
					let mut global_storage = GLOBAL_TYPE_STORAGE.write().unwrap();
					global_storage.strings.insert(str);
					self.strings.insert(str);
					return str;
				}
			}
		}
	}

	pub fn insert_type(&mut self, ty: Type) -> Type<'static> {
		use Type::*;
		match ty {
			Id{name, generics} => {
				let name = self.insert_string(name.to_str());
				let generics = self.insert_type_list(generics);
				self.type_lists.insert(TypeWrapper::cast_slice_from_type(generics));

				return Id{
					name: name.as_ntstrptr(),
					generics,
				};
			}
			Resource {
				extension,
			} => {
				Resource {
					extension: self.insert_string(extension.to_str()).as_ntstrptr()
				}
			}
			Entity {
				entity_type: Some(ty),
			} => {
				Entity {
					entity_type: Some(self.insert_string(ty.to_str()).as_ntstrptr())
				}
			}
			Void => Void,
			Bool => Bool,
			Number => Number,
			String => String,
			Entity{entity_type: None} => Entity{entity_type: None},
			Existential {
				idx: usize
			} => Existential{idx: usize}
		}
	}

	pub fn insert_type_list<'a, 'b>(&mut self, list: &'a [Type<'b>]) -> &'static [Type<'static>] {
		let list = TypeWrapper::cast_slice_from_type(list);
		// does a shallow check to check if the list exists
		match self.type_lists.get::<&[TypeWrapper]>(&list) {
			Some(list) => {
				// SAFETY: The returned list is actually a
				// `&'static [TypeWrapper<'static>]` but it gets
				// returned as a &'_ [TypeWrapper<'_>] because of
				// a trait solver limitation.
				//
				// We just transmute it back to it's actual type
				let list: &'static [TypeWrapper<'static>] = unsafe{std::mem::transmute::<&'a [TypeWrapper<'b>], &'static [TypeWrapper<'static>]>(list)};
				return TypeWrapper::cast_slice_to_type(list);
			}
			None => {
				{
					let global_storage = GLOBAL_TYPE_STORAGE.read().unwrap();
					match global_storage.type_lists.get::<&[TypeWrapper]>(&list) {
						Some(list) => {
							// SAFETY: The returned list is actually a
							// `&'static [TypeWrapper<'static>]` but it gets
							// returned as a &'_ [TypeWrapper<'_>] because of
							// a trait solver limitation.
							//
							// We just transmute it back to it's actual type
							let list: &'static [TypeWrapper<'static>] = unsafe{std::mem::transmute::<&'a [TypeWrapper<'b>], &'static [TypeWrapper<'static>]>(list)};
							self.type_lists.insert(list);
							return TypeWrapper::cast_slice_to_type(list);
						}
						None => (),
					}
				}
				{
					let list = list.iter().map(|ty| {
						TypeWrapper(self.insert_type(ty.0))
					}).collect::<Vec<_>>();

					match self.type_lists.get(&*list) {
						Some(list) => return TypeWrapper::cast_slice_to_type(list),
						None => {
							let global_storage = GLOBAL_TYPE_STORAGE.read().unwrap();
							match global_storage.type_lists.get(&*list) {
								Some(list) => {
									self.type_lists.insert(list);
									return TypeWrapper::cast_slice_to_type(list);
								}
								None => (),
							}
						}
					}
					
					let list = GLOBAL_TYPE_ARENA.slice_from_iter(list.into_iter());
					let mut global_storage = GLOBAL_TYPE_STORAGE.write().unwrap();
					global_storage.type_lists.insert(list);
					self.type_lists.insert(list);
					return TypeWrapper::cast_slice_to_type(list);
				}
			}
		}
	}
}

