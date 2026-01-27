use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::cell::RefCell;
use std::borrow::Borrow;
use std::hash::Hash;

use crate::xar::{Xar, XarHandle};

pub struct CacheMap<K, V> {
	values: Xar<V>,
	map: RefCell<HashMap<K, XarHandle<'static, V>>>,
}
// pub struct CacheMap<K, V>(RefCell<HashMap<K, Box<V>>>);

impl<K, V> CacheMap<K, V> {
	pub fn new() -> Self {
		Self {
			values: Xar::new(),
			map: RefCell::new(HashMap::new()),
		}
	}

	pub fn clear(&mut self) {
		self.map.get_mut().drain().for_each(|(_, V)| {
			unsafe{
				self.values.delete(V);
			}
		})
	}
}

impl<K: Hash + Eq, V> CacheMap<K, V> {
	pub fn get<Q>(&self, key: &Q) -> Option<&V> where
		K: Borrow<Q>,
		Q: Hash + Eq + ?Sized,
	{
		Some(unsafe{self.map.borrow().get(key)?.cloned_ref().detach_lifetime().get_ref()})
	}

	// pub fn get_or_insert_with<Q, KF, VF>(&self, key: &Q, kf: KF, vf: VF) -> &V where 
	// 	K: Borrow<Q>,
	// 	Q: Hash + Eq + ?Sized,
	// 	KF: FnOnce() -> K,
	// 	VF: FnOnce() -> V,
	// {
	// 	if let Some(value) = self.0.borrow().get(key) {
	// 		return unsafe{&*(&**value as *const _)};
	// 	} else {
	// 		self.0.borrow_mut().insert(kf(), Box::new(vf()));
	// 		self.get(key).unwrap()
	// 	}
	// }

	pub fn try_insert(&self, k: K, v: V) -> Result<(), (K, V)> {
		let mut borrow = self.map.borrow_mut();
		let borrow = &mut *borrow;
		if borrow.get(&k).is_some() {
			Err((k, v))
		} else {
			match borrow.entry(k) {
				Entry::Vacant(x) => {
					x.insert_entry(unsafe{self.values.insert(v).detach_lifetime()});
					Ok(())
				}
				_ => unreachable!()
			}
		}
	}
}

impl<K: Hash + Eq, V: PartialEq> PartialEq for CacheMap<K, V> {
	fn eq(&self, other: &Self) -> bool {
		self.map.borrow().eq(&other.map.borrow())
	}
}

impl<K: Hash + Eq, V: Eq> Eq for CacheMap<K, V> {
}

use std::fmt::{Debug, Formatter};
impl<K: Hash + Eq + Debug, V: Debug> Debug for CacheMap<K, V> {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		self.map.borrow().fmt(f)
	}
}

impl<K, V> Drop for CacheMap<K, V> {
	fn drop(&mut self) {
		self.clear()
	}
}
