use std::collections::HashMap;
use std::collections::hash_map::{self, Entry};
use std::cell::{Ref, RefCell};
use std::borrow::Borrow;
use std::hash::Hash;

use crate::xar::{Xar, XarHandle};

pub struct CacheMap<K, V> {
	values: Xar<V>,
	/// The XarHandle here is unique. 
	/// We only give out shared references to the data with a lifetime tied to
	/// a shared borrow of self.
	///
	/// Deletion is only done with a mutable reference to self which
	/// invalidates all existing shared references which makes it okay.
	map: RefCell<HashMap<K, XarHandle<'static, V>>>,
}

impl<K, V> CacheMap<K, V> {
	pub fn new() -> Self {
		Self {
			values: Xar::new(),
			map: RefCell::new(HashMap::new()),
		}
	}

	pub fn clear(&mut self) {
		self.map.get_mut().drain().for_each(|(_, v)| {
			// SAFETY: all handles in self.map come from self.values
			unsafe{
				self.values.delete(v);
			}
		})
	}

	pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
		IterMut::new(self.map.get_mut().iter_mut())
	}

	pub fn find_key(&self, to_find: &V) -> Option<Ref<'_, K>> where
		V: PartialEq<V>
	{
		Ref::filter_map(self.map.borrow(), |inner|
			inner.iter().find(|(_, v)| unsafe {v.get_ref()} == to_find).map(|x| x.0)
		).ok()
	}
}

impl<K: Hash + Eq, V> CacheMap<K, V> {
	pub fn get<Q>(&self, key: &Q) -> Option<&V> where
		K: Borrow<Q>,
		Q: Hash + Eq + ?Sized,
	{
		// SAFETY: We only give out shared references to the value behind the XarHandle
		// and the lifetime is restrained by self.
		Some(unsafe{self.map.borrow().get(key)?.get_ref()})
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
					// SAFETY: We never give out a XarHandle or a reference with a static lifetime
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
		let other = other.map.borrow();
		let self_map = self.map.borrow();
		for (k, v_0) in self_map.iter() {
			match other.get(k) {
				None => return false,
				// getting a shared reference is okay if we have a shared reference to self
				Some(v_1) if unsafe{v_0.get_ref() != v_1.get_ref()} => return false,
				_ => (),
			}
		}
		for (k, v_0) in other.iter() {
			match self_map.get(k) {
				None => return false,
				// getting a shared reference is okay if we have a shared reference to self
				Some(v_1) if unsafe{v_0.get_ref() != v_1.get_ref()} => return false,
				_ => (),
			}
		}
		return true
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

pub struct IterMut<'a, K: 'a, V: 'a> {
	inner: hash_map::IterMut<'a, K, XarHandle<'static, V>>,
}

impl<'a, K: 'a, V: 'a> IterMut<'a, K, V> {
	fn new(inner: hash_map::IterMut<'a, K, XarHandle<'static, V>>) -> Self {
		Self {
			inner
		}
	}
}

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
	type Item = (&'a K, &'a mut V);
	fn next(&mut self) -> Option<Self::Item> {
		let (k, v) = self.inner.next()?;
		// SAFETY: IterMut comes from a &mut CacheMap which allows mutable
		// access to the data
		Some((k, unsafe{v.get_mut()}))
	}
}
