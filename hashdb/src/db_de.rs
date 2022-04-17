use std::{any::Any, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, hash::{Hash as StdHash, Hasher}, sync::Arc};

use bumpalo::Bump;
use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Fallible, validation::validators::DefaultValidator};

use crate::{Datastore, DatastoreError, Hash, Link, LinkArc, LinkType, NativeHashtype};

/// Represents a Deserializer that deserializes from an archive and allocates a linked structure using an arbitrary LinkType
pub trait DatastoreDeserializer<'a>: Fallible<Error: From<DatastoreError>> {
	type Dedup: 'a + Deduplicator<'a>; 
	type LinkL<V: 'a + StdHash>: LinkType<'a> = <Self::Dedup as Deduplicator<'a>>::LinkL<V>;
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, <Self as Fallible>::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>;
	fn alloc<V: StdHash>(&mut self, val: V) -> Link<'a, V, Self::LinkL<V>>;
}

/// Represents an object that can deduplicate data when deserializing or adding objects manually
pub trait Deduplicator<'a> {
	type LinkL<V: 'a + StdHash>: LinkType<'a> where Self: 'a;
	fn add<V: StdHash>(&'a self, val: V) -> Link<'a, V, Self::LinkL<V>>;
}

pub struct HashDeserializer<'a, D: Deduplicator<'a> + 'a> {
	db: &'a Datastore,
	dedup: &'a mut D,
}

impl<'a, 'l, D: Deduplicator<'l>> Fallible for HashDeserializer<'l, D> {
	type Error = DatastoreError;
}
impl<'a, D: Deduplicator<'a> + 'a> DatastoreDeserializer<'a> for HashDeserializer<'a, D> {
	type Dedup = D;
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, DatastoreError>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>
	{
		let data = Datastore::get(self.db, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(ret)
	}
	fn alloc<V: StdHash>(&mut self, val: V) -> Link<'a, V, D::LinkL<V>> {
		self.dedup.add(val)
	}
}

/// This doesn't work in non-static environments
pub struct LinkArcs {
	map: RefCell<HashMap<u64, Arc<dyn Any + Send + Sync + 'static>>>,
}
impl LinkArcs {
	pub fn new() -> Self {
		Self { map: Default::default() }
	}
	pub fn add<T: std::hash::Hash + Send + Sync + 'static>(&self, val: T) -> Link<'static, T, Arc<T>> {
		let hasher = &mut DefaultHasher::new();
		val.hash(hasher);
		let hash = hasher.finish();
		let arc = Arc::new(val);
		if let Some(val) = self.map.borrow().get(&hash) {
			// Safety: Only this type can create this hash
			unsafe {
				return Link::new(val.clone().downcast::<T>().unwrap())
			}
		} else {
			self.map.borrow_mut().insert(hash, arc.clone());
		}
		Link::new(arc)
	}
}

pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, &'a ()>>,
}
impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self { arena: Bump::new(), map: Default::default() }
	}
	pub fn alloc<T>(&'a self, val: T) -> &'a T {
		self.arena.alloc(val)
	}
	pub fn dedup<T: std::hash::Hash>(&self, val: &'a T) -> &'a T {
		let hasher = &mut DefaultHasher::new();
		val.hash(hasher);
		let hash = hasher.finish();
		if let Some(&val) = self.map.borrow().get(&hash) {
			// Safety: Only this type can create this hash, theoretically. (ok, I know this can cause UB but I can't think of a better implementation)
			unsafe {
				return &*(val as *const ()).cast()
			}
		} else {
			unsafe { self.map.borrow_mut().insert(hash, &*(val as *const T).cast()); }
		}
		val
	}
	pub fn alloc_dedup<T: std::hash::Hash>(&'a self, val: T) -> &'a T {
		let val = self.arena.alloc(val);
		self.dedup(val)
	}
	/// Add value to arena and get link
	pub fn add<T: std::hash::Hash>(&'a self, val: T) -> Link<T, &'a T> {
		Link::new(self.alloc_dedup(val))
	}
}

impl<'a> Deduplicator<'a> for LinkArena<'a> {
	type LinkL<V: 'a + StdHash> = &'a V;
    fn add<V: StdHash>(&'a self, val: V) -> Link<'a, V, Self::LinkL<V>> {
        self.add(val)
    }
}

/* pub struct ArenaHashDeserializer<'a>(&'a Datastore, &'a LinkArena<'a>);
impl<'a> Fallible for ArenaHashDeserializer<'a> {
    type Error = DatastoreError;
}
impl<'a, LV: std::hash::Hash> DatastoreDeserializer<'a, LV, &'a LV> for ArenaHashDeserializer<'a> {
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, DatastoreError>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>
	{
		let data = Datastore::get(self.0, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(ret)
	}
	fn alloc(&mut self, val: LV) -> Link<'a, LV, &'a LV> {
		Link::new(self.1.alloc_dedup(val))
	}
} */