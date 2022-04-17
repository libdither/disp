use std::{any::Any, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, hash::Hasher, sync::Arc};

use bumpalo::Bump;
use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Fallible, validation::validators::DefaultValidator};

use crate::{Datastore, DatastoreError, Hash, Link, LinkArc, LinkType, NativeHashtype};

/// Represents a Deserializer that deserializes from an archive and allocates a linked structure using an arbitrary LinkType
pub trait DatastoreDeserializer<'a, LV, L: LinkType<'a, LV>>: Fallible<Error: From<DatastoreError>> {
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, <Self as Fallible>::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>;
	fn alloc(&mut self, val: LV) -> Link<'a, LV, L>;
}

pub struct LinkArcs {
	map: RefCell<HashMap<u64, Arc<dyn Any + Send + Sync + 'static>>>,
}
impl LinkArcs {
	pub fn new() -> Self {
		Self { map: Default::default() }
	}
	pub fn add<T: std::hash::Hash + Send + Sync + 'static>(&self, val: T) -> Link<T, Arc<T>> {
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
	pub fn join<'a>(&'a self, db: &'a Datastore) -> ArcHashDeserializer<'a> {
		ArcHashDeserializer(self, db)
	}
}

pub struct ArcHashDeserializer<'a>(&'a LinkArcs, &'a Datastore);
impl<'a> Fallible for ArcHashDeserializer<'a> {
	type Error = DatastoreError;
}
impl<'db, LV> DatastoreDeserializer<'static, LV, Arc<LV>> for ArcHashDeserializer<'db> {
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, DatastoreError>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>
	{
		let data = Datastore::get(self.1, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(ret)
	}
	fn alloc(&mut self, val: LV) -> Link<'static, LV, Arc<LV>> {
		LinkArc::new(Arc::new(val))
	}
}

pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, &'a dyn Any>>,
}
impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self { arena: Bump::new(), map: Default::default() }
	}
	pub fn alloc<T>(&'a self, val: T) -> &'a T {
		self.arena.alloc(val)
	}
	pub fn dedup<T: std::hash::Hash + 'static>(&self, val: &'a T) -> &'a T {
		let hasher = &mut DefaultHasher::new();
		val.hash(hasher);
		let hash = hasher.finish();
		if let Some(&val) = self.map.borrow().get(&hash) {
			// Safety: Only this type can create this hash
			unsafe {
				return val.downcast_ref_unchecked()
			}
		} else {
			self.map.borrow_mut().insert(hash, val);
		}
		val
	}
	pub fn alloc_dedup<T: std::hash::Hash + 'static>(&'a self, val: T) -> &'a T {
		let val = self.arena.alloc(val);
		self.dedup(val)
	}
	/// Add value to arena and get link
	pub fn add<T: std::hash::Hash + 'static>(&'a self, val: T) -> Link<T, &'a T> {
		Link::new(self.alloc_dedup(val))
	}
	pub fn join(&'a self, db: &'a Datastore) -> ArenaHashDeserializer<'a> {
		ArenaHashDeserializer(db, self)
	}
}

pub struct ArenaHashDeserializer<'a>(&'a Datastore, &'a LinkArena<'a>);
impl<'a> Fallible for ArenaHashDeserializer<'a> {
    type Error = DatastoreError;
}
impl<'a, LV: std::hash::Hash + 'static> DatastoreDeserializer<'a, LV, &'a LV> for ArenaHashDeserializer<'a> {
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
}