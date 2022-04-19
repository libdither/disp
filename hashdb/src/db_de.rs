use std::{any::Any, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, hash::{Hash as StdHash, Hasher}, sync::Arc};

use bumpalo::Bump;
use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Fallible, validation::validators::DefaultValidator};

use crate::{Datastore, DatastoreError, Hash, NativeHashtype};

pub trait DatastoreDeserializer<'a>: Fallible + Sized {
	fn get(&'a self) -> &'a HashDeserializer<'a>;
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>;
}

pub struct HashDeserializer<'a> {
	pub db: &'a Datastore,
	pub arena: &'a LinkArena<'a>,
}
impl<'a> Fallible for HashDeserializer<'a> {
	type Error = DatastoreError;
}
impl<'a> DatastoreDeserializer<'a> for HashDeserializer<'a> {
	fn get(&'a self) -> &'a HashDeserializer<'a> { self }
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>
	{
		let data = Datastore::get(self.db, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(self.arena.add(ret))
	}
}
pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, *const ()>>, // Lookup map
	reverse_lookup: RefCell<HashMap<u64, *const ()>>, // Reverse lookup map
	p: std::marker::PhantomData<&'a ()>,
}
impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self {
			arena: Bump::new(),
			map: Default::default(),
			reverse_lookup: Default::default(),
			p: Default::default()
		}
	}
	pub fn dedup<T: std::hash::Hash>(&self, val: &'a T) -> &'a T {
		let hasher = &mut DefaultHasher::new();
		val.hash(hasher);
		let hash = hasher.finish();

		// Safety: Only this type can create this hash, theoretically. (ok, I know this can cause UB but I can't think of a better implementation)
		unsafe {
			let ptr = *self.map.borrow_mut().entry(hash).or_insert((val as *const T).cast());
			&*ptr.cast::<T>()
		}
	}
	pub fn alloc_dedup<T: std::hash::Hash>(&'a self, val: T) -> &'a T {
		let val = self.arena.alloc(val);
		self.dedup(val)
	}
	/// Add value to arena and get link
	pub fn add<T: std::hash::Hash>(&'a self, val: T) -> &'a T {
		self.alloc_dedup(val)
	}
	pub fn add_with_lookups<T: StdHash + NativeHashtype>(&'a self, val: T) -> &'a T {
		self.add(val)
	}
}