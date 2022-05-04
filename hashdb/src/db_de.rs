use std::{
	cell::RefCell,
	collections::{hash_map::DefaultHasher, HashMap},
	hash::{Hash as StdHash, Hasher},
};

use bumpalo::Bump;
use bytecheck::CheckBytes;
use rkyv::{validation::validators::DefaultValidator, Archive, Deserialize, Fallible};

use crate::{Datastore, DatastoreError, DatastoreSerializer, Hash, NativeHashtype, TypedHash};

pub trait DatastoreDeserializer<'a>: Fallible + Sized {
	fn get(&'a self) -> &'a HashDeserializer<'a>;
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>;
}

pub struct HashDeserializer<'a> {
	pub db: &'a Datastore,
	pub arena: &'a LinkArena<'a>,
}
impl<'a> Fallible for HashDeserializer<'a> {
	type Error = DatastoreError;
}
impl<'a> DatastoreDeserializer<'a> for HashDeserializer<'a> {
	fn get(&'a self) -> &'a HashDeserializer<'a> {
		self
	}
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>,
	{
		let data = Datastore::get(self.db, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(self.arena.add(ret))
	}
}
pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, *const ()>>,             // Lookup map
	reverse_lookup: RefCell<HashMap<Hash, (*const (), &'static str)>>, // Reverse lookup map, str represents object type
	p: std::marker::PhantomData<&'a ()>,
}
impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self {
			arena: Bump::new(),
			map: Default::default(),
			reverse_lookup: Default::default(),
			p: Default::default(),
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
	/// Safety: Yes, this can easily segfault, i'll fix it later
	pub fn add_with_lookups<T: StdHash + NativeHashtype>(&'a self, val: T, ser: &mut impl DatastoreSerializer) -> &'a T {
		let ret = self.add(val);
		let mut lookups = self.reverse_lookup.borrow_mut();
		for link in ret.reverse_links(ser) {
			lookups.insert(link, ((ret as *const T).cast(), std::any::type_name::<T>()));
		}
		ret
	}
	/// Safety: Yes, this can easily segfault, make sure you don't add_with_lookups two different objects that link to the same object and that you used the correct object
	pub fn lookup<T: NativeHashtype, L: NativeHashtype>(&'a self, hash: &TypedHash<L>) -> Option<&'a T> {
		self.reverse_lookup.borrow().get(hash.into()).map(|(ptr, type_str)| {
			if type_str == &std::any::type_name::<T>() { Some(unsafe { &*(ptr.cast()) }) }
			else { None }
		}).flatten()
	}
}
