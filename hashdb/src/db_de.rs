use std::{
	cell::RefCell,
	collections::{hash_map::DefaultHasher, HashMap},
	hash::{Hash as StdHash, Hasher},
};

use bumpalo::Bump;
use bytecheck::CheckBytes;
use rkyv::{validation::validators::DefaultValidator, Archive, Deserialize, Fallible};

use crate::{Datastore, DatastoreError, DatastoreSerializer, Hash, NativeHashtype, TypedHash};

/// Trait to deserialize Hash from Datastore into bump arena
pub trait DatastoreDeserializer<'d>: Fallible + Sized {
	fn fetch<'a, T: NativeHashtype>(&mut self, hash: &Hash, arena: &'a LinkArena<'a>) -> Result<&'a T, <Self as Fallible>::Error>
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
