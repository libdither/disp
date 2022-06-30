//! Forms of storage for Disp Expressions
//! Arena Storage (Memory)
//!  - Stores expressions in a bump arena with support for deduplication of new expressions through in-memory hash table lookup
//! Hash Table Storage (Memory)
//!  - Simple HashMap which maps Hash to serialized 
//! Hash Table Storage

use std::hash::Hash as StdHash;
use bytecheck::CheckBytes;
use rkyv::Fallible;
use rkyv::validation::validators::DefaultValidator;
use rkyv::{Archive, Deserialize, Serialize, ser::Serializer};

mod arena;
mod db;
mod ser;
pub use arena::*;
pub use db::*;
use rkyv::Archived;

use crate::TypedHash;
use crate::Hash;

/// Arena Storage in-memory
/// Good for fast accessing, recursive descent and in-memory manipulations of expressions
/// All types that store a T in an arena and return a reference to that T that lasts as long as the arena.
pub trait TypeStore<'a>: 'a {
	fn add<T: TypeStorable>(&'a self, val: T) -> &'a T;
}
/// All types that can be stored in a LinkArena.
pub trait TypeStorable: StdHash {}

impl<T: StdHash> TypeStorable for T {}

/// All types that can store a piece of data by its multihash.
pub trait DataStore: DataStoreRead {
	type Data: AsRef<[u8]>;
	/// Store data in table.
	fn add(&mut self, data: Self::Data) -> Hash;
}
/// All types that can retrieve a pi ece of data using its multihash.
pub trait DataStoreRead {
	type Error;
	/// Get data from table
	fn get(&self, hash: &Hash) -> Result<&[u8], Self::Error>;
	/// Check if data is available in table.
	fn contains(&self, hash: &Hash) -> bool { self.get(hash).is_ok() }
}

/// Archive object store, extends hash store with functions that can store typed objects using rkyv.
/// All types that can store ArchiveStorable
pub trait ArchiveStore: DataStore + ArchiveStoreRead + Sized + Serializer {
	/// Store storable type in table. Returns a typed hash with the archived version of the type.
	fn store<T: ArchiveStorable<Self>>(&mut self, data: &T) -> Result<TypedHash<Archived<T>>, <Self as Fallible>::Error>;
}

/// Retrival functions of an Archive object store. Returns archived and unarchived objects respectively.
pub trait ArchiveStoreRead: DataStoreRead + Sized {
	type ArchiveError: From<Self::Error> + 'static;
	/// Fetch storable type from table.
	fn fetch_archive<'s, T: ArchiveInterpretable<'s, Self>>(&'s self, hash: &TypedHash<Archived<T>>) -> Result<&'s Archived<T>, Self::ArchiveError>;
	/* fn fetch<'s, 'a: 's, A: TypeStore<'a>, T: ArchiveFetchable<'s, 'a, Self, A>>(&'s self, hash: &TypedHash<Archived<T>>, arena: &'a A) -> Result<T, Self::ArchiveError>; */
}

/* /// A type that encompases an ArchiveStore that can be deserialized from and a TypeStore for things to be registered into.
pub trait ArchiveDeserializer<'a>: TypeStore<'a> + ArchiveStoreRead {
	fn fetch<T>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>;
} */

pub struct ArchiveDeserializer<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> {
	store: &'s S, pub type_store: &'a A
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> From<(&'s S, &'a A)> for ArchiveDeserializer<'s, 'a, S, A> {
	fn from(pair: (&'s S, &'a A)) -> Self {
		Self { store: pair.0, type_store: pair.1 }
	}
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> Fallible for ArchiveDeserializer<'s, 'a, S, A> {
	type Error = S::ArchiveError;
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> ArchiveDeserializer<'s, 'a, S, A> {
	pub fn fetch<T: ArchiveFetchable<'s, 'a, S, A>>(&mut self, hash: &TypedHash<Archived<T>>) -> Result<T, <Self as Fallible>::Error> {
		let archive: &Archived<T> = self.store.fetch_archive::<T>(hash)?;
		Ok(Deserialize::deserialize(archive, self)?)
	}
	pub fn fetch_ref<T: ArchiveFetchable<'s, 'a, S, A> + TypeStorable>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error> {
		let typed_hash: &TypedHash<Archived<T>> = hash.into();
		let item = self.fetch(typed_hash)?;
		Ok(self.type_store.add(item))
	}
}

/// A type that can be stored in an ArchiveStore using a custom serializer
pub trait ArchiveStorable<Store: ArchiveStore>: 
	Serialize<Store> + Sized
{
	fn store(&self, db: &mut Store) -> Result<TypedHash<Self::Archived>, <Store as Fallible>::Error> {
		db.store(self)
	}
}

/// A type that can be zero-copy deserialized without full deserialization i.e. fetching subobjects from store.
pub trait ArchiveInterpretable<'s, S: ArchiveStoreRead>:
	Archive<Archived: CheckBytes<DefaultValidator<'s>>> + Sized
{}

/// A type that can be fetched in its Archived form from an ArchiveStore for lifetime 's and can be placed into a TypeStore<'a> for lifetime 'a
pub trait ArchiveFetchable<'s, 'a, S: ArchiveStoreRead + 's, A: TypeStore<'a>>:
	ArchiveInterpretable<'s, S> + Archive<Archived: Deserialize<Self, ArchiveDeserializer<'s, 'a, S, A>>> + 's
{}