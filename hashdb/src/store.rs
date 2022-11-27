//! Forms of storage for Disp Expressions
//! Arena Storage (Memory)
//!  - Stores expressions in a bump arena with support for deduplication of new expressions through in-memory hash table lookup
//! Hash Table Storage (Memory)
//!  - Simple HashMap which maps Hash to serialized 
//! Hash Table Storage

use std::hash::{Hash as StdHash, Hasher};
use bytecheck::CheckBytes;
use hashdb_derive::impl_hashtype_for_many;
use rkyv::Fallible;
use rkyv::ser::{ScratchSpace, Serializer};
use rkyv::validation::validators::DefaultValidator;
use rkyv::{Archive, Deserialize, Serialize};

mod arena;
mod db;
mod ser;
mod rev;
pub use arena::*;
pub use db::*;
pub use rev::*;
use rkyv::Archived;

use crate::TypedHash;
use crate::Hash;

/// Arena Storage in-memory
/// Good for fast accessing, recursive descent and in-memory manipulations of expressions
/// All types that store a T in an arena and return a reference to that T that lasts as long as the arena.
pub trait TypeStore<'a> {
	fn add<T: HashType<'a>>(&'a self, val: T) -> &'a T;
}
pub trait RevTypeStore<'a>: TypeStore<'a> + Sized {
	fn rev_add<T: RevHashType<'a>>(&'a self, val: T) -> &'a T
	where T: for<'s> ArchiveFetchable<'a, ArchiveToType<'s, 'a, Datastore, Self>> + ArchiveStorable<Datastore>;

	type Iter<'i: 'a, L: 'i>: Iterator<Item = &'i L> where Self: 'i;
	fn links<T: HashType<'a>, L: RevHashType<'a>>(&'a self, val: &T) -> Self::Iter<'a, L> 
	where
		L: for<'s> ArchiveFetchable<'a, ArchiveToType<'s, 'a, Datastore, Self>>;
}

/// All types that can be stored in a LinkArena.

/// Type that has fields that implement HashType that may be reverse-linked (i.e. given the value of a field, find the HashType containing it)
/// Implemented by #[hashtype] macro
pub trait ReverseLinks {
	fn reverse_links(&self) -> impl Iterator<Item = u64>;
}

/// Type that has a unique id, implemented by #[hashtype] macro
pub trait UniqueId {
	fn unique_id() -> u64;
}
impl<'a, T: UniqueId> UniqueId for &'a T {
    fn unique_id() -> u64 {
        T::unique_id()
    }
}

pub trait UniqueHash: StdHash + UniqueId {
	fn unique_hash(&self, hasher: &mut impl Hasher);
}
impl<T: UniqueId + StdHash> UniqueHash for T {
    fn unique_hash(&self, hasher: &mut impl Hasher) {
        hasher.write_u64(Self::unique_id());
		self.hash(hasher);
    }
} 


/// Defines a HashType, represents a Rust type with a UniqueId and some set of defined ReverseLinks
pub trait HashType<'a>: UniqueHash + std::fmt::Debug + 'a {}
impl<'a, T: UniqueHash + std::fmt::Debug + 'a> HashType<'a> for T {}

pub trait RevHashType<'a>: HashType<'a> + ReverseLinks + UniqueId {}
impl<'a, T: HashType<'a> + ReverseLinks + UniqueId> RevHashType<'a> for T {}

impl_hashtype_for_many! {
	String,
	usize
}

/// All types that can store a piece of data by its multihash.
pub trait DataStore: DataStoreRead {
	type Data: AsRef<[u8]>;
	/// Store data in table.
	fn add(&mut self, data: Self::Data) -> Hash;
}
/// All types that can retrieve a pi ece of data using its multihash.
pub trait DataStoreRead {
	type DataError;
	/// Get data from table
	fn get(&self, hash: &Hash) -> Result<&[u8], Self::DataError>;
	/// Check if data is available in table.
	fn contains(&self, hash: &Hash) -> bool { self.get(hash).is_ok() }
}

/// Archive object store, extends hash store with functions that can store typed objects using rkyv.
/// All types that can store ArchiveStorable
pub trait ArchiveStore: DataStore + ArchiveStoreRead + Serializer + ScratchSpace {
	/// Store storable type in table. Returns a typed hash with the archived version of the type.
	fn store<T: ArchiveStorable<Self>>(&mut self, data: &T) -> Result<TypedHash<T>, <Self as Fallible>::Error>;
}

/// Retrival functions of an Archive object store. Returns archived and unarchived objects respectively.
pub trait ArchiveStoreRead: DataStoreRead + Sized + Fallible {
	/// Fetch storable type from table.
	fn fetch_archive<T>(&self, hash: &TypedHash<T>) -> Result<&Archived<T>, Self::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>> + ArchiveInterpretable;
}

/// A type that encompases the functions of an ArchiveStoreRead and a TypeStore so that objects can be deserialized from the ArchiveStore into the TypeStore
pub trait ArchiveDeserializer<'a>: ArchiveStoreRead + TypeStore<'a> {
	fn fetch<T: ArchiveFetchable<'a, Self>>(&mut self, hash: &TypedHash<T>) -> Result<T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>;
	fn fetch_ref<T: ArchiveFetchable<'a, Self> + HashType<'a>>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>;
}

/// A type that is generic over any ArchiveStoreRead and TypeStore and implements ArchiveDeserializer
pub struct ArchiveToType<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> {
	pub store: &'s S,
	pub type_store: &'a A
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> From<(&'s S, &'a A)> for ArchiveToType<'s, 'a, S, A> {
	fn from(pair: (&'s S, &'a A)) -> Self {
		Self { store: pair.0, type_store: pair.1 }
	}
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> Fallible for ArchiveToType<'s, 'a, S, A> {
	type Error = S::Error;
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> DataStoreRead for ArchiveToType<'s, 'a, S, A> {
    type DataError = <S as DataStoreRead>::DataError;

    fn get(&self, hash: &Hash) -> Result<&[u8], Self::DataError> {
        self.store.get(hash)
    }
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> ArchiveStoreRead for ArchiveToType<'s, 'a, S, A> {
    fn fetch_archive<'s1, T: ArchiveInterpretable>(&'s1 self, hash: &TypedHash<T>) -> Result<&'s1 Archived<T>, Self::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>> + ArchiveInterpretable
	{
        self.store.fetch_archive::<T>(hash)
    }
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> TypeStore<'a> for ArchiveToType<'s, 'a, S, A> {
    fn add<T: HashType<'a>>(&'a self, val: T) -> &'a T {
        self.type_store.add(val)
    }
}
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> ArchiveDeserializer<'a> for ArchiveToType<'s, 'a, S, A> {
    fn fetch<T: ArchiveFetchable<'a, Self>>(&mut self, hash: &TypedHash<T>) -> Result<T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>
	{
        let archive: &Archived<T> = self.store.fetch_archive::<T>(hash)?;
		Ok(Deserialize::deserialize(archive, self)?)
    }

    fn fetch_ref<T: ArchiveFetchable<'a, Self> + HashType<'a>>(&mut self, hash: &Hash) -> Result<&'a T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>> + ArchiveInterpretable
	{
        let typed_hash: &TypedHash<T> = hash.into();
		let item = self.fetch(typed_hash)?;
		Ok(self.type_store.add(item))
    }
}

/// A type that can be stored in an ArchiveStore using a custom serializer
pub trait ArchiveStorable<Store: ArchiveStore>: 
	Serialize<Store> + Sized
{
	fn store(&self, db: &mut Store) -> Result<TypedHash<Self>, <Store as Fallible>::Error> {
		db.store(self)
	}
}

/// A type that can be zero-copy deserialized without full deserialization i.e. fetching subobjects from store.
pub trait ArchiveInterpretable:
	Archive + Sized
{}

/// A type that can be fetched in its Archived form from an ArchiveStore for lifetime 's and can be placed into a TypeStore<'a> for lifetime 'a
pub trait ArchiveFetchable<'a, D: ArchiveDeserializer<'a>>:
	ArchiveInterpretable + Archive<Archived: Deserialize<Self, D> + for<'v> CheckBytes<DefaultValidator<'v>>> + 'a
{}

/// Impls for all types
impl<'a, S: ArchiveStore, T: Serialize<S>> ArchiveStorable<S> for T {}

impl<T: Archive + Sized> ArchiveInterpretable for T {}

impl<'a, D: ArchiveDeserializer<'a>, T> ArchiveFetchable<'a, D> for T
where
	T: ArchiveInterpretable + Archive<Archived: Deserialize<Self, D> + for<'v> CheckBytes<DefaultValidator<'v>>> + 'a
{}