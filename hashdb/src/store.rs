//! Forms of storage for Disp Expressions
//! Arena Storage (Memory)
//!  - Stores expressions in a bump arena with support for deduplication of new expressions through in-memory hash table lookup
//! Hash Table Storage (Memory)
//!  - Simple HashMap which maps Hash to serialized 
//! Hash Table Storage

use std::fmt;
use std::hash::Hash as StdHash;
use std::ops::Deref;
use std::sync::Arc;
use bytecheck::CheckBytes;
use rkyv::Fallible;
use rkyv::ser::{ScratchSpace, Serializer};
use rkyv::validation::validators::DefaultValidator;
use rkyv::{Archive, Deserialize, Serialize};

mod arena;
mod db;
mod ser;
pub use arena::*;
pub use db::*;
use rkyv::Archived;

use crate::{Link, TypedHash};
use crate::Hash;

pub trait RefFamily<'a> {
	type Member<T>: Ref<'a, T, Family = Self>;
}

pub trait Ref<'a, T: 'a + ?Sized>: Sized + Deref<Target = T> {
	type Family: RefFamily<'a>;
}

struct BoxFamily;
impl RefFamily<'static> for BoxFamily {
	type Member<T> = Box<T>;
}
impl<T> Ref<'static, T> for Box<T> {
	type Family = BoxFamily;
}

struct ArcFamily;
impl RefFamily<'static> for ArcFamily {
	type Member<T> = Arc<T>;
}
impl<T> Ref<'static, T> for Arc<T> {
	type Family = ArcFamily;
}

struct ReferenceFamily;
impl<'a> RefFamily<'a> for ReferenceFamily {
	type Member<T> = &'a T;
}
impl<'a, T> Ref<'a, T> for &'a T {
	type Family = ReferenceFamily;
}

/* impl<'a, T: StdHash, F: RefFamily> StdHash for F::Member<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.deref().hash(state)
	}
}
impl<'a, T: Clone, F: RefFamily> Clone for F::Member<T> {
	fn clone(&self) -> Self {
		self.clone()
	}
}
impl<'a, T: fmt::Debug, F: RefFamily> fmt::Debug for F::Member<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
}
impl<'a, T: fmt::Debug, F: RefFamily> fmt::Debug for F::Member<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
} */

/// Arena Storage in-memory
/// Good for fast accessing, recursive descent and in-memory manipulations of expressions
/// All types that store a T in an arena and return a reference to that T that lasts as long as the arena.
pub trait TypeStore<'a>: Sized {
	type StorableBounds = ();

	/// Reference type of this TypeStore, can be &'a T, or Box<T> or Arc<T> etc.
	type Ref<T>: Ref<'a, T> + where Self: 'a;
	/// Add type to type store
	fn add<T: TypeStorable + 'a>(&'a self, val: T) -> Self::Ref<T>;
	fn link<T: TypeStorable + 'a>(&'a self, val: T) -> Link<'a, T, Self> {
		Link::new(self.add(val))
	}
}

/// All types that can be stored in a LinkArena.
pub trait TypeStorable: StdHash {}

/// TypeStorable is impl'd for unit by default, but can be specialized for certain `TypeStore`s
impl<T: StdHash> TypeStorable for T {}

/// All types that can store a piece of data by its multihash.
pub trait DataStore: DataStoreRead {
	type Data: AsRef<[u8]>;
	/// Store data in table.
	fn add(&mut self, data: Self::Data) -> Hash;
}
/// All types that can retrieve a piece of data using its multihash.
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

/// A type that is implemented to allow for Deserializing an Archive into a TypeStore TS
pub trait ArchiveDeserializer<'a, TS: TypeStore<'a>>: Fallible + Sized {
	fn fetch<T: ArchiveFetchable<'a, TS, Self>>(&mut self, hash: &TypedHash<T>) -> Result<T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>;
	fn fetch_ref<T: ArchiveFetchable<'a, TS, Self> + TypeStorable>(&mut self, hash: &TypedHash<T>) -> Result<<TS as TypeStore<'a>>::Ref<T>, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>;
	fn fetch_link<T: ArchiveFetchable<'a, TS, Self> + TypeStorable>(&mut self, hash: &TypedHash<T>) -> Result<Link<'a, T, TS>, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>;
}


/// A type that is generic over any ArchiveStoreRead and TypeStore and implements ArchiveDeserializer
pub struct ArchiveToType<'a, S: ArchiveStoreRead, TS: TypeStore<'a>> {
	store: &'a S,
	type_store: &'a TS
}
impl<'a, S: ArchiveStoreRead, TS: TypeStore<'a>> From<(&'a S, &'a TS)> for ArchiveToType<'a, S, TS> {
	fn from(pair: (&'a S, &'a TS)) -> Self {
		Self { store: pair.0, type_store: pair.1 }
	}
}
impl<'a, S: ArchiveStoreRead, TS: TypeStore<'a>> Fallible for ArchiveToType<'a, S, TS> {
	type Error = S::Error;
}
impl<'a, S: ArchiveStoreRead, TS: TypeStore<'a>> ArchiveDeserializer<'a, TS> for ArchiveToType<'a, S, TS> {
	fn fetch<T: ArchiveFetchable<'a, TS, Self>>(&mut self, hash: &TypedHash<T>) -> Result<T, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>
	{
		let archive: &Archived<T> = self.store.fetch_archive::<T>(hash)?;
		Ok(Deserialize::deserialize(archive, self)?)
	}
	fn fetch_ref<T: ArchiveFetchable<'a, TS, Self> + TypeStorable>(&mut self, hash: &TypedHash<T>) -> Result<<TS as TypeStore<'a>>::Ref<T>, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>
	{
		let item = self.fetch(hash)?;
		Ok(self.type_store.add(item))
	}
	fn fetch_link<T: ArchiveFetchable<'a, TS, Self> + TypeStorable>(&mut self, hash: &TypedHash<T>) -> Result<Link<'a, T, TS>, <Self as Fallible>::Error>
	where
		T: Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>
	{
		let item = self.fetch(hash)?;
		Ok(self.type_store.link(item))
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

/// A type that can be fetched in its Archived form from an ArchiveStore for lifetime 'a and can be placed into a TypeStore<'a> for lifetime 'a
pub trait ArchiveFetchable<'a, TS: TypeStore<'a>, D: ArchiveDeserializer<'a, TS>>:
	ArchiveInterpretable + TypeStorable + Archive<Archived: Deserialize<Self, D> + for<'v> CheckBytes<DefaultValidator<'v>>> + Sized + 'a
{}

/// Impls for all types
impl<'a, S: ArchiveStore, T: Serialize<S>> ArchiveStorable<S> for T {}
impl<T> ArchiveInterpretable for T
where
	T: Archive + Sized,
{}

impl<'a, TS: TypeStore<'a>, D: ArchiveDeserializer<'a, TS>, T> ArchiveFetchable<'a, TS, D> for T
where
	T: ArchiveInterpretable + TypeStorable + Archive<Archived: Deserialize<Self, D> + for<'v> CheckBytes<DefaultValidator<'v>>> + Sized + 'a
{}