use std::{any::Any, borrow::Borrow, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, fmt, hash::Hasher, iter, marker::PhantomData, ops::Deref, sync::Arc};

use bincode::config::NativeEndian;
use bumpalo::Bump;
use bytes::buf::Chain;
use rkyv::{AlignedVec, Archive, Archived, Deserialize, Fallible, Infallible, Resolver, Serialize, ser::{ScratchSpace, Serializer, SharedSerializeRegistry, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, AllocSerializer, FallbackScratch, HeapScratch, SharedSerializeMap, SharedSerializeMapError}}, validation::validators::DefaultValidator, with::{ArchiveWith, DeserializeWith, Immutable, SerializeWith, With}};

use bytecheck::CheckBytes;
use crate::{Data, Datastore, DatastoreDeserializer, DatastoreError, DatastoreLinkSerializer, Hash, LinkSerializer, LinkType, DatastoreSerializer};
// const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: std::hash::Hash + fmt::Debug + Archive<Archived: std::fmt::Debug> + for<'a> Serialize<DatastoreLinkSerializer<'a>> + Serialize<LinkSerializer> + Sized + 'static {
	/// Calculate hash and data from type
	fn store<'a>(&self, db_ser: &mut DatastoreLinkSerializer<'a>) -> TypedHash<Self> {
		db_ser.store(self).unwrap().into()
	}
	fn calc_hash(&self, ser: &mut LinkSerializer) -> TypedHash<Self> {
		ser.hash(self)
	}

	/// List of hashes representing any data that should link to this type
	type LinkIter: LinkIterConstructor<Self> = iter::Empty<Hash>; // OH I LOVE GENERICS
	fn reverse_links(&self) -> Self::LinkIter { LinkIterConstructor::construct(self) }
}
impl NativeHashtype for String {}
impl NativeHashtype for Vec<u8> {}

pub trait LinkIterConstructor<T: NativeHashtype>: Iterator<Item = Hash> {
	fn construct(hashtype: &T) -> Self;
}

impl<'a, T: NativeHashtype> LinkIterConstructor<T> for iter::Empty<Hash> {
	fn construct(_hashtype: &T) -> Self {
		iter::empty()
	}
}


#[derive(Debug, PartialEq, Eq, CheckBytes, Archive, Serialize, Deserialize)]
pub struct TypedHash<T> {
	hash: Hash,
	_type: PhantomData<T>,
}
impl<T> Clone for TypedHash<T> {
	fn clone(&self) -> Self {
		Self { hash: self.hash.clone(), _type: PhantomData::default() }
	}
}

impl<T> From<Hash> for TypedHash<T> {
	fn from(hash: Hash) -> Self { Self { hash, _type: PhantomData::<T>::default() } }
}
impl<T> Into<Hash> for TypedHash<T> {
	fn into(self) -> Hash { self.hash }
}
impl<T> From<&Hash> for &TypedHash<T> {
	// Safety: TypedHash and Hash are equivalent
	fn from(hash: &Hash) -> Self { unsafe { std::mem::transmute(hash) } }
}
impl<'a, T> Into<&'a Hash> for &'a TypedHash<T> {
	fn into(self) -> &'a Hash {
		&self.hash
	}
}

impl<T> TypedHash<T> {
	pub fn new(hash: &Hash) -> Self { TypedHash::<T> { hash: hash.clone(), _type: Default::default() } }
	pub fn cast<R>(&self) -> &TypedHash<R> { unsafe { std::mem::transmute(self) } }
	pub fn untyped(self) -> Hash { self.hash }
	pub fn as_bytes(&self) -> &[u8] { self.hash.as_bytes() }
	pub fn as_hash(&self) -> &Hash { &self.hash }
}
impl<T: NativeHashtype> TypedHash<T> {
	pub fn fetch<'a, L: LinkType<'a, T>, D: DatastoreDeserializer<'a, T, L>>(&self, db: &mut D) -> Result<T, D::Error>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, D>,
	{
		db.fetch(self.into())
	}
}