use std::marker::PhantomData;

use rkyv::{Archive, Deserialize, Fallible, Serialize, with::{ArchiveWith, DeserializeWith, SerializeWith}};
use bytecheck::CheckBytes;

use crate::{Hash, store::{ArchiveDeserializer, ArchiveFetchable, ArchiveStorable, ArchiveStore, ArchiveStoreRead, TypeStorable, TypeStore}};

#[derive(Debug, PartialEq, Eq, CheckBytes, Archive, Serialize, Deserialize)]
pub struct TypedHash<T> {
	hash: Hash,
	_type: PhantomData<T>,
}
impl<T> Clone for TypedHash<T> {
	fn clone(&self) -> Self {
		Self {
			hash: self.hash.clone(),
			_type: PhantomData::default(),
		}
	}
}

impl<T> From<Hash> for TypedHash<T> {
	fn from(hash: Hash) -> Self {
		Self {
			hash,
			_type: PhantomData::<T>::default(),
		}
	}
}
impl<T> Into<Hash> for TypedHash<T> {
	fn into(self) -> Hash {
		self.hash
	}
}
impl<T> From<&Hash> for &TypedHash<T> {
	// Safety: TypedHash and Hash are equivalent
	fn from(hash: &Hash) -> Self {
		unsafe { std::mem::transmute(hash) }
	}
}
impl<'a, T> Into<&'a Hash> for &'a TypedHash<T> {
	fn into(self) -> &'a Hash {
		&self.hash
	}
}

impl<T> TypedHash<T> {
	pub fn new(hash: &Hash) -> Self {
		TypedHash::<T> {
			hash: hash.clone(),
			_type: Default::default(),
		}
	}
	pub fn cast<R>(&self) -> &TypedHash<R> {
		unsafe { &*(self as *const TypedHash<T>).cast::<TypedHash<R>>() }
	}
	pub fn untyped(self) -> Hash {
		self.hash
	}
	pub fn as_bytes(&self) -> &[u8] {
		self.hash.as_bytes()
	}
	pub fn as_hash(&self) -> &Hash {
		&self.hash
	}
}

impl<A> TypedHash<A> {
	// Resolve TypedHash using Datastore and add to TypeStore
	pub fn fetch<'s, 'a: 's, Store, Arena, T>(&self, db: &'s Store, arena: &'a Arena) -> Result<&'a T, <Store as ArchiveStoreRead>::ArchiveError>
	where
		Store: ArchiveStoreRead,
		Arena: TypeStore<'a> + 'a,
		T: TypeStorable + ArchiveFetchable<'s, 'a, Store, Arena> + Archive<Archived = A>,
	{
		let mut deserializer: ArchiveDeserializer<'s, 'a, Store, Arena> = (db, arena).into();
		Ok(arena.add(deserializer.fetch(self)?))
	}
}

#[derive(Debug)]
pub struct ArchivedLink<T>(Hash, PhantomData<T>);

impl<T> From<Hash> for ArchivedLink<T> {
	fn from(hash: Hash) -> Self {
		Self(hash, PhantomData::default())
	}
}

impl<__C: ?Sized, T: Archive<Archived: CheckBytes<__C>>> CheckBytes<__C> for ArchivedLink<T> {
	type Error = <Hash as CheckBytes<__C>>::Error;

	unsafe fn check_bytes<'a>(value: *const Self, context: &mut __C) -> Result<&'a Self, Self::Error> {
		let ret = Hash::check_bytes(value.cast(), context)?;
		return Ok(&*(ret as *const Hash).cast());
	}
}

pub struct HashType;

// impl Archive for any With<&'a T, HashType> if T can be archived itself.
impl<'a, T: Archive> ArchiveWith<&'a T> for HashType {
	type Archived = ArchivedLink<T>;
	type Resolver = Hash;

	#[inline]
	unsafe fn resolve_with(_field: &&'a T, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}

// impl Serialize for any With<&'a T, HashType> if Archived<T> can be stored into an ArchiveStore
impl<'a, S: ArchiveStore, T: ArchiveStorable<S>> SerializeWith<&'a T, S> for HashType {
	#[inline]
	fn serialize_with(field: &&'a T, serializer: &mut S) -> Result<Self::Resolver, <S as Fallible>::Error> {
		Ok(serializer.store::<T>(field)?.into())
	}
}

/// impl Deserialize for any With<&'a T, HashType> if T has an archived form which can be deserialized into a TypeStore for some lifetime &'a T. 
impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a> + 'a, T: ArchiveFetchable<'s, 'a, S, A> + TypeStorable> DeserializeWith<ArchivedLink<T>, &'a T, ArchiveDeserializer<'s, 'a, S, A>> for HashType {
	#[inline]
	fn deserialize_with(field: &ArchivedLink<T>, deserializer: &mut ArchiveDeserializer<'s, 'a, S, A>) -> Result<&'a T, <ArchiveDeserializer<'s, 'a, S, A> as Fallible>::Error> {
		deserializer.fetch_ref(&field.0)
	}
}

impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a> + 'a, T: ArchiveFetchable<'s, 'a, S, A> + TypeStorable> Deserialize<&'a T, ArchiveDeserializer<'s, 'a, S, A>> for ArchivedLink<T> {
	fn deserialize(&self, deserializer: &mut ArchiveDeserializer<'s, 'a, S, A>) -> Result<&'a T, <ArchiveDeserializer<'s, 'a, S, A> as Fallible>::Error> {
		deserializer.fetch_ref(&self.0)
	}
}