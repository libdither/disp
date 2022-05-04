use std::{fmt::Debug, hash::Hash as StdHash, iter, marker::PhantomData};

use rkyv::{
	validation::validators::DefaultValidator,
	with::{ArchiveWith, DeserializeWith, SerializeWith},
	Archive, Deserialize, Fallible, Serialize,
};

use crate::{Datastore, DatastoreDeserializer, DatastoreSerializer, Hash, HashDeserializer, LinkArena};
use bytecheck::CheckBytes;

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: StdHash + Debug + Archive + Sized {
	/// Calculate hash and data from type
	fn store<S: DatastoreSerializer>(&self, ser: &mut S) -> TypedHash<Self>
	where
		Self: Serialize<S>,
	{
		ser.store(self).map_err(|_| "failed to serialize").unwrap().into()
	}

	/// List of hashes representing any data that should link to this type
	type LinkIter<S: DatastoreSerializer>: HashIterConstructor<S, Self> = iter::Empty<Hash>; // OH I LOVE GENERICS
	fn reverse_links<S: DatastoreSerializer>(&self, ser: &mut S) -> Self::LinkIter<S> {
		HashIterConstructor::construct(self, ser)
	}
}

/* impl<T> NativeHashtype for T
where T: StdHash + fmt::Debug + Archive<Archived: fmt::Debug>
{} */
impl NativeHashtype for String {}
impl NativeHashtype for Vec<u8> {}

pub trait HashIterConstructor<S: DatastoreSerializer, T: NativeHashtype>: Iterator<Item = Hash> {
	fn construct(hashtype: &T, ser: &mut S) -> Self;
}

impl<'a, S: DatastoreSerializer, T: NativeHashtype> HashIterConstructor<S, T> for iter::Empty<Hash> {
	fn construct(_hashtype: &T, _ser: &mut S) -> Self {
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
impl<T: NativeHashtype> TypedHash<T> {
	pub fn fetch<'a>(&self, db: &'a Datastore, arena: &'a LinkArena<'a>) -> Result<&'a T, <HashDeserializer<'a> as Fallible>::Error>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, HashDeserializer<'a>>,
	{
		let de = &mut HashDeserializer { db, arena };
		de.fetch(self.into())
	}
}

#[derive(Debug)]
pub struct ArchivedLink<T>(Hash, PhantomData<T>);

impl<T> From<Hash> for ArchivedLink<T> {
	fn from(hash: Hash) -> Self {
		Self(hash, PhantomData::default())
	}
}

impl<__C: ?Sized, T: NativeHashtype> CheckBytes<__C> for ArchivedLink<T> {
	type Error = <Hash as CheckBytes<__C>>::Error;

	unsafe fn check_bytes<'a>(value: *const Self, context: &mut __C) -> Result<&'a Self, Self::Error> {
		let ret = Hash::check_bytes(value.cast(), context)?;
		return Ok(&*(ret as *const Hash).cast());
	}
}

pub struct HashType;

impl<'a, F: NativeHashtype> ArchiveWith<&'a F> for HashType {
	type Archived = ArchivedLink<F>;
	type Resolver = Hash;

	#[inline]
	unsafe fn resolve_with(_field: &&'a F, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}

impl<'a, F: NativeHashtype + Serialize<S>, S: DatastoreSerializer + ?Sized> SerializeWith<&'a F, S> for HashType {
	#[inline]
	fn serialize_with(field: &&'a F, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		Ok(serializer.store::<F>(field)?.into())
	}
}

impl<'a, F: Archive + NativeHashtype, D: DatastoreDeserializer<'a>> DeserializeWith<ArchivedLink<F>, &'a F, D> for HashType
where
	ArchivedLink<F>: Deserialize<&'a F, D> + for<'v> CheckBytes<DefaultValidator<'v>>,
	F::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<F, D>,
{
	#[inline]
	fn deserialize_with(field: &ArchivedLink<F>, deserializer: &mut D) -> Result<&'a F, D::Error> {
		deserializer.fetch::<F>(&field.0)
	}
}

impl<'a, T: NativeHashtype, __D: DatastoreDeserializer<'a>> Deserialize<&'a T, __D> for ArchivedLink<T>
where
	<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, __D>,
{
	fn deserialize(&self, deserializer: &mut __D) -> Result<&'a T, <__D as Fallible>::Error> {
		deserializer.fetch::<T>(&self.0)
	}
}
