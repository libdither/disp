use std::marker::PhantomData;
use bytecheck::CheckBytes;
use rkyv::{Archive, Serialize, Deserialize, Fallible, validation::validators::DefaultValidator};

use crate::{ArchiveFetchable, ArchiveStore, Hash, Link, TypeStorable, TypeStore, store::{ArchiveDeserializer, ArchiveInterpretable, ArchiveToType}};

#[derive(Derivative, Eq, Archive, Serialize, Deserialize)]
#[derivative(Clone(bound=""), Debug(bound=""), PartialEq(bound=""))]
pub struct TypedHash<T> {
	hash: Hash,
	#[derivative(Debug="ignore")]
	_type: PhantomData<T>,
}
impl<__C: ?Sized, T: ArchiveInterpretable> CheckBytes<__C> for TypedHash<T> {
	type Error = <Hash as CheckBytes<__C>>::Error;

	unsafe fn check_bytes<'a>(value: *const Self, context: &mut __C) -> Result<&'a Self, Self::Error> {
		let ret = Hash::check_bytes(value.cast(), context)?;
		return Ok(&*(ret as *const Hash).cast());
	}
}

impl<T> Into<Hash> for TypedHash<T> {
	fn into(self) -> Hash {
		self.hash
	}
}
impl<'a, T> Into<&'a Hash> for &'a TypedHash<T> {
	fn into(self) -> &'a Hash {
		&self.hash
	}
}

impl<T> TypedHash<T> {
	pub unsafe fn new(hash: Hash) -> Self {
		TypedHash::<T> { hash: hash, _type: Default::default() }
	}
	pub unsafe fn cast<R>(&self) -> &TypedHash<R> {
		&*(self as *const TypedHash<T>).cast::<TypedHash<R>>()
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

impl<T: ArchiveInterpretable> TypedHash<T> {
	// Resolve TypedHash using Datastore and add to TypeStore<'a>
	pub fn fetch<'a, AS, TS>(&self, db: &'a AS, store: &'a TS) -> Result<Link<'a, T, TS>, <AS as Fallible>::Error>
	where
		T: ArchiveFetchable<'a, TS, ArchiveToType<'a, AS, TS>> + Archive<Archived: for<'v> CheckBytes<DefaultValidator<'v>>>,
		AS: ArchiveStore,
		TS: TypeStore<'a>,
	{
		let mut deserializer: ArchiveToType<'a, AS, TS> = (db, store).into();
		deserializer.fetch_link(self)
	}
}