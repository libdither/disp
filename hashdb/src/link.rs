use std::{any::Any, borrow::Borrow, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, fmt::Debug, hash::{Hash as StdHash, Hasher}, marker::PhantomData, ops::Deref, sync::Arc};

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Fallible, Serialize, validation::validators::DefaultValidator};

use crate::{Datastore, DatastoreDeserializer, DatastoreSerializer, Hash, HashDeserializer, NativeHashtype};

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Link<'a, T>(&'a T);

impl<'a, T: PartialEq<T>> PartialEq<T> for Link<'a, T> {
    fn eq(&self, other: &T) -> bool {
        self.deref() == other
    }
}

impl<'a, T: NativeHashtype> NativeHashtype for Link<'a, T> {}

impl<'a, T> Link<'a, T> {
	pub fn new(link: &'a T) -> Link<'a, T> {
		Link(link)
	}
}

impl<'a, T> Deref for Link<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.0.borrow()
	}
}
impl<'a, T> AsRef<T> for Link<'a, T> {
	fn as_ref(&self) -> &T {
		self.0.borrow()
	}
}

#[derive(Debug)]
pub struct ArchivedLink<T>(Hash, PhantomData<T>);

impl<T> From<Hash> for ArchivedLink<T> { fn from(hash: Hash) -> Self { Self(hash, PhantomData::default()) } }

impl<'a, 'db, T: NativeHashtype> Archive for Link<'a, T> {
	type Archived = ArchivedLink<T>;

	type Resolver = Hash;

	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}

impl<'a, 'db, T: NativeHashtype, __S: DatastoreSerializer + ?Sized> Serialize<__S> for Link<'a, T>
where T: Serialize<__S>,
{
	fn serialize(&self, serializer: &mut __S) -> Result<Self::Resolver, <__S as Fallible>::Error> {
		Ok(serializer.store::<T>(self.0.borrow())?.into())
	}
}
impl<'a, T: NativeHashtype, __D: DatastoreDeserializer<'a>> Deserialize<Link<'a, T>, __D> for ArchivedLink<T>
where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, __D>,
{
	fn deserialize(&self, deserializer: &mut __D) -> Result<Link<'a, T>, <__D as Fallible>::Error> {
		deserializer.fetch::<T>(&self.0)
	}
}

impl<__C: ?Sized, T: NativeHashtype> CheckBytes<__C> for ArchivedLink<T> {
	type Error = <Hash as CheckBytes<__C>>::Error;

	unsafe fn check_bytes<'a>(value: *const Self, context: &mut __C)
		-> Result<&'a Self, Self::Error> {
		let ret = Hash::check_bytes(value.cast(), context)?;
		return Ok(&*(ret as *const Hash).cast())
	}
}