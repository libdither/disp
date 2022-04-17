use std::{any::Any, borrow::Borrow, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, fmt::Debug, hash::{Hash as StdHash, Hasher}, marker::PhantomData, ops::Deref, sync::Arc};

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Fallible, Serialize, validation::validators::DefaultValidator};

use crate::{Datastore, DatastoreDeserializer, DatastoreSerializer, Hash, LinkArena, NativeHashtype};

pub trait LinkType<'a> {
	type Storage;
	type Link<V: 'a>: Borrow<V>;
	fn wrap<V: 'a + StdHash>(val: V, c: &'a mut Self::Storage) -> Self::Link<V>;
}
impl<T> LinkType<'static> for Arc<T> {
	type Storage = ();
	type Link<V: 'static> = Arc<V>;
	fn wrap<V: 'static>(val: V, c: &'static mut Self::Storage) -> Self::Link<V> { Arc::new(val) }
}
impl<'a, T: StdHash> LinkType<'a> for &'a T {
	type Storage = LinkArena<'a>;
	type Link<V: 'a> = &'a V;
	fn wrap<V: 'a + StdHash>(val: V, c: &'a mut Self::Storage) -> Self::Link<V> { c.alloc_dedup(val) }
}

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Link<'a, T, L: LinkType<'a>>(L::Link<T>, PhantomData<&'a T>);

impl<'a, T: NativeHashtype, L: LinkType<'a> + NativeHashtype> NativeHashtype for Link<'a, T, L>
where L::Link<T>: StdHash + Debug
{}


pub type LinkArc<T> = Link<'static, T, Arc<T>>;

impl<T> From<Arc<T>> for LinkArc<T> { fn from(a: Arc<T>) -> Self { Self(a, Default::default()) } }
impl<T> LinkArc<T> {
	pub fn arc(t: T) -> LinkArc<T> {
		Link(Arc::new(t), Default::default())
	}
}

pub type LinkRef<'a, T> = Link<'a, T, &'a T>;

impl<'a, T, L: LinkType<'a>> Link<'a, T, L> {
	pub fn new(link: L::Link<T>) -> Link<'a, T, L> {
		Link(link, Default::default())
	}
}

impl<'a, T, L: LinkType<'a>> Deref for Link<'a, T, L> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.0.borrow()
	}
}
impl<'a, T, L: LinkType<'a>> AsRef<T> for Link<'a, T, L> {
	fn as_ref(&self) -> &T {
		self.0.borrow()
	}
}

#[derive(Debug)]
pub struct ArchivedLink<T>(Hash, PhantomData<T>);

impl<T> From<Hash> for ArchivedLink<T> { fn from(hash: Hash) -> Self { Self(hash, PhantomData::default()) } }

impl<'a, 'db, T: NativeHashtype, L: LinkType<'a>> Archive for Link<'a, T, L> {
	type Archived = ArchivedLink<T>;

	type Resolver = Hash;

	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}

impl<'a, 'db, T: NativeHashtype, L: LinkType<'a>, __S: DatastoreSerializer + ?Sized> Serialize<__S> for Link<'a, T, L>
where T: Serialize<__S>,
{
	fn serialize(&self, serializer: &mut __S) -> Result<Self::Resolver, <__S as Fallible>::Error> {
		Ok(serializer.store::<T>(self.0.borrow())?.into())
	}
}
impl<'a, 'db, T: NativeHashtype, L: LinkType<'a>, __D: DatastoreDeserializer<'a, LinkL<T> = L> + ?Sized> Deserialize<Link<'a, T, L>, __D> for ArchivedLink<T>
where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, __D>,
{
	fn deserialize(&self, deserializer: &mut __D) -> Result<Link<'a, T, L>, <__D as Fallible>::Error> {
		let hash = &self.0;
		let t = deserializer.fetch::<T>(hash)?;
		Ok(deserializer.alloc(t))
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