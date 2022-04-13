use std::{borrow::Borrow, collections::HashMap, fmt, iter, marker::PhantomData, ops::Deref, sync::Arc};

use bincode::config::NativeEndian;
use bytes::buf::Chain;
use rkyv::{AlignedVec, Archive, Archived, Deserialize, Fallible, Infallible, Resolver, Serialize, ser::{ScratchSpace, Serializer, SharedSerializeRegistry, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, AllocSerializer, FallbackScratch, HeapScratch, SharedSerializeMap, SharedSerializeMapError}}, validation::validators::DefaultValidator, with::{ArchiveWith, DeserializeWith, Immutable, SerializeWith, With}};

use bytecheck::CheckBytes;
use crate::{Data, Datastore, Hash, db::{DatastoreError}};
// const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: fmt::Debug + Archive<Archived: std::fmt::Debug> + for<'db> Serialize<HashSerializer<'db>> + Sized + 'static {
	/// Calculate hash and data from type
	fn store(&self, ser: &mut HashSerializer) -> TypedHash<Self> {
		ser.store(self).unwrap().into()
	}

	/// List of hashes representing any data that should link to this type
	type LinkIter<'a>: LinkIterConstructor<'a, Self> = iter::Empty<&'a Hash>; // OH I LOVE GENERICS
	fn reverse_links<'a>(&'a self) -> Self::LinkIter<'a> { LinkIterConstructor::construct(self) }
}
impl NativeHashtype for String {}
impl NativeHashtype for Vec<u8> {}

pub trait LinkIterConstructor<'a, T: NativeHashtype>: Iterator<Item = &'a Hash> {
	fn construct(hashtype: &'a T) -> Self;
}

impl<'a, T: NativeHashtype> LinkIterConstructor<'a, T> for iter::Empty<&'a Hash> {
	fn construct(_hashtype: &'a T) -> Self {
		iter::empty()
	}
}


#[derive(Debug, PartialEq, Eq, CheckBytes, Archive, Serialize, Deserialize)]
pub struct TypedHash<T> {
	hash: Hash,
	_type: PhantomData<T>,
}
/* impl<T> Clone for TypedHash<T> {
	fn clone(&self) -> Self {
		Self { hash: self.hash.clone(), _type: PhantomData::default() }
	}
} */
/* impl<T> Archive for TypedHash<T> {
	type Resolver = [(); Hash::len()];
	type Archived = TypedHash<T>;
	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		self.hash.resolve(pos, resolver, out.cast())
	}
}
impl<T, S: ScratchSpace + Serializer + ?Sized> Serialize<S> for TypedHash<T> {
	fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		self.hash.serialize(serializer)
	}
}
impl<T, D: Fallible + ?Sized> Deserialize<TypedHash<T>, D> for Archived<TypedHash<T>> {
	fn deserialize(&self, deserializer: &mut D) -> Result<TypedHash<T>, D::Error> {
		self.hash.deserialize(deserializer).map(|t: Hash|t.into())
	}
} */

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
	pub fn fetch<'db>(&self, mut db: &'db Datastore) -> Result<T, DatastoreError>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, &'db Datastore>,
	{
		db.fetch(self.into())
	}
}

pub trait DatastoreSerializer: Fallible {
	fn store<T: NativeHashtype>(&mut self, hashtype: &T) -> Result<Hash, <Self as Fallible>::Error> where T: Serialize<Self>;
}
pub trait DatastoreDeserializer: Fallible<Error: From<DatastoreError>> {
	fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, <Self as Fallible>::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>;
}

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Link<T>(Arc<T>);

impl<T> From<Arc<T>> for Link<T> { fn from(a: Arc<T>) -> Self { Self(a) } }
/// Super-cool dependently typed version
/// set-from (a => Link::first a)

#[derive(Debug)]
pub struct ArchivedLink<T>(Hash, PhantomData<T>);

impl<T> From<Hash> for ArchivedLink<T> { fn from(hash: Hash) -> Self { Self(hash, PhantomData::default()) } }

impl<'db, T: NativeHashtype> Archive for Link<T> {
	type Archived = ArchivedLink<T>;

	type Resolver = Hash;

	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}
impl<'db, T: NativeHashtype, __S: DatastoreSerializer + ?Sized> Serialize<__S> for Link<T>
where T: Serialize<__S>,
{
	fn serialize(&self, serializer: &mut __S) -> Result<Self::Resolver, <__S as Fallible>::Error> {
		Ok(serializer.store::<T>(self.0.deref())?.into())
	}
}
impl<'db, T: NativeHashtype, __D: DatastoreDeserializer + ?Sized> Deserialize<Link<T>, __D> for ArchivedLink<T>
where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, __D>,
{
	fn deserialize(&self, deserializer: &mut __D) -> Result<Link<T>, <__D as Fallible>::Error> {
		let hash = &self.0;
		let t = deserializer.fetch::<T>(hash)?;
		Ok(Link(Arc::new(t)))
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

#[derive(Debug, Error)]
pub enum HashSerializeError {
	#[error("scratch error: {0}")]
	AllocScratchError(#[from] AllocScratchError),
	#[error("data store error: {0}")]
	DatastoreError(#[from] DatastoreError),
	#[error("shared map error: {0}")]
	SharedSerializeMapError(#[from] SharedSerializeMapError)
}

/// Serialize linked objects into hashtypes linked by hashes
pub struct HashSerializer<'a> {
	scratch: FallbackScratch<HeapScratch<1024>, AllocScratch>, // Scratch space
	serializer: AlignedSerializer<AlignedVec>, // Serializer
	pointer_map: HashMap<*const (), (Hash, usize)>, // Check for shared pointers to avoid serializing the same object twice
	reverse_links: Vec<Hash>,
	pub db: &'a mut Datastore,
}

impl<'db> DatastoreSerializer for HashSerializer<'db> {
    fn store<T: NativeHashtype>(&mut self, hashtype: &T) -> Result<Hash, Self::Error> {
        let ptr = (hashtype as *const T).cast::<()>();
		Ok(if let Some((hash, _)) = self.pointer_map.get(&ptr) {
			hash.clone().into()
		} else {
			let _pos = self.serialize_value(hashtype)?;
			let vec = self.get_vec();
			// Safety: We just serialized this data, therefore it is a valid archive
			let data = unsafe { Data::new_typed_unsafe::<T>(vec.into()) };
			let hash = self.db.store(data).into();
			self.db.register::<T>(hashtype.reverse_links(), &hash);
			hash
		})
    }
}
impl<'db> HashSerializer<'db> {
	pub fn new(db: &'db mut Datastore) -> Self {
		Self {
			scratch: Default::default(),
			serializer: Default::default(),
			pointer_map: Default::default(),
			reverse_links: Default::default(),
			db,
		}
	}
	pub fn get_vec(&mut self) -> AlignedVec {
		std::mem::take(&mut self.serializer).into_inner()
	}
}

impl<'db> Fallible for HashSerializer<'db> {
	type Error = HashSerializeError;
}

impl<'db> ScratchSpace for HashSerializer<'db> {
	unsafe fn push_scratch(&mut self, layout: std::alloc::Layout) -> Result<std::ptr::NonNull<[u8]>, Self::Error> {
		Ok(self.scratch.push_scratch(layout)?)
	}

	unsafe fn pop_scratch(&mut self, ptr: std::ptr::NonNull<u8>, layout: std::alloc::Layout) -> Result<(), Self::Error> {
		Ok(self.scratch.pop_scratch(ptr, layout)?)
	}
}

impl<'db> Serializer for HashSerializer<'db> {
	fn pos(&self) -> usize {
		self.serializer.pos()
	}

	fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		Ok(self.serializer.write(bytes).unwrap())
	}
}

impl<'db> DatastoreDeserializer for &'db Datastore {
    fn fetch<T: NativeHashtype>(&mut self, hash: &Hash) -> Result<T, DatastoreError>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, Self>
	{
        let data = Datastore::get(*self, hash.into())?;
		let result = data.archived::<T>()?;
		let ret = result.deserialize(self)?;
		Ok(ret)
    }
}