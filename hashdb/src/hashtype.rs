use std::{borrow::Borrow, collections::HashMap, fmt, iter, marker::PhantomData, ops::Deref, sync::Arc};

use bincode::config::NativeEndian;
use bytes::buf::Chain;
use rkyv::{AlignedVec, Archive, Archived, Deserialize, Fallible, Infallible, Resolver, Serialize, ser::{ScratchSpace, Serializer, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, AllocSerializer, FallbackScratch, HeapScratch, SharedSerializeMap}}, validation::validators::DefaultValidator, with::{ArchiveWith, DeserializeWith, Immutable, SerializeWith, With}};

use bytecheck::CheckBytes;
use crate::{Data, Datastore, Hash, db::{DatastoreError}};
// const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: fmt::Debug + Archive<Archived: std::fmt::Debug> + for<'db> Serialize<HashSerializer<'db>> + Sized + 'static {
	/// Calculate hash and data from type
	fn store(&self, db: &mut Datastore) -> TypedHash<Self> {
		HashSerializer::new(db).hash(self).unwrap()
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


#[derive(Debug, PartialEq, Eq, Clone, CheckBytes)]
pub struct TypedHash<T: NativeHashtype> {
	hash: Hash,
	_type: PhantomData<T>,
}
impl<T: NativeHashtype> Archive for TypedHash<T> {
	type Resolver = [(); Hash::len()];
	type Archived = TypedHash<T>;
	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		self.hash.resolve(pos, resolver, out.cast())
	}
}
impl<T: NativeHashtype, S: ScratchSpace + Serializer + ?Sized> Serialize<S> for TypedHash<T> {
	fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		self.hash.serialize(serializer)
	}
}

impl<T: NativeHashtype> From<Hash> for TypedHash<T> {
	fn from(hash: Hash) -> Self { Self { hash, _type: PhantomData::<T>::default() } }
}
impl<T: NativeHashtype> Into<Hash> for TypedHash<T> {
	fn into(self) -> Hash { self.hash }
}
impl<T: NativeHashtype> From<&Hash> for &TypedHash<T> {
	// Safety: TypedHash and Hash are equivalent
	fn from(hash: &Hash) -> Self { unsafe { std::mem::transmute(hash) } }
}
impl<'a, T: NativeHashtype> Into<&'a Hash> for &'a TypedHash<T> {
	fn into(self) -> &'a Hash {
		&self.hash
	}
}
impl<T: NativeHashtype> TypedHash<T> {
	pub fn new(hash: &Hash) -> Self { TypedHash::<T> { hash: hash.clone(), _type: Default::default() } }
	pub fn cast<R: NativeHashtype>(self) -> TypedHash<R> { self.hash.into() }
	pub fn as_bytes(&self) -> &[u8] { self.hash.as_bytes() }
	pub fn as_hash(&self) -> &Hash { &self.hash }
	pub fn fetch<'a>(&self, mut db: &'a Datastore) -> Result<Arc<T>, DatastoreError>
	where
		<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, &'a Datastore>,
	{
		HashType::<T>::deserialize_with(self, &mut db)
	}
}


/// Wrapper type that serializes references to NativeHashtype as Hashes
#[derive(Default)]
pub struct HashType<T: NativeHashtype> {
	_type: PhantomData<T>,
}


#[derive(Debug, Error)]
pub enum HashSerializeError {
	#[error("scratch error: {0}")]
	AllocScratchError(#[from] AllocScratchError),
	#[error("data store error: {0}")]
	DatastoreError(#[from] DatastoreError),
}

/// Serialize linked objects into hashtypes linked by hashes
pub struct HashSerializer<'a> {
	scratch: FallbackScratch<HeapScratch<1024>, AllocScratch>, // Scratch space
	serializer: AlignedSerializer<AlignedVec>, // Serializer
	pointer_map: HashMap<*const (), (Hash, usize)>, // Check for shared pointers to avoid serializing the same object twice
	pub db: &'a mut Datastore
}
impl<'db> HashSerializer<'db> {
	pub fn new(db: &'db mut Datastore) -> Self {
		Self {
			scratch: Default::default(),
			serializer: Default::default(),
			pointer_map: Default::default(),
			db,
		}
	}
	/// Hash the data of the passed reference
	pub fn hash<T: NativeHashtype>(&mut self, hashtype: impl Borrow<T>) -> Result<TypedHash<T>, HashSerializeError> {
		let ptr = (hashtype.borrow() as *const T).cast::<()>();
		Ok(if let Some((hash, _)) = self.pointer_map.get(&ptr) {
			hash.clone().into()
		} else {
			let _pos = self.serialize_value(hashtype.borrow())?;
			let vec = self.get_vec().into();
			self.db.store(Data::new(vec)).into()
		})
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

impl<T: NativeHashtype, B: Borrow<T>> ArchiveWith<B> for HashType<T> {
    type Archived = Hash;
    type Resolver = TypedHash<T>;

    unsafe fn resolve_with(field: &B, pos: usize, resolver: TypedHash<T>, out: *mut Self::Archived) {
        resolver.resolve(pos, [(); Hash::len()], out.cast());
    }
}

// Serialize reference of T -> Hash of T
impl<'db, T: NativeHashtype> SerializeWith<Arc<T>, HashSerializer<'db>> for HashType<T> {
    fn serialize_with(field: &Arc<T>, serializer: &mut HashSerializer<'db>) -> Result<Self::Resolver, HashSerializeError> {
		Ok(serializer.hash((*field).borrow())?)
    }
}

/// Deserialize Hash of T -> reference of T
impl<'db, T: NativeHashtype> DeserializeWith<TypedHash<T>, Arc<T>, &'db Datastore> for HashType<T>
where
	<T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>> + Deserialize<T, &'db Datastore>,
{
    fn deserialize_with(field: &TypedHash<T>, deserializer: &mut &'db Datastore) -> Result<Arc<T>, DatastoreError> {
		let archive = deserializer.fetch::<T>(field)?;
		let t = archive.deserialize(deserializer).unwrap();
        Ok(Arc::new(t))
    }
}
