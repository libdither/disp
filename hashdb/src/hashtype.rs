use std::{fmt, iter, marker::PhantomData};

use rkyv::{Archive, Archived, Serialize, ser::{ScratchSpace, Serializer, serializers::AllocSerializer}, validation::validators::DefaultValidator};

use bytecheck::CheckBytes;
use crate::{Data, Datastore, Hash, db::{DatastoreError}};
// const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);

/* 
#[derive(Debug, Error)]
pub enum HashtypeResolveError {
	#[error("Datastore error: {0}")]
	DatastoreError(#[from] DatastoreError),
	#[error("Invalid Layout: {0}")]
	InvalidLayout(Hash),
	#[error("Invalid Link Type: {0:?}")]
	InvalidLinkType(TypeId),
	#[error("Reader Error")]
	ReaderError(#[from] std::io::Error),

	#[error("Deserialization Error")]
	DeserializeError(#[from] bincode::Error),
} */

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: fmt::Debug + Archive<Archived: std::fmt::Debug> + Serialize<AllocSerializer::<0>> + Sized + 'static {
	fn archive(&self) -> Data {
		let mut serializer = AllocSerializer::<0>::default();
		serializer.serialize_value(self).unwrap();
		let data = serializer.into_serializer().into_inner();
		// Safety, serialized data is valid because it was just serialized
		unsafe { Data::new_typed_unsafe::<Self>(data.into()) }
	}

	/// Calculate hash and data from type
	fn hash(&self) -> TypedHash<Self> {
		let mut serializer = AllocSerializer::<0>::default();
		serializer.serialize_value(self).unwrap();
		let data = serializer.into_serializer().into_inner();
		Hash::hash(&data).into()
	}

	/// Serialize & store in db
	fn store<'db>(self, db: &'db mut Datastore) -> TypedHash<Self>
	where Self::Archived: CheckBytes<DefaultValidator<'db>>
	{ db.add(self).0.into() }

	/// List of hashes representing any data that should link to this type
	type LinkIter<'a>: LinkIterConstructor<'a, Self> = iter::Empty<&'a Hash>; // OH I LOVE GENERICS
	fn reverse_links<'a>(&'a self) -> Self::LinkIter<'a> { LinkIterConstructor::construct(self) }
}

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
	pub fn fetch<'a>(&self, db: &'a Datastore) -> Result<&'a Archived<T>, DatastoreError>
	where
		<T as Archive>::Archived: CheckBytes<DefaultValidator<'a>>
	{ db.fetch(self) }
	/* pub fn fetch_cloned<'a>(&self, db: &'a Datastore) -> Result<&'a Archived<T>, DatastoreError>
	where
		<T as Archive>::Archived: CheckBytes<DefaultValidator<'a>>
	{ db.get(self.as_hash()). } */
	
}
/* 
#[derive(Debug, Error)]
#[error("Invalid hash: {0}")]
pub struct InvalidHashError(Hash);

/// Hashtype is a type that can go to an from being a hash
pub trait PrimitiveHashtype: Sized + 'static {
	fn to_hash(&self) -> TypedHash<Self>;
	fn from_hash(hash: &TypedHash<Self>) -> Result<Self, InvalidHashError>;
}

pub struct RustTypeDef;
impl PrimitiveHashtype for RustTypeDef {
	fn to_hash(&self) -> TypedHash<Self> { RUST_TYPE.into() }
	fn from_hash(hash: &TypedHash<Self>) -> Result<Self, InvalidHashError> {
		let hash = hash.untyped_ref();
		if *hash == RUST_TYPE { Ok(RustTypeDef) }
		else { Err(InvalidHashError(hash.clone()))? }
	}
}

impl<T: PrimitiveHashtype> Hashtype for T {
	fn hash(&self, _: &mut Datastore) -> TypedHash<Self> {
		self.to_hash()
	}
	fn resolve(hash: &TypedHash<Self>, _: &Datastore) -> Result<Self, HashtypeResolveError> {
		Ok(Self::from_hash(hash)?)
	}
} */

impl NativeHashtype for String {
	
    /* fn hash(&self) -> TypedHash<Self> {
        Hash::hash(&bincode::serialize(self).unwrap()).into()
    } */
    /* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
        Ok(bincode::deserialize(db.get(hash.into())?.as_bytes())?)
    } */
}

