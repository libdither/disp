use std::{any::TypeId, marker::PhantomData};

use crate::{Datastore, Hash, db::{DatastoreError, Link}};
// const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);


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
}

pub trait Hashtype: Sized + 'static {
	/// Calculate hash and data from type
	fn hash(&self) -> TypedHash<Self>;
	/// Create Link from hashtype
	fn link(self) -> Link<Self> { Link::new(self) }
	/// Create link and store in db
	fn store(self, db: &mut Datastore) -> Link<Self> {
		let mut link = self.link();
		link.store(db); link
	}

	/// Construct Type from Datastore (calling construct functions of linked data)
	fn fetch(hash: &TypedHash<Self>, db: &Datastore) -> Result<Link<Self>, HashtypeResolveError> { db.fetch(hash) }
	/// List of hashes representing any data that should link to this type
	fn reverse_links(&self) -> Vec<TypedHash<Self>> { vec![] }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedHash<T: Hashtype> {
	hash: Hash,
	_type: PhantomData<T>
}
impl<T: Hashtype> From<Hash> for TypedHash<T> {
	fn from(hash: Hash) -> Self { Self { hash, _type: PhantomData::<T>::default() } }
}
impl<T: Hashtype> Into<Hash> for TypedHash<T> {
	fn into(self) -> Hash { self.hash }
}
impl<T: Hashtype> From<&Hash> for &TypedHash<T> {
	// Safety: TypedHash and Hash are equivalent
	fn from(hash: &Hash) -> Self { unsafe { std::mem::transmute(hash) } }
}
impl<'a, T: Hashtype> Into<&'a Hash> for &'a TypedHash<T> {
	fn into(self) -> &'a Hash {
		&self.hash
	}
}
impl<T: Hashtype> TypedHash<T> {
	pub fn new(hash: &Hash) -> Self { TypedHash::<T> { hash: hash.clone(), _type: Default::default() } }
	pub fn cast<R: Hashtype>(self) -> TypedHash<R> { self.hash.into() }
	pub fn as_bytes(&self) -> &[u8] { self.hash.as_bytes() }
	pub fn as_hash(&self) -> &Hash { &self.hash }
	pub fn resolve<'a>(&self, db: &'a Datastore) -> Result<Link<T>, HashtypeResolveError> {
		Link::fetch(self, db)
	}
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

impl Hashtype for String {
    fn hash(&self) -> TypedHash<Self> {
        Hash::hash(&bincode::serialize(self).unwrap()).into()
    }
    /* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
        Ok(bincode::deserialize(db.get(hash.into())?.as_bytes())?)
    } */
}

