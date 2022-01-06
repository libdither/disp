

use std::{any::TypeId, marker::PhantomData, ops::Deref, rc::Rc};

use crate::{Datastore, Hash, db::DatastoreError};
const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);
/// Immutable and Mutable are serialized to `Hash`es

/// Shared reference to a piece of data, serialized to hash
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Link<T: Hashtype> {
	hash: Rc<Hash>,
	hashtype: Rc<T>,
}
impl<T: Hashtype> Link<T> {
	pub fn from_rc(hash: Rc<Hash>, hashtype: Rc<T>) -> Link<T> { Link { hash, hashtype } }
	pub fn hash(&self) -> &TypedHash<T> {
		let hash = self.hash.deref();
		hash.into()
	}
	pub fn as_ref(&self) -> &T {
		self.hashtype.as_ref()
	}
	/* pub fn new(hash: TypedHash<T>, db: &mut Datastore) -> Link<T> {
		
	} */
}
impl<T: Hashtype> Deref for Link<T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.hashtype.deref()
	}
}

/* use serde::{Serialize, Deserialize};
impl<T: Hashtype> Serialize for Link<T> {
	fn serialize(&self) {

	}
}
 */
#[derive(Debug, Error)]
pub enum HashtypeResolveError {
	#[error("Datastore error: {0}")]
	DatastoreError(#[from] DatastoreError),
	#[error("Invalid Hash: {0}")]
	InvalidHashError(#[from] InvalidHashError),
	#[error("Invalid Link Type: {0:?}")]
	InvalidLinkType(TypeId),
	#[error("Deserialization Error")]
	DeserializeError(#[from] bincode::Error),
	#[error("Reader Error")]
	ReaderError(#[from] std::io::Error)
}

pub trait Hashtype: Sized + 'static {
	/// Construct from Datastore (calling construct functions of linked data)
	fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError>;
	fn hash(&self, db: &mut Datastore) -> TypedHash<Self>;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedHash<T: Hashtype> {
	hash: Hash,
	_type: PhantomData<T>
}
impl<T: Hashtype> From<Hash> for TypedHash<T> {
	fn from(hash: Hash) -> Self { Self { hash, _type: PhantomData::<T>::default() } }
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
	pub fn cast<R: Hashtype>(self) -> TypedHash<R> { self.untyped().into() }
	pub fn untyped(self) -> Hash { self.hash }
	pub fn untyped_ref(&self) -> &Hash { &self.hash }
	pub fn as_bytes(&self) -> &[u8] { self.hash.as_bytes() }
	
	pub fn resolve<'a>(&self, db: &'a Datastore) -> Result<T, HashtypeResolveError> {
		T::resolve(self, db)
	}
}

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
}

// /// Implement this type for all rust types that should be serialized into self-defining structures
/* trait RustHashtype {
	type FromDataError;
	fn to_data(&self) -> Data;
	fn from_data(data: &Data) -> Result<Self, Self::FromDataError>;
}

impl<T> RustHashtype for T
where T: Serialize + DeserializeOwned,
	RustHashtype::FromDataError: From<bincode::Error>,
{
	fn to_data(&self) -> Data {
		Data::from_vec(bincode::serialize(self).unwrap())
	}
	fn from_data(data: &Data) -> Result<Self, Self::FromDataError> {
		Ok(bincode::deserialize(data.as_bytes())?)
	}
} */

/* struct RustType<T: RustHashtype> {
	value: T,
}
impl<T: RustHashtype> Hashtype for RustType<T> {

} */

/* #[derive(Debug, Error)]
enum ParseError {
	#[error("Failed to read hash")]
	HashReadError(#[from] std::io::Error),
	#[error("Unknown Layout")]
	UnknownLayout(Hash),
	#[error("Hash not in Datastore")]
	DatastoreError(#[from] DatastoreError),
} */

/* fn parse_rusttype_layout(layout: Hash, db: &Datastore) -> Result<&str, ParseError> {
	let data = &mut db.get(&layout)?.as_bytes();
	let layout_def = Hash::from_reader(data)?;
	if layout_def == RUST_TYPE {
		String::from_utf8_lossy(data)
	} else {
		Err(ParseError::UnknownLayout(layout_def))
	}
} */

/* fn parse_data(data: &[u8], db: &Datastore) -> Result<RustType<String>, ParseError> {
	let layout = Hash::from_reader(data)?;
	Ok(match layout {
		RUST_TYPE => panic!("do not expect rust type here"),
		_ => ParseError::UnknownLayout(layout)
	})
} */