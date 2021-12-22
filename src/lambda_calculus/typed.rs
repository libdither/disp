use std::{fmt, marker::PhantomData};

use serde::{Deserialize, Serialize, de::Visitor};

use hashdb::{Hash, Data, Datastore};

#[derive(Clone)]
pub struct TypedData<'a, T: Hashtype> {
	data: &'a Data,
	_type: PhantomData<T>,
}
impl<'a, T: Hashtype> TypedData<'a, T> {
	unsafe fn from_data_unchecked(data: &'a Data, marker: PhantomData<T>) -> TypedData<'a, T> {
		Self { data, _type: marker }
	}
	pub fn untyped(&self) -> &Data { &self.data }
	pub fn as_bytes(&self) -> &[u8] { self.data.as_bytes() }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedHash<T> {
	hash: Hash,
	_type: PhantomData<T>
}
impl<T: Hashtype> TypedHash<T> {
	pub unsafe fn from_hash_unchecked(hash: Hash, marker: PhantomData<T>) -> TypedHash<T> {
		Self { hash, _type: marker }
	}
	pub unsafe fn from_bytes(bytes: &[u8]) -> TypedHash<T> {
		Self::from_hash_unchecked(Hash::from_raw_bytes(bytes), PhantomData::<T>::default())
	}
	pub unsafe fn cast_type<R>(self) -> TypedHash<R> {
		TypedHash::<R> { hash: self.hash, _type: PhantomData::<R>::default() }
	}
	pub fn as_bytes(&self) -> &[u8] { self.hash.as_bytes() }
	pub fn untyped(&self) -> &Hash {
		&self.hash
	}
	pub fn resolve<'a>(&self, db: &'a Datastore) -> Result<T, <T as Hashtype>::Error> {
		T::from_hash(self, db)
	}
}
impl<T> Serialize for TypedHash<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
			S: serde::Serializer {
		serializer.serialize_bytes(self.hash.as_bytes())
	}
}
impl<'d, T: Hashtype> Deserialize<'d> for TypedHash<T> {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
			D: serde::Deserializer<'d> {
		struct HashVisitor;

		impl<'de> Visitor<'de> for HashVisitor {
			type Value = Hash;

			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				formatter.write_str("byte array")
			}
			fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
			where
					E: serde::de::Error, {
				Ok(Hash::from_raw_bytes(v))
			}
		}
		let hash = deserializer.deserialize_bytes(HashVisitor)?;
		unsafe { Ok(Self::from_hash_unchecked(hash, PhantomData::<T>::default())) }
	}
}

pub trait Hashtype: Sized {
	type Error;
	fn to_hash(&self, db: &mut Datastore) -> TypedHash<Self>;
	fn from_hash(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, Self::Error>;
}

pub trait Datatype: Sized {
	type Error;
	fn to_data_untyped(&self) -> Data;
	fn from_data_untyped(data: &Data) -> Result<Self, Self::Error>;
	fn db_error(hash: Hash) -> Self::Error;
}

impl<T> Hashtype for T where T: Datatype {
	type Error = <T as Datatype>::Error;
	fn to_hash(&self, db: &mut Datastore) -> TypedHash<Self> {
		let data = self.to_data_untyped();
		// Safety: type of self passed to to_untyped_data is known
		unsafe { TypedHash::from_hash_unchecked(db.add(data), PhantomData::<Self>::default()) }
	}
	fn from_hash(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, Self::Error> {
		let untyped = hash.untyped();
		let data = db.get(untyped).ok_or_else(|| Self::db_error(untyped.clone()))?;
		Self::from_data_untyped(data)
	}
}