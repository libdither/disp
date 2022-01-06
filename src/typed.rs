/* use std::marker::PhantomData;

use crate::{Data, hashtype::Hashtype}; */

/* #[derive(Clone)]
pub struct TypedData<'a, T> where T: Hashtype {
	data: &'a Data,
	_type: PhantomData<T>,
}
impl<'a, T: Hashtype> TypedData<'a, T> {
	unsafe fn from_data_unchecked(data: &'a Data, marker: PhantomData<T>) -> TypedData<'a, T> {
		Self { data, _type: marker }
	}
	pub fn untyped(&self) -> &Data { &self.data }
	pub fn as_bytes(&self) -> &[u8] { self.data.as_bytes() }
} */
/* impl<T: Hashtype + DisplayWithDatastore> DisplayWithDatastore for TypedHash<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		match self.resolve(db) {
			Ok(value) => value.fmt(f, db),
			Err(_) => write!(f, "<{}>", self.hash),
		}
	}
}
impl<T: Hashtype + DisplayWithDatastore> DisplayWithDatastore for Option<TypedHash<T>> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		match self {
			Some(thing) => match thing.resolve(db) {
				Ok(value) => value.fmt(f, db),
				Err(_) => write!(f, "<{}>", thing.hash),
			}
			None => Ok(())
		}
	}
} */

/* impl<T: Hashtype> fmt::Display for TypedHash<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "<{}>", self.hash)
	}
} */

/* pub trait Hashtype: Sized {
	type Error;
	/// Add type to datastore
	fn to_hash(&self, db: &mut Datastore) -> TypedHash<Self>;
	/// Resolve hash to type
	fn from_hash(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, Self::Error>;
	/// Get sub hashes of type that should be fetched from the store
	fn get_linked(&self) -> Vec<&Hash>;
	/// Get hashes that should be reverse-linked
	fn get_reverse_lnked(&self) -> Vec<&Hash>;
} */

/* pub trait LinkedHashtype: Sized {
	fn linked_hashes(&self) -> Vec<&Hash>;
	fn reverse_linked_hashes(&self) -> Vec<&Hash> { vec![] }
} */

/* pub trait Datatype: Sized {
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
} */
/* impl<T> Hashtype for T where T: Datatype + LinkedHashtype {
	fn get_linked(&self) -> Vec<&Hash> {
		self.linked()
	}
	fn get_reverse_lnked(&self) -> Vec<&Hash> {
		self.reverse_linked();
	}
} */