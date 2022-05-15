use std::{collections::HashMap, io};

use bytecheck::CheckBytes;
use rkyv::{validation::validators::DefaultValidator, Fallible};
use serde::{Deserialize, Serialize};

use crate::{Data, DatastoreSerializer, Hash, LinkArena, NativeHashtype, TypedHash, data::DataError, hash::TrimHasher};

#[derive(Debug, Error)]
pub enum DatastoreError {
	#[error("Not in Datastore: {0}")]
	NotInDatastore(Hash),
	#[error("Not Reverse Linked: {0}")]
	NotReverseLinked(Hash),
	#[error("Not a valid rkyv Archive: {0}")]
	NotAnArchive(Hash),
	#[error("Failed to Serialize/Deserialize Datastore: {0}")]
	SerdeError(#[from] bincode::Error),
	#[error("Data Error: {0}")]
	DataError(#[from] DataError),
}

#[derive(Default, Serialize, Deserialize)]
pub struct Datastore {
	map: HashMap<Hash, Data, TrimHasher>,
	reverse_lookup: HashMap<Hash, Hash, TrimHasher>, // Map types to types that link to types
	root: Option<(Hash, String)>,
}

impl Datastore {
	pub fn new() -> Self {
		Self::default()
	}
	pub fn register<'a, S: DatastoreSerializer, T: NativeHashtype>(&mut self, mut reverse_links: T::LinkIter<'a, S>, hash: &Hash) {
		while let Some(subhash) = reverse_links.next() {
			self.reverse_lookup.insert(subhash.clone(), hash.clone());
		}
	}
	/// Add Data to Datastore
	pub fn store(&mut self, data: Data) -> Hash {
		let hash = data.hash();
		self.store_data(hash.clone(), data);
		hash
	}
	#[inline]
	fn store_data(&mut self, hash: Hash, data: Data) -> &Data {
		self.map.entry(hash).or_insert(data)
	}
	// Get Raw Data
	pub fn get(&self, hash: &Hash) -> Result<&Data, DatastoreError> {
		self.map.get(hash).ok_or(DatastoreError::NotInDatastore(hash.clone()))
	}
	#[inline]
	pub fn lookup(&self, hash: &Hash) -> Result<&Hash, DatastoreError> {
		self.reverse_lookup.get(hash).ok_or(DatastoreError::NotReverseLinked(hash.clone()))
	}
	pub fn lookup_typed<'a, F: NativeHashtype, T: NativeHashtype + 'a>(&'a self, hash: &TypedHash<F>) -> Result<TypedHash<T>, DatastoreError>
	where
		T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		let linked_hash = self.lookup(hash.as_hash())?;
		self.get(linked_hash)?.assert_type::<T>()?;

		Ok(linked_hash.clone().into())
	}
	pub fn save(&self, writer: impl io::Write) -> Result<(), DatastoreError> {
		Ok(bincode::serialize_into(writer, self)?)
	}
	pub fn load(&mut self, reader: impl io::Read) -> Result<(), DatastoreError> {
		*self = bincode::deserialize_from(reader)?;
		Ok(())
	}
	pub fn set_root<T>(&mut self, hash: TypedHash<T>) {
		self.root = Some((hash.into(), std::any::type_name::<T>().to_owned()));
	}
	pub fn root<T>(&self) -> Option<TypedHash<T>> {
		let (hash, typename) = self.root.as_ref()?;
		if typename == std::any::type_name::<T>() { Some(hash.clone().into()) } else { None }
	}
	pub fn clear(&mut self) {
		self.map.clear();
		self.reverse_lookup.clear();
	}
}
impl<'db> Fallible for &'db Datastore {
	type Error = DatastoreError;
}
