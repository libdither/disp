use std::{collections::HashMap};

use bytecheck::CheckBytes;
use rkyv::{Archive, Archived, Fallible, ser::Serializer, validation::validators::DefaultValidator};
use serde::{Serialize as SerdeSerialize, Deserialize as SerdeDeserialize};

use crate::{Hash, TypedHash, hash::TrimHasher};

use super::{ArchiveInterpretable, ArchiveStorable, ArchiveStore, ArchiveStoreRead, DataStore, DataStoreRead, ser::{LinkSerializeError, LinkSerializer}};

#[derive(Debug, Error)]
pub enum DatastoreError {
	#[error("Not in Datastore: {0}")]
	NotInDatastore(Hash),
	#[error("Not a valid rkyv Archive: {0}")]
	InvalidArchive(Hash),
	#[error("Failed to serialize Link: {0}")]
	LinkSerializeError(#[from] LinkSerializeError)
	/* #[error("Not Reverse Linked: {0}")]
	NotReverseLinked(Hash),
	#[error("Failed to Serialize/Deserialize Datastore: {0}")]
	SerdeError(#[from] bincode::Error), */
}

#[derive(Default, SerdeSerialize, SerdeDeserialize)]
pub struct Datastore {
	map: HashMap<Hash, Vec<u8>, TrimHasher>,
	// reverse_lookup: HashMap<Hash, Hash, TrimHasher>, // Map types to types that link to types
	#[serde(skip)]
	serializer: LinkSerializer,
}
impl DataStoreRead for Datastore {
	type DataError = DatastoreError;

	fn get(&self, hash: &Hash) -> Result<&[u8], Self::DataError> {
		self.map.get(hash).ok_or_else(|| DatastoreError::NotInDatastore(hash.clone())).map(|v|v.as_slice())
	}
}
impl DataStore for Datastore {
	type Data = Vec<u8>;

	fn add(&mut self, data: Self::Data) -> Hash {
		let hash = Hash::hash(&data);
		self.map.insert(hash.clone(), data);
		hash
	}
}

impl ArchiveStoreRead for Datastore {
	fn fetch_archive<T: ArchiveInterpretable>(&self, hash: &TypedHash<T>) -> Result<&Archived<T>, Self::Error>
	where <T as Archive>::Archived: for<'v> CheckBytes<DefaultValidator<'v>>,
	{
		let data = self.get(hash.into())?;
		rkyv::check_archived_root::<T>(data).map_err(|_|DatastoreError::InvalidArchive(hash.as_hash().clone()))
	}
}
impl Serializer for Datastore {
    fn pos(&self) -> usize {
        self.serializer.pos()
    }

    fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        self.serializer.write(bytes).map_err(|err|DatastoreError::LinkSerializeError(err))
    }
}
impl Fallible for Datastore { type Error = DatastoreError; }

impl ArchiveStore for Datastore {
	fn store<T: ArchiveStorable<Self>>(&mut self, data: &T) -> Result<TypedHash<T>, DatastoreError> {
		let _pos = self.serialize_value(data).expect("This should never error");
		let data = self.serializer.get_vec();
		let hash = self.add(data.to_vec());
		Ok(hash.into())
	}
}

impl Datastore {
	/// Add Data to Datastore
	pub fn store_data(&mut self, data: Vec<u8>) -> Hash {
		let hash = Hash::hash(&data);
		self.map.entry(hash.clone()).or_insert(data);
		hash
	}
}
impl Datastore {
	pub fn new() -> Self {
		Self::default()
	}
}
/* impl Datastore {
	
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
	pub fn clear(&mut self) {
		self.map.clear();
		self.reverse_lookup.clear();
	}
}
impl<'db> Fallible for &'db Datastore {
	type Error = DatastoreError;
} */
