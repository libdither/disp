
use std::{collections::HashMap, io};

use bytecheck::CheckBytes;
use rkyv::{Archived, validation::validators::DefaultValidator};
use serde::{Serialize, Deserialize};

use crate::{Data, Hash, data::DataError, hashtype::{NativeHashtype, TypedHash}};

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
	map: HashMap<Hash, Data>,
	reverse_lookup: HashMap<Hash, Hash>, // Map types to types that link to types
}

impl Datastore {
    pub fn new() -> Self {
        Self::default()
    }
	pub fn add<'a, T: NativeHashtype>(&'a mut self, hashtype: T) -> (Hash, &'a Archived<T>)
	where T::Archived: CheckBytes<DefaultValidator<'a>>
	{
		let mut reverse_links = hashtype.reverse_links();
		
		let data = hashtype.archive();
		let hash = data.hash();

		while let Some(subhash) = reverse_links.next() {
			self.reverse_lookup.insert(subhash.clone(), hash.clone());
		}
		
		(hash.clone(), self.store_data(hash, data).archived::<'a, T>().expect("this should be a type"))
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
	// Fetch Typed (Archived) Data
	pub fn fetch<'a, T: NativeHashtype>(&'a self, hash: &TypedHash<T>) -> Result<&'a Archived<T>, DatastoreError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>
	{
		let mhash = hash.as_hash();
		Ok(self.get(mhash)?.archived::<'a, T>()?)
	}

	/* pub fn fetch_recursive<'a, T: NativeHashtype>(&'a self, hash: &TypedHash<T>) -> Result<Archived<T>, DatastoreError>{
		let archived = self.fetch(hash);
	} */
	#[inline]
	pub fn lookup(&self, hash: &Hash) -> Result<&Hash, DatastoreError> {
		self.reverse_lookup.get(hash).ok_or(DatastoreError::NotReverseLinked(hash.clone()))
	}
	pub fn lookup_typed<'a, F: NativeHashtype, T: NativeHashtype>(&'a self, hash: &TypedHash<F>) -> Result<TypedHash<T>, DatastoreError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>
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

#[test]
fn test_loading() {
	let db = &mut Datastore::new();

	// Self-referential type
	#[derive(PartialEq, Eq, Debug, Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
	#[archive(compare(PartialEq))]
	// To use the safe API, you have to derive CheckBytes for the archived type
	#[archive_attr(derive(bytecheck::CheckBytes, Debug))]	
	struct StringType { string: String, linked: Option<TypedHash<StringType>> }
	impl NativeHashtype for StringType {}

	let string = StringType { string: "string".to_owned(), linked: None }.store(db);
	let string2 = StringType { string: "string2".to_owned(), linked: Some(string.clone()) }.store(db);

	assert_eq!(string2.fetch(db).unwrap().linked.as_ref().unwrap(), &string);
}