
use std::{collections::HashMap, io, sync::Arc};

use bytecheck::CheckBytes;
use rkyv::{AlignedVec, Archived, Fallible, de::{SharedDeserializeRegistry, deserializers::SharedDeserializeMap}, ser::{ScratchSpace, Serializer, SharedSerializeRegistry, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, AllocSerializer, FallbackScratch, HeapScratch}}, validation::validators::DefaultValidator, with::{DeserializeWith, SerializeWith}};
use serde::{Serialize, Deserialize};

use crate::{Data, Hash, data::DataError, hash::TrimHasher, hashtype::{ArchivedLink, DatastoreDeserializer, DatastoreSerializer, HashSerializer, Link, NativeHashtype, TypedHash}};

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
}

impl Datastore {
	pub fn new() -> Self {
		Self::default()
	}
	pub fn register<'a, T: NativeHashtype>(&mut self, mut reverse_links: T::LinkIter, hash: &Hash) {
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
	// Fetch Typed (Archived) Data
	/* pub fn fetch<'a, T: NativeHashtype>(&'a self, hash: &TypedHash<T>) -> Result<&'a Archived<T>, DatastoreError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>
	{
		let mhash = hash.as_hash();
		Ok(self.get(mhash)?.archived::<'a, T>()?)
	} */

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
	pub fn serializer<'s>(&'s mut self) -> HashSerializer<'s> {
		HashSerializer::new(self)
	}
}
impl<'db> Fallible for &'db Datastore {
	type Error = DatastoreError;
}

#[test]
fn test_loading() {
	let db = &mut Datastore::new();

	// Self-referential type
	#[derive(Debug, PartialEq, Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
	// To use the safe API, you have to derive CheckBytes for the archived type
	#[archive_attr(derive(bytecheck::CheckBytes, Debug))]
	#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer"))]
	enum StringType {
		String(String),
		Link(#[omit_bounds] Link<StringType>, #[omit_bounds] Link<StringType>),
	}
	impl NativeHashtype for StringType {}

	let ser = &mut HashSerializer::new(db);

	let string = Link::new(StringType::String("Hello".into()));
	let string2 = Link::new(StringType::Link(string.clone().into(), string.clone().into()));

	let hash = string2.store(ser);
	let ret: Link<StringType> = hash.fetch(ser.db).unwrap();
	
	assert_eq!(ret.store(ser), string2.store(ser));
}