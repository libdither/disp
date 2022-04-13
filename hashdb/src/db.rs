
use std::{collections::HashMap, io, sync::Arc};

use bytecheck::CheckBytes;
use rkyv::{AlignedVec, Archived, Fallible, ser::{ScratchSpace, Serializer, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, AllocSerializer, FallbackScratch, HeapScratch}}, validation::validators::DefaultValidator, with::{DeserializeWith, SerializeWith}};
use serde::{Serialize, Deserialize};

use crate::{Data, Hash, data::DataError, hash::TrimHasher, hashtype::{HashSerializer, NativeHashtype, TypedHash, HashType}, rkyv_map::Map};

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
	/* pub fn add<'a, T: NativeHashtype>(&'a mut self, hashtype: T, serializer: &mut HashSerializer<'a>) -> (TypedHash<T>, &'a Archived<T>)
	where T::Archived: CheckBytes<DefaultValidator<'a>>
	{
		let mut reverse_links = hashtype.reverse_links();
		let hash = serializer.hash(hashtype).unwrap();
		
		while let Some(subhash) = reverse_links.next() {
			self.reverse_lookup.insert(*subhash, hash.into());
		}
		let data = self.get(&hash.into()).unwrap();
		
		(hash, data.archived::<'a, T>().expect("this should be a type"))
	} */
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
	pub fn serializer<'s>(&'s mut self) -> HashSerializer<'s> {
		HashSerializer::new(self)
	}
}
impl Fallible for Datastore {
	type Error = DatastoreError;
}
impl<'db> Fallible for &'db Datastore {
	type Error = DatastoreError;
}

#[test]
fn test_loading() {
	let db = &mut Datastore::new();

	// Self-referential type
	#[derive(PartialEq, Eq, Debug, Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
	// To use the safe API, you have to derive CheckBytes for the archived type
	#[archive_attr(derive(bytecheck::CheckBytes, Debug))]
	#[archive(bound(deserialize = "__D: Fallible, HashType: DeserializeWith<Hash, Arc<StringType>, __D>", serialize = "__S: Serializer, HashType: SerializeWith<Arc<StringType>, __S>"))]
	enum StringType {
		String(String),
		Link(#[with(HashType)] #[omit_bounds] Arc<StringType>, #[with(HashType)] #[omit_bounds] Arc<StringType>),
	}
	impl NativeHashtype for StringType {}

	let ser = &mut HashSerializer::new(db);

	let string = Arc::new(StringType::String("Hello".into()));
	let string2 = StringType::Link(string.clone(), string.clone());

	let hash: Hash = string2.store(ser).into();
	let ret: Arc<StringType> = HashType::deserialize_with(&hash, ser.db).unwrap();
	// let ret: Arc<StringType> = TypedHash::fetch(&hash, ser.db).unwrap();
	//let ret = HashType::<StringType>::deserialize_with(&hash, &mut &*(ser.db)).unwrap();
	
	assert_eq!(*ret, string2);
}