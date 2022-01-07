
use std::{any::Any, collections::HashMap, ops::Deref, rc::Rc};

use crate::{Hash, hashtype::{Hashtype, HashtypeResolveError, TypedHash}};

/// Represents the hashing operation of a specific type, since the only way to create this object is to use a T: Hashtype, the hash field of this type
/// should always be a valid hash. (i.e. an execution of Hashtype::hash())
#[derive(Clone, Debug, Eq)]
pub struct Link<T: Hashtype> {
	hashtype: Rc<T>,
	hash: TypedHash<T>,
}
impl<T: Hashtype> Link<T> {
	pub fn new(hashtype: T) -> Self {
		let hash = hashtype.hash();
		Self { hashtype: Rc::new(hashtype), hash }
	}
	pub fn hash(&self) -> &TypedHash<T> { &self.hash }
	
	pub fn as_ref(&self) -> &T {
		self.hashtype.as_ref()
	}
	//pub fn inner<H: Hashtype + Clone>(link: &Link<H>) -> H { link.hashtype.as_ref().clone() }
}
impl<T: Hashtype> Deref for Link<T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.hashtype.deref()
	}
}
impl<T: Hashtype> PartialEq for Link<T> {
	fn eq(&self, other: &Self) -> bool {
		self.hash().as_bytes() == other.hash().as_bytes()
	}
}

#[derive(Debug, Error)]
pub enum DatastoreError {
	#[error("Not in Datastore: {0}")]
	NotInDatastore(Hash),
	#[error("Not Reverse Linked: {0}")]
	NotReverseLinked(Hash),
}

#[derive(Default)]
pub struct Datastore {
	map: HashMap<Hash, Rc<dyn Any>>,
	// Cache for actively resolved types
	//resolved_links: RefCell<HashMap<Hash, (Rc<Hash>, Rc<dyn Any>)>>,

	reverse_lookup: HashMap<Hash, Hash>, // Map types to types that link to types
}

impl Datastore {
    pub fn new() -> Self {
        Self::default()
    }
	pub fn store_type<T: Hashtype>(&mut self, hashtype: T) -> Link<T> {
		let hash: Hash = hashtype.hash().into();
		// Register Reverse Lookup
		for subhash in hashtype.reverse_links() {
			self.reverse_lookup.insert(subhash.clone(), hash.clone());
		}

		let hashtype = Rc::new(hashtype);
		// Register into map
		if !self.map.contains_key(&hash) {
			if self.map.insert(hash.clone(), hashtype.clone()).is_some() { panic!("There should not already be something in here") };
		}
		Link { hashtype, hash: hash.into() }
	}
	/// Add Data to datastore
	pub fn store_link<T: Hashtype>(&mut self, link: &mut Link<T>) -> TypedHash<T> {
		let Link { hash , hashtype } = link;
		let hash = hash.as_hash().clone();

		// Register Reverse Lookup
		for subhash in hashtype.reverse_links() {
			self.reverse_lookup.insert(subhash.clone(), hash.clone());
		}
		// Register into map
		if !self.map.contains_key(&hash) {
			if self.map.insert(hash.clone(), hashtype.clone()).is_some() { panic!("There should not already be something in here") };
		} else {
			*hashtype = self.map.get(&hash).unwrap().clone().downcast().expect("malformed link");
		}

		hash.into()
	}
	pub fn fetch<T: Hashtype>(&self, hash: &TypedHash<T>) -> Result<Link<T>, HashtypeResolveError> {
		let hash = hash.as_hash();
		let rc = self.map.get(hash).ok_or_else(||DatastoreError::NotInDatastore(hash.clone()))?.clone();
		// Don't use downcast_ref here
		let hashtype = match rc.downcast() {
			Ok(t) => t,
			Err(rc) => Err(HashtypeResolveError::InvalidLinkType(rc.as_ref().type_id()))?
		};
		let hash = hash.clone().into();
		let link = Link { hashtype, hash };
		Ok(link)
	}
	pub fn lookup<T: Hashtype, H: Hashtype>(&self, hash: &TypedHash<T>) -> Result<Link<H>, HashtypeResolveError> {
		let hash = hash.as_hash();
		let linked_hash = self.reverse_lookup.get(hash).ok_or(DatastoreError::NotReverseLinked(hash.clone()))?;
		Ok(self.fetch(linked_hash.into())?)
	}
}

#[test]
fn test_loading() {
	let db = &mut Datastore::new();

	// Self-referential type
	#[derive(PartialEq, Eq, Debug, Clone)]
	struct StringType { string: String, linked: Option<Link<StringType>> }
	impl Hashtype for StringType {
		fn hash(&self) -> TypedHash<Self> {
			use bytes::{BytesMut, BufMut};
			let mut buf = BytesMut::with_capacity(128);
			bincode::serialize_into((&mut buf).writer(), &self.string).unwrap();
			buf.put_u8(if self.linked.is_some() { 1 } else { 0 });
			if let Some(linked) = &self.linked {
				buf.put(linked.hash().as_bytes());
			}
			Hash::hash(&buf).into()
		}
	}

	let string = StringType { string: "string".to_owned(), linked: None }.store(db);
	let string2 = StringType { string: "string2".to_owned(), linked: Some(string.clone()) }.store(db);

	assert_eq!(string2.linked.clone().unwrap().hash(), string.hash());
}