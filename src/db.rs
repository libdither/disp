
use std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{Data, Hash, hashtype::{Hashtype, HashtypeResolveError, Link, TypedHash}};

#[derive(Debug, Error)]
pub enum DatastoreError {
	#[error("Not in Datastore: {0}")]
	NotInDatastore(Hash),
}

#[derive(Default)]
pub struct Datastore {
	map: HashMap<Hash, Data>,
	// Cache for actively resolved types
	resolved_links: RefCell<HashMap<Hash, (Rc<Hash>, Rc<dyn Any>)>>,
}

impl Datastore {
    pub fn new() -> Self {
        Self::default()
    }
	pub fn add(&mut self, data: Data) -> Hash {
		let hash = Hash::hash(data.as_bytes());
		if !self.map.contains_key(&hash) {
			if self.map.insert(hash.clone(), data).is_some() { panic!("There should not already be something in here") };
		}  
		hash
	}

	pub fn remove(&mut self, hash: &Hash) -> Option<Data> {
		self.map.remove(hash)
	}
	pub fn get<'a>(&self, hash: impl Into<&'a Hash>) -> Result<&Data, DatastoreError> {
		let hash = hash.into();
		self.map.get(hash).ok_or_else(||DatastoreError::NotInDatastore(hash.clone()))
	}
	pub fn resolve_link<T: Hashtype>(&self, hash: &TypedHash<T>) -> Result<Link<T>, HashtypeResolveError> {
		let untyped = hash.untyped_ref();
		let mut resolved_links = self.resolved_links.borrow_mut();
		let (hash, val_rc) = if let Some(v) = resolved_links.get(untyped) { v.clone() }
		else {
			let hash_rc = Rc::new(untyped.clone());
			let val_rc: Rc<dyn Any> = Rc::new(T::resolve(hash, self)?);
			resolved_links.insert(untyped.clone(), (hash_rc.clone(), val_rc.clone()));
			(hash_rc, val_rc)
		};
		// Don't use downcast_ref here
		let hashtype = match val_rc.downcast() {
			Ok(t) => t,
			Err(rc) => Err(HashtypeResolveError::InvalidLinkType(rc.as_ref().type_id()))?
		};  
		Ok(Link::from_rc(hash, hashtype))
	}
}

#[test]
fn test_loading() {
	let db = &mut Datastore::new();

	// Self-referential type
	#[derive(PartialEq, Eq, Debug)]
	struct StringType { string: String, linked: Option<Link<StringType>> }
	impl Hashtype for StringType {
		fn hash(&self, db: &mut Datastore) -> TypedHash<Self> {
			use bytes::{BytesMut, BufMut};
			let mut buf = BytesMut::with_capacity(128);
			bincode::serialize_into((&mut buf).writer(), &self.string).unwrap();

			buf.put_u8(if self.linked.is_some() { 1 } else { 0 });
			if let Some(linked) = &self.linked {
				buf.put(linked.hash().as_bytes());
			}
			
			db.add(Data::new(&buf)).into()
			
		}
		fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
			use bytes::Buf;
			let mut data = db.get(hash)?.as_bytes();
			let string = bincode::deserialize_from(&mut data)?;
			
			let linked = if data.get_u8() == 0 { None } else {
				let hash = Hash::from_reader(&mut data)?;
				Some(db.resolve_link(&hash.into())?)
			};

			Ok(StringType { string, linked })
		}
	}

	let string = StringType { string: "string".to_owned(), linked: None };
	println!("string typeid: {:?}", string.type_id());
	let string_hash = string.hash(db);
	assert_eq!(string, string_hash.resolve(db).unwrap());

	let string_linked = db.resolve_link(&string_hash).unwrap();
	let string2 = StringType { string: "string2".to_owned(), linked: Some(string_linked) };
	let string2_hash = string2.hash(db);

	let string2_resolved = string2_hash.resolve(db).unwrap();
	assert_eq!(string2, string2_resolved);

	let string_resolved = string2_resolved.linked.unwrap();
	assert_eq!(&string, string_resolved.as_ref());
}