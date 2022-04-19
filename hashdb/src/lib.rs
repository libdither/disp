#![allow(unused)]
#![allow(incomplete_features)]
#![allow(const_evaluatable_unchecked)]

#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_mut_refs)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(generic_associated_types)]
#![feature(downcast_unchecked)]

#[macro_use]
extern crate thiserror;

mod hash;
mod data;
mod db;
pub mod hashtype;
mod db_ser;
mod db_de;

pub use db::{Datastore, DatastoreError};
pub use hashtype::{NativeHashtype, TypedHash, HashType};
pub use db_ser::{DatastoreSerializer, LinkSerializer, DatastoreLinkSerializer};
pub use db_de::{LinkArena, HashDeserializer, DatastoreDeserializer};
pub use hash::Hash;
pub use data::Data;

#[cfg(test)]
mod tests {
	use crate::{Datastore, DatastoreSerializer, Hash, LinkArena, LinkSerializer, NativeHashtype, DatastoreDeserializer, HashType};

	#[test]
	fn test_db() {
		let mut db = &mut Datastore::new();
		let ser = &mut LinkSerializer::new();
		let string = String::from("hello").store(&mut ser.join(db));
		
		let arena = LinkArena::new();
		assert_eq!(*string.fetch(db, &arena).unwrap(), "hello");
	}

	#[test]
	fn test_loading() {

		// Self-referential type
		#[derive(Debug, Hash, PartialEq, Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
		// To use the safe API, you have to derive CheckBytes for the archived type
		#[archive_attr(derive(bytecheck::CheckBytes, Debug))]
		#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
		enum StringType<'a> {
			String(String),
			Link(#[with(HashType)] #[omit_bounds] &'a StringType<'a>, #[with(HashType)] #[omit_bounds] &'a StringType<'a>),
		}
		impl<'a> NativeHashtype for StringType<'a> {}

		let links = LinkArena::new();
		let string = links.add(StringType::String("Hello".into()));
		let string2 = links.add(StringType::Link(string, string));

		let db = &mut Datastore::new();

		let ser = &mut LinkSerializer::new();
		let hash = string2.store(&mut ser.join(db));
		

		let links_de = LinkArena::new();
		let ret = hash.fetch(db, &links_de).unwrap();
		/* let links_de = LinkArena::new();
		let de = &mut links_de.join(db);
		let ret: StringType = hash.fetch(de).unwrap();
		
		let ser = &mut LinkSerializer::new(); let ser = &mut ser.join(db);
		assert_eq!(ret.store(ser), string2.store(ser)); */
	}
}
