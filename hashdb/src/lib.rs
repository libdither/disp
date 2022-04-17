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
mod link;

pub use db::{Datastore, DatastoreError};
pub use hashtype::{NativeHashtype, TypedHash};
pub use link::{Link, ArchivedLink, LinkType, LinkArc, LinkRef};
pub use db_ser::{DatastoreSerializer, LinkSerializer, DatastoreLinkSerializer};
pub use db_de::{DatastoreDeserializer, LinkArcs, LinkArena, Deduplicator, HashDeserializer};
pub use hash::Hash;
pub use data::Data;

#[cfg(test)]
mod tests {
	use crate::{Datastore, DatastoreDeserializer, DatastoreSerializer, Hash, Link, LinkArena, LinkRef, LinkSerializer, NativeHashtype};

	#[test]
	fn test_db() {
		let mut db = &mut Datastore::new();
		let string = {
			let ser = &mut LinkSerializer::new();
			String::from("hello").store(ser, db)
		};
		
		let de = LinkArena::new(); let de = &mut de.join(&mut db);
		assert_eq!(string.fetch(de).unwrap(), "hello");
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
			Link(#[omit_bounds] LinkRef<'a, StringType<'a>>, #[omit_bounds] LinkRef<'a, StringType<'a>>),
		}
		impl<'a> NativeHashtype for StringType<'a> {}

		let links = LinkArena::new();
		let string = links.add(StringType::String("Hello".into()));
		let string2 = links.add(StringType::Link(string.clone().into(), string.clone().into()));

		let db = &mut Datastore::new();

		let ser = &mut LinkSerializer::new();
		let hash = string2.store(ser, db);
		

		let links_de = LinkArena::new();
		let ret = hash.fetch(db, links_de).unwrap();
		/* let links_de = LinkArena::new();
		let de = &mut links_de.join(db);
		let ret: StringType = hash.fetch(de).unwrap();
		
		let ser = &mut LinkSerializer::new(); let ser = &mut ser.join(db);
		assert_eq!(ret.store(ser), string2.store(ser)); */
	}
}
