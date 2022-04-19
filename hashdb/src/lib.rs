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

mod data;
mod db;
mod db_de;
mod db_ser;
mod hash;
pub mod hashtype;

pub use data::Data;
pub use db::{Datastore, DatastoreError};
pub use db_de::{DatastoreDeserializer, HashDeserializer, LinkArena};
pub use db_ser::{DatastoreLinkSerializer, DatastoreSerializer, LinkSerializer};
pub use hash::Hash;
pub use hashtype::{HashType, NativeHashtype, TypedHash};

#[cfg(test)]
mod tests {
	use crate::{Datastore, DatastoreDeserializer, DatastoreSerializer, HashType, LinkArena, LinkSerializer, NativeHashtype};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
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
			Link(
				#[with(HashType)]
				#[omit_bounds]
				&'a StringType<'a>,
				#[with(HashType)]
				#[omit_bounds]
				&'a StringType<'a>,
			),
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
		assert_eq!(ret, string2);
	}
}
