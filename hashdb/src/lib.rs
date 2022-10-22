#![allow(incomplete_features)]
#![allow(const_evaluatable_unchecked)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_mut_refs)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(downcast_unchecked)]
#![feature(type_alias_impl_trait)]
#![feature(return_position_impl_trait_in_trait)]

#[macro_use]
extern crate thiserror;

extern crate hashdb_derive;
pub use hashdb_derive::*;

mod store;
mod hash;
pub mod link;

pub use store::{Datastore, DatastoreError, LinkArena, ArchiveStore, ArchiveDeserializer, ArchiveStorable, ArchiveFetchable, TypeStore, HashType, UniqueHashTypeId};
pub use hash::Hash;
pub use link::{WithHashType, TypedHash};

#[cfg(test)]
mod tests {
	use crate::{Datastore, HashType, LinkArena, store::{ArchiveStorable, ArchiveStore, ArchiveDeserializer, TypeStore}, WithHashType, hashtype, UniqueHashTypeId};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let string = db.store(&String::from("hello")).unwrap();

		let arena = LinkArena::new();
		assert_eq!(*string.fetch(db, &arena).unwrap(), "hello");
	}

	#[hashtype]
	#[derive(Debug)]
	struct TestName<'e> {
		#[subtype]
		string: &'e String,
	}

	#[hashtype]
	#[derive(Debug)]
	struct TestStruct<'e> {
		#[subtype_reverse_link]
		name: &'e TestName<'e>,
		number: u64,
	}

	#[test]
	fn test_loading() {
		// Self-referential type
		#[derive(Debug, Clone)]
		#[hashtype]
		enum StringType<'a> {
			String(String),
			Link(
				#[subtype]
				&'a StringType<'a>,
				#[subtype]
				&'a StringType<'a>,
			),
		}

		let links = LinkArena::new();
		let string = links.add(StringType::String("Hello".into()));
		let string2 = links.add(StringType::Link(string, string));

		let db = &mut Datastore::new();

		// let ser = &mut LinkSerializer::new();
		let hash = string2.store(db).unwrap();
		// let hash = string2.store(&mut ser.join(db));

		let links_de = LinkArena::new();
		let ret = hash.fetch(db, &links_de).unwrap();
		assert_eq!(ret, string2);
	}
}
