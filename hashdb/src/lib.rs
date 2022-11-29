#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_mut_refs)]
#![feature(associated_type_bounds)]
#![feature(return_position_impl_trait_in_trait)]

#[macro_use]
extern crate thiserror;

extern crate hashdb_derive;
pub use hashdb_derive::*;

mod store;
mod hash;
pub mod link;

pub use store::{Datastore, DatastoreError, LinkArena, ArchiveStore, ArchiveDeserializer, ArchiveStorable, ArchiveFetchable, TypeStore, HashType, UniqueId, ReverseLinks, RevHashType, RevTypeStore, RevLinkStore, RevLinkArena, UniqueHash, ArchiveToType};
pub use hash::Hash;
pub use link::{WithHashType, TypedHash};

#[cfg(test)]
mod tests {
	use std::io::Cursor;

use crate::{RevLinkArena, RevTypeStore};
	pub use crate::{Datastore, HashType, LinkArena, store::{ArchiveStorable, ArchiveStore, ArchiveDeserializer, TypeStore}, WithHashType, hashtype, UniqueId, ReverseLinks, UniqueHash};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let string = db.store(&String::from("hello")).unwrap();

		let arena = LinkArena::new();
		assert_eq!(*string.fetch(db, &arena).unwrap(), "hello");
	}

	mod hashdb {
		pub use super::{WithHashType, HashType, UniqueId, ReverseLinks, UniqueHash, ArchiveDeserializer, ArchiveStore};
	}

	#[test]
	fn test_db_linked() {
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

	#[test]
	fn test_reverse_link() {
		let links = &LinkArena::new();
		let rev_links = RevLinkArena::new(links);

		{
			let name = links.add(TestName { string: links.add("Hello".to_owned()) });

			// Store reverse linked
			rev_links.rev_add(TestStruct { name, number: 42 });
		};

		let name = links.add(TestName { string: links.add("Hello".to_owned()) });

		let test = rev_links.links::<TestName, TestStruct>(name).next().expect("Should be at least one rev-linked structure here");

		assert_eq!(test.number, 42);
	}

	#[test]
	fn test_save_load() {
		let data = {
			let mut writer = Vec::with_capacity(512);
			let links = &LinkArena::new();
			let links = RevLinkArena::new(links);
	
			let name = links.add(TestName { string: links.add("hello".to_owned()) });
			links.rev_add(TestStruct { name, number: 42 });

			links.save(&mut writer).expect("failed to save");

			dbg!(links);
			writer
		};
		let links = &LinkArena::new();
		let mut links = RevLinkArena::new(links);
		links.load(&mut Cursor::new(data)).expect("failed to load");

		let name_unfound = links.add(TestName { string: links.add("goodbye".to_owned()) });

		assert_eq!(links.links::<TestName, TestStruct>(name_unfound).next(), None); // No TestStruct found for Name "goodbye"

		let name = links.add(TestName { string: links.add("hello".to_owned()) });

		let test = links.links::<TestName, TestStruct>(name).next().expect("Should be at least one rev-linked structure here");

		assert_eq!(test.number, 42);
	}

	#[derive(Debug)]
	#[hashtype]
	struct TestName<'e> {
		#[subtype]
		string: &'e String,
	}

	#[derive(Debug)]
	#[hashtype]
	struct TestStruct<'e> {
		#[subtype_reverse_link]
		name: &'e TestName<'e>,
		number: u64,
	}
}
