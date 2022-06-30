#![allow(incomplete_features)]
#![allow(const_evaluatable_unchecked)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_mut_refs)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(generic_associated_types)]
#![feature(downcast_unchecked)]
#![feature(specialization)]
#![feature(type_alias_impl_trait)]

#[macro_use]
extern crate thiserror;

/* mod data;
mod db;
mod db_de;
mod db_ser; */
mod store;
mod hash;
pub mod link;

pub use store::{Datastore, DatastoreError, LinkArena};
pub use hash::Hash;
pub use link::{HashType, TypedHash};

#[cfg(test)]
mod tests {
	use crate::{Datastore, HashType, LinkArena, store::{ArchiveFetchable, ArchiveInterpretable, ArchiveStorable, ArchiveStore, ArchiveStoreRead, TypeStore}};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let string = db.store(String::from("hello"));

		let arena = LinkArena::new();
		assert_eq!(*string.fetch(db, &arena).unwrap(), "hello");
	}

	#[test]
	fn test_loading() {
		// Self-referential type
		#[derive(Debug, Hash, PartialEq, Clone, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
		// To use the safe API, you have to derive CheckBytes for the archived type
		#[archive_attr(derive(bytecheck::CheckBytes, Debug))]
		// #[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: ArchiveStorable"))]
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
		impl<'a, S: ArchiveStore> ArchiveStorable<S> for StringType<'a> {}
		impl<'s, 'a, S: ArchiveStoreRead> ArchiveInterpretable<'s, S> for StringType<'a> {}
		impl<'s, 'a, S: ArchiveStoreRead, A: TypeStore<'a>> ArchiveFetchable<'s, 'a, S, A> for StringType<'a> {}
		/* impl<'a> NativeHashtype for StringType<'a> {
			type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = crate::Hash> + 's;

			fn reverse_links<'s, S: DatastoreSerializer>(&'s self, _ser: &'s mut S) -> Self::LinkIter<'s, S> {
				std::iter::empty()
			}
		} */

		let links = LinkArena::new();
		let string = links.add(StringType::String("Hello".into()));
		let string2 = links.add(StringType::Link(string, string));

		let db = &mut Datastore::new();

		// let ser = &mut LinkSerializer::new();
		let hash = string2.store(db);
		// let hash = string2.store(&mut ser.join(db));

		let links_de = LinkArena::new();
		let ret = hash.fetch(db, &links_de).unwrap();
		assert_eq!(ret, string2);
	}
}
