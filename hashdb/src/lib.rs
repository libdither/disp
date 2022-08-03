#![allow(unused_imports)]

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

#[macro_use]
extern crate derivative;

mod store;
mod hash;
pub mod link;
mod typed_hash;

pub use store::{Datastore, DatastoreError, LinkArena, ArchiveStore, ArchiveDeserializer, ArchiveStorable, ArchiveFetchable, TypeStore, TypeStorable};
pub use hash::Hash;
pub use link::Link;
pub use typed_hash::TypedHash;

#[cfg(test)]
mod tests {
	use crate::{Datastore, Link, LinkArena, store::{ArchiveStorable, ArchiveStore, ArchiveDeserializer, TypeStore}};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let string = db.store(&String::from("hello")).unwrap();

		let arena = LinkArena::new();
		assert_eq!(*string.fetch(db, &arena).unwrap(), "hello");
	}

	#[test]
	fn test_loading() {
		// Self-referential type
		#[derive(Derivative, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
		#[derivative(Debug(bound=""), PartialEq(bound=""), Clone(bound=""), Hash(bound=""))] // Requires Derivative because https://github.com/rust-lang/rust/issues/26925
		// To use the safe API, you have to derive CheckBytes for the archived type
		#[archive_attr(derive(bytecheck::CheckBytes, Debug))]
		#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'a, S>"))]
		enum StringType<'a, S: TypeStore<'a> + 'a> {
			String(String),
			Link(
				#[omit_bounds]
				Link<'a, StringType<'a, S>, S>,
				#[omit_bounds]
				Link<'a, StringType<'a, S>, S>,
			),
		}
		/* impl<'a, S: TypeStore<'a> + 'a> std::hash::Hash for StringType<'a, S> {
			fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
				match self {
					SztringType::String(s) => s.hash(state),
					StringType::Link(l, r) => (l, r).hash(state)
				}
			}
		} */

		let links = LinkArena::new();
		let string = links.link(StringType::<'_, LinkArena::<'_>>::String("Hello".into()));
		let string2 = links.add(StringType::Link(string, string));

		let db = &mut Datastore::new();

		// let ser = &mut LinkSerializer::new();
		let hash = string2.store(db).unwrap();
		// let hash = string2.store(&mut ser.join(db));

		let links_de = LinkArena::new();
		let ret = hash.fetch(db, &links_de).unwrap();
		assert_eq!(&*ret, string2);
	}
}
