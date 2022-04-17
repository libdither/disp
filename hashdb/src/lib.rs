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
pub use link::{Link, ArchivedLink, LinkType, LinkArc};
pub use db_ser::{DatastoreSerializer, LinkSerializer, DatastoreLinkSerializer};
pub use db_de::{DatastoreDeserializer, ArenaHashDeserializer, ArcHashDeserializer, LinkArcs, LinkArena};
pub use hash::Hash;
pub use data::Data;

#[cfg(test)]
mod tests {
	use crate::{Datastore, Hash, LinkArena, LinkSerializer, NativeHashtype};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let ser = &mut LinkSerializer::new(); let ser = &mut ser.join(db);
		//let data = Data::new(&[01u8, 32u8]);
		let string = String::from("hello").store(ser);
		let de = LinkArena::new(); let de = &mut de.join(db);
		assert_eq!(string.fetch(de).unwrap(), "hello");
	}
}
