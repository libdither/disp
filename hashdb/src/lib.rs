#![allow(unused)]
#![allow(incomplete_features)]
#![allow(const_evaluatable_unchecked)]

#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_mut_refs)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(generic_associated_types)]

#[macro_use]
extern crate thiserror;

mod hash;
mod data;
mod db;
pub mod hashtype;
pub mod rkyv_map;

pub use db::{Datastore, DatastoreError};
pub use hashtype::{NativeHashtype, TypedHash};
pub use hash::Hash;
pub use data::Data;

#[cfg(test)]
mod tests {
	use crate::{Datastore, Hash, NativeHashtype};

	#[test]
	fn test_db() {
		let db = &mut Datastore::new();
		let ser = &mut db.serializer();
		//let data = Data::new(&[01u8, 32u8]);
		let string = String::from("hello").store(ser);
		assert_eq!(*string.fetch(ser.db).unwrap(), "hello");
	}
}
