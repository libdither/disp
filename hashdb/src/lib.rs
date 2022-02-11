
#![allow(incomplete_features)]
#![allow(const_evaluatable_unchecked)]

#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_for)]
#![feature(const_mut_refs)]
#![feature(unsized_tuple_coercion)]
#![feature(type_name_of_val)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(generic_associated_types)]

#[macro_use]
extern crate thiserror;

mod hash;
mod data;
mod db;
mod typed;
pub mod hashtype;

pub use db::{Datastore, DatastoreError};
pub use hashtype::{NativeHashtype, TypedHash};
pub use hash::Hash;
pub use data::Data;
//pub use typed::*;

#[cfg(test)]
mod tests {
    use crate::{Datastore, NativeHashtype};

    #[test]
    fn test_db() {
		let db = &mut Datastore::new();
		//let data = Data::new(&[01u8, 32u8]);
        let string = String::from("hello").store(db);
        assert_eq!(string.as_bytes(), b"hello");
    }
}
