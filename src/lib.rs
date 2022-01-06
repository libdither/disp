
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(const_for)]
#![feature(const_mut_refs)]
#![feature(unsized_tuple_coercion)]
#![feature(type_name_of_val)]

#[macro_use]
extern crate thiserror;

mod hash;
mod data;
mod db;
mod typed;
pub mod hashtype;

pub use db::Datastore;
pub use hash::Hash;
pub use data::Data;
//pub use typed::*;

#[cfg(test)]
mod tests {
    use crate::{Data, Datastore, Hash};

    #[test]
    fn test_db() {
		let mut db = Datastore::new();
		let data = Data::new(&[01u8, 32u8]);
        let hash = db.add(data);
        let hash_2 =  Hash::hash(&[01u8, 32u8]);
        assert_eq!(hash.as_bytes(), hash_2.as_bytes());
    }
}
