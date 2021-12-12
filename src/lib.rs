
mod hash;
mod data;
mod db;

pub use db::Datastore;
pub use hash::Hash;
pub use data::Data;

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
