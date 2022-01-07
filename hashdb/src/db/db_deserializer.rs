
use serde::{de::{Deserializer, Visitor}, Deserialize};

use crate::Datastore;

trait DatastoreDeserializer<'de>: Deserializer<'de> {
	fn get_db<'a>(&'a self) -> &'a mut Datastore;
}

trait DatastoreDeserialize<'de> {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where
    	D: DatastoreDeserializer<'de>;
}
impl<'de, T: DatastoreDeserialize<'de>> Deserialize<'de> for T {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where
    D: Deserializer<'de> {
		deserializer.
	}
}