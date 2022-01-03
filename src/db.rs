
use std::collections::HashMap;

use crate::{Data, Hash};

#[derive(Default)]
pub struct Datastore {
	map: HashMap<Hash, Data>,
}

impl Datastore {
    pub fn new() -> Self {
        Self::default()
    }
	pub fn add(&mut self, data: Data) -> Hash {
		let hash = Hash::hash(data.as_bytes());
		if !self.map.contains_key(&hash) {
			if self.map.insert(hash.clone(), data).is_some() { panic!("There should not already be something in here") };
		}  
		hash
	}

	pub fn remove(&mut self, hash: &Hash) -> Option<Data> {
		self.map.remove(hash)
	}
	pub fn get(&self, hash: &Hash) -> Option<&Data> {
		self.map.get(hash)
	}
}