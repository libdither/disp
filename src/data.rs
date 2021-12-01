#![allow(dead_code)]

use std::collections::HashMap;

use super::Hash;

#[derive(Clone)]
pub struct Data {
	data: Vec<u8>,
}
impl Data {
	pub fn new(data: &[u8]) -> Self {
		Self { data: data.to_vec() }
	}
	pub fn as_bytes(&self) -> &[u8] {
		&self.data[..]
	}
}
impl<'a> Into<&'a [u8]> for &'a Data {
	fn into(self) -> &'a [u8] {
		self.as_bytes()
	}
}

#[derive(Default)]
pub struct Database {
	map: HashMap<Hash, Data>,
}

impl Database {
	pub fn add(&mut self, data: Data) -> Hash {
		let hash = Hash::new(&data);
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