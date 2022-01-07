#![allow(dead_code)]

#[derive(Clone)]
pub struct Data {
	data: Vec<u8>,
}
impl Data {
	pub fn new(data: &[u8]) -> Self {
		Self { data: data.to_vec() }
	}
	pub fn from_vec(data: Vec<u8>) -> Self {
		Self { data }
	}
	pub fn as_bytes(&self) -> &[u8] {
		&self.data[..]
	}
}

impl AsRef<[u8]> for Data {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}