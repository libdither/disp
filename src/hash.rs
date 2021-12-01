#![allow(dead_code)]

use std::fmt;

use multihash::{Code, MultihashGeneric, MultihashDigest, U64};
use base58::ToBase58;

use crate::data::Data;

pub type Multihash = MultihashGeneric<U64>;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Hash(Multihash);
impl From<&[u8]> for Hash {
    fn from(data: &[u8]) -> Self {
        Self(Code::Sha2_256.digest(data))
    }
}
impl From<&Data> for Hash {
	fn from(data: &Data) -> Self {
		Self(Code::Sha2_256.digest(data.as_bytes()))
	}
}
impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", ToBase58::to_base58(&self.0.to_bytes()[..]))
    }
}
impl Hash {
	pub fn new(data: &Data) -> Self { data.into() }
	pub fn inner(&self) -> &Multihash { &self.0 }
	pub fn into_inner(self) -> Multihash { self.0 }
}