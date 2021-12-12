#![allow(dead_code)]

use std::fmt;

use multihash::{Code, MultihashDigest,};
use base58::ToBase58;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Hash(Vec<u8>);

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", ToBase58::to_base58(self.as_bytes()))
    }
}
impl Hash {
	pub fn hash(data: impl AsRef<[u8]>) -> Self { Self(Code::Sha2_256.digest(data.as_ref()).to_bytes()) }
	pub fn inner(&self) -> &Vec<u8> { &self.0 }
	pub fn into_inner(self) -> Vec<u8> { self.0 }
	pub fn as_bytes(&self) -> &[u8] { &self.0[..] }
}