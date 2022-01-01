#![allow(dead_code)]

use std::fmt;

use multihash::{Code, MultihashDigest, MultihashGeneric, U32};
use base58::ToBase58;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Hash(Vec<u8>);

impl fmt::Debug for Hash {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", ToBase58::to_base58(self.as_bytes()))
	}
}
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
	pub fn as_bytes_mut(&mut self) -> &mut [u8] { &mut self.0[..] }
	pub fn len(&self) -> usize { self.0.len() }
	
	pub fn from_raw_bytes(data: &[u8]) -> Self { Self(data.to_vec()) }
	pub fn from_sha256_digest(digest: [u8; 32]) -> Self {
		let multihash = MultihashGeneric::<U32>::wrap(Code::Sha2_256 as u64, &digest).unwrap();
		Self(multihash.to_bytes())
	}
}