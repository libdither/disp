use std::fmt;

use multihash::{Code, MultihashGeneric, MultihashDigest, U64};
use base58::ToBase58;

#[derive(Debug)]
pub struct Hash(MultihashGeneric<U64>);
impl From<&[u8]> for Hash {
    fn from(data: &[u8]) -> Self {
        Self(Code::Sha2_256.digest(data))
    }
}
impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", ToBase58::to_base58(&self.0.to_bytes()[..]))
    }
}

fn main() {
    let data = "Hello, World!";
    let hash = Hash::from(data.as_bytes());

    println!("Data: {}", data);
    println!("Hash: {}", hash);
}
