use std::hash::{BuildHasher, Hasher};

/* /// Hasher trait, hashers implement this, generic over hasher size
pub trait Hasher<const S: usize> {
	fn new() -> Self;
	fn update(&mut self, data: impl AsRef<[u8]>);
	fn finalize(self) -> [u8; S];
	fn digest(data: impl AsRef<[u8]>) -> [u8; S] {
		let mut hasher = Self::new();
		hasher.update(data);
		hasher.finalize()
	}
} */

/// Hasher that just takes the first 64 bits of whatever is passed and uses that as a hash.
/// Makes HashMap more efficient when mapping existing hashes to objects.
/// Warning: any byte array passed to write() should be at least 8 bytes long and be a uniformly distributed random number (i.e. a hash)
#[derive(Default, Clone)]
pub struct TrimHasher {
	state: u64,
}
impl Hasher for TrimHasher {
	fn finish(&self) -> u64 {
		self.state
	}

	fn write(&mut self, bytes: &[u8]) {
		debug_assert!(bytes.len() >= 8);
		let stuff = &bytes[0..8];
		let mut bytes = [0u8; 8];
		bytes.copy_from_slice(stuff);
		self.state = u64::from_be_bytes(bytes);
	}
}
impl BuildHasher for TrimHasher {
	type Hasher = Self;

	fn build_hasher(&self) -> Self::Hasher {
		self.clone()
	}
}
