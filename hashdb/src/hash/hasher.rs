

/// Hasher trait, hashers implement this, generic over hasher size
pub trait Hasher<const S: usize> {
	fn new() -> Self;
	fn update(&mut self, data: impl AsRef<[u8]>);
	fn finalize(self) -> [u8; S];
	fn digest(data: impl AsRef<[u8]>) -> [u8; S] {
		let mut hasher = Self::new();
		hasher.update(data);
		hasher.finalize()
	}
}