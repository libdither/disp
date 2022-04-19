// TODO: make this function work for all values of usize
pub const fn calc_varint_len(count: usize) -> usize {
	match count {
		0..=127 => 1,
		128..=16383 => 2,
		16384..=2097151 => 3,
		2097152..=268435455 => 4,
		268435456..=34359738368 => 5,
		_ => 10,
	}
	//let buf: &mut [u8; 10];
	//unsigned_varint::encode::usize(count, buf).len()
}
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Code {
	Sha2_256,
	Sha2_512,
	Sha3_256,
}
impl Code {
	pub const fn hasher(self) -> multihash::Code {
		match self {
			Self::Sha2_256 => multihash::Code::Sha2_256,
			Self::Sha2_512 => multihash::Code::Sha2_256,
			_ => panic!("hash: is not support by hashdb"),
		}
	}
	pub const fn digest_len(self) -> usize {
		match self {
			Code::Sha2_256 => 32,
			Code::Sha2_512 => 64,
			_ => panic!("hash: is not supported by hashdb"),
		}
	}
	pub const fn format_code(self) -> usize {
		match self {
			Code::Sha2_256 => 0x12,
			Code::Sha2_512 => 0x13,
			_ => panic!("hash is not supported by hashdb"),
		}
	}
	pub const fn valid_code(code: usize) -> bool {
		match code {
			0x12 => true,
			0x13 => true,
			_ => false,
		}
	}
	pub const fn total_len(self) -> usize {
		calc_varint_len(Self::format_code(self)) + calc_varint_len(Self::digest_len(self)) + Self::digest_len(self)
	}
}
