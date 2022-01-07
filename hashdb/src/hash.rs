
/// This file is a clusterfudge of generic where expressions, hopefully this is made easier in the future...

use std::{fmt, io::Read, mem::ManuallyDrop};

use base58::ToBase58;

mod code;
/* pub mod hasher;
mod sha2; */
use code::{calc_varint_len, Code};

pub type Hash = Multihash<{Code::Sha2_256}>;


#[repr(C)] // For predictable memory layout
#[derive(PartialEq, Eq, Clone, Hash)]
struct InternalMultihashStructure<const C: Code>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized
{
	code: [u8; calc_varint_len(C.format_code())],
	length: [u8; calc_varint_len(C.digest_len())],
	digest: [u8; C.digest_len()],
}
const fn gen_varint_const_array<const N: usize>() -> [u8; calc_varint_len(N)] {
	let mut n = N;
	let mut i = 0;
	let mut buf: [u8; calc_varint_len(N)] = [0u8; calc_varint_len(N)];
	
	loop {
		let b = &mut buf[i];
		*b = n as u8 | 0x80;
		n >>= 7;
		if n == 0 {
			*b &= 0x7f;
			break
		}
		i += 1
	}
	buf
}
impl<const C: Code> Default for InternalMultihashStructure<C>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized,
{
	fn default() -> Self {
		InternalMultihashStructure {
			code: gen_varint_const_array::<{C.format_code()}>(),
			length: gen_varint_const_array::<{C.digest_len()}>(),
			digest: [0u8; C.digest_len()],
		}
	}
}

/// Generic Structure for Multihash Encoding
#[repr(C)]
#[derive(Eq)]
pub union Multihash<const C: Code>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	structure: ManuallyDrop<InternalMultihashStructure<C>>,
	data: [u8; C.total_len()]
}
impl<const C: Code> Default for Multihash<C>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn default() -> Self {
		Multihash::<C> { structure: ManuallyDrop::new(InternalMultihashStructure::default()) }
	}
}
impl<const C: Code> PartialEq for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn eq(&self, other: &Self) -> bool {
		unsafe { self.data == other.data }
	}
}
impl<const C: Code> std::hash::Hash for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		unsafe { state.write(&self.data) }
	}
}
impl<const C: Code> Clone for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn clone(&self) -> Self {
		unsafe { Multihash::from_array(self.data) }
	}
}

impl<const C: Code> Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	pub fn hash(data: &[u8]) -> Self {
		let mc = Code::hasher(C);
		use multihash::MultihashDigest;
		let mh = mc.digest(data);
		let mh_slice = mh.digest();

		// Safety: mh_slice and structure.digest should always be the same size
		unsafe {
			let mut ret = Self::default();
			(&mut ret.structure).digest.copy_from_slice(mh_slice);
			ret
		}
	}
	pub const fn from_digest(digest: [u8; C.digest_len()]) -> Self {
		// This should be using ..Default::default(), but const traits aren't in rust yet
		Self { structure: ManuallyDrop::new(InternalMultihashStructure { digest,
			code: gen_varint_const_array::<{C.format_code()}>(),
			length: gen_varint_const_array::<{C.digest_len()}>(), }) }
	}
	pub const fn from_array(array: [u8; C.total_len()]) -> Self {
		Self { data: array }
	}
	pub fn from_reader(mut reader: impl Read) -> std::io::Result<Self> {
		let mut hash = Self::default();
		unsafe { reader.read_exact(&mut hash.data)?; }
		Ok(hash)
	}
	/// Used for defining type primitives
	/* pub const fn const_digest(data: &[u8]) -> Self {
		let buf: [u8; C.digest_len()];
		let buf_mut = buf.split_at_mut(data.len()).0;
		buf_mut.copy_from_slice(data);
		Self::from_digest(buf)
	} */
	/// Safety: This returns immutable data with no memory pointers
	pub fn as_bytes(&self) -> &[u8] { unsafe { &self.data } }
	pub fn digest(&self) -> &[u8] { unsafe { &self.structure.digest } }
}

impl<const C: Code> fmt::Debug for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", ToBase58::to_base58(self.as_bytes()))
	}
}
impl<const C: Code> fmt::Display for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let hash_type = match C {
			Code::Sha2_256 => "sha2_256",
			Code::Sha2_512 => "sha2_512",
			_ => "???"
		};
        write!(f, "{}:{}", hash_type, ToBase58::to_base58(self.digest()))
    }
}

#[test]
fn test_hashes() {
	let hash = Multihash::<{Code::Sha2_256}>::hash("Hello, World!\n".as_bytes());
	assert_eq!("QmbuQbmm7z1AeZjbBw2iX1557ZoUFQ8vrMKaaw2UYrt5zG".to_owned(), format!("{:?}", hash))
}