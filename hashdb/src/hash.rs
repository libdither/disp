#![allow(dead_code)]
use bytecheck::CheckBytes;
use rkyv::{
	ser::{ScratchSpace, Serializer},
	Archive, Archived, Fallible,
};
use serde::{Deserialize, Serialize};
/// This file is a clusterfudge of generic where expressions, hopefully this is made easier in the future...
use std::{fmt, io::Read, mem::ManuallyDrop, str::FromStr};

#[cfg(feature = "async")]
use futures::{AsyncRead, AsyncReadExt};

mod code;
mod hasher;
use code::{calc_varint_len, Code};
pub use hasher::TrimHasher;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Hash([u8; Code::Sha2_256.total_len()]);
//pub type Hash = Multihash<{Code::Sha2_256}>;

impl fmt::Debug for Hash {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Hash({})", self)
	}
}

impl Hash {
	pub fn hash(data: &[u8]) -> Self {
		let mhash = Multihash::<{ Code::Sha2_256 }>::hash(data);
		unsafe { Self(mhash.data) }
	}
	pub const fn from_digest(digest: [u8; Code::Sha2_256.digest_len()]) -> Self {
		// This should be using ..Default::default(), but const traits aren't in rust yet
		let multihash = Multihash::<{ Code::Sha2_256 }> {
			structure: ManuallyDrop::new(InternalMultihashStructure {
				digest,
				code: gen_varint_const_array::<{ Code::Sha2_256.format_code() }>(),
				length: gen_varint_const_array::<{ Code::Sha2_256.digest_len() }>(),
			}),
		};
		unsafe { Self(multihash.data) }
	}
	pub const fn from_array(array: [u8; Code::Sha2_256.total_len()]) -> Self {
		Self(array)
	}
	pub fn from_reader(mut reader: impl Read) -> std::io::Result<Self> {
		let mut hash = Self::default();
		reader.read_exact(&mut hash.0)?;
		Ok(hash)
	}
	#[cfg(feature = "async")]
	pub async fn from_async_reader<A: AsyncRead + Unpin>(mut reader: A) -> std::io::Result<Self> {
		let mut hash = Self::default();
		reader.read_exact(&mut hash.0).await?;
		Ok(hash)
	}
	/// Used for defining type primitives
	/* pub const fn const_digest(data: &[u8]) -> Self {
		let buf: [u8; C.digest_len()];
		let buf_mut = buf.split_at_mut(data.len()).0;
		buf_mut.copy_from_slice(data);
		Self::from_digest(buf)
	} */
	pub const fn len() -> usize {
		Code::Sha2_256.total_len()
	}
	/// Safety: This returns immutable data with no memory pointers
	pub fn as_bytes(&self) -> &[u8] {
		&self.0
	}
	// pub fn digest(&self) -> &[u8] { unsafe { &self.structure.digest } }
}

impl AsRef<[u8]> for Hash {
	fn as_ref(&self) -> &[u8] {
		&self.0
	}
}

impl Default for Hash {
	fn default() -> Self {
		let multihash = Multihash::<{ Code::Sha2_256 }>::default();
		unsafe { Self(multihash.data) }
	}
}
impl Serialize for Hash {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		serializer.serialize_bytes(self.as_bytes())
	}
}
impl fmt::Display for Hash {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", bs58::encode(self).into_string())
	}
}

struct HashVisitor;

impl<'de> serde::de::Visitor<'de> for HashVisitor {
	type Value = Hash;

	fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		formatter.write_str("an integer between -2^31 and 2^31")
	}
	fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
	where
		E: serde::de::Error,
	{
		Ok(Hash::from_reader(v).map_err(|e| serde::de::Error::custom(e))?)
	}
}

impl<'de> Deserialize<'de> for Hash {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		deserializer.deserialize_bytes(HashVisitor)
	}
}

impl Archive for Hash {
	type Archived = Hash;
	type Resolver = [(); Code::Sha2_256.total_len()];

	#[inline]
	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		// Treat Multihash<C> as a const-size array of bytes (C.total_len() in size)
		self.0.resolve(pos, resolver, out.cast());
	}
}

impl<S: ScratchSpace + Serializer + ?Sized> rkyv::Serialize<S> for Hash {
	fn serialize(&self, _serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		Ok([(); Code::Sha2_256.total_len()])
	}
}
impl<D: ?Sized + Fallible> rkyv::Deserialize<Hash, D> for Archived<Hash> {
	fn deserialize(&self, _deserializer: &mut D) -> Result<Hash, D::Error> {
		Ok(self.clone())
	}
}

impl<S: ?Sized> CheckBytes<S> for Hash {
	type Error = unsigned_varint::decode::Error;

	unsafe fn check_bytes<'a>(value: *const Self, _context: &mut S) -> Result<&'a Self, Self::Error> {
		let code = unsigned_varint::decode::usize(&(*value).0)?;
		if code.0 == Code::Sha2_256.format_code() {
			Ok(&*value)
		} else {
			Err(unsigned_varint::decode::Error::Insufficient)
		}
	}
}

/// FromStr implemente to convert from Base58 encodings
impl FromStr for Hash {
	type Err = bs58::decode::Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut hash = Hash::default();
		bs58::decode(s).into(&mut hash.0)?;
		Ok(hash)
	}
}

#[repr(C)] // For predictable memory layout
#[derive(PartialEq, Eq, Clone, Hash)]
struct InternalMultihashStructure<const C: Code>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized,
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
			break;
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
			code: gen_varint_const_array::<{ C.format_code() }>(),
			length: gen_varint_const_array::<{ C.digest_len() }>(),
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
	[(); C.total_len()]: Sized,
{
	structure: ManuallyDrop<InternalMultihashStructure<C>>,
	data: [u8; C.total_len()],
}
impl<const C: Code> Default for Multihash<C>
where
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized,
{
	fn default() -> Self {
		Multihash::<C> {
			structure: ManuallyDrop::new(InternalMultihashStructure::default()),
		}
	}
}
impl<const C: Code> PartialEq for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized,
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
	[(); C.total_len()]: Sized,
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
	[(); C.total_len()]: Sized,
{
	fn clone(&self) -> Self {
		unsafe { Multihash::from_array(self.data) }
	}
}

/* impl<const C: Code> Serialize for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
			S: serde::Serializer {
		serializer.serialize_bytes(self.as_bytes())
	}
}

struct MultihashVisitor<const C: Code>;

impl<'de, const C: Code> serde::de::Visitor<'de> for MultihashVisitor<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	type Value = Multihash<C>;

	fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		formatter.write_str("an integer between -2^31 and 2^31")
	}
	fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
	where
			E: serde::de::Error, {
		Ok(Multihash::from_reader(v).map_err(|e|serde::de::Error::custom(e))?)
	}

}

impl<'de, const C: Code> Deserialize<'de> for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
			D: serde::Deserializer<'de> {
		deserializer.deserialize_bytes(MultihashVisitor::<C>)
	}
} */

// TODO: This throws the compiler bug:
// "error: internal compiler error: compiler/rustc_middle/src/ty/normalize_erasing_regions.rs:179:90: Failed to normalize <hashdb::hash::Multihash<C> as rkyv::Archive>::Resolver, maybe try to call `try_normalize_erasing_regions` instead"
// currently working around it by implementing directly on Hash
/* impl<const C: Code> Archive for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	type Archived = Multihash<C>;
	type Resolver = [(); C.total_len()];

	#[inline]
	unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		// Treat Multihash<C> as a const-size array of bytes (C.total_len() in size)
		self.data.resolve(pos, resolver, out.cast());
	}
}

impl<const C: Code, S: ScratchSpace + Serializer + ?Sized> rkyv::Serialize<S> for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	fn serialize(&self, _serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		Ok([(); C.total_len()])
	}
}

impl<S: ?Sized, const C: Code> CheckBytes<S> for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized
{
	type Error = unsigned_varint::decode::Error;

	unsafe fn check_bytes<'a>(value: *const Self, _context: &mut S) -> Result<&'a Self, Self::Error> {
		let code = unsigned_varint::decode::usize(&(*value).structure.code)?;
		if Code::valid_code(code.0) { Ok(&*value) } else { Err(unsigned_varint::decode::Error::Insufficient) }
	}
} */

impl<const C: Code> Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized,
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
		Self {
			structure: ManuallyDrop::new(InternalMultihashStructure {
				digest,
				code: gen_varint_const_array::<{ C.format_code() }>(),
				length: gen_varint_const_array::<{ C.digest_len() }>(),
			}),
		}
	}
	pub const fn from_array(array: [u8; C.total_len()]) -> Self {
		Self { data: array }
	}
	pub fn from_reader(mut reader: impl Read) -> std::io::Result<Self> {
		let mut hash = Self::default();
		unsafe {
			reader.read_exact(&mut hash.data)?;
		}
		Ok(hash)
	}
	pub const fn len() -> usize {
		C.total_len()
	}
	/// Safety: This returns immutable data with no memory pointers
	pub fn as_bytes(&self) -> &[u8] {
		unsafe { &self.data }
	}
	pub fn digest(&self) -> &[u8] {
		unsafe { &self.structure.digest }
	}
}

impl<const C: Code> fmt::Debug for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", bs58::encode(self.as_bytes()).into_string())
	}
}
impl<const C: Code> fmt::Display for Multihash<C>
where
	[(); calc_varint_len(C.digest_len())]: Sized,
	[(); calc_varint_len(C.format_code())]: Sized,
	[(); C.digest_len()]: Sized,
	[(); C.total_len()]: Sized,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let hash_type = match C {
			Code::Sha2_256 => "sha2_256",
			Code::Sha2_512 => "sha2_512",
			_ => "???",
		};
		write!(f, "{}:{}", hash_type, bs58::encode(self.digest()).into_string())
	}
}

#[test]
fn test_hashes() {
	let hash = Multihash::<{ Code::Sha2_256 }>::hash("Hello, World!\n".as_bytes());
	assert_eq!("QmbuQbmm7z1AeZjbBw2iX1557ZoUFQ8vrMKaaw2UYrt5zG".to_owned(), format!("{:?}", hash))
}
