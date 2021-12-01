mod hash;
mod data;

/// Built-in Types for Dither, recognized by the program as a multihash with the digest set to an arbitrary number
/// Multihash type - 0 as Multihash
/// Type type - 1 as Multihash
/// 	Interpreted as <multiformat's unsigned varint><list of hash digests where the hash type and length are defined by the multihash linking type>
/// Bit type - 2 as Multihash
/// Basic types
/// Byte type - <8><Bit><Bit><Bit><Bit><Bit><Bit><Bit><Bit> as Type
/// Dyte type - <2><Byte><Byte> as Type
/// Qyte type - <4><Byte><Byte><Byte><Byte> as Type
/// Eyte type - <8><Byte><Byte><Byte><Byte><Byte><Byte><Byte><Byte> as Type

pub use hash::Hash;
pub use data::{Data, Database};