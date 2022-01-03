

/// Idea for simple pre-lambda Types
/// RustType is a language-specific type that supports semi self-definition. (in that it defines its layout somewhat, but only in relation to Rust's type system)
/// Rust types will eventually be able to be transformed to lambda-constructed types (once that system is fleshed out)

use crate::Hash;
const RUST_TYPE: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0]);

trait RustHashtype {

}

trait Hashtype {

}

struct RustType<T: RustHashtype> {
	value: T,
}
impl<T> Hashtype for RustType<T>
where T: RustHashtype {}

impl RustHashtype for String {

}

fn parse_data(data: &[u8]) -> impl Hashtype {
	RustType::<String> { value: "hello".to_owned() }
}