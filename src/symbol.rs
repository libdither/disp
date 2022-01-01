
use hashdb::Data;

use crate::lambda_calculus::{Datatype, LambdaError};
pub struct Symbol {
	name: String,
}
impl Symbol {
	pub fn new(name: impl Into<String>) -> Self { Self { name: name.into() } }
}
impl Datatype for Symbol {
    type Error = LambdaError;

    fn to_data_untyped(&self) -> hashdb::Data {
        Data::from_vec(bincode::serialize(&self.name).unwrap())
    }

    fn from_data_untyped(data: &hashdb::Data) -> Result<Self, Self::Error> {
        Ok(Symbol { name: bincode::deserialize(data.as_bytes())? } )
    }

    fn db_error(hash: hashdb::Hash) -> Self::Error {
        LambdaError::NotInDatastore(hash)
    }
}