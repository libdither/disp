use std::any::type_name;

use bytecheck::CheckBytes;
use rkyv::{validation::validators::DefaultValidator, Archived};
use serde::{Deserialize, Serialize};

use crate::{Hash, NativeHashtype};

#[derive(Debug, Error)]
pub enum DataError {
	#[error("Marked as Archived, but data not valid as type: {0}")]
	ArchiveInvalidAsType(&'static str),
	#[error("Not an Archive")]
	NotAnArchive,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
	data: Vec<u8>,
}

impl Data {
	/// Create a new data type from raw data
	pub fn new(data: Vec<u8>) -> Self {
		Self { data }
	}
	pub fn as_bytes(&self) -> &[u8] {
		&self.data
	}

	/// Get Archived<T> if correct format
	pub fn archived<'a, T: NativeHashtype>(&'a self) -> Result<&Archived<T>, DataError>
	where
		T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		Ok(rkyv::check_archived_root::<'a, T>(&self.data).map_err(|_| DataError::ArchiveInvalidAsType(type_name::<T>()))?)
	}
	pub unsafe fn archived_unsafe<'a, T: NativeHashtype>(&'a self) -> &Archived<T> {
		rkyv::archived_root::<T>(self.as_bytes())
	}
	pub fn assert_type<'a, T: NativeHashtype + 'a>(&'a self) -> Result<(), DataError>
	where
		T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		// Safety: I don't know if this is safe
		rkyv::check_archived_root::<'a, T>(&self.data).map_err(|_| DataError::ArchiveInvalidAsType(type_name::<T>()))?;
		Ok(())
	}
	pub fn hash(&self) -> Hash {
		Hash::hash(&self.data)
	}
}
