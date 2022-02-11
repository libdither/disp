use std::{any::{TypeId, type_name}, cell::Cell};

use rkyv::{Archived, validation::validators::DefaultValidator};
use bytecheck::CheckBytes;
use serde::{Serialize, Deserialize};

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
	is_archived: bool,
	#[serde(skip)]
	archived_type: Cell<Option<TypeId>>,
}

impl Data {
	/// Create a new data type from raw data
	pub fn new(data: Vec<u8>) -> Self {
		Self { data, is_archived: false, archived_type: Cell::new(None) }
	}
	pub fn as_bytes(&self) -> &[u8] { &self.data }
	/// Create a new data type with associated Archived Rust Type
	pub fn new_typed<'a, T: NativeHashtype>(data: Vec<u8>) -> Result<Self, DataError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		let ret = Self {
			data,
			is_archived: true,
			archived_type: Cell::new(None),
		};
		ret.assert_type::<T>()?;
		Ok(ret)
	}
	/// Create Data object with associated Archived type
	pub unsafe fn new_typed_unsafe<T: NativeHashtype>(data: Vec<u8>) -> Self {
		Self { data, is_archived: true, archived_type: Cell::new(Some(TypeId::of::<T>())) }
	}
	/// Get Archived<T> if exists
	pub fn archived<'a, T: NativeHashtype>(&'a self) -> Result<&Archived<T>, DataError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		if self.is_archived {
			if Some(TypeId::of::<T>()) == self.archived_type.get() {
				Ok(unsafe { rkyv::archived_root::<T>(&self.data) })
			} else {
				let out = rkyv::check_archived_root::<'a, T>(&self.data).map_err(|_|DataError::ArchiveInvalidAsType(type_name::<T>()))?;
				self.archived_type.set(Some(TypeId::of::<T>()));
				Ok(out)
			}
		} else { Err(DataError::NotAnArchive) }
		
	}
	pub fn assert_type<'a, T: NativeHashtype>(&self) -> Result<(), DataError>
	where T::Archived: CheckBytes<DefaultValidator<'a>>,
	{
		if self.is_archived {
			if Some(TypeId::of::<T>()) == self.archived_type.get() {
				Ok(())
			} else {
				//rkyv::check_archived_root::<'a, T>(&self.data).map_err(|_|DataError::ArchiveInvalidAsType(type_name::<T>()))?;
				// Safety: IDK if this is safe, I'm just going to assume that it is, in the future, Archived data should be stored in an Arena datastype in Datastore
				unsafe {
					let len = self.data.len();
					let ptr = self.data.as_ptr();
					let validator = &mut DefaultValidator::new(std::slice::from_raw_parts(ptr, len));
					T::Archived::check_bytes(ptr.cast(), validator).map_err(|_|DataError::ArchiveInvalidAsType(type_name::<T>()))?;
				}
				Ok(())
			}
		} else { Err(DataError::NotAnArchive) }
	}
	pub fn hash(&self) -> Hash { Hash::hash(&self.data) }
}