use std::fmt;

use thiserror::Error;

use hashdb::{Datastore, DatastoreError};

#[derive(Debug, Error)]
pub enum DisplayWithDatastoreError {
	#[error("Datastore Error: {0}")]
	DatastoreError(#[from] DatastoreError),

	#[error("Format Error: {0}")]
	FormatError(#[from] fmt::Error),
}

pub trait DisplayWithDatastore: Sized {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError>;
	fn display<'a>(&'a self, db: &'a Datastore) -> DatastoreDisplay<'a, Self> {
		DatastoreDisplay(self, db)
	}
}
pub struct DatastoreDisplay<'a, T: DisplayWithDatastore>(pub &'a T, pub &'a Datastore);
impl<'a, T: DisplayWithDatastore> fmt::Display for DatastoreDisplay<'a, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.fmt(f, self.1) {
			Err(DisplayWithDatastoreError::DatastoreError(err)) => { write!(f, "DatastoreError: {}", err) }
			Err(DisplayWithDatastoreError::FormatError(err)) => Err(err),
			_ => Ok(())
		}
	}
}