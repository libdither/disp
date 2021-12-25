use std::fmt;

use hashdb::Datastore;

pub trait DisplayWithDatastore: Sized {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result;
	fn display<'a>(&'a self, db: &'a Datastore) -> DatastoreDisplay<'a, Self> {
		DatastoreDisplay(self, db)
	}
}
pub struct DatastoreDisplay<'a, T: DisplayWithDatastore>(pub &'a T, pub &'a Datastore);
impl<'a, T: DisplayWithDatastore> fmt::Display for DatastoreDisplay<'a, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.0.fmt(f, self.1)
	}
}