use std::{fmt::Debug, hash::Hash as StdHash, iter, marker::PhantomData};

use rkyv::{
	validation::validators::DefaultValidator,
	with::{ArchiveWith, DeserializeWith, SerializeWith},
	Archive, Deserialize, Fallible, Serialize,
};

use crate::{Datastore, Hash, LinkArena};
use bytecheck::CheckBytes;

/// Represents a Rust type with an rykv Archive implementation that can be fetched from a Datastore via its hash
pub trait NativeHashtype: StdHash + Debug + Archive + Sized {
	/// Calculate hash and data from type
	fn store<S: DatastoreSerializer>(&self, ser: &mut S) -> TypedHash<Self>
	where
		Self: Serialize<S>,
	{
		ser.store(self).map_err(|_| "failed to serialize").unwrap().into()
	}

	type LinkIter<'s, S: DatastoreSerializer>: Iterator<Item = Hash> + 's
	where S: 's, Self: 's;
	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, ser: &'s mut S) -> Self::LinkIter<'s, S>;
}

/* impl<T> NativeHashtype for T
where T: StdHash + Debug + Archive + Sized
{
	type LinkIter<S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash>;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, _ser: &'s mut S) -> Self::LinkIter<'s, S> {
        iter::empty::<Hash>()
    }
} */
impl NativeHashtype for String {
	type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash> + 's;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, _ser: &'s mut S) -> Self::LinkIter<'s, S> {
        iter::empty::<Hash>()
    }
}
impl NativeHashtype for Vec<u8> {
	type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash> + 's;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, _ser: &'s mut S) -> Self::LinkIter<'s, S> {
        iter::empty::<Hash>()
    }
}
