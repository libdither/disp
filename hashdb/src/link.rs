use std::{fmt, hash::{Hasher as StdHasher, Hash as StdHash}, marker::PhantomData, ops::Deref};

use rkyv::{Archive, Deserialize, Fallible, Serialize, validation::validators::DefaultValidator, with::{ArchiveWith, DeserializeWith, SerializeWith}};
use bytecheck::CheckBytes;

use crate::{Hash, TypedHash, store::{ArchiveDeserializer, ArchiveFetchable, ArchiveInterpretable, ArchiveStorable, ArchiveStore, TypeStorable, TypeStore}};

// Reference that is generic over its TypeStore<'a>
pub struct Link<'a, T: ?Sized + 'a, TS: TypeStore<'a> + 'a> {
	ptr: TS::Ref<T>,
}
impl<'a, T: 'a, TS: TypeStore<'a> + 'a> Deref for Link<'a, T, TS> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.ptr.deref()
    }
}

impl<'a, T: StdHash + 'a, TS: TypeStore<'a> + 'a> Link<'a, T, TS> {
	pub fn new(ptr: TS::Ref<T>) -> Self { Self { ptr } }
}
impl<'a, T: StdHash + 'a, TS: TypeStore<'a> + 'a> StdHash for Link<'a, T, TS> {
	fn hash<H: StdHasher>(&self, state: &mut H) {
		(&*self.ptr).hash(state);
	}
}
impl<'a, T: fmt::Debug + 'a, TS: TypeStore<'a> + 'a> fmt::Debug for Link<'a, T, TS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Link").field(&self.ptr.deref()).finish()
    }
}
impl<'a, T: PartialEq + 'a, TS: TypeStore<'a> + 'a> PartialEq for Link<'a, T, TS> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr.deref() == other.ptr.deref()
    }
}
impl<'a, T: 'a, TS: TypeStore<'a> + 'a> Clone for Link<'a, T, TS> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr.clone() }
    }
}
impl<'a, T: 'a, TS: TypeStore<'a> + 'a> Copy for Link<'a, T, TS>
where
	TS::Ref<T>: Copy,
{}
impl<'a, T: Archive + 'a, TS: TypeStore<'a> + 'a> Archive for Link<'a, T, TS> {
    type Archived = TypedHash<T>;

    type Resolver = Hash;

    unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
        resolver.resolve(pos, [(); Hash::len()], out.cast())
    }
}
impl<'a, S: ArchiveStore, T: ArchiveStorable<S> + 'a, TS: TypeStore<'a> + 'a> Serialize<S> for Link<'a, T, TS> {
    fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
		Ok(serializer.store::<T>(self.ptr.deref())?.into())
    }
}
impl<'a, TS: TypeStore<'a>, D: ArchiveDeserializer<'a, TS>, T: ArchiveFetchable<'a, TS, D>> Deserialize<Link<'a, T, TS>, D> for TypedHash<T> {
    fn deserialize(&self, deserializer: &mut D) -> Result<Link<'a, T, TS>, <D as Fallible>::Error> {
		deserializer.fetch_link(&self)
    }
}
