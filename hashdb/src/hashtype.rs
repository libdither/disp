use std::{marker::PhantomData, ops::Deref};

use rkyv::{Archive, Deserialize, Fallible, with::{ArchiveWith, DeserializeWith, SerializeWith}};

use crate::{ArchiveDeserializer, ArchiveFetchable, ArchiveStorable, ArchiveStore, Hash, TypeStorable, TypeStore, TypedHash};

#[derive(Default)]
pub struct HashType<'a, TS: TypeStore<'a>, T> { _phantom: PhantomData<(TS, &'a T)> }

// impl Archive for any With<&'a T, HashType> if T can be archived itself.
impl<'a, TS: TypeStore<'a>, T: Archive> ArchiveWith<TS::Ref<T>> for HashType<'a, TS, T> {
	type Archived = TypedHash<T>;
	type Resolver = Hash;

	#[inline]
	unsafe fn resolve_with(_field: &TS::Ref<T>, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
		resolver.resolve(pos, [(); Hash::len()], out.cast())
	}
}

// impl Serialize for any With<&'a T, HashType> if Archived<T> can be stored into an ArchiveStore
impl<'a, TS: TypeStore<'a>, S: ArchiveStore, T: ArchiveStorable<S>> SerializeWith<TS::Ref<T>, S> for HashType<'a, TS, T> {
	#[inline]
	fn serialize_with(field: &TS::Ref<T>, serializer: &mut S) -> Result<Self::Resolver, <S as Fallible>::Error> {
		Ok(serializer.store::<T>((*field).deref())?.into())
	}
}

/// impl Deserialize for any With<&'a T, HashType> if T has an archived form which can be deserialized into a TypeStore for some lifetime &'a T. 
impl<'a, TS: TypeStore<'a>, T: ArchiveFetchable<'a, TS, D> + TypeStorable, D: ArchiveDeserializer<'a, TS>> DeserializeWith<TypedHash<T>, TS::Ref<T>, D> for HashType<'a, TS, T> {
	#[inline]
	fn deserialize_with(field: &TypedHash<T>, deserializer: &mut D) -> Result<TS::Ref<T>, <D as Fallible>::Error> {
		deserializer.fetch_ref(field)
	}
}

/* impl<'a, T: ArchiveFetchable<'a, D> + TypeStorable, D: ArchiveDeserializer<'a>> Deserialize<TS::Ref<T>, D> for TypedHash<T> {
	fn deserialize(&self, deserializer: &mut D) -> Result<TS::Ref<T>, <D as Fallible>::Error> {
		deserializer.fetch_ref(&self.0)
	}
} */