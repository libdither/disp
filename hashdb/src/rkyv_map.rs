// From: https://gist.github.com/djkoloski/880e809f2ba3fcff7d6f2b53a0cb6ef3

use std::{error::Error, hint::unreachable_unchecked, marker::PhantomData, ptr};

use rkyv::{
    archived_root,
    option::ArchivedOption,
    out_field,
    ser::{ScratchSpace, Serializer},
    vec::{ArchivedVec, VecResolver},
    with::{ArchiveWith, DeserializeWith, SerializeWith},
    Archive, Deserialize, Fallible, Infallible, Serialize,
};

// Some type that does not implement Archive but has an ArchiveWith wrapper
/* #[derive(Debug)]
struct Inner {}

#[derive(Archive, Deserialize, Serialize, Debug)]
struct Outer {
    #[with(Map<InnerWrapper>)]
    vec: Vec<Inner>,
    #[with(Map<InnerWrapper>)]
    opt: Option<Inner>,
    #[with(Map<Map<InnerWrapper>>)]
    opt_vec: Option<Vec<Inner>>,
    #[with(Map<Map<InnerWrapper>>)]
    vec_opt: Vec<Option<Inner>>,
} */

// ##### impl {Archive, Deserialize, Serialize}With for wrapper of Inner #####

/* struct InnerWrapper;

impl ArchiveWith<Inner> for InnerWrapper {
    type Archived = ();
    type Resolver = ();

    unsafe fn resolve_with(_: &Inner, _: usize, _: Self::Resolver, _: *mut Self::Archived) {}
}

impl<S: Fallible> SerializeWith<Inner, S> for InnerWrapper {
    fn serialize_with(_: &Inner, _: &mut S) -> Result<Self::Resolver, S::Error> {
        Ok(())
    }
}

impl<D: Fallible> DeserializeWith<(), Inner, D> for InnerWrapper {
    fn deserialize_with(_: &(), _: &mut D) -> Result<Inner, D::Error> {
        Ok(Inner {})
    }
} */

// ##### wrapper for Vecs #####

pub struct Map<Archivable> {
    phantom: PhantomData<Archivable>,
}

impl<A, O> ArchiveWith<Vec<O>> for Map<A>
where
    A: ArchiveWith<O>,
{
    type Archived = ArchivedVec<<A as ArchiveWith<O>>::Archived>;
    type Resolver = VecResolver;

    unsafe fn resolve_with(
        field: &Vec<O>,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        ArchivedVec::resolve_from_len(field.len(), pos, resolver, out)
    }
}

impl<A, O, S> SerializeWith<Vec<O>, S> for Map<A>
where
    S: Fallible + ScratchSpace + Serializer,
    A: ArchiveWith<O> + SerializeWith<O, S>,
{
    fn serialize_with(field: &Vec<O>, s: &mut S) -> Result<Self::Resolver, S::Error> {
        // Wrapper for O so that we have an Archive and Serialize implementation
        // and ArchivedVec::serialize_from_* is happy about the bound constraints
        struct RefWrapper<'o, A, O>(&'o O, PhantomData<A>);

        impl<A: ArchiveWith<O>, O> Archive for RefWrapper<'_, A, O> {
            type Archived = <A as ArchiveWith<O>>::Archived;
            type Resolver = <A as ArchiveWith<O>>::Resolver;

            unsafe fn resolve(
                &self,
                pos: usize,
                resolver: Self::Resolver,
                out: *mut Self::Archived,
            ) {
                A::resolve_with(self.0, pos, resolver, out)
            }
        }

        impl<A, O, S> Serialize<S> for RefWrapper<'_, A, O>
        where
            A: ArchiveWith<O> + SerializeWith<O, S>,
            S: Fallible + Serializer,
        {
            fn serialize(&self, s: &mut S) -> Result<Self::Resolver, S::Error> {
                A::serialize_with(self.0, s)
            }
        }

        let iter = field
            .iter()
            .map(|value| RefWrapper::<'_, A, O>(value, PhantomData));

        ArchivedVec::serialize_from_iter(iter, s)
    }
}

impl<A, O, D> DeserializeWith<ArchivedVec<<A as ArchiveWith<O>>::Archived>, Vec<O>, D>
    for Map<A>
where
    A: ArchiveWith<O> + DeserializeWith<<A as ArchiveWith<O>>::Archived, O, D>,
    D: Fallible,
{
    fn deserialize_with(
        field: &ArchivedVec<<A as ArchiveWith<O>>::Archived>,
        d: &mut D,
    ) -> Result<Vec<O>, D::Error> {
        field
            .iter()
            .map(|value| <A as DeserializeWith<_, _, D>>::deserialize_with(value, d))
            .collect()
    }
}

// ##### wrapper for Options #####

// Copy-paste from Option's impls for the most part
impl<A, O> ArchiveWith<Option<O>> for Map<A>
where
    A: ArchiveWith<O>,
{
    type Archived = ArchivedOption<<A as ArchiveWith<O>>::Archived>;
    type Resolver = Option<<A as ArchiveWith<O>>::Resolver>;

    unsafe fn resolve_with(
        field: &Option<O>,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        match resolver {
            None => {
                let out = out.cast::<ArchivedOptionVariantNone>();
                ptr::addr_of_mut!((*out).0).write(ArchivedOptionTag::None);
            }
            Some(resolver) => {
                let out = out.cast::<ArchivedOptionVariantSome<<A as ArchiveWith<O>>::Archived>>();
                ptr::addr_of_mut!((*out).0).write(ArchivedOptionTag::Some);

                let value = if let Some(value) = field.as_ref() {
                    value
                } else {
                    unreachable_unchecked();
                };

                let (fp, fo) = out_field!(out.1);
                A::resolve_with(value, pos + fp, resolver, fo);
            }
        }
    }
}

impl<A, O, S> SerializeWith<Option<O>, S> for Map<A>
where
    S: Fallible,
    A: ArchiveWith<O> + SerializeWith<O, S>,
{
    fn serialize_with(field: &Option<O>, s: &mut S) -> Result<Self::Resolver, S::Error> {
        field
            .as_ref()
            .map(|value| A::serialize_with(value, s))
            .transpose()
    }
}

impl<A, O, D> DeserializeWith<ArchivedOption<<A as ArchiveWith<O>>::Archived>, Option<O>, D>
    for Map<A>
where
    D: Fallible,
    A: ArchiveWith<O> + DeserializeWith<<A as ArchiveWith<O>>::Archived, O, D>,
{
    fn deserialize_with(
        field: &ArchivedOption<<A as ArchiveWith<O>>::Archived>,
        d: &mut D,
    ) -> Result<Option<O>, D::Error> {
        match field {
            ArchivedOption::Some(value) => Ok(Some(A::deserialize_with(value, d)?)),
            ArchivedOption::None => Ok(None),
        }
    }
}

#[repr(u8)]
enum ArchivedOptionTag {
    None,
    Some,
}

#[repr(C)]
struct ArchivedOptionVariantNone(ArchivedOptionTag);

#[repr(C)]
struct ArchivedOptionVariantSome<T>(ArchivedOptionTag, T);