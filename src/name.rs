use std::iter;

use rkyv::{Archive, Serialize, Deserialize, with::Map};
use bytecheck::CheckBytes;

use hashdb::{DatastoreDeserializer, DatastoreSerializer, Hash, HashType, LinkArena, LinkSerializer, NativeHashtype};
use crate::expr::Expr;

/// Object in disp that has a name
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'e>"))]
pub enum NamedObject<'e> {
	Namespace(#[with(HashType)] #[omit_bounds] &'e Namespace<'e>),
	Expr(#[with(HashType)] #[omit_bounds] &'e Expr<'e>),
}
impl<'e> NativeHashtype for NamedObject<'e> {
	type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash> + 's;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, ser: &'s mut S) -> Self::LinkIter<'s, S> {
		iter::empty()
	}
}

/// Object that contains a named object and its name. Both name and expression are reverse-linked to this object.
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'e>"))]
pub struct Name<'e> {
	#[with(HashType)] #[omit_bounds]
	pub string: &'e String,
	pub object: NamedObject<'e>,
}
impl<'e> Name<'e> {
	pub fn new(name: impl Into<String>, expr: &'e Expr<'e>, exprs: &'e LinkArena<'e>, ser: &mut LinkSerializer) -> &'e Name<'e> {
		let name = name.into();
		exprs.add_with_lookups(Self {
			string: exprs.add(name),
			object: NamedObject::Expr(expr),
		}, ser)
	}
}

impl<'e> NativeHashtype for Name<'e> {
	type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash>;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, ser: &'s mut S) -> Self::LinkIter<'s, S> {
		iter::once(self.string.store(ser).into()).chain(iter::once(self.object.store(ser).into()))
	}
}

// A list of names
#[derive(Clone, Hash, Debug, Archive, Serialize, Deserialize, Default)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'e>"))]
pub struct Namespace<'e> {
	#[with(Map<HashType>)] #[omit_bounds]
	items: Vec<&'e Name<'e>>,
}
impl<'e> Namespace<'e> {
	pub fn add(&mut self, name: &'e Name<'e>) {
		self.items.push(name);
	}
}
impl<'e> NativeHashtype for Namespace<'e> {
	type LinkIter<'s, S: DatastoreSerializer> where S: 's, Self: 's = impl Iterator<Item = Hash>;

	fn reverse_links<'s, S: DatastoreSerializer>(&'s self, ser: &'s mut S) -> Self::LinkIter<'s, S> {
		self.items.iter().map(|item|item.store(ser).into())
	}
}