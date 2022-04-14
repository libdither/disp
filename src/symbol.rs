
use std::iter;

use hashdb::{Datastore, DatastoreError, Hash, NativeHashtype, Link, DatastoreSerializer, DatastoreDeserializer, hashtype::LinkIterConstructor};
use crate::lambda_calculus::Expr;

#[derive(Debug, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
#[archive_attr(derive(Debug, bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer"))]
pub struct Symbol {
	pub name: Link<String>,
	pub expr: Link<Expr>,
}
impl Symbol {
	pub fn new(name: impl Into<String>, expr: &Link<Expr>, db: &mut Datastore) -> Link<Symbol> {
		Link::new(Self { name: Link::new(name.into()), expr: expr.clone() })
	}
}
impl NativeHashtype for Symbol {
	type LinkIter<'a> = SymbolLinkIter<'a>;
}
pub struct SymbolLinkIter<'a> {
	iter: iter::Chain<iter::Once<&'a Hash>, iter::Once<&'a Hash>>,
}
impl<'a> Iterator for SymbolLinkIter<'a> {
	type Item = &'a Hash;
	fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}
impl<'a> LinkIterConstructor<'a, Symbol> for SymbolLinkIter<'a> {
	fn construct(symbol: &'a Symbol) -> Self {
		SymbolLinkIter { iter: iter::once(symbol.name.hash()).chain(iter::once(symbol.name.as_hash())) }
	}
}