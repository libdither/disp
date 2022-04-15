
use std::iter;

use hashdb::{Datastore, DatastoreDeserializer, DatastoreError, DatastoreSerializer, Hash, HashSerializer, Link, NativeHashtype, hashtype::LinkIterConstructor};
use crate::lambda_calculus::Expr;

#[derive(Debug, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
#[archive_attr(derive(Debug, bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer"))]
pub struct Symbol {
	pub name: Link<String>,
	pub expr: Link<Expr>,
}
impl Symbol {
	pub fn new(name: impl Into<String>, expr: &Link<Expr>, ser: &mut HashSerializer) -> Link<Symbol> {
		Link::new(Self { name: Link::new(name.into()), expr: expr.clone() }).store(ser).fetch(ser.db).unwrap()
	}
}
impl NativeHashtype for Symbol {
	type LinkIter = SymbolLinkIter;
}
pub struct SymbolLinkIter {
	iter: iter::Chain<iter::Once<Hash>, iter::Once<Hash>>,
}
impl Iterator for SymbolLinkIter {
	type Item = Hash;
	fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}
impl LinkIterConstructor<Symbol> for SymbolLinkIter {
	fn construct(symbol: &Symbol) -> Self {
		SymbolLinkIter {
			iter: iter::once(symbol.name.as_ref().calc_hash().into())
				.chain(iter::once(symbol.name.as_ref().calc_hash().into()))
		}
	}
}