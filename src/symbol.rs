
use std::iter;

use hashdb::{Datastore, DatastoreDeserializer, DatastoreError, DatastoreSerializer, Hash, Link, LinkArena, NativeHashtype, hashtype::LinkIterConstructor};
use crate::lambda_calculus::Expr;

#[derive(Debug, Hash, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
#[archive_attr(derive(Debug, bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub struct Symbol<'a> {
	pub name: Link<'a, String>,
	pub expr: Link<'a, Expr<'a>>,
}
impl<'a> Symbol<'a> {
	pub fn new(name: impl Into<String>, expr: &Link<'a, Expr<'a>>, exprs: &'a LinkArena<'a>) -> Link<'a, Symbol<'a>> {
		exprs.add(Self { name: exprs.add(name.into()), expr: expr.clone() })
	}
}
impl<'a> NativeHashtype for Symbol<'a> {
	type LinkIter = SymbolLinkIter;
}
pub struct SymbolLinkIter {
	iter: iter::Chain<iter::Once<Hash>, iter::Once<Hash>>,
}
impl Iterator for SymbolLinkIter {
	type Item = Hash;
	fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}
impl<'a> LinkIterConstructor<Symbol<'a>> for SymbolLinkIter {
	fn construct(symbol: &Symbol, ser: &mut HashSerializer) -> Self {
		SymbolLinkIter {
			iter: iter::once(symbol.name.calc_hash(ser).into())
				.chain(iter::once(symbol.name.calc_hash(ser).into()))
		}
	}
}