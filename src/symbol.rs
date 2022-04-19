
use std::iter;

use hashdb::{DatastoreDeserializer, DatastoreSerializer, Hash, HashType, LinkArena, NativeHashtype, hashtype::HashIterConstructor};
use crate::expr::Expr;

#[derive(Debug, Hash, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub struct Symbol<'a> {
	#[with(HashType)] pub name: &'a String,
	#[with(HashType)] pub expr: &'a Expr<'a>,
}
impl<'a> Symbol<'a> {
	pub fn new(name: impl Into<String>, expr: &&'a Expr<'a>, exprs: &'a LinkArena<'a>) -> &'a Symbol<'a> {
		exprs.add(Self { name: exprs.add(name.into()), expr: expr.clone() })
	}
}
impl<'a> NativeHashtype for Symbol<'a> {
	type LinkIter<S: DatastoreSerializer> = SymbolLinkIter;
}
pub struct SymbolLinkIter {
	iter: iter::Chain<iter::Once<Hash>, iter::Once<Hash>>,
}
impl Iterator for SymbolLinkIter {
	type Item = Hash;
	fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}
impl<'a, S: DatastoreSerializer> HashIterConstructor<S, Symbol<'a>> for SymbolLinkIter {
	fn construct(symbol: &Symbol, ser: &mut S) -> Self {
		SymbolLinkIter {
			iter: iter::once(symbol.name.store(ser).into())
				.chain(iter::once(symbol.name.store(ser).into()))
		}
	}
}