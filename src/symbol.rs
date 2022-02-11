
use std::iter;

use hashdb::{Datastore, DatastoreError, Hash, NativeHashtype, TypedHash, hashtype::LinkIterConstructor};
use crate::lambda_calculus::Expr;

#[derive(Debug, rkyv::Archive, rkyv::Serialize)]
#[archive(compare(PartialEq))]
#[archive_attr(derive(Debug, bytecheck::CheckBytes))]
pub struct Symbol {
	name: TypedHash<String>,
	expr: TypedHash<Expr>,
}
impl Symbol {
	pub fn new(name: impl Into<String>, expr: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Symbol> {
		Self { name: name.into().store(db), expr: expr.clone() }.store(db)
	}
}

impl ArchivedSymbol {
	pub fn expr(&self) -> TypedHash<Expr> {
		self.expr.clone()
	}
	pub fn name<'a>(&self, db: &'a Datastore) -> Result<&'a rkyv::Archived<String>, DatastoreError> { self.name.fetch(db) }
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
		SymbolLinkIter { iter: iter::once(symbol.name.as_hash()).chain(iter::once(symbol.name.as_hash())) }
	}
}