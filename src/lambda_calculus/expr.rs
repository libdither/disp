use std::fmt;

use lazy_static::lazy_static;
use rkyv::{Archive, Serialize};
use bytecheck::CheckBytes;
//use serde::{Serialize, Deserialize};

use hashdb::{Datastore, NativeHashtype, TypedHash};

use crate::{symbol::Symbol};

use super::{DisplayWithDatastore, DisplayWithDatastoreError};

/* 
pub const VARIABLE:    Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
pub const LAMBDA:      Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
pub const APPLICATION: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);
 */

pub mod pointer_helpers {
	#![allow(unused)]
    use hashdb::{Datastore, TypedHash, NativeHashtype};
    use super::{PointerTree, PT_NONE};

	pub fn none(db: &mut Datastore) -> TypedHash<PointerTree> { PointerTree::None.store(db) }
	pub fn end(db: &mut Datastore) -> TypedHash<PointerTree> { PointerTree::End.store(db) }
	pub fn left(p: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		PointerTree::Branch(p, PT_NONE.clone()).store(db)
	}
	pub fn right(p: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		PointerTree::Branch(PT_NONE.clone(), p).store(db)
	}
	pub fn both(l: TypedHash<PointerTree>, r: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		PointerTree::Branch(l, r).store(db)
	}
}


/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize)]
#[archive(compare(PartialEq))]
#[archive_attr(derive(CheckBytes, Debug))]
pub enum PointerTree {
	None,
	End,
	Branch(TypedHash<PointerTree>, TypedHash<PointerTree>), // u32 represents highest variable abstraction level in this expression
}
lazy_static! {
	pub static ref PT_NONE: TypedHash<PointerTree> = PointerTree::None.hash();
	pub static ref PT_BOTH_NONE: TypedHash<PointerTree> = PointerTree::Branch(PT_NONE.clone(), PT_NONE.clone()).hash();
}

impl NativeHashtype for PointerTree {}
impl DisplayWithDatastore for TypedHash<PointerTree> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError> {
		match self.fetch(db).unwrap() { 
			ArchivedPointerTree::Branch(left, right) => {
				match (right.fetch(db).unwrap() == &PointerTree::None, left.fetch(db).unwrap() == &PointerTree::None) {
					(true, true) => write!(f, "BOTH(NONE, NONE)")?,
					(true, false) => write!(f, "<{}", left.display(db))?,
					(false, true) => write!(f, ">{}", right.display(db))?,
					(false, false) => write!(f, "({},{})", left.display(db), right.display(db))?,
				}
			},
			ArchivedPointerTree::End => write!(f, ".")?,
			ArchivedPointerTree::None => {},
		}
		Ok(())
	}
}

#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize)]
#[archive_attr(derive(CheckBytes, Debug))]
pub enum Expr {
	Variable,
	Lambda { tree: TypedHash<PointerTree>, expr: TypedHash<Expr> },
	Application { func: TypedHash<Expr>, sub: TypedHash<Expr> },
}
impl NativeHashtype for Expr {}
impl DisplayWithDatastore for TypedHash<Expr> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError> {
		match db.lookup_typed::<Expr, Symbol>(&self) {
			Ok(symbol) => write!(f, "{}", symbol.fetch(db)?.name(db)?)?,
			Err(_) => {},
		}
		match self.fetch(db)? {
			ArchivedExpr::Variable => write!(f, "x")?,
			ArchivedExpr::Lambda { tree, expr } => {
				// let arena = ReduceArena::new();
				// let mut tree = arena.index();
				// let expr = arena.push_lambda(&mut tree, &self, db).unwrap();
		
				write!(f, "(λ[{}] {})", tree.display(db), expr.display(db))?
			},
			ArchivedExpr::Application { func, sub } => {
				write!(f, "({} {})", func.display(db), sub.display(db))?
			},
		}
		Ok(())
	}
}


impl Expr {
	pub fn var(db: &mut Datastore) -> TypedHash<Expr> {
		Expr::Variable.store(db)
	}
	pub fn lambda(pointer: TypedHash<PointerTree>, expr: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		/* let arena = ReduceArena::new();
		let mut tree = arena.none();
		arena.push_pointer_tree(&mut tree, 0, &pointer, db).unwrap();
		arena.pop_lambda(tree, &mut index, expr.clone(), db).unwrap() */
		Expr::Lambda { tree: pointer, expr: expr.clone() }.store(db)
	}
	pub fn app(function: &TypedHash<Expr>, substitution: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		Expr::Application { func: function.clone(), sub: substitution.clone() }.store(db)
	}
}