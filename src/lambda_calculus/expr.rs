use std::fmt;

use lazy_static::lazy_static;
use rkyv::{Archive, Serialize, Deserialize};
use bytecheck::CheckBytes;
//use serde::{Serialize, Deserialize};

use hashdb::{Datastore, NativeHashtype, Link, DatastoreSerializer, DatastoreDeserializer};

use crate::{symbol::Symbol};

use super::{DisplayWithDatastore, DisplayWithDatastoreError};

pub mod pointer_helpers {
	#![allow(unused)]
    use hashdb::{Datastore, Link, NativeHashtype};
    use super::{PointerTree, PT_NONE};

	pub fn none(db: &mut Datastore) -> Link<PointerTree> { PT_NONE.clone() }
	pub fn end(db: &mut Datastore) -> Link<PointerTree> { Link::new(PointerTree::End) }
	pub fn left(p: Link<PointerTree>, db: &mut Datastore) -> Link<PointerTree> {
		Link::new(PointerTree::Branch(p, PT_NONE.clone()))
	}
	pub fn right(p: Link<PointerTree>, db: &mut Datastore) -> Link<PointerTree> {
		Link::new(PointerTree::Branch(PT_NONE.clone(), p))
	}
	pub fn both(l: Link<PointerTree>, r: Link<PointerTree>, db: &mut Datastore) -> Link<PointerTree> {
		Link::new(PointerTree::Branch(l, r))
	}
}


/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer"))]
pub enum PointerTree {
	None,
	End,
	Branch(#[omit_bounds] Link<PointerTree>, #[omit_bounds] Link<PointerTree>), // u32 represents highest variable abstraction level in this expression
}
lazy_static! {
	pub static ref PT_NONE: Link<PointerTree> = Link::new(PointerTree::None);
	pub static ref PT_BOTH_NONE: Link<PointerTree> = Link::new(PointerTree::Branch(PT_NONE.clone(), PT_NONE.clone()));
}

impl NativeHashtype for PointerTree {}
impl DisplayWithDatastore for Link<PointerTree> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError> {
		match self.as_ref() { 
			PointerTree::Branch(left, right) => {
				match (**right == PointerTree::None, **left == PointerTree::None) {
					(true, true) => write!(f, "BOTH(NONE, NONE)")?,
					(true, false) => write!(f, "<{}", left.display(db))?,
					(false, true) => write!(f, ">{}", right.display(db))?,
					(false, false) => write!(f, "({},{})", left.display(db), right.display(db))?,
				}
			},
			PointerTree::End => write!(f, ".")?,
			PointerTree::None => {},
		}
		Ok(())
	}
}

#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer"))]
pub enum Expr {
	/// By itself, an unbound term, a unit of undefined meaning, ready for construction
	Variable,
	/// Create a function
	Lambda { tree: Link<PointerTree>, #[omit_bounds] expr: Link<Expr> },
	/// Apply functions to expressions
	Application { #[omit_bounds] func: Link<Expr>, #[omit_bounds] sub: Link<Expr> },
	// Type of all types
	// Type,
	// Create dependent types
	// Dependent { tree: Link<PointerTree>, expr: Link<Expr> },
}
impl NativeHashtype for Expr {}
impl DisplayWithDatastore for Link<Expr> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError> {
		/* match db.lookup_typed::<Expr, Symbol>(&self.store()) {
			Ok(symbol) => write!(f, "{}", symbol.fetch().name(db)?)?,
			Err(_) => {},
		} */
		match self.as_ref() {
			Expr::Variable => write!(f, "x")?,
			Expr::Lambda { .. } => {
				let arena = crate::lambda_calculus::ReduceArena::new();
				let mut index = arena.index();
				let expr = arena.push_lambda(&mut index, &self, db).unwrap();
		
				write!(f, "(λ{}[{}] {})", index.index, index.tree, expr.display(db))?
			},
			Expr::Application { func, sub } => {
				write!(f, "({} {})", func.display(db), sub.display(db))?
			},
		}
		Ok(())
	}
}


impl Expr {
	pub fn var(db: &mut Datastore) -> Link<Expr> {
		Link::new(Expr::Variable)
	}
	pub fn lambda(pointer: Link<PointerTree>, expr: &Link<Expr>, db: &mut Datastore) -> Link<Expr> {
		Link::new(Expr::Lambda { tree: pointer, expr: expr.clone() })
	}
	pub fn app(function: &Link<Expr>, substitution: &Link<Expr>, db: &mut Datastore) -> Link<Expr> {
		Link::new(Expr::Application { func: function.clone(), sub: substitution.clone() })
	}
}