use std::fmt;

use lazy_static::lazy_static;
use rkyv::{Archive, Serialize, Deserialize};
use bytecheck::CheckBytes;
//use serde::{Serialize, Deserialize};

use hashdb::{Datastore, DatastoreDeserializer, DatastoreSerializer, Link, LinkArena, NativeHashtype};

use crate::{symbol::Symbol};

use super::{DisplayWithDatastore, DisplayWithDatastoreError};


/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub enum PointerTree<'a> {
	None,
	End,
	Branch(#[omit_bounds] Link<'a, PointerTree<'a>>, #[omit_bounds] Link<'a, PointerTree<'a>>), // u32 represents highest variable abstraction level in this expression
}
impl<'a> PointerTree<'a> {
	pub const NONE: Link<'static, PointerTree<'static>> = Link::new(&PointerTree::None);
	pub const END: Link<'static, PointerTree<'static>> = Link::new(&PointerTree::End);
	pub fn left(p: Link<'a, PointerTree<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, PointerTree<'a>> {
		arena.new(PointerTree::Branch(p, PointerTree::none(arena)))
	}
	pub fn right(p: Link<'a, PointerTree<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, PointerTree<'a>> {
		arena.new(PointerTree::Branch(PointerTree::none(arena), p))
	}
	pub fn branch(l: Link<'a, PointerTree<'a>>, r: Link<'a, PointerTree<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, PointerTree<'a>> {
		arena.new(PointerTree::Branch(l, r))
	}
	pub fn branch_reduce(l: Link<'a, PointerTree<'a>>, r: Link<'a, PointerTree<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, PointerTree<'a>> {
		if l == Self::NONE && r == Self::NONE { Self::NONE }
		else { arena.new(PointerTree::Branch(l, r)) }
	}
}

impl<'a> NativeHashtype for PointerTree<'a> {}
impl<'a> DisplayWithDatastore for Link<'a, PointerTree<'a>> {
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

#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub enum Expr<'a> {
	/// By itself, an unbound term, a unit of undefined meaning, ready for construction
	Variable,
	/// Create a function
	Lambda {
		tree: Link<'a, PointerTree<'a>>,
		#[omit_bounds] expr: Link<'a, Expr<'a>>
	},
	/// Apply functions to expressions
	Application {
		#[omit_bounds] func: Link<'a, Expr<'a>>,
		#[omit_bounds] args: Link<'a, Expr<'a>>
	},
	// Type of all types
	// Type,
	// Create dependent types
	// Dependent { tree: Link<'a, PointerTree<'a>>, expr: Link<Expr> },
}
impl<'a> NativeHashtype for Expr<'a> {}
impl<'a> DisplayWithDatastore for Link<'a, Expr<'a>> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> Result<(), DisplayWithDatastoreError> {
		/* match db.lookup_typed::<Expr, Symbol>(&self.store()) {
			Ok(symbol) => write!(f, "{}", symbol.fetch().name(db)?)?,
			Err(_) => {},
		} */
		match self.as_ref() {
			Expr::Variable => write!(f, "x")?,
			Expr::Lambda { .. } => {
				let arena = &LinkArena::new();
				let mut index = arena.index();
				let expr = arena.push_lambda(&mut index, &self, db).unwrap();
		
				write!(f, "(λ{}[{}] {})", index.index, index.tree, expr.display(db))?
			},
			Expr::Application { func, args: sub } => {
				write!(f, "({} {})", func.display(db), sub.display(db))?
			},
		}
		Ok(())
	}
}


impl<'a> Expr<'a> {
	pub const VAR: Link<'a, Expr<'a>> = Link::new(&Expr::Variable);
	pub fn lambda(tree: Link<'a, PointerTree>, expr: Link<'a, Expr<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, Expr<'a>> {
		arena.add(Expr::Lambda { tree, expr })
	}
	pub fn app(func: Link<'a, Expr<'a>>, args: Link<'a, Expr<'a>>, arena: &'a LinkArena<'a>) -> Link<'a, Expr<'a>> {
		arena.add(Expr::Application { func, args })
	}
}