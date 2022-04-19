use std::fmt;

use rkyv::{Archive, Serialize, Deserialize};
use bytecheck::CheckBytes;

use hashdb::{DatastoreDeserializer, DatastoreSerializer, LinkArena, NativeHashtype, HashType};

use super::ReplaceIndex;




/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub enum PointerTree<'a> {
	None,
	End,
	Branch(#[with(HashType)] #[omit_bounds] &'a PointerTree<'a>, #[with(HashType)] #[omit_bounds] &'a PointerTree<'a>), // u32 represents highest variable abstraction level in this expression
}
impl<'a> PointerTree<'a> {
	pub const NONE: &'static PointerTree<'static> = &PointerTree::None;
	pub const END: &'static PointerTree<'static> = &PointerTree::End;
	pub fn left(p: &'a PointerTree<'a>, arena: &'a LinkArena<'a>) -> &'a PointerTree<'a> {
		arena.add(PointerTree::Branch(p, Self::NONE))
	}
	pub fn right(p: &'a PointerTree<'a>, arena: &'a LinkArena<'a>) -> &'a PointerTree<'a> {
		arena.add(PointerTree::Branch(Self::NONE, p))
	}
	pub fn branch(l: &'a PointerTree<'a>, r: &'a PointerTree<'a>, arena: &'a LinkArena<'a>) -> &'a PointerTree<'a> {
		arena.add(PointerTree::Branch(l, r))
	}
	pub fn branch_reduce(l: &'a PointerTree<'a>, r: &'a PointerTree<'a>, arena: &'a LinkArena<'a>) -> &'a PointerTree<'a> {
		if l == Self::NONE && r == Self::NONE { Self::NONE }
		else { arena.add(PointerTree::Branch(l, r)) }
	}
}

impl<'a> NativeHashtype for PointerTree<'a> {}
impl<'a> fmt::Display for PointerTree<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match *self { 
			PointerTree::Branch(left, right) => {
				match (*right == PointerTree::None, *left == PointerTree::None) {
					(true, true) => write!(f, "BOTH(NONE, NONE)")?,
					(true, false) => write!(f, "<{}", left)?,
					(false, true) => write!(f, ">{}", right)?,
					(false, false) => write!(f, "({},{})", left, right)?,
				}
			},
			PointerTree::End => write!(f, ".")?,
			PointerTree::None => {},
		}
		Ok(())
	}
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub enum Expr<'a> {
	/// By itself, an unbound term, a unit of undefined meaning, ready for construction
	Variable,
	/// Create a function
	Lambda {
		#[with(HashType)] #[omit_bounds] tree: &'a PointerTree<'a>,
		#[with(HashType)] #[omit_bounds] expr: &'a Expr<'a>
	},
	/// Apply functions to expressions
	Application {
		#[with(HashType)] #[omit_bounds] func: &'a Expr<'a>,
		#[with(HashType)] #[omit_bounds] args: &'a Expr<'a>
	},
	// Type of all types
	// Type,
	// Create dependent types
	// Dependent { tree: &'a PointerTree<'a>, expr: Link<Expr> },
}
impl<'a> NativeHashtype for Expr<'a> {}
impl<'a> fmt::Display for &'a Expr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		/* match db.lookup_typed::<Expr, Symbol>(&self.store()) {
			Ok(symbol) => write!(f, "{}", symbol.fetch().name(db)?)?,
			Err(_) => {},
		} */
		match self {
			Expr::Variable => write!(f, "x")?,
			Expr::Lambda { .. } => {
				thread_local! {
					static REPS: LinkArena<'static> = LinkArena::new();
				}
				REPS.with(|reps| {
					let mut index = ReplaceIndex::DEFAULT;
					let expr = index.push_lambda(&self, reps).unwrap();
					write!(f, "(λ{}[{}] {})", index.index, index.tree, expr)
				})?;
				
			},
			Expr::Application { func, args: sub } => {
				write!(f, "({} {})", func, sub)?
			},
		}
		Ok(())
	}
}


impl<'a> Expr<'a> {
	pub const VAR: &'static Expr<'static> = &Expr::Variable;
	pub fn lambda(tree: &'a PointerTree, expr: &'a Expr<'a>, arena: &'a LinkArena<'a>) -> &'a Expr<'a> {
		arena.add(Expr::Lambda { tree, expr })
	}
	pub fn app(func: &'a Expr<'a>, args: &'a Expr<'a>, arena: &'a LinkArena<'a>) -> &'a Expr<'a> {
		arena.add(Expr::Application { func, args })
	}
}