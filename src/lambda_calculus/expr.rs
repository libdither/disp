use std::fmt;

use lazy_static::lazy_static;
use rkyv::{Archive, Serialize};
use bytecheck::CheckBytes;
//use serde::{Serialize, Deserialize};

use hashdb::{Datastore, Hash, NativeHashtype, TypedHash};

use crate::symbol::Symbol;

use super::{DisplayWithDatastore, DisplayWithDatastoreError, LambdaError};

/* 
pub const VARIABLE:    Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
pub const LAMBDA:      Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
pub const APPLICATION: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);
 */

pub mod pointer_helpers {
    use hashdb::{Datastore, TypedHash, NativeHashtype};
    use super::{PointerTree, PT_NONE};

	pub fn none(db: &mut Datastore) -> TypedHash<PointerTree> { PT_NONE.clone() }
	pub fn end(val: u32, db: &mut Datastore) -> TypedHash<PointerTree> { PointerTree::End(val).store(db) }
	pub fn left(p: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		//if let Some(val) = p.fetch(db).unwrap().value() {
			PointerTree::Branch(p, PT_NONE.clone()).store(db)
		//} else { PointerTree::None }.store(db)
	}
	pub fn right(p: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		//if let Some(val) = p.fetch(db).unwrap().value() {
			PointerTree::Branch(PT_NONE.clone(), p).store(db)
		//} else { PointerTree::None }.store(db)
	}
	pub fn both(l: TypedHash<PointerTree>, r: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		//if let Some(val) = l.fetch(db).unwrap().value().map(|v|u32::max(v, r.fetch(db).unwrap().value().unwrap_or(0))) {
			PointerTree::Branch(l, r).store(db)
		//} else { PointerTree::None }.store(db)
	}
}


/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize)]
#[archive(compare(PartialEq))]
#[archive_attr(derive(CheckBytes, Debug))]
pub enum PointerTree {
	None,
	End(u32),
	Branch(TypedHash<PointerTree>, TypedHash<PointerTree>), // u32 represents highest variable abstraction level in this expression
}
lazy_static! {
	pub static ref PT_NONE: TypedHash<PointerTree> = PointerTree::None.hash();
	pub static ref PT_BOTH_NONE: TypedHash<PointerTree> = PointerTree::Branch(PT_NONE.clone(), PT_NONE.clone()).hash();
}
impl PointerTree {
	pub fn join(left: TypedHash<PointerTree>, right: TypedHash<PointerTree>, db: &mut Datastore) -> TypedHash<PointerTree> {
		if left == *PT_NONE && right == *PT_NONE {
			PT_NONE.clone()
			
		} else { PointerTree::Branch(left, right).store(db) }
	}
	pub fn inc_ends(tree: TypedHash<PointerTree>, index: u32, db: &mut Datastore) -> Result<TypedHash<PointerTree>, LambdaError> {
		Ok(match tree.fetch(db)? {
			ArchivedPointerTree::None => tree,
			ArchivedPointerTree::End(val) => PointerTree::End(val + index).store(db),
			ArchivedPointerTree::Branch(left, right) => {
				let left = left.clone(); let right = right.clone();
				PointerTree::Branch(
					PointerTree::inc_ends(left.clone(), index, db)?,
					PointerTree::inc_ends(right.clone(), index, db)?
				).store(db)
			},
		})
	}
	pub fn merge(left: TypedHash<PointerTree>, right: TypedHash<PointerTree>, db: &mut Datastore) -> Result<TypedHash<PointerTree>, LambdaError> {
		Ok(match left.fetch(db)? {
			ArchivedPointerTree::None => right,
			ArchivedPointerTree::End(_) => if right == *PT_NONE { left } else { Err(LambdaError::PointerTreeMismatch)? },
			ArchivedPointerTree::Branch(l1, r1) => {
				if right == *PT_NONE { left } else {
					if let ArchivedPointerTree::Branch(l2, r2) = right.fetch(db)? {
						// let v1 = *v1; let v2 = *v2;
						let l1 = l1.clone(); let l2 = l2.clone();
						let r1 = r1.clone(); let r2 = r2.clone();
						PointerTree::Branch(PointerTree::merge(l1, l2, db)?, PointerTree::merge(r1, r2, db)?).store(db)
					} else { Err(LambdaError::PointerTreeMismatch)? }
				}
			}
		})
	}
}
impl ArchivedPointerTree {
	pub fn split(&self) -> Result<Option<(&TypedHash<PointerTree>, &TypedHash<PointerTree>)>, LambdaError> {
		Ok(match self {
			ArchivedPointerTree::None => None,
			ArchivedPointerTree::Branch(left, right) => Some((left, right)),
			_ => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	pub fn split_none(&self) -> Result<(TypedHash<PointerTree>, TypedHash<PointerTree>), LambdaError> {
		Ok(if let Some((left, right)) = self.split()?{
			(left.clone(), right.clone())
		} else { (PT_NONE.clone(), PT_NONE.clone()) })
	}
	/* pub fn value(&self) -> Option<u32> {
		match self {
			ArchivedPointerTree::None => None,
			ArchivedPointerTree::Branch(_, _, val) => Some(*val),
			ArchivedPointerTree::End(val) => Some(*val),
		}
	} */
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
			ArchivedPointerTree::End(v) => write!(f, "{}", v)?,
			ArchivedPointerTree::None => write!(f, "N")?,
		}
		Ok(())
	}
}

#[derive(Clone, PartialEq, Eq, Debug, Archive, Serialize)]
#[archive_attr(derive(CheckBytes, Debug))]
pub enum Expr {
	Variable,
	Lambda { index: u32, tree: TypedHash<PointerTree>, expr: TypedHash<Expr> },
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
			ArchivedExpr::Lambda { index, tree, expr } => {
				write!(f, "(λ{}[{}] {})", index, tree.display(db), expr.display(db))?
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
	pub fn lambda(tree: TypedHash<PointerTree>, index: u32, expr: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		Expr::Lambda { tree, index, expr: expr.clone() }.store(db)
	}
	pub fn app(function: &TypedHash<Expr>, substitution: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		Expr::Application { func: function.clone(), sub: substitution.clone() }.store(db)
	}
	pub fn store(&self, db: &mut Datastore) -> TypedHash<Expr> { self.store(db) }
}