use std::fmt;

use thiserror::Error;
use rkyv::{Archive, Serialize};

use hashdb::{Datastore, DatastoreError, NativeHashtype, TypedHash};

mod expr;
mod display;
pub use expr::*;
pub use display::*;



#[derive(Error, Debug)]
pub enum LambdaError {
	#[error("Format Error")]
	FormatError(#[from] fmt::Error),

	// Datastore Error
	#[error("Datastore Error: {0}")]
	HashtypeResolveError(#[from] DatastoreError),

	// Beta Reduction Error
	#[error("Recursion Depth for beta reduction exceeded")]
	RecursionDepthExceeded,

	#[error("Pointer Tree Error")]
	PointerTreeMismatch,
}

/* #[derive(Clone)]
pub struct ReplaceTree {
	pub tree: TypedHash<PointerTree>,
	pub max_index: u32,
}
impl ReplaceTree {
	pub fn new(tree: TypedHash<PointerTree>, db: &Datastore) -> Result<Self, LambdaError> {
		Ok(Self { tree, max_index: tree.fetch(db)?.value() })
	}
	pub fn split(self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		Ok(match self.tree.fetch(db)? {
			ArchivedPointerTree::None => (self.clone(), self),
			ArchivedPointerTree::End(val) => Err(LambdaError::PointerTreeMismatch)?,
			ArchivedPointerTree::Branch(left, right, max_index) => (
				ReplaceTree { tree: left.clone(), max_index: *max_index },
				ReplaceTree { tree: right.clone(), max_index: *max_index },
			)
		})
	}
	pub fn join(self, other: Self, db: &mut Datastore) -> Self {
		let max_index = u32::max(self.max_index, other.max_index);
		ReplaceTree { tree: PointerTree::Branch(self.tree, other.tree, max_index).store(db), max_index  }
	}
} */

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace(
	replace_in_expr: TypedHash<Expr>,
	replace_in_tree: TypedHash<PointerTree>,
	replacement: &TypedHash<Expr>,
	replacement_tree: &TypedHash<PointerTree>,
	replace_index: u32, db: &mut Datastore) -> Result<(TypedHash<Expr>, TypedHash<PointerTree>), LambdaError>
{
	Ok(match replace_in_expr.fetch(db)?.clone() {
		ArchivedExpr::Variable => {
			// When encounter a variable and index is correct, replace with replacement
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in replace_in_tree
			match replace_in_tree.fetch(db)? {
				ArchivedPointerTree::Branch(_, _, _) => Err(LambdaError::PointerTreeMismatch)?,
				// Max variable index is replacement tree
				ArchivedPointerTree::End(val) if *val == replace_index => (replacement.clone(), replacement_tree.clone()),
				// Don't replace if replace_in_tree == (End(val) where val != replace_index or None)
				_ => (replace_in_expr, replace_in_tree),
			}
		},
		// When encounter a lambda, unwrap, recurse, re-wrap
		ArchivedExpr::Lambda { tree, index, expr } => {
			let replace_in_expr = expr.clone();
			let tree = tree.clone();
			let index = *index;
			let (replaced_expr, replaced_tree) = recur_replace(replace_in_expr, replace_in_tree, replacement, replacement_tree, replace_index, db)?;
			(Expr::Lambda { tree, index, expr: replaced_expr }.store(db), replaced_tree)
		},
		// When encounter an application in the replacement expression:
		ArchivedExpr::Application { func, sub } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (func_tree, subs_tree, max_index) = replace_in_tree.fetch(db)?.split_none()?;

			let (func, sub) = (func.clone(), sub.clone());
			// Only evaluate if 
			let (func, sub, replace_tree) = if max_index >= replace_index {
				let (func, func_tree) = recur_replace(func, func_tree.clone(), replacement, replacement_tree, replace_index, db)?;
				let (sub, sub_tree) = recur_replace(sub, subs_tree.clone(), replacement, replacement_tree, replace_index, db)?;
				
				let replace_tree = PointerTree::join(func_tree, sub_tree, max_index, db);
				(func, sub, replace_tree)
			} else { (func, sub, replace_in_tree) };

			(Expr::Application { func, sub }.store(db), replace_tree)
		}
	})
}


impl PointerTree {
	/* fn push_lambda_pointer(&self, pointer: &TypedHash<LambdaPointer>, count: usize, db: &mut Datastore) -> Result<TypedHash<Self>, LambdaError> {
		use PointerTree as PT;
		Ok(match (self, pointer) {
			// If PointerTree is None, fill in pointer
			(PointerTree::None, pointer) => match pointer.as_ref() {
				LambdaPointer::Left(l) => {
					PointerTree::Branch(PointerTree::None.push_lambda_pointer(l, count, db)?, PointerTree::None.store(db)).store(db)
				},
				LambdaPointer::Right(r) => {
					PointerTree::Branch(PointerTree::None.store(db), PointerTree::None.push_lambda_pointer(&r, count, db)?).store(db)
				},
				LambdaPointer::Both(l, r) => {
					PointerTree::Branch(
						PointerTree::None.push_lambda_pointer(l, count, db)?,
						PointerTree::None.push_lambda_pointer(r, count, db)?,
					).store(db)
				}
				LambdaPointer::End => PointerTree::End(count).store(db),
			},
			(PointerTree::Branch(left, right), pointer) => match pointer.as_ref() {
				LambdaPointer::Left(l) => PointerTree::Branch(left.push_lambda_pointer(l, count, db)?, right.clone()).store(db),
				LambdaPointer::Right(r) => PointerTree::Branch(left.clone(), right.push_lambda_pointer(r, count, db)?).store(db),
				LambdaPointer::Both(l, r) => PointerTree::Branch(
					left.push_lambda_pointer(l, count, db)?,
					right.push_lambda_pointer(r, count, db)?
				).store(db),
				LambdaPointer::End => Err(LambdaError::PointerTreeMismatch)?
			}
			(PointerTree::End(_), _) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn pop_lambda_pointer(&self, index: usize, db: &mut Datastore) -> Result<(TypedHash<Self>, Option<TypedHash<LambdaPointer>>), LambdaError> {
		Ok(match self {
			PointerTree::Branch(l, r) => {
				let (l, lambda_left) = l.pop_lambda_pointer(index, db)?;
				let (r, lambda_right) = r.pop_lambda_pointer(index, db)?;
				
				(
					if l.is_none() && r.is_none() { PointerTree::None } else { PointerTree::Branch(l, r) }.store(db),
					match (lambda_left, lambda_right) {
						(Some(l), Some(r)) => Some(LambdaPointer::Both(l, r).store(db)),
						(Some(l), None) => Some(LambdaPointer::Left(l).store(db)),
						(None, Some(r)) => Some(LambdaPointer::Right(r).store(db)),
						(None, None) => None,
					}
				)
			},
			PointerTree::End(count) => if index == *count {
				(PointerTree::None.store(db), Some(LambdaPointer::End.store(db)))
			} else { (self.clone().store(db), None) },
			PointerTree::None => (PointerTree::None.store(db), None),
		})
	} */
}

/* #[derive(Clone)]
struct ReduceTree(PointerTree, usize);
impl ReduceTree {
	fn new() -> Self { Self(PointerTree::None, 0) }
	fn split(&mut self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		let (l, r) = self.0.split(db)?;
		Ok((ReduceTree(l, self.1), ReduceTree(r, self.1)))
	}
	fn join_into(&mut self, left: Self, right: Self, db: &mut Datastore) {
		self.0 = left.0.join(&right.0, db);
	}

	/* // Add LambdaPointer to PointerTree, keeping track of how many have been added
	fn push_lambda_pointer(&mut self, pointer: &Option<TypedHash<LambdaPointer>>, db: &mut Datastore) -> Result<usize, LambdaError> {
		self.1 += 1;
		if let Some(pointer) = pointer {
			self.0 = self.0.push_lambda_pointer(pointer, self.1, db)?
		};
		Ok(self.1)	
	}
	fn pop_lambda_pointer(&mut self, index: usize, db: &mut Datastore) -> Result<Option<TypedHash<LambdaPointer>>, LambdaError> {
		if self.1 != index || self.1 == 0 { return Err(LambdaError::PointerTreeMismatch) }
		let (out, pointer) = self.0.pop_lambda_pointer(index, db)?;
		self.0 = out;
		self.1 -= 1; Ok(pointer)
	} */
}
impl DisplayWithDatastore for ReduceTree {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		write!(f, "{}/{}", self.0.display(db), self.1)
	}
} */

/* #[test]
fn test_pointer_trees() {
	let db = &mut Datastore::new();
	PointerTree::None.store(db);
	let r = &mut ReduceTree::new();
	println!("start: [{}]", r.display(db));

	let expr = crate::parse_to_expr("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", db).unwrap();
	let (pointer_1, expr) = if let Expr::Lambda { tree, index, expr } = &*expr { (tree, expr.clone()) } else { unreachable!() };
	let (pointer_2, expr) = if let Expr::Lambda { tree, index, expr } = &*expr { (tree, expr.clone()) } else { unreachable!() };
	let (pointer_3, expr) = if let Expr::Lambda { tree, index, expr } = &*expr { (tree, expr.clone()) } else { unreachable!() };
	let (pointer_4, _expr) = if let Expr::Lambda { tree, index, expr }  = &*expr { (tree, expr.clone()) } else { unreachable!() };

	let index_1 = r.push_lambda_pointer(pointer_1, db).unwrap();
	println!("add [{}] = [{}]", pointer_1.display(db), r.display(db));

	let index_2 = r.push_lambda_pointer(pointer_2, db).unwrap();
	println!("add [{}] = [{}]", pointer_2.display(db), r.display(db));

	let index_3 = r.push_lambda_pointer(pointer_3, db).unwrap();
	println!("add [{}] = [{}]", pointer_3.display(db), r.display(db));

	let index_4 = r.push_lambda_pointer(pointer_4, db).unwrap();
	println!("add [{}] = [{}]", pointer_4.display(db), r.display(db));

	let r_before = r.clone();
	let (left, right) = r.split(db).unwrap();
	r.join_into(left, right, db);
	assert_eq!(r_before.0, r.0);
	assert_eq!(r_before.1, r.1);

	let pointer_4_pop = r.pop_lambda_pointer(index_4, db).unwrap();
	println!("pop {} [{}] = [{}]", index_4, r.display(db), pointer_4_pop.display(db));
	assert_eq!(pointer_4, &pointer_4_pop);

	let pointer_3_pop = r.pop_lambda_pointer(index_3, db).unwrap();
	println!("pop {} [{}] = [{}]", index_3, r.display(db), pointer_3_pop.display(db));
	assert_eq!(pointer_3, &pointer_3_pop);

	let pointer_2_pop = r.pop_lambda_pointer(index_2, db).unwrap();
	println!("pop {} [{}] = [{}]", index_2, r.display(db), pointer_2_pop.display(db));
	assert_eq!(pointer_2, &pointer_2_pop);

	let pointer_1_pop = r.pop_lambda_pointer(index_1, db).unwrap();
	println!("pop {} [{}] = [{}]", index_1, r.display(db), pointer_1_pop.display(db));
	assert_eq!(pointer_1, &pointer_1_pop);

	// Test Split & Join
	let r = &mut ReduceTree(PointerTree::None.store(db), 0);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::End(0).store(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::End(1).store(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::Branch(PointerTree::None.store(db), PointerTree::None.store(db)).store(db), 0);
	// test_split(r, db).unwrap(); // This will error

	let r = &mut ReduceTree(PointerTree::Branch(PointerTree::End(1).store(db), PointerTree::None.store(db)).store(db), 1);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::Branch(PointerTree::Branch(PointerTree::None.store(db), PointerTree::End(2).store(db)).store(db), PointerTree::End(2).store(db)).store(db), 2);
	test_split(r, db).unwrap();
}
 */
/* fn test_split(r: &mut ReduceTree, db: &mut Datastore) -> Result<(), LambdaError> {
	let r_before = r.clone();
	print!("split [{}] ", r.display(db));
	let (left, right) = r.split(db).map_err(|e|{println!("err"); e})?;
	print!("([{}] [{}])", left.display(db), right.display(db));
	r.join_into(left, right, db);
	println!(" = [{}]", r.display(db));
	assert_eq!(r_before.0, r.0);
	assert_eq!(r_before.1, r.1);
	Ok(())
} */


/// Reduces reducing_expr and returns TypedHash<Expr>
fn partial_beta_reduce(reducing_expr: TypedHash<Expr>, reducing_tree: TypedHash<PointerTree>, depth: usize, db: &mut Datastore) -> Result<(TypedHash<Expr>, TypedHash<PointerTree>), LambdaError> {
	if depth > 200 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;

	struct DisplayIter<T: fmt::Display, I: Iterator<Item=T> + Clone>(I);
	impl<T: fmt::Display, I: Iterator<Item=T> + Clone> fmt::Display for DisplayIter<T, I> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			for string in self.0.clone() { write!(f, "{}", string)?; }; Ok(())
		}
	}
	let pad = DisplayIter(std::iter::repeat("    ").take(depth));

	let ret = match reducing_expr.fetch(db)? {
		ArchivedExpr::Variable => {
			//println!("{}[{}] reducing var {} -------- [{}]", pad, depth, reducing_expr.display(db), reducing_tree.display(db));
			(reducing_expr, reducing_tree)
		},
		ArchivedExpr::Lambda { index, tree, expr } => {
			//print!("{}[{}] reducing lam {} -------- [{}] -> ", pad, depth, Expr::Lambda { tree: tree.clone(), index: *index, expr: expr.clone() }.display(db), reducing_tree.display(db));

			// When Lambda, pass lambda's tree as reducing_tree 
			let index = *index;
			let (reduced_expr, reduced_tree) = partial_beta_reduce(expr.clone(), tree.clone(), depth, db)?;

			(Expr::Lambda {
				index,
				tree: reduced_tree,
				expr: reduced_expr
			}.store(db), reducing_tree) // Make sure to return original reducing_tree
		}
		ArchivedExpr::Application { func, sub } => {
			//println!("{}[{}] reducing app {} -------- [{}]", pad, depth, Expr::Application { func: func.clone(), sub: sub.clone() }.display(db), reducing_tree.display(db));
			
			// Descend to subtrees
			let (func_tree, sub_tree, app_index) = reducing_tree.fetch(db)?.split_none()?;

			let (func, sub) = (func.clone(), sub.clone());
			// Reduce function & function tree, if func_tree is None, this should return None for func_tree
			let (func, func_tree) = partial_beta_reduce(func, func_tree, depth, db)?;

			// Match on beta_reduced func
			match func.fetch(db)? {
				ArchivedExpr::Variable => {
					// If Variable, beta reduce subs & return Application with joined trees
					let (sub, sub_tree) = partial_beta_reduce(sub.clone(), sub_tree, depth, db)?;
					
					(Expr::Application {
						func: Expr::Variable.store(db),
						sub,
					}.store(db), PointerTree::join(func_tree, sub_tree, app_index, db))
				},
				ArchivedExpr::Lambda { index, tree, expr } => {
					// If Lambda, replace in expr with tree at index with subs
					
					let (replaced_expr, replaced_tree) = recur_replace(expr.clone(), tree.clone(), &sub, &sub_tree, *index, db)?;

					// Reduce output expr & tree
					partial_beta_reduce(replaced_expr, replaced_tree, depth, db)?
				},
				ArchivedExpr::Application { .. } => {
					// Beta reduce sub, return application with joined trees
					let (sub, sub_tree) = partial_beta_reduce(sub.clone(), sub_tree, depth, db)?;
					(Expr::Application { func, sub }.store(db), PointerTree::join(func_tree, sub_tree, app_index, db))
				}
			}
		}
	};
	//println!("{}[{}] returning reduction: {} -------- [{}]", pad, depth, ret.display(db), reducing_tree.display(db));
	Ok(ret)
}

pub fn beta_reduce(expr: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	//println!("Reducing: {}", expr.display(db));
	Ok(partial_beta_reduce(expr.clone(), PT_NONE.clone(), 0, db)?.0)
}