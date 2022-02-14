use std::fmt;

use thiserror::Error;

use hashdb::{Datastore, DatastoreError, NativeHashtype, TypedHash};

mod expr;
mod display;
mod reduce_tree;
pub use expr::*;
pub use display::*;
pub use reduce_tree::*;



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

	#[error("Pointer Tree Mismatch, make sure lambda pointer trees match with expressions.")]
	PointerTreeMismatch,

	#[error("Reduce Tree Mismatch, something seems to be wrong with the reducing algorithm")]
	ReduceArenaMismatch,
}



/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace<'a>(
	replace_in_expr: TypedHash<Expr>, // Expr that should be replaced in
	arena: &'a ReduceArena<'a>,
	replace_index: &mut ReplaceIndex<'a>,
	replacement: &TypedHash<Expr>,
	replacement_tree: &'a ReplaceTree<'a>,
	db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError>
{
	// println!("replace {} of [{}] in {} with {} : [{}]", replace_index, lambda_tree.display(db), replace_in_expr.display(db), replacement.display(db), replacement_tree.display(db));
	Ok(match replace_in_expr.fetch(db)?.clone() {
		ArchivedExpr::Variable => {
			// When encounter a variable and index is correct, replace with replacement
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in external_tree
			match replace_index.tree {
				ReplaceTree::Branch(_, _) => Err(LambdaError::PointerTreeMismatch)?,
				ReplaceTree::End(val) if *val == replace_index.index => {
					replace_index.tree = replacement_tree;
					replacement.clone()
				},
				_ => replace_in_expr,
			}
		},
		// When encounter a lambda, unwrap, recurse, re-wrap
		ArchivedExpr::Lambda { tree, expr } => {
			let tree = tree.clone();
			let replaced_expr = recur_replace(expr.clone(), arena, replace_index, replacement, replacement_tree, db)?;
			
			Expr::Lambda { tree, expr: replaced_expr }.store(db)
		},
		// When encounter an application in the replacement expression:
		ArchivedExpr::Application { func, sub } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (mut func_index, mut sub_index) = replace_index.split()?;

			let (func, sub) = (func.clone(), sub.clone());
			let func = recur_replace(func, arena, &mut func_index, replacement, replacement_tree, db)?;
			let sub = recur_replace(sub, arena, &mut sub_index, replacement, replacement_tree, db)?;

			*replace_index = arena.join_index(func_index, sub_index);
			Expr::Application { func, sub }.store(db)
		}
	})
}

/// Reduces reducing_expr and returns TypedHash<Expr>
fn partial_beta_reduce<'a>(reducing_expr: TypedHash<Expr>, arena: &'a ReduceArena<'a>, replace_index: &mut ReplaceIndex<'a>, depth: usize, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	struct DisplayIter<T: fmt::Display, I: Iterator<Item=T> + Clone>(I);
	impl<T: fmt::Display, I: Iterator<Item=T> + Clone> fmt::Display for DisplayIter<T, I> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			for string in self.0.clone() { write!(f, "{}", string)?; }; Ok(())
		}
	}
	let pad = DisplayIter(std::iter::repeat("    ").take(depth));

	if depth > 200 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;

	let ret = match reducing_expr.fetch(db)? {
		ArchivedExpr::Variable => reducing_expr,
		ArchivedExpr::Lambda { tree, expr } => {
			println!("{}[{}] reducing lam {} : {}", pad, depth, reducing_expr.display(db), replace_index);
			arena.push_pointer_tree(replace_index, tree, db)?;

			let reduced_expr = partial_beta_reduce(expr.clone(), arena, replace_index, depth, db)?;

			let ret = Expr::Lambda {
				tree: arena.pop_pointer_tree(replace_index, db)?,
				expr: reduced_expr
			}.store(db);
			println!("{}[{}] returning lam: {} : {}", pad, depth, ret.display(db), replace_index);
			ret
		}
		ArchivedExpr::Application { func, sub } => {
			println!("{}[{}] reducing app {} : {}", pad, depth, reducing_expr.display(db), replace_index);
			
			// Descend to subtrees
			let (mut func_tree, mut sub_tree) = replace_index.split()?;

			let (func, sub) = (func.clone(), sub.clone());
			// Reduce function & function tree, if func_tree is None, this should return None for func_tree
			let func = partial_beta_reduce(func, arena, &mut func_tree, depth, db)?;

			// Match on beta_reduced func
			let ret = match func.fetch(db)? {
				ArchivedExpr::Variable => {
					// If Variable, beta reduce subs & return Application with joined trees
					let sub = partial_beta_reduce(sub.clone(), arena, &mut sub_tree, depth, db)?;
					*replace_index = arena.join_index(func_tree, sub_tree);

					Expr::Application { func, sub, }.store(db)
				},
				ArchivedExpr::Lambda { tree, expr } => {
					*replace_index = func_tree;
					//println!("{}[{}] debug: adding {} to {:?}", pad, depth, tree.display(db), replace_index);
					arena.push_pointer_tree(replace_index, tree, db)?;
					//println!("{}[{}] debug: {} : {}", pad, depth, func.display(db), replace_index);

					let index = replace_index.index;
					
					print!("{}[{}] replace index {} in {}: [{}] with {}: [{}] → ", pad, depth, index, func.display(db), replace_index, sub.display(db), sub_tree);

					let replaced_expr = recur_replace(expr.clone(), arena, replace_index, &sub, &sub_tree.tree, db)?;
					println!("{}: [{}]", replaced_expr.display(db), replace_index);
					
					replace_index.index -= 1; // Needed because all PointerTree::End(index) were replaced in recur_replace, thus the top index must be decremented

					// Reduce output expr & tree
					partial_beta_reduce(replaced_expr, arena, replace_index, depth, db)?
				},
				ArchivedExpr::Application { .. } => {
					// Beta reduce sub, return application with joined trees
					let sub = partial_beta_reduce(sub.clone(), arena, &mut sub_tree, depth, db)?;
					*replace_index = arena.join_index(func_tree, sub_tree);
					Expr::Application { func, sub }.store(db)
				}
			};
			println!("{}[{}] returning app: {} : {}", pad, depth, ret.display(db), replace_index);
			ret
		}
	};
	Ok(ret)
}

pub fn beta_reduce(expr: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	println!("Reducing: {}", expr.display(db));
	let arena = ReduceArena::new();
	Ok(partial_beta_reduce(expr.clone(), &arena, &mut arena.index(), 0, db)?)
}