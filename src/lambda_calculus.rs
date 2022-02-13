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

	#[error("Pointer Tree Mismatch, make sure lambda pointer trees match with expressions.")]
	PointerTreeMismatch,
}

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace(
	replace_in_expr: TypedHash<Expr>, // Expr that should be replaced in
	external_tree: TypedHash<PointerTree>, // PointerTree representing binded variables higher up
	lambda_tree: TypedHash<PointerTree>, // PointerTree representing variables to replace
	replace_index: u32, // Indicies in lambda_tree that should be replaced
	replacement: &TypedHash<Expr>,
	replacement_tree: &TypedHash<PointerTree>,
	db: &mut Datastore) -> Result<(TypedHash<Expr>, TypedHash<PointerTree>, TypedHash<PointerTree>), LambdaError>
{
	// println!("replace {} of [{}] in {} with {} : [{}]", replace_index, lambda_tree.display(db), replace_in_expr.display(db), replacement.display(db), replacement_tree.display(db));
	Ok(match replace_in_expr.fetch(db)?.clone() {
		ArchivedExpr::Variable => {
			// When encounter a variable and index is correct, replace with replacement
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in external_tree
			match lambda_tree.fetch(db)? {
				ArchivedPointerTree::Branch(_, _) => Err(LambdaError::PointerTreeMismatch)?,
				// Max variable index is replacement tree
				ArchivedPointerTree::End(val) if *val == replace_index => (replacement.clone(), replacement_tree.clone(), PT_NONE.clone()),
				// Don't replace if external_tree == (End(val) where val != replace_index or None)
				_ => (replace_in_expr, external_tree, lambda_tree.clone()),
			}
		},
		// When encounter a lambda, unwrap, recurse, re-wrap
		ArchivedExpr::Lambda { tree, index, expr } => {
			let replace_in_expr = expr.clone();
			let tree = tree.clone();
			let index = *index;
			let (replaced_expr, external_tree, lambda_tree) = recur_replace(replace_in_expr, external_tree, lambda_tree, replace_index, replacement, replacement_tree, db)?;
			
			(Expr::Lambda { index, tree, expr: replaced_expr }.store(db), external_tree, lambda_tree)
		},
		// When encounter an application in the replacement expression:
		ArchivedExpr::Application { func, sub } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (external_tree_func, external_tree_sub) = external_tree.fetch(db)?.split_none()?;
			let (func_tree, sub_tree) = lambda_tree.fetch(db)?.split_none()?;

			let (func, sub) = (func.clone(), sub.clone());
			// Only evaluate if 
			//let (func, sub, external_tree, lambda_tree) = if max_index >= replace_index {
				let (func, external_tree_func, func_tree) = recur_replace(func, external_tree_func.clone(), func_tree.clone(), replace_index, replacement, replacement_tree, db)?;
				let (sub, external_tree_sub, sub_tree) = recur_replace(sub, external_tree_sub.clone(), sub_tree.clone(), replace_index, replacement, replacement_tree, db)?;
				
				let external_tree = PointerTree::join(external_tree_func, external_tree_sub, db);
				let lambda_tree = PointerTree::join(func_tree, sub_tree, db);
				//(func, sub, replaced_external_tree, replaced_lambda_tree)
			//} else { (func, sub, external_tree, lambda_tree) };

			(Expr::Application { func, sub }.store(db), external_tree, lambda_tree)
		}
	})
}

/// Reduces reducing_expr and returns TypedHash<Expr>
fn partial_beta_reduce(reducing_expr: TypedHash<Expr>, reducing_tree: TypedHash<PointerTree>, depth: usize, db: &mut Datastore) -> Result<(TypedHash<Expr>, TypedHash<PointerTree>), LambdaError> {
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
		ArchivedExpr::Variable => {
			// println!("{}[{}] reducing var {} -------- [{}]", pad, depth, reducing_expr.display(db), reducing_tree.display(db));
			(reducing_expr, reducing_tree)
		},
		ArchivedExpr::Lambda { index, tree, expr } => {
			println!("{}[{}] reducing lam {} -------- [{}] ", pad, depth, reducing_expr.display(db), reducing_tree.display(db));

			let expr = expr.clone();
			let mut index = *index;
			let mut reducing_tree = PointerTree::merge(reducing_tree, tree.clone(), db)?;

			// When Lambda, pass lambda's tree as reducing_tree
			let (mut reduced_expr, mut reduced_tree) = partial_beta_reduce(expr.clone(), reducing_tree, depth, db)?;
			
			if let ArchivedExpr::Lambda { index: sub_index, tree, expr } = reduced_expr.fetch(db)? {
				let tree = tree.clone();
				let sub_index = *sub_index;
				let expr = expr.clone();
				print!("{}[{}] reducing adjacent lambdas: {} -> ", pad, depth, Expr::lambda(reduced_tree.clone(), index, &reduced_expr, db).display(db));
				index = index + sub_index + 1;
				reduced_expr = expr.clone();
				reduced_tree = PointerTree::inc_ends(reduced_tree, index, db)?;
				reduced_tree = PointerTree::merge(reduced_tree, tree.clone(), db)?;
				println!("{}", Expr::lambda(reduced_tree.clone(), index, &reduced_expr, db).display(db));
			}

			println!("{}					[{}] -> ", pad, reduced_tree.display(db));

			(Expr::Lambda {
				index,
				tree: reduced_tree,
				expr: reduced_expr
			}.store(db), reducing_tree) // Make sure to return original reducing_tree
		}
		ArchivedExpr::Application { func, sub } => {
			println!("{}[{}] reducing app {} -------- [{}]", pad, depth, reducing_expr.display(db), reducing_tree.display(db));
			
			// Descend to subtrees
			let (func_tree, sub_tree) = reducing_tree.fetch(db)?.split_none()?;

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
					}.store(db), PointerTree::join(func_tree, sub_tree, db))
				},
				ArchivedExpr::Lambda { index, tree, expr } => {
					print!("{}[{}] replace index {} in {}: [{}] with {}: [{}] → ", pad, depth, index, func.display(db), func_tree.display(db), sub.display(db), sub_tree.display(db));

					// If Lambda, replace in expr with tree at index with subs
					
					let tree = tree.clone();
					let index = *index;
					let (mut replaced_expr, replaced_tree, lambda_tree) = recur_replace(expr.clone(), func_tree.clone(), tree.clone(), index, &sub, &sub_tree, db)?;
					
					if index != 0 {
						replaced_expr = Expr::Lambda { index: index - 1, tree: lambda_tree.clone(), expr: replaced_expr  }.store(db);
					}
					
					println!("{}: [{}]", replaced_expr.display(db), replaced_tree.display(db));

					//println!("{}Replaced every {} with [{}] in [{}] -> [{}]", pad, index, sub_tree.display(db), func_tree.display(db), reducing_tree.display(db));

					// Reduce output expr & tree
					partial_beta_reduce(replaced_expr, replaced_tree, depth, db)?
				},
				ArchivedExpr::Application { .. } => {
					// Beta reduce sub, return application with joined trees
					let (sub, sub_tree) = partial_beta_reduce(sub.clone(), sub_tree, depth, db)?;
					(Expr::Application { func, sub }.store(db), PointerTree::join(func_tree, sub_tree, db))
				}
			}
		}
	};
	println!("{}[{}] returning reduction: {} -------- [{}]", pad, depth, ret.0.display(db), ret.1.display(db));
	Ok(ret)
}

pub fn beta_reduce(expr: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	println!("Reducing: {}", expr.display(db));
	Ok(partial_beta_reduce(expr.clone(), PointerTree::None.store(db), 0, db)?.0)
}