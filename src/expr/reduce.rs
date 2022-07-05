//! This file contains functions to evaluate or beta reduce expressions

use hashdb::LinkArena;

use super::{BindIndex, BindSubTree, Expr, LambdaError};

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace<'a, 'r>(
	working_expr: &'a Expr<'a>,         // Working Expression
	working_bind: &mut BindIndex<'r>,  // Replace index in ReplaceTree to replace
	replacement: &'a Expr<'a>,         // Replacement Expr
	replacement_bind: &'r BindSubTree<'r>, // Bind Tree to replace with
	reps: &'r LinkArena<'r>,            // BindSubTree Arena
	exprs: &'a LinkArena<'a>,           // Expr Arena
) -> Result<&'a Expr<'a>, LambdaError> {
	Ok(match working_expr {
		Expr::Variable => {
			// When encounter a variable and index is correct, replace with replacement
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in external_tree
			match working_bind.tree {
				BindSubTree::Branch(_, _) => Err(LambdaError::UnexpectedBranchInSubstitution)?,
				BindSubTree::End(val) if *val == working_bind.index => {
					working_bind.tree = replacement_bind;
					replacement
				}
				_ => &working_expr,
			}
		}
		// When encounter a lambda, unwrap, recurse, re-wrap
		Expr::Lambda { bind, expr } => {
			let replaced_expr = recur_replace(expr, working_bind, replacement, replacement_bind, reps, exprs)?;

			Expr::lambda(bind, &replaced_expr, exprs)
		}
		// When encounter an application in the replacement expression:
		Expr::Application { func, args } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (mut func_index, mut args_index) = working_bind.split()?;

			let func = recur_replace(func, &mut func_index, replacement, replacement_bind, reps, exprs)?;
			let args = recur_replace(args, &mut args_index, replacement, replacement_bind, reps, exprs)?;

			*working_bind = BindIndex::join(func_index, args_index, reps);
			Expr::app(&func, &args, exprs)
		}
	})
}

/// Reduces reducing_expr and returns &'a Expr<'a>
fn partial_beta_reduce<'a, 'r>(reducing_expr: &'a Expr<'a>, reps: &'r LinkArena<'r>, replace_index: &mut BindIndex<'r>, depth: usize, exprs: &'a LinkArena<'a>) -> Result<&'a Expr<'a>, LambdaError> {
	if depth > 200 {
		return Err(LambdaError::RecursionDepthExceeded);
	}

	Ok(match reducing_expr {
		Expr::Variable => reducing_expr,
		Expr::Lambda { bind, expr } => {
			replace_index.push_binding(bind, reps)?;

			let reduced_expr = partial_beta_reduce(expr, reps, replace_index, depth, exprs)?;

			Expr::lambda(replace_index.pop_binding(reps, exprs)?, reduced_expr, exprs)
		}
		Expr::Application { func, args } => {
			// Split subtrees
			let (mut func_bind, mut args_bind) = replace_index.split()?;
			let func = partial_beta_reduce(func, reps, &mut func_bind, depth, exprs)?;

			match func {
				Expr::Lambda { bind, expr } => {
					// Replace all tree in expr & reduce the output
					*replace_index = func_bind;
					replace_index.push_binding(bind, reps)?;

					let replaced_expr = recur_replace(expr, replace_index, &args, &args_bind.tree, reps, exprs)?;

					replace_index.index -= 1; // All of current index will be replaced in recur_replace, thus this is needed

					let depth = depth + 1;
					partial_beta_reduce(replaced_expr, reps, replace_index, depth, exprs)?
				}
				_ => {
					// If Variable, reduce substitution & return Application with joined trees
					let args = partial_beta_reduce(args, reps, &mut args_bind, depth, exprs)?;
					*replace_index = BindIndex::join(func_bind, args_bind, reps);

					Expr::app(func, args, exprs)
				}
			}
		}
	})
}

pub fn beta_reduce<'a>(expr: &'a Expr<'a>, exprs: &'a LinkArena<'a>) -> Result<&'a Expr<'a>, LambdaError> {
	let reps = &LinkArena::new();
	Ok(partial_beta_reduce(expr, reps, &mut BindIndex::DEFAULT.clone(), 0, exprs)?)
}
