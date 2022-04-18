use hashdb::{Datastore, NativeHashtype, Link, LinkArena};

use crate::lambda_calculus::DisplayWithDatastore;

use super::{Expr, LambdaError, ReplaceIndex, ReplaceTree};

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace<'a>(
	working_expr: Link<Expr>, // Working Expression
	reps: &'a LinkArena<'a>, // ReplaceTree Arena
	replace_index: &mut ReplaceIndex<'a>, // Replace index in ReplaceTree to replace
	replacement: &Link<Expr>, // Replacement Expr
	replacement_tree: &'a ReplaceTree<'a>, // Replacement Tree
	exprs: &'a LinkArena<'a>, // Expr Arena
) -> Result<Link<'a, Expr<'a>>, LambdaError>
{
	Ok(match working_expr {
		Expr::Variable => {
			// When encounter a variable and index is correct, replace with replacement
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in external_tree
			match replace_index.tree {
				ReplaceTree::Branch(_, _) => Err(LambdaError::PointerTreeMismatch)?,
				ReplaceTree::End(val) if *val == replace_index.index => {
					replace_index.tree = replacement_tree;
					replacement.clone()
				},
				_ => working_expr,
			}
		},
		// When encounter a lambda, unwrap, recurse, re-wrap
		Expr::Lambda { tree, expr } => {
			let tree = tree.clone();
			let replaced_expr = recur_replace(expr.clone(), reps, replace_index, replacement, replacement_tree, exprs)?;
			
			Expr::lambda(tree, &replaced_expr, exprs)
		},
		// When encounter an application in the replacement expression:
		Expr::Application { func, args } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (mut func_index, mut sub_index) = replace_index.split()?;

			let (func, sub) = (func.clone(), args.clone());
			let func = recur_replace(func, reps, &mut func_index, replacement, replacement_tree, exprs)?;
			let args = recur_replace(sub, reps, &mut sub_index, replacement, replacement_tree, exprs)?;

			*replace_index = reps.join_index(func_index, sub_index);
			Expr::app(&func, &args, exprs)
		}
	})
}

/// Reduces reducing_expr and returns Link<Expr>
fn partial_beta_reduce<'a>(reducing_expr: Link<Expr>, reps: &'a LinkArena<'a>, replace_index: &mut ReplaceIndex<'a>, depth: usize, exprs: &'a LinkArena<'a>) -> Result<Link<'a, Expr<'a>>, LambdaError> {
	if depth > 200 { return Err(LambdaError::RecursionDepthExceeded) }

	Ok(match reducing_expr.as_ref() {
		Expr::Variable => reducing_expr,
		Expr::Lambda { tree, expr } => {
			reps.push_pointer_tree(replace_index, tree, exprs)?;

			let reduced_expr = partial_beta_reduce(expr, reps, replace_index, depth, exprs)?;

			Expr::lambda(reps.pop_pointer_tree(replace_index, exprs)?, reduced_expr)
		}
		Expr::Application { func, args: sub } => {
			// Split subtrees
			let (mut func_tree, mut sub_tree) = replace_index.split()?;
			let (func, sub) = (func.clone(), sub.clone());
			let func = partial_beta_reduce(func.clone(), reps, &mut func_tree, depth, exprs)?;

			match func.as_ref() {
				Expr::Variable | Expr::Application { .. } => {
					// If Variable, reduce substitution & return Application with joined trees
					let args = partial_beta_reduce(sub.clone(), reps, &mut sub_tree, depth, exprs)?;
					*replace_index = reps.join_index(func_tree, sub_tree);

					Expr::app(func, args)
				},
				Expr::Lambda { tree, expr } => {
					// Replace all tree in expr & reduce the output
					*replace_index = func_tree;
					reps.push_pointer_tree(replace_index, tree, exprs)?;

					let replaced_expr = recur_replace(expr.clone(), reps, replace_index, &sub, &sub_tree.tree, exprs)?;
					
					replace_index.index -= 1; // All of current index will be replaced in recur_replace, thus this is needed
					
					let depth = depth + 1;
					partial_beta_reduce(replaced_expr, reps, replace_index, depth, exprs)?
				},
				/* Expr::Application { .. } => {
					// Beta reduce sub, return application with joined trees
					// let sub = partial_beta_reduce(sub.clone(), arena, &mut sub_tree, depth, db)?;
					*replace_index = arena.join_index(func_tree, sub_tree);
					Expr::Application { func, sub }
				} */
			}
		}
	})
}

pub fn beta_reduce<'a>(expr: &'a Link<Expr>, exprs: &'a LinkArena<'a>) -> Result<Link<'a, Expr<'a>>, LambdaError> {
	println!("Reducing: {}", expr);
	let arena = &LinkArena::new();
	Ok(partial_beta_reduce(expr.clone(), &arena, &mut arena.index(), 0, exprs)?)
}