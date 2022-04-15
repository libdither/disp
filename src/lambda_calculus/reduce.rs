use hashdb::{Datastore, NativeHashtype, Link};

use crate::lambda_calculus::DisplayWithDatastore;

use super::{Expr, LambdaError, ReduceArena, ReplaceIndex, ReplaceTree};

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
fn recur_replace<'a>(
	replace_in_expr: Link<Expr>, // Expr that should be replaced in
	arena: &'a ReduceArena<'a>,
	replace_index: &mut ReplaceIndex<'a>,
	replacement: &Link<Expr>,
	replacement_tree: &'a ReplaceTree<'a>,
	db: &mut Datastore) -> Result<Link<Expr>, LambdaError>
{
	Ok(match replace_in_expr.as_ref() {
		Expr::Variable => {
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
		Expr::Lambda { tree, expr } => {
			let tree = tree.clone();
			let replaced_expr = recur_replace(expr.clone(), arena, replace_index, replacement, replacement_tree, db)?;
			
			Link::new(Expr::Lambda { tree, expr: replaced_expr })
		},
		// When encounter an application in the replacement expression:
		Expr::Application { func, args } => {
			// Split into the function and substitution portions of the pointer tree to replace in
			let (mut func_index, mut sub_index) = replace_index.split()?;

			let (func, sub) = (func.clone(), args.clone());
			let func = recur_replace(func, arena, &mut func_index, replacement, replacement_tree, db)?;
			let sub = recur_replace(sub, arena, &mut sub_index, replacement, replacement_tree, db)?;

			*replace_index = arena.join_index(func_index, sub_index);
			Link::new(Expr::Application { func, args: sub })
		}
	})
}

/// Reduces reducing_expr and returns Link<Expr>
fn partial_beta_reduce<'a>(reducing_expr: Link<Expr>, arena: &'a ReduceArena<'a>, replace_index: &mut ReplaceIndex<'a>, depth: usize, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	if depth > 200 { return Err(LambdaError::RecursionDepthExceeded) }

	Ok(match reducing_expr.as_ref() {
		Expr::Variable => reducing_expr,
		Expr::Lambda { tree, expr } => {
			arena.push_pointer_tree(replace_index, tree, db)?;

			let reduced_expr = partial_beta_reduce(expr.clone(), arena, replace_index, depth, db)?;

			Link::new(Expr::Lambda {
				tree: arena.pop_pointer_tree(replace_index, db)?,
				expr: reduced_expr
			})
		}
		Expr::Application { func, args: sub } => {
			// Split subtrees
			let (mut func_tree, mut sub_tree) = replace_index.split()?;
			let (func, sub) = (func.clone(), sub.clone());
			let func = partial_beta_reduce(func.clone(), arena, &mut func_tree, depth, db)?;

			match func.as_ref() {
				Expr::Variable | Expr::Application { .. } => {
					// If Variable, reduce substitution & return Application with joined trees
					let sub = partial_beta_reduce(sub.clone(), arena, &mut sub_tree, depth, db)?;
					*replace_index = arena.join_index(func_tree, sub_tree);

					Link::new(Expr::Application { func, args: sub, })
				},
				Expr::Lambda { tree, expr } => {
					// Replace all tree in expr & reduce the output
					*replace_index = func_tree;
					arena.push_pointer_tree(replace_index, tree, db)?;

					let replaced_expr = recur_replace(expr.clone(), arena, replace_index, &sub, &sub_tree.tree, db)?;
					
					replace_index.index -= 1; // All of current index will be replaced in recur_replace, thus this is needed
					
					let depth = depth + 1;
					partial_beta_reduce(replaced_expr, arena, replace_index, depth, db)?
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

pub fn beta_reduce(expr: &Link<Expr>, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	println!("Reducing: {}", expr.display(db));
	let arena = ReduceArena::new();
	Ok(partial_beta_reduce(expr.clone(), &arena, &mut arena.index(), 0, db)?)
}