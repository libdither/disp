//! This file contains functions to evaluate or beta reduce expressions
use thiserror::Error;

use hashdb::{LinkArena, RevTypeStore, TypeStore, hashtype};

use super::{BindIndex, BindSubTree, BindTreeError, Expr};

#[derive(Error, Debug)]
pub enum ReduceError {
	// Beta Reduction Error
	#[error("recursion depth for beta reduction exceeded")]
	RecursionDepthExceeded,

	#[error("binding level mismatch: make sure variable bindings match with variable positions in expressions and that bindings don't overlap")]
	BindingLevelMismatch,

	#[error("found variable in expression but binding tree is branching")]
	UnexpectedBranchInSubstitution,

	#[error("bind error: {0}")]
	BindError(#[from] BindTreeError)
}

/// Recursively substitute expressions for certain variables
/// Takes lambda expression, for each variable in Lambda { expr }, if Lambda { tree } index == replace_index, replace subexpr with replacement and subtree with replacement_tree
impl<'e> Expr<'e> {
	fn recur_replace<'r>(
		self: &'e Expr<'e>,					// Working Expression
		bind_index: &mut BindIndex<'r>,	// Replace index in ReplaceTree to replace
		replace_expr: &'e Expr<'e>,			// Replacement Expr
		replace_bind: &'r BindSubTree<'r>,	// Bind Tree to replace with
		binds: &'r impl TypeStore<'r>,		// BindSubTree Arena
		exprs: &'e impl TypeStore<'e>,		// Expr Arena
	) -> Result<&'e Expr<'e>, ReduceError> {
		Ok(match self {
			Expr::Variable => {
				// When encounter a variable and index is correct, replace with replacement
				// Must be BindTree::None because replace_in_expr's variables aren't registered in external_tree
				match bind_index.tree {
					BindSubTree::Branch(_, _) => Err(ReduceError::UnexpectedBranchInSubstitution)?,
					BindSubTree::End(val) if *val == bind_index.index => {
						bind_index.tree = replace_bind;
						replace_expr
					}
					_ => &self,
				}
			}
			// When encounter a lambda, unwrap, recurse, re-wrap
			Expr::Lambda { bind, expr } => {
				let replaced_expr = expr.recur_replace(bind_index, replace_expr, replace_bind, binds, exprs)?;
	
				Expr::lambda(bind, &replaced_expr, exprs)
			}
			// When encounter an application in the replacement expression:
			Expr::Application { func, args } => {
				// Split into the function and substitution portions of the pointer tree to replace in
				let (mut func_bind, mut args_bind) = bind_index.split()?;
	
				let func = func.recur_replace(&mut func_bind, replace_expr, replace_bind, binds, exprs)?;
				let args = args.recur_replace(&mut args_bind, replace_expr, replace_bind, binds, exprs)?;
	
				*bind_index = BindIndex::join(func_bind, args_bind, binds);
				Expr::app(&func, &args, exprs)
			}
		})
	}
	
	/// Reduces reducing_expr and returns &'e Expr<'e>
	fn partial_reduce<'r>(
		self: &'e Expr<'e>,
		bind_index: &mut BindIndex<'r>,
		depth: usize,
		reps: &'r impl TypeStore<'r>,
		exprs: &'e impl TypeStore<'e>
	) -> Result<&'e Expr<'e>, ReduceError> {
		if depth > 200 {
			return Err(ReduceError::RecursionDepthExceeded);
		}
	
		Ok(match self {
			Expr::Variable => self,
			Expr::Lambda { bind, expr } => {
				bind_index.push_binding(bind, reps)?;
	
				let reduced_expr = expr.partial_reduce(bind_index, depth, reps, exprs)?;
	
				Expr::lambda(bind_index.pop_binding(reps, exprs)?, reduced_expr, exprs)
			}
			Expr::Application { func, args } => {
				// Split subtrees
				let (mut func_bind, mut args_bind) = bind_index.split()?;
				// Reduce function tree
				let func: &Expr = func.partial_reduce(&mut func_bind, depth, reps, exprs)?;
	
				match func {
					Expr::Lambda { bind, expr } => {
						// Replace all tree in expr & reduce the output
						*bind_index = func_bind;
						bind_index.push_binding(bind, reps)?;
	
						let replaced_expr = expr.recur_replace(bind_index, &args, &args_bind.tree, reps, exprs)?;
	
						bind_index.index -= 1; // All of current index will be replaced in recur_replace, thus this is needed
	
						let depth = depth + 1;
						replaced_expr.partial_reduce(bind_index, depth, reps, exprs)?
					}
					_ => {
						// If Variable or unreduced Application, reduce substitution & return unreduced Application and merge variable bindings.
						let args = args.partial_reduce(&mut args_bind, depth, reps, exprs)?;
						*bind_index = BindIndex::join(func_bind, args_bind, reps);
	
						Expr::app(func, args, exprs)
					}
				}
			}
		})
	}
	
	pub fn reduce(self: &'e Expr<'e>, exprs: &'e impl TypeStore<'e>) -> Result<&'e Expr<'e>, ReduceError> {
		let reps = &LinkArena::new();
		Ok(self.partial_reduce(&mut BindIndex::DEFAULT.clone(), 0, reps, exprs)?)
	}
}

#[hashtype]
#[derive(Debug)]
pub struct ReduceLink<'e> {
	#[subtype_reverse_link] expr: &'e Expr<'e>,
	#[subtype_reverse_link] reduced: &'e Expr<'e>,
}
impl<'e> ReduceLink<'e> {
	fn reduce(expr: &'e Expr<'e>, links: &'e impl RevTypeStore<'e>) -> Result<&'e ReduceLink<'e>, ReduceError> {
		let reduced = expr.reduce(links)?;
		Ok(links.rev_add(ReduceLink {
			expr, reduced
		}))
	}
}