use std::fmt;

use thiserror::Error;

use hashdb::{Datastore, Hash};

mod expr;
mod parse;
mod typed;
pub use expr::*;
pub use typed::TypedHash;
pub use parse::parse_to_expr;

use self::typed::Hashtype;

#[derive(Error, Debug)]
pub enum LambdaError {
	// Data -> Hash -> Data interpretation
	#[error("Invalid hashtype, got {0}")]
	InvalidLayout(Hash),
	#[error("Can't convert Variable variant to data, there is no data to serialize")]
	NoDataForVariable,
	#[error("Error in bincode")]
	SerdeError(#[from] bincode::Error),

	#[error("Format Error")]
	FormatError(#[from] fmt::Error),

	// Datastore Error
	#[error("Not in datastore: {0}")]
	NotInDatastore(Hash),

	// Beta Reduction Error
	#[error("Recursion Depth for beta reduction exceeded")]
	RecursionDepthExceeded,
}

/// Takes lambda expression and traverses down the binary tree using bitslices, replacing variables & rehashing.
fn recur_replace(replace_in_expr: Expr, replace_pointers: &TypedHash<LambdaPointer>, replacement: Expr, db: &mut Datastore) -> Result<Expr, LambdaError> {
	match replace_in_expr {
		// When encounter a variable, replace
		Expr::Var(_) => {
			Ok(replacement)
		},
		// When encounter a lambda, skip
		Expr::Lam(Lambda { pointers, expr }) => {
			let sub_replace_in_expr = expr.resolve(db)?;
			let replaced_expr = recur_replace(sub_replace_in_expr, replace_pointers, replacement, db)?.to_hash(db);
			Ok(Expr::Lam(Lambda { pointers, expr: replaced_expr }))
		},
		// When encounter a function application, check if any of the bit arrays want to go down each path, then go down those paths.
		Expr::App(Application { function, substitution }) => {
			let replace_pointer = replace_pointers.resolve(db)?;
			let sub_replace_pointers = match &replace_pointer {
				LambdaPointer::Left(hash) => (Some(hash), None),
				LambdaPointer::Right(hash) => (None, Some(hash)),
				LambdaPointer::Both(left, right) => (Some(left), Some(right)),
				LambdaPointer::End => (None, None),
			};
			let function = match sub_replace_pointers.0 {
				Some(pointers) => recur_replace(function.resolve(db)?, pointers, replacement.clone(), db)?.to_hash(db),
				None => function,
			};
			let substitution = match sub_replace_pointers.1  {
				Some(pointers) => recur_replace(substitution.resolve(db)?, pointers, replacement.clone(), db)?.to_hash(db),
				None => substitution,
			};

			Ok(Expr::App(Application { function, substitution }))
		}
	}
}

// Beta reduces expression without rehashing
pub fn partial_beta_reduce(reducing_expr: Expr, pointer_trees: &mut Vec<TypedHash<LambdaPointer>>, depth: usize, db: &mut Datastore) -> Result<Expr, LambdaError> {
	if depth > 20 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;

	Ok(match reducing_expr {
		Expr::Var(_) => reducing_expr,
		Expr::Lam(Lambda { expr, pointers }) => {
			let expr = Expr::from_hash(&expr, db)?;
			// Add pointers to bitsqueue so that if reduction moves variables, pointers can be updated
			let has_pointers = pointers.is_some();
			if let Some(pointers) = pointers {
				pointer_trees.push(pointers)
			}

			let reduced_expr = partial_beta_reduce(expr, pointer_trees, depth, db)?;

			let pointers = if has_pointers { pointer_trees.pop() } else { None };

			Expr::Lam(Lambda {
				pointers,
				expr: reduced_expr.to_hash(db)
			})
		}
		Expr::App(Application { function, substitution }) => {
			let function_reduced = partial_beta_reduce(function.resolve(db)?, pointer_trees, depth, db)?;
			match function_reduced {
				Expr::Var(_) => Expr::App(Application {
					function: Expr::Var(Variable).to_hash(db),
					substitution: partial_beta_reduce(substitution.resolve(db)?, pointer_trees, depth, db)?.to_hash(db)
				}),
				Expr::Lam(Lambda { pointers, expr }) => {
					let expr = expr.resolve(db)?;
					let replacement = Expr::from_hash(&substitution, db)?;

					let replaced_form = if let Some(pointers) = &pointers {
						// Debug
						let mut pointer_tree = String::new();
						write_lambda_pointers(&mut pointer_tree, pointers, db)?;
						print!("replace in: {} at [{}] with {}", format_expr(&expr, db)?, pointer_tree, format_expr(&replacement, db)?);
						let replaced_form = recur_replace(expr, &pointers, replacement, db)?;
						println!(" = {}", format_expr(&replaced_form, db)?);
						replaced_form
					} else {
						expr
					};

					partial_beta_reduce(replaced_form, pointer_trees, depth, db)?
				},
				Expr::App(Application { function: _, substitution: _ }) => {
					let reduced_substitution = partial_beta_reduce(substitution.resolve(db)?, pointer_trees, depth, db)?;
					Expr::App(Application { function: function_reduced.to_hash(db), substitution: reduced_substitution.to_hash(db) } )
				}
			}
		}
	})
}

pub fn beta_reduce(hash: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	Ok(partial_beta_reduce(hash.resolve(db)?, &mut vec![], 0, db)?.to_hash(db))
}