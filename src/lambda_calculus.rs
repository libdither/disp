use std::fmt;

use thiserror::Error;

use hashdb::{Datastore, Hash};

mod expr;
mod parse;
mod display;
mod typed;
pub use expr::*;
pub use typed::TypedHash;
pub use parse::parse_to_expr;
pub use display::*;

use self::typed::Hashtype;

#[derive(Error, Debug)]
pub enum LambdaError {
	// Data -> Hash -> Data interpretation
	#[error("Invalid hashtype, got {0}")]
	InvalidLayout(Hash),
	#[error("Error in bincode deserialization")]
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

struct DisplayIter<T: fmt::Display, I: Iterator<Item=T> + Clone>(I);

impl<T: fmt::Display, I: Iterator<Item=T> + Clone> fmt::Display for DisplayIter<T, I> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for string in self.0.clone() { write!(f, "{}", string)?; }; Ok(())
	}
}

// left (dir = false), right (dir = true)
fn descend_pointer_tree(pointer_tree: TypedHash<LambdaPointer>, dir: bool, db: &mut Datastore) -> Result<Option<TypedHash<LambdaPointer>>, LambdaError> {
	Ok(match (pointer_tree.resolve(db)?, dir) {
		(LambdaPointer::Left(l), false) => Some(l),
		(LambdaPointer::Right(r), true) => Some(r),
		(LambdaPointer::Both(l, r), right) => if right { Some(r) } else { Some(l) },
		_ => None,
	})
}

// left (dir = false), right (dir = true)
fn descend_pointer_trees(pointer_trees: &mut Vec<Option<TypedHash<LambdaPointer>>>, dir: bool, db: &mut Datastore) -> Result<(), LambdaError> {
	for pointer_tree in pointer_trees {
		*pointer_tree = if let Some(pointer_tree) = pointer_tree {
			descend_pointer_tree(pointer_tree.clone(), dir, db)?
		} else { None };
	}
	Ok(())
}

// Beta reduces expression without rehashing
pub fn partial_beta_reduce(reducing_expr: Expr, pointer_trees: &mut Vec<Option<TypedHash<LambdaPointer>>>, depth: usize, db: &mut Datastore) -> Result<Expr, LambdaError> {
	if depth > 20 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;
	let pad = DisplayIter(std::iter::repeat("    ").take(depth));

	let ret = match reducing_expr {
		Expr::Var(_) => {
			println!("{}[{}] reducing var {}", pad, depth, reducing_expr.display(db));
			reducing_expr
		},
		Expr::Lam(Lambda { expr, pointers }) => {
			println!("{}[{}] reducing lam {}", pad, depth, Expr::Lam(Lambda { pointers: pointers.clone(), expr: expr.clone() }).display(db));

			let expr = Expr::from_hash(&expr, db)?;
			// Add pointers to pointer_trees so that if reduction moves variables, pointers can be updated
			let has_pointers = pointers.is_some();
			if let Some(pointers) = pointers {
				pointer_trees.push(Some(pointers))
			}

			let reduced_expr = partial_beta_reduce(expr, pointer_trees, depth, db)?;

			let pointers = if has_pointers { pointer_trees.pop().expect("unreachable: there should be enough in this queue") } else { None };

			Expr::Lam(Lambda {
				pointers,
				expr: reduced_expr.to_hash(db)
			})
		}
		Expr::App(Application { function, substitution }) => {
			println!("{}[{}] reducing app {}", pad, depth, Expr::App(Application { function: function.clone(), substitution: substitution.clone() }).display(db));
			let function_reduced = partial_beta_reduce(function.resolve(db)?, pointer_trees, depth, db)?;
			match function_reduced {
				Expr::Var(_) => Expr::App(Application {
					function: Expr::Var(Variable).to_hash(db),
					substitution: partial_beta_reduce(substitution.resolve(db)?, pointer_trees, depth, db)?.to_hash(db)
				}),
				Expr::Lam(Lambda { pointers, expr }) => {
					let expr = expr.resolve(db)?;
					let replacement = Expr::from_hash(&substitution, db)?;

					let expr_d = expr.clone();
					let replacement_d = replacement.clone();

					let replaced_form = if let Some(pointers) = &pointers {
						recur_replace(expr, &pointers, replacement, db)?
					} else {
						expr
					};

					use itertools::Itertools;
					println!("{}Pointer Trees: [{}]", pad, pointer_trees.iter().map(|p|p.display(db)).format(","));

					print!("{}[{}] replace in: {} at [{}] with {}", pad, depth, expr_d.display(db), pointers.display(db), replacement_d.display(db));
					println!(" = {}", replaced_form.display(db));

					partial_beta_reduce(replaced_form, pointer_trees, depth, db)?
				},
				Expr::App(Application { function: _, substitution: _ }) => {
					let reduced_substitution = partial_beta_reduce(substitution.resolve(db)?, pointer_trees, depth, db)?;
					Expr::App(Application { function: function_reduced.to_hash(db), substitution: reduced_substitution.to_hash(db) } )
				}
			}
		}
	};
	println!("{}[{}] returning reduction: {}", pad, depth, ret.display(db));
	Ok(ret)
}

pub fn beta_reduce(hash: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	println!("Reducing: {}", hash.display(db));
	Ok(partial_beta_reduce(hash.resolve(db)?, &mut vec![], 0, db)?.to_hash(db))
}