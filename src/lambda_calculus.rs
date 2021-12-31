use std::{fmt, marker::PhantomData};

use thiserror::Error;

use hashdb::{Data, Datastore, Hash};

mod expr;
mod parse;
mod display;
mod typed;
pub use expr::*;
pub use typed::TypedHash;
pub use parse::parse_to_expr;
pub use display::*;

use self::typed::{Datatype, Hashtype};

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

	#[error("Pointer Tree Error")]
	PointerTreeMismatch,
}

/// Takes lambda expression and traverses down the binary tree using bitslices, replacing variables & rehashing.
fn recur_replace(replace_in_expr: &TypedHash<Expr>, replace_in_tree: &mut ReduceTree, index: usize, replacement: &TypedHash<Expr>, replacement_tree: &ReduceTree, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	Ok(match replace_in_expr.resolve(db)? {
		// When encounter a variable, replace
		Expr::Var(_) => {
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in replace_in_tree
			match replace_in_tree.0.resolve(db)? {
				PointerTree::End(val) if val == index => {
					*replace_in_tree = replacement_tree.clone();
					replacement.clone()
				},
				PointerTree::None => replace_in_expr.clone(),
				_ => Err(LambdaError::PointerTreeMismatch)?,
			}
		},
		// When encounter a lambda, skip
		Expr::Lam(Lambda { pointers, expr }) => {
			let replaced_expr = recur_replace(&expr, replace_in_tree, index, replacement, replacement_tree, db)?;
			Expr::Lam(Lambda { pointers, expr: replaced_expr }).to_hash(db)
		},
		// When encounter a function application, check if any of the bit arrays want to go down each path, then go down those paths.
		Expr::App(Application { function, substitution }) => {
			let (mut l, mut r) = replace_in_tree.split(db)?;
			
			let function = recur_replace(&function, &mut l, index, replacement, replacement_tree, db)?;
			let substitution = recur_replace(&substitution, &mut r, index, replacement, replacement_tree, db)?;

			replace_in_tree.0 = PointerTree::Both(l.0, r.0).to_hash(db);

			Expr::App(Application { function, substitution }).to_hash(db)
		}
	})
}

use serde::{Serialize, Deserialize};
#[derive(Serialize, Deserialize)]
enum PointerTree {
	None,
	End(usize),
	Both(TypedHash<PointerTree>, TypedHash<PointerTree>),
}
impl Datatype for PointerTree {
	type Error = LambdaError;
	fn from_data_untyped(data: &Data) -> Result<Self, Self::Error> {
		Ok(bincode::deserialize(data.as_bytes())?)
	}
	fn to_data_untyped(&self) -> Data {
		Data::from_vec(bincode::serialize(self).unwrap())
	}
	fn db_error(hash: Hash) -> Self::Error { LambdaError::NotInDatastore(hash) }
}
impl DisplayWithDatastore for PointerTree {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		match self { 
			PointerTree::Both(left, right) if right.untyped() == PT_NONE.untyped()  => {
				write!(f, "<{}", left.display(db))
			},
			PointerTree::Both(left, right) if left.untyped() == PT_NONE.untyped() => {
				write!(f, ">{}", right.display(db))
			},
			PointerTree::Both(left, right) => {
				write!(f, "({},{})", left.display(db), right.display(db))
			},
			PointerTree::End(v) => write!(f, "{}", v),
			PointerTree::None => write!(f, "?"),
		}
	}
}
use lazy_static::lazy_static;
lazy_static! {
	static ref PT_NONE: TypedHash<PointerTree> = unsafe { TypedHash::from_hash_unchecked(Hash::hash(PointerTree::None.to_data_untyped()), PhantomData::default()) };
}
impl TypedHash<PointerTree> {
	// 
	fn split(&self, db: &Datastore) -> Result<(TypedHash<PointerTree>, TypedHash<PointerTree>), LambdaError> {
		Ok(match self.resolve(db)? {
			PointerTree::Both(l, r) => (l, r),
			PointerTree::None => (PT_NONE.clone(), PT_NONE.clone()),
			PointerTree::End(_) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn push_lambda_pointer(&self, pointer: &TypedHash<LambdaPointer>, count: usize, db: &mut Datastore) -> Result<Self, LambdaError> {
		Ok(match (self.resolve(db)?, pointer.resolve(db)?) {
			// If PointerTree is None, fill in pointer
			(PointerTree::None, pointer) => match pointer {
				LambdaPointer::Left(l) => {
					PointerTree::Both(PT_NONE.push_lambda_pointer(&l, count, db)?, PT_NONE.clone()).to_hash(db)
				},
				LambdaPointer::Right(r) => {
					PointerTree::Both(PT_NONE.clone(), PT_NONE.push_lambda_pointer(&r, count, db)?).to_hash(db)
				},
				LambdaPointer::Both(l, r) => {
					PointerTree::Both(
						PT_NONE.push_lambda_pointer(&l, count, db)?,
						PT_NONE.push_lambda_pointer(&r, count, db)?,
					).to_hash(db)
				}
				LambdaPointer::End => PointerTree::End(count).to_hash(db),
			},
			(PointerTree::Both(left, right), pointer) => match pointer {
				LambdaPointer::Left(l) => PointerTree::Both(left.push_lambda_pointer(&l, count, db)?, right).to_hash(db),
				LambdaPointer::Right(r) => PointerTree::Both(left, right.push_lambda_pointer(&r, count, db)?).to_hash(db),
				LambdaPointer::Both(l, r) => PointerTree::Both(
					right.push_lambda_pointer(&l, count, db)?,
					right.push_lambda_pointer(&r, count, db)?
				).to_hash(db),
				LambdaPointer::End => Err(LambdaError::PointerTreeMismatch)?
			}
			(PointerTree::End(_), _) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn pop_lambda_pointer(&mut self, index: usize, db: &mut Datastore) -> Result<Option<TypedHash<LambdaPointer>>, LambdaError> {
		Ok(Some(match self.resolve(db)? {
			PointerTree::Both(mut l, mut r) => {
				let lambda_left = l.pop_lambda_pointer(index, db)?;
				let lambda_right = r.pop_lambda_pointer(index, db)?;
				*self = PointerTree::Both(l, r).to_hash(db);
				match (lambda_left, lambda_right) {
					(Some(l), Some(r)) => LambdaPointer::Both(l, r),
					(Some(l), None) => LambdaPointer::Left(l),
					(None, Some(l)) => LambdaPointer::Right(l),
					(None, None) => return Ok(None),
				}.to_hash(db)
			},
			PointerTree::End(count) => if index == count {
				*self = PT_NONE.clone();
				LambdaPointer::End.to_hash(db)
			} else { return Ok(None) }
			PointerTree::None => return Ok(None),
		}))
	}
}

#[derive(Clone)]
struct ReduceTree(TypedHash<PointerTree>, usize);
impl ReduceTree {
	fn new() -> Self { Self(PT_NONE.clone(), 0) }
	fn split(&self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		let (l, r) = self.0.split(db)?;
		Ok((ReduceTree(l, self.1), ReduceTree(r, self.1)))
	}
	/* fn descend_tree(self, dir: bool),
	fn ascend_tree(self, wrapped: ) */

	// Add LambdaPointer to PointerTree, keeping track of how many have been added
	fn push_lambda_pointer(&mut self, pointer: &Option<TypedHash<LambdaPointer>>, db: &mut Datastore) -> Result<usize, LambdaError> {
		self.1 += 1;
		if let Some(pointer) = pointer {
			self.0 = self.0.push_lambda_pointer(pointer, self.1, db)?
		};
		Ok(self.1)	
	}
	fn pop_lambda_pointer(&mut self, index: usize, db: &mut Datastore) -> Result<Option<TypedHash<LambdaPointer>>, LambdaError> {
		if self.1 != index { return Err(LambdaError::PointerTreeMismatch) }
		let pointer = self.0.pop_lambda_pointer(index, db)?;
		Ok(match self.1 {
			0 => None,
			_ => { self.1 -= 1; pointer } 
		})
	}
}
impl DisplayWithDatastore for ReduceTree {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		write!(f, "{}/{}", self.0.display(db), self.1)
	}
}


// Beta reduces expression without rehashing
fn partial_beta_reduce(reducing_expr: Expr, reducing_tree: &mut ReduceTree, depth: usize, db: &mut Datastore) -> Result<Expr, LambdaError> {
	if depth > 20 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;

	struct DisplayIter<T: fmt::Display, I: Iterator<Item=T> + Clone>(I);
	impl<T: fmt::Display, I: Iterator<Item=T> + Clone> fmt::Display for DisplayIter<T, I> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			for string in self.0.clone() { write!(f, "{}", string)?; }; Ok(())
		}
	}
	let pad = DisplayIter(std::iter::repeat("    ").take(depth));

	let ret = match reducing_expr {
		Expr::Var(_) => {
			println!("{}[{}] reducing var {} -------- [{}]", pad, depth, reducing_expr.display(db), reducing_tree.display(db));
			reducing_expr
		},
		Expr::Lam(Lambda { expr, pointers }) => {
			println!("{}[{}] reducing lam {} -------- [{}]", pad, depth, Expr::Lam(Lambda { pointers: pointers.clone(), expr: expr.clone() }).display(db), reducing_tree.display(db));

			let expr = Expr::from_hash(&expr, db)?;
			// Add pointers to pointer_trees so that if reduction moves variables, pointers can be updated
			let index = reducing_tree.push_lambda_pointer(&pointers, db)?;

			let reduced_expr = partial_beta_reduce(expr, reducing_tree, depth, db)?;

			Expr::Lam(Lambda {
				pointers: reducing_tree.pop_lambda_pointer(index, db)?,
				expr: reduced_expr.to_hash(db)
			})
		}
		Expr::App(Application { function, substitution }) => {
			println!("{}[{}] reducing app {} -------- [{}]", pad, depth, Expr::App(Application { function: function.clone(), substitution: substitution.clone() }).display(db), reducing_tree.display(db));
			
			// Descend to subtrees
			let (mut function_tree, mut substitution_tree) = reducing_tree.split(db)?;

			let function_reduced = partial_beta_reduce(function.resolve(db)?, &mut function_tree, depth, db)?;

			let ret = match function_reduced {
				Expr::Var(_) => {
					let substitution = partial_beta_reduce(substitution.resolve(db)?, &mut substitution_tree, depth, db)?.to_hash(db);
					
					reducing_tree.0 = PointerTree::Both(function_tree.0, substitution_tree.0).to_hash(db);
					
					Expr::App(Application {
						function: Expr::Var(Variable).to_hash(db),
						substitution,
					})
				},
				Expr::Lam(Lambda { pointers, expr }) => {
					let index = function_tree.push_lambda_pointer(&pointers, db)?;
					function_tree.1 -= 1; // Needed because all PointerTree::Ends should be replaced in recur_replace

					print!("{}[{}] replaced in: {} at [{}] with {} → ", pad, depth, expr.display(db), function_tree.display(db), substitution.display(db));
					let replaced_form = if let Some(_) = &pointers {
						recur_replace(&expr, &mut function_tree, index, &substitution, &substitution_tree, db)?
					} else {
						expr.clone()
					};
					println!("{}", replaced_form.display(db));

					println!("{}Reducing Tree: [{}], Function Tree: [{}], Sub Tree: [{}]", pad, reducing_tree.display(db), function_tree.display(db), substitution_tree.display(db));

					reducing_tree.0 = function_tree.0;

					partial_beta_reduce(replaced_form.resolve(db)?, reducing_tree, depth, db)?
				},
				Expr::App(Application { function: _, substitution: _ }) => {
					let reduced_substitution = partial_beta_reduce(substitution.resolve(db)?, &mut substitution_tree, depth, db)?;
					
					reducing_tree.0 = PointerTree::Both(function_tree.0, substitution_tree.0).to_hash(db);
					
					Expr::App(Application { function: function_reduced.to_hash(db), substitution: reduced_substitution.to_hash(db) } )
				}
			};
			//*reducing_tree = ReduceTree(PointerTree::Both(function_tree.0, substitution_tree.0).to_hash(db), substitution_tree.1);
			ret
		}
	};
	println!("{}[{}] returning reduction: {} -------- [{}]", pad, depth, ret.display(db), reducing_tree.display(db));
	Ok(ret)
}

pub fn beta_reduce(hash: &TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	println!("Reducing: {}", hash.display(db));
	Ok(partial_beta_reduce(hash.resolve(db)?, &mut ReduceTree::new(), 0, db)?.to_hash(db))
}