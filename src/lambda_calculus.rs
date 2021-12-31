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

use self::{pointer_helpers::left, typed::{Datatype, Hashtype}};

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
				PointerTree::Both(_, _) => Err(LambdaError::PointerTreeMismatch)?,
				PointerTree::End(val) if val == index => {
					*replace_in_tree = replacement_tree.clone();
					replacement.clone()
				},
				_ => Expr::var(),
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

			replace_in_tree.join_into(l, r, db)?;

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
			PointerTree::Both(left, right) => {
				match (right.is_none(), left.is_none()) {
					(true, true) => write!(f, "BOTH(NONE, NONE)"),
					(true, false) => write!(f, "<{}", left.display(db)),
					(false, true) => write!(f, ">{}", right.display(db)),
					(false, false) => write!(f, "({},{})", left.display(db), right.display(db)),
				}
			},
			PointerTree::End(v) => write!(f, "{}", v),
			PointerTree::None => write!(f, "?"),
		}
	}
}
use lazy_static::lazy_static;
lazy_static! {
	static ref PT_NONE: TypedHash<PointerTree> = unsafe { TypedHash::from_hash_unchecked(Hash::hash(PointerTree::None.to_data_untyped()), PhantomData::default()) };
	static ref PT_NONE_BOTH: TypedHash<PointerTree> = unsafe { TypedHash::from_hash_unchecked(Hash::hash(PointerTree::Both(PT_NONE.clone(), PT_NONE.clone()).to_data_untyped()), PhantomData::default()) };
}
impl TypedHash<PointerTree> {
	// 
	fn is_none(&self) -> bool {
		self.untyped() == PT_NONE.untyped()
	}
	fn split(self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		Ok(match self.resolve(db)? {
			PointerTree::Both(l, r) => (l, r),
			PointerTree::None => (self.clone(), self),
			PointerTree::End(_) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn join(self, other: Self, db: &mut Datastore) -> Result<Self, LambdaError> {
		Ok(match (self.resolve(db)?, other.resolve(db)?) {
			(PointerTree::None, PointerTree::None) => self,
			(_, _) => {
				PointerTree::Both(self, other).to_hash(db)
			}
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
					left.push_lambda_pointer(&l, count, db)?,
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
				if l.is_none() && r.is_none() { *self = PT_NONE.clone() } else {
					*self = PointerTree::Both(l, r).to_hash(db)
				}
				
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
	fn split(&mut self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		let (l, r) = self.0.clone().split(db)?;
		Ok((ReduceTree(l, self.1), ReduceTree(r, self.1)))
	}
	fn join_into(&mut self, left: Self, right: Self, db: &mut Datastore) -> Result<(), LambdaError> {
		self.0 = left.0.join(right.0, db)?; Ok(())
		//Ok(ReduceTree(self.0.join(&other.0, db)?, usize::max(self.1, other.1)))
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
		if self.1 != index || self.1 == 0 { return Err(LambdaError::PointerTreeMismatch) }
		let pointer = self.0.pop_lambda_pointer(index, db)?;
		self.1 -= 1; Ok(pointer)
	}
}
impl DisplayWithDatastore for ReduceTree {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		write!(f, "{}/{}", self.0.display(db), self.1)
	}
}

#[test]
fn test_pointer_trees() {
	use pointer_helpers::*;
	let db = &mut Datastore::new();
	PointerTree::None.to_hash(db);
	let r = &mut ReduceTree::new();
	println!("start: [{}]", r.display(db));

	let expr = parse_to_expr("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", db).unwrap();
	let (pointer_1, expr) = if let Expr::Lam(Lambda { pointers, expr } ) = expr { (pointers, expr.resolve(db).unwrap()) } else { unreachable!() };
	let (pointer_2, expr) = if let Expr::Lam(Lambda { pointers, expr } ) = expr { (pointers, expr.resolve(db).unwrap()) } else { unreachable!() };
	let (pointer_3, expr) = if let Expr::Lam(Lambda { pointers, expr } ) = expr { (pointers, expr.resolve(db).unwrap()) } else { unreachable!() };
	let (pointer_4, expr) = if let Expr::Lam(Lambda { pointers, expr } ) = expr { (pointers, expr.resolve(db).unwrap()) } else { unreachable!() };

	let index_1 = r.push_lambda_pointer(&pointer_1, db).unwrap();
	println!("add [{}] = [{}]", pointer_1.display(db), r.display(db));

	let index_2 = r.push_lambda_pointer(&pointer_2, db).unwrap();
	println!("add [{}] = [{}]", pointer_2.display(db), r.display(db));

	let index_3 = r.push_lambda_pointer(&pointer_3, db).unwrap();
	println!("add [{}] = [{}]", pointer_3.display(db), r.display(db));

	let index_4 = r.push_lambda_pointer(&pointer_4, db).unwrap();
	println!("add [{}] = [{}]", pointer_4.display(db), r.display(db));

	let r_before = r.clone();
	let (left, right) = r.split(db).unwrap();
	r.join_into(left, right, db).unwrap();
	assert_eq!(r_before.0.untyped(), r.0.untyped());
	assert_eq!(r_before.1, r.1);

	let pointer_4_pop = r.pop_lambda_pointer(index_4, db).unwrap();
	println!("pop {} [{}] = [{}]", index_4, r.display(db), pointer_4_pop.display(db));
	assert_eq!(pointer_4, pointer_4_pop);

	let pointer_3_pop = r.pop_lambda_pointer(index_3, db).unwrap();
	println!("pop {} [{}] = [{}]", index_3, r.display(db), pointer_3_pop.display(db));
	assert_eq!(pointer_3, pointer_3_pop);

	let pointer_2_pop = r.pop_lambda_pointer(index_2, db).unwrap();
	println!("pop {} [{}] = [{}]", index_2, r.display(db), pointer_2_pop.display(db));
	assert_eq!(pointer_2, pointer_2_pop);

	let pointer_1_pop = r.pop_lambda_pointer(index_1, db).unwrap();
	println!("pop {} [{}] = [{}]", index_1, r.display(db), pointer_1_pop.display(db));
	assert_eq!(pointer_1, pointer_1_pop);

	// Test Split & Join
	let r = &mut ReduceTree(PointerTree::None.to_hash(db), 0);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::End(0).to_hash(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::End(1).to_hash(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::Both(PT_NONE.clone(), PT_NONE.clone()).to_hash(db), 0);
	// test_split(r, db).unwrap(); // This will error

	let r = &mut ReduceTree(PointerTree::Both(PointerTree::End(1).to_hash(db), PT_NONE.clone()).to_hash(db), 1);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::Both(PointerTree::Both(PT_NONE.clone(), PointerTree::End(2).to_hash(db)).to_hash(db), PointerTree::End(2).to_hash(db)).to_hash(db), 2);
	test_split(r, db).unwrap();
}

fn test_split(r: &mut ReduceTree, db: &mut Datastore) -> Result<(), LambdaError> {
	let r_before = r.clone();
	print!("split [{}] ", r.display(db));
	let (left, right) = r.split(db).map_err(|e|{println!("err"); e})?;
	print!("([{}] [{}])", left.display(db), right.display(db));
	r.join_into(left, right, db).map_err(|e|{println!("err"); e})?;
	println!(" = [{}]", r.display(db));
	assert_eq!(r_before.0.untyped(), r.0.untyped());
	assert_eq!(r_before.1, r.1);
	Ok(())
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
			print!("{}[{}] reducing lam {} -------- [{}] -> ", pad, depth, Expr::Lam(Lambda { pointers: pointers.clone(), expr: expr.clone() }).display(db), reducing_tree.display(db));

			// Add pointers to pointer_trees so that if reduction moves variables, pointers can be updated
			let index = reducing_tree.push_lambda_pointer(&pointers, db)?;
			println!("[{}]", reducing_tree.display(db));

			let reduced_expr = partial_beta_reduce(Expr::from_hash(&expr, db)?, reducing_tree, depth, db)?.to_hash(db);
			println!("{}					[{}] -> ", pad, reducing_tree.display(db));

			Expr::Lam(Lambda {
				pointers: reducing_tree.pop_lambda_pointer(index, db)?,
				expr: reduced_expr
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
					
					reducing_tree.join_into(function_tree, substitution_tree, db)?;

					Expr::App(Application {
						function: Expr::Var(Variable).to_hash(db),
						substitution,
					})
				},
				Expr::Lam(Lambda { pointers, expr }) => {
					*reducing_tree = function_tree.clone();
					let index = reducing_tree.push_lambda_pointer(&pointers, db)?;
					reducing_tree.1 -= 1; // Needed because all PointerTree::Ends should be replaced in recur_replace

					println!("{}[{}] replace in {}: every {} in [{}] with {}: [{}] → ", pad, depth, expr.display(db), index, reducing_tree.display(db), substitution.display(db), substitution_tree.display(db));
					let replaced_form = if let Some(_) = &pointers {
						recur_replace(&expr, reducing_tree, index, &substitution, &substitution_tree, db)?
					} else {
						expr.clone()
					};
					println!("{}{}: [{}]", pad, replaced_form.display(db), reducing_tree.display(db));

					//println!("{}Replaced every {} with [{}] in [{}] -> [{}]", pad, index, substitution_tree.display(db), function_tree.display(db), reducing_tree.display(db));

					partial_beta_reduce(replaced_form.resolve(db)?, reducing_tree, depth, db)?
				},
				Expr::App(Application { function: _, substitution: _ }) => {
					let reduced_substitution = partial_beta_reduce(substitution.resolve(db)?, &mut substitution_tree, depth, db)?;
					
					reducing_tree.join_into(function_tree, substitution_tree, db)?;
					
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