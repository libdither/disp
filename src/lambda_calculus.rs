use std::{fmt, marker::PhantomData};

use thiserror::Error;

use hashdb::{Data, Datastore, Hash, Hashtype, HashtypeResolveError, Link, TypedHash};

mod expr;
mod display;
pub use expr::*;
pub use display::*;



#[derive(Error, Debug)]
pub enum LambdaError {
	// Data -> Hash -> Data interpretation
	#[error("Invalid hashtype, got {0}")]
	InvalidLayout(Hash),

	#[error("Format Error")]
	FormatError(#[from] fmt::Error),

	// Datastore Error
	#[error("Not in datastore: {0}")]
	NotInDatastore(Hash),

	// Hashtype Error
	#[error("Failed to resolve hashtype")]
	HashtypeResolveError(#[from] HashtypeResolveError),

	// Beta Reduction Error
	#[error("Recursion Depth for beta reduction exceeded")]
	RecursionDepthExceeded,

	#[error("Pointer Tree Error")]
	PointerTreeMismatch,
}

/// Takes lambda expression and traverses down the binary tree using bitslices, replacing variables & rehashing.
fn recur_replace(replace_in_expr: Link<Expr>, replace_in_tree: &mut ReduceTree, index: usize, replacement: Link<Expr>, replacement_tree: &ReduceTree, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	Ok(match &*replace_in_expr {
		// When encounter a variable, replace
		Expr::Variable => {
			// Must be PointerTree::None because replace_in_expr's variables aren't registered in replace_in_tree
			match *replace_in_tree.0 {
				PointerTree::Both(_, _) => Err(LambdaError::PointerTreeMismatch)?,
				PointerTree::End(val) if val == index => {
					*replace_in_tree = replacement_tree.clone();
					replacement
				},
				_ => replace_in_expr,
			}
		},
		// When encounter a lambda, skip
		Expr::Lambda { pointers, expr } => {
			let replaced_expr = recur_replace(expr.clone(), replace_in_tree, index, replacement, replacement_tree, db)?;
			Expr::Lambda { pointers: pointers.clone(), expr: replaced_expr }.store(db)
		},
		// When encounter a function application, check if any of the bit arrays want to go down each path, then go down those paths.
		Expr::Application { function, substitution } => {
			let (mut l, mut r) = replace_in_tree.split(db)?;
			
			let function = recur_replace(function.clone(), &mut l, index, replacement.clone(), replacement_tree, db)?;
			let substitution = recur_replace(substitution.clone(), &mut r, index, replacement, replacement_tree, db)?;

			replace_in_tree.join_into(l, r, db);

			Expr::Application { function, substitution }.store(db)
		}
	})
}

#[derive(Clone, Debug)]
enum PointerTree {
	None,
	End(usize),
	Both(Link<PointerTree>, Link<PointerTree>),
}
impl Hashtype for PointerTree {
	fn hash(&self) -> TypedHash<Self> {
		use bytes::BufMut;
		let mut buf = Vec::new();
		use PointerTree::*;
		buf.put_u8(match self { None => 0, End(_) => 1, Both(_, _) => 2 });
		match self {
			_ => {},
			End(v) => buf.put_u64(*v as u64),
			Both(l, r) => {
				buf.put(l.hash().as_bytes());
				buf.put(r.hash().as_bytes());
			},
		}
		Hash::hash(&buf).into()
	}
	/* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, hashdb::hashtype::HashtypeResolveError> {
		use bytes::Buf;
		let mut data = db.get(hash)?;
		Ok(match data.get_u8() {
			0 => PointerTree::None,
			1 => PointerTree::End(data.get_u64()),
			2 => PointerTree::Both(Link::resolve(&mut data)?, Link::resolve(&mut data)),
		})
	} */
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

/* lazy_static! {
	static ref PT_NONE: Link<PointerTree> = PointerTree::None.store(db);
	static ref PointerTree::None_BOTH: TypedHash<PointerTree> = unsafe { TypedHash::from_hash_unchecked(Hash::hash(PointerTree::Both(PointerTree::None.store(db), PointerTree::None.store(db)).to_data_untyped()), PhantomData::default()) };
} */

impl PointerTree {
	fn is_none(&self) -> bool {
		if let PointerTree::None = self { true } else { false }
	}
	fn split(&self, db: &Datastore) -> Result<(Link<Self>, Link<Self>), LambdaError> {
		Ok(match self {
			PointerTree::Both(l, r) => (l.clone(), r.clone()),
			PointerTree::None => (PointerTree::None.link(), PointerTree::None.link()),
			PointerTree::End(_) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn join(&self, other: &Self, db: &mut Datastore) -> Link<Self> {
		match (self, other) {
			(PointerTree::None, PointerTree::None) => PointerTree::None.store(db),
			(_, _) => {
				PointerTree::Both(self.clone().store(db), other.clone().store(db)).store(db)
			}
		}
	}
	fn push_lambda_pointer(&self, pointer: &Link<LambdaPointer>, count: usize, db: &mut Datastore) -> Result<Link<Self>, LambdaError> {
		use PointerTree as PT;
		Ok(match (self, pointer) {
			// If PointerTree is None, fill in pointer
			(PointerTree::None, pointer) => match pointer.as_ref() {
				LambdaPointer::Left(l) => {
					PointerTree::Both(PointerTree::None.push_lambda_pointer(l, count, db)?, PointerTree::None.store(db)).store(db)
				},
				LambdaPointer::Right(r) => {
					PointerTree::Both(PointerTree::None.store(db), PointerTree::None.push_lambda_pointer(&r, count, db)?).store(db)
				},
				LambdaPointer::Both(l, r) => {
					PointerTree::Both(
						PointerTree::None.push_lambda_pointer(l, count, db)?,
						PointerTree::None.push_lambda_pointer(r, count, db)?,
					).store(db)
				}
				LambdaPointer::End => PointerTree::End(count).store(db),
			},
			(PointerTree::Both(left, right), pointer) => match pointer.as_ref() {
				LambdaPointer::Left(l) => PointerTree::Both(left.push_lambda_pointer(l, count, db)?, right.clone()).store(db),
				LambdaPointer::Right(r) => PointerTree::Both(left.clone(), right.push_lambda_pointer(r, count, db)?).store(db),
				LambdaPointer::Both(l, r) => PointerTree::Both(
					left.push_lambda_pointer(l, count, db)?,
					right.push_lambda_pointer(r, count, db)?
				).store(db),
				LambdaPointer::End => Err(LambdaError::PointerTreeMismatch)?
			}
			(PointerTree::End(_), _) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn pop_lambda_pointer(&self, index: usize, db: &mut Datastore) -> Result<(Link<Self>, Option<Link<LambdaPointer>>), LambdaError> {
		Ok(match self {
			PointerTree::Both(l, r) => {
				let (l, lambda_left) = l.pop_lambda_pointer(index, db)?;
				let (r, lambda_right) = r.pop_lambda_pointer(index, db)?;
				
				(
					if l.is_none() && r.is_none() { PointerTree::None } else { PointerTree::Both(l, r) }.store(db),
					match (lambda_left, lambda_right) {
						(Some(l), Some(r)) => Some(LambdaPointer::Both(l, r).store(db)),
						(Some(l), None) => Some(LambdaPointer::Left(l).store(db)),
						(None, Some(r)) => Some(LambdaPointer::Right(r).store(db)),
						(None, None) => None,
					}
				)
			},
			PointerTree::End(count) => if index == *count {
				(PointerTree::None.store(db), Some(LambdaPointer::End.store(db)))
			} else { (self.clone().store(db), None) },
			PointerTree::None => (PointerTree::None.store(db), None),
		})
	}
}

#[derive(Clone)]
struct ReduceTree(Link<PointerTree>, usize);
impl ReduceTree {
	fn new() -> Self { Self(PointerTree::None.link(), 0) }
	fn split(&mut self, db: &Datastore) -> Result<(Self, Self), LambdaError> {
		let (l, r) = self.0.split(db)?;
		Ok((ReduceTree(l, self.1), ReduceTree(r, self.1)))
	}
	fn join_into(&mut self, left: Self, right: Self, db: &mut Datastore) {
		self.0 = left.0.join(&right.0, db);
	}

	// Add LambdaPointer to PointerTree, keeping track of how many have been added
	fn push_lambda_pointer(&mut self, pointer: &Option<Link<LambdaPointer>>, db: &mut Datastore) -> Result<usize, LambdaError> {
		self.1 += 1;
		if let Some(pointer) = pointer {
			self.0 = self.0.push_lambda_pointer(pointer, self.1, db)?
		};
		Ok(self.1)	
	}
	fn pop_lambda_pointer(&mut self, index: usize, db: &mut Datastore) -> Result<Option<Link<LambdaPointer>>, LambdaError> {
		if self.1 != index || self.1 == 0 { return Err(LambdaError::PointerTreeMismatch) }
		let (out, pointer) = self.0.pop_lambda_pointer(index, db)?;
		self.0 = out;
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
	let db = &mut Datastore::new();
	PointerTree::None.store(db);
	let r = &mut ReduceTree::new();
	println!("start: [{}]", r.display(db));

	let expr = crate::parse_to_expr("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", db).unwrap();
	let (pointer_1, expr) = if let Expr::Lambda { pointers, expr } = &*expr { (pointers, expr.clone()) } else { unreachable!() };
	let (pointer_2, expr) = if let Expr::Lambda { pointers, expr } = &*expr { (pointers, expr.clone()) } else { unreachable!() };
	let (pointer_3, expr) = if let Expr::Lambda { pointers, expr } = &*expr { (pointers, expr.clone()) } else { unreachable!() };
	let (pointer_4, _expr) = if let Expr::Lambda { pointers, expr }  = &*expr { (pointers, expr.clone()) } else { unreachable!() };

	let index_1 = r.push_lambda_pointer(pointer_1, db).unwrap();
	println!("add [{}] = [{}]", pointer_1.display(db), r.display(db));

	let index_2 = r.push_lambda_pointer(pointer_2, db).unwrap();
	println!("add [{}] = [{}]", pointer_2.display(db), r.display(db));

	let index_3 = r.push_lambda_pointer(pointer_3, db).unwrap();
	println!("add [{}] = [{}]", pointer_3.display(db), r.display(db));

	let index_4 = r.push_lambda_pointer(pointer_4, db).unwrap();
	println!("add [{}] = [{}]", pointer_4.display(db), r.display(db));

	let r_before = r.clone();
	let (left, right) = r.split(db).unwrap();
	r.join_into(left, right, db);
	assert_eq!(r_before.0, r.0);
	assert_eq!(r_before.1, r.1);

	let pointer_4_pop = r.pop_lambda_pointer(index_4, db).unwrap();
	println!("pop {} [{}] = [{}]", index_4, r.display(db), pointer_4_pop.display(db));
	assert_eq!(pointer_4, &pointer_4_pop);

	let pointer_3_pop = r.pop_lambda_pointer(index_3, db).unwrap();
	println!("pop {} [{}] = [{}]", index_3, r.display(db), pointer_3_pop.display(db));
	assert_eq!(pointer_3, &pointer_3_pop);

	let pointer_2_pop = r.pop_lambda_pointer(index_2, db).unwrap();
	println!("pop {} [{}] = [{}]", index_2, r.display(db), pointer_2_pop.display(db));
	assert_eq!(pointer_2, &pointer_2_pop);

	let pointer_1_pop = r.pop_lambda_pointer(index_1, db).unwrap();
	println!("pop {} [{}] = [{}]", index_1, r.display(db), pointer_1_pop.display(db));
	assert_eq!(pointer_1, &pointer_1_pop);

	// Test Split & Join
	let r = &mut ReduceTree(PointerTree::None.store(db), 0);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::End(0).store(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::End(1).store(db), 0);
	test_split(r, db).unwrap_err();

	let r = &mut ReduceTree(PointerTree::Both(PointerTree::None.store(db), PointerTree::None.store(db)).store(db), 0);
	// test_split(r, db).unwrap(); // This will error

	let r = &mut ReduceTree(PointerTree::Both(PointerTree::End(1).store(db), PointerTree::None.store(db)).store(db), 1);
	test_split(r, db).unwrap();

	let r = &mut ReduceTree(PointerTree::Both(PointerTree::Both(PointerTree::None.store(db), PointerTree::End(2).store(db)).store(db), PointerTree::End(2).store(db)).store(db), 2);
	test_split(r, db).unwrap();
}

fn test_split(r: &mut ReduceTree, db: &mut Datastore) -> Result<(), LambdaError> {
	let r_before = r.clone();
	print!("split [{}] ", r.display(db));
	let (left, right) = r.split(db).map_err(|e|{println!("err"); e})?;
	print!("([{}] [{}])", left.display(db), right.display(db));
	r.join_into(left, right, db);
	println!(" = [{}]", r.display(db));
	assert_eq!(r_before.0, r.0);
	assert_eq!(r_before.1, r.1);
	Ok(())
}

// Beta reduces expression without rehashing
fn partial_beta_reduce(reducing_expr: Link<Expr>, reducing_tree: &mut ReduceTree, depth: usize, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	if depth > 200 { return Err(LambdaError::RecursionDepthExceeded) }
	let depth = depth + 1;

	struct DisplayIter<T: fmt::Display, I: Iterator<Item=T> + Clone>(I);
	impl<T: fmt::Display, I: Iterator<Item=T> + Clone> fmt::Display for DisplayIter<T, I> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			for string in self.0.clone() { write!(f, "{}", string)?; }; Ok(())
		}
	}
	let pad = DisplayIter(std::iter::repeat("    ").take(depth));

	let ret = match reducing_expr.as_ref() {
		Expr::Variable => {
			println!("{}[{}] reducing var {} -------- [{}]", pad, depth, reducing_expr.display(db), reducing_tree.display(db));
			reducing_expr
		},
		Expr::Lambda { expr, pointers } => {
			print!("{}[{}] reducing lam {} -------- [{}] -> ", pad, depth, Expr::Lambda { pointers: pointers.clone(), expr: expr.clone() }.display(db), reducing_tree.display(db));

			// Add pointers to pointer_trees so that if reduction moves variables, pointers can be updated
			let index = reducing_tree.push_lambda_pointer(&pointers, db)?;
			println!("[{}]", reducing_tree.display(db));

			let reduced_expr = partial_beta_reduce(expr.clone(), reducing_tree, depth, db)?;
			println!("{}					[{}] -> ", pad, reducing_tree.display(db));

			Expr::Lambda {
				pointers: reducing_tree.pop_lambda_pointer(index, db)?,
				expr: reduced_expr
			}.store(db)
		}
		Expr::Application { function, substitution } => {
			println!("{}[{}] reducing app {} -------- [{}]", pad, depth, Expr::Application { function: function.clone(), substitution: substitution.clone() }.display(db), reducing_tree.display(db));
			
			// Descend to subtrees
			let (mut function_tree, mut substitution_tree) = reducing_tree.split(db)?;

			let function_reduced = partial_beta_reduce(function.clone(), &mut function_tree, depth, db)?;

			let ret = match function_reduced.as_ref() {
				Expr::Variable => {
					let substitution = partial_beta_reduce(substitution.clone(), &mut substitution_tree, depth, db)?;
					
					reducing_tree.join_into(function_tree, substitution_tree, db);

					Expr::Application {
						function: Expr::Variable.fetch(db)?,
						substitution,
					}.store(db)
				},
				Expr::Lambda { pointers, expr } => {
					*reducing_tree = function_tree.clone();
					let index = reducing_tree.push_lambda_pointer(&pointers, db)?;
					reducing_tree.1 -= 1; // Needed because all PointerTree::Ends should be replaced in recur_replace

					println!("{}[{}] replace in {}: every {} in [{}] with {}: [{}] → ", pad, depth, expr.display(db), index, reducing_tree.display(db), substitution.display(db), substitution_tree.display(db));
					let replaced_form = if let Some(_) = &pointers {
						recur_replace(expr.clone(), reducing_tree, index, substitution.clone(), &substitution_tree, db)?
					} else {
						expr.clone()
					};
					println!("{}{}: [{}]", pad, replaced_form.display(db), reducing_tree.display(db));

					//println!("{}Replaced every {} with [{}] in [{}] -> [{}]", pad, index, substitution_tree.display(db), function_tree.display(db), reducing_tree.display(db));

					partial_beta_reduce(replaced_form, reducing_tree, depth, db)?
				},
				Expr::Application { function: _, substitution: _ } => {
					let reduced_substitution = partial_beta_reduce(substitution.clone(), &mut substitution_tree, depth, db)?;
					
					reducing_tree.join_into(function_tree, substitution_tree, db);
					
					Expr::Application { function: function_reduced, substitution: reduced_substitution }.store(db)
				}
			};
			ret
		}
	};
	println!("{}[{}] returning reduction: {} -------- [{}]", pad, depth, ret.display(db), reducing_tree.display(db));
	Ok(ret)
}

pub fn beta_reduce(expr: &Link<Expr>, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	println!("Reducing: {}", expr.display(db));
	Ok(partial_beta_reduce(expr.clone(), &mut ReduceTree::new(), 0, db)?)
}