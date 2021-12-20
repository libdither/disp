use std::fmt;

use lazy_static::lazy_static;
use thiserror::Error;
use bitvec::prelude::*;
use bytes::{Buf, BufMut};

use hashdb::{Data, Datastore, Hash};

mod parse;
pub use parse::parse_to_expr;

lazy_static! {
	pub static ref VARIABLE:     Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	pub static ref LAMBDA:      Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
	pub static ref APPLICATION: Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);
}

#[derive(Error, Debug)]
pub enum LambdaError {
	// Data -> Hash -> Data interpretation
	#[error("Invalid hashtype, got {0}")]
	InvalidLayout(Hash),
	#[error("Can't convert Variable variant to data, there is no data to serialize")]
	NoDataForVariable,
	#[error("Error in bincode")]
	SerdeError(#[from] bincode::Error),

	// Datastore Error
	#[error("Not in datastore: {0}")]
	NotInDatastore(Hash),

	// Beta Reduction Error
	#[error("Recursion Depth for beta reduction exceeded")]
	RecursionDepthExceeded,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
	Variable,
	Lambda(Hash, Vec<BitVec>),
	Application(Hash, Hash),
}

impl Expr {
	pub fn lambda(pointers: Vec<BitVec>, expr: &Hash, db: &mut Datastore) -> Hash {
		Expr::Lambda(expr.clone(), pointers).to_hash(db)
	}
	pub fn app(lambda: &Hash, expr: &Hash, db: &mut Datastore) -> Hash {
		Expr::Application(lambda.clone(), expr.clone()).to_hash(db)
	}

	pub fn to_hash(&self, db: &mut Datastore) -> Hash {
		match self {
			Expr::Variable => VARIABLE.clone(),
			_ => db.add(self.to_data().expect("unreachable: Expr::to_data errors on Expr::Variable which is separately matched"))
		}
	}
	pub fn to_data(&self) -> Result<Data, LambdaError> {
		match self {
			Expr::Variable => Err(LambdaError::NoDataForVariable),
			Expr::Lambda(expr, pointers) => {
				let mut buf = Vec::new();
				buf.put(LAMBDA.as_bytes());
				buf.put(expr.as_bytes());
				
				bincode::serialize_into(&mut buf, pointers)?;
			
				Ok(Data::from_vec(buf))
			}
			Expr::Application(lambda, expr) => {
				let mut buf = Vec::new();
				buf.put(APPLICATION.as_bytes());
				buf.put(lambda.as_bytes());
				buf.put(expr.as_bytes());
				Ok(Data::from_vec(buf))
			}
		}
	}
	pub fn from_hash(hash: &Hash, db: &mut Datastore) -> Result<Self, LambdaError> {
		if *hash == *VARIABLE { Ok(Expr::Variable) }
		else { Self::from_data( db.get(&hash).ok_or(LambdaError::NotInDatastore(hash.clone()))? ) }
	}
	pub fn from_data(data: &Data) -> Result<Self, LambdaError> {
		let mut reader = data.as_bytes();
		let layout = &reader.copy_to_bytes(LAMBDA.len())[..];
		Ok(match layout {
			_ if layout == LAMBDA.as_bytes() => {
				let expr = Hash::from_raw_bytes(&reader.copy_to_bytes(LAMBDA.len())[..]);
				let pointers = bincode::deserialize_from(reader)?;
				
				Expr::Lambda(expr, pointers)
			},
			_ if layout == APPLICATION.as_bytes() => {
				Expr::Application(
					Hash::from_raw_bytes(&reader.copy_to_bytes(LAMBDA.len())[..]),
					Hash::from_raw_bytes(&reader.copy_to_bytes(LAMBDA.len())[..])
				)
			},
			_ => Err(LambdaError::InvalidLayout(Hash::from_raw_bytes(layout)))?
		})
	}
}

// TODO: Make this better
impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}
pub fn print_expr(hash: &Hash, db: &mut Datastore) -> Result<(), LambdaError> {
	Ok(println!("{}", format_expr(&Expr::from_hash(hash, db)?, db)?))
}
pub fn format_expr(expr: &Expr, db: &mut Datastore) -> Result<String, LambdaError> {
	Ok(match expr {
		Expr::Variable => "x".to_owned(),
		Expr::Lambda(hash, pointers) => {
			//use itertools::Itertools;
			let bool_array = pointers.iter().map(|s|{
				s.iter().by_val().map(|b|if b {'1'} else {'0'}).collect::<String>()
			}).intersperse(",".to_owned()).collect::<String>();
			format!("(λ[{}] {})", bool_array, format_expr(&Expr::from_hash(&hash, db)?, db)?)
		},
		Expr::Application(lambda, expr) => {
			let lambda_string = format_expr(&Expr::from_hash(lambda, db)?, db)?;
			let expr_string = format_expr(&Expr::from_hash(expr, db)?, db)?;
			format!("({} {})", lambda_string, expr_string)
		},
	})
}

pub fn parse_hash(hash: &Hash, db: &mut Datastore) -> Result<String, LambdaError> {
	format_expr(&Expr::from_hash(hash, db)?, db)
}

/// Takes lambda expression and traverses down the binary tree using bitslices, replacing variables & rehashing.
fn recur_replace(replace_in_expr: Expr, pointers: Vec<&BitSlice<Lsb0>>, replacement: Expr, db: &mut Datastore) -> Result<Expr, LambdaError> {
	if pointers.len() == 0 { return Ok(replace_in_expr) }

	let going_left = &mut false;
	let going_right = &mut false;

	match replace_in_expr {
		// When encounter a variable, replace
		Expr::Variable => {
			Ok(replacement)
		},
		// When encounter a lambda, skip
		Expr::Lambda(expr, ptrs) => {
			let pointers = pointers.into_iter().filter_map(|pointer| {
				// Check first bit
				*going_left |= !pointer[0];
				*going_right |= pointer[0];
				// If more than 1 bit left, return slice of bitslice
				if pointer.len() > 1 {
					Some(&pointer[1..])
				} else { None }
			}).collect::<Vec<&BitSlice<Lsb0>>>();

			let sub_replace_in_expr = Expr::from_hash(&expr, db)?;
			Ok(Expr::Lambda(recur_replace(sub_replace_in_expr, pointers, replacement, db)?.to_hash(db), ptrs))
		},
		// When encounter a function application, check if any of the bit arrays want to go down each path, then go down those paths.
		Expr::Application(lambda, expr) => {
			// Check if any of the bit arrays are 0 or 1
			let new_vec = pointers.into_iter().filter_map(|pointer| {
				// Check first bit
				*going_left |= !pointer[0];
				*going_right |= pointer[0];
				// If more than 1 bit left, return slice of bitslice
				if pointer.len() > 1 {
					Some(&pointer[1..])
				} else { None }
			}).collect::<Vec<&BitSlice<Lsb0>>>();

			let left_replace = if *going_left {
				Some(recur_replace(Expr::from_hash(&lambda, db)?, new_vec.clone(), replacement.clone(), db)?.to_hash(db))
			} else { None };

			let right_replace = if *going_right {
				Some(recur_replace(Expr::from_hash(&expr, db)?, new_vec, replacement.clone(), db)?.to_hash(db))
			} else { None };

			let lambda = left_replace.unwrap_or(lambda);
			let expr = right_replace.unwrap_or(expr);

			Ok(Expr::Application(lambda, expr))
		}
	}
}

// Beta reduces expression without rehashing
pub fn partial_beta_reduce(reducing_expr: Expr, max_depth: usize, db: &mut Datastore) -> Result<Expr, LambdaError> {
	if max_depth == 0 { return Err(LambdaError::RecursionDepthExceeded) }
	let max_depth = max_depth - 1;

	Ok(match reducing_expr {
		Expr::Variable => reducing_expr,
		Expr::Lambda(expr, pointers) => {
			// IMPORTANT: Lambda exprs shouldn't be able to reduce far enough to change pointed to variables (i.e. pointers shouldn't point to children of any lambdas which are in a function application)
			Expr::Lambda(beta_reduce(&expr, db)?, pointers)
		}
		Expr::Application(lambda_hash, replace_hash) => {
			let lambda_reduced = partial_beta_reduce(Expr::from_hash(&lambda_hash, db)?, max_depth, db)?;
			match lambda_reduced {
				Expr::Variable => Expr::Application(VARIABLE.clone(), beta_reduce(&replace_hash, db)?),
				Expr::Lambda(expr_hash, pointers) => {
					let expr = Expr::from_hash(&expr_hash, db)?;
					// Convert BitVecs to BitSlices for recur_replace
					let pointers = pointers.iter().map(|s|s.as_bitslice()).collect::<Vec<&BitSlice<Lsb0>>>();
					let replacement = Expr::from_hash(&replace_hash, db)?;

					// Debug
					let bool_array = pointers.iter().map(|s|{
						s.iter().by_val().map(|b|if b {'1'} else {'0'}).collect::<String>()
					}).intersperse(", ".to_owned()).collect::<String>();
					print!("replace in: {} at [{}] with {}", format_expr(&expr, db)?, bool_array, format_expr(&replacement, db)?);
					let replaced_form = recur_replace(expr, pointers, replacement, db)?;
					println!(" = {}", format_expr(&replaced_form, db)?);

					partial_beta_reduce(replaced_form, max_depth, db)?
				},
				Expr::Application(_, _) => {
					Expr::Application(lambda_reduced.to_hash(db), beta_reduce(&replace_hash, db)?)
				}
			}
		}
	})
}

pub fn beta_reduce(hash: &Hash, db: &mut Datastore) -> Result<Hash, LambdaError> {
	Ok(partial_beta_reduce(Expr::from_hash(hash, db)?, 20, db)?.to_hash(db))
}