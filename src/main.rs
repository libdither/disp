#![feature(iter_intersperse)]
#![feature(option_result_contains)]

use bitvec::prelude::*;

use hashdb::*;

mod lambda_calculus;

use crate::lambda_calculus::{Expr, VARIABLE, beta_reduce, parse_hash, parse_to_expr};

fn main() {
	let db = &mut Datastore::new();
	
}
