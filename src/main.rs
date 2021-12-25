#![feature(iter_intersperse)]
#![feature(option_result_contains)]

use hashdb::*;

mod lambda_calculus;

use crate::lambda_calculus::{Expr, VARIABLE, beta_reduce, parse_to_expr};

fn main() {
	let db = &mut Datastore::new();
	
}
