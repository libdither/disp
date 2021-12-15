#![feature(iter_intersperse)]

use std::{hash, io::Read};
use bitvec::prelude::*;

use hashdb::*;

use crate::lambda_calculus::{APPLICATION, Expr, LAMBDA, VARIABLE, beta_reduce, format_expr, parse_hash, print_expr};

mod lambda_calculus;

fn main() {
	let db = &mut Datastore::new();

	//let data = Data::new("Hello, World!\n".as_bytes());
	// Identity Function
	let id_hash = Expr::Lambda(VARIABLE.clone(), vec![bitvec![0]]).to_hash(db);
	println!("I = {}", parse_hash(&id_hash, db).unwrap());

	// Apply identity function to itself
	let ii_app_hash = Expr::Application(id_hash.clone(), id_hash.clone()).to_hash(db);
	println!("(I I) = {}", parse_hash(&ii_app_hash, db).unwrap());

	let ii_hash = beta_reduce(&ii_app_hash, db).unwrap();
	println!("(I I) = I = {}", parse_hash(&ii_hash, db).unwrap());

	assert_eq!(id_hash, ii_hash);

	let k_hash = Expr::Lambda(Expr::Lambda(VARIABLE.clone(), vec![]).to_hash(db), vec![bitvec![0, 0]]).to_hash(db);
	println!("K = {}", parse_hash(&k_hash, db).unwrap());
	
	let ki_app_hash = Expr::Application(k_hash.clone(), id_hash.clone()).to_hash(db);
	println!("(K I) = {}", parse_hash(&ki_app_hash, db).unwrap());

	let ki_hash = beta_reduce(&ki_app_hash, db).unwrap();
	println!("(K I) = KI = {}", parse_hash(&ki_hash, db).unwrap());

	let false_hash = ki_hash.clone();
	let true_hash = k_hash.clone();

	let not_hash = Expr::Lambda(
		Expr::Application(
			Expr::Application(VARIABLE.clone(), false_hash.clone()).to_hash(db),
			true_hash.clone()
		).to_hash(db), vec![bitvec![0, 0, 0]]
	).to_hash(db);
	println!("(λ[000] ((x KI) K)) = Not = {}", parse_hash(&not_hash, db).unwrap());
	let reduced_not_hash = beta_reduce(&not_hash, db).unwrap();
	assert_eq!(not_hash, reduced_not_hash);

	let not_true_app_hash = Expr::Application(not_hash, true_hash).to_hash(db);
	println!("(Not True) = {}", parse_hash(&not_true_app_hash, db).unwrap());
	let not_true_hash = beta_reduce(&not_true_app_hash, db).unwrap();
	println!("(Not True) = {}", parse_hash(&not_true_hash, db).unwrap());

	assert_eq!(not_true_hash, false_hash);
}
