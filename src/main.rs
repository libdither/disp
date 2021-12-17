#![feature(iter_intersperse)]

use bitvec::prelude::*;

use hashdb::*;

mod lambda_calculus;

use crate::lambda_calculus::{Expr, VARIABLE, beta_reduce, parse_hash, parse_to_expr};

fn main() {
	let db = &mut Datastore::new();

	//let data = Data::new("Hello, World!\n".as_bytes());
	// Identity Function
	let id_hash = Expr::lambda(vec![bitvec![0]], &VARIABLE, db);
	println!("I = {}", parse_hash(&id_hash, db).unwrap());

	// Apply identity function to itself
	let ii_app_hash = Expr::app(&id_hash, &id_hash, db);
	println!("(I I) = {}", parse_hash(&ii_app_hash, db).unwrap());

	let ii_hash = beta_reduce(&ii_app_hash, db).unwrap();
	println!("(I I) = I = {}", parse_hash(&ii_hash, db).unwrap());

	assert_eq!(id_hash, ii_hash);

	let id_hash_2 = parse_to_expr("(λ[0] x)", db).unwrap().to_hash(db);
	println!("I_v2 = {}", parse_hash(&id_hash_2, db).unwrap());
	assert_eq!(id_hash, id_hash_2);

	let k_hash = Expr::lambda( vec![], &Expr::lambda(vec![bitvec![0, 0]], &VARIABLE, db), db);
	println!("K = {}", parse_hash(&k_hash, db).unwrap());
	
	let ki_app_hash = Expr::app(&k_hash, &id_hash, db);
	println!("(K I) = {}", parse_hash(&ki_app_hash, db).unwrap());

	let ki_hash = beta_reduce(&ki_app_hash, db).unwrap();
	println!("(K I) = KI = {}", parse_hash(&ki_hash, db).unwrap());

	let false_hash = ki_hash.clone();
	let true_hash = k_hash.clone();

	let not_hash = Expr::lambda(vec![bitvec![0, 0, 0]], 
		&Expr::app(&Expr::app(&VARIABLE, &false_hash, db),&true_hash, db), db
	);

	println!("(λ[000] ((x KI) K)) = Not = {}", parse_hash(&not_hash, db).unwrap());
	let reduced_not_hash = beta_reduce(&not_hash, db).unwrap();
	assert_eq!(not_hash, reduced_not_hash);

	let not_true_app_hash = Expr::app(&not_hash, &true_hash, db);
	println!("(Not True) = {}", parse_hash(&not_true_app_hash, db).unwrap());
	let not_true_hash = beta_reduce(&not_true_app_hash, db).unwrap();
	println!("(Not True) = {}", parse_hash(&not_true_hash, db).unwrap());
	assert_eq!(not_true_hash, false_hash);

	let and_hash = Expr::lambda(vec![bitvec![0, 0, 0, 0]],
		&Expr::lambda(vec![bitvec![0, 0, 1]],
			&Expr::app(
				&Expr::app(&VARIABLE, &VARIABLE, db),
				&false_hash, db
			), db, 
		), db
	);
	println!("And = (λ[0000] (λ[001] ((x x) False))) = {}", parse_hash(&and_hash, db).unwrap());

	let and_true_true_app_hash = Expr::app(&Expr::app(&and_hash, &true_hash, db), &true_hash, db);

	let and_true_true_hash = beta_reduce(&and_true_true_app_hash, db).unwrap();

	println!("And True True = {}", parse_hash(&and_true_true_hash, db).unwrap());
	assert_eq!(and_true_true_hash, true_hash);
}
