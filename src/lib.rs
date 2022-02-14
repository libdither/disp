#![allow(unused)]

#![feature(generic_associated_types)]

/// Built-in Types for Dither, recognized by the program as a multihash with the digest set to an arbitrary number
/// Multihash type - 0 as Multihash
/// Type type - 1 as Multihash
/// 	Interpreted as <multiformat's unsigned varint><list of hash digests where the hash type and length are defined by the multihash linking type>
/// Bit type - 2 as Multihash
/// Basic types
/// Byte type - <8><Bit><Bit><Bit><Bit><Bit><Bit><Bit><Bit> as Type
/// Dyte type - <2><Byte><Byte> as Type
/// Qyte type - <4><Byte><Byte><Byte><Byte> as Type
/// Eyte type - <8><Byte><Byte><Byte><Byte><Byte><Byte><Byte><Byte> as Type

pub use hashdb::{Data, Datastore, TypedHash};

pub mod lambda_calculus;
pub mod symbol;
pub mod parse;

use lambda_calculus::{Expr, pointer_helpers::{end, left, right, both, none}, beta_reduce, DisplayWithDatastore};
use symbol::Symbol;
use parse::{parse_expr, parse_reduce};

fn setup_boolean_logic(db: &mut Datastore) -> (TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>) {
	let variable = Expr::var(db);

	let id_hash = Expr::lambda(end(db), &variable, db);

	let true_hash = Expr::lambda(end(db), &Expr::lambda(none(db), &variable, db), db);

	let false_hash = beta_reduce(&Expr::app(&true_hash, &id_hash, db), db).unwrap();

	let not_hash = Expr::lambda(left(left(end(db), db), db), 
		&Expr::app(&Expr::app(&variable, &false_hash, db), &true_hash, db), db
	);

	let and_hash = Expr::lambda(left(left(end(db), db), db),
		&Expr::lambda(left(right(end(db), db), db),
			&Expr::app(
				&Expr::app(&variable, &variable, db),
				&false_hash, db
			), db, 
		), db
	);

	(id_hash, true_hash, false_hash, not_hash, and_hash)
}

#[test]
fn test_reduce() {
	let db = &mut Datastore::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(db);

	let ii_hash = beta_reduce(&Expr::app(&id_hash, &id_hash, db), db).unwrap();
	assert_eq!(id_hash, ii_hash);

	assert_eq!(beta_reduce(&Expr::app(&not_hash, &false_hash, db), db).unwrap(), true_hash);
	assert_eq!(beta_reduce(&Expr::app(&not_hash, &true_hash, db), db).unwrap(), false_hash);

	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &true_hash, db), &true_hash, db), db).unwrap(), true_hash);
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &true_hash, db), &false_hash, db), db).unwrap(), false_hash);
	println!("and false true = false");
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &false_hash, db), &true_hash, db), db).unwrap(), false_hash);
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &false_hash, db), &false_hash, db), db).unwrap(), false_hash);
}

#[test]
fn test_parsing() {
	let db = &mut Datastore::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(db);

	let parsed_id_hash = parse_expr("(λ0[0] x)", db).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse_expr("(λ1[1] x)", db).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse_expr("(λ1[0] x)", db).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse_expr("(λ0[<<0] ((x (λ1[0] x)) (λ1[1] x)))", db).unwrap();
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse_expr("(λ1[<(1,0)] ((x x) (λ1[0] x)))", db).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	let db = &mut Datastore::new();

	let zero = parse_expr("(λ1[0] x)", db).unwrap();
	Symbol::new("zero", &zero, db);

	let one = parse_expr("(λ1[(1,0)] (x x))", db).unwrap();

	let succ = parse_expr("(λ2[(1,((2,1), 0))] (x ((x x) x)))", db).unwrap();
	Symbol::new("succ", &succ, db);

	// Succ Zero = One
	let succ_zero = beta_reduce(&Expr::app(&succ, &zero, db), db).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = beta_reduce(&Expr::app(&succ, &one, db), db).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(&Expr::app(&succ, &succ_zero, db), db).unwrap(), succ_one);

	let mult = parse_reduce("(λ[<<.] (λ[<><.] (λ[<>>.] (λ[>.] ((x (x x)) x)))))", db).unwrap();
	Symbol::new("mult", &mult, db);
	
	println!("{}", parse_expr("((mult 2) 3)", db).unwrap().display(db));
	assert_eq!(parse_reduce("((mult 0) 0)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 0) 1)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 2) 3)", db).unwrap(), parse_reduce("6", db).unwrap());
	
	let y_segment = parse_expr("(λ[><<(.,.)] (λ[(.,<>.)] (x (λ[>.] (((x x) x) x)))))", db).unwrap();
	let y = Expr::app(&y_segment, &y_segment, db);

	//let is_zero = parse_expr("()")

}