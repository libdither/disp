#![allow(unused)]

#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(try_blocks)]
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
use parse::{parse_to_expr, parse_reduce};

fn setup_boolean_logic(db: &mut Datastore) -> (TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>, TypedHash<Expr>) {
	let variable = Expr::var(db);

	let id_hash = Expr::lambda(end(0, db), 0, &variable, db); // Replace first variable with 0 index with applied value

	let true_hash = Expr::lambda(end(1, db), 1, &variable, db); // Replace 1 and then ignore application

	let false_hash = beta_reduce(&Expr::app(&true_hash, &id_hash, db), db).unwrap(); // (true id) β> false

	let not_hash = Expr::lambda(left(left(end(0, db), db), db), 0, 
		&Expr::app(&Expr::app(&variable, &false_hash, db), &true_hash, db), db
	);

	let and_hash = Expr::lambda(left(both(end(1, db), end(0, db), db), db), 1,
		&Expr::app(
			&Expr::app(&variable, &variable, db),
			&false_hash, db
		), db, 
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

	let parsed_id_hash = parse_to_expr("(λ[.] x)", db).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse_to_expr("(λ[.] (λ[] x))", db).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse_to_expr("(λ[] (λ[.] x))", db).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse_to_expr("(λ[<<.] ((x (λ[] (λ[.] x))) (λ[.] (λ[] x))))", db).unwrap();
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse_to_expr("(λ[<<.] (λ[<>.] ((x x) (λ[] (λ[.] x)))))", db).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	let db = &mut Datastore::new();

	let zero = parse_to_expr("(λ[] (λ[.] x))", db).unwrap();
	Symbol::new("zero", &zero, db);

	let one = parse_to_expr("(λ[<.] (λ[>.] (x x)))", db).unwrap();

	let succ = parse_to_expr("(λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x)))))", db).unwrap();
	Symbol::new("succ", &succ, db);

	// Succ Zero = One
	let succ_zero = Expr::app(&succ, &zero, db);
	assert_eq!(beta_reduce(&succ_zero, db).unwrap(), one);

	let succ_one = beta_reduce(&Expr::app(&succ, &one, db), db).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(&Expr::app(&succ, &succ_zero, db), db).unwrap(), succ_one);

	let mult = parse_to_expr("(λ[<<.] (λ[<><.] (λ[<>>.] (λ[>.] ((x (x x)) x)))))", db).unwrap();
	Symbol::new("mult", &mult, db);
	
	println!("{}", parse_to_expr("((mult 2) 3)", db).unwrap().display(db));
	assert_eq!(parse_reduce("((mult 0) 0)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 0) 1)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 2) 3)", db).unwrap(), parse_reduce("6", db).unwrap());
	
	let y_segment = parse_to_expr("(λ[><<(.,.)] (λ[(.,<>.)] (x (λ[>.] (((x x) x) x)))))", db).unwrap();
	let y = Expr::app(&y_segment, &y_segment, db);

}