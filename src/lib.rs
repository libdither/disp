#![allow(unused)]

#![feature(generic_associated_types)]

use hashdb::NativeHashtype;
pub use hashdb::{Data, Datastore, TypedHash};

pub mod lambda_calculus;
pub mod symbol;
pub mod parse;

use lambda_calculus::{Expr, pointer_helpers::{end, left, right, both, none}, beta_reduce, DisplayWithDatastore};
use symbol::Symbol;
use parse::{parse, parse_reduce};

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

	let parsed_id_hash = parse("(λ1[1] x)", db).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse("(λ2[2] x)", db).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse("(λ2[1] x)", db).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse("(λ1[<<1] ((x (λ2[1] x)) (λ2[2] x)))", db).unwrap();
	println!("not: {} vs {}", parsed_not_hash.display(db), not_hash.display(db));
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse("(λ2[<(2,1)] ((x x) (λ2[1] x)))", db).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	let db = &mut Datastore::new();

	let zero = parse("(λ2[1] x)", db).unwrap();
	Symbol::new("zero", &zero, db);

	let one = parse("(λ2[(2,1)] (x x))", db).unwrap();

	let succ = parse("(λ3[(2,((3,2), 1))] (x ((x x) x)))", db).unwrap();
	Symbol::new("succ", &succ, db);

	// Succ Zero = One
	let succ_zero = beta_reduce(&Expr::app(&succ, &zero, db), db).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = beta_reduce(&Expr::app(&succ, &one, db), db).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(&Expr::app(&succ, &succ_zero, db), db).unwrap(), succ_one);

	let mult = parse_reduce("(λ3[(3,(2,1))] (x (x x)))", db).unwrap();
	Symbol::new("mult", &mult, db);

	println!("{}", parse("((mult 2) 3)", db).unwrap().display(db));
	assert_eq!(parse_reduce("((mult 0) 0)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 0) 1)", db).unwrap(), parse_reduce("0", db).unwrap());
	assert_eq!(parse_reduce("((mult 2) 3)", db).unwrap(), parse_reduce("6", db).unwrap());

	let pred = parse_reduce("(λ3[<((3,>>2),1)] (((x (λ2[(1,<2)] (x (x x)))) (λ1[N] x)) (λ1[1] x)))", db).unwrap();
	Symbol::new("pred", &pred, db);

	let iszero = parse_reduce("(λ1[<<1] ((x (λ3[1] x)) (λ2[2] x)))", db).unwrap();
	Symbol::new("iszero", &iszero, db);

	let pair = parse_reduce("(λ3[((1,3),2)] ((x x) x))", db).unwrap();
	Symbol::new("pair", &pair, db);
	Symbol::new("first", &zero, db);

	let fact_seg = parse::parse_line("set factseg λ2[(>>2,(>2,1))] pair (succ x) (mult x x)", db).unwrap();
	let fact = parse("λ1[<<<1] x (λ1[<1] x factseg) (pair 1 1) first", db).unwrap();
	Symbol::new("factorial", &fact, db);

	assert_eq!(parse_reduce("(factorial 2)", db).unwrap(), parse_reduce("2", db).unwrap());
	assert_eq!(parse_reduce("(factorial 4)", db).unwrap(), parse_reduce("24", db).unwrap());

	//let is_zero = parse("()")

}

#[test]
fn test_hashdb() {
	let db = &mut Datastore::new();
	let ser = &mut db.serializer();
	//let data = Data::new(&[01u8, 32u8]);
	let string = String::from("Hello").store(ser);
	assert_eq!(*string.fetch(ser.db).unwrap(), "hello");
}