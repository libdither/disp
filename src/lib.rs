#![allow(unused)]

#![feature(generic_associated_types)]

use hashdb::{LinkArena, LinkSerializer, NativeHashtype};
pub use hashdb::{Data, Datastore};

pub mod expr;
pub mod symbol;
pub mod parse;

use expr::{Expr, Binding as PT, beta_reduce};
use symbol::Symbol;
use parse::{parse, parse_reduce};

fn setup_boolean_logic<'a>(exprs: &'a LinkArena<'a>) -> (&'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>) {
	let id_hash = Expr::lambda(PT::END, &Expr::VAR, exprs);

	let true_hash = Expr::lambda(PT::END, &Expr::lambda(PT::NONE, &Expr::VAR, exprs), exprs);

	let false_hash = beta_reduce(&Expr::app(&true_hash, &id_hash, exprs), exprs).unwrap();

	let not_hash = Expr::lambda(PT::left(PT::left(PT::END, exprs), exprs), 
		&Expr::app(&Expr::app(&Expr::VAR, &false_hash, exprs), &true_hash, exprs), exprs
	);

	let and_hash = Expr::lambda(PT::left(PT::left(PT::END, exprs), exprs),
		&Expr::lambda(PT::left(PT::right(PT::END, exprs), exprs),
			&Expr::app(
				&Expr::app(&Expr::VAR, &Expr::VAR, exprs),
				&false_hash, exprs
			), exprs, 
		), exprs
	);

	(id_hash, true_hash, false_hash, not_hash, and_hash)
}

#[test]
fn test_reduce() {
	let exprs = &LinkArena::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(exprs);

	let ii_hash = beta_reduce(&Expr::app(&id_hash, &id_hash, exprs), exprs).unwrap();
	assert_eq!(id_hash, ii_hash);

	assert_eq!(beta_reduce(&Expr::app(&not_hash, &false_hash, exprs), exprs).unwrap(), true_hash);
	assert_eq!(beta_reduce(&Expr::app(&not_hash, &true_hash, exprs), exprs).unwrap(), false_hash);

	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &true_hash, exprs), &true_hash, exprs), exprs).unwrap(), true_hash);
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &true_hash, exprs), &false_hash, exprs), exprs).unwrap(), false_hash);
	println!("and false true = false");
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &false_hash, exprs), &true_hash, exprs), exprs).unwrap(), false_hash);
	assert_eq!(beta_reduce(&Expr::app(&Expr::app(&and_hash, &false_hash, exprs), &false_hash, exprs), exprs).unwrap(), false_hash);
}

#[test]
fn test_parsing() {
	let exprs = &LinkArena::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(exprs);

	let parsed_id_hash = parse("(λ1[1] x)", exprs).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse("(λ2[2] x)", exprs).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse("(λ2[1] x)", exprs).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse("(λ1[<<1] ((x (λ2[1] x)) (λ2[2] x)))", exprs).unwrap();
	println!("not: {} vs {}", parsed_not_hash, not_hash);
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse("(λ2[<(2,1)] ((x x) (λ2[1] x)))", exprs).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	let exprs = &LinkArena::new();

	let zero = parse("(λ2[1] x)", exprs).unwrap();
	Symbol::new("zero", &zero, exprs);

	let one = parse("(λ2[(2,1)] (x x))", exprs).unwrap();

	let succ = parse("(λ3[(2,((3,2), 1))] (x ((x x) x)))", exprs).unwrap();
	Symbol::new("succ", &succ, exprs);

	// Succ Zero = One
	let succ_zero = beta_reduce(&Expr::app(&succ, &zero, exprs), exprs).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = beta_reduce(&Expr::app(&succ, &one, exprs), exprs).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(&Expr::app(&succ, &succ_zero, exprs), exprs).unwrap(), succ_one);

	let mult = parse_reduce("(λ3[(3,(2,1))] (x (x x)))", exprs).unwrap();
	Symbol::new("mult", &mult, exprs);

	println!("{}", parse("((mult 2) 3)", exprs).unwrap());
	assert_eq!(parse_reduce("((mult 0) 0)", exprs).unwrap(), parse_reduce("0", exprs).unwrap());
	assert_eq!(parse_reduce("((mult 0) 1)", exprs).unwrap(), parse_reduce("0", exprs).unwrap());
	assert_eq!(parse_reduce("((mult 2) 3)", exprs).unwrap(), parse_reduce("6", exprs).unwrap());

	let pred = parse_reduce("(λ3[<((3,>>2),1)] (((x (λ2[(1,<2)] (x (x x)))) (λ1[N] x)) (λ1[1] x)))", exprs).unwrap();
	Symbol::new("pred", &pred, exprs);

	let iszero = parse_reduce("(λ1[<<1] ((x (λ3[1] x)) (λ2[2] x)))", exprs).unwrap();
	Symbol::new("iszero", &iszero, exprs);

	let pair = parse_reduce("(λ3[((1,3),2)] ((x x) x))", exprs).unwrap();
	Symbol::new("pair", &pair, exprs);
	Symbol::new("first", &zero, exprs);

	let fact_seg = parse::parse_line("set factseg λ2[(>>2,(>2,1))] pair (succ x) (mult x x)", exprs).unwrap();
	let fact = parse("λ1[<<<1] x (λ1[<1] x factseg) (pair 1 1) first", exprs).unwrap();
	Symbol::new("factorial", &fact, exprs);

	assert_eq!(parse_reduce("(factorial 2)", exprs).unwrap(), parse_reduce("2", exprs).unwrap());
	assert_eq!(parse_reduce("(factorial 4)", exprs).unwrap(), parse_reduce("24", exprs).unwrap());

	//let is_zero = parse("()")

}

#[test]
fn test_hashdb() {
	let exprs = &LinkArena::new();
	let ser = &mut LinkSerializer::new();
	let db = &mut Datastore::new();
	//let data = Data::new(&[01u8, 32u8]);
	let string = String::from("Hello").store(&mut ser.join(db));
	assert_eq!(string.fetch(&db, exprs).unwrap(), "Hello");
}