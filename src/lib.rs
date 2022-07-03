#![allow(dead_code)]

pub use hashdb::{Datastore};
use hashdb::{LinkArena};

pub mod expr;
pub mod name;
mod parse;

use expr::{beta_reduce, Binding as PT, Expr};
pub use parse::{parse, parse_reduce};

fn setup_boolean_logic<'a>(
	exprs: &'a LinkArena<'a>,
) -> (&'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>) {
	let id_hash = Expr::lambda(PT::END, &Expr::VAR, exprs);

	let true_hash = Expr::lambda(PT::END, &Expr::lambda(PT::NONE, &Expr::VAR, exprs), exprs);

	let false_hash = beta_reduce(&Expr::app(&true_hash, &id_hash, exprs), exprs).unwrap();

	let not_hash = Expr::lambda(
		PT::left(PT::left(PT::END, exprs), exprs),
		&Expr::app(&Expr::app(&Expr::VAR, &false_hash, exprs), &true_hash, exprs),
		exprs,
	);

	let and_hash = Expr::lambda(
		PT::left(PT::left(PT::END, exprs), exprs),
		&Expr::lambda(
			PT::left(PT::right(PT::END, exprs), exprs),
			&Expr::app(&Expr::app(&Expr::VAR, &Expr::VAR, exprs), &false_hash, exprs),
			exprs,
		),
		exprs,
	);

	(id_hash, true_hash, false_hash, not_hash, and_hash)
}

#[test]
fn test_reduce() {
	let exprs = &LinkArena::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(exprs);

	let ii_hash = beta_reduce(&Expr::app(&id_hash, &id_hash, exprs), exprs).unwrap();
	assert_eq!(id_hash, ii_hash);

	assert_eq!(
		beta_reduce(&Expr::app(&not_hash, &false_hash, exprs), exprs).unwrap(),
		true_hash
	);
	assert_eq!(
		beta_reduce(&Expr::app(&not_hash, &true_hash, exprs), exprs).unwrap(),
		false_hash
	);

	assert_eq!(
		beta_reduce(
			&Expr::app(&Expr::app(&and_hash, &true_hash, exprs), &true_hash, exprs),
			exprs
		)
		.unwrap(),
		true_hash
	);
	assert_eq!(
		beta_reduce(
			&Expr::app(&Expr::app(&and_hash, &true_hash, exprs), &false_hash, exprs),
			exprs
		)
		.unwrap(),
		false_hash
	);
	println!("and false true = false");
	assert_eq!(
		beta_reduce(
			&Expr::app(&Expr::app(&and_hash, &false_hash, exprs), &true_hash, exprs),
			exprs
		)
		.unwrap(),
		false_hash
	);
	assert_eq!(
		beta_reduce(
			&Expr::app(&Expr::app(&and_hash, &false_hash, exprs), &false_hash, exprs),
			exprs
		)
		.unwrap(),
		false_hash
	);
}

#[test]
fn test_parsing() {
	use crate::name::NamespaceMut;

	let exprs = &LinkArena::new();
	let namespace = &mut NamespaceMut::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(exprs);

	let parsed_id_hash = parse("([x] x)", namespace, exprs).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse("[x y] x", namespace, exprs).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse("[x y] y", namespace, exprs).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse("[x] x ([x y] y) ([x y] x)", namespace, exprs).unwrap();
	println!("not: {} vs {}", parsed_not_hash, not_hash);
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse("[a b] a b ([x y] y)", namespace, exprs).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	use crate::name::NamespaceMut;
	
	let exprs = &LinkArena::new();
	let namespace = &mut NamespaceMut::new();

	let zero = parse("[x y] y", namespace, exprs).unwrap();
	namespace.add("zero", zero, exprs);
	namespace.add("false", zero, exprs);

	let true_expr = parse("[x y] x", namespace, exprs).unwrap();
	namespace.add("true", true_expr, exprs);

	let one = parse("[x y] x y", namespace, exprs).unwrap();

	let succ = parse("[n f x] f (n f x)", namespace, exprs).unwrap();
	namespace.add("succ", succ, exprs);

	// Succ Zero = One
	let succ_zero = beta_reduce(Expr::app(&succ, &zero, exprs), exprs).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = beta_reduce(Expr::app(&succ, &one, exprs), exprs).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(Expr::app(succ, succ_zero, exprs), exprs).unwrap(), succ_one);

	let mult = parse_reduce("[m n f] m (n f)", namespace, exprs).unwrap();
	namespace.add("mult", mult, exprs);

	println!("{}", parse("mult 2 3", namespace, exprs).unwrap());
	assert_eq!(
		parse_reduce("mult 0 0", namespace, exprs).unwrap(),
		parse_reduce("0", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 0 1", namespace, exprs).unwrap(),
		parse_reduce("0", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 2 3", namespace, exprs).unwrap(),
		parse_reduce("6", namespace, exprs).unwrap()
	);

	let pred = parse_reduce("[n f x] n ([g h] h (g f)) ([u] x) ([u] u)", namespace, exprs).unwrap();
	namespace.add("pred", pred, exprs);

	assert_eq!(
		parse_reduce("pred 1", namespace, exprs).unwrap(),
		parse_reduce("0", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("pred 6", namespace, exprs).unwrap(),
		parse_reduce("5", namespace, exprs).unwrap()
	);

	let iszero = parse_reduce("[n] n ([u] [x y] y) ([x y] x)", namespace, exprs).unwrap();
	namespace.add("iszero", iszero, exprs);

	assert_eq!(
		parse_reduce("iszero 0", namespace, exprs).unwrap(),
		parse_reduce("true", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("iszero 6", namespace, exprs).unwrap(),
		parse_reduce("false", namespace, exprs).unwrap()
	);

	let pair = parse_reduce("[x y f] f x y", namespace, exprs).unwrap();
	namespace.add("pair", pair, exprs);
	namespace.add("first", true_expr, exprs);
	namespace.add("second", zero, exprs);

	assert_eq!(
		parse_reduce("(pair 0 1) first", namespace, exprs).unwrap(),
		parse_reduce("0", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("(pair 0 1) second", namespace, exprs).unwrap(),
		parse_reduce("1", namespace, exprs).unwrap()
	);

	let fact_seg = parse("[x y] pair (succ x) (mult x y)", namespace, exprs).unwrap();
	namespace.add("factseg", fact_seg, exprs);

	let fact = parse_reduce("[n] n ([x] x factseg) (pair 1 1) second", namespace, exprs).unwrap();
	namespace.add("factorial", fact, exprs);

	assert_eq!(
		parse_reduce("factorial 2", namespace, exprs).unwrap(),
		parse_reduce("2", namespace, exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("factorial 4", namespace, exprs).unwrap(),
		parse_reduce("24", namespace, exprs).unwrap()
	);
}

#[test]
fn test_hashdb() {
	use hashdb::ArchiveStorable;
	let exprs = &LinkArena::new();
	let db = &mut Datastore::new();
	//let data = Data::new(&[01u8, 32u8]);
	let string = String::from("Hello").store(db).unwrap();
	assert_eq!(string.fetch(db, exprs).unwrap(), "Hello");
}
