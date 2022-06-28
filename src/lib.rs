#![allow(unused)]
#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]

pub use hashdb::{Data, Datastore};
use hashdb::{LinkArena, LinkSerializer, NativeHashtype};

pub mod expr;
pub mod name;
mod parse;

use expr::{beta_reduce, Binding as PT, Expr};
use name::Name;
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
	let exprs = &LinkArena::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(exprs);

	let parsed_id_hash = parse("([x] x)", exprs).unwrap();
	assert_eq!(parsed_id_hash, id_hash);

	let parsed_true_hash = parse("[x y] x", exprs).unwrap();
	assert_eq!(parsed_true_hash, true_hash);

	let parsed_false_hash = parse("[x y] y", exprs).unwrap();
	assert_eq!(parsed_false_hash, false_hash);

	let parsed_not_hash = parse("[x] x ([x y] y) ([x y] x)", exprs).unwrap();
	println!("not: {} vs {}", parsed_not_hash, not_hash);
	assert_eq!(parsed_not_hash, not_hash);

	let parsed_and_hash = parse("[a b] a b ([x y] y)", exprs).unwrap();
	assert_eq!(parsed_and_hash, and_hash);
}

#[test]
fn test_factorial() {
	let exprs = &LinkArena::new();
	let ser = &mut LinkSerializer::new();

	let zero = parse("[x y] y", exprs).unwrap();
	Name::new("zero", zero, exprs, ser);
	Name::new("false", zero, exprs, ser);

	let true_expr = parse("[x y] x", exprs).unwrap();
	Name::new("true", true_expr, exprs, ser);

	let one = parse("[x y] x y", exprs).unwrap();

	let succ = parse("[n f x] f (n f x)", exprs).unwrap();
	Name::new("succ", succ, exprs, ser);

	// Succ Zero = One
	let succ_zero = beta_reduce(Expr::app(&succ, &zero, exprs), exprs).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = beta_reduce(Expr::app(&succ, &one, exprs), exprs).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(beta_reduce(Expr::app(succ, succ_zero, exprs), exprs).unwrap(), succ_one);

	let mult = parse_reduce("[m n f] m (n f)", exprs).unwrap();
	Name::new("mult", mult, exprs, ser);

	println!("{}", parse("mult 2 3", exprs).unwrap());
	assert_eq!(
		parse_reduce("mult 0 0", exprs).unwrap(),
		parse_reduce("0", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 0 1", exprs).unwrap(),
		parse_reduce("0", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 2 3", exprs).unwrap(),
		parse_reduce("6", exprs).unwrap()
	);

	let pred = parse_reduce("[n f x] n ([g h] h (g f)) ([u] x) ([u] u)", exprs).unwrap();
	Name::new("pred", pred, exprs, ser);

	assert_eq!(
		parse_reduce("pred 1", exprs).unwrap(),
		parse_reduce("0", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("pred 6", exprs).unwrap(),
		parse_reduce("5", exprs).unwrap()
	);

	let iszero = parse_reduce("[n] n ([u] [x y] y) ([x y] x)", exprs).unwrap();
	Name::new("iszero", iszero, exprs, ser);

	assert_eq!(
		parse_reduce("iszero 0", exprs).unwrap(),
		parse_reduce("true", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("iszero 6", exprs).unwrap(),
		parse_reduce("false", exprs).unwrap()
	);

	let pair = parse_reduce("[x y f] f x y", exprs).unwrap();
	Name::new("pair", pair, exprs, ser);
	Name::new("first", true_expr, exprs, ser);
	Name::new("second", zero, exprs, ser);

	assert_eq!(
		parse_reduce("(pair 0 1) first", exprs).unwrap(),
		parse_reduce("0", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("(pair 0 1) second", exprs).unwrap(),
		parse_reduce("1", exprs).unwrap()
	);

	let fact_seg = parse("[x y] pair (succ x) (mult x y)", exprs).unwrap();
	Name::new("factseg", fact_seg, exprs, ser);

	let fact = parse_reduce("[n] n ([x] x factseg) (pair 1 1) second", exprs).unwrap();
	Name::new("factorial", fact, exprs, ser);

	assert_eq!(
		parse_reduce("factorial 2", exprs).unwrap(),
		parse_reduce("2", exprs).unwrap()
	);
	assert_eq!(
		parse_reduce("factorial 4", exprs).unwrap(),
		parse_reduce("24", exprs).unwrap()
	);
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
