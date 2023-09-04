#![allow(dead_code)]
#![allow(incomplete_features)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(generic_const_exprs)]
#![feature(type_alias_impl_trait)]
#![feature(try_blocks)]

use hashdb::{TypeStore, LinkArena, RevLinkArena};

pub mod expr;
pub mod name;
pub mod check;
mod parse;
mod new_parser;

use expr::{Binding as B, Expr};
pub use parse::{parse, parse_reduce};
pub use name::*;
pub use check::*;

fn setup_boolean_logic<'a>(
	links: &'a impl TypeStore<'a>,
) -> (&'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>) {
	let id_hash = Expr::lambda(B::END, &Expr::VAR, links);

	let true_hash = Expr::lambda(B::END, &Expr::lambda(B::NONE, &Expr::VAR, links), links);

	let false_hash = Expr::app(&true_hash, &id_hash, links).reduce(links).unwrap();

	let not_hash = Expr::lambda(
		B::left(B::left(B::END, links), links),
		&Expr::app(&Expr::app(&Expr::VAR, &false_hash, links), &true_hash, links),
		links,
	);

	let and_hash = Expr::lambda(
		B::left(B::left(B::END, links), links),
		&Expr::lambda(
			B::left(B::right(B::END, links), links),
			&Expr::app(&Expr::app(&Expr::VAR, &Expr::VAR, links), &false_hash, links),
			links,
		),
		links,
	);

	(id_hash, true_hash, false_hash, not_hash, and_hash)
}

#[test]
fn test_reduce() {
	let links = &LinkArena::new();

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(links);

	let ii_hash = Expr::app(&id_hash, &id_hash, links).reduce(links).unwrap();
	assert_eq!(id_hash, ii_hash);

	assert_eq!(
		Expr::app(&not_hash, &false_hash, links).reduce(links).unwrap(),
		true_hash
	);
	assert_eq!(
		Expr::app(&not_hash, &true_hash, links).reduce(links).unwrap(),
		false_hash
	);

	assert_eq!(
		Expr::app(&Expr::app(&and_hash, &true_hash, links), &true_hash, links).reduce(links).unwrap(),
		true_hash
	);
	assert_eq!(
		Expr::app(&Expr::app(&and_hash, &true_hash, links), &false_hash, links).reduce(links).unwrap(),
		false_hash
	);
	println!("and false true = false");
	assert_eq!(
		Expr::app(&Expr::app(&and_hash, &false_hash, links), &true_hash, links).reduce(links).unwrap(),
		false_hash
	);
	assert_eq!(
		Expr::app(&Expr::app(&and_hash, &false_hash, links), &false_hash, links).reduce(links).unwrap(),
		false_hash
	);
}

#[test]
fn test_parsing() {
	let links = &LinkArena::new();
	let links = &RevLinkArena::new(links);

	let (id_hash, true_hash, false_hash, not_hash, and_hash) = setup_boolean_logic(links);

	let parsed_id_hash = parse("([x] x)", links).unwrap();
	assert_eq!(parsed_id_hash.expr, id_hash);

	let parsed_true_hash = parse("[x y] x", links).unwrap();
	assert_eq!(parsed_true_hash.expr, true_hash);

	let parsed_false_hash = parse("[x y] y", links).unwrap();
	assert_eq!(parsed_false_hash.expr, false_hash);

	let parsed_not_hash = parse("[x] x ([x y] y) ([x y] x)", links).unwrap();
	println!("not: {} vs {}", parsed_not_hash, not_hash);
	assert_eq!(parsed_not_hash.expr, not_hash);

	let parsed_and_hash = parse("[a b] a b ([x y] y)", links).unwrap();
	assert_eq!(parsed_and_hash.expr, and_hash);
}

#[test]
fn test_factorial() {
	
	let exprs = &LinkArena::new();
	let links = &RevLinkArena::new(exprs);

	let zero = parse("[x y] y", links).unwrap().expr;
	
	NamedExpr::new_linked("zero", zero, links);
	NamedExpr::new_linked("false", zero, links);

	let true_expr = parse("[x y] x", links).unwrap().expr;
	NamedExpr::new_linked("true", true_expr, links);

	let one = parse("[x y] x y", links).unwrap().expr;

	let succ = parse("[n f x] f (n f x)", links).unwrap().expr;
	NamedExpr::new_linked("succ", succ, links);

	// Succ Zero = One
	let succ_zero = Expr::app(&succ, &zero, links).reduce(links).unwrap();
	assert_eq!(succ_zero, one);

	let succ_one = Expr::app(&succ, &one, links).reduce(links).unwrap();

	// Succ Succ Zero = Succ 1
	assert_eq!(Expr::app(succ, succ_zero, links).reduce(links).unwrap(), succ_one);

	let mult = parse_reduce("[m n f] m (n f)", links).unwrap();
	NamedExpr::new_linked("mult", mult, links);

	// println!("{}", parse("mult 2 3", links).unwrap());
	assert_eq!(
		parse_reduce("mult 0 0", links).unwrap(),
		parse_reduce("0", links).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 0 1", links).unwrap(),
		parse_reduce("0", links).unwrap()
	);
	assert_eq!(
		parse_reduce("mult 2 3", links).unwrap(),
		parse_reduce("6", links).unwrap()
	);

	let pred = parse_reduce("[n f x] n ([g h] h (g f)) ([u] x) ([u] u)", links).unwrap();
	NamedExpr::new_linked("pred", pred, links);

	assert_eq!(
		parse_reduce("pred 1", links).unwrap(),
		parse_reduce("0", links).unwrap()
	);
	assert_eq!(
		parse_reduce("pred 6", links).unwrap(),
		parse_reduce("5", links).unwrap()
	);

	let iszero = parse_reduce("[n] n ([u] [x y] y) ([x y] x)", links).unwrap();
	NamedExpr::new_linked("iszero", iszero, links);

	assert_eq!(
		parse_reduce("iszero 0", links).unwrap(),
		parse_reduce("true", links).unwrap()
	);
	assert_eq!(
		parse_reduce("iszero 6", links).unwrap(),
		parse_reduce("false", links).unwrap()
	);

	let pair = parse_reduce("[x y f] f x y", links).unwrap();
	NamedExpr::new_linked("pair", pair, links);
	NamedExpr::new_linked("first", true_expr, links);
	NamedExpr::new_linked("second", zero, links);

	assert_eq!(
		parse_reduce("(pair 0 1) first", links).unwrap(),
		parse_reduce("0", links).unwrap()
	);
	assert_eq!(
		parse_reduce("(pair 0 1) second", links).unwrap(),
		parse_reduce("1", links).unwrap()
	);

	let fact_seg = parse("[x y] pair (succ x) (mult x y)", links).unwrap().expr;
	NamedExpr::new_linked("factseg", fact_seg, links);

	let fact = parse_reduce("[n] n ([x] x factseg) (pair 1 1) second", links).unwrap();
	NamedExpr::new_linked("factorial", fact, links);

	assert_eq!(
		parse_reduce("factorial 2", links).unwrap(),
		parse_reduce("2", links).unwrap()
	);
	assert_eq!(
		parse_reduce("factorial 4", links).unwrap(),
		parse_reduce("24", links).unwrap()
	);
}

#[test]
fn test_hashdb() {
	use hashdb::ArchiveStorable;
	let links = &LinkArena::new();
	let db = &mut hashdb::Datastore::new();
	//let data = Data::new(&[01u8, 32u8]);
	let string = String::from("Hello").store(db).unwrap();
	assert_eq!(string.fetch(db, links).unwrap(), "Hello");
}
