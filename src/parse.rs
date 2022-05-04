use std::cell::RefCell;

use chumsky::{prelude::*, text::keyword};
use hashdb::{LinkArena, LinkSerializer, NativeHashtype};

use crate::expr::{BindSubTree, Expr};

// Represents active bound variables in the course of parsing an expression
#[derive(Default, Debug)]
struct BindMap {
	map: RefCell<Vec<String>>,
}
impl BindMap {
	// Get binding index for this variable
	fn bind_index(&self, string: &String) -> Option<usize> {
		self.map.borrow().iter().enumerate().rev().find(|(index, e)|*e == string).map(|val|val.0 + 1)
	}
	fn push_bind(&self, string: &String) -> usize {
		let mut map = self.map.borrow_mut();
		map.push(string.clone());
		map.len()
	}
	fn pop_bind(&self, string: &String) -> Option<usize> {
		let mut map = self.map.borrow_mut();
		let ret = map.len();
		map.pop()?;
		Some(ret)
	}
}

fn lookup_expr<'a>(string: &str, exprs: &'a LinkArena<'a>) -> Option<&'a Expr<'a>> {
	thread_local! {
		static SER: RefCell<LinkSerializer> = RefCell::new(LinkSerializer::new());
	}
	SER.with(|ser| {
		let string_hash = string.to_owned().store(&mut *ser.borrow_mut());
		exprs.lookup(&string_hash)
	})
}

fn parser<'e: 'b, 'b>(exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b BindMap) -> impl Parser<char, (&'e Expr<'e>, &'b BindSubTree<'b>), Error = Simple<char>> {
	recursive(|expr: Recursive<'b, char, (&'e Expr<'e>, &'b BindSubTree<'b>), Simple<char>>| {
		// A symbol, can be pretty much any string not including whitespace
		let symbol = text::ident().padded();

		// A resolved symbol, variable, or paranthesised expression.
		let atom = symbol.clone().map(|string| {
			if let Some(val) = bind_map.bind_index(&string) {
				(Expr::VAR, BindSubTree::end(val, binds))
			} else if let Some(expr) = lookup_expr(&string, exprs) {
				(expr, BindSubTree::NONE)
			} else { (Expr::VAR, BindSubTree::NONE) }
		}).or(expr.clone().delimited_by(just('('), just(')')).padded());

		// Parse `[x y z] x y z` as `[x] ([y] ([z] x y z))`
		let lambda = symbol
    		.repeated().at_least(1)
			.delimited_by(just('['), just(']'))
			.map(|symbols| {
				symbols.iter().for_each(|string|{
					bind_map.push_bind(string);
				});
				symbols
			}).then(expr.clone()).foldr(|bind_symbol, (lam_expr, mut bind_tree)| {
				if let Some(index) = bind_map.pop_bind(&bind_symbol) {
					let binding = bind_tree.pop_binding(binds, index, exprs).expect("failed to pop lambda");
					(Expr::lambda(binding, lam_expr, exprs), bind_tree)
				} else { panic!("failed to remove bind") }
			});
		
		// Parse `x y z` as `((x y) z)`
		let application = atom.clone()
			.then(atom.clone().repeated().at_least(1))
			.foldl(|(func, func_index), (args, args_index)| {
				(Expr::app(func, args, exprs), BindSubTree::branch(func_index, args_index, binds))
			});


		// An expression can be a lambda: `[x y]` an application: `x y` or a standalone variable / symbol: `x`
		lambda.or(application).or(atom)
	}).then_ignore(end())
}
pub fn parse<'e>(string: &str, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	let binds = &LinkArena::new();
	let bind_map = &BindMap::default();
	{
		let parsed = parser(exprs, binds, bind_map).parse(string);
		match parsed {
			Ok((expr, _)) => Ok(expr),
			Err(errors) => {
				errors.into_iter().for_each(|error| {
					println!("{error}")
				});
				Err(anyhow::anyhow!("parse error"))
			}
		}
	}
	
}

pub fn parse_reduce<'e>(string: &str, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	Ok(crate::beta_reduce(parse(string, exprs)?, exprs)?)
}

/* pub fn command_parser<'e: 'b, 'b>(string: &str, exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b> bind_map: &'b BindMap) -> impl Parser<char, (&'e Expr<'e>, &'b BindSubTree<'b>), Error = Simple<char>> {
	let set = keyword(keyword)
}

pub fn parse_line<'e: 'b, 'b>(string: &str, exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b BindMap) -> Result<&'e Expr<'e>, anyhow::Error> {

} */

#[test]
fn parse_test() {
	use crate::expr::Binding;

	let exprs = &LinkArena::new();
	let parsed = parse("[x y] x y", exprs).unwrap();
	let test = Expr::lambda(Binding::left(Binding::END, exprs),
	Expr::lambda(Binding::right(Binding::END, exprs),
			Expr::app(Expr::VAR, Expr::VAR, exprs),
		exprs),
	exprs);
	assert_eq!(parsed, test);

	assert_eq!(test, parse("[x y] (x y)", exprs).unwrap());

	let parsed = parse_reduce("([x y] x) ([x y] y) ([x y] x)", exprs).unwrap();
	let parsed_2 = parse("([x y] y)", exprs).unwrap();
	assert_eq!(parsed, parsed_2);
}