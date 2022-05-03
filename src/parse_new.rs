use std::{cell::{Cell, RefCell}, collections::BTreeMap};

use chumsky::{Error, prelude::*, text::Character};
use hashdb::{LinkArena, LinkSerializer, NativeHashtype};
use smallvec::SmallVec;

use crate::expr::{BindIndex, BindSubTree, Binding, Expr};

// Represents active bound variables in the course of parsing an expression
#[derive(Default, Debug)]
struct BindMap {
	map: RefCell<BTreeMap<String, SmallVec<[usize; 2]>>>,
	lambdas: Cell<usize>,
}
impl BindMap {
	fn bind_index(&self, string: &String) -> Option<usize> {
		let map = self.map.borrow();

		map.get(string)?.last().map(|n|map.len() - *n)
	}
	fn push_bind(&self, string: &String) {
		let mut map = self.map.borrow_mut();
		let stack = map.entry(string.clone()).or_insert(SmallVec::new());
		let ret = self.lambdas.get();
		stack.push(ret);
		self.lambdas.set(ret + 1);
	}
	fn pop_bind(&self, string: &String) -> Option<usize> {
		let mut map = self.map.borrow_mut();
		let stack = map.get_mut(string)?;
		let ret = stack.pop();
		if stack.is_empty() { map.remove(string); }
		let lambdas_count = self.lambdas.get();

		if ret.is_some() { self.lambdas.set(lambdas_count - 1); }
		ret.map(|v| lambdas_count - v)
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

fn parser<'a, 'e: 'a, 'b: 'a>(exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b BindMap) -> impl Parser<char, (&'e Expr<'e>, BindIndex<'b>), Error = Simple<char>> + 'a {
	recursive(|expr: Recursive<'a, char, (&'e Expr<'e>, BindIndex<'b>), Simple<char>>| {
		// A symbol, can be pretty much any string not including whitespace
		let symbol = text::ident().padded();

		// A resolved symbol, variable, or paranthesised expression.
		let atom = symbol.clone().map(|string| {
			if let Some(val) = bind_map.bind_index(&string) {
				(Expr::VAR, BindIndex::new(val, BindSubTree::end(val, binds)))
			} else if let Some(expr) = lookup_expr(&string, exprs) {
				(expr, BindIndex::DEFAULT)
			} else { (Expr::VAR, BindIndex::DEFAULT) }
		}).or(expr.clone().delimited_by(just('('), just(')')));

		// Parse `[x y z] x y z` as `[x] ([y] ([z] x y z))`
		let lambda = symbol
    		.repeated().at_least(1)
			.delimited_by(just('['), just(']'))
			.map(|symbols| {
				symbols.iter().for_each(|string|{
					bind_map.push_bind(string);
					println!("bound: {string}");
				});
				symbols
			}).then(expr.clone()).foldr(|bind_symbol, (lam_expr, mut bind_index)| {
				if let Some(index) = bind_map.pop_bind(&bind_symbol) {
					/* let mut index = BindIndex {
						index, tree: bind_index.tree,
					}; */
					print!("Popping index {} with {} = ", bind_index, lam_expr);
					let result = bind_index.pop_lambda(lam_expr, binds, exprs).expect("failed to pop lambda");
					println!("{bind_index} with {result}");
					(result, bind_index)
				} else { panic!("failed to remove bind") }
			});
		
		// Parse `x y z` as `((x y) z)`
		let application = atom.clone()
			.then(atom.clone().repeated().at_least(1))
			.foldl(|(func, mut func_index), (args, mut args_index)| {
				let max = usize::max(func_index.index, args_index.index);
				func_index.index = max; args_index.index = max;
				(Expr::app(func, args, exprs), BindIndex::join(func_index, args_index, binds))
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

#[test]
fn parse_test() {
	let exprs = &LinkArena::new();
	let parsed = parse("[x y] x y", exprs).unwrap();
	let test = Expr::lambda(Binding::left(Binding::END, exprs),
	Expr::lambda(Binding::right(Binding::END, exprs),
			Expr::app(Expr::VAR, Expr::VAR, exprs),
		exprs),
	exprs);
	assert_eq!(parsed, test)
}