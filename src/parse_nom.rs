
use std::{char::ParseCharError, collections::HashSet};

use hashdb::LinkArena;
use nom::{IResult, Parser, branch::alt, character::{complete::{alphanumeric0, alphanumeric1, char, multispace0}, is_alphanumeric, is_digit, is_space}, combinator::*, complete::take, error::{Error as NomError, ParseError as NomParseError}, multi::{fold_many0, many0}, sequence::{delimited, preceded, terminated, tuple}};

use crate::expr::{Binding, Expr};

fn ws<'a, O, E: NomParseError<&'a str>, F: Parser<&'a str, O, E>>(f: F) -> impl Parser<&'a str, O, E> {
delimited(multispace0, f, multispace0)
}

#[derive(PartialEq, Eq, Debug)]
enum SymbolBounds<'a> {
	None,
	Symbol(&'a str),
	Tree(&'a mut SymbolBounds<'a>, &'a mut SymbolBounds<'a>)
}

// Parse `] <expr> | <bound> <binding>`
fn partial_lambda_binding<'a, 'e>(exprs: &'e LinkArena<'e>, bounds: &'a mut SymbolBounds<'a>) -> impl FnMut(&'a str) -> IResult<&'a str, &'e Expr<'e>> {
	move |i| {
		alt((
			preceded(char(']'), expr(exprs, bounds)), // Find end of binding and call expr
			map(alphanumeric0, |str| {

			})
		))(i)
	}
}

// Parse `<λ?>[<binding>`
fn lambda<'a, 'e>(exprs: &'e LinkArena<'e>, bounds: &'a mut SymbolBounds<'a>) -> impl FnMut(&'a str) -> IResult<&'a str, &'e Expr<'e>> {
	move |i| {
		preceded(
			preceded(opt(char('λ')), char('[')),
			partial_lambda_binding(exprs, bounds),
		)(i)
	}
}
#[test]
fn test_parse_lambda() {
	let exprs = &LinkArena::new();
	let bounds = &mut SymbolBounds::None;
	let lambda = lambda(exprs, bounds)("[x y] x").unwrap().1;
	let bounds = &mut SymbolBounds::None;
	let lambda2 = lambda(exprs, bounds)("[x] [y] x").unwrap().1;

	let key = Expr::lambda(Binding::END, Expr::lambda(Binding::NONE, Expr::VAR, exprs), exprs);
	assert_eq!(lambda, key);
	assert_eq!(lambda2, key);
}

// Parse `{ x: Type, y: Type }`
fn pi_binding<'a>(map: &'a SymbolBounds<'a>) -> impl FnMut(&'a str) -> IResult<&'a str, SymbolBounds<'a>> {
	|i| delimited(
		char('{'),
		fold_many0(ws(alphanumeric1), || map, |mut acc, item| {
			acc.list.push(item);
			acc
		}),
		char('}')
	)(i)
}


/* fn lambda<'a, 'e, 'r>(exprs: &'e LinkArena<'e>) -> impl FnMut(&'a str) -> IResult<&'a str, (&'e Binding<'e>, &'e Expr<'e>)> {
	|i| {
		tuple((
			preceded(opt(char('λ')), symbol_binding),
			expr(exprs)
		))(i)
	}
} */

fn expr<'a, 'e>(exprs: &'e LinkArena<'e>, bounds: &'a mut SymbolBounds<'a>) -> impl FnMut(&'a str) -> IResult<&'a str, &'e Expr<'e>> {
	|i| {
		alt((
			//map(pi(exprs), |bind, bind_type, expr| { exprs.add(Expr::Pi { bind, bind_type, expr }) }),
			map(var(bounds))
			map(lambda(exprs, bounds), |(bind, expr)| Expr::Lambda { bind, expr }),
			// map(symbol_binding),
		))(i)
	}
}

