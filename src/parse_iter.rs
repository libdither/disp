use std::{iter, ops::Range};
use thiserror::Error;
use either::{Left, Right};

use hashdb::LinkArena;


use crate::expr::Expr;

type Span = Range<usize>;

#[derive(Debug, Error)]
pub enum ParseError {

}

pub enum Brackets {
	Parens,
	Curly,
	Square,
}

pub enum SyntaxTree<'e> {
	Slice(Span),
	Symbol(Symbol),
	Brackets(Brackets, Span, ),
	Expr(&'e Expr<'e>)
}

/* pub fn parse<'e>(string: &str, exprs: &LinkArena<'e>) -> Result<&'e Expr<'e>, ParseError> {

} */

// Slice -> Symbol
pub struct Symbol {
	pub span: Span
}

fn symbols<'e>(string: &str, iter: impl Iterator<Item = SyntaxTree<'e>>) -> impl Iterator<Item = SyntaxTree<'e>> {
	iter.flat_map(|tree| {
		match tree {
			SyntaxTree::Slice(span) => {
				let slice = &string[span];

				Left()
			}
			item => Right(iter::once(item)),
		}
	})
}

