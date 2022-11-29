use thiserror::Error;

use hashdb::{RevTypeStore, hashtype};

use crate::{expr::{Binding, Expr, ReduceError, ReduceLink}, name::{Name, NamedExpr}};

/// A Judgement is a program that is run on another program. 
#[hashtype]
#[derive(Debug)]
pub struct Judgement<'e> {
	#[subtype_reverse_link] pub expr: &'e Expr<'e>,
	#[subtype_reverse_link] pub checker: &'e Expr<'e>,
}

/// Encode an expression using locally-defined encoding
/// Expects `lambda` `app` `var` `branch` `end` and `none` to be defined.
fn encode_expr<'e>(
	expr: &'e Expr<'e>,
	links: &'e impl RevTypeStore<'e>,
	var: &'e Expr<'e>,
	lambda: &'e Expr<'e>,
	app: &'e Expr<'e>,
	none: &'e Expr<'e>,
	branch: &'e Expr<'e>,
	end: &'e Expr<'e>,
) -> &'e Expr<'e> {
	match expr {
		Expr::Variable => var,
		Expr::Lambda { bind, expr } => {
			let bind_encode = encode_binding(bind, links, none, branch, end);
			let expr_encode = encode_expr(expr, links, var, lambda, app, none, branch, end);
			Expr::app(Expr::app(lambda, bind_encode, links), expr_encode, links)
		},
		Expr::Application { func, args } => {
			let func_encode = encode_expr(func, links, var, lambda, app, none, branch, end);
			let args_encode = encode_expr(args, links, var, lambda, app, none, branch, end);
			Expr::app(Expr::app(lambda, func_encode, links), args_encode, links)
		},
	}
}
fn encode_binding<'e>(
	binding: &'e Binding<'e>,
	links: &'e impl RevTypeStore<'e>,
	none: &'e Expr<'e>,
	branch: &'e Expr<'e>,
	end: &'e Expr<'e>,
) -> &'e Expr<'e> {
	match binding {
		Binding::None => none,
		Binding::End => end,
		Binding::Branch(left, right) => {
			let left_encode = encode_binding(left, links, none, branch, end);
			let right_encode = encode_binding(right, links, none, branch, end);
			Expr::app(Expr::app(branch, left_encode, links), right_encode, links)
		},
	}
}

#[derive(Error, Debug)]
pub enum TypeCheckError<'e> {
	#[error("failed to reduce expr: {0}")]
	ReduceError(#[from] ReduceError),
	#[error("failed to typecheck, residual: {0}")]
	Residual(&'e Expr<'e>),
	#[error("encoding `{0}` not defined")]
	EncodingNotDefined(&'static str),
	#[error("encoding `{0}` defined multiple times")]
	EncodingDefinedTooMuch(&'static str),
}

fn lookup_encoding<'e>(string: &'static str, links: &'e impl RevTypeStore<'e>) -> Result<&'e Expr<'e>, TypeCheckError<'e>> {
	let name = Name::add(links.add(string.to_owned()), links);
	let mut iter = links.links::<Name<'e>, NamedExpr<'e>>(name);
	let ret = iter.next().map(|ne|ne.expr).ok_or(TypeCheckError::EncodingNotDefined(string));
	if iter.next().is_some() { return Err(TypeCheckError::EncodingDefinedTooMuch(string)) }
	ret
}

impl<'e> Judgement<'e> {
	pub fn check(expr: &'e ReduceLink<'e>, checker: &'e ReduceLink<'e>, links: &'e impl RevTypeStore<'e>) -> Result<&'e Judgement<'e>, TypeCheckError<'e>> {
		let expr = expr.reduced;
		let checker = checker.reduced;

		let var = lookup_encoding("var", links)?;
		let lambda = lookup_encoding("lambda", links)?;
		let app = lookup_encoding("app", links)?;
		let none = lookup_encoding("none", links)?;
		let branch = lookup_encoding("branch", links)?;
		let end = lookup_encoding("end", links)?;
		let expr_encoded = encode_expr(expr, links, var, lambda, app, none, branch, end);

		let residual = Expr::app(checker, expr_encoded, links).reduce(links)?;
		if let &Expr::Variable = residual {
			Ok(links.rev_add(Judgement { expr, checker }))
		} else {
			Err(TypeCheckError::Residual(residual))
		}
	}
}