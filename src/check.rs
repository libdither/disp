use thiserror::Error;

use hashdb::{RevTypeStore, hashtype};

use crate::expr::{Binding, Expr, ReduceError, ReduceLink};

/// A Judgement is a program that is run on another program. 
#[hashtype]
#[derive(Debug)]
struct Judgement<'e> {
	#[subtype_reverse_link] expr: &'e Expr<'e>,
	#[subtype_reverse_link] checker: &'e Expr<'e>,
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
	Residual(&'e Expr<'e>)
}

impl<'e> Judgement<'e> {
	fn check(expr: &'e ReduceLink<'e>, checker: &'e ReduceLink<'e>, links: &'e impl RevTypeStore<'e>) -> Result<&'e Judgement<'e>, TypeCheckError<'e>> {
		let expr = expr.reduced;
		let checker = checker.reduced;

		let residual = Expr::app(checker, expr, links).reduce(links)?;
		if let &Expr::Variable = residual {
			Ok(links.rev_add(Judgement { expr, checker }))
		} else {
			Err(TypeCheckError::Residual(residual))
		}
	}
}