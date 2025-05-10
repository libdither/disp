use thiserror::Error;

use crate::lower::{BuiltinType, Context, Term, TermKey, TypedTerm};

#[derive(Debug, Error)]
pub enum InferError {
	#[error("InvalidTermKey: {0:?}")]
	InvalidTermKey(TermKey),
	#[error("invalid TypedTerm: {0:?}")]
	InvalidTypeForTerm(TypedTerm),
	#[error("todo error")]
	TODO,
}

// Bidirectional type checker, infers and checks types as needed.
pub fn verify(typed: TypedTerm, ctx: &mut Context) -> Result<TypedTerm, InferError> {
	let term = ctx
		.terms
		.get(typed.term)
		.ok_or(InferError::InvalidTermKey(typed.term))?
		.clone();
	let typ = ctx
		.terms
		.get(typed.typ)
		.ok_or(InferError::InvalidTermKey(typed.typ))?
		.clone();
	match (term, typ) {
		(term, Term::UnknownType(hole)) => Ok({
			let typ = match term {
				Term::Nat(_) => Term::BuiltinTyp(BuiltinType::Nat),
				Term::String(_) => Term::BuiltinTyp(BuiltinType::String),
				Term::Bool(_) => Term::BuiltinTyp(BuiltinType::Bool),
				Term::BuiltinTyp(_) => Term::Uni(0),
				Term::Uni(i) => Term::Uni(i + 1),
				Term::Variable(ident_key) => todo!(), // do lookup
				Term::Set(vec) => Term::Set(vec.iter().map(|(i, _)| (i.clone(), ctx.new_unknown_type())).collect()),
				Term::Abs { args, body } => Term::Abs {
					args,
					body: verify(
						TypedTerm {
							term: body,
							typ: ctx.new_unknown_type(),
						},
						ctx,
					)?
					.typ,
				},
				Term::App { func, args } => Term::App {
					func: verify(
						TypedTerm {
							term: func,
							typ: ctx.new_unknown_type(),
						},
						ctx,
					)?
					.typ,
					args: verify(
						TypedTerm {
							term: args,
							typ: ctx.new_unknown_type(),
						},
						ctx,
					)?
					.typ,
				},
				Term::UnknownType(_) => Err(InferError::TODO)?,
			};
			TypedTerm {
				term: typed.term,
				typ: ctx.add(typ),
			}
		}),
		(Term::Set(mut terms), Term::Set(mut typs)) => {
			if terms.len() == typs.len() {
				terms
					.iter_mut()
					.enumerate()
					.try_for_each::<_, Result<_, InferError>>(|(i, term)| try {
						let typed = verify(
							TypedTerm {
								term: term.1,
								typ: typs.get(i).expect("mismatch typ").1,
							},
							ctx,
						)?;
						typs[i].1 = typed.typ;
					})?;
				for i in 0..terms.len() {
					let typed = verify(
						TypedTerm {
							term: terms[i].1,
							typ: typs[i].1,
						},
						ctx,
					)?;
					typs[i].1 = typed.typ;
				}
				Ok(TypedTerm {
					term: typed.term,
					typ: ctx.add(Term::Set(typs)),
				})
			} else {
				panic!("set term size and typ size mismatch")
			}
		}
		_ => Err(InferError::InvalidTypeForTerm(typed)),
	}
}
