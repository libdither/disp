use slotmap::{secondary, SecondaryMap};
use smallvec::SmallVec;
use thiserror::Error;

use crate::lower::{Context, IdentKey, LoweringError, Term, TermKey};

#[derive(Debug, Error)]
pub enum ReduceError {
	#[error("unknown term key: {0:?}")]
	UnknownTermKey(TermKey),
	#[error("type hole found, this should be removed during type inference")]
	TypeHoleFound,
	#[error("free variable {0:?} found. can't reduce term with free variables")]
	FreeVariableFound(IdentKey),
}
struct Environment {
	pub assignments: SecondaryMap<IdentKey, SmallVec<usize, 2>>, // map ident to stack indices, there may be multiple
	pub stack: Vec<TermKey>,                                     // execution stack for variable bindings
}
impl Environment {
	pub fn lookup(&self, ident_key: IdentKey) -> Result<TermKey, ReduceError> {
		let index = self
			.assignments
			.get(ident_key)
			.ok_or(ReduceError::FreeVariableFound(ident_key))?
			.last()
			.unwrap();
		Ok(self.stack[*index])
	}
	pub fn push_var(&mut self, ident_key: IdentKey, term: TermKey) {
		let vec = self.assignments.entry(ident_key).expect("invalid ident").or_default();
		self.stack.push(term);
		vec.push(self.stack.len() - 1);
	}
	pub fn pop_var(&mut self, ident_key: IdentKey) {
		let vec = self.assignments.entry(ident_key).expect("invalid ident");
		if let secondary::Entry::Occupied(mut val) = vec {
			val.get_mut().pop();
			if val.get().is_empty() {
				// remove if empty
				val.remove_entry();
			}
		}
	}
}

pub fn reduce(term: TermKey, ctx: &mut Context, env: &mut Environment) -> Result<TermKey, ReduceError> {
	Ok(match ctx.terms.get(term).ok_or(ReduceError::UnknownTermKey(term))? {
		Term::UnknownType(_) => return Err(ReduceError::TypeHoleFound),
		Term::Nat(_) | Term::String(_) | Term::Bool(_) | Term::BuiltinTyp(_) | Term::Uni(_) => term, // irreducibles
		Term::Variable(ident_key) => env.lookup(*ident_key).unwrap_or(term), // variables should be replaced with environment assignments, or they are irreducible
		Term::Set(items) => {
			let items = items.clone();
			// reduce set items one-by-one
			items.iter_mut().try_for_each(|(ident, item)| try {
				if let Some(ident) = ident {
					env.push_var(*ident, *item);
				}
				let reduced = reduce(*item, ctx, env)?;
				if *item != reduced {
					*item = reduced
				}
			})?;
			ctx.add_term(Term::Set(items))
		}
		Term::Abs { args, body } => reduce(*body, ctx, env)?,
		/// TODO
		/// What we need to reduce:
		/// - A function - multiple arguments to satisfy
		/// - Some arguments - might be a set, might be a value
		///   - Ideally, if set we should be able to match against idents in set and satisfy them one by one
		///   - Ideally if not a set, we should be able to figure out which arg it satisfies
		///   - if its a variable
		///   - Solution: Need all arg sets to have idents either derived from strings, *or derived from types*
		Term::App { func, args } => {
			// reduce args
			let args = reduce(*args, ctx, env)?;

			// reduce function
			let func = ctx
				.terms
				.get(reduce(*func, ctx, env)?)
				.expect("reduce should return valid term in context");
			if let Term::Abs { args: func_args, body } = func {
				// if func is function, reduce body with extended environment
				// go through arg one by one. if for each one, search for first matching ident,
				func_args.iter().for_each(|(func_arg, _)| {
					// args
					env.push_var(*func_arg, *body);
				});
			} else {
				ctx.add_term(Term::App {
					func: ctx.add_term(func.clone()),
					args,
				})
			}
		}
	})
}
