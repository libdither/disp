//! Types of Expressions

use std::fmt;

use hashdb::{LinkArena, TypeStore, hashtype};

mod bind;
mod reduce;
mod extract;
pub use bind::*;
pub use reduce::*;

#[hashtype]
#[derive(Debug)]
pub enum Expr<'e> {
	/// By itself, an unbound term, a unit of undefined meaning, ready for construction
	Variable,
	/// Create a function
	Lambda {
		#[subtype] bind: &'e Binding<'e>,
		#[subtype] expr: &'e Expr<'e>,
	},
	/// Apply functions to expressions
	Application {
		#[subtype] func: &'e Expr<'e>,
		#[subtype] args: &'e Expr<'e>,
	},
}

impl<'e> fmt::Display for &'e Expr<'e> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		thread_local! {
			static BINDS: LinkArena<'static> = LinkArena::new();
		}
		match self {
			Expr::Variable => write!(f, "x")?,
			Expr::Lambda { .. } => {
				BINDS.with(|reps| {
					let mut index = BindIndex::DEFAULT;
					let expr = index.push_lambda(&self, reps).unwrap();
					write!(f, "(λ{}[{}] {})", index.index, index.tree, expr)
				})?;
			}
			Expr::Application { func, args: sub } => write!(f, "({} {})", func, sub)?,
		}
		Ok(())
	}
}

impl<'e> Expr<'e> {
	pub const VAR: 	&'static Expr<'static> = &Expr::Variable;
	pub fn lambda(bind: &'e Binding<'e>, expr: &'e Expr<'e>, arena: &'e impl TypeStore<'e>) -> &'e Expr<'e> {
		arena.add(Expr::Lambda { bind, expr })
	}
	pub fn app(func: &'e Expr<'e>, args: &'e Expr<'e>, arena: &'e impl TypeStore<'e>) -> &'e Expr<'e> {
		arena.add(Expr::Application { func, args })
	}
}
