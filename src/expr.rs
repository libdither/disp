//! Types of Expressions

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

use hashdb::{ArchiveDeserializer, ArchiveStore, HashType, LinkArena, TypeStore};

mod bind;
mod reduce;
mod check;
pub use bind::*;
pub use reduce::*;
pub use check::*;

#[derive(Error, Debug)]
pub enum LambdaError {
	// Beta Reduction Error
	#[error("recursion depth for beta reduction exceeded")]
	RecursionDepthExceeded,

	#[error("binding level mismatch: make sure variable bindings match with variable positions in expressions and that bindings don't overlap")]
	BindingLevelMismatch,

	#[error("found variable in expression but binding tree is branching")]
	UnexpectedBranchInSubstitution,

	#[error("bind error: {0}")]
	BindError(#[from] BindTreeError)
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'a>"))]
pub enum Expr<'a> {
	/// By itself, an unbound term, a unit of undefined meaning, ready for construction
	Variable,
	/// Create a function
	Lambda {
		#[with(HashType)]
		#[omit_bounds]
		bind: &'a Binding<'a>,
		#[with(HashType)]
		#[omit_bounds]
		expr: &'a Expr<'a>,
	},
	/// Apply functions to expressions
	Application {
		#[with(HashType)]
		#[omit_bounds]
		func: &'a Expr<'a>,
		#[with(HashType)]
		#[omit_bounds]
		args: &'a Expr<'a>,
	},
	// Type of all types
	Universe(usize),
	// Create dependent types
	Pi {
		#[with(HashType)]
		#[omit_bounds]
		bind: &'a Binding<'a>,
		#[with(HashType)]
		#[omit_bounds]
		bind_type: &'a Expr<'a>,
		#[with(HashType)]
		#[omit_bounds]
		expr: &'a Expr<'a>
	},
}

impl<'a> fmt::Display for &'a Expr<'a> {
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
			Expr::Universe(order) => {
				if *order == 0 {
					write!(f, "U")?;
				} else {
					write!(f, "U{}", order)?;
				}
			}
			Expr::Pi { .. } => {
				BINDS.with(|reps| {
					let mut index = BindIndex::DEFAULT;
					let expr = index.push_lambda(&self, reps).unwrap();
					write!(f, "(Π{}[{}] {})", index.index, index.tree, expr)
				})?;
			}
		}
		Ok(())
	}
}

impl<'a> Expr<'a> {
	pub const VAR: 	&'static Expr<'static> = &Expr::Variable;
	pub const UNI0: &'static Expr<'static> = &Expr::Universe(0);
	pub const UNI1: &'static Expr<'static> = &Expr::Universe(1);
	pub fn lambda(bind: &'a Binding<'a>, expr: &'a Expr<'a>, arena: &'a impl TypeStore<'a>) -> &'a Expr<'a> {
		arena.add(Expr::Lambda { bind, expr })
	}
	pub fn app(func: &'a Expr<'a>, args: &'a Expr<'a>, arena: &'a impl TypeStore<'a>) -> &'a Expr<'a> {
		arena.add(Expr::Application { func, args })
	}
	pub fn uni(order: usize, arena: &'a impl TypeStore<'a>) -> &'a Expr<'a> {
		match order {
			0 => Self::UNI0,
			1 => Self::UNI1,
			order => arena.add(Expr::Universe(order))
		}
	}
	pub fn pi(bind: &'a Binding<'a>, bind_type: &'a Expr<'a>, expr: &'a Expr<'a>, arena: &'a impl TypeStore<'a>) -> &'a Expr<'a> {
		arena.add(Expr::Pi { bind, bind_type, expr })
	}
	/// Return true if this Expr can be used as a type
	pub fn is_a_type(&'a self) -> Result<(), TypeError> {
		match self {
			Self::Pi { .. } | Self::Universe(_) => Ok(()),
			_ => Err(TypeError::NotAType(self)),
		}
	}
	/// Get the smallest universe order that can contain this type
	pub fn smallest_uni(&'a self) -> usize {
		match self {
			&Self::Universe(order) => order,
			Self::Pi { bind: _, bind_type, expr } => { bind_type.smallest_uni().max(expr.smallest_uni()) + 1 }
			Self::Application { func, args } => { func.smallest_uni().max(args.smallest_uni()) }
			Self::Lambda { expr, .. } => expr.smallest_uni(),
			Self::Variable => 0,
		}
	}
}
