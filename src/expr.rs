//! Types of Expressions

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

use hashdb::{ArchiveDeserializer, ArchiveStore, WithHashType, LinkArena, TypeStore};

mod bind;
mod reduce;
pub use bind::*;
pub use reduce::*;

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
		#[with(WithHashType)]
		#[omit_bounds]
		bind: &'a Binding<'a>,
		#[with(WithHashType)]
		#[omit_bounds]
		expr: &'a Expr<'a>,
	},
	/// Apply functions to expressions
	Application {
		#[with(WithHashType)]
		#[omit_bounds]
		func: &'a Expr<'a>,
		#[with(WithHashType)]
		#[omit_bounds]
		args: &'a Expr<'a>,
	},
	// Type of all types
	Universe(usize),
	// Create dependent types
	Pi {
		#[with(WithHashType)]
		#[omit_bounds]
		bind: &'a Binding<'a>,
		#[with(WithHashType)]
		#[omit_bounds]
		bind_type: &'a Expr<'a>,
		#[with(WithHashType)]
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
}
