use std::fmt;

use thiserror::Error;

use hashdb::{Datastore, DatastoreError, NativeHashtype, TypedHash};

mod expr;
mod display;
mod reduce_tree;
mod reduce;
pub use expr::*;
pub use display::*;
pub use reduce_tree::*;
pub use reduce::*;

#[derive(Error, Debug)]
pub enum LambdaError {
	#[error("Format Error")]
	FormatError(#[from] fmt::Error),

	// Datastore Error
	#[error("Datastore Error: {0}")]
	HashtypeResolveError(#[from] DatastoreError),

	// Beta Reduction Error
	#[error("Recursion Depth for beta reduction exceeded")]
	RecursionDepthExceeded,

	#[error("Pointer Tree Mismatch, make sure lambda pointer trees match with expressions.")]
	PointerTreeMismatch,
}