use std::{marker::PhantomData};


use crate::expr::{Expr};
use hashdb::{ArchiveDeserializer, ArchiveStore, LinkArena, HashType, TypeStore, hashtype};

/// Name of a thing
#[hashtype]
pub struct Name<'e> {
	name: String,
	_phantom: PhantomData::<&'e ()>
}

/* /// Links Names to Exprs through Reverse Links
#[hashtype]
pub struct RevLink<'e, A: HashType + Archive + 'e, B: HashType + Archive + 'e> {
	#[subtype_reverse_link]
	subject: &'e A,
	#[subtype_reverse_link]
	object: &'e B
} */

/* #[hashtype]
type NamedExpr<'e> = RevLink<'e, Name<'e>, Expr<'e>>; */

/// Link between Name and Expr
#[hashtype]
pub struct NamedExpr<'e> {
	#[subtype_reverse_link]
	name: &'e Name<'e>,
	#[subtype_reverse_link]
	expr: &'e Expr<'e>
}

/// Link between NameTree and Expr
#[hashtype]
pub struct NameTreeExpr<'e> {
	#[subtype_reverse_link]
	name_tree: &'e NameTree<'e>,
	#[subtype_reverse_link]
	expr: &'e Expr<'e>
}

/// Represents a tree of names
#[hashtype]
pub enum NameTree<'e> {
	Abs {
		#[subtype] bind: &'e String,
		#[subtype] expr: &'e NameTree<'e>
	},
	App (
		#[subtype] &'e NameTree<'e>,
		#[subtype] &'e NameTree<'e>,
	),
	Var,
	NamedExpr(#[subtype] &'e NamedExpr<'e>),
}