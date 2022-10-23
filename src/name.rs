use std::{cell::RefCell, collections::HashMap, fmt, iter, marker::PhantomData};


use crate::expr::{BindTree, Expr};
use hashdb::{ArchiveDeserializer, ArchiveStore, WithHashType, LinkArena, HashType, TypeStore, hashtype};
use rkyv::Archive;

/// Name of a thing
#[hashtype]
pub struct Name<'e> {
	name: String,
	_phantom: PhantomData::<&'e ()>
}

/// Links Names to Exprs through Reverse Links
#[hashtype]
pub struct RevLink<'e, A: HashType + 'e, B: HashType + 'e> {
	#[subtype_reverse_link]
	subject: &'e A,
	#[subtype_reverse_link]
	object: &'e B
}

#[hashtype]
type NamedExpr<'e> = RevLink<'e, Name<'e>, Expr<'e>>;

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