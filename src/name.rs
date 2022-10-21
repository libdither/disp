use std::{cell::RefCell, fmt, iter};

use bytecheck::CheckBytes;
use rkyv::{with::Map, Archive, Deserialize, Serialize};

use crate::expr::{BindTree, Expr};
use hashdb::{ArchiveDeserializer, ArchiveStore, HashType, LinkArena, ReverseLinked, TypeStore};

/// Name of a thing
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'a>"))]
pub struct Name {
	name: String,
}

/// A context is some set of things
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'a>"))]
pub struct Context<'e> {
	#[with(Map<HashType>)]
	#[omit_bounds]
	pub items: HashMap<String, ContextItem<'e>>,
}
impl<'e> Context<'e> {
	fn new(items: Vec<ContextItem<'e>>) -> Self {
		Self { items }
	}
}

/// Links Names to Exprs through Reverse Links
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'a>"))]
pub struct RevLink<'e, A: 'e, B: 'e> {
	#[with(HashType)]
	subject: &'e A,
	#[with(HashType)]
	object: &'e B
}
impl<'e, A: 'e, B: 'e> ReverseLinked for RevLink<'e, A, B> {
    fn reverse_links(&self) -> impl Iterator<Item = u64> {
		iter::once(self.subject.hash()).chain(iter::once(self.object.hash()))
    }
}

type NamedExpr<'e> = Link<'e, Name<'e>, Expr<'e>>;

/// Represents a tree of names
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'e>"))]
pub enum NameTree<'e> {
	Abs {
		#[with(HashType)] bind: &'e String,
		#[with(HashType)] #[omit_bounds] expr: &'e NameTree<'e>
	},
	App (
		#[with(HashType)] #[omit_bounds] &'e NameTree<'e>,
		#[with(HashType)] #[omit_bounds] &'e NameTree<'e>,
	),
	Var,
	NamedExpr(#[with(HashType)] &'e NamedExpr<'e>),
}