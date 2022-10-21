use std::{cell::RefCell, fmt};

use bytecheck::CheckBytes;
use rkyv::{with::Map, Archive, Deserialize, Serialize};

use crate::expr::{BindTree, Expr};
use hashdb::{ArchiveDeserializer, ArchiveStore, HashType, LinkArena, TypeStore};

/// Represents a tree of names
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'e>"))]
enum NameTree<'e> {
	BoundName {
		#[with(HashType)] bind: &'e String,
		#[with(HashType)] name: &'e String,
		#[with(HashType)] #[omit_bounds] child: &'e NameTree<'e>
	},
	Branch (
		#[with(HashType)] #[omit_bounds] &'e NameTree<'e>,
		#[with(HashType)] #[omit_bounds] &'e NameTree<'e>,
	),
	Name(#[with(HashType)] &'e String),
	End,
}
impl<'e> fmt::Display for NameTree<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Object that represents an expression and names for various parts of it
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'e>"))]
pub struct NamedExpr<'e, T> {
	#[with(HashType)]
	#[omit_bounds]
	expr: &'e Expr<'e>,
	#[with(HashType)]
	#[omit_bounds]
	name_bind: &'e NameTree<'e>,
}
impl<'e> fmt::Display for Named<'e, Expr<'e>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.expr, self.name_bind) {
			(_, NameTree::End(name)) => write!(f, "{name}"), // End of name_bind names an whole expr
			(Expr::Application { func, args }, NameTree::Branch(left, right)) => { // Branch of name_bind displays recursively
				write!(f, "({}, {})", NamedExpr { expr: *func, name_bind: *left }, NamedExpr { expr: *args, name_bind: *right })
			}
			(expr, NameTree::NONE) => write!(f, "{expr}"),
			(Expr::Application { .. }, _) => {
				println!("[error] mistmatched Expr and NameTree, Expr was Application, but NameTree wasn't Branch");
				write!(f, "{}", self.expr)
			},

        }
    }
}

/// Object in disp that has a name
#[derive(Debug, Hash, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'e>"))]
pub enum NamedObject<'e> {
	Namespace(
		#[with(HashType)]
		#[omit_bounds]
		&'e NamedSpace<'e>,
	),
	Expr(
		#[with(HashType)]
		#[omit_bounds]
		&'e NamedExpr<'e>
	),
}
impl<'e> NamedObject<'e> {
	fn name(&self) -> &'e str {
		match self {
			Namespace(namespace) => namespace.
			Expr(expr)
		}
	}
}

impl<'e> fmt::Display for NamedObject<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
			Self::Namespace(namespace) => write!(f, "{namespace}"),
			Self::Expr(expr) => write!(f, "{expr}"),
		}
    }
}

#[derive(Default)]
pub struct NamespaceMut<'e> {
	namespace: RefCell<Namespace<'e>>,
}
impl<'e> NamespaceMut<'e> {
	pub fn new() -> Self { Self::default() }
	pub fn add(&self, name: impl Into<String>, expr: &'e Expr<'e>, exprs: &'e LinkArena<'e>) {
		self.namespace.borrow_mut().add(name, expr, exprs)
	}
	pub fn find<P: FnMut(&&&'e NamedObject<'e>) -> bool>(&self, predicate: P) -> Option<&'e NamedObject<'e>> {
		self.namespace.borrow().items.iter().find(predicate).as_deref().map(|n|*n)
	}
	pub fn for_each<F: FnMut(&&'e NamedObject<'e>)>(&self, func: F) {
		self.namespace.borrow().items.iter().for_each(func)
	}
	pub fn store_inner(&self, exprs: &'e LinkArena<'e>) -> &'e Namespace<'e> {
		exprs.add(self.namespace.borrow().clone())
	}
	pub fn extend(&self, namespace: &Namespace<'e>) {
		self.namespace.borrow_mut().items.extend(&namespace.items);
	}
}
impl<'e> From<Namespace<'e>> for NamespaceMut<'e> {
    fn from(namespace: Namespace<'e>) -> Self {
        NamespaceMut { namespace: RefCell::new(namespace) }
    }
}

// A list of names
#[derive(Clone, Hash, Debug, Archive, Serialize, Deserialize, Default)]
#[archive_attr(derive(CheckBytes))]
#[archive(bound(serialize = "__S: ArchiveStore", deserialize = "__D: ArchiveDeserializer<'e>"))]
pub struct Namespace<'e> {
	#[with(Map<HashType>)]
	#[omit_bounds]
	pub items: Vec<&'e NamedObject<'e>>,
}
impl<'e> Namespace<'e> {
	pub fn new() -> Self { Namespace::default() }
	pub fn add_name(&mut self, name: &'e NamedObject<'e>) {
		self.items.push(name);
	}
	pub fn add(&mut self, name: impl Into<String>, expr: &'e Expr<'e>, exprs: &'e LinkArena<'e>) {
		self.items.push(NamedObject::new(name, expr, exprs))
	}
}
impl<'e> fmt::Display for Namespace<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.items {
			match item.object {
				NamedObject::Expr(expr) => writeln!(f, "{expr}")?,
				_ => {},
			}
		}
		Ok(())
    }
}
