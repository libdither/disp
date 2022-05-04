///! BindTrees allow for more easy manipulation of Lambda abstractions during beta reduction and creation
use std::{fmt, hash::Hash as StdHash};
use thiserror::Error;
use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};

use hashdb::{DatastoreDeserializer, DatastoreSerializer, HashType, LinkArena, NativeHashtype};

use super::{Expr, LambdaError};

#[derive(Debug, Error)]
pub enum BindError {
	#[error("there was already a bound variable in the tree")]
	TreeAlreadyBound,
	#[error("attempted to bind variable at branch in bind tree")]
	InvalidBindLocation,
	#[error("attempted to split bind tree on leaf")]
	InvalidSplit,
}

/// PointerTree represents where the variables are in a Lambda abstraction.
#[derive(Clone, Hash, PartialEq, Eq, Debug, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes, Debug))]
#[archive(bound(serialize = "__S: DatastoreSerializer", deserialize = "__D: DatastoreDeserializer<'a>"))]
pub enum Binding<'a> {
	None,
	End,
	Branch(
		#[with(HashType)]
		#[omit_bounds]
		&'a Binding<'a>,
		#[with(HashType)]
		#[omit_bounds]
		&'a Binding<'a>,
	), // u32 represents highest variable abstraction level in this expression
}
impl<'a> Binding<'a> {
	pub const NONE: &'static Binding<'static> = &Binding::None;
	pub const END: &'static Binding<'static> = &Binding::End;
	pub fn left(p: &'a Binding<'a>, arena: &'a LinkArena<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(p, Self::NONE))
	}
	pub fn right(p: &'a Binding<'a>, arena: &'a LinkArena<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(Self::NONE, p))
	}
	pub fn branch(l: &'a Binding<'a>, r: &'a Binding<'a>, arena: &'a LinkArena<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(l, r))
	}
	pub fn branch_reduce(l: &'a Binding<'a>, r: &'a Binding<'a>, arena: &'a LinkArena<'a>) -> &'a Binding<'a> {
		if l == Self::NONE && r == Self::NONE {
			Self::NONE
		} else {
			arena.add(Binding::Branch(l, r))
		}
	}
}

impl<'a> NativeHashtype for Binding<'a> {}
impl<'a> fmt::Display for Binding<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match *self {
			Binding::Branch(left, right) => match (*right == Binding::None, *left == Binding::None) {
				(true, true) => write!(f, "BOTH(NONE, NONE)")?,
				(true, false) => write!(f, "<{}", left)?,
				(false, true) => write!(f, ">{}", right)?,
				(false, false) => write!(f, "({},{})", left, right)?,
			},
			Binding::End => write!(f, ".")?,
			Binding::None => {}
		}
		Ok(())
	}
}

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum BindTree<'a, T: StdHash> {
	None,
	End(T),
	Branch(&'a BindTree<'a, T>, &'a BindTree<'a, T>),
}
impl<'a, 'e, T: StdHash + 'e> BindTree<'a, T> {
	pub const NONE: &'e BindTree<'e, T> = &BindTree::None;
	pub fn split(&'a self) -> Result<(&'a Self, &'a Self), BindError> {
		Ok(match self {
			BindTree::Branch(l, r) => (l, r),
			BindTree::None => (self, self),
			BindTree::End(_) => Err(BindError::InvalidSplit)?,
		})
	}
	fn branch_new(left: &'a Self, right: &'a Self) -> Self {
		if let (BindTree::None, BindTree::None) = (left, right) {
			BindTree::None
		} else {
			BindTree::Branch(left, right)
		}
	}
	pub fn branch(left: &'a Self, right: &'a Self, binds: &'a LinkArena<'a>) -> &'a Self {
		binds.add(Self::branch_new(left, right))
	}
	pub fn left(&'a self, binds: &'a LinkArena<'a>) -> &'a Self {
		Self::branch(self, BindTree::NONE, binds)
	}
	pub fn right(&'a self, binds: &'a LinkArena<'a>) -> &'a Self {
		Self::branch(BindTree::NONE, self, binds)
	}
	pub fn end(val: T, binds: &'a LinkArena<'a>) -> &'a Self {
		binds.add(BindTree::End(val))
	}
}

impl<'a, T: fmt::Display + StdHash> fmt::Display for BindTree<'a, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			BindTree::Branch(BindTree::None, right) => write!(f, ">{}", right)?,
			BindTree::Branch(left, BindTree::None) => write!(f, "<{}", left)?,
			BindTree::Branch(left, right) => write!(f, "({},{})", left, right)?,
			BindTree::End(val) => write!(f, "{}", val)?,
			BindTree::None => write!(f, "N")?,
		}
		Ok(())
	}
}

pub type BindSubTree<'a> = BindTree<'a, usize>;
impl<'a, 'e> BindSubTree<'a> {
	/// Add PointerTree to ReplaceTree at certain abstraction level
	pub fn push_binding(self: &mut &'a Self, binds: &'a LinkArena<'a>, level: usize, pointer: &'e Binding<'e>) -> Result<(), BindError> {
		*self = match (*self, pointer) {
			// If ReplaceTree is None, fill in pointer
			(tree, Binding::None) => tree,
			(BindTree::None, Binding::End) => Self::end(level, binds),
			(BindTree::None, Binding::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_binding(binds, level, l)?;
				right.push_binding(binds, level, r)?;
				Self::branch(left, right, binds)
			}
			(BindTree::Branch(mut left, mut right), Binding::Branch(l, r)) => {
				left.push_binding(binds, level, l)?;
				right.push_binding(binds, level, r)?;
				Self::branch(left, right, binds)
			}
			(BindTree::End(_), _) => return Err(BindError::TreeAlreadyBound),
			(_, Binding::End) => return Err(BindError::InvalidBindLocation),
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	pub fn pop_binding(self: &mut &'a Self, binds: &'a LinkArena<'a>, level: usize, ptrs: &'e LinkArena<'e>) -> Result<&'e Binding<'e>, BindError> {
		use Binding as PT;
		Ok(match self {
			BindTree::Branch(mut l, mut r) => {
				let left = l.pop_binding(binds, level, ptrs)?;
				let right = r.pop_binding(binds, level, ptrs)?;
				*self = Self::branch(l, r, binds);
				PT::branch_reduce(left, right, ptrs)
			}
			BindTree::End(count) if level == *count => {
				*self = Self::NONE;
				PT::END
			}
			_ => PT::NONE,
		})
	}
}

pub type BindTypeTree<'a, 'e> = BindTree<'a, &'e Expr<'e>>;
impl<'a, 'e> BindTypeTree<'a, 'e> {
	// Push Binding and type Expr onto BindTypeTree
	pub fn push_binding(self: &mut &'a Self, bind: &'e Binding<'e>, bind_type: &'e Expr<'e>, binds: &'a LinkArena<'a>) -> Result<(), BindError> {
		*self = match (*self, bind) {
			// If ReplaceTree is None, fill in pointer
			(tree, Binding::None) => tree,
			(BindTree::None, Binding::End) => Self::end(bind_type, binds),
			(BindTree::None, Binding::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_binding(l, bind_type, binds)?;
				right.push_binding(r, bind_type, binds)?;
				Self::branch(left, right, binds)
			}
			(BindTree::Branch(mut left, mut right), Binding::Branch(l, r)) => {
				left.push_binding(l, bind_type, binds)?;
				right.push_binding(r, bind_type, binds)?;
				Self::branch(left, right, binds)
			}
			(BindTree::End(_), _) => return Err(BindError::TreeAlreadyBound),
			(_, Binding::End) => return Err(BindError::InvalidBindLocation),
		};
		Ok(())
	}
}

// Index = 0 means no lambda history (tree should ReplaceTree::None)
// Index > 0 means there is history
#[derive(PartialEq, Debug, Clone)]
pub struct BindIndex<'a> {
	pub index: usize,
	pub tree: &'a BindSubTree<'a>,
}

impl<'a, 'e> BindIndex<'a> {
	pub const DEFAULT: BindIndex<'a> = BindIndex::new(0, BindTree::NONE);
	pub const fn new(index: usize, tree: &'a BindSubTree<'a>) -> Self {
		Self { index, tree }
	}
	pub fn split(&self) -> Result<(Self, Self), BindError> {
		let (left, right) = self.tree.split()?;
		Ok((BindIndex::new(self.index, left), BindIndex::new(self.index, right)))
	}
	pub fn join(left: BindIndex<'a>, right: BindIndex<'a>, binds: &'a LinkArena<'a>) -> BindIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		BindIndex::new(left.index, BindTree::branch(left.tree, right.tree, binds))
	}
	/// Push PointerTree onto ReplaceIndex
	pub fn push_binding(&mut self, pointer: &'e Binding<'e>, binds: &'a LinkArena<'a>) -> Result<(), BindError> {
		let BindIndex { index, tree } = self;
		*index += 1;
		tree.push_binding(binds, *index, pointer)?;
		Ok(())
	}
	/// Pop PointerTree from ReplaceIndex
	pub fn pop_binding(&mut self, binds: &'a LinkArena<'a>, ptrs: &'e LinkArena<'e>) -> Result<&'e Binding<'e>, LambdaError> {
		let BindIndex { index, tree } = self;
		if *index == 0 {
			return Err(LambdaError::BindingLevelMismatch);
		}
		let ret = tree.pop_binding(binds, *index, ptrs)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build ReplaceIndex from Lambda Expression
	pub fn push_lambda(&mut self, expr: &'a Expr<'a>, binds: &'a LinkArena<'a>) -> Result<&'a Expr<'a>, BindError> {
		Ok(if let Expr::Lambda { bind: pointer_tree, expr } = expr {
			let expr = self.push_lambda(expr, binds)?;
			self.push_binding(pointer_tree, binds)?;
			expr
		} else {
			&expr
		})
	}
	/// Creates nested lambda expression from given ReplaceTree
	pub fn pop_lambda(&mut self, expr: &'e Expr<'e>, binds: &'a LinkArena<'a>, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, LambdaError> {
		let pointer_tree = self.pop_binding(binds, exprs)?;
		let expr = if self.index == 0 {
			Expr::lambda(pointer_tree, &expr, exprs)
		} else {
			Expr::lambda(pointer_tree, self.pop_lambda(expr, binds, exprs)?, exprs)
		};
		Ok(expr)
	}
}

impl<'a> fmt::Display for BindIndex<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}/{}", self.tree, self.index)
	}
}

#[test]
fn test_replace_tree() {
	use Binding as PT;
	let binds = &LinkArena::new();
	let exprs = &LinkArena::new();
	let mut r = BindIndex::DEFAULT;
	println!("start: [{}]", r);

	let lambda = crate::parse::parse("[x y z w] x (y z) w", exprs).unwrap();
	println!("lambda: {}", lambda);
	let expr = r.push_lambda(&lambda, binds).unwrap();
	println!("after push: {} : {}", expr, r);
	let lambda_2 = r.pop_lambda(expr, binds, exprs).unwrap();
	println!("after pop: {}", lambda_2);
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = BindIndex::DEFAULT;
	test_split(binds, r).unwrap();

	let _pts = &LinkArena::new();
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::END, binds).unwrap();
	test_split(binds, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::left(PT::END, exprs), binds).unwrap();
	test_split(binds, r).unwrap();

	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::branch(PT::right(PT::END, exprs), PT::END, exprs), binds).unwrap();
	test_split(binds, r).unwrap();
}
#[allow(dead_code)]
fn test_split<'a>(binds: &'a LinkArena<'a>, r: BindIndex<'a>) -> Result<(), BindError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e| {
		println!("split err: {}", e);
		e
	})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = BindIndex::join(left, right, binds);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}
