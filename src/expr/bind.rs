///! Bindings allow for more easy manipulation of Lambda abstractions during beta reduction and creation
use std::fmt;
use thiserror::Error;

use hashdb::{HashType, TypeStore, hashtype};

use super::{Expr, ReduceError};

/// PointerTree represents where the variables are in a Lambda abstraction.
#[hashtype]
#[derive(Debug)]
pub enum Binding<'a> {
	None,
	End,
	Branch(
		#[subtype] &'a Binding<'a>,
		#[subtype] &'a Binding<'a>,
	),
}
impl<'a> Binding<'a> {
	pub const NONE: &'static Binding<'static> = &Binding::None;
	pub const END: &'static Binding<'static> = &Binding::End;
	pub fn left(p: &'a Binding<'a>, arena: &'a impl TypeStore<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(p, Self::NONE))
	}
	pub fn right(p: &'a Binding<'a>, arena: &'a impl TypeStore<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(Self::NONE, p))
	}
	pub fn branch(l: &'a Binding<'a>, r: &'a Binding<'a>, arena: &'a impl TypeStore<'a>) -> &'a Binding<'a> {
		arena.add(Binding::Branch(l, r))
	}
	pub fn branch_reduce(l: &'a Binding<'a>, r: &'a Binding<'a>, arena: &'a impl TypeStore<'a>) -> &'a Binding<'a> {
		if l == Self::NONE && r == Self::NONE {
			Self::NONE
		} else {
			arena.add(Binding::Branch(l, r))
		}
	}
}
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

/// Errors associated with various operations on `BindTree`s
#[derive(Debug, Error)]
pub enum BindTreeError {
	#[error("there was already a bound variable in the tree")]
	AlreadyBound,
	#[error("attempted to bind variable at branch in bind tree")]
	InvalidBindLocation,
	#[error("attempted to split bind tree on leaf")]
	InvalidSplit,
}

// Associates a value with various parts of an `Expr`
#[derive(Clone, Debug, Hash, hashdb::UniqueId, PartialEq)]
pub enum BindTree<'a, T: HashType<'a>> {
	None,
	End(T),
	Branch(&'a BindTree<'a, T>, &'a BindTree<'a, T>),
}
impl<'a, T: HashType<'a>> BindTree<'a, T> {
	pub const NONE: &'a BindTree<'a, T> = &BindTree::None;
	pub fn split(&'a self) -> Result<(&'a Self, &'a Self), BindTreeError> {
		Ok(match self {
			BindTree::Branch(l, r) => (l, r),
			BindTree::None => (self, self),
			BindTree::End(_) => Err(BindTreeError::InvalidSplit)?,
		})
	}
	fn branch_new(left: &'a Self, right: &'a Self) -> Self {
		if let (BindTree::None, BindTree::None) = (left, right) { BindTree::None }
		else { BindTree::Branch(left, right) }
	}
	pub fn branch(left: &'a Self, right: &'a Self, trees: &'a impl TypeStore<'a>) -> &'a Self {
		trees.add(Self::branch_new(left, right))
	}
	pub fn left(&'a self, trees: &'a impl TypeStore<'a>) -> &'a Self {
		Self::branch(self, BindTree::NONE, trees)
	}
	pub fn right(&'a self, trees: &'a impl TypeStore<'a>) -> &'a Self {
		Self::branch(BindTree::NONE, self, trees)
	}
	pub fn end(val: T, trees: &'a impl TypeStore<'a>) -> &'a Self {
		trees.add(BindTree::End(val))
	}
	/// Add PointerTree to ReplaceTree at certain abstraction level
	pub fn push_binding<'e>(self: &mut &'a Self, trees: &'a impl TypeStore<'a>, end: T, binds: &'e Binding<'e>) -> Result<(), BindTreeError>
		where T: Clone,
	{
		*self = match (*self, binds) {
			// If ReplaceTree is None, fill in binds
			(tree, Binding::None) => tree,
			(BindTree::None, Binding::End) => Self::end(end, trees),
			(BindTree::None, Binding::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_binding(trees, end.clone(), l)?;
				right.push_binding(trees, end, r)?;
				Self::branch(left, right, trees)
			}
			(BindTree::Branch(mut left, mut right), Binding::Branch(l, r)) => {
				left.push_binding(trees, end.clone(), l)?;
				right.push_binding(trees, end, r)?;
				Self::branch(left, right, trees)
			}
			(BindTree::End(_), _) => return Err(BindTreeError::AlreadyBound),
			(_, Binding::End) => return Err(BindTreeError::InvalidBindLocation),
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	pub fn pop_binding<'e>(self: &mut &'a Self, trees: &'a impl TypeStore<'a>, end: &T, binds: &'e impl TypeStore<'e>) -> Result<&'e Binding<'e>, BindTreeError> 
		where T: PartialEq,
	{
		Ok(match self {
			BindTree::Branch(mut l, mut r) => {
				let left = l.pop_binding(trees, end, binds)?;
				let right = r.pop_binding(trees, end, binds)?;
				*self = Self::branch(l, r, trees);
				Binding::branch_reduce(left, right, binds)
			}
			BindTree::End(count) if *end == *count => {
				*self = Self::NONE;
				Binding::END
			}
			_ => Binding::NONE,
		})
	}
}

impl<'a, T: fmt::Display + HashType<'a>> fmt::Display for BindTree<'a, T> {
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

/// BindTree that can represent multiple lambda abstractions at once
pub type BindSubTree<'a> = BindTree<'a, usize>;

// Index = 0 means no lambda history (tree should BindTree::None)
// Index > 0 means there is history
#[derive(PartialEq, Debug, Clone)]
pub struct BindIndex<'a> {
	pub index: usize,
	pub tree: &'a BindSubTree<'a>,
}

impl<'a> BindIndex<'a> {
	pub const DEFAULT: BindIndex<'a> = BindIndex::new(0, BindTree::NONE);
	/// Create new BindIndex
	pub const fn new(index: usize, tree: &'a BindSubTree<'a>) -> Self {
		Self { index, tree }
	}
	/// Split on branch of BindTree
	pub fn split(&self) -> Result<(Self, Self), BindTreeError> {
		let (left, right) = self.tree.split()?;
		Ok((BindIndex::new(self.index, left), BindIndex::new(self.index, right)))
	}
	// Join two BindIndexs of same index
	pub fn join(left: BindIndex<'a>, right: BindIndex<'a>, trees: &'a impl TypeStore<'a>) -> BindIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		BindIndex::new(left.index, BindTree::branch(left.tree, right.tree, trees))
	}
	/// Push Binding onto BindIndex
	pub fn push_binding<'e>(&mut self, binds: &'e Binding<'e>, trees: &'a impl TypeStore<'a>) -> Result<(), BindTreeError> {
		let BindIndex { index, tree } = self;
		*index += 1;
		tree.push_binding(trees, *index, binds)?;
		Ok(())
	}
	/// Pop Binding from BindIndex
	pub fn pop_binding<'e>(&mut self, trees: &'a impl TypeStore<'a>, binds: &'e impl TypeStore<'e>) -> Result<&'e Binding<'e>, ReduceError> {
		let BindIndex::<'a> { index, tree } = self;
		if *index == 0 {
			return Err(ReduceError::BindingLevelMismatch);
		}
		let ret = tree.pop_binding(trees, index, binds)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build BindIndex from nested Lambda expressions
	pub fn push_lambda<'e>(&mut self, expr: &'e Expr<'e>, trees: &'a impl TypeStore<'a>) -> Result<&'e Expr<'e>, BindTreeError> {
		Ok(if let Expr::Lambda { bind: binds_tree, expr } = expr {
			let pushed_expr = self.push_lambda(expr, trees)?;
			self.push_binding(binds_tree, trees)?;
			pushed_expr
		} else { &expr })
	}
	/// Creates nested Lambda expressions from BindIndex
	#[allow(dead_code)]
	pub fn pop_lambda<'e>(&mut self, expr: &'e Expr<'e>, trees: &'a impl TypeStore<'a>, exprs: &'e impl TypeStore<'e>) -> Result<&'e Expr<'e>, ReduceError> {
		let binds_tree = self.pop_binding(trees, exprs)?;
		let popped_expr = if self.index == 0 { &expr } else { self.pop_lambda(expr, trees, exprs)? };
		Ok(Expr::lambda(binds_tree, popped_expr, exprs))
	}
}

impl<'a> fmt::Display for BindIndex<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}/{}", self.tree, self.index)
	}
}
/* 
#[test]
fn test_replace_tree() {
	use hashdb::{LinkArena, RevLinkArena};

	use Binding as B;
	let trees = &LinkArena::new();
	let exprs = &LinkArena::new();
	let links = RevLinkArena::new(exprs);
	let mut r = BindIndex::DEFAULT;
	println!("start: [{}]", r);

	let lambda = crate::parse::parse("[x y z w] x (y z) w", &mut links).unwrap().expr;
	println!("lambda: {}", lambda);
	let expr = r.push_lambda(&lambda, trees).unwrap();
	println!("after push: {} : {}", expr, r);
	let lambda_2 = r.pop_lambda(expr, trees, exprs).unwrap();
	println!("after pop: {}", lambda_2);
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = BindIndex::DEFAULT;
	test_split(trees, r).unwrap();

	let _pts = &LinkArena::new();
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&B::END, trees).unwrap();
	test_split(trees, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&B::left(B::END, exprs), trees).unwrap();
	test_split(trees, r).unwrap();

	let mut r = BindIndex::DEFAULT;
	r.push_binding(&B::branch(B::right(B::END, exprs), B::END, exprs), trees).unwrap();
	test_split(trees, r).unwrap();
}
#[allow(dead_code)]
fn test_split<'a>(trees: &'a impl TypeStore<'a>, r: BindIndex<'a>) -> Result<(), BindTreeError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e| {
		println!("split err: {}", e);
		e
	})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = BindIndex::join(left, right, trees);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}
 */