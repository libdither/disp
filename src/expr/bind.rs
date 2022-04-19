///! BindTrees allow for more easy manipulation of Lambda abstractions during beta reduction and creation
use std::fmt;

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};

use hashdb::{DatastoreDeserializer, DatastoreSerializer, HashType, LinkArena, NativeHashtype};

use super::{Expr, LambdaError};

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
pub enum BindTree<'a> {
	None,
	End(usize),
	Branch(&'a BindTree<'a>, &'a BindTree<'a>),
}

impl<'a, 'e> BindTree<'a> {
	pub const NONE: &'static BindTree<'static> = &BindTree::None;
	pub fn split(&'a self) -> Result<(&'a Self, &'a Self), LambdaError> {
		Ok(match self {
			BindTree::Branch(l, r) => (l, r),
			BindTree::None => (self, self),
			BindTree::End(_) => Err(LambdaError::BindingMismatch)?,
		})
	}
	fn branch_new(left: &'a Self, right: &'a Self) -> Self {
		if let (BindTree::None, BindTree::None) = (left, right) {
			BindTree::None
		} else {
			BindTree::Branch(left, right)
		}
	}
	pub fn branch(left: &'a Self, right: &'a Self, reps: &'a LinkArena<'a>) -> &'a Self {
		reps.add(Self::branch_new(left, right))
	}
	pub fn left(&'a self, reps: &'a LinkArena<'a>) -> &'a Self {
		Self::branch(self, BindTree::NONE, reps)
	}
	pub fn right(&'a self, reps: &'a LinkArena<'a>) -> &'a Self {
		Self::branch(BindTree::NONE, self, reps)
	}
	pub fn end(num: usize, reps: &'a LinkArena<'a>) -> &'a Self {
		reps.add(BindTree::End(num))
	}
	/// Add PointerTree to ReplaceTree at certain abstraction level
	fn push_binding(self: &mut &'a Self, reps: &'a LinkArena<'a>, level: usize, pointer: &'e Binding<'e>) -> Result<(), LambdaError> {
		*self = match (*self, pointer) {
			// If ReplaceTree is None, fill in pointer
			(tree, Binding::None) => tree,
			(BindTree::None, Binding::End) => Self::end(level, reps),
			(BindTree::None, Binding::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_binding(reps, level, l)?;
				right.push_binding(reps, level, r)?;
				Self::branch(left, right, reps)
			}
			(BindTree::Branch(mut left, mut right), Binding::Branch(l, r)) => {
				left.push_binding(reps, level, l)?;
				right.push_binding(reps, level, r)?;
				Self::branch(left, right, reps)
			}
			(BindTree::End(_), _) | (_, Binding::End) => Err(LambdaError::BindingMismatch)?,
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	fn pop_binding(self: &mut &'a Self, reps: &'a LinkArena<'a>, level: usize, ptrs: &'e LinkArena<'e>) -> Result<&'e Binding<'e>, LambdaError> {
		use Binding as PT;
		Ok(match self {
			BindTree::Branch(mut l, mut r) => {
				let left = l.pop_binding(reps, level, ptrs)?;
				let right = r.pop_binding(reps, level, ptrs)?;
				*self = Self::branch(l, r, reps);
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

impl<'a> fmt::Display for BindTree<'a> {
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

// Index = 0 means no lambda history (tree should ReplaceTree::None)
// Index > 0 means there is history
#[derive(PartialEq, Debug, Clone)]
pub struct BindIndex<'a> {
	pub index: usize,
	pub tree: &'a BindTree<'a>,
}

impl<'a, 'e> BindIndex<'a> {
	pub const DEFAULT: BindIndex<'a> = BindIndex::new(0, BindTree::NONE);
	pub const fn new(index: usize, tree: &'a BindTree<'a>) -> Self {
		Self { index, tree }
	}
	pub fn split(&self) -> Result<(Self, Self), LambdaError> {
		let (left, right) = self.tree.split()?;
		Ok((BindIndex::new(self.index, left), BindIndex::new(self.index, right)))
	}
	pub fn join(left: BindIndex<'a>, right: BindIndex<'a>, reps: &'a LinkArena<'a>) -> BindIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		BindIndex::new(left.index, BindTree::branch(left.tree, right.tree, reps))
	}
	/// Push PointerTree onto ReplaceIndex
	pub fn push_binding(&mut self, pointer: &'e Binding<'e>, reps: &'a LinkArena<'a>) -> Result<(), LambdaError> {
		let BindIndex { index, tree } = self;
		*index += 1;
		tree.push_binding(reps, *index, pointer)?;
		Ok(())
	}
	/// Pop PointerTree from ReplaceIndex
	pub fn pop_binding(&mut self, reps: &'a LinkArena<'a>, ptrs: &'e LinkArena<'e>) -> Result<&'e Binding<'e>, LambdaError> {
		let BindIndex { index, tree } = self;
		if *index == 0 {
			return Err(LambdaError::BindingMismatch);
		}
		let ret = tree.pop_binding(reps, *index, ptrs)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build ReplaceIndex from Lambda Expression
	pub fn push_lambda(&mut self, expr: &'a Expr<'a>, reps: &'a LinkArena<'a>) -> Result<&'a Expr<'a>, LambdaError> {
		Ok(if let Expr::Lambda { tree: pointer_tree, expr } = expr {
			let expr = self.push_lambda(expr, reps)?;
			self.push_binding(pointer_tree, reps)?;
			expr
		} else {
			&expr
		})
	}
	/// Creates nested lambda expression from given ReplaceTree
	pub fn pop_lambda(&mut self, expr: &'e Expr<'e>, reps: &'a LinkArena<'a>, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, LambdaError> {
		let pointer_tree = self.pop_binding(reps, exprs)?;
		let expr = if self.index == 0 {
			Expr::lambda(pointer_tree, &expr, exprs)
		} else {
			Expr::lambda(pointer_tree, self.pop_lambda(expr, reps, exprs)?, exprs)
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
	let reps = &LinkArena::new();
	let exprs = &LinkArena::new();
	let mut r = BindIndex::DEFAULT;
	println!("start: [{}]", r);

	let lambda = crate::parse::parse("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", reps).unwrap();
	println!("lambda: {}", lambda);
	let expr = r.push_lambda(&lambda, reps).unwrap();
	println!("after push: {} : {}", expr, r);
	let lambda_2 = r.pop_lambda(expr, reps, exprs).unwrap();
	println!("after pop: {}", lambda_2);
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = BindIndex::DEFAULT;
	test_split(reps, r).unwrap();

	let _pts = &LinkArena::new();
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::END, reps).unwrap();
	test_split(reps, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::left(PT::END, exprs), reps).unwrap();
	test_split(reps, r).unwrap();

	let mut r = BindIndex::DEFAULT;
	r.push_binding(&PT::branch(PT::right(PT::END, exprs), PT::END, exprs), reps).unwrap();
	test_split(reps, r).unwrap();
}
#[allow(dead_code)]
fn test_split<'a>(reps: &'a LinkArena<'a>, r: BindIndex<'a>) -> Result<(), LambdaError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e| {
		println!("split err: {}", e);
		e
	})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = BindIndex::join(left, right, reps);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}
