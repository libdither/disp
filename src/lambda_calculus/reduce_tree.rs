///! ReplaceTrees allow for more easy manipulation of Lambda abstractions during beta reduction and creation

use std::{fmt, ptr::NonNull};

use hashdb::{LinkArena, NativeHashtype};

use crate::{Datastore, lambda_calculus::{Expr, PointerTree, LambdaError}};

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum ReplaceTree<'a> {
	None,
	End(usize),
	Branch(&'a ReplaceTree<'a>, &'a ReplaceTree<'a>),
}

impl<'a, 'e> ReplaceTree<'a> {
	pub const NONE: &'static ReplaceTree<'static> = &ReplaceTree::None;
	pub fn split(&'a self) -> Result<(&'a Self, &'a Self), LambdaError> {
		Ok(match self {
			ReplaceTree::Branch(l, r) => (l, r),
			ReplaceTree::None => (self, self),
			ReplaceTree::End(_) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn branch_new(left: &'a Self, right: &'a Self) -> Self {
		if let (ReplaceTree::None, ReplaceTree::None) = (left, right) { ReplaceTree::None }
		else { ReplaceTree::Branch(left, right) }
	}
	pub fn branch(left: &'a Self, right: &'a Self, reps: &'a LinkArena<'a>) -> &'a Self {
		reps.add(Self::branch_new(left, right))
	}
	pub fn left(&'a self, reps: &'a LinkArena<'a>) -> &'a Self { Self::branch(self, ReplaceTree::NONE, reps) }
	pub fn right(&'a self, reps: &'a LinkArena<'a>) -> &'a Self { Self::branch(ReplaceTree::NONE, self, reps) }
	pub fn end(num: usize, reps: &'a LinkArena<'a>) -> &'a Self { reps.add(ReplaceTree::End(num)) }
	/// Add PointerTree to ReplaceTree at certain abstraction level
	fn push_pointer_tree(self: &mut &'a Self, reps: &'a LinkArena<'a>, level: usize, pointer: &'e PointerTree<'e>) -> Result<(), LambdaError> {
		*self = match (*self, pointer) {
			// If ReplaceTree is None, fill in pointer
			(tree, PointerTree::None) => tree,
			(ReplaceTree::None, PointerTree::End) => Self::end(level, reps),
			(ReplaceTree::None, PointerTree::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_pointer_tree(reps, level, l)?;
				right.push_pointer_tree(reps, level, r)?;
				Self::branch(left, right, reps)
			}
			(ReplaceTree::Branch(mut left, mut right), PointerTree::Branch(l, r)) => {
				left.push_pointer_tree(reps, level, l)?;
				right.push_pointer_tree(reps, level, r)?;
				Self::branch(left, right, reps)
			}
			(ReplaceTree::End(_), _) | (_, PointerTree::End) => Err(LambdaError::PointerTreeMismatch)?
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	fn pop_pointer_tree(self: &mut &'a Self, reps: &'a LinkArena<'a>, level: usize, ptrs: &'e LinkArena<'e>) -> Result<&'e PointerTree<'e>, LambdaError> {
		use PointerTree as PT;
		Ok(match self {
			ReplaceTree::Branch(mut l, mut r) => {
				let left = l.pop_pointer_tree(reps, level, ptrs)?;
				let right = r.pop_pointer_tree(reps, level, ptrs)?;
				*self = Self::branch(l, r, reps);
				PT::branch_reduce(left, right, ptrs)
			},
			ReplaceTree::End(count) if level == *count =>  {
				*self = Self::NONE;
				PT::END
			},
			_ => PT::NONE,
		})
	}
}

impl<'a> fmt::Display for ReplaceTree<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ReplaceTree::Branch(ReplaceTree::None, right) => write!(f, ">{}", right)?,
			ReplaceTree::Branch(left, ReplaceTree::None) => write!(f, "<{}", left)?,
			ReplaceTree::Branch(left, right) => write!(f, "({},{})", left, right)?,
			ReplaceTree::End(val) => write!(f, "{}", val)?,
			ReplaceTree::None => write!(f, "N")?,
		}
		Ok(())
	}
}

// Index = 0 means no lambda history (tree should ReplaceTree::None)
// Index > 0 means there is history
#[derive(PartialEq, Debug, Clone)]
pub struct ReplaceIndex<'a> {
	pub index: usize,
	pub tree: &'a ReplaceTree<'a>
}

impl<'a, 'e> ReplaceIndex<'a> {
	pub const DEFAULT: ReplaceIndex<'a> = ReplaceIndex::new(0, ReplaceTree::NONE);
	pub const fn new(index: usize, tree: &'a ReplaceTree<'a>) -> Self { Self { index, tree } }
	pub fn split(&self) -> Result<(Self, Self), LambdaError> {
		let (left, right) = self.tree.split()?;
		Ok((ReplaceIndex::new(self.index, left), ReplaceIndex::new(self.index, right)))
	}
	pub fn join(left: ReplaceIndex<'a>, right: ReplaceIndex<'a>, reps: &'a LinkArena<'a>) -> ReplaceIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		ReplaceIndex::new(left.index, ReplaceTree::branch(left.tree, right.tree, reps))
	}
	/// Push PointerTree onto ReplaceIndex
	pub fn push_pointer_tree(&mut self, pointer: &'e PointerTree<'e>, reps: &'a LinkArena<'a>) -> Result<(), LambdaError> {
		let ReplaceIndex { index, tree } = self;
		*index += 1;
		tree.push_pointer_tree(reps, *index, pointer)?;
		Ok(())
	}
	/// Pop PointerTree from ReplaceIndex
	pub fn pop_pointer_tree(&mut self, reps: &'a LinkArena<'a>, ptrs: &'e LinkArena<'e>) -> Result<&'e PointerTree<'e>, LambdaError> {
		let ReplaceIndex { index, tree } = self;
		if *index == 0 { return Err(LambdaError::PointerTreeMismatch) }
		let ret = tree.pop_pointer_tree(reps, *index, ptrs)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build ReplaceIndex from Lambda Expression
	pub fn push_lambda(&mut self, expr: &'a Expr<'a>, reps: &'a LinkArena<'a>) -> Result<&'a Expr<'a>, LambdaError> {
		Ok(if let Expr::Lambda { tree: pointer_tree, expr} = expr {
			let expr = self.push_lambda(expr, reps)?;
			self.push_pointer_tree(pointer_tree, reps)?;
			expr
		} else { &expr })
	}
	/// Creates nested lambda expression from given ReplaceTree
	pub fn pop_lambda(&mut self, expr: &'e Expr<'e>, reps: &'a LinkArena<'a>, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, LambdaError> {
		let pointer_tree = self.pop_pointer_tree(reps, exprs)?;
		let expr = if self.index == 0 { Expr::lambda(pointer_tree, &expr, exprs) }
		else { Expr::lambda(pointer_tree, self.pop_lambda(expr, reps, exprs)?, exprs) };
		Ok(expr)
	}
}

impl<'a> fmt::Display for ReplaceIndex<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}/{}", self.tree, self.index)
	}
}

#[test]
fn test_replace_tree() {
	use PointerTree as PT;
	let reps = &LinkArena::new();
	let exprs = &LinkArena::new();
	let mut r = ReplaceIndex::DEFAULT;
	println!("start: [{}]", r);

	let lambda = crate::parse::parse("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", reps).unwrap();
	println!("lambda: {}", lambda);
	let expr = r.push_lambda(&lambda, reps).unwrap();
	println!("after push: {} : {}", expr, r);
	let lambda_2 = r.pop_lambda(expr, reps, exprs).unwrap();
	println!("after pop: {}", lambda_2);
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = ReplaceIndex::DEFAULT;
	test_split(reps, r).unwrap();

	let pts = &LinkArena::new();
	let mut r = ReplaceIndex::DEFAULT;
	r.push_pointer_tree(&PT::END, reps).unwrap();
	test_split(reps, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = ReplaceIndex::DEFAULT;
	r.push_pointer_tree(&PT::left(PT::END, exprs), reps).unwrap();
	test_split(reps, r).unwrap();

	let mut r = ReplaceIndex::DEFAULT;
	r.push_pointer_tree(&PT::branch(PT::right(PT::END, exprs), PT::END, exprs), reps).unwrap();
	test_split(reps, r).unwrap();
}

fn test_split<'a>(reps: &'a LinkArena<'a>, r: ReplaceIndex<'a>) -> Result<(), LambdaError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e|{println!("split err: {}", e); e})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = ReplaceIndex::join(left, right, reps);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}