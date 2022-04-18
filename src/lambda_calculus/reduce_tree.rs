///! ReplaceTrees allow for more easy manipulation of Lambda abstractions during beta reduction and creation

use std::{fmt, ptr::NonNull};

use hashdb::{LinkArena, NativeHashtype};

use crate::{Datastore, Link, lambda_calculus::{Expr, PointerTree, DisplayWithDatastore, LambdaError}};

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum ReplaceTree<'a> {
	None,
	End(usize),
	Branch(Link<'a, ReplaceTree<'a>>, Link<'a, ReplaceTree<'a>>),
}

impl<'a> ReplaceTree<'a> {
	pub const NONE: Link<'static, ReplaceTree<'static>> = Link::new(&ReplaceTree::None);
	pub fn split(&'a self) -> Result<(&'a Self, &'a Self), LambdaError> {
		Ok(match self {
			ReplaceTree::Branch(l, r) => (l, r),
			ReplaceTree::None => (self, self),
			ReplaceTree::End(_) => Err(LambdaError::PointerTreeMismatch)?
		})
	}
	fn join(left: Link<'a, Self>, right: Link<'a, Self>) -> Self {
		if let (ReplaceTree::None, ReplaceTree::None) = (*left, *right) { ReplaceTree::None }
		else { ReplaceTree::Branch(left, right) }
	}
	fn join_alloc(left: Link<'a, Self>, right: Link<'a, Self>, reps: &'a LinkArena<'a>) -> Link<'a, Self> {
		reps.add(Self::join(left, right))
	}
	pub fn end(num: usize, reps: &'a LinkArena<'a>) -> Link<'a, Self> { reps.add(ReplaceTree::End(num)) }
	pub fn join_index(left: ReplaceIndex<'a>, right: ReplaceIndex<'a>) -> ReplaceIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		ReplaceIndex::new(left.index, self.join(left.tree, right.tree))
	}
	/// Add PointerTree to ReplaceTree at certain abstraction level
	fn push_pointer_tree(&'a mut self, reps: &'a LinkArena<'a>, level: usize, pointer: &Link<PointerTree>, ptrs: &'a LinkArena<'a>) -> Result<(), LambdaError> {
		*self = match (&self, pointer.as_ref()) {
			// If ReplaceTree is None, fill in pointer
			(tree, PointerTree::None) => **tree,
			(ReplaceTree::None, PointerTree::End) => reps.end(level),
			(ReplaceTree::None, PointerTree::Branch(l, r)) => {
				let (mut left, mut right) = (Self::NONE, Self::NONE);
				left.push_pointer_tree(reps, level, l, ptrs)?;
				right.push_pointer_tree(reps, level, r, ptrs)?;
				reps.join(left, right)
			}
			(ReplaceTree::Branch(mut left, mut right), PointerTree::Branch(l, r)) => {
				left.push_pointer_tree(reps, level, l, ptrs)?;
				right.push_pointer_tree(reps, level, r, ptrs)?;
				reps.join(left, right)
			}
			(ReplaceTree::End(_), _) | (_, PointerTree::End) => Err(LambdaError::PointerTreeMismatch)?
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	fn pop_pointer_tree(&'a mut self, reps: &'a LinkArena<'a>, level: usize, ptrs: &'a LinkArena<'a>) -> Result<Link<'a, PointerTree<'a>>, LambdaError> {
		use PointerTree as PT;
		Ok(match self {
			ReplaceTree::Branch(mut l, mut r) => {
				let left = l.pop_pointer_tree(reps, level, ptrs)?;
				let right = r.pop_pointer_tree(reps, level, ptrs)?;
				*self = reps.join(l, r);
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
			ReplaceTree::Branch(Link::new(&ReplaceTree::None), right) => write!(f, ">{}", right)?,
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
#[derive(PartialEq, Debug)]
pub struct ReplaceIndex<'a> {
	pub index: usize,
	pub tree: &'a ReplaceTree<'a>
}

impl<'a> ReplaceIndex<'a> {
	pub fn new(index: usize, tree: &'a ReplaceTree<'a>) -> Self { Self { index, tree } }
	pub fn split(&self) -> Result<(Self, Self), LambdaError> {
		let (left, right) = self.tree.split()?;
		Ok((ReplaceIndex::new(self.index, left), ReplaceIndex::new(self.index, right)))
	}
	/// Push PointerTree onto ReplaceIndex
	pub fn push_pointer_tree(&'a self, pointer: &Link<PointerTree>, reps: &'a LinkArena<'a>, ptrs: &'a LinkArena<'a>) -> Result<(), LambdaError> {
		let ReplaceIndex { index, tree } = self;
		*index += 1;
		tree.push_pointer_tree(self, *index, pointer, ptrs)?;
		Ok(())
	}
	/// Pop PointerTree from ReplaceIndex
	pub fn pop_pointer_tree(&'a mut self, reps: &'a LinkArena<'a>, ptrs: &'a LinkArena<'a>) -> Result<Link<PointerTree>, LambdaError> {
		let ReplaceIndex { index, tree } = self;
		if *index == 0 { return Err(LambdaError::PointerTreeMismatch) }
		let ret = tree.pop_pointer_tree(self, *index, ptrs)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build ReplaceIndex from Lambda Expression
	pub fn push_lambda(&'a mut self, expr: &'a Link<'a, Expr<'a>>, reps: &'a LinkArena<'a>, exprs: &'a LinkArena<'a>) -> Result<&'a Link<'a, Expr<'a>>, LambdaError> {
		Ok(if let Expr::Lambda { tree: pointer_tree, expr} = expr {
			let expr = self.push_lambda(self, expr, exprs)?;
			self.push_pointer_tree(pointer_tree, exprs)?;
			expr
		} else { &expr })
	}
	/// Creates nested lambda expression from given ReplaceTree
	pub fn pop_lambda(&'a mut self, expr: Link<Expr>, reps: &'a LinkArena<'a>, exprs: &'a LinkArena<'a>) -> Result<Link<Expr>, LambdaError> {
		let pointer_tree = self.pop_pointer_tree(reps, exprs)?;
		let expr = if self.index == 0 { Expr::lambda(pointer_tree, &expr) }
		else { Expr::lambda(pointer_tree, self.pop_lambda(expr, reps, exprs)?) };
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
	let r = &mut reps.index();
	println!("start: [{}]", r);

	let lambda = crate::parse::parse("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", reps).unwrap();
	println!("lambda: {}", lambda.display(reps));
	let expr = reps.push_lambda(r, &lambda, reps).unwrap();
	println!("after push: {} : {}", expr.display(reps), r);
	let lambda_2 = reps.pop_lambda(r, expr.clone(), reps).unwrap();
	println!("after pop: {}", lambda_2.display(reps));
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = reps.index();
	test_split(reps, r).unwrap();

	let pts = &LinkArena::new();
	let mut r = reps.index();
	reps.push_pointer_tree(&mut r, &PT::END, reps).unwrap();
	test_split(reps, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = reps.index();
	reps.push_pointer_tree(&mut r, &PT::left(PT::END, pts), pts).unwrap();
	test_split(reps, r).unwrap();

	let mut r = reps.index();
	reps.push_pointer_tree(&mut r, &PT::both(PT::right(PT::END, pts), PT::END, pts), pts).unwrap();
	test_split(reps, r).unwrap();
}

fn test_split<'a>(a: &'a LinkArena<'a>, r: ReplaceIndex<'a>) -> Result<(), LambdaError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e|{println!("split err: {}", e); e})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = a.join_index(left, right);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}