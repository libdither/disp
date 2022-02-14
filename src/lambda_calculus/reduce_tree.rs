///! ReplaceTrees allow for more easy manipulation of Lambda abstractions during beta reduction and creation

use std::{fmt, ptr::NonNull};

use hashdb::NativeHashtype;
use typed_arena::Arena;

use crate::{Datastore, TypedHash, lambda_calculus::{ArchivedExpr, ArchivedPointerTree, DisplayWithDatastore, Expr, LambdaError, PT_NONE, PointerTree}};

#[derive(Clone, PartialEq, Debug)]
pub enum ReplaceTree<'a> {
	None,
	End(usize),
	Branch(&'a ReplaceTree<'a>, &'a ReplaceTree<'a>),
}

impl<'a> ReplaceTree<'a> {
	pub fn split(&'a self) -> Result<(&'a ReplaceTree<'a>, &'a ReplaceTree<'a>), LambdaError> {
		Ok(match self {
			ReplaceTree::Branch(l, r) => (l, r),
			ReplaceTree::None => (self, self),
			ReplaceTree::End(_) => Err(LambdaError::ReduceArenaMismatch)?
		})
	}
	fn join(left: &'a Self, right: &'a Self) -> Self {
		if let (ReplaceTree::None, ReplaceTree::None) = (left, right) { ReplaceTree::None }
		else { ReplaceTree::Branch(left, right) }
	}
	/// Add PointerTree to ReplaceTree at certain abstraction level
	fn push_pointer_tree(self: &mut &'a Self, arena: &'a ReduceArena<'a>, level: usize, pointer: &TypedHash<PointerTree>, db: &Datastore) -> Result<(), LambdaError> {
		*self = match (&self, pointer.fetch(db)?) {
			// If ReplaceTree is None, fill in pointer
			(tree, ArchivedPointerTree::None) => tree,
			(ReplaceTree::None, ArchivedPointerTree::End) => arena.end(level),
			(ReplaceTree::None, ArchivedPointerTree::Branch(l, r)) => {
				let (mut left, mut right) = (arena.none(), arena.none());
				left.push_pointer_tree(arena, level, l, db)?;
				right.push_pointer_tree(arena, level, r, db)?;
				arena.join(left, right)
			}
			(ReplaceTree::Branch(mut left, mut right), ArchivedPointerTree::Branch(l, r)) => {
				left.push_pointer_tree(arena, level, l, db)?;
				right.push_pointer_tree(arena, level, r, db)?;
				arena.join(left, right)
			}
			(ReplaceTree::End(_), _) | (_, ArchivedPointerTree::End) => Err(LambdaError::ReduceArenaMismatch)?
		};
		Ok(())
	}
	/// Constructs PointerTree from ReplaceTree at certain abstraction level
	fn pop_pointer_tree(self: &mut &'a Self, arena: &'a ReduceArena<'a>, level: usize, db: &mut Datastore) -> Result<TypedHash<PointerTree>, LambdaError> {
		Ok(match self {
			ReplaceTree::Branch(mut l, mut r) => {
				let left = l.pop_pointer_tree(arena, level, db)?;
				let right = r.pop_pointer_tree(arena, level, db)?;
				*self = arena.join(l, r);
				if left == *PT_NONE && right == *PT_NONE { PointerTree::None.store(db) }
				else { PointerTree::Branch(left, right).store(db) }
			},
			ReplaceTree::End(count) if level == *count =>  {
				*self = arena.none();
				PointerTree::End.store(db)
			},
			_ => PointerTree::None.store(db),
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
}

impl<'a> fmt::Display for ReplaceIndex<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}/{}", self.tree, self.index)
	}
}

pub struct ReduceArena<'a> {
	arena: Arena<ReplaceTree<'a>>,
	none: NonNull<ReplaceTree<'a>>,
}
impl<'a> ReduceArena<'a> {
	pub fn new() -> Self {
		let arena = Arena::new();
		let mut ret = Self { arena, none: NonNull::dangling() };
		ret.none = NonNull::from(ret.arena.alloc(ReplaceTree::None));
		ret
	}

	// Safety: Arena's underlying memory never moves while it exists, thus self.none pointer should never become invalid.
	// Safety: This pointer will also never outlive ReduceArena because the reference has the same lifetime as the Arena
	pub fn none(&'a self) -> &'a ReplaceTree<'a> { unsafe { self.none.as_ref()} }
	pub fn end(&'a self, num: usize) -> &'a ReplaceTree<'a> { self.arena.alloc(ReplaceTree::End(num)) }
	pub fn join(&'a self, left: &'a ReplaceTree<'a>, right: &'a ReplaceTree<'a>) -> &'a ReplaceTree<'a> {
		self.arena.alloc(ReplaceTree::join(left, right))
	}
	pub fn join_index(&'a self, left: ReplaceIndex<'a>, right: ReplaceIndex<'a>) -> ReplaceIndex<'a> {
		debug_assert_eq!(left.index, right.index);
		ReplaceIndex::new(left.index, self.join(left.tree, right.tree))
	}
	pub fn left(&'a self, tree: &'a ReplaceTree<'a>) -> &'a ReplaceTree<'a> { self.join(tree, self.none()) }
	pub fn right(&'a self, tree: &'a ReplaceTree<'a>) -> &'a ReplaceTree<'a> { self.join(self.none(), tree) }
	pub fn index(&'a self) -> ReplaceIndex<'a> { ReplaceIndex { index: 0, tree: self.none() } }

	/// Push PointerTree onto ReplaceIndex
	pub fn push_pointer_tree(&'a self, replace_index: &mut ReplaceIndex<'a>, pointer: &TypedHash<PointerTree>, db: &Datastore) -> Result<(), LambdaError> {
		let ReplaceIndex { index, tree } = replace_index;
		*index += 1;
		tree.push_pointer_tree(self, *index, pointer, db)?;
		Ok(())
	}
	/// Pop PointerTree from ReplaceIndex
	pub fn pop_pointer_tree(&'a self, replace_index: &mut ReplaceIndex<'a>, db: &mut Datastore) -> Result<TypedHash<PointerTree>, LambdaError> {
		let ReplaceIndex { index, tree } = replace_index;
		if *index == 0 { return Err(LambdaError::ReduceArenaMismatch) }
		let ret = tree.pop_pointer_tree(self, *index, db)?;
		*index -= 1;
		Ok(ret)
	}
	/// Build ReplaceIndex from Lambda Expression
	pub fn push_lambda<'b>(&'a self, tree: &mut ReplaceIndex<'a>, expr: &'b TypedHash<Expr>, db: &'b Datastore) -> Result<&'b TypedHash<Expr>, LambdaError> {
		Ok(if let ArchivedExpr::Lambda { tree: pointer_tree, expr} = expr.fetch(db)? {
			let expr = self.push_lambda(tree, expr, db)?;
			self.push_pointer_tree(tree, pointer_tree, db)?;
			expr
		} else { &expr })
	}
	/// Creates nested lambda expression from given ReplaceTree
	pub fn pop_lambda(&'a self, tree: &mut ReplaceIndex<'a>, expr: TypedHash<Expr>, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
		let pointer_tree = self.pop_pointer_tree(tree, db)?;
		Ok(if tree.index == 0 { Expr::Lambda { tree: pointer_tree, expr } }
		else {
			Expr::Lambda { tree: pointer_tree, expr: self.pop_lambda(tree, expr, db)? }
		}.store(db))
	}
}

#[test]
fn test_replace_tree() {
	let db = &mut Datastore::new();
	let a = & ReduceArena::new();
	let r = &mut a.index();
	println!("start: [{}]", r);

	let lambda = crate::parse_expr("(λ[] (λ[><<.] (λ[(.,<>.)] (λ[>>.] (x ((x x) x))))))", db).unwrap();
	println!("lambda: {}", lambda.display(db));
	let expr = a.push_lambda(r, &lambda, db).unwrap();
	println!("after push: {} : {}", expr.display(db), r);
	let lambda_2 = a.pop_lambda(r, expr.clone(), db).unwrap();
	println!("after pop: {}", lambda_2.display(db));
	assert_eq!(lambda, lambda_2);

	// Test Split & Join
	let r = a.index();
	test_split(a, r).unwrap();

	let mut r = a.index();
	a.push_pointer_tree(&mut r, &end(db), db).unwrap();
	test_split(a, r).unwrap_err();

	// let r = &mut ReduceArena(, 0);
	// test_split(r, db).unwrap(); // This will error
	let mut r = a.index();
	a.push_pointer_tree(&mut r, &left(end(db), db), db).unwrap();
	test_split(a, r).unwrap();

	let mut r = a.index();
	a.push_pointer_tree(&mut r, &both(right(end(db), db), end(db), db), db).unwrap();
	test_split(a, r).unwrap();
}

fn test_split<'a>(a: &'a ReduceArena<'a>, r: ReplaceIndex<'a>) -> Result<(), LambdaError> {
	print!("split [{}] ", r);
	let (left, right) = r.split().map_err(|e|{println!("split err: {}", e); e})?;
	print!(" - ([{}] [{}])", left, right);
	let r_after = a.join_index(left, right);
	println!(" = [{}]", r);
	assert_eq!(r, r_after);
	Ok(())
}