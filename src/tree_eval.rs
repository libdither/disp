/*
This is a reference implementation of Tree Calculus in rust.
Use this as reference to generate an evaluation program in rust.
type t =
  | Leaf
  | Stem of t
  | Fork of t * t

let rec apply a b =
  match a with
  | Leaf -> Stem b
  | Stem a -> Fork (a, b)
  | Fork (Leaf, a) -> a
  | Fork (Stem a1, a2) -> apply (apply a1 b) (apply a2 b)
  | Fork (Fork (a1, a2), a3) ->
	match b with
	| Leaf -> a1
	| Stem u -> apply a2 u
	| Fork (u, v) -> apply (apply a3 u) v

(* Example: Negating booleans *)
let _false = Leaf
let _true = Stem Leaf
let _not = Fork (Fork (_true, Fork (Leaf, _false)), Leaf)
apply _not _false (* Stem Leaf = _true  *)
apply _not _true  (* Leaf      = _false *)

*/
use slotmap::{new_key_type, SlotMap};
use std::{collections::HashMap, fmt};

use crate::tree_parse::TreeExpr;

// TermKey will be our reference to terms in the SlotMap
new_key_type! { pub struct TreeTermKey; }

#[derive(Clone, PartialEq, Eq, Hash)]
enum TreeTerm {
	Leaf,
	Stem(TreeTermKey),
	Fork { left: TreeTermKey, right: TreeTermKey },
}

// For display purposes
pub struct TermDisplayer<'a> {
	key: TreeTermKey,
	store: &'a TermStore,
	to_ident: bool,
}
impl<'a> TermDisplayer<'a> {
	fn with(&self, key: TreeTermKey) -> Self {
		Self {
			key,
			store: self.store,
			to_ident: self.to_ident,
		}
	}
}

impl<'a> fmt::Display for TermDisplayer<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.to_ident {
			// if we should be looking up idents, lookup ident name and return
			if let Some(string) = self.store.term_ident.get(&self.key) {
				return write!(f, "{}", string);
			} /* else {
				 write!(
					 f,
					 "[FIL: {}]",
					 Self {
						 key: self.key,
						 store: self.store,
						 to_ident: false
					 }
				 )?;
			 } */
		}

		match self.store.get_term(self.key) {
			Some(TreeTerm::Leaf) => write!(f, "△"),
			Some(TreeTerm::Stem(key)) => write!(f, "Stem({})", self.with(key)),
			Some(TreeTerm::Fork { left, right }) => {
				write!(f, "({} {})", self.with(left), self.with(right))
			}
			None => write!(f, "<invalid_key>"),
		}
	}
}

pub struct TermStore {
	terms: SlotMap<TreeTermKey, TreeTerm>,
	dedup: HashMap<TreeTerm, TreeTermKey>,                   // dedup terms
	cache: HashMap<(TreeTermKey, TreeTermKey), TreeTermKey>, // Cache reduction of two normal trees
	pub leaf: TreeTermKey,                                   // To have one canonical Leaf if needed
	ident_term: HashMap<String, TreeTermKey>,
	term_ident: HashMap<TreeTermKey, String>,
}

// write some code that takes a tree_parse::TreeExpr

#[derive(Debug, thiserror::Error)]
pub enum TreeLoweringError {
	#[error("unknown identifier: {0}")]
	UnknownIdent(String),
}

impl TermStore {
	pub fn new() -> Self {
		let mut terms = SlotMap::with_key();
		let leaf = terms.insert(TreeTerm::Leaf);
		TermStore {
			terms,
			dedup: HashMap::new(),
			cache: HashMap::new(),
			leaf,
			ident_term: HashMap::new(),
			term_ident: HashMap::new(),
		}
	}
	pub fn iter_idents(&self) -> impl Iterator<Item = (&String, &TreeTermKey)> {
		self.ident_term.iter()
	}

	pub fn lower_assign(&mut self, assign: (String, TreeExpr)) -> Result<TreeTermKey, TreeLoweringError> {
		let ident = assign.0;
		let term = self.lower(assign.1)?;
		// let reduced_term = self.reduce(term); // reduce top-level fork
		// only reduced terms should be recorded with associated ident maps
		self.ident_term.insert(ident.clone(), term);
		self.term_ident.insert(term, ident.clone());
		Ok(term)
	}
	pub fn lower_and_reduce(&mut self, expr: TreeExpr) -> Result<TreeTermKey, TreeLoweringError> {
		let term = self.lower(expr)?;
		Ok(match self.get_term(term).unwrap() {
			TreeTerm::Fork { left, right } => self.apply(left, right),
			_ => term,
		})
	}
	pub fn lower(&mut self, expr: TreeExpr) -> Result<TreeTermKey, TreeLoweringError> {
		match expr {
			TreeExpr::Ident(ident) => {
				if let Some(key) = self.ident_term.get(&ident) {
					Ok(*key)
				} else {
					Err(TreeLoweringError::UnknownIdent(ident))
				}
			}
			TreeExpr::Leaf => Ok(self.leaf),
			TreeExpr::Stem(expr) => {
				let key = self.lower(*expr)?;
				Ok(self.stem(key))
			}
			TreeExpr::Fork(left, right) => {
				let left = self.lower(*left)?;
				let right = self.lower(*right)?;
				let fork = self.fork(left, right);
				Ok(fork)
			}
		}
	}

	fn get_term(&self, key: TreeTermKey) -> Option<TreeTerm> {
		self.terms.get(key).cloned()
	}

	fn add_term(&mut self, term: TreeTerm) -> TreeTermKey {
		if let Some(cached_key) = self.dedup.get(&term) {
			return *cached_key;
		}
		let key = self.terms.insert(term.clone());
		self.dedup.insert(term, key);
		key
	}

	pub fn stem(&mut self, key: TreeTermKey) -> TreeTermKey {
		self.add_term(TreeTerm::Stem(key))
	}

	pub fn fork(&mut self, left: TreeTermKey, right: TreeTermKey) -> TreeTermKey {
		// Optional: Interning for branches too for structural sharing
		// For now, always create a new one. Caching normalize handles redundancy.
		self.add_term(TreeTerm::Fork { left, right })
	}

	pub fn display_term(&self, key: TreeTermKey, to_ident: bool) -> TermDisplayer<'_> {
		TermDisplayer {
			key,
			store: self,
			to_ident,
		}
	}
	fn apply(&mut self, func: TreeTermKey, arg: TreeTermKey) -> TreeTermKey {
		// check for cached eval
		if let Some(cached_key) = self.cache.get(&(func, arg)) {
			println!(
				"found cache: apply({}, {}) -> {}",
				self.display_term(func, false),
				self.display_term(arg, false),
				self.display_term(*cached_key, false)
			);
			return *cached_key;
		}
		// get result
		let result_key = match self.get_term(func) {
			Some(TreeTerm::Leaf) => self.stem(arg),       // 0a: (t a) => Stem(a)
			Some(TreeTerm::Stem(a)) => self.fork(a, arg), // 0b: ((t a) b) => (a b)
			Some(TreeTerm::Fork { left: a, right: a3 }) => match self.get_term(a) {
				Some(TreeTerm::Leaf) => a3, // ((t a3) arg) => a3
				Some(TreeTerm::Stem(a1)) => {
					// (t a) b => (a b)
					let app1 = self.apply(a1, arg);
					let app2 = self.apply(a3, arg);
					self.fork(app1, app2)
				}
				Some(TreeTerm::Fork { left: a1, right: a2 }) => match self.get_term(arg) {
					Some(TreeTerm::Leaf) => a1,
					Some(TreeTerm::Stem(u)) => self.apply(a2, u),
					Some(TreeTerm::Fork { left: u, right: v }) => {
						let app1 = self.apply(a3, u);
						self.apply(app1, v)
					}
					None => panic!("invalid key"),
				},
				None => panic!("invalid key"),
			},
			None => panic!("invalid key"),
		};

		println!(
			"reduced: apply({}, {}) -> {}",
			self.display_term(func, false),
			self.display_term(arg, false),
			self.display_term(result_key, false),
		);
		result_key
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_apply() {
		let mut s = TermStore::new();
		let _false = s.leaf;
		println!("_false: {}", s.display_term(_false, false));

		let _true = s.stem(_false);
		println!("_true: {}", s.display_term(_true, false));

		let fork_leaf_false = s.fork(s.leaf, _false);
		let fork_true_fork_leaf_false = s.fork(_true, fork_leaf_false);
		let _not = s.fork(fork_true_fork_leaf_false, s.leaf);

		let app1 = s.apply(_not, _false);
		println!("NOT False: {}", s.display_term(app1, false));
		assert_eq!(app1, _true);
		let app2 = s.apply(_not, _true);
		println!("NOT True: {}", s.display_term(app2, false));
		assert_eq!(app2, _false);
	}
}
