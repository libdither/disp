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

use crate::tree_parse::{self, TreeExpr};

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
	fn replace(&self, key: TreeTermKey) -> Self {
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
			if let Some(string) = self.store.term_ident.get(&self.key) {
				return write!(f, "{}", string);
			}
		}

		match self.store.get_term(self.key) {
			Some(TreeTerm::Leaf) => write!(f, "△"),
			Some(TreeTerm::Stem(key)) => write!(f, "Stem({})", self.replace(*key)),
			Some(TreeTerm::Fork { left, right }) => {
				// Basic parenthesizing for clarity, could be made smarter
				write!(f, "({} {})", self.replace(*left), self.replace(*right))
			}
			None => write!(f, "<invalid_key>"),
		}
	}
}

pub struct TermStore {
	terms: SlotMap<TreeTermKey, TreeTerm>,
	cache: HashMap<TreeTermKey, TreeTermKey>, // For normalize memoization
	canonical_leaf: Option<TreeTermKey>,      // To have one canonical Leaf if needed
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
		let mut store = TermStore {
			terms: SlotMap::with_key(),
			cache: HashMap::new(),
			canonical_leaf: None,
			ident_term: HashMap::new(),
			term_ident: HashMap::new(),
		};
		// Ensure there's a canonical leaf node if we need to refer to "the" Leaf
		let leaf_key = store.terms.insert(TreeTerm::Leaf);
		store.canonical_leaf = Some(leaf_key);
		store
	}

	pub fn lower_assign(&mut self, assign: (String, TreeExpr)) -> Result<TreeTermKey, TreeLoweringError> {
		let ident = assign.0;
		let term = self.lower(assign.1)?;
		// only reduced terms should be recorded with associated ident maps
		self.ident_term.insert(ident.clone(), term);
		self.term_ident.insert(term, ident.clone());
		Ok(term)
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
			TreeExpr::Leaf => Ok(self.leaf()),
			TreeExpr::App(left, right) => {
				let left_key = self.lower(*left)?;
				let right_key = self.lower(*right)?;
				let fork = self.fork(left_key, right_key);
				Ok(self.reduce(fork))
			}
		}
	}

	fn get_term(&self, key: TreeTermKey) -> Option<&TreeTerm> {
		self.terms.get(key)
	}

	pub fn leaf(&mut self) -> TreeTermKey {
		// Could return canonical_leaf if we want all leaves to be identical by key
		// For structural distinction and caching different "instances" of leaves initially,
		// inserting a new one is fine. Tree calculus's Δ is unique though.
		// Let's use the canonical one for simplicity, matching the single Δ concept.
		self.canonical_leaf.unwrap()
	}

	pub fn stem(&mut self, key: TreeTermKey) -> TreeTermKey {
		self.terms.insert(TreeTerm::Stem(key))
	}

	pub fn fork(&mut self, left: TreeTermKey, right: TreeTermKey) -> TreeTermKey {
		// Optional: Interning for branches too for structural sharing
		// For now, always create a new one. Caching normalize handles redundancy.
		self.terms.insert(TreeTerm::Fork { left, right })
	}

	pub fn display_term(&self, key: TreeTermKey, to_ident: bool) -> TermDisplayer<'_> {
		TermDisplayer {
			key,
			store: self,
			to_ident,
		}
	}
	// reduces a term
	pub fn reduce(&mut self, term: TreeTermKey) -> TreeTermKey {
		// lookup to see if already been reduced in past
		let result = match if let Some(cached_key) = self.cache.get(&term) {
			return *cached_key;
		} else {
			self.terms.get(term).expect("invalid key").clone()
		} {
			TreeTerm::Fork { left, right } => self.reduce_fork(left, right),
			_ => term,
		};

		println!(
			"reducing: {} -> {}",
			self.display_term(term, false),
			self.display_term(result, false)
		);
		self.cache.insert(term, result);
		result
	}
	fn reduce_fork(&mut self, func: TreeTermKey, arg: TreeTermKey) -> TreeTermKey {
		/*
		   let rec apply a b =
			 match a with
			 | Leaf -> Stem b
			 | Stem a -> Fork (a, b)
			 | Fork (Leaf, a3) -> a3
			 | Fork (Stem a1, a2) -> apply (apply a1 b) (apply a2 b)
			 | Fork (Fork (a1, a2), a3) ->
			 match b with
			 | Leaf -> a1
			 | Stem u -> apply a2 u
			 | Fork (u, v) -> apply (apply a3 u) v
		*/
		// get result
		let result_key = match self.get_term(func).cloned() {
			Some(TreeTerm::Leaf) => self.stem(arg),
			Some(TreeTerm::Stem(a)) => self.fork(a, arg),
			Some(TreeTerm::Fork { left: a, right: a3 }) => match self.get_term(a).cloned() {
				Some(TreeTerm::Leaf) => a3,
				Some(TreeTerm::Stem(a1)) => {
					let app1 = self.reduce_fork(a1, arg);
					let app2 = self.reduce_fork(a3, arg);
					self.fork(app1, app2)
				}
				Some(TreeTerm::Fork { left: a1, right: a2 }) => match self.get_term(arg).cloned() {
					Some(TreeTerm::Leaf) => a1,
					Some(TreeTerm::Stem(u)) => self.reduce_fork(a2, u),
					Some(TreeTerm::Fork { left: u, right: v }) => {
						let app1 = self.reduce_fork(a3, u);
						self.reduce_fork(app1, v)
					}
					None => panic!("invalid key"),
				},
				None => panic!("invalid key"),
			},
			None => panic!("invalid key"),
		};
		result_key
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_apply() {
		let mut s = TermStore::new();
		let leaf = s.leaf();
		let _false = s.leaf();
		println!("_false: {}", s.display_term(_false, false));

		let _true = s.stem(_false);
		println!("_true: {}", s.display_term(_true, false));

		let fork_leaf_false = s.fork(leaf, _false);
		let fork_true_fork_leaf_false = s.fork(_true, fork_leaf_false);
		let _not = s.fork(fork_true_fork_leaf_false, leaf);

		let not_false = s.fork(_not, _false);
		let app1 = s.reduce(not_false);
		println!("NOT False: {}", s.display_term(app1, false));
		assert_eq!(app1, _true);
		let app2 = s.reduce_fork(_not, _true);
		println!("NOT True: {}", s.display_term(app2, false));
		assert_eq!(app2, _false);
	}
}
