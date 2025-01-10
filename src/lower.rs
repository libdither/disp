#![allow(unused)]

use crate::parse::{ParseTree, ParseTreeSetItem};
use std::{
	collections::HashMap,
	hash::{Hash, Hasher},
};
use thiserror::Error;

/* new_key_type! { struct StackKey; }
new_key_type! { struct TermKey; } */

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum BuiltinType {
	Unit,
	Nat,
	String,
	Bool,
}

/// Represents both expressions and types.
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Term {
	UnknownType, // represents a type that is not-yet inferred

	Nat(u64),                // number literal
	String(String),          // string literal
	Bool(bool),              // boolean literal
	BuiltinTyp(BuiltinType), // default types
	Uni(usize),              // For managing the hierarchy of types. Type is Uni(0)

	Variable(String), // some variable bound in an expression (not used for func args)
	// basic constructs
	Set(Vec<TermHash>), // Association of ident with literal or type
	// both function and type of function
	Abs {
		param: Vec<(String, TermHash)>,
		body: TermHash,
	},
	// application of a function given some argument (usually a set)
	App {
		function: TermHash,
		argument: TermHash,
	},
	TypeApp {
		res: TermHash,
		func: TermHash,
	},
}

/// Type := ?
/// A := (T : Type) -> List{T} : (Type -> Type)
/// let Type := Type : ?
/// thing := 3
/// AssignIdentExpr {thing, 3, None}
/// TypedTerm{3, UnknownType}
///
/// `thing := 3; foo := "bar"`
/// ExprSet[AssignIdentExpr{thing, 3, None}, AIE{foo, "bar", None}]
/// Set[("thing", Nat(3)), ("foo", String("bar"))]
#[derive(PartialEq, Debug, Clone, Hash)]
pub struct TypedTerm {
	term: Term,
	typ: Term,
}
pub type TermHash = u64;

pub struct Context {
	terms: HashMap<TermHash, TypedTerm>,
}

impl Context {
	pub fn new() -> Self {
		Context { terms: HashMap::new() }
	}
	pub fn add_term(&mut self, term: TypedTerm) -> TermHash {
		use std::hash::BuildHasher;
		let hasher = self.terms.hasher();
		let mut hasher = hasher.build_hasher();
		term.hash(&mut hasher);
		let hash = hasher.finish();
		self.terms.insert(hash, term);
		hash
	}
}

#[derive(Debug, Error)]
pub enum LoweringError {
	#[error("Identifier not found: {0}")]
	UnknownIdentifier(String),
	#[error("Type mismatch: expected type {expected:?}, got expression of type {got:?}")]
	TypeMismatch { expected: TermHash, got: TermHash },
	#[error("Cannot infer type for the expression")]
	CannotInferType,
	#[error("Expected a type, but got an expression")]
	ExpectedTypeGotExpression, // Might be less relevant now
	#[error("Expected an expression, but got a type")]
	ExpectedExpressionGotType, // Might be less relevant now
	#[error("ExprSet must contain AssignIdentExpr, found: {0:?}")]
	InvalidElementsOfExprSet(ParseTree),
	#[error("TypeSet must contain AssignTypeExpr, found: {0:?}")]
	InvalidElementsOfTypeSet(ParseTree),
	#[error("AssignIdent(Expr/Type) should contain ident")]
	AssignIdentHasNoIdent,
}

/// Lowers a ParseTree into a partially-typed TypedTerm.
pub fn lower(tree: ParseTree, ctx: &mut Context) -> Result<TypedTerm, LoweringError> {
	match tree {
		ParseTree::Number(num) => Ok(TypedTerm {
			term: Term::Nat(num),
			typ: Term::BuiltinTyp(BuiltinType::Nat),
		}),
		ParseTree::String(string) => Ok(TypedTerm {
			term: Term::String(string),
			typ: Term::BuiltinTyp(BuiltinType::String),
		}),
		ParseTree::Ident(name) => Ok(TypedTerm {
			term: Term::Variable(name),
			typ: Term::UnknownType,
		}),
		ParseTree::Set(elems) => {
			let expr_set: Vec<TermHash> = elems
				.into_iter()
				.map::<Result<_, LoweringError>, _>(|set_item| try {
					let typed_term = match set_item {
						ParseTreeSetItem::Atom(atom) => lower(atom, ctx)?,
						ParseTreeSetItem::AssignIdentExpr { ident, def, typ } => TypedTerm {
							term: lower(*def, ctx)?.term,
							typ: if let Some(typ) = typ {
								lower(*typ, ctx)?.term
							} else {
								Term::UnknownType
							},
						},
						ParseTreeSetItem::AssignTypeIdent { ident, typ } => TypedTerm {
							term: Term::Variable(ident.clone()),
							typ: lower(*typ, ctx)?.term,
						},
					};
					ctx.add_term(typed_term)
				})
				.try_collect()?;
			let typ_set: Vec<TermHash> = expr_set
				.iter()
				.map(|hash| {
					ctx.terms
						.get(hash)
						.map(|_| hash.clone())
						.expect("context changed in course of parsing exprset, this shouldn't happen")
				})
				.collect();
			Ok(TypedTerm {
				term: Term::Set(expr_set),
				typ: Term::Set(typ_set),
			})
		}
		ParseTree::Func { args, body } => {
			todo!()
		}
		ParseTree::Apply { func, args } => {
			todo!()
		}
		_ => todo!(),
	}
}
