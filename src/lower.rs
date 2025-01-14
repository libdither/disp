use crate::parse::{ParseTree, ParseTreeArgSetItem, ParseTreeSetItem};
use core::fmt;
use itertools::Itertools;
use slotmap::{new_key_type, SlotMap};
use std::{cmp::Ordering, collections::HashMap, hash::Hash};
use thiserror::Error;

/* new_key_type! { struct StackKey; }
new_key_type! { struct TermKey; } */

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum BuiltinType {
	Unit,
	Nat,
	String,
	Bool,
}

/// Represents both expressions and types.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Term {
	UnknownType(HoleKey), // represents a type that is not yet inferred

	Nat(u64),                // number literal
	String(String),          // string literal
	Bool(bool),              // boolean literal
	BuiltinTyp(BuiltinType), // default types
	Uni(usize),              // For managing the hierarchy of types. Type is Uni(0)

	Variable(IdentKey), // some variable bound in an expression (not used for func args)
	// basic constructs
	Set(Vec<(Option<IdentKey>, TermKey)>), // Association of ident with literal or type
	// both function and type of function
	Abs {
		args: Vec<(IdentKey, TermKey)>, // bind name and type
		body: TermKey,
	},
	// application of a function given some argument (usually a set)
	App {
		func: TermKey,
		args: TermKey,
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
#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct TypedTerm {
	pub term: TermKey,
	pub typ: TermKey,
}
new_key_type! {pub struct TermKey;}
new_key_type! {pub struct IdentKey;}
new_key_type! {pub struct HoleKey;}

pub struct Context {
	// cons-hashing for idents and terms
	pub idents: SlotMap<IdentKey, String>,        // idents go here
	ident_map: HashMap<String, IdentKey>,         // deduplicating idents
	pub terms: SlotMap<TermKey, Term>,            // terms go here
	term_map: HashMap<Term, TermKey>,             // deduplicating terms
	pub holes: SlotMap<HoleKey, Option<TermKey>>, // holes go here
	pub binds: Vec<TermKey>,                      // lambdas or sets go here while parsing (for ident lookup)
}
impl fmt::Debug for Context {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Context")
			.field("idents", &self.idents)
			.field("terms", &self.terms)
			.finish()
	}
}

impl Context {
	pub fn new() -> Self {
		Context {
			idents: SlotMap::with_key(),
			ident_map: HashMap::new(),
			terms: SlotMap::with_key(),
			term_map: HashMap::new(),
			holes: SlotMap::with_key(),
			binds: Vec::new(),
		}
	}
	pub fn add_ident(&mut self, ident: String) -> IdentKey {
		if let Some(key) = self.ident_map.get(&ident) {
			key.clone()
		} else {
			self.idents.insert(ident)
		}
	}
	pub fn add_term(&mut self, term: Term) -> TermKey {
		if let Some(key) = self.term_map.get(&term) {
			key.clone()
		} else {
			self.terms.insert(term)
		}
	}
	pub fn add_hole(&mut self) -> HoleKey {
		self.holes.insert(None)
	}
	pub fn new_unknown_type(&mut self) -> TermKey {
		let hole = self.add_hole();
		self.add_term(Term::UnknownType(hole))
	}
	pub fn fmt_term<'c>(&'c self, term: TermKey) -> FormatTerm<'c> {
		FormatTerm { ctx: self, term }
	}
	pub fn fmt_ident<'c>(&'c self, ident: IdentKey) -> FormatIdent<'c> {
		FormatIdent { ctx: self, ident }
	}
}
pub struct FormatIdent<'c> {
	ctx: &'c Context,
	ident: IdentKey,
}
impl<'c> fmt::Debug for FormatIdent<'c> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(ident) = self.ctx.idents.get(self.ident) {
			write!(f, "{ident:?}")
		} else {
			write!(f, "{:?}", self.ident)
		}
	}
}
impl<'c> fmt::Display for FormatIdent<'c> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(ident) = self.ctx.idents.get(self.ident) {
			write!(f, "{ident}")
		} else {
			write!(f, "{:?}", self.ident)
		}
	}
}

pub struct FormatTerm<'c> {
	ctx: &'c Context,
	term: TermKey,
}
impl<'c> fmt::Debug for FormatTerm<'c> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(term) = self.ctx.terms.get(self.term) {
			match term {
				Term::Variable(ident_key) => f
					.debug_tuple("Variable")
					.field(&self.ctx.fmt_ident(*ident_key))
					.finish(),
				Term::Set(vec) => f
					.debug_tuple("Set")
					.field(
						&vec.iter()
							.map(|(ident, term)| {
								(ident.map(|ident| self.ctx.fmt_ident(ident)), self.ctx.fmt_term(*term))
							})
							.collect::<Vec<(Option<FormatIdent>, FormatTerm)>>(),
					)
					.finish(),
				Term::Abs { args, body } => f
					.debug_struct("Abs")
					.field(
						"args",
						&args
							.iter()
							.map(|(ident, term)| (self.ctx.fmt_ident(*ident), self.ctx.fmt_term(*term)))
							.collect::<Vec<(FormatIdent, FormatTerm)>>(),
					)
					.field("body", &self.ctx.fmt_term(*body))
					.finish(),
				Term::App { func, args } => f
					.debug_struct("App")
					.field("func", &self.ctx.fmt_term(*func))
					.field("args", &self.ctx.fmt_term(*args))
					.finish(),
				_ => write!(f, "{:?}", term),
			}
		} else {
			write!(f, "{:?}", self.term)
		}
	}
}
impl<'c> fmt::Display for FormatTerm<'c> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let ctx = self.ctx;
		let Some(term) = ctx.terms.get(self.term) else {
			write!(f, "{:?}", self.term);
			return Ok(());
		};
		match term {
			Term::UnknownType(_) => write!(f, "?"),
			Term::Nat(nat) => write!(f, "{nat}"),
			Term::String(string) => write!(f, "{string:?}"),
			Term::Bool(bool) => write!(f, "{bool}"),
			Term::BuiltinTyp(builtin_type) => write!(f, "{builtin_type:?}"),
			Term::Uni(u) => {
				if *u == 0 {
					write!(f, "Type")
				} else {
					write!(f, "U{u}")
				}
			}
			Term::Variable(ident_key) => {
				if let Some(ident) = ctx.idents.get(*ident_key) {
					write!(f, "{ident}")
				} else {
					write!(f, "{ident_key:?}")
				}
			}
			Term::Set(vec) => {
				write!(f, "{{")?;
				for (ident_key, term_key) in vec.iter() {
					if let Some(key) = ident_key {
						if let Some(ident) = ctx.idents.get(*key) {
							write!(f, "{ident}")?
						} else {
							write!(f, "{key:?}")?
						}
						write!(f, " : ")?
					}
					write!(f, "{}, ", ctx.fmt_term(*term_key))?;
				}
				write!(f, "}}")
			}
			Term::Abs { args, body } => {
				write!(f, "{{")?;
				for (ident_key, typ_key) in args.iter() {
					write!(f, "{:?} : {}, ", ctx.fmt_ident(*ident_key), ctx.fmt_term(*typ_key))?;
				}
				write!(f, "}}")?;
				write!(f, " -> {}", ctx.fmt_term(*body))
			}
			Term::App { func, args } => write!(f, "({} {})", ctx.fmt_term(*func), ctx.fmt_term(*args)),
		}
	}
}

#[derive(Debug, Error)]
pub enum LoweringError {
	#[error("Identifier not found: {0}")]
	UnknownIdentifier(String),
	#[error("Type mismatch: expected type {expected:?}, got expression of type {got:?}")]
	TypeMismatch { expected: TermKey, got: TermKey },
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
			term: ctx.add_term(Term::Nat(num)),
			typ: ctx.add_term(Term::BuiltinTyp(BuiltinType::Nat)),
		}),
		ParseTree::String(string) => Ok(TypedTerm {
			term: ctx.add_term(Term::String(string)),
			typ: ctx.add_term(Term::BuiltinTyp(BuiltinType::String)),
		}),
		ParseTree::Ident(name) => {
			let key = ctx.add_ident(name);
			let hole = ctx.add_hole();
			Ok(TypedTerm {
				term: ctx.add_term(Term::Variable(key)),
				typ: ctx.add_term(Term::UnknownType(hole)),
			})
		}
		ParseTree::Set(elems) => {
			let mut items: Vec<(Option<IdentKey>, TermKey, TermKey)> = elems
				.into_iter()
				.map::<Result<_, LoweringError>, _>(|set_item| try {
					match set_item {
						ParseTreeSetItem::Atom(atom) => {
							let typed = lower(atom, ctx)?;
							(None, typed.term, typed.typ)
						}
						ParseTreeSetItem::AssignIdentExpr { ident, def, typ } => {
							let ident = ctx.add_ident(ident);
							let typed = TypedTerm {
								term: lower(*def, ctx)?.term,
								typ: if let Some(typ) = typ {
									lower(*typ, ctx)?.term
								} else {
									let hole = ctx.add_hole();
									ctx.add_term(Term::UnknownType(hole))
								},
							};
							(Some(ident), typed.term, typed.typ)
						}
						ParseTreeSetItem::AssignTypeIdent { ident, typ } => {
							let ident = ctx.add_ident(ident);
							let typed = TypedTerm {
								term: ctx.add_term(Term::Variable(ident)),
								typ: lower(*typ, ctx)?.term,
							};
							(Some(ident), typed.term, typed.typ)
						}
					}
				})
				.try_collect()?;
			items.sort_by(|a, b| {
				match (
					a.0.map(|a| ctx.idents.get(a)).flatten(),
					b.0.map(|b| ctx.idents.get(b)).flatten(),
				) {
					(None, None) => Ordering::Equal,
					(None, Some(_)) => Ordering::Less,
					(Some(_), None) => Ordering::Greater,
					(Some(sa), Some(sb)) => sa.cmp(sb),
				}
			});
			Ok(TypedTerm {
				term: ctx.add_term(Term::Set(
					items
						.iter()
						.map(|(ident, term, _)| (ident.clone(), term.clone()))
						.collect(),
				)),
				typ: ctx.add_term(Term::Set(
					items
						.iter()
						.map(|(ident, _, typ)| (ident.clone(), typ.clone()))
						.collect(),
				)),
			})
		}
		ParseTree::Func { args, body } => {
			let mut args: Vec<(IdentKey, TermKey)> = args
				.into_iter()
				.map::<Result<_, LoweringError>, _>(|set_item| try {
					let ParseTreeArgSetItem { ident, typ } = set_item;
					let ident = ctx.add_ident(ident);
					let typ = if let Some(typ) = typ {
						lower(*typ, ctx)?.term
					} else {
						let hole = ctx.add_hole();
						ctx.add_term(Term::UnknownType(hole))
					};
					(ident, typ)
				})
				.try_collect()?;
			args.sort_by_key(|a| ctx.idents.get(a.0));
			let typed_body = lower(*body, ctx)?;
			Ok(TypedTerm {
				term: ctx.add_term(Term::Abs {
					args: args.clone(),
					body: typed_body.term,
				}),
				typ: ctx.add_term(Term::Abs {
					args,
					body: typed_body.typ,
				}),
			})
		}
		ParseTree::Apply { func, args } => {
			let func = lower(*func, ctx)?;
			let args = lower(*args, ctx)?;
			Ok(TypedTerm {
				term: ctx.add_term(Term::App {
					func: func.term,
					args: args.term,
				}),
				typ: ctx.add_term(Term::App {
					func: func.typ,
					args: args.typ,
				}),
			})
		}
	}
}
