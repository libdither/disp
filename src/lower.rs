use crate::parse::{ParseTree, ParseTreeArgSetItem, ParseTreeSetItem};
use core::fmt;
use itertools::Itertools;
use slotmap::{new_key_type, SlotMap};
use std::{collections::HashMap, hash::Hash};
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
	pub top_binds: Vec<(IdentKey, TermKey)>,      // active while-parsing bindings and globals
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
			top_binds: Vec::new(),
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
	pub fn register_binding(&mut self, term: TermKey) {
		self.binds.push(term)
	}
	pub fn register_active(&mut self, ident: IdentKey, typ: TermKey) {
		self.top_binds.push((ident, typ));
	}
	pub fn find_bind_type(&self, ident: IdentKey) -> Option<TermKey> {
		self.top_binds
			.iter()
			.rev()
			.find(|(idt, _)| *idt == ident)
			.map(|(_, t)| *t)
			.or_else(|| {
				self.binds.iter().rev().find_map(|&binding| {
					let bind = self.terms.get(binding)?;
					match bind {
						Term::Abs { args, body: _ } => {
							args.iter().find(|(found_ident, _)| *found_ident == ident).map(|a| a.1)
						}
						Term::Set(args) => args
							.iter()
							.find(|(found_ident, _)| *found_ident == Some(ident))
							.map(|a| a.1),
						_ => None,
					}
				})
			})
	}
	pub fn find_bind_type_term(&self, ident: IdentKey) -> Result<Term, LoweringError> {
		self.find_bind_type(ident)
			.and_then(|key| self.terms.get(key))
			.cloned()
			.ok_or(LoweringError::IdentNotBoundToType(ident))
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
	#[error("Type mismatch: expected type {expected:?}, got expression of type {got:?}")]
	TypeMismatch { expected: &'static str, got: Term },
	#[error("Type mismatch: expected type {expected:?}, got expression of type {got:?}")]
	TypeLookupMismatch { expected: Term, got: Term },
	#[error("Binding not found in context: {0:?}")]
	IdentNotBound(IdentKey),
	#[error("Type binding not found in context for: {0:?}")]
	IdentNotBoundToType(IdentKey),
}

// "best guess" infer type from term
pub fn infer(term: &Term, ctx: &mut Context) -> Option<TermKey> {
	Some(match term {
		Term::UnknownType(_) => ctx.new_unknown_type(),
		Term::Nat(_) => ctx.add_term(Term::BuiltinTyp(BuiltinType::Nat)),
		Term::String(_) => ctx.add_term(Term::BuiltinTyp(BuiltinType::String)),
		Term::Bool(_) => ctx.add_term(Term::BuiltinTyp(BuiltinType::Bool)),
		Term::BuiltinTyp(_) => ctx.add_term(Term::Uni(0)),
		Term::Uni(i) => ctx.add_term(Term::Uni(i + 1)),
		Term::Variable(ident_key) => return ctx.find_bind_type(*ident_key),
		// Test:
		// {n := 3, l := }
		// Term::Set(vec) => ctx.add_term(Term::Set(vec.iter().map(|a|))),
		/* Term::Abs { args, body } => Term::Abs {
			args: ctx.new_unknown_type(),
			body: ctx.new_unknown_type(),
		},
		Term::App { func, args } => Term::App {
			func: ctx.new_unknown_type(),
			args: ctx.new_unknown_type(),
		}, */
		_ => None?,
	})
}

/* // takes a parse tree and lowers it into a term and potentially a parsed type.
pub fn lower(tree: ParseTree, ctx: &mut Context) -> Result<(Term, Option<Term>), LoweringError> {
	Ok(match tree {
		ParseTree::Number(num) => (Term::Nat(num), None),
		ParseTree::String(string) => (Term::String(string), None),
		ParseTree::Ident(name) => (
			match &name[..] {
				"true" => Term::Bool(true),
				"false" => Term::Bool(false),
				"Bool" => Term::BuiltinTyp(BuiltinType::Bool),
				"Nat" => Term::BuiltinTyp(BuiltinType::Nat),
				"String" => Term::BuiltinTyp(BuiltinType::String),
				_ => Term::Variable(ctx.add_ident(name)),
			},
			None,
		),
		ParseTree::Set(vec) => todo!(),
		ParseTree::Func { args, body } => todo!(),
		ParseTree::Apply { func, args } => todo!(),
	})
} */

/* pub fn unify_typ_from_key(
	typ1: Option<TermKey>,
	typ2: Option<TermKey>,
	ctx: &mut Context,
) -> Result<Option<Term>, LoweringError> {
	let typ1 = typ1.map(|key| ctx.terms.get(key).unwrap().clone());
	let typ2 = typ2.map(|key| ctx.terms.get(key).unwrap().clone());
	unify_typ(typ1, typ2, ctx)
} */

/// given two optional terms that represent types, unify them by ensuring they are the same or that there is only one
/// have reduction do this on terms
/// may do beta reduction/normalization on applications
/* pub fn unify_typ(typ1: Option<Term>, typ2: Option<Term>, ctx: &mut Context) -> Result<Option<Term>, LoweringError> {
	Ok(match (typ1, typ2) {
		(None, typ) => typ,
		(Some(typ), None) => Some(typ),
		(Some(typ1), Some(typ2)) => match (typ1, typ2) {
			(Term::UnknownType(hole_key), typ) | (typ, Term::UnknownType(hole_key)) => todo!(), // TODO: update hole, return typ2
			(Term::Nat(_), Term::Nat(_)) => return Err(panic!("can't unify terms as types")),
			(Term::String(_), Term::String(_)) => return Err(panic!("can't unify terms as types")),
			(Term::Bool(_), Term::Bool(_)) => return Err(panic!("can't unify terms as types")),
			(Term::BuiltinTyp(b_typ1), Term::BuiltinTyp(b_typ2)) => {
				if b_typ1 == b_typ2 {
					Some(Term::BuiltinTyp(b_typ1))
				} else {
					return Err(todo!());
				}
			}
			(Term::Uni(val1), Term::Uni(val2)) => Some(Term::Uni(val1.max(val2))), // we have cumulative universes
			(Term::Variable(ident_key), typ) | (typ, Term::Variable(ident_key)) => {
				todo!() // lookup variable
			}
			(Term::Set(items1), Term::Set(items2)) => todo!(), // unify each item
			(
				Term::Abs {
					args: args1,
					body: body1,
				},
				Term::Abs {
					args: args2,
					body: body2,
				},
			) => todo!(), // unify args and body, ensure
			(Term::App { func, args }, typ) | (typ, Term::App { func, args }) => todo!(), // reduce application and inspect
			_ => return Err(panic!("mismatched type, can't unify")),
		},
	})
} */

pub fn lower_to_key(
	term: ParseTree,
	typ: Option<Term>,
	ctx: &mut Context,
) -> Result<(TermKey, TermKey), LoweringError> {
	let out = lower(term, typ, ctx)?;
	Ok((ctx.add_term(out.0), ctx.add_term(out.1)))
}

fn lower_set(
	term_items: Vec<ParseTreeSetItem>,
	typ_items: Option<Term>,
	ctx: &mut Context,
) -> Result<(Term, Term), LoweringError> {
	// given a term set, make sure all elements match elements in type set, or infer type set from term set args

	// first: if type set has types
	//  - ensure no AssignIdentExpr, as that doesn't really make sense in this context
	//  - lower the type parsetrees one-by-one, extending context with idents if exist
	//  - match on corresponding
	// parse type, get set of expected idents with their associated types, as well as unnamed types.
	// associated types may be dependent on names in the typ set

	// expected type iterator, either an actual type or

	/* let expected_typs = typ_items.into_iter().flatten().map(Some).chain(std::iter::repeat(None)); */

	/* let mut terms: Vec<(Option<IdentKey>, TermKey)> = Vec::with_capacity(term_items.len());
	let mut typs: Vec<(Option<IdentKey>, TermKey)> = Vec::with_capacity(elems.len());
	for (elem, typ) in elems
		.into_iter()
		.zip(expected_typs.into_iter().flat_map(|vec| vec.into_iter().map(Some)))
	{
		match (elem, typ) {
			ParseTreeSetItem::Atom(parse_tree) => {
				let (term, typ) = lower_to_key(parse_tree, None, ctx)?;
				terms.push((None, term));
				typs.push((None, typ));
			}
			ParseTreeSetItem::AssignIdentExpr { ident, def, typ } => {
				let ident = ctx.add_ident(ident);
				let (term, typ) = lower_to_key(*def, typ.map(|typ| *typ), ctx)?;
				terms.push((Some(ident), term));
				typs.push((Some(ident), typ));
			}
			ParseTreeSetItem::AssignTypeIdent { ident, typ } => {
				let ident = ctx.add_ident(ident);
				let (typ, typ_of_typ) = lower_to_key(*typ, None, ctx)?;
				terms.push((Some(ident), typ));
				typs.push((None, typ_of_typ));
			}
		}
	} */
}

/*
hello := {x} -> x : Identity
Set {
	elems: [
		{ ident : "hello", term: Function{[Ident("x")], Ident("x")}, typ: Some(Ident("Identity")) }
	]
}
	args: [Ident("x")]
	body: Ident("x")
	typ: Some(Term::Ident(IdentIdx))
*/

fn lower_func(
	args: Vec<ParseTreeArgSetItem>,
	body: ParseTree,
	typ: Option<Term>,
) -> Result<(Term, Term), LoweringError> {
	// iterate over args
	// get list of (IdentKey, Option<Term>)
	// get iterator over (IdentKey, Option<Term>)
	//

	if let Some(p_typ) = typ {}
	/* let outer_typ = if let Some(p_typ) = typ {
		if let ParseTree::Func { args, body } = p_typ {
			Some((args, body))
		} else {
			return Err(panic!("function mismatch, expected function found: {:?}", typ));
		}
	} else {
		None
	}; */

	// unify types of arguments
	let inputs: Vec<(IdentKey, TermKey)> = Vec::with_capacity(args.len());
	for arg in args {}
	let mut i = 0;
	while i < args.len() {
		let ParseTreeArgSetItem {
			ident,
			typ: inner_arg_typ,
		} = args[i];
		let outer_arg_typ = outer_typ
			.and_then(|(outer_typ_args, _)| outer_typ_args[i].typ)
			.map(|a| *a);

		let inner_arg_typ = if let Some(arg_typ) = inner_arg_typ {
			Some(lower_to_key(*arg_typ, None, ctx)?.0) // get term version of arg type
		} else {
			None
		};

		/* let unified = unify_typ_from_key(outer_arg_typ, inner_arg_typ, ctx)?
			.unwrap_or_else(|| Term::UnknownType(ctx.add_hole()));
		let unified_key = ctx.add_term(unified);
		let ident = ctx.add_ident(ident); */
		/* let ident_key =
		inputs.push((ident, unified_key)) */
	}
	// unify body of function
	let (body, typ_body) = lower_to_key(*body, outer_typ.map(|a| *a.1), ctx)?;

	Ok((
		Term::Abs {
			args: inputs.clone(),
			body,
		},
		Term::Abs {
			args: inputs,
			body: typ_body,
		},
	))
}

/// Lowers a ParseTree and some already-lowered normalized type (optional) into a tuple of terms representing the parsed term and either an inferred type or initially-passed type
/// This combines the functionality of type inference and type checking into one function depending on the inputs
pub fn lower(term: ParseTree, typ: Option<Term>, ctx: &mut Context) -> Result<(Term, Term), LoweringError> {
	Ok(match (term, typ) {
		(ParseTree::Number(num), typ) => (
			Term::Nat(num),
			match typ {
				None | Some(Term::BuiltinTyp(BuiltinType::Nat)) => Term::BuiltinTyp(BuiltinType::Nat),
				Some(typ) => {
					return Err(LoweringError::TypeMismatch {
						expected: "Nat",
						got: typ,
					})
				}
			},
		),
		(ParseTree::String(string), typ) => (
			Term::String(string),
			match typ {
				None | Some(Term::BuiltinTyp(BuiltinType::String)) => Term::BuiltinTyp(BuiltinType::String),
				Some(typ) => {
					return Err(LoweringError::TypeMismatch {
						expected: "String",
						got: typ,
					})
				}
			},
		),
		(ParseTree::Ident(name), typ) => {
			let term = match &name[..] {
				"true" => Term::Bool(true),
				"false" => Term::Bool(false),
				"Bool" => Term::BuiltinTyp(BuiltinType::Bool),
				"Nat" => Term::BuiltinTyp(BuiltinType::Nat),
				"String" => Term::BuiltinTyp(BuiltinType::String),
				_ => Term::Variable(ctx.add_ident(name)),
			};
			(
				term,
				match (term, typ) {
					(Term::Bool(_), None | Some(Term::BuiltinTyp(BuiltinType::Bool))) => {
						Term::BuiltinTyp(BuiltinType::Bool)
					}
					(Term::BuiltinTyp(_), None | Some(Term::Uni(0))) => Term::Uni(0),
					(Term::Variable(ident), expected_typ) => {
						let typ = ctx.find_bind_type_term(ident)?;
						if let Some(expected) = expected_typ {
							if expected != typ {
								return Err(LoweringError::TypeLookupMismatch { expected, got: typ });
							}
						}
						typ
					}
					(Term::BuiltinTyp(_) | Term::Bool(_), Some(typ)) => {
						return Err(LoweringError::TypeMismatch {
							expected: "Uni(0)",
							got: typ,
						})
					}
					_ => unreachable!(),
				},
			)
		}
		(ParseTree::Set(elems), typ) => lower_set(elems, typ, ctx)?,
		(ParseTree::Func { args, body }, typ) => lower_func(args, body, typ)?,
		(ParseTree::Apply { func, args }, typ) => {
			let func = lower_to_key(*func, None, ctx)?;
			let args = lower_to_key(*args, None, ctx)?;
			(
				Term::App {
					func: func.0,
					args: args.0,
				},
				Term::App {
					func: func.1,
					args: args.1,
				},
			)
		}
		(ParseTree::String(_), None) => todo!(),
		(ParseTree::String(_), Some(_)) => todo!(),
		(ParseTree::Ident(_), None) => todo!(),
		(ParseTree::Ident(_), Some(_)) => todo!(),
		(ParseTree::Set(parse_tree_set_items), None) => todo!(),
	})
}
