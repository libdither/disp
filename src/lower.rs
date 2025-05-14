use crate::{
	eval::{reduce, Environment},
	parse::{ParseTree, ParseTreeArgSetItem, ParseTreeSetItem},
};
use core::fmt;
use indexmap::IndexMap;
use slotmap::{new_key_type, SlotMap};
use smallvec::SmallVec;
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
	Set(SmallVec<(Option<IdentKey>, TermKey), 3>), // Association of ident with literal or type
	// both function and type of function
	Abs {
		args: SmallVec<(IdentKey, TermKey), 2>, // bind name and type
		body: TermKey,
	},
	// application of a function given some argument (usually a set)
	App {
		func: TermKey,
		args: TermKey,
	},
}
const SIZE: usize = size_of::<Term>();

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
	pub idents: SlotMap<IdentKey, String>, // idents go here
	ident_map: HashMap<String, IdentKey>,  // deduplicating idents

	pub terms: SlotMap<TermKey, Term>, // terms go here
	term_map: HashMap<Term, TermKey>,  // deduplicating terms & reduction

	pub holes: SlotMap<HoleKey, Option<TermKey>>, // holes go here
	pub binds: IndexMap<IdentKey, TermKey>,       // active while-parsing bindings and globals, maintains ordering
	pub env: Environment,                         // environment for registering assignments to identifiers
}
impl fmt::Debug for Context {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Context")
			.field("idents", &self.idents)
			.field("terms", &self.terms)
			.field("env", &self.env)
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
			reduce_map: SlotMap::with_key(),
			holes: SlotMap::with_key(),
			binds: IndexMap::new(),
			env: Environment::default(),
		}
	}
	pub fn add_ident(&mut self, ident: String) -> IdentKey {
		if let Some(key) = self.ident_map.get(&ident) {
			key.clone()
		} else {
			self.idents.insert_with_key(|k| {
				self.ident_map.insert(ident.clone(), k);
				ident
			})
		}
	}
	/// add a term, get either regular term key or reduced term key if term has already been reduced.
	pub fn add_term(&mut self, term: Term) -> TermKey {
		if let Some(key) = self.term_map.get(&term) {
			key.clone()
		} else {
			self.terms.insert_with_key(|k| {
				self.term_map.insert(term.clone(), k);
				term
			})
		}
	}
	/// add reduced term and link original term version to reduced term so that whenever original term is added, via add_term, you get the reduced version
	pub fn add_reduced_term(&mut self, original_term: Term, reduced_term: Term) -> TermKey {
		let reduced_term_key = self.add_term(reduced_term);
		self.term_map.insert(original_term, reduced_term_key);
		reduced_term_key
	}
	pub fn add_hole(&mut self) -> HoleKey {
		self.holes.insert(None)
	}
	pub fn new_unknown_type(&mut self) -> TermKey {
		let hole = self.add_hole();
		self.add_term(Term::UnknownType(hole))
	}
	/// Given an identifier, find its type in context
	pub fn register_bind(&mut self, ident: IdentKey, typ: TermKey) {
		self.binds.insert(ident, typ);
	}
	/// Given an identifier, find the type that is bound to this identifier in the context
	/// Goes through the
	pub fn get_bind_type_key(&self, ident: IdentKey) -> Option<TermKey> {
		self.binds.get(&ident).cloned()

		// self.binds.iter().rev().find(|(idt, _)| *idt == ident).map(|(_, t)| *t)
		/* .or_else(|| {
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
		}) */
	}
	pub fn get_bind_type(&self, ident: IdentKey) -> Result<Term, LoweringError> {
		self.get_bind_type_key(ident)
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
	#[error("Set type expected ident for set item, but couldn't : {ident:?}")]
	ExpectedIdentInSet { ident: IdentKey },
	#[error("Term unification failed: found {found:?} but expected {expected:?}")]
	TermMismatch { found: Term, expected: Term },
	#[error("Ident unification failed: found {found:?} but expected {expected:?}")]
	IdentMismatch { found: IdentKey, expected: IdentKey },
	#[error("Binding not found in context: {0:?}")]
	IdentNotBound(IdentKey),
	#[error("Type binding not found in context for: {0:?}")]
	IdentNotBoundToType(IdentKey),

	/// When lowering a Set, and we have a specific Set given as a typ, the items in the given type Set should correspond to the values in the Set we are currently lowering. If there are missing types
	#[error("Set was given explicit type, but couldn't find matching type for term {item:?}")]
	NoCorrespondingTypeInSetFor { item: ParseTreeSetItem },
	#[error("Abstraction argument was given explicit type, but couldn't find matching ident {item:?}")]
	NoCorrespondingTypeInAbsArgsFor { item: ParseTreeArgSetItem },
}

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

/// takes two optional terms, if both are Some, check that they are equal and return one of them, otherwise return whichever one is Some(), otherwise return None. If mismatch, return LoweringError::FailedToUnifyTerm
pub fn unify_term(found_term: Option<Term>, expected_term: Option<Term>) -> Result<Option<Term>, LoweringError> {
	match (found_term, expected_term) {
		(None, None) => Ok(None),
		(Some(term), None) | (None, Some(term)) => Ok(Some(term)),
		(Some(found), Some(expected)) => {
			if found == expected {
				Ok(Some(expected))
			} else {
				Err(LoweringError::TermMismatch { found, expected })
			}
		}
	}
}
/// does the same thing as unify_term but for idents
pub fn unify_ident(
	found_idt: Option<IdentKey>,
	expected_idt: Option<IdentKey>,
) -> Result<Option<IdentKey>, LoweringError> {
	match (found_idt, expected_idt) {
		(None, None) => Ok(None),
		(Some(term), None) | (None, Some(term)) => Ok(Some(term)),
		(Some(found), Some(expected)) => {
			if found == expected {
				Ok(Some(expected))
			} else {
				Err(LoweringError::IdentMismatch { found, expected })
			}
		}
	}
}

// lower both term and type sets to a tuple of two terms
fn lower_set(
	term_items: Vec<ParseTreeSetItem>,
	typ_items: Option<Term>,
	ctx: &mut Context,
) -> Result<(Term, Term), LoweringError> {
	// first: if type set has types
	//  - ensure no AssignIdentExpr, as that doesn't really make sense in this context
	//  - lower the type parsetrees one-by-one, extending context with idents if exist
	//  - match on corresponding
	// parse type, get set of expected idents with their associated types, as well as unnamed types.
	// associated types may be dependent on names in the typ set

	// 1. Process the overall expected type `typ_items` if provided.
	/* let expected_type_iter: TypeIter<_, _, _> = match typ_items {
		Some(Term::Set(items)) => TypeIter::SetIter(items.into_iter().map(|(idt, tk)| Some((idt, tk)))),
		Some(other_term) => {
			return Err(LoweringError::TypeMismatch {
				expected: "Set",
				got: other_term.clone(),
			})
		} // wrong type for set!
		Some(Term::Uni(n)) => TypeIter::Uni(std::iter::repeat(Some((None, Term::Uni(n))))), // all things should be of type Type
		None => TypeIter::None(std::iter::repeat(None)), // No overall type hint; rely on inference or inline types.
	}; */

	// take pairs of ParseTreeSetItem and Option<(Option<IdentKey>, Term)> representing optional expected types and lower to pairs of (Option<IdentKey>, Term)
	let mut terms = SmallVec::new();
	let mut typs = SmallVec::new();

	for (i, term) in term_items.into_iter().enumerate() {
		let res: Result<(Option<IdentKey>, (Term, Term)), LoweringError> = try {
			// get expected type
			let expected_typ = match typ_items {
				Some(Term::Set(ref items)) => {
					let expected_typ_key = items
						.get(i)
						.ok_or_else(|| LoweringError::NoCorrespondingTypeInSetFor { item: term.clone() })?;
					Some((
						expected_typ_key.0,
						ctx.terms
							.get(expected_typ_key.1)
							.expect("ctx should never delete keys")
							.clone(),
					))
				}
				Some(other_term) => {
					return Err(LoweringError::TypeMismatch {
						expected: "Set",
						got: other_term.clone(),
					})
				}
				None => None,
			};
			// check
			match (term, expected_typ) {
				(ParseTreeSetItem::Atom(parse_tree), typ) => {
					// ensure type doesn't have ident for Atom set item variant
					if let Some(ident) = typ.as_ref().and_then(|t| t.0.clone()) {
						Err(LoweringError::ExpectedIdentInSet { ident })?
					}
					(None, lower(parse_tree, typ.map(|t| t.1), ctx)?)
				}
				(
					ParseTreeSetItem::AssignIdentExpr {
						ident,
						def,
						typ: inner_expected_typ,
					},
					outer_expected_typ,
				) => {
					// we are assigning an expression to something, this is a definition, we should be able to use the definition via the environment, and check its type
					// to do this we can lower the def with the typ and then add to the context
					// we should first lower the typ, and check if the inferred type matches the given type (if any)
					/*
					TODO:
					- Lower given type parsetree
					- Check against given type
					- Check against inferred type if Some(_), else use inferred type
					- Lower term with inferred/expected type
					- add def to environment and typing context
					*/
					// lower given type parsetree
					let inner_expected_typ = if let Some(typ) = inner_expected_typ {
						Some(lower(*typ, None, ctx)?.0)
					} else {
						None
					};
					let outer_expected_idt = outer_expected_typ.as_ref().and_then(|t| t.0.clone());
					// unify inner and outer expected types
					let expected_typ = unify_term(inner_expected_typ, outer_expected_typ.map(|t| t.1))?;
					// unify inner and outer identifiers
					let expected_idt = unify_ident(Some(ctx.add_ident(ident)), outer_expected_idt)?;
					// lower term with unified expected type
					(expected_idt, lower(*def, expected_typ, ctx)?)
				}
				(ParseTreeSetItem::AssignTypeIdent { ident, typ }, typ_of_typ) => {
					// we are assigning an ident to a type, so just lower the typ
					(Some(ctx.add_ident(ident)), lower(*typ, typ_of_typ.map(|t| t.1), ctx)?)
				}
			}
		};
		let (idt, (term, typ)) = res?;
		terms.push((idt, ctx.add_term(term)));
		let typ = ctx.add_term(typ);
		typs.push((idt, typ));
		if let Some(idt) = idt {
			ctx.register_bind(idt, typ);
		}
	}
	Ok((Term::Set(terms), Term::Set(typs)))
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

func := { X : Type, x : X } -> ???
func {X:=Nat, x:=8}
*/

/// Lower Function arguments and body into Term, given optional previously parsed type
/// What do we have?
///  - args: some parsed list of function arguments, may have types
///  - body: some parsetree that contains bound variables and to be parsed needs to have an updated context
///  - typ: some external type of this function. Should be variant `Term::Func`.
/// What do we do?
///  - verify typ is correct variant (Func), extract `args_typ : Option<Vec<(IdentKey, TermKey)>>` and `args_body : Option<TermKey>`
///  - lower args one using corresponding typ if exists into `Vec<(IdentKey, Option<TermKey>)>``, preserving order
///    - ensure identkey matches IdentKey of type
///  - verify equality with args_typ via iterator, if true,
///  - extend typing context with args
///  -
fn lower_func(
	pt_args: Vec<ParseTreeArgSetItem>,
	pt_body: ParseTree,
	typ: Option<Term>,
	ctx: &mut Context,
) -> Result<(Term, Term), LoweringError> {
	// arguments of function (and function typ) to return
	let mut args: SmallVec<(IdentKey, TermKey), 2> = SmallVec::new();
	// iterate through function arguments
	for (i, pt_arg) in pt_args.into_iter().enumerate() {
		let res: Result<(IdentKey, Term), LoweringError> = try {
			// get expected type for arg
			let expected_typ = match typ {
				Some(Term::Abs { ref args, body: _ }) => {
					// if abstraction, get ith argument. TODO: more flexible argument typing?
					let expected_typ_key = args
						.get(i)
						.ok_or_else(|| LoweringError::NoCorrespondingTypeInAbsArgsFor { item: pt_arg.clone() })?;
					Some((
						expected_typ_key.0,
						ctx.terms
							.get(expected_typ_key.1)
							.expect("ctx should never delete keys")
							.clone(),
					))
				}
				Some(other_term) => {
					// if not abstraction, type mismatch
					return Err(LoweringError::TypeMismatch {
						expected: "Abs",
						got: other_term.clone(),
					});
				}
				None => None, // no typ? no problem, just infer it
			};
			// get parsetree ident
			let arg_ident = ctx.add_ident(pt_arg.ident);
			// unify inner and outer identifiers
			let outer_expected_idt = expected_typ.as_ref().map(|t| t.0.clone());
			let expected_idt = unify_ident(Some(arg_ident), outer_expected_idt)?
				.expect("we passed Some, output guaranteed to be Some");

			// lower argument typ
			let inner_typ = if let Some(t) = pt_arg.typ {
				Some(lower(*t, None, ctx)?.0)
			} else {
				None
			};
			// get expected type (if any)
			let outer_typ = expected_typ.map(|t| t.1);
			// unify inner and outer typs
			let expected_typ =
				unify_term(inner_typ, outer_typ)?.ok_or(LoweringError::IdentNotBoundToType(expected_idt))?;
			(expected_idt, expected_typ)
		};
		// get
		let (idt, typ) = res?;
		let typ = ctx.add_term(typ);
		ctx.register_bind(idt, typ); // register argument as binding
		args.push((idt, typ));
	}
	// lower body
	let expected_body_typ = match typ {
		Some(Term::Abs { args: _, body }) => ctx.terms.get(body).cloned(), // check with body of lambda
		Some(Term::Uni(_)) => typ,                                         // check with universe typ
		Some(t) => {
			return Err(LoweringError::TypeMismatch {
				expected: "Abstraction",
				got: t,
			})
		}
		None => None,
	};
	// unify body of function
	let (body, typ_body) = lower_to_key(pt_body, expected_body_typ, ctx)?;

	Ok((
		Term::Abs {
			args: args.clone(),
			body,
		},
		Term::Abs { args, body: typ_body },
	))
}

// function that takes an argument list, i.e. SmallVec<(IdentKey, TermKey), 2> and an Option<IdentKey> and TermKey representing a type, and figures out how to

pub fn lower_app(
	func: ParseTree,
	args: ParseTree,
	typ: Option<Term>,
	ctx: &mut Context,
) -> Result<(Term, Term), LoweringError> {
	// we are parsing an application of some kind. we will assume we are given a fully reduced typ.
	// to verify that this application is valid, we need to simply ensure that the body is of the expected `typ`
	// first we parse the function and infer its type.
	// then we parse the argument, infer its type
	// apply the function type to the arg type and reduce, and then unify the reduced type it with the expected type (if any)

	// generally we do not have the full function type, only the return typ, so we infer it from context
	let (func, func_typ) = lower(func, None, ctx)?;
	// we should verify that the argument typ matches the function input type
	let (arg, arg_typ) = lower(args, None, ctx)?;
	let unreduced_app = Term::App {
		func: ctx.add_term(func_typ),
		args: ctx.add_term(arg_typ),
	};
	// potential things this reduction could be:
	// - {1} : Vec{Nat}
	// -
	let reduced_app = reduce(ctx.add_term(unreduced_app), ctx, &mut ctx.env);

	let (func_typ_args, func_typ_body) = match func_typ {
		Term::Abs { args, body } => (args, body),
		_ => {
			return Err(LoweringError::TypeMismatch {
				expected: "Abstraction",
				got: func_typ,
			})
		}
	};
	// check if we can infer the argument type automatically
	let (args, args_typ) = if let Ok((args, args_typ)) = lower(*args, None, ctx) {
		// options:
		// - args is a variable -> search for something in func_typ_args that matches variable name first, then variable type
		// - args is a set -> go through set one by one, matching names first, then typs, then order
		// - args is something else (constant, function, another application) -> find first typ that matches arg typ
	} else {
		// otherwise see if we can
	};

	// take function and args, infer return type of function
	Ok((
		Term::App {
			func: func.0,
			args: args.0,
		},
		Term::App {
			func: func.1,
			args: args.1,
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
			let typ = match (&term, typ) {
				(Term::Bool(_), None | Some(Term::BuiltinTyp(BuiltinType::Bool))) => {
					Term::BuiltinTyp(BuiltinType::Bool)
				}
				(Term::BuiltinTyp(_), None | Some(Term::Uni(0))) => Term::Uni(0),
				(Term::Variable(ident), expected_typ) => {
					let typ = ctx.get_bind_type(*ident)?;
					unify_term(Some(typ), expected_typ)?.expect("guaranteed to be Some")
				}
				(Term::BuiltinTyp(_) | Term::Bool(_), Some(typ)) => {
					return Err(LoweringError::TypeMismatch {
						expected: "Uni(0)",
						got: typ,
					})
				}
				_ => unreachable!(),
			};
			(term, typ)
		}
		(ParseTree::Set(elems), typ) => lower_set(elems, typ, ctx)?,
		(ParseTree::Func { args, body }, typ) => lower_func(args, *body, typ, ctx)?,
		(ParseTree::Apply { func, args }, typ) => lower_app(*func, *args, typ, ctx)?,
		(ParseTree::String(_), None) => todo!(),
		(ParseTree::String(_), Some(_)) => todo!(),
		(ParseTree::Ident(_), None) => todo!(),
		(ParseTree::Ident(_), Some(_)) => todo!(),
		(ParseTree::Set(parse_tree_set_items), None) => todo!(),
	})
}
