use std::collections::HashMap;
use slotmap::{new_key_type, SlotMap, Key};
use thiserror::Error;
use winnow::combinator::todo;
use crate::parse::ParseTree;

new_key_type! { struct StackKey; }
new_key_type! { struct TermKey; }

#[derive(Clone, Debug, PartialEq)]
enum BuiltinType {
	Unit,
	Nat,
	String,
	Bool,
}

/// Represents both expressions and types.
#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    UnknownType, // represents a type that is un-inferred

    Nat(u64), // number literal
    String(String), // string literal
    Bool(bool), // boolean literal
	BuiltinTyp(BuiltinType), // default types

	Variable(String),
    Typing { term: Box<Term>, typ: Box<Term> },
	// basic constructs
    Set(Vec<Box<Term>>), // Both expression sets and type sets
	// function literal, param must be a set of variables (?)
    Lam { param: Box<Term>, body: Box<Term> },
	// application literal
    App { function: Box<Term>, argument: Box<Term> },
    // Potentially more complex type formers like Pi-types (dependent function types)
    Pi { param: Box<Term>, body_typ: Box<Term> },
    Universe(usize), // For managing the hierarchy of types (Type, Kind, etc.)
}
// thing := 3
// ({thing} -> {...}) {3}
// AssignIdentExpr { ident = "thing", def = 3 }
// PartialTypedTerm { term: App { function: Lam { param: body:  } }, typ: UnknownType }
struct PartialTypedTerm {
    term: Term,
    typ: Term,
}

impl Context {
    pub fn new() -> Self {
        let mut terms = SlotMap::<TermKey, TypedTerm>::with_key();

        Context {
            idents: HashMap::new(),
            stacks: Default::default(),
            terms,
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
}

type LoweringResult<TermKey> = Result<TermKey, LoweringError>;

/// Lowers a ParseTree into a TypedTerm.
pub fn lower(tree: ParseTree, ctx: &mut Context) -> LoweringResult<Term> {
    match tree {
        ParseTree::Number(num) => Ok(ctx.terms.insert(TypedTerm {
			term: Term::Number(num),
			typ: Term::BuiltinTyp(BuiltinType::Nat),
		})),
        ParseTree::String(string) => Ok(ctx.terms.insert(TypedTerm {
			term: Term::String(string),
			typ: Term::BuiltinTyp(BuiltinType::String),
		})),
        ParseTree::Ident(name) => {
            todo!()
        }
        ParseTree::AssignIdentExpr { ident, def, typ } => {
            todo!()
        }
        ParseTree::AssignIdentType { ident, typ } => {
            todo!()
        }
        ParseTree::ExprSet(elements) => {
            todo!()
        }
        ParseTree::TypeSet(elements) => { // Now treated the same as ExprSet conceptually
            todo!()
        }
        ParseTree::ExprFunc { args, body } => {
            todo!()
        }
        ParseTree::TypeFunc { args, body } => {
            todo!()
        }
        ParseTree::Apply { func, args } => {
            todo!()
        }
    }
}