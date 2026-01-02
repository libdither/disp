//! This file extracts semantic information from parser::SpannedAST

use std::collections::HashMap;

use hashdb::LinkArena;
use slotmap::{new_key_type, SlotMap};

use crate::parse::{IdentType, SpannedAST, AST, Literal};

// Some Atomic Thing, Represented using a symbol in syntax, but is represented as a UUID in a purely semantic context.
// type AtomID = u64;

/// A structure that may be evaluated
pub enum SemTree<'e> {
	/// Associate atomic constants with a list of items
	Map { access: &'e [AtomID], items: &'e [SemTree<'e>] }, 
	/// List of items
	List { items: &'e [SemTree<'e>] },
	/// Abstraction
	Abs { args: &'e SemTree<'e>, body: &'e SemTree<'e> },
	/// Application
	App { func: &'e SemTree<'e>, args: &'e SemTree<'e> },
	Atom(AtomID),
	Literal(Literal, &'e str),
}

new_key_type! {
	pub struct AtomID;
}

pub struct NameState<'e> {
	name_map: SlotMap<AtomID, &'e str>, // map string hashes to expressions
}

enum ExtractError {
	Error
}

// Extract 
fn extract<'a: 'e, 'e>(expr: SpannedAST<'a>, links: &LinkArena<'e>, context: NameState<'a>) -> Result<(SemTree<'e>, NameState<'e>), ExtractError> {
	Ok((match expr.item {
		AST::Def { idt, def, typ } => {
			match idt.item {
				AST::Ident(IdentType::Symbol, idt) => {
					todo!()
				},
				_ => return Err(ExtractError::Error),
			}
			// check if symbol already exists (in which case it is aliased)

		},
		AST::Typ { idt, typ } => todo!(),
		AST::Ident(ident_type, ident) => match ident_type {
			IdentType::Literal(lit) => SemTree::Literal(lit.clone(), ident.0),
			IdentType::Symbol => todo!(),
		},
		AST::Seq(_) => todo!(),
		AST::Map(_) => todo!(),
		AST::List(_) => todo!(),
		AST::Func(_) => todo!(),
		AST::App(_) => todo!(),
	}, context))
}


