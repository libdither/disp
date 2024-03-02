//! This file extracts semantic information from parser::SpannedAST

use std::collections::HashMap;

use disp::parse::Literal;
use hashdb::LinkArena;
use slotmap::SlotMap;

use crate::parse::{IdentType, SpannedAST, AST};

// Some Atomic Thing, Represented using a symbol in syntax, but is represented as a UUID in a purely semantic context.
type AtomID = u64;

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

pub struct NameState<'e> {
	name_map: SlotMap<&'e str>, // map string hashes to expressions
}

enum ExtractError {
	
}

// Extract 
fn extract<'a, 'e>(expr: SpannedAST<'a>, links: &LinkArena<'e>, context: NameState<'e>) -> Result<(SemTree<'e>, NameState<'e>), ExtractError> {
	match expr.item {
		AST::Def { idt, def, typ } => {
			match idt.item {
				AST::Ident(IdentType::Symbol, idt) => {
					
				},
				_ => return Err(),
			}
			// check if symbol already exists (in which case it is aliased)

		},
		AST::Typ { idt, typ } => todo!(),
		AST::Ident(ident_type, ident) => match ident_type {
			IdentType::Literal(lit) => SemTree::Literal(lit, ident.0),
			IdentType::Symbol => todo!(),
		},
		AST::Seq(_) => todo!(),
		AST::Map(_) => todo!(),
		AST::List(_) => todo!(),
		AST::Func(_) => todo!(),
		AST::App(_) => todo!(),
	}
}


