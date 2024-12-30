use std::collections::HashMap;

use slotmap::{new_key_type, SlotMap};

use crate::parse::ParseTree;

new_key_type! { struct StackKey; }
new_key_type! { struct ExprKey; }

#[derive(Clone)]
enum ExprTree {
	Number(u64), // string literal
	String(String), // 
	Ref(ExprKey), // 
	Universal(u64), // `Type` in some universe
	// Sets, Functions, Application
	Lambda {  }
}

// all expression trees have a type
#[derive(Clone)]
struct TypedExpr {
	pub term: ExprTree,
	pub typ: ExprTree,
}

struct Context {
	// a given string maps to a unique StackKey
	idents: HashMap<String, StackKey>,
	// 
	stacks: SlotMap<StackKey, Vec<ExprKey>>,
	exprs: SlotMap<ExprKey, TypedExpr>,
	pub nat_key: ExprKey, // default built-in Nat
	pub string_key: ExprKey, // default built-in String
}
impl Context {
	fn get_key(&self, ident: &str) -> Option<StackKey> {
		self.idents.get(ident).cloned()
	}
	fn get_stack_mut(&mut self, ident: &str) -> Option<&mut Vec<ExprKey>> {
		self.stacks.get_mut(self.idents.get(ident).cloned()?)
	}
	fn get_expr(&self, ident: &str) -> Option<&TypedExpr> {
		let stack = self.get_key(ident)?;
		let expr_key = self.stacks.get(stack)?.first()?.clone();
		self.exprs.get(expr_key)
	}
}

enum LoweringError {
	NoNameLookup(String),
	ExpectedIdentifier,
	AssignmentsAreUntyped,
}

// lower a parsetree term with some parsetree type into an exprtree with some context
pub fn lower(tree: ParseTree, typ: Option<ParseTree>, context: &mut Context) -> Result<TypedExpr, LoweringError> {
	Ok(match tree {
		ParseTree::Number(num) => {
			// check if has type, otherwise assume built-in Nat for typechecker to deal with later
			let typ_expr = if let Some(typ) = typ {
				// parse type
				lower(typ, None, context)?.typ
			} else { ExprTree::Ref(context.nat_key) };
			TypedExpr {
				term: ExprTree::Number(num),
				typ: typ_expr // infer default type of number
			}
		},
		ParseTree::String(string) => {
			// check if has type, otherwise assume built-in Nat for typechecker to deal with later
			let typ_expr = if let Some(typ) = typ {
				// parse type
				lower(typ, None, context)?.typ
			} else { ExprTree::Ref(context.string_key) };
			TypedExpr {
				term: ExprTree::String(string),
				typ: typ_expr // infer default type of number
			}
		},
		ParseTree::Ident(name) => {
			// lookup type of ident from context and return existing TypedExpr
			context.get_expr(&name).ok_or_else(||LoweringError::NoNameLookup(name))?.clone()
		},
		ParseTree::AssignIdentExpr { ident, def, typ } => {
			// ensure assignment has type
			if typ.is_some() { return Err(LoweringError::AssignmentsAreUntyped) }
			// extract ident string from ident parsetree
			let ParseTree::Ident(ident) = *ident else {
				return Err(LoweringError::ExpectedIdentifier)
			};
			// add def to context
			if let Some(stack) = context.get_stack_mut(&ident) {
				// lower 
				// id already defined, push to vec
				stack.push(def)
			}
		},
		ParseTree::AssignIdentType { ident, typ } => todo!(),
		ParseTree::ExprSet(_) => todo!(),
		ParseTree::TypeSet(_) => todo!(),
		ParseTree::ExprFunc { args, body } => todo!(),
		ParseTree::TypeFunc { args, body } => todo!(),
		ParseTree::Apply { func, args } => todo!(),
	})
}