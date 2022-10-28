use std::fmt;


use crate::expr::{BindTree, Binding, Expr};
use hashdb::{ArchiveDeserializer, ArchiveStore, LinkArena, RevTypeStore, TypeStore, hashtype};

/// Name of a thing
#[hashtype]
#[derive(Debug)]
pub struct Name<'e> {
	#[subtype] name: &'e String,
}
impl<'e, S: AsRef<str>> PartialEq<S> for Name<'e> {
    fn eq(&self, other: &S) -> bool {
        self.name == other.as_ref()
    }
}
impl<'e> fmt::Display for Name<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl<'e> Name<'e> {
	pub fn add(name: &'e String, links: &'e impl TypeStore<'e>) -> &'e Name<'e> {
		let name = Name { name };
		let name = links.add(name);
		name
	}
}

/// Link between Name and Expr
#[hashtype]
#[derive(Debug)]
pub struct NamedExpr<'e> {
	#[subtype_reverse_link]
	pub name: &'e Name<'e>,
	#[subtype_reverse_link]
	pub expr: &'e Expr<'e>
}
impl<'e> NamedExpr<'e> {
	pub fn new_linked(name: &str, expr: &'e Expr<'e>, links: &'e impl RevTypeStore<'e>) -> &'e NamedExpr<'e> {
		links.rev_add(NamedExpr { name: Name::add(links.add(name.to_string()), links), expr })
	}
}

/// Links a parsed syntax to a program, i.e. the semantics.
#[hashtype]
#[derive(Clone, Debug)]
pub struct SemanticTree<'e> {
	#[subtype_reverse_link]
	pub syn_tree: &'e SyntaxTree<'e>,
	#[subtype_reverse_link]
	pub expr: &'e Expr<'e>
}

type NameBindTree<'e> = BindTree<'e, &'e Name<'e>>;

impl<'e: 'b, 'b> SemanticTree<'e> {
	pub const VAR: SemanticTree<'static> = SemanticTree { expr: Expr::VAR, syn_tree: SyntaxTree::VAR };
	pub fn lambda(expr_bind: &'e Binding<'e>, name_bind: &'e Name<'e>, sub: SemanticTree<'e>, grouped: bool, links: &'e impl TypeStore<'e>) -> SemanticTree<'e> {
		let expr = links.add(Expr::Lambda { bind: expr_bind, expr: sub.expr });
		let syn_tree = links.add(SyntaxTree::Abs { bind: name_bind, sub: sub.syn_tree, grouped });
		SemanticTree { syn_tree, expr }
	}
	pub fn app(func: SemanticTree<'e>, args: SemanticTree<'e>, links: &'e impl TypeStore<'e>) -> SemanticTree<'e> {
		let expr = links.add(Expr::Application { func: func.expr, args: args.expr });
		let syn_tree = links.add(SyntaxTree::App { func: func.syn_tree, args: args.syn_tree });
		SemanticTree { syn_tree, expr }
	}
	pub fn named(named_expr: &'e NamedExpr<'e>, links: &'e impl TypeStore<'e>) -> SemanticTree<'e> {
		let syn_tree = links.add(SyntaxTree::NamedExpr(named_expr));
		SemanticTree { syn_tree, expr: named_expr.expr }
	}
	pub fn parens(inner: SemanticTree<'e>, links: &'e impl TypeStore<'e>) -> SemanticTree<'e> {
		let syn_tree = links.add(SyntaxTree::Parenthesized(inner.syn_tree));
		SemanticTree { syn_tree, expr: inner.expr }
	}

	fn display(&self, bind_tree: &mut &'e NameBindTree<'b>, f: &mut fmt::Formatter<'_>, binds: &'b LinkArena<'b>) -> anyhow::Result<()> {
		match (self.syn_tree, self.expr, *bind_tree) {
			(&SyntaxTree::Parenthesized(syn_tree), expr, _) => {
				write!(f, "(")?;
				SemanticTree { syn_tree, expr }.display(bind_tree, f, binds)?;
				write!(f, ")")?;
			}
			// Named Expressions must not have any bound variables.
			(&SyntaxTree::NamedExpr(named_expr), _, NameBindTree::None) => {
				write!(f, "{}", named_expr.name)?;
			}
			// Variables must have a bound name
			(SyntaxTree::Var, Expr::Variable, NameBindTree::End(name)) => write!(f, "{name}")?,
			(SyntaxTree::Var, Expr::Variable, NameBindTree::None) => write!(f, "*")?,
			// Functions can be nested either like `[x] [y]` or like `[x y]` depending on whether the expr that binds `y` has `grouping` enabled or not.
			(SyntaxTree::Abs { bind: name_bind, sub: syn_tree, grouped }, Expr::Lambda { bind, expr }, _) => {
				// Push binding onto NameBindTree to keep track of bound name
				bind_tree.push_binding(binds, name_bind, bind)?;

				// If not grouped, mark a new binding with `[`
				if !grouped { write!(f, "[")?; }

				// write binding
				write!(f, "{}", name_bind)?;

				// Check if subexpression is a grouped lambda to figure out whether or not to put a closing `]`
				if let SyntaxTree::Abs { grouped: true, .. } = syn_tree {
					()
				} else {
					write!(f, "]")?;
				}
				// Add space between this binding and subexpression
				write!(f, " ")?;

				// Recursively display subexpression
				let sub_expr = SemanticTree { expr, syn_tree };
				sub_expr.display(bind_tree, f, binds)?;
			}
			(&SyntaxTree::App { func: syn_func, args: syn_args }, &Expr::Application { func: expr_func, args: expr_args }, tree) => {
				let (mut left, mut right) = tree.split()?;
				SemanticTree { syn_tree: syn_func, expr: expr_func }.display(&mut left, f, binds)?;
				write!(f, " ")?;
				SemanticTree { syn_tree: syn_args, expr: expr_args }.display(&mut right, f, binds)?;
				*bind_tree = NameBindTree::branch(left, right, binds)
			},
			_ => anyhow::bail!("Invalid SyntaxTree, Expr, and BindTree pair: \n SyntaxTree: {:?}\nExpr: {:?}\nBindTree {:?}", self.syn_tree, self.expr, bind_tree)
		}
		Ok(())
	}
}
impl<'e> fmt::Display for SemanticTree<'e> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		thread_local! {
			static BINDS: LinkArena<'static> = LinkArena::new();
		}
		BINDS.with(|binds| {
			let mut name_bind = NameBindTree::NONE; // Start with nothing bound.
			match self.display(&mut name_bind, f, binds) {
				Err(err) => write!(f, "\nerror: {}", err),
				_ => Ok(())
			}
		})?;
		Ok(())
	}
}

/// Represents the representative syntax of an `Expr`
#[hashtype]
#[derive(Debug)]
pub enum SyntaxTree<'e> {
	Abs {
		#[subtype] bind: &'e Name<'e>,
		#[subtype] sub: &'e SyntaxTree<'e>,
		grouped: bool, // Whether or not this Abs is grouped with the previous one
	},
	App {
		#[subtype] func: &'e SyntaxTree<'e>,
		#[subtype] args: &'e SyntaxTree<'e>,
	},
	Var,
	NamedExpr(#[subtype] &'e NamedExpr<'e>),
	Parenthesized(#[subtype] &'e SyntaxTree<'e>),
}
impl<'e> SyntaxTree<'e> {
	pub const VAR: &'static SyntaxTree<'static> = &SyntaxTree::Var;
}