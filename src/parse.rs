#![allow(dead_code)]

use std::{cell::RefCell};

use ariadne::{Color, Label, Report, Fmt, ReportKind, Source};
use chumsky::{prelude::*, text::unicode, extra::Full, recursive::Direct, input::SpannedInput};
use hashdb::{HashType, LinkArena, RevHashType, RevLinkArena, RevLinkStore, RevTypeStore, TypeStore, ArchiveFetchable, Datastore, ArchiveToType, ArchiveStorable};
use itertools::Itertools;

use crate::{expr::{BindSubTree, Expr}, name::{Name, SemanticTree, NamedExpr}};

// Represents active bound variables in the course of parsing an expression
#[derive(Default, Debug)]
pub struct NameBindStack<'e> {
	stack: RefCell<Vec<&'e Name<'e>>>,
}
impl<'e> NameBindStack<'e> {
	// Get binding index for this variable
	fn name_index(&self, string: &String) -> Option<usize> {
		self.stack.borrow().iter().enumerate().rev().find(|(_, e)|**e == string).map(|val|val.0 + 1)
	}
	fn push_name(&self, string: &'e Name<'e>) -> usize {
		let mut map = self.stack.borrow_mut();
		map.push(string);
		map.len()
	}
	fn pop_name(&self) -> (usize, Option<&'e Name<'e>>) {
		let mut map = self.stack.borrow_mut();
		let len = map.len();
		let ret = map.pop();
		(len, ret)
	}
}

/// Find first link of type L registered in RevTypeStore that contains `object` and matches a predicate
fn lookup<'e, L: RevHashType<'e>, TS: RevTypeStore<'e>>(links: &'e TS, object: &'e impl HashType<'e>, predicate: impl Fn(&&'e L) -> bool) -> Option<&'e L>
where
	L: for<'s> ArchiveFetchable<'e, ArchiveToType<'s, 'e, Datastore, TS>> + ArchiveStorable<Datastore>
{
	links.links::<_, L>(object).find(predicate)
}

fn lookup_expr<'e, TS: RevTypeStore<'e>>(links: &'e TS, string: &str) -> Option<SemanticTree<'e>> {
	let name = Name::add(links.add(string.to_string()), links);
	// println!("looking up: {:?}", string);
	let named_expr: &'e NamedExpr<'e> = lookup::<NamedExpr, TS>(links, name, |_expr| {
		true // Return first expression found
	})?;
	// println!("found {name:?}");

	Some(SemanticTree::named(named_expr, links))
}

#[derive(PartialEq, Debug, Clone)]
enum Literal {
	String,
	Integer,
	Decimal,
}
#[derive(PartialEq, Debug, Clone)]
enum Bracket {
	Paren,
	Square,
	Curly,
	Caret,
}

#[derive(PartialEq, Debug, Clone)]
enum Token<'s> {
	/// Associates a name with being the member of some type (used to defer definition)
	/// `type`, as in `type thing : Type`
    TypeDef,
	/// Associates a name with an expression.
	/// `let` as in `let thing := 3`
	LetDef,
	/// Assign an expression to a name. (In TypeDef it is treated as a "default" definition)
	/// `:=` as in `let thing := 3`
	AssignExpr,
	/// Describes the type of an expression or name.
	/// `:` as in `type thing : Type`
	AssignType,
	/// A constant literal, resolves to an expression.
	Literal(Literal, &'s str),
	Boolean(bool),

	OpenBracket(Bracket),
	ClosedBracket(Bracket),

	/// Identifier
	Ident(&'s str),
}

struct LexerState<'src> {
	buf: Vec<(Token<'src>, SimpleSpan<usize>)>,
}
impl<'src> LexerState<'src> {
	fn new() -> Self { LexerState { buf: Default::default() } }
	fn parser() -> impl Parser<'src, &'src str, (), extra::State<LexerState<'src>>> {
		let ident = unicode::ident().padded().map_slice(Token::Ident);

		let string = just('"')
			.then(any().filter(|c: &char| *c != '"').repeated())
			.then(just('"'))
			.map_slice(|s|Token::Literal(Literal::String, s));
		
		let tokens = choice((
			unicode::keyword("type").to(Token::TypeDef),
			unicode::keyword("let").to(Token::LetDef),
			just(':').padded().to(Token::AssignType),
			just(":=").padded().to(Token::AssignExpr),
		));

		// A token can be a string, a unicode ident, or another fixed token.
		string
			.or(ident)
			.or(tokens)
			.map_with_state(|token, span, state: &mut LexerState<'src>|state.buf.push((token, span)))
			.padded()
			.repeated()
	} 
}

type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;


/* pub enum ASTDef<'expr> {
	/// `let name := thing : Type`
	LetDef {
		name: Name<'expr>,
		val: &'expr ASTVal<'expr>,
		implied_type: Option<&'expr ASTVal<'expr>>
	},
	/// `type name : Type := thing
	TypeDef {
		typ: &'expr ASTVal<'expr>,
		default_name: Name<'expr>,
		default_val: Option<&'expr ASTVal<'expr>>
	},
} */

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
pub enum ASTExpr<'expr> {
	/// `let <name> := <term> : <type>`
	/// Requires <name> to be undefined in context, and for <term> and <type> to typecheck correctly
	LetDef {
		val: &'expr ASTExpr<'expr>,
		name: Name<'expr>,
		implied_type: Option<&'expr ASTExpr<'expr>>,
	},
	/// `<name>`
	Ref(Name<'expr>),
	/// `true`
	True,
	/// `false`
	False,
	/// Bool
	Bool,
	/// `"<string literal>"`
	ConstString(&'expr str),
	/// `'<unicode character>'`
	Char(&'expr str),
	/// `<integer or natural>`
	Integer(i64),
	/// `<floating point>`
	Decimal(f64),
	/// `{ <val>, <val> }`
	/// Unordered set. Set of terms is the term for a set of types.
	Set(&'expr [ASTExpr<'expr>]),
	/// `[ <val>, <val> ]`
	/// Ordered set. List of terms is a term for a list of types
	List(&'expr [ASTExpr<'expr>]),
	/// `<val> <val> -> <val>`
	/// This is constructed to contain the first two `<val>` in the function
	ImplicitList(&'expr [ASTExpr<'expr>]),
	/// `<val> -> <val>`
	/// A function abstraction
	Fn { input: &'expr ASTExpr<'expr>, ouput: &'expr ASTExpr<'expr> }
}

/// This represents a Type
pub enum ASTType<'expr> {
	/// Type definition with extra context
	TypeDef {
		typ: &'expr ASTType<'expr>,
		term_name: Option<&'expr Name<'expr>>, // Optionally assign a name for the term
		term_val: Option<&'expr ASTTerm<'expr>>, // Optionally assign a default value
	},
	/// Reference to type definition
	Ref(Name<'expr>),
	/// Unordered set type, i.e. `{type thing : Type, type thing2 : Type}`
	SetType(&'expr [ASTType<'expr>]),
	/// Ordered set type, i.e. `[Nat, Int, Type]`
	ListType(&'expr [ASTType<'expr>]),
	/// Function type, i.e. `Type -> Type`
	FnType { input: &'expr ASTType<'expr>, output: &'expr ASTType<'expr> },

	/// Type of all types, i.e. `Type`
	Type,
}

pub enum AST<'expr> {
	Val(ASTVal<'expr>),
}

fn parser<'tokens, 'src: 'tokens, 'expr: 'b, 'b, B: TypeStore<'b>, E: TypeStore<'expr>>(links: &'expr RevLinkStore<'expr, E>, binds: &'b B, bind_map: &'b NameBindStack<'expr>) -> impl Parser<
	'tokens,
	ParserInput<'tokens, 'src>, // Input
	Spanned<(SemanticTree<'expr>, &'b BindSubTree<'b>)>, // Output
	extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
	
	// Parses expression recursively
	recursive(|expr| {
		// A natural number
		
		/* let number = text::int::<_, Full<char>>(10).padded()
			.try_map(|s, span|
				s.parse::<usize>()
				.map_err(|e| Full::custom(span, format!("{}", e)))
			).try_map(|num, span| {
				match (lookup_expr(links, "zero"), lookup_expr(links, "succ")) {
					(Some(zero), Some(succ)) => {
						let expr = (0..num).into_iter().fold(zero, |acc, _|SemanticTree::app(succ.clone(), acc, links));
						Ok((
							expr,
							BindSubTree::NONE
						))
					}
					_ => Err(Full::custom(span, "names `zero` and `succ` must be defined to use numbers"))
				}
			}).labelled("number"); */
		
		// Parse valid name, number, or paranthesised expression.
		/* let atom = name_parser().try_map(|string, span| {
			if string == "*" { // Check if name is specifically unbound
				Ok((SemanticTree::VAR, BindSubTree::NONE))
			} else if let Some(val) = bind_map.name_index(&string) { // Check if name is bound
				Ok((SemanticTree::VAR, BindSubTree::end(val, binds)))
			} else if let Some(expr) = lookup_expr(links, &string) { // Check if name is defined
				Ok((expr, BindSubTree::NONE))
			} else { // Throw error if none of the above
				Err(Full::custom(span, "Name not bound or not defined, If you intended this to be an unbound variable, use `*`"))
			}
		})
    	.or(number) */
		.or(
			expr.clone().map(|(inner, bind)|(SemanticTree::parens(inner, links), bind))
			.delimited_by(just('('), just(')')
		).padded()).labelled("atom");

		// Parse lambda binding i.e. `[x y z]` and then recursively parse subexpresion (i.e. `x y z`)
		let lambda = name_parser()
    		.repeated().at_least(1)
			.delimited_by(just('['), just(']'))
			.map(|symbols| {
				let len = symbols.len();
				// For each binding (i.e. "x", "y", "z") in parsed `[x y z]`, push onto bind_map
				symbols.into_iter().for_each(|string|{
					let string = links.add(string);
					let name = Name::add(string, links);
					bind_map.push_name(name);
				});
				// Return iterator counting down to 0 for each symbol in the bind expression
				0..len
			}).then(expr.clone()).foldr(|symbol_idx: usize, (lam_expr, mut bind_tree)| { // Fold right, i.e. [x y] (...) -> Lam(x, Lam(y, ...))
				// get bind index and bind name
				let (bind_idx, name_bind) = bind_map.pop_name();
				// add bind_name 
				let name_bind = name_bind.expect("expected bound variables").clone();
				let binding = bind_tree.pop_binding(binds, &bind_idx, links).expect("failed to pop lambda");
				(
					SemanticTree::lambda(binding, name_bind, lam_expr, symbol_idx != 0, links),
					bind_tree
				)
			}).labelled("lambda");
		
		// Parse consecutive atoms, i.e. `x y z` as left-folded function application
		let application = atom.clone()
			.then(atom.clone().repeated().at_least(1))
			.foldl(|(func, func_index), (args, args_index)| {
				(
					SemanticTree::app(func, args, links),
					BindSubTree::branch(func_index, args_index, binds)
				)
			}).labelled("application");

		
		// An expression can be a lambda: `[x y]` an application: `x y` or an atom (name or nested expression)
		lambda.or(application).or(atom).padded().labelled("expression")
	}).then_ignore(end())
}
// Parse expression and register name tree
pub fn parse<'e>(string: &str, links: &'e RevLinkArena<'e>) -> Result<&'e SemanticTree<'e>, anyhow::Error> {
	let binds = &LinkArena::new();
	let bind_map = &NameBindStack::default();
	{
		let parsed = parser(links, binds, bind_map).parse(string);
		match parsed {
			Ok((expr, _)) => Ok(links.rev_add(expr)), // Register NameTreeExpr
			Err(errors) => {
				gen_report(errors).try_for_each(|report|report.print(Source::from(&string)))?;
				Err(anyhow::anyhow!("Error"))
			}
		}
	}
}

/// Generate cool errors with ariadne
pub fn gen_report<'a>(errors: impl IntoIterator<Item = Rich<'a, &'a str>>) -> impl Iterator<Item = Report<'a>> {
	// Taken from json.rs example on chumsky github
	errors.into_iter().map(|e| {
        let msg = if let chumsky::error::Rich::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => expected.to_string(),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                },
            )
        };

        let report = Report::build(ReportKind::Error, (), e.span().start)
            .with_code(3)
            .with_message(msg)
            .with_label(
                Label::new(e.span())
                    .with_message(match e.reason() {
                        chumsky::error::Rich::Custom(msg) => msg.clone(),
                        _ => format!(
                            "Unexpected {}",
                            e.found()
                                .map(|c| format!("token {}", c.fg(Color::Red)))
                                .unwrap_or_else(|| "end of input".to_string())
                        ),
                    })
                    .with_color(Color::Red),
            );

        let report = match e.reason() {
            chumsky::error::Rich::Unclosed { span, delimiter } => report.with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_color(Color::Yellow),
            ),
            chumsky::error::Rich::Unexpected => report,
            chumsky::error::Rich::Custom(_) => report,
        };

        report.finish()
    })
}

/// Parse and reduce a string
pub fn parse_reduce<'e>(string: &str, links: &'e RevLinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	let nte = parse(string, links)?;
	Ok(nte.expr.reduce(links)?)
}

/// Commands for cli
#[derive(Debug, Clone)]
pub enum Command<'e> {
	/// Do nothing
	None,
	/// Evaluate passed expression and store output in 
	Reduce(&'e SemanticTree<'e>),
	/// Set a name in a links to a certain value (`set thing <expr>`)
	Name(String, &'e SemanticTree<'e>),
	/// Get the definition of a passed name (`get thing`)
	Get(String),
	/// Check an expression using a type-expr
	Check(&'e SemanticTree<'e>, &'e SemanticTree<'e>),
	// Load a symbol from a file
	Load { /* name: String,  */file: String },
	// Save a symbol to a file, either overwriting or not overwriting the file.
	Save { /* name: String,  */file: String, overwrite: bool },
	/// Import names, if none listed, imports all names
	Use { name: String, items: Vec<String> },
	/// Clear current links
	Clear,
	/// List current links's names
	List(Vec<&'e Name<'e>>),
}
/// Parse commands
pub fn command_parser<'a, 'e: 'b, 'b>(links: &'e RevLinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b NameBindStack<'e>) -> impl Parser<'a, &'a str, Command<'e>> + 'b {
	let expr = parser(links, binds, bind_map);

	let filepath = just::<_, _, Full<'a, &'a str>>('"')
		.ignore_then(any().filter(|c| *c != '\\' && *c != '"').repeated())
		.then_ignore(just('"'))
		.collect::<String>()
    	.padded()
		.labelled("filepath");


	/* #[derive(Clone, Copy)]
	enum Comm { None, Set, List, Clear, Use, Load, Save, Reduce };
	let command = end().to(Comm::None)
    	.or(keyword("set").to(Comm::Set))
    	.or(keyword("list").to(Comm::List))
    	.or(keyword("clear").to(Comm::Clear))
    	.or(keyword("use").to(Comm::Use))
    	.or(keyword("load").to(Comm::Load))
    	.or(keyword("save").to(Comm::Save))
    .or(empty().to(Comm::Reduce))
    	.labelled("command").map(||) */

	let expr_test = expr.clone();
	end().to(Command::None)
    	.or(choice((
			keyword("set").ignore_then(ident().padded())
			.then(expr.clone()).map(|(symbol, (expr, _))| Command::Name(symbol, links.rev_add(expr))),
			keyword("get").ignore_then(ident().padded()).map(|name: String| Command::Get(name)),
			keyword("list").ignore_then(ident().padded().repeated()).map(|names: Vec<String>| 
				Command::List(
					names.into_iter().map(|name|Name::add(links.add(name), links)).collect_vec()
				)
			),
			keyword("load").ignore_then(filepath).map(|file|Command::Load { file }),
			keyword("save").ignore_then(filepath).map(|file|Command::Save { file, overwrite: false }),
		)))
		.or(
			// TODO: This is absolutely terrible, please replace
			any().take_until(just(':')).try_map(move |(expr_syms, _), _| expr_test.parse(&expr_syms[..]).map_err(|e|e[0].clone())).then(expr.clone()).map(|((expr, _), (ty, _))|Command::Check(links.rev_add(expr), links.rev_add(ty)))
		)
		.or(
			expr.clone().map(|(expr, _)|Command::Reduce(links.rev_add(expr)))
		)
		
		/* .or(
			expr.clone().then(end().map(|()|None).or(keyword(":").padded().ignore_then(expr.clone()).map(|e|Some(e))))
			.map(|((expr, _), option_type)| {
				if let Some((ty, _)) = option_type {
					Command::Check(links.rev_add(expr), links.rev_add(ty))
				} else {
					Command::Reduce(links.rev_add(expr))
				}
			}).labelled("reduce")
		) */
		.labelled("command")
}

#[test]
fn parse_test() {
	use crate::expr::Binding;
	use hashdb::LinkArena;

	let exprs = &LinkArena::new();
	let links = &RevLinkArena::new(exprs);
	// let links = &mut Context::new();
	let parsed = parse("[x y] x y", links).unwrap();
	let test = Expr::lambda(Binding::left(Binding::END, links),
	Expr::lambda(Binding::right(Binding::END, links),
			Expr::app(Expr::VAR, Expr::VAR, links),
		exprs),
	exprs);
	assert_eq!(parsed.expr, test);

	assert_eq!(test, parse("[x y] (x y)", links).unwrap().expr);

	let parsed = parse_reduce("([x y] x) ([x y] y) ([x y] x)", links).unwrap();
	let parsed_2 = parse("([x y] y)", links,).unwrap();
	assert_eq!(parsed, parsed_2.expr);

	let iszero = parse_reduce("[n] n ([u] [x y] y) ([x y] x)", links).unwrap();
	NamedExpr::new_linked("iszero", iszero, links);

	let test = parse_reduce("iszero ([x y] y)", links).unwrap();
	assert_eq!(test, parse("[x y] x", links).unwrap().expr)

	// Test semicolon detection
}