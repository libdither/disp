#![allow(dead_code)]

use std::cell::RefCell;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, text::{unicode::{ident, keyword}, whitespace}, recursive::Direct, extra::ParserExtra};
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
	fn name_index(&self, string: &str) -> Option<usize> {
		self.stack.borrow().iter().enumerate().rev().find(|(_, &n)|n == &string).map(|val|val.0 + 1)
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
	L: for<'s> ArchiveFetchable<'e, ArchiveToType<'s, 'e, Datastore, TS>> + ArchiveStorable<Datastore>,
	L::Archived: for<'v> rkyv::CheckBytes<rkyv::validation::validators::DefaultValidator<'v>>,
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


/* fn name_parser<'a>() -> impl Parser<'a, &'a str, &'a str> + Clone {
	just('*').to("*".to_string()).or(ident()).labelled("name")
} */

pub struct ParserState<'e: 'b, 'b, B: TypeStore<'b> + 'b, E: TypeStore<'e> + 'e> {
	/// Storage for the output of the parser
	pub links: &'e RevLinkStore<'e, E>,
	/// Storage for active data used during parsing
	pub binds: &'b B,
	/// Name binding
	pub bind_map: NameBindStack<'e>
}
type CustomExtra<'i, 'e, 'b, B, E> = extra::Full<Rich<'i, char>, ParserState<'e, 'b, B, E>, ()>;
type CustomParser<'i, 'e: 'b + 'i, 'b: 'i, B: TypeStore<'b> + 'b, E: TypeStore<'e> + 'e> = impl Parser<'i, &'i str, (SemanticTree<'e>, &'b BindSubTree<'b>), CustomExtra<'i, 'e, 'b, B, E>> + Clone;

fn expr_parser<'i, 'e: 'b + 'i, 'b: 'i, B: TypeStore<'b> + 'b, E: TypeStore<'e> + 'e>() -> CustomParser<'i, 'e, 'b, B, E> {
	// Parses expression recursively
	recursive(|expr: Recursive<Direct<'i, 'i, &'i str, (SemanticTree<'e>, &'b BindSubTree<'b>), CustomExtra<'i, 'e, 'b, B, E>>>| {
		// A natural number
		let number = text::int::<_, _, CustomExtra<_, _>>(10).padded()
			.try_map(|s: &str, span|
				s.parse::<usize>()
				.map_err(|e| Rich::custom(span, format!("{}", e)))
			).try_map_with(|num, e| {
				match (lookup_expr(e.state().links, "zero"), lookup_expr(e.state().links, "succ")) {
					(Some(zero), Some(succ)) => {
						let expr = (0..num).into_iter().fold(zero, |acc, _|SemanticTree::app(succ.clone(), acc, e.state().links));
						Ok((
							expr,
							BindSubTree::NONE
						))
					}
					_ => Err(Rich::custom(e.span(), "names `zero` and `succ` must be defined to use numbers"))
				}
			}).labelled("number");
		
		// Parse valid name, number, or paranthesised expression.
		let atom = ident::<_, _, CustomExtra<_, _>>().try_map_with(|string: &str, e| {
			let state = e.state();
			if string == "*" { // Check if name is specifically unbound
				Ok((SemanticTree::VAR, BindSubTree::NONE))
			} else if let Some(val) = state.bind_map.name_index(string) { // Check if name is bound
				Ok((SemanticTree::VAR, BindSubTree::end(val, state.binds)))
			} else if let Some(expr) = lookup_expr(state.links, &string) { // Check if name is defined
				Ok((expr, BindSubTree::NONE))
			} else { // Throw error if none of the above
				Err(Rich::custom(e.span(), "Name not bound or not defined, If you intended this to be an unbound variable, use `*`"))
			}
		})
    	.or(number)
		.or( // atom expression that is parenthesized
			expr.clone().map_with(|(inner, bind), e|(SemanticTree::parens(inner, e.state().links), bind))
			.delimited_by(just('('), just(')')
		)).padded().labelled("atom");

		// Parse lambda binding i.e. `[x y z]` and then recursively parse subexpresion (i.e. `x y z`)
		let lambda = ident().padded()
			.repeated().at_least(1).collect::<Vec<_>>()
			.delimited_by(just('['), just(']'))
			.map_with(|symbols: Vec<&str>, e: &mut chumsky::combinator::MapExtra<&'i str, CustomExtra<B, E>>| {
				// let state = e.state();
				let len = symbols.len();
				// For each binding (i.e. "x", "y", "z") in parsed `[x y z]`, push onto bind_map
				symbols.into_iter().for_each(|string|{
					let string = e.state().links.add(string.to_owned());
					let name = Name::add(string, e.state().links);
					e.state().bind_map.push_name(name);
				});
				// Return iterator counting down to 0 for each symbol in the bind expression
				0..len
			}).into_iter()
			.foldr_with(expr.clone(), |symbol_idx, (lam_expr, mut bind_tree), e| { // Fold right, i.e. [x y] (...) -> Lam(x, Lam(y, ...))
				let state = e.state();
				// get bind index and bind name
				let (bind_idx, name_bind) = state.bind_map.pop_name();
				// add bind_name 
				let name_bind = name_bind.expect("expected bound variables");
				let binding = bind_tree.pop_binding(state.binds, &bind_idx, state.links).expect("failed to pop lambda");
				(
					SemanticTree::lambda(binding, name_bind, lam_expr, symbol_idx != 0, state.links),
					bind_tree
				)
			})
			.labelled("lambda");
		
		// Parse consecutive atoms, i.e. `x y z` as left-folded function application
		let application = atom.clone()
			.foldl_with(atom.clone().repeated().at_least(1), |(func, func_index), (args, args_index), e| {
				(
					SemanticTree::app(func, args, e.state().links),
					BindSubTree::branch(func_index, args_index, e.state().binds)
				)
			}).labelled("application");

		
		// An expression can be a lambda: `[x y]` an application: `x y` or an atom (name or nested expression)
		lambda.or(application).or(atom).padded().labelled("expression")
	})
}
// Parse expression and register name tree
pub fn parse<'e>(string: &str, links: &'e RevLinkArena<'e>) -> Result<&'e SemanticTree<'e>, anyhow::Error> {
	let binds = &LinkArena::new();
	let bind_map = NameBindStack::default();
	let mut state = ParserState {
		links, binds, bind_map
	};
	{
		let parsed = expr_parser().parse_with_state(string, &mut state).into_result();
		match parsed {
			Ok((expr, _)) => Ok(state.links.rev_add(expr)), // Register NameTreeExpr
			Err(errors) => {
				gen_report(errors).try_for_each(|report|report.print(Source::from(&string)))?;
				Err(anyhow::anyhow!("Error"))
			}
		}
	}
}
pub fn pretty_parse<'i, O: std::fmt::Debug, E: ParserExtra<'i, &'i str, Error = Rich<'i, char>>, P: Parser<'i, &'i str, O, E>>(string: &'i str, parser: P, state: &mut E::State)
where <E as ParserExtra<'i, &'i str>>::Context: std::default::Default
{
	let parsed = parser.parse_with_state(string, state).into_result();
	match parsed {
		Ok(out) => println!("parsed: {out:?}"), // Register NameTreeExpr
		Err(errors) => {
			gen_report(errors).try_for_each(|report|report.print(Source::from(&string))).unwrap()
		}
	}
}

pub fn gen_report<'a>(errors: impl IntoIterator<Item = Rich<'a, char>>) -> impl Iterator<Item = Report<'a>> {
	errors.into_iter().map(|e| {
		let msg = match e.reason() {
			chumsky::error::RichReason::Many(errs) => errs.iter().map(|e|format!("{e}")).join(","),
			_ => {e.to_string()}
		};

        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(msg)
            .with_label(
                Label::new(e.span().into_range())
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            ).with_labels(e.contexts().map(|(label, span)| {
				Label::new(span.into_range())
					.with_message(format!("while parsing this {}", label))
					.with_color(Color::Yellow)
			}))
            .finish()
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

fn filepath_parser<'i, E: ParserExtra<'i, &'i str, Error = Rich<'i, char>>>() -> impl Parser<'i, &'i str, &'i str, E> {
	none_of("\"")
	//any::<'i, &'i str, _>()
	.repeated()
	.to_slice()
	.delimited_by(just('"'), just('"'))
	.labelled("filepath").as_context()
}

/// Parse commands
pub fn command_parser<'i, 'e: 'i + 'b, 'b: 'i, B: TypeStore<'b> + 'b, E: TypeStore<'e> + 'e>() -> impl Parser<'i, &'i str, Command<'e>, CustomExtra<'i, 'e, 'b, B, E>> {
	let expr = expr_parser();

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
    	.labelled("keyword"); */
	
	keyword("set").labelled("set").ignore_then(ident().labelled("set ident").padded()).then(expr.clone()).map_with(|(symbol, (expr, _)), e| Command::Name(symbol.to_owned(), e.state().links.rev_add(expr)))

	.or(
	keyword("get").ignore_then(ident().labelled("get ident").padded()).map(|name: &str| Command::Get(name.to_owned()))
	)

	.or(
	keyword("list").ignore_then(ident().padded().repeated().collect::<Vec<&str>>().map_with(|names, e: &mut MapExtra<&'i str, CustomExtra<B, E>>| 
		Command::List(
			names.into_iter().map(|name|Name::add(e.state().links.add(name.to_owned()), e.state().links)).collect_vec()
		)
	))
		.or(
		keyword("eval").ignore_then(expr.clone().padded()).map_with(|(expr, _), extra|Command::Reduce(extra.state().links.rev_add(expr)))
		)
	).or(
		keyword("load").padded().ignore_then(filepath_parser().padded()).map(|file|Command::Load { file: file.to_owned() })
		.labelled("load").as_context()
		.or(
			keyword("save").ignore_then(filepath_parser().padded()).map(|file|Command::Save { file: file.to_owned(), overwrite: false })
		)
	).labelled("command").as_context()
		
}

#[test]
fn parse_test() {
	use crate::expr::Binding;
	use hashdb::LinkArena;

	let exprs = &LinkArena::new();
	let links = &RevLinkArena::new(exprs);
	// let links = &mut Context::new();
	let parsed = parse("[x y] (x y)", links).unwrap();
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

fn test_label_parse(string: &str) -> ParseResult<&str, Rich<'_, char>> {
	let parser = keyword::<&str, char, &str, extra::Err<Rich<char>>>("first");
	parser.labelled("first").as_context().parse(string)
}

/* #[test]
fn label_test() {
	let uses_label = test_label_parse("[first");
	let incorrect = test_label_parse("firsd");
	uses_label.errors().for_each(|err|println!("{err}"));
	incorrect.errors().for_each(|err|println!("{err}"));
	panic!();
}

#[test]
fn filepath_test() {
	// let eval = keyword("eval").padded().labelled("keyword").ignore_then(ident()).labelled("eval").as_context();
	let parser = just("test").then(whitespace().at_least(1)).labelled("keyword").as_context().ignore_then(filepath_parser::<extra::Err<Rich<char>>>()).labelled("command").as_context();
	pretty_parse("testp test", parser, &mut ());
	panic!()
} */