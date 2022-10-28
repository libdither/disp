#![allow(dead_code)]

use std::{cell::RefCell, iter};

use ariadne::{Color, Label, Report, Fmt, ReportKind, Source};
use chumsky::{prelude::*, text::keyword};
use hashdb::{HashType, LinkArena, RevHashType, RevLinkArena, RevLinkStore, RevTypeStore, TypeStore};

use crate::{expr::{BindSubTree, Expr}, name::{self, Name, SemanticTree, NamedExpr}};

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
fn lookup<'e, L: RevHashType<'e>>(links: &'e impl RevTypeStore<'e>, object: &'e impl HashType<'e>, predicate: impl Fn(&&'e L) -> bool) -> Option<&'e L> {
	links.links::<_, L>(object).find(predicate)
}

fn lookup_expr<'e>(links: &'e impl RevTypeStore<'e>, string: &str) -> Option<SemanticTree<'e>> {
	let name = Name::add(links.add(string.to_string()), links);
	println!("looking up: {:?}", string);
	let named_expr: &'e NamedExpr<'e> = lookup::<NamedExpr>(links, name, |expr| {
		true // Return first expression found
	})?;
	println!("found {name:?}");

	Some(SemanticTree::named(named_expr, links))
}


fn name_parser() -> impl Parser<char, String, Error = Simple<char>> + Clone {
	text::ident().padded().labelled("name")
}

fn parser<'e: 'b, 'b, B: TypeStore<'b>, E: TypeStore<'e>>(links: &'e RevLinkStore<'e, E>, binds: &'b B, bind_map: &'b NameBindStack<'e>) -> impl Parser<char, (SemanticTree<'e>, &'b BindSubTree<'b>), Error = Simple<char>> + Clone {
	recursive(|expr: Recursive<'b, char, (SemanticTree<'e>, &'b BindSubTree<'b>), Simple<char>>| {
		// A symbol, can be pretty much any string not including whitespace
		let number = text::int::<_, Simple<char>>(10).padded()
			.try_map(|s, span|
				s.parse::<usize>()
				.map_err(|e| Simple::custom(span, format!("{}", e)))
			).try_map(|num, span| {
				match (lookup_expr(links, "zero"), lookup_expr(links, "succ")) {
					(Some(zero), Some(succ)) => {
						let expr = (0..num).into_iter().fold(zero, |acc, _|SemanticTree::app(succ.clone(), acc, links));
						Ok((
							expr,
							BindSubTree::NONE
						))
					}
					_ => Err(Simple::custom(span, "names `zero` and `succ` must be defined to use numbers"))
				}
			}).labelled("number");

		// A resolved symbol, variable, or paranthesised expression.
		let atom = name_parser().try_map(|string, span| {
			println!("found name: {:?}", string);
			if string == "*" { // Check if name is specifically unbound
				Ok((SemanticTree::VAR, BindSubTree::NONE))
			} else if let Some(val) = bind_map.name_index(&string) { // Check if name is bound
				Ok((SemanticTree::VAR, BindSubTree::end(val, binds)))
			} else if let Some(expr) = lookup_expr(links, &string) { // Check if name is defined
				Ok((expr, BindSubTree::NONE))
			} else {
				Err(Simple::custom(span, "Name not bound or not defined, If you intended this to be an unbound variable, use `*`"))
			}
		}).labelled("expression")
    	.or(number)
		.or(
			expr.clone().map(|(inner, bind)|(SemanticTree::parens(inner, links), bind))
			.delimited_by(just('('), just(')')
		).padded());

		// Parse `[x y z] x y z` as `[x] ([y] ([z] x y z))`
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
		
		// Parse `x y z` as `((x y) z)`
		let application = atom.clone()
			.then(atom.clone().repeated().at_least(1))
			.foldl(|(func, func_index), (args, args_index)| {
				(
					SemanticTree::app(func, args, links),
					BindSubTree::branch(func_index, args_index, binds)
				)
			}).labelled("application");


		// An expression can be a lambda: `[x y]` an application: `x y` or a standalone variable / symbol: `x`
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
pub fn gen_report(errors: Vec<Simple<char>>) -> impl Iterator<Item = Report> {
	// Taken from json.rs example on chumsky github
	errors.into_iter().map(|e| {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
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
                        chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
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
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report.with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_color(Color::Yellow),
            ),
            chumsky::error::SimpleReason::Unexpected => report,
            chumsky::error::SimpleReason::Custom(_) => report,
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
	// Do nothing
	None,
	// Set a name in a links to a certain value
	Set(String, &'e SemanticTree<'e>),
	// Load a symbol from a file
	Load { /* name: String,  */file: String },
	// Save a symbol to a file, either overwriting or not overwriting the file.
	Save { /* name: String,  */file: String, overwrite: bool },
	/// Import names, if none listed, imports all names
	Use { name: String, items: Vec<String> },
	/// Clear current links
	Clear,
	/// List current links's names
	List,
	// Evaluate passed expression and store output in 
	Reduce(&'e SemanticTree<'e>),
}
/// Parse commands
pub fn command_parser<'e: 'b, 'b>(links: &'e RevLinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b NameBindStack<'e>) -> impl Parser<char, Command<'e>, Error = Simple<char>> + 'b {
	let expr = parser(links, binds, bind_map);

	let filepath = just::<_, _, Simple<char>>('"')
		.ignore_then(filter(|c| *c != '\\' && *c != '"').repeated())
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

	end().to(Command::None)
    	.or(choice((
			keyword("set")
			.ignore_then(text::ident().padded())
			.then(expr.clone()).map(|(symbol, (expr, _))| Command::Set(symbol, links.rev_add(expr))),
			keyword("list").to(Command::List),
			keyword("load").ignore_then(filepath).map(|file|Command::Load { file }),
			keyword("save").ignore_then(filepath).map(|file|Command::Save { file, overwrite: false }),
		)))
		.or(
			expr.clone().map(|(expr, _)|Command::Reduce(links.rev_add(expr)))
		)
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
}