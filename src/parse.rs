use std::cell::RefCell;

use ariadne::{Color, Label, Report, Fmt, ReportKind, Source};
use chumsky::{prelude::*, text::keyword};
use hashdb::{LinkArena, LinkSerializer, NativeHashtype};

use crate::{expr::{BindSubTree, Expr}, symbol::Symbol};

// Represents active bound variables in the course of parsing an expression
#[derive(Default, Debug)]
pub struct BindMap {
	map: RefCell<Vec<String>>,
}
impl BindMap {
	// Get binding index for this variable
	fn bind_index(&self, string: &String) -> Option<usize> {
		self.map.borrow().iter().enumerate().rev().find(|(index, e)|*e == string).map(|val|val.0 + 1)
	}
	fn push_bind(&self, string: &String) -> usize {
		let mut map = self.map.borrow_mut();
		map.push(string.clone());
		map.len()
	}
	fn pop_bind(&self, string: &String) -> Option<usize> {
		let mut map = self.map.borrow_mut();
		let ret = map.len();
		map.pop()?;
		Some(ret)
	}
}

fn lookup_expr<'a>(string: &str, exprs: &'a LinkArena<'a>) -> Option<&'a Expr<'a>> {
	thread_local! {
		static SER: RefCell<LinkSerializer> = RefCell::new(LinkSerializer::new());
	}
	SER.with(|ser| {
		let string_hash = string.to_owned().store(&mut *ser.borrow_mut());
		let symbol = exprs.lookup::<Symbol, String>(&string_hash)?;
		Some(symbol.expr)
	})
}

fn parser<'e: 'b, 'b>(exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b BindMap) -> impl Parser<char, (&'e Expr<'e>, &'b BindSubTree<'b>), Error = Simple<char>> + Clone {
	recursive(|expr: Recursive<'b, char, (&'e Expr<'e>, &'b BindSubTree<'b>), Simple<char>>| {
		// A symbol, can be pretty much any string not including whitespace
		let symbol = text::ident().padded().labelled("symbol");

		let number = text::int::<_, Simple<char>>(10).padded()
			.try_map(|s, span|
				s.parse::<usize>()
				.map_err(|e| Simple::custom(span, format!("{}", e)))
			).try_map(|num, span| {
				match (lookup_expr("zero", exprs), lookup_expr("succ", exprs)) {
					(Some(zero), Some(succ)) => {
						let expr = (0..num).into_iter().fold(zero, |acc, _|Expr::app(succ, acc, exprs));
						Ok((expr, BindSubTree::NONE))
					}
					_ => Err(Simple::custom(span, "symbols `zero` and `succ` must be defined to use numbers"))
				}
			}).labelled("number");

		// A resolved symbol, variable, or paranthesised expression.
		let atom = symbol.clone().map(|string| {
			if let Some(val) = bind_map.bind_index(&string) {
				(Expr::VAR, BindSubTree::end(val, binds))
			} else if let Some(expr) = lookup_expr(&string, exprs) {
				(expr, BindSubTree::NONE)
			} else { (Expr::VAR, BindSubTree::NONE) }
		}).labelled("expression")
    	.or(number)
		.or(expr.clone().delimited_by(just('('), just(')')).padded());

		// Parse `[x y z] x y z` as `[x] ([y] ([z] x y z))`
		let lambda = symbol
    		.repeated().at_least(1)
			.delimited_by(just('['), just(']'))
			.map(|symbols| {
				symbols.iter().map(|string|{
					bind_map.push_bind(string)
				}).collect::<Vec<_>>()
			}).then(expr.clone()).foldr(|bind_index, (lam_expr, mut bind_tree)| {
				let binding = bind_tree.pop_binding(binds, bind_index, exprs).expect("failed to pop lambda");
				(Expr::lambda(binding, lam_expr, exprs), bind_tree)
			}).labelled("lambda");
		
		// Parse `x y z` as `((x y) z)`
		let application = atom.clone()
			.then(atom.clone().repeated().at_least(1))
			.foldl(|(func, func_index), (args, args_index)| {
				(Expr::app(func, args, exprs), BindSubTree::branch(func_index, args_index, binds))
			}).labelled("application");


		// An expression can be a lambda: `[x y]` an application: `x y` or a standalone variable / symbol: `x`
		lambda.or(application).or(atom).padded().labelled("expression")
	}).then_ignore(end())
}
pub fn parse<'e>(string: &str, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	let binds = &LinkArena::new();
	let bind_map = &BindMap::default();
	{
		let parsed = parser(exprs, binds, bind_map).parse(string);
		match parsed {
			Ok((expr, _)) => Ok(expr),
			Err(errors) => {
				gen_report(errors).try_for_each(|report|report.print(Source::from(&string)))?;
				Err(anyhow::anyhow!("Error"))
			}
		}
	}
}

pub fn gen_report(errors: Vec<Simple<char>>) -> impl Iterator<Item = Report> {
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

pub fn parse_reduce<'e>(string: &str, exprs: &'e LinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	Ok(crate::beta_reduce(parse(string, exprs)?, exprs)?)
}

#[derive(Debug, Clone)]
pub enum Command<'e> {
	None,
	Reduce(&'e Expr<'e>),
	Set(String, &'e Expr<'e>),
}
pub fn command_parser<'e: 'b, 'b>(exprs: &'e LinkArena<'e>, binds: &'b LinkArena<'b>, bind_map: &'b BindMap) -> impl Parser<char, Command<'e>, Error = Simple<char>> + 'b {
	let expr = parser(exprs, binds, bind_map);

	let reduce = expr.clone().map(|(expr, _)|Command::Reduce(expr));

	let none = end().to(Command::None);

	let set = keyword("set")
		.ignore_then(text::ident().padded())
		.then(expr.clone()).map(|(symbol, (expr, _))| Command::Set(symbol, expr));

	set.or(none).or(reduce).labelled("command")
}

#[test]
fn parse_test() {
	use crate::expr::Binding;

	let exprs = &LinkArena::new();
	let ser = &mut LinkSerializer::new();
	let parsed = parse("[x y] x y", exprs).unwrap();
	let test = Expr::lambda(Binding::left(Binding::END, exprs),
	Expr::lambda(Binding::right(Binding::END, exprs),
			Expr::app(Expr::VAR, Expr::VAR, exprs),
		exprs),
	exprs);
	assert_eq!(parsed, test);

	assert_eq!(test, parse("[x y] (x y)", exprs).unwrap());

	let parsed = parse_reduce("([x y] x) ([x y] y) ([x y] x)", exprs).unwrap();
	let parsed_2 = parse("([x y] y)", exprs).unwrap();
	assert_eq!(parsed, parsed_2);

	let iszero = parse_reduce("[n] n ([u] [x y] y) ([x y] x)", exprs).unwrap();
	Symbol::new("iszero", iszero, exprs, ser);

	let test = parse_reduce("iszero ([x y] y)", exprs).unwrap();
	assert_eq!(test, parse("[x y] x", exprs).unwrap())
}