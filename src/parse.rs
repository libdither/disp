#![allow(unused, non_snake_case)]
use core::fmt;

use itertools::Itertools;
use winnow::{
	combinator::{
		self, alt, delimited, dispatch, empty, fail, opt, peek, preceded, repeat, separated, separated_pair, terminated,
	},
	error::{AddContext, ContextError, ErrMode, ErrorKind, ParseError, ParserError, StrContext, StrContextValue},
	prelude::*,
	stream::{Checkpoint, Stream},
	token::{any, one_of},
};

use crate::lexer::{lexer, Token};

#[derive(Clone, PartialEq, Debug)]
struct AssignIdentExpr {
	ident: Box<ParseTree>,
	def: Box<ParseTree>,
	typ: Option<Box<ParseTree>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParseTreeArgSetItem {
	ident: String,
	typ: Option<Box<ParseTree>>,
}
#[derive(Clone, PartialEq, Debug)]
pub enum ParseTreeSetItem {
	Atom(ParseTree),
	/// ident := def : Type
	/// ident : Type := def
	/// ident := def
	AssignIdentExpr {
		ident: String,
		def: Box<ParseTree>,
		typ: Option<Box<ParseTree>>,
	},
	AssignTypeIdent {
		ident: String,
		typ: Box<ParseTree>,
	},
}

#[derive(Clone, PartialEq, Debug)]
pub enum ParseTree {
	Number(u64),
	String(String),
	Ident(String),

	/// list or named set (or combination) in term form
	/// { thing1 := "abc" : String, thing2 := 123, 456, "seven" }
	/// list or named set of types
	/// { thing1 : String, thing2 : Number, OtherType }
	Set(Vec<ParseTreeSetItem>),
	/// `TypeSet(Vec<ParseTree>)`
	/// list of idents and an expression (λ expr)
	/// { val } -> {1,2,3,4,5}
	/// list of input types and ident or typeset (Π type)
	/// { val: Num } -> List{Num,val}
	Func {
		args: Vec<ParseTreeArgSetItem>, // arguments are a special type of set
		body: Box<ParseTree>,           // item, set, or another function
	},
	Apply {
		func: Box<ParseTree>,
		args: Box<ParseTree>,
	},
}

fn fmt_vec_parsetree(vec: &Vec<ParseTree>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match vec.len() {
		1 => write!(f, "{}", vec[0]),
		_ => write!(f, "{{{}}}", vec.iter().format(", ")),
	}
}
impl fmt::Display for ParseTreeArgSetItem {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} ", self.ident)?;
		if let Some(typ) = &self.typ {
			write!(f, " : {typ}")?;
		}
		Ok(())
	}
}
impl fmt::Display for ParseTreeSetItem {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParseTreeSetItem::Atom(parse_tree) => write!(f, "{parse_tree}"),
			ParseTreeSetItem::AssignIdentExpr { ident, def, typ } => {
				write!(f, "{ident} := {def}")?;
				if let Some(typ) = typ {
					write!(f, " : {typ}")?;
				}
				Ok(())
			}
			ParseTreeSetItem::AssignTypeIdent { ident, typ } => write!(f, "{ident} : {typ}"),
		}
	}
}
impl fmt::Display for ParseTree {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParseTree::Number(num) => write!(f, "{}", num),
			ParseTree::String(string) => write!(f, "\"{}\"", string),
			ParseTree::Ident(ident) => write!(f, "{}", ident),
			ParseTree::Set(elems) => write!(f, "{{{}}}", elems.iter().format(", ")),
			ParseTree::Func { args, body } => {
				match args.len() {
					1 => write!(f, "{}", args[0])?,
					_ => write!(f, "{{{}}}", args.iter().format(", "))?,
				}
				write!(f, "-> {body}",)
			}
			ParseTree::Apply { func, args } => write!(f, "{func} {args}"),
		}
	}
}

// literals
fn literal(input: &mut &[Token]) -> PResult<ParseTree> {
	any.verify_map(|t| match t {
		Token::Number(num) => Some(ParseTree::Number(num)),
		Token::String(string) => Some(ParseTree::String(string)),
		_ => None,
	})
	.context(StrContext::Label("literal"))
	.parse_next(input)
}

fn PTstring(string: &str) -> ParseTree {
	ParseTree::String(string.to_owned())
}
fn PTnum(num: u64) -> ParseTree {
	ParseTree::Number(num)
}

// specifically ident
fn ident(input: &mut &[Token]) -> PResult<String> {
	any.verify_map(|t| match t {
		Token::Ident(ident) => Some(ident),
		_ => None,
	})
	.context(StrContext::Label("ident"))
	.parse_next(input)
}

fn PTident(string: &str) -> ParseTree {
	ParseTree::Ident(string.to_owned())
}
fn PTargident(string: &str, typ: Option<ParseTree>) -> ParseTreeArgSetItem {
	ParseTreeArgSetItem {
		ident: string.to_owned(),
		typ: typ.map(Box::new),
	}
}

#[test]
fn test_parse_string() {
	assert_eq!(
		literal.parse_peek(&mut &lexer(&mut "\"string\"[").unwrap()),
		Ok((&[Token::OpenSquareBracket][..], ParseTree::String("string".to_owned())))
	);
}
#[test]
fn test_parse_num() {
	assert_eq!(
		literal.parse_peek(&mut &lexer(&mut "927834[").unwrap()),
		Ok((&[Token::OpenSquareBracket][..], ParseTree::Number(927834)))
	);
}

#[test]
fn test_parse_ident() {
	assert_eq!(
		ident.parse_peek(&mut &lexer(&mut "ident46[").unwrap()),
		Ok((&[Token::OpenSquareBracket][..], "ident46".to_owned()))
	);
}

pub fn paren_expr(input: &mut &[Token]) -> PResult<ParseTree> {
	delimited(
		one_of(Token::OpenParen).context(StrContext::Expected(StrContextValue::CharLiteral('('))),
		expr,
		one_of(Token::ClosedParen).context(StrContext::Expected(StrContextValue::CharLiteral(')'))),
	)
	.parse_next(input)
}
/* pub fn paren_typ(input: &mut &[Token]) -> PResult<ParseTree> {
	delimited(
		one_of(Token::OpenParen).context(StrContext::Expected(StrContextValue::CharLiteral('('))),
		typ,
		one_of(Token::ClosedParen).context(StrContext::Expected(StrContextValue::CharLiteral(')'))),
	)
	.parse_next(input)
} */

pub fn expr_atom(input: &mut &[Token]) -> PResult<ParseTree> {
	alt((ident.map(ParseTree::Ident), literal, expr_set, paren_expr))
		.context(StrContext::Label("expr"))
		.parse_next(input)
}
/// examples
/// thing
/// "yeet"
/// 123
/// { thing: Thing } -> thing
/// { thing: Thing } -> { thing }
/// thing -> thing -> thing
/// (thing thing)
/// (function args args2 arg3)
/// function {args, args, args}
pub fn expr(input: &mut &[Token]) -> PResult<ParseTree> {
	// check for function
	let maybe_args = opt(terminated(
		alt((
			args_set,
			ident.map(|s| vec![ParseTreeArgSetItem { ident: s, typ: None }]),
		)),
		one_of(Token::FuncOp),
	))
	.parse_next(input)?;
	if let Some(args) = maybe_args {
		Ok(ParseTree::Func {
			args: args,
			body: expr.map(Box::new).parse_next(input)?,
		})
	} else {
		repeat(1.., expr_atom)
			.fold(
				|| None,
				|func, args| {
					if let Some(func) = func {
						Some(ParseTree::Apply {
							func: Box::new(func),
							args: Box::new(args),
						})
					} else {
						Some(args)
					}
				},
			)
			.map(Option::unwrap)
			.context(StrContext::Label("expr"))
			.parse_next(input)
	}
}

fn assign_op(input: &mut &[Token]) -> PResult<()> {
	one_of(Token::AssignOp)
		.context(StrContext::Expected(StrContextValue::StringLiteral(":=")))
		.value(())
		.parse_next(input)
}
// parse :
fn typ_op(input: &mut &[Token]) -> PResult<()> {
	one_of(Token::TypeOp)
		.context(StrContext::Expected(StrContextValue::StringLiteral(":")))
		.value(())
		.parse_next(input)
}
fn set_item(input: &mut &[Token]) -> PResult<ParseTreeSetItem> {
	let ident = ident.parse_next(input)?;
	// parse := or :
	let is_assign = peek(alt((
		// dispatch doesn't work here for some reason?
		one_of(Token::AssignOp).value(true),
		one_of(Token::TypeOp).value(false),
		fail.context(StrContext::Expected(StrContextValue::StringLiteral(":=")))
			.context(StrContext::Expected(StrContextValue::StringLiteral(":"))),
	)))
	.parse_next(input)?;

	Ok(if is_assign {
		ParseTreeSetItem::AssignIdentExpr {
			ident,
			def: preceded(assign_op, expr.map(Box::new)).parse_next(input)?,
			typ: opt(preceded(typ_op, expr.map(Box::new))).parse_next(input)?,
		}
	} else {
		let typ = preceded(typ_op, expr.map(Box::new)).parse_next(input)?;
		let def = opt(preceded(assign_op, expr.map(Box::new))).parse_next(input)?;
		if let Some(def) = def {
			ParseTreeSetItem::AssignIdentExpr {
				ident,
				def,
				typ: Some(typ),
			}
		} else {
			ParseTreeSetItem::AssignTypeIdent { ident, typ }
		}
	})
}

fn argset_item(input: &mut &[Token]) -> PResult<ParseTreeArgSetItem> {
	combinator::trace(
		"type_assign",
		combinator::seq! {ParseTreeArgSetItem {
			ident: ident.context(StrContext::Label("ident")),
			typ: opt(preceded((one_of(Token::TypeOp))
			.context(StrContext::Expected(StrContextValue::StringLiteral(":"))), expr.map(Box::new))),
		}},
	)
	.parse_next(input)
}

fn PTsetitem(ident: &str, def: Option<ParseTree>, typ: Option<ParseTree>) -> ParseTreeSetItem {
	let ident = ident.to_owned();
	match (def, typ) {
		(None, None) => ParseTreeSetItem::Atom(ParseTree::Ident(ident)),
		(None, Some(typ)) => ParseTreeSetItem::AssignTypeIdent {
			ident,
			typ: Box::new(typ),
		},
		(Some(def), typ) => ParseTreeSetItem::AssignIdentExpr {
			ident,
			def: Box::new(def),
			typ: typ.map(Box::new),
		},
	}
}

fn PTargitem(ident: String, typ: Option<ParseTree>) -> ParseTreeArgSetItem {
	ParseTreeArgSetItem {
		ident,
		typ: typ.map(Box::new),
	}
}

#[test]
fn test_parse_def() {
	// test assign and typing
	assert_eq!(
		set_item.parse_peek(&mut &lexer(&mut "one := 123 : Number :").unwrap()),
		Ok((
			&[Token::TypeOp][..],
			PTsetitem("one", Some(ParseTree::Number(123)), Some(PTident("Number")))
		))
	);
	// test typing and assign
	assert_eq!(
		set_item.parse_peek(&mut &lexer(&mut "one : Number := 123 :").unwrap()),
		Ok((
			&[Token::TypeOp][..],
			PTsetitem("one", Some(ParseTree::Number(123)), Some(PTident("Number")))
		))
	);
	// test no type
	assert_eq!(
		set_item.parse_peek(&mut &lexer(&mut "one := 123").unwrap()),
		Ok((&[][..], PTsetitem("one", Some(ParseTree::Number(123)), None)))
	);
	// test assign func
	assert_eq!(
		set_item.parse_peek(&mut &lexer(&mut "id := x -> x").unwrap()),
		Ok((
			&[][..],
			PTsetitem("id", Some(PTefunc(vec![PTargident("x", None)], PTident("x"))), None)
		))
	);
	// test assign application
	assert_eq!(
		set_item.parse_peek(&mut &lexer(&mut "one := succ zero").unwrap()),
		Ok((
			&[][..],
			PTsetitem("one", Some(PTeapp(PTident("succ"), PTident("zero"))), None)
		))
	);

	// fail tests
	// test invalid assign op
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "a = 123").unwrap())),
		dbg!(Err(ErrMode::Backtrack(
			ContextError::new()
				.add_context(
					&"",
					&"".checkpoint(),
					StrContext::Expected(StrContextValue::StringLiteral(":="))
				)
				.add_context(
					&"",
					&"".checkpoint(),
					StrContext::Expected(StrContextValue::StringLiteral(":"))
				)
		)))
	);
	// test if valid ident
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "{} := 123").unwrap())),
		dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Label("ident")
		))))
	);
	// test if invalid ident is caught
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
		dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Label("ident")
		))))
	);
	// test if invalid expr is caught
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
		dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Label("expr")
		))))
	);
}

#[test]
fn test_parse_typing() {
	assert_eq!(
		argset_item.parse_peek(&mut dbg!(&lexer(&mut "ident : Number Thingy").unwrap())),
		Ok((
			&[][..],
			PTargitem("ident".to_owned(), Some(PTeapp(PTident("Number"), PTident("Thingy"))))
		))
	);
	// test if := is caught
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "ident := 123").unwrap())),
		Ok((&[][..], PTsetitem("ident", Some(PTnum(123)), None)))
	);
	// test if invalid ident is caught
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
		dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Label("ident")
		))))
	);
	// test if invalid expr is caught
	assert_eq!(
		set_item.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
		dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Label("expr")
		))))
	);
}

fn expr_set(input: &mut &[Token]) -> PResult<ParseTree> {
	delimited(
		one_of(Token::OpenCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
		separated(
			0..,
			alt((
				set_item,
				ident.map(|s| ParseTreeSetItem::Atom(ParseTree::Ident(s))),
				literal.map(ParseTreeSetItem::Atom),
			)),
			one_of(|t| matches!(t, Token::Separator(_)))
				.context(StrContext::Expected(StrContextValue::CharLiteral(',')))
				.context(StrContext::Expected(StrContextValue::StringLiteral("\\n"))),
		),
		one_of(Token::ClosedCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('}'))),
	)
	.map(ParseTree::Set)
	.parse_next(input)
}

fn PTexprset(set: &[ParseTreeSetItem]) -> ParseTree {
	ParseTree::Set(set.to_owned())
}

#[test]
fn test_parse_expr_set() {
	assert_eq!(
		expr_set.parse(&mut dbg!(&lexer(
			&mut "{ thingy := 123: Number, other_thingy := \"hi\", third_thingy, 456 }"
		)
		.unwrap())),
		Ok(PTexprset(&[
			PTsetitem("thingy", Some(ParseTree::Number(123)), Some(PTident("Number"))),
			PTsetitem("other_thingy", Some(PTstring("hi")), None),
			ParseTreeSetItem::Atom(PTident("third_thingy")),
			ParseTreeSetItem::Atom(PTnum(456)),
		]))
	);
	let lex = dbg!(lexer(&mut "{ thingy := 123: Number, other_thingy := }").unwrap());
	let parse_res = expr_set.parse(&mut &lex).unwrap_err();
	assert_eq!(
		parse_res.inner(),
		&ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Expected(StrContextValue::CharLiteral('}'))
		)
	);
}

/* fn typ_set(input: &mut &[Token]) -> PResult<ParseTree> {
	delimited(
		one_of(Token::OpenCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
		separated(
			0..,
			alt((typ_assign, ident.map(ParseTree::Ident))),
			one_of(|t| matches!(t, Token::Separator(_)))
				.context(StrContext::Expected(StrContextValue::CharLiteral(',')))
				.context(StrContext::Expected(StrContextValue::StringLiteral("\\n"))),
		),
		one_of(Token::ClosedCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('}'))),
	)
	.map(ParseTree::Set)
	.parse_next(input)
}

fn PTtypset(set: &[ParseTree]) -> ParseTree {
	ParseTree::Set(set.to_owned())
} */

/* #[test]
fn test_parse_typ_set() {
	assert_eq!(
		typ_set.parse(&mut dbg!(
			&lexer(&mut "{ thingy: Number, String, String, lol: Lol }").unwrap()
		)),
		Ok(PTtypset(&[
			PTargitem("thingy".to_owned(), PTident("Number")),
			PTident("String"),
			PTident("String"),
			PTargitem("lol".to_owned(), PTident("Lol")),
		]))
	);
	let lex = dbg!(lexer(&mut "{ thingy := 123: Number, other_thingy := }").unwrap());
	let parse_res = expr_set.parse(&mut &lex).unwrap_err();
	assert_eq!(
		parse_res.inner(),
		&ContextError::new().add_context(
			&"",
			&"".checkpoint(),
			StrContext::Expected(StrContextValue::CharLiteral('}'))
		)
	);
}
 */
fn args_set(input: &mut &[Token]) -> PResult<Vec<ParseTreeArgSetItem>> {
	delimited(
		one_of(Token::OpenCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
		separated(
			0..,
			ident.map(|s| ParseTreeArgSetItem { ident: s, typ: None }),
			one_of(|t| matches!(t, Token::Separator(_)))
				.context(StrContext::Expected(StrContextValue::CharLiteral(',')))
				.context(StrContext::Expected(StrContextValue::StringLiteral("\\n"))),
		),
		one_of(Token::ClosedCurlyBracket).context(StrContext::Expected(StrContextValue::CharLiteral('}'))),
	)
	.parse_next(input)
}

fn PTefunc(args: Vec<ParseTreeArgSetItem>, body: ParseTree) -> ParseTree {
	ParseTree::Func {
		args,
		body: Box::new(body),
	}
}

#[test]
fn test_parse_expr_func() {
	assert_eq!(
		expr.parse(&mut dbg!(&lexer(&mut "x -> x").unwrap())),
		Ok(PTefunc(vec![PTargident("x", None)], PTident("x")))
	);
	assert_eq!(
		dbg!(expr.parse(&mut &lexer(&mut "x -> x -> x").unwrap())),
		Ok(PTefunc(
			vec![PTargident("x", None)],
			PTefunc(vec![PTargident("x", None)], PTident("x"))
		))
	);
	assert_eq!(
		dbg!(expr.parse(&mut dbg!(&lexer(&mut "x -> {y} -> x").unwrap()))),
		Ok(PTefunc(
			vec![PTargident("x", None)],
			PTefunc(vec![PTargident("y", None)], PTident("x"))
		))
	);
}

fn PTeapp(func: ParseTree, args: ParseTree) -> ParseTree {
	ParseTree::Apply {
		func: Box::new(func),
		args: Box::new(args),
	}
}

#[test]
fn test_parse_expr_app() {
	// normal
	assert_eq!(
		expr.parse(&mut dbg!(&lexer(&mut "a b c").unwrap())),
		Ok(PTeapp(PTeapp(PTident("a"), PTident("b")), PTident("c")))
	);
	// parens
	assert_eq!(
		dbg!(expr.parse(&mut &lexer(&mut "a (b c)").unwrap())),
		Ok(PTeapp(PTident("a"), PTeapp(PTident("b"), PTident("c"))))
	);
	//
	assert_eq!(
		dbg!(expr.parse(&mut dbg!(&lexer(&mut "(a)(b)(c)").unwrap()))),
		Ok(PTeapp(PTeapp(PTident("a"), PTident("b")), PTident("c")))
	);
}

pub fn parse_file(input: &mut &[Token]) -> PResult<Vec<ParseTreeSetItem>> {
	repeat(
		0..,
		delimited(
			opt(one_of(|t| matches!(t, Token::Separator(_)))),
			set_item,
			opt(one_of(|t| matches!(t, Token::Separator(_)))),
		),
	)
	.parse_next(input)
}
