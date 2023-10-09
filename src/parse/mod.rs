#![allow(dead_code)]

pub mod lexer;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{prelude::*, combinator::MapExtra};
use hashdb::{TypeStore, hashtype};

use lexer::{Token, ParserInput, Spanned};
use rkyv::{Archive, Deserialize, Serialize};

use self::lexer::Span;

/// A Literal expression
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum Literal {
	String, // String literal, "thing"
	Char, // Char literal, 'λ'
	Integer, // Integer literal, 123
	Decimal, // Decimal literal, 123.456
}

/// Separators for sets or lists may be Comma
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum Separator {
	Comma,
	Semicolon,
}

/// Identifier
#[derive(Debug)]
#[hashtype]
pub struct Ident<'e>(&'e str);

// Relation that definitionally equates two objects
// idt := obj
#[derive(Debug)]
#[hashtype]
pub struct DefRel<'e> {
	idt: &'e AST<'e>,
	obj: &'e AST<'e>,
}
// relation that associates a type with an object.
// idt : typ
#[derive(Debug)]
#[hashtype]
pub struct TypRel<'e> {
	idt: &'e AST<'e>,
	typ: &'e AST<'e>,
}

/// `let <name> := <val> : <type>`
/// `let <name> : <type> := <val>`
/// Requires <name> to be undefined in context, and for <term> and <type> to typecheck correctly
#[derive(Debug)]
#[hashtype]
pub struct LetDef<'e> {
	idt: &'e Ident<'e>,
	def: &'e DefRel<'e>,
	opt_typ: Option<&'e TypRel<'e>>,
}

/// `type <name> : <type>`
/// Names an undefined term of some type in some context.
#[derive(Debug)]
#[hashtype]
pub struct TypeDef<'e> {
	name: &'e Ident<'e>,
	rel: &'e TypRel<'e>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitSize {
	S8, S16, S32, S64, S128, S256,
}
impl BitSize {
	fn check_u64(val: u64) -> BitSize {
		if (val >> 32) != 0 { BitSize::S64 }
		else if (val >> 16) != 0 { BitSize::S32 }
		else if (val >> 8)  != 0 { BitSize::S16 }
		else { BitSize::S8 }
	}
	fn check_i64(val: i64) -> BitSize {
		if i8::try_from(val).is_ok() { BitSize::S8 }
		else if i16::try_from(val).is_ok() { BitSize::S16 }
		else if i32::try_from(val).is_ok() { BitSize::S32 }
		else { BitSize::S64 }
	}
}

/// Primitive Type
#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
	Bool,
	/// Nat8, Nat16, Nat32, Nat64, Nat
	Nat(BitSize),
	/// Int8, Int16, Int32, Int64, Int
	Int(BitSize),
	/// Float32, Float64, Float
	Float(BitSize),
	String,
	Char,
}
/// Primitive Term
#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveTerm<'e> {
	Bool(bool),
	Nat(u64, BitSize),
	Int(i64, BitSize),
	F32(f32),
	F64(f64),
	String(&'e str),
	Char(char),
}

/// Literals are technically identifiers, right?
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum IdentType {
	Literal(Literal),
	Symbol,
}

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
#[derive(Debug)]
#[hashtype]
pub enum AST<'e> {
	LetDef(LetDef<'e>),
	TypeDef(TypeDef<'e>),
	Ident(IdentType, &'e Ident<'e>),

	/// `{ <expr>, <expr> }` or `{ <expr>;\n <expr> }`
	/// Unordered set of expressions
	Set(&'e [&'e AST<'e>], Separator),
	/// `[ <expr>, <expr> ]` or `[ <expr>;\n <expr> ]`
	/// Ordered list of expressions
	List(&'e [&'e AST<'e>], Separator),
	/// `<val> -> <val>`
	/// A function abstraction
	Fn { input: &'e AST<'e>, ouput: &'e AST<'e> },
	/// `<func> {arg1, arg2}`
	/// <func> [arg1, arg2]
	/// <func> (arg1)
	/// <func> <args>
	Application { func: &'e AST<'e>, args: &'e AST<'e> },
	/// `(expr1, expr2, ...)`
	Grouping(&'e [&'e AST<'e>]),
}

pub struct CustomState<'s, 'e, E: TypeStore<'e> + 'e> {
	pub src: &'s str,
	/// Storage for the output of the parser
	pub links: &'e E,
}
type CustomExtra<'t, 's, 'e, E> = extra::Full<Rich<'t, Token<'s>, Span>, CustomState<'s, 'e, E>, ()>;

fn parser<'s: 't, 't, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, Spanned<&'e AST<'e>>, CustomExtra<'t, 's, 'e, E>> {
	recursive(|expr| {
		let literal = select! {
			Token::Literal(x, src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Literal(x), state.links.add(Ident(state.links.add_str(src))))
			},
		}.labelled("literal")
		.map_with(|expr, extra: &mut MapExtra<ParserInput, CustomExtra<E>>|extra.state().links.add(expr));

		let symbol = select! {
			Token::Ident(src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Symbol, state.links.add(Ident(state.links.add_str(src))))
			},
		}.labelled("symbol")
		.map_with(|expr, extra: &mut MapExtra<ParserInput, CustomExtra<E>>|extra.state().links.add(expr));

		/* let set = end();
		let list = end();
		let app = end();
		let abs = end(); */

		let object = literal.or(symbol);

		let def_relation = symbol.then_ignore(just(Token::AssignOp)).then(object.clone()).map_with(|(idt, obj), extra|extra.state().links.add(DefRel { idt, obj }));
		let typ_relation = symbol.then_ignore(just(Token::TypeOp)).then(object.clone()).map_with(|(idt, typ), extra|extra.state().links.add(TypRel { idt, typ }));

		let let_dec = just(Token::LetKW).ignore_then(def_relation).try_map_with(|rel, extra| match rel.idt {
			AST::Ident(IdentType::Symbol, idt) => Ok(extra.state().links.add(AST::LetDef(LetDef { idt, def: rel, opt_typ: None }))),
			_ => Err(Rich::custom(extra.span(), "failed to associate definition relation with let declaration. let declaration requires a identifier in the relation")),
		});
		let typ_dec = just(Token::TypeKW).ignore_then(typ_relation).try_map_with(|rel, extra| match rel.idt {
			AST::Ident(IdentType::Symbol, idt) => Ok(extra.state().links.add(AST::TypeDef(TypeDef { name: idt, rel }))),
			_ => Err(Rich::custom(extra.span(), "failed to associate type relation with type declaration. type declaration requires a identifier in the relation")),
		});

		let_dec.or(typ_dec).map_with(|x, extra|(x, extra.span())).or(expr)
	})
}
/// 
/// parser := { E: impl{TypeStore} } -> {
/// 	match {
/// 		Token.Literal
/// 	}
/// }
/// 
/// 
/// 
/// 

/// Generate cool errors with ariadne
pub fn gen_reports<'a, I: std::fmt::Display + 'a>(errors: impl IntoIterator<Item = Rich<'a, I>>) -> impl Iterator<Item = Report<'a>> {
	errors.into_iter().map(|e| {
        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(e.to_string())
            .with_label(
                Label::new(e.span().into_range())
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
    })
}
/// Use gen_reports to fancy-print errors
pub fn fancy_print_errors<'a, I: std::fmt::Display + 'a>(errors: Vec<Rich<'a, I>>, source: &str) {
	let source = ariadne::Source::from(source);
	let mut reports = gen_reports(errors.into_iter());
	reports.try_for_each(|rep|rep.eprint(source.clone())).unwrap();
}