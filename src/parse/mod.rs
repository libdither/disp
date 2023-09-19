#![allow(dead_code)]

pub mod lexer;

use ariadne::{Color, Label, Report, Fmt, ReportKind, Source};
use chumsky::{prelude::*, text::{unicode, ascii::ident}, extra::{Full, ParserExtra}, recursive::Direct, input::SpannedInput};
use hashdb::{HashType, LinkArena, RevHashType, RevLinkArena, RevLinkStore, TypeStore, ArchiveFetchable, Datastore, ArchiveToType, ArchiveStorable};

use crate::{expr::{BindSubTree, Expr}, name::{Name, SemanticTree, NamedExpr}};

/// A Literal expression
#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
	String, // String literal
	Integer, // Integer literal
	Decimal, // Decimal literal
	Boolean(bool), // boolean
}

/// Identifier
pub struct Ident<'e>(&'e str);

/// `let <name> := <val> : <type>`
/// `let <name> : <type> := <val>`
/// Requires <name> to be undefined in context, and for <term> and <type> to typecheck correctly
pub struct LetDef<'e> {
	name: Ident<'e>,
	val: &'e AST<'e>,
	optional_typ: Option<&'e AST<'e>>,
}

/// `type <name> : <type>`
/// Names an undefined term of some type in some context.
pub struct TypeDef<'e> {
	name: Ident<'e>,
	typ: &'e AST<'e>,
}

/// `func(arg) := thing`
/// Anonymous definition, usually used for providing typeclass implementations (i.e. partial function definitions)
pub struct AnonDef<'e> {
	def: &'e AST<'e>,
	val: &'e AST<'e>,
}

/// Separators for sets or lists may be Comma
pub enum Separator {
	Comma,
	Newline,
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

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
pub enum AST<'e> {
	LetDef(LetDef<'e>),
	TypeDef(TypeDef<'e>),
	AnonDef(AnonDef<'e>),
	Ident(Ident<'e>),
	PrimitiveTerm(PrimitiveTerm<'e>),
	PrimitiveType(PrimitiveType),

	/// `{ <expr>, <expr> }` or `{ <expr> \n <expr> }`
	/// Unordered set of expressions
	Set(&'e [&'e AST<'e>], Separator),
	/// `[ <expr>, <expr> ]` or `[ <expr> \n <expr> ]`
	/// Ordered list of expressions
	List(&'e [&'e AST<'e>], Separator),
	/// `<val> -> <val>`
	/// A function abstraction
	Fn { input: &'e AST<'e>, ouput: &'e AST<'e>, implicit: bool },
	/// `<func> {arg1, arg2}`
	/// <func> [arg1, arg2]
	/// <func> (arg1)
	/// <func> <args>
	Application { func: &'e AST<'e>, args: &'e AST<'e> },
	/// `(expr1, expr2, ...)`
	Grouping(&'e [&'e AST<'e>]),
}

pub struct CustomState<'e, E: TypeStore<'e> + 'e> {
	/// Storage for the output of the parser
	pub links: &'e RevLinkStore<'e, E>,
}
type CustomExtra<'s, 'e, E> = extra::Full<Rich<'s, char>, CustomState<'e, E>, ()>;

fn primitive_type<'s, E: ParserExtra<'s, &'s str, Error = EmptyErr>>() -> impl Parser<'s, &'s str, PrimitiveType, E> {
	ident().try_map(|ident: &str, _| match ident {
		"Bool" 	=> Ok(PrimitiveType::Bool),
		"Nat8" 	=> Ok(PrimitiveType::Nat(BitSize::S8)),
		"Nat16" => Ok(PrimitiveType::Nat(BitSize::S16)),
		"Nat32" => Ok(PrimitiveType::Nat(BitSize::S32)),
		"Nat64" => Ok(PrimitiveType::Nat(BitSize::S64)),
		"Int8" 	=> Ok(PrimitiveType::Int(BitSize::S8)),
		"Int16" => Ok(PrimitiveType::Int(BitSize::S16)),
		"Int32" => Ok(PrimitiveType::Int(BitSize::S32)),
		"Int64" => Ok(PrimitiveType::Int(BitSize::S64)),
		"Float32" => Ok(PrimitiveType::Float(BitSize::S32)),
		"Float64" => Ok(PrimitiveType::Float(BitSize::S64)),
		"String" => Ok(PrimitiveType::String),
		"Char" => Ok(PrimitiveType::Char),
		_ => Err(EmptyErr::default()),
	})
}
fn string<'s, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'s, &'s str, PrimitiveTerm<'e>, CustomExtra<'s, 'e, E>> {
	just('"')
		.ignore_then(none_of('"').repeated())
		.then_ignore(just('"')).slice()
		.map_with_state(|s: &'s str, _span, state: &mut CustomState<'e, E>|{
			PrimitiveTerm::String(state.links.add_str(s))
		})
}
fn num<'s, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'s, &'s str, PrimitiveTerm<'e>, CustomExtra<'s, 'e, E>> {
	just('-').repeated()
	.then(text::int(10))
	.then(just('.').then(text::digits(10)).or_not())
	.slice().try_map(|string: &str, span| {
		if string.contains(".") {
			match string.parse::<f32>() {
				Ok(val) => Ok(PrimitiveTerm::F32(val)),
				Err(_) => match string.parse::<f64>() {
					Ok(val) => Ok(PrimitiveTerm::F64(val)),
					Err(_) => Err(Rich::custom(span, "failed to parse Float"))
				}
			}
		} else if string.contains("-") {
			match string.parse::<i64>() {
				Ok(val) => Ok(PrimitiveTerm::Int(val, BitSize::check_i64(val))),
				Err(_) => Err(Rich::custom(span, "failed to parse Int"))
			}
		} else {
			match string.parse::<u64>() {
				Ok(val) => Ok(PrimitiveTerm::Nat(val, BitSize::check_u64(val))),
				Err(_) => Err(Rich::custom(span, "failed to parse Nat"))
			}
		}
	})
}
#[cfg(test)]
mod tests {
    use hashdb::{RevLinkStore, LinkArena};
	use super::*;

	#[test]
	fn test_prim_types() {
		let parse = primitive_type::<extra::Default>().padded().repeated().collect::<Vec<_>>().parse("String Num32 Int32").into_result().unwrap();
		use PrimitiveType as PT;
		assert_eq!(parse, vec![PT::String, PT::Nat(BitSize::S32), PT::Int(BitSize::S32)]);
	}
}

fn char<'s, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'s, &'s str, PrimitiveTerm<'e>, CustomExtra<'s, 'e, E>> {
	just('\'').ignore_then(none_of('\'')).then_ignore(just('\'')).slice()
	.try_map(|ch: &str, span| match ch.parse::<char>() {
		Ok(ch) => Ok(PrimitiveTerm::Char(ch)), Err(e) => Err(Rich::custom(span, e))
	})
}
fn primitive_term<'s, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'s, &'s str, PrimitiveTerm<'e>, CustomExtra<'s, 'e, E>> {
	choice((
		just("true").to(PrimitiveTerm::Bool(true)),
		just("false").to(PrimitiveTerm::Bool(false)),
		string(),
		char(),
		num(),
	))
}

fn parser<'s, 'e: 's, E: TypeStore<'e> + 'e>() -> impl Parser<'s, &'s str, AST<'e>, CustomExtra<'s, 'e, E>> {

	/* // Parses expression recursively
	recursive(|expr| {
		
	}).then_ignore(end()) */
	primitive_term().map(|term|AST::PrimitiveTerm(term))
}

// Parse expression and register name tree
/* pub fn parse<'e>(string: &str, links: &'e RevLinkArena<'e>) -> Result<&'e SemanticTree<'e>, anyhow::Error> {
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
} */

/// Generate cool errors with ariadne
pub fn gen_report<'a>(errors: impl IntoIterator<Item = Rich<'a, &'a str>>) -> impl Iterator<Item = Report<'a>> {
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
