#![allow(dead_code)]

use ariadne::{Color, Label, Report, Fmt, ReportKind, Source};
use chumsky::{prelude::*, text::{unicode, ascii::ident}, extra::{Full, ParserExtra}, recursive::Direct, input::SpannedInput};
use hashdb::{HashType, LinkArena, RevHashType, RevLinkArena, RevLinkStore, TypeStore, ArchiveFetchable, Datastore, ArchiveToType, ArchiveStorable};

use crate::{expr::{BindSubTree, Expr}, name::{Name, SemanticTree, NamedExpr}};

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
	String,
	Integer,
	Decimal,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Bracket {
	Paren,
	Square,
	Curly,
	Caret,
}

/* #[derive(PartialEq, Debug, Clone)]
pub enum Token<'s> {
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
 */

/* type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;
 */

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
	fn test_num() {
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

/* /// Parse and reduce a string
pub fn parse_reduce<'e>(string: &str, links: &'e RevLinkArena<'e>) -> Result<&'e Expr<'e>, anyhow::Error> {
	let nte = parse(string, links)?;
	Ok(nte.expr.reduce(links)?)
} */

/* /// Commands for cli
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
	let expr = parser();

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
} */

/* #[test]
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
} */