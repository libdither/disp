use std::{fmt, marker::PhantomData};

use chumsky::text::{inline_whitespace, newline, whitespace};
use chumsky::{input::MapExtra, prelude::*, text::unicode, input::SpannedInput};
use hashdb::{hashtype, HashType};
use rkyv::with::{ArchiveWith, DeserializeWith, SerializeWith};
use rkyv::{Archive, Deserialize, Fallible, Serialize};

use super::{Literal, Separator};

/// Types of brackets
#[derive(PartialEq, Debug, Clone)]
pub enum BracketType {
	Paren,
	Square,
	Curly,
	Caret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token<'s> {
	/// Associates a name with being the member of some type (used to defer definition)
	/// `type`, as in `type thing : Type`
    TypeKW,
	/// Associates a name with an expression.
	/// `let` as in `let thing := 3`
	LetKW,
	/// Assign an expression to a name. (In TypeDef it is treated as a "default" definition)
	/// `:=` as in `let thing := 3`
	AssignOp,
	/// Describes the type of an expression or name.
	/// `:` as in `type thing : Type`
	TypeOp,
	// Describes the creation of an abstraction
	/// `->` as in `{a: Nat, b: Nat} -> Nat`
	AbsOp,
	// Other Operation token
	Op(&'s str),
	/// A constant literal identifier, resolves to an expression.
	Literal(Literal, &'s str),

	OpenBracket(BracketType),
	ClosedBracket(BracketType),

	Separator(Separator),

	/// Symbolic identifier
	Symbol(&'s str),

	/// Comment
	Comment(&'s str),
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::TypeKW => write!(f, "type"),
            Token::LetKW => write!(f, "let"),
            Token::AssignOp => write!(f, ":="),
			Token::AbsOp => write!(f, "->"),
            Token::TypeOp => write!(f, ":"),
            Token::Op(op) => write!(f, "{op}"),
            Token::Literal(literal, src) => match literal{
                Literal::String => write!(f, "\"{src}\""),
                Literal::Char => write!(f, "\'{src}\'"),
                _ => write!(f, "{src}"),
            },
            Token::OpenBracket(typ) => write!(f, "{}", match typ {
                BracketType::Paren => "(",
                BracketType::Square => "[",
                BracketType::Curly => "{",
                BracketType::Caret => "<",
            }),
            Token::ClosedBracket(typ) => write!(f, "{}", match typ {
                BracketType::Paren => ")",
                BracketType::Square => "]",
                BracketType::Curly => "}",
                BracketType::Caret => ">",
            }),
            Token::Separator(typ) => write!(f, "{}", match typ {
				Separator::Comma => ",",
				Separator::CommaNewline => ",\n",
				Separator::Newline => "\n",
			}),
            Token::Symbol(src) => write!(f, "{src}"),
            Token::Comment(src) => write!(f, "//{src}"),
        }
    }
}

pub struct LexerState<'s> {
	buf: Vec<(Token<'s>, SimpleSpan<usize>)>,
}
impl<'s> LexerState<'s> {
	pub fn new() -> Self { LexerState { buf: Default::default() } }
	pub fn slice(&self) -> &[(Token<'s>, SimpleSpan<usize>)] { &self.buf }
	pub fn reset(&mut self) { self.buf.clear(); }
}
type LexerExtra<'i> = extra::Full<Rich<'i, char>, LexerState<'i>, ()>;

pub fn line_end<'i>() -> impl Parser<'i, &'i str, Token<'i>, LexerExtra<'i>> {
	just(",").or_not()
		.then(inline_whitespace().ignore_then(newline()))
		.map(|(v, _)|if v.is_some() { Token::Separator(Separator::CommaNewline) } else { Token::Separator(Separator::Newline) }).then_ignore(whitespace())
}
#[test]
fn lexer_newline() {
	assert_eq!(line_end().parse_with_state("\n", &mut LexerState::new()).unwrap(), Token::Separator(Separator::Newline));
}
#[test]
fn lexer_comma_newline() {
	assert_eq!(line_end().parse_with_state(",\n", &mut LexerState::new()).unwrap(), Token::Separator(Separator::CommaNewline));
	assert_eq!(line_end().parse_with_state(", 	\n", &mut LexerState::new()).unwrap(), Token::Separator(Separator::CommaNewline));
}

pub fn lexer<'i>() -> impl Parser<'i, &'i str, (), LexerExtra<'i>> {
	let num = text::int(10)
        .ignore_then(just('.').ignore_then(text::digits(10)).to(()).or_not())
        .map_with(|out, extra|Token::Literal(out.map_or(Literal::Integer, |_|Literal::Decimal), extra.slice()));
	
	let string = none_of("\"").repeated().delimited_by(just('"'), just('"'))
		.to_slice().map(|src|Token::Literal(Literal::String, src));
	// need more complicate char parsing (to deal with unicode stuff, \n, etc.)
	let char = just('\'').ignore_then(none_of('\'')).then_ignore(just('\'')).to_slice().map(|src|Token::Literal(Literal::Char, src));

	let op = one_of("+*-/!=:").repeated().at_least(1).to_slice().map(|op| match op{
		":=" => Token::AssignOp,
		":" => Token::TypeOp,
		"->" => Token::AbsOp,
		_ => Token::Op(op),
	});

	// open-tokens ignore whitespace after them
	let open = select! {
        '{' => Token::OpenBracket(BracketType::Curly),
		'[' => Token::OpenBracket(BracketType::Square),
		'(' => Token::OpenBracket(BracketType::Paren),
		'<' => Token::OpenBracket(BracketType::Caret),
    }.then_ignore(whitespace::<_, &'i str, LexerExtra<'i>>());
	// line end tokens
	let line_end = line_end();
	// closed tokens
	let closed = select! {
		'}' => Token::ClosedBracket(BracketType::Curly),
		']' => Token::ClosedBracket(BracketType::Square),
		')' => Token::ClosedBracket(BracketType::Paren),
		'>' => Token::ClosedBracket(BracketType::Caret),
		',' => Token::Separator(Separator::Comma),
	};
	let singles = open.or(line_end.or(closed));

	let idents = unicode::ident().map(|ident: &str| match ident {
        "let" => Token::LetKW,
        "type" => Token::TypeKW,
        _ => Token::Symbol(ident),
    });

	let comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated().to_slice().map(|src|Token::Comment(src)))
        .padded();

	let token = comment.or(num.or(string.or(char)).or((singles.or(op)).or(idents)));

	// A token can be a string, a unicode ident, or another fixed token.
	whitespace().ignore_then(token
		.map_with(|tok_span, extra: &mut MapExtra<&'i str, LexerExtra<'i>>|{
			let span = extra.span();
			extra.state().buf.push((tok_span, span))
		})
		// If we encounter an error, skip and attempt to lex the next character as a token instead
		.recover_with(skip_then_retry_until(any().ignored(), end()))
		.repeated().collect::<()>())

} 

/* trait Isomorphism {
	type Unarchivable;
	type Archivable: Archive;
    const TO_ARCHIVE: fn(&Self::Unarchivable) -> Self::Archivable;
    const FROM_ARCHIVE: fn(Self::Archivable) -> Self::Unarchivable;
}
impl<I: Isomorphism> ArchiveWith<I::Unarchivable> for I {
    type Archived = I::Archivable;

    type Resolver = <I::Archivable as Archive>::Resolver;

    unsafe fn resolve_with(
        field: &I::Unarchivable,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        let archivable = I::TO_ARCHIVE(field);
		archivable.resolve(pos, (), out);
    }
}

pub struct SimpleSpanAsRange<T>;
impl<T: Archive> Isomorphism for SimpleSpanAsRange<T> {
	type Unarchivable = SimpleSpan<T>;
	type Archivable = Range<T>;
    const TO_ARCHIVE: fn(&SimpleSpan<T>) -> Range<T> = Into::into;
    const FROM_ARCHIVE: fn(Range<T>) -> SimpleSpan<T> = From::from;
} */

pub type Span = SimpleSpan<usize>;

/* use rkyv_with::{ArchiveWith, DeserializeWith};
use rkyv::with::{ArchiveWith, DeserializeWith, With};
use rkyv::{Archive, Deserialize, Infallible, Serialize};
#[derive(Archive, ArchiveWith, Deserialize, DeserializeWith)]
#[archive_with(from(SimpleSpan))]
pub struct ArchivableSimpleSpan<T> {
	start: T,
	end: T,
	context: (),
} */
#[derive(Archive, Serialize, Deserialize)]
struct LocalSpan {
	start: u64,
	end: u64,
}
struct SpanArchiver;
impl ArchiveWith<Span> for SpanArchiver {
    type Archived = <LocalSpan as Archive>::Archived;

    type Resolver = <LocalSpan as Archive>::Resolver;

    unsafe fn resolve_with(
        field: &Span,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {

        LocalSpan { start: field.start as u64, end: field.end as u64 }.resolve(pos, resolver, out);
    }
}
impl<S: Fallible + ?Sized> SerializeWith<Span, S> for SpanArchiver {
    fn serialize_with(field: &SimpleSpan, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        Ok(LocalSpan { start: field.start as u64, end: field.end as u64 }.serialize(serializer)?)
    }
}
impl<D: Fallible + ?Sized> DeserializeWith<ArchivedLocalSpan, Span, D> for SpanArchiver {
    fn deserialize_with(field: &ArchivedLocalSpan, _deserializer: &mut D) -> Result<Span, <D as Fallible>::Error> {
        Ok(Span::new(field.start as usize, field.end as usize))
    }
}

#[hashtype]
#[derive(Debug, Clone)]
pub struct Spanned<'a, T: HashType<'a> + Clone> {
	pub item: T,
	#[with(SpanArchiver)]
	pub span: Span,
	pub _phantom: PhantomData<&'a ()>,
}
impl<'a, T: HashType<'a> + Clone> Spanned<'a, T> {
	pub fn new(item: T, span: Span) -> Self {
		Spanned { item, span, _phantom: Default::default() }
	}
	pub fn tup(&self) -> (&T, &Span) { (&self.item, &self.span) }
}
pub type ParserInput<'src, 'tokens> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

fn test_lexer(string: &str, expected: &[Token]) {
	let mut state = LexerState::new();

	let _res = lexer().parse_with_state(string, &mut state);
	/* if res.has_errors() {
		fancy_print_errors(res.into_errors(), string);
	} */
	
	let found = state.buf.into_iter().map(|(tok, _span)| tok).collect::<Vec<Token>>();

	assert_eq!(expected, found);
}

#[test]
fn lexer_tests() {
	test_lexer("hello world", &[Token::Symbol("hello"), Token::Symbol("world")]);

	test_lexer("hello let yeet", &[Token::Symbol("hello"), Token::LetKW, Token::Symbol("yeet")]);

	use Token::{Symbol, LetKW, AssignOp, ClosedBracket, OpenBracket, Separator, TypeKW, TypeOp, Op, Literal};
	use BracketType::*;
	use self::Separator::*;
	use self::Literal::*;
	let string = 
	r#"type thing : Nat

let thing := 3

let programming_languages_are_cool := true
	"#;
	assert!(string.contains("\n"));
	test_lexer(string, &[
		TypeKW, Symbol("thing"), TypeOp, Symbol("Nat"), Separator(Newline), LetKW, Symbol("thing"), AssignOp, Literal(Integer, "3"), Separator(Newline), LetKW, Symbol("programming_languages_are_cool"), AssignOp, Symbol("true"), Separator(Newline)
	]);
	test_lexer(r#"
		let literals := [3, 3.3, 'λ', "hi there"]
	"#, &[
		LetKW, Symbol("literals"), AssignOp, OpenBracket(Square), Literal(Integer, "3"), Separator(Comma), Literal(Decimal, "3.3"), Separator(Comma), Literal(Char, "'λ'"), Separator(Comma), Literal(String, "\"hi there\""), ClosedBracket(Square), Separator(Newline)
	]);
	test_lexer(r#"
		List { T : Type } := data {
			nil : List(T)
			cons : { head : T, tail : List(T) } -> List(T),
		}
	"#, &[
		Symbol("List"), OpenBracket(Curly), Symbol("T"), TypeOp, Symbol("Type"), ClosedBracket(Curly), AssignOp, Symbol("data"), OpenBracket(Curly), Symbol("nil"), TypeOp, Symbol("List"), OpenBracket(Paren), Symbol("T"), ClosedBracket(Paren), Separator(Newline), Symbol("cons"), TypeOp, OpenBracket(Curly), Symbol("head"), TypeOp, Symbol("T"), Separator(Comma), Symbol("tail"), TypeOp, Symbol("List"), OpenBracket(Paren), Symbol("T"), ClosedBracket(Paren), ClosedBracket(Curly), Op("-"), ClosedBracket(Caret), Symbol("List"), OpenBracket(Paren), Symbol("T"), ClosedBracket(Paren), Separator(CommaNewline), ClosedBracket(Curly), Separator(Newline),
	]);
}
