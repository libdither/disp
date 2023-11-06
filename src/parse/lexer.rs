use std::fmt;

use chumsky::{prelude::*, text::unicode, input::SpannedInput, combinator::MapExtra};

use crate::parse::fancy_print_errors;

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
	/// A constant literal, resolves to an expression.
	Literal(Literal, &'s str),

	OpenBracket(BracketType),
	ClosedBracket(BracketType),

	Separator(Separator),

	/// Identifier
	Ident(&'s str),

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
				Separator::Semicolon => ";",
			}),
            Token::Ident(src) => write!(f, "{src}"),
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

	// single-character standalone tokens
	let single = select! {
        '{' => Token::OpenBracket(BracketType::Curly),
		'[' => Token::OpenBracket(BracketType::Square),
		'(' => Token::OpenBracket(BracketType::Paren),
		'<' => Token::OpenBracket(BracketType::Caret),
		'}' => Token::ClosedBracket(BracketType::Curly),
		']' => Token::ClosedBracket(BracketType::Square),
		')' => Token::ClosedBracket(BracketType::Paren),
		'>' => Token::ClosedBracket(BracketType::Caret),
		';' => Token::Separator(Separator::Semicolon),
		',' => Token::Separator(Separator::Comma),
    };

	let idents = unicode::ident().map(|ident: &str| match ident {
        "let" => Token::LetKW,
        "type" => Token::TypeKW,
        _ => Token::Ident(ident),
    });

	let comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated().to_slice().map(|src|Token::Comment(src)))
        .padded();

	let token = comment.or(num.or(string.or(char)).or((single.or(op)).or(idents)));

	// A token can be a string, a unicode ident, or another fixed token.
	token
		.map_with(|tok_span, extra: &mut MapExtra<&'i str, LexerExtra<'i>>|{
			let span = extra.span();
			extra.state().buf.push((tok_span, span))
		})
		.padded()
		// If we encounter an error, skip and attempt to lex the next character as a token instead
		.recover_with(skip_then_retry_until(any().ignored(), end()))
		.repeated().collect::<()>()

} 

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

pub type ParserInput<'src, 'tokens> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

fn test_lexer(string: &str, expected: &[Token]) {
	let mut state = LexerState::new();

	let res = lexer().parse_with_state(string, &mut state);
	if res.has_errors() {
		fancy_print_errors(res.into_errors(), string);
	}
	
	let found = state.buf.into_iter().map(|(tok, _span)| tok).collect::<Vec<Token>>();

	assert_eq!(expected, found);
}

#[test]
fn num_test() {
	test_lexer("hello world", &[Token::Ident("hello"), Token::Ident("world")]);

	test_lexer("hello let yeet", &[Token::Ident("hello"), Token::LetKW, Token::Ident("yeet")]);

	use Token::{Ident, LetKW, AssignOp, ClosedBracket, OpenBracket, Separator, TypeKW, TypeOp, Op, Literal};
	use BracketType::*;
	use self::Separator::*;
	use self::Literal::*;
	test_lexer(r#"
		type thing : Nat;

		let thing := 3;

		let programming_languages_are_cool := true;
	"#, &[
		TypeKW, Ident("thing"), TypeOp, Ident("Nat"), Separator(Semicolon), LetKW, Ident("thing"), AssignOp, Literal(Integer, "3"), Separator(Semicolon), LetKW, Ident("programming_languages_are_cool"), AssignOp, Ident("true"), Separator(Semicolon)
	]);
	test_lexer(r#"
		let literals := [3, 3.3, 'λ', "hi there"];
	"#, &[
		LetKW, Ident("literals"), AssignOp, OpenBracket(Square), Literal(Integer, "3"), Separator(Comma), Literal(Decimal, "3.3"), Separator(Comma), Literal(Char, "'λ'"), Separator(Comma), Literal(String, "\"hi there\""), ClosedBracket(Square), Separator(Semicolon)
	]);
	test_lexer(r#"
		let List { T : Type } := data {
			type nil : List(T),
			type cons : { head : T, tail : List(T) } -> List(T),
		}
	"#, &[
		LetKW, Ident("List"), OpenBracket(Curly), Ident("T"), TypeOp, Ident("Type"), ClosedBracket(Curly), AssignOp, Ident("data"), OpenBracket(Curly), TypeKW, Ident("nil"), TypeOp, Ident("List"), OpenBracket(Paren), Ident("T"), ClosedBracket(Paren), Separator(Comma), TypeKW, Ident("cons"), TypeOp, OpenBracket(Curly), Ident("head"), TypeOp, Ident("T"), Separator(Comma), Ident("tail"), TypeOp, Ident("List"), OpenBracket(Paren), Ident("T"), ClosedBracket(Paren), ClosedBracket(Curly), Op("-"), ClosedBracket(Caret), Ident("List"), OpenBracket(Paren), Ident("T"), ClosedBracket(Paren), Separator(Comma), ClosedBracket(Curly)
	]);
}
