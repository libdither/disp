use chumsky::{prelude::*, text::unicode, input::SpannedInput};

use super::Literal;

/// Types of brackets
#[derive(PartialEq, Debug, Clone)]
pub enum BracketType {
	Paren,
	Square,
	Curly,
	Caret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
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
	// Other Operation token
	Op,
	/// A constant literal, resolves to an expression.
	Literal(Literal),

	OpenBracket(BracketType),
	ClosedBracket(BracketType),

	/// Identifier
	Ident,

	/// Comment
	Comment,
}

struct LexerState {
	buf: Vec<(Token, SimpleSpan<usize>)>,
}
impl LexerState {
	fn new() -> Self { LexerState { buf: Default::default() } }
}

fn lexer<'i>() -> impl Parser<'i, &'i str, (), extra::State<LexerState>> {

	let num = text::int(10)
        .ignore_then(just('.').ignore_then(text::digits(10)).to(()).or_not())
        .map(|opt|Token::Literal(opt.map_or(Literal::Integer, |_|Literal::Integer)));
	
	let string = just('"')
		.then(none_of("\"").repeated())
		.then(just('"'))
		.to(Token::Literal(Literal::String));

	let op = one_of("+*-/!=:").repeated().at_least(1).map_slice(|op| match op{
		":=" => Token::AssignOp,
		":" => Token::TypeOp,
		_ => Token::Op,
	});

	let idents = unicode::ident().map(|ident: &str| match ident {
        "let" => Token::LetKW,
        "type" => Token::TypeKW,
        "true" => Token::Literal(Literal::Boolean(true)),
        "false" => Token::Literal(Literal::Boolean(false)),
        _ => Token::Ident,
    });

	let comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated().to(Token::Comment))
        .padded();

	let token = comment.or(num.or(string).or(op).or(idents));


	// A token can be a string, a unicode ident, or another fixed token.
	token
		.map_with_span(|tok, span|(tok, span))
		.padded()
		// If we encounter an error, skip and attempt to lex the next character as a token instead
		.recover_with(skip_then_retry_until(any().ignored(), end()))
		.map_with_state(|tok_span, _, state: &mut LexerState|state.buf.push(tok_span))

} 

type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);

type ParserInput<'tokens> = SpannedInput<Token, Span, &'tokens [(Token, Span)]>;

fn test_lexer(string: &str, expected: &[(Token, Option<&str>)]) {
	let mut state = LexerState::new();
	let res = lexer().parse_with_state("input", &mut state);
	for (i, tok) in expected.iter().enumerate() {
		assert_eq!(&state.buf[i], tok);
	}
	
}

#[test]
fn num_test() {
	test_lexer("hello world", $[(Token::Ident, None)])
}