use core::fmt;
use std::str::FromStr;

use winnow::{ascii::{alphanumeric1, digit1, multispace0, multispace1, newline}, combinator::{alt, dispatch, fail, peek, preceded, repeat, separated_pair, terminated}, error::{AddContext, ParserError, StrContext}, prelude::*, stream::{AsBStr, AsChar}, token::{any, none_of, one_of, take, take_while}};

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
	Number(u64),
	Ident(String),
	String(String),
	AssignOp,
	TypeOp,
    FuncOp,
    Minus,
	OpenCurlyBracket,
	ClosedCurlyBracket,
	OpenParen,
	ClosedParen,
	OpenSquareBracket,
	ClosedSquareBracket,
    Separator(Separator),
}
#[derive(Clone, Debug, PartialEq)]
pub enum Separator {
    Comma, Newline
}
impl fmt::Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::Comma => write!(f, ","),
            Separator::Newline => write!(f, "\n"),
        }
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(num) => write!(f, "{num}"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::String(string) => write!(f, "\"{string}\""),
            Token::AssignOp => write!(f, ":="),
            Token::TypeOp => write!(f, ":"),
            Token::FuncOp => write!(f, "->"),
            Token::Minus => write!(f, "-"),
            Token::OpenCurlyBracket => write!(f, "{{"),
            Token::ClosedCurlyBracket => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::ClosedParen => write!(f, ")"),
            Token::OpenSquareBracket => write!(f, "["),
            Token::ClosedSquareBracket => write!(f, "]"),
            Token::Separator(sep) => write!(f, "{sep}"),
        }
    }
}

/* impl AsBStr for &'_[Token] {
    fn as_bstr(&self) -> &[u8] {
        todo!()
    }
} */

fn unicode_escape<'i, E: ParserError<&'i str>>(input: &mut &'i str) -> PResult<char, E> {
    fn u16_hex<'i, E: ParserError<&'i str>>(input: &mut &'i str) -> PResult<u16, E> {
		take(4usize)
			.verify_map(|s| u16::from_str_radix(s, 16).ok())
			.parse_next(input)
	}
	alt((
        // Not a surrogate
        u16_hex
            .verify(|cp| !(0xD800..0xE000).contains(cp))
            .map(|cp| cp as u32),
        // See https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF for details
        separated_pair(u16_hex, "\\u", u16_hex)
            .verify(|(high, low)| (0xD800..0xDC00).contains(high) && (0xDC00..0xE000).contains(low))
            .map(|(high, low)| {
                let high_ten = (high as u32) - 0xD800;
                let low_ten = (low as u32) - 0xDC00;
                (high_ten << 10) + low_ten + 0x10000
            }),
    ))
    .verify_map(
        // Could be probably replaced with .unwrap() or _unchecked due to the verify checks
        std::char::from_u32,
    )
    .parse_next(input)
}

fn character<'i, E: ParserError<&'i str>>(input: &mut &'i str) -> PResult<char, E> {
    let c = none_of('\"').parse_next(input)?;
    if c == '\\' {
        alt((
            any.verify_map(|c| {
                Some(match c {
                    '"' | '\\' | '/' => c,
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => return None,
                })
            }),
            preceded('u', unicode_escape),
        ))
        .parse_next(input)
    } else {
        Ok(c)
    }
}

/// This parser gathers all `char`s up into a `String`with a parse to take the double quote
/// character, before the string (using `preceded`) and after the string (using `terminated`).
fn string<'i, E: ParserError<&'i str> + AddContext<&'i str, StrContext>>(
    input: &mut &'i str,
) -> PResult<String, E> {
    preceded(
        '\"', terminated(
            repeat(0.., character).fold(String::new, |mut string, c| {
                string.push(c);
                string
            }),
            '\"',
        ),
    )
    // `context` lets you add a static string to errors to provide more information in the
    // error chain (to indicate which parser had an error)
    .context(StrContext::Label("string"))
    .parse_next(input)
}

fn token(i: &mut &str) -> PResult<Token> {
    dispatch! {peek(any);
        '0'..='9' => digit1.try_map(FromStr::from_str).map(Token::Number),
		'a'..='z' | 'A'..='Z' => (
            one_of(AsChar::is_alphanum),
            take_while(0.., |c|AsChar::is_alphanum(c) || c == '-' || c == '_'))
                .take().map(|s: &str|Token::Ident(s.to_owned())
        ),
		'"' => string.map(Token::String),
        '(' => '('.value(Token::OpenParen),
        ')' => ')'.value(Token::ClosedParen),
        '{' => '{'.value(Token::OpenCurlyBracket),
        '}' => '}'.value(Token::ClosedCurlyBracket),
        '[' => '['.value(Token::OpenSquareBracket),
        ']' => ']'.value(Token::ClosedSquareBracket),
        ':' => alt((":=".value(Token::AssignOp), ":".value(Token::TypeOp))),
        '-' => alt(("->".value(Token::FuncOp), "-".value(Token::Minus))),
        ',' => ','.value(Token::Separator(Separator::Comma)),
        _ => alt((newline.value(Token::Separator(Separator::Newline)), fail)),
    }
    .parse_next(i)
}

#[test]
fn test_lexer_number() {
	assert_eq!(token.parse_peek("21c"), Ok(("c", Token::Number(21))));
}
#[test]
fn test_lexer_ident() {
	assert_eq!(token.parse_peek("hi4+"), Ok(("+", Token::Ident("hi4".to_owned()))));
}
#[test]
fn test_lexer_parens() {
	assert_eq!(token.parse_peek("()a"), Ok((")a", Token::OpenParen)));
	assert_eq!(token.parse_peek(")(a"), Ok(("(a", Token::ClosedParen)));
}
#[test]
fn test_lexer_curly() {
	assert_eq!(token.parse_peek("{}a"), Ok(("}a", Token::OpenCurlyBracket)));
	assert_eq!(token.parse_peek("}{a"), Ok(("{a", Token::ClosedCurlyBracket)));
}
#[test]
fn test_lexer_square() {
	assert_eq!(token.parse_peek("[]a"), Ok(("]a", Token::OpenSquareBracket)));
	assert_eq!(token.parse_peek("][a"), Ok(("[a", Token::ClosedSquareBracket)));
}
#[test]
fn test_lexer_string() {
	assert_eq!(token.parse_peek(r#""yeetaloni"3"#), Ok(("3", Token::String("yeetaloni".to_owned()))));
}
#[test]
fn test_lexer_ops() {
	assert_eq!(token.parse_peek(":=:a"), Ok((":a", Token::AssignOp)));
	assert_eq!(token.parse_peek("::=a"), Ok((":=a", Token::TypeOp)));
	assert_eq!(token.parse_peek("->a"), Ok(("a", Token::FuncOp)));
	assert_eq!(token.parse_peek("-a"), Ok(("a", Token::Minus)));
}

pub fn lexer(i: &mut &str) -> PResult<Vec<Token>> {
    preceded(multispace0, repeat(1.., terminated(token, multispace0))).parse_next(i)
}

#[test]
fn test_lexer_tokenstream() {
    use Token::*;
    assert_eq!(lexer(&mut "(abcabc)"), Ok(vec![OpenParen, Ident("abcabc".to_owned()), ClosedParen]));

    assert_eq!(lexer(&mut r#"a := "thing""#), Ok(vec![Token::Ident("a".to_owned()), AssignOp, String("thing".to_owned())]));
}

impl winnow::stream::ContainsToken<Token> for Token {
    #[inline(always)]
    fn contains_token(&self, token: Token) -> bool {
        *self == token
    }
}

impl winnow::stream::ContainsToken<Token> for &'_ [Token] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for &'_ [Token; LEN] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for [Token; LEN] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

/* impl fmt::Display for &'_ [Token] {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(self.iter().try_for_each(|t|t.fmt(f))?)
    }
}

impl<const LEN: usize> fmt::Display for &'_ [Token; LEN] {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for [Token; LEN] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
} */