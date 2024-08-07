use core::fmt;

use winnow::{combinator::{self, alt, delimited, fail, opt, preceded, repeat, separated, separated_pair}, error::{AddContext, ContextError, ErrMode, ErrorKind, ParserError, StrContext, StrContextValue}, prelude::*, stream::{Checkpoint, Stream}, token::{any, one_of}};

use crate::lexer::{lexer, Token};

#[derive(Clone, PartialEq, Debug)]
pub enum ParseTree {
	Number(u64),
    String(String),
	Ident(String),
    /// ident := def : Type
    IdentAssign { ident: Box<ParseTree>, def: Box<ParseTree>, typ: Option<Box<ParseTree>> },
    /// ident : Type
    IdentTyping { ident: Box<ParseTree>, typ: Box<ParseTree> },
    /// list or named set (or combination) in term form
    /// { thing1 := "abc" : String, thing2 := 123, 456, "seven" }
    SetTerm(Vec<ParseTree>),
    /// list or named set of types
    /// { thing1 : String, thing2 : Number, OtherType }
    SetType(Vec<ParseTree>),
}

impl fmt::Display for ParseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseTree::Number(num) => write!(f, "{}", num),
            ParseTree::String(string) => write!(f, "\"{}\"", string),
            ParseTree::Ident(ident) => write!(f, "{}", ident),
            ParseTree::IdentAssign { ident, def, typ } => {
                write!(f, "{ident} := {def}")?;
                if let Some(typ) = typ { write!(f, " : {typ}")?; } Ok(())
            },
            ParseTree::IdentTyping { ident, typ } => write!(f, "{ident} : {typ}"),
            ParseTree::SetTerm(_) => todo!(),
            ParseTree::SetType(_) => todo!(),
        }
    }
}

// literals
fn literal(input: &mut &[Token]) -> PResult<ParseTree> {
    any.verify_map(|t|match t {
        Token::Number(num) => Some(ParseTree::Number(num)),
        Token::String(string) => Some(ParseTree::String(string)),
        _ => None,
    })
        .context(StrContext::Label("literal"))
        .parse_next(input)
}

// specifically ident
fn ident(input: &mut &[Token]) -> PResult<ParseTree> {
    any.verify_map(|t|match t {
        Token::Ident(ident) => Some(ParseTree::Ident(ident)),
        _ => None,
    })
        .context(StrContext::Label("ident"))
        .parse_next(input)
}

#[test]
fn test_parse_string() {
	assert_eq!(literal.parse_peek(&mut &lexer(&mut "\"string\"[").unwrap()), Ok((&[Token::OpenSquareBracket][..], ParseTree::String("string".to_owned()))));
}
#[test]
fn test_parse_num() {
	assert_eq!(literal.parse_peek(&mut &lexer(&mut "927834[").unwrap()), Ok((&[Token::OpenSquareBracket][..], ParseTree::Number(927834))));
}

#[test]
fn test_parse_ident() {
	assert_eq!(ident.parse_peek(&mut &lexer(&mut "ident46[").unwrap()), Ok((&[Token::OpenSquareBracket][..], ParseTree::Ident("ident46".to_owned()))));
}

fn expr(input: &mut &[Token]) -> PResult<ParseTree> {
    combinator::trace("expr", alt((ident, literal, expr_set, typ_set)))
        .context(StrContext::Label("expression"))
        .parse_next(input)
}

fn def(input: &mut &[Token]) -> PResult<ParseTree> {
    combinator::trace(
        "def",
        combinator::seq! {ParseTree::IdentAssign {
            ident: ident.map(Box::new),
            _ : one_of(Token::AssignOp)
                .context(StrContext::Expected(StrContextValue::StringLiteral(":="))),
            def: expr.map(Box::new),
            typ: opt(preceded(one_of(Token::TypeOp), expr.map(Box::new))),
        }},
    ).parse_next(input)
}

fn typing(input: &mut &[Token]) -> PResult<ParseTree> {
    combinator::trace(
        "naming",
        combinator::seq! {ParseTree::IdentTyping {
            ident: ident.map(Box::new),
            _ : (one_of(Token::TypeOp))
                .context(StrContext::Expected(StrContextValue::StringLiteral(":"))),
            typ: expr.map(Box::new),
        }},
    ).parse_next(input)
}


fn PTassign(ident: ParseTree, expr: ParseTree, typ: Option<ParseTree>) -> ParseTree { ParseTree::IdentAssign { ident: Box::new(ident), def: Box::new(expr), typ: typ.map(Box::new) } }

fn PTtyping(ident: ParseTree, typ: ParseTree) -> ParseTree { ParseTree::IdentTyping { ident: Box::new(ident), typ: Box::new(typ) } }

fn PTident(string: &str) -> ParseTree { ParseTree::Ident(string.to_owned()) }

#[test]
fn test_parse_def() {
    assert_eq!(
        def.parse_peek(&mut &lexer(&mut "one := 123 : Number :").unwrap()),
        Ok((&[Token::TypeOp][..], PTassign(PTident("one"), ParseTree::Number(123), Some(PTident("Number")))))
    );
    // test if valid ident
    assert_eq!(
        def.parse_peek(&mut dbg!(&lexer(&mut "{} := 123").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid ident is caught
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid expr is caught
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("expression")))))
    );
}

#[test]
fn test_parse_typing() {
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "ident : Number Thingy").unwrap())),
        Ok((&[Token::Ident("Thingy".to_owned())][..], PTtyping(PTident("ident"), PTident("Number"))))
    );
    // test if := is caught
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "ident := 123").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::StringLiteral(":"))))))
    );
    // test if invalid ident is caught
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid expr is caught
    assert_eq!(
        typing.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("expression")))))
    );
}

fn expr_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(one_of(Token::OpenCurlyBracket), separated(0.., def, one_of(|t|matches!(t, Token::Separator(_)))), one_of(Token::ClosedCurlyBracket)).map(ParseTree::SetTerm).parse_next(input)
}
fn typ_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(one_of(Token::OpenSquareBracket), separated(0.., typing, one_of(|t|matches!(t, Token::Separator(_)))), one_of(Token::ClosedSquareBracket)).map(ParseTree::SetType).parse_next(input)
}


#[test]
fn parse_number() {
	let to_pase = "thing := 5";
}