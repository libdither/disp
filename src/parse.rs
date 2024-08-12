use core::fmt;

use winnow::{combinator::{self, alt, delimited, fail, opt, preceded, repeat, separated, separated_pair}, error::{AddContext, ContextError, ErrMode, ErrorKind, ParseError, ParserError, StrContext, StrContextValue}, prelude::*, stream::{Checkpoint, Stream}, token::{any, one_of}};

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
    TermSet(Vec<ParseTree>),
    /// list or named set of types
    /// { thing1 : String, thing2 : Number, OtherType }
    TypeSet(Vec<ParseTree>),
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
            ParseTree::TermSet(_) => todo!(),
            ParseTree::TypeSet(_) => todo!(),
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

fn PTstring(string: &str) -> ParseTree {
    ParseTree::String(string.to_owned())
}
fn PTnum(num: u64) -> ParseTree {
    ParseTree::Number(num)
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

fn PTident(string: &str) -> ParseTree { ParseTree::Ident(string.to_owned()) }

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
            typ: opt(preceded(
                one_of(Token::TypeOp).context(StrContext::Expected(StrContextValue::StringLiteral(":"))),
                expr.map(Box::new)
            )),
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
        def.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid expr is caught
    assert_eq!(
        def.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::StringLiteral(":="))))))
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
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::CharLiteral('{'))).add_context(&"", &"".checkpoint(), StrContext::Label("expression")))))
    );
}

fn expr_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
        separated(
            0..,
            alt((def, ident, literal)),
            one_of(|t|matches!(t, Token::Separator(_)))
                .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
                .context(StrContext::Expected(StrContextValue::StringLiteral("\\n")))
        ),
        one_of(Token::ClosedCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('}')))
    ).map(ParseTree::TermSet).parse_next(input)
}

fn PTtermset(set: &[ParseTree]) -> ParseTree {
    ParseTree::TermSet(set.to_owned())
}

#[test]
fn test_parse_expr_set() {
    assert_eq!(
        expr_set.parse(&mut dbg!(&lexer(&mut 
            "{ thingy := 123: Number, other_thingy := \"hi\", third_thingy, 456 }"
        ).unwrap())),
        Ok(PTtermset(&[
            PTassign(PTident("thingy"), ParseTree::Number(123), Some(PTident("Number"))),
            PTassign(PTident("other_thingy"), PTstring("hi"), None),
            PTident("third_thingy"),
            PTnum(456),
        ]))
    );
    let lex = dbg!(lexer(&mut 
        "{ thingy := 123: Number, other_thingy := }"
    ).unwrap());
    let parse_res = expr_set.parse(&mut &lex).unwrap_err();
    assert_eq!(
        parse_res.inner(),
        &ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::CharLiteral('}')))
    );
}

fn typ_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
        separated(
            0..,
            alt((typing, ident)),
            one_of(|t|matches!(t, Token::Separator(_)))
                .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
                .context(StrContext::Expected(StrContextValue::StringLiteral("\\n")))
        ),
        one_of(Token::ClosedCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('}')))
    ).map(ParseTree::TypeSet).parse_next(input)
}

fn PTtypset(set: &[ParseTree]) -> ParseTree {
    ParseTree::TypeSet(set.to_owned())
}

#[test]
fn test_parse_typ_set() {
    assert_eq!(
        typ_set.parse(&mut dbg!(&lexer(&mut 
            "{ thingy: Number, String, String, lol: Lol }"
        ).unwrap())),
        Ok(PTtypset(&[
            PTtyping(PTident("thingy"), PTident("Number")),
            PTident("String"),
            PTident("String"),
            PTtyping(PTident("lol"), PTident("Lol")),
        ]))
    );
    let lex = dbg!(lexer(&mut 
        "{ thingy := 123: Number, other_thingy := }"
    ).unwrap());
    let parse_res = expr_set.parse(&mut &lex).unwrap_err();
    assert_eq!(
        parse_res.inner(),
        &ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::CharLiteral('}')))
    );
}


#[test]
fn parse_number() {
	let to_pase = "thing := 5";
}