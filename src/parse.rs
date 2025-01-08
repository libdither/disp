#![allow(unused, non_snake_case)]
use core::fmt;

use itertools::Itertools;
use winnow::{combinator::{self, alt, dispatch, delimited, empty, fail, opt, preceded, repeat, separated, separated_pair, terminated}, error::{AddContext, ContextError, ErrMode, ErrorKind, ParseError, ParserError, StrContext, StrContextValue}, prelude::*, stream::{Checkpoint, Stream}, token::{any, one_of}};

use crate::lexer::{lexer, Token};

#[derive(Clone, PartialEq, Debug)]
pub enum ParseTree {
	Number(u64),
    String(String),
	Ident(String),
    /// ident := def : Type
    /// ident : Type := def
    /// ident := def
    AssignIdentExpr { ident: Box<ParseTree>, def: Box<ParseTree>, typ: Option<Box<ParseTree>> },
    /// ident : Type
    AssignIdentType { ident: Box<ParseTree>, typ: Box<ParseTree> },
    /// list or named set (or combination) in term form
    /// { thing1 := "abc" : String, thing2 := 123, 456, "seven" }
    ExprSet(Vec<ParseTree>),
    /// list or named set of types
    /// { thing1 : String, thing2 : Number, OtherType }
    TypeSet(Vec<ParseTree>),
    /// list of idents and an expression (λ expr)
    /// { val } -> {1,2,3,4,5}
    ExprFunc { args: Box<ParseTree>, body: Box<ParseTree> },
    /// list of input types and ident or typeset (Π type)
    /// { val: Num } -> List{Num,val}
    TypeFunc { args: Box<ParseTree>, body: Box<ParseTree> },
    Apply { func: Box<ParseTree>, args: Box<ParseTree> },
}

impl fmt::Display for ParseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseTree::Number(num) => write!(f, "{}", num),
            ParseTree::String(string) => write!(f, "\"{}\"", string),
            ParseTree::Ident(ident) => write!(f, "{}", ident),
            ParseTree::AssignIdentExpr { ident, def, typ } => {
                write!(f, "{ident} := {def}")?;
                if let Some(typ) = typ { write!(f, " : {typ}")?; } Ok(())
            },
            ParseTree::AssignIdentType { ident, typ } => write!(f, "{ident} : {typ}"),
            ParseTree::ExprSet(items) => write!(f, "{{{}}}", items.iter().format(", ")),
            ParseTree::TypeSet(items) => write!(f, "{{{}}}", items.iter().format(", ")),
            ParseTree::ExprFunc { args, body } => write!(f, "{args} -> {body}"),
            ParseTree::TypeFunc { args, body } => write!(f, "{args} -> {body}"),
            ParseTree::Apply { func, args } => write!(f, "{func} {args}"),
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
    }).parse_next(input)
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

pub fn paren_expr(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenParen)
            .context(StrContext::Expected(StrContextValue::CharLiteral('('))),
        expr,
        one_of(Token::ClosedParen)
            .context(StrContext::Expected(StrContextValue::CharLiteral(')')))
    ).parse_next(input)
}
pub fn paren_typ(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenParen)
            .context(StrContext::Expected(StrContextValue::CharLiteral('('))),
        typ,
        one_of(Token::ClosedParen)
            .context(StrContext::Expected(StrContextValue::CharLiteral(')')))
    ).parse_next(input)
}

pub fn expr_atom(input: &mut &[Token]) -> PResult<ParseTree> {
    alt((ident, literal, expr_set, typ_set, paren_expr))
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
    let maybe_args = opt(terminated(alt((args_set, ident)), one_of(Token::FuncOp))).parse_next(input)?;
    if let Some(args) = maybe_args {
        Ok(ParseTree::ExprFunc { args: Box::new(args), body: expr.map(Box::new).parse_next(input)? })
    } else {
        repeat(1.., expr_atom).fold(||None, |func, args| {
            if let Some(func) = func {
                Some(ParseTree::Apply { func: Box::new(func), args: Box::new(args) })
            } else { Some(args) }
        }).map(Option::unwrap).parse_next(input)
    }
}

fn expr_assign(input: &mut &[Token]) -> PResult<ParseTree> {
    // parse :=
    fn assign_op_context(input: &mut &[Token]) -> PResult<()> {
        one_of(Token::AssignOp).context(StrContext::Expected(StrContextValue::StringLiteral(":="))).value(()).parse_next(input)
    }
    // parse :
    fn typ_op_context(input: &mut &[Token]) -> PResult<()> {
        one_of(Token::TypeOp).context(StrContext::Expected(StrContextValue::StringLiteral(":"))).value(()).parse_next(input)
    }
    // parse ident
    let ident = ident.map(Box::new).context(StrContext::Label("ident")).parse_next(input)?;
    // parse := or :
    let is_expr_next = alt(( // dispatch doesn't work here for some reason?
        one_of(Token::AssignOp).value(true),
        one_of(Token::TypeOp).value(false),
        fail.context(StrContext::Expected(StrContextValue::StringLiteral(":=")))
                .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
    )).parse_next(input)?;

    Ok(if is_expr_next {
        // parse expr, and optional type
        let def = expr.map(Box::new).parse_next(input)?;
        let typ = opt(preceded(typ_op_context, typ.map(Box::new))).parse_next(input)?;
        ParseTree::AssignIdentExpr { ident, def, typ }
    } else {
        // parse typ and expr
        let typ = typ.map(Box::new).parse_next(input)?;
        let def = preceded(assign_op_context, expr.map(Box::new)).parse_next(input)?;
        ParseTree::AssignIdentExpr { ident, def, typ: Some(typ) }
    })
}

fn typ(input: &mut &[Token]) -> PResult<ParseTree> {
    combinator::trace(
        "type",
        (
            alt((typ_set, paren_typ, ident)),
            opt(preceded(one_of(Token::FuncOp), typ.map(Box::new)))
        ).map(|(typ_atom, opt_body)|if let Some(body) = opt_body {
            ParseTree::TypeFunc { args: Box::new(typ_atom), body }
        } else {
            typ_atom
        }).context(StrContext::Label("type"))
    ).parse_next(input)
}

fn typ_assign(input: &mut &[Token]) -> PResult<ParseTree> {
    combinator::trace(
        "type_assign",
        combinator::seq! {ParseTree::AssignIdentType {
            ident: ident.map(Box::new).context(StrContext::Label("ident")),
            _ : (one_of(Token::TypeOp))
                .context(StrContext::Expected(StrContextValue::StringLiteral(":"))),
            typ: typ.map(Box::new),
        }},
    ).parse_next(input)
}


fn PTassign(ident: ParseTree, expr: ParseTree, typ: Option<ParseTree>) -> ParseTree { ParseTree::AssignIdentExpr { ident: Box::new(ident), def: Box::new(expr), typ: typ.map(Box::new) } }

fn PTtyping(ident: ParseTree, typ: ParseTree) -> ParseTree { ParseTree::AssignIdentType { ident: Box::new(ident), typ: Box::new(typ) } }

#[test]
fn test_parse_def() {
    // test assign and typing
    assert_eq!(
        expr_assign.parse_peek(&mut &lexer(&mut "one := 123 : Number :").unwrap()),
        Ok((&[Token::TypeOp][..], PTassign(PTident("one"), ParseTree::Number(123), Some(PTident("Number")))))
    );
    // test typing and assign
    assert_eq!(
        expr_assign.parse_peek(&mut &lexer(&mut "one : Number := 123 :").unwrap()),
        Ok((&[Token::TypeOp][..], PTassign(PTident("one"), ParseTree::Number(123), Some(PTident("Number")))))
    );
    // test no type
    assert_eq!(
        expr_assign.parse_peek(&mut &lexer(&mut "one := 123").unwrap()),
        Ok((&[][..], PTassign(PTident("one"), ParseTree::Number(123), None)))
    );
    // test assign func
    assert_eq!(
        expr_assign.parse_peek(&mut &lexer(&mut "id := x -> x").unwrap()),
        Ok((&[][..], PTassign(PTident("id"), PTefunc(PTident("x"), PTident("x")), None)))
    );
    // test assign application
    assert_eq!(
        expr_assign.parse_peek(&mut &lexer(&mut "one := succ zero").unwrap()),
        Ok((&[][..], PTassign(PTident("one"), PTeapp(PTident("succ"), PTident("zero")), None)))
    );

    // fail tests
    // test invalid assign op
    assert_eq!(
        expr_assign.parse_peek(&mut dbg!(&lexer(&mut "a = 123").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::StringLiteral(":=")))
        .add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::StringLiteral(":"))))))
    );
    // test if valid ident
    assert_eq!(
        expr_assign.parse_peek(&mut dbg!(&lexer(&mut "{} := 123").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid ident is caught
    assert_eq!(
        expr_assign.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid expr is caught
    assert_eq!(
        expr_assign.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("type")))))
    );
}

#[test]
fn test_parse_typing() {
    assert_eq!(
        typ_assign.parse_peek(&mut dbg!(&lexer(&mut "ident : Number Thingy").unwrap())),
        Ok((&[Token::Ident("Thingy".to_owned())][..], PTtyping(PTident("ident"), PTident("Number"))))
    );
    // test if := is caught
    assert_eq!(
        typ_assign.parse_peek(&mut dbg!(&lexer(&mut "ident := 123").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Expected(StrContextValue::StringLiteral(":"))))))
    );
    // test if invalid ident is caught
    assert_eq!(
        typ_assign.parse_peek(&mut dbg!(&lexer(&mut "123 : Ident").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("ident")))))
    );
    // test if invalid expr is caught
    assert_eq!(
        typ_assign.parse_peek(&mut dbg!(&lexer(&mut "ident : :=").unwrap())),
        dbg!(Err(ErrMode::Backtrack(ContextError::new().add_context(&"", &"".checkpoint(), StrContext::Label("type")))))
    );
}

fn expr_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
        separated(
            0..,
            alt((expr_assign, ident, literal)),
            one_of(|t|matches!(t, Token::Separator(_)))
                .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
                .context(StrContext::Expected(StrContextValue::StringLiteral("\\n")))
        ),
        one_of(Token::ClosedCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('}')))
    ).map(ParseTree::ExprSet).parse_next(input)
}

fn PTexprset(set: &[ParseTree]) -> ParseTree {
    ParseTree::ExprSet(set.to_owned())
}

#[test]
fn test_parse_expr_set() {
    assert_eq!(
        expr_set.parse(&mut dbg!(&lexer(&mut 
            "{ thingy := 123: Number, other_thingy := \"hi\", third_thingy, 456 }"
        ).unwrap())),
        Ok(PTexprset(&[
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
            alt((typ_assign, ident)),
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

fn args_set(input: &mut &[Token]) -> PResult<ParseTree> {
    delimited(
        one_of(Token::OpenCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('{'))),
        separated(
            0..,
            ident,
            one_of(|t|matches!(t, Token::Separator(_)))
                .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
                .context(StrContext::Expected(StrContextValue::StringLiteral("\\n")))
        ),
        one_of(Token::ClosedCurlyBracket)
            .context(StrContext::Expected(StrContextValue::CharLiteral('}')))
    ).map(ParseTree::ExprSet).parse_next(input)
}

fn PTefunc(args: ParseTree, body: ParseTree) -> ParseTree {
    ParseTree::ExprFunc { args: Box::new(args), body: Box::new(body)}
}

#[test]
fn test_parse_expr_func() {
    assert_eq!(
        expr.parse(&mut dbg!(&lexer(&mut 
            "x -> x"
        ).unwrap())),
        Ok(PTefunc(PTident("x"), PTident("x")))
    );
    assert_eq!(
        dbg!(expr.parse(&mut &lexer(&mut 
            "x -> x -> x"
        ).unwrap())),
        Ok(PTefunc(PTident("x"), PTefunc(PTident("x"), PTident("x"))))
    );
    assert_eq!(
        dbg!(expr.parse(&mut dbg!(&lexer(&mut 
            "x -> {y} -> x"
        ).unwrap()))),
        Ok(PTefunc(PTident("x"), PTefunc(PTexprset(&[PTident("y")]), PTident("x"))))
    );
}

fn PTeapp(func: ParseTree, args: ParseTree) -> ParseTree {
    ParseTree::Apply { func: Box::new(func), args: Box::new(args) }
}

#[test]
fn test_parse_expr_app() {
    // normal
    assert_eq!(
        expr.parse(&mut dbg!(&lexer(&mut 
            "a b c"
        ).unwrap())),
        Ok(PTeapp(PTeapp(PTident("a"), PTident("b")), PTident("c")))
    );
    // parens
    assert_eq!(
        dbg!(expr.parse(&mut &lexer(&mut 
            "a (b c)"
        ).unwrap())),
        Ok(PTeapp(PTident("a"), PTeapp(PTident("b"), PTident("c"))))
    );
    //
    assert_eq!(
        dbg!(expr.parse(&mut dbg!(&lexer(&mut 
            "(a)(b)(c)"
        ).unwrap()))),
        Ok(PTeapp(PTeapp(PTident("a"), PTident("b")), PTident("c")))
    );
}

pub fn parse_file(input: &mut &[Token]) -> PResult<Vec<ParseTree>> {
    repeat(0.., delimited(
        opt(one_of(|t|matches!(t, Token::Separator(_)))),
        expr_assign,
        opt(one_of(|t|matches!(t, Token::Separator(_))))
    )).parse_next(input)
}