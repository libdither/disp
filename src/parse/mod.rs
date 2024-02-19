#![allow(dead_code)]

pub mod lexer;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{input::MapExtra, prelude::*};
use hashdb::{hashtype, LinkArena, TypeStore};

use lexer::{Token, ParserInput, Spanned};
use rkyv::{Archive, Deserialize, Serialize};

use self::lexer::{Span, LexerState};

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
	CommaNewline,
	Newline,
}

/// Identifier
#[derive(Debug, Clone)]
#[hashtype]
pub struct Ident<'e>(&'e str);

/// Relation that denotes a function or type implication
/// Thing -> Thing
#[derive(Debug, Clone)]
#[hashtype]
pub struct FuncRel<'e> {
	arg: SpannedAST<'e>, // usually a set
	body: SpannedAST<'e>,
}

/// Relation that associates a function and an argument
/// `<func> {arg1, arg2}`
/// <func> [arg1, arg2]
/// <func> (arg1)
/// <func> <args>
#[derive(Debug, Clone)]
#[hashtype]
pub struct AppRel<'e> {
	func: SpannedAST<'e>,
	args: SpannedAST<'e>,
}

/// Literals are technically identifiers, right?
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum IdentType {
	Literal(Literal),
	Symbol,
}

type SpannedAST<'e> = Spanned<'e, &'e AST<'e>>;

/// <item>, <item> | <item>; <item>
#[derive(Debug, Clone)]
#[hashtype]
pub struct ASTSeq<'e> {
	items: &'e [SpannedAST<'e>],
}

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
#[derive(Debug)]
#[hashtype]
pub enum AST<'e> {
	Def { idt: SpannedAST<'e>, def: SpannedAST<'e>, typ: Option<SpannedAST<'e>> },
	Typ { idt: SpannedAST<'e>, typ: Option<SpannedAST<'e>> },
	
	/// Symbol or Literal
	Ident(IdentType, &'e Ident<'e>),

	Seq(ASTSeq<'e>),
	Set(ASTSeq<'e>),
	List(ASTSeq<'e>),
	/// `<expr> -> <expr>`
	/// A function abstraction
	Func(FuncRel<'e>),

	/// An application abstraction
	App(AppRel<'e>),
}

pub struct CustomState<'e, E: TypeStore<'e> + 'e> {
	/// Storage for the output of the parser
	pub links: &'e E,
}
type CustomExtra<'t, 's, 'e, E> = extra::Full<Rich<'t, Token<'s>, Span>, CustomState<'e, E>, ()>;

fn literal_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	// Literal
	select! {
		Token::Literal(typ, src) = e => {
			let state: &mut CustomState<E> = e.state();
			AST::Ident(IdentType::Literal(typ), state.links.add(Ident(state.links.add_str(src))))
		},
	}.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("literal")
}
fn symbol_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	// Symbol
	select! {
		Token::Symbol(src) = e => {
			let state: &mut CustomState<E> = e.state();
			AST::Ident(IdentType::Symbol, state.links.add(Ident(state.links.add_str(src))))
		},
	}
	.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("symbol")
}

fn ast_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	// An expression
	let mut expr = Recursive::declare();
	// An item, i.e. of a set or module
	// <rel> ::= <ident> + (":=" | ":") + <expr> + ((":=" | ":") + <expr>)?
	let relation = symbol_parser::<E>() // ident
	.then(just(Token::AssignOp).or(just(Token::TypeOp)) // := or :
	.then(expr.clone())) // expression
	.then(
		just(Token::AssignOp).or(just(Token::TypeOp)).then(expr.clone()).or_not()
	) // expression
	.try_map_with(|((idt, def), typ): ((SpannedAST, (Token, SpannedAST)), Option<(Token, SpannedAST)>), e|
		match (def, typ) {
			((Token::AssignOp, def), Some((Token::TypeOp, typ))) | ((Token::TypeOp, typ), Some((Token::AssignOp, def))) => Ok(AST::Def { idt, def, typ: Some(typ) }),
			((Token::AssignOp, def), None) => Ok(AST::Def { idt, def, typ: None }),
			((Token::TypeOp, typ), None) => Ok(AST::Typ { idt, typ: Some(typ) }),
			_ => panic!("somehow while parsing a relation at {:?}, we've parsed something other than := or :", e.span()),
		}
	)
	.map_with(|ast, e|Spanned::new(e.state().links.add(ast), e.span()))
	.labelled("relation");

	// <item> ::= <ast_rels> | <ident>
	let item = relation.labelled("statement");

	// seq ::= (<item> + "," | <item> + "\n" | item + ",\n")+
	let item_sequence = item.clone()
		.separated_by(any().filter(|t|match t {Token::Separator(_) => true, _ => false}))
		.collect::<Vec<SpannedAST<'e>>>()
	.map_with(|items, e: &mut MapExtra<_, CustomExtra<E>>| ASTSeq { items: e.state().links.add_slice(&items[..])});

	/* // set := { <sequence> }
	let set = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Curly)), just(Token::ClosedBracket(lexer::BracketType::Curly)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::Set(seq)), extra.span())).labelled("set");

	// list := [ <sequence> ]
	let list = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Square)), just(Token::ClosedBracket(lexer::BracketType::Square)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::List(seq)), extra.span())).labelled("list"); */

	// (<expr>)
	// let grouping = expr.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Paren)), just(Token::ClosedBracket(lexer::BracketType::Paren)));

	/* // <app> := <expr> <expr>
	let app = expr.clone().then(expr.clone())
		.map_with(|(func, args), extra|Spanned::new(extra.state().links.add(AST::App(AppRel { func, args })), extra.span()));

	// <abs> ::= <expr> + ("->" <expr>)?
	let abs = expr.clone().then_ignore(just(Token::AbsOp)).repeated().foldr_with(expr.clone(), |item, acc, extra| {
		Spanned::new(extra.state().links.add(AST::Func(FuncRel { arg: acc, body: item })), extra.span())
	}); */

	expr.define(literal_parser());

	let module = item_sequence.clone().map_with(|seq, e|Spanned::new(e.state().links.add(AST::Seq(seq)), e.span()))
	.recover_with(skip_then_retry_until(any().ignored(), end()));

	module
}

pub struct ParserState<'s, 'e> {
	lexer_state: LexerState<'s>,
	parser_state: CustomState<'e, LinkArena<'e>>,
}
impl<'s, 'e> ParserState<'s, 'e> {
	fn new(links: &'e LinkArena<'e>) -> Self {
		ParserState {
			lexer_state: LexerState::new(),
			parser_state: CustomState { links },
		}
	}
	fn reset(&mut self) {
		self.lexer_state.reset();
	}
}

// parse a string given some state
pub fn parse<'s, 'e: 's>(src: &'s str, state: &'e mut ParserState<'s, 'e>) -> (Option<SpannedAST<'e>>, impl Iterator<Item = Rich<'s, String>>)  {
	let lexer = self::lexer::lexer();
	let (lex_success, _) = lexer.parse_with_state(src, &mut state.lexer_state).into_output_errors();

	let ast_parser = ast_parser();
	
	let (spanned_ast, ast_errs) = if lex_success.is_some() {
		ast_parser.parse_with_state(state.lexer_state.slice().spanned((src.len()..src.len()).into()), &mut state.parser_state).into_output_errors()
	} else { (None, Vec::new()) };

	(
		spanned_ast,
		ast_errs
			.into_iter()
			.map(|e| e.map_token(|tok| tok.to_string())),
	)
}

pub fn parse_test(src: &'static str) -> (Option<SpannedAST<'_>>, Option<Vec<Rich<'_, String>>>) {
	let links = &*Box::leak(Box::new(LinkArena::new()));
	let state = Box::leak(Box::new(ParserState::new(links)));

	let (ast, errs) = parse(src, state);
	let errors = errs.collect::<Vec<Rich<_>>>();
	
	
	println!("AST: {ast:?}");
	println!("ERRs: {:?}", errors);
	// fancy_print_errors(errors.into_iter(), src);
	return (ast, if errors.is_empty() {None} else {Some(errors)})
}

#[test]
fn test_parse_error() {
	parse_test("thing := ").1.unwrap();
}
#[test]
fn test_parse_stmt() {
	parse_test(r#"thing := "hi!""#).0.unwrap();
}
#[test]
fn test_parse_function_and_sets() {
	parse_test(r#"
	let thing := {} {} -> ({} -> {
	
	})
	"#);
	panic!("check output");
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
pub fn fancy_print_errors<'a, I: std::fmt::Display + 'a>(errors: impl IntoIterator<Item = Rich<'a, I>>, source: &str) {
	let source = ariadne::Source::from(source);
	let mut reports = gen_reports(errors);
	reports.try_for_each(|rep|rep.eprint(source.clone())).unwrap();
}
