#![allow(dead_code)]

pub mod lexer;

use std::io::Write;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{input::MapExtra, prelude::*, recursive::Indirect};
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
pub struct Ident<'e>(pub &'e str);

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

pub type SpannedAST<'e> = Spanned<'e, &'e AST<'e>>;

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
	Map(ASTSeq<'e>),
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

fn seq_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>(expr: impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone) -> impl Parser<'t, ParserInput<'s, 't>, ASTSeq<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	// An item, i.e. of a set or module
	// <rel> ::= <ident> + ((":=" | ":") + <expr>)? + ((":=" | ":") + <expr>)?
	let item = symbol_parser::<E>() // ident
	.then(just(Token::AssignOp).or(just(Token::TypeOp)) // := or :
	.then(expr.clone()).or_not()) // expression
	.then(
		just(Token::AssignOp).or(just(Token::TypeOp)).then(expr.clone()).or_not()
	) // expression
	.try_map_with(|((idt, def), typ): ((SpannedAST, Option<(Token, SpannedAST)>), Option<(Token, SpannedAST)>), e|
		match (def, typ) {
			(Some((Token::AssignOp, def)), Some((Token::TypeOp, typ))) | (Some((Token::TypeOp, typ)), Some((Token::AssignOp, def))) => Ok(AST::Def { idt, def, typ: Some(typ) }),
			(Some((Token::AssignOp, def)), None) => Ok(AST::Def { idt, def, typ: None }),
			(Some((Token::TypeOp, typ)), None) => Ok(AST::Typ { idt, typ: Some(typ) }),
			(None, None) => Ok(AST::Typ { idt, typ: None }),
			_ => panic!("somehow while parsing a relation at {:?}, we've parsed something other than := or :", e.span()),
		}
	)
	.map_with(|ast, e|Spanned::new(e.state().links.add(ast), e.span()))
	.labelled("relation");

	// <sequence> ::= (<item> + ("," | "\n" | ",\n"))*
	let item_sequence = item.clone()
		.separated_by(any().filter(|t|match t {Token::Separator(_) => true, _ => false})).allow_trailing()
		.collect::<Vec<SpannedAST<'e>>>()
		.map_with(|items, e: &mut MapExtra<_, CustomExtra<E>>| ASTSeq { items: e.state().links.add_slice(&items[..])});

	item_sequence
}

fn expr_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	// An expression
	let mut expr = Recursive::declare();

	let item_sequence = seq_parser::<E>(expr.clone());

	// set := { <sequence> }
	let set = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Curly)), just(Token::ClosedBracket(lexer::BracketType::Curly)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::Set(seq)), extra.span())).labelled("set");

	// list := [ <sequence> ]
	let list = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Square)), just(Token::ClosedBracket(lexer::BracketType::Square)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::List(seq)), extra.span())).labelled("list");

	// <atom> := <set> | <list> | <literal> | <symbol> | ( "(" + <expr> + ")" )
	let atom = set.or(list).or(literal_parser().or(symbol_parser()))
	.or( // atom expression that is parenthesized
		expr.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Paren)), just(Token::ClosedBracket(lexer::BracketType::Paren)))
	).labelled("atomic expression");

	// <app> := <atom> + (" " + <atom>)+
	// Note: use atom instead of direct expr to avoid infinite recursion
	let app = atom.clone().foldl_with(atom.clone().repeated().at_least(1), |acc, item, extra| {
		Spanned::new(extra.state().links.add(AST::App(AppRel { func: acc, args: item })), extra.span())
	});

	// <abs> := (<atom> + "->") + <atom>
	let abs = atom.clone().then_ignore(just(Token::FuncOp)).then(atom.clone()).map_with(|(arg, body), extra| {
		Spanned::new(extra.state().links.add(AST::Func(FuncRel { arg, body })), extra.span())
	});

	// <expr> := <abs> | <app> | <atom>
	expr.define(app.or(abs).or(atom).labelled("expression"));

	expr
}

fn file_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> + Clone {
	let item_sequence = seq_parser::<E>(expr_parser::<E>());
	// <module> := <sequence>
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
pub fn parse<'s, 'e: 's>(src: &'s str, state: &'e mut ParserState<'s, 'e>) -> (
	Option<SpannedAST<'e>>,
	impl Iterator<Item = Rich<'s, String>>,
	&'e [(Token<'s>, Span)],
	impl Iterator<Item = Rich<'s, String>>, // lexer errors
)  {
	state.lexer_state.reset();
	let lexer = self::lexer::lexer();
	let (lex_success, lexer_errs) = lexer.parse_with_state(src, &mut state.lexer_state).into_output_errors();
	let tokens = state.lexer_state.slice().spanned((src.len()..src.len()).into());

	let ast_parser = expr_parser();
	
	let (spanned_ast, parse_errs) = if lex_success.is_some() {
		ast_parser.parse_with_state(tokens, &mut state.parser_state).into_output_errors()
	} else { (None, Vec::new()) };

	(
		spanned_ast,
        parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),

		state.lexer_state.slice(),
		lexer_errs.into_iter().map(|e| e.map_token(|c| c.to_string())),
	)
}

pub fn parse_test(src: &'static str) -> (Option<SpannedAST<'_>>, Option<Vec<Rich<'_, String>>>) {
	let links = &*Box::leak(Box::new(LinkArena::new()));
	let state = Box::leak(Box::new(ParserState::new(links)));

	println!("PARSING: {src:?}");

	let (ast, errs, tokens, lex_errs) = parse(src, state);

	let errors = errs.collect::<Vec<Rich<_>>>();
	let lex_errors = lex_errs.collect::<Vec<Rich<_>>>();
	
	println!("TOKENS: {tokens:?}");
	println!("LEX_ERRS: {lex_errors:?}");
	println!("AST: {ast:?}");
	println!("ERRs: {:?}", errors);
	fancy_print_errors(errors.iter(), src, std::io::stdout());
	return (ast, if errors.is_empty() {None} else {Some(errors)})
}

#[test]
fn parse_test_error() {
	parse_test("thing := ").1.unwrap();
}
#[test]
fn parse_test_relation() {
	parse_test(r#"thing := "hi!""#).0.unwrap();
	parse_test(r#"thing : "hi!""#).0.unwrap();
	parse_test(r#"thing : Type"#).0.unwrap();
	parse_test(r#"
		thing : String := "LOL"
		thing := "LOL" : String
	"#).0.unwrap();
}
#[test]
fn test_parse_set() {
	parse_test(r#"
	thing := { one := "test", two := "yeet" : String, }
	"#).0.unwrap();
}

#[test]
fn test_parse_application() {
	parse_test(r#"
	thing := one two
	thing := one two three
	"#).0.unwrap();
}
#[test]
fn test_parse_function() {
	parse_test(r#"
	thing := one -> two
	thing := { one, two } -> three
	"#).0.unwrap();
	// should error
	parse_test(r#"
	thing := { one, two } -> three four
	"#).1.unwrap();
	parse_test(r#"
	thing := ({ one, two } -> three) -> four
	"#).0.unwrap();
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
pub fn gen_reports<'i, 'a: 'i, I: std::fmt::Display + 'a>(errors: impl Iterator<Item = &'i Rich<'a, I>> + 'i) -> impl Iterator<Item = Report<'a>> + 'i {
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
pub fn fancy_print_errors<'i, 'a: 'i, I: std::fmt::Display + 'a>(errors: impl Iterator<Item = &'i Rich<'a, I>> + 'i, source: &str, mut writer: impl Write) {
	let source = ariadne::Source::from(source);
	let reports = gen_reports(errors);
	for rep in reports {
		rep.write_for_stdout(source.clone(), &mut writer).expect("failed to write fancy errors");
	}
}
