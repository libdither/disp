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
	Semicolon,
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

/// `<idt> := <val>`
/// `<idt> : <typ>`
/// `<idt> := <def> : <typ>`
/// `<idt> : <typ> := <def>`
/// A definition or type relation between two or three exprs.
/// <idt> may or may not be a Ident
#[derive(Debug, Clone)]
#[hashtype]
pub struct ASTRel<'e> {
	idt: SpannedAST<'e>,
	def: Option<SpannedAST<'e>>,
	typ: Option<SpannedAST<'e>>,
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
	Rel(ASTRel<'e>), // non-prefaced relations
	LetDef(ASTRel<'e>),
	TypDef(ASTRel<'e>),
	
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


fn ast_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> {
	// Literal
	let literal = select! {
		Token::Literal(typ, src) = e => {
			let state: &mut CustomState<E> = e.state();
			AST::Ident(IdentType::Literal(typ), state.links.add(Ident(state.links.add_str(src))))
		},
	}.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("literal");

	// Symbol
	let symbol = select! {
		Token::Ident(src) = e => {
			let state: &mut CustomState<E> = e.state();
			AST::Ident(IdentType::Symbol, state.links.add(Ident(state.links.add_str(src))))
		},
		Token::Literal(typ, src) = e => {
			let state: &mut CustomState<E> = e.state();
			AST::Ident(IdentType::Literal(typ), state.links.add(Ident(state.links.add_str(src))))
		},
	}
	.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("symbol");

	// <ident> ::= <literal> | <symbol>
	let ident = literal.or(symbol);
	
	// An expression
	let mut expr = Recursive::declare();
	// An item, i.e. of a set or module
	// <rel> ::= <ident> + (":=" | ":") + <expr> + ((":=" | ":") + <expr>)?
	let relation = ident.clone() // ident
		.then(just(Token::AssignOp).or(just(Token::TypeOp)) // := or :
		.then(expr.clone())) // expression
		.then(just(Token::AssignOp).or(just(Token::TypeOp)) // := or :
		.then(expr.clone()).or_not()) // expression
		.try_map_with(|((idt, def), typ): ((SpannedAST, (Token, SpannedAST)), Option<(Token, SpannedAST)>), e| match (def, typ) {
		((Token::AssignOp, def), Some((Token::TypeOp, typ))) => Ok(ASTRel { idt, def: Some(def), typ: Some(typ) }),
		((Token::TypeOp, typ), None) => Ok(ASTRel { idt, typ: Some(typ), def: None }),
		((Token::TypeOp, typ), Some((Token::AssignOp, def))) => Ok(ASTRel { idt, typ: Some(typ), def: Some(def) }),
		_ => Err(Rich::custom(e.span(), "relation must have have at least one of `:=` or `:` and at most one of both")),
	}).labelled("relation");

	// <ast_rels> ::= ("let" | "type")? + <rel>
	let ast_rels = just(Token::LetKW).or(just(Token::TypeKW)).or(empty().to(Token::TypeOp))
	.then(relation).map_with(|(tok, rel): (Token, ASTRel), e: &mut MapExtra<_, CustomExtra<E>>| Spanned::new(e.state().links.add(match tok {
		Token::LetKW => AST::LetDef(rel),
		Token::TypeKW => AST::TypDef(rel),
		Token::TypeOp => AST::Rel(rel),
		_ => unreachable!(),
	}), e.span()));

	// <item> ::= <ast_rels> | <ident>
	let item = ast_rels.or(ident);

	// <item> + "," | <item> + ";"
	let item_sequence = item.clone().then_ignore(
		just(Token::Separator(Separator::Comma))
		.or(
			just(Token::Separator(Separator::Semicolon))
		)
	).repeated().collect::<Vec<SpannedAST<'e>>>()
	.map_with(|items, e: &mut MapExtra<_, CustomExtra<E>>| ASTSeq { items: e.state().links.add_slice(&items[..])}).labelled("sequence");

	/* // <item>, <item>, ...,
	let item_comma_sequence = item.clone().or(ident).clone()
	.separated_by(just(Token::Separator(Separator::Comma))).allow_trailing()
	.collect::<Vec<Spanned<&AST<'e>>>>().map_with(|items, e: &mut MapExtra<_, CustomExtra<E>>|ASTSeq { items: e.state().links.add_slice(&items[..]), sep: Separator::Comma}).labelled("comma-separated sequence");
	// <expr>; <expr>; ...;
	let item_semicolon_sequence = item.clone().clone()
		.separated_by(just(Token::Separator(Separator::Semicolon))).allow_trailing()
		.collect::<Vec<Spanned<&AST<'e>>>>().map_with(|items, e: &mut MapExtra<_, CustomExtra<E>>|ASTSeq { items: e.state().links.add_slice(&items[..]), sep: Separator::Semicolon }).labelled("semicolon-separated sequence"); */

	// <comma_sequence> OR <semicolon_sequence> 
	// let item_sequence = item_comma_sequence.clone().or(item_semicolon_sequence.clone()).labelled("sequence");

	// set := { <sequence> }
	let set = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Curly)), just(Token::ClosedBracket(lexer::BracketType::Curly)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::Set(seq)), extra.span())).labelled("set");

	// list := [ <sequence> ]
	let list = item_sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Square)), just(Token::ClosedBracket(lexer::BracketType::Square)))
	.map_with(|seq, extra|Spanned::new(extra.state().links.add(AST::List(seq)), extra.span())).labelled("list");

	// (<expr>)
	// let grouping = expr.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Paren)), just(Token::ClosedBracket(lexer::BracketType::Paren)));

	/* // <app> := <expr> <expr>
	let app = expr.clone().then(expr.clone())
		.map_with(|(func, args), extra|Spanned::new(extra.state().links.add(AST::App(AppRel { func, args })), extra.span()));

	// <abs> ::= <expr> + ("->" <expr>)?
	let abs = expr.clone().then_ignore(just(Token::AbsOp)).repeated().foldr_with(expr.clone(), |item, acc, extra| {
		Spanned::new(extra.state().links.add(AST::Func(FuncRel { arg: acc, body: item })), extra.span())
	}); */

	expr.define(set.or(list).or(ident)/* .or(app.or(abs)) */);

	let module = item_sequence.clone().map_with(|seq, e|Spanned::new(e.state().links.add(AST::Seq(seq)), e.span()));

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
	let (lex_success, lex_errs) = lexer.parse_with_state(src, &mut state.lexer_state).into_output_errors();

	let ast_parser = ast_parser();
	
	let (spanned_ast, ast_errs) = if lex_success.is_some() {
		ast_parser.parse_with_state(state.lexer_state.slice().spanned((src.len()..src.len()).into()), &mut state.parser_state).into_output_errors()
	} else { (None, Vec::new()) };

	(
		spanned_ast,
		lex_errs.into_iter()
			.map(|e| e.map_token(|c| c.to_string()))
			.chain(
				ast_errs
					.into_iter()
					.map(|e| e.map_token(|tok| tok.to_string())),
			)
	)
}

pub fn parse_test(src: &str) {
	let links = &LinkArena::new();
	let mut state = ParserState::new(links);

	let (ast, errs) = parse(src, &mut state);

	println!("{ast:?}");
	fancy_print_errors(errs, src);
}

#[test]
fn test_parse_error() {
	parse_test("let thing := ");
	panic!("check output");
}
#[test]
fn test_parse_stmt() {
	parse_test(r#"let thing := "hi!";"#);
	panic!("check output");
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
