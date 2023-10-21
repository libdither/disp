#![allow(dead_code)]

pub mod lexer;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{prelude::*, combinator::MapExtra};
use hashdb::{TypeStore, hashtype, LinkArena};

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
#[derive(Debug)]
#[hashtype]
pub struct Ident<'e>(&'e str);

/// Relation that definitionally equates two objects
/// idt := obj
#[derive(Debug)]
#[hashtype]
pub struct DefRel<'e> {
	idt: Spanned<&'e AST<'e>>,
	obj: Spanned<&'e AST<'e>>,
}
/// Relation that associates a type with an object.
/// idt : typ
#[derive(Debug)]
#[hashtype]
pub struct TypRel<'e> {
	idt: Spanned<&'e AST<'e>>,
	typ: Spanned<&'e AST<'e>>,
}

/// Relation that associates a semantic parameter to a semantic body
#[derive(Debug)]
#[hashtype]
pub struct FuncRel<'e> {
	params: Spanned<&'e AST<'e>>, // usually a set
	body: Spanned<&'e AST<'e>>,
}

/// Relation that associates a function and an argument
/// `<func> {arg1, arg2}`
/// <func> [arg1, arg2]
/// <func> (arg1)
/// <func> <args>
#[derive(Debug)]
#[hashtype]
pub struct AppRel<'e> {
	func: Spanned<&'e AST<'e>>,
	args: Spanned<&'e AST<'e>>,
}

/// `let <name> := <val> : <type>`
/// `let <name> : <type> := <val>`
/// Requires <name> to be undefined in context, and for <term> and <type> to typecheck correctly
#[derive(Debug)]
#[hashtype]
pub struct LetDef<'e> {
	idt: Spanned<&'e Ident<'e>>,
	def: Spanned<&'e DefRel<'e>>,
	opt_typ: Option<Spanned<&'e TypRel<'e>>>,
}

/// `type <name> : <type>`
/// Names an undefined term of some type in some context.
#[derive(Debug)]
#[hashtype]
pub struct TypeDef<'e> {
	name: Spanned<&'e Ident<'e>>,
	rel: Spanned<&'e TypRel<'e>>,
}

/// Literals are technically identifiers, right?
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum IdentType {
	Literal(Literal),
	Symbol,
}

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
#[derive(Debug)]
#[hashtype]
pub enum AST<'e> {
	LetDef(LetDef<'e>),
	TypeDef(TypeDef<'e>),
	Ident(IdentType, &'e Ident<'e>),

	/// `{ <expr>, <expr> }` or `{ <expr>;\n <expr> }`
	/// Unordered set of expressions
	Set(&'e [Spanned<&'e AST<'e>>], Separator),
	/// `[ <expr>, <expr> ]` or `[ <expr>;\n <expr> ]`
	/// Ordered list of expressions
	List(&'e [Spanned<&'e AST<'e>>], Separator),
	/// `<val> -> <val>`
	/// A function abstraction
	Func(FuncRel<'e>),
	
	App(AppRel<'e>),
	/// `(expr1, expr2, ...)`
	Grouping(&'e [Spanned<&'e AST<'e>>]),
}

pub struct CustomState<'e, E: TypeStore<'e> + 'e> {
	/// Storage for the output of the parser
	pub links: &'e E,
}
type CustomExtra<'t, 's, 'e, E> = extra::Full<Rich<'t, Token<'s>, Span>, CustomState<'e, E>, ()>;

fn ast_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, Spanned<&'e AST<'e>>, CustomExtra<'t, 's, 'e, E>> {
	recursive(|expr| {
		let literal = select! {
			Token::Literal(x, src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Literal(x), state.links.add(Ident(state.links.add_str(src))))
			},
		}.labelled("literal")
		.map_with(|expr, extra: &mut MapExtra<ParserInput, CustomExtra<E>>|(extra.state().links.add(expr), extra.span()));

		let symbol = select! {
			Token::Ident(src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Symbol, state.links.add(Ident(state.links.add_str(src))))
			},
		}.labelled("symbol")
		.map_with(|expr, extra: &mut MapExtra<ParserInput, CustomExtra<E>>|(extra.state().links.add(expr), extra.span()));

		let sequence = expr.repeated().collect::<Vec<Spanned<&AST<'e>>>>();

		/* let set = end();
		let list = end();
		let app = end();
		let abs = end(); */

		let object = literal.or(symbol);

		// <expr> := <expr>
		let def_relation = symbol.then_ignore(just(Token::AssignOp)).then(object.clone()).map_with(|(idt, obj), extra|extra.state().links.add(DefRel { idt, obj }));
		// <expr> : <expr>
		let typ_relation = symbol.then_ignore(just(Token::TypeOp)).then(object.clone()).map_with(|(idt, typ), extra|extra.state().links.add(TypRel { idt, typ }));
		// let <ident> := <expr>
		let let_dec = just(Token::LetKW).ignore_then(def_relation).try_map_with(|rel, extra| match rel.idt {
			(AST::Ident(IdentType::Symbol, idt), idt_span) => Ok(extra.state().links.add(AST::LetDef(LetDef { idt: (idt, idt_span), def: (rel, extra.span()), opt_typ: None }))),
			_ => Err(Rich::custom(extra.span(), "failed to associate definition relation with let declaration. let declaration requires a identifier in the relation")),
		});
		// type <ident> : <expr>
		let typ_dec = just(Token::TypeKW).ignore_then(typ_relation).try_map_with(|rel, extra| match rel.idt {
			(AST::Ident(IdentType::Symbol, idt), idt_span) => Ok(extra.state().links.add(AST::TypeDef(TypeDef { name: (idt, idt_span), rel: (rel, extra.span()) }))),
			_ => Err(Rich::custom(extra.span(), "failed to associate type relation with type declaration. type declaration requires a identifier in the relation")),
		});

		// an expr can be a
		let_dec.or(typ_dec).map_with(|x, extra|(x, extra.span()))
	})
}

struct ParserState<'s, 'e> {
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

fn parse<'s, 'e: 's>(src: &'s str, state: &'e mut ParserState<'s, 'e>) -> (Option<Spanned<&'e AST<'e>>>, impl Iterator<Item = Rich<'s, String>>)  {
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

#[test]
fn test_parse() {
	let links = &LinkArena::new();
	let mut state = ParserState::new(links);

	let src = "let thing := ";
	let (ast, errs) = parse(src, &mut state);
	println!("{ast:?}");
	fancy_print_errors(errs, src);

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