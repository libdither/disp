#![allow(dead_code)]

pub mod lexer;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{input::MapExtra, prelude::*, primitive::select};
use hashdb::{hashtype, HashType, LinkArena, TypeStore};

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
	idt: SpannedAST<'e>,
	obj: SpannedAST<'e>,
}
/// Relation that associates a type with an object.
/// idt : typ
#[derive(Debug)]
#[hashtype]
pub struct TypRel<'e> {
	idt: SpannedAST<'e>,
	typ: SpannedAST<'e>,
}

/// Relation that denotes a function or type implication
/// Thing -> Thing
#[derive(Debug)]
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
#[derive(Debug)]
#[hashtype]
pub struct AppRel<'e> {
	func: SpannedAST<'e>,
	args: SpannedAST<'e>,
}

/// `let <name> := <val> : <type>`
/// 
/// `let <name> : <type> := <val>`
/// 
/// Requires <name> to be undefined in context, and for <term> and <type> to typecheck correctly
#[derive(Debug)]
#[hashtype]
pub struct LetDef<'e> {
	idt: Spanned<'e, &'e Ident<'e>>,
	def: Spanned<'e, &'e DefRel<'e>>,
	opt_typ: Option<Spanned<'e, &'e TypRel<'e>>>,
}

/// `type <name> : <type>`
/// Names an undefined term of some type in some context.
#[derive(Debug)]
#[hashtype]
pub struct TypeDef<'e> {
	name: Spanned<'e, &'e Ident<'e>>,
	rel: Spanned<'e, &'e TypRel<'e>>,
}

/// Literals are technically identifiers, right?
#[derive(Debug, Clone, PartialEq, Hash, Archive, Serialize, Deserialize)]
pub enum IdentType {
	Literal(Literal),
	Symbol,
}

type SpannedAST<'e> = Spanned<'e, &'e AST<'e>>;

/// A value is a reference to a previously defined term, a constant literal, term construction, or type construction
#[derive(Debug)]
#[hashtype]
pub enum AST<'e> {
	DefRel(DefRel<'e>),
	TypRel(TypRel<'e>),
	LetDef(LetDef<'e>),
	TypeDef(TypeDef<'e>),
	Ident(IdentType, &'e Ident<'e>),

	/// `{ <expr>, <expr> }` or `{ <expr>;\n <expr> }`
	/// Unordered set of expressions
	Set(&'e [SpannedAST<'e>], Separator),
	/// `[ <expr>, <expr> ]` or `[ <expr>;\n <expr> ]`
	/// Ordered list of expressions
	List(&'e [SpannedAST<'e>], Separator),
	/// `<val> -> <val>`
	/// A function abstraction
	Func(FuncRel<'e>),
	
	App(AppRel<'e>),
}

pub struct CustomState<'e, E: TypeStore<'e> + 'e> {
	/// Storage for the output of the parser
	pub links: &'e E,
}
type CustomExtra<'t, 's, 'e, E> = extra::Full<Rich<'t, Token<'s>, Span>, CustomState<'e, E>, ()>;

fn ast_parser<'s: 't, 't, 'e: 't, E: TypeStore<'e> + 'e>() -> impl Parser<'t, ParserInput<'s, 't>, SpannedAST<'e>, CustomExtra<'t, 's, 'e, E>> {
	recursive(|expr| {
		let literal = select! {
			Token::Literal(typ, src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Literal(typ), state.links.add(Ident(state.links.add_str(src))))
			},
		}.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("literal");

		let symbol = select! {
			Token::Ident(src) = e => {
				let state: &mut CustomState<E> = e.state();
				AST::Ident(IdentType::Symbol, state.links.add(Ident(state.links.add_str(src))))
			},
		}
		.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(item), extra.span())).labelled("symbol");

		let atom = literal.or(symbol);
	
		// <expr> := <expr>
		let def_relation = symbol.then_ignore(just(Token::AssignOp)).then(expr.clone()).map_with(|(idt, obj), _|DefRel { idt, obj }).labelled("def relation");
		// <expr> : <expr>
		let typ_relation = symbol.then_ignore(just(Token::TypeOp)).then(expr.clone()).map_with(|(idt, typ), _|TypRel { idt, typ }).labelled("typ relation");
		// let <ident> := <expr>
		let let_dec = just(Token::LetKW).ignore_then(def_relation.clone()).try_map_with(|rel, extra| match rel.idt.tup() {
			(AST::Ident(IdentType::Symbol, idt), idt_span) => Ok(Spanned::new(extra.state().links.add(AST::LetDef(LetDef { idt: Spanned::new(idt, idt_span.clone()), def: Spanned::new(extra.state().links.add(rel), extra.span()), opt_typ: None })), extra.span())),
			_ => Err(Rich::custom(extra.span(), "failed to associate definition relation with let declaration. let declaration requires a identifier in the relation")),
		});
		// type <ident> : <expr>
		let typ_dec = just(Token::TypeKW).ignore_then(typ_relation.clone()).try_map_with(|rel, extra| match rel.idt.tup() {
			(AST::Ident(IdentType::Symbol, idt), idt_span) => Ok(Spanned::new(extra.state().links.add(AST::TypeDef(TypeDef { name: Spanned::new(idt, idt_span.clone()), rel: Spanned::new(extra.state().links.add(rel), extra.span()) })), extra.span())),
			_ => Err(Rich::custom(extra.span(), "failed to associate type relation with type declaration. type declaration requires a identifier in the relation")),
		});

		// ast-wrapped versions of DefRel and TypRel
		let def_relation_ast = def_relation.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(AST::DefRel(item)), extra.span()));

		let typ_relation_ast = typ_relation.map_with(|item, extra: &mut MapExtra<_, CustomExtra<E>>|Spanned::new(extra.state().links.add(AST::TypRel(item)), extra.span()));

		let item = atom.or(def_relation_ast.or(typ_relation_ast)).or(let_dec.or(typ_dec));

		/* // <expr>, <expr>, ...,
		let comma_sequence = item.clone()
			.separated_by(just(Token::Separator(Separator::Comma))).allow_trailing()
			.collect::<Vec<Spanned<&AST<'e>>>>().map(|x|(x, Separator::Comma)).labelled("comma-separated sequence");
		// <expr>; <expr>; ...;
		let semicolon_sequence = item.clone()
			.separated_by(just(Token::Separator(Separator::Semicolon))).allow_trailing()
			.collect::<Vec<Spanned<&AST<'e>>>>().map(|x|(x, Separator::Semicolon)).labelled("semicolon-separated sequence");

		// <comma_sequence> OR <semicolon_sequence>		
		let sequence = comma_sequence.or(semicolon_sequence).labelled("sequence");
		
		// set := { <sequence> }
		let set = sequence.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Curly)), just(Token::ClosedBracket(lexer::BracketType::Curly)))
			.map_with(|(seq, sep): (Vec<Spanned<'_, &AST::<'_>>>, Separator), extra: &mut MapExtra<_, CustomExtra<E>>| AST::Set(extra.state().links.add_slice(&seq[..]), sep))
			.map_with(|item, extra|Spanned::new(extra.state().links.add(item), extra.span())).labelled("set");
		
		// list := [ <sequence> ]
		let list = sequence.delimited_by(just(Token::OpenBracket(lexer::BracketType::Square)), just(Token::ClosedBracket(lexer::BracketType::Square)))
		.map_with(|(seq,sep), extra: &mut MapExtra<_, CustomExtra<E>>| AST::List(extra.state().links.add_slice(&seq[..]), sep))
		.map_with(|item, extra|Spanned::new(extra.state().links.add(item), extra.span())).labelled("list"); */

		// (<expr>)
		/* let grouping = object.clone().delimited_by(just(Token::OpenBracket(lexer::BracketType::Paren)), just(Token::ClosedBracket(lexer::BracketType::Paren)));

		let object = object.or(grouping); */

		/* let app = expr.clone().then(expr.clone())
			.map_with(|(func, args), extra|Spanned::new(extra.state().links.add(AST::App(AppRel { func, args })), extra.span()));

		let abs = expr.clone().then_ignore(just(Token::AbsOp)).repeated().foldr_with(expr.clone(), |item, acc, extra| {
			Spanned::new(extra.state().links.add(AST::Func(FuncRel { arg: acc, body: item })), extra.span())
		}); */
		
		item
		/* set.or(list) *//* .or(app.or(abs)) */
	})
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

#[test]
fn test_parse() {
	let links = &LinkArena::new();
	let mut state = ParserState::new(links);

	let src = "let thing := ";
	let (ast, errs) = parse(src, &mut state);
	println!("{ast:?}");
	fancy_print_errors(errs, src);

	panic!("check output");

	/* let src = r#"
let thing := {} {} -> ({} -> {

})
	"#; */
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