use std::fs;
use std::{iter::Peekable};

use logos::{Logos, Span};
use thiserror::Error;
use hashdb::{Datastore, DatastoreError, HashSerializer, Link, NativeHashtype, TypedHash};

use crate::lambda_calculus::{Expr, LambdaError, ReduceArena, ReplaceIndex, ReplaceTree, beta_reduce};
use crate::Symbol;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
	// Tokens can be literal strings, of any length.
	#[token("λ")]
	Lambda,

	#[token("[")]
	OpenSquareBracket,

	#[token("]")]
	CloseSquareBracket,

	#[token(".")]
	Period,

	#[token(",")]
	Comma,

	#[token("<")]
	OpenCarat,

	#[token(">")]
	CloseCarat,

	#[token("(")]
	OpenParen,

	#[token(")")]
	CloseParen,

	#[token("x")]
	Variable,

	#[regex("[a-zA-Z-_][a-zA-Z0-9]*")]
	Symbol,

	#[regex(r#""([^"\\]|\\.)*""#)]
	String,

	#[regex("[0-9]+", |lex| lex.slice().parse())]
	Number(u64),
	
	// We can also use this variant to define whitespace,
	// or any other matches we wish to skip.
	#[regex(r"[ \t\n\f]+", logos::skip)]
	Space,

	// Logos requires one token variant to handle errors,
	// it can be named anything you wish.
	#[error]
	Error
}


#[derive(Debug, Error,)]
pub enum ParseError<'a> {
	#[error("Found {found:?}, expected Token(s) {expected:?}\n{}", .loc.display_span(.span))]
	WrongToken { loc: Location<'a>, span: Span, found: Token, expected: Vec<Token> },

	#[error("Unexpected token: {2:?}\n{}", .0.display_span(.1))]
	UnexpectedToken(Location<'a>, Span, Token),

	#[error("Lexer Error: {2:?}\n{}",.0.display_span(.1))]
	LexerError(Location<'a>, Span, Token),

	#[error("Unexpected end of stream:\n{}", .0.display(1))]
	UnexpectedEndOfStream(Location<'a>),

	#[error("Unknown Symbol: {1}\n{}", .0.display_str(.1))]
	UnknownSymbol(Location<'a>, &'a str),

	#[error("Number-Encoding functions undefined, must define succ and zero functions\n{}", .0.display_span(.1))]
	NoNumberFunctions(Location<'a>, Span),

	#[error("Command Error: {0}: {1}")]
	CommandError(&'static str, String),

	#[error("Datastore Error: {0}")]
	DatastoreError(#[from] DatastoreError),

	#[error("I/O Error: {0}")]
	IOError(#[from] std::io::Error),

	#[error("Error: {2}\n{}", .0.display_span(.1))]
	LocalError(Location<'a>, Span, &'static str),
}
impl<'a> ParseError<'a> {
	fn wrong_token(feeder: &mut TokenFeeder<'a>, span: Span, found: Token, expected: Vec<Token>) -> Self {
		ParseError::WrongToken { loc: feeder.location(), span, found, expected }
	}
}

#[derive(Debug)]
pub struct Location<'a> {
	string: &'a str,
	char_position: usize,
}

impl<'a> Location<'a> {
	fn display_span(&self, span: &Span) -> String {
		self.display(span.len())
	}
	fn display_str(&self, string: &'a str) -> String {
		self.display(string.len())
	}
	fn display(&self, len: usize) -> String {
		let whitespace_amount = self.char_position.saturating_sub(len);
		let span_marking = std::iter::repeat(" ").take(whitespace_amount)
			.chain(std::iter::repeat("^").take(len));
		format!("{}\n{}", self.string, span_marking.collect::<String>())
	}
}

struct TokenFeeder<'a> {
	string: &'a str,
	iter: Peekable<logos::SpannedIter<'a, Token>>,
	char_position: usize,
}
impl<'a> TokenFeeder<'a> {
	pub fn from_string(string: &'a str) -> Self {
		let lexer = Token::lexer(string);
		Self {
			string,
			iter: Iterator::peekable(lexer.spanned()),
			char_position: 0,
		}
	}
	pub fn location(&self) -> Location<'a> { Location { string: self.string, char_position: self.char_position } }
	pub fn next(&mut self) -> Result<(Token, Span), ParseError<'a>> {
		if let Some((token, span)) = Iterator::next(&mut self.iter) {
			self.char_position += span.len();
			Ok((token, span))
		} else {
			Err(ParseError::UnexpectedEndOfStream(self.location()))
		}
	}
	pub fn expect_next(&mut self, token: Token) -> Result<Span, ParseError<'a>> {
		let (next, span) = self.next()?;
		if next != token { Err(ParseError::wrong_token(self, span, next, vec![token])) } else { Ok(span) }
	}
	pub fn peek(&mut self) -> Result<(&Token, &Span), ParseError<'a>> {
		let location = self.location();
		if let Some((token, span)) = self.iter.peek() {
			Ok((token, span))
		} else {
			Err(ParseError::UnexpectedEndOfStream(location))
		}
	}
	pub fn test_end_of_expression(&mut self, depth: usize) -> bool {
		match self.iter.peek() {
			Some((Token::CloseParen, _)) => {
				true
			},
			None if depth == 0 => true, // Treat end of stream as end of expression if there are no enclosing expressions
			_ => false,
		}
	}
}

fn parse_lambda_pointer<'a, 'b>(feeder: &mut TokenFeeder<'a>, arena: &'b ReduceArena<'b>, max_level: usize, db: &mut Datastore) -> Result<&'b ReplaceTree<'b>, ParseError<'a>> {
	if let Token::CloseSquareBracket = feeder.peek()?.0 { return Ok(arena.none()) }
	
	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		Token::Number(num) => {
			arena.end(num as usize)
		},
		Token::Symbol if &feeder.string[next_span.clone()] == "N" => { arena.none() }
		Token::Period => arena.end(max_level),
		Token::OpenCarat => {
			arena.left(parse_lambda_pointer(feeder, arena, max_level, db)?)
		},
		Token::CloseCarat => {
			arena.right(parse_lambda_pointer(feeder, arena, max_level, db)?)
		},
		Token::OpenParen => {
			let left = parse_lambda_pointer(feeder, arena, max_level, db)?;
			feeder.expect_next(Token::Comma)?;
			let right = parse_lambda_pointer(feeder, arena, max_level, db)?;
			feeder.expect_next(Token::CloseParen)?;
			arena.join(left, right)
		},
		_ => Err(ParseError::UnexpectedToken(feeder.location(), next_span, next_token))?
	})
	
}

fn lookup_expr(string: &str, db: &mut Datastore) -> Result<Link<Expr>, DatastoreError> {
	let symbol: TypedHash<Symbol> = db.lookup_typed(&string.to_owned().store(&mut db.serializer()))?;
	Ok(symbol.fetch(db)?.expr)
}
fn lookup_symbol<'a>(feeder: &mut TokenFeeder<'a>, span: Span, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let string = &feeder.string[span.clone()];
	lookup_expr(string, db).map_err(|_|ParseError::UnknownSymbol(feeder.location(), string))
}

// Parse Token stream until reach closing parentheses or if depth == 0 & end of stream and return Application expression
fn parse_application<'a>(feeder: &mut TokenFeeder<'a>, initial: Link<Expr>, depth: usize, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let func = initial;
	Ok(if !feeder.test_end_of_expression(depth) {
		let sub = parse_token(feeder, depth, db)?.2;
		parse_application(feeder, Expr::Application { func, sub }.store(db), depth, db)?
	} else { func })
}

// Parse Token stream until reach parentheses or end of stream and return expression, it will not consume the ending parenthses
fn parse_token<'a>(feeder: &mut TokenFeeder<'a>, depth: usize, db: &mut Datastore) -> Result<(Token, Span, Link<Expr>), ParseError<'a>> {
	let (next_token, next_span) = feeder.next()?;
	let (token, span) = (next_token.clone(), next_span.clone());

	let expr = match next_token {
		Token::Lambda => {
			// Read PointerTree metadata
			let (max_level, span, _loc) = match feeder.next()? {
				(Token::Number(val), span) => {
					let loc = feeder.location();
					feeder.expect_next(Token::OpenSquareBracket)?;
					(val as usize, span, loc)
				},
				(Token::OpenSquareBracket, span) => {
					let loc = feeder.location();
					(1, span, loc)
				},
				(token, span) => Err(ParseError::wrong_token(feeder, span, token, vec![Token::Number(0)]))?,
			};
			
			// Parse Pointertree expression
			let arena = ReduceArena::new();
			let tree = parse_lambda_pointer(feeder, &arena, max_level, db)?;
			let mut index = ReplaceIndex::new(max_level, tree);
			feeder.expect_next(Token::CloseSquareBracket)?;
			
			// Treat rest of lambda as expression of the same depth (no parentheses required), this allows for same-line lambdas
			let arg_expr = parse_token(feeder, depth, db)?.2;
			let arg_expr = parse_application(feeder, arg_expr, depth, db)?;

			arena.pop_lambda(&mut index, arg_expr, db)
			.map_err(|_|ParseError::LocalError(feeder.location(), span, "ReplaceTree failed to expand to lambda expression"))?
		}
		Token::Symbol => lookup_symbol(feeder, next_span, db)?,
		Token::Variable => Expr::var(db),
		Token::Number(value) => {
			if value > 1_000_000 { ParseError::CommandError("{integer}", "number is too large (must be less than 1,000,000)".into()); }
			let zero = lookup_expr("zero", db).map_err(|_|ParseError::NoNumberFunctions(feeder.location(), next_span.clone()))?;
			let succ = lookup_expr("succ", db).map_err(|_|ParseError::NoNumberFunctions(feeder.location(), next_span))?;
			let mut ret = zero;
			for _ in 0..value {
				ret = Expr::app(&succ, &ret, db)
			}
			ret
		}
		Token::OpenParen => { // Parse Subexpression
			let ret = parse_expr(feeder, depth + 1, db)?;
			feeder.expect_next(Token::CloseParen)?;
			ret
		},
		Token::Error => { return Err(ParseError::LexerError(feeder.location(), next_span, next_token.clone())) },
		_ => Err(ParseError::UnexpectedToken(feeder.location(), next_span, next_token.clone()))?
	};
	Ok((token, span, expr))
}

fn parse_expr<'a>(feeder: &mut TokenFeeder<'a>, depth: usize, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let (token, span, expr) = parse_token(feeder, depth, db)?;
	let ret = match token {
		Token::Symbol | Token::Variable | Token::Number(_) | Token::OpenParen => parse_application(feeder, expr, depth, db)?,
		Token::Lambda => expr,
		_ => Err(ParseError::UnexpectedToken(feeder.location(), span, token))?
	};
	Ok(ret)
}


fn parse_command<'a>(feeder: &'a mut TokenFeeder, db: &mut Datastore) -> Result<Option<Link<Expr>>, ParseError<'a>> {
	let span = feeder.expect_next(Token::Symbol)?;
	let string = &feeder.string[span.clone()];
	Ok(match string {
		"set" => {
			let span = feeder.expect_next(Token::Symbol)?;
			let name = &feeder.string[span];

			let next_expr = parse_expr(feeder, 0, db)?;
			let reduced = match beta_reduce(&next_expr, db) {
				Ok(ret) => ret,
				Err(LambdaError::RecursionDepthExceeded) => { println!("warning: this expresion will run for a long time"); next_expr },
				Err(err) => Err(ParseError::CommandError("set", format!("failed to beta reduce expr: {}", err)))?
			};
			
			Symbol::new(name, &reduced, db);

			Some(reduced)
		}
		"save" => {
			let location = feeder.expect_next(Token::String)?;
			let location = &feeder.string[location];
			let location = &location[1..location.len()-1];
			println!("Saving to: {:?}", location);
			db.save(fs::File::create(location)?)?;
			None
		}
		"load" => {
			let location = feeder.expect_next(Token::String)?;
			let location = &feeder.string[location];
			let location = &location[1..location.len()-1];
			println!("Loading from: {:?}", location);
			db.load(fs::File::open(location)?)?;
			None
		}
		"clear" => {
			db.clear();
			None
		}
		_ => {
			let expr = lookup_symbol(feeder, span, db)?;
			Some(parse_application(feeder, expr, 0, db)?)
		},
	})
}

pub fn parse<'a>(string: &'a str, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(string);
	println!("parsing: {}", string);
	let expr = parse_expr(feeder, 0, db)?;
	println!("parsed: {}", crate::lambda_calculus::DisplayWithDatastore::display(&expr, db));
	Ok(expr)
}

pub fn parse_reduce<'a>(string: &'a str, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(string);
	let expr = parse_expr(feeder, 0, db)?;
	let expr = beta_reduce(&expr, db).unwrap_or(expr);
	Ok(expr)
}

pub fn parse_line<'a>(line: &'a str, db: &mut Datastore) -> Result<Option<Link<Expr>>, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(line);
	let (next, _) = feeder.peek()?;
	Ok(if Token::Symbol == *next {
		match parse_command(feeder, db) {
			Err(err) => {println!("Failed to parse command: {}", err); None}
			Ok(expr) => expr,
		}
	} else {
		Some(parse_expr(feeder, 0, db)?)
	})
}