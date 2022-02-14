use std::fs;
use std::{iter::Peekable};

use logos::{Logos, Span};
use thiserror::Error;
use hashdb::{Datastore, DatastoreError, NativeHashtype, TypedHash};

use crate::lambda_calculus::{Expr, LambdaError, ReduceArena, ReplaceIndex, ReplaceTree, beta_reduce};
use crate::Symbol;

#[derive(Logos, Debug, PartialEq)]
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
/* 
	// Or regular expressions.
	#[regex("[a-zA-Z][a-zA-Z0-9]+")]
	Text, */

	#[regex("[a-zA-Z-_][a-zA-Z0-9]*")]
	Symbol,

	#[regex("\"([a-zA-Z]*)\"")]
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


#[derive(Debug, Error)]
pub enum ParseError<'a> {
	#[error("Found {found:?}, expected Token(s) {expected:?}\n{}", .loc.display_span(.span))]
	WrongToken { loc: Location<'a>, span: Span, found: Token, expected: Vec<Token> },

	#[error("Unexpected token: {2:?}\n{}", .0.display_span(.1))]
	UnexpectedToken(Location<'a>, Span, Token),

	#[error("Unexpected end of stream")]
	UnexpectedEndOfStream(Location<'a>),

	#[error("Invalid Token: {2:?}\n{}", .0.display_span(.1))]
	InvalidToken(Location<'a>, Span, Token),

	#[error("Unknown Symbol: {2}\n{}", .0.display_span(.1))]
	UnknownSymbol(Location<'a>, Span, &'a str),

	#[error("Number-Encoding functions undefined, must define succ and zero functions\n{}", .0.display_span(.1))]
	NoNumberFunctions(Location<'a>, Span),

	#[error("Expected end of stream, found token: {1:?}\n{}", .0.display_span(.2))]
	ExpectedEndOfStream(Location<'a>, Token, Span),

	#[error("Command Error: {0}: {1}")]
	SubcommandError(&'static str, String),

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
		let string = self.string;
		let span_len = span.len();

		let whitespace_amount = self.char_position - span_len;
		let span_marking = std::iter::repeat(" ").take(whitespace_amount)
			.chain(std::iter::repeat("^").take(span.len()));
		format!("{}\n{}", string, span_marking.collect::<String>())
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
	pub fn expect_end(&mut self) -> Result<(), ParseError<'a>> {
		if let Some((token, span)) = self.iter.next() {
			Err(ParseError::ExpectedEndOfStream(self.location(), token, span))
		} else { Ok(()) }
	}
	pub fn peek(&mut self) -> Result<(&Token, &Span), ParseError<'a>> {
		let location = self.location();
		if let Some((token, span)) = self.iter.peek() {
			Ok((token, span))
		} else {
			Err(ParseError::UnexpectedEndOfStream(location))
		}
	}
}

pub fn parse_expr<'a>(string: &'a str, db: &mut Datastore) -> Result<TypedHash<Expr>, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(string);
	parse_tokens(feeder, 0, db)
}

fn parse_lambda_pointer<'a, 'b>(feeder: &mut TokenFeeder<'a>, arena: &'b ReduceArena<'b>, max_level: usize, db: &mut Datastore) -> Result<&'b ReplaceTree<'b>, ParseError<'a>> {
	if let Token::CloseSquareBracket = feeder.peek()?.0 { return Ok(arena.none()) }
	
	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		Token::Number(num) => {
			arena.end(num as usize + 1)
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

fn lookup_expr(string: &str, db: &mut Datastore) -> Result<TypedHash<Expr>, DatastoreError> {
	let symbol: TypedHash<Symbol> = db.lookup_typed(&string.to_owned().hash())?;
	Ok(symbol.fetch(db)?.expr())
}

fn parse_tokens<'a>(feeder: &mut TokenFeeder<'a>, depth: usize, db: &mut Datastore) -> Result<TypedHash<Expr>, ParseError<'a>> {
	let depth = depth + 1;

	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		// OpenParen represents new s-expression, should cause recursion
		Token::OpenParen => {
			let ret = match feeder.peek()?.0 {
				Token::Lambda => { parse_tokens(feeder, depth, db)? }
				Token::Symbol => { parse_tokens(feeder, depth, db)? }
				_ => {
					let expr_1 = parse_tokens(feeder, depth, db)?;
	
					let expr_2 = parse_tokens(feeder, depth, db)?;
	
					Expr::Application { func: expr_1, sub: expr_2 }.store(db)
				}
			};
			feeder.expect_next(Token::CloseParen)?;
			if depth == 1 { feeder.expect_end()? }
			
			ret
		},
		// There shouldn't be any `()` in any expressions passed to parse_tokens
		Token::CloseParen => {
			return Err(ParseError::UnexpectedToken(feeder.location(), next_span, Token::CloseParen))
		},
		Token::Variable => {
			Expr::Variable.store(db)
		},
		Token::Lambda => {
			let (max_level, span, _loc) = match feeder.next()? {
				(Token::Number(val), span) => {
					let loc = feeder.location();
					feeder.expect_next(Token::OpenSquareBracket)?;
					(val as usize + 1, span, loc)
				},
				(Token::OpenSquareBracket, span) => {
					let loc = feeder.location();
					(1, span, loc)
				},
				(token, span) => Err(ParseError::wrong_token(feeder, span, token, vec![Token::Number(0)]))?,
			};
			
			let arena = ReduceArena::new();
			let tree = parse_lambda_pointer(feeder, &arena, max_level, db)?;
			let mut index = ReplaceIndex::new(max_level, tree);

			feeder.expect_next(Token::CloseSquareBracket)?;
		
			let arg_expr = parse_tokens(feeder, depth, db)?;

			arena.pop_lambda(&mut index, arg_expr, db)
			.map_err(|_|ParseError::LocalError(feeder.location(), span, "ReplaceTree failed to expand to lambda expression"))?
		},
		Token::Symbol => {
			let string = &feeder.string[next_span.clone()];

			// println!("Depth: {}, Passed string: {:?}", depth, string);
			if depth == 2 {
				if let Some(ret) = match string {
					"set" => {
						let span = feeder.expect_next(Token::Symbol)?;
						let name = &feeder.string[span];

						let next_expr = parse_tokens(feeder, depth, db)?;
						let reduced = beta_reduce(&next_expr, db).map_err(|_|ParseError::SubcommandError("set", "failed to beta reduce expr".into()))?;
						
						Symbol::new(name, &reduced, db);

						Some(reduced)
					}
					"save" => {
						let location = feeder.expect_next(Token::String)?;
						let location = &feeder.string[location];
						let location = &location[1..location.len()-1];
						println!("Saving to: {:?}", location);
						db.save(fs::File::create(location)?)?;
						Some(Expr::var(db))
					}
					"load" => {
						let location = feeder.expect_next(Token::String)?;
						let location = &feeder.string[location];
						let location = &location[1..location.len()-1];
						println!("Loading from: {:?}", location);
						db.load(fs::File::open(location)?)?;
						Some(Expr::var(db))
					}
					"clear" => {
						db.clear();
						Some(Expr::var(db))
					}
					_ => { None },
				} { return Ok(ret) }
			}
			
			// Assume application
			let func = lookup_expr(string, db).map_err(|_|ParseError::UnknownSymbol(feeder.location(), next_span.clone(), string))?;
			match feeder.peek() {
				Ok((token, _)) => {
					if let Token::CloseParen = token { func }
					else { Expr::Application { func, sub: parse_tokens(feeder, depth, db)? }.store(db) }
				},
				Err(_) => func,
			}
		},
		Token::Number(value) => {
			if value > 1_000_000 { ParseError::SubcommandError("{integer}", "number is too large (must be less than 1,000,000)".into()); }
			let zero = lookup_expr("zero", db).map_err(|_|ParseError::NoNumberFunctions(feeder.location(), next_span.clone()))?;
			let succ = lookup_expr("succ", db).map_err(|_|ParseError::NoNumberFunctions(feeder.location(), next_span))?;
			let mut acc = zero;
			for _ in 0..value {
				acc = Expr::app(&succ, &acc, db)
			}
			acc
		},
		Token::Error => { return Err(ParseError::InvalidToken(feeder.location(), next_span, next_token)) },
		token => return Err(ParseError::UnexpectedToken(feeder.location(), next_span, token)),
	})
}

pub fn parse_reduce<'a>(string: &'a str, db: &mut Datastore) -> Result<TypedHash<Expr>, LambdaError> {
	let expr = parse_expr(&string, db).expect("Failed to parse expression");
	Ok(beta_reduce(&expr, db)?)
}