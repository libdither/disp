use std::{iter::Peekable};

use logos::{Logos, Span};
use thiserror::Error;
use hashdb::{Datastore, Hashtype, HashtypeResolveError, Link, TypedHash};

use crate::lambda_calculus::{Expr, LambdaPointer, beta_reduce, LambdaError};
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

	// Or regular expressions.
	#[regex("[a-zA-Z]+")]
	Text,

	/* #[regex("\"[a-zA-Z]+\"")]
	String, */

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
	#[error("Found Token {2:?}, expected Token {3:?}\n{}", .0.display_span(.1))]
	WrongToken(Location<'a>, Span, Token, Token),

	#[error("Unexpected token: {2:?}\n{}", .0.display_span(.1))]
	UnexpectedToken(Location<'a>, Span, Token),

	#[error("Unexpected end of stream")]
	UnexpectedEndOfStream(Location<'a>),

	#[error("Invalid Token")]
	InvalidToken(Location<'a>, Span, Token),

	#[error("Unknown Symbol: {2}\n{}", .0.display_span(.1))]
	UnknownSymbol(Location<'a>, Span, &'a str),

	#[error("Number-Encoding functions undefined, must define succ and zero functions\n{}", .0.display_span(.1))]
	NoNumberFunctions(Location<'a>, Span),

	#[error("Expected end of stream, found token: {1:?}\n{}", .0.display_span(.2))]
	ExpectedEndOfStream(Location<'a>, Token, Span),

	#[error("command: {0}: {1}")]
	SubcommandError(&'static str, String)
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
		if next != token { Err(ParseError::WrongToken(self.location(), span, token, next)) } else { Ok(span) }
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

pub fn parse_to_expr<'a>(string: &'a str, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(string);
	parse_tokens(feeder, 0, db)
}

fn parse_lambda_pointer<'a>(feeder: &mut TokenFeeder<'a>, db: &mut Datastore) -> Result<Option<Link<LambdaPointer>>, ParseError<'a>> {
	if let Token::CloseSquareBracket = feeder.peek()?.0 { return Ok(None) }
	
	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		Token::Period => {
			Some(LambdaPointer::End.store(db))
		},
		Token::OpenCarat => {
			parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Left(p).store(db))
		},
		Token::CloseCarat => {
			parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Right(p).store(db))
		},
		Token::OpenParen => {
			let first = parse_lambda_pointer(feeder, db)?;
			feeder.expect_next(Token::Comma)?;
			let second = parse_lambda_pointer(feeder, db)?;
			feeder.expect_next(Token::CloseParen)?;

			first.zip(second).map(|(left, right)|LambdaPointer::Both(left, right).store(db))
		},
		_ => Err(ParseError::UnexpectedToken(feeder.location(), next_span, next_token))?
	})
	
}

fn lookup_expr(string: &str, db: &mut Datastore) -> Option<Link<Expr>> {
	let symbol: Link<Symbol> = db.lookup(&string.to_owned().hash()).ok()?;
	Some(symbol.expr())
}

fn parse_tokens<'a>(feeder: &mut TokenFeeder<'a>, depth: usize, db: &mut Datastore) -> Result<Link<Expr>, ParseError<'a>> {
	let depth = depth + 1;

	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		// OpenParen represents new s-expression, should cause recursion
		Token::OpenParen => {
			let ret = match feeder.peek()?.0 {
				Token::Lambda => { parse_tokens(feeder, depth, db)? }
				Token::Text => { parse_tokens(feeder, depth, db)? }
				_ => {
					let expr_1 = parse_tokens(feeder, depth, db)?;
	
					let expr_2 = parse_tokens(feeder, depth, db)?;
	
					Expr::Application { function: expr_1, substitution: expr_2 }.store(db)
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
			feeder.expect_next(Token::OpenSquareBracket)?;
			let pointers = parse_lambda_pointer(feeder, db)?;
			feeder.expect_next(Token::CloseSquareBracket)?;

			let arg_expr = parse_tokens(feeder, depth, db)?;
			
			Expr::Lambda { pointers, expr: arg_expr }.store(db)
		},
		Token::Text => {
			let string = &feeder.string[next_span.clone()];

			if depth == 2 {
				match string {
					"set" => {
						let span = feeder.expect_next(Token::Text)?;
						let name = &feeder.string[span];

						let next_expr = parse_tokens(feeder, depth, db)?;
						let reduced = beta_reduce(&next_expr, db).map_err(|_|ParseError::SubcommandError("set", "failed to beta reduce expr".into()))?;
						
						Symbol::new(name, &reduced, db);
						return Ok(reduced);
					}
					_ => {},
				}
			}
			
			// Assume application
			let function = lookup_expr(string, db).ok_or(ParseError::UnknownSymbol(feeder.location(), next_span.clone(), string))?;
			match feeder.peek() {
				Ok((token, _)) => {
					if let Token::CloseParen = token { function }
					else { Expr::Application { function, substitution: parse_tokens(feeder, depth, db)? }.store(db) }
				},
				Err(_) => function,
			}
		},
		Token::Number(value) => {
			if value > 1_000_000 { ParseError::SubcommandError("{integer}", "number is too large (must be less than 1,000,000)".into()); }
			let zero = lookup_expr("zero", db).ok_or(ParseError::NoNumberFunctions(feeder.location(), next_span.clone()))?;
			let succ = lookup_expr("succ", db).ok_or(ParseError::NoNumberFunctions(feeder.location(), next_span))?;
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

pub fn parse_reduce<'a>(string: &'a str, db: &mut Datastore) -> Result<Link<Expr>, LambdaError> {
	let expr = parse_to_expr(&string, db).expect("Failed to parse expression");
	Ok(beta_reduce(&expr, db)?)
}