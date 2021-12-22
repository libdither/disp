use std::{iter::Peekable};

use logos::{Logos, Span};
use thiserror::Error;

use hashdb::Datastore;

use crate::lambda_calculus::{Application, Lambda, Variable, typed::Hashtype};

use super::{Expr, LambdaPointer};

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
	#[regex("[a-zA-Z]+", |lex| lex.slice().to_owned())]
	Text(String),

	/* #[regex("[0-9]+", |lex| lex.slice().parse())]
	Number(u64), */
	
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
	InvalidToken(Location<'a>, Span, Token)
}

#[derive(Debug)]
pub struct Location<'a> {
	string: &'a str,
	char_position: usize,
}

impl<'a> Location<'a> {
	fn display_span(&self, span: &Span) -> String {
		let string = self.string;
		// Count continuation bytes
		let num_continuation_bytes = string.bytes().filter(|&b|(b & 0b11000000) == 0b10000000).count();
		let char_count = string.chars().count();
		let byte_count = string.bytes().count();
		println!("{}, {}, {}", num_continuation_bytes, char_count, byte_count);
		let whitespace_amount = span.start + 1 - num_continuation_bytes;
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
	pub fn expect_next(&mut self, token: Token) -> Result<(), ParseError<'a>> {
		let (next, span) = self.next()?;
		if next != token { Err(ParseError::WrongToken(self.location(), span, token, next)) } else { Ok(()) }
	}
	pub fn peek(&mut self) -> Result<&Token, ParseError<'a>> {
		let location = self.location();
		if let Some((token, _)) = self.iter.peek() {
			Ok(token)
		} else {
			Err(ParseError::UnexpectedEndOfStream(location))
		}
	}
}

pub fn parse_to_expr<'a>(string: &'a str, db: &mut Datastore) -> Result<Expr, ParseError<'a>> {
	let feeder = &mut TokenFeeder::from_string(string);
	parse_tokens(feeder, 0, db)
}

fn parse_lambda_pointer<'a>(feeder: &mut TokenFeeder<'a>, db: &mut Datastore) -> Result<Option<LambdaPointer>, ParseError<'a>> {
	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		Token::Period => Some(LambdaPointer::End),
		Token::OpenCarat => {
			parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Left(p.to_hash(db)))
		},
		Token::CloseCarat => {
			parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Right(p.to_hash(db)))
		},
		Token::OpenParen => {
			feeder.expect_next(Token::OpenSquareBracket)?;
			let first = parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Right(p.to_hash(db)));
			feeder.expect_next(Token::Comma)?;
			let second = parse_lambda_pointer(feeder, db)?.map(|p|LambdaPointer::Right(p.to_hash(db)));
			let ret = first.zip(second).map(|(left, right)|LambdaPointer::Both(left.to_hash(db), right.to_hash(db)));
			feeder.expect_next(Token::CloseSquareBracket)?;
			ret
		},
		_ => Err(ParseError::UnexpectedToken(feeder.location(), next_span, next_token))?
	})
	
}

fn parse_tokens<'a>(feeder: &mut TokenFeeder<'a>, depth: usize, db: &mut Datastore) -> Result<Expr, ParseError<'a>> {
	let depth = depth + 1;

	let (next_token, next_span) = feeder.next()?;
	Ok(match next_token {
		// OpenParen represents new s-expression, should cause recursion
		Token::OpenParen => {
			let ret: Expr;
			if let Token::Lambda = feeder.peek()? {
				ret = parse_tokens(feeder, depth, db)?
			} else {
				let expr_1 = parse_tokens(feeder, depth, db)?;

				let expr_2 = parse_tokens(feeder, depth, db)?;

				ret = Expr::App(Application { function: expr_1.store(db), substitution: expr_2.store(db) })
			}
			feeder.expect_next(Token::CloseParen)?;
			
			ret
		},
		// There shouldn't be any `()` in any expressions passed to parse_tokens
		Token::CloseParen => {
			return Err(ParseError::UnexpectedToken(feeder.location(), next_span, Token::CloseParen))
		},
		Token::Variable => {
			Expr::Var(Variable)
		},
		Token::Lambda => {
			let pointers = parse_lambda_pointer(feeder, db)?.map(|p|p.to_hash(db));
			let arg_expr = parse_tokens(feeder, depth, db)?.to_hash(db);
			
			Expr::Lam(Lambda { pointers, expr: arg_expr } )
		},
		Token::Error => { return Err(ParseError::InvalidToken(feeder.location(), next_span, next_token)) }
		_ => unreachable!()
	})
}