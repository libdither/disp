use std::iter::Peekable;

use bitvec::prelude::*;
use logos::{Logos, Span};
use thiserror::Error;

use hashdb::Datastore;

use super::Expr;

fn parse_bool_list(string: &str) -> Vec<BitVec> {
	let string = &string[3..string.len() - 1];

	string.split(",").into_iter().filter_map(|s| {
		let bvec = s.chars().map(|c|c == '1').collect::<BitVec>();
		if bvec.is_empty() { None } else { Some(bvec) } // Make sure if there is nothing in bvec, nothing is returned
	}).collect::<Vec<BitVec>>()
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
	// Tokens can be literal strings, of any length.
	#[regex(r"λ\[(\d+(?:,\d+)*)?\]", |lex| parse_bool_list(lex.slice()) )]
	Lambda(Vec<BitVec>),

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

fn display_span<'a>(string: &'a str, span: &Span) -> String {
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

#[derive(Debug, Error)]
pub enum ParseError<'a> {
	#[error("Found Token {2:?}, expected Token {3:?}\n{}", display_span(.0, .1))]
	WrongToken(&'a str, Span, Token, Token),

	#[error("Unexpected token: {2:?}\n{}", display_span(.0, .1))]
	UnexpectedToken(&'a str, Span, Token),

	#[error("Unexpected end of stream")]
	UnexpectedEndOfStream(&'a str, usize),

	#[error("Invalid Token")]
	InvalidToken(&'a str, Span, Token)
}

pub fn parse_to_expr<'a>(string: &'a str, db: &mut Datastore) -> Result<Expr, ParseError<'a>> {
	let lex = Token::lexer(string);
	parse_tokens(string, &mut Iterator::peekable(lex.spanned()), 0, db)
}

fn parse_tokens<'a>(string: &'a str, iter: &mut Peekable<impl Iterator<Item = (Token, Span)>>, depth: usize, db: &mut Datastore) -> Result<Expr, ParseError<'a>> {
	let (next_token, next_span) = iter.next().ok_or(ParseError::UnexpectedEndOfStream(string, 0))?;
	Ok(match next_token {
		// OpenParen represents new s-expression, should cause recursion
		Token::OpenParen => {
			let (peek_token, _) = iter.peek().ok_or(ParseError::UnexpectedEndOfStream(string, next_span.end))?;
			
			let ret: Expr;
			if let Token::Lambda(_) = peek_token {
				ret = parse_tokens(string, iter, depth + 1, db)?
			} else {
				let expr_1 = parse_tokens(string, iter, depth + 1, db)?;

				let expr_2 = parse_tokens(string, iter, depth + 1, db)?;

				ret = Expr::Application(expr_1.to_hash(db), expr_2.to_hash(db))
			}
			
			// Expect Closing Parenthesis
			let (close_paren, span) = iter.next().ok_or(ParseError::UnexpectedEndOfStream(string, 0))?;
			if close_paren != Token::CloseParen { return Err(ParseError::WrongToken(string, span, close_paren, Token::CloseParen)) }
			
			ret
		},
		// There shouldn't be any `()` in any expressions passed to parse_tokens
		Token::CloseParen => {
			return Err(ParseError::UnexpectedToken(string, next_span, Token::CloseParen))
		},
		Token::Variable => {
			Expr::Variable
		},
		Token::Lambda(pointers) => {
			let arg_expr = parse_tokens(string, iter, depth, db)?;
			
			Expr::Lambda(arg_expr.to_hash(db), pointers)	
		},
		Token::Error => { return Err(ParseError::InvalidToken(string, next_span, next_token)) }
		_ => unreachable!()
	})
}