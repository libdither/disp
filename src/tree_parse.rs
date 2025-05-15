//! Some AI garbage to make a simpler parser for the tree eval thingy.
#![allow(unused, non_snake_case)]
use core::fmt;

use winnow::{
	combinator::{alt, delimited, opt, repeat},
	error::{ContextError, ParseError, StrContext, StrContextValue}, // Corrected: ErrMode not directly used by basic parsers here
	prelude::*,
	token::{any, one_of},
	// stream::Stream, // Not strictly needed for this example's direct &[Token] usage
};

pub fn parse_line(input: &mut &[Token]) -> PResult<(Option<String>, TreeExpr)> {
	// Attempt to parse an identifier followed by an assignment operator
	let assignment_part = opt(winnow::combinator::separated_pair(
		ident_string_parser.context(StrContext::Label("identifier")),
		one_of(|t| matches!(t, Token::AssignOp)).context(StrContext::Expected(StrContextValue::StringLiteral(":="))),
		expr_parser.context(StrContext::Label("expression")),
	))
	.parse_next(input)?;

	match assignment_part {
		Some((ident, expr)) => Ok((Some(ident), expr)),
		None => {
			// If no assignment, try to parse just an expression
			let expr = expr_parser.parse_next(input)?;
			Ok((None, expr))
		}
	}
}

// --- Token Definition (Simplified) ---
#[derive(Clone, PartialEq, Debug)]
pub enum Token {
	Ident(String),
	AssignOp, // :=
	KeywordT, // Represents the literal 't'
	OpenParen,
	ClosedParen,
}

// --- Lexer (Simplified) ---
// A basic lexer to convert an input string into a Vec<Token>
pub fn lexer(input: &str) -> Result<Vec<Token>, String> {
	let mut tokens = Vec::new();
	let mut chars = input.chars().peekable();

	while let Some(&c) = chars.peek() {
		match c {
			'(' => {
				tokens.push(Token::OpenParen);
				chars.next();
			}
			')' => {
				tokens.push(Token::ClosedParen);
				chars.next();
			}
			't' => {
				chars.next(); // Consume 't'
				  // If 't' is followed by another alphanumeric char, it's an ident (e.g., "tvar").
				  // Otherwise, it's the keyword 't'.
				if chars.peek().map_or(true, |&next_char| !next_char.is_alphanumeric()) {
					tokens.push(Token::KeywordT);
				} else {
					let mut ident_str = String::from('t');
					while let Some(&next_char) = chars.peek() {
						if next_char.is_alphanumeric() {
							ident_str.push(next_char);
							chars.next();
						} else {
							break;
						}
					}
					tokens.push(Token::Ident(ident_str));
				}
			}
			c if c.is_alphabetic() => {
				// Identifiers start with an alphabetic character
				let mut ident_str = String::new();
				while let Some(&next_char) = chars.peek() {
					if next_char.is_alphanumeric() {
						// Can continue with alphanumeric characters
						ident_str.push(next_char);
						chars.next();
					} else {
						break;
					}
				}
				tokens.push(Token::Ident(ident_str));
			}
			c if c.is_whitespace() => {
				chars.next(); // Skip whitespace
			}
			':' => {
				chars.next(); // Consume ':'
				if let Some(&'=') = chars.peek() {
					chars.next(); // Consume '='
					tokens.push(Token::AssignOp);
				} else {
					// Handle just ':' if needed, or report error
					return Err(format!("Unexpected character after ':': {:?}", chars.peek()));
				}
			}
			_ => return Err(format!("Unexpected character: {}", c)),
		}
	}
	Ok(tokens)
}

// --- ParseTree Definition ---
#[derive(Clone, PartialEq, Debug)]
pub enum TreeExpr {
	Ident(String),
	Leaf, // Corresponds to the 't' atom
	Stem(Box<TreeExpr>),
	Fork(Box<TreeExpr>, Box<TreeExpr>),
}

impl fmt::Display for TreeExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TreeExpr::Ident(s) => write!(f, "{}", s),
			TreeExpr::Leaf => write!(f, "t"),
			TreeExpr::Stem(expr) => write!(f, "Stem({})", expr), // Display for Stem
			// Parenthesize applications for clarity and to show structure
			TreeExpr::Fork(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
		}
	}
}

// --- Parser Functions ---

// Helper parser to extract the String from a Token::Ident
fn ident_string_parser(input: &mut &[Token]) -> PResult<String> {
	any.verify_map(|token: Token| match token {
		Token::Ident(s) => Some(s),
		_ => None,
	})
	.context(StrContext::Label("identifier string"))
	.parse_next(input)
}

impl winnow::stream::ContainsToken<Token> for Token {
	#[inline(always)]
	fn contains_token(&self, token: Token) -> bool {
		*self == token
	}
}

impl winnow::stream::ContainsToken<Token> for &'_ [Token] {
	#[inline]
	fn contains_token(&self, token: Token) -> bool {
		self.iter().any(|t| *t == token)
	}
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for &'_ [Token; LEN] {
	#[inline]
	fn contains_token(&self, token: Token) -> bool {
		self.iter().any(|t| *t == token)
	}
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for [Token; LEN] {
	#[inline]
	fn contains_token(&self, token: Token) -> bool {
		self.iter().any(|t| *t == token)
	}
}

// Parses an atom `A := <ident> | (E) | 't'`
fn atom_parser(input: &mut &[Token]) -> PResult<TreeExpr> {
	alt((
		// <ident>
		ident_string_parser.map(TreeExpr::Ident),
		// 't'
		one_of(Token::KeywordT).value(TreeExpr::Leaf),
		// (E)
		delimited(
			one_of(Token::OpenParen).context(StrContext::Expected(StrContextValue::CharLiteral('('))),
			expr_parser, // Recursive call to parse the inner expression E
			one_of(Token::ClosedParen).context(StrContext::Expected(StrContextValue::CharLiteral(')'))),
		)
		.map(|inner_expr| {
			// inner_expr is the result of expr_parser on the content within parens
			if let TreeExpr::Fork(_, _) = inner_expr {
				// if fork, resolve as fork
				inner_expr
				// Specifically (t) -> Stem(Leaf)
			} else {
				// otherwise, extra parens means stem
				TreeExpr::Stem(Box::new(inner_expr))
			}
		}),
	))
	.context(StrContext::Label("atom"))
	.parse_next(input)
}

// Parses an expression `E := A | A A A A...` (left-associative)
pub fn expr_parser(input: &mut &[Token]) -> PResult<TreeExpr> {
	repeat(1.., atom_parser)
		.fold(
			|| None::<TreeExpr>, // Initial state: no expression parsed yet
			|acc, item| {
				// If acc is None, this is the first atom.
				// If acc is Some, this is a subsequent atom, forming an application.
				if let Some(current_expr) = acc {
					Some(TreeExpr::Fork(Box::new(current_expr), Box::new(item)))
				} else {
					Some(item)
				}
			},
		)
		// repeat(1..) guarantees at least one item, so fold will always produce Some.
		.map(|opt_expr| opt_expr.expect("repeat(1..) ensures at least one item"))
		.context(StrContext::Label("expression"))
		.parse_next(input)
}

// --- Tests ---
#[cfg(test)]
mod tests {
	use super::*; // For lexer, Token, TreeExpr, parsers
	use winnow::Parser; // For .parse_peek() and .parse() methods on parsers

	// Helper functions to create TreeExpr variants for tests
	fn TEIdent(s: &str) -> TreeExpr {
		TreeExpr::Ident(s.to_string())
	}
	fn TELeaf() -> TreeExpr {
		TreeExpr::Leaf
	}
	fn TEApp(lhs: TreeExpr, rhs: TreeExpr) -> TreeExpr {
		TreeExpr::Fork(Box::new(lhs), Box::new(rhs))
	}
	fn TEStem(expr: TreeExpr) -> TreeExpr {
		TreeExpr::Stem(Box::new(expr))
	}

	#[test]
	fn test_lexer_simple() {
		assert_eq!(lexer("id"), Ok(vec![Token::Ident("id".to_string())]));
		assert_eq!(lexer("t"), Ok(vec![Token::KeywordT]));
		assert_eq!(lexer("test"), Ok(vec![Token::Ident("test".to_string())]));
		assert_eq!(lexer("tt"), Ok(vec![Token::Ident("tt".to_string())]));
		assert_eq!(
			lexer("(id t)"),
			Ok(vec![
				Token::OpenParen,
				Token::Ident("id".to_string()),
				Token::KeywordT,
				Token::ClosedParen,
			])
		);
		assert_eq!(
			lexer("id1 t id2"),
			Ok(vec![
				Token::Ident("id1".to_string()),
				Token::KeywordT,
				Token::Ident("id2".to_string()),
			])
		);
		assert_eq!(lexer(" t "), Ok(vec![Token::KeywordT]));
	}

	#[test]
	fn test_parse_atom_ident() {
		let tokens = lexer("foo bar").unwrap();
		let (remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEIdent("foo"));
		assert_eq!(remaining, &[Token::Ident("bar".to_string())][..]);
	}

	#[test]
	fn test_parse_atom_true() {
		// Parses 't' keyword
		let tokens = lexer("t bar").unwrap();
		let (remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TELeaf());
		assert_eq!(remaining, &[Token::Ident("bar".to_string())][..]);
	}

	#[test]
	fn test_parse_atom_stem_leaf() {
		// Parses '(t)' into Stem(Leaf)
		let tokens = lexer("(t)").unwrap();
		let (_remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEStem(TELeaf()));
	}

	#[test]
	fn test_parse_atom_paren_not_just_t() {
		// Parses '(ident)' into Ident("ident")
		let tokens = lexer("(ident)").unwrap();
		let (_remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEStem(TEIdent("ident")));
	}

	#[test]
	fn test_parse_atom_paren_expr() {
		// (foo t) is an atom A of the form (E).
		// Inner E is "foo t", which parses to App(Ident("foo"), Leaf).
		let tokens = lexer("(foo t)").unwrap();
		let (_remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEApp(TEIdent("foo"), TELeaf()));
	}

	#[test]
	fn test_parse_atom_nested_paren_expr() {
		// ((t) foo)
		// Outer atom is (E1) where E1 is "(t) foo".
		// expr_parser on "(t) foo" will parse "(t)" as an atom, then "foo" as an atom.
		// Atom "(t)" becomes Stem(Leaf). Atom "foo" becomes Ident("foo").
		// E1 becomes App(Stem(Leaf), Ident("foo")).
		// The delimited map gets App(Stem(Leaf), Ident("foo")), which is not Leaf, so it returns it as is.
		let tokens = lexer("((t) foo)").unwrap();
		let (_remaining, parsed) = atom_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEApp(TEStem(TELeaf()), TEIdent("foo")));
	}

	#[test]
	fn test_expr_parser_single_atom() {
		let tokens_id = lexer("myIdent").unwrap();
		assert_eq!(expr_parser.parse(&tokens_id[..]).unwrap(), TEIdent("myIdent"));

		let tokens_t = lexer("t").unwrap();
		assert_eq!(expr_parser.parse(&tokens_t[..]).unwrap(), TELeaf());

		let tokens_paren = lexer("(id)").unwrap(); // (E) where E is "id"
		assert_eq!(expr_parser.parse(&tokens_paren[..]).unwrap(), TEStem(TEIdent("id")));

		let tokens_stem_leaf = lexer("(t)").unwrap(); // (t) -> Stem(Leaf)
		assert_eq!(expr_parser.parse(&tokens_stem_leaf[..]).unwrap(), TEStem(TELeaf()));
	}

	#[test]
	fn test_expr_parser_simple_app() {
		// foo bar -> App(Ident("foo"), Ident("bar"))
		let tokens = lexer("foo bar").unwrap();
		assert_eq!(
			expr_parser.parse(&tokens[..]).unwrap(),
			TEApp(TEIdent("foo"), TEIdent("bar"))
		);
	}

	#[test]
	fn test_expr_parser_left_associativity() {
		// foo bar baz -> App(App(Ident("foo"), Ident("bar")), Ident("baz"))
		let tokens = lexer("foo bar baz").unwrap();
		let expected = TEApp(TEApp(TEIdent("foo"), TEIdent("bar")), TEIdent("baz"));
		assert_eq!(expr_parser.parse(&tokens[..]).unwrap(), expected);
	}

	#[test]
	fn test_expr_parser_mixed_atoms_in_app() {
		// foo (bar t) baz
		// A1 = Ident("foo")
		// A2 = (bar t) -> App(Ident("bar"), Leaf)
		// A3 = Ident("baz")
		// Result: App(App(A1, A2), A3)
		let tokens = lexer("foo (bar t) baz").unwrap();
		let expected = TEApp(TEApp(TEIdent("foo"), TEApp(TEIdent("bar"), TELeaf())), TEIdent("baz"));
		assert_eq!(expr_parser.parse(&tokens[..]).unwrap(), expected);
	}

	#[test]
	fn test_expr_parser_parens_for_grouping_alters_associativity() {
		// foo (bar baz)
		// A1 = Ident("foo")
		// A2 = (bar baz) -> App(Ident("bar"), Ident("baz"))
		// Result: App(A1, A2)
		let tokens = lexer("foo (bar baz)").unwrap();
		let expected = TEApp(TEIdent("foo"), TEApp(TEIdent("bar"), TEIdent("baz")));
		assert_eq!(expr_parser.parse(&tokens[..]).unwrap(), expected);
	}

	#[test]
	fn test_expr_parser_complex_nesting() {
		// ((foo bar) (t id)) t
		// A_outer1 = ((foo bar) (t id))
		//   A_outer1 is (E_outer1)
		//   E_outer1 = (foo bar) (t id) which is Fork(A_inner1, A_inner2)
		//     A_inner1 = (foo bar) -> App(Ident("foo"), Ident("bar"))
		//     A_inner2 = (t id)    -> App(Leaf, Ident("id"))
		//   So, E_outer1 = App(App(Ident("foo"), Ident("bar")), App(Leaf, Ident("id")))
		//   A_outer1 (result of delimited for E_outer1) = E_outer1 because E_outer1 is not Leaf
		// A_outer2 = t -> Leaf
		// Result: App(A_outer1, A_outer2)
		let tokens = lexer("((foo bar) (t id)) t").unwrap();
		let expected = TEApp(
			TEApp(TEApp(TEIdent("foo"), TEIdent("bar")), TEApp(TELeaf(), TEIdent("id"))),
			TELeaf(),
		);
		assert_eq!(expr_parser.parse(&tokens[..]).unwrap(), expected);
	}

	#[test]
	fn test_display_tree_expr() {
		// Test Fork display
		let complex_expr = TEApp(
			TEApp(
				TEApp(TEIdent("foo"), TEIdent("bar")), // (foo bar)
				TEApp(TELeaf(), TEIdent("id")),        // (t id)
			), // ((foo bar) (t id))
			TELeaf(), // t
		); // (((foo bar) (t id)) t)
		assert_eq!(complex_expr.to_string(), "(((foo bar) (t id)) t)");

		let simple_app = TEApp(TEIdent("a"), TEIdent("b"));
		assert_eq!(simple_app.to_string(), "(a b)");

		let left_assoc_app = TEApp(TEApp(TEIdent("a"), TEIdent("b")), TEIdent("c"));
		assert_eq!(left_assoc_app.to_string(), "((a b) c)");

		// Test Stem display
		let stem_leaf = TEStem(TELeaf());
		assert_eq!(stem_leaf.to_string(), "Stem(t)");
		let stem_ident = TEStem(TEIdent("x"));
		assert_eq!(stem_ident.to_string(), "Stem(x)");
	}

	#[test]
	fn test_empty_input_to_expr_parser() {
		let tokens: Vec<Token> = Vec::new();
		let result = expr_parser.parse(&tokens[..]);
		assert!(
			result.is_err(),
			"Parser should fail on empty input because E requires at least one A"
		);
		// Error chain would be: expr_parser -> repeat -> atom_parser
		let err = result.unwrap_err();
		assert!(err.inner().context().any(|c| *c == StrContext::Label("expression")));
	}

	#[test]
	fn test_mismatched_open_paren() {
		// TODO: Fix this test idk why its not giving correct context
		// Input: "(foo"
		let tokens = vec![Token::OpenParen, Token::Ident("foo".to_string())];
		let result = expr_parser.parse(&tokens[..]); // or atom_parser for a more direct test
		assert!(result.is_err(), "Parser should fail due to unclosed parenthesis");

		let err = result.unwrap_err();
		// The error should be "expected ')'" within the "atom" context.
		println!("err: {:?}", err);
		assert!(err
			.inner()
			.context()
			.any(|c| *c == StrContext::Expected(StrContextValue::CharLiteral(')'))));
	}

	#[test]
	fn test_mismatched_close_paren() {
		// Input: "foo)"
		// This should parse "foo" and leave ")" as remaining.
		let tokens = vec![Token::Ident("foo".to_string()), Token::ClosedParen];
		let (remaining, parsed) = expr_parser.parse_peek(&tokens[..]).unwrap();
		assert_eq!(parsed, TEIdent("foo"));
		assert_eq!(remaining, &[Token::ClosedParen][..]);
	}
}
