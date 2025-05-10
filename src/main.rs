#![feature(iterator_try_collect, try_blocks, try_find)]

pub mod eval;
mod lexer;
mod lower;
mod parse;
mod verify;

use std::{env, io};

use winnow::Parser;

fn main() -> io::Result<()> {
	let filename = env::args().nth(1).expect("Usage: disp <PROGRAM>");
	let program = std::fs::read_to_string(filename)?;

	println!("Program:");
	println!("{}", program.as_str());

	// lex string
	println!("--- LEXING ---");
	let lex = match lexer::lexer.parse(&mut program.as_str()) {
		Ok(lex) => lex,
		Err(err) => {
			println!("{}", err.to_string());
			return Ok(());
		}
	};
	println!("lex: {lex:?}");

	// parse tokens
	println!("--- PARSING ---");
	let stmts = match parse::parse_file.parse(&mut lex.as_slice()) {
		Ok(stmts) => stmts,
		Err(err) => {
			println!("{err}");
			return Ok(());
		}
	};
	println!("Parse:");
	for (i, tree) in stmts.iter().enumerate() {
		println!("{i:03} {tree:?}");
		println!("{i:03} {tree}");
	}

	// thing := 3 : Nat
	// takes_ints { i : Int } -> {...}
	// takes_ints{thing}

	// convert parsetree to semantic tree
	/* println!("--- LOWERING ---");
	let mut ctx = lower::Context::new();
	let typed_term = match lower::lower(ParseTree::Set(stmts), &mut ctx) {
		Ok(typed) => typed,
		Err(err) => {
			println!("{err}");
			return Ok(());
		}
	};
	println!("Lowered:");
	println!(
		"{:?} : {:?}",
		ctx.fmt_term(typed_term.term),
		ctx.fmt_term(typed_term.typ)
	);
	println!("{} : {}", ctx.fmt_term(typed_term.term), ctx.fmt_term(typed_term.typ));

	// infer types
	println!("--- INFERRING ---");
	let typed_term = match verify::verify(typed_term, &mut ctx) {
		Ok(typed) => typed,
		Err(err) => {
			println!("{err}");
			return Ok(());
		}
	};

	println!("Inferred:");
	println!(
		"{:?} : {:?}",
		ctx.fmt_term(typed_term.term),
		ctx.fmt_term(typed_term.typ)
	);
	println!("{} : {}", ctx.fmt_term(typed_term.term), ctx.fmt_term(typed_term.typ)); */

	// type check tree

	// evaluate term

	// print output

	/* for line in reader.lines() {
		   println!("{}", line?);
	   }
	*/
	Ok(())
}

#[test]
fn test_lex_and_parse() {
	let program = r##"
    main := print "hello world!"
    main : Unit := (print "hello world")
    id := x -> x

    "##;
}
