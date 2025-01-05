mod parse;
mod lexer;
mod lower;

use std::collections::HashMap;
use std::env;
use std::io;

use parse::ParseTree;
use winnow::Parser;

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("Usage: disp <PROGRAM>");
    let program = std::fs::read_to_string(filename)?;

    // lex string
    println!("--- LEXING ----");
    let lex = match lexer::lexer.parse(&mut program.as_str()) {
        Ok(lex) => lex,
        Err(err) => {
            println!("{err}");
            return Ok(());
        }
    };
    println!("lex: {lex:?}");

    // parse tokens
    println!("--- PARSING ----");
    let stmts = match parse::parse_file.parse(&mut lex.as_slice()) {
        Ok(stmts) => stmts,
        Err(err) => {
            println!("{err:?}");
            return Ok(());
        }
    };
    println!("Program:");
    println!("{}", program.as_str());
    println!("Parse:");
    for (i, tree) in stmts.iter().enumerate() {
        println!("{i:03} {tree}");
    }

    // convert parsetree to semantic tree
    println!("--- LOWERING ----");
    
    

    // infer types

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