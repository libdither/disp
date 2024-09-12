mod parse;
mod lexer;

use std::env;
use std::io;

use winnow::Parser;

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("Usage: disp <PROGRAM>");
    let program = std::fs::read_to_string(filename)?;
    // parse it
    let lex = match lexer::lexer.parse(&mut program.as_str()) {
        Ok(lex) => lex,
        Err(err) => {
            println!("{err}");
            return Ok(());
        }
    };
    
    println!("lex: {lex:?}");
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
    
    // type check it

    // run / print output

    /* for line in reader.lines() {
        println!("{}", line?);
    }
 */
    Ok(())
}