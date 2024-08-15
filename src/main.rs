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
    let tree = match parse::expr.parse(&mut lex.as_slice()) {
        Ok(tree) => tree,
        Err(err) => {
            println!("{err:?}");
            return Ok(());
        }
    };
    println!("tree: {tree}");
    // type check it

    // run / print output

    /* for line in reader.lines() {
        println!("{}", line?);
    }
 */
    Ok(())
}