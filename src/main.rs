mod parse;
mod lexer;

use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("Usage: <program> <filename>");
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    // parse it



    // type check it

    // run / print output

    for line in reader.lines() {
        println!("{}", line?);
    }

    Ok(())
}