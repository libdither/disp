#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(generic_associated_types)]

use std::fs;

use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod lambda_calculus;
mod symbol;
mod parse;

use crate::{lambda_calculus::{beta_reduce, DisplayWithDatastore}};
use symbol::Symbol;
use parse::{parse_line};

fn main() {
	let db = &mut Datastore::new();

    let load_file = std::env::args().nth(1);

    if let Some(file) = &load_file {
        db.load(fs::File::open(file).unwrap()).expect("could not load disp file")
    }

	let mut rl = Editor::<()>::new();
	println!("Welcome to disp (λ)");
    if rl.load_history(".disp_history").is_err() {
       // println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

				match parse_line(line.as_str(), db) {
					Ok(Some(expr)) => {
                        match beta_reduce(&expr, db) {
                            Ok(expr) => println!("{}", expr.display(db)),
                            Err(err) => {println!("failed to reduce expression: {}", err); continue},
                        };
                    },
                    Ok(None) => {},
					Err(err) => {println!("failed to parse expression: {}", err); continue},
				};
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history(".disp_history").unwrap();

    if let Some(file) = &load_file {
        db.save(fs::File::create(&file).unwrap()).unwrap()
    }
}
