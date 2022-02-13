#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(generic_associated_types)]

use std::io::{self, Write};

use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod lambda_calculus;
mod symbol;
mod parse;

use crate::lambda_calculus::{beta_reduce, DisplayWithDatastore};
use symbol::Symbol;
use parse::parse_expr;

fn main() {
	let db = &mut Datastore::new();

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

				let expr = match parse_expr(line.as_str(), db) {
					Ok(expr) => expr,
					Err(err) => {println!("failed to parse expression: {}", err); continue},
				};
				let expr = match beta_reduce(&expr, db) {
					Ok(expr) => expr,
					Err(err) => {println!("failed to reduce expression: {}", err); continue},
				};
				println!("{}", expr.display(db));
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
}
