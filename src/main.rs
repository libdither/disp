#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(generic_associated_types)]

use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod expr;
mod parse;
mod symbol;

use crate::expr::beta_reduce;
use parse::parse_line;

fn main() {
	let exprs = &LinkArena::new();

	let _load_file = std::env::args().nth(1);

	/* if let Some(file) = &load_file {
		ser.db.load(fs::File::open(file).unwrap()).expect("could not load disp file")
	} */

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

				match parse_line(line.as_str(), exprs) {
					Ok(Some(expr)) => {
						match beta_reduce(&expr, exprs) {
							Ok(expr) => println!("{}", expr),
							Err(err) => {
								println!("failed to reduce expression: {}", err);
								continue;
							}
						};
					}
					Ok(None) => {}
					Err(err) => {
						println!("failed to parse expression: {}", err);
						continue;
					}
				};
			}
			Err(ReadlineError::Interrupted) => {
				println!("CTRL-C");
			}
			Err(ReadlineError::Eof) => {
				println!("CTRL-D");
				break;
			}
			Err(err) => {
				println!("Error: {:?}", err);
				break;
			}
		}
	}
	rl.save_history(".disp_history").unwrap();

	/* if let Some(file) = &load_file {
		db.save(fs::File::create(&file).unwrap()).unwrap()
	} */
}
