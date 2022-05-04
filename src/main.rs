#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(generic_associated_types)]

use ariadne::Source;
use chumsky::Parser;
use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod expr;
mod parse;
mod symbol;

use expr::beta_reduce;
use symbol::Symbol;

fn main() {
	let exprs = &LinkArena::new();
	let ser = &mut LinkSerializer::new();
	let db = &mut Datastore::new();

	let _load_file = std::env::args().nth(1);

	/* if let Some(file) = &load_file {
		db.load(fs::File::open(file).unwrap()).expect("could not load disp file")
	} */

	let mut rl = Editor::<()>::new();
	println!("Welcome to disp (λ)");
	if rl.load_history(".disp_history").is_err() {
		// println!("No previous history.");
	}

	let binds = &LinkArena::new();
	let bind_map = parse::BindMap::default();
	let parser = parse::command_parser(exprs, binds, &bind_map);

	loop {
		let readline = rl.readline(">> ");
		match readline {
			Ok(line) => {
				rl.add_history_entry(line.as_str());

				use parse::Command;
				match parser.parse(line.as_str()) {
					Ok(Command::None) => {},
					Ok(Command::Set(string, expr)) => {
						println!("{expr}");
						let reduced = beta_reduce(expr, exprs).unwrap();
						println!("{reduced}");
						Symbol::new(string, reduced, exprs, ser);
					}
					Ok(Command::Reduce(expr)) => {
						println!("{expr}");
						let reduced = beta_reduce(expr, exprs).unwrap();
						println!("{reduced}");
					}
					Ok(Command::Load(file)) => {

					}
					Ok(Command::Save(file)) => {
						
					}
					Err(errors) => {
						parse::gen_report(errors).try_for_each(|report|report.print(Source::from(&line))).unwrap();
					}
				}
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
