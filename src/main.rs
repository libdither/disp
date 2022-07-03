#![feature(iter_intersperse)]
#![feature(option_result_contains)]
#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]
#![feature(try_blocks)]

use std::fs;

use ariadne::Source;
use chumsky::Parser;
use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod expr;
mod name;
mod parse;

use expr::beta_reduce;
use name::*;


fn main() -> Result<(), Box<dyn std::error::Error>> {
	let exprs = &LinkArena::new();
	let db = &mut Datastore::new();

	let _load_file = std::env::args().nth(1);

	/* if let Some(file) = &load_file {
		db.load(fs::File::open(file).unwrap()).expect("could not load disp file")
	} */

	let mut rl = Editor::<()>::with_config(rustyline::Config::builder().max_history_size(1000).build());
	println!("Welcome to disp (λ)");
	if rl.load_history(".disp_history").is_err() {
		// println!("No previous history.");
	}

	let binds = &LinkArena::new();
	let bind_map = parse::BindMap::default();

	// Current namespace of this REPL, contains all the currently accessible names
	let namespace = NamespaceMut::new();

	let parser = parse::command_parser(&namespace, exprs, binds, &bind_map);

	loop {
		let readline = rl.readline(">> ");
		match readline {
			Ok(line) => {
				rl.add_history_entry(line.as_str());

				use parse::Command;
				match parser.parse(line.as_str()) {
					Ok(Command::None) => {}
					Ok(Command::Set(string, expr)) => {
						println!("{expr}");
						let reduced = beta_reduce(expr, exprs).unwrap();
						println!("{reduced}");
						namespace.add(string, reduced, exprs);
					}
					Ok(Command::List) => {
						namespace.for_each(|name| println!("{name}"))
					}
					Ok(Command::Reduce(expr)) => {
						println!("{expr}");
						let reduced = beta_reduce(expr, exprs).unwrap();
						println!("{reduced}");
					}
					Ok(Command::Load { file: filename }) => {
						let result: Result<(), Box<dyn std::error::Error>> = try {
							let mut file = fs::File::open(filename)?;
							let hash: TypedHash<Namespace> = bincode::deserialize_from::<_, Hash>(&mut file)?.into();
							let mut db = Datastore::new();
							db.load(&mut file)?;
							let clone = hash.fetch(&db, exprs)?.clone();
							namespace.extend(&clone);
						};
						match result { Err(err) => println!("failed to load: {err}"), Ok(_) => {} }
					}
					Ok(Command::Save { file: filename, overwrite: _ }) => {
						let saved_hash = {
							db.clear();
							namespace.store_inner(exprs).store(db).unwrap()
						};
						let mut file = fs::File::create(&filename).unwrap();
						bincode::serialize_into(&mut file, saved_hash.as_hash())?;
						db.save(&mut file)?;
						println!("Saved current namespace to {}", &filename);
					}
					Ok(_) => {}
					Err(errors) => {
						parse::gen_report(errors)
							.try_for_each(|report| report.print(Source::from(&line)))
							.unwrap();
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
	Ok(())
}
