#![allow(incomplete_features)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(generic_const_exprs)]

use std::fs;

use ariadne::Source;
use chumsky::Parser;
use rustyline::{error::ReadlineError, Editor};

use hashdb::*;

mod expr;
mod name;
mod check;
mod parse;

use name::*;
use check::*;

use crate::expr::ReduceLink;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let exprs = &LinkArena::new();
	let db = &mut Datastore::new();

	let load_file = std::env::args().nth(1);

	if let Some(file) = &load_file {
		db.load(&mut fs::File::open(file).unwrap()).expect("could not load disp file")
	}

	let mut rl = Editor::<()>::with_config(rustyline::Config::builder().max_history_size(1000).build());
	println!("Welcome to disp (λ)");
	if rl.load_history(".disp_history").is_err() {
		// println!("No previous history.");
	}

	let binds = &LinkArena::new();
	let bind_map = parse::NameBindStack::default();

	// Current namespace of this REPL, contains all the currently accessible names
	let links = &RevLinkArena::new(exprs);

	let parser = parse::command_parser(links, binds, &bind_map);

	loop {
		let readline = rl.readline(">> ");
		match readline {
			Ok(line) => {
				rl.add_history_entry(line.as_str());

				use parse::Command;
				match parser.parse(line.as_str()) {
					Ok(Command::None) => {}
					Ok(Command::Name(string, sem)) => {
						println!("{sem}");
						let reduce_link = ReduceLink::create(sem.expr, links).expect("failed to reduce");
						NamedExpr::new_linked(&string, reduce_link.reduced, links);
					}
					Ok(Command::Reduce(sem)) => {
						println!("{sem}");
						let reduced = sem.expr.reduce(exprs).unwrap();
						println!("{reduced}");
					}
					Ok(Command::Get(string)) => {

					}
					Ok(Command::List) => {
						// namespace.for_each(|name| println!("{name}"))
					}
					
					/* Ok(Command::Load { file: filename }) => {
						let result: anyhow::Result<()> = try {
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
					} */
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
