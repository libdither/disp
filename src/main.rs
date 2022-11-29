#![allow(incomplete_features)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(generic_const_exprs)]
#![feature(try_blocks)]

use std::fs;

use anyhow::Context;
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
	let mut links = &RevLinkArena::new(exprs);

	let load_file = std::env::args().nth(1);

	if let Some(file) = &load_file {
		links.load(&mut fs::File::open(file).unwrap()).unwrap()
	}

	let mut rl = Editor::<()>::with_config(rustyline::Config::builder().max_history_size(1000).build());
	println!("Welcome to disp (λ)");
	if rl.load_history(".disp_history").is_err() {
		// println!("No previous history.");
	}

	let binds = &LinkArena::new();
	let bind_map = parse::NameBindStack::default();

	let parser = parse::command_parser(&links, binds, &bind_map);

	loop {
		let readline = rl.readline(">> ");
		match readline {
			Ok(line) => {
				rl.add_history_entry(line.as_str());
				let res: Result<(), Box<dyn std::error::Error>> = try {
					use parse::Command;
					match parser.parse(line.as_str()) {
						Ok(Command::None) => {}
						Ok(Command::Name(string, sem)) => {
							println!("{sem}");
							let reduce_link = ReduceLink::create(sem.expr, links).with_context(||"failed to set because failed to reduce")?;
							NamedExpr::new_linked(&string, reduce_link.reduced, links);
						}
						Ok(Command::Reduce(sem)) => {
							println!("{sem}");
							let reduce_link = ReduceLink::create(sem.expr, links).with_context(||"failed to reduce")?;
							println!("{}", reduce_link.reduced);
						}
						Ok(Command::Check(term, ty)) => {
							println!("Checking {term} : {ty}");
							let term = ReduceLink::create(term.expr, links).with_context(||"failed to reduce term")?;
							let ty = ReduceLink::create(ty.expr, links).with_context(||"failed to reduce type")?;
							match Judgement::check(term, ty, links) {
								Ok(_judgement) => println!("Typechecking succeeded"),
								Err(residual) => println!("Failed to check: {}", residual),
							}

						}
						Ok(Command::Get(_string)) => {

						}
						Ok(Command::List) => {
							// let name = Name::add(links.add(""), links)
							// links.links::<Name, NamedExpr>()
							// namespace.for_each(|name| println!("{name}"))
						}
						
						Ok(Command::Load { file: filename }) => {
							let mut file = fs::File::open(filename)?;
							links.load(&mut file)?;
						}
						Ok(Command::Save { file: filename, overwrite: _ }) => {
							let mut file = fs::File::create(&filename)?;
							links.save(&mut file)?;
						}
						Ok(_) => {}
						Err(errors) => {
							parse::gen_report(errors)
								.try_for_each(|report| report.print(Source::from(&line)))?;
						}
					};
				};
				if let Err(err) = res {
					println!("{err}");
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
