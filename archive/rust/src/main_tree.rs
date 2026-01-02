use std::{io, io::Write, path::PathBuf};

use rustyline_async::{Readline, ReadlineEvent, SharedWriter};
use winnow::Parser;

mod tree_eval;
mod tree_parse;

/// Clap command parser struct that takes one argument, a file path (should check to make sure file exists and load it) and a --executor (-e) flag that can be set to one of 2 different enum variants: <repl>, or <main>

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
	/// Path to the input file
	#[arg(value_name = "FILE")]
	file: PathBuf,

	/// Choose the executor mode
	#[arg(short, long, value_enum, default_value_t = Executor::Main)]
	executor: Executor,
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum Executor {
	/// Run as a Read-Eval-Print Loop
	Repl,
	/// Run the main function from the file
	Main,
}

fn main() -> io::Result<()> {
	let args = <Args as clap::Parser>::parse();
	println!("file: {:?}, executor: {:?}", args.file, args.executor);

	let mut store = tree_eval::TermStore::new();

	// write some code that if a filename is included in cli, splits the file into lines and adds to store.
	let program = std::fs::read_to_string(args.file)?;
	for line in program.lines() {
		if let Ok(tokens) = tree_parse::lexer(line) {
			if let Ok((Some(ident), expr)) = tree_parse::parse_line.parse(&tokens[..]) {
				let _ = store.lower_assign((ident, expr));
			}
		}
	}

	match args.executor {
		Executor::Repl => {
			async_std::task::block_on(async { start_repl(store).await })?;
		}
		Executor::Main => {
			let eval_result = store.lower_and_reduce(tree_parse::TreeExpr::Ident("main".to_owned()));
			// handle error and print
			match eval_result {
				Ok(reduced) => println!(
					"Result: {} := {}",
					store.display_term(reduced, true),
					store.display_term(reduced, false)
				),
				Err(e) => println!("Lowering Error: {:?}", e),
			}
		}
	}

	Ok(())
}

fn repl_parse_line(store: &mut tree_eval::TermStore, wr: &mut SharedWriter, line: &str) -> io::Result<()> {
	match tree_parse::lexer(&line) {
		Ok(tokens) => match tree_parse::parse_line.parse(&tokens[..]) {
			Ok((Some(ident), expr)) => {
				let assign_result = store.lower_assign((ident.clone(), expr));
				// handle error and print
				match assign_result {
					Ok(reduced) => writeln!(
						wr,
						"{} := {} := {}",
						ident,
						store.display_term(reduced, true),
						store.display_term(reduced, false)
					)?,
					Err(e) => writeln!(wr, "Lowering Error: {:?}", e)?,
				}
			}
			Ok((None, expr)) => {
				let eval_result = store.lower_and_reduce(expr);
				// handle error and print
				match eval_result {
					Ok(reduced) => writeln!(
						wr,
						"Result: {} := {}",
						store.display_term(reduced, true),
						store.display_term(reduced, false)
					)?,
					Err(e) => writeln!(wr, "Lowering Error: {:?}", e)?,
				}
			}
			Err(e) => writeln!(wr, "Parse Error: {:?}", e)?,
		},
		Err(e) => writeln!(wr, "Lex Error: {:?}", e)?,
	};
	Ok(())
}

async fn start_repl(mut store: tree_eval::TermStore) -> io::Result<()> {
	let (mut rl, mut wr) = Readline::new("> ".to_string()).expect("couldn't create readline");

	loop {
		match rl.readline().await {
			Ok(ReadlineEvent::Line(line)) => match line.as_str() {
				"/l" => store
					.iter_idents()
					.try_for_each(|(ident, key)| writeln!(wr, "{ident}: {}", store.display_term(*key, true)))?,
				line => repl_parse_line(&mut store, &mut wr, line)?,
			},
			Ok(_) => break,
			Err(err) => {
				println!("Error: {err:?}");
				break;
			}
		}
	}
	Ok(())
}
