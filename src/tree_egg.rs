//! Tree‑calculus evaluator rewritten to use the `egg` e‑graph library.

/* ------------------------------------------------------------------------
   Crate layout
   ------------
   This file can live at `src/main.rs` and re‑uses your existing `tree_parse`
   module unchanged.  Simply rename the old `tree_eval` module to
   `tree_eval_old` (or delete it) and drop this file in its place.  Don’t
   forget to add `egg = "0.9"` to your `Cargo.toml`.

   ├── src
   │   ├── main.rs          <- (this file)
   │   ├── tree_parse.rs    <- the parser you already have
   │   └── …
   └── Cargo.toml           <- add the egg dependency

   The code keeps the same CLI (file input + --executor flag) and REPL UX, but
   the runtime semantics are now implemented as equality‑saturation over an
   e‑graph instead of an ad‑hoc interpreter.
   --------------------------------------------------------------------- */

   use std::{
    collections::HashMap,
    io::{self, Write},
    path::PathBuf,
};

use async_std::task;
use clap::Parser as _; // just import the trait
use egg::{define_language, rewrite as rw, *};
use rustyline_async::{Readline, ReadlineEvent, SharedWriter};
use winnow::Parser;

mod tree_parse;
use tree_parse::{TreeExpr, Token};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   1.  The term language (AST) used inside the e‑graph
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

define_language! {
    /// The three node types of tree‑calculus plus arbitrary *symbol* leaves
    /// for user‑defined identifiers ("main", "id", …).
    ///
    ///  Leaf     ≙ the constant `△`
    ///  Stem x   ≙ unary constructor
    ///  Fork x y ≙ binary constructor *and* application node
    ///            (as in the original interpreter)
    pub enum TreeLang {
        "△" = Leaf,
        "Stem" = Stem(Id),
        "Fork" = Fork(Id, Id),
        // Any other string is treated as an identifier / constant symbol
        Symbol(Symbol),
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   2.  Helper: convert the old `TreeExpr` (produced by the winnow parser)
       into an `egg::RecExpr<TreeLang>` so it can be inserted into the
       e‑graph.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
fn treeexpr_to_recexpr(expr: &TreeExpr) -> RecExpr<TreeLang> {
    fn go(expr: &TreeExpr, nodes: &mut Vec<TreeLang>) -> Id {
        match expr {
            TreeExpr::Leaf => {
                let id = nodes.len();
                nodes.push(TreeLang::Leaf);
                id.into()
            }
            TreeExpr::Ident(s) => {
                let id = nodes.len();
                nodes.push(TreeLang::Symbol(Symbol::from(s.as_str())));
                id.into()
            }
            TreeExpr::Stem(inner) => {
                let child = go(inner, nodes);
                let id = nodes.len();
                nodes.push(TreeLang::Stem(child));
                id.into()
            }
            TreeExpr::Fork(l, r) => {
                let l_id = go(l, nodes);
                let r_id = go(r, nodes);
                let id = nodes.len();
                nodes.push(TreeLang::Fork(l_id, r_id));
                id.into()
            }
        }
    }

    let mut nodes = Vec::<TreeLang>::new();
    go(expr, &mut nodes);
    RecExpr::from(nodes)
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   3.  The tree‑calculus reduction rules expressed as `egg` rewrites
       (direct translations of the five clauses of `apply` in the old code).
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
fn rules() -> Vec<Rewrite<TreeLang, ()>> {
    vec![
        // 0a: (Fork △ a)  →  (Stem a)
        rw!("beta‑leaf";  "(Fork △ ?a)" => "(Stem ?a)"),

        // 0b: (Fork (Stem a) b) → (Fork a b)
        rw!("beta‑stem";  "(Fork (Stem ?a) ?b)" => "(Fork ?a ?b)"),

        // 1 : (Fork (Fork △ a) b) → a
        rw!("beta‑fork‑leaf"; "(Fork (Fork △ ?a) ?b)" => "?a"),

        // 2 : (Fork (Fork (Stem a1) a2) b) → (Fork (Fork a1 b) (Fork a2 b))
        rw!("beta‑fork‑stem";
            "(Fork (Fork (Stem ?a1) ?a2) ?b)" => "(Fork (Fork ?a1 ?b) (Fork ?a2 ?b))"),

        // 3a: (Fork (Fork (Fork a1 a2) a3) △) → a1
        rw!("beta‑forkA";
            "(Fork (Fork (Fork ?a1 ?a2) ?a3) △)" => "?a1"),

        // 3b: (Fork (Fork (Fork a1 a2) a3) (Stem u)) → (Fork a2 u)
        rw!("beta‑forkB";
            "(Fork (Fork (Fork ?a1 ?a2) ?a3) (Stem ?u))" => "(Fork ?a2 ?u)"),

        // 3c: (Fork (Fork (Fork a1 a2) a3) (Fork u v)) → (Fork (Fork a3 u) v)
        rw!("beta‑forkC";
            "(Fork (Fork (Fork ?a1 ?a2) ?a3) (Fork ?u ?v))" => "(Fork (Fork ?a3 ?u) ?v)"),
    ]
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   4.  Cost function: use *AST size* so the extractor prefers smaller normal
       forms (this matches the intent of the old interpreter pretty well).
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
use egg::cost_function::AstSize;

type CostFn = AstSize;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   5.  Command‑line interface (unchanged)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
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
    /// Run as a Read‑Eval‑Print Loop
    Repl,
    /// Run the `main` function from the file
    Main,
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   6.  A tiny wrapper struct that owns the global e‑graph and an identifier
       → eclass mapping.  This replaces the old `TermStore` completely.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#[derive(Default)]
struct GraphStore {
    egraph: EGraph<TreeLang, ()>,
    id_map: HashMap<String, Id>,
}

impl GraphStore {
    fn new() -> Self {
        Self::default()
    }

    /// Lower an assignment `ident := expr` into the e‑graph.
    fn lower_assign(&mut self, ident: String, expr: TreeExpr) {
        let re = treeexpr_to_recexpr(&expr);
        let expr_id = self.egraph.add_expr(&re);
        let ident_id = self
            .id_map
            .entry(ident.clone())
            .or_insert_with(|| self.egraph.add(TreeLang::Symbol(Symbol::from(ident.as_str()))))
            .clone();
        self.egraph.union(expr_id, ident_id);
        self.egraph.rebuild();
    }

    /// Insert an *anonymous* expression and return its root id.
    fn add_expr(&mut self, expr: TreeExpr) -> Id {
        let re = treeexpr_to_recexpr(&expr);
        let id = self.egraph.add_expr(&re);
        self.egraph.rebuild();
        id
    }

    /// Extract the *best* expression represented by the given e‑class id.
    fn extract(&self, id: Id) -> RecExpr<TreeLang> {
        let extractor = Extractor::new(&self.egraph, CostFn);
        let (_cost, expr) = extractor.find_best(id);
        expr
    }

    /// Pretty‑print a [`RecExpr<TreeLang>`] using prefix notation.
    fn display_recexpr(expr: &RecExpr<TreeLang>) -> String {
        expr.to_string()
    }

    /// Get (or panic if missing) the e‑class id for an identifier symbol.
    fn id_for(&mut self, ident: &str) -> Id {
        self
            .id_map
            .entry(ident.to_owned())
            .or_insert_with(|| self.egraph.add(TreeLang::Symbol(Symbol::from(ident))))
            .clone()
    }

    /// Run equality saturation once with our rewrite rules.
    fn saturate(&mut self) {
        let runner = Runner::default()
            .with_egraph(std::mem::take(&mut self.egraph))
            .with_iter_limit(64)
            .run(&rules());
        self.egraph = runner.egraph;
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   7.  REPL helpers (lightly adapted to the new `GraphStore`)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
fn repl_parse_line(store: &mut GraphStore, wr: &mut SharedWriter, line: &str) -> io::Result<()> {
    match tree_parse::lexer(line) {
        Ok(tokens) => match tree_parse::parse_line.parse(&tokens[..]) {
            Ok((Some(ident), expr)) => {
                store.lower_assign(ident.clone(), expr);
                store.saturate();
                let best = store.extract(store.id_for(&ident));
                writeln!(wr, "{ident} := {}", GraphStore::display_recexpr(&best))?;
            }
            Ok((None, expr)) => {
                let id = store.add_expr(expr);
                store.saturate();
                let best = store.extract(id);
                writeln!(wr, "Result: {}", GraphStore::display_recexpr(&best))?;
            }
            Err(e) => writeln!(wr, "Parse Error: {e:?}")?,
        },
        Err(e) => writeln!(wr, "Lex Error: {e:?}")?,
    }
    Ok(())
}

async fn start_repl(mut store: GraphStore) -> io::Result<()> {
    let (mut rl, mut wr) = Readline::new("> ".to_owned()).expect("couldn’t create readline");

    loop {
        match rl.readline().await {
            Ok(ReadlineEvent::Line(line)) => match line.as_str() {
                "/l" => {
                    for (ident, id) in &store.id_map {
                        let best = store.extract(*id);
                        writeln!(&mut wr, "{ident}: {}", GraphStore::display_recexpr(&best))?;
                    }
                }
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

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   8.  main()
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
fn main() -> io::Result<()> {
    let args = <Args as clap::Parser>::parse();
    println!("file: {:?}, executor: {:?}", args.file, args.executor);

    let mut store = GraphStore::new();

    // ── 1) load definitions from the file … ────────────────────────────
    let program = std::fs::read_to_string(&args.file)?;
    for line in program.lines() {
        if let Ok(tokens) = tree_parse::lexer(line) {
            if let Ok((Some(ident), expr)) = tree_parse::parse_line.parse(&tokens[..]) {
                store.lower_assign(ident, expr);
            }
        }
    }
    // … and saturate once so they’re all reduced.
    store.saturate();

    // ── 2) execute according to requested mode ─────────────────────────
    match args.executor {
        Executor::Repl => task::block_on(async { start_repl(store).await })?,
        Executor::Main => {
            let id = store.id_for("main");
            let best = store.extract(id);
            println!("Result: {}", GraphStore::display_recexpr(&best));
        }
    }

    Ok(())
}
