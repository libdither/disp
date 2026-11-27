# Disp

Disp is a self-verified dependently-typed programming language built on the [tree calculus](https://github.com/barry-jay-personal/tree-calculus), a computation model whose programs can inspect other programs. That reflection is what lets types be ordinary functions: a type takes a value and returns true or false. The long-term goals are to:
 - Create the ultimate programming language, one that other programming languages can be re-created in and transpiled to.
 - Create a "decentralized" programming language, where different programmers can build and adopt new features (syntax, types) independently, with automatic translation between their dialects.
 - Use disp to create a general-purpose program optimizer: a program that takes a specification (a type, a predicate, or more generally a loss function) and returns something that maximally satisfies it.
 - Write the type of such optimizers and have the optimizer create itself, yielding an interpretable (symbolic) recursively self-improving optimizer.
 - Create hardware cost models at varying levels of detail and have the optimizer target to produce behaviorally equivalent code that minimizes cost for those hardware models.

## Why Disp?

[Rust](https://www.rust-lang.org/) is fast, but you can't verify your programs. [Lean](https://lean-lang.org/) can verify your programs, but it takes real effort and the result is usually not fast. And in every language you still write the implementation yourself: no toolchain takes a high-level specification and satisfies it for you. The closest thing today is a coding agent like Claude Code, which gets expensive at scale and still needs a language that can state formal specifications in the first place (Lean does not make this as easy as it could be).

A language where all you write is the constraints, as detailed and rigorous as you need, with the implementation derived automatically, requires constraint checking that is itself fast and modular. This is the problem disp solves.

## AI usage

For disp in its current state, AI is heavily used for development. AI is mostly good for writing code and idea generation / reflection but still needs a lot of oversight for design decisions, especially for the field of PL design where good design is rather rare. AI use should be rather obvious based on how it writes, but as of 7/7/26 nearly all of the TypeScript host, type system kernel, standard library, tests, evaluator backends and documentation were written using Claude Opus 4.8 and Fable 5.

The [Documentation](#documentation) section below details important docs and source files, whether they are human or AI-written, and how important they might be to read for understanding. The website's Why page (`website/src/routes/why/`) explains what disp is trying to do and how, for a general technical reader.

## Disp by example 🤖

This section is one file, top to bottom ([`lib/tests/by_example.test.disp`](lib/tests/by_example.test.disp)), and the test suite runs it. A `test lhs = rhs` passes when both sides reduce to the identical tree, so everything this section claims is machine-checked. Code blocks are tagged `rust` only to borrow GitHub's syntax highlighting; the code is disp. The website's Learn page (`website/src/lib/learn/`) runs the same examples in the browser and explains them at length.

<!-- The blocks below are copied from lib/tests/by_example.test.disp, which the test
     suite runs; the website's Learn page shows the same file. Edit that file, then run
       python scripts/sync-examples.py -->

<!-- example: imports -->
```rust
open use raw "../prelude.disp" {}
open use raw "../list.disp" {}
open use raw "../verdict.disp" {}
open use raw "../stream.disp" {}
open use raw "../claim.disp" {}
open use raw "fixtures.disp" {}
open use raw "../../IDEAS.disp" {}
open use raw "../confirm.disp" {}
```

### Everything is a tree

<!-- example: trees -->
```rust
// Disp compiles to the tree calculus: five rewrite rules over binary trees grown
// from a single leaf `t`. Numbers, functions and tests are all such trees.

test true = t                                     // the leaf itself
test false = t(t)                                 // a stem: one branch up
test tree_eq(3, succ(succ(succ(zero)))) = true    // 3 is sugar, and the trees are identical

// Three of the five rules dispatch on the shape of a tree (leaf, stem or fork),
// so any program can take any other apart, with no quoting and no reflection API:

let shape_of := t(t("leaf", {u} => "stem"), {u, v} => "fork")
test shape_of(t) = "leaf"
test shape_of(t(t)) = "stem"
test shape_of(t(t, t)) = "fork"
test shape_of(not) = "fork"
```

### A claim is a test, asked of every tree

<!-- example: claim -->
```rust
// A claim is a program from a witness and a challenge to a Bool. This one says a
// function takes numbers to numbers. It says nothing about trees that are not numbers.

nat_fn := {g, x} => if (NatR(x)) { NatR(g(x)) } else { true }

// `Trees` is every tree there is, smallest first. The honest check searches it for
// a challenge the witness fails, up to a budget.

by_search := honest(Trees.as_items)
test by_search(nat_fn, {n} => pair(n, n), 60) = refuted(1)

// A failure is final, but running out of budget proves nothing. Open only means
// "no failure so far". This program is wrong on 3, the 65th tree:

sneaky := {n} => if (n == 3) { t(t) } else { n }
test by_search(nat_fn, sneaky, 64) = "Open"
test by_search(nat_fn, sneaky, 65) = refuted(3)
test by_search(nat_fn, succ, 150) = "Open"
```

### Proved needs a shortcut

<!-- example: proved -->
```rust
// The stream of trees never ends, so search can never say Proved. A finite list
// of challenges can, when every tree is covered by something on the list:

bool_fn := {v, x} => if (Bool(x)) { Bool(v(x)) } else { true }
test over(bool_fn, not, [true, false]) = "Proved"

// The cover is a lens: it carries witnesses forward and challenges back. Its
// promise is that a witness passing the pulled-back challenge passes the original.
// That promise is itself a claim, so search can catch a bad shortcut. This one
// sends every tree to `true`, and misses a program that fails only on `false`:

half_bad := {x} => if (x) { true } else { pair(x, x) }
bool_cover := lens({w} => w, {_w, x} => if (Bool(x)) { x } else { true })
bad_cover := lens({w} => w, {_w, _x} => true)
test by_search(lens_holds(bool_fn, bool_fn, bool_cover), half_bad, 60) = "Open"
test by_search(lens_holds(bool_fn, bool_fn, bad_cover), half_bad, 60) = refuted(false)
```

### Different programs, proved the same

<!-- example: same -->
```rust
// This is what an optimizer needs: two different programs, known to do the same
// thing. Comparing trees is instant, and too strict. These are different programs:

dbl_add := {d2, n} => if (is_zero(n)) { zero } else { add(2, d2(nat_pred(n))) }
test tree_eq(dbl_step, dbl_add) = false

// Two cases cover every number, zero and one more than some number, and each
// program's recursive call on the smaller number is named by one token. Both cases
// match, so the programs agree on every number:

test eq_by_induction(dbl_step, dbl_add) = "Proved"

// A wrong step disagrees on a case, and the checker returns a number to run:

wrong_step := {d2, n} => if (is_zero(n)) { zero } else { succ(d2(nat_pred(n))) }
test eq_by_induction(dbl_step, wrong_step) = refuted(1)

// Whether this reaches the rewrites that matter is the project's biggest open
// question (FOUNDATIONS.md, Q1). Map fusion and strength reduction are not yet
// attempted here. The rules for building shortcuts are the trusted part, and they
// have not themselves been proved sound.
```

The TypeScript and Rust hosts only accelerate these in-language definitions and are validated against them. Five evaluator backends sit behind one ABI and must agree byte for byte, so no single evaluator has to be trusted ([`EVALUATOR.md`](EVALUATOR.md)). Each decision above is argued against its historical precedents in [`FOUNDATIONS.md`](FOUNDATIONS.md).

## Status 🤖

Working today:

- The full pipeline: parser, elaborator, and tree-calculus evaluation (`src/`), with the surface grammar specified in [`SYNTAX.typ`](SYNTAX.typ).
- The kernel (`lib/kernel/`): the promoted evaluator-map type system ([`TYPE_SYSTEM.typ`](TYPE_SYSTEM.typ)), where every type is a `(check, assume)` pair and assumptions are composable `Eval -> Eval` layers on a 7-field `Kit` with zero custom runtime opcodes, structural/lexicographic totality (plus explicit `PartialPi` with Scott induction), quotients, cross-module certificates, full stdlib checking (`lib/kernel/std_*.disp`), and verified `.opt.disp` rewrite guards (`lib/kernel/verified_opt*.disp`).
- Earlier kernels and prototypes, retired under `archive/`: `archive/live-kernel/` (the earlier two-op trusted core, still exercised by driver tests and website examples), `archive/provenance-kernel/` (the intermediate provenance-ledger kernel), and `archive/type-system-goals/` (the five prototypes leading up to `lib/kernel/`).
- Rust evaluator backends (`evaluators/`): `rust-eager`, the fast checker backend, about twice as fast as the TypeScript oracle on the full suite; and `rust-ic-net` M0-M2, the materialized interaction net, sequential, with parallel reduction under cargo tests. Beside them, `rust-ic-strands` runs the net on a lattice where wires are physical (CPU, GPU, a browser player, a chip design), on the rules in `tree-net`.
- Verified rewrites in the promoted kernel (`lib/kernel/verified_opt.disp` + `lib/kernel/verified_opt.opt.disp` via `fn_equiv_guard`), plus an earlier archived slice (map fusion among them) at `archive/live-kernel/tests/opt_q1_*.test.disp`.

Designed but not built: the optimizer itself ([`research/OPTIMIZER.typ`](research/OPTIMIZER.typ)), extensible `meta` state for linearity/effects/cost, cubical/HoTT path types, stratified universes ([`FINAL_TYPE_SYSTEM_NEXT_STEPS.md`](FINAL_TYPE_SYSTEM_NEXT_STEPS.md)), and the neural proposer. The open research risks, most sharply whether a decidable fragment of behavioral equivalence is rich enough to license the rewrites an optimizer needs, are catalogued as falsifiable questions in [`FOUNDATIONS.md`](FOUNDATIONS.md) §V.

## Getting Started 🤖

Requires Node.js. The Rust backends are optional; the test suite runs without a Rust toolchain.

```shell
npm install
npm test                                  # host + object-language suites (vitest)
npm run disp -- lib/kernel/kernel.test.disp   # run a single .disp file
npm run disp -- --evaluator=rust-eager lib/kernel/kernel.test.disp   # pick a backend (if built)
```

A `.disp` file is a sequence of definitions and `test` declarations. Running a file elaborates it and reports test results; a test passes when both sides reduce to the identical tree.

Suggested reading order:

1. [`FOUNDATIONS.md`](FOUNDATIONS.md): what disp is attempting, the lineage of every design piece, and the risk assessment. Written for a general reader.
2. [`GOALS.md`](GOALS.md): the original goal statement.
3. `lib/kernel/kernel.test.disp`, `lib/kernel/pts.test.disp`, and `lib/tests/`: the language and type system, demonstrated (the archived corpus under `archive/live-kernel/tests/` is a museum of an earlier surface).
4. [`SYNTAX.typ`](SYNTAX.typ) and [`TYPE_SYSTEM.typ`](TYPE_SYSTEM.typ): the formal surface grammar and the evaluator-map type-system specification (with remaining milestones in [`FINAL_TYPE_SYSTEM_NEXT_STEPS.md`](FINAL_TYPE_SYSTEM_NEXT_STEPS.md)).
5. `lib/kernel/`: the type system's source. Reading order: `kernel.disp` (the evaluator-map engine and composable type formers), `surface.disp` (surface `:` vocabulary, `check_module`, and verified rewrite guards), `prelude.disp` (the barrel), and `std_*.disp` (the checked standard library).

<a id="documentation"></a>

## Documentation 🤖

The 🧑/🤖 column estimates who typed the words; git does not record the split, so the percentages are honest guesses, and a plain 🤖 means the human share is under 10%. Everything passed through both hands regardless: AI drafts land through human review, and the human prose has picked up AI-written annotations. Quality is a reading guide, from 10/10 (polished and current) down to 1/10 (working notes, possibly stale, kept for the record).

| Document | What it is | 🧑/🤖? | Quality |
|---|---|---|---|
| [`README.md`](README.md) | This file. The example section is machine-checked literate code. | 🧑 25% · 🤖 75% | 9/10 |
| [`GOALS.md`](GOALS.md) | The long-term vision: neural-guided synthesis, self-improving optimizer. | 🧑 85% · 🤖 15% | 8/10 |
| [`TYPE_SYSTEM_GOALS.md`](TYPE_SYSTEM_GOALS.md) | The five design goals for Disp's type system (refutation as meaning, sound shortcuts, evaluator-as-checker). | 🧑 85% · 🤖 15% | 8/10 |
| [`TYPE_SYSTEM.typ`](TYPE_SYSTEM.typ) | Specification of the promoted evaluator-map kernel (`lib/kernel/`): `(check, assume)` types, `Eval -> Eval` layers, totality, and verified rewrites. | 🤖 | 8/10 |
| [`FINAL_TYPE_SYSTEM_NEXT_STEPS.md`](FINAL_TYPE_SYSTEM_NEXT_STEPS.md) | Open milestones for the promoted kernel (extensible `meta` state, HoTT paths, stratified universes, stream-type soundness). | 🤖 | 8/10 |
| [`FOUNDATIONS.md`](FOUNDATIONS.md) | Every design piece's precedent, why prior attempts stalled, disp's bet, and the make-or-break questions. *Interesting Read* | 🤖 | 7/10 |
| [`EVALUATOR.md`](EVALUATOR.md) | The reduction-backend subsystem: the `Session` ABI, the five backends, the differential-oracle discipline. | 🤖 | 6/10 |
| [`SYNTAX.typ`](SYNTAX.typ) | Surface grammar and AST. Authoritative for the parser. | 🤖 | 7/10 |
| [`research/CLAIMS_OVER_TREES.md`](research/CLAIMS_OVER_TREES.md) | The claim layer: a claim is a test on bounded runs under quantifiers over all trees; optimizations remove quantifiers; §9 the landed lens form. | 🤖 | 7/10 |
| [`archive/live-kernel/TYPE_THEORY.typ`](archive/live-kernel/TYPE_THEORY.typ) | Type-theory spec for the *archived* two-op kernel (`archive/live-kernel/`). Superseded by [`TYPE_SYSTEM.typ`](TYPE_SYSTEM.typ). *Long* | 🤖 | 5/10 |
| [`archive/live-kernel/KERNEL_DESIGN.md`](archive/live-kernel/KERNEL_DESIGN.md) | Tree-calculus implementation idioms for the archived kernel: wait/fix, signatures, neutrals, bracket abstraction. | 🤖 | 6/10 |

The kernel is source code written to be read: the type system is a library, so these files are the type theory's implementation and its documentation at once.

| Source file | What it is | 🧑/🤖? | Quality |
|---|---|---|---|
| [`lib/prelude.disp`](lib/prelude.disp) | The raw substrate below the type system: booleans, pairs, triage, wait/fix, tree_eq. | 🤖 | 8/10 |
| [`lib/kernel/kernel.disp`](lib/kernel/kernel.disp) | The evaluator-map kernel: `Eval`/`Kit`, `(check, assume)` formers, structural/lexicographic totality, Scott induction, and cross-module certificates. | 🤖 | 8/10 |
| [`lib/kernel/surface.disp`](lib/kernel/surface.disp) | The surface `:` hook (`Pi`, `check_module`), declaration guard protocol (`default_guard`, `freeze`, `let`, `guard`, `given`), and verified rewrite guards (`rel_guard`, `fn_equiv_guard`). | 🤖 | 8/10 |
| [`lib/kernel/prelude.disp`](lib/kernel/prelude.disp) | The kernel barrel: opens `prelude.disp`, `list.disp`, `kernel.disp`, and `surface.disp`. | 🤖 | 8/10 |
| [`lib/claim.disp`](lib/claim.disp) | Claims and lenses: the honest search, the finite check, the lens law, implication between claims, counterexample-guided search. | 🤖 | 7/10 |

Earlier kernels live intact under [`archive/live-kernel/`](archive/live-kernel/) (the two-op trusted core) and [`archive/provenance-kernel/`](archive/provenance-kernel/) (the provenance-ledger kernel).

`.typ` files are [Typst](https://typst.app/) sources; prebuilt PDFs (`SYNTAX.pdf`, `archive/live-kernel/TYPE_THEORY.pdf`) sit alongside them.

## Repository Layout 🤖

```
src/                        -- the host implementation (TypeScript; an accelerator for the in-language spec)
  core/tree.ts              --   tree-calculus runtime: hash-consed trees, apply, tree_eq fast-path
  parse.ts                  --   tokenizer / parser (implements SYNTAX.typ)
  compile.ts, elab/         --   elaborator: surface AST -> trees (bracket abstraction, sugar, driver)
  eval/                     --   Session ABI + backend registry (eager / naive / rust / lambada)
  run.ts                    --   file runner / CLI

lib/                        -- the language, written in itself
  prelude.disp              --   the raw substrate: booleans, pairs, wait/fix, triage, tree_eq
  list.disp, machine.disp   --   lists; the evaluator's loop as a resumable value
  stream.disp, verdict.disp --   coalgebra streams, budgets and endings; Proved / Refuted / Open
  growth.disp               --   step counts on the machine, fitted exactly to polynomials, as checkable step bounds
  claim.disp                --   claims (witness vs. challenge), the honest search, lenses between claims
  confirm.disp              --   the symbolic confirmer: the machine run on unknown pieces, and one loop that runs each case, splits it by cases derived from a type's recognizer, and cites the hypothesis or a lemma; a function inside a value is judged the same way
  rules.disp                --   the rule lenses the floor stands on (three atoms, lemmas, two combinators), `judge`, which decides a value at a type with them alone, and its work list read back as data and as a lens
  trees.disp                --   every tree as a stream (a search source), and the Bool recognizer
  theories/                 --   the lens/`judge` theory ladder (affine, graded, cost, cube, MLTT, System T, classical, dialectica, finite, grammar) plus the older `judge`-based `checked.disp` hook
  kernel/                   --   the promoted evaluator-map kernel (`kernel.disp`, `surface.disp`, `prelude.disp`), checked stdlib (`std_*.disp`), verified rewrites (`verified_opt*.disp`), and test suites
  tests/                    --   substrate-level tests, the refuter, the confirmer's tests, the certified rewrites (claim.test.disp)

evaluators/                 -- alternative reduction backends (Rust: rust-eager, rust-ic-net; lambada peers), and the strand lattice (rust-ic-strands, on tree-net)
test/                       -- vitest harness + host unit tests
bench/                      -- evaluator benchmarks
research/                   -- research notes and unbuilt designs (OPTIMIZER.typ, CLAIMS_OVER_TREES.md, NATIVE_BOOTSTRAP_PLAN.typ)
archive/                    -- retired work kept for reference: live-kernel/, provenance-kernel/, type-system-goals/, superseded proposals (see archive/README.md)
editors/                    -- editor support (vscode-disp)
*.md, *.typ                 -- docs for what runs today: the kernel, the host pipeline, the evaluator (rated in the Documentation table above)
```

## Acknowledgments

Disp builds directly on Barry Jay's [tree calculus](https://github.com/barry-jay-personal/tree-calculus) and stands in the NuPRL tradition of types as predicates (a type is a program you run on a value; the verdict is the type check). [`FOUNDATIONS.md`](FOUNDATIONS.md) credits the full ancestry, from LCF and the Futamura projections to equality saturation and verifier-filtered neural search.

## License

This is licensed under the [Unlicense](LICENSE), i.e. it is dedicated to the public domain.
