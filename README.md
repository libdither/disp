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

## AI disclosure

Most of this repository was written by an AI. The current implementation (2026 onward) is a collaboration with Anthropic's Claude models running in Claude Code: nearly all of the TypeScript host, the kernel and standard library, the tests, the Rust evaluator backends, and the documentation, including most of this README, were drafted by Claude and landed through the maintainer's review. The commit history does not mark the split.

What is not AI-written: the goals and the judgment. Disp started in 2021 as years of solo prototypes (lambda calculi over a content-addressed store, in Rust), and the vision in [`GOALS.md`](GOALS.md), the choice of what to attempt next, and the accept or reject call on every design are human. So is the editing hand on the prose.

"AI-written" is accurate but incomplete for the designs themselves. The two-op kernel, types as raw predicates, telescopes as the one negative former: these came out of long design dialogues in which the human supplies goals, constraints, and taste, the AI drafts, argues, and implements, and each side regularly kills the other's proposals. For most of the load-bearing decisions neither party could honestly claim sole authorship. That seems to be the real shape of AI collaborative design at this scale: not code generation and not autocomplete, but a fast, well-read collaborator whose ideas still have to survive a human's sense of what the project is for.

The working discipline is the project's own thesis applied to its construction: trust the checker, not the author. AI-written code fails in characteristic ways (confident overstatement, quiet drift between claim and implementation), so nothing here rests on trusting its provenance. Every claim in this README is a machine-checked test, the kernel verifies the library built on top of it, and the evaluator backends must agree byte for byte. Disp is an attempt at a language in which machine-generated code can be verified rather than trusted, built by the kind of author it is designed to police. Errors that survive all of that are the maintainer's responsibility.

## Disp by example

This section is one file, top to bottom, and it loads and passes as-is. A `test lhs = rhs` passes when both sides reduce to the identical tree, so everything this section claims is machine-checked. Code blocks are tagged `rust` only to borrow GitHub's syntax highlighting; the code is disp.

<!-- Literate-code invariant: the blocks below concatenate to one .disp file that must pass.
     After editing them, re-verify with:
       awk '/^```rust$/{f=1;next} /^```$/{f=0;next} f' README.md > lib/tests/readme_check_tmp.disp
       npm run disp -- lib/tests/readme_check_tmp.disp && rm lib/tests/readme_check_tmp.disp -->

```rust
open use "../kernel/prelude.disp"   // the entire type system, an ordinary library
                                    // (re-exports the raw combinators: tree_eq, is_fork, succ, ...)
open use "../std/nat/ops.disp"      // double
```

### Everything is a tree

```rust
// Disp compiles to the tree calculus: five rewrite rules over binary trees
// grown from a single leaf `t`. Numbers, lambdas, types, proofs, and the type
// checker itself are all such trees. Some of them are small:

// from the prelude:  true : Bool := {m, ct, cf} -> ct
test true = t t (t t)                             // the four-leaf tree behind the name
test tree_eq 3 (succ (succ (succ zero))) = true   // 3 is sugar, and the trees are identical

// Three of the five rules dispatch on the shape of a tree (leaf, stem, or
// fork), so case analysis over arbitrary data is a rewrite rule, not library
// code. This is the self-reflection everything else builds on:

let shape_of := t (t "leaf" ({u} -> "stem")) ({u, v} -> "fork")
test shape_of t = "leaf"
test shape_of (t t) = "stem"
test shape_of (t t t) = "fork"

// Elaboration is deterministic, so equal programs are literally the same tree
// (the checker backends hash-cons this down to a pointer comparison). Trees
// are data, so programs can take programs apart, exactly as shape_of just
// did. The rest of the language is built on that power, and the kernel
// exists to police it.
```

### Syntax is sugar

```rust
// Surface constructs expand to library calls. Expansions are trees, so pin them:

test tree_eq ({r} -> r.x) ({r} -> r (acc "x")) = true   // projection is application

let Point := { x : Nat, y : Nat }
let PointCells := Telescope (t (proj_cell "x" Nat) ({_x} -> t (proj_cell "y" Nat) ({_y} -> t)))
test tree_eq Point PointCells = true                    // the literal is exactly the library call

// Telescope is the one former behind functions, pairs, records, and unit;
// sums are its dual; recursion is one more cell kind. A new type former is
// library code, not kernel surgery (NEGATIVE_TYPES.md).
```

### Checking is running

```rust
// A type is a predicate. To check data, apply the type. Raw application,
// no checker in sight:

test Nat 3 = Ok true
test Nat true = Ok false

// Structure runs too: b's recipe is evaluated during the check.

let TDs := { a : Nat, b := double a }
test TDs { a := 2; b := 4 } = Ok true
test TDs { a := 2; b := 5 } = Ok false   // ran double 2, compared, rejected
```

### Functions need a promise

```rust
// This annotation claims something about infinitely many inputs:

quadruple : Nat -> Nat := {n} -> double (double n)
test quadruple 3 = 12

// No amount of running visits them all, and raw application, which worked
// for data above, yields no verdict:

let NatToNat := Pi Nat ({_} -> Nat)                // the annotation above, desugared
test tree_eq (NatToNat quadruple) (Ok true) = false    // raw: no verdict comes back
test param_apply NatToNat quadruple = Ok true       // the dispatcher: yes
test param_apply NatToNat ({n} -> true) = Ok false     // wrong codomain: no

// The difference: checking a function requires minting a hypothesis, a fresh
// opaque tree carrying a type and nothing else: a promise that a Nat will be
// here. A type that could forge promises could forge evidence, so minting is
// kernel operation #1 (bind_hyp), and kernel operations fire only under the
// kernel's dispatcher, param_apply. Raw application never reaches the kernel:
// no mint, no verdict. Under the dispatcher, Pi mints the promise, applies
// the body to it, and watches what comes out.
```

### What a promise can do

```rust
// So quadruple's body ran double on a value with no digits, and double got
// stuck. That stuckness is the mechanism. The only legal move on a promise is
// an observation (apply it, project it, eliminate it), and every observation
// routes through kernel operation #2, hyp_reduce: read the promise's stored
// type, forward the observation to that type's `respond`. A respond has
// exactly two moves, and you can watch both:

let hN := make_hyp Nat 0                           // mint one by hand (the 0 is a fresh id)
test param_apply Nat hN = Ok true                   // a promise of a Nat counts as a Nat

let hPi := make_hyp (Pi Nat ({_} -> Bool)) 0
test neutral_type (param_apply hPi zero) = Bool   // Extend: stuck, at the codomain type

let Pt := { a : Nat, b := succ a }
let hPt := make_hyp Pt 0
test neutral_type (param_apply hPt (acc "a")) = Nat                               // Extend: opaque field
test tree_eq (param_apply hPt (acc "b")) (succ (param_apply hPt (acc "a"))) = true  // Reduce: derived field
                                                                                  // computes through it

// And this is what double did: its recursor parked on the promise as a stuck
// elimination, typed by the motive. quadruple's body finished as a stuck tree
// of type Nat, the codomain matched, and the definition was accepted. This
// technique is called neutral evaluation.

test is_neutral (nat_rec ({_} -> Bool) true ({n, rec} -> false) hN) = true

// Reading the promise's raw shape is missing from that list on purpose. The
// walker refuses the question instead of answering it, since either answer
// would leak information the promise does not contain:

test param_apply (Pi Nat ({_} -> Bool)) ({x} -> is_fork x) = Err   // illegal question

// Everything above hangs on promises staying unforgeable and uninspectable.
// bind_hyp, hyp_reduce, and the dispatcher enforce that, and they are the
// entire trusted core of disp (SEALING.md, lib/kernel/engine.disp). Pi,
// records, Nat, and equality are library code consuming them.
```

### Proving is running

```rust
// Propositions are types too. An equality type has exactly one proof value,
// and it is the leaf itself:

test refl = t

// Whether refl proves an equation is decided by running both sides. Raw
// application is back, because proofs are data:

test (Eq Nat (double 2) 4) refl = Ok true   // double 2 and 4 meet at the same tree
test (Eq Nat 3 4) refl = Ok false

// A statement about every Nat is a Pi into a proposition, so checking it
// takes one promise. Here is a theorem in surface form, the kernel's verdict
// on it, and a false statement refl cannot prove (the stuck ends differ):

n_eq_n : {n : Nat} -> Eq Nat n n := {n} -> refl
test param_apply (Pi Nat ({n} -> Eq Nat n n)) n_eq_n = Ok true
test param_apply (Pi Nat ({n} -> Eq Nat n (succ n))) ({n} -> refl) = Ok false
```

### Who checks the types?

```rust
// A respond answers every observation under every binder, so a wrong respond
// breaks every check downstream. Responds get checked in two ways. Types are
// values with a recognizable shape, so the universe is a predicate like any
// other, and it accepts itself:

test param_apply Type Nat = Ok true
test param_apply Type zero = Ok false   // not a type
test param_apply Type Type = Ok true   // the universe passes its own checker

// The deeper check is behavioral. GoodRespond aims the promise machinery at
// the type itself: its probes mint hypotheses and fire observations through
// hyp_reduce at a candidate respond, comparing answers against the type's
// constructors. A respond can lie in two directions, and both are caught:

let MyNat := Coproduct [pair "z" [], pair "s" [Rec]]   // a home-made Nat: zero and successor
test param_apply Type MyNat = Ok true

// read a type's respond out of its metadata:
let resp_of := {T} -> (type_meta T).respond (type_meta T).recognizer_params
test verify_good MyNat (resp_of MyNat) = Ok true                    // its real respond: honest
test verify_good MyNat (inductive_respond unit_witness) = Ok false   // waves junk through
test verify_good MyNat (inert_respond unit_witness) = Ok false       // refuses everything

// So the checker checks the checkers, using the same two kernel operations
// it uses on everything else. This loop is the "self-verified" in the first
// sentence of this README.
```

The TypeScript and Rust hosts only accelerate these in-language definitions and are validated against them. Five evaluator backends sit behind one ABI and must agree byte for byte, so no single evaluator has to be trusted ([`EVALUATOR.md`](EVALUATOR.md)). Each decision above is argued against its historical precedents in [`FOUNDATIONS.md`](FOUNDATIONS.md).

## Status

Working today:

- The full pipeline: parser, elaborator, and tree-calculus evaluation (`src/`), with the surface grammar specified in [`SYNTAX.typ`](SYNTAX.typ).
- The two-op kernel and the library type system on top of it (`lib/kernel/`), including the self-inhabiting strict universe and automatic verification of typed module exports through the kernel.
- A standard library (`lib/std/`): naturals, lists, options, results, pairs, sets, and streams, with generic derived operations (folds, recursors, functorial maps) read off type structure rather than hand-written per type.
- About 1,000 object-language tests across 50 files (`lib/tests/`), including soundness tests that pin what the checker must reject, plus host-level parser and runtime unit tests (`test/`).
- Rust evaluator backends (`evaluators/`): `rust-eager`, the fast checker backend, about twice as fast as the TypeScript oracle on the full suite; and `rust-ic-net` M0-M2, the materialized interaction net, sequential, with parallel reduction under cargo tests.
- A first end-to-end slice of the optimizer: machine-checked equality witnesses licensing real rewrites (map fusion among them) past syntactic equality, with zero kernel changes (`lib/tests/opt_q1_*.test.disp`).

Designed but not built: the optimizer itself ([`OPTIMIZER.typ`](OPTIMIZER.typ)), effects as a library, cost as a typing-level resource, cubical path types, and the neural proposer. The open research risks, most sharply whether a decidable fragment of behavioral equivalence is rich enough to license the rewrites an optimizer needs, are catalogued as falsifiable questions in [`FOUNDATIONS.md`](FOUNDATIONS.md) §V.

## Getting Started

Requires Node.js. The Rust backends are optional; the test suite runs without a Rust toolchain.

```shell
npm install
npm test                                  # host + object-language suites (vitest)
npm run disp -- lib/tests/nat.test.disp   # run a single .disp file
npm run disp -- --evaluator=rust-eager lib/tests/nat.test.disp   # pick a backend (if built)
```

A `.disp` file is a sequence of definitions and `test` declarations. Running a file elaborates it and reports test results; a test passes when both sides reduce to the identical tree.

Suggested reading order:

1. [`FOUNDATIONS.md`](FOUNDATIONS.md): what disp is attempting, the lineage of every design piece, and the risk assessment. Written for a general reader.
2. [`GOALS.md`](GOALS.md): the original goal statement.
3. `lib/tests/`: the language, demonstrated. `nat.test.disp`, `record.test.disp`, and `telescope.test.disp` are good first files.
4. [`SYNTAX.typ`](SYNTAX.typ) and [`TYPE_THEORY.typ`](TYPE_THEORY.typ): the formal surface grammar and type-theory spec.
5. `lib/kernel/`: the type system's source. Reading order: `cut`, `engine`, `cells`, `base`, `positive`, `generic`, `universe`.

## Documentation

| Document | What it is |
|---|---|
| [`GOALS.md`](GOALS.md) | The long-term vision: neural-guided synthesis, self-improving optimizer. |
| [`FOUNDATIONS.md`](FOUNDATIONS.md) | Every design piece's precedent, why prior attempts stalled, disp's bet, and the make-or-break questions. |
| [`TYPE_THEORY.typ`](TYPE_THEORY.typ) | Authoritative type-theory spec: the two-op kernel, manifest contracts, library types, validators. |
| [`SYNTAX.typ`](SYNTAX.typ) | Surface grammar and AST. Authoritative for the parser. |
| [`COMPILATION.typ`](COMPILATION.typ) | Parse, elaborate, emit pipeline. |
| [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) | Tree-calculus implementation idioms: wait/fix, signatures, neutrals, bracket abstraction. |
| [`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) | Why function types, pairs, records, and unit are one telescope former; the cell/walker architecture. |
| [`EVALUATOR.md`](EVALUATOR.md) | The reduction-backend subsystem: the `Session` ABI, the five backends, the differential-oracle discipline. |
| [`OPTIMIZER.typ`](OPTIMIZER.typ) | Unified design for the verified self-improving optimizer (unbuilt). |
| [`SEALING.md`](SEALING.md) | The kernel's trust discipline as generative sealing and noninterference. |
| [`CLAUDE.md`](CLAUDE.md) | Working context: code layout, implementation status, known workarounds. |
| `research/` | Deeper research notes (interaction combinators, equality for verified optimization, and more). |

`.typ` files are [Typst](https://typst.app/) sources; prebuilt PDFs (`TYPE_THEORY.pdf`, `SYNTAX.pdf`, `COMPILATION.pdf`) sit alongside them.

## Repository Layout

```
src/                        -- the host implementation (TypeScript; an accelerator for the in-language spec)
  core/tree.ts              --   tree-calculus runtime: hash-consed trees, apply, tree_eq fast-path
  parse.ts                  --   tokenizer / parser (implements SYNTAX.typ)
  compile.ts, elab/         --   elaborator: surface AST -> trees (bracket abstraction, sugar, driver)
  eval/                     --   Session ABI + backend registry (eager / naive / rust / lambada)
  run.ts                    --   file runner / CLI

lib/                        -- the language, written in itself
  prelude.disp              --   fundamental combinators (booleans, pairs, wait/fix, triage)
  kernel/                   --   the type system: 7 fragments around a 2-op trusted core
  std/                      --   standard library (nat, list, option, result, set, stream, ...)
  tests/                    --   ~1,000 object-language tests, 50 files

evaluators/                 -- alternative reduction backends (Rust: rust-eager, rust-ic-net; lambada peers)
test/                       -- vitest harness + host unit tests
bench/                      -- evaluator benchmarks
research/                   -- research notes and designs
```

## Acknowledgments

Disp builds directly on Barry Jay's [tree calculus](https://github.com/barry-jay-personal/tree-calculus) and stands in the NuPRL tradition of types as predicates. [`FOUNDATIONS.md`](FOUNDATIONS.md) credits the full ancestry, from LCF and the Futamura projections to equality saturation and verifier-filtered neural search.

## License

Public domain, under the [Unlicense](LICENSE).
