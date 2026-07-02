# Disp

Disp is a self-verified dependently-typed programming language built on the self-reflective [tree calculus](https://github.com/barry-jay-personal/tree-calculus). Because the underlying computation model is self-reflective, types in disp can be written as just functions that take in other functions and return true (if it typechecks) or false. The long-term goals are to:
 - Create the ultimate programming language that can essentially re-create and transpile all other programming languages into it.
 - Create a "decentralized" programming language, where development and new features (syntax, types) can be used independently by different programmers and automatically translated between.
 - Use disp to create a general-purpose program optimizer, i.e. a program that takes a type (predicate, or more generally, a loss function) and returns something that maximally satisfies the function. Then write a type that specifies the class of such functions and have it create itself, for the goal of creating interpretable (symbolic) recursive self-improving optimizers.
 - Create accurate models of various detail for hardware and have an optimizer optimize for the model.

## Why Disp?

Rust is fast, but you can't verify your programs. In lean you can verify your programs, but with much effort and its usually not very fast. In all languages, you usually can't have an optimizer take a high-level specification and satisfy it. The closest thing we have to this is claude-code but its pretty expensive in practice to do at a large scale (and of course you would need a language that supports formal specifications first, and Lean does not make this as easy as it could be).

In order to create a language where all you are required to do is define the high-level constraints, as detailed and rigorous as you require, and then it automatically derives an implementation, you need a language where the constraint-checking itself is optimized and ideally modular. This is the problem disp solves. 

## Disp by example

This section is one file, top to bottom, and it loads and passes as-is. A `test lhs = rhs` passes when both sides reduce to the identical tree, so everything this section claims is machine-checked.

```disp
open use "../prelude.disp"          // raw combinators (tree_eq, is_fork, succ, ...)
open use "../kernel/prelude.disp"   // the entire type system, an ordinary library
open use "../std/nat/ops.disp"      // double
```

### Everything is a tree

```disp
// Disp compiles to tree calculus: three rewrite rules over binary trees grown
// from a single leaf `t`. Numbers, lambdas, types, proofs, and the type
// checker itself are all such trees. Some of them are small:

// from the prelude:  TT : Bool := {m, ct, cf} -> ct
test TT = t t (t t)                             // "true" is this four-leaf tree
test tree_eq 3 (succ (succ (succ zero))) = TT   // 3 is sugar, and the trees are identical

// Elaboration is deterministic, so equal programs are literally the same tree
// (the checker backends hash-cons this down to a pointer comparison). And
// since trees are data, programs can take programs apart. The rest of the
// language is built on that, and the kernel exists to police it.
```

### Syntax is sugar

```disp
// Surface constructs expand to library calls. Expansions are trees, so pin them:

test tree_eq ({r} -> r.x) ({r} -> r (acc "x")) = TT   // projection is application

let Point = { x : Nat, y : Nat }
let PointCells = Telescope (t (proj_cell "x" Nat) ({_x} -> t (proj_cell "y" Nat) ({_y} -> t)))
test tree_eq Point PointCells = TT                    // the literal is exactly the library call

// Telescope is the one former behind functions, pairs, records, and unit;
// sums are its dual; recursion is one more cell kind. A new type former is
// library code, not kernel surgery (NEGATIVE_TYPES.md).
```

### Checking is running

```disp
// A type is a predicate. To check data, apply the type. Raw application,
// no checker in sight:

test Nat 3 = Ok TT
test Nat TT = Ok FF

// Structure runs too: b's recipe is evaluated during the check.

let TDs = { a : Nat, b := double a }
test typecheck TDs { a := 2; b := 4 } = Ok TT
test param_apply TDs { a := 2; b := 5 } = Ok FF   // ran double 2, compared, rejected
```

### Functions need a promise

```disp
// This annotation claims something about infinitely many inputs:

quadruple : Nat -> Nat := {n} -> double (double n)
test quadruple 3 = 12

// No amount of running visits them all, and raw application, which worked
// for data above, yields no verdict:

test tree_eq ((Pi Nat ({_} -> Nat)) quadruple) (Ok TT) = FF   // raw: nothing
test param_apply (Pi Nat ({_} -> Nat)) quadruple = Ok TT      // dispatcher: yes
test param_apply (Pi Nat ({_} -> Nat)) ({n} -> TT) = Ok FF    // wrong codomain: no

// The difference: checking a function requires minting a hypothesis, a fresh
// opaque tree carrying a type and nothing else: a promise that a Nat will be
// here. A type that could forge promises could forge evidence, so minting is
// kernel operation #1 (bind_hyp), and kernel operations fire only under the
// kernel's dispatcher, param_apply. Raw application never reaches the kernel:
// no mint, no verdict. Under the dispatcher, Pi mints the promise, applies
// the body to it, and watches what comes out.
```

### What a promise can do

```disp
// So quadruple's body ran double on a value with no digits, and double got
// stuck. That stuckness is the mechanism. The only legal move on a promise is
// an observation (apply it, project it, eliminate it), and every observation
// routes through kernel operation #2, hyp_reduce: read the promise's stored
// type, forward the observation to that type's `respond`. A respond has
// exactly two moves, and you can watch both:

let hN = make_hyp Nat 0
test param_apply Nat hN = Ok TT                   // a promise of a Nat counts as a Nat

let hPi = make_hyp (Pi Nat ({_} -> Bool)) 0
test neutral_type (param_apply hPi zero) = Bool   // Extend: stuck, at the codomain type

let Pt = { a : Nat, b := succ a }
let hPt = make_hyp Pt 0
test neutral_type (param_apply hPt (acc "a")) = Nat                               // Extend: opaque field
test tree_eq (param_apply hPt (acc "b")) (succ (param_apply hPt (acc "a"))) = TT  // Reduce: derived field
                                                                                  // computes through it

// And this is what double did: its recursor parked on the promise as a stuck
// elimination, typed by the motive. quadruple's body finished as a stuck tree
// of type Nat, the codomain matched, and the definition was accepted. This
// technique is called neutral evaluation.

test is_neutral (nat_rec ({_} -> Bool) TT ({n, rec} -> FF) hN) = TT

// Reading the promise's raw shape is missing from that list on purpose. The
// walker refuses the question instead of answering it, since either answer
// would leak information the promise does not contain:

test param_apply (Pi Nat ({_} -> Bool)) ({x} -> is_fork x) = Err   // illegal question

// Everything above hangs on promises staying unforgeable and uninspectable.
// bind_hyp, hyp_reduce, and the dispatcher enforce that, and they are the
// entire trusted core of disp (SEALING.md, lib/kernel/engine.disp). Pi,
// records, Nat, and equality are library code consuming them.
```

### Who checks the types?

```disp
// A respond answers every observation under every binder, so a wrong respond
// breaks every check downstream. Responds get checked in two ways. Types are
// values with a recognizable shape, so the universe is a predicate like any
// other, and it accepts itself:

test typecheck Type Nat = Ok TT
test typecheck Type zero = Ok FF   // not a type
test typecheck Type Type = Ok TT   // the universe passes its own checker

// The deeper check is behavioral. GoodRespond aims the promise machinery at
// the type itself: its probes mint hypotheses and fire observations through
// hyp_reduce at a candidate respond, comparing answers against the type's
// constructors. A respond can lie in two directions, and both are caught:

let MyNat = Coproduct [pair "z" [], pair "s" [Rec]]   // a home-made Nat: zero and successor
test typecheck Type MyNat = Ok TT

let resp_of = {T} -> (type_meta T).respond (type_meta T).recognizer_params
test verify_good MyNat (resp_of MyNat) = Ok TT                    // its real respond: honest
test verify_good MyNat (inductive_respond unit_witness) = Ok FF   // waves junk through
test verify_good MyNat (inert_respond unit_witness) = Ok FF       // refuses everything

// So the checker checks the checkers, using the same two kernel operations
// it uses on everything else. This loop is the "self-verified" in the first
// sentence of this README.
```

The TypeScript and Rust hosts only accelerate these in-language definitions and are validated bit-for-bit against them; five evaluator backends sit behind one ABI and must agree byte-identically, so no single evaluator has to be trusted ([`EVALUATOR.md`](EVALUATOR.md)). Each decision above is argued against its historical precedents in [`FOUNDATIONS.md`](FOUNDATIONS.md).

## Status

Working today:

- The full pipeline: parser, elaborator, and tree-calculus evaluation (`src/`), with the surface grammar specified in [`SYNTAX.typ`](SYNTAX.typ).
- The two-op kernel and the library type system on top of it (`lib/kernel/`), including the self-inhabiting strict universe and automatic verification of typed module exports through the kernel.
- A standard library (`lib/std/`): naturals, lists, options, results, pairs, sets, and streams, with generic derived operations (folds, recursors, functorial maps) read off type structure rather than hand-written per type.
- About 1,000 object-language tests across 50 files (`lib/tests/`), including soundness tests that pin what the checker must reject, plus host-level parser and runtime unit tests (`test/`).
- Rust evaluator backends (`evaluators/`): `rust-eager`, the fast checker backend, roughly 2x the TypeScript oracle on the full suite; and `rust-ic-net` M0-M2, the materialized interaction net, sequential, with parallel reduction under cargo tests.
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
