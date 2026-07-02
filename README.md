# Disp

Disp is a self-verified dependently-typed programming language built on the self-reflective [tree calculus](https://github.com/barry-jay-personal/tree-calculus). Because the underlying computation model is self-reflective, types in disp can be written as just functions that take in other functions and return true (if it typechecks) or false. The long-term goals are to:
 - Create the ultimate programming language that can essentially re-create and transpile all other programming languages into it.
 - Create a "decentralized" programming language, where development and new features (syntax, types) can be used independently by different programmers and automatically translated between.
 - Use disp to create a general-purpose program optimizer, i.e. a program that takes a type (predicate, or more generally, a loss function) and returns something that maximally satisfies the function. Then write a type that specifies the class of such functions and have it create itself, for the goal of creating interpretable (symbolic) recursive self-improving optimizers.
 - Create accurate models of various detail for hardware and have an optimizer optimize for the model.

## Why Disp?

Rust is fast, but you can't verify your programs. In lean you can verify your programs, but with much effort and its usually not very fast. In all languages, you usually can't have an optimizer take a high-level specification and satisfy it. The closest thing we have to this is claude-code but its pretty expensive in practice to do at a large scale (and of course you would need a language that supports formal specifications first, and Lean does not make this as easy as it could be).

In order to create a language where all you are required to do is define the high-level constraints, as detailed and rigorous as you require, and then it automatically derives an implementation, you need a language where the constraint-checking itself is optimized and ideally modular. This is the problem disp solves. 

## Example

```disp
open use "../kernel/prelude.disp"   // the type system, an ordinary library
open use "../std/nat/ops.disp"      // double, pred, is_zero

// A definition: `name : Type := value`. A lambda is `{args} -> body`.
quadruple : Nat -> Nat := {n} -> double (double n)

// `test` declarations run at load time: both sides must reduce to the
// same tree.
test quadruple 3 = 12

// A record type, written like a value. Later fields may depend on earlier
// ones, and `b := double a` is a DERIVED field whose value is computed.
let TDs = { a : Nat, b := double a }

// Types are programs: checking a value is running the type on it.
test typecheck TDs { a := 2; b := 4 } = Ok TT
test param_apply TDs { a := 2; b := 5 } = Ok FF   // 5 is not double 2

// `build` fills derived fields in for you:
test build TDs { a := 3 } = { a := 3; b := 6 }

// Full dependency: the TYPE of field `x` is the VALUE of field `T`.
let TDep = { T : Type, x : T }
test typecheck TDep { T := Nat; x := 3 } = Ok TT
test typecheck TDep { T := Bool; x := FF } = Ok TT
```

Types are first-class values that can be bound with `let` and passed to functions. The record former above is the same one that provides function types, pairs, and the unit type. The entire type system arrived through the `open use` of a library file.

## Design

Each of these is a deliberate decision, argued with precedents and failure modes in [`FOUNDATIONS.md`](FOUNDATIONS.md). Implementation detail lives in the linked documents.

1. **Programs are trees; equality is structural.** Type checking constantly compares terms for equality, and in disp that comparison asks whether two terms are literally the same tree (`tree_eq`). Deterministic elaboration guarantees that the same program produces the same tree, and the checker backends hash-cons every term, which turns the comparison into an O(1) pointer check. The O(1) part is a backend property, not a calculus property: the interaction-net backend drops hash-consing deliberately so the optimizer can attribute cost per candidate. ([`KERNEL_DESIGN.md`](KERNEL_DESIGN.md))
2. **Types are predicates.** A type is a function from raw trees to yes/no, in the NuPRL tradition. There is no separate language of typed terms. ([`TYPE_THEORY.typ`](TYPE_THEORY.typ))
3. **The object language is the specification.** Every checker and elaborator component has an in-language definition. Host fast paths must produce bit-identical results and are validated against the in-language reference. The one live native fast path is `tree_eq`.
4. **A small sealed kernel.** The trusted center is two operations (`hyp_reduce`, `bind_hyp`) plus one dispatcher (`param_apply`) that polices reflection, in the LCF tradition: untrusted library code can consume evidence of well-typedness but cannot forge or inspect it. Everything else, including Pi, Sigma, Nat, Bool, equality, and the universe, is ordinary library code in `lib/kernel/`. The universe is metacircular: `Type` is a type that passes its own checker. ([`SEALING.md`](SEALING.md), `lib/kernel/engine.disp`)
5. **One negative former, one positive former.** Function types, pair types, records, and the unit type are a single telescope former differing only in a per-field observation; sums are its dual (`Coproduct`); recursion and corecursion are cells on the same walker rather than special-cased fixpoint machinery. New type formers plug in as new cell operations with no changes to the core walk. ([`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md))
6. **The substrate stays pure; effects are values.** An effect is data describing an action, interpreted by handlers, with a single impure driver at the program boundary. (Design: [`TYPE_THEORY.typ`](TYPE_THEORY.typ) §15.)
7. **Many evaluators, one truth.** Five reduction backends sit behind one `Session` ABI: the TypeScript reference oracle, a naive honesty backend, a fast Rust reducer, a parallel interaction-net substrate for the future optimizer, and external peers. They are held to byte-identical agreement as a standing differential oracle. No single evaluator is trusted; agreement is checked, not assumed. ([`EVALUATOR.md`](EVALUATOR.md))

## Status

Working today:

- The full pipeline: parser, elaborator, and tree-calculus evaluation (`src/`), with the surface grammar specified in [`SYNTAX.typ`](SYNTAX.typ).
- The two-op kernel and the library type system on top of it (`lib/kernel/`), including the self-inhabiting strict universe and automatic verification of typed module exports through the kernel.
- A standard library (`lib/std/`): naturals, lists, options, results, pairs, sets, and streams, with generic derived operations (folds, recursors, functorial maps) read off type structure rather than hand-written per type.
- About 1,000 object-language tests across 50 files (`lib/tests/`), including soundness tests that pin what the checker must reject, plus host-level parser and runtime unit tests (`test/`).
- Rust evaluator backends (`evaluators/`): `rust-eager`, the fast checker backend, roughly 2x the TypeScript oracle on the full suite; and `rust-ic-net` M0-M2, the materialized interaction net, sequential and parallel-under-cargo.
- A first end-to-end slice of the optimizer story: machine-checked equality witnesses licensing real rewrites (map fusion among them) past syntactic equality, with zero kernel changes (`lib/tests/opt_q1_*.test.disp`).

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
src/                        -- the host implementation (TypeScript; an accelerator, not the spec)
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
