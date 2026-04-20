# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting (kernel reified as a tree-calculus inhabitant, not just as a host function).

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase. Every design decision should be compatible; if it isn't, one of them changes, and changing the philosophy is the harder path.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — the semantics the object language commits to: types as predicates, fresh-hypothesis discipline, the evaluator interface, core type constructors, soundness invariants. Authoritative for anything touching the kernel.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape of `.disp` source files. Authoritative for anything touching the parser or the AST types in `src/ast.ts`.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline, name-resolution algorithm, error-reporting contract.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — reference notes from the removed prototype kernel: tagged-form encoding, bind trees, splice, context trees, meta substrate, dispatch shapes, general tree-calculus pitfalls. Not prescriptive; read when re-encoding the theory.
- `lib/{debruijn,ctxtree,semantic}/DESIGN.md` — per-backend NbE strategy notes. Predate the TYPE_THEORY sweep; treat as reference, not current spec.

## Core discipline, in brief

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding before it ships. Before using any host-language feature (`Map`, `Set`, `try`/`catch`, mutation, exceptions for control flow) in such code, confirm that a tree-analog exists or is planned.

"We'll port it later" is forbidden. It's the pattern that stopped Lean, Coq, and Agda from bootstrapping their checkers. Our tree calculus is small enough (three constructors, five reduction rules) that this discipline is feasible; squandering it defeats the project's premise.

When in doubt, reread `DEVELOPMENT_PHILOSOPHY.md`.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, evaluation budgets, `FAST_EQ` primitive.
- `src/ast.ts` — surface AST types. **Stale:** still has the `Core`/`Surface` tier split and `Use` in `Stmt`; needs to match the single-tier AST in `SYNTAX.typ` (`Use` is an expression kind; `Stmt = Let | Test`).
- `src/parse.ts` — surface tokenizer / parser / bracket-abstraction. **Stale:** implements the pre-revision surface (`def`, `elab`, `raw`, `\x.`). Rewrite to the current `SYNTAX.typ` grammar is the next work item.
- `src/elaborate.ts` — host-side surface elaborator. **Stale:** produced tagged forms for the removed kernel. Will be rewritten alongside `src/parse.ts`.
- `src/run.ts` — driver: loads a `.disp` file, runs tests. Will evolve into a pure compiler driver per `COMPILATION.typ`.
- `lib/{debruijn,ctxtree,semantic}/DESIGN.md` — per-backend NbE strategy notes (reference only; see above).
- `test/tree.test.ts` + `test/disp.test.ts` — vitest suites. `test/parser.test.ts` is an in-progress rewrite against the new grammar.

## Current state (as of 2026-04-20)

Just completed a type-theory cleanup sweep. State:

- **`TYPE_THEORY.typ` landed.** Canonical spec for types-as-predicates, the evaluator interface (`apply` / `fresh_hyp` / `eq` / `pred` / `quote`), fresh-hypothesis discipline, core type constructors, worked examples, and soundness invariants.
- **Old kernel deleted.** `lib/predicates.disp`, all `impl.disp` stubs, and `lib/suite/main.disp` are gone. Algorithmic content preserved in `KERNEL_DESIGN.md` for reference.
- **Surface-syntax revision landed in docs** (`SYNTAX.typ`, `COMPILATION.typ`). `src/ast.ts`, `src/parse.ts`, `src/elaborate.ts`, `src/run.ts` still reflect the pre-revision design — all flagged stale above.
- **No working end-to-end toolchain right now.** `npm test` against `src/` runs the old pipeline; nothing compiles against the new spec.

Next-step order (proposal, not commitment):
1. Rewrite `src/ast.ts` to match `SYNTAX.typ` (mechanical).
2. Rewrite `src/parse.ts` to emit the new AST.
3. Pick an initial backend representation from `KERNEL_DESIGN.md` / per-backend DESIGN.mds; implement against the evaluator interface in `TYPE_THEORY.typ`.
4. Rebuild the test harness.

## Sharp lessons from the substrate

General tree-calculus kernel pitfalls that survive the cleanup — apply to any future implementation:

- **Recursive-call arg order under `wait` / `fix`**: inner-binder-derived arg goes first. Outer-binder-derived args cause eager `wait` firing and compile-time divergence.
- **Strict-branch deferral**: `triage` evaluates every branch eagerly. Wrap each case as `\u. body` and apply after dispatch to prevent eager recursion.
- **Hash-consing is load-bearing**: `fast_eq` is O(1) tree identity. Anything that makes equivalent trees have different structure undermines the type system's equality.

Encoding-specific lessons (tagged forms, bind trees, H-token conventions, ctx-tree NbE, etc.) moved to `KERNEL_DESIGN.md`.

## Testing

`npm test` runs the vitest suite. Currently tests the old pipeline; will need updating as the new parser / elaborator land.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- `fast_eq` is O(1) tree identity; protect hash-cons by not introducing structural variants of semantically-equal trees.
- When proposing design changes, state which document(s) they affect (`TYPE_THEORY.typ` / `SYNTAX.typ` / `COMPILATION.typ` / a backend DESIGN.md) and whether the theory's invariants move.
