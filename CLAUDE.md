# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting (kernel reified as a tree-calculus inhabitant, not just as a host function).

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase. Every design decision should be compatible; if it isn't, one of them changes, and changing the philosophy is the harder path.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — core idea: trees as S/K/Triage combinators, types as executable predicates, Pi as partially-applied `PiCheck`, hash-cons identity as type equality.
- [`ELABORATION_DESIGN.md`](ELABORATION_DESIGN.md) — operational spec: two universes (elab + runtime), tagged forms with explicit binders, `normalize`-based conversion via `fastEq`, metas as ctx entries, `erase` to SKI after checking.

## Core discipline, in brief

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding before it ships. Before using any host-language feature (`Map`, `Set`, `try`/`catch`, mutation, exceptions for control flow) in such code, confirm that a tree-analog exists or is planned.

"We'll port it later" is forbidden. It's the pattern that stopped Lean, Coq, and Agda from bootstrapping their checkers. Our tree calculus is small enough (three constructors, five reduction rules) that this discipline is feasible; squandering it defeats the project's premise.

When in doubt, reread `DEVELOPMENT_PHILOSOPHY.md`.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, evaluation budgets. The only source file.
- `test/tree.test.ts` — vitest suite covering the runtime.

The elaborator, surface parser, REPL, and kernel predicates were deliberately deleted (commit in git history) and are to be rebuilt against `ELABORATION_DESIGN.md`. Do not resurrect the old shapes.

## Current state (as of 2026-04-13)

- **Implemented**: tree calculus runtime (hash-consing, eager `apply`, budgets). Nothing else.
- **Next**: build the elaboration universe per `ELABORATION_DESIGN.md` — `normalize` first (as a tree program, then TS mirror), then `unify`, then `check`/`infer`, then `erase`, then a new surface parser targeting tagged forms.

## Testing

`npm test` runs the vitest suite.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- Hash-consing is a load-bearing property: `fastEq` is O(1) tree identity, and anything that makes equivalent trees have different structure undermines it.
