# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting (kernel reified as a tree-calculus inhabitant, not just as a host function).

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase. Every design decision should be compatible; if it isn't, one of them changes, and changing the philosophy is the harder path.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — technical spec of the type theory (as currently implemented).
- [`ELABORATION_DESIGN.md`](ELABORATION_DESIGN.md) — proposed redesign of the elaboration pipeline: two universes (elab + runtime), tagged forms with explicit binders, `normalize`-based conversion via `fastEq`, metas as ctx entries. **Not yet implemented.** Intended to resolve the blocked cases below.
- [`NATIVE_TYPE_THEORY_ISSUES.md`](NATIVE_TYPE_THEORY_ISSUES.md) — open issues and blocked cases.

## Core discipline, in brief

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding before it ships. Before using any host-language feature (`Map`, `Set`, `try`/`catch`, mutation, exceptions for control flow) in such code, confirm that a tree-analog exists or is planned.

"We'll port it later" is forbidden. It's the pattern that stopped Lean, Coq, and Agda from bootstrapping their checkers. Our tree calculus is small enough (three constructors, five reduction rules) that this discipline is feasible; squandering it defeats the project's premise.

When in doubt, reread `DEVELOPMENT_PHILOSOPHY.md`.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, evaluation budgets.
- `src/elaborate.ts` — elaborator (surface → annotated tree → bare tree). Primary iteration site.
- `src/parse.ts` — tokenizer + recursive-descent parser → `SExpr`.
- `src/repl.ts`, `src/main.ts` — REPL and entry point.
- `types.disp` — type predicates and `piCheck`, compiled in bare mode as bootstrap.
- `prelude.disp` — standard library (Bool, Nat, arithmetic, eliminators).
- `test/elaborate.test.ts`, `test/typecheck-e2e.test.ts` — vitest suites.

## Current state (as of 2026-04)

- **Working**: base predicates (Bool, Nat, Tree), non-dependent Pi, simple dependent Pi, REPL, basic end-to-end type checking.
- **Blocked**: nested parameter applications (`{f x} -> f (f x)`, `compose`, `flip`, inline triages with multiple parameters). See `NATIVE_TYPE_THEORY_ISSUES.md` for the specific pattern.
- **Direction** (specified in `ELABORATION_DESIGN.md`, not yet implemented): separate elaboration universe (tagged forms with explicit binders, de Bruijn levels, metas) from runtime universe (bracket-abstracted SKI). Checking happens in the elab universe via `normalize` + `fastEq`. `erase` compiles to SKI only after checking passes. The current `types.disp` complexity (`sOrAsc`, `sCodomain`, cross-wire-format disambiguation) is a symptom of trying to make checker input = runtime output.

## Testing

`npm test` runs the vitest suite.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- The `piCheck` predicate should stay small. If a fix for a checking issue involves growing `piCheck`, consider whether the elaborator could produce a better-shaped input instead.
- Hash-consing is a load-bearing property: `fastEq` is O(1) tree identity, and anything that makes equivalent trees have different structure undermines it.
