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

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, evaluation budgets, `FAST_EQ` primitive.
- `src/parse.ts` — surface tokenizer/parser/bracket-abstraction. Handles `def`, `test`, `\x.`, juxtaposed application, `t` for leaf. Exposes `fast_eq` as a global to disp programs.
- `src/run.ts` — driver: load `.disp` file, run `test` declarations, assert tree equality.
- `examples/*.disp` — tree-native programs implementing the elaborator substrate (see ELABORATION_DESIGN.md "Current state" for inventory).
- `test/tree.test.ts` + `test/disp.test.ts` — vitest suites.

## Current state (as of 2026-04-13)

**Implemented (substrate)**: tree-calc runtime + surface parser + `wait`/`fix` recursion + tagged forms (V/H/App/Lam/Pi) + bind-trees (BE/BN/BApp/BLam/BPi) + `splice` + `normalize` + `infer` + `check` (Lam-vs-Pi descent + App via infer + H lookup + conversion). Most of these live tree-side in `examples/*.disp`, host-side only does runtime + parser + test harness. 105 tree-native test cases pass.

**Plan**: 3 phases ahead, see `ELABORATION_DESIGN.md` "Plan" section.
1. **Types-as-predicates kernel** — refactor so `apply(type, value)` IS the check (per `TREE_NATIVE_TYPE_THEORY.md`). `pred_of` derives the predicate form from the tagged form.
2. **Surface elaborator** — `\(x : T). body` and `(x : T) → R` syntax; auto-compute bind-trees from term structure; `def name : T = e` triggers check.
3. **Metavariables** — positional canonical metas + Miller-pattern unification.

**Sharp lessons from the substrate** (must respect when extending — see ELABORATION_DESIGN.md for detail):
- Recursive-call arg order under `wait`/`fix`: inner-binder-derived arg goes first.
- Strict-branch deferral: wrap each case as `\u. body` and apply after dispatch.
- `H` wraps its type, not its binder (departure from `BIND_TREE_NBE_IDEA.md` §3.4).
- Bind-trees are load-bearing data, not decoration — splice trusts them.

## Testing

`npm test` runs the vitest suite.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- Hash-consing is a load-bearing property: `fastEq` is O(1) tree identity, and anything that makes equivalent trees have different structure undermines it.
