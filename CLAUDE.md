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
- `src/elaborate.ts` — host-side surface elaborator. Mirrors `examples/predicates.disp`'s tagged-form encoding so output is consumable by `pred_of` and `erase`. `compute_bind` walks elaborated body looking for the H-marker introduced by `\(x : T)`; `replaceMarker` swaps tokens for V before emitting `mkLam`/`mkPi`.
- `src/run.ts` — driver: load `.disp` file, run `test` declarations, assert tree equality.
- `examples/*.disp` — tree-native programs implementing the elaborator substrate (see ELABORATION_DESIGN.md "Current state" for inventory).
- `test/tree.test.ts` + `test/disp.test.ts` — vitest suites.

## Current state (as of 2026-04-13)

**Implemented (substrate + Phase 1 + Phase 2)**: tree-calc runtime + surface parser + `wait`/`fix` recursion + tagged forms (V/H/App/Lam/Pi) + bind-trees (BE/BN/BApp/BLam/BPi) + `splice` + `normalize` + `infer` + `check` + `pred_of` (types-as-predicates kernel) + `mkAtom` + `erase` (tagged → bracket-abstracted runtime SKI) + surface elaborator (`\(x : T). body`, `A -> B`, `(x : T) -> R`) + typed-def ergonomics (`def NAME : T = EXPR` elaborates both sides and runs `check` at parse time) + tree-native `compute_bind`/`replace_marker` (mirror the host elaborator's binder-rewriting) + `Type` universe sentinel (Type:Type via direct hash-cons; atoms inhabit Type via plain-def `mkH Type marker`) + `raw (EXPR)` escape (typed surface ↔ runtime: parses EXPR with the untyped grammar, bracket-abstracts, embeds the resulting runtime tree). Tree-native code in `examples/*.disp`; host-side has runtime, parser, surface elaborator, and test harness. 223 tree-native tests pass across 14 example files.

**Plan**: see `ELABORATION_DESIGN.md` "Plan" section.
1. ~~**Types-as-predicates kernel**~~ — DONE (`examples/predicates.disp`). `apply(ty, term)` IS the check; `pred_of` derives the callable predicate from the tagged form.
2. ~~**Surface elaborator**~~ — DONE (`src/elaborate.ts` + `examples/elab.disp`). Annotated lambdas, arrows, dependent Pis; auto-computed bind-trees; type-checking via `pred_of` downstream in tests. `def NAME : T = EXPR` elaborates and checks at parse time, throwing on FF.
3. **Metavariables** (next) — positional canonical metas + Miller-pattern unification.

**Sharp lessons from the substrate** (must respect when extending — see ELABORATION_DESIGN.md for detail):
- Recursive-call arg order under `wait`/`fix`: inner-binder-derived arg goes first.
- Strict-branch deferral: wrap each case as `\u. body` and apply after dispatch.
- `H` wraps its type, not its binder (departure from `BIND_TREE_NBE_IDEA.md` §3.4).
- Bind-trees are load-bearing data, not decoration — splice trusts them.
- H-comparisons need direct `fast_eq` first, H-unwrap as fallback (Phase 1 lesson).
- `mkH` carries a freshness marker (`mkH ty lvl`); single-arg form collapses two same-type free hypotheses to identical hash-cons. Descent in `pred_of` threads fork-shaped levels (distinct from leaf/stem hand-constructed markers).

## Testing

`npm test` runs the vitest suite.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- Hash-consing is a load-bearing property: `fastEq` is O(1) tree identity, and anything that makes equivalent trees have different structure undermines it.
