# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting (kernel reified as a tree-calculus inhabitant, not just as a host function).

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase. Every design decision should be compatible; if it isn't, one of them changes, and changing the philosophy is the harder path.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — core idea: trees as S/K/Triage combinators, types as executable predicates, Pi as partially-applied `PiCheck`, hash-cons identity as type equality.
- `lib/{debruijn,ctxtree,semantic}/DESIGN.md` — three competing backend designs currently being implemented side-by-side. Read whichever one the current task targets.

## Core discipline, in brief

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding before it ships. Before using any host-language feature (`Map`, `Set`, `try`/`catch`, mutation, exceptions for control flow) in such code, confirm that a tree-analog exists or is planned.

"We'll port it later" is forbidden. It's the pattern that stopped Lean, Coq, and Agda from bootstrapping their checkers. Our tree calculus is small enough (three constructors, five reduction rules) that this discipline is feasible; squandering it defeats the project's premise.

When in doubt, reread `DEVELOPMENT_PHILOSOPHY.md`.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, evaluation budgets, `FAST_EQ` primitive.
- `src/parse.ts` — surface tokenizer / parser / bracket-abstraction. Handles `def`, `test`, `elab`, `raw`, `\x.`, juxtaposed application, `t` for leaf. Module system: `use "path"` imports defs from another file into current scope; `{ ... }` blocks introduce scoped frames. Exposes `fast_eq` as a global to disp programs.
- `src/elaborate.ts` — host-side surface elaborator producing tagged forms (KV/KH/KA/KL/KP/KM) with bind-trees. Mirror of the `lib/predicates.disp` encoding so output is kernel-consumable.
- `src/run.ts` — driver: load `.disp` file, run `test` declarations, assert tree equality.
- `lib/predicates.disp` — the current ctx-tree-threaded `pred_of_lvl` kernel. The ctxtree backend `use`s this file. Defines tagged-form encoding, bind-trees, splice / compute_bind / replace_marker, ctx-tree primitives (CtxV/CtxApp/CtxLam/CtxPi, ctx_enter_binder, ctx_lam_split_*), `normalize_pair`, `try_unify_pair`, `pred_of_lvl`. Phase 1 checker plus ctx-tree Phase 5 threading — **minus `ctx_exit_binder`**, the stale-bindtree gap.
- `lib/{debruijn,ctxtree,semantic}/` — three competing backend designs. Each has `DESIGN.md` (spec) and `impl.disp` (stub today, real implementation TBD). See `lib/suite/main.disp` for the shared export contract each backend must satisfy.
- `lib/suite/main.disp` — implementation-agnostic test suite. Same assertions run against each backend via `{ use "../X/impl.disp"; test ... }` blocks.
- `test/tree.test.ts` + `test/disp.test.ts` — vitest suites.

## Current state (as of 2026-04-18)

Infrastructure phase: three backends have DESIGN.md specs and stub impl.disp files exporting placeholder-FF values for the shared contract (`check_id_nat`, `check_const`, `check_id_poly`, `check_apply_id`, `check_refl_zero`, `check_refl_succ_zero_one`, `reject_kstar_shadowed_dep`, `backend_name`). The suite harness passes 8 assertions per backend × 3 backends + gate tests = 26 tests in `lib/suite/main.disp`.

The **ctxtree** backend has a head start: `lib/predicates.disp` already implements its ctx-tree-threaded Lam-vs-Pi descent. Phase 5 missing piece is `ctx_exit_binder` (extract fresh bind-tree from post-reduction ctx at Lam ascent), which flips the `check_refl_succ_zero_one` litmus test from FF to TT. Spec lives in `lib/ctxtree/DESIGN.md`.

The **debruijn** backend is unstarted — classical closure NbE with de Bruijn levels, serves as the correctness oracle.

The **semantic** backend is unstarted — Dybjer/Filinski Val-domain NbE on raw bracket-abstracted SKI. Known blocker: triage-on-neutral (identity `I` applied to a neutral produces a stuck triage, not the neutral). See `lib/semantic/DESIGN.md` for candidate fixes.

## Sharp lessons from the substrate

Must respect when extending `lib/predicates.disp` / the ctxtree backend:

- **Recursive-call arg order under `wait`/`fix`**: inner-binder-derived arg goes first. Outer-binder-derived args cause eager `wait` firing and divergence at compile time.
- **Strict-branch deferral**: `triage` evaluates every branch. Wrap each case as `\u. body` and apply after dispatch to prevent eager recursion.
- **`H` wraps its type, not its binder.** `mkH ty lvl = tagged(KH, fork(ty, lvl))`. `type_of_H` is one fork-projection. Prior "wrap-the-binder" design was abandoned after the depth-2 dependent type bug.
- **`mkH` carries a freshness marker (`mkH ty lvl`)**. Single-arg form collapses two same-type free hypotheses to identical hash-cons identity, silently breaking e.g. `(x:A) → (y:A) → x`. Descent threads fork-shaped level markers (`lvl_start = t t t`, `lvl_next = \l. t l t`). Hand-constructed free hypotheses use leaf-rooted markers (disjoint namespace).
- **Bind-trees are load-bearing data, not decoration.** `splice` trusts them; mismatched bind-trees silently corrupt the term. After a beta that changes body shape, outer bind-trees become stale — this is the gap `ctx_exit_binder` closes.
- **H-comparisons need direct `fast_eq` first, H-unwrap as fallback.** Two H-tokens with the same type + marker are hash-cons-equal; direct `fast_eq` is cheaper than `type_of_H` extraction.

## Testing

`npm test` runs the vitest suite.

To run a single `.disp` file directly: `npx tsx src/run.ts path/to/file.disp`.

## Operating notes

- Prefer editing existing files over creating new ones, especially docs. New top-level docs are a design event, not a casual action.
- When proposing a new feature or fix, explicitly state its object-language encoding, or explicitly note why that's deferred and what's blocking the encoding work.
- Hash-consing is a load-bearing property: `fast_eq` is O(1) tree identity, and anything that makes equivalent trees have different structure undermines it.
- The backend-comparison framework is the current organizing principle. When proposing design changes, state which backend(s) they affect and whether the suite assertions change.
