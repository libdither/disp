# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY_V2.typ`](TYPE_THEORY_V2.typ) — **authoritative** type-theory spec. Data-as-eliminator architecture: inductive types are encoded as recq-based per-value Pi templates, constructors are Scott-style closures, eliminators are identity-applied to target. The kernel registers only Pi, Type, Ord, OrdLt, ord_lt/le/max as primitives. Where this document and the implementation disagree, the document is authoritative.
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — the prior-design spec (Bool/Nat/Eq as kernel-registered checkers, certified eliminators with StuckElim). The current `lib/kernel.disp` still implements this design; migration to V2 is in progress.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms: `hyp_reduce`, wait/fix/recq, deferred branching, bracket abstraction optimizations, performance notes.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — NbE operations, type predicates, the elaborator — are tree programs.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `FAST_EQ`.
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar: unified `recBody` (file bodies and inline `{ ... }` share the same grammar), `name := expr` (exported field), `let name = expr` (private binding), `test`/`open` (side-effects), `{x : A} -> e` binders, `A -> B` arrow sugar, `{x : A}` recTypes, `.field` projection. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `lib/prelude.disp` — fundamental combinators (TT/FF, triage, ite2/ited, pairs, and, wait/fix/recq).
- `lib/kernel.disp` — recursive-record kernel, type-tracking neutrals, infer, conversion, primitive type constructors, eliminators, and arithmetic. Also hosts the standalone parametric walker (`checked_apply_walker`) and the CheckedResult helpers; the walker is not yet wired through `q_checked_apply_fn`, which remains the raw-apply stub.
- `lib/dae.disp` — V2 data-as-eliminator library types (`Bool_template`, `Nat_template`, `Eq_template`) with Scott-style constructors (`TT_dae`, `FF_dae`, `zero_dae`, `succ_dae`, `refl_dae`) and identity-applied eliminators (`bool_rec_dae`, `nat_rec_dae`, `eq_J_dae`). Additive — does not replace the kernel-registered Bool/Nat/Eq checkers yet.
- `lib/*.test.disp` — tests per module.
- `test/disp.test.ts` — vitest harness that globs `lib/*.test.disp` and runs each.
- `test/parser.test.ts` — parser unit tests (85 tests).
- `test/tree.test.ts` — tree calculus runtime tests (21 tests).

## V2 migration in progress

The kernel is being migrated from the prior `TYPE_THEORY.typ` design (kernel-registered Bool/Nat/Eq checkers, certified eliminators that mint `StuckElim`) to the V2 data-as-eliminator architecture in `TYPE_THEORY_V2.typ`. Status:

- ✅ Walker (`checked_apply_walker`) consolidated into `kernel.disp`. Adversarial tests in `lib/walker.test.disp` validate that triage-on-neutral, reflective predicates on hypotheses, and stem-rule fork-formation with neutral roots are all rejected.
- ✅ DAE library (`lib/dae.disp`) with `Bool_template`, `Nat_template`, `Eq_template`, Scott constructors, identity-applied eliminators, and per-value hypothesis minting. Validated by 33 tests in `lib/dae.test.disp` covering closed reductions, hypothesis application via hyp_reduce, and stuck-type tracking.
- ✅ Eliminator handlers (`q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`) added to the kernel record. Each is arity-tracked: meta = `(remaining-count, accumulated-args)`; partial applications return a wait-form with the same `kernel.X_rec` signature so the dispatcher routes every step through the handler. The eliminator body runs in raw mode, so its is-neutral / StuckElim minting is safe.
- ✅ `bool_rec`, `nat_rec`, and `eq_J` all switched to wait-form (`wait kernel_ref.X_rec init_meta`). Routed through their kernel handlers; StuckElim-minting bodies live inside the handlers and run in raw mode. nat_rec's recursive succ-branch uses `match` (not `select_lazy`) — see "parser/compiler workarounds" below.
- ✅ **Walker dispatcher activated.** Implemented as a native TypeScript fast-path in `src/tree.ts` that mirrors the parametric apply rules: triage-on-neutral fails; stem-rule fork-formation rejects neutral roots; I-shortcut accepts identity on a hypothesis. The host runtime intercepts `apply(checked_apply, …)`, runs `nativeDispatch` (signature recognition + walker default), and returns bit-identical CheckedResults. Activated via `setNativeDispatcherTreeId` after kernel.disp loads. Soundness attacks 3-5 in `lib/soundness.test.disp` (BadFam patterns probing hypotheses via `is_neutral` / `tree_eq` / `is_fork`) are now correctly rejected.
- ✅ `add` migrated from raw triage to `nat_rec` so `Nat -> Nat -> Nat` Pi-checks under the walker (see `lib/math.disp`).
- ✅ Walker-safe Pi/Arrow. `unguard_checked` registered as a kernel handler (`kernel.unguard`). User calls dispatch via the kernel signature route — handler runs raw, body's triage on the type variable is safe. Polymorphic Pi (`Pi Type0 ({A} -> Arrow A A)`-style) now type-checks. All 119 disp tests pass.
- ⏳ Drop kernel-level Bool/Nat/Eq predicates (full V2 migration). Investigation showed partial drops are infeasible — `q_core_type_fn` references `ks.bool`/`ks.nat` for core-type recognition, and hypothesis minting via `Hyp Bool` requires `Bool` to be Pi-shaped (which means per-value templates + Scott constructors). Full migration plan documented in task #3.
- ⏳ Replace kernel-level Bool/Nat/Eq checkers with Pi-checking against the per-value templates in `lib/dae.disp`. Drop `q_bool_fn`, `q_nat_fn`, `q_eq_fn` from the kernel record.
- ⏳ Add `Ord` / `OrdLt` (library DAE types) and `q_ord_lt_fn` / `q_ord_le_fn` / `q_ord_max_fn` kernel comparison primitives. Re-base universe ranks on `Ord`.
- ⏳ Update `src/compile.ts` `makeKernelHelpers` for new metadata layouts.

## Parser / compiler workarounds (V2 migration)

Two issues surfaced during walker wiring that affect any kernel-level code involving recursion or multi-line conditional dispatch:

- **Match arm bodies are single-line.** A multi-line match arm body like `FF => triage \n (arg1) \n (arg2)` only parses `triage` because the arm body uses `lineExpr`. Wrap multi-line content in parens to make it one atom: `FF => (triage \n (arg1) \n (arg2))`. Same workaround applies to multi-line let bodies.
- **`select_lazy` + self-recursion blows the compile-time budget.** A thunk `{_} -> self meta (pair_snd x)` compiles to `K body`. After outer bracket abstraction, `cirToTree`'s reduction of the closed combinator eagerly evaluates internal apps via `applyTree` (10M-step budget). For self-referential expressions, this fires fix-unfolding at compile time even though runtime semantics would short-circuit via `select_lazy`'s lazy thunk dispatch. **Workaround**: use `match` instead of `select_lazy` for bodies containing recursive calls. `match` desugars to `select branchTT branchFF cond fvs...` where each branch is wrapped in a closure over its free vars, side-stepping the eager K-body evaluation.

## Current state (as of 2026-05-09)

- **Unified program/recValue grammar.** File bodies and inline `{ ... }` share the same `recBody` grammar. `name := expr` exports a field; `let name = expr` is private. `test`/`open` are side-effect statements allowed in both contexts. Expression atoms refuse to consume `IDENT :=` after a newline, preventing accidental field-as-argument parsing.
- **Full kernel pipeline as `.disp` source.** Split into `lib/prelude.disp` (combinators) and `lib/kernel.disp` (recursive-record kernel and type surface). Files use `open use "dep.disp"` for dependencies; `open` brings names into scope but does not re-export them. Types are `wait(checker)(metadata)` — type checking is raw `apply(T, v) = TT`. Type-former tags have been removed.
- **Typed eliminators.** `bool_rec` and `nat_rec` are neutral-aware recursors: they check `is_neutral(target)` before dispatching, producing `StuckElim(motive target, target)` when stuck. The stuck term stores the computed result type directly. This solves the triage-on-neutral problem for functions that branch on their arguments.
- **Eq type implemented.** `Eq A x y` predicate, `refl = LEAF`, J eliminator (`eq_J`), transport (`eq_subst`), symmetry (`eq_sym`), congruence (`eq_cong`).
- **Arithmetic working.** `add` via select-then-apply + fix. Eq proofs on concrete values including commutativity (`add 2 3 = add 3 2`).
- **Wait-based type encoding.** Types are `wait(checker)(metadata)`. Type checking is raw `apply(T, v) = TT` — no napply needed. Each type former (Pi, Nat, Bool, Eq, Type n) has its own checker with the H-rule inlined via `fix`.
- **Type-tracking neutrals.** Neutrals use the kernel's `hyp_reduce` handler. Metadata stores the current type at `pair_fst`; when a Pi-typed neutral is applied, `hyp_reduce` computes `codFn(v)` as the result type. `infer` is O(1): just extracts the stored type. `is_neutral` is one signature check against `kernel.hyp_reduce`. No val_apply or type_apply — raw `apply` handles types, neutrals, and functions uniformly.
- **Performance: ~113ms for test suite** (comparable to previous 108ms).
- **Bracket abstraction optimized.** Three optimizations: η-reduction (`[x](f x) → f`), K-composition (`S(K p)(K q) → K(p q)`), `S(K p)(I) → p`. Binder parameters correctly shadow scope variables.
- **Elaborator still needed.** The parser compiles to untyped tree calculus (types erased). The elaborator is the remaining frontier: it would supply motives to eliminators and support the full typed compilation pipeline. Pi types derive hypothesis identity from their own metadata, so no external depth counter is needed.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`ited` for deferred branching.** Branches are `{_} -> expr` thunks; only the chosen one is forced. Required because `triage` evaluates all branches eagerly. **Caveat:** bracket abstraction over shared free variables defeats `ited`'s laziness; use select-then-apply pattern instead (see `KERNEL_DESIGN.md`).
- **Select-then-apply for branching with shared vars.** Compile branches as closed functions, select via `ite2`, apply shared args after selection. Critical for type checkers, Nat, Type n, add, and other recursive branches.
- **Wait-based types.** `wait(checker)(metadata)`. Signature = `pair_fst(T)` (constant per checker). Metadata = `pair_snd(pair_snd(T))`. Type-former recognition uses checker signatures or canonical identity, not tags.
- **H-rule inlined via fix.** Each checker reconstructs its own type via `wait (ks query) meta` for H-rule self-comparison. `recq` provides the lazy kernel self-reference.
- **Type-tracking neutrals.** `hyp_reduce` tracks types through accumulation: if the neutral's type is Pi, result_type = codFn(v); otherwise `InvalidType`. Metadata is named as `make_neutral_meta(type, payload)`; application extends the identity spine with `extend_neutral_meta(old_meta, result_type, arg)`. `neutral_type(v) = neutral_meta_type(type_meta(v))`. `infer` is O(1) extraction, not spine walking. `is_neutral = has_sig kernel.hyp_reduce v` is O(1). No val_apply or type_apply needed.
- **Pi checker branches on is_neutral(result).** After evaluating the codomain on a hypothesis, if the result is neutral, the checker compares the stored type; if concrete, raw apply checks. No type_apply indirection.
- **Typed eliminators for neutral-awareness.** `bool_rec`/`nat_rec`/`eq_J` check `is_neutral` before dispatching. When stuck, produce `StuckElim(result_type, target)`. The motive is applied at the elimination site to compute `result_type`; it is not stored in the neutral. Raw `ite2`/`triage` should NOT be used on values that might be neutral.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.

## Testing

`npm test` runs vitest. `lib/*.test.disp` is the primary test suite (178 tests across 3 files). `test/parser.test.ts` (75 tests) and `test/tree.test.ts` (21 tests) cover the host infrastructure.

## Operating notes

- NbE backends are tree programs, not TypeScript. Host implementations are optimizations only.
- The reference for the type system is `TYPE_THEORY.typ` + `lib/*.disp`. When they disagree, investigate.
- Type checking is raw `apply(T, v) = TT`, not napply. Types are wait-based raw functions.
- No val_apply or type_apply. Raw apply handles types (runs checker), neutrals (accumulates spine via `hyp_reduce`), and functions (normal reduction).
- `is_neutral` is a single O(1) signature check against `kernel.hyp_reduce`.
- Type-former tags are gone. Neutral metadata payloads are private; code should rely only on the stored type at `pair_fst(metadata)`.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions between scope defs and lambda params are safe but should be avoided for clarity.
- Files with any `name := expr` field use the new export model: only `:=` fields export. Files with only `let`/`test`/`open` use legacy mode where all `let` bindings export (backward compat). Prefer `name := expr` for new code.
