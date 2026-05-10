# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — **authoritative** type-theory spec. Data-as-eliminator architecture: inductive types are encoded as `recq`-based per-value Pi templates, constructors are Scott-style closures, eliminators are identity-applied to target. The kernel registers Pi, Type, hyp_reduce, guard, eliminators, and `unguard` as primitives. Where this document and the implementation disagree, the document is authoritative.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms: `hyp_reduce`, wait/fix/recq, deferred branching, bracket abstraction optimizations, performance notes.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — type predicates, the parametric walker, the elaborator — are tree programs. Native fast-paths in `src/tree.ts` mirror in-language code semantically (bit-identical results) and are validated against the in-language reference.

## Code layout

### Source
- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `tree_eq` fast-path, **native parametric-walker / dispatcher** fast-path (intercepts `apply(checked_apply, …)` to run dispatch + walker in TS rather than executing the in-language fix-form chain).
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `src/compile.ts` — elaborator: typed bindings, kernel-helpers, native dispatcher / signature anchor registration.

### Library (`.disp` files in `lib/`)
- `prelude.disp` — fundamental combinators (TT/FF, triage, select, pair, wait/fix/recq, tree_eq, nat_le).
- `kernel.disp` — recursive-record kernel: `q_pi_fn`, `q_hyp_reduce_fn`, `q_guard_fn`, `q_core_type_fn`, `q_guarded_type_fn`, `q_unguard_fn`, eliminator handlers (`q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`), the dispatcher stub, public type constructors (`Pi`, `Arrow`, `Type`, `Bool`, `Nat`, `Eq`), `Hyp`/`StuckElim`, eliminator wrappers, signature anchors for the host fast-path.
- `dae.disp` — data-as-eliminator library types: `Bool_template` / `Nat_template` / `Eq_template` (recq-encoded per-value Pi templates), Scott constructors `TT_dae` / `FF_dae` / `zero_dae` / `succ_dae` / `refl_dae`, identity-applied eliminators `bool_rec_dae` / `nat_rec_dae` / `eq_J_dae`, hypothesis minting via per-value templates (`mint_bool_hyp` etc.).
- `ord.disp` — ordinal type up to ε₀: `zero_ord` / `omega_plus` constructors, `ord_rec` eliminator, `succ_ord` / `omega` derived helpers, `mint_ord_hyp`, closed comparisons `ord_lt` / `ord_le` / `ord_max`.
- `math.disp` — `add` (via `nat_rec` for walker safety).
- `nat.disp` — `pred`, `is_zero`, equality proofs (`add_zero_l/r`, `add_comm`, etc.).
- `list.disp`, `set.disp`, `fin.disp` — additional library types.
- `*.test.disp` — tests per module.

### Tests
- `test/disp.test.ts` — vitest harness that globs `lib/*.test.disp`.
- `test/parser.test.ts` — parser unit tests.
- `test/tree.test.ts` — tree calculus runtime tests.

## Implementation status

The kernel implements the data-as-eliminator architecture. Native parametric walker is active. **Bool is fully Scott-encoded** per spec §4.5. **Nat / Eq Scott migration is deferred** because it requires elaborator changes (q_pi_fn must mint per-value Pi-template hypotheses for DAE library types).

### Landed
- ✅ **Native parametric walker** active. Reflective predicates on hypotheses (`is_neutral`, `is_fork`, `pair_fst`, `tree_eq`, `has_sig` against a hypothesis) return `Fail` under `checked_apply`. Forgery of neutral-rooted forks via user code is rejected (stem-rule constructor check). I-shortcut is the only soundness carve-out (per spec §6.3). Verified by `lib/walker.test.disp` and `lib/soundness.test.disp`.
- ✅ **Walker-safe Pi/Arrow** via the `kernel.unguard` handler. `unguard_checked` is a wait-form whose application dispatches via the kernel signature route — handler runs raw, body's triage on the type variable is safe. Polymorphic Pi (`Pi Type0 ({A} -> Arrow A A)`-style) type-checks under the walker.
- ✅ **Eliminator routing.** `bool_rec`, `nat_rec`, `eq_J` are wait-forms (`wait kernel_ref.X_rec init_meta`) routed through arity-tracked kernel handlers. Each handler accumulates partial-app args in metadata; when fully applied, the body runs raw (StuckElim minting safe). `nat_rec`'s recursive succ-branch uses `match` (not `select_lazy`) — see "compiler workarounds" below.
- ✅ **DAE library** (`lib/dae.disp`, spec §4) — Bool/Nat/Eq per-value Pi templates via recq, Scott constructors, identity-applied eliminators, hypothesis minting (`mint_bool_hyp` etc.) producing Pi-typed hypotheses that work uniformly under `hyp_reduce`. 33 tests.
- ✅ **Ord library** (`lib/ord.disp`, spec §4.8) — DAE-encoded ordinals up to ε₀ in CNF. Constructors, eliminator, hypothesis minting, closed comparisons via Scott eliminator probing. 31 tests.
- ✅ **Bool/Nat/Eq dropped from dispatcher signature list.** Their predicate applications no longer have native fast-path routing — fall through to the walker, which executes their tree-shape recognition correctly for closed values. Recq fields stay (used internally by `q_core_type_fn`'s `is_registered` check).
- ✅ **Bool migrated to Scott** (spec §4.5). `prelude.disp`'s `TT`/`FF` are now `{m,ct,cf}->ct` / `{m,ct,cf}->cf` (compiled tree shapes `K K` / `K (K I)`). `select` / `select_lazy` dispatch via Scott eliminator application instead of triage-on-tree-shape. `src/tree.ts`'s `tree_eq` fast-path returns `SCOTT_TT` / `SCOTT_FF` (built directly from primitives so they hash-cons-match the prelude-compiled forms). `compile.ts`'s host-side `TT` constant imports `SCOTT_TT`. `q_bool_fn` updated to recognise the Scott shapes.

### Open work

- ⏳ **Scott migration of `zero`/`succ`/`refl`** (the spec's culmination for Nat/Eq). Spec §4.5–4.7 mandates Scott constructors throughout, but completing this requires elaborator-level changes that the current `q_pi_fn` doesn't make: when checking `Pi Nat (...)`, the kernel must mint hypotheses whose stored type is `Nat_template hyp_id` (a literal Pi expression) rather than the public guarded `Nat`. Without this, applying a Scott-encoded function (e.g. `succ`) to a hypothesis produces a tree containing the hypothesis whose shape `q_nat_fn` can't validate via tree-shape recursion + H-rule — the OLD encoding's per-shape recursion is exactly what handles `fork(LEAF, hyp)` today, and Scott has no analogue without Pi-template-checking. Work needed: (1) refactor `q_pi_fn` so domain dispatches to a template-family function for hypothesis stored type; (2) drop `q_bool_fn` / `q_nat_fn` / `q_eq_fn` / `q_bool_rec_fn` / `q_nat_rec_fn` / `q_eq_J_fn` / `core_Bool` / `core_Nat` / `core_Eq` / `StuckElim` from `kernel.disp`; (3) move `Bool` / `Nat` / `Eq` public types to use the per-value templates; (4) migrate `prelude.disp`'s `zero` / `succ` and `kernel.disp`'s `refl` to Scott; (5) sweep ~80 test sites. Coherent multi-hour follow-up.

- ⏳ **Hypothesis-typed Ord comparisons** (spec §5.5). Kernel-routed `ord_lt` / `ord_le` / `ord_max` with bound-consulting identities (e.g. `r : OrdLt ω ⊢ ord_lt r ω = TT`). Ships only the ω identity initially. Add `OrdLt k` refinement type. Re-base universe ranks from Nat to Ord in `q_core_type_fn` / `q_guarded_type_fn`.

- ⏳ **`src/compile.ts` `makeKernelHelpers` updates** for new metadata layouts as Ord/OrdLt land — `isOrd`, `isOrdLt`, `ordLtBound` predicates; `isUniverse` returns Ord not Nat.

## Compiler workarounds

Two issues surfaced during data-as-eliminator work that affect any kernel-level code involving recursion or multi-line conditional dispatch:

- **Match arm bodies are single-line.** A multi-line match arm body like `FF => triage \n (arg1) \n (arg2)` parses only the bare `triage` because the arm body uses `lineExpr`. Wrap multi-line content in parens to make it one atom: `FF => (triage \n (arg1) \n (arg2))`. Same workaround for multi-line `let` bodies.
- **`select_lazy` + self-recursion blows the compile-time budget.** A thunk `{_} -> self meta (pair_snd x)` compiles to `K body`. After outer bracket abstraction, `cirToTree`'s reduction of the closed combinator eagerly evaluates internal apps via `applyTree` (10M-step budget). For self-referential expressions, this fires fix-unfolding at compile time even though runtime semantics would short-circuit via `select_lazy`'s lazy thunk dispatch. **Workaround**: use `match` instead of `select_lazy` for bodies containing recursive calls. `match` desugars to `select branchTT branchFF cond fvs...` where each branch is wrapped in a closure over its free vars, side-stepping the eager K-body evaluation.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`recq` lazy self-proxy.** Each kernel handler receives `(ks, raw, query)`. `ks` is a lazy proxy (`{q} -> wait raw q`) so `ks.field` references resolve only when forced. Avoids forcing recursive field selection too early.
- **Wait-based types.** `wait(checker)(metadata)`. Signature = `pair_fst(T)` (constant per checker). Metadata = `pair_snd(pair_snd(T))`. Type-former recognition is via signature comparison.
- **H-rule inlined.** Each checker reconstructs its own type via `wait (ks query) meta` for H-rule self-comparison.
- **Hypothesis stored types are Pi.** Per the spec's DAE design, hypotheses minted via per-value Pi templates (`mint_X_hyp`) so applying a hypothesis fires `hyp_reduce` which extends the spine through the Pi-template's chain. No special "stuck eliminator" sentinel needed.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.
- **Native fast-paths.** `tree_eq` and `checked_apply` (the dispatcher + walker) are native fast-paths in `src/tree.ts`. They produce bit-identical results to the in-language reference; the in-language code is the spec.

## Testing

`npm test` runs vitest. `lib/*.test.disp` is the primary test suite (120 tests as of writing). `test/parser.test.ts` (85 tests) and `test/tree.test.ts` (21 tests) cover the host infrastructure.

## Operating notes

- The reference for the type system is `TYPE_THEORY.typ` + `lib/kernel.disp` + `lib/dae.disp` + `lib/ord.disp`. When they disagree, investigate.
- Type checking is raw `apply(T, v) = TT`. Types are wait-based raw functions.
- `is_neutral` is a single O(1) signature check against `kernel.hyp_reduce`.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions are safe but should be avoided for clarity.
- Files with any `name := expr` field use the export model where only `:=` fields export. Files with only `let`/`test`/`open` use legacy mode where all `let` bindings export. Prefer `name := expr` for new code.
