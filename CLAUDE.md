# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer) and the metacircular discipline used to get there.
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — **authoritative** type-theory spec (target). **Two** Σ-operations (`hyp_reduce`, `bind_hyp`) plus the two-argument dispatcher `param_apply` over a *fixed* Σ that routes pinned sigs to the *registered* handler (Σ is a routing table, not just a trust set — `param_apply f x` runs `h (wait_meta f) x` for the registered `h`, ignoring `f`'s embedded handler). `eliminator_frame` is demoted to a library `elim` over `hyp_reduce` + a type's `respond` meta-field. `postulate` is **removed** — effects are now an entirely-library construction: a free monad `Eff R X` over an operation signature, interpreted by deep handlers, with one impure *driver* at the program boundary (§15); the substrate's purity forces effects to be values, never dispatch targets. Walker protection is one set: `seal(Σ)` (trusted-token producers — `hyp_reduce`; unforgeable *and* uninspectable), `forge(Σ) = seal(Σ)` (the `funnel` set dissolved with host effects); `bind_hyp` is in neither (its invocations are built by library recognizers under the walker). `respond` is a *constitutive* (non-optional) meta field, typed `RespondShape` and checked by `StrictType`/`BehavioralType`; the two-tag `Action` (`Extend | Return`) with rejection = `Extend InvalidType`. Manifest contracts over the tree-calculus substrate; `CheckerResult` monad with named-variant errors; library types carry MetaShape-conforming meta records (`recognizer_params`, `functor`, `respond`, `behavioral_specs`); cubical operations live in each type's `functor` meta-field; validators (`Type` / `StrictType` / `BehavioralType`) are ordinary library wait-forms.
- [`TYPE_THEORY_LEGACY.typ`](TYPE_THEORY_LEGACY.typ) — previous spec, retained for history. Seven kernel primitives (`hyp_reduce`, `guard`, `unguard`, `checked_apply`, `predicate_frame`, `eliminator_frame`, `bind_hyp`). The codebase has **migrated off** this shape to `TYPE_THEORY.typ`'s two-Σ-op kernel — see "Implementation status" below.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms for the current two-Σ-op kernel: `hyp_reduce`/`bind_hyp`/`param_apply`, wait/fix, signatures, neutrals, the `tree_eq` native fast-path, bracket-abstraction caveats.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — type predicates, the parametric walker, the elaborator — are tree programs. A native fast-path in `src/tree.ts` must mirror in-language code semantically (bit-identical results) and be validated against the in-language reference. Currently the only live native fast-path is `tree_eq`; the dispatcher (`param_apply`) runs in-language. (A native dispatcher fast-path existed for the legacy kernel but was orphaned and removed in the cutover; re-introducing one requires restoring an equivalence test.)

## Code layout

### Source
- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `tree_eq` native fast-path. (The dispatcher/parametric-walker runs in-language via `param_apply` in `lib/kernel/core.disp`; there is no native dispatcher fast-path.)
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `src/compile.ts` — elaborator: typed bindings, kernel-helpers, `tree_eq` native-fast-path tree-id registration.

### Library layout (`.disp` files in `lib/`)
- `prelude.disp` — fundamental combinators (TT/FF Scott-encoded, triage, select, pair, wait/fix, tree_eq, nat_le, zero/succ).
- `kernel/` — the two-Σ-op kernel center:
  - `prelude.disp` — canonical entry point: `open`s `../prelude.disp` + `utils.disp` + `core.disp`. Files that build on the type system do `open use "../kernel/prelude.disp"`.
  - `utils.disp` — the `type_meta` accessor, the `checker_sig` signature, `InvalidType`. (Action / CheckerResult and the neutral-meta record moved to `core.disp`, since they are built from the §2.6 cut.)
  - `core.disp` — the entire kernel + library type system, built bottom-up on the substrate only:
    - **Σ-ops:** `hyp_reduce` (push a frame onto a neutral via the stored type's `respond`) and `bind_hyp` (mint a fresh hyp, run the body, `occurs` escape-check).
    - **Dispatcher:** `param_apply` = the in-language parametric walker (`walk`) with reader carve-outs (`ROOT_SIG`/`STORED_TYPE`/`I`/`tree_eq`) + Σ routing on `hyp_sig`/`bind_hyp_sig`.
    - **The cut (§2.6):** value-level `prod`/`annihilate`/`proj`/`inj`/`acc`/`record_val`/`field`/`match_co`. MetaShape metadata (built by `make_meta`), each former's `recognizer_params` (inline `{ }` literals, e.g. Pi's `{ dom; cod }`), and `checked`'s `{ dom, fn }` are §2.6 records read **by name** through the cut (`.field`) — no accessor wrappers.
    - **Control coproducts:** `Action` (`Extend | Return`) and `CheckerResult` (`Ok | Fail`) are §2.6 coproducts — `inj` constructors, `match` elimination — the same construct library types use, not bespoke tag dispatch. The neutral-meta `{ stored_type; payload }` and the eliminator frame `{ motive; cases }` are likewise records read by name.
    - **H-rule:** `make_recognizer` (recognizer side) + `type_predicate_h_rule` (predicate side).
    - **Library types:** Bool, Nat, Pi/Arrow, Type, Unit, False/Not, String, Eq (+ `eq_J`/`eq_subst`/`eq_sym`/`eq_cong`), Ord, Sigma, Refinement, Intersection, Coproduct, Record — each `wait (make_recognizer body) meta`. Recursors `nat_rec`/`bool_rec`/`ord_rec` via the library `elim`.
- `std/` — standard library on top of the kernel (`open use "../kernel/prelude.disp"`): `nat/arith.disp` (`add`), `nat/ops.disp` (`pred`, `is_zero`, `double`), `list.disp`, `set.disp`, `fin.disp`, `option.disp`, `result.disp`, `pair.disp`. (Records live in the kernel: the §2.6 cut — `record_val`/`field`/`Record`.)
- `tests/` — all `*.test.disp` files. Test runner globs recursively under `tests/`.

### Host tests
- `test/disp.test.ts` — vitest harness; recursively globs `lib/tests/**/*.test.disp`.
- `test/parser.test.ts` — parser unit tests.
- `test/tree.test.ts` — tree calculus runtime tests.

## Implementation status

**The codebase implements the two-Σ-op kernel of `TYPE_THEORY.typ`** — the cutover from the legacy 7-primitive shape has landed (`lib/types/` and `lib/kernel/{handlers,walker}.disp` were retired). The kernel surface is `hyp_reduce` + `bind_hyp` + the dispatcher `param_apply` over a fixed Σ; `eliminator_frame` is now the library `elim`; Pi/Bool/Nat/Eq/Type/Ord/Sigma/Refinement/Intersection/Coproduct/Record are ordinary library types in `lib/kernel/core.disp`.

### Landed
- ✅ **In-language parametric walker.** `param_apply` runs the walker + Σ routing in-language (no native dispatcher fast-path). Forging a neutral-rooted fork (stem-forge) and triaging on a neutral are rejected (`Fail`); root-sig reads and `tree_eq` are the carve-outs. Verified by `lib/tests/soundness.test.disp`.
- ✅ **Recognizer + predicate H-rule.** `make_recognizer` handles the recognizer-side H-rule; `type_predicate_h_rule` the predicate side. Polymorphic Pi (`Pi Type ({A} -> Pi A ({_} -> A))`) type-checks.
- ✅ **The cut (§2.6).** Coproduct / Record / match / projection over one `prod`/`annihilate` shape. MetaShape metadata, multi-field recognizer_params, and `checked`'s `{dom,fn}` are §2.6 records read by name.
- ✅ **Recursors.** `nat_rec` / `bool_rec` / `ord_rec` and `eq_J` (+ Eq lemmas) via the library `elim`.
- ✅ **Bool Scott encoding.** `prelude.disp`'s `TT`/`FF` are `{m,ct,cf}->ct` / `{m,ct,cf}->cf` (`K K` / `K (K I)`); `select`/`select_lazy` dispatch via Scott eliminator application.
- ✅ **Field exports.** Library files use explicit `name := expr` exports; duplicate exported/record-type fields are parse errors.

### Remaining Work (tracked, not yet implemented)
- Route-to-registered handler in `param_apply` (§5.4) and body-walking in `bind_hyp` (§7.2) — both pinned by characterization tests in `lib/tests/soundness.test.disp`.
- `StrictType` / `BehavioralType` validators, `RespondShape` / `RecognizerShape`, and behavioral_specs coherence Paths (§11–§12); make `Type` stricter (check the recognizer is `make_recognizer`-built).
- Effects: the `Eff R X` free monad + driver (§15). Cubical: `Path` / `Partial` / `comp` / `transp` / `Glue` (§13). `strip` / erasure (§10). `wf_fix` / `Total` / `TotalWith`.
- Richer `CheckerError` vocabulary (currently folded to a single `Fail`); source-span diagnostics and multi-error reporting.

## Compiler workarounds

Two issues affect kernel-level code involving recursion or multi-line conditional dispatch:

- **Match arm bodies are single-line.** A multi-line match arm body like `FF => triage \n (arg1) \n (arg2)` parses only the bare `triage` because the arm body uses `lineExpr`. Wrap multi-line content in parens to make it one atom: `FF => (triage \n (arg1) \n (arg2))`. Same workaround for multi-line `let` bodies.
- **`select_lazy` + self-recursion blows the compile-time budget.** A thunk `{_} -> self meta (pair_snd x)` compiles to `K body`. After outer bracket abstraction, `cirToTree`'s reduction of the closed combinator eagerly evaluates internal apps via `applyTree` (10M-step budget). For self-referential expressions, this fires fix-unfolding at compile time even though runtime semantics would short-circuit via `select_lazy`'s lazy thunk dispatch. **Workaround**: use `match` instead of `select_lazy` for bodies containing recursive calls. `match` desugars to `select branchTT branchFF cond fvs...` where each branch is wrapped in a closure over its free vars, side-stepping the eager K-body evaluation.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **Plain `fix` handlers.** The kernel's Σ-ops (`hyp_reduce`/`bind_hyp`) are plain `fix`-forms. The old `(ks, raw, query)` self-proxy handler protocol and the `rec`/`recq` self-referential-record combinators were retired with the 7-primitive kernel (nothing in the new kernel used them).
- **Wait-based types.** `wait(checker)(metadata)`. Signature = `pair_fst(T)` (constant per checker). Metadata = `type_meta(T) = pair_snd(pair_snd(T))`, a §2.6 record whose fields (`respond`, `recognizer_params`, …) are read **by name** via the cut (`m.respond`, `m.recognizer_params`). Type-former recognition is via signature comparison.
- **H-rule via `make_recognizer`.** The universal recognizer-side H-rule lives in `make_recognizer`, which reconstructs the type via `wait (wait wrap body) meta` and short-circuits `Ok TT` when the candidate is a neutral of that stored type. `type_predicate_h_rule` is the predicate-side dual.
- **Hash-consing is load-bearing.** Conversion is `tree_eq`, an O(1) hash-cons identity check. Deterministic elaboration ensures same type → same tree.
- **Native fast-paths.** `tree_eq` is a native fast-path in `src/tree.ts`, producing bit-identical results to the in-language reference; the in-language code is the spec. The dispatcher (`param_apply`) runs in-language — no native fast-path (the legacy native dispatcher was removed in the cutover).

## Testing

`npm test` runs vitest. `lib/tests/**/*.test.disp` is the primary object-language suite. `test/parser.test.ts` and `test/tree.test.ts` cover the host infrastructure.

## Operating notes

- Type checking is raw `apply(T, v) = TT`. Types are wait-based raw functions.
- `is_neutral` is an O(1) signature check (`pair_fst v` against `hyp_sig`); `is_hyp_fork` adds the fork-shape guard where partial wait-forms could share the bare signature.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions are safe but should be avoided for clarity.
- Files with any `name := expr` field use the export model where only `:=` fields export. Files with only `let`/`test`/`open` use legacy mode where all `let` bindings export. Prefer `name := expr` for new code.
