# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — **authoritative** type-theory spec. Seven kernel primitives (`hyp_reduce`, `guard`, `unguard`, `checked_apply`, `predicate_frame`, `eliminator_frame`, `bind_hyp`); inductive types and quantifier types are library-defined.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms describing the **current** kernel: `hyp_reduce`, wait/fix/recq, deferred branching, bracket abstraction optimizations, performance notes. Reflects the codebase, not the spec target.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — type predicates, the parametric walker, the elaborator — are tree programs. Native fast-paths in `src/tree.ts` mirror in-language code semantically (bit-identical results) and are validated against the in-language reference.

## Code layout

### Source
- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `tree_eq` fast-path, **native parametric-walker / dispatcher** fast-path (intercepts `apply(checked_apply, …)` to run dispatch + walker in TS rather than executing the in-language fix-form chain).
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `src/compile.ts` — elaborator: typed bindings, kernel-helpers, native dispatcher / signature anchor registration.

### Library layout (`.disp` files in `lib/`)
- `prelude.disp` — fundamental combinators (TT/FF Scott-encoded, triage, select, pair, wait/fix/recq, tree_eq, nat_le).
- `kernel/` — the kernel-privileged center:
  - `prelude.disp` — canonical entry point: opens `utils` + `handlers` + `walker` + every type in `types/`. Files that build on the type system do `open use "../kernel/prelude.disp"`.
  - `utils.disp` — metadata accessors, signatures, Action protocol (`Extend`/`Return`), CheckedResult (`Ok`/`Fail`), `must_ok_*`.
  - `handlers.disp` — the seven `q_*_fn` primitive handlers, recq kernel record, `kernel_ref` proxy, public neutral / guard API (`Hyp`, `StuckElim`, `guard`, `unguard_checked`, `predicate_frame_form`, `eliminator_frame_form`, …), signature anchors for host fast-path.
  - `walker.disp` — standalone `checked_apply_walker` (reference impl of the parametric walker; native fast-path in `src/tree.ts` is the runtime version).
- `types/` — library type definitions. Files import `kernel/utils.disp` + `kernel/handlers.disp` (not `kernel/prelude.disp`, to avoid the import cycle through `kernel/prelude.disp → types/*.disp`).
  - `bool.disp` — Bool + bool_rec (predicate_frame + eliminator_frame).
  - `nat.disp` — Nat + nat_rec.
  - `pi.disp` — Pi/Arrow + bind_hyp wait-form + is_pi / pi_dom / pi_cod_fn.
  - `type.disp` — Type universe + is_universe.
  - `eq.disp` — Eq + refl + eq_J + eq_subst / eq_sym / eq_cong (predicate_frame + eliminator_frame).
  - `ord.disp` — Ord (predicate_frame + eliminator_frame): zero_ord/omega_plus constructors (tagged), ord_rec, succ_ord/omega, comparisons.
  - `conv.disp` — `conv_structural` (deep equality on type trees; cross-cuts Pi + Type).
- `std/` — standard library built on `types/`. Files import `kernel/prelude.disp` for the full surface.
  - `nat/arith.disp` — `add` (was `math.disp`). Imports `kernel/utils` + `kernel/handlers` + `types/nat` (not the full prelude — cycles through ord.disp → arith.disp).
  - `nat/ops.disp` — `pred`, `is_zero`, `double`, equality proofs (was `nat.disp`).
  - `list.disp`, `set.disp`, `fin.disp` — additional library types.
- `tests/` — all `*.test.disp` files. Test runner globs recursively under `tests/`.

### Host tests
- `test/disp.test.ts` — vitest harness; recursively globs `lib/tests/**/*.test.disp`.
- `test/parser.test.ts` — parser unit tests.
- `test/tree.test.ts` — tree calculus runtime tests.

## Implementation status

**The codebase now implements the unified seven-primitive kernel shape for the core library types.** Pi, Bool, Nat, Eq, Type, and Ord are ordinary library types under `lib/types/`; the kernel no longer has per-type handlers for them.

### Landed
- ✅ **Native parametric walker** active. Reflective predicates on hypotheses (`is_neutral`, `is_fork`, `pair_fst`, `tree_eq`, `has_sig` against a hypothesis) return `Fail` under `checked_apply`. Forgery of neutral-rooted forks via user code is rejected (stem-rule constructor check). I-shortcut is the only soundness carve-out. Verified by `lib/walker.test.disp` and `lib/soundness.test.disp`.
- ✅ **Walker-safe Pi/Arrow** via the `kernel.unguard` handler. `unguard_checked` is a wait-form whose application dispatches via the kernel signature route — handler runs raw, body's triage on the type variable is safe. Polymorphic Pi (`Pi Type ({A} -> Arrow A A)`-style) type-checks under the walker.
- ✅ **Generic predicate/eliminator routing.** `Bool`, `Nat`, `Eq`, `Ord`, `Pi`, and `Type` are predicate_frame types. `bool_rec`, `nat_rec`, `ord_rec`, and `eq_J` are built via `eliminator_frame`.
- ✅ **Canonical Type.** Ranked `Type k` syntax has been removed in favor of the single library value `Type`.
- ✅ **Field exports.** Library files use explicit `name := expr` exports. Duplicate exported fields and duplicate record-type fields are parse errors.
- ✅ **Bool Scott encoding.** `prelude.disp`'s `TT`/`FF` are `{m,ct,cf}->ct` / `{m,ct,cf}->cf` (compiled tree shapes `K K` / `K (K I)`). `select` / `select_lazy` dispatch via Scott eliminator application.

### Remaining Work

- Make `Type` stricter about validating type metadata and library type-former laws.
- Implement `wf_fix`, `Total`, and `TotalWith` from the type-theory doc.
- Improve source-span diagnostics and multi-error reporting.

## Compiler workarounds

Two issues affect kernel-level code involving recursion or multi-line conditional dispatch:

- **Match arm bodies are single-line.** A multi-line match arm body like `FF => triage \n (arg1) \n (arg2)` parses only the bare `triage` because the arm body uses `lineExpr`. Wrap multi-line content in parens to make it one atom: `FF => (triage \n (arg1) \n (arg2))`. Same workaround for multi-line `let` bodies.
- **`select_lazy` + self-recursion blows the compile-time budget.** A thunk `{_} -> self meta (pair_snd x)` compiles to `K body`. After outer bracket abstraction, `cirToTree`'s reduction of the closed combinator eagerly evaluates internal apps via `applyTree` (10M-step budget). For self-referential expressions, this fires fix-unfolding at compile time even though runtime semantics would short-circuit via `select_lazy`'s lazy thunk dispatch. **Workaround**: use `match` instead of `select_lazy` for bodies containing recursive calls. `match` desugars to `select branchTT branchFF cond fvs...` where each branch is wrapped in a closure over its free vars, side-stepping the eager K-body evaluation.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`recq` lazy self-proxy.** Each kernel handler receives `(ks, raw, query)`. `ks` is a lazy proxy (`{q} -> wait raw q`) so `ks.field` references resolve only when forced. Avoids forcing recursive field selection too early.
- **Wait-based types.** `wait(checker)(metadata)`. Signature = `pair_fst(T)` (constant per checker). Metadata = `pair_snd(pair_snd(T))`. Type-former recognition is via signature comparison.
- **H-rule inlined.** Each checker reconstructs its own type via `wait (ks query) meta` for H-rule self-comparison.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.
- **Native fast-paths.** `tree_eq` and `checked_apply` (the dispatcher + walker) are native fast-paths in `src/tree.ts`. They produce bit-identical results to the in-language reference; the in-language code is the spec.

## Testing

`npm test` runs vitest. `lib/tests/**/*.test.disp` is the primary object-language suite. `test/parser.test.ts` and `test/tree.test.ts` cover the host infrastructure.

## Operating notes

- Type checking is raw `apply(T, v) = TT`. Types are wait-based raw functions.
- `is_neutral` is a fork-shape check plus an O(1) signature check against `kernel.hyp_reduce`.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions are safe but should be avoided for clarity.
- Files with any `name := expr` field use the export model where only `:=` fields export. Files with only `let`/`test`/`open` use legacy mode where all `let` bindings export. Prefer `name := expr` for new code.
