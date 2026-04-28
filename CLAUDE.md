# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — the semantics the object language commits to. Types as predicates, wait-based type encoding, the H-rule, type constructors, soundness invariants. Authoritative for anything touching the kernel.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms: tag encoding, wait/fix, deferred branching, bracket abstraction optimizations, performance notes.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — NbE operations, type predicates, the elaborator — are tree programs.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `FAST_EQ`.
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar: `let`/`test`/`use`/`open` items, `{x : A} -> e` binders, `A -> B` arrow sugar, `{x := e}` recValues, `{x : A}` recTypes, `.field` projection. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `lib/prelude.disp` — fundamental combinators (TT/FF, triage, ite2/ited, pairs, and, wait/fix).
- `lib/nbe.disp` — tag infrastructure (neutrals only: VHyp/VStuck/VStuckElim), val_apply, type_apply, ton_check, conv, Pi/Arrow constructors via wait-based encoding, metadata-tagged recognition (PI_TAG/UNIV_TAG/EQ_TAG in metadata).
- `lib/types.disp` — type definitions using wait-based checkers: Nat, Bool, Eq, Type n. Typed eliminators (bool_rec, nat_rec, eq_J), arithmetic. Each checker inlines the H-rule via `fix`.
- `lib/*.test.disp` — tests per module (prelude: 8, nbe: 26, types: 102).
- `test/disp.test.ts` — vitest harness that globs `lib/*.test.disp` and runs each.
- `test/parser.test.ts` — parser unit tests (58 tests).
- `test/tree.test.ts` — tree calculus runtime tests (21 tests).

## Current state (as of 2026-04-28)

- **Parser rewritten to match `SYNTAX.typ`.** New comment syntax (`//`, `/* */`), `let`/`test` items, unified `Expr` AST with binders, recTypes, recValues, use expressions, projections. Church-encoded records with field metadata for projection.
- **Full NbE pipeline as `.disp` source** (136 tests across 3 files). Split into `lib/prelude.disp` (combinators), `lib/nbe.disp` (neutrals, type_apply, val_apply, ton_check, conv, Pi/Arrow), `lib/types.disp` (Nat, Bool, Eq, Type n, eliminators, add). Files use `open use "dep.disp"` for dependencies. Types are `wait(checker)(metadata)` — type checking is raw `apply(T, v) = TT`.
- **Typed eliminators.** `bool_rec` and `nat_rec` are neutral-aware recursors: they check `is_neutral(target)` before dispatching, producing `VStuckElim(motive, target)` when stuck. `type_of_neutral` handles the new neutral form by applying the motive. This solves the triage-on-neutral problem for functions that branch on their arguments.
- **Eq type implemented.** `mkEq A x y` predicate with EQ_TAG metadata, `refl = LEAF`, J eliminator (`eq_J`), transport (`eq_subst`), symmetry (`eq_sym`), congruence (`eq_cong`).
- **Arithmetic working.** `add` via select-then-apply + fix. Eq proofs on concrete values including commutativity (`add 2 3 = add 3 2`).
- **Wait-based type encoding.** Types are `wait(checker)(fork(TAG, payload))`. Type checking is raw `apply(T, v) = TT` — no napply needed. Each type former (Pi, Nat, Bool, Eq, Type n) has its own checker with the H-rule inlined via `fix`. `type_apply` handles abstract type variables (neutral-as-type) via ton_check; `val_apply` handles neutral-aware function application.
- **Bracket abstraction optimized.** Three optimizations: η-reduction (`[x](f x) → f`), K-composition (`S(K p)(K q) → K(p q)`), `S(K p)(I) → p`. Binder parameters correctly shadow scope variables.
- **Elaborator still needed.** The parser compiles to untyped tree calculus (types erased). The elaborator is the remaining frontier: it would supply motives to eliminators, manage hypothesis depths, and support the full typed compilation pipeline.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`ited` for deferred branching.** Branches are `{_} -> expr` thunks; only the chosen one is forced. Required because `triage` evaluates all branches eagerly. **Caveat:** bracket abstraction over shared free variables defeats `ited`'s laziness; use select-then-apply pattern instead (see `KERNEL_DESIGN.md`).
- **Select-then-apply for branching with shared vars.** Compile branches as closed functions, select via `ite2`, apply shared args after selection. Critical for type checkers, type_of_neutral, Nat, Type n, add.
- **Wait-based types.** `wait(checker)(fork(TAG, payload))`. Signature = `pair_fst(T)` (constant per checker). Metadata = `pair_snd(pair_snd(T))`. Tags (PI_TAG, UNIV_TAG, EQ_TAG) live inside metadata, not as wrapper tags.
- **H-rule inlined via fix.** Each checker reconstructs its own type via `wait(self)(meta)` for H-rule self-comparison. `fix` provides the self-reference.
- **`type_apply(T, v)`.** Minimal replacement for napply — for neutral T uses ton_check, for wait-based T uses raw `apply(T, v)`.
- **`val_apply(f, x)`.** Neutral-aware function application — produces VStuck for neutral functions, raw apply otherwise.
- **Typed eliminators for neutral-awareness.** `bool_rec`/`nat_rec`/`eq_J` check `is_neutral` before dispatching. When stuck, produce `VStuckElim(motive, target)`. The motive (supplied at each elimination site) determines the return type. Raw `ite2`/`triage` should NOT be used on values that might be neutral.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.

## Testing

`npm test` runs vitest. `lib/*.test.disp` is the primary test suite (136 tests across 3 files). `test/parser.test.ts` (58 tests) and `test/tree.test.ts` (21 tests) cover the host infrastructure.

## Operating notes

- NbE backends are tree programs, not TypeScript. Host implementations are optimizations only.
- The reference for the type system is `TYPE_THEORY.typ` + `lib/*.disp`. When they disagree, investigate.
- Type checking is raw `apply(T, v) = TT`, not napply. Types are wait-based raw functions.
- `type_apply` is needed when the codomain might be an abstract type variable (neutral-as-type).
- Metadata tags (PI_TAG, UNIV_TAG, EQ_TAG) live inside wait metadata, not as VLam wrapper tags.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions between scope defs and lambda params are safe but should be avoided for clarity.
