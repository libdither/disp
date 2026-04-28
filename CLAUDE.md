# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Read before making design changes

- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — **load-bearing**. The discipline governing what's allowed in the codebase.
- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — the semantics the object language commits to. Types as predicates, the Val domain, `napply` with H-rule, type constructors, soundness invariants. Authoritative for anything touching the kernel.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST shape. Authoritative for the parser.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms: tag encoding, wait/fix, deferred branching, bracket abstraction optimizations, performance notes.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/tree.ts`) is the only host code; everything else — NbE operations, type predicates, the elaborator — are tree programs.

## Code layout

- `src/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `FAST_EQ`.
- `src/parse.ts` — tokenizer / parser / bracket-abstraction / driver. Implements `SYNTAX.typ` grammar: `let`/`test`/`use` items, `{x : A} -> e` binders, `A -> B` arrow sugar, `{x := e}` recValues, `{x : A}` recTypes, `.field` projection. Bracket abstraction with η-reduction + K-composition optimizations.
- `src/run.ts` — file runner: loads `.disp`, parses, compiles, executes tests.
- `test/nbe_tree.disp` — **Primary test suite** (152 tests). The full NbE pipeline as `.disp` source: combinators, tag infrastructure, napply, type constructors, typed eliminators, Eq type, arithmetic, integration tests.
- `test/nbe_tree.test.ts` — vitest harness that runs `nbe_tree.disp` via `runFile`.
- `test/parser.test.ts` — parser unit tests (58 tests): tokenizer, expressions, items, compilation, errors.
- `test/tree.test.ts` — tree calculus runtime tests (21 tests).

## Current state (as of 2026-04-28)

- **Parser rewritten to match `SYNTAX.typ`.** New comment syntax (`//`, `/* */`), `let`/`test` items, unified `Expr` AST with binders, recTypes, recValues, use expressions, projections. Church-encoded records with field metadata for projection.
- **Full NbE pipeline as `.disp` source** (152 tests). All NbE components are tree programs running via the parser: napply (with H-rule), type_of_neutral (CPS ton_check), conv (fast_eq + structural), Pi/Arrow construction, Type n (universe predicate with cumulativity), Nat, Bool, nat_le/lt, fresh_hyp.
- **Typed eliminators.** `bool_rec` and `nat_rec` are neutral-aware recursors: they check `is_neutral(target)` before dispatching, producing `VStuckElim(motive, target)` when stuck. `type_of_neutral` handles the new neutral form by applying the motive. This solves the triage-on-neutral problem for functions that branch on their arguments.
- **Eq type implemented.** `mkEq A x y` predicate with EQ_TAG metadata, `refl = LEAF`, J eliminator (`eq_J`), transport (`eq_subst`), symmetry (`eq_sym`), congruence (`eq_cong`).
- **Arithmetic working.** `add` via select-then-apply + fix. Eq proofs on concrete values including commutativity (`add 2 3 = add 3 2`).
- **Pi body normalization.** `fast_eq(napply(cod, napply(f, hyp)), TT)` — anything that isn't TT becomes FF. Handles the case where checking against abstract hypothesis types produces stuck terms.
- **Bracket abstraction optimized.** Three optimizations: η-reduction (`[x](f x) → f`), K-composition (`S(K p)(K q) → K(p q)`), `S(K p)(I) → p`. Binder parameters correctly shadow scope variables.
- **Elaborator still needed.** The parser compiles to untyped tree calculus (types erased). The elaborator is the remaining frontier: it would supply motives to eliminators, manage hypothesis depths, and support the full typed compilation pipeline.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`ited` for deferred branching.** Branches are `{_} -> expr` thunks; only the chosen one is forced. Required because `triage` evaluates all branches eagerly. **Caveat:** bracket abstraction over shared free variables defeats `ited`'s laziness; use select-then-apply pattern instead (see `KERNEL_DESIGN.md`).
- **Select-then-apply for branching with shared vars.** Compile branches as closed functions, select via `ite2`, apply shared args after selection. Critical for napply, type_of_neutral, Nat, Type n, add.
- **Fix outside VLam.** Recursive predicates use `fix` for the body, `mkVLam` wraps externally. `fix` returns wait-encoded partials, not VLams.
- **Typed eliminators for neutral-awareness.** `bool_rec`/`nat_rec`/`eq_J` check `is_neutral` before dispatching. When stuck, produce `VStuckElim(motive, target)`. The motive (supplied at each elimination site) determines the return type. Raw `ite2`/`triage` should NOT be used on values that might be neutral.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.

## Testing

`npm test` runs vitest. `test/nbe_tree.disp` is the primary test suite (152 tests run as `.disp` source). `test/parser.test.ts` (58 tests) and `test/tree.test.ts` (21 tests) cover the host infrastructure.

## Operating notes

- NbE backends are tree programs, not TypeScript. Host implementations are optimizations only.
- The reference for the type system is `TYPE_THEORY.typ` + `test/nbe_tree.disp`. When they disagree, investigate.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation. Name collisions between scope defs and lambda params are safe but should be avoided for clarity.
