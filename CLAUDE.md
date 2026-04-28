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
- `src/ast.ts` — surface AST types. **Stale:** needs to match `SYNTAX.typ`.
- `src/parse.ts` — tokenizer / parser / bracket-abstraction. **Stale:** implements old surface syntax.
- `src/elaborate.ts` — surface elaborator. **Stale.**
- `src/run.ts` — driver. **Stale.**
- `test/nbe_design.test.ts` — **NbE design validation** (33 tests, TypeScript). The semantic reference for the type system. Tests: base types, arrow types, higher-order functions, polymorphic identity, indexed families, universe hierarchy, cumulativity, rejection of ill-typed terms.
- `test/nbe_tree.test.ts` — **Tree-level NbE** (29+ tests). Tag infrastructure, Val recognizers, napply_simple, wait+fix recursion, deferred branching — all as tree-calculus programs on the unmodified runtime.
- `test/tree.test.ts` — tree calculus runtime tests.
- `lib/{debruijn,ctxtree,semantic}/DESIGN.md` — per-backend strategy notes. Predate the current design; treat as historical reference.

## Current state (as of 2026-04-28)

- **`TYPE_THEORY.typ` rewritten.** Specifies the NbE-based type system: Val domain (VLam, VHyp, VStuck), napply with universal H-rule, type_of_neutral spine inference, Pi/Type/Nat/Bool as predicates. Documents the select-then-apply compilation pattern.
- **Design validated by two test suites.** `nbe_design.test.ts` (40 TypeScript prototype tests) validates the full architecture. `nbe_tree.test.ts` (89 tree-level tests) validates the complete NbE pipeline as tree programs.
- **Full NbE pipeline running as tree programs.** napply (with H-rule), type_of_neutral, conv (fast_eq + structural), Pi construction (mkPi_prog), Type n (universe predicate with 4 cases + cumulativity), Nat, Bool, nat_le/lt, fresh_hyp --- all tree programs. Integration test: polymorphic identity `{A:Type 0, x:A} -> x` type-checks against `Pi(Type 0, {A}->Pi(A, {_}->A))`.
- **Performance wall resolved.** The bracket-abstraction eagerness bug (S-combinator evaluating thunk bodies in non-taken branches) was fixed by the select-then-apply pattern. All checks complete within the 10M-step budget.
- **Parser and elaborator still stale.** Rewrite needed. The elaborator is the remaining frontier for self-hosting.

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application.
- **`ited` for deferred branching.** Branches are `{_} -> expr` thunks; only the chosen one is forced. Required because `triage` evaluates all branches eagerly. **Caveat:** bracket abstraction over shared free variables defeats `ited`'s laziness; use select-then-apply pattern instead (see `KERNEL_DESIGN.md`).
- **Select-then-apply for branching with shared vars.** Compile branches as closed functions, select via `ite2`, apply shared args after selection. Critical for napply, type_of_neutral, Nat, Type n.
- **Fix outside VLam.** Recursive predicates use `fix` for the body, `mkVLam` wraps externally. `fix` returns wait-encoded partials, not VLams.
- **Hash-consing is load-bearing.** `conv = fast_eq` is O(1). Deterministic elaboration ensures same type → same tree.

## Testing

`npm test` runs vitest. `test/nbe_design.test.ts` and `test/nbe_tree.test.ts` are the primary test suites for the type system. `test/tree.test.ts` tests the runtime.

## Operating notes

- NbE backends are tree programs, not TypeScript. Host implementations are optimizations only.
- The reference for the type system is `TYPE_THEORY.typ` + `test/nbe_design.test.ts`. When they disagree, investigate.
- Prefer editing existing files over creating new ones.
