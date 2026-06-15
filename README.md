# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system whose checker lives as an inhabitant of the object language, not just as a host-language function.

## Documentation

- [`GOALS.md`](GOALS.md) — the long-term vision (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — authoritative type-theory spec. Kernel of two Σ-operations (`hyp_reduce`, `bind_hyp`) plus the dispatcher `param_apply` over a fixed Σ; manifest contracts over the tree-calculus substrate; library types (Pi, Bool, Nat, Eq, …), validators, and cubical extensions on top.
- [`archive/TYPE_THEORY_LEGACY.typ`](archive/TYPE_THEORY_LEGACY.typ) — previous spec (seven kernel primitives), kept for history. The codebase has migrated off it to the two-Σ-op shape.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms (current codebase).
- [`CLAUDE.md`](CLAUDE.md) — current implementation status, code layout, open work.

## Running tests

```shell
npm install
npm test
```

## Layout

```
src/tree.ts        -- Tree calculus runtime + tree_eq native fast-path
src/parse.ts       -- Tokenizer / parser
src/compile.ts     -- Bracket abstraction, elaborator, program driver
src/run.ts         -- File runner

lib/prelude.disp          -- Fundamental combinators
lib/kernel/*.disp         -- Two-Σ-op kernel + library types (core.disp, prelude.disp)
lib/std/**/*.disp         -- Standard library modules
lib/tests/**/*.test.disp  -- Object-language test suite
test/                     -- Vitest harness + parser/runtime unit tests
```
