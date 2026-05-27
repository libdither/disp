# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system whose checker lives as an inhabitant of the object language, not just as a host-language function.

## Documentation

- [`GOALS.md`](GOALS.md) — the long-term vision (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — authoritative type-theory spec. Kernel of four Σ-operations (`hyp_reduce`, `bind_hyp`, `eliminator_frame`, `postulate`) plus a Σ-parameterized dispatcher (`safe_apply`); manifest contracts over the tree-calculus substrate; library types, validators, and cubical extensions on top.
- [`TYPE_THEORY_LEGACY.typ`](TYPE_THEORY_LEGACY.typ) — previous spec (seven kernel primitives). The current codebase still tracks this shape; kept for reference until the migration lands.
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
src/tree.ts        -- Tree calculus runtime + native fast-paths (tree_eq, walker)
src/parse.ts       -- Tokenizer / parser
src/compile.ts     -- Bracket abstraction, elaborator, program driver
src/run.ts         -- File runner

lib/prelude.disp          -- Fundamental combinators
lib/kernel/*.disp         -- Kernel handlers (currently seven; spec target is four), helpers, walker reference
lib/types/*.disp          -- Library types: Pi, Type, Bool, Nat, Eq, Ord, conversion
lib/std/**/*.disp         -- Standard library modules
lib/tests/**/*.test.disp  -- Object-language test suite
test/                     -- Vitest harness + parser/runtime unit tests
```
