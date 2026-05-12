# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system whose checker lives as an inhabitant of the object language, not just as a host-language function.

## Documentation

- [`GOALS.md`](GOALS.md) — the long-term vision (neural-guided synthesis, self-improving optimizer).
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — authoritative type-theory spec. Unified design: small kernel of seven primitives; inductive types and quantifier types are library-defined.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar.
- [`COMPILATION.typ`](COMPILATION.typ) — parse/elaborate/emit pipeline.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms.
- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — the discipline governing how features are added. Load-bearing — read before making design changes.
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
lib/kernel/*.disp         -- Seven primitive handlers, helpers, walker reference
lib/types/*.disp          -- Library types: Pi, Type, Bool, Nat, Eq, Ord, conversion
lib/std/**/*.disp         -- Standard library modules
lib/tests/**/*.test.disp  -- Object-language test suite
test/                     -- Vitest harness + parser/runtime unit tests
```
