# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system whose checker lives as an inhabitant of the object language, not just as a host-language function.

**Status**: early. The tree-calculus runtime is implemented. The elaborator, surface parser, and kernel are being rebuilt against [`ELABORATION_DESIGN.md`](ELABORATION_DESIGN.md); prior prototype code was deleted rather than carried forward.

## Documentation

- [`GOALS.md`](GOALS.md) — the long-term vision (neural-guided synthesis, self-improving optimizer).
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — core idea: trees as S/K/Triage combinators, types as executable predicates, Pi as partially-applied `PiCheck`, hash-cons identity as type equality.
- [`ELABORATION_DESIGN.md`](ELABORATION_DESIGN.md) — operational spec: two universes (elab + runtime), tagged forms, `normalize` + `fastEq` conversion, metas as ctx entries, `erase` to SKI after checking.
- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — the discipline governing how features are added. Load-bearing — read before making design changes.

## Running Tests

```shell
npm install
npm test
```

Currently covers the tree-calculus runtime only.

## Current Code

```
src/tree.ts        -- Tree calculus runtime: hash-consed trees, eager apply, budgets
test/tree.test.ts  -- Runtime tests
```

Surface syntax, elaborator, and kernel are not yet (re)implemented.
