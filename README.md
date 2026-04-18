# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system whose checker lives as an inhabitant of the object language, not just as a host-language function.

**Status**: three competing type-system designs are being implemented side-by-side under `lib/{debruijn,ctxtree,semantic}/`, tested against a shared implementation-agnostic suite in `lib/suite/main.disp`. The winner becomes the kernel.

## Documentation

- [`GOALS.md`](GOALS.md) — the long-term vision (neural-guided synthesis, self-improving optimizer).
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — core idea: trees as S/K/Triage combinators, types as executable predicates, Pi as partially-applied `PiCheck`, hash-cons identity as type equality.
- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — the discipline governing how features are added. Load-bearing — read before making design changes.
- `lib/debruijn/DESIGN.md` — array-context closure NbE (reference oracle).
- `lib/ctxtree/DESIGN.md` — tagged-form bind-tree NbE with parallel ctx-tree; builds on `lib/predicates.disp`.
- `lib/semantic/DESIGN.md` — Val-domain NbE on raw bracket-abstracted SKI (Dybjer/Filinski style).

## Running Tests

```shell
npm install
npm test
```

## Layout

```
src/tree.ts          -- Tree calculus runtime
src/parse.ts         -- Parser: def/test/elab/raw + `use "path"` + { } blocks
src/elaborate.ts     -- Host-side tagged-form elaborator
src/run.ts           -- Driver
lib/predicates.disp  -- Current ctx-tree-threaded kernel (ctxtree backend uses this)
lib/{debruijn,ctxtree,semantic}/
                     -- Competing backend designs + impl.disp stubs
lib/suite/main.disp  -- Shared implementation-agnostic test suite
```
