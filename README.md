# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective -- terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp uses the Calculus of Constructions (CoC) as its type theory, with CoC terms encoded directly as trees.

## Quick Start

```shell
npm install
npx tsx src/main.ts
```

This starts the REPL, which auto-loads `prelude.disp` (Church-encoded Bool, Nat, Pair, Either).

```
> let id : (A : Type) -> A -> A := {A x} -> x
id : (A : Type) -> A -> A

> id Bool true
true : (R : Type) -> R -> R -> R

> add 3 4
7 : (R : Type) -> (R -> R) -> R -> R

> :type not true
(R : Type) -> R -> R -> R

> :tree {x} -> x
(tri (tri t t) t)
```

## Running Tests

```shell
npm test
```

## Architecture

```
src/tree.ts     -- Tree calculus runtime: hash-consed trees, eager evaluation
src/parse.ts    -- Tokenizer + recursive descent parser -> SExpr
src/compile.ts  -- SExpr -> Tree via bracket abstraction (S/K/I combinators)
src/coc.ts      -- CoC-on-trees type checker + tree-native builtins
src/repl.ts     -- Interactive REPL with commands
src/main.ts     -- Entry point
```

**Pipeline**: Parse (source -> SExpr) -> Type Check (CoC-on-trees) -> Compile (bracket abstraction) -> Evaluate (tree calculus)

## Syntax

```
let name : type := value       -- typed definition
let rec name : type := value   -- recursive definition
{x y z} -> body                -- lambda (multi-param sugar)
(x : A) -> B                   -- dependent function type (Pi)
A -> B                         -- non-dependent function type
f x                            -- application
Type                           -- the universe
true / false                   -- Church booleans
0, 1, 2, ...                   -- Church numerals
{x : A, y : B}                 -- record type (Church-encoded)
<Left : A | Right : B>         -- coproduct type (Church-encoded)
```

## REPL Commands

- `:type <expr>` / `:t` -- show the type of an expression
- `:tree <expr>` -- show the compiled tree calculus form
- `:ctx` -- show the current context
- `:load <file>` / `:l` -- load declarations from a file
- `:save <file>` / `:s` -- save declarations to a file
- `:help` / `:h` -- show help
- `:quit` / `:q` -- exit

## Goals

- **All-purpose**: scripting, shells, and compiled languages
- **Speed**: competitive with Rust/C for high-level code via neural compilation
- **Provably safe**: programs carry proofs of correctness
- **Syntax-agnostic**: multiple syntax frontends for the same core

## Design Notes

- **Tree calculus application is eager** -- `apply(f, x)` recursively evaluates. Trees are always in normal form.
- **Type:Type** -- inconsistent as logic, fine as a programming language. No universe hierarchy.
- **Church encodings only** -- no inductive types. Booleans, naturals, pairs, etc. are all Church-encoded.
- **Bracket abstraction** is optimized: eta reduction, S(K p)(K q) = K(p q), S(K p) I = p.
- **CoC terms are trees** -- the type checker operates on tree-encoded terms. Tree calculus `apply()` IS substitution.

See `PLAN.md` for the full roadmap including neural program synthesis (Phase 2).
