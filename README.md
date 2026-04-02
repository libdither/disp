# Disp

A dependently-typed programming language built on tree calculus.

Tree calculus is natively reflective — terms ARE data, so the type checker, the optimizer, and the programs it produces all inhabit the same universe. Disp uses a tree-native type system where types and programs are both trees, with type annotations embedded directly into compiled combinators.

## Quick Start

```shell
npm install
npx tsx src/main.ts
```

This starts the REPL, which auto-loads `prelude.disp` (Bool, Nat, Pair, Either, arithmetic).

```
> let id : (A : Type) -> A -> A := {A x} -> x
id : (A : Type) -> A -> A

> not true
false : Bool

> add 3 4
7 : Nat

> :type not true
Bool

> :tree {x} -> x
(tri (tri t t) t)
```

## Running Tests

```shell
npm test
```

## Architecture

```
src/parse.ts                  -- Tokenizer + recursive descent parser -> SExpr
src/tree-native-elaborate.ts  -- TypedExpr pipeline: elaborate (Phase 1) + compile (Phase 2)
src/tree-native-checker.ts    -- Annotated tree checker: checkAnnotated(defs, ann, type)
src/tree.ts                   -- Tree calculus runtime: hash-consed trees, eager evaluation
src/compile.ts                -- Legacy compiler: bracket abstraction, FIX combinator
src/tree-native.ts            -- Tree-native builtins and step functions
src/repl.ts                   -- Interactive REPL with commands
src/main.ts                   -- Entry point
```

**Pipeline**: Parse (source -> SExpr) -> Elaborate (SExpr -> TypedExpr) -> Compile (TypedExpr -> annotated Tree) -> Check (annotated Tree -> bool)

## Syntax

```
let name : type := value       -- typed definition
let rec name : type := value   -- recursive definition
{x y z} -> body                -- lambda (multi-param sugar)
(x : A) -> B                   -- dependent function type (Pi)
A -> B                         -- non-dependent function type
f x                            -- application
Type                           -- the universe
true / false                   -- booleans (tree-encoded)
0, 1, 2, ...                   -- natural numbers (tree-encoded)
Tree / Bool / Nat              -- primitive types
leaf / stem / fork             -- tree constructors
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

## Design Notes

- **Tree calculus application is eager** — `apply(f, x)` recursively evaluates. Trees are always in normal form.
- **Type:Type** — inconsistent as logic, fine as a programming language. No universe hierarchy.
- **Primitive types** — Tree, Bool, Nat are tree-encoded (not Church-encoded). true=leaf, false=stem(leaf), zero=leaf, succ(n)=stem(n).
- **Annotated trees** — type annotations (D at S nodes) are embedded in the compiled tree. The checker verifies structural typing rules directly.
- **Bracket abstraction** is optimized: eta reduction, S(K p)(K q) = K(p q), S(K p) I = p.

See `PLAN.md` for the full roadmap including neural program synthesis.
