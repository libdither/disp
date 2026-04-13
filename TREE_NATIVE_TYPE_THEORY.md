# Tree-Native Type Theory

A dependent type system for tree calculus where types are executable predicate trees and type equality is tree identity.

This document states the core idea. The operational machinery — elaboration universe, checking, conversion, erasure to runtime — is in [`ELABORATION_DESIGN.md`](ELABORATION_DESIGN.md).

## Trees as Combinators

In tree calculus, every tree is one of three things:

```
leaf          As a function: apply(leaf, x) = stem(x)
stem(a)       As a function: apply(stem(a), x) = fork(a, x)
fork(a, b)    A combinator whose behavior depends on a
```

Every `fork(a, b)` falls into exactly one of three categories, determined by the shape of `a`:

```
fork(leaf,      v)  =  K(v)        Constant: ignores argument, returns v
fork(stem(c),   b)  =  S(c, b)     Share: applies c and b to argument, combines
fork(fork(c,d), b)  =  Triage      Match: dispatches on argument's tag
```

Compiled programs are trees of S/K/Triage nodes. S = contraction (argument shared), K = weakening (argument discarded), Triage = elimination (pattern-match).

## Types as Predicates

**Central claim**: a type is a tree that, applied to a value, returns `true` (= `leaf`) or `false` (= `stem(leaf)`). Types are not opaque tags — they are executable predicates. The universal type-checking operation is `apply(type, value)`.

```
Tree  = {_} -> true                      Accepts everything
Bool  = triage true isLeaf (K(K false))  Accepts leaf and stem(leaf)
Nat   = fix {self} ->                    Accepts leaf and stem-chains
          triage true self (K(K false))
```

Types are first-class: pass them around, apply them, store them.

```
apply(Nat, leaf)             = true
apply(Nat, stem(stem(leaf))) = true
apply(Nat, fork(leaf, leaf)) = false
apply(Bool, stem(leaf))      = true
```

**Type : Type**. Without a universe hierarchy, the predicate for types is `Tree = K(true)` — every tree is a valid type. Inconsistent as logic, fine as a programming language. Universes can be added later.

**Termination**. Base predicates (Nat, Bool) terminate by structural recursion. User-defined predicates may diverge; the runtime uses evaluation budgets as a practical bound. Decidability is not guaranteed for arbitrary predicates — the known tradeoff of types-as-programs.

## Pi Types as Partially-Applied Checkers

A function type is `PiCheck` partially applied to its domain and codomain family:

```
Pi(A, B) = apply(apply(PiCheck, A), B)
```

`A` is a domain predicate. `B` is a codomain family — `apply(B, x)` is the output type for input `x`. Non-dependent `A -> C` is `Pi(A, K(C))`.

`PiCheck` is itself a tree program. Partial application to `A` and `B` eagerly reduces, producing a residual predicate specialized to that Pi type — the **Futamura projection** applied to type checking. `apply(Pi(A,B), v)` returns true/false via the same `apply` used for base types.

This uniformity is load-bearing: base types and compound types share one protocol. The elaborator never needs to case-split on "is this a Pi or a base predicate?" at runtime — both answer `apply`.

## Hash-Consing and O(1) Type Equality

Trees are hash-consed: structurally equal trees share the same id.

- **Type equality is tree identity.** `a.id === b.id` decides equality in O(1), no normalization needed.
- **Pi types are shared.** `Pi(Nat, K(Nat))` is constructed once; every occurrence of `Nat -> Nat` in the program points to the same tree.
- **Specialized checkers are shared.** The residual predicate produced by `apply(apply(PiCheck, A), B)` is shared across all uses of that Pi type.

Hash-cons identity as the conversion relation is the property that makes the whole system cheap. Any design change that makes definitionally-equal types have different tree structure — by introducing ad-hoc wire formats, unnormalized forms, or redundant tags — undermines this.

## Prior Art

Type checking S/K/I combinators (simple types) is classical: Curry & Feys 1958, Hindley 1969.

Barry Jay ("Typed Program Analysis without Encodings," PEPM 2025) types tree calculus using structural program types with subtyping. Key difference: Jay's types are inert data in a separate grammar. Here, types are executable predicates in the same language as values — which is what makes dependent types cheap (codomain families compute; executable types make computation free).

Altenkirch et al. (FSCD 2023) work algebraically with categories of families. Zaldivar Aiello (2024) adds an M combinator. Neither produces a practical checking algorithm.

Novel here: types as executable predicates, Pi types as partially-applied checkers sharing one `apply` protocol with base types, hash-cons identity as the conversion relation, and a checker that is itself a tree program — all inside the smallest known Turing-complete calculus (three constructors, five reduction rules).
