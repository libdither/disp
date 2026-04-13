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

## Type Systems as Composable Families of Predicates

Ultimately what a function type `A -> B` or `FunCheck(A,B)(f)` does is takes a program `p`, and through some inspection of the program (maybe by cases, maybe through recursion and then by cases) can somehow ensure that given `A(input) = true`, `B(f(input)) = true`. In theory you can write a 'general purpose' version of this composed type where it just iterates through all trees, notes all of them that satisfy `A`, and then ensure those inputs, when run via the function, satisfy `B` (and typechecking *will terminate* if your domain is finite and your function is terminating!). For most data structures though, this is *way* too infeasible.

In practice, functions have structure, and utilize primitive operators that have known (or assumed to be known) types. 

