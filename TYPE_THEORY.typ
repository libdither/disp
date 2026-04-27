#set document(title: "Disp Type Theory")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#let rulebox(title, body) = block(
  breakable: false,
  above: 1em, below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  {
    text(weight: "bold")[#title]
    v(-0.2em)
    body
  }
)

#align(center, text(22pt, weight: "bold")[Disp Type Theory])
#v(0.3em)
#align(center)[
  The semantics the object language commits to.\
  Companion to #raw("SYNTAX.typ") and #raw("COMPILATION.typ").\
  Reference implementation: #raw("test/nbe_design.test.ts") (33 tests).
]
#v(1em)

= Types as predicates

A *type* is a predicate: applied to a candidate value, it returns
`TT` (accepted) or `FF` (rejected). Checking `v : T` is:

```
  napply(T, v) = TT
```

`napply` is a tree-calculus program --- a single raw tree that the
unmodified runtime executes. It is the *bootstrapping layer* between
the raw tree-calculus runtime and the type system. Types and values
are represented as *Vals* (tagged trees) that `napply` knows how to
evaluate. The raw runtime just runs `napply`; all type-system
intelligence lives inside it.

Why not plain `apply(T, v)`? Because types carry metadata (Pi stores
its domain and codomain; universes store their rank) that
`type_of_neutral` needs for spine inference. This metadata is encoded
as tags inside Vals. Raw `apply` would dispatch on the tag structure
instead of running the predicate body. `napply` handles both: it
recognizes tagged forms and dispatches correctly, and falls through
to raw tree-calculus rules for untagged data.

= `napply` --- the bootstrapping evaluator

`napply` is the single entry point. It is defined as a tree-calculus
program (via `fix` and `wait`):

```disp
napply = fix ({self, f, x} ->
  // H-rule: if x is neutral and its type matches f, short-circuit
  match x
  | VHyp _ _ | VStuck _ _ ->
      if conv f (type_of_neutral x) then TT
      else dispatch self f x
  | _ -> dispatch self f x)

dispatch = {self, f, x} ->
  match f
  | VHyp _ _    -> VStuck f x
  | VStuck _ _  -> VStuck f x
  | VLam _ body -> body x
  | _           -> f x          // raw tree-calculus rules
```

The *H-rule* fires first: if `x` is neutral and `conv(f,
type_of_neutral(x)) = TT`, return `TT` directly. This is the
universal mechanism that makes types-as-predicates work for symbolic
values --- including hypotheses used as types.

The `match` here is pattern dispatch on Val tags. In tree calculus this
compiles to deferred triage (each branch wrapped as a thunk, only the
taken branch forced); see #raw("KERNEL_DESIGN.md") for the `ited`
encoding.

= Vals

Vals are tagged trees that `napply` operates on.

== Val kinds

#table(
  columns: (auto, 1fr, auto),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Meaning*][*Tag*],
  [`VLam(meta, body)`],
    [Function. `meta` carries type-former info (Pi, universe). `body`
     is a tree-calculus function.],
    [`KV_LAM`],
  [`VHyp(type, id)`],
    [Hypothesis --- an opaque symbolic inhabitant of `type`,
     introduced under a binder. Distinct per binder depth.],
    [`KV_HYP`],
  [`VStuck(head, arg)`],
    [Stuck application --- a hypothesis (or deeper stuck) applied to
     an argument that could not reduce.],
    [`KV_STUCK`],
  [data: leaf, stem, fork],
    [Untagged tree data. `Zero = leaf`, `Succ n = fork(leaf, n)`.],
    [none],
)

Tags use a stem-chain encoding inside a double-fork:
`tagged(kind, payload) = fork(fork(TAG_ROOT, kind), payload)` where
`TAG_ROOT` is a fixed canonical tree.

== Metadata

VLam's `meta` field distinguishes type formers. Reflection functions
(`is_pi`, `pi_dom`, `pi_cod`, `is_universe`, `universe_rank`) inspect
`meta` --- they are library functions, not part of `napply`.

Pi metadata: `fork(PI_TAG, fork(domain, codomain_fn))`. \
Universe metadata: `fork(UNIV_TAG, rank)`. \
Plain lambda / data predicate: `LEAF` (no metadata).

= The evaluator: `napply`

`napply(f, x)` is the complete evaluator. It handles all Val kinds and
the five tree-calculus reduction rules. It is a tree-calculus program.

== H-rule (universal, before dispatch)

Before any tag dispatch, `napply` checks: if `x` is a neutral (VHyp
or VStuck) and `conv(f, type_of_neutral(x)) = TT`, return `TT`
directly. This is the mechanism that makes types-as-predicates work
for symbolic values.

The H-rule is *universal* --- it fires for every predicate, including
hypotheses used as types. `napply` has no knowledge of specific type
formers.

= Auxiliary operations

== `conv(a, b) → Bool`

Semantic equality. With hash-consing, `conv = fast_eq` (O(1)) for
identically-constructed Vals. Structural comparison (introducing fresh
hypotheses for Pi codomain comparison) is needed only when the same
type is constructed via different code paths; deterministic elaboration
avoids this in practice.

== `fresh_hyp(A) → Val`

Produces a `VHyp` with stored type `A` and a unique identity derived
from binder depth. Satisfies *opacity* (only the H-rule inspects the
stored type) and *distinctness* (hypotheses under distinct binders are
`conv`-unequal).

== `type_of_neutral(v) → Val`

Spine inference: recursively reads the type from the hypothesis at the
head of a stuck-application chain.

```
type_of_neutral(VHyp(T, _))      = T
type_of_neutral(VStuck(head, arg)) =
  let head_type = type_of_neutral(head)
  napply(pi_cod(head_type), arg)
```

Uses `napply` (without the H-rule) to instantiate codomains.

= Type constructors

Type constructors are VLam values whose bodies implement the checking
logic. `napply` treats them as ordinary lambdas.

== `Pi` --- dependent function type

`Pi(A, B)` produces a VLam with `PI_TAG` metadata carrying domain `A`
and codomain function `B`:

```disp
Pi A B = VLam(
  meta = fork(PI_TAG, fork(A, B)),
  body = {f} ->
    let a = fresh_hyp(A)
    napply(napply(B, a), napply(f, a)))
```

`{x : A} -> B` in surface syntax elaborates to `Pi A ({x} -> B)`.
Non-dependent `A -> B` is `Pi A ({_} -> B)`.

`is_pi` checks the PI_TAG in `meta`. `pi_dom` and `pi_cod` extract from
`meta`. These are library functions.

Type information flows *top-down*: the predicate supplies `A` to
`fresh_hyp`. The term `f` carries no type annotations. Only types need
metadata (for `type_of_neutral`); terms do not.

== `Type n` --- universe family

`Type n` is a VLam with `UNIV_TAG` metadata carrying rank `n`:

```disp
Type n = VLam(
  meta = fork(UNIV_TAG, n),
  body = {t} -> match t
    | VHyp _ _ | VStuck _ _ ->       // Case 4: cumulative neutral
        let t_type = type_of_neutral t
        match t_type
        | VLam (fork UNIV_TAG k) _ -> le k n
        | _ -> FF
    | VLam (fork UNIV_TAG k) _ ->     // Case 1: universe below n
        lt k n
    | VLam (fork PI_TAG (fork dom cod)) _ ->  // Case 2: Pi at rank ≤ n
        and (napply (Type n) dom)
            { let a = fresh_hyp dom
              napply (Type n) (napply cod a) }
    | _ ->                             // Case 3: registered base type
        is_registered t)
```

Case 4 (cumulative neutral) is handled in the body, not by the
H-rule, because cumulativity requires `rank ≤ n` rather than exact
type match. This is the one predicate with body-level neutral logic.

Cumulativity falls out: every case uses `<` or `≤`, monotone in `n`.

== Data types

Data predicates are VLam with no metadata (`meta = LEAF`). The body
pattern-matches on tree structure. The H-rule in `napply` intercepts
neutrals before the body runs, so data predicates never encounter
hypotheses directly.

```disp
Nat = fix ({self} -> VLam(
  meta = LEAF,
  body = {n} -> match n
    | leaf         -> TT                     // Zero
    | fork leaf n' -> napply self n'          // Succ: recurse
    | _            -> FF))
```

== `Eq` --- propositional equality

```disp
Eq A a b = {_} -> conv a b
```

Semantic equality as a type: `Eq A a b` accepts any witness when
`conv(a, b) = TT`.

= Worked examples

== `3 : Nat`

```
napply(Nat, Succ(Succ(Succ(Zero))))
→ pattern match: is_fork? yes, first=leaf? yes, recurse on Succ(Succ(Zero))
→ ... recurse twice more ...
→ napply(Nat, Zero) → fast_eq Zero Zero → TT
```

== `{x : Nat} -> x` checked against `Nat -> Nat`

```
napply(Pi Nat ({_}->Nat), {x}->x)
→ a = fresh_hyp(Nat)
→ napply({x}->x, a) = a
→ napply(Nat, a)
→ H-rule: is_neutral(a)? yes. type_of_neutral(a) = Nat.
  conv(Nat, Nat) = TT → TT
```

== `{f:Nat->Nat, x:Nat} -> f x`

```
napply(Pi (Nat->Nat) ({_}->Nat->Nat), term)
→ h_f = fresh_hyp(Nat->Nat)
→ napply(term, h_f) = {x}->napply(h_f, x) (inner lambda)
→ napply(Pi Nat ({_}->Nat), inner_lambda)
→ h_x = fresh_hyp(Nat)
→ napply(inner_lambda, h_x) = napply(h_f, h_x) = VStuck(h_f, h_x)
→ napply(Nat, VStuck(h_f, h_x))
→ H-rule: type_of_neutral(VStuck(h_f, h_x))
  = napply(pi_cod(type_of_hyp(h_f)), h_x)
  = napply({_}->Nat, h_x) = Nat
  conv(Nat, Nat) = TT → TT
```

== Polymorphic identity

`{A:Type 0, x:A} -> x` checked against `Pi(Type 0, {A}->Pi(A, {_}->A))`:

```
→ h_A = fresh_hyp(Type 0)
→ napply(codomain, h_A) = Pi(h_A, {_}->h_A)
→ h_x = fresh_hyp(h_A)
→ napply({x}->x, h_x) = h_x
→ napply(h_A, h_x)
→ H-rule: type_of_neutral(h_x) = h_A. conv(h_A, h_A) = TT.
```

The H-rule fires on a *hypothesis used as a type*. This is how
abstract type variables work.

== Rejection: `{A:Type 0, x:A} -> x x`

```
→ h_A = fresh_hyp(Type 0), h_x = fresh_hyp(h_A)
→ napply(h_x, h_x) = VStuck(h_x, h_x)
→ napply(h_A, VStuck(h_x, h_x))
→ H-rule: type_of_neutral(VStuck(h_x, h_x)):
  head_type = h_A. is_pi(h_A)? h_A is VHyp, not Pi. FAIL.
  → H-rule doesn't fire.
→ napply(h_A, stuck) = VStuck(h_A, stuck). Not TT. Rejected.
```

= Soundness

Soundness means: if `napply(T, v) = TT`, then `v` semantically
inhabits the type denoted by `T`.

The argument rests on four invariants:

+ *Freshness.* `fresh_hyp` produces opaque, distinct hypotheses.
  Opacity ensures parametricity (one check = universal
  quantification). Distinctness ensures multi-parameter types don't
  conflate their variables.

+ *Semantic equality.* `conv` agrees with tree-calculus identity on
  closed normal forms. Two evaluations that produce the same value
  produce `conv`-equal Vals (via hash-consing).

+ *Predicate faithfulness.* Each type constructor's predicate returns
  `TT` iff the value inhabits the intended type. `Pi`, `Eq`, `Type n`
  are all defined to match their standard semantics.

+ *Universe well-foundedness.* The `Type n` hierarchy is strictly
  well-founded: no cycles, no self-containing universes. Girard's
  paradox is blocked by stratification.

Budget exhaustion (the evaluation limit) is a *completeness*
concession, not an unsoundness. A check that runs out of budget is
rejected, never falsely accepted.

= Elaboration boundary

The elaborator works in the Val domain. It produces `(term_val,
type_val)` and checks via `napply(type_val, term_val) = TT`. After
checking, the term is *quoted* (bracket-abstracted) to a raw
tree-calculus term for runtime execution. Types are erased.

The parser and surface syntax are I/O layers, not part of the type
system. The elaborator is the boundary: it consumes AST, produces Vals
using the NbE API, and emits raw trees.

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`Val`],         [A tagged tree in the NbE domain. One of: VLam, VHyp, VStuck, or untagged data.],
  [`napply(f, x)`],[The evaluator. H-rule + tag dispatch + tree-calculus rules.],
  [`conv(a, b)`],  [Semantic equality on Vals. `fast_eq` with hash-consing.],
  [`fresh_hyp(A)`],[Create a hypothesis of type `A` at the current binder depth.],
  [`type_of_neutral(v)`],[Infer a neutral's type by walking its stuck-application spine.],
  [`is_pi`, `pi_dom`, `pi_cod`],[Reflection on VLam metadata. Library functions.],
  [`is_universe`, `universe_rank`],[Reflection on universe metadata.],
  [`is_neutral`],  [`TT` iff `v` is VHyp or VStuck.],
  [`TT` / `FF`],   [Encoded booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`H-rule`],      [In `napply`: if `x` is neutral and `conv(f, type_of_neutral(x)) = TT`, return `TT`. Universal, not type-specific.],
  [`wait`],        [Deferred application: `wait a b c = a(b)(c)` but `wait(a)(b)` does not evaluate `a(b)`. See #raw("KERNEL_DESIGN.md").],
  [`fix`],         [Fixed-point via `wait`. Demand-driven recursion. See #raw("KERNEL_DESIGN.md").],
  [`TAG_ROOT`],    [Canonical tree prefix distinguishing tagged Vals from data.],
  [`PI_TAG` / `UNIV_TAG`],[Metadata tags in VLam's `meta` field.],
  [*quote*],       [Convert a closed Val to a raw tree-calculus term (bracket abstraction). One-way, lossy (metadata stripped).],
  [*registry*],    [Ambient set of rank-0 base types recognized by `Type n`.],
)

= Open problems

== Compilation efficiency

`napply`, type constructors, and their dependencies compile to
tree-calculus programs of 1000--3000 nodes via bracket abstraction.
Executing these on the unoptimized runtime requires tens of millions
of reduction steps per type-check call. η-reduction and K-composition
help but do not close the gap.

The lambada project's compilation pipeline (with `wait`-based
recursion, optimized combinator selection, and self-hosted compilation)
is the reference for practical tree-calculus program execution. Native
TypeScript fast-paths for `napply` and `conv` are the near-term
solution, per the development philosophy ("host implementations are
optimizations").

The design is validated semantically (40/40 TypeScript prototype
tests). The tree-level foundation works (29+ tests for `napply_simple`,
tag infrastructure, `wait`+`fix`). The remaining gap is compilation
quality, not design correctness.

== Structural `conv`

The current implementation uses `conv = fast_eq` (hash-consing
identity). This suffices when the elaborator constructs types
deterministically (same source → same tree). Full structural
comparison (introducing fresh hypotheses for Pi codomain comparison)
is validated in the TypeScript prototype but not yet as a tree program.

== `apply(T, v) = TT` without `napply`

The current design requires `napply(T, v) = TT` because types are
VLam (tagged) and raw `apply` dispatches on the tag structure rather
than running the predicate body. A future design might compile type
predicates to raw bracket-abstracted combinators (with `napply` logic
inlined into each predicate's body), eliminating the `napply` entry
point. This would require either:
- inlining the H-rule and tag dispatch into every predicate, or
- a compilation strategy that makes VLam reduce correctly under raw
  `apply` (e.g., encoding VLam as a combinator whose tree-calc
  reduction matches `napply`'s dispatch).

The bootstrapping tests (#raw("test/nbe_design.test.ts") §"Raw-predicate
bootstrapping") validate that the raw/tagged boundary is transparent
to `napply`. The step from `napply(T, v)` to `apply(T, v)` is an
encoding question, not a design question.
