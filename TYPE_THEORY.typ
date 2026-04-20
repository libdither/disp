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
  Every construct here must have a tree-calculus encoding.
]
#v(1em)

= Orientation

This document explains *what* the type system does and *why* that design
is sound, independent of any particular implementation. It is the
object-language specification: every predicate and invariant here must
have a declared tree-calculus encoding, per
#raw("DEVELOPMENT_PHILOSOPHY.md"). The host-side parser and driver are
not part of the type system — they are I/O layers.

Reading order: §2 gives the core premise (types as predicates), §3
explains why dependent types force the use of *fresh hypotheses*, §4
describes the evaluator interface every backend must satisfy, §5
defines the core type constructors against that interface, §6 walks
through typecheck examples step-by-step, and §7 assembles the soundness
argument from the pieces.

= The core premise: types as predicates

A *type* is a tree-calculus program `T : Tree → Tree` that returns the
encoded boolean `TT` if its argument inhabits the type and `FF` otherwise.

Type checking is therefore a single operation: *application*.

```
  v : T        ≡        T(v) reduces to TT
```

There is no separate type table, no annotated AST carried through
compilation, no side-data in the kernel. The elaborator produces two
trees — a value tree and a type tree — and the kernel asks exactly one
question: does the type tree, applied to the value tree, reduce to `TT`?

*Example — the natural numbers.* Pick an encoding: `Zero := t` (leaf),
`Succ n := fork(t, n)`. Then `Nat` is the predicate

```disp
let Nat : Type = fix ({self} -> {n} ->
  or (fast_eq n Zero)
     (and (is_fork n)
          (and (fast_eq (first n) t)
               (self (second n)))))
```

`Nat Zero` reduces by the left disjunct to `TT`. `Nat (Succ Zero)` takes
the right disjunct, recurses on `Zero`, returns `TT`. A malformed input
like `Nat (fork (Succ Zero) Zero)` fails the `first n = t` check (the
first child is a fork, not the leaf) and returns `FF`. The predicate
*is* the semantics; there is nothing else to consult.

*Consequence.* Anything that can be a well-defined tree-calculus
predicate can be a type: user-defined data types, Pi types, record
types, equality types, even the universe `Type` itself. The rest of
this document is about (a) how to write predicates for the interesting
constructors and (b) why those predicates mean what we want them to
mean.

= The dependency problem and fresh hypotheses

== What goes wrong for non-dependent function types

Suppose we want to check `f : Nat → Nat`. Naively, a first attempt:

```disp
let FunNatNat = {f} ->
  and (Nat (f Zero))
  (and (Nat (f (Succ Zero)))
       // ... and so on, forever
  )
```

This already fails: there are infinitely many `Nat`s, and we can't
enumerate them. Even if we could, the above only checks specific
inputs — the function might behave correctly on the ones we sampled and
wrong on others. We need a check that is *universal* in the input.

The classical move in type theory: check the function against one
*arbitrary* input and demand the output be of the right type. If the
check is set up so the function *cannot learn anything* about the input
except its type, then universal-over-all-of-`A` and holds-for-this-one
are equivalent.

== The fresh-hypothesis primitive

Introduce a symbol `a` that stands for "some arbitrary inhabitant of
`A`, about which we commit to nothing." Apply `f` to `a`. Check that
the result inhabits the codomain.

For this argument to *mean* universal quantification, `a` must satisfy
two properties:

+ *Opacity.* Nothing in `f` can pattern-match `a` against a specific
  `A`-inhabitant. If it could, `f` might return different values for
  different concrete `a`, and our single check would miss the
  divergence.
+ *Distinctness.* Two fresh hypotheses introduced under different
  binders — `{x : A} → {y : A} → x` vs. `... → y` — must be
  distinguishable from each other. Otherwise both functions check
  identically and the type system conflates them.

Two distinct entities need names:

- `fresh_hyp A` — the *evaluator primitive* that mints a new hypothesis
  of type `A`. The evaluator tracks binder depth internally; no depth
  is passed as an argument. Called from the body of `Pi`, `Arrow`, and
  other type constructors that need to introduce a hypothetical
  inhabitant.
- `mkH A lvl` — the *object-language term* produced when a hypothesis
  is serialized to tree form (via `quote`, §4). Trees are closed and
  stateless, so the depth must appear explicitly as `lvl`. Any choice
  of `lvl` encoding is admissible so long as different depths yield
  different hash-cons identities (distinctness (2)) and the dedicated
  `KH` tag namespace is recognized by the evaluator's H-hypothesis
  rule (opacity (1), see §4).

The two are related by `quote (fresh_hyp A) = mkH A L` where `L` is
whatever the evaluator's notion of current depth is, rendered in
canonical tree form.

== Arrow and Pi, defined

With fresh hypotheses in hand, the non-dependent `A → B` check becomes:

```disp
let Arrow = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    B (f a) }
```

For the *dependent* version `{x : A} -> B x`, `B` is a family
parametrized by `x`. Same machinery, with `B` applied to the fresh `a`:

```disp
let Pi = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    (B a) (f a) }
```

This is the workhorse predicate of dependent type theory in
tree-calculus dress. To actually reduce it, we need an evaluator — the
next section specifies the minimal interface every backend provides.

= Evaluation — what every evaluator must provide

Tree-calculus reduction is confluent: closed terms reduce to unique
normal forms. Any evaluator that faithfully implements the calculus
therefore agrees with every other on every reduction. Multiple
implementations may coexist, differing only in the *data structure
they use to represent partially-reduced terms* — not in the terms
themselves, and not in their normal forms.

== What is a Value?

A *Value* is an evaluator's internal representation of a
(possibly-partially-reduced) term. Treat it as an opaque handle: the
primitives below consume and produce handles, and `quote` is the only
way to look inside, returning a canonical tree. Different evaluators
choose different handle representations — a tree, a closure, a
domain element — and callers never depend on the choice.

Three things a Value might represent at any moment:

- A *concrete result* — a tree like `TT`, `Zero`, or `fork TT FF`
  that has no further reductions to do.
- A *fresh hypothesis* — an opaque symbolic inhabitant minted under
  a binder, carrying a stored type. See the `KH` convention below.
- A *stuck term* — a partial application or a value awaiting more
  reduction, either because it's waiting on an argument (a function
  value) or because it's applied to a hypothesis whose structure
  isn't available (a neutral).

The primitives below compose on Values without the caller knowing
which of the three a given handle is. `apply` and `eq` inspect enough
to proceed; `pred` is the composition that answers type-check
questions; `quote` is the escape hatch when you need the tree.

== The canonical interface

Every evaluator commits to these five operations. Behavior is specified;
representation is free.

#rulebox("apply(f, v) → Value",
  [Head β-reduction: apply `f` to `v` and reduce just enough that the
   caller can see what kind of Value came out — a concrete result, a
   hypothesis, a function waiting for another argument, or a stuck
   application. Deeper reductions happen on demand as subsequent
   operations ask for them.

   *H-hypothesis rule.* When the *predicate side* `f` is a fresh
   hypothesis with stored type `A`, and the *candidate side* `v` is
   the same hypothesis, `apply` returns `TT`. This is the load-bearing
   step that makes Pi checks terminate on polymorphic identities. It
   is a rule about how `apply` treats hypothesis-shaped values on the
   predicate side, not a separate operation.])

#rulebox("fresh_hyp(A) → Value",
  [Produces a Value representing "an arbitrary inhabitant of `A` under
   the current binder." The evaluator tracks binder depth internally;
   no depth argument is passed. Must satisfy *opacity* (no operation
   other than `apply`'s H-hypothesis rule can reveal anything about
   the Value's stored type or structure) and *distinctness*
   (hypotheses minted under distinct binders are `eq`-unequal).])

#rulebox("eq(v1, v2) → Bool",
  [Structural equality on Values: `TT` iff `quote v1` and `quote v2`
   produce hash-cons-equal trees. For terms in normal form this is
   hash-cons identity of the normal-form trees; for Values containing
   fresh hypotheses it distinguishes hypotheses minted under distinct
   binders.])

#rulebox("pred(T, v) → Bool",
  [`eq(apply(T, v), TT)`. The single operation every type check
   reduces to.])

#rulebox("quote(v) → Tree",
  [Convert a Value back to its canonical tree form. Applied to a
   result of `fresh_hyp A`, produces `mkH A L` where `L` is the
   evaluator's current depth rendered in the canonical level-tree
   encoding. Used at evaluator boundaries: serializing to another
   compilation unit, comparing Values produced by different evaluators,
   or handing a term to a surrounding host driver.])

== Example Value representations

The canonical form of every Value is a closed tree in the `mkH`
convention. An evaluator either works with that form directly or
de-canonicalizes it internally for efficiency, re-canonicalizing at
`quote`. A few representative choices:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Representation*][*Consequences*],
  [Canonical tree],
    [Values are already the canonical tree. `apply` is tree
     application; `eq` is hash-cons identity; `quote` is the identity
     function. Binder depth lives as an explicit `lvl`-tree argument
     threaded through the descent.],
  [Closure + environment],
    [A Value is a pair of a term and an environment mapping symbolic
     outer-binder references to Values. `apply` extends the
     environment; `eq` β-normalizes both sides and compares; `quote`
     walks the closure, re-emitting `mkH A L` at each free slot.
     Binder depth is a counter in the evaluator state.],
  [Algebraic value domain],
    [A Value is a tagged variant — `VFun`, `VNeu`, `VData`, … — with
     a dedicated neutral constructor for hypotheses carrying a depth
     index. `apply` dispatches on the variant; `eq` quotes to a normal
     form and compares there; `quote` is an explicit readback.],
)

Each representation is an *optimization*. Semantically they are the
same evaluator; confluence guarantees identical observable behavior.
If two disagree on a test, exactly one of them has a bug — never a
design difference.

== Fresh-hypothesis discipline is the load-bearing part

Every evaluator stands or falls on its `fresh_hyp` and its H-hypothesis
rule in `apply`. If opacity or distinctness fails, the soundness story
collapses. The canonical tree form `tagged(KH, fork(A, lvl))` is the
object-language definition; every evaluator must faithfully implement
its behavior. Any evaluator that re-uses the same hypothesis identity
under two nested binders, or that lets a `KH`-tagged value be
structurally inspected by ordinary tree operations, silently breaks
Pi-checking.

The `lvl` carried inside an `mkH` term must be chosen so that distinct
evaluator depths produce distinct hash-cons identities. One workable
convention: a fork-shaped marker tree built by `lvl_start = t t t` and
`lvl_next = {l} -> t l t`, with hand-constructed hypotheses (used in
test harnesses or primitive definitions) drawn from a disjoint
leaf-rooted namespace. The specific shapes are one choice among many;
what matters is that the shapes cannot collide.

= Core type constructors

Each constructor is defined as a tree-calculus predicate, written in
disp surface syntax. The actual in-kernel encoding is the
bracket-abstracted form; helpers like `or`, `and`, `fast_eq`, `fix`,
`is_fork`, `first`, `second`, `fresh_hyp` live in the prelude.

== The universe `Type`

In the first-cut design, `Type` is a single predicate that accepts any
tree-calculus term which, when applied to any argument within the
evaluation budget, reduces to `TT` or `FF`. Structurally this means:
applied to a probe value, `Type` runs its argument on a fresh
hypothesis and checks that the result is a boolean.

```disp
let Type : Type = {t} ->
  { let probe = fresh_hyp Top       // Top = accepts-anything predicate
    is_bool (t probe) }
```

where `is_bool b = or (fast_eq b TT) (fast_eq b FF)`. Self-typing
(`Type : Type`) is logically inconsistent but sufficient for a
programming language. A stratified hierarchy
`Type 0, Type 1, …` is a future extension: `Type n` is a fix-pointed
predicate that accepts base types, Pi types over rank-`n` components,
and `Type k` iff `k < n`. Universe polymorphism would carry an
implicit rank metavariable.

== `Pi` — dependent function type

```disp
let Pi : {A : Type} -> (A -> Type) -> Type
       = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    (B a) (f a) }
```

`{x : A} -> B` in the surface syntax elaborates to `Pi A ({x} -> B)`.
Non-dependent `A -> B` is `Pi A ({_} -> B)`.

Note that `Pi` is *itself* a type (inhabits `Type`), because for any
concrete `A, B`, `Pi A B` is a well-formed predicate.

== Record types — dependent products

For a `RecType` `{x1 : X1, ..., xn : Xn}`, the predicate is the Church
product:

For the two-field non-dependent case:

```disp
// RecType for { x : A, y : B }
let Pair = {A} -> {B} -> {r} ->
  r ({v1} -> {v2} -> and (A v1) (B v2))
```

For the dependent case `{ x : A, y : B x }`, the consumer threads the
first field into the second check using the same fresh-hypothesis
mechanism as `Pi`:

```disp
let DepPair = {A} -> {B} -> {r} ->
  r ({v1} -> {v2} -> and (A v1) ((B v1) v2))
```

The general `n`-field RecType `{x1 : X1, ..., xn : Xn}` extends this to
a Church product where each field may reference earlier ones.

The empty RecType `{}` is the unit predicate (always `TT`), inhabited
by the Church unit.

== `Eq` — propositional equality

```disp
let Eq : {A : Type} -> A -> A -> Type
       = {A} -> {a} -> {b} -> fast_eq a b
```

Hash-cons identity as a type. `Eq`'s inhabitants are *irrelevant*: the
type's TT/FF is determined entirely by `a` and `b`, ignoring the
candidate witness. `refl : {a : A} -> Eq a a` can be the polymorphic
unit — the type accepts any value when `a` and `b` reduce to the same
tree.

*Caveat.* Definitional equality here is narrow — only hash-cons identity
of normal forms. Two terms that reduce to the same tree satisfy `Eq`;
two terms that are "extensionally equal" but structurally different do
not. Stronger equalities (e.g., function extensionality) would require
additional predicates not given by the kernel.

== Data types

User data types are ordinary predicate lambdas written in surface
syntax. Examples:

```disp
let Nat : Type = fix ({self} -> {n} ->
  or (fast_eq n Zero)
     (and (is_fork n)
          (and (fast_eq (first n) t)
               (self (second n)))))

let Bool : Type = {b} -> or (fast_eq b TT) (fast_eq b FF)

let List : {A : Type} -> Type
        = {A} -> fix ({self} -> {xs} ->
  or (fast_eq xs Nil)
     (and (is_cons xs)
          (and (A (head xs))
               (self (tail xs)))))
```

No separate "data declaration" machinery is required: a data type is
just a predicate the user writes. Well-foundedness of `fix` is the
user's burden; the kernel enforces an evaluation budget to turn
divergence into a compile error rather than a hang.

= Worked examples

== Example 1: `3 : Nat`

Assume the encoding `3 = Succ (Succ (Succ Zero))`. Apply `Nat` to `3`:

```
Nat 3
= or (fast_eq 3 Zero) (and (is_fork 3) (and (fast_eq (first 3) t) (Nat (second 3))))
= or FF                (and TT          (and TT                   (Nat 2)))
= Nat 2
= Nat 1
= Nat Zero
= or (fast_eq Zero Zero) ...
= TT
```

The whole check is one application, reducing structurally through the
predicate. No type-checker state, no AST annotations.

== Example 2: `{x : Nat} -> x : Nat -> Nat`

Surface: `(({x : Nat} -> x) : Nat -> Nat)`. Elaborator produces:

- Value: `{x} -> x` (tagged KL, param type `Nat`).
- Type: `Pi Nat ({_} -> Nat)`, which expands per §5 to a lambda of the
  form `{f} -> { let a = fresh_hyp Nat; (({_} -> Nat) a) (f a) }`.

Check: apply the type to the value.

```
Pi Nat ({_} -> Nat) ({x} -> x)
= { let a = fresh_hyp Nat
    (({_} -> Nat) a) (({x} -> x) a) }
= Nat a                          // after β on codomain and body
```

Now `a = fresh_hyp Nat`. Does `Nat a` reduce to `TT`?

This is the interesting case. `a` is a fresh hypothesis — its
canonical tree form is `mkH Nat L` for the current depth `L`, and it
lives in the `KH` tag namespace. A naive `Nat` predicate (as written
above) would reduce `fast_eq a Zero` to `FF`, `is_fork a` to `FF`, and
return `FF` — rejecting the program.

The H-hypothesis rule (§4, the special case of `apply`) handles this:
when the predicate side is a hypothesis and the candidate side is the
same hypothesis — or, more generally, when applying the predicate
reduces to comparing the hypothesis's stored type `Nat` against itself
— `apply` returns `TT` directly. Operationally:

```
Nat a     where a = mkH Nat L     ⟶   TT    (by the H-hypothesis rule)
```

This built-in rule is what makes fresh hypotheses actually *opaque* —
`Nat` doesn't need to crack open `a`; the evaluator's treatment of
`KH`-tagged values under predicate application does the work.

The check therefore succeeds. Good.

== Example 3: polymorphic identity

Surface: `let id : {A : Type} -> A -> A = {A} -> {x} -> x`.

The type elaborates to `Pi Type ({A} -> Pi A ({_} -> A))`. Apply to the
value `{A} -> {x} -> x`:

```
Step 1 — outer Pi (evaluator enters a new binder, depth L1):
  let α = fresh_hyp Type
  (({A} -> Pi A ({_} -> A)) α) (({A} -> {x} -> x) α)
= Pi α ({_} -> α) ({x} -> x)

Step 2 — inner Pi (evaluator enters another binder, depth L2):
  let a = fresh_hyp α
  (({_} -> α) a) (({x} -> x) a)
= α a

Step 3 — H-hypothesis rule: α is the hypothesis from Step 1, `a` is a
fresh α-typed hypothesis; applying α to `a` matches the stored type,
⇒ TT.
```

Three reduction steps, and the whole polymorphic-identity specification
is verified. The structure is identical for any Pi type; the `KH` rule
does the heavy lifting.

== Example 4: a program that should be rejected

Consider `let bad : {A : Type} -> A -> A = {A} -> {x} -> (x x)`.
Elaboration produces a well-formed lambda tree; the binder count
matches the Pi's arity, so elab doesn't complain. The check is what
catches the error:

```
Step 1 — outer Pi: let α = fresh_hyp Type; reduces to Pi α ({_} -> α) ({x} -> x x)
Step 2 — inner Pi: let a = fresh_hyp α; reduces to α (a a)
Step 3 — H-hypothesis rule:
  `a a` is a fork, not an H-tagged value with stored type α.
  The KH rule does not fire; predicate α proceeds to its fallback
  (FF for anything that isn't the specific stored hypothesis).
= FF
```

Rejected as expected. The check fails because `a a` isn't the stored
hypothesis — the only way to produce something `α` accepts is to
return `a` itself, i.e., to be the identity.

= Soundness

Soundness in this setting: *if compilation produces a tree `top_value`
and a tree `top_type` and the kernel reports `top_type(top_value) = TT`,
then `top_value` inhabits the intended semantic type denoted by
`top_type`.*

Two halves:

- *Type = predicate semantics.* We *define* `v : T` to mean `T(v) = TT`.
  At this definitional level soundness is trivial: the thing we checked
  is by definition what it means to inhabit the type.
- *The predicates mean what we intend.* This is the non-trivial part.
  We want `Pi` to mean "all functions from A to B[x:=a]" — not some
  weaker approximation — and `Eq` to mean hash-cons identity of normal
  forms, etc.

The second half is what must be argued. It rests on three invariants.

== Invariant I — Freshness

`fresh_hyp A` must produce a value that

+ is *opaque*: the only operation that reveals its stored type is
  `apply`'s H-hypothesis rule;
+ is *distinct*: two hypotheses minted under distinct binders (at
  distinct evaluator depths) are `eq`-unequal.

(1) is enforced by *convention*, not by a kernel-level guarantee: the
elaborator does not emit tree-destructuring operations (`first`,
`second`, `is_fork`, …) that target `KH`-tagged values. In the raw
tree calculus, a `KH` tree is data like any other and *could* be
destructured by a hostile predicate; the soundness story depends on
the elaborator never generating such code. Any hand-written kernel
primitive that manipulates `KH` must preserve this opacity.

(1) then ensures a function `f` checked against `Pi A B` cannot behave
differently on concrete `A`-inhabitants than on the fresh hypothesis —
it has no way to tell them apart. A single check against the fresh
hypothesis is therefore equivalent to universal quantification. This
is *parametricity*, enforced by the combination of evaluator opacity
and elaborator discipline.

(2) ensures type constructors with multiple binders do not conflate
their parameters. Without distinctness,
`{x : A} → {y : A} → x` and `{x : A} → {y : A} → y` would accept
the same set of functions — the type system would become unable to
distinguish projections.

Where this invariant is at risk: any evaluator whose internal
representation collapses distinct binder depths; any kernel primitive
that lets `KH` values escape their dedicated namespace. Any `lvl`-tree
convention must keep elaborator-generated and hand-constructed
hypotheses in disjoint families — if their tree representations
collided, so would their hash-cons identities.

== Invariant II — Structural equality

`eq` must coincide with tree-calculus hash-cons identity on closed
normal forms. This is the unspoofable ground truth the whole edifice
leans on. `fast_eq` gets this for free from the tree-calculus runtime;
any backend that quotes to a tree first and compares there inherits it.

Failure mode: a backend optimization that collapses two observably
different values (e.g., treating all closures with the same body as
equal regardless of env). That would make the type system accept terms
it shouldn't.

== Invariant III — Predicate faithfulness

Every type constructor's predicate must, when applied to a value, yield
`TT` iff the value semantically inhabits the type.

By construction:

- `Pi A B` (as defined in §5) is `TT` iff, for the fresh `a : A`,
  `B(a)` accepts `f(a)`. By Invariant I this is equivalent to "for all
  `a : A`, `B(a)` accepts `f(a)`." That is the semantic definition of
  the Pi type.
- `RecType` is a Church-encoded conjunction of field checks; it is
  `TT` iff the value is the Church pair of values each inhabiting the
  corresponding field type.
- `Eq a b` is `TT` iff `fast_eq a b` — hash-cons identity by
  definition.
- `Type` is `TT` iff its argument reduces to a boolean when applied
  to a fresh hypothesis under the evaluation budget.
- User-defined data types are `TT` iff the user's predicate body says
  so. Soundness of *user* types is the user's burden — if they write a
  predicate that accepts non-nats as Nats, the system obliges.

The induction: every type constructor combines sub-predicates via
predicate-level operations (conjunction, fresh-hypothesis introduction)
that preserve faithfulness. If the leaves (primitive predicates and
user-written data-type predicates) are faithful, the whole tree is.

== What breaks if any invariant slips

Illustrative table of how things go wrong:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Broken invariant*][*Resulting unsoundness*],
  [Opacity of `fresh_hyp`],
    [A function `f` inside a Pi can inspect the hypothesis and return
     different values on real `A`-inhabitants, passing the check for
     types it doesn't actually inhabit.],
  [Distinctness of depths],
    [`{x : A} → {y : A} → x` and `... → y` become indistinguishable;
     the type system cannot reject a swapped-projection term.],
  [`eq` is not hash-cons identity],
    [Either `Eq a b` accepts non-identical terms (accepts bad
     equalities) or rejects identical ones (rejects valid programs).
     Either way, the equality type becomes a liability.],
  [Predicate faithfulness — e.g., a broken `Pi` encoding],
    [A function passes Pi-checking without actually respecting its
     intended type; downstream uses rely on type info that doesn't
     hold.],
)

Evaluation-budget exhaustion is *not* on this list. When the kernel
runs out of budget mid-check it raises a compile error — a completeness
concession, not an unsoundness. The check that *would* have succeeded
is rejected because the kernel gave up. No wrong answer is ever
reported as `TT`.

== What soundness does *not* include

Deliberately out of scope at this stage:

- *Strong normalization.* The kernel relies on the evaluation budget
  as a finite approximation; a user can write a non-terminating
  predicate that the kernel flags as error, not as `FF`. A logically
  consistent system would need a termination argument for any
  user-writable predicate.
- *Logical consistency.* `Type : Type` collapses the universe
  hierarchy; Girard's paradox means there exists a closed term
  inhabiting `False` (if we define `False` as the empty predicate).
  Fine for a programming language, fatal for a proof assistant. A
  rank-stratified `Type n` hierarchy recovers consistency; deferred.
- *Function extensionality.* Two functions that agree on all inputs
  but are structurally different do not satisfy `Eq`. Extensional
  equality, if needed, is a separate predicate.

Each of these is a choice, not a bug. The soundness story above holds
on its own terms even under these concessions; we just don't claim
more than it delivers.

= Open design questions

- *Universe stratification.* Replace the single `Type : Type`
  predicate with a rank-indexed family `Type n` such that `Type m`
  accepts `Type k` iff `k < m`, recovering logical consistency.
  Universe polymorphism would carry an implicit rank metavariable at
  each use site.
- *Extensional equality.* `Eq` is hash-cons identity only. If a use
  case demands function extensionality (two functions equal when they
  agree on all inputs), that is a separate predicate — possibly a
  companion to `Eq` with restricted introduction rules. Deferred
  until a concrete need.
- *Cross-evaluator agreement.* When multiple evaluators coexist, their
  `fresh_hyp`, `eq`, and `quote` must agree on every closed term. A
  shared test suite that exercises Pi-checking, record projection,
  and the H-hypothesis rule under each evaluator is the practical
  cross-check for Invariants I and II.
- *Canonical `lvl` encoding.* The document specifies only that `lvl`
  trees must be distinct per depth and in a disjoint namespace from
  hand-rolled hypotheses. Pinning down *one* encoding simplifies the
  `quote` contract across evaluators; choosing among the options
  (fork-shaped markers, nat-shaped markers, pair-with-parity, …) is
  an open call.

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`t` / `△`], [The tree-calculus leaf. Source syntax: `t`; math
                notation: `△`.],
  [`TT` / `FF`], [The encoded booleans. `TT` is truth; `FF` is falsity.
                  Concrete tree encodings are implementation-defined but
                  must be hash-cons distinct.],
  [`fork(a, b)`], [A tree with two children. Written `fork a b` when
                   used as a value constructor.],
  [`fast_eq a b`], [Hash-cons identity: returns `TT` iff `a` and `b`
                    have the same tree-calculus structure. O(1).],
  [`or`, `and`], [Short-circuiting boolean operators on `TT`/`FF`.
                  Prelude-defined.],
  [`fix`], [Fix-point combinator: `fix f = f (fix f)`. Used for
            recursive predicates. Prelude-defined.],
  [`mkH A lvl`], [Object-language term for a free hypothesis of type
                  `A` at depth `lvl`. Lives in the `KH` tag namespace.
                  See §4 for the `lvl`-tree discipline.],
  [`KH`, `KL`, `KV`, `KA`, …],
    [Tagged-form constructors of elaborated terms: `KH` hypothesis,
     `KL` lambda, `KV` variable reference, `KA` application, etc.
     The elaborator synthesizes these; the evaluator dispatches on
     them during reduction.],
  [`apply(f, v)`], [Evaluator primitive: head β-reduction. Includes
                    the H-hypothesis rule.],
  [`fresh_hyp A`], [Evaluator primitive: mint a new hypothesis of
                    type `A` at the current binder depth.],
  [`eq(v1, v2)`], [Evaluator primitive: structural equality on values,
                   coincident with `fast_eq` on their `quote`d trees.],
  [`pred(T, v)`], [`eq(apply(T, v), TT)` — the single operation every
                   type check reduces to.],
  [`quote(v)`], [Evaluator primitive: convert an internal value to
                 its canonical tree form.],
  [*opacity*], [Property of fresh hypotheses: elaborator-emitted code
                never destructures their tree form; only the
                H-hypothesis rule in `apply` inspects them.],
  [*distinctness*], [Property of fresh hypotheses: those minted under
                     distinct binder depths are `eq`-unequal.],
  [*hash-consing*], [The tree-calculus runtime's invariant: structurally
                     equal trees share the same object identity, so
                     `fast_eq` is pointer-equality.],
  [*parametricity*], [The consequence of opacity: a function checked
                      against a Pi can't distinguish concrete
                      `A`-inhabitants from the fresh hypothesis, so
                      one check is equivalent to universal
                      quantification.],
)
