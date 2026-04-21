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
not part of the type system --- they are I/O layers.

Reading order: §2 gives the core premise (types as predicates), §3
explains why dependent types force the use of *fresh hypotheses*, §4
describes the evaluator interface every backend must satisfy, §5
defines the core type constructors against that interface, §6 walks
through typecheck examples step-by-step, and §7 assembles the soundness
argument from the pieces.

= The core premise: types as predicates

A *type* is a Value `T` that behaves as a predicate `Value → Bool`: it
returns the encoded boolean `TT` if its argument inhabits the type and
`FF` otherwise.

Type checking is therefore a single operation: *application*.

```
  v : T        ≡        apply(T, v)  reduces to  TT
```

There is no separate type table, no annotated AST carried through
compilation, no side-data in the kernel. The elaborator produces two
Values --- a value Value and a type Value --- and the checker asks exactly
one question: does the type, applied to the value, reduce to `TT`?

*Example --- the natural numbers.* Pick an encoding: `Zero := t` (leaf),
`Succ n := fork(t, n)`. Then `Nat` is the predicate

```disp
let Nat = fix ({self} -> {n} ->
  or (fast_eq n Zero)
     (and (is_fork n)
          (and (fast_eq (first n) t)
               (self (second n)))))
```

`Nat Zero` reduces by the left disjunct to `TT`. `Nat (Succ Zero)` takes
the right disjunct, recurses on `Zero`, returns `TT`. A malformed input
like `Nat (fork (Succ Zero) Zero)` fails the `first n = t` check and
returns `FF`. The predicate *is* the semantics; there is nothing else
to consult.

*Consequence.* Anything that can be a well-defined tree-calculus
predicate can be a type: user-defined data types, `Pi` types, record
types, equality types, and the universe family `Type n` itself. The
rest of this document is about (a) how to write predicates for the
interesting constructors and (b) why those predicates mean what we
want them to mean.

= The dependency problem and fresh hypotheses

== What goes wrong for non-dependent function types

Suppose we want to check `f : Nat → Nat`. A naive first attempt:

```disp
let FunNatNat = {f} ->
  and (Nat (f Zero))
  (and (Nat (f (Succ Zero)))
       // ... and so on, forever
  )
```

This fails: there are infinitely many `Nat`s, and sampling specific
inputs misses misbehavior on unsampled ones. We need a check that is
*universal* in the input.

The classical move: check the function against one *arbitrary* input
and demand the output be of the right type. If the check is set up so
the function *cannot learn anything* about the input except its type,
then universal-over-all-of-`A` and holds-for-this-one are equivalent.

== The `fresh_hyp` primitive

Introduce a Value `a` that stands for "some arbitrary inhabitant of
`A`, about which we commit to nothing." Apply `f` to `a`. Check that
the result inhabits the codomain.

For this argument to *mean* universal quantification, `a` must satisfy
two properties:

+ *Opacity.* Nothing in `f` can pattern-match `a` against a specific
  `A`-inhabitant. If it could, `f` might return different values for
  different concrete `a`s, and our single check would miss the
  divergence.
+ *Distinctness.* Two fresh hypotheses introduced under different
  binders --- `{x : A} -> {y : A} -> x` vs. `... -> y` --- must be
  distinguishable from each other. Otherwise both functions check
  identically and the type system conflates them.

`fresh_hyp A` is the evaluator primitive that mints such a Value. It
takes one argument (the stored type) and produces a fresh hypothesis
Value. The evaluator tracks binder depth internally; the caller does
not supply an identity. Distinct invocations under distinct binders
yield `conv`-distinct Values.

== Arrow and Pi, defined

With `fresh_hyp` in hand, the non-dependent `A → B` check becomes:

```disp
let Arrow = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    B (f a) }
```

For the *dependent* version `{x : A} -> B x`, `B` is a family
parametrized by `x`. Same machinery, with `B` applied to the fresh
`a`:

```disp
let Pi = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    (B a) (f a) }
```

The `let`-sugar relies on eager β (call-by-value): `fresh_hyp A` is
evaluated once, and `a` refers to the same Value in both uses. An
evaluator that lazily re-invoked `fresh_hyp` at each occurrence would
produce two distinct hypotheses and silently break the Pi check. §4
commits to eager β.

This is the workhorse predicate of dependent type theory in
tree-calculus dress. To actually reduce it, we need an evaluator ---
§4 specifies the minimal interface every backend provides.

= Evaluation --- what every evaluator must provide

Tree-calculus reduction is confluent: closed terms reduce to unique
normal forms. Any evaluator that faithfully implements the calculus
therefore agrees with every other on every reduction. Multiple
implementations may coexist, differing only in the *data structure
they use to represent partially-reduced terms* --- not in the terms
themselves, and not in their observable behavior.

== What is a Value?

A *Value* is a tree: the evaluator's representation of a term, possibly
carrying binder-descent context alongside. Callers treat it as an
opaque handle: the primitives below consume and produce Values, and the
reflection primitives are the only way to inspect their shape.

Canonical shape: a Value is a pair `(term, ctx)` where `term` is a
tagged tree and `ctx` is either a closed marker or a parallel
context-tree carrying binder info. Closed Values (most types,
fully-reduced data) have a trivial ctx component; Values introduced
during binder descent carry a non-trivial ctx.

Backends may internally represent Values differently (e.g., as
host-level closures with environments); the observable semantics must
agree with the canonical `(term, ctx)` form. If two backends disagree
on any observable operation, exactly one of them has a bug.

Three things a Value might represent at any moment:

- A *concrete result* --- a tree like `TT`, `Zero`, or a fully-reduced
  structure.
- A *fresh hypothesis* --- an opaque symbolic inhabitant minted under
  a binder, carrying a stored type.
- A *stuck term* --- a partial application or a value awaiting more
  reduction, typically because it's applied to a hypothesis whose
  structure isn't available.

== Primitive interface, two tiers

The evaluator exposes two tiers of primitives. The *Val-level* tier is
what disp predicates (Pi, Type, user-defined types) call. The
*tree-level* tier is backend-internal machinery that backends use to
implement the Val-level primitives; disp predicates SHOULD avoid it
(guideline, not enforced).

=== Val-level primitives

#rulebox("apply(f, v) → Value",
  [Head β-reduction with eager evaluation: apply `f` to `v`, reducing
   to normal form. Returns a new Value. Reduction is deterministic;
   two `apply` calls with the same inputs produce identical Values.

   *H-hypothesis rule.* When `v` is a hypothesis with stored type `T`
   and `conv(f, T) = TT`, `apply(f, v)` returns `TT` directly, without
   reducing `f`'s body. This is the load-bearing rule for Pi-checking
   under binders: it says a function that receives "some arbitrary
   inhabitant of `T`" satisfies the predicate `T` on that inhabitant by
   construction.])

#rulebox("conv(v1, v2) → Bool",
  [Semantic equality on Values: `TT` iff `v1` and `v2` denote the same
   term up to reduction (definitional equality). Implementations may
   specialize: in the canonical `(term, ctx)` form, `conv` coincides
   with `fast_eq` on both components after normalization. Other
   representations may need structural recursion.

   `conv` is the equality every disp-level predicate uses. Never
   `fast_eq` directly (which is backend-internal, tree-level).])

#rulebox("fresh_hyp(A) → Value",
  [Mint a Value representing "an arbitrary inhabitant of `A` under the
   current binder." One argument (the stored type). The evaluator
   tracks binder depth internally. Must satisfy *opacity* (no operation
   other than the H-hypothesis rule reveals the stored type) and
   *distinctness* (`conv`-distinct under distinct binders).])

#rulebox("is_H(v) → Bool",
  [`TT` iff `v` is a fresh hypothesis. Partial reflection on
   introduction mode.])

#rulebox("type_of_H(v) → Value",
  [Partial: requires `is_H(v) = TT`. Returns the stored type a
   hypothesis was introduced with.])

#rulebox("identity_of_H(v) → Value",
  [Partial: requires `is_H(v) = TT`. Returns the evaluator's chosen
   identity marker (canonically an encoded Nat --- binder depth or
   fresh counter). Distinct per `fresh_hyp` invocation.])

#rulebox("is_pi(v) → Bool",
  [`TT` iff `v` is a `Pi` type constructor applied to domain and
   codomain (as defined in §5). Each backend implements this against
   its `Pi` representation.])

#rulebox("pi_dom(v), pi_cod(v) → Value",
  [Partial: require `is_pi(v) = TT`. Extract the domain and codomain
   of a Pi type.])

#rulebox("is_universe(v) → Bool",
  [`TT` iff `v` is `Type k` for some `k` (as defined in §5.1). Each
   backend implements against its universe representation.])

#rulebox("universe_rank(v) → Value",
  [Partial: requires `is_universe(v) = TT`. Returns `k` (an encoded
   Nat).])

#rulebox("is_registered_base_type(v) → Bool",
  [`TT` iff `v` is a member of the current universe predicate's
   closed-over base-type registry. Each `Type n` closes over a specific
   registry at its definition site (§5.1); this primitive consults
   the registry of the enclosing `Type`.])

Plus scalar and boolean helpers (`encode_nat`, `decode_nat`, `lt`,
`le`, `succ`, `zero`, `TT`, `FF`, `and`, `or`, `not`), encoded in the
usual way.

=== Tree-level primitives

These operate on raw trees and are used by backends to implement
Val-level reflection. Disp predicates SHOULD avoid calling them
directly: doing so couples the predicate to the backend's internal
representation.

- `triage`, `is_leaf`, `is_stem`, `is_fork` --- tree shape.
- `first`, `second` --- fork decomposition.
- `kind`, `payload` --- tagged-form accessors (backend-specific
  conventions).
- `fast_eq` --- O(1) tree hash-cons identity. Backend-internal;
  `conv` is what disp predicates use.

== Eager β and hash-cons canonicality

Every `apply` call reduces eagerly. `let x = e in body` desugars to
`(λx. body) e`; the argument is evaluated once and shared. This is
relied on by predicate bodies like `Pi` that use `let a = fresh_hyp A`.

Canonical construction is preserved across β-reduction: two evaluations
that should produce the same term produce `conv`-equal Values, and
ideally identity-equal trees under hash-consing. Backends that can
guarantee the stronger property (ctxtree-style) get `conv = fast_eq`
as a performance win; backends that cannot fall back to structural
recursive comparison.

== Quote and erase

Two boundary operations, not generally used by disp predicates:

- `quote(v) → Tree` --- convert a Value to its canonical closed-tree
  form. Useful for serialization, debugging, and cross-backend
  comparison. Canonical for a given Value; the round-trip
  `apply(quote(v), _)` agrees with `apply(v, _)`.
- `erase(v) → SKI_Tree` --- strip type-checking machinery (tags,
  bindtrees, hypothesis markers) to produce a pure tree-calculus term
  suitable for runtime execution. Specified in #raw("COMPILATION.typ"),
  not here.

= Core type constructors

Each constructor is defined as a tree-calculus predicate in disp
surface syntax. Every binding here is *prelude-canonical*: the
kernel's structural reflection primitives (`is_pi`, `is_universe`,
etc.) recognize these specific definitions. Users can define their
own analogous predicates, but such user-defined type-formers are not
structurally recognized; they participate in the universe hierarchy
only via the registry (see §5.1).

== The universe family `Type n`

`Type` is a rank-indexed family of predicates. `Type n` accepts all
rank-$≤n$ types; `Type k` (as a value) inhabits `Type (k+1)`. This
stratification recovers logical consistency --- Girard's paradox, which
afflicted the earlier `Type : Type` design, does not apply.

```disp
let Type : Nat -> Predicate
  = fix ({self, n, t} ->
      or (and (is_universe t)
              (lt (universe_rank t) n))              // case 1: universe below n
     (or (and (is_pi t)
              (and (self n (pi_dom t))
                   (self n ((pi_cod t) (fresh_hyp (pi_dom t))))))  // case 2: Pi
     (or (is_registered_base_type t)                 // case 3: registered base
         (and (is_H t)                               // case 4: cumulative hypothesis
              (and (is_universe (type_of_H t))
                   (le (universe_rank (type_of_H t)) n))))))
```

Four admissible shapes:

+ *Universe below `n`.* `mk_universe k` with `k < n`. Case 1 uses the
  `is_universe`/`universe_rank` reflection primitives.
+ *Compound type constructor.* A `Pi` whose domain and codomain (under
  a fresh hypothesis) are themselves rank-$≤n$ types. Records
  elaborate to `Pi` chains (§5.3); `is_pi` captures them. Other
  type-formers are not structurally recognized.
+ *Registered base type.* `Nat`, `Bool`, `Unit`, and any user-declared
  `let X : Type 0 = body` entries. Each `Type n` closes over a specific
  registry at its definition site; users extend it by declaring new
  rank-0 types.
+ *Cumulative hypothesis.* A hypothesis `h` whose stored type is itself
  a universe `Type k` with `k ≤ n`. This is how a hypothetical
  `{A : Type 0}` participates as a valid rank-1 type: `Type 1 A` fires
  case 4 via `is_H A`, then `is_universe (type_of_H A) = is_universe
  (Type 0) = TT`, then `universe_rank (Type 0) = 0 ≤ 1 = TT`.

*Cumulativity* falls out: every case has a `<` or `≤` comparison that
is monotone in `n`. A value accepted by `Type k` is accepted by
`Type n` for every `n ≥ k`, without any separate subtyping rule.

*Registry extension.* Every `let X : Type 0 = body` declaration creates
a *new* `Type` predicate, closed over an extended registry
`{Nat, Bool, Unit, ..., X}`. Code written after the declaration
references the new `Type`; code written before is unaffected. This
makes each extension of the trusted base visible and explicit in the
program text --- a user cannot silently grow the rank-0 admission set.
The bootstrap registry is `{Nat, Bool, Unit}`.

*Trust discipline.* The kernel does not structurally validate user
declarations at rank 0; a malformed `let X : Type 0 = ...` corrupts
only that user's hierarchy. Soundness of the rank-0 level is the
user's burden, same as well-foundedness of any `fix`-defined
predicate.

== `Pi` --- dependent function type

```disp
let Pi : {A : Type 0} -> (A -> Type 0) -> Type 0     // rank-0 witness
       = {A} -> {B} -> {f} ->
  { let a = fresh_hyp A
    (B a) (f a) }
```

`{x : A} -> B` in the surface syntax elaborates to `Pi A ({x} -> B)`.
Non-dependent `A -> B` is `Pi A ({_} -> B)`. The rank of `Pi A B` is
the max of the ranks of `A` and `B(a)` for a fresh `a : A`; this falls
out of `Type`'s case 2 applied recursively.

`Pi` is *itself* a predicate inhabiting some `Type n` --- specifically,
the smallest `n` that covers both its domain and codomain. For
`Pi Nat Nat`, `n = 0`. For `Pi (Type 0) ({A} -> A -> A)`, `n = 1`.

== Record types --- Church products over `Pi`

A `RecType` `{x1 : X1, ..., xn : Xn}` elaborates to a Church product
--- a specific `Pi`-chain. For the two-field non-dependent case:

```disp
// RecType for { x : A, y : B }  elaborates to
let Pair = {A} -> {B} -> {r} ->
  r ({v1} -> {v2} -> and (A v1) (B v2))
```

For dependent fields `{ x : A, y : B x }`:

```disp
let DepPair = {A} -> {B} -> {r} ->
  r ({v1} -> {v2} -> and (A v1) ((B v1) v2))
```

Because records elaborate to `Pi` chains, they participate in `Type n`
via `is_pi`; no separate `is_rectype` primitive is required. The empty
RecType `{}` is the unit predicate (always `TT`).

== `Eq` --- propositional equality

```disp
let Eq : {A : Type 0} -> A -> A -> Type 0
       = {A} -> {a} -> {b} -> {_} -> conv a b
```

Semantic equality as a type: `Eq A a b` accepts any witness when
`conv(a, b) = TT`. The trailing `{_} ->` makes the expression a
predicate awaiting an (irrelevant) witness; `refl : {a : A} -> Eq a a`
can be the polymorphic unit.

*Note:* `Eq` uses `conv`, not `fast_eq`. Two terms that are
definitionally equal but structurally different (e.g., pre- and
post-β-reduction forms in some backends) satisfy `Eq`. Extensional
equality on functions --- pointwise agreement without structural
identity --- is strictly stronger and not provided by the kernel.

== Data types

User data types are ordinary predicate lambdas written in surface
syntax. Examples:

```disp
let Nat : Type 0 = fix ({self} -> {n} ->
  or (fast_eq n Zero)
     (and (is_fork n)
          (and (fast_eq (first n) t)
               (self (second n)))))

let Bool : Type 0 = {b} -> or (fast_eq b TT) (fast_eq b FF)

let List : {A : Type 0} -> Type 0
         = {A} -> fix ({self} -> {xs} ->
  or (fast_eq xs Nil)
     (and (is_cons xs)
          (and (A (head xs))
               (self (tail xs)))))
```

Data-type bodies legitimately use tree-level primitives (`is_fork`,
`first`, `fast_eq`) because they pattern-match on concrete encodings,
not on Val semantics. This is one of the guideline's acknowledged
exceptions: a predicate defining *what it means to be data* must
inspect data shape.

A `let Nat : Type 0 = ...` declaration extends the enclosing `Type`'s
registry (§5.1); subsequent predicates that use `Nat` will find it
via case 3 of `Type`.

Well-foundedness of `fix` is the user's burden; the kernel enforces an
evaluation budget to turn divergence into a compile error rather than
a hang.

= Worked examples

== Example 1: `3 : Nat`

With `3 = Succ (Succ (Succ Zero))`:

```
apply Nat 3
= or (fast_eq 3 Zero)
     (and (is_fork 3) (and (fast_eq (first 3) t) (Nat (second 3))))
= or FF (and TT (and TT (Nat 2)))
= Nat 2
= Nat 1
= Nat Zero
= TT
```

One application, reducing structurally through the predicate. No
type-checker state, no AST annotations.

== Example 2: `({x : Nat} -> x) : Nat -> Nat`

Value: `{x} -> x`. Type: `Pi Nat ({_} -> Nat)`, which reduces per
§5.2 to `{f} -> { let a = fresh_hyp Nat; (({_} -> Nat) a) (f a) }`.

Check:

```
apply (Pi Nat ({_} -> Nat)) ({x} -> x)
= { let a = fresh_hyp Nat
    (({_} -> Nat) a) (({x} -> x) a) }
= apply Nat a                    // after β on codomain and body
```

`a` is a hypothesis with stored type `Nat`. The H-hypothesis rule
fires: `conv(Nat, type_of_H(a)) = conv(Nat, Nat) = TT`, so
`apply(Nat, a) = TT` directly. The check succeeds.

== Example 3: polymorphic identity

`let id : {A : Type 0} -> A -> A = {A} -> {x} -> x`.

Type: `Pi (Type 0) ({A} -> Pi A ({_} -> A))`.

```
Step 1 — outer Pi, depth L1:
  let α = fresh_hyp (Type 0)
  (({A} -> Pi A ({_} -> A)) α) (({A} -> {x} -> x) α)
= Pi α ({_} -> α) ({x} -> x)

Step 2 — inner Pi, depth L2:
  let a = fresh_hyp α
  (({_} -> α) a) (({x} -> x) a)
= apply α a

Step 3 — H-hypothesis rule: type_of_H(a) = α; conv(α, α) = TT;
  apply α a = TT.
```

Three reduction steps, and polymorphic-identity is verified.

== Example 4: cumulativity under a binder

`{A : Type 0} -> A -> A` as a rank-1 type. Check: `apply (Type 1) (Pi (Type 0) ({A} -> Pi A ({_} -> A)))`.

```
Type 1 (Pi (Type 0) ({A} -> Pi A ({_} -> A)))
= case 2 (is_pi = TT):
  — domain: apply (Type 1) (Type 0)
    = case 1: is_universe (Type 0) = TT, rank = 0, 0 < 1 = TT ✓
  — codomain: let A = fresh_hyp (Type 0)
                apply (Type 1) (Pi A ({_} -> A))
    = case 2 (is_pi = TT):
      — domain: apply (Type 1) A
        = case 1 fails (A is a hypothesis, not mk_universe)
        = case 4: is_H A = TT, is_universe (Type 0) = TT,
                  universe_rank (Type 0) = 0, 0 ≤ 1 = TT ✓
      — codomain: symmetric, TT ✓
```

Case 4 is what makes cumulativity work: a fresh hypothesis whose
stored type is `Type 0` passes `Type n` for every `n ≥ 0`, without
a separate subtyping rule.

== Example 5: a program that should be rejected

`let bad : {A : Type 0} -> A -> A = {A} -> {x} -> (x x)`.
Elaboration is well-formed (binder arity matches); the check rejects:

```
Step 1 — outer Pi: α = fresh_hyp (Type 0); ⟶ Pi α ({_} -> α) ({x} -> x x)
Step 2 — inner Pi: a = fresh_hyp α; ⟶ apply α (a a)
Step 3 — H-hypothesis rule: `a a` is not a hypothesis — the H rule does
  not fire. α's body proceeds: α was introduced with stored type
  (Type 0), so its predicate semantics admit rank-0 types. (a a) is
  neither a universe, nor a Pi, nor a registered base, nor a
  hypothesis — all cases of α's expansion fail.
⟶ FF
```

Rejected. The only way `(x x)` passes is if `x` has a type that
accepts its own application to itself; the hypothesis `a : α` with α
abstract provides no such information.

= Soundness

Soundness in this setting: *if compilation produces a Value `top_value`
and a Value `top_type` and the kernel reports
`apply(top_type, top_value) = TT`, then `top_value` inhabits the
intended semantic type denoted by `top_type`.*

Two halves:

- *Type = predicate semantics.* We *define* `v : T` to mean
  `apply(T, v) = TT`. At the definitional level soundness is trivial.
- *Predicates mean what we intend.* `Pi` should mean "all functions
  from A to B[x:=a]," `Eq` should mean definitional equality of normal
  forms, `Type n` should mean "types at rank $≤ n$."

The second half is what must be argued. It rests on four invariants.

== Invariant I --- Freshness

`fresh_hyp A` produces a Value that

+ is *opaque*: the only operation that inspects its stored type is
  the H-hypothesis rule in `apply`;
+ is *distinct*: hypotheses minted under distinct binders are
  `conv`-unequal.

Opacity is enforced by *convention*: the elaborator does not generate
tree-destructuring operations targeting hypothesis Values. User
predicates *may* inspect them (the two-tier guideline is not
enforced), but doing so is the user's responsibility.

Opacity ensures parametricity: a function `f` checked against `Pi A B`
has no way to distinguish a concrete `A`-inhabitant from the fresh
hypothesis, so a single check is equivalent to universal quantification.

Distinctness ensures type constructors with multiple binders do not
conflate their parameters. Without it, `{x : A} -> {y : A} -> x` and
`... -> y` would accept the same functions --- the type system could
not distinguish projections.

== Invariant II --- Semantic equality

`conv(v1, v2)` must coincide with tree-calculus hash-cons identity on
closed normal forms, modulo the canonicalization guarantees each
backend commits to.

Ctxtree-style backends satisfy this trivially via `conv = fast_eq` on
(term, ctx) pairs after normalization. Closure-based backends must
recursively compare structures, introducing fresh neutrals at binders
and bottoming out at tree-identity comparisons.

Failure mode: a backend that collapses two observably different Values
(e.g., treating all closures with the same body as equal regardless of
env). That would make the type system accept terms it shouldn't.

== Invariant III --- Predicate faithfulness

Every type constructor's predicate must, when applied to a Value,
yield `TT` iff the value semantically inhabits the type.

By construction:

- `Pi A B` (§5.2) is `TT` iff, for the fresh `a : A`, `B(a)` accepts
  `f(a)`. By Invariant I this is equivalent to "for all `a : A`, `B(a)`
  accepts `f(a)`" --- the semantic Pi type.
- `RecType` is a Church-encoded conjunction of field checks; it is `TT`
  iff the value is the Church product of inhabitants.
- `Eq a b` is `TT` iff `conv(a, b) = TT` --- definitional equality by
  definition.
- `Type n` is `TT` on a Value iff the Value fits one of the four
  admissible shapes (universe below `n`, Pi at rank $≤n$, registered
  base, cumulative hypothesis). Each clause is structurally well-
  defined; their disjunction covers the intended universe.
- User-defined data types are `TT` iff the user's predicate body says
  so. Soundness of user types is the user's burden.

Every type constructor combines sub-predicates via operations
(conjunction, fresh-hypothesis introduction, recursive self-application)
that preserve faithfulness. If the leaves are faithful, the tree is.

== Invariant IV --- Universe well-foundedness

The finite universe hierarchy is strictly well-founded: every Value
accepted by `Type n` either

+ is a compound type-constructor whose components satisfy `Type n` at
  strictly-reducible positions;
+ is a registered base type at rank 0;
+ is `mk_universe k` for `k < n`, itself satisfying `Type (k+1)` via
  case 1 --- no self-reference;
+ is a hypothesis whose stored type is a universe at rank $≤n$,
  traceable to its binder.

Closed false-inhabitants require a cycle in these cases or an
inhabitant of `False` at a finite rank. Cycles are blocked by the
inductive definition; `False` (the empty predicate) has no inhabitants
by construction. Logical consistency holds modulo strong
normalization (§7, *What soundness does not include*).

== What breaks if any invariant slips

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
    [`{x : A} -> {y : A} -> x` and `... -> y` become indistinguishable;
     the type system cannot reject a swapped-projection term.],
  [`conv` is not semantic equality],
    [Either `Eq a b` accepts non-equal terms (accepts bad equalities)
     or rejects equal ones (rejects valid programs). The equality type
     becomes a liability.],
  [Predicate faithfulness --- e.g., a broken `Pi` encoding],
    [A function passes Pi-checking without actually respecting its
     intended type; downstream uses rely on type info that doesn't
     hold.],
  [Universe well-foundedness --- e.g., a malformed rank-0 declaration],
    [A user type admitted at rank 0 that is actually type-valued
     collapses the stratification in that user's hierarchy; Girard-like
     paradoxes become reachable within that scope.],
)

Evaluation-budget exhaustion is *not* on this list. When the kernel
runs out of budget mid-check it raises a compile error --- a
completeness concession, not an unsoundness. The check that *would*
have succeeded is rejected because the kernel gave up. No wrong answer
is ever reported as `TT`.

== What soundness does *not* include

Deliberately out of scope at this stage:

- *Strong normalization.* The kernel relies on the evaluation budget
  as a finite approximation; a user can write a non-terminating
  predicate that the kernel flags as error, not as `FF`. Logical
  consistency above is contingent on strong normalization of the
  tree-calculus terms in play.
- *Function extensionality.* Two functions that agree on all inputs
  but are structurally different do not satisfy `Eq`. Extensional
  equality, if needed, is a separate predicate.
- *Transfinite ranks.* `Type n` is indexed by a user-writable `Nat`.
  Ranks above `ω` (generalizations for libraries that quantify over
  "any finite universe") are not provided.

Each is a choice, not a bug. The soundness story above holds on its
own terms; we just don't claim more than it delivers.

= Open design questions

- *Spine-applied hypotheses.* `is_H` is bare-hypothesis-only: a Value
  of the form `h t` (hypothesis applied to a concrete argument) fails
  `is_H` even when `h : A -> Type`. Types like `{A : Nat -> Type 0} ->
  (n : Nat) -> A n` do not typecheck via the current §5.1 rules.
  Generalizing to `is_neutral` / `type_of_neutral` (reflection on
  arbitrary neutrals carrying their inferred types) would resolve
  this; the cost varies per backend (cheap for closure-based; requires
  spine-type inference for ctxtree).
- *Extensional equality.* `Eq` is `conv` only. Function-extensional
  equality as a companion predicate with restricted introduction rules
  is deferred until a concrete need.
- *`is_universe` as derivable vs. primitive.* The current design makes
  `is_universe` a kernel primitive each backend implements. An
  alternative would be making `Type k` structurally recognizable in
  disp alone (via canonical lambda pattern-match). The latter is more
  purist but depends on canonical-form discipline across β-reduction
  that not all backends preserve.
- *Cross-backend Value exchange.* `quote` provides canonical trees,
  but two backends comparing Values directly must agree on the
  canonical form. A shared test suite exercising Pi-checking, record
  projection, and the cumulative-hypothesis rule under each backend is
  the practical cross-check for Invariants I--IV.

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`t` / `△`], [The tree-calculus leaf. Source syntax: `t`; math
                notation: `△`.],
  [`TT` / `FF`], [The encoded booleans. Hash-cons distinct.],
  [`fork(a, b)`], [A tree with two children.],
  [*Value*], [The evaluator's representation of a term --- canonically
              a `(term, ctx)` pair. Opaque to callers; manipulated
              only via the primitive interface (§4).],
  [`apply(f, v)`], [Val-level primitive: head β-reduction, eager.
                    Includes the H-hypothesis rule.],
  [`conv(v1, v2)`], [Val-level primitive: semantic equality. Used by
                     disp-level predicates for type equality.],
  [`fresh_hyp A`], [Val-level primitive: mint a hypothesis of type `A`
                    at the current binder depth. One-argument; the
                    evaluator tracks depth.],
  [`is_H`, `type_of_H`, `identity_of_H`],
    [Val-level reflection on hypotheses.],
  [`is_pi`, `pi_dom`, `pi_cod`],
    [Val-level reflection on Pi types (§5.2).],
  [`is_universe`, `universe_rank`],
    [Val-level reflection on universe values (§5.1).],
  [`is_registered_base_type`],
    [Val-level check of membership in the enclosing `Type`'s closed-over
     registry of rank-0 types.],
  [`fast_eq`], [Tree-level primitive: O(1) hash-cons identity.
                Backend-internal. Disp predicates use `conv` instead.],
  [`triage`, `is_fork`, `first`, `second`, `kind`, `payload`],
    [Tree-level primitives for raw tree inspection. Backend-internal;
     disp predicates use Val-level reflection instead.],
  [`mk_universe k`], [Informal name for the canonical Value of `Type k`
                      as a first-class type. `Type k` is the predicate;
                      `mk_universe k` denotes its Value form. In
                      practice they are the same Value.],
  [*opacity*], [Property of fresh hypotheses: only the H-hypothesis
                rule inspects them (by convention; user predicates
                may opt out).],
  [*distinctness*], [Property of fresh hypotheses: those minted under
                     distinct binder depths are `conv`-unequal.],
  [*cumulativity*], [Property of `Type n`: a Value accepted by
                     `Type k` is accepted by `Type n` for every
                     `n ≥ k`. Falls out of §5.1's four-case definition,
                     not a separate rule.],
  [*registry*], [The set of rank-0 types `Type n` closes over at its
                 definition site. Bootstrap `{Nat, Bool, Unit}`;
                 extended by `let X : Type 0 = body` declarations.],
  [*hash-consing*], [The tree-calculus runtime's invariant: structurally
                     equal trees share object identity, so `fast_eq` is
                     pointer-equality.],
  [*parametricity*], [The consequence of opacity: a function checked
                      against a Pi cannot distinguish concrete
                      `A`-inhabitants from the fresh hypothesis, so
                      one check is equivalent to universal
                      quantification.],
  [*prelude-canonical*], [The specific predicate definitions (`Pi`,
                          `Type`, `Eq`, `Nat`, `Bool`, `Unit`) that the
                          kernel's structural reflection recognizes.
                          Users may define analogous predicates, but
                          they are not recognized structurally.],
)
