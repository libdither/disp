#set document(title: "Cubical Type Theory for Disp")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#let note(body) = block(
  breakable: false,
  above: 0.8em,
  below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  body,
)

#let q(body) = block(above: 1.5em, below: 0.6em)[*Q.* #body]
#let a(body) = block(above: 0.6em, below: 0.6em)[*A.* #body]

#align(center, text(22pt, weight: "bold")[Cubical Type Theory for Disp])
#v(0.3em)
#align(center)[
  A Q&A-style walkthrough of cubical TT, adapted to Disp's
  vocabulary and cross-referenced against `CUBICAL_PROPOSAL.typ`.
]
#v(1em)

#note[
  *Status.* Pedagogical companion to `CUBICAL_PROPOSAL.typ`. The proposal
  argues that cubical operations fit on top of Disp's existing seven-handler
  kernel as library extensions. This document explains *why* cubical
  exists, *what* its operations do, and *how* they map onto the proposal's
  specific Disp constructs.

  Adapted from Andy Shiue's
  #link("https://gist.github.com/AndyShiue/cfc8c75f8b8655ca7ef2ffeb8cfb1faf")[
    Cubical Type Theory Q&A
  ]. Notation is Disp-flavored throughout: lambdas are `{i} -> body`,
  the interval is `I` with constants `I_zero`, `I_one` and operators
  `I_and`, `I_or`, `I_inv`. Operations like `transp`, `hcomp`, `Glue` refer
  to the proposal's library functions, not generic cubical primitives.
]

= Motivation

#q[What is cubical type theory?]

#a[A type theory giving homotopy type theory (HoTT) its computational
meaning. Where HoTT *postulates* univalence as an axiom (which then
"gets stuck" at runtime), cubical TT *implements* univalence as a
theorem that actually reduces — equality between equivalent types
becomes something the kernel can compute with.]

#q[What is homotopy type theory?]

#a[Martin-Löf type theory plus two big additions:
- *Higher inductive types* (HITs) — datatypes whose constructors can
  introduce equalities between values, not just values themselves.
- *The univalence axiom* — equivalent types are equal, in the
  computationally-meaningful sense.]

#q[What's a HIT?]

#a[A type with custom equality constructors. The standard example is a
"natural-numbers-but-mod-2" type whose `succ (succ n) = n` as part of
its definition, baked into the type rather than proven separately.]

#q[And univalence?]

#a[It declares that the type universe has the strongest reasonable
equality — equivalent types are equal. You don't lose the ability to
distinguish types when needed (you can still observe that `Bool` and
`Nat` are different types); but when two types *can* be put in
correspondence, they become interchangeable.]

#q[Why should I care, concretely?]

#a[In Disp today, suppose you've defined `List_Scott` and `List_Tree` —
two equivalent encodings of the list type — and proved them iso. You
still have to thread conversions everywhere. Univalence collapses this:
the proof of equivalence *becomes* an equality, and transport along
that equality moves values between representations without manual
shimming. See §5 of `CUBICAL_PROPOSAL.typ` for the staged path.]

= Equality with structure

#q[In Disp today, what's a proof of equality?]

#a[A closed inhabitant of `Eq A x y`, of which there is essentially one:
`refl A x : Eq A x x`. Equality is opaque — a witness exists or
doesn't, but you can't look "inside" it.]

#q[What does cubical replace this with?]

#a[A *function*. Specifically, a function from an interval `I` to the
type. In Disp this is just `Pi I` — a regular dependent function with
`I` as its domain. No new type former needed:

```
Path  : {A : Type, x : A, y : A} -> Type
Path  := {A, _, _} -> Pi I ({_} -> A)

PathP : {A : I -> Type, x : A I_zero, y : A I_one} -> Type
PathP := {A, _, _} -> Pi I A
```

The endpoint arguments `x, y` are *documentation only* — they're ignored
in the body. A `Path A x y` reduces to `Pi I ({_} -> A)`, and the
"endpoints" are recovered by applying the function to `I_zero` and
`I_one`. The walker's discipline (no triaging on `i`) guarantees these
endpoint values are whatever the function returns at the boundaries.

`PathP` (the dependent version) is `Pi I A` where `A` itself depends on
`i` — used for heterogeneous paths whose endpoints live in different
types.

The trivial path (`refl`) is just a constant function:

```
refl : {A : Type, x : A} -> Pi I ({_} -> A)
refl := {A, x, i} -> x
```

`refl Nat 3` applies the first two arguments, leaving `{i} -> 3` — both
endpoints are `3` by reduction.]

#q[Why is this better than `Eq`?]

#a[Because paths *compute*. `refl Nat 3 I_zero` reduces to `3` by
β-reduction — definitionally, not propositionally. Endpoint values
fall out of function application; you don't need a separate proof that
"this path goes from here to there."

You also get functorial laws for free:

```
cong : {A, B : Type, f : A -> B, p : Pi I ({_} -> A)} -> Pi I ({_} -> B)
cong := {A, B, f, p, i} -> f (p i)
```

Function extensionality (provable here without axiom):

```
funext : {A, B : Type, h : (a : A) -> Pi I ({_} -> B)} -> Pi I ({_} -> A -> B)
funext := {A, B, h, i, a} -> h a i
```

Both are one-liners. In intensional MLTT, the first is a theorem
requiring `J`; the second is unprovable without an axiom. None of
these definitions mention endpoint values — they all just operate on
the function shape.]

= The interval, in Disp terms

#q[What is `I`?]

#a[A type with two distinguished values `I_zero` and `I_one`, plus three
operations:

```
I_and : I -> I -> I    -- "meet" (think min)
I_or  : I -> I -> I    -- "join" (think max)
I_inv : I -> I         -- "involution" (think 1 - i)
```

Together these form a *De Morgan algebra*. The laws look like Boolean
algebra (associativity, commutativity, De Morgan's law), but one
critical law is missing: `i I_or (I_inv i)` is *not* always `I_one`.
This is what keeps the interval continuous rather than discrete.]

#q[Useful intuition for `I`?]

#a[A "fuzzy Boolean." `I_zero` and `I_one` are the two extremes, like
`FF` and `TT`, but `i : I` for an *abstract* `i` is "somewhere in
between" — not committed to either endpoint. The De Morgan operations
work on this fuzzy structure.

`I_inv` plays the role of NOT (`I_inv I_zero = I_one`,
`I_inv I_one = I_zero`). `I_and` and `I_or` play AND and OR. But you
can leave `i` unspecified and still combine it with concrete values.

In the proposal (`CUBICAL_PROPOSAL.typ` §5.3), `I` is built as a
predicate-frame library type with these operations as smart
constructors.]

#q[Crucial rule about interval variables?]

#a[*You cannot inspect them*. Given an abstract `i : I`, you cannot ask
"is `i` equal to `I_zero`?" via `tree_eq` or by triage. You can only
thread `i` through the De Morgan operations and through other type
formers that accept it. This is the *parametricity discipline* — and
it's already enforced by Disp's walker, which rejects triage on
kernel-minted neutrals. See `CUBICAL_PROPOSAL.typ` §8.2.]

= Building paths

#q[How do I build a path?]

#a[Write a lambda `{i} -> body` where `body` involves `i` only through
the De Morgan operations and through paths/type-formers, never via
direct inspection. Examples:

*The constant path (refl):*
```
{i} -> x
```

*Path reversal* (precomposition with `I_inv`):
```
sym : {A : Type, p : Pi I ({_} -> A)} -> Pi I ({_} -> A)
sym := {A, p, i} -> p (I_inv i)
```

*Mapping a function over a path* (cong, shown above):
```
{i} -> f (p i)
```

*Composition (via hcomp — see §6).* Composition of paths
`p : Path A x y` and `q : Path A y z` to get `Path A x z` is *not* a
primitive operation. It's built using `hcomp`, the operation that
"fills in the missing face of a partial cube."]

#q[What's a path between paths?]

#a[A function of *two* interval variables. Given paths `p, q : Path A x y`,
a proof that `p` and `q` are themselves equal as paths is:

```
sq : Path (Path A x y) p q
sq := {i, j : I} -> body
```

where `body[I_zero/i]` is `p`, `body[I_one/i]` is `q`, and the side
conditions `body[I_zero/j]` and `body[I_one/j]` are `x` and `y`
respectively.

This is a 2D square. Each higher dimension is another interval
variable. The "cubical" in the name refers to this — proofs in cubical
TT correspond to functions out of n-dimensional cubes with
prescribed faces.]

#q[Can I draw the square?]

#a[Yes, and it helps. For the square `{i, j} -> body`:

```
              top edge: q
              (i = I_one)
         x  ─────────────── y
         │                  │
left edge│                  │ right edge
(j=I_zero│                  │ (j=I_one
 = refl x)                   = refl y)
         │                  │
         x  ─────────────── y
              bottom edge: p
              (i = I_zero)
```

The boundaries are paths, the interior is the square. Building a square
is "filling in the inside of this picture given the four edges."]

#q[What are *connections*?]

#a[Given one path `p : Path A x y`, you can mechanically build a square
from it using `I_and` or `I_or`:

```
{i, j} -> p (i I_and j)
{i, j} -> p (i I_or j)
```

These are squares whose four edges are mixtures of `p` and constant
paths. They give you 2D evidence about `p` "for free," without needing
any new constructor. Connections are why the De Morgan algebra is the
right structure — they let you generate higher-dimensional shapes by
threading interval variables through `I_and` and `I_or`.]

= Transport (`transp`)

#q[What does `transp` do?]

#a[Given a *path of types* `P : I -> Type` (so `P I_zero` and `P I_one`
are some types, possibly different) and a value `x : P I_zero`,
`transp P x` produces a value of `P I_one`.

In Disp's proposal terms:
```
transp : (P : I -> Type) -> P I_zero -> P I_one
```

Operationally, it "rides the value along as the type deforms."]

#q[How does it reduce?]

#a[By dispatching on the *outermost type former* that `P` evaluates to.
Each library type former in the proposal has a `transp_fn` slot in its
metadata. The proposal's `transp` reads this slot and routes to the
type-specific rule. From `CUBICAL_PROPOSAL.typ` §5.2:

```
Bool.transp_fn := {P, x, _} -> x          -- discrete: identity
Nat.transp_fn  := {P, x, _} -> x          -- discrete: identity

Pair.transp_fn := {P, x, _} ->            -- structural: recurse
  pair (transp ({i} -> first_of (P i)) (pair_fst x))
       (transp ({i} -> second_of (P i)) (pair_snd x))
```

If the type doesn't depend on `i` at all (like `Nat`), transport is
identity. If it's structural (like `Pair`), transport recurses on
components. If it's a function type (`Pi`), transport adapts the
function. If it's a `Glue` type, transport applies the stored
equivalence (this is what makes univalence compute).]

#q[Walked example?]

#a[Suppose we have a constant family `P := {i} -> Bool`. Then
`transp P TT` reduces as follows:

```
1. P I_zero = Bool
2. P I_one  = Bool
3. tree_eq P_I_zero P_I_one = TT   (fast path)
4. return TT
```

The proposal's `transp` has a fast path: if `tree_eq T0 T1` is `TT`
(the type is constant across the path), return `x` immediately.

For a non-trivial path like `P := ua not_equiv : Path Type Bool Bool`
(built via `Glue` from the `not : Bool -> Bool` equivalence), endpoints
are both `Bool` but the path is non-constant. Transport here falls
through to `Glue.transp_fn`, which applies the stored equivalence:

```
transp (ua not_equiv) TT  ⇝  not_equiv.forward TT  =  not TT  =  FF
```

So even though both endpoints are `Bool`, the path acts as the `not`
function under transport. This is the magic.]

#q[Where does the proposal hand-wave?]

#a[Three places worth flagging (also called out in the proposal review):

+ The `journey` parameter in every `transp_fn` is threaded around but
  never read. In real cubical, this is the *constancy cofibration* φ.
  Stages 1–3 don't need it functionally; it should either be defined
  or removed.

+ `Pi.transp_fn` uses `transp_back` and a partial application
  `B_path . _`. The first is "transport along the reversed path"
  (`transp ({i} -> P (I_inv i))`); the second elides a piece called
  `transp_fill` that's required for dependent Pi. Not defined in the
  proposal.

+ `Glue.transp_fn` is sketched as `...`. This rule is the *entire
  content* of computational univalence; it needs proper specification
  involving partial elements (see §8).

Note: because `Path` is now just `Pi I (...)` (no separate type
former), the `transp_fn` slot lives on `Pi`'s metadata — not on a
separate `Path` metadata. There's one `pi_transp_fn` rule that handles
all paths.]

= Composition (`hcomp` — "capping")

#q[What problem does `hcomp` solve?]

#a[Building paths *within a single type* by gluing partial paths
together. Where `transp` moves a value along a varying type family,
`hcomp` constructs new values inside a fixed type by filling in
missing faces of a partial cube.]

#q[Concrete intuition?]

#a[Suppose you have three sides of a square:
- *Bottom*: a path `p : Path A x y`
- *Left*: a path `l : Path A x z`
- *Right*: a path `r : Path A y w`

You want the *top* — a path from `z` to `w` that fits with the other
three. `hcomp` produces it:

```
         z ─── top ─── w     ← hcomp gives you this
         │            │
         l            r
         │            │
         x ─── p ──── y
```

Path composition falls out as a special case: given
`p : Path A x y` and `q : Path A y z`, the composite
`p · q : Path A x z` is `hcomp` applied with `refl x` on the left,
`q` on the right, and `p` as the bottom. The top is `p · q`.]

#q[Why is composition derived rather than primitive?]

#a[Because once you have `hcomp`, you don't need a separate composition
operator — composition is one specific shape of cube-filling. And you
need `hcomp` *anyway* for other reasons (HIT eliminators, Glue
reduction). So compositionality reduces to the more fundamental
"fill in the missing faces" operation.]

#q[How does `hcomp` reduce per type?]

#a[Same dispatch pattern as `transp`. From the proposal, each library
type-former gets an `hcomp_fn` slot (or a folded `cubical_ops` slot):

```
Bool.hcomp_fn := {_, _, base} -> base    -- discrete: ignore sides
Nat.hcomp_fn  := {_, _, base} -> base    -- discrete: ignore sides
```

For discrete types, `hcomp` returns the base (composition of identities
is identity). For structural types it recurses component-wise.

The proposal (§5.5) sketches the shape but leaves the partial-element
machinery (which `hcomp` *needs*) underspecified. See §8 below.]

#q[The terminology — why "capping" in the original gist?]

#a[Andy Shiue's gist calls `hcomp` "capping" — the intuition being that
you're building the *cap* (top face) of a cube given its floor and
walls. The cubical-Agda literature calls it `hcomp` for "homogeneous
composition" (same type throughout). The proposal uses `hcomp`. Same
operation.]

= Higher inductive types (HITs)

#q[What's the simplest HIT?]

#a[The circle, `S1`. Two constructors:
```
type S1 where
  base : S1
  loop : Path S1 base base
```

The first constructor is a point; the second is a *path* from `base`
to itself. So `S1` has one "value" (`base`) but it has a non-trivial
path on it (`loop`). Geometrically: a loop, which is what a circle is.]

#q[How do you write a function out of `S1`?]

#a[You match on constructors *including the path constructors*. To map
`S1 -> Unit`:

```
shrink : S1 -> Unit
shrink base := unit
shrink loop := {_ : I} -> unit
```

The `loop` case is a Path requirement: you need to provide a path in
`Unit` from `unit` to `unit`. The only one is the constant path
`{_} -> unit` — which is `refl unit`.

In general, any function `f : S1 -> T` must respect `loop`: it must
provide a path in `T` from `f base` to `f base` to match what `loop`
demands.]

#q[Why are HITs harder to support in Disp?]

#a[Because their eliminators need to *reduce* on the path constructors.
For an `S1`-eliminator `f`, applying `f loop` should produce the path
the user supplied. But that requires the kernel to recognize "this is
a path-typed constructor application" and dispatch accordingly.

The proposal's `eliminator_frame` mints `StuckElim` on hypothesis
targets. For HITs you need the dispatcher to also reduce on the path
target *and* fire when the path concretizes to `refl`. This is the
"late firing" problem flagged in `CUBICAL_PROPOSAL.typ` §8.4.

Bottom line: HITs require either Stage 4 (late-firing stucks, if that
design works) or Stage 5 (substitutable interval hypotheses, full
abstract-`i` evaluation). Not part of Stages 1–3.]

= Equivalences

#q[What's an equivalence between types?]

#a[Roughly: two functions `f : A -> B` and `g : B -> A` that compose to
identity in both directions. There are several formally equivalent
ways to spell this out (contractible fibers, "half-adjoint"
formulation, bi-invertible, etc.). They're all interchangeable; pick
one.

In Disp terms:
```
isEquiv : {A, B} -> (A -> B) -> Type
isEquiv f := contractible-fibers-or-equivalent-definition

_~=_ : Type -> Type -> Type
A ~= B := Sigma (f : A -> B) (isEquiv f)
```

Equivalences are just dependent pairs of a function and a proof of
invertibility.]

#q[Self-referential issue?]

#a["Equivalences are equivalent" sounds circular, but it isn't —
different formulations of `~=` are provably related by an equivalence
proof, which lives at the same type-theoretic level. You can prove
`(A ~=_1 B) ~=_1 (A ~=_2 B)` by hand; it's just recursion on the
definition, not a paradox.]

= Univalence (UA)

#q[What is the univalence axiom?]

#a[A statement of the form:
```
ua_axiom : (A ~= B) ~= (Path Type A B)
```

"The equivalence type and the path-in-Type are equivalent." In MLTT
this is added as an axiom (with no computational content — every
operation it touches gets stuck). In cubical TT this is a *theorem*
that reduces.]

#q[Where does the computational content come from?]

#a[From `Glue`. Given an equivalence `e : A ~= B`, you can construct a
specific `Path Type A B` using `Glue`. Call this `ua e`. This is what
`ua` is in the proposal — a *function*, not an axiom:

```
ua : {A, B} -> (A ~= B) -> Path Type A B
```

The path you get *computes* — transport along it applies the
equivalence. So univalence is not a postulate; it's a definable
operation.]

#q[Why was UA so hard before cubical?]

#a[Because there was no way to give it computational content. In MLTT,
`Path Type A B` could only be `refl A` (when `A = B`); there was no
construction that built a non-trivial path in `Type` from an
equivalence. Adding UA as an axiom got you the type-checking story
("yes, equivalences are equalities"), but operations involving the
axiom would get stuck at runtime because the axiom had no reduction
rule.

Cubical solves this by providing `Glue` as a type former whose values
*encode* equivalences directly, and whose transport rule unpacks
them.]

= `Glue` (heterogeneous cubes)

#q[What does `Glue` do?]

#a[It lets you build a type that is "almost `B`" but with specified
faces replaced by other types via equivalences.

In the proposal's terms (`CUBICAL_PROPOSAL.typ` §5.6):

```
Glue : (B : Type) -> (phi : I) -> (T : Type) -> (e : T ~= B) -> Type
```

Read informally: `Glue B phi T e` is a type that:
- Reduces to `T` when `phi = I_one` (the face is reached).
- Reduces to `B` when `phi = I_zero` (the face is not reached).
- Is some new type "in between" — pairs of a `B`-value with a partial
  `T`-value, with compatibility along the equivalence.]

#q[How does `Glue` give you a path in `Type`?]

#a[The univalence path is constructed by varying `phi` with `i`:

```
ua := {A, B : Type, e : A ~= B, i : I} ->
  Glue B (I_or i (I_inv i)) A e
```

(Or something close — see §8 for the gap.) At `i = I_zero`,
`I_or I_zero (I_inv I_zero) = I_or I_zero I_one = I_one`, so `Glue`
reduces to `A`. At `i = I_one`, similarly reduces to `B`. So this
function `I -> Type` sends `I_zero` to `A` and `I_one` to `B` — a
path from `A` to `B` in `Type`.

In between, it's a `Glue`-type carrying the equivalence. Transport
along this path unpacks the equivalence.]

#q[Constructors and destructors?]

#a[
```
glue   : ({u : T}) -> (b : B) -> Glue B phi T e         (constructor)
unglue : Glue B phi T e -> B                              (destructor)
```

You build a `Glue`-value from a `T`-fragment and a `B`-base, with a
compatibility constraint (the `T`-value must match `e.forward` of the
`B`-base on the appropriate face). You destruct by extracting the
`B`-component.]

= The gap the proposal needs to close

#q[The proposal says `Glue` is single-face. What does that mean and why
is it a problem?]

#a[Real cubical `Glue` takes a *system* of types and equivalences
indexed by faces of a cofibration `phi` — not just one `(T, e)` pair.
Written out:

```
Glue : (B : Type) -> (phi : I) ->
       (T : Partial phi Type) ->
       (e : PartialP phi ({_} -> T ~= B)) ->
       Type
```

A `Partial phi Type` is "a type for each proof that `phi` holds." When
`phi` is `(I_or (i_iface_eq i I_zero) (i_iface_eq i I_one))` (the
boundary cofibration), this is "a type for the `i = 0` face *and* a
type for the `i = 1` face" — possibly different types.

The univalence path needs `T_at_zero = A` and `T_at_one = B`, with
different equivalences on each face. The proposal's single-`T`-single-`e`
`Glue` can't express this. Without it, `ua` cannot be encoded as the
proposal claims.

To close the gap, the proposal would need to build out the partial
element machinery (`Partial`, `PartialP`, `IsOne`, `1=1`) — a
substantial library extension beyond what's currently sketched.]

#q[What about `hcomp` partial systems?]

#a[Same issue. Real `hcomp` takes:
```
hcomp : {A : Type} -> {phi : I} ->
        (u : I -> Partial phi A) ->
        (base : A) ->
        A
```

The `u` is a partial-element-valued function: for each `i : I` and
each face where `phi` holds, you supply a fragment of the cube. Without
`Partial`, you can only handle trivially-faceted compositions.

Stages 1–3 of the proposal are doable with a stripped-down `hcomp`
that only handles 2-face compositions (the path-composition case).
Beyond that, partial elements are needed.]

= The proposal's staging, in this vocabulary

#q[What's actually achievable at each stage?]

#a[Mapping the proposal's stages to capabilities:

#table(
  columns: 3,
  stroke: 0.4pt + gray,
  align: left,
  inset: 6pt,
  [*Stage*], [*Adds*], [*Capability*],
  [0],
    [Nothing],
    [Manual iso threading; `Eq` is intensional],
  [1],
    [`transp_fn` slot on `Pi` (and other type-formers); `transp`
     library function; `Path`/`PathP` as one-line aliases over `Pi I`;
     per-former `transp_fn` for Bool/Nat/Pi/Pair/List/Eq/Type],
    [Transport along `Pi I`-shaped paths;
     `refl`/`cong`/`sym`/`funext` as one-liners],
  [2],
    [`I` as De-Morgan algebra; `hcomp_fn` slot;
     `hcomp` library function],
    [Path composition; 2-face homogeneous composition;
     connections (squares from a single path)],
  [3],
    [`Partial`/`PartialP`; `Glue` with face systems;
     `ua` derivation],
    [Univalence as derivable theorem; transport along `ua e`
     applies the equivalence],
  [4],
    [`LateStuck` constructor or equivalent reframing
     (see §8.4 of the proposal — design open)],
    [`J` eliminator over paths reducing at `refl`; HIT constructor laws],
  [5],
    [Substitutable interval hypotheses (new walker variant)],
    [Full abstract-`i` evaluation; non-endpoint path induction;
     synthetic homotopy theory],
)

For Disp's stated goals (representation independence, optimizer
rewrites, neural-guided synthesis), Stages 1–3 deliver the bulk of the
practical wins. Stage 4 is the design-open piece. Stage 5 is research
territory.]

= Cheat sheet

#q[Quick reference for what each operation does?]

#a[
#table(
  columns: 2,
  stroke: 0.4pt + gray,
  align: left,
  inset: 6pt,
  [*Operation*], [*What it does*],
  [`Path A x y`], [Alias for `Pi I ({_} -> A)`. `x, y` are documentation.],
  [`PathP A x y`], [Alias for `Pi I A`. Dependent codomain (heterogeneous).],
  [`refl x`], [Constant function `{i} -> x`. Inhabits any `Path A x x`.],
  [`cong f p`], [Function composition with the path: `{i} -> f (p i)`.],
  [`sym p`], [Precomposition with `I_inv`: `{i} -> p (I_inv i)`.],
  [`p · q` (compose)], [`hcomp`-built composite. Endpoint check is structural.],
  [`funext h`], [Lambda commutation: `{i, a} -> h a i`.],
  [`transp P x`], [Move `x : P I_zero` along the type-path to `P I_one`.],
  [`hcomp u base`], [Fill in the missing face of a partial cube.],
  [`ua e`], [Build a path in `Type` from an equivalence `A ~= B`.],
  [`Glue B phi T e`], [Type "almost `B` but `T` on `phi`-faces, glued by `e`".],
  [`glue / unglue`], [Construct / destruct a `Glue`-typed value.],
  [`J P base p`], [Path induction. `transp` along motive-as-path. Stage 4.],
)
]

= Where to go from here

#q[I want to do something concrete in Disp. What's tractable today?]

#a[Nothing cubical is implemented yet — the proposal is a design
document. The current Disp kernel still has per-type handlers (see
`CLAUDE.md`'s "Implementation status"). The migration to the unified
seven-handler kernel (Pi/Bool/Nat/Eq as library types) is the
precondition for any of this work.

If you want to play with cubical *ideas* in Disp's syntax, the most
tractable starting point is to write paths as raw `I -> A` functions
(treating `I` as `Bool` for now, since stages 1–3 don't strictly need
the free De Morgan algebra) and see how `refl`, `cong`, `sym`, and
funext fall out as trivial one-liners. The proposal's Stage 1 is
essentially "make this principled by adding `transp_fn` slots."]

#q[I want to read further. Where?]

#a[
- `CUBICAL_PROPOSAL.typ` (this repo) — the Disp-specific design.
- `TYPE_THEORY.typ` (this repo) — the underlying seven-handler design
  the proposal builds on.
- #link("https://arxiv.org/abs/1611.02108")[Cohen, Coquand, Huber, Mörtberg — "Cubical Type Theory: a constructive interpretation of the univalence axiom"] (CCHM) — the foundational paper.
- #link("https://arxiv.org/abs/1802.01170")[Angiuli, Brunerie, Coquand, Favonia, Harper, Licata — "Cartesian Cubical Computational Type Theory"] (ABCFHL) — alternative formulation without De Morgan.
- #link("https://agda.readthedocs.io/en/latest/language/cubical.html")[Cubical Agda documentation] — practical implementation reference.
]
