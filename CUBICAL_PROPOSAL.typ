#set document(title: "Disp Cubical Integration Proposal")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")

#let note(body) = block(
  breakable: false,
  above: 0.8em,
  below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  body,
)

#align(center, text(22pt, weight: "bold")[Cubical Integration Proposal])
#v(0.3em)
#align(center)[
  Design exploration for cubical-flavored transport in Disp, framed
  as library extensions over the existing seven-handler kernel.
]
#v(1em)

#note[
  *Status.* Exploratory proposal. Builds on `TYPE_THEORY.typ`'s unified
  design (7 kernel primitives). Argues that `transp`, `hcomp`, `Glue`,
  and the interval `I` can all be implemented as library constructions
  using existing kernel mechanisms — `predicate_frame`, `hyp_reduce`'s
  Action protocol, `eliminator_frame`'s stuck-mint, the walker's
  parametricity — *without adding a new kernel handler*. The kernel
  stays at seven primitives; the library grows by a metadata slot, two
  type formers, and two dispatcher functions.

  Not authoritative. The goal is to map the design space concretely
  and identify what falls out of existing structure versus what needs
  fundamentally new kernel capability.
]

= Motivation

== The one-sentence pitch

*Cubical adds a symbolic-execution layer over an "interval" that makes
isomorphisms compute definitionally — so iso roundtrip reduces to
`refl` structurally, instead of producing propositional proofs you have
to thread by hand.*

That's the practical core. Everything else — the De Morgan algebra,
`Glue`, partial elements, the ∞-groupoid story — is machinery in
service of that goal. For Disp specifically, this is exactly the
capability the optimizer roadmap needs: representation-independence as
a *computational* fact rather than a manually-discharged proof
obligation.

== Why current Disp can't deliver this

Disp's existing design (`TYPE_THEORY.typ`) handles propositional
equality via intensional `Eq` and the H-rule on stuck-Eq values. For
representation-independence and definitional equivalence of types,
this leaves a gap:

- *Iso-roundtrip is propositional, not definitional.* Given
  `f : A -> B` and `g : B -> A` with `g (f x) = x` provable, the
  type system treats `g (f x)` and `x` as distinct values. Conversion
  (`tree_eq`) returns `FF`. To use the equality, you thread the proof
  through every site.

- *Equivalent types are not interchangeable computationally.* Two
  equivalent types `A ≃ B` are different trees; no operation lets you
  transparently move values between them.

- *Transport along equivalences must be threaded explicitly.*
  `f : A -> X` does not type-check on `b : B` even when `A ≃ B`;
  the conversion is yours to manage.

These follow from Pi being *weak* in the standard type-theoretic
sense — function types without judgmental η-equality, where pointwise
equality only gives propositional equality.

== What cubical changes

Cubical replaces propositional equality of values (`Eq A x y`) with
*paths* — functions from a special "interval" type `I` to `A`, with
the endpoints being `x` and `y` by computation. Equality becomes a
function shape rather than an inductive datatype. Three consequences:

+ *Function extensionality is free.* Pointwise-equal functions are
  path-equal by a one-line construction (`funext h := λi a. h a i`).
  No axiom, no propositional indirection.

+ *Univalence is derivable, not axiomatic.* The type former `Glue`
  lets you construct `Path Type A B` from an equivalence `A ≃ B`. The
  resulting path *computes*: transport along it applies the
  equivalence, structurally reducing to a tree that's tree-eq-identical
  to the right answer.

+ *Iso-roundtrip is refl-checkable.* For ua-mediated equivalences,
  `transp (sym (ua e)) (transp (ua e) x)` reduces — by the per-type
  rules — to a tree structurally equal to `x`. Conversion via `tree_eq`
  returns `TT`. Definitional, not propositional.

The mechanism is symbolic execution over interval variables: paths
are functions of an abstract `i : I` that thread through type-formers
without being inspected. The walker's parametricity discipline (no
triage on neutrals) is mechanically the same constraint, already in
place — which is why this fits Disp's existing kernel without
modification.

== Three orthogonal benefits, in decreasing relevance to Disp

While "iso-roundtrip is definitional" is the headline, cubical
delivers two further capabilities worth flagging:

+ *(Primary)* Univalence-mediated representation independence. Direct
  payoff for synthesizer/optimizer goals.
+ *(Secondary)* Free function extensionality. Useful in any
  parametric/equivalence reasoning, even when no isos are involved.
+ *(Tertiary)* Higher inductive types — types whose constructors
  include equational laws. The route to "optimizer rewrites baked into
  the type" for synthesizer search-space collapse.

What's *not* on this list, and is safely deferrable: higher-dimensional
path reasoning (paths between paths between paths, ∞-groupoid
structure, synthetic homotopy theory). These are research-grade
capabilities; nothing in Disp's roadmap demands them.

== Why this fits Disp's existing kernel

An earlier draft of this document proposed adding a new kernel handler
(`path_apply`) parallel to `hyp_reduce` to mediate transport. *That
addition is unnecessary.* On closer inspection of Disp's existing
primitives, `transp`/`hcomp`/`Glue` fit cleanly into the
library-dispatch pattern already used by `predicate_frame` and
`eliminator_frame`, with `StuckElim` providing the universal fallback
for cases that cannot structurally reduce.

This document specifies that library design (§3-§7), then investigates
what evaluating a type-family `P : I → Type` at abstract `i` would
require, and where the genuine walls are (§8). The unification of
`transp` and `hcomp` into a single `comp` primitive, and the addition
of `Partial` cofibration machinery, is investigated in §10.

= The structural insight

Look at what `transp`, `hcomp`, and `Glue` need to do operationally:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Operation*], [*Shape*],
    [`transp P x`],
      [Dispatch on `P`'s head type-former; recurse component-wise;
       fallback to a stuck.],
    [`hcomp φ sides base`],
      [Dispatch on the target type's head former; recurse component-wise;
       fallback to a stuck.],
    [`Glue B [φ ↦ (T, e)]`],
      [Library type with non-trivial recognizer; codomain_fn that reads
       the normalized φ-formula.],
  ),
  caption: [Structure of the three cubical operations.],
)

The shape — *dispatch on type-former, recurse structurally, stuck-fallback* —
is the same shape Disp already uses for `predicate_frame` (recognizer
dispatch with H-rule fallback) and `eliminator_frame` (case dispatcher
with stuck-mint fallback). Cubical operations are not new in kind; they
are new instances of an existing pattern.

#note[
  *`StuckElim` is already Disp's transport mechanism.* When an
  eliminator fires on a hypothesis target, the kernel mints
  `StuckElim (motive target) target` — a value of the motive's type,
  opaque, identified by the target. That is structurally identical to
  "transport produces a value of the codomain type, opaque, identified
  by the source value." Paths become *additional kinds of evidence*
  that can either reduce structurally (when the target type's
  `transp_fn` recognizes the path's shape) or get stuck.
]

= Background: shared dispatch pattern

== Existing pattern — `hyp_reduce` + `codomain_fn`

When `apply(neutral, v)` evaluates, the dispatcher routes to
`hyp_reduce`. The handler reads the neutral's stored type's
`codomain_fn` slot and dispatches on the returned `Action`
(`Extend` / `Return`). Each library type-former contributes its own
`codomain_fn` in its metadata. The kernel handler is a thin
dispatcher; the per-former logic lives in the library.

== Cubical's pattern — per-type-former transport rules

```
transp (λi. Nat) φ x                  = x   -- non-dependent: identity
transp (λi. A i × B i) φ (a, b)       = (transp ... a, transp ... b)
transp (λi. (a : A i) -> B i a) φ f   = λa. transp ... (f (transp_back ...))
transp (λi. Glue ...) φ x             = <equivalence-mediated conversion>
```

Per-type-former rules. Same dispatch shape as `codomain_fn`: look up a
type-specific function, apply it.

== The new framing

In the previous draft, this parallel motivated a new kernel handler
(`path_apply`). The new framing keeps the parallel but locates it at
the library layer: each library type's metadata gains a `transp_fn`
slot, and `transp` is a library function that reads it. No kernel
addition.

= Library design

== Metadata extension

Each library type-former's metadata grows from a 3-tuple to a 4-tuple:

```
T_meta := pair recognizer_sig
              (pair params
                    (pair codomain_fn       // existing: for hyp_reduce
                          transp_fn))        // NEW: for library transp
```

`transp_fn` is either:
- The LEAF sentinel `t` (T does not participate in structural
  transport; `transp` falls back to `StuckElim`).
- A function `(P, x, journey) -> transported_value` that
  knows how this type former composes under transport.

The slot is read by the library `transp` function, never by the
kernel. The kernel handlers are unchanged.

== Per-type-former `transp_fn` values

First, the helpers `transp_fill` (forward fill of a trajectory along a
type-path) and `transp_back` (transport along the reversed path):

```
transp_fill := \{P, i, x\} -> transp (\{j\} -> apply P (i I_and j)) x
transp_back := \{P, y\} -> transp (\{i\} -> apply P (I_inv i)) y
transp_fill_back := \{P, i, y\} ->
  transp (\{j\} -> apply P (i I_or (I_inv j))) y
```

Per-type rules:

```
// Discrete: identity (only reached when fast path doesn't trigger).
Bool.transp_fn  := \{self, P, x\} -> x
Nat.transp_fn   := \{self, P, x\} -> x
False.transp_fn := \{self, P, x\} -> x

// Pair: component-wise recursion.
Pair.transp_fn := \{self, P, x\} -> \{
  let A_path = \{i\} -> pair_fst (params_of (apply P i))
  let B_path = \{i\} -> pair_snd (params_of (apply P i))
  pair (self A_path (pair_fst x))
       (self B_path (pair_snd x))
\}

// Sigma: dependent second component via a-trajectory.
Sigma.transp_fn := \{self, P, x\} -> \{
  let A_path = \{i\} -> pair_fst (params_of (apply P i))
  let B_fn   = \{i\} -> pair_snd (params_of (apply P i))
  let a0     = pair_fst x
  let b0     = pair_snd x
  let a_traj = \{i\} -> transp_fill A_path i a0
  let a1     = a_traj I_one
  let B_path = \{i\} -> apply (B_fn i) (a_traj i)
  pair a1 (self B_path b0)
\}

// Pi (general dependent): contravariant in A, covariant in B,
// threading a-trajectory through B.
Pi.transp_fn := \{self, P, f\} -> \{
  let A_path  = \{i\} -> pi_dom (apply P i)
  let B_fn_at = \{i\} -> pi_cod (apply P i)
  \{a1\} -> \{
    let a0     = transp_back A_path a1
    let a_traj = \{i\} -> transp_fill_back A_path (I_inv i) a1
    let B_path = \{i\} -> apply (B_fn_at i) (a_traj i)
    self B_path (apply f a0)
  \}
\}

// Eq: refl at the new endpoints (requires endpoint paths to coincide).
Eq.transp_fn := \{self, P, p\} -> \{
  let A_path = \{i\} -> eq_carrier (apply P i)
  let x_path = \{i\} -> eq_lhs (apply P i)
  refl (A_path I_one) (x_path I_one)
\}

// List: recurse on the value's structure.
List.transp_fn := fix (\{loop, self, P, xs\} -> \{
  let A_path = \{i\} -> list_elem_type (apply P i)
  match (is_nil xs) \{
    TT => nil
    FF => cons (self A_path (list_head xs))
               (loop self P (list_tail xs))
  \}
\})

// Type: closed types transport as themselves; non-trivial only via Glue.
Type.transp_fn := \{self, P, X\} -> X
```

For non-dependent (discrete) types: identity transport. For structural
types: recurse component-wise. For dependent types: thread an
`a`-trajectory built via `transp_fill_back`. For Glue, see §5.6.

== The `transp` library function

```
transp := fix (\{self, P, x\} -> \{
  let T0 = apply P I_zero
  let T1 = apply P I_one
  // Fast path: constant family → identity transport.
  match (tree_eq T0 T1) \{
    TT => x
    FF => \{
      // Dispatch on T0's transp_fn slot.
      let tfn = transp_fn_of T0
      match (tree_eq tfn t) \{
        TT => StuckElim T1 (pair P x)   // sentinel: stuck
        FF => apply tfn (pair self (pair P x))
      \}
    \}
  \}
\})
```

The dispatcher reads the target type's `transp_fn` from metadata. If
present, it invokes it (passing `self` so the rule can recurse via
`transp`). If absent (sentinel `t`), it produces a stuck. Each
library type opts in by populating its slot.

#note[
  *Why metadata, not a library switch.* Each type former carries its
  own transport rule, so new types extend the system without modifying
  a central function. This matches the metadata-driven philosophy of
  the unified design: `codomain_fn`, `transp_fn`, and (potentially)
  future slots all live on the type, dispatched by the kernel or
  library by reading metadata.
]

== `I` — the De Morgan algebra

`I` is a library type whose elements are formulas in the free De Morgan
algebra over a set of names. Variables are kernel-minted neutrals of
stored type `I`. Constants and operations are tagged trees:

```
let tag_zero = stem t
let tag_one  = stem (stem t)
let tag_and  = stem (stem (stem t))
let tag_or   = stem (stem (stem (stem t)))
let tag_inv  = stem (stem (stem (stem (stem t))))

let I_zero = pair tag_zero t
let I_one  = pair tag_one  t

let I_and = \{a, b\} -> pair tag_and (pair a b)
let I_or  = \{a, b\} -> pair tag_or  (pair a b)
let I_inv = \{a\}    -> pair tag_inv a

let I_recognizer = \{_, v\} ->
  select_lazy
    (\{_\} -> TT)                                       // I_zero or I_one
    (\{_\} -> select_lazy
      (\{_\} -> dispatch_op v)                          // tagged op
      (\{_\} -> FF)
      (is_concrete_fork v))
    (or (tree_eq v I_zero) (tree_eq v I_one))

I := guard (predicate_frame_form
  (t I_recognizer_sig (t t t)))
```

The recognizer recurses via `I (pair_fst body)`, so I-typed neutral
subterms (variables in formulas) route through `predicate_frame`'s
H-rule. Walker-safe: tags are tree-eq'd, no triage on neutrals.

A library function `I_normalize` reduces formulas to a canonical form
(DNF, say). Two I-formulas are De-Morgan-equal iff their normalized
trees are hash-cons-identical. Conversion stays O(1) on normalized
inputs.

== `Path` and `PathP` as aliases over `Pi I`

`Path A x y` and `PathP A x y` do not need to be separate library
types. They are one-line aliases for `Pi I`:

```
Path  := \{A, _, _\} -> Pi I (\{_\} -> A)
PathP := \{A, _, _\} -> Pi I A
```

The endpoint arguments are *documentation only* — discarded in the
body. After elaboration, `Path A x y` is just `Pi I (\{_\} -> A)`,
and `PathP A x y` is `Pi I A` (dependent codomain). Endpoint values
are recovered by applying paths to `I_zero` and `I_one`; the walker's
discipline (no triage on `i`) ensures the boundary values are
whatever the function returns at the endpoints.

Why this works:

- *No separate `path_recognizer`.* The Pi-recognizer already handles
  "for every `i : I`, `f i : A`" via `bind_hyp I (\{i\} -> A (f i))`.
  That body check IS the path's body-well-typedness check.
- *No separate `path_transp_fn`.* `Pi.transp_fn` dispatches uniformly
  on the codomain; paths are the special case where the domain is `I`.
- *No boundary-check kernel slot.* The "boundaries hold at the
  endpoints" property is purely *computational* — by the walker's
  discipline, the only paths constructible are those whose endpoints
  are the function's values at `I_zero` and `I_one`. No assertion
  needed.

Core operations become one-liners over the alias:

```
refl   := \{A, x, i\} -> x
cong   := \{A, B, f, p, i\} -> f (p i)
sym    := \{A, p, i\} -> p (I_inv i)
funext := \{A, B, h, i, a\} -> h a i
```

None of these reference endpoint values; they all operate on the
underlying `Pi I` function shape.

When a theorem needs to *assert* specific endpoints (e.g., "this is a
path from `x` to `y`"), use a propositional helper rather than a
type-level check:

```
endpoints := \{A, f, x, y\} ->
  Sigma (Eq A (f I_zero) x) (\{_\} -> Eq A (f I_one) y)
```

The proof obligation is moved from type-level to propositional. This
covers the rare cases where named endpoints actually matter (path
composition compatibility, `J`'s motive); the rest of cubical operates
purely on the function shape.

== `hcomp` as a library function

Real cubical `hcomp` takes a partial element `u : (i : I) -> Partial φ A`.
For Stages 1–2 without `Partial` (see §10 for the full design), we use
a stripped-down 2-face variant that handles path composition and
component-wise structural recursion:

```
TwoSides := \{T : Type\} -> Type
TwoSides T := Sigma (Pi I (\{_\} -> T))
              (\{_\} -> Pi I (\{_\} -> T))

hcomp : \{T : Type, sides : TwoSides T, base : T\} -> T
hcomp := \{T, sides, base\} -> \{
  let hfn = hcomp_fn_of T
  match (tree_eq hfn t) \{
    TT => StuckElim T (pair sides base)
    FF => apply hfn (pair sides base)
  \}
\}

// Filling: give the entire trajectory, not just the top face.
hfill : \{T : Type, sides : TwoSides T, base : T\} -> Pi I (\{_\} -> T)
hfill := \{T, sides, base, i\} ->
  hcomp T
    (pair (\{j\} -> apply (pair_fst sides) (i I_and j))
          (\{j\} -> apply (pair_snd sides) (i I_and j)))
    base
```

Per-type rules:

```
// Discrete: identity on base.
Bool.hcomp_fn  := \{sides, base\} -> base
Nat.hcomp_fn   := \{sides, base\} -> base
False.hcomp_fn := \{sides, base\} -> base

// Pair: component-wise.
Pair.hcomp_fn := \{sides, base\} -> \{
  let A = pair_fst (params_of (type_of base))
  let B = pair_snd (params_of (type_of base))
  let left_A  = \{i\} -> pair_fst (apply (pair_fst sides) i)
  let left_B  = \{i\} -> pair_snd (apply (pair_fst sides) i)
  let right_A = \{i\} -> pair_fst (apply (pair_snd sides) i)
  let right_B = \{i\} -> pair_snd (apply (pair_snd sides) i)
  pair
    (hcomp A (pair left_A right_A) (pair_fst base))
    (hcomp B (pair left_B right_B) (pair_snd base))
\}

// Pi: pointwise hcomp.
Pi.hcomp_fn := \{sides, base\} -> \{
  let A    = pi_dom (type_of base)
  let B_fn = pi_cod (type_of base)
  \{a\} -> \{
    let B_at_a    = apply B_fn a
    let left_at   = \{i\} -> apply (apply (pair_fst sides) i) a
    let right_at  = \{i\} -> apply (apply (pair_snd sides) i) a
    hcomp B_at_a (pair left_at right_at) (apply base a)
  \}
\}

// Sigma: fill first component first, transport second through its
// trajectory (uses transp; would be cleaner with comp — see §10).
Sigma.hcomp_fn := \{sides, base\} -> \{
  let A    = pair_fst (params_of (type_of base))
  let B_fn = pair_snd (params_of (type_of base))
  let left_fst  = \{i\} -> pair_fst (apply (pair_fst sides) i)
  let right_fst = \{i\} -> pair_fst (apply (pair_snd sides) i)
  let fst_fill  = hfill A (pair left_fst right_fst) (pair_fst base)
  let new_fst   = apply fst_fill I_one
  let left_snd  = \{i\} -> pair_snd (apply (pair_fst sides) i)
  let right_snd = \{i\} -> pair_snd (apply (pair_snd sides) i)
  let snd_transported = transp (\{i\} -> apply B_fn (apply fst_fill i))
                               (pair_snd base)
  pair new_fst
       (hcomp (apply B_fn new_fst) (pair left_snd right_snd) snd_transported)
\}
```

Path composition falls out as a 4-line `hcomp` application:

```
compose_path : \{A : Type, x, y, z : A,
                p : Path A x y, q : Path A y z\} -> Path A x z
compose_path := \{A, x, y, z, p, q, i\} ->
  hcomp A
    (pair (\{_\} -> x)   // left wall: constant at x (refl x)
          q)             // right wall: q
    (apply p i)          // base: p i
```

== `Glue2` as a library type

Real cubical `Glue B [φ ↦ (T, e)]` takes a partial element system
`(T, e)`. Stages 1–3 without full `Partial` machinery use `Glue2`, a
2-face version parameterized by the interval position, sufficient for
`ua`. The full `Partial`-based design is deferred to §10.

```
// Glue2: parameterized by base B, two face-types T_zero, T_one, two
// equivalences, and an interval position i.
//   at i = I_zero: reduces to T_zero
//   at i = I_one:  reduces to T_one
//   strictly between: a "glued" type pairing B with partial T-info

let glue2_recognizer = \{params, v\} -> \{
  let B      = glue2_B params
  let T_zero = glue2_T_zero params
  let T_one  = glue2_T_one params
  let i      = glue2_i params
  match (tree_eq i I_zero) \{
    TT => apply T_zero v
    FF => match (tree_eq i I_one) \{
      TT => apply T_one v
      FF => apply B (pair_fst v)   // strictly between: check B-component
    \}
  \}
\}

let glue2_transp_fn = \{self, P, v\} -> \{
  let B_path = \{i\} -> glue2_B (params_of (apply P i))
  let i_at   = \{j\} -> glue2_i (params_of (apply P j))
  let P0_params = params_of (apply P I_zero)
  let P1_params = params_of (apply P I_one)
  match (tree_eq (i_at I_zero) I_zero) \{
    TT => \{
      // Source at i=0 face: v inhabits T_zero
      let b_src = apply (equiv_fwd (glue2_e_zero P0_params)) v
      let b_dst = self B_path b_src
      match (tree_eq (i_at I_one) I_zero) \{
        TT => apply (equiv_bwd (glue2_e_zero P1_params)) b_dst
        FF => match (tree_eq (i_at I_one) I_one) \{
          TT => apply (equiv_bwd (glue2_e_one P1_params)) b_dst
          FF => StuckElim (apply P I_one) (pair P v)
        \}
      \}
    \}
    FF => match (tree_eq (i_at I_zero) I_one) \{
      TT => \{
        // Source at i=1 face: v inhabits T_one
        let b_src = apply (equiv_fwd (glue2_e_one P0_params)) v
        let b_dst = self B_path b_src
        match (tree_eq (i_at I_one) I_zero) \{
          TT => apply (equiv_bwd (glue2_e_zero P1_params)) b_dst
          FF => match (tree_eq (i_at I_one) I_one) \{
            TT => apply (equiv_bwd (glue2_e_one P1_params)) b_dst
            FF => StuckElim (apply P I_one) (pair P v)
          \}
        \}
      \}
      FF => StuckElim (apply P I_one) (pair P v)  // mid-region: stuck
    \}
  \}
\}

Glue2 := \{B, T_zero, T_one, e_zero, e_one, i\} ->
  guard (wait kernel_ref.predicate_frame
    (pair glue2_recognizer_sig
          (pair (pair B (pair T_zero (pair T_one
                  (pair e_zero (pair e_one i)))))
                (pair t (pair glue2_transp_fn glue2_hcomp_fn)))))

ua := \{A, B, e, i\} -> Glue2 B A B e (id_equiv B) i
// Endpoints check:
//   ua A B e I_zero = Glue2 B A B e id I_zero -> recognizer collapses to A ✓
//   ua A B e I_one  = Glue2 B A B e id I_one  -> recognizer collapses to B ✓
```

`Glue2`'s recognizer reads `i` and dispatches via `tree_eq` against
endpoints — no triage on `i` itself, walker-safe. Its `transp_fn`
handles the equivalence-mediated case-analysis at faces. Mid-region
transport (when both endpoints' `i` are strictly between `I_zero` and
`I_one`) is left stuck pending the full `Partial`-based design.

== HIT eliminator machinery

HITs require eliminators that reduce on path-typed constructors. The
`eliminator_frame` pattern extends naturally — the dispatcher recognizes
tagged path constructors and routes to user-supplied path-cases.

Example: the circle `S1`.

```
// Tagged constructors
s1_base_tag := stem t
s1_loop_tag := stem (stem t)

S1_base := pair s1_base_tag t
S1_loop := \{i\} -> pair s1_loop_tag i  // Pi I (\{_\} -> S1)

// Note: S1_loop I_zero = pair s1_loop_tag I_zero, structurally distinct
// from S1_base. The boundary equality (loop I_zero ≡ base) is enforced
// propositionally by the recognizer's expectations on eliminator cases.

s1_recognizer := \{_, v\} -> \{
  let tag = pair_fst v
  match (tree_eq tag s1_base_tag) \{
    TT => TT
    FF => match (tree_eq tag s1_loop_tag) \{
      TT => I_recognizer t (pair_snd v)   // payload must be I-valued
      FF => FF
    \}
  \}
\}

S1 := guard (wait kernel_ref.predicate_frame
  (pair s1_recognizer_sig (pair t t)))

// Eliminator dispatcher: cases are (base_case, loop_case).
// loop_case : Pi I (\{_\} -> C) with boundary loop_case I_zero ≡ base_case.
s1_dispatcher := \{motive, cases, target\} -> \{
  let base_case = pair_fst cases
  let loop_case = pair_snd cases
  let tag = pair_fst target
  match (tree_eq tag s1_base_tag) \{
    TT => base_case
    FF => match (tree_eq tag s1_loop_tag) \{
      TT => apply loop_case (pair_snd target)
      FF => StuckElim (apply motive target) (pair cases target)
    \}
  \}
\}

S1_rec := wait kernel_ref.eliminator_frame
  (init_meta_arity_3 s1_dispatcher)
```

For HITs with higher-dimensional constructors (squares, cubes), the
dispatcher pattern extends: more tags, more cases, with each higher
case demanding a higher-dimensional path in the result type. The
kernel machinery is unchanged; only the per-HIT dispatcher grows.

*Boundary obligation.* The user-supplied `loop_case` must satisfy
`loop_case I_zero ≡ base_case` and `loop_case I_one ≡ base_case`
(propositionally, via `Eq`). The recognizer does not enforce this; it
is a library obligation tracked at the eliminator's construction site.
For HITs where boundaries hold *definitionally* (e.g., constant loop
cases), this is automatic.

= What the library design delivers

== Capabilities

+ *Definitional iso roundtrip.* `transport_back p (transport p x) ⇝ x`
  via library normalization on stuck identities. `transport p_inv
  (transport p x)` produces `StuckElim A (p_inv, StuckElim B (p, x))`,
  which a library normalizer recognizes by hash-cons pattern and
  collapses to `x`.

+ *Structural transport on type formers.* Each library type opts in by
  populating its `transp_fn`. `transp` recurses via the slot.

+ *Univalence as a definable theorem.* `ua e` constructs a Glue-based
  path; transport along it reduces via `Glue.transp_fn`, which applies
  the equivalence.

+ *Representation independence in practice.* Functions over
  `List BoolTree` work on `List BoolScott` via transport, with
  `List.transp_fn` recursing element-wise.

+ *No kernel growth.* The seven primitives remain seven. Cubical is
  added entirely at the library layer.

== Limitations

+ *Endpoint-only path evaluation.* `transp` evaluates `P` at `I_zero`
  and `I_one`, then dispatches on the shared head former. Paths whose
  intermediate behavior matters are not captured.

+ *Walker constraints on path bodies.* Path bodies cannot triage on
  their `i`-argument. Bodies must use `i` only via I-operations or by
  passing it as data to I-consuming type formers (Glue, etc.). This
  matches cubical's discipline but excludes some classical-looking
  definitions.

+ *Conversion modulo De Morgan.* `tree_eq` compares trees structurally,
  not modulo lattice equalities. Library smart constructors must
  normalize their I-arguments (`Glue (I_normalize phi) ...`) so that
  hash-cons identity captures De-Morgan equivalence.

+ *No higher inductive types directly.* HITs require constructor laws
  that are themselves paths; absent the abstract-i machinery (§8),
  HITs are out of scope for this design.

= Single-pass implementation plan

This proposal commits to *one full pass* — no intermediate phases, no
transitional `transp_fn`/`hcomp_fn` slots — going directly to `comp` as
the unified primitive with `Partial` machinery in place from the start.
The motivation is design coherence: every transitional state is debt
that has to be migrated later, and the unified `comp` approach is
cleaner in the dependent cases (Sigma, Pi) where separate `transp`/`hcomp`
would need hand-coordination.

The implementation has nine layers organized by dependency. Each layer
is mechanically completable once the layers below it are in place.

== Layer 0 — Interval

```
lib/cubical/interval.disp           // I type, I_zero, I_one, I_and, I_or, I_inv
lib/cubical/interval_normalize.disp // I_normalize to canonical (DNF) form
```

`I` is a `predicate_frame`-based library type with tagged operators.
Smart constructors auto-normalize so that `tree_eq` captures De-Morgan
equivalence. No upstream dependencies. ~150 lines.

== Layer 1 — Cofibrations and partial elements

```
lib/cubical/isone.disp           // IsOne : I -> Type
lib/cubical/partial.disp         // Partial, PartialP
lib/cubical/systems.disp         // Smart constructors for face systems
```

`IsOne phi` is a unit-when-`phi=I_one`, empty-when-`phi=I_zero`,
neutral-when-abstract proposition. `Partial phi A := IsOne phi -> A`.
`PartialP phi A := Pi (IsOne phi) A`. See §11 for the deep dive on
how `Partial` actually works and what it needs to handle.

Walker integration: `IsOne phi` for abstract `phi` produces neutral
proofs that library smart constructors can case-analyze raw (privileged
via kernel handlers), but user code cannot. ~100 lines.

== Layer 2 — Metadata refactor

```
lib/kernel/handlers.disp         // extend predicate_frame metadata to 4-tuple
src/tree.ts                       // update host fast-path projections
lib/types/*.disp                  // every type-former gets a comp_fn slot
test/*.test.disp                  // sweep ~120 tests against new metadata shape
```

Metadata layout changes once, irreversibly:

```
T_meta := pair recognizer_sig (pair params (pair codomain_fn comp_fn))
```

The biggest mechanical change — 30-50 file edits, but each is
straightforward. ~50 new lines plus ~150 disrupted lines in existing
type files. Test sweep is the largest single mechanical task.

== Layer 3 — `comp` dispatcher

```
lib/cubical/comp.disp            // comp, transp, hcomp library functions
lib/cubical/fill.disp            // comp_fill, hfill, transp_fill
```

```
comp : (A : Pi I (\{_\} -> Type))
       -> (phi : I)
       -> (u : Pi I (\{i\} -> Partial phi (apply A i)))
       -> (u0 : apply A I_zero)
       -> apply A I_one

comp := fix (\{self, A, phi, u, u0\} -> \{
  let T0 = apply A I_zero
  let cfn = comp_fn_of T0
  match (tree_eq cfn t) \{
    TT => StuckElim (apply A I_one)
                    (pair A (pair phi (pair u u0)))
    FF => apply cfn
                (pair self (pair A (pair phi (pair u u0))))
  \}
\})

// transp and hcomp as one-line derivations
transp := \{A, x\} -> comp A I_zero empty_partial_family x
hcomp  := \{T, phi, u, u0\} -> comp (\{_\} -> T) phi u u0

// fills (one-line via De Morgan connection)
comp_fill := \{A, phi, u, u0, i\} ->
  comp (\{j\} -> apply A (i I_and j))
       (phi I_or (I_inv i))
       (restrict u i)
       u0
```

~100 lines.

== Layer 4 — Per-type `comp_fn` rules

```
lib/types/bool.disp              // Bool.comp_fn  (discrete: u0)
lib/types/nat.disp               // Nat.comp_fn
lib/types/false.disp             // False.comp_fn
lib/types/pair.disp              // Pair.comp_fn (component-wise)
lib/types/sigma.disp             // Sigma.comp_fn (fill first, transport second)
lib/types/pi.disp                // Pi.comp_fn (a-trajectory, dependent codomain)
lib/types/eq.disp                // Eq.comp_fn
lib/types/list.disp              // List.comp_fn (structural recursion)
lib/types/type.disp              // Type.comp_fn (identity, Glue handles non-trivial)
```

Each rule receives the full 5-argument `comp` signature
`(self, A, phi, u, u0)` and produces a value of `A I_one`. Discrete
types return `u0`. Structural types recurse component-wise on both the
type-path `A` and the side-system `u`. Dependent types thread an
`a`-trajectory through the codomain.

Total: ~400 lines across the type files.

== Layer 5 — Path/PathP aliases and core operations

```
lib/cubical/path.disp            // Path, PathP aliases
lib/cubical/path_ops.disp        // refl, cong, sym, funext, compose_path, inv_path, J
```

All one-liners over `comp`. ~50 lines total. See §5.4 for the alias
design.

== Layer 6 — Equivalences

```
lib/cubical/equiv.disp           // isContr, fiber, isEquiv, Equiv (~=)
```

`A ~= B := Sigma (f : A -> B) (\{_\} -> isEquiv f)`, with `isEquiv`
defined via contractibility of fibers. All in terms of Sigma/Pi/Path.
~80 lines.

== Layer 7 — Glue and univalence

```
lib/cubical/glue.disp            // Glue, glue, unglue, Glue.comp_fn
lib/cubical/ua.disp              // ua, ua_compute helper
```

`Glue` takes proper `Partial`-based face systems. `Glue.comp_fn` is the
substantial implementation — the workhorse of computational univalence.
See §12 for the deep dive on how it works.

```
ua := \{A, B, e, i\} ->
  Glue B (i I_or (I_inv i))
    (system_boundary A B)
    (system_boundary e id_equiv)
```

~250 lines (Glue is dense).

== Layer 8 — HIT eliminator machinery

```
lib/kernel/handlers.disp         // eliminator_frame extension for path constructors
lib/cubical/hit_dispatcher.disp  // pattern library for tagged HIT dispatchers
lib/types/circle.disp            // S^1 as worked example
```

Extend `eliminator_frame`'s dispatcher to recognize tagged
constructor-application-to-interval patterns. Library-side dispatcher
pattern for any HIT. Boundary-equality obligations on user-supplied
path cases become propositional `Eq` proofs. ~200 lines kernel +
per-HIT extras.

== Layer 9 — Test sweep

The largest single task. Update ~120 existing tests for the new
4-tuple metadata layout. Add ~80-150 new tests for `comp`-derived
operations, per-type rules, `ua e`-transport reduction, iso-roundtrip
structural equality, and S^1 eliminator behavior.

== Scope summary

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Layer*], [*New lines*], [*Disrupted lines*],
    [0. Interval], [~150], [0],
    [1. Cofibrations], [~100], [0],
    [2. Metadata refactor], [~50], [~150 (existing types)],
    [3. comp dispatcher], [~100], [0],
    [4. Per-type rules], [~400], [~50 (existing type files)],
    [5. Path/PathP, ops], [~50], [0],
    [6. Equivalences], [~80], [0],
    [7. Glue, ua], [~250], [0],
    [8. HIT machinery], [~200], [~30 (eliminator_frame)],
    [9. Test sweep], [~600], [~150 (existing tests)],
    [*Total*], [*~2000*], [*~380*],
  ),
  caption: [Estimated implementation scope.],
)

Roughly 2400 lines of changes. With a focused 1-2 week effort and the
two deep-dive sections below (§11 on `Partial`, §12 on `Glue.comp_fn`)
as detailed references, the design is concrete enough to implement
without further architectural decisions.

== Risks specific to single-pass

+ *Partial's walker integration may need iteration.* The privilege
  boundary (library can analyze `IsOne` proofs raw, user can't) is
  subtle. Expect 1-2 rounds of refinement.

+ *Glue.comp_fn coherence is the hardest piece.* The mid-region
  case-analysis involves multiple hcomps composed via boundary
  cofibrations. Dedicate debugging time specifically here.

+ *Test sweep is mostly mechanical but tedious.* Most failures will be
  metadata-shape assertions that need straightforward updates.

+ *Interval-formula normalization is optimization-sensitive.* DNF vs.
  ANF choice affects `tree_eq` performance on real workloads. Benchmark
  before committing.

= Open questions

== One metadata slot or many?

`transp_fn`, `hcomp_fn`, and any future operation-fn are all "library
dispatcher hooks." Two layouts to consider:

+ *Multiple slots.* Each operation gets its own metadata slot. Clean
  separation; metadata grows linearly with operations.

+ *Single dispatch table.* One metadata slot holds a record
  `\{transp_fn, hcomp_fn, ...\}`. Closed for extension at the metadata
  layout level; types implement the record.

The single-table approach scales better but couples metadata layout to
a moving target. The per-slot approach is concrete but means library
type definitions become longer over time. The current code uses the
3-tuple `(recognizer_sig, params, codomain_fn)`; extending to a record
of named fields would be a one-time refactor.

Recommendation: start with multiple slots (4-tuple at Stage 1, 5-tuple
at Stage 2), revisit at Stage 3 if the slot count exceeds five.

== `tree_eq`-based conversion versus normalized conversion

Disp's commitment is `conv = tree_eq`, O(1). Once I-formulas appear in
type metadata, this requires smart constructors to normalize on
construction so that `Glue B phi1 T e` and `Glue B phi2 T e` (with
`phi1` and `phi2` De-Morgan-equivalent) produce the same tree.

This is achievable but adds a normalization step to every smart
constructor that takes I-arguments. The alternative is a
conversion-modulo-De-Morgan check, which breaks the O(1) property.

Recommendation: normalize on construction. Pay the cost at type-build
time, keep conversion O(1).

== Path bodies that produce types

Type-valued paths (`Path Type A B`) are central to univalence. The body
of such a path returns a Type, which is itself a wait-form. Walker
discipline applies: the body cannot triage on `i`, but it can produce
types whose metadata contains `i` as data. Glue is precisely such a
construction — `\i. Glue B [phi(i) ↦ (T, e)]` is walker-safe iff
`phi` is a closed combinator on I that doesn't triage on its
argument. This works.

= Abstract-i evaluation — what would it actually require

This section investigates evaluating `P : I → Type` at an abstract `i`
and inspecting the result. The investigation reveals that the walker
already allows much more than expected, and the genuine wall is
elsewhere than initially thought.

== What "abstract i" means

`P : I → Type` can be evaluated three ways:

- *Closed evaluation*: `P I_zero`, `P I_one` — fully concrete results.
- *Closed-substitution evaluation*: `P (some_closed_I_value)` — for
  any closed I-formula. Reduces to a concrete tree.
- *Hypothesis evaluation*: applying `P` to an I-typed neutral. Runs
  under the walker; result is a tree containing the neutral as data.

Stages 1-3 use only the first two forms. Abstract-i evaluation means
making the third form productive — inspecting `P i` for a hypothesis
`i` to make decisions the closed forms cannot.

== What the walker already allows (and it is more than expected)

Trace `apply(P, i_h)` where `i_h = Hyp I id` and
`P = \{i\} -> Pair (A_path i) (B_path i)`:

+ Dispatcher routes by signature on `P`. P has no kernel signature →
  walker.
+ Walker enters fork rule on `P`'s compiled tree, S-rule cascade,
  eventually reaches the Pair-constructor closure applied to
  `(A_path i_h, B_path i_h)`.
+ `Pair_constructor = \{a, b\} -> pair Pair_sig (pair a b)` — a closed
  combinator with no triage on its arguments.
+ Result: `pair Pair_sig (pair A_result B_result)` where `A_result`
  and `B_result` are trees containing `i_h` as data.

The result's outer root is `Pair_sig` — closed. Triage on it (e.g.,
`pair_fst`) is fine: walker rejects triage on neutral-rooted values,
and `Pair_sig` is not a neutral.

The neutral `i_h` flows through metadata but never becomes a root. The
walker's parametricity constraint — *no triage on a tree whose root is
a kernel-minted neutral* — is exactly the cubical discipline
("path bodies must not branch on intervals"). It matches what abstract-i
evaluation needs, mechanically enforced.

Operations that work under the walker at abstract `i`:

+ *Reading the outer signature*: `pair_fst (P i_h) = Pair_sig`. Closed.
+ *Reading metadata*: `pair_snd (P i_h)` returns a fork with closed
  root. Traverse further while reading closed sub-roots.
+ *Threading `i_h` through closed combinators*: I-operations like
  `I_or i_h I_zero` build forks with closed tags as roots, never
  triaging on `i_h`.
+ *Returning types with `i_h` in metadata*: `Glue B [phi(i_h) ↦ ...]`
  is a wait-form with `Glue_sig` root — closed.

This means *`transp`'s structural recursion can read `P i_h` for
abstract `i_h` for every type former whose metadata is laid out
inertly in `i_h`.* Stages 1-3 do not actually require closed-endpoint
evaluation — they could evaluate at abstract `i_h` and get the same
dispatch information.

== What actually breaks

Four cases worth examining:

*Case 1 — substitution-into-evaluated trees.* I initially thought
`transp` needs to "substitute `I_zero` for `i` in an already-evaluated
`P i_h`." It does not. Closed substitution is just another
application: `apply(P, I_zero)` reduces fully concretely. `transp` has
`P` in hand and can call it at any I-value at any time. Substitution
into already-evaluated terms is never needed.

*Case 2 — lattice-equality conversion.* Two types `Glue B (I_or i
I_zero) T e` and `Glue B i T e` differ structurally but are
De-Morgan-equal. `tree_eq` distinguishes them. The fix: smart
constructors normalize their I-arguments on construction. `Glue B phi
T e := guarded_glue B (I_normalize phi) T e`. After normalization,
hash-cons identity captures lattice equality. Library hygiene, no
kernel change.

*Case 3 — parametric reasoning over paths.* `Pi (Path A x y) (\{p\}
-> body using p)` mints `p_h` as a hypothesis of stored type
`Path A x y`. If body invokes `transp p_h x`, the dispatch tries to
read a `transp_fn` from `p_h`'s metadata — but `p_h` is opaque; no
metadata to read. Result: `transp p_h x ⇝ StuckElim B (p_h, x)`,
which is *correct*: under an abstract path, transport is opaque.
Downstream rules can match this stuck shape when `p_h` becomes
concrete.

*Case 4 — late firing of stuck identities under substitution.* This
is the genuine wall.

Consider `J : (P : (b : A) -> Path A x b -> Type) -> P x refl -> (b :
A) -> (p : Path A x b) -> P b p`. To define `J`, given a hypothesis
`p_h : Path A x b_h`, we must produce a value of `P b_h p_h`.
Cubical's `J` reduces `J P base x refl ⇝ base` when the path is
`refl`. In Disp's stuck framework:

```
J P base b_h p_h ⇝ StuckElim (P b_h p_h) (J_id, base)
```

This is typeable. But when later `p_h` becomes `refl` (e.g., via Eq's
H-rule or other substitution), the stuck's identity contains `p_h` as
a hash-cons-frozen subterm. There is *no operation in the current
kernel that re-fires the stuck's computation with the new value of
`p_h`*. The stuck remains opaque forever.

Cubical handles this by having the J-reduction encoded as part of the
*reduction relation on paths*: when a path reduces to `refl`, every
J-application on that path reduces too. This is "late firing" — a
deferred computation that re-runs when its dependency becomes concrete.

Tree calculus has no late firing. Hash-cons identity is structural: a
tree containing `p_h` does not auto-update when `p_h` reduces. The
only way to "update" is to re-build the tree with the new value, which
requires knowing where `p_h` appears and what to recompute — which is
exactly what late firing would do, if it existed.

== The genuine wall: deferred firing under concretization

The structural problem is not "abstract i" per se. It is:

#note[
  *Stuck identities are frozen.* `StuckElim T id` is a kernel-minted
  neutral whose hash-cons identity depends on `id`. When `id` contains
  a hypothesis `p_h`, the stuck's identity is fixed. Later
  concretization of `p_h` (via H-rule or library reduction) produces a
  *new* concrete value for `p_h`, but no operation traverses
  pre-existing stucks to recompute them with the new `p_h`.

  Cubical's reduction relation has built-in propagation of
  concretization through the term graph. Tree calculus's only
  reduction is application: re-applying an already-applied term
  requires re-running the application, which means re-traversing the
  whole computation.
]

What's needed is a kernel-level notion of *late-firing stuck*:

```
LateStuck T id late_fn := stuck-typed neutral whose identity is `id`,
  but which carries a closed combinator `late_fn` such that, when `id`
  is later concretized to a value v, the stuck's "real" reduction is
  `late_fn v`.
```

When the kernel observes that `id` has reduced to a concrete value
(via some triggering mechanism — possibly hash-cons-id-change tracking
or explicit "fire" calls), it replaces the stuck with `late_fn v`.

This is genuinely new kernel structure, but it is *one* extension with
clear local semantics — not a sibling walker variant.

== A revised Stage 4 — late-firing stucks

```
StuckElim_late : T -> id -> late_fn -> Tree
  -- Produces a stuck of stored type T, identity id, with deferred rule
  -- late_fn that fires when id concretizes.

fire_stuck : stuck -> concrete_id_value -> Tree
  -- If stuck is a LateStuck whose id matches concrete_id_value's id,
  -- returns late_fn concrete_id_value. Otherwise identity.
```

The kernel addition: one new constructor (`LateStuck`), one operation
(`fire_stuck`). The reduction discipline is unchanged; the walker is
unchanged. The new structure is purely additive.

Library use:

- `J`'s implementation: `J P base b_h p_h := StuckElim_late (P b_h p_h)
  p_h (\{p_concrete\} -> if p_concrete = refl then base else ...)`.
- When `p_h` later reduces to `refl` (via Eq's H-rule firing on the
  path's type), the library `fire_stuck` traversal hits the late-rule
  and reduces.

This is *not* full cubical — it does not give abstract-i triage. But
it does enable `J` over paths, HIT-like patterns with constructor laws,
and equivalence-mediated transport that needs to fire when the
equivalence becomes definitionally `refl`. These are the practical
wins beyond Stage 1-3.

== Two stages instead of one

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Stage*], [*Kernel addition*], [*Capability*],
    [4. Late-firing stucks],
      [`LateStuck` constructor + `fire_stuck` operation. Additive; no
       walker change.],
      [`J` eliminator over paths. HIT constructor laws.
       Equivalence-fires-to-refl reduction.],
    [5. Full abstract-i],
      [New reduction discipline for I-typed hypotheses (parametric
       value carrier, substitution as kernel primitive).],
      [Abstract-i triage. Full path induction including non-endpoint
       cases. Synthetic homotopy theory.],
  ),
  caption: [Refined post-Stage-3 staging.],
)

Stage 4 (late-firing) is a small, local addition that delivers the
genuinely practical wins missing from Stages 1-3. Stage 5 is the
research-grade addition that delivers the theoretical completeness.

For Disp's stated goals (synthesizer search-space collapse, optimizer
rewrites, representation independence), Stages 1-3 cover the bulk of
practical wins. Stage 4 unlocks `J` and HIT-style patterns where the
optimizer benefits from "this computation fires when the path becomes
concrete." Stage 5 is open research, justified only if a use case
demands abstract-i reasoning.

= `comp` unification and `Partial` machinery

The library design in §5 uses `transp` and `hcomp` as separate
operations with separate metadata slots. Real CCHM cubical packages
them as a single primitive `comp` that does both — and the proper
specification of `hcomp` (and `Glue`'s mid-region transport) requires
the `Partial` machinery this section sketches.

== `comp` as the unified primitive

The CCHM signature:

```
comp : (A : Pi I (\{_\} -> Type))
       -> (phi : I)
       -> (u : Pi I (\{i\} -> Partial phi (apply A i)))
       -> (u0 : apply A I_zero)
       -> apply A I_one
```

Read informally: given a *varying* type family `A`, a cofibration `phi`,
side data `u` filling `phi`-faces of the cube, and a base `u0` at
`i = I_zero`, produce a value at `i = I_one` that agrees with `u I_one`
on `phi`.

This generalizes both:

```
transp A x       = comp A I_zero (empty-side-system) x
hcomp T u u0     = comp (\{_\} -> T) phi u u0
```

`transp` is `comp` over a varying family with no sides; `hcomp` is
`comp` over a constant family with sides. The reduction rules for `comp`
on each type former subsume both.

=== Unified per-type rule

With `comp` as primitive, each type former exposes a single `comp_fn`
slot replacing the separate `transp_fn` and `hcomp_fn`:

```
T_meta := pair recognizer_sig
              (pair params
                    (pair codomain_fn comp_fn))   // 4-tuple, not 5
```

Per-type rules (sketched, full versions below):

```
Bool.comp_fn := \{self, A, phi, u, u0\} -> u0
Nat.comp_fn  := \{self, A, phi, u, u0\} -> u0
// Discrete: identity on base. Side system u is dispatched to but
// never matters since values can't change inside the type.

Pair.comp_fn := \{self, A, phi, u, u0\} -> \{
  let A_path = \{i\} -> pair_fst (params_of (apply A i))
  let B_path = \{i\} -> pair_snd (params_of (apply A i))
  let u_fst  = \{i\} -> partial_map pair_fst (apply u i)
  let u_snd  = \{i\} -> partial_map pair_snd (apply u i)
  pair (self A_path phi u_fst (pair_fst u0))
       (self B_path phi u_snd (pair_snd u0))
\}

Pi.comp_fn := \{self, A, phi, u, u0\} -> \{
  let A_dom  = \{i\} -> pi_dom (apply A i)
  let B_at   = \{i\} -> pi_cod (apply A i)
  \{a1\} -> \{
    let a_traj = \{i\} -> transp_fill_back A_dom (I_inv i) a1
    let a0     = a_traj I_zero
    let B_path = \{i\} -> apply (B_at i) (a_traj i)
    let u_at_a = \{i\} -> partial_map (\{f\} -> apply f (a_traj i)) (apply u i)
    self B_path phi u_at_a (apply u0 a0)
  \}
\}

Sigma.comp_fn := \{self, A, phi, u, u0\} -> \{
  let A_path = \{i\} -> pair_fst (params_of (apply A i))
  let B_fn   = \{i\} -> pair_snd (params_of (apply A i))
  let u_fst  = \{i\} -> partial_map pair_fst (apply u i)
  let fst_fill = \{i\} ->
    self (\{j\} -> A_path (i I_and j)) phi
         (\{j\} -> partial_restrict u_fst (i I_and j))
         (pair_fst u0)
  let new_fst = fst_fill I_one
  let B_path = \{i\} -> apply (B_fn i) (fst_fill i)
  let u_snd  = \{i\} -> partial_map pair_snd (apply u i)
  pair new_fst (self B_path phi u_snd (pair_snd u0))
\}
```

Sigma's rule shows the payoff: with separate `transp` + `hcomp`, the
first-component fill and the second-component transport are separate
steps that need explicit coherence; with unified `comp`, the
first-component fill *is* the trajectory, and the second-component lives
in a type computed *along that trajectory* in one operation.

=== Trade-offs of `comp` unification

*Pros:*
- *One slot, simpler metadata.* The 5-tuple (with separate `transp_fn`
  and `hcomp_fn`) collapses back to 4-tuple.
- *Cleaner dependent cases.* Sigma, Pi, and any future dependent type
  former need only one rule that handles both transport and composition
  coherently.
- *Matches CCHM.* The literature's primary primitive is `comp`; a Disp
  implementation aligning with CCHM is easier to verify against
  published soundness proofs.
- *`hcomp` and `transp` as derived.* Both become library functions over
  `comp`, not separate dispatchers:
  ```
  transp := \{P, x\} -> comp P I_zero (\{_\} -> empty_partial) x
  hcomp  := \{T, sides, base\} ->
    comp (\{_\} -> T) (boundary_phi) (sides_to_partial sides) base
  ```

*Cons:*
- *Each rule is more complex.* Even the "discrete" rule must accept all
  five arguments (`self, A, phi, u, u0`) even though most are unused.
  Verbose for simple cases.
- *Requires `Partial`.* `comp` cannot be specified without `Partial`/`PartialP`.
  Adding `Partial` is non-trivial (next subsection).
- *Less intuitive for users.* Library authors writing per-type rules
  must understand both the type-varying and side-varying axes
  simultaneously; the separation in §5 lets each axis be reasoned about
  independently.

*Recommendation.* For Disp specifically: start with separate `transp_fn`
and `hcomp_fn` (§5) as Stages 1–2; consider unifying to `comp_fn` at
Stage 3 when `Partial` is built out anyway. The early stages benefit
from per-axis clarity; once `Partial` is in play, unification pays off.

== `Partial` — the cofibration machinery

`Partial phi A` is "an `A`-value defined where `phi` holds." Conceptually:

```
Partial phi A := IsOne phi -> A
```

where `IsOne : I -> Type` is the proposition "this formula equals
`I_one`." Operationally, a partial element is a function whose domain
is restricted to proofs that `phi` is satisfied.

=== `IsOne` as a library type

```
let isone_recognizer = \{phi, proof\} -> \{
  // proof is a witness that phi evaluates to I_one
  match (tree_eq phi I_one) \{
    TT => tree_eq proof unit_witness   // canonical singleton
    FF => FF                            // proof shouldn't exist
  \}
\}

IsOne := \{phi\} -> guard (wait kernel_ref.predicate_frame
  (pair isone_recognizer_sig (pair phi t)))

one_is_one : IsOne I_one
one_is_one := unit_witness
```

`IsOne I_one` has exactly one inhabitant (`unit_witness`); `IsOne I_zero`
has no inhabitants; `IsOne i` for abstract `i` is propositionally
undetermined.

=== `Partial` and `PartialP`

```
Partial : I -> Type -> Type
Partial := \{phi, A\} -> Pi (IsOne phi) (\{_\} -> A)

PartialP : (phi : I) -> Partial phi Type -> Type
PartialP := \{phi, A\} -> Pi (IsOne phi) A
```

A `Partial phi A` is a function from `IsOne phi`-proofs to `A`. When
`phi = I_one`, there's exactly one such proof (`one_is_one`), so a
`Partial I_one A` is essentially "an `A`-value." When `phi = I_zero`,
no proofs exist, so the function is trivial.

=== System notation

The system notation `[i = I_zero ↦ a, i = I_one ↦ b]` desugars to a
case-analysis function on `IsOne phi`-proofs:

```
system_2 : \{A : Type, a : A, b : A\} ->
           Partial (I_or (i I_eq I_zero) (i I_eq I_one)) A
system_2 := \{A, a, b\} -> \{proof_phi\} -> \{
  // proof_phi witnesses (i = I_zero) ∨ (i = I_one)
  // case-analysis on which disjunct holds yields a or b
  ...
\}
```

The case-analysis requires *inspecting* the proof. For walker-safety,
this inspection happens inside the library function that *creates* the
system, not by user code that *uses* it. Library smart constructors
build systems by composing pre-existing partial elements via union and
intersection.

=== `glue` and `unglue` as proper constructors

With `Partial`, `Glue` recovers its standard form:

```
Glue : (B : Type) -> (phi : I) ->
       (T : Partial phi Type) ->
       (e : PartialP phi (\{_\} -> T ~= B)) ->
       Type

glue   : (\{t : PartialP phi T\}) -> (b : B) -> Glue B phi T e
         // t and b must agree along e on phi-faces
unglue : Glue B phi T e -> B
         // projects out the B-component
```

This is the full cubical `Glue`, including the mid-region case the
`Glue2` of §5.6 left stuck. With it:

```
ua e := \{i\} ->
  Glue B
    (I_or (i I_eq I_zero) (i I_eq I_one))
    (system_2 A B)
    (system_2 e id_equiv)
```

The face system encodes both endpoints. `ua e`'s `comp_fn` (or
`transp_fn` if not unified) handles the full case-analysis on which
face is active at each endpoint.

=== Costs of `Partial`

Adding `Partial`/`PartialP`/`IsOne` is a substantial library extension:

- *Three new library types* (`IsOne`, `Partial`, `PartialP`).
- *Smart constructors* for systems with compatibility checks.
- *Walker integration*: `IsOne phi` for abstract `phi` produces neutral
  proofs that thread through; library code uses them only by case-split
  *within the library*, not by user code under the walker.
- *Hash-cons normalization* of `phi` formulas (mentioned in §7) so that
  De-Morgan-equivalent cofibrations produce identical `IsOne` types.

The cost is real but bounded — about three additional library files
of comparable size to the existing type-former files (`pi.disp`,
`bool.disp`, etc.). The kernel is unchanged.

=== Recommendation

Build `Partial` at Stage 3, alongside `Glue`. At Stage 3 you need
`Glue`'s mid-region transport, which needs `Partial` either way.
Doing `comp` unification at the same time (replacing separate
`transp_fn`/`hcomp_fn` slots with a single `comp_fn` slot) costs one
metadata-shape sweep but produces a cleaner final design. Stages 1–2
can defer both — the separate-slot approach in §5 is sufficient for the
practical wins those stages deliver.

= Trade-offs summary

#figure(
  table(
    columns: 4,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Stage*], [*Kernel additions*], [*Library additions*], [*Capability*],
    [0], [None], [None], [Manual iso threading],
    [1], [None],
      [`transp_fn` slot in metadata; `transp` function; `Path`/`PathP`
       as aliases over `Pi I`; per-former rules for Bool/Nat/Pi/Sigma/
       Pair/List/Eq/Type; one-liner `refl`/`cong`/`sym`/`funext`.],
      [Structural transport. Paths-as-functions. Definitional iso
       roundtrip via stuck normalization.],
    [2], [None],
      [`I` library type with De Morgan ops + `I_normalize`;
       `hcomp_fn` slot; `hcomp` function.],
      [Path composition. Connections as data. Paths in `I`.],
    [3], [None],
      [`Glue` library type with non-trivial codomain_fn /
       `transp_fn`; `ua` derivation.],
      [Univalence as derivable theorem. Equivalence-as-equality
       computationally for closed-endpoint paths.],
    [4],
      [`LateStuck` constructor + `fire_stuck` operation. Additive,
       walker-unchanged.],
      [Library `J` eliminator. HIT constructor laws. Equivalence-
       fires-to-refl reduction.],
      [Late-firing stucks (practical post-Stage-3 wins).],
    [5],
      [New reduction discipline for I-typed hypotheses (parametric
       carrier + substitution-as-primitive).],
      [None beyond Stage 4.],
      [Full cubical with abstract-i triage. Synthetic homotopy.],
  ),
  caption: [Staged additions.],
)

== Disp-specific observations

The unified design's commitment to "minimal kernel, library
type-formers" is *preserved through Stage 3* under this framing. The
metadata-driven dispatch pattern generalizes naturally: `codomain_fn`
for application of neutrals, `transp_fn` for structural transport,
`hcomp_fn` for composition. All three are library hooks dispatched by
library functions over closed metadata reads.

Stage 4 (late-firing stucks) extends an existing kernel mechanism
additively — `StuckElim` already exists; it just gains a deferred-rule
variant. The reduction discipline is unchanged; the walker is
unchanged. This is the natural successor to the metadata-driven
pattern.

Stage 5 is qualitatively different. Substitutable hypotheses are a
sibling discipline to parametric ones, not an extension. If Disp ever
needs full abstract-i cubical, that is the architectural decision;
nothing in Stages 1-4 forecloses it, and nothing in Stages 1-4 demands
it.

= Summary

The architectural insight: `transp`, `hcomp`, `Glue`, and `I` all fit
the library-dispatch pattern Disp already uses. `StuckElim` is the
kernel's existing notion of "value-by-evidence"; paths are just
additional kinds of evidence; transport is just additional kinds of
elimination. The kernel handlers were already shaped right; the
library fills in the content.

Three concrete additions deliver practical cubical:

- *Stage 1.* `transp_fn` metadata slot + library `transp` + `Path` type.
  Structural transport along library-constructed paths. No kernel
  change.

- *Stage 2.* `I` as De Morgan algebra library type + `hcomp` library
  function + connections. No kernel change.

- *Stage 3.* `Glue` as a library type + `ua` derivation. Univalence
  as a derivable theorem. No kernel change.

A fourth stage delivers the genuine cubical: abstract-i evaluation via
a new `bind_interval` primitive and a second walker variant for
substitutable hypotheses. This is the only stage that fundamentally
extends the kernel's reduction discipline. It is open for future work
and not required for Disp's stated goals.

The architectural shape Disp already has — metadata-driven dispatch
via `hyp_reduce` + `codomain_fn`, `predicate_frame` + recognizer,
`eliminator_frame` + dispatcher — makes Stages 1-3 fall out as new
instances of an existing pattern. The kernel stays at seven
primitives; cubical lives in the library.
