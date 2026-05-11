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
  Design exploration for adding cubical-flavored transport to Disp,
  built on the unified-design kernel.
]
#v(1em)

#note[
  *Status.* Exploratory proposal. Builds on `TYPE_THEORY.typ`'s unified
  design (7 kernel primitives). Sketches a path to cubical-style
  transport via Option B (parallel `path_apply` handler) and identifies
  what would be required beyond Option B to reach full cubical
  (intervals, Glue, higher paths).

  Not authoritative. The goal is to map out the design space concretely,
  not commit to implementation.
]

= Motivation

The unified design's kernel (`TYPE_THEORY.typ`) handles propositional
equality via intensional `Eq` and the H-rule on stuck-Eq values. For
representation-independence and definitional equivalence of types,
this leaves a gap:

- Iso-roundtrip is propositional (via weak Pi) but not definitional.
- Equivalent types are not interchangeable computationally.
- Transport along equivalences must be threaded explicitly.

Cubical TT addresses these by making transport reduce structurally on
type formers. The hypothesis here: Disp's existing `hyp_reduce` +
`codomain_fn` dispatch pattern is structurally identical to cubical's
`transp` + per-type-former rules. A parallel kernel primitive
(`path_apply`) with the same dispatch pattern can deliver cubical's
structural transport while leaving the rest of the kernel untouched.

This document specifies that parallel primitive (Option B), then
discusses what additional pieces (interval, Glue, higher paths) would
be required to reach full cubical.

= Background: shared dispatch pattern

== Existing pattern — `hyp_reduce` + `codomain_fn`

When `apply(neutral, v)` evaluates, the dispatcher routes to
`hyp_reduce`. The handler reads the neutral's stored type's
`codomain_fn` and dispatches on the returned `Action` (`Extend` or
`Return`):

```
q_hyp_reduce_fn := \{ks, raw, query\} -> fix (\{self, meta, v\} -> \{
  let stored = q_unguard_or_self ks (neutral_meta_type meta)
  let cod_fn = pair_snd (pair_snd (type_meta stored))
  let action = cod_fn meta v
  match (is_extend action) \{
    TT => extend_spine
    FF => match (is_return action) \{
      TT => return_value
      FF => InvalidType
    \}
  \}
\})
```

Each library type-former contributes its own `codomain_fn` in its
metadata. The kernel handler is a thin dispatcher. The `codomain_fn`
runs in raw mode (per spec §3.4), so it can introspect neutrals freely.

== Cubical's pattern — `transp` + per-type-former rules

Cubical's `transp` does structurally analogous work:

```
transp : (P : I -> Type) -> (φ : I) -> P 0 -> P 1
```

It dispatches on `P`'s outer type former and applies the per-former rule:

```
transp (λi. Nat) φ x                  = x   -- non-dependent: identity
transp (λi. A i × B i) φ (a, b)       = (transp ... a, transp ... b)
transp (λi. (a : A i) -> B i a) φ f   = λa. transp ... (f (transp_back ...))
transp (λi. Path (A i) (x i) (y i)) φ p = <path composition>
transp (λi. Glue ...) φ x             = <equivalence-mediated conversion>
```

Per-type-former rules. Same dispatch pattern as `codomain_fn`: read a
type-specific function, apply it.

== The parallel

Both kernels solve the same shape of problem:

- A reduction on some value, where behavior is type-former-specific.
- A kernel dispatcher that reads metadata and applies the rule.
- Type-former-specific rules supplied by library code.

The operations differ (`hyp_reduce` extends a spine or returns a value;
`transp` produces a transported value), but the *mechanism* is
identical.

= Option B — `path_apply` parallel primitive

== The handler

Add a new kernel primitive `path_apply`, parallel to `hyp_reduce`:

```
q_path_apply_fn := \{ks, raw, query\} -> \{meta, x\} -> \{
  // meta encodes the path: (source_type, target_type, journey)
  let A = pair_fst (pair_fst meta)
  let B = pair_snd (pair_fst meta)
  let journey = pair_snd meta

  // Dispatch via the source type's transp_fn
  let A_transp_fn = read_transp_fn A
  A_transp_fn (journey, x)
\}
```

`path_apply` runs in raw mode (kernel handler). Type-former-specific
`transp_fn` values run inside it, also in raw mode. The handler is
structurally identical to `hyp_reduce`: read a dispatch function from
metadata, apply it.

== `Path A B` as a library wait-form

```
make_type_path : (A : Type) -> (B : Type) -> Journey -> Path A B
make_type_path A B journey := wait kernel_ref.path_apply
  (pair (pair A B) journey)
```

`Path A B` values have `path_apply`'s signature. Applying a path to a
value `x` is dispatched by the kernel's signature recognizer, routed to
`path_apply`, which performs the transport.

== Journey encoding

The journey is the path's structural content. Variants:

- `refl_journey` — identity transport.
- `iso_journey (to : A -> B) (from : B -> A)` — iso-induced.
- `compose_journey (p : Path A B) (q : Path B C)` — composed paths.
- `parametric_journey (inner : Path X Y)` — used by container types
  (List, Pair, etc.) whose transport recurses via the inner path.

Each variant is a tagged value the kernel recognizes. New journey
kinds can be added as library extensions; the kernel just routes them
through `transp_fn`.

== Metadata extension

Each library type-former gains a new metadata slot `transp_fn`:

```
T_meta := pair recognizer_sig
              (pair params
                    (pair codomain_fn       // existing: for hyp_reduce
                          transp_fn))        // NEW: for path_apply
```

Per-type-former `transp_fn` values:

```
Bool.transp_fn := \{journey, x\} -> x
Nat.transp_fn  := \{journey, x\} -> x

Pi.transp_fn := \{journey, f\} -> \arg ->
  let dom_path = source_journey journey
  let cod_path = target_journey journey
  apply cod_path (f (apply (inv_journey dom_path) arg))

List.transp_fn := fix (\{self, journey, xs\} ->
  match (is_nil xs) \{
    TT => nil
    FF => cons (apply (inner_journey journey) (head xs))
               (self journey (tail xs))
  \})

Eq.transp_fn := <path composition formula>
```

For non-dependent types: transport is identity. For dependent types:
structural recursion via the journey's inner components.

== Hash-cons stability

Path construction is deterministic in inputs:

```
make_type_path A B refl_journey   -- same tree for given (A, B)
```

Two structurally-identical paths share hash-cons identity. Conv via
`tree_eq` on paths is O(1). The H-rule reconstruction for path-typed
hypotheses works analogously to existing predicate_frame H-rule.

== Inverse-pair collapse

For `transport_back p (transport p x)` to reduce to `x` definitionally,
`path_apply` checks for inverse-journey patterns:

- `transport p x` evaluates via `path_apply`, may produce a stuck if
  `x` is hypothesis-typed.
- `transport_back p (that_stuck)` evaluates via `path_apply` with
  inverse journey. Before producing another stuck, checks the inner
  stuck's journey identity. If it matches the inverse of the current
  journey, collapse to the inner hypothesis.

This is the `TypePath`-style stuck-collapse mechanism from previous
discussions, integrated into `path_apply`.

== Soundness

`path_apply` runs in raw mode. Per-type-former `transp_fn` values are
library-supplied; the kernel runs them as-is.

Library authors are responsible for correctness of `transp_fn`. A
miswritten rule produces type-mismatched transports, caught at the
public boundary by predicate evaluation on the result. The trust model
matches the existing `codomain_fn`: kernel-routed, library-supplied,
weakly verified.

The journey field is verified at path construction time (per Disp's
weak Pi). Specifically, `iso_journey` requires proofs of the
roundtrip laws; `compose_journey` requires the two paths to type-line-
up correctly.

= What Option B delivers

== Capabilities

+ *Definitional iso roundtrip.* `transport_back p (transport p x) ≡ x`
  via stuck-collapse.

+ *Structural transport on type formers.* Transport on `List`, `Pi`,
  `Pair`, etc. recurses through structure automatically.

+ *Type-level equivalence as equality* (limited form). `ua_path iso`
  gives a path between types; transport along it converts values via
  the iso. Not full univalence (no Glue), but the practical computational
  effect is the same for iso-based equivalences.

+ *Library-defined path composition* with kernel-mediated reduction.
  `compose_path p q` is a value; transport along it composes the
  transports.

+ *Representation-independence in practice.* Functions over `List
  BoolTree` work on `List BoolScott` via transport, with the kernel
  doing the element-wise conversion.

== Limitations

+ *No abstract intervals.* Paths have fixed endpoints. There's no way
  to abstract over `i : I` and reason about parametric paths.

+ *No path induction (J eliminator).* Without abstract intervals,
  cubical's `J` eliminator can't be defined; you can use specific paths
  but can't quantify over them.

+ *No univalence-as-theorem.* `ua_path iso` works because we built it
  to apply the iso; but there's no internal theorem that *every*
  equivalence yields a path. The connection is by library construction,
  not by kernel computation.

+ *No higher paths.* Paths between paths can be iterated
  (`Path (Path A x y) p q`) but have no computational content beyond
  Option B's first-order transport.

= What's missing for full cubical

== Adding interval variables

Cubical's `I` is a type with two distinguished inhabitants (`i0`, `i1`),
plus abstract interval variables that can be bound. Path is
`(i : I) -> A` with boundary conditions `p i0 ≡ x ∧ p i1 ≡ y`.

#note[
  *What's actually new isn't `i0`/`i1` — it's the abstract variable.*
  Option B already handles "paths between specific types A and B"
  without an interval. The journey encodes the endpoints implicitly.

  What Option B *can't* express is a binder over a path's interval
  position: `\\i. some-expression-using-i`. This requires `I` to be a
  type whose values include hypothesis-like abstract variables. The
  constants `i0`/`i1` are then specific values that can substitute
  for those variables and reduce.

  So the kernel addition is "a type with both opaque (hypothesis-like)
  and substitutable (specific-value) inhabitants" — a new kind of
  binder.
]

What's needed beyond Option B:

+ *Interval primitive.* A new kernel-recognized type `I` with:
  - Two distinguished constants: `i0`, `i1`.
  - A binding mechanism for abstract interval variables, analogous to
    `bind_hyp` but typed at `I`.

+ *Boundary check as a definitional invariant.* When type-checking a
  path proof `p : (i : I) -> A` against `Path A x y`, the kernel must
  verify `p i0 ≡ x ∧ p i1 ≡ y` by substitution and structural
  comparison. This is a new kind of kernel invariant beyond what
  `predicate_frame` currently checks.

+ *Path-as-function unification.* `Path A x y` becomes the type
  `(i : I) -> A` with boundary `p i0 = x ∧ p i1 = y`. Option B's
  `Path A B` becomes a refinement that pins endpoints.

+ *Connection operations.* `i ∧ j`, `i ∨ j`, `~ i` — the De Morgan
  operations on intervals, used in transport-of-Pi and Glue formulas.

*Kernel additions:* one new type primitive (`I`) with abstract binder
plus boundary-check semantics. Bounded but architecturally distinct
from existing primitives.

== Adding Glue (with `hcomp`)

`Glue B [φ ↦ (T, e)]` is the type that is `B` everywhere except where
`φ : I` holds, where it equals `T` with equivalence `e : T ≃ B`. It's
what makes univalence reduce computationally — `transp (ua e) x`
reduces because `ua e` is internally a Glue type whose transport is
defined to apply `e`.

#note[
  *Glue and `hcomp` come together.* Glue's transport rule is *defined*
  using `hcomp` (homogeneous composition) — there's no way to add Glue
  without also adding `hcomp`. So practically, Stage 3 and Stage 4
  are a single conceptual addition split only by what they enable:
  Glue gives univalence-as-computation; `hcomp` enables higher
  composition. You can't have one without the other.

  This is a correction to the staging — they should be considered
  together.
]

What's needed:

+ *`hcomp` primitive.* Homogeneous composition: takes a partial path
  filling and a base, produces a composed path. Required by Glue's
  transport rule.

+ *Glue type primitive.* A new kernel-recognized type former with its
  own predicate and per-type metadata.

+ *Glue's transport rule.* The most complex of the cubical transport
  rules:
  ```
  transp (λi. Glue B_i [φ_i ↦ (T_i, e_i)]) ψ x  =  ...
  ```
  Case analysis on whether `φ` holds at the path's endpoints, applying
  the equivalence where appropriate, plus `hcomp` to fill in.

+ *Univalence as a definitional theorem.* With Glue, `ua : (A ≃ B) ->
  Path Type A B` is defined as
  `\\i -> Glue B [i = i0 ↦ (A, e), i = i1 ↦ (B, id_equiv)]`.
  Univalence reduces computationally.

#note[
  *Practical implication for Disp.* Option B's `iso_journey` already
  gives transport-along-an-iso for representation-independence use
  cases. The thing Glue *adds* is "every equivalence (not just iso)
  yields a path in the type universe, and transport reduces" — a
  theoretical strengthening more than a practical one.

  For Disp's stated goals (representation independence, optimizer
  rewrites, synthesizer search-space collapse), Option B's iso paths
  appear to cover the practical use cases. Glue's incremental value
  is mostly for HoTT-style formal mathematics, not for the
  applications described in `GOALS.md`.
]

*Kernel additions:* two new primitives (Glue and `hcomp`), plus the
transport rule and `ua` derivation. The most invasive piece.

== Higher paths — emerges with Stage 3

Paths between paths: `Path (Path A x y) p q`. Cubical handles these
uniformly via the same path-as-function-from-`I` machinery once `I`
and `hcomp` are in place.

What's needed:

+ *Iterated path types.* With the interval, `Path (Path A x y) p q`
  becomes `(i : I) -> (j : I) -> A` with appropriate boundary
  conditions. No new primitive needed; just iteration of Stage 2's
  Path machinery.

+ *Higher transport rules.* Per-type-former transport rules for
  iterated paths. These extend Option B's `transp_fn` to handle nested
  path types — library extensions, not kernel changes.

+ *`hcomp` is already present* from Stage 3 (Glue requires it).

For most practical programming, higher paths are not needed (most
types are h-sets). Once Stage 3 is in place, higher paths are
essentially free — iteration of existing machinery plus per-type
library rules.

*Kernel additions beyond Stage 3:* none (the `hcomp` primitive is
already present). Higher path usage is library-level extension.

= Staged implementation plan

A possible incremental path:

== Stage 0 — no changes (current Disp)

Isos are library records. Conversion is explicit. Iso roundtrip is
propositional via weak Pi.

== Stage 1 — Option B core

Add `path_apply` kernel handler. Add `transp_fn` metadata slot.
Implement per-type-former rules for common types (Bool, Nat, Pi, Sigma,
List, Eq). Library `Path` type with constructors (`refl`, `ua`,
`compose`, `inv`) and transport operations.

*Capability:* structural transport via library-defined paths. Iso-
roundtrip is definitional via stuck-collapse. Refactoring across
equivalent types is computational.

*Still missing:* abstract interval reasoning, univalence-as-computation,
higher paths.

== Stage 2 — interval and parametric paths

Add `I` type as kernel primitive. Add endpoint reduction. Generalize
Path to be `(i : I) -> A` with boundary conditions.

*Capability:* parametric reasoning about abstract paths. Path induction
(the `J` eliminator) for non-trivial paths. Functions that take paths
as arguments.

*Still missing:* univalence-as-computation, higher paths.

== Stage 3 — Glue + `hcomp` (together)

Add Glue and `hcomp` primitives (they're inseparable — Glue's transport
rule uses `hcomp`). Define `ua` via Glue. Univalence as a definitional
theorem.

*Capability:* equivalences ARE paths, definitionally. Univalent
categories. Full cubical-style reasoning about type equivalence. Higher
paths become usable as a side effect (iteration of Path + `hcomp`).

*Practical implication:* Stage 1's iso paths already cover
representation-independence; Stage 3 mostly adds theoretical power
(univalence as theorem, synthetic homotopy). Significant kernel
investment for primarily theoretical gain.

= Trade-offs

#table(
  columns: 4,
  stroke: 0.4pt + gray,
  align: left,
  inset: 6pt,
  [*Stage*], [*Kernel additions*], [*Capability gained*], [*Use cases*],
  [0], [None], [None], [Manual iso threading],
  [1], [`path_apply` + `transp_fn` slot], [Structural transport, def. iso roundtrip], [Representation independence, optimizer rewrites, synthesizer search-space collapse],
  [2], [`I` primitive with abstract binder, boundary check], [Abstract paths, J eliminator, parametric reasoning], [Path induction proofs, formal reasoning about paths],
  [3], [Glue + `hcomp` (together)], [Univalence-as-computation, higher paths], [HoTT-style proofs, synthetic homotopy theory],
)

== Disp-specific observations

The unified design's commitment to "minimal kernel, library type-
formers" is preserved through Stage 1. The metadata-driven dispatch
pattern generalizes naturally; `path_apply` is structurally identical
to `hyp_reduce`.

Stages 2-4 introduce kernel primitives that are *not* extensions of the
existing pattern. The interval requires substitution rules; Glue
requires partial-element handling; `hcomp` requires composition.
These are genuine architectural additions, not extensions of metadata-
driven dispatch.

For Disp's stated goals (neural-guided synthesis, self-improving
optimizer), Stage 1 likely covers the practical wins. Stages 2-4 are
research-grade features whose value depends on specific use cases
(synthetic homotopy, formal category theory).

= Summary

Option B (`path_apply` parallel to `hyp_reduce`) is a clean,
architecturally consistent extension of Disp's existing kernel pattern.
It delivers cubical's structural transport without disrupting the
unified design's existing primitives.

Full cubical requires two additional pieces beyond Option B:

- *Interval primitive* (Stage 2) — for abstract paths and path induction.
- *Glue + `hcomp`* (Stage 3, joint) — for univalence-as-computation;
  higher paths emerge automatically as iteration of existing machinery
  plus library extensions.

Each is a genuine kernel addition rather than an extension of existing
patterns. A staged implementation can ship after any stage with usable
capabilities:

- Stage 1 (Option B): moderate work, big practical win. *Likely
  sufficient for Disp's stated goals.*
- Stage 2 (interval): substantial work, theoretical value, modest
  practical value for most users.
- Stage 3 (Glue + `hcomp`): substantial work, big theoretical value
  (univalence + higher paths), modest direct practical value beyond
  Stage 1.

The architectural shape Disp already has (metadata-driven dispatch via
`hyp_reduce` + `codomain_fn`) makes Option B unusually clean. Adding it
doesn't change the kernel's character. Beyond Option B, full cubical
requires kernel growth that, while feasible, fundamentally extends the
design.

Whether to go past Stage 1 is a goals-driven question: does Disp need
the additional cubical features, or do Stage 1's capabilities suffice
for the synthesis/optimizer/self-hosting trajectory described in
`GOALS.md`? The architecture supports either choice.
