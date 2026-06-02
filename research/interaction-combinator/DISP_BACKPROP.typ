// DISP_BACKPROP: Synthesis-as-Reverse-Mode for Tree Calculus
// A speculative bridge between disp, TC-Net, and NeoGen/Neo

#import "@preview/fletcher:0.5.8": diagram, node, edge

#set document(
  title: "DISP_BACKPROP — Synthesis as Reverse-Mode Type Evaluation",
  author: "Research Notes",
  date: datetime.today(),
)

#set text(font: "New Computer Modern", size: 11pt)
#set page(margin: 2.4cm, numbering: "1")
#set par(justify: true, leading: 0.65em)
#set heading(numbering: "1.")
#set math.equation(numbering: "(1)")

#show heading.where(level: 1): it => {
  v(1.2em)
  text(size: 14pt, weight: "bold", it)
  v(0.6em)
}
#show heading.where(level: 2): it => {
  v(0.8em)
  text(size: 12pt, weight: "bold", it)
  v(0.4em)
}
#show heading.where(level: 3): it => {
  v(0.6em)
  text(size: 11pt, weight: "bold", style: "italic", it)
  v(0.3em)
}

#let tri = $triangle.stroked.t$
#let sup = $Sigma$
#let mark(body) = text(fill: eastern, weight: "bold", body)

// ═══════════════════════════════════════════════════════════════
// Title
// ═══════════════════════════════════════════════════════════════

#align(center)[
  #text(size: 19pt, weight: "bold")[DISP_BACKPROP]
  #v(0.3em)
  #text(size: 13pt, style: "italic")[Synthesis as Reverse-Mode Type Evaluation]
  #v(0.4em)
  #text(size: 11pt, style: "italic")[A bridge between disp, TC-Net, NeoGen, and conflict-driven learning]
  #v(0.6em)
  #text(size: 10.5pt)[Research Notes --- #datetime.today().display("[month repr:long] [day], [year]")]
]

#v(0.8em)

#block(
  width: 100%,
  inset: 12pt,
  stroke: 0.5pt + luma(120),
  radius: 4pt,
)[
  *Abstract.* We argue that gradient-based learning, CDCL SAT solving, Dillig's
  Neo synthesizer, and Taelin's NeoGen are four interpretations of the same
  underlying object: a *forward composition DAG* paired with a *backward
  propagation functor*, where both are parameterised by the choice of semiring
  for "feedback". We then ask whether disp --- which currently has only the
  forward direction (predicate-as-type, codomain_fn as LCCC eval) --- can
  acquire the backward direction by combining TC-Net's substrate with an
  HVM-style superposition agent and a Neo-style learned-clause mechanism.
  Most of the architecture lines up cleanly with disp's `CATEGORY_THEORY_FOUNDATIONS`
  proposal (where codomain_fn is identified with the LCCC eval morphism); the
  rest sits behind a small number of well-defined risks, which we enumerate.
  The result is a concrete, partially-rigorous proposal for hole-filling and
  type-directed program search inside disp, plus a precise statement of where
  the design might fail.
]

// ═══════════════════════════════════════════════════════════════
= Introduction
// ═══════════════════════════════════════════════════════════════

== Four paradigms, one shape

Gradient-based learning, CDCL SAT solving, Neo's CDCL program synthesis,
and Taelin's NeoGen all operate on a composition DAG of primitive
operations. They differ in three places: what carrier the DAG transports
("activations," partial assignments, partial programs, superposed terms);
what counts as a "loss" at the leaves (scalar error, UNSAT, spec violation,
test mismatch); and what backward signal is computed from that loss
(gradient, conflict clause, conflict clause, collapse). The substrate is
the same; the semiring differs.

#figure(
  table(
    columns: 5,
    align: (left, left, left, left, left),
    stroke: 0.5pt,
    inset: 7pt,
    table.header(
      [*System*], [*Forward DAG*], [*Carrier $T$*], [*Leaf loss*], [*Backward signal*]
    ),
    [Autodiff (backprop)],
      [Neural network composition],
      [$RR$ (activations)],
      [Scalar loss $L$],
      [$partial L slash partial theta$ via chain rule],
    [CDCL SAT (z3, MiniSat)],
      [Implication graph],
      [${"TT", "FF"}$ (truth)],
      [Empty clause (UNSAT)],
      [Conflict clause via resolution on the cut],
    [Neo / CDCL synthesis],
      [Partial-program AST],
      [Abstract values (deduction)],
      [Spec violation],
      [Equivalence-class conflict clause],
    [NeoGen / HVM-superposition],
      [Interaction-net reduction],
      [Sets of candidate terms (via SUP)],
      [Test mismatch on a $sup$-branch],
      [Collapse: branches survive],
  ),
  caption: [The four-paradigm landscape. They share a structure --- forward DAG with a backward functor --- and differ in the choice of carrier and feedback semiring.],
)

== What disp has, and what disp doesn't

Reading `KERNEL_DESIGN.md`, `TYPE_THEORY.typ`, and `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ`
together, disp is already half-way there: it has a *forward* propagation
mechanism for *types* along application spines, in the form of `codomain_fn`
dispatch on hypotheses (`lib/kernel/handlers.disp:121--147`). The category-theory
proposal makes the identification explicit: `codomain_fn` is the eval morphism
of an LCCC for $Pi$-like types, and the subobject-classifier membership
relation $in$ for predicate-like types.

What disp *lacks*, by its own admission (`COMPILATION.typ:252`,
`src/compile.ts:294`), is the *backward* direction: hole-filling, unification,
metavariable solving, type-directed synthesis. The syntax already reserves
`_` for holes; the elaborator throws. This is exactly the slot where a
backward functor on the forward DAG would land.

This document explores filling that slot.

== Roadmap

Section 2 develops the unifying abstraction: forward composition DAGs
parameterised by a semiring for feedback. Section 3 recapitulates disp's
forward stack with precise pointers into the kernel. Section 4 examines
TC-Net (this folder's tree-calculus interaction-net system) and asks what
extensions are needed to support superposition. Section 5 proposes a
concrete synthesis loop. Section 6 names the failure modes honestly.
Section 7 sketches the smallest experiment that would falsify the proposal.

// ═══════════════════════════════════════════════════════════════
= The unifying abstraction
// ═══════════════════════════════════════════════════════════════

== Composition DAGs

Fix a category $cal(C)$ of "operations". A *forward composition DAG* is a
finite acyclic morphism-labelled graph in $cal(C)$ with a designated set of
input wires and a designated set of output wires. Concretely:

#block(inset: (left: 1.5em))[
  - Each *node* is a morphism $f : A_1 times.o dots.c times.o A_n -> B_1 times.o dots.c times.o B_m$.
  - Each *wire* connects an output port of one node to an input port of another,
    or to a free input/output of the whole DAG.
  - The DAG is acyclic and well-typed: connected ports carry the same object.
]

This is just an open string diagram in a symmetric monoidal category. It is
also exactly what a neural network is (in the category of differentiable maps),
what a CNF formula's implication graph is (in the category of Boolean implications),
and what a partial program's AST is (in the category of typed components).
A tree-calculus reduction trace --- the substrate of disp, HVM, and TC-Net ---
is the same picture with $cal(C)$ specialised to interaction-net morphisms.

== Semirings as interpretation

The forward DAG is a syntactic object. To compute with it, we have to interpret
it: equip $cal(C)$ with a functor into a category of "values," and pick a
semiring $(T, plus.o, times.o)$ on those values for how to combine them
along the DAG.

#figure(
  table(
    columns: 5,
    align: (left, left, left, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Semiring*], [$bold(T)$], [$bold(plus.o)$], [$bold(times.o)$], [*Recovers*]),
    [Boolean], [${"TT", "FF"}$], [$or$], [$and$], [SAT / type-check],
    [Real], [$RR$], [$+$], [$dot$], [Autodiff],
    [Tropical], [$RR_(>= 0) union {oo}$], [$min$], [$+$], [Shortest path / Viterbi],
    [Set / superposition], [$cal(P)(V)$], [$union$], [$times$], [Nondet / NeoGen],
    [Provenance], [polynomials over decisions], [$+$], [$dot$ on decision sets], [CDCL conflict analysis],
    [Probability], [$[0, 1]$], [$+$], [$dot$], [Belief / soft logic],
  ),
  caption: [Semirings on the forward DAG. The same DAG yields type-check, autodiff, search, blame, or belief depending on which semiring interprets the wires.],
)

The forward computation is the *change-of-base functor* $cal(C) -> cal(C)_T$
that replaces each morphism with its $T$-valued action. The forward pass
through the DAG is then the iterated composition of these actions.

== The chain rule as a backward functor

For each semiring $T$ that admits a notion of "differentiation" or "inverse",
there is a *backward functor*

$ partial : "DAG"_T -> "DAG"_T^"op" $

that, given a $T$-valued loss at the output, propagates a $T$-valued cotangent
along each wire to each input. The familiar instances:

#block(inset: (left: 1.5em))[
  - *Real semiring.* $partial$ is reverse-mode autodiff. Each node $f$ contributes
    its Jacobian $partial f slash partial x$; the chain rule composes Jacobians by
    matrix multiplication.
  - *Boolean semiring.* $partial$ is *resolution along the implication graph*
    (1UIP cut in modern SAT solvers). A failing leaf yields a conflict clause:
    a disjunction of negated decisions along the cut.
  - *Provenance semiring.* $partial$ is *blame attribution*. Each failing leaf
    yields a polynomial naming the decisions whose product caused the failure.
  - *Set semiring.* $partial$ is *collapse*. A failing $sup$-branch is removed;
    the others survive. There is no explicit cotangent --- the structure of the
    SUP carries the information.
]

The semiring $T$ determines what kind of feedback is available; the backward
functor is fixed by $T$ once chosen.

#figure(
  block(
    width: 100%,
    inset: 12pt,
    stroke: 0.4pt + luma(120),
    radius: 4pt,
  )[
    *Thesis of this document.* The chain rule, conflict-clause derivation,
    and NeoGen-style collapse are *the same operation* on the same forward
    DAG, interpreted in three different semirings. A type system that can
    talk about its own predicates can host all three simultaneously, with
    the kernel as the universal forward pass and the choice of semiring left
    to the user / scheduler / solver.
  ]
)

== Free, polynomial, and enriched categories

The cleanest categorical setting is *enrichment*. The forward DAG is a 1-cell
in $V"-"cal(C)"at"$, the 2-category of $V$-enriched categories, for $V$ a
chosen semiring (regarded as a one-object monoidal category). Backward
propagation lifts to a *fibration* over the forward composition: each forward
wire carries a "cotangent fibre" whose total space is the backward propagation.

This generalises:

- Lawvere's metric-space enrichment: $V = ([0, oo], +, gt.eq)$, backward
  propagation is the dual cost map.
- Cockett--Cruttwell's *differential categories*: $V = $ commutative rig,
  morphisms carry derivative structure as part of their data.
- The *fibrational* semantics of dependent type theory (Jacobs, Streicher):
  types over contexts as a fibration $cal(E) -> cal(B)$.

Disp's `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL` identifies its `codomain_fn`
field with "the eval morphism of an LCCC" --- which is exactly the structure
needed for the forward direction of this fibration. The reverse direction is
*pullback along the indexing morphism*, which is also LCCC structure.
LCCC closure gives both forward and backward for free; disp just doesn't
currently expose the backward.

// ═══════════════════════════════════════════════════════════════
= Disp's forward stack
// ═══════════════════════════════════════════════════════════════

We give a precise account of what disp's kernel already implements, with
file/line citations, before describing what would need to be added.

== Hypotheses are typed wires

A *hypothesis* is a kernel-minted neutral token carrying its stored type as
metadata:

$ "Hyp" : (T : "Type") -> (i : "Id") -> "Tree" $

defined at `lib/kernel/handlers.disp:387--389`:

```
Hyp := {ty, id} -> wait kernel.hyp_reduce (make_neutral_meta ty id)
```

A hypothesis is opaque to triage (the walker rejects intensional inspection)
but carries enough metadata for the kernel to dispatch when the hypothesis
is applied. In the language of category theory, $"Hyp"(T, i)$ is a
*generic point* of $T$ --- equivalent to the universal element of the presheaf
represented by $T$.

== Forward propagation along the spine

When a hypothesis is applied to an argument, `q_hyp_reduce_fn`
(`lib/kernel/handlers.disp:121--147`) consults the stored type's `codomain_fn`:

```
let cod_fn = pair_snd (pair_snd (type_meta stored_inner))
...
dispatch_action (cod_fn ks raw meta v)
```

The codomain function returns an *Action*: either `Extend new_stored_type`
(append the new type to the hypothesis spine and continue) or `Return value`
(emit a concrete value, e.g. a Bool for predicate types). In the language of
fibrations, `Extend` is the *cartesian lift* of the indexing morphism along
the substitution, and `Return` is the *fibre evaluation* at a point.

The forward pass over an application spine $h space a_1 space a_2 space dots.c space a_n$
is then:

$ T_0 attach(arrow, t: "cod"(a_1)) T_1 attach(arrow, t: "cod"(a_2)) T_2 attach(arrow, t: "cod"(a_3)) dots.c attach(arrow, t: "cod"(a_n)) T_n $

where each $T_(k+1) = "Extend"$'s argument when `cod_fn(T_k, a_(k+1))` returns
`Extend(T_(k+1))`. This is *literally a forward pass through a composition DAG*.

== Predicate-as-type as the boundary

The boundary is `q_guard_fn` (`lib/kernel/handlers.disp:100--108`):

```
{core, v} ->
  match (q_scan_no_neutral raw v) {
    TT => match (tree_eq (core v) TT) { TT => TT; FF => FF }
    FF => FF
  }
```

Type-checking is reduced to `core v == TT`. The predicate `core` is just a
tree-calculus program; `v` is the value being checked; the result is a
Boolean. In semiring terms, this is the *Boolean interpretation* of the
forward DAG of $"core"$ applied to $v$.

The walker scan (`q_scan_no_neutral`) is the parametricity check that
restricts the category of valid predicates --- it places disp in a
*parametric topos* (`CATEGORY_THEORY_FOUNDATIONS_PROPOSAL §2.1`). For our
purposes this is a constraint on which forward DAGs are admissible, not a
change to the shape.

== The H-rule is a pullback

`q_h_rule_fn` (`lib/kernel/handlers.disp:110--113`) checks

$ "wait"(T_("query"), "meta") space tilde.equiv space "type_meta"(v) $

This is the *fibre coherence* of the cartesian lift: the type the hypothesis
"should" have after extension must agree with what the neutral's metadata
records. Categorically, this is the universal property of the pullback used
to define `Extend` in the LCCC.

== Summary: disp has the forward DAG

The picture so far:

#figure(
  table(
    columns: 3,
    align: (left, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Categorical object*], [*Disp realisation*], [*Status*]),
    [Object of contexts], [`Type`-typed tree], [Present],
    [Object of types over a context], [Guarded `predicate_frame`], [Present],
    [Pullback along subst.], [`Extend new_stored`], [Present],
    [Eval / projection], [`Return value`], [Present],
    [Subobject classifier $Omega$], [`Bool`], [Present],
    [Characteristic morphism $chi$], [Recognizer field], [Present],
    [Generic point of $T$], [`Hyp(T, id)`], [Present],
    [Forward composition], [`q_hyp_reduce_fn` dispatch loop], [Present],
    [Backward / cotangent], [---], [#text(fill: red)[Missing]],
    [Unification / hole-filling], [---], [#text(fill: red)[Missing]],
    [Conflict-clause learning], [---], [#text(fill: red)[Missing]],
    [Superposition of candidates], [---], [#text(fill: red)[Missing]],
  ),
  caption: [Disp's forward stack is complete. The backward stack is empty.],
)

// ═══════════════════════════════════════════════════════════════
= TC-Net plus superposition
// ═══════════════════════════════════════════════════════════════

== TC-Net as the substrate

`tc-net.typ` (this folder) develops an interaction-net system for tree
calculus with agents $L, S, F, P, E, A, T_1, T_2, delta, epsilon$ and ten
sets of producer-consumer rules. The system is rooted (every term has a
principal port facing outward) and strongly confluent (Theorem 2 of
`tc-net.typ`).

Crucially, TC-Net has no SUP agent. Duplication ($delta$) and erasure
($epsilon$) are present, but there is no merging dual --- no node that
*introduces* a superposition. This is by design: tree calculus has no
bound variables, so it doesn't need labelled duplication for binding scope.

To run NeoGen-style search inside TC-Net, we have to add a SUP agent
*for synthesis purposes*. The agent does not change the semantics of source
terms; it only appears during a search phase, and a successful search
collapses it back to an ordinary tree.

== The proposed agent: $sup_lambda$

Following HVM2 and Taelin's Interaction Calculus, we add a labelled
superposition agent:

#figure(
  table(
    columns: 5,
    align: (center, center, center, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Agent*], [*Arity*], [*Ports*], [*Role*], [*Category*]),
    [$sup_lambda$], [2], [$p, a, b$], [Labelled superposition of two trees], [Search],
  ),
  caption: [The new agent for search.],
)

The label $lambda$ identifies which $sup_lambda$ pairs with which $delta_lambda$
in the labelled commutation discipline (Mazza, 2007). Source-level TC-Net
$delta$ becomes $delta_lambda$ for some canonical $lambda_0$; SUPs introduced
by the synthesiser carry fresh labels.

== Interaction rules for $sup_lambda$ with TC-Net consumers

Every TC-Net consumer needs a rule against $sup_lambda$. The pattern is
*push the consumer through the superposition*, producing one copy of the
consumer per branch.

$
E(r) times.o sup_lambda(a, b) &:quad E(r_1) times.o a, quad E(r_2) times.o b, quad r -> sup_lambda(r_1, r_2) \
A(c, r) times.o sup_lambda(a, b) &:quad delta_lambda times.o c, quad A(c_a, r_1) times.o a, quad A(c_b, r_2) times.o b, quad r -> sup_lambda(r_1, r_2) \
T_1(b, c, r) times.o sup_lambda(a_1, a_2) &:quad delta_lambda times.o b, quad delta_lambda times.o c, dots \
T_2(w, x, b, r) times.o sup_lambda(c_1, c_2) &:quad delta_lambda times.o w, quad delta_lambda times.o x, dots \
delta_mu times.o sup_lambda(a, b) &:quad #text(style: "italic")[(annihilation if $mu = lambda$; commutation if $mu eq.not lambda$)] \
epsilon times.o sup_lambda(a, b) &:quad epsilon times.o a, quad epsilon times.o b
$

The $delta_mu times.o sup_lambda$ rule is the well-known SUP--DUP
interaction from symmetric interaction combinators:

- *Annihilation* ($mu = lambda$): the duplicator's two outputs bind directly
  to the SUP's two branches. Search labels pair up with their dedicated
  duplicators, recovering the candidate trees.
- *Commutation* ($mu eq.not lambda$): each branch is independently duplicated
  with label $mu$. Nested searches compose without interfering.

== Soundness sketch

#block(inset: (left: 1em))[
  *Conjecture 1 (SUP soundness in TC-Net).* If $tau attach(arrow.long, t: *) sup_lambda(v_1, v_2)$
  in TC-Net+SUP, where $tau$ is a source-level term (no SUPs in its
  initial encoding), then both $v_1$ and $v_2$ are reachable from $tau$ in
  pure TC-Net.
]

_Sketch._ SUPs are only introduced by the synthesiser, not by any
source-level rule. Each occurrence $sup_lambda(c_1, c_2)$ in the synthesiser's
output represents the disjunction "the hole could be filled with $c_1$ or
$c_2$". The interaction rules above commute SUPs upward through consumers
without ever mixing the two branches semantically: the rule
$E times.o sup_lambda(a, b)$ produces two independent demands $E(r_1)$
and $E(r_2)$, each operating on its own branch. By induction on the
reduction trace, every value $v_i$ obtainable from a SUP'd input is obtainable
from the corresponding non-SUP'd input.

The only place where branches *interact* is annihilation, where a synthesis
label's $delta_lambda$ meets its $sup_lambda$; that interaction is a
substitution, not a value-mixing.

#block(inset: (left: 1em))[
  *Conjecture 2 (Type-check distributivity).* If $T$ is a parametric
  predicate (i.e. a disp library type) and $v_1, v_2$ are typeable trees,
  then in TC-Net+SUP, $T(sup_lambda(v_1, v_2))
  attach(arrow.long, t: *) sup_lambda(T(v_1), T(v_2))$.
]

_Sketch._ $T$ is a tree-calculus program with no SUP agents in its source.
Every interaction $T$ has with $sup_lambda$ is one of the consumer-vs-SUP
rules above. None of those rules collapses the SUP. The reduction
distributes.

The conjectures depend on the walker's parametricity guarantee --- $T$ must
not be able to triage on a neutral, which means in particular it cannot
triage on the constructor identity of a SUP (the walker rejects this in
disp via `q_scan_no_neutral`, `lib/kernel/handlers.disp:72`). If this
guarantee holds, then type-check distributes over superposition, which is
the property we need.

== Why this isn't gratuitous

A reader could object: "you've just added HVM2 inside TC-Net." Correct.
The point is that TC-Net's `tc-net.typ §6` deliberately omits SUPs because
they aren't needed for *execution*. They become necessary for *synthesis*,
which is a different use case. The categorical content of `Sigma` is
*coproduct*, and synthesis needs coproducts at every hole. Adding labelled
SUPs is the cheapest way to get them without losing TC-Net's confluence and
without re-introducing binding-scope labels.

// ═══════════════════════════════════════════════════════════════
= The proposed synthesis loop
// ═══════════════════════════════════════════════════════════════

We now compose the pieces into a concrete algorithm, with the disp kernel
as the forward pass and a Neo-style backward analysis on top.

== Step 0 --- the input

Given:
- A goal type $T$ (a disp library type).
- A *context* $Gamma$: a list of named values with their types.
- A *partial term* $tau$ which is well-formed except for some holes
  $square_1 : tau_1, dots.c, square_k : tau_k$ with declared types $tau_i$.
  (A hole with no declared type is a metavariable; for now, assume types
  are declared. Inferring hole types is just unification, treated below.)

In disp, the partial term is represented as a tree with $"Hyp"(tau_i, i)$
substituted for hole $square_i$. The hypothesis already exists as a kernel
primitive (`lib/kernel/handlers.disp:387`); we just need to admit it from
the parser.

== Step 1 --- candidate enumeration per hole

For each hole $square_i$ with declared type $tau_i$, build a *typed candidate
stream* $C_i$. Sources of candidates, in order:

#block(inset: (left: 1em))[
  1. *In-scope variables* of type $tau_i$ (or that unify with $tau_i$).
  2. *Constructors* of $tau_i$ (looked up via $tau_i$'s `Functor` record,
     once the categorical foundations proposal is implemented).
  3. *Eliminators* whose codomain unifies with $tau_i$.
  4. *Library functions* whose return type unifies with $tau_i$ (only
     enumerated if $tau_i$ is "complex enough" --- a heuristic cutoff).
  5. *Sub-holes* of fresh type variables (for compositional synthesis).
]

The stream is lazy and ordered by an outside-supplied prior (size,
frequency in the standard library, LLM logits, etc.). The stream is
*type-correct by construction*: each candidate $c$ has been pre-checked
to satisfy $T_("decl")(c) = "TT"$ where $T_("decl")$ is its declared type.

== Step 2 --- superpose the candidates

Replace the hypothesis $"Hyp"(tau_i, i)$ in $tau$ by

$ sup_(lambda_i)^("first n")(C_i) = sup_(lambda_i)(c_(i,1), sup_(lambda_i)(c_(i,2), dots.c, sup_(lambda_i)(c_(i,n-1), c_(i,n)))) $

for some prefix of length $n$ from the candidate stream. The $lambda_i$ is
fresh per hole. The resulting tree $tau'$ contains $k$ superpositions, one
per hole, each with the same fresh label per hole and fresh labels across holes.

== Step 3 --- reduce the type predicate

Apply the goal type's recognizer:

$ r := T_("rec")(tau') $

By Conjecture 2 (type-check distributivity), this reduces to a *tree of
TT/FF values* shaped like the original SUP nesting:

$ r = sup_(lambda_1)(dots.c sup_(lambda_k)(dots.c "TT"/"FF" dots.c) dots.c) $

with one TT/FF leaf per combinatorial choice of candidates across the holes.

== Step 4 --- collapse

Walk the result tree, keeping branches where the leaf is TT. The collapsed
result is a SUP of *exactly the candidate combinations that typecheck*.
If non-empty, pick one (by prior, or interactively) and substitute back into
$tau$.

== Step 5 --- conflict analysis (the new bit)

This is where the design exceeds pure-NeoGen and reaches into Neo / CDCL
territory.

If *every* combination fails (the collapsed SUP is empty), we want to learn
*why*, so that the next round's candidate streams are smaller. Recall Neo's
mechanism (from Feng et al., PLDI 2018): *equivalence modulo conflict*.

#block(inset: (left: 1em))[
  *Definition.* Two candidates $c, c'$ for hole $square_i$ are *equivalent
  modulo conflict* in context $tau$ if substituting either of them fails
  the type-check at the same intermediate reduction position, with the same
  failure mode.
]

To detect this, we have to reify the failure: $T_("rec")(tau')$ returns FF,
but we want to know *which subterm reduction at which spine position
generated the FF*. In disp's kernel, this is recoverable because every FF
in `q_guard_fn` and `q_h_rule_fn` comes from a specific `tree_eq` failure
at a specific position in the reduction trace.

The kernel currently throws away this information --- type-check returns a
Bool. To support Step 5, we need an instrumented variant `q_guard_fn_traced`
that returns a *witness*: a tree-position together with the expected and
actual types. This witness is the *conflict graph*, and resolution on it
yields a learned predicate.

#block(inset: (left: 1em))[
  *Learned predicate (typed conflict clause).* A predicate
  $kappa : "Tree" -> "Bool"$ that returns FF on every candidate for hole
  $square_i$ that exhibits the same failure mode in this context.
]

The cheapest realisation: $kappa$ is a triage program (in tree calculus)
that pattern-matches on the head constructor of its argument and rejects
the shape that produced the failure. This is exactly Neo's "lemma," lifted
into the host language by virtue of disp's predicate-as-type design.

The next round's candidate stream for $square_i$ is filtered by $kappa$
before being superposed.

== Loop

Repeat steps 2--5 with the refined candidate streams. Terminate when:

- A non-empty collapse yields a valid term (success), or
- The candidate streams are empty (no synthesis possible at the current
  depth), or
- A depth/time budget is exhausted (give up).

If the bottom branch fires, *increase synthesis depth*: allow candidates
that introduce sub-holes (Step 1.5 above), and recurse.

== The chain rule, finally

The whole loop is one application of the chain rule in the appropriate
semiring:

#figure(
  table(
    columns: 3,
    align: (left, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Semiring used*], [*Forward pass*], [*Backward operation*]),
    [Set / coproduct], [SUP propagation through predicate], [Collapse (Step 4)],
    [Boolean], [Pointwise TT/FF on each combination], [Resolution (Step 5)],
    [Provenance], [Decision-tagged TT/FF], [Conflict clause as polynomial],
    [Real (smoothed)], [Soft scoring per branch], [Beam search / ranked retry],
  ),
  caption: [The synthesis loop, semiring by semiring.],
)

The first row is NeoGen-as-stated. The second row is Neo-as-stated. The third
row is Neo with explicit blame attribution (useful for explaining failures
to a user or LLM). The fourth row is the bridge to neural-guided synthesis:
treat candidates as a probability distribution, propagate beliefs through
the forward pass, refine via a gradient on the prior. This is, structurally,
*differentiable theorem proving* (NeuroSAT, SATNet, $partial$ILP) realised
inside disp.

// ═══════════════════════════════════════════════════════════════
= Chain rule, made disp-categorical
// ═══════════════════════════════════════════════════════════════

We now connect Section 5's algorithm to `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL`
to give a categorical account.

== Forward pass = LCCC eval

The forward pass of Section 5 Step 3 is the iterated application of the
LCCC eval morphism. For a $Pi$-typed hypothesis $h : Pi A B$ applied to
an argument $a$:

$ "eval"_(A, B) : (A -> B) times A -> B $

generalised to dependent codomain via the *cartesian morphism* of the
fibration $cal(E) -> cal(B)$:

$ "Extend"(B[a]) "is the cartesian lift of" a^* "along" B $

The forward pass through an application spine of length $n$ is the
$n$-fold iteration of this cartesian lift.

== Backward pass = right adjoint to substitution

In an LCCC with dependent products, substitution $a^* : "Type"(Gamma . A)
-> "Type"(Gamma)$ has *both* a left adjoint $Sigma_a$ and a right adjoint
$Pi_a$:

$ Sigma_a tack.r.double a^* tack.r.double Pi_a $

The cartesian lift is on the $a^* tack.r.double Pi_a$ side; the *backward pass*
of the chain rule sits on the $Sigma_a tack.r.double a^*$ side. Specifically:

#block(inset: (left: 1em))[
  Given a constraint on the output type $T_n$ ("the spine must end with type
  $C$"), the *required intermediate types* $T_(n-1), dots.c, T_0$ are obtained
  by taking $Sigma$-style pullbacks back along the cartesian lift.
]

For Pi-like types, this is unification under dependent types --- the
familiar bidirectional typing rules:

$ Gamma tack f : Pi A B quad Gamma tack a : A " infers " Gamma tack f space a : B[a] $

becomes, in checking mode:

$ Gamma tack f : Pi A B quad Gamma tack f space a arrow.double.t C " requires " Gamma tack a : "Solve"(B[?] = C) $

The "Solve" operation is the backward pass at that node. For
$lambda$-abstractions and applications, this is undecidable in general
(higher-order unification), but decidable in *Miller's pattern fragment*
and in many practical fragments used by elaborators (Agda, Coq, Lean).

== Backward pass for predicate-like types

For predicates (Bool-valued types), the categorical setup is different:
the type $T$ is a subobject of `Tree`, characterised by $chi_T : "Tree" -> Omega$.
A "value of type $T$" is an element of the subobject; the *backward pass*
is *refinement*: given that $chi_T(v) = "FF"$, find a smaller subobject
$T' subset.eq T^c$ whose negation excludes $v$.

In tree-calculus terms, $T'$ is the conflict clause $kappa$ from Section 5
Step 5. The categorical claim is:

#block(inset: (left: 1em))[
  *Claim.* The Neo-style learned-clause mechanism is the *refinement
  pullback* in the topos of parametric predicates over `Tree`, with $Omega$
  as the subobject classifier.
]

Concretely: the conflict clause $kappa$ is the *characteristic morphism* of
a sub-subobject that excludes the failed candidate's equivalence class.
Adding $kappa$ to the candidate filter is intersecting the candidate set
with the complement of this sub-subobject.

== Polynomials and the differential category

The categorical setting that unifies all this is the *differential
category* (Blute, Cockett, Seely; Cockett--Cruttwell), which axiomatises
"morphisms with derivatives" without committing to a specific semiring.
A differential category has:

- A monoidal structure for composition.
- A coalgebra structure $!: A -> !A$ (the "bang") representing
  unrestricted-use copies of values.
- A differentiation operator $partial$ taking $f : !A -> B$ to its
  derivative $partial f : !A times.o A -> B$.

In disp's case:
- The monoidal structure is tree-calculus application.
- The bang is the $delta$ duplicator (TC-Net's structural copy).
- The differentiation $partial$ would be the backward pass we're proposing
  to add.

Notably, *the existence of a coherent $partial$ requires that the bang
satisfies the comonoidal axioms* --- which TC-Net's `tc-net.typ §3.4.5`
already discusses ("$delta$ is a consumer of term producers... if an
implementation adds lower-level graph-sharing nodes where duplicators can
meet duplicators, it must prove the usual comonoid coherence laws
separately").

This is where the four-paradigm landscape lands inside differential
categories:

#figure(
  table(
    columns: 3,
    align: (left, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Differential category over*], [*$partial$ is*], [*Recovers*]),
    [Smooth manifolds], [Jacobian], [Backprop],
    [Boolean lattice], [Resolution], [CDCL SAT],
    [Provenance polynomials], [Blame attribution], [Neo],
    [Coproduct of trees], [Collapse], [NeoGen],
  ),
  caption: [Differential categories specialised by the choice of bang/coproduct.],
)

Disp's existing infrastructure (codomain_fn + $delta$ + walker parametricity)
gives the forward direction and the bang. Adding the backward pass is
*completing the differential category*.

// ═══════════════════════════════════════════════════════════════
= Where it might fail
// ═══════════════════════════════════════════════════════════════

We name the obstacles honestly. None of these is fatal in isolation, but
collectively they constitute the design risk.

== Risk 1: SUPs and dependent types

The SUP soundness conjectures (Section 4) are stated for predicates over
`Tree`. They rely on the predicate $T$ being parametric --- in particular,
on $T$ not triaging on the constructor identity of a SUP node.

If a future disp library type *does* triage on its argument's constructor
(which would be a violation of parametricity by the walker), distributivity
fails. The walker rejection of triage-on-neutrals is the load-bearing piece
here, and the SUP agent must be classified as a *neutral-like form* for the
walker (i.e., treat $sup_lambda(a, b)$ as opaque to is_fork / is_stem /
pair_fst / pair_snd inside user code).

This is the same discipline that already protects neutrals; extending it to
SUPs is mechanical, but it must be done explicitly. *If it's missed, a
malicious or buggy library type could violate distributivity and the search
will return wrong answers.*

== Risk 2: Codomain naturality

For a $Pi$-type's codomain_fn to commute with SUP, it must be *natural in
its argument*. That is, if we have $"cod_fn"("meta", sup_lambda(a, b))$, the
result should be a SUP of $"cod_fn"("meta", a)$ and $"cod_fn"("meta", b)$.

The `Functor` record in `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL §3.3` requires
identity and composition laws but does *not* yet require naturality with
respect to arbitrary categorical structure. The relevant law is:

$ "morphism_action"(f, "Sigma_obj")(x) = "Sigma_obj"("morphism_action"(f, "branches")(x)) $

This is a coherence requirement *between* the functor structure and the
coproduct structure. It's not automatic; library type-formers may violate
it.

The mitigation is to add a *naturality witness* to the `Functor` record,
or to bake naturality into the smart constructor `make_type_former`.

== Risk 3: Hash-cons explosion

Disp relies on `conv = fast_eq` being O(1) via hash-consing
(`CLAUDE.md:85`, "hash-consing is load-bearing"). SUPs are
combinatorial: $k$ holes with $n$ candidates each yield $n^k$ shapes that
the reducer might visit.

The HVM2 paper's whole point is that sharing makes this *not* an $n^k$
blowup --- but the sharing is in the reduction trace, not in the hash-cons
table. Each unique intermediate SUP value still needs a unique hash-cons
entry.

The two mitigations are:

- *Canonicalisation*: choose a canonical nesting order for SUPs (left-associated,
  candidates sorted by hash) so that equivalent superpositions hash-cons
  identically.
- *Bounded materialisation*: never materialise the full $n^k$ tree as a
  reduced form; instead, lazily collapse and walk.

The HVM2 implementation handles this; disp would need to inherit or
replicate the machinery.

== Risk 4: Conflict-clause representation

A learned predicate $kappa$ must be expressible in tree calculus. For
simple syntactic conflicts ("never use head-constructor `succ` at this
hole"), $kappa$ is a small triage program. For semantic conflicts
("never use any term whose evaluation produces a successor at depth 3"),
$kappa$ may be hard to compress.

Neo handles this by working over an abstract domain (size, monotonicity,
range). Disp would need to choose: do we accept arbitrary tree-calculus
conflict predicates (precise but possibly expensive to evaluate), or
restrict to a syntactic abstract domain (cheap but lossy)?

*A precise design choice is required.* The literature recommends starting
with a coarse abstract domain (head-constructor only) and refining as the
search reveals which kinds of conflicts dominate.

== Risk 5: Termination

The search space is unbounded. Even with conflict learning, synthesis of
arbitrary terms is undecidable. We need explicit budgets:

- A *depth* budget for sub-hole introduction.
- A *count* budget on superposition cardinality.
- A *time* budget on reduction interactions.

Disp already has `applyTree(T, v, budget)` (per `COMPILATION.typ`); the
budget plumbing is mostly present. But the search loop needs its own
budgets at each level.

The user-facing question: how do budgets compose with conflict learning?
Naively, a learned $kappa$ in a high-depth search may not transfer to
low-depth retries. The mitigation is to keep $kappa$'s context-attribution
explicit (which depth, which hole, which scope), and only apply $kappa$
when the context matches.

== Risk 6: The Conjectures are conjectures

Conjectures 1 and 2 in Section 4 are not proved. They are plausible, and
they match the published HVM2 semantics, but for TC-Net specifically they
need to be *checked* --- ideally with a small Coq or Lean proof, or at
minimum a hand-written soundness argument that surveys every rule pair.

The most likely failure mode: a corner case where a TC-Net rule (e.g. the
S-rule's specific $delta$ insertion) interacts non-trivially with SUP
labels. Symmetric IC has a published semantics that handles this; TC-Net's
$delta$ is identified with symmetric IC's $delta$, so the same theorems
should port --- but the porting hasn't been done.

== Risk 7: Universe issues for typed holes

A hole `_` annotated with a type expression that itself contains holes
creates a universe-level recursion: we need to synthesise the type before
synthesising the term. This is the standard *constraint-set* approach in
modern elaborators (Agda's metavariables, Coq's evars).

The categorical-foundations proposal's universe-self-typing recursion
(`Type : Type`, allowed because Russell paradoxes diverge rather than
contradicting, `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL §5`) means *disp can
handle this*, but the search will need to interleave term-level and
type-level holes. This is more work but not a soundness problem.

== Summary of risks

#figure(
  table(
    columns: 4,
    align: (left, left, left, left),
    stroke: 0.4pt,
    inset: 7pt,
    table.header([*Risk*], [*Severity*], [*Mitigation*], [*Difficulty*]),
    [SUPs + parametricity], [Soundness], [Walker treats SUP as neutral-like], [Low],
    [Codomain naturality], [Correctness], [Add naturality law to `Functor`], [Medium],
    [Hash-cons explosion], [Performance], [Canonicalisation + lazy collapse], [Medium-high],
    [Conflict clause repr.], [Design], [Choose abstract domain], [Medium],
    [Termination], [Practical], [Explicit budgets at each level], [Low-medium],
    [Conjectures unproved], [Foundations], [Port symmetric IC proofs], [Medium],
    [Universe interleaving], [Engineering], [Standard metavar discipline], [Medium],
  ),
  caption: [Risk inventory.],
)

No "Severity = Fatal" entries. The aggregate is significant engineering
work, but no single risk seems to defeat the design.

// ═══════════════════════════════════════════════════════════════
= Minimal viable prototype
// ═══════════════════════════════════════════════════════════════

The smallest experiment that would falsify the proposal:

== Step A --- holes as hypotheses

In `src/compile.ts:294`, replace the `throw new Error("hole '_' cannot
appear in untyped compilation")` with a call to mint a fresh hypothesis at
a declared type. Plumb the hypothesis through the elaborator so partial
terms can be checked. Estimated effort: 50--100 lines TypeScript, plus
matching kernel hookups.

This alone validates Risk 7's plumbing and lets us run partial-term
type-checking. *If this doesn't work, the rest of the proposal is moot.*

== Step B --- a single-hole, type-directed enumerator

Implement a TypeScript function `synthesizeHole(goal: Tree, context: Scope):
Tree | null` that:

1. Enumerates candidates from `context` (in-scope vars) whose declared
   type fast-equals `goal`.
2. For each candidate, runs the kernel's type-check.
3. Returns the first one that passes, or null.

No SUPs, no conflict learning. This is just bidirectional checking with
fall-through. Estimated effort: 100 lines.

*If this returns plausible terms for simple goals (e.g., synthesising
`succ zero : Nat` from a context containing both), the forward pass works
correctly under hypothesis-substitution, which is the main precondition for
everything else.*

== Step C --- naive SUP enumeration

Replace Step B's "try each in turn" with: build a single $sup_lambda$ over
all candidates, reduce the type-check, collapse. This requires either:

- (a) Implementing $sup_lambda$ at the TS tree level (a new tag for the
  tree representation, plus consumer-vs-SUP rules in `applyTree`), or
- (b) Encoding SUPs purely in tree calculus using a labelled-pair convention
  (slower but no host changes).

Option (a) is faster; option (b) keeps disp's "object language is the
specification" discipline (`DEVELOPMENT_PHILOSOPHY.md`). Recommended: (b)
first, then (a) as an optimisation.

*If type-check distributes over SUP and collapse yields the same candidates
as Step B, Conjecture 2 holds for this case.* Run on enough examples to
build confidence.

== Step D --- conflict learning (simple form)

When all candidates in a SUP fail, instrument the kernel to return the
*head constructor* of the failing subterm. Filter the next round's
candidates to exclude that head constructor. Iterate.

No deduction engine, no abstract interpretation --- just the simplest possible
learned-clause filter. Estimated effort: 100 lines TS + a small kernel
patch.

*If this measurably reduces the candidate set on repeated runs, even with
the trivial $kappa$, the whole pipeline is structurally correct and the
remaining work is purely engineering.*

== Step E --- the bend

Replace the TS-side synthesiser with a disp-language synthesiser, written
in disp's library. This closes the self-hosting circle: the synthesiser
becomes a first-class disp program operating on the same tree-calculus
terms it manipulates.

This is the "NeoGen written in Bend" move (per
https://x.com/VictorTaelin/status/1945497309573251320), adapted to disp.
Estimated effort: substantial; ~1000 lines of disp library code.

*If all of A--D work, E is engineering, not research.*

// ═══════════════════════════════════════════════════════════════
= Open questions
// ═══════════════════════════════════════════════════════════════

1. *Pi-fragment unification.* The "Solve" operation in Section 6.2 is
   undecidable in general. Which fragment does disp need? Miller-pattern is
   standard; for predicate-typed holes, simpler fragments may suffice. This
   is a design choice with significant implications.

2. *Cross-hole conflict propagation.* If hole $square_1$'s filling
   constrains hole $square_2$'s candidates, the conflict graph spans both.
   Neo handles this via SMT; can it be done with tree-calculus predicates
   alone, or do we need an external SMT solver?

3. *LLM integration.* The "probability semiring" entry in Section 2.2
   maps directly to soft prior over candidates. How does disp's type-system
   interface with neural priors? Concretely: can an LLM emit candidates
   *with confidence weights* that the search uses as the prior on the SUP?

4. *Relation to neural-guided synthesis.* DeepCoder, RobustFill, and the
   later neuro-symbolic synthesis literature (e.g., FrAngel, BUSTLE)
   integrate neural priors into enumerative search. The proposal here
   reaches similar territory but via a different vocabulary (semirings on
   composition DAGs). It would be worthwhile to draw out the precise
   correspondence.

5. *Cubical interaction.* The `CUBICAL_PROPOSAL.typ` extends disp's
   `codomain_fn` slot (renamed `morphism_action` in the categorical
   foundations) to handle cubical composition. The backward pass under
   cubical structure is *higher-dimensional unification* --- equality
   reasoning along paths. Open: does the proposed synthesis loop generalise
   to cubical types, or does Glue's transport machinery require a fully
   different backward direction?

6. *Differential categories and parametricity.* The walker's parametricity
   constraint restricts disp's category to *parametric* morphisms. Is this
   compatible with the differential-category axioms? Specifically: does
   parametricity force the bang $delta$ to satisfy the comonoidal laws
   that differential categories require? A clean answer to this would
   ground disp's setting in established categorical foundations.

7. *Provenance soundness.* The provenance semiring (decision polynomials)
   gives clean blame attribution, but only if the kernel preserves
   provenance through all reductions. Disp's hash-consing collapses
   equal-by-evaluation terms; does this lose provenance? If so, the
   blame attribution may need a parallel non-hash-consed evaluation
   track.

// ═══════════════════════════════════════════════════════════════
= Conclusion
// ═══════════════════════════════════════════════════════════════

The thesis: gradient descent, CDCL, Neo's synthesis, and NeoGen's
program-mining are four readings of the same underlying machinery ---
a forward composition DAG paired with a backward functor, parameterised
by a semiring of feedback. Disp has the forward direction in finished form
(codomain_fn as LCCC eval, predicate-as-type as the loss surface, walker
parametricity as the admissibility constraint on morphisms); disp lacks
the backward direction.

Adding the backward direction requires three things:
1. Labelled superposition agents in TC-Net to host coproduct-shaped search.
2. Instrumented type-checking that reifies failure provenance.
3. A learned-clause mechanism for conflict-driven pruning, expressed as
   tree-calculus predicates.

All three fit within disp's existing kernel discipline and the
`CATEGORY_THEORY_FOUNDATIONS_PROPOSAL` vocabulary. None requires growing
the seven-primitive kernel. The risks are real but bounded; the prototype
plan in Section 7 falsifies the design at each layer before committing
substantial work to the next.

The unique advantage disp would have over NeoGen-as-stated is that
*the type-checker is the fitness function* --- no separate I/O examples,
no separate spec language, no separate test harness. Predicates are first-class,
composable, and already what the kernel knows how to evaluate. The proposed
loop just runs them with `Sigma`-valued arguments and uses the resulting
$Sigma$-valued result to refine the search.

The unique advantage disp would have over Neo is that *the conflict clauses
are first-class disp predicates* --- they participate in the same type
system they constrain. A learned clause about a hole's failure is itself
a piece of disp code, and can be checked, optimised, transported, or
exposed to the user with no representation impedance.

The unique advantage disp would have over backprop is honesty about the
discrete structure: the search returns *programs*, not approximations,
and the semiring is chosen to fit the problem rather than fixed at
$RR$.

The unique disadvantage versus all three is that none of this is
implemented yet. The next move is Section 7 Step A.

// ═══════════════════════════════════════════════════════════════
// References
// ═══════════════════════════════════════════════════════════════

#heading(numbering: none)[References]

#set text(size: 9.5pt)
#set par(hanging-indent: 1.5em)

Yu Feng, Ruben Martins, Osbert Bastani, Isil Dillig. _Program Synthesis using
Conflict-Driven Learning._ PLDI 2018. (Neo) https://fredfeng.github.io/papers/pldi18-neo.pdf

Victor Taelin. _Fast Discrete Program Search with HVM Superpositions (SUP nodes)._
Gist, 2024. https://gist.github.com/VictorTaelin/d5c318348aaee7033eb3d18b0b0ace34

Victor Taelin. _Accelerating Discrete Program Search with SUP Nodes._
Gist, 2024. https://gist.github.com/VictorTaelin/7fe49a99ebca42e5721aa1a3bb32e278

Victor Taelin. _HVM2: A Parallel Evaluator for Interaction Combinators._
HigherOrderCO, 2024. https://github.com/HigherOrderCO/HVM2

Victor Taelin. NeoGen / Bend2 announcements (X.com), 2025.
https://x.com/VictorTaelin/status/1957775213053022614

Yves Lafont. _Interaction Combinators._ Information and Computation 137(1),
1997, pp. 69--101.

Damiano Mazza. _A Denotational Semantics for the Symmetric Interaction
Combinators._ MSCS, 2007.

Bart Jacobs. _Categorical Logic and Type Theory._ Studies in Logic and the
Foundations of Mathematics 141, North-Holland, 1999.

Richard Blute, Robin Cockett, Robert Seely. _Differential Categories._
MSCS 16(6), 2006, pp. 1049--1083.

J. Robin B. Cockett, Geoff Cruttwell. _Differential structure, tangent
structure, and SDG._ Applied Categorical Structures 22, 2014.

F. William Lawvere. _Metric spaces, generalized logic, and closed
categories._ Rend. Sem. Mat. Fis. Milano 43, 1973.

Yves Lafont. _From Proof-Nets to Interaction Nets._ Advances in Linear
Logic, 1995.

Disp project. _Type theory, kernel design, and category-theoretic
foundations._ Internal docs: `TYPE_THEORY.typ`, `KERNEL_DESIGN.md`,
`CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ`, `CUBICAL_PROPOSAL.typ`,
`COMPILATION.typ`. (https://github.com/zyansheep/disp or local repo)

TC-Net (this folder). `tc-net.typ`.
