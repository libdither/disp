// TC-Net: Tree Calculus on Interaction Nets
// Self-Reflecting Programs with Explicit Parallel Sharing

#import "@preview/fletcher:0.5.8": diagram, node, edge

#set document(
  title: "TC-Net: Tree Calculus on Interaction Nets",
  author: "Research Notes",
  date: datetime.today(),
)

#set text(font: "New Computer Modern", size: 11pt)
#set page(margin: 2.5cm, numbering: "1")
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

// Shorthands
#let tri = $triangle.stroked.t$
#let mark(body) = text(fill: eastern, weight: "bold", body)

// ═══════════════════════════════════════════════════════════════
// Title
// ═══════════════════════════════════════════════════════════════

#align(center)[
  #text(size: 20pt, weight: "bold")[TC-Net: Tree Calculus on Interaction Nets]
  #v(0.4em)
  #text(size: 14pt, style: "italic")[Self-Reflecting Programs with Explicit Parallel Sharing]
  #v(0.8em)
  #text(size: 11pt)[Research Notes --- #datetime.today().display("[month repr:long] [day], [year]")]
]

#v(1em)

#block(
  width: 100%,
  inset: 12pt,
  stroke: 0.5pt + luma(120),
  radius: 4pt,
)[
  *Abstract.* We construct TC-Net, an interaction net system for evaluating Barry Jay's tree calculus inside Yves Lafont's interaction net framework. The revised construction treats every tree-calculus expression as a principal-rooted term and treats evaluation, application, triage, duplication, and erasure as consumers of that root. This removes the earlier orientation problem where application results were exposed only through auxiliary ports. The key insight remains that tree calculus's triage operation is case analysis on a fixed three-way recursive type, while sharing is structural copy/delete for that same first-order term algebra. The system therefore avoids Lamping-style binding-scope bookkeeping, not because duplication is free, but because there are no binders whose scope identities must be tracked. A second duplicator species --- demand-before-copy --- upgrades structural copying to call-by-need; because there are no binders, this alone yields at-most-once reduction of every application, the tree-calculus analog of optimal sharing, again with no oracle.
]

// ═══════════════════════════════════════════════════════════════
= Introduction
// ═══════════════════════════════════════════════════════════════

Two fundamental models of computation have emerged from different research traditions, each excelling where the other is silent:

- *Interaction combinators* (Lafont, 1997), rooted in linear logic proof nets, provide optimal sharing of subcomputations and inherent parallelism. They underlie HVM/Bend, achieving massive GPU-parallel functional evaluation. But they are _extensional_: programs are black boxes defined by input-output behavior, with no capacity for structural self-inspection.

- *Tree calculus* (Jay, ~2009--2021), rooted in intensional computation and pattern calculus, provides intrinsic self-reflection. Programs can inspect, compare, and transform other programs (including themselves) without external encoding. But it has no machinery for optimal sharing or parallel evaluation.

These strengths are _complementary, not competing_. Both systems make something implicit in lambda calculus explicit:

#figure(
  table(
    columns: 3,
    align: (left, center, center),
    stroke: 0.5pt,
    inset: 8pt,
    table.header([], [*Interaction Combinators*], [*Tree Calculus*]),
    [Makes explicit], [Sharing (via $delta$/$epsilon$)], [Structure (via triage)],
    [Character], [Extensional], [Intensional],
    [Key operation], [Duplication / erasure], [Case dispatch on constructors],
    [Origin], [Linear logic (Lafont)], [Pattern calculus (Jay)],
    [Parallelism], [Intrinsic], [Not a design focus],
    [Self-reflection], [Not possible], [Native],
  ),
  caption: [Complementary strengths of the two formalisms.],
)

Lambda calculus hides both sharing and structure. TC-Net makes both explicit simultaneously. The reason the two explicitations compose without friction is a single property: *duplication preserves constructor identity*. When you copy a fork, you get a fork. Self-reflection (which inspects constructors) survives sharing (which copies them).

// ═══════════════════════════════════════════════════════════════
= Preliminaries
// ═══════════════════════════════════════════════════════════════

== Tree Calculus

Tree calculus has a single primitive symbol $tri$ and binary application. The abstract syntax is:

$ E ::= tri | E space E $

Application is left-associative: $tri (tri tri) tri tri$ means $((tri (tri tri)) tri) tri$. There are three value forms:

#block(inset: (left: 2em))[
  - *Leaf:* $tri$ (the bare symbol)
  - *Stem:* $tri a$ (one application)
  - *Fork:* $tri a b$ (two applications)
]

Reduction fires only when a fork receives a third argument. The _triage formulation_ (Bader's reformulation of Jay's original rules) has five reduction rules:

$
  tri tri y z &arrow.long y #h(11em) & "(K)" \
  tri (tri x) y z &arrow.long x z (y z) #h(6.3em) & "(S)" \
  tri (tri w x) y tri &arrow.long w & "(triage: leaf)" \
  tri (tri w x) y (tri u) &arrow.long x u & "(triage: stem)" \
  tri (tri w x) y (tri u v) &arrow.long y u v & "(triage: fork)"
$

Rules K and S provide Turing completeness (they encode the SK combinators). The triage rules provide _intensionality_: a program can branch on whether its argument is a leaf, stem, or fork --- making the internal structure of _any_ value (including other programs) available for inspection.

The *triage combinator* is defined as $"triage"{l, s, f} = tri (tri l s) f$, which when applied to a tree $t$ returns $l$ if $t$ is a leaf, $s u$ if $t = tri u$ is a stem, or $f u v$ if $t = tri u v$ is a fork.

=== Self-Reflection

Because programs _are_ trees and triage inspects tree structure, programs can analyze other programs (or themselves) without any encoding:

- A *size* program computes the node count of any tree --- including `size(size)`.
- An *equality* program decides structural equality --- including `equal?(equal?, equal?)`.
- A *self-evaluator* evaluates any program --- including itself: `eval(eval, x) = eval(x)`.

No Gödel numbering, no quotation, no serialization.

== Interaction Nets

Interaction nets (Lafont, 1990) are a graph-rewriting model of computation. An interaction net consists of *agents* (nodes), each with a fixed number of typed *ports*: exactly one _principal_ port and zero or more _auxiliary_ ports. Ports are connected by *wires* (edges), each linking exactly two ports.

An *active pair* is two agents connected at their principal ports. The fundamental properties:

+ *Locality:* each interaction rule rewrites only the two agents in the active pair and their immediate connections.
+ *Independence:* active pairs share no agents (since each agent has one principal port, it participates in at most one active pair).
+ *Strong confluence:* if a net has multiple active pairs, reducing them in any order (or simultaneously) yields the same result.

Property 3, for a well-formed interaction net system with at most one rule for each unordered pair of agent types, guarantees determinism and enables _massive parallelism_: all active pairs can fire in one parallel step.

Lafont's _symmetric interaction combinators_ use three agent types ($gamma$, $delta$, $epsilon$) and six interaction rules. They are universal --- any interaction net system can be encoded in them. HVM2 implements a practical GPU runtime based on this framework, achieving tens of thousands of MIPS on modern GPUs.

=== The Bookkeeping Problem

When encoding lambda calculus in interaction nets via Lamping's algorithm (1990), additional *bookkeeping nodes* (brackets, croissants) are required to track variable scope during duplication. When a $delta$ agent duplicates a $lambda$-body, the copies carry implicit scope information. When two $delta$ agents from different $beta$-reductions meet, the system must determine whether to _annihilate_ (same scope) or _commute_ (different scope). This is the *oracle problem*.

Bookkeeping nodes add overhead: they multiply the number of reduction steps and can dominate evaluation cost. HVM sidesteps this by working at the interaction combinator level rather than reducing lambda terms directly, but the underlying complexity remains in the encoding.

// ═══════════════════════════════════════════════════════════════
= The Rooted TC-Net Construction
// ═══════════════════════════════════════════════════════════════

We now define TC-Net as a _rooted_ interaction net system. Every tree-calculus expression has a principal-rooted representation, including unevaluated applications. Evaluation is driven by consumers that meet this root.

This is the main repair to the naive construction. If application exposes its result only through an auxiliary port, erasers and duplicators cannot reach arbitrary unevaluated arguments. In Rooted TC-Net, application is a suspended term node with its own principal root, so erasure and duplication are ordinary local interactions.

== Agents

#figure(
  table(
    columns: 5,
    align: (center, center, center, left, left),
    stroke: 0.5pt,
    inset: 8pt,
    table.header([*Agent*], [*Arity*], [*Ports*], [*Role*], [*Category*]),
    [$L$], [0], [$p$], [Leaf $tri$], [Constructor],
    [$S$], [1], [$p, x$], [Stem $tri a$], [Constructor],
    [$F$], [2], [$p, l, r$], [Fork $tri a b$], [Constructor],
    [$P$], [2], [$p, f, a$], [Suspended application $f space a$], [Term],
    [$E$], [1], [$p, "res"$], [Demand weak-head value], [Consumer],
    [$A$], [2], [$p, "arg", "res"$], [Apply one argument], [Consumer],
    [$T_1$], [3], [$p, b, c, "res"$], [First-level dispatch], [Dispatch],
    [$T_2$], [4], [$p, w, x, b, "res"$], [Second-level dispatch], [Dispatch],
    [$delta$], [2], [$p, l, r$], [Duplicator], [Sharing],
    [$epsilon$], [0], [$p$], [Eraser], [Sharing],
  ),
  caption: [TC-Net agent types. Every agent has exactly one principal port $p$.],
)

The *term producers* ($L$, $S$, $F$, $P$) represent expressions. Their principal ports face outward toward consumers. The constructors represent values; $P$ represents a suspended application.

The *consumer agents* ($E$, $A$, $T_1$, $T_2$, $delta$, $epsilon$) ask something of a term root. $E$ demands a weak-head value, $A$ applies one argument to such a value, $T_1$ and $T_2$ dispatch on constructor shape, and $delta$/$epsilon$ copy or delete a term structurally.

The slogan is:

#block(inset: (left: 1em))[
  *Constructors and suspended applications are producers. Evaluation, application, triage, duplication, and erasure are consumers.*
]

== Encoding Tree Calculus Terms

A tree calculus term is encoded as a TC-Net as follows:

- *Leaf* $tri$: a single $L$ agent with principal port facing outward.
- *Stem* $tri a$: an $S$ agent with $S.x$ connected to the encoding of $a$.
- *Fork* $tri a b$: an $F$ agent with $F.l$ connected to $a$'s encoding and $F.r$ connected to $b$'s.
- *Application* $(f space a)$: a $P$ agent with $P.f$ connected to $f$'s encoding, $P.a$ connected to $a$'s encoding, and $P.p$ as the expression root.

A source term by itself is inert graph structure. It reduces when a consumer is connected to its root. To evaluate a closed term to weak-head form, connect $E$ to the root. To apply a term to an argument, connect $A$ to the function root with the argument on $A."arg"$.

== Reading the Rules: the $times.o$ Active Pair

Every reduction below is written $alpha times.o beta : "right-hand side"$. Before the rule tables begin we fix what $times.o$ denotes and what happens when a rule fires, so that the tables read as wiring instructions rather than equations.

#block(inset: (left: 1em))[
  *Active pair.* $alpha times.o beta$ is not an operation applied to two agents. It names a _configuration_: the two agents whose _principal ports are joined by a wire_. This is the interaction-net notion of a redex. It is symmetric, so $alpha times.o beta$ and $beta times.o alpha$ are the same site (rules are indexed by the unordered pair of agent types); we write the consumer on the left by convention.
]

Because every agent has exactly one principal port, an agent belongs to at most one active pair at a time. That is the entire content of the independence property above, and it is what allows active pairs to fire in parallel.

*Firing.* When $alpha times.o beta$ is an active pair and a rule exists for the unordered pair ${alpha, beta}$, the pair may _fire_. Firing is a purely local graph rewrite in three moves:

+ _Delete_ the two agents $alpha$ and $beta$ together with the wire joining their principal ports.
+ _Keep the interface._ Each auxiliary port of $alpha$ and $beta$ held a wire out to the rest of the net. Firing never touches the far ends of those wires; the right-hand side must reconnect every one of them exactly once. (Interaction rules are interface-preserving: the replacement net exposes exactly the free ports the two agents did.)
+ _Install the right-hand side_, minting the agents it names and attaching the interface wires to them as written.

So the "reconnects ports" intuition is right, with one refinement: $times.o$ marks _where_ a rewrite is enabled, and the text after the colon _is_ the rewrite. Firing consumes the two principal-joined agents and re-attaches the wires that hung off their auxiliary ports, sometimes to freshly minted agents. Nothing outside the pair is read or moved; only wires whose near end was an auxiliary port of $alpha$ or $beta$ change.

The right-hand side is assembled from three notations, each stating where an interface wire goes:

- *A new agent with a placed principal port,* $C["port"_1 := w_1, dots, "port"_k := w_k].p -> w_0$: mint an agent $C$, wire its auxiliary port $"port"_i$ to interface wire $w_i$ and its principal port $p$ to $w_0$. A positional spelling $C(w_1, dots, w_k)$ fills the auxiliary ports in order; the reduction atlas often writes the same thing as $w_0 -> C(w_1, dots, w_k)$. Example: $A(c, r) times.o S(x)$ fires to $F[l := x, r := c].p -> r$. The applicator and the stem vanish, a fork appears, and the three interface wires that were $A."arg" = c$, $A."res" = r$, and $S.x$ are reattached to $F.r$, $F.p$, and $F.l$. This is exactly $tri space x space c$: the stem $tri x$ applied to $c$ is the fork $tri x c$.

- *A bare wire,* $w_0 -> w_1$: splice two interface wires directly together, with no agent between them. This is how a rule returns one of its inputs untouched. Example: the K rule $T_1(b, c, r) times.o L$ fires to $(r -> b)$ and $(epsilon times.o c)$. The result wire $r$ is spliced onto the retained branch $b$, while the discarded branch $c$ is capped by a fresh eraser.

- *A fresh active pair.* When the right-hand side places a consumer's principal port onto an interface wire that leads to a producer, it has created a new redex, which fires next. This is how computation chains. The table form $A["arg" := a, "res" := r].p -> f$ and the diagram form $A(a, r) times.o f$ used in the atlas denote the same net: $A$'s principal port meets the wire $f$, and once demand exposes a producer there, $A times.o "producer"$ is the next active pair. Writing it with $times.o$ only emphasizes that an interaction is now enabled.

*Why firing is deterministic and parallel.* Two constraints govern it. The rule table is _functional_ (at most one rule per unordered pair), so an active pair fires in exactly one way. And distinct active pairs are vertex-disjoint (one principal port per agent), so firing one neither enables nor disables another. Together these give strong confluence (Theorem 2): every active pair present in a net may fire in the same parallel step, independently of order. The $times.o$ sites are precisely the places where parallel work happens.

== Demand and Application Rules

The demand agent exposes the weak-head constructor of a term:

$ E(r) times.o L &: quad "L.p" -> r \
  E(r) times.o S(x) &: quad S[x := x].p -> r \
  E(r) times.o F(a, b) &: quad F[l := a, r := b].p -> r \
  E(r) times.o P(f, a) &: quad A["arg" := a, "res" := r].p -> f $

Application is a consumer of a demanded function value:

$
  A(c, r) times.o L &: quad S[x := c].p -> r \
  A(c, r) times.o S(x) &: quad F[l := x, r := c].p -> r \
  A(c, r) times.o F(a, b) &: quad T_1[b := b, c := c, "res" := r].p -> a \
  A(c, r) times.o P(f, a) &: quad A_1["arg" := a, "res" := t].p -> f, quad A_2["arg" := c, "res" := r].p -> t
$

The $A times.o P$ rule is associativity of demand: to apply $(f space a)$ to $c$, first demand $f space a$, then apply $c$ to the result.

== Dispatch Rules

$T_1$ dispatches on the first argument $a$ of $tri a b c$. If that argument is a suspended application, it is first demanded until it exposes a constructor:

$
  T_1(b, c, r) times.o P(f, a) &: quad A["arg" := a, "res" := t].p -> f, quad T_1[b := b, c := c, "res" := r].p -> t \
  T_1(b, c, r) times.o L &: quad r -> b, quad epsilon -> c \
  T_1(b, c, r) times.o S(x) &: quad "delta.p" -> c, quad "delta.l" -> c_1, quad "delta.r" -> c_2, \
  & quad A_1["arg" := c_1, "res" := t_1].p -> x, quad A_2["arg" := c_2, "res" := t_2].p -> b, quad A_3["arg" := t_2, "res" := r].p -> t_1 \
  T_1(b, c, r) times.o F(w, x) &: quad T_2[w := w, x := x, b := b, "res" := r].p -> c
$

- *$T_1 times.o L$ (K rule):* $a$ is a leaf. Return $b$, erase $c$.
- *$T_1 times.o S(x)$ (S rule):* $a$ is a stem containing $x$. Compute $(x c)(b c)$. The two occurrences of $c$ are supplied by one structural $delta$.
- *$T_1 times.o F(w, x)$ (triage):* $a$ is a fork. Create second-level dispatch $T_2$ whose principal port faces $c$.

$T_2$ dispatches on the third argument $z$ of $tri (tri w x) y z$:

$
  T_2(w, x, b, r) times.o P(f, a) &: quad A["arg" := a, "res" := t].p -> f, quad T_2[w := w, x := x, b := b, "res" := r].p -> t \
  T_2(w, x, b, r) times.o L &: quad r -> w, quad epsilon -> x, quad epsilon -> b \
  T_2(w, x, b, r) times.o S(u) &: quad A["arg" := u, "res" := r].p -> x, quad epsilon -> w, quad epsilon -> b \
  T_2(w, x, b, r) times.o F(u, v) &: quad A_1["arg" := u, "res" := t].p -> b, quad A_2["arg" := v, "res" := r].p -> t, quad epsilon -> w, quad epsilon -> x
$

- *$T_2 times.o L$ (triage leaf):* $z$ is a leaf. Return $w$.
- *$T_2 times.o S(u)$ (triage stem):* $z$ is a stem containing $u$. Compute $x u$.
- *$T_2 times.o F(u, v)$ (triage fork):* $z$ is a fork. Compute $(b u) v$.

=== Summary Matrix

#figure(
  table(
    columns: 5,
    align: (left, left, left, left, left),
    stroke: 0.5pt,
    inset: 8pt,
    table.header([*Consumer*], [*meets $P$*], [*meets $L$*], [*meets $S$*], [*meets $F$*]),
    [$E$], [Demand function then apply], [Return leaf], [Return stem], [Return fork],
    [$A$], [Demand inner application], [Build stem], [Build fork], [Create $T_1$],
    [$T_1$], [Demand discriminator], [K: return $b$], [S: $(x c)(b c)$ + $delta$], [Create $T_2$],
    [$T_2$], [Demand inspected term], [Return $w$], [Apply $x u$], [Apply $(b u) v$],
  ),
  caption: [
    The consumer/producer rule matrix. The original 3$times$3 core is preserved in the $A$, $T_1$, and $T_2$ rows against constructors; the $P$ column repairs arbitrary suspended applications.
  ],
)

== Sharing Rules

Duplication and erasure are structural operations on the first-order term algebra generated by $L$, $S$, $F$, and $P$. They propagate through term producers and preserve constructor identity:

$
  delta times.o L &: quad L_1.p -> "delta.l", quad L_2.p -> "delta.r" \
  delta times.o S(x) &: quad "delta_x.p" -> x; quad S_1[x := "delta_x.l"].p -> "delta.l", quad S_2[x := "delta_x.r"].p -> "delta.r" \
  delta times.o F(a, b) &: quad "delta_a.p" -> a, quad "delta_b.p" -> b; \
  & quad F_1[l := "delta_a.l", r := "delta_b.l"].p -> "delta.l", \
  & quad F_2[l := "delta_a.r", r := "delta_b.r"].p -> "delta.r" \
  delta times.o P(f, a) &: quad "delta_f.p" -> f, quad "delta_a.p" -> a; \
  & quad P_1[f := "delta_f.l", a := "delta_a.l"].p -> "delta.l", \
  & quad P_2[f := "delta_f.r", a := "delta_a.r"].p -> "delta.r"
$

$
  epsilon times.o L &: quad "delete" \
  epsilon times.o S(x) &: quad "epsilon.p" -> x \
  epsilon times.o F(a, b) &: quad "epsilon_a.p" -> a, quad "epsilon_b.p" -> b \
  epsilon times.o P(f, a) &: quad "epsilon_f.p" -> f, quad "epsilon_a.p" -> a
$

*The crucial property:* a duplicated leaf is still a leaf, a duplicated stem is still a stem, a duplicated fork is still a fork, and a duplicated suspended application is still a suspended application. Constructor identity is invariant under duplication. This is the property that makes self-reflection compatible with sharing.

Rooted TC-Net does not require a primitive $delta$-$delta$ annihilation theorem for source-generated term graphs: $delta$ is a consumer of term producers. If an implementation adds lower-level graph-sharing nodes where duplicators can meet duplicators, it must prove the usual comonoid coherence laws separately. The tree-calculus-specific point is narrower and cleaner: no rule needs to preserve binding-scope labels, because there are no binders.

=== Implementation Note: Hash-Consing and Equality

Rooted TC-Net admits a natural hash-consed implementation because every source expression is a first-order principal-rooted producer. A runtime may store immutable canonical nodes keyed by tag and child identity:

$
  "hash"(L) &= H("L") \
  "hash"(S(x)) &= H("S", "id"(x)) \
  "hash"(F(a, b)) &= H("F", "id"(a), "id"(b)) \
  "hash"(P(f, a)) &= H("P", "id"(f), "id"(a))
$

With this representation, structural duplication can often be implemented by returning two references to the same canonical node rather than eagerly allocating deep copies. The formal $delta$ rules still describe structural copy, but the implementation realizes that copy lazily through sharing.

Hash-consing also gives a clean story for structural comparison:

- *Raw equality* compares canonical syntax, including suspended $P$ nodes.
- *Weak-head equality* first demands each side to weak-head form, then compares the exposed constructor and demanded children as needed.
- *Normal-form equality* requires a recursive normalizer consumer, then reduces structural equality of canonical normal forms to pointer equality.

This optimization should not be confused with observational equality of programs. Hash-consing makes equality of already-materialized tree structure cheap; it does not decide whether two different programs compute the same behavior.

== Variant: Demand-Before-Copy Duplication <demand-before-copy>

The structural rule $delta times.o P$ copies a _suspended computation_ as syntax. If a shared subterm is duplicated before it has been evaluated, each copy is subsequently demanded on its own, and the same dispatch interactions are re-performed once per copy. Structural $delta$ shares _structure_, never _work_ (@work-sharing makes this precise).

There is a one-rule repair. Write $delta^n$ ("need") for a second duplicator species and $delta^s$ ("structural") for the original. Instead of copying a suspended application, $delta^n$ _demands_ it and parks itself on the result wire:

$
  delta^n (l, r) times.o P(f, a) : quad A["arg" := a, "res" := t].p -> f, quad delta^n [l := l, r := r].p -> t
$

Port accounting: the four external wires ($"delta.l"$, $"delta.r"$, $P.f$, $P.a$) are each used exactly once, and the single internal wire $t$ connects $A."res"$ (an auxiliary port) to the parked duplicator's principal port. The net is well-formed and the rule table remains functional (one rule per unordered agent pair), so the strong confluence argument (Theorem 2) is untouched.

Because the parked $delta^n$ faces an _auxiliary_ port, no active pair exists yet: it waits. Every dispatch chain terminates by wiring a producer's principal root to its result wire, so eventually:

- a constructor $L$, $S$, or $F$ arrives: the structural copy rule fires, copying _one_ weak-head constructor (the children duplicators spawned by that rule are again $delta^n$ --- the species propagates through the copy wave);
- another suspended $P$ arrives (possible: e.g. the K rule returns its $b$ branch unevaluated): $delta^n times.o P$ fires again and the demand continues.

The shared computation is therefore performed _once_; only weak-head constructors are ever copied; and suspended children of the result remain shared behind further parked duplicators. This is call-by-need rendered as an interaction net, in the style of Sinot's token-passing nets: the demand chain is the evaluation token, and parking a consumer on an auxiliary result wire is the only way an interaction net can express "wait until this value exists."

=== The Two Species Coexist Without an Oracle

Duplicators only ever face term producers (or parked result wires) at their principal ports, so $delta^s$ and $delta^n$ can never meet principal-to-principal, and no coherence rule between the species is required. Contrast optimal $lambda$-reducers, where distinguishing duplicator species is precisely where index labels and the oracle enter. A compiler may choose per duplication site:

- $delta^n$ for _computational_ sharing --- in particular, the S-rule ($T_1 times.o S$) should spawn $delta^n$;
- $delta^s$ where copying syntax is the intent --- e.g. a reflective program materializing a structural copy of an unevaluated program as data.

Erasure is unchanged: $epsilon times.o P$ still deletes a suspended application without evaluating it. Laziness of _discard_ is single-consumer and independent of sharing of _demand_.

== Reduction Diagrams

The following Fletcher diagrams form an atlas of the active-pair reductions. The symbol $times.o$ marks the principal-principal connection. The right-hand side is a wiring sketch, not a sequential program: every newly exposed active pair may reduce in parallel with the others.

#let rewrite(lhs, rhs, note) = block(width: 100%, inset: (bottom: 0.6em))[
  #diagram(
    cell-size: 12mm,
    node-stroke: 0.55pt + luma(95),
    node-fill: luma(248),
    edge-stroke: 0.7pt + luma(70),
    node((0, 0), align(center, lhs), name: <lhs>, corner-radius: 3pt, inset: 5pt),
    node((4.2, 0), align(center, rhs), name: <rhs>, corner-radius: 3pt, inset: 5pt),
    edge(<lhs>, <rhs>, "-|>"),
  )
  #v(0.15em)
  #text(size: 8.7pt, fill: luma(70))[#note]
]

=== Demand and Application

#figure(
  grid(
    columns: 1,
    row-gutter: 0.2em,
    rewrite([$E(r) times.o L$], [$r -> L$], [Expose a leaf value.]),
    rewrite([$E(r) times.o S(x)$], [$r -> S(x)$], [Expose a stem value.]),
    rewrite([$E(r) times.o F(a, b)$], [$r -> F(a, b)$], [Expose a fork value.]),
    rewrite([$E(r) times.o P(f, a)$], [$A(a, r) times.o f$], [Demand the function, then apply the stored argument.]),
    rewrite([$A(c, r) times.o L$], [$r -> S(c)$], [Build stem: $tri space c$.]),
    rewrite([$A(c, r) times.o S(x)$], [$r -> F(x, c)$], [Build fork: $tri space x space c$.]),
    rewrite([$A(c, r) times.o F(a, b)$], [$T_1(b, c, r) times.o a$], [A fork receiving a third argument enters first-level dispatch.]),
    rewrite([$A(c, r) times.o P(f, a)$], [
      $A(a, t) times.o f$ \
      $A(c, r) times.o t$
    ], [Associate nested application by demanding $f space a$ first.]),
  ),
  caption: [
    Demand and application reductions. These rules make suspended applications principal-rooted while preserving the familiar argument-accumulation behavior of tree calculus.
  ],
)

=== First-Level Dispatch

#figure(
  grid(
    columns: 1,
    row-gutter: 0.2em,
    rewrite([$T_1(b, c, r) times.o P(f, a)$], [
      $A(a, t) times.o f$ \
      $T_1(b, c, r) times.o t$
    ], [Demand the discriminator until it exposes a constructor.]),
    rewrite([$T_1(b, c, r) times.o L$], [
      $r -> b$ \
      $epsilon times.o c$
    ], [K case: $tri space tri space b space c arrow.long b$.]),
    rewrite([$T_1(b, c, r) times.o S(x)$], [
      $delta times.o c$ \
      $A(c_1, t_1) times.o x$ \
      $A(c_2, t_2) times.o b$ \
      $A(t_2, r) times.o t_1$
    ], [S case: duplicate $c$ and compute $(x space c)(b space c)$.]),
    rewrite([$T_1(b, c, r) times.o F(w, x)$], [$T_2(w, x, b, r) times.o c$], [Fork discriminator enters triage.]),
  ),
  caption: [
    First-level dispatch reductions. $T_1$ chooses K, S, or triage from the first argument.
  ],
)

=== Second-Level Dispatch

#figure(
  grid(
    columns: 1,
    row-gutter: 0.2em,
    rewrite([$T_2(w, x, b, r) times.o P(f, a)$], [
      $A(a, t) times.o f$ \
      $T_2(w, x, b, r) times.o t$
    ], [Demand the inspected term until it exposes a constructor.]),
    rewrite([$T_2(w, x, b, r) times.o L$], [
      $r -> w$ \
      $epsilon times.o x$ \
      $epsilon times.o b$
    ], [Triage leaf case.]),
    rewrite([$T_2(w, x, b, r) times.o S(u)$], [
      $A(u, r) times.o x$ \
      $epsilon times.o w$ \
      $epsilon times.o b$
    ], [Triage stem case.]),
    rewrite([$T_2(w, x, b, r) times.o F(u, v)$], [
      $A(u, t) times.o b$ \
      $A(v, r) times.o t$ \
      $epsilon times.o w$ \
      $epsilon times.o x$
    ], [Triage fork case.]),
  ),
  caption: [
    Second-level dispatch reductions. $T_2$ chooses the leaf, stem, or fork branch from the inspected third argument.
  ],
)

=== Copy

#figure(
  grid(
    columns: 1,
    row-gutter: 0.2em,
    rewrite([$delta times.o L$], [
      $"delta.l" -> L_1$ \
      $"delta.r" -> L_2$
    ], [Copy leaf.]),
    rewrite([$delta times.o S(x)$], [
      $delta_x times.o x$ \
      $"delta.l" -> S("delta_x.l")$ \
      $"delta.r" -> S("delta_x.r")$
    ], [Copy stem and child.]),
    rewrite([$delta times.o F(a, b)$], [
      $delta_a times.o a$ \
      $delta_b times.o b$ \
      $"delta.l" -> F("delta_a.l", "delta_b.l")$ \
      $"delta.r" -> F("delta_a.r", "delta_b.r")$
    ], [Copy fork and both children.]),
    rewrite([$delta times.o P(f, a)$], [
      $delta_f times.o f$ \
      $delta_a times.o a$ \
      $"delta.l" -> P("delta_f.l", "delta_a.l")$ \
      $"delta.r" -> P("delta_f.r", "delta_a.r")$
    ], [Copy suspended application.]),
  ),
  caption: [
    Structural copy reductions. Copying preserves the producer tag and recursively copies each child.
  ],
)

=== Delete

#figure(
  grid(
    columns: 1,
    row-gutter: 0.2em,
    rewrite([$epsilon times.o L$], [$nothing$], [Delete leaf.]),
    rewrite([$epsilon times.o S(x)$], [$epsilon times.o x$], [Delete stem child.]),
    rewrite([$epsilon times.o F(a, b)$], [
      $epsilon_a times.o a$ \
      $epsilon_b times.o b$
    ], [Delete both fork children.]),
    rewrite([$epsilon times.o P(f, a)$], [
      $epsilon_f times.o f$ \
      $epsilon_a times.o a$
    ], [Delete suspended function and argument.]),
  ),
  caption: [
    Structural delete reductions. Deleting an unused term recursively deletes exactly its children.
  ],
)

// ═══════════════════════════════════════════════════════════════
= Properties
// ═══════════════════════════════════════════════════════════════

== Correctness <correctness>

#block(inset: (left: 1em))[
*Theorem 1 (Weak-Head Simulation).* For any tree calculus term $t$, if $t arrow.long^* v$ to a weak-head constructor form, then $E$ connected to the Rooted TC-Net encoding of $t$ reduces to the encoding of $v$ connected to the result wire.
]

_Sketch._ A suspended application $P(f, a)$ is demanded by replacing the demand with an $A$ consumer facing $f$. Repeated $A times.o P$ interactions reassociate nested applications until the function position exposes $L$, $S$, or $F$.

Once a fork receives its third argument, the five tree-calculus rules translate directly:
- K rule: $A times.o F$ creates $T_1$, then $T_1 times.o L$ returns $b$ and erases $c$.
- S rule: $A times.o F$ creates $T_1$, then $T_1 times.o S$ creates one $delta$ and three $A$ consumers implementing $(x c)(b c)$.
- Triage rules: $A times.o F$ creates $T_1$, $T_1 times.o F$ creates $T_2$, and $T_2 times.o L\/S\/F$ selects the leaf, stem, or fork branch.

Full normal-form computation requires either repeatedly demanding subterms or adding a recursive normalizer consumer. Rooted TC-Net's core semantics is deliberately demand-driven: unused suspended applications can be erased without evaluation.

== Strong Confluence and Parallelism

#block(inset: (left: 1em))[
*Theorem 2 (Strong Confluence).* If a TC-Net $N$ has active pairs $alpha$ and $beta$ with $alpha eq.not beta$, then reducing $alpha$ does not prevent or alter the reduction of $beta$.
]

_Proof._ Each agent has exactly one principal port. An active pair consists of two agents connected at their principal ports. Since $alpha$ and $beta$ are distinct active pairs, they share no agents (an agent in both would need two principal-port connections, contradicting the single-principal-port invariant). The rule table is also functional: each unordered consumer/producer pair has at most one rule. Therefore the two reductions operate on disjoint subgraphs and commute.

#block(inset: (left: 1em))[
*Corollary.* All active pairs in any TC-Net can be reduced simultaneously. The parallel evaluation is deterministic.
]

This is the same parallelism guarantee that makes HVM's GPU execution possible.

== Self-Reflection Preservation <self-reflection>

#block(inset: (left: 1em))[
*Theorem 3 (Reflection Invariance).* Triage on a duplicated tree value produces the same result as triage on the original.
]

_Proof._ Let $v$ be a tree value with outermost constructor $C in {L, S, F}$. When $delta$ duplicates $v$, the duplication rules produce two copies, each with the same outermost constructor $C$ (by the structure of the duplication rules). When $T_1$ or $T_2$ dispatches on a copy, it encounters $C$ at its principal port and applies the same interaction rule as it would for the original. The auxiliary ports of the copy are themselves duplicated versions of the original's children, so recursive triage also produces the same results, by induction on tree depth.

This theorem is the local invariant needed for examples such as `size(size)`, `equal?(equal?, equal?)`, and `eval(eval, x) = eval(x)`: when the program is duplicated, the function copy's triage on the argument copy dispatches the same way as it would on the original. Full correctness for those programs still depends on their tree-calculus definitions and the weak-head simulation theorem.

== No Binding-Scope Bookkeeping <no-bookkeeping>

#block(inset: (left: 1em))[
*Theorem 4 (No Binding Oracle Required).* Rooted TC-Net requires no Lamping-style bookkeeping nodes for binding scopes.
]

_Proof._ Bookkeeping nodes in Lamping's algorithm exist to track variable scope: when $delta$ duplicates a $lambda$-body, copies carry implicit scope information that must be matched when two $delta$'s from different $beta$-reductions meet.

Tree calculus has *no bound variables*. There is no $lambda x. M$ where $x$ occurs multiply. The only source of computational duplication in the core evaluator is the S-rule ($T_1 times.o S$), which duplicates the third argument $c$. The duplicated copies enter independent subcomputations ($(x c_1)$ and $(b c_2)$) that share no binding structure.

The structural sharing layer therefore has one obligation: copy and delete first-order term graphs while preserving constructor identity. It has no additional obligation to remember which lambda binder introduced which copy, because there are no lambda binders.

*Consequence:* Rooted TC-Net still performs administrative copy/delete work, but it avoids _binding-scope_ administrative work. This is a weaker claim than "zero overhead," and it is the claim supported by the construction.

This remains a genuine structural advantage over direct lambda-calculus optimal-reduction machinery, where scope tracking is the source of the oracle/bookkeeping problem.

== The S-Rule as Sole Source of Sharing

#block(inset: (left: 1em))[
*Observation.* Among the computational dispatch rules, only $T_1 times.o S$ introduces a $delta$ agent.
]

All triage rules ($T_2 times.o L\/S\/F$) produce only $A$'s and $epsilon$'s --- no duplication. The K rule ($T_1 times.o L$) produces only an $epsilon$. The application rules ($A times.o L\/S\/F$) produce only constructors or dispatch agents.

*Consequence:* Self-reflection (triage) introduces no new duplication rule. The duplication cost is proportional to S-combinator usage and structural sharing policy. Programs that heavily use triage but avoid the S-combinator incur no computational $delta$ cost from triage itself.

== Work Sharing and First-Order Optimality <work-sharing>

The two duplicator species of @demand-before-copy differ in what they share.

#block(inset: (left: 1em))[
*Proposition 5 (Structural duplication is not work-sharing).* With $delta^s times.o P$ as the only duplication rule for suspended applications, there are nets in which residuals of a single source redex are reduced twice.
]

_Proof._ Let $c$ be the suspended application `not true` (six interactions to weak-head form; see the worked example below) and encode $tri (tri x) y space c$ where $x$ and $y$ are both strict in their argument. The S-rule fires $delta^s times.o P(N, S(L))$, copying the suspended application as syntax. Each copy is then demanded independently and the dispatch interactions of `not true` are performed twice. The two copies are residuals of one source application, so the same work is done once per residual.

#block(inset: (left: 1em))[
*Theorem 6 (At-most-once reduction).* In a TC-Net in which every duplicator that can reach a suspended application is $delta^n$, every $P$ agent is dispatched at most once. Equivalently: no dispatch interaction is ever performed on two residuals of the same application.
]

_Proof sketch._ Residuals are created only by duplication. The $delta^n$ copy rules apply only to constructor agents ($L$, $S$, $F$); a $P$ agent is never copied --- when $delta^n$ meets $P$, the pair is consumed and replaced by a single demand chain plus a parked duplicator, and the species propagates through copy waves, so no $delta^s$ is ever introduced above a $P$. Each $P$ has exactly one principal wire, hence exactly one consumer, hence participates in exactly one dispatch. New $P$ agents arising during reduction are distinct individuals with their own single consumers. By induction over the reduction, no application agent ever acquires two residuals, so none is dispatched twice.

#block(inset: (left: 1em))[
*Corollary (Binderlessness collapses optimal sharing to need-sharing).* The tree-calculus analog of Lévy optimality --- no redex family contracted more than once --- is achieved by plain call-by-need duplication, with no labels, no brackets or croissants, and no oracle.
]

Why this is cheap here and expensive for $lambda$-calculus: $beta$-reduction substitutes _into_ binder bodies, so copying a $lambda$-value copies the redexes inside it, splitting their families --- keeping those families shared is exactly what Lamping's machinery exists for. Tree calculus never substitutes into anything: the S-rule builds new applications _around_ shared subtrees, and triage reads constructors without instantiating them. There are no redexes hiding under binders because there are no binders. First-order graph reduction with need-sharing is therefore already family-optimal.

Weak-head simulation (Theorem 1) is preserved under demand-driven scheduling: call-by-need and call-by-name coincide observationally in orthogonal rewrite systems; only the cost model changes.

=== What Is Still Not Shared

Theorem 6 is per-net sharing of _residuals_, not semantic memoization:

- Applying the same function tree to two _different_ arguments re-runs the dispatch spine once per application. (Lévy-optimal $lambda$-reducers do not share across distinct families either.)
- Two syntactically identical but _distinct_ subnets are never identified; that is the complementary mechanism of hash-consing (the implementation note above).
- When a binder language is compiled to tree calculus by bracket abstraction, each source-level $beta$ still expands to $Theta(|"body"|)$ dispatch interactions that thread the argument to its use sites. $delta^n$ guarantees the argument's _own_ work happens at most once, but the per-application distribution overhead remains. The asymptotic gap between TC-Net and optimal $lambda$-reducers on binder-heavy workloads is exactly this distribution cost --- never re-reduction.

=== Costs of $delta^n$

The need species trades away two properties of the purely structural system:

+ *Eager-safety.* In the original system every active pair is either demanded work or a terminating copy/delete wave; suspended $P$ agents are inert, so a fire-everything scheduler (HVM2-style) performs no speculative computation. $delta^n$ spawns _live_ demand chains: under fire-everything scheduling, shared arguments are evaluated speculatively --- still at most once, but possibly unneeded, and a divergent-but-discardable shared argument now diverges. This is the same semantics shift as strict-mode HVM2. The completeness direction of Theorem 1 therefore requires a demand-driven scheduler once $delta^n$ is in play; the choice of scheduler now affects the work profile, where previously it affected only copy-wave timing.
+ *Erasure completeness.* If both copies of a shared suspended term are discarded, the parked $delta^n$ and its unfired demand chain are garbage that $epsilon$ cannot reach ($epsilon$ interacts at principal ports; the parked duplicator is reachable only through auxiliary wires). A lazy implementation needs reachability-based garbage collection --- the known cost of lazy interaction-net evaluators. Under fire-everything scheduling there is no leak (the chain runs and $epsilon$ meets its result), at the price of the speculation above.
+ *Need-strictness.* Once _either_ copy is demanded, the shared work is performed, even if the sibling copy is later erased. This is ordinary call-by-need behavior.

// ═══════════════════════════════════════════════════════════════
= Worked Example: Boolean Negation
// ═══════════════════════════════════════════════════════════════

We trace `not false` and `not true` through Rooted TC-Net to illustrate demand-driven dispatch.

== Definitions

$
  "false" &= tri = L \
  "true" &= tri tri = S(L) \
  "not" &= tri (tri (tri tri) (tri tri tri)) tri = F(F(S(L), F(L, L)), L)
$

== Trace: `not false`

The source term `not false` is encoded as the suspended application $P(N, L)$ where $N = F(F(S(L), F(L, L)), L)$. A top-level demand $E$ starts evaluation.

#block(inset: (left: 1em))[
  *Step 1:* $E times.o P(N, L)$ creates $A["arg" := L, "res" := r].p -> N$.

  *Step 2:* $A(L, r) times.o F(underbrace(F(S(L), F(L,L)), a), underbrace(L, b))$.

  Creates $T_1$. $quad T_1.p -> a = F(S(L), F(L,L)), quad T_1.b = L, quad T_1.c = L$

  *Step 3:* $T_1 times.o F(underbrace(S(L), w), underbrace(F(L,L), x))$ --- $a$ is a fork.

  Creates $T_2$. $quad T_2.p -> c = L, quad T_2.w = S(L), quad T_2.x = F(L,L), quad T_2.b = L$

  *Step 4:* $T_2 times.o L$ --- $c$ is a leaf. Triage leaf case: result $= w = S(L) = tri tri = "true"$ #h(0.5em) $checkmark$

  Erase $x = F(L, L)$ and $b = L$.
]

*Four interaction steps including top-level demand.* The computational dispatch still takes the same three local steps after the demand exposes the application.

== Trace: `not true`

The source term `not true` is encoded as $P(N, S(L))$ and evaluated by a top-level $E$.

#block(inset: (left: 1em))[
  *Steps 1--3:* Same as above, but $T_2.p -> c = S(L)$.

  *Step 4:* $T_2 times.o S(L)$ --- $c$ is a stem with $u = L$. Triage stem case: compute $x u = F(L,L)$ applied to $L$.

  Creates $A'[p -> F(L,L), "arg" -> L, "res" -> r]$. Erase $w = S(L)$ and $b = L$.

  *Step 5:* $A' times.o F(L, L)$ --- Creates $T_1'[p -> L, b' = L, c' = L]$.

  *Step 6:* $T_1' times.o L$ --- K rule: result $= b' = L = tri = "false"$ #h(0.5em) $checkmark$
]

*Six interaction steps including top-level demand.* The triage dispatches on `true` = $S(L)$ (a stem), selects the stem handler, and the subsequent computation resolves via the K rule.

// ═══════════════════════════════════════════════════════════════
= The Linear Logic Perspective
// ═══════════════════════════════════════════════════════════════

TC-Net has a clean interpretation through the lens of linear logic.

== Tree as a Recursive Additive Type

Tree calculus values inhabit the recursive type:

$ "Tree" = 1 plus.o "Tree" plus.o ("Tree" times.o "Tree") $

where $1$ is the unit (leaf), the first $"Tree"$ is the stem case, and $"Tree" times.o "Tree"$ is the fork case. This is an *additive disjunction* ($plus.o$) --- a sum type with three variants.

Triage is the *elimination form* for this type: provide three handlers (one per variant), receive a tree, dispatch.

== Additives in Interaction Nets

Additive connectives ($plus.o$, $&$) are traditionally difficult to represent in interaction nets. HVM encodes them indirectly via Scott or NumScott encoding --- pattern matching is compiled into lambdas and numeric switches.

TC-Net's insight: tree calculus's triage is a *specific* additive, not a general one. It is case analysis on a *fixed* three-way recursive type. This specific case can be implemented as direct interaction rules (the $T_1$/$T_2$ agents) without the general complexity of arbitrary additives.

== The Full Picture

The interaction net framework provides the *multiplicative* structure:
- $times.o$ (tensor): parallel composition of ports
- $⅋$ (par): the dual
- $!$ (bang): $delta$-mediated unrestricted use

Tree calculus provides the *additive* structure:
- $plus.o$: the three-way tree type
- Case analysis via triage

TC-Net combines both:
- *Multiplicative*: parallel composition, local interaction (from interaction nets)
- *Additive*: structural case analysis (from triage)
- *Exponential*: sharing and duplication (from $delta$/$epsilon$)

Categorically, TC-Net implements an interaction net system for the fragment of MAELL (multiplicative-additive-exponential linear logic) specialized to the recursive Tree type. The specialization is what makes it tractable: full MAELL in interaction nets is complex, but MAELL restricted to one universal recursive type is elegant.

// ═══════════════════════════════════════════════════════════════
= Comparison with HVM
// ═══════════════════════════════════════════════════════════════

#figure(
  table(
    columns: 3,
    align: (left, left, left),
    stroke: 0.5pt,
    inset: 8pt,
    table.header([], [*HVM ($lambda$-calculus)*], [*TC-Net (tree calculus)*]),
    [Bookkeeping], [Binding-scope bookkeeping in optimal lambda reduction], [*No binding-scope labels*],
    [Self-reflection], [Requires external encoding], [*Native*],
    [Parallelism], [$checkmark$ (strong confluence)], [$checkmark$ (strong confluence)],
    [Sharing], [Engineered graph sharing], [Structural ($delta^s$) or call-by-need ($delta^n$, at-most-once)],
    [Constructor dispatch], [Scott/NumScott encoding], [*Direct interaction rules*],
    [GPU-amenable], [$checkmark$ (proven by HVM2)], [Plausible, unbenchmarked],
    [Self-application], [$lambda x. x x$ via encoding], [Via S-combinator],
    [Bound variables], [Yes (source of complexity)], [*None*],
    [Expressiveness], [Full $lambda$-calculus], [Full TC (Turing-complete)],
    [Rule shape], [Interaction combinators plus language encodings], [Producer/consumer matrix plus sharing],
  ),
  caption: [Structural comparison of HVM and TC-Net.],
)

=== Advantages of TC-Net

+ *Simpler binding story.* No binding-scope labels or oracle problem are needed because source terms have no binders.
+ *Native reflection.* No Scott/NumScott encoding overhead for pattern matching. Dispatch is a single interaction step, not a chain of lambda applications.
+ *Clean sharing obligation.* Copy/delete is structural over $L$, $S$, $F$, and $P$; implementation-level copier coherence remains a separate proof obligation.
+ *Programs = data.* The same constructors ($L$, $S$, $F$) represent both code and data. The same dispatch mechanism works on both.

=== Advantages of HVM

+ *Mature tooling.* Bend compiler, GPU runtime, established ecosystem.
+ *Lambda calculus compatibility.* Direct compilation from conventional functional languages.
+ *Proven GPU performance.* $tilde$74,000 MIPS on RTX 4090.

=== Relationship to Interaction Calculus

Taelin's Interaction Calculus (IC) is a textual representation of interaction nets with affine variables: each variable occurs at most once. This means IC cannot express $lambda x. x x$ (self-application).

Tree calculus _can_ express self-application (via the S-combinator, which duplicates its third argument). TC-Net handles this via $delta$ --- the duplication is explicit in the net, not implicit in variable usage. This suggests TC-Net may bridge the gap between IC's parallelism and lambda calculus's expressiveness, while avoiding the binding-scope machinery specific to lambda terms.

// ═══════════════════════════════════════════════════════════════
= Discussion
// ═══════════════════════════════════════════════════════════════

== Why Self-Reflection Is "Just" Pattern Matching

The deepest simplification: tree calculus's self-reflection is nothing more than *ADT pattern matching on a universal type*. In conventional languages, you can pattern-match on data (lists, trees, enums) but not on functions. In tree calculus, functions _are_ trees --- the same ADT as all data. So pattern matching (triage) works on everything, including programs.

In TC-Net, this means: the same constructors ($L$, $S$, $F$) that represent data also represent code. The same $T_1$/$T_2$ dispatch mechanism that analyzes data also analyzes programs. There is no distinction, and no special mechanism for reflection. It falls out of the interaction rules for free.

== Why No Binding-Scope Bookkeeping Matters

The oracle problem and bookkeeping overhead are the main sources of complexity in Lamping-style optimal reduction. They exist because lambda calculus has bound variables, which create nested scopes that interact non-trivially with duplication.

Tree calculus eliminates this issue at the source: *no bound variables, no scopes, no binding oracle*. The resulting interaction net system still needs structural copy/delete propagation, but it does not need to remember which binder a duplicated subgraph belongs to.

For a practical GPU implementation, this means:
- No binding-scope state to track per-agent
- No oracle decisions about lambda scopes during reduction
- Direct structural rules for copying and erasing suspended tree terms
- Simpler memory layout (agents have fixed, small port counts)

== The Two-Level Dispatch as a Feature

The two-level dispatch structure (first on $a$, then on $z$) might seem like complexity, but it is a faithful reflection of tree calculus's computational structure:

- *Level 1* ($T_1$): determines _what operation_ to perform (K, S, or triage)
- *Level 2* ($T_2$): determines _what data_ is being inspected (leaf, stem, or fork)

Self-reflection is precisely the _composition_ of these two levels: making computational decisions (level 1) based on structural inspection (level 2) of data that is itself computation.

// ═══════════════════════════════════════════════════════════════
= Open Questions
// ═══════════════════════════════════════════════════════════════

+ *Lévy-Optimality for Tree Calculus.* Largely resolved in @work-sharing: structural $delta^s$ duplication is _not_ work-sharing (Proposition 5 --- the earlier intuition that TC-Net "appears to satisfy" optimality was wrong for suspended applications), while the demand-before-copy species $delta^n$ ensures every application is dispatched at most once (Theorem 6), with no oracle, because binderlessness means no redex ever acquires two residuals. What remains open is the formalization: define redex families for tree calculus via labelled reduction à la Lévy, prove the at-most-once property coincides with family-optimality, and extend the statement from weak-head to full normalization (see the next item).

+ *Full Normalization.* The core system is weak-head and demand-driven. A full normalizer should be added as a recursive consumer that demands constructor children when full normal forms are required.

+ *Copier Coherence.* The source-level system treats $delta$ as a consumer of term producers. A lower-level implementation that allows copier-copier interactions must prove the expected comonoid laws.

+ *HVM Extension.* Rather than building TC-Net from scratch, HVM2 could be extended with native tree calculus support: add $L$/$S$/$F$/$P$ as agent types with tag-based dispatch, and $A$/$T_1$/$T_2$ as consumer agents. This would inherit HVM2's GPU runtime and engineering.

+ *Typed TC-Net.* Jay has developed typed versions of tree calculus. Linear types from the interaction net framework could combine with these: linear tree values (consumed once, no $delta$ needed) vs. unrestricted tree values (freely duplicated). This could eliminate $delta$ overhead for programs provably used linearly.

+ *Benchmarking.* The theoretical advantages (no binding-scope bookkeeping, direct dispatch, native reflection) need empirical validation. A prototype TC-Net evaluator, benchmarked against HVM2 on equivalent programs, would quantify the practical gains.

+ *Higher-Level Languages.* Tree calculus programs are verbose (deeply nested $tri$'s). A high-level language compiling to tree calculus (analogous to how Bend compiles to HVM) would make TC-Net practical. The self-reflection capability opens unique possibilities: programs that optimize themselves, meta-programs that generate and test candidate implementations, and debuggers that introspect their own execution.

+ *Categorical Semantics.* TC-Net implements a symmetric monoidal category with a retractive object (the Tree type, where $"Tree"^"Tree"$ is a retract of $"Tree"$). The interaction of this retraction with the monoidal structure (parallel composition) and the additive structure (triage) deserves categorical investigation.

// ═══════════════════════════════════════════════════════════════
= Conclusion
// ═══════════════════════════════════════════════════════════════

TC-Net demonstrates that self-reflection and explicit parallel sharing are not in tension. The key insight is deceptively simple: tree calculus programs are trees, interaction nets can duplicate trees while preserving their constructor identity, and triage dispatches on constructor identity. Therefore triage (self-reflection) composes naturally with structural sharing.

The rooted construction sharpens the original idea. Suspended applications are principal-rooted term producers, while evaluation, application, dispatch, duplication, and erasure are consumers. This fixes erasure and duplication of arbitrary unevaluated arguments without losing the compact dispatch structure.

The demand-before-copy variant sharpens it once more: a single replacement rule turns structural sharing into call-by-need, and binderlessness turns call-by-need into at-most-once reduction of every application --- the sharing for which $lambda$-calculus needs the full optimal-reduction apparatus. The costs are a scheduler obligation (demand-driven firing for laziness) and reachability garbage collection, not labels or an oracle.

Whether this theoretical elegance translates to practical performance remains to be demonstrated, but the structural advantages --- no binding oracle, native reflection, and direct constructor dispatch --- suggest that TC-Net may offer a clean foundation for parallel self-reflecting computation.

// ═══════════════════════════════════════════════════════════════
// References
// ═══════════════════════════════════════════════════════════════

#heading(numbering: none)[References]

#set text(size: 10pt)
#set par(hanging-indent: 1.5em)

Barry Jay. _Reflective Programs in Tree Calculus._ 2021. Book and Coq proofs: https://github.com/barry-jay-personal/tree-calculus

Barry Jay. "A Combinatory Account of Internal Structure." _Journal of Symbolic Logic_, 2009.

Barry Jay and Johannes Bader. Tree Calculus specification. https://treecalcul.us/specification/

Yves Lafont. "Interaction Nets." _Proceedings of POPL_, 1990.

Yves Lafont. "Interaction Combinators." _Information and Computation_ 137(1), 1997, pp. 69--101.

John Lamping. "An Algorithm for Optimal Lambda Calculus Reduction." _Proceedings of POPL_, 1990.

Victor Taelin. HVM2: A Massively Parallel Interaction Combinator Evaluator. https://github.com/HigherOrderCO/HVM

Victor Taelin. Interaction Calculus. https://github.com/VictorTaelin/Interaction-Calculus

Andrea Asperti and Stefano Guerrini. _The Optimal Implementation of Functional Programming Languages._ Cambridge University Press, 1999.

Damiano Mazza. "A Denotational Semantics for the Symmetric Interaction Combinators." _Mathematical Structures in Computer Science_, 2007.

François-Régis Sinot. "Call-by-Name and Call-by-Value as Token-Passing Interaction Nets." _Proceedings of TLCA_, 2005. (Call-by-need extension: "Token-Passing Nets: Call-by-Need for Free," _ENTCS_, 2006.)

Matt Brown and Jens Palsberg. "Breaking Through the Normalization Barrier: A Self-Interpreter for F-omega." _Proceedings of POPL_, 2016.
