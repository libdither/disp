// Sequent Calculus and Disp: from judgments to a terminating filter
// Compile with:  typst compile sequent.typ   (fetches @preview/curryst and @preview/cetz on first run)
// Typst 0.14+

#import "@preview/curryst:0.5.1": rule, prooftree
#import "@preview/cetz:0.4.2": canvas, draw

#set document(title: "Sequent Calculus and Disp", author: "research notes")
#set page(paper: "a4", margin: (x: 2.1cm, y: 2.4cm), numbering: "1")
#set text(size: 10.5pt, lang: "en")
#set par(justify: true, leading: 0.62em, spacing: 1.0em)
#set heading(numbering: "1.")
#show figure: set block(breakable: true)

#let note(title, body) = block(
  fill: luma(245), inset: 9pt, radius: 4pt, width: 100%, stroke: 0.5pt + luma(200),
  [#text(weight: "bold")[#title] \ #body],
)
#let intuition(body) = note(text(fill: rgb("#1d6f42"))[Intuition], body)
#let caveat(body) = note(text(fill: rgb("#a3431b"))[Where it bends], body)

// The document's color code, used consistently across diagrams:
// blue = payload / argument derivations, green = selected / surviving pieces,
// purple = tags and foci, warm highlight = annihilating rule pairs, gray = deleted.
#let cB = rgb("#0b63b0")
#let cG = rgb("#1d6f42")
#let cP = rgb("#7d3c98")
#let cR = rgb("#b3401f")
#let hl(body) = box(fill: rgb("#fdeadd"), inset: (x: 2.5pt, y: 1.5pt), radius: 2pt, body)

// Derivation trees via curryst. `pt` centers one tree; `ptrow` sets several side by side.
#let pt(t) = align(center, box(inset: (y: 0.45em), prooftree(t)))
#let ptrow(..ts) = align(center, box(inset: (y: 0.45em), grid(
  columns: ts.pos().len(), column-gutter: 2.4em, align: horizon,
  ..ts.pos().map(t => prooftree(t)))))

#align(center)[
  #text(19pt, weight: "bold")[Sequent Calculus and Disp]
  #v(0.25em)
  #text(12.5pt)[From judgments to a terminating filter]
  #v(0.4em)
  #text(9.5pt, style: "italic")[
    A type is a set of judgments. A checker is that set made decidable. \
    The kernel is what remains of the sequent calculus after five optimizations.
  ]
]

#v(0.6em)
#line(length: 100%, stroke: 0.5pt + luma(180))
#v(0.4em)

#outline(title: [Contents], depth: 2, indent: 1em)

#v(0.6em)
#line(length: 100%, stroke: 0.5pt + luma(180))

#note[Status][
  Reading companion (2026-07). Nothing here is normative: `TYPE_THEORY.typ` owns the
  type theory, `KERNEL_DESIGN.md` the implementation idioms, `NEGATIVE_TYPES.md` the
  telescope rationale, `MODULES.md` the module system. The document is a story: it
  starts from the naive definition of a type as a set of judgments and derives the
  kernel as the sequence of moves that makes membership decidable, with the sequent
  calculus supplying the name and the license for each move. Bare §N references point
  within this document; "spec §N" points into `TYPE_THEORY.typ`. The dictionary table
  and a fully traced example sit in the appendices for reference.
]

= The primitive act is judging

Start below types, below terms even, with the act itself. A *judgment* is an
association of a term with a type: $v : A$, "this tree is one of those." Nobody
gets to just assert one. A judgment is *warranted* by other judgments, and some of
those are taken on credit: a *context* $Γ$ is a finite stock of judgments assumed
rather than derived, and the *sequent*

$ Γ ⊢ v : A $

says "from the credit in $Γ$, the association $v : A$ is warranted." The sequent
calculus is the bookkeeping system for this: a small set of *rules*, each a licensed
way to extend the stock of warranted judgments from judgments you already hold.
A *derivation tree* is the audit trail, leaves at the top, the judgment it warrants
at the bottom. Here is one, drawn next to the disp value it warrants:

#figure(
  grid(
    columns: 2, column-gutter: 2.6em, align: horizon,
    prooftree(rule(
      name: text(fill: cP)[$⊕$R#sub[some]],
      [$⊢$ #text(fill: cP)[`inj some`] (#text(fill: cB)[`succ`] #text(fill: cG)[`zero`]) $: mono("Option") thin ℕ$],
      rule(
        name: text(fill: cB)[succR],
        [$⊢$ #text(fill: cB)[`succ`] #text(fill: cG)[`zero`] $: ℕ$],
        rule(name: text(fill: cG)[zeroR], [$⊢$ #text(fill: cG)[`zero`] $: ℕ$]),
      ),
    )),
    canvas(length: 1cm, {
      import draw: *
      line((0, 0.9), (-1.05, 0.05), stroke: 0.7pt + luma(120))
      line((0, 0.9), (1.05, 0.05), stroke: 0.7pt + luma(120))
      line((1.05, 0.05), (0.45, -0.8), stroke: 0.7pt + luma(120))
      line((1.05, 0.05), (1.65, -0.8), stroke: 0.7pt + luma(120))
      circle((0, 0.9), radius: 0.16, stroke: 1pt + cP, fill: cP.lighten(82%))
      content((0.78, 1.12), text(size: 8.5pt, fill: cP)[`pair`])
      circle((-1.05, 0.05), radius: 0.16, stroke: 1pt + cP, fill: cP.lighten(82%))
      content((-1.15, -0.38), text(size: 8.5pt, fill: cP)[`"some"`])
      circle((1.05, 0.05), radius: 0.16, stroke: 1pt + cB, fill: cB.lighten(82%))
      content((2.05, 0.22), text(size: 8.5pt, fill: cB)[`fork(△,·)`])
      circle((0.45, -0.8), radius: 0.16, stroke: 1pt + cB, fill: cB.lighten(82%))
      content((0.32, -1.2), text(size: 8.5pt, fill: cB)[`△`])
      circle((1.65, -0.8), radius: 0.16, stroke: 1pt + cG, fill: cG.lighten(80%))
      content((1.82, -1.2), text(size: 8.5pt, fill: cG)[`△`])
    }),
  ),
  caption: [The same picture drawn twice. Colors match rule applications to value
    nodes: the #text(fill: cP)[$⊕$R step] is the `pair` root and its tag, the
    #text(fill: cB)[succR step] is the `fork(△,·)` node, the
    #text(fill: cG)[zeroR axiom] is the leaf. §3.1 turns this into an algorithm:
    to check the value, read the derivation off it.],
) <fig-twice>

Now the two rules that belong to no connective, the bookkeeping pair every
calculus carries:

#ptrow(
  rule(name: [init], [$Γ, x : A ⊢ x : A$]),
  rule(name: [cut], [$Γ ⊢ c[a slash x] : C$], [$Γ ⊢ a : A$], [$Γ, x : A ⊢ c : C$]),
)

*Init* draws on a credit: if the stock contains $x : A$, then $x : A$ is warranted.
*Cut* splices two derivations: if $a : A$ is warranted, and $c : C$ is warranted on
the credit $x : A$, then $c[a slash x] : C$ is warranted with that credit paid off.
Cut is how lemmas work, how function calls work, how anything composes with
anything.

#intuition[
  What exactly gets cut? The *formula*. Look at the cut rule: $A$ appears in both
  premises and is absent from the conclusion. From below the line, $A$ is invisible;
  it is the private interface the two derivations agreed on, cut out of the public
  record. Gentzen's name (Schnitt) is about that hidden seam. The verb you are
  probably reaching for, cutting *steps out of the tree*, is the right image too,
  but it belongs to cut *elimination*: the process (§3.2) that removes cut rules
  from a derivation by surgery, cancelling matched rule pairs and splicing or
  deleting whole subtrees. That surgery, step for step, is what programmers call
  reduction.
]

= Types are harvested sets

Fix a rule set. It inductively generates $cal(D)$, the set of all derivable
sequents: start from the axioms, close under the rules. Now ask what a type *is*,
and take the boring answer seriously:

$ sans("ext")(A, Γ) = { v mid(|) (Γ ⊢ v : A) ∈ cal(D) } $

A type is the set of terms judged at it. What a type can type is what the type is:
the rules mentioning $A$ determine which associations are derivable, and that
harvest of judgments *is* $A$'s meaning. Membership then has an honest, terrible
algorithm:

#figure(
  align(center, grid(
    columns: 7, column-gutter: 0.6em, align: horizon,
    box(stroke: 0.7pt + luma(110), inset: (x: 7pt, y: 5pt), radius: 3pt)[the rule set],
    $⟶$,
    box(stroke: 0.7pt + luma(110), inset: (x: 7pt, y: 5pt), radius: 3pt)[
      $cal(D)$: every derivable $Γ ⊢ w : T'$, \ dovetail all finite trees],
    $⟶$,
    box(stroke: 0.7pt + luma(110), inset: (x: 7pt, y: 5pt), radius: 3pt)[keep $T' = A$],
    $⟶$,
    box(stroke: 0.7pt + luma(110), inset: (x: 7pt, y: 5pt), radius: 3pt)[
      watch for $w = v$ \ #text(size: 8pt, fill: luma(120))[(may wait forever)]],
  )),
  caption: [The naive filter: generate, filter, wait. Sound, complete on
    yes-instances, and not an algorithm. Everything after §2 is a transformation
    of this pipeline.],
) <fig-pipeline>

This is a *specification*, not an algorithm: derivability is semi-decidable at
best. Everything else in this document is the sequence of moves that turns the
naive filter into the disp kernel, and the sequent calculus is where each move
gets its license. Before the moves, three facts about the specification itself,
each of which shapes what follows.

*The definition is circular, and the circularity has a direction.* The rules that
generate $A$'s harvest mention other types, and not symmetrically. `Nat`'s rules
mention only `Nat`: its harvest is a least fixed point, grown from below, and
enumerating it means enumerating constructions. But $A → B$'s defining condition
consults $A$'s harvest *negatively*: $f$ is in $sans("ext")(A → B)$ when for every
$a$ in $sans("ext")(A)$, the judgment $f thin a : B$ is warranted. There is no
growing that from below by constructions; the honest reading enumerates *uses*.
Types split by which side of their definition is the generative one, and
@fig-polarity (§3.5) draws the split and turns it into the polarity discipline.

*Harvests overlap, and that is fine.* Sets defined by filtration are not
partitions. One tree may lie in many harvests, and in disp the smallest constants
do (spec §2.6, the collision doctrine):

#figure(
  canvas(length: 1cm, {
    import draw: *
    rect((-4.8, -2.35), (4.8, 2.6), radius: 0.25, stroke: 0.6pt + luma(140))
    content((-3.85, 2.32), text(size: 8.5pt, style: "italic", fill: luma(110))[all trees])
    circle((-1.2, 0.45), radius: (2.15, 1.2), stroke: 0.8pt + cB, fill: rgb(11, 99, 176, 20))
    content((-2.9, 1.5), text(size: 9pt, fill: cB)[$sans("ext")(mono("Nat"))$])
    circle((1.2, 0.45), radius: (2.15, 1.2), stroke: 0.8pt + cG, fill: rgb(29, 111, 66, 20))
    content((2.9, 1.5), text(size: 9pt, fill: cG)[$sans("ext")(mono("Bool"))$])
    circle((0, -0.7), radius: (2.5, 1.0), stroke: 0.8pt + cP, fill: rgb(125, 60, 152, 18))
    content((0, -1.42), text(size: 9pt, fill: cP)[$sans("ext")(mono("Eq A x x"))$])
    circle((0, 0.12), radius: 0.06, fill: black)
    content((0, 0.46), text(size: 8.5pt)[`△`])
    content((0, -0.25), text(size: 7.5pt, fill: luma(90))[\= zero = true = refl])
    circle((-2.5, 0.4), radius: 0.05, fill: cB)
    content((-2.5, 0.1), text(size: 8pt, fill: cB)[`succ zero`])
    circle((2.5, 0.4), radius: 0.05, fill: cG)
    content((2.5, 0.1), text(size: 8pt, fill: cG)[`△ △` (false)])
    circle((3.95, -1.95), radius: 0.05, fill: luma(110))
    content((3.5, -1.65), text(size: 8pt, fill: luma(110))[`t false zero`])
  }),
  caption: [Types are predicates carving overlapping regions out of one universe
    of trees. The smallest constants pool at the intersection; nothing is tagged
    with a unique type. The test discipline (cross-type wrong-values are `false`
    or `succ zero`, never `true` against `Nat`) is how you probe overlapping sets
    responsibly.],
) <fig-collision>

*Disp starts where the optimization ends.* Nothing in the kernel enumerates
anything. A disp type is handed over as its own characteristic function: a tree
program `T` with `param_apply T v` evaluating to `Ok true` or `Ok false`. The
generated set is the *specification* such a program answers to, and the rule
presentations survive in the library as arguments to the *makers*:
`Coproduct [(V_i, S_i)]` takes a list of right rules and returns the decision
procedure they determine; `Telescope cells` takes an inventory of observations
(left-side interface) and returns its filter. Declaring a datatype is presenting
inference rules; the maker is a compiler from rules to filters. The rest of this
document is the story of what that compiler must do.

(The lineage of §2's framing, for the curious: derivability-as-generated-set is
the logic-programming reading of the sequent calculus, made precise in Miller's
uniform proofs; types-as-predicates-over-an-untyped-substrate is realizability;
types-as-sets-of-designs-closed-under-testing is Girard's ludics, and it is the
closest ancestor of disp's behavioral types.)

= Five moves from a specification to an algorithm

Each move below is a theorem of proof theory wearing an engineer's hat: a fact
about which derivations can be ignored, turned into a step the pipeline of
@fig-pipeline no longer performs. The kernel is what is left when all five have
been made.

== Move 1: check, don't search. The term is the derivation

The naive filter searches all of $cal(D)$ for a sequent about $v$. But
@fig-twice already showed the shortcut: the derivation tree and the positive
value are the same picture, node for node. The tag names the last rule, the
payload is the subtree, and so on down to the leaves. A positive value is not
*evidence about* a derivation; it is the derivation, serialized. Constructor
application `inj V e` stores the rule name in `pair_fst` and the premises in
`pair_snd` (a k-ary constructor packs its premises as a right-nested pair). So
for a concrete positive candidate the search collapses to *reading*: recover the
last rule from the tag, recurse on the premises.

That is exactly disp's positive recognizer (`positive.disp`): `lookup_arm` maps
the value's tag to the declared variant, `coproduct_walk` walks the payload
against that variant's premise telescope, and recursion enters through `Rec` /
`RecUnder` / `RecAt` cells, so an inductive type's rule presentation carries its
own induction structure. The maker `Coproduct` is move 1 packaged as a function:
give it the right rules, receive the reader.

One subtlety pays rent later: the premise reader must handle a premise that is
*on credit*. `succ h` with `h` a hypothesis is a constructor applied to a
non-derivation-shaped subtree; `make_rec_recognizer` re-enters the H-rule (§3.4)
at every structural level so a credit member nested inside a concrete
construction is recognized rather than inspected.

== Move 2: only normal derivations matter. Cut elimination is evaluation

The naive filter's stream contains monstrous derivations: every lemma, every
composition, every roundabout proof of $v : A$ that routes through cut. Gentzen's
Hauptsatz says all of them are noise: every derivable sequent has a *cut-free*
derivation, and cut-free derivations have the subformula property, mentioning
only pieces of the sequent they prove. So the filter may restrict its attention
to cut-free trees, and the way you restrict to cut-free trees algorithmically is
to *run cut elimination yourself*: take the candidate, eliminate its cuts,
inspect the residue. Cut elimination is not a preprocessing trick bolted onto
evaluation. It *is* evaluation. Watch it once in full.

Let $⊕$ abbreviate `Coproduct [(V₁,S₁), (V₂,S₂)]` and let $P$ be a handler record
with fields $V_1 = {x} → b_1$ and $V_2 = {y} → b_2$. In @fig-cut-match and
@fig-cut-beta, color tracks identity across the surgery: #text(fill: cB)[blue] is
the payload's derivation, #text(fill: cG)[green] the selected branch,
#text(fill: luma(140))[struck gray] the branch about to die, the cut formula is
#text(fill: cR)[red], and the annihilating rule pair sits on #hl[highlighted]
labels. @fig-cut-match draws a match as a derivation with a cut on $⊕$, and the
one surgery step that eliminates it:

#figure(
  {
    pt(rule(
      name: [cut],
      [$⊢ (mono("prod") thin P) thin (mono("inj") thin V_1 thin e) : R$],
      rule(
        name: hl[$⊕$R#sub[1]],
        [$⊢ mono("inj") thin V_1 thin e :$ #text(fill: cR)[$⊕$]],
        text(fill: cB)[$⊢ e : S_1$],
      ),
      rule(
        name: hl[$⊕$L],
        [$c :$ #text(fill: cR)[$⊕$] $⊢ (mono("prod") thin P) thin c : R$],
        text(fill: cG)[$x : S_1 ⊢ b_1 : R$],
        text(fill: luma(140))[#strike[$y : S_2 ⊢ b_2 : R$]],
      ),
    ))
    align(center, text(size: 9pt, style: "italic")[
      $⇝$ one surgery step: the #hl[highlighted pair] annihilates, #text(fill: cB)[blue]
      and #text(fill: cG)[green] are spliced into a smaller cut, the struck branch is
      deleted
    ])
    pt(rule(
      name: [cut],
      [$⊢ b_1 [e slash x] : R$],
      text(fill: cB)[$⊢ e : S_1$],
      text(fill: cG)[$x : S_1 ⊢ b_1 : R$],
    ))
  },
  caption: [Cut elimination at $⊕$, which is what a `match` does: the principal
    pair annihilates, the payload and the selected branch splice into a smaller
    cut, and the unselected branch is deleted outright.],
) <fig-cut-match>

The cut formula $⊕$ is *principal* on both sides: the left premise ends by
building it ($⊕$R#sub[1]), the right premise ends by consuming it ($⊕$L). Gentzen's
key step: such a matched pair annihilates. The right rule told us which injection;
the left rule was prepared for every injection; so the payload derivation is
spliced into the matching branch and the rest of the tree is thrown away. Two
whole rule steps vanished, the $b_1$ branch got grafted, and the entire
$y : S_2 ⊢ b_2 : R$ subtree was *deleted*: cut out, in the fullest sense. The
residual cut is smaller (on $S_1$, a subformula) and elimination continues into
it. Now read the same surgery as the substrate saw it:

```disp
(prod P) (inj V₁ e)  →  (proj P V₁) e  →  b₁ e        // the §2.6 cut, firing
```

Disp's value-level primitive is named `cut` because it is this rule, and applying
it is this surgery: match selects a branch, discards the others, feeds the
payload. The deleted subtree is the unselected arm, and its deletion is weakening
performed at runtime (§5 prices exactly this: in the net evaluator the deleted
branch is what ε-cells erase).

The same step at the arrow is β-reduction (@fig-cut-beta): $→$R meets $→$L, the
argument's derivation in blue, the body's in green.

#figure(
  {
    pt(rule(
      name: [cut],
      [$⊢ (λ x. b) thin a : B$],
      rule(
        name: hl[$→$R],
        [$⊢ λ x. b :$ #text(fill: cR)[$A → B$]],
        text(fill: cG)[$x : A ⊢ b : B$],
      ),
      rule(
        name: hl[$→$L],
        [$f :$ #text(fill: cR)[$A → B$] $⊢ f thin a : B$],
        text(fill: cB)[$⊢ a : A$],
        rule(name: [init], text(fill: luma(140))[$z : B ⊢ z : B$]),
      ),
    ))
    align(center, text(size: 9pt, style: "italic")[
      $⇝$ the pair annihilates; the leftover cut against init evaporates (cutting
      into a bare credit-draw changes nothing)
    ])
    pt(rule(
      name: [cut],
      [$⊢ b[a slash x] : B$],
      text(fill: cB)[$⊢ a : A$],
      text(fill: cG)[$x : A ⊢ b : B$],
    ))
  },
  caption: [β-reduction is the same surgery at the arrow; eliminating the
    residual cut performs the substitution $b[a slash x]$.],
) <fig-cut-beta>

Eliminating the residual cut is the substitution itself, $⊢ b[a slash x] : B$.
In disp the surgeon is `apply`: one principal pair cancelled per step. Not every
cut sits at a principal pair; when the cut formula is not principal the cut
*commutes* upward past the intervening rule, bookkeeping without computation,
which is the congruence closure your evaluator calls "reducing under a context."
The interaction-combinator evaluator makes the taxonomy literal: a principal cut
is an active pair (two cells meeting at their principal ports), annihilation and
commutation are its two rule families, and cut elimination is local graph
surgery (`research/interaction-combinator/`).

What disp keeps from move 2:

- *Evaluate first, then read.* The filter normalizes the candidate and inspects
  the cut-free residue; move 1's reading is valid because normal forms of
  positive type are constructor-rooted (a cut-free derivation ends in a right
  rule). This is the sequent-calculus statement of "canonical forms."
- *Intern the cut-free trees.* Hash-consing stores every normal form once, so
  "these two derivations have the same cut-free form" is a pointer comparison.
  Conversion's O(1) cost (§6) is bought here.
- *Honesty: there is no Hauptsatz for the substrate.* Tree calculus is untyped
  and has `fix`; some cuts never eliminate. The kernel runs cut elimination as a
  budgeted attempt (`APPLY_BUDGET` is fuel), and the theorem that would restore
  termination for the typed fragment (`wf_fix` / `Total`, spec §10) is deferred
  work. Cut elimination here is a per-run behavioral fact, exactly as manifest
  contracts make typing a per-run behavioral fact.
- *Cut with a lemma is a declaration.* `let x := e` and later uses of `x` is the
  cut rule at file scope, and the elaborator eliminates it eagerly
  (deterministic elaboration interns the lemma's normal form once). Module fills
  are the same cut at module scope (§4).

== Move 3: one search order. Focusing makes the filter syntax-directed

Cut-free derivations of $⊢ v : A$ still allow spurious freedom: rules can be
applied in many orders, and a naive backward search branches on all of them.
Focusing (Andreoli) removes the freedom. Some rules are *invertible*: their
conclusion is derivable exactly when their premises are, so applying them eagerly
loses nothing and requires no choice point. The others come in uninterrupted runs
on one formula (the *focus*). The focusing theorem says the focused derivations,
alternating an exhaustive invertible phase with committed focused phases, are
complete: every derivable sequent has one, essentially unique in shape.

For the filter this is the difference between search and a deterministic walk.
Checking $v$ against a negative type applies the invertible right rules
exhaustively, and there is nothing to backtrack over: recognition can be a
*predicate*. In disp the invertible phase is `tele_walk true` (spec §12.7),
walking the type's cell inventory left to right, threading dependencies, no
choice points, refutation as `Ok false`. The focused phases are where the
remaining two moves live: committing to constructions (move 1 already did it:
reading a tag *is* the committed choice, made by the value rather than the
search) and running a hypothesis's uses forward (§4, the spine).

== Move 4: one generic member stands for all. The mint

Now the move the whole kernel bends around. Consider the filter for
$Π(x : A). B$. Read its defining condition extensionally and despair: one premise
per member of $A$'s harvest, which is in general infinite. The sequent calculus
already knows the escape, and it is the oldest one in logic: to prove a statement
about *all* members, prove it about *one arbitrary* member. The infinite premise
family collapses to a single premise plus a side condition (@fig-collapse):

#figure(
  {
    pt(rule(
      name: [$∀$R, read extensionally],
      [$Γ ⊢ v : Π(x : A). thin B thin x$],
      [$Γ ⊢ v thin a_1 : B thin a_1$],
      [$Γ ⊢ v thin a_2 : B thin a_2$],
      text(fill: luma(130))[$⋯$ #text(size: 8pt)[one per member of $sans("ext")(A)$]],
    ))
    align(center, text(size: 10pt)[$arrow.b.double$ #text(size: 9pt, style: "italic")[
      genericity: one fresh credit, about which nothing is known, stands for every member
    ]])
    pt(rule(
      name: [$→$R / $∀$R],
      [$Γ ⊢ v : Π(x : A). thin B thin x$],
      [$Γ, #text(fill: cP)[$h : A$] ⊢ v thin h : B thin h$],
      [$h ∉ sans("support")("result")$],
    ))
  },
  caption: [Move 4. The extensional reading of $∀$R has one premise per member of
    $A$'s harvest; genericity collapses the family to one minted credit plus the
    freshness side condition.],
) <fig-collapse>

Disp performs the move literally. `bind_hyp` mints the eigenvariable (a fresh
neutral carrying `stored_type = A`; deterministic under hash-consing, fresh per
binder, identical across re-checks), applies the candidate to it, and checks the
result against the codomain:

```disp
bind_hyp : (T : Type) -> ((h : T) -> CheckerResult (Pub h R)) -> CheckerResult R
//  Pub h R := Refinement R ({v} -> fresh_for h v)     (the sealing modality)
```

The side condition is enforced twice over, and this is the load-bearing point:

- *At the exit*: `occurs` scans the result for $h$, descending through stuck
  eliminations, and an escapee is `Err`. That is Gentzen's freshness condition run
  dynamically, internalized as the type `Pub h R` ("an $R$ that provably does not
  mention $h$") with the scan as its certificate.
- *In the interior*: one generic member stands for all members *only if the body
  never looks inside it*. If the check could ask "is $h$ a leaf?" it could pass
  for leaves and fail for forks, and the single test would stand for nothing.
  This is the walker's entire reason to exist (@fig-capsule). `param_apply` runs
  the body under a discipline whose two rejections (triage on a neutral; forging a
  neutral-rooted tree) are precisely the two ways a body could break genericity.
  Parametricity is not a bonus property here; it is the *validity condition of
  move 4*, the fine print on the license to replace an infinite enumeration
  with one test.

#figure(
  canvas(length: 1cm, {
    import draw: *
    circle((0, 0), radius: (1.2, 0.62), stroke: 1.1pt + cP, fill: cP.lighten(88%))
    content((0, 0.1), [$h : A$])
    content((0, -0.3), text(size: 7.5pt, fill: cP)[sealed: no shape])
    line((1.28, 0.12), (2.7, 0.55), mark: (end: ">"), stroke: 0.9pt + cG)
    content((2.85, 0.62), anchor: "west", text(size: 8pt, fill: cG)[✓ apply a frame: the spine grows (§4)])
    line((0.7, 0.58), (1.6, 1.3), mark: (end: ">"), stroke: 0.9pt + cG)
    content((1.75, 1.4), anchor: "west", text(size: 8pt, fill: cG)[✓ `is_neutral` / `neutral_type`: init's lookup (below)])
    line((-1.28, 0.12), (-2.7, 0.55), mark: (end: ">"), stroke: 0.9pt + cR)
    content((-2.85, 0.62), anchor: "east", text(size: 8pt, fill: cR)[✗ `triage h` (look inside): `Err`])
    line((0, -0.66), (0, -1.45), mark: (end: ">"), stroke: 0.9pt + cR)
    content((0, -1.72), text(size: 8pt, fill: cR)[✗ escape the scope: caught by `occurs`])
  }),
  caption: [The mint's contract, drawn. The two green observations preserve
    genericity (they never depend on $h$'s shape); the two red ones are exactly
    the walker's rejections. One test stands for all members only while the
    capsule stays sealed.],
) <fig-capsule>

The mint needs one more piece to be usable at all. The generic member $h$ will
flow into other checks; sooner or later some recognizer receives it as a
candidate. It has no construction to read (move 1 is helpless) and inspecting it
is forbidden (the walker will say `Err`). But the filter holds exactly one fact
about it, and that fact is init: $h$'s membership in $A$ was taken on credit, so
membership follows by a lookup, not a derivation. Disp calls this the *H-rule*,
and `make_recognizer` wraps every type's body in it:

```disp
// engine.disp: the recognizer shim. `self` is the type being applied.
if (is_neutral v)
  then (Ok (tree_eq self (neutral_type v)))   // init: h : A proves h : A
  else (body self meta v)                     // concrete: read the derivation
```

`is_neutral` is one signature comparison and `tree_eq` one pointer comparison, so
init costs O(1) at every type. Sequent calculi usually take init at atoms only
and derive compound init by η-expansion; disp takes deep init as primitive and
pays the coherence debt elsewhere (the η-readback gate on inductive responds, §4).
The H-rule has a second face for the polymorphic case: when the *type itself* is
a credit ($A : mono("Type")$ minted while checking a `Pi Type` body), applying it
to a candidate answers by the same lookup, implemented as `Type`'s respond
(`type_predicate_h_rule`, spec §12.18). Both faces are the same rule at the two
positions a credit can occupy, and together they are what makes
`Pi Type ({A} -> Pi A ({_} -> A))` checkable: the inner body's $A$-credits are
accepted by the abstract $A$ in O(1).

#note[Extensions with credit members separate types][
  Closed harvests can coincide: `Unit` and `Eq Nat zero zero` both have exactly
  the canonical inhabitant `t` (the collision doctrine at work, @fig-collision). But
  the H-rule seeds every type's harvest with its own generic members, keyed by
  tree identity of the stored type, and $h_A ∈ sans("ext")(B)$ holds exactly when
  $A$ and $B$ are the same tree. So once judgments on credit are counted,
  extensional identity of harvests collapses onto intensional identity of type
  trees. Deep init does not merely speed the filter up; it makes "same set" and
  "same tree" agree (see §6 for what this buys conversion).
]

== Move 5: negative membership is coverage. Enumerate the observations instead

Move 1 read constructions; move 4 tamed the arrow. What remains is the general
principle they specialize. Recall §2's circularity: some types' defining
conditions generate members (enumerate *builds*), others constrain uses
(enumerate *observations*). Drawn side by side in @fig-polarity, the two kinds
of harvest do not even look alike:

#figure(
  grid(
    columns: 2, column-gutter: 2.0em, align: top,
    canvas(length: 1cm, {
      import draw: *
      circle((0, 0), radius: (2.25, 1.6), stroke: 0.8pt + cB.lighten(45%))
      circle((0, -0.2), radius: (1.45, 1.0), stroke: 0.8pt + cB.lighten(22%))
      circle((0, -0.4), radius: (0.65, 0.42), stroke: 0.8pt + cB)
      content((0, -0.4), text(size: 9pt)[`△`])
      content((0, 0.34), text(size: 8.5pt)[`succ △`])
      content((0, 1.16), text(size: 8.5pt)[`succ (succ △)` #text(fill: luma(130))[$⋯$]])
      line((0.7, -0.34), (1.32, -0.16), mark: (end: ">"), stroke: 0.8pt + cB)
      line((1.48, 0.05), (2.12, 0.4), mark: (end: ">"), stroke: 0.8pt + cB)
      content((3.5, 0.1), text(size: 8pt, fill: cB)[succR pumps \ outward])
      content((0, -2.05), text(size: 8.5pt, style: "italic")[positive: grown from below \ (least fixed point; enumerate builds)])
    }),
    canvas(length: 1cm, {
      import draw: *
      rect((-2.5, -1.75), (2.5, 1.75), radius: 0.2, stroke: 0.6pt + luma(140))
      content((-1.72, 1.55), text(size: 8pt, style: "italic", fill: luma(110))[all trees])
      line((-2.5, 0.82), (2.5, 0.38), (2.5, -0.52), (-2.5, -0.95), close: true,
        fill: rgb(29, 111, 66, 20), stroke: none)
      line((-2.5, 0.82), (2.5, 0.38), stroke: 0.9pt + cG)
      line((-2.5, -0.95), (2.5, -0.52), stroke: 0.9pt + cG)
      content((0, 1.12), text(size: 8pt, fill: cG)[obligation: `·.fst` lands in `Nat`])
      content((0, -1.28), text(size: 8pt, fill: cG)[obligation: `·.snd` lands in `Nat`])
      content((0, -0.06), text(size: 9pt, fill: cG)[$sans("ext")({mono("fst") : ℕ, mono("snd") : ℕ})$])
      content((0, -2.05), text(size: 8.5pt, style: "italic")[negative: carved from above \ (meet of obligations; cover observations)])
    }),
  ),
  caption: [Two ways a harvest gets defined. Left: constructors pump members
    outward, so the filter reads builds (moves 1 and 2). Right: each observation
    is a constraint slicing the universe, and the harvest is the region
    satisfying all of them, so the filter covers observations (this move, plus
    move 4 wherever a slot is infinite). Polarity is which picture you are in.],
) <fig-polarity>

For the carved kind the filter's job is *coverage*: $v$ is a member when every
observation the type names lands well. If the observation inventory is finite,
cover it by iteration; if a slot in it is infinite (an argument position), cover
it by move 4's mint. Membership in a negative type is a finite walk of
obligations, each either checked directly or checked generically:

#pt(rule(
  name: [dep-$\&$R],
  [$Γ ⊢ v : mono("Telescope") [f_1 : A_1 ; thin f_2 : A_2 thin f_1 ; thin …]$],
  [$Γ ⊢ v.f_1 : A_1$],
  [$Γ ⊢ v.f_2 : A_2 thin (v.f_1)$],
  [$⋯$],
))

This derives the polarity table (@tab-polarity) instead of assigning it. A former is *positive*
when the generative side of its definition is the enumerable one: membership is
decided by reading a construction. It is *negative* when the observation side is:
membership is decided by covering an interface. Operationally disp marks the
negative ones as wait-forms ("a value drives its own elimination exactly when it
is a wait-form," spec §2.7), and the shift into the negative fragment happens at
abstraction, never at construction, so concrete data stays raw.

#figure(
  table(
    columns: (auto, auto, auto, auto),
    stroke: 0.4pt + gray, align: left, inset: 5.5pt,
    [*Connective*], [*Enumerable side*], [*Disp former*], [*The filter reads / covers*],
    [$⊕$], [builds], [`Coproduct` (per-variant positional telescopes)], [read the tag, recurse on premises],
    [$⊗$], [builds], [constructor payloads], [read the pair positions],
    [$1$], [builds], [`Unit`], [one construction, shared by collision],
    [$0$], [builds (none)], [`False`], [nothing to read; vacuous],
    [$→, ∀$], [observations], [`Pi` (`mint`-lead telescope)], [cover the one infinite slot by minting],
    [dependent $\&$], [observations], [`Telescope` (`Sigma`, `Record` are instances)], [cover the field inventory in order],
    [$⊤$], [observations (none)], [`Telescope t` $=$ `Tree`], [empty inventory; everything passes],
    [$∩$, refinement], [observations], [`Intersection`, `Refinement` (`qid`/`refine` cells)], [extra obligations on the same subject],
  ),
  caption: [Polarity as "which side of your definition can be enumerated." Atoms
    (`Bool`, `Nat`, `Ord`) are shape-encoded positives with view isos.],
) <tab-polarity>

The observation inventory is disp's `Telescope`, and it is worth saying what that
object *is* in sequent terms: a reified context. De Bruijn's telescopes are
dependent contexts $x_1 : A_1, thin x_2 : A_2(x_1), …$; disp's
`t cell (λx. rest)` is that, with dependency carried by real λ-tails. The cells
classify context-entry kinds:

#figure(
  table(
    columns: (auto, 1fr),
    stroke: 0.4pt + gray, align: left, inset: 5.5pt,
    [*Cell op*], [*Context-entry reading*],
    [`mint x : A`], [a credit to extend the stock with (move 4's eigenvariable; a function argument)],
    [`proj name : A`], [an observation obligation (a record field the subject must supply)],
    [`apply : B x`], [a use of the subject at the priors (a codomain)],
    [`deriv name := recipe`], [a definition in the context (a let-entry; transparent on elimination)],
    [`rec` / `rec_at`], [an induction hypothesis as a context entry (positive formers' knot)],
    [`qid` / `refine` / `imeet` / `eqends`], [constraint entries: intersections, refinements, equational assumptions in $Γ$],
  ),
  caption: [Cells are context-entry kinds; a new observation mode is a new op, no walker edit.],
) <tab-cells>

Two consequences fall out of "telescope = context." A concrete record checked
against `Telescope Γ` is a *substitution check*: the record supplies one value
per context entry, dependencies threaded, which is exactly the judgment
$γ : Δ → Γ$ ("this is a well-typed simultaneous substitution for the context").
And the empty telescope is the empty context, the empty meet of obligations:
`Telescope t` is $⊤$, definitionally `Tree`, the substrate's floor. Coverage of
nothing is free.

One classical landmark gets relocated by this move. The dependent pair, which
most presentations make positive (eliminated by pattern matching), is in disp a
two-cell telescope `{ fst : A, snd : B fst }`: the *negative* presentation of
pairing, eliminated by projection, with the positive pair surviving only as
constructor payloads. Conjunction has always had two sequent presentations ($⊗$
and $\&$) that only bookkeeping distinguishes; disp keeps both and assigns them
by how they are consumed.

= Running under credit: elimination is forward derivation

The five moves decide membership for closed candidates. But move 4 fills the
filter's world with credit members, and computation keeps trying to *use* them:
apply the minted function, project the minted record, match on the minted
number. A use of a credit is a left rule, and here the algorithm's relation to
the calculus flips direction. Right rules ran *backward* (decompose the goal:
recognition). Left rules run *forward*: the term is computing, it hits a
hypothesis in head position, and the derivation grows at the leaf as the run
proceeds. Disp stores that growing derivation in the value itself. A neutral's
payload is

```disp
Spine = Mint id | Ext parent frame        // universe.disp
```

which is a left-focused derivation in Herbelin's sense, an init leaf (`Mint`)
followed by the list of left-rule instances applied to it (`Ext`), the
head-variable-with-spine normal form of LJT. Watch one grow in @fig-spine. Take
$h : Π(x : ℕ). thin P$ with $P = {mono("fst") : ℕ, mono("snd") : ℕ}$:

#figure(
  canvas(length: 1cm, {
    import draw: *
    let st(x, w, lab) = {
      rect((x - w, -0.42), (x + w, 0.42), radius: 0.12, stroke: 0.8pt + luma(90))
      content((x, 0), lab)
    }
    st(0, 1.55, [$⟨h : Π(x : ℕ). thin P⟩$])
    st(4.6, 1.15, [$⟨h thin a : P⟩$])
    st(9.0, 1.5, [$⟨(h thin a).mono("fst") : ℕ⟩$])
    line((1.65, 0), (3.35, 0), mark: (end: ">"), stroke: 0.9pt)
    content((2.5, 0.42), text(size: 8pt)[frame: argument $a$])
    content((2.5, -0.45), text(size: 8pt, fill: luma(110))[`Extend P`])
    line((5.85, 0), (7.4, 0), mark: (end: ">"), stroke: 0.9pt)
    content((6.62, 0.42), text(size: 8pt)[frame: `acc fst`])
    content((6.62, -0.45), text(size: 8pt, fill: luma(110))[`Extend ℕ`])
    line((0, -0.55), (0, -1.15), stroke: (dash: "dashed", paint: luma(150)))
    line((4.6, -0.55), (4.6, -1.15), stroke: (dash: "dashed", paint: luma(150)))
    line((9.0, -0.55), (9.0, -1.15), stroke: (dash: "dashed", paint: luma(150)))
    content((0, -1.5), box(stroke: 0.7pt + cB, inset: 3.5pt, radius: 3pt)[`Mint h`])
    content((4.6, -1.5), box(stroke: 0.7pt + cB, inset: 3.5pt, radius: 3pt)[
      `Ext(`#box(stroke: 0.6pt + cB.lighten(40%), inset: 2.5pt, radius: 2.5pt)[`Mint h`]`, a)`])
    content((9.0, -1.5), box(stroke: 0.7pt + cB, inset: 3.5pt, radius: 3pt)[
      `Ext(`#box(stroke: 0.6pt + cB.lighten(40%), inset: 2.5pt, radius: 2.5pt)[
        `Ext(`#box(stroke: 0.6pt + cB.lighten(60%), inset: 2pt, radius: 2pt)[`Mint h`]`, a)`]`, acc fst)`])
    content((4.6, -2.25), text(size: 8.5pt, style: "italic")[the payload: the left-rule derivation so far, stored in the value])
  }),
  caption: [Left rules run forward. Top: the focused judgment stepping frame by
    frame, the stored type playing the stoup. Bottom: the spine accumulating one
    `Ext` per rule instance. When a recognizer finally meets the rightmost
    neutral, the H-rule closes the branch with one lookup.],
) <fig-spine>

Where positive values store right-rule derivations (§3.1), neutrals store
left-rule derivations. Every disp value is a proof object; its class says which
fragment of the calculus built it.

The engine that grows spines is one function, and each type's `respond`
meta-field is its left-rule table:

```disp
hyp_reduce := fix ({self, meta, frame} -> {
  let tmeta := type_meta meta.stored_type
  let this  := wait self meta
  match (tmeta.respond tmeta.recognizer_params this frame) {
    Extend new_type => wait self (extend_neutral_meta this new_type frame)
    Reduce v        => v
  }
})
```

`Extend T'` applies a left rule and stays in focus: the spine grows one frame,
the stoup formula updates to $T'$. `Reduce v` ends the focus with a value. There
is no third case: an illegal frame answers `Extend InvalidType`, a left rule into
an absorbing empty type, so failure stays inside the derivation discipline and
every later check on the dead branch fails deterministically. Writing
$⟨n : A⟩$ for "the neutral $n$, stored type $A$, under focus":

#figure(
  table(
    columns: (auto, auto, 1fr),
    stroke: 0.4pt + gray, align: left, inset: 5.5pt,
    [*Sequent rule*], [*Frame*], [*`respond` verdict*],
    [$→$L / $∀$L], [an argument $a$], [$⟨n : Π(x:A). B⟩ → ⟨n thin a : B thin a⟩$, i.e. `Extend (B a)`],
    [dep-$\&$L#sub[i] (opaque field)], [`acc name`], [walk to the field, priors fed their own projection-neutrals: `Extend ty`; dependent case: $n.mono("snd")$ lands `Extend (B (n.fst))`],
    [dep-$\&$L#sub[i] (derived field)], [`acc name`], [`Reduce recipe`: definitional transparency is a left rule that computes],
    [$⊕$L / induction], [`{ motive; cases }`], [`Extend (motive n)` after the coherence gate; concrete targets take the surgery of @fig-cut-match instead],
    [$=$L (J)], [`{ motive; … }`], [`Extend (motive rhs)`: eliminating $p : mono("Eq") thin A thin x thin y$ lands the motive at the endpoint],
    [$0$L (ex falso)], [any], [`False` has no closed proof; a `False`-credit eliminates via its respond, and `Not A` is $A → mono("False")$],
    [predicate init], [a candidate $v$], [`Type`-typed credit: `Return (tree_eq (neutral_type v) self)` (§3.4)],
    [the floor], [any tree], [`Tree`: application answers `Extend Tree` (a tree applied to a tree is a tree); a motive-shaped frame routes to gated elimination],
    [no rule applies], [-], [`Extend InvalidType`, the absorbing dead state],
  ),
  caption: [`respond` as the left-rule table. One engine (`hyp_reduce`), one table per type.],
) <tab-respond>

Three disciplines keep the forward direction honest:

*Positive left rules wait for concrete data.* `elim` gates on `is_neutral`: a
concrete coproduct takes the surgery of @fig-cut-match (inversion loses nothing; the value
names its own right rule), while a neutral target cannot be inverted (that would
be triage on an eigenvariable, the walker rejection). The elimination is instead
deferred as `Extend (motive n)`: a derivation left open at its $⊕$L, waiting for
a cut to supply the scrutinee.

*Minor premises are audited before the rule commits.* A left rule's minor
premises are derivations too. `gated_inductive_respond` η-readback-checks the
supplied cases against the motive before extending; incoherent cases land
`InvalidType`. The premises are walked through the spec §7A policed token
(`apply_policed`): the engine citing its own judgment for sub-derivations rather
than raw-reducing them, which is also what lets `param_apply` and `verify` carry
machine-checked annotations. The proof system can state its own rules as types.

*The rule table itself is fixed.* `param_apply` routes a pinned signature to the
*registered* kernel handler, ignoring whatever handler a forged wait-form embeds
(spec §5.4). A term cannot ship its own inference rule: Σ is a routing table, not
a trust set, and the walker's two rejections are the two ways a term could try to
amend the generator from inside a run (forge an init leaf; invert a credit). A
fixed rule set is what makes the harvest well-defined at all, so this is not
hygiene on top of the story: it is the story's precondition, enforced.

Contexts scale up by the same grammar. A module with `given` declarations *is* a
sequent with named credits, $mono("given") thin X_1 : T_1, …, X_k : T_k ⊢
mono("exports") : R$. Filling (`use "f" { X := v }`) is cut: substitute a
derivation for the credit. Bare checked use takes the deduction theorem instead:
read the file back as a function from its dependencies to its exports, one
telescope (`[mint dep ; apply out : Exports]`), turning $Γ, X ⊢ R$ into
$⊢ Π X. R$ with the readback lambda as proof term. That functor instantiation is
tree-identical to direct elaboration with the fills is the coherence of cut with
the deduction theorem, pinned by test rather than assumed (`MODULES.md`).
Hermeticity is the sequent discipline itself: a file sees its own declarations,
opens, and givens, nothing else, so its sequent is exact; and root files cannot
declare givens, matching the rule that the public judgment is closed
(`typecheck` gates on `is_closed`; open terms are checked only inside some
mint's scope).

= Bookkeeping the stock: structural rules at four layers

Three rules manipulate the credit stock without touching any formula:

#ptrow(
  rule(name: [weakening], [$Γ, A ⊢ C$], [$Γ ⊢ C$]),
  rule(name: [contraction], [$Γ, A ⊢ C$], [$Γ, A, A ⊢ C$]),
  rule(name: [exchange], [$Γ, B, A, Δ ⊢ C$], [$Γ, A, B, Δ ⊢ C$]),
)

In the harvest reading these are not rules so much as *invariance theorems of the
specification*: weakening says harvests are monotone in the stock
($sans("ext")(T, Γ) ⊆ sans("ext")(T, Γ ∪ {A})$: idle credit hurts nothing);
contraction says credit is not consumed by use (derivability cares about
presence, not multiplicity); exchange says the stock is a set, not a sequence,
except where one entry's *type* mentions another's *value*, in which case the
stock is a telescope and exchange holds only up to the dependency order.
Substructural logics are what you get by revoking the invariances (linear:
neither weakening nor contraction; affine: weakening only; relevant: contraction
only; ordered: not even exchange). Disp's position is layered: the invariances
all hold for the *set*, and each *implementation layer* re-prices them.

== The substrate: the terms carry the structural rules

Disp does not treat weakening and contraction as admissible meta-lemmas; two of
its five reduction rules *are* them; @fig-ks shows them firing on actual trees
(triangles stand for arbitrary subtrees, `@` for a pending application):

#figure(
  canvas(length: 1cm, {
    import draw: *
    let tri(x, y, col, lab) = {
      line((x - 0.32, y - 0.52), (x + 0.32, y - 0.52), (x, y), close: true,
        stroke: 0.8pt + col, fill: col.lighten(88%))
      content((x, y - 0.34), text(size: 8pt, fill: col)[#lab])
    }
    let gtri(x, y) = {
      line((x - 0.32, y - 0.52), (x + 0.32, y - 0.52), (x, y), close: true,
        stroke: (dash: "dashed", paint: cB.lighten(30%)))
      content((x, y - 0.34), text(size: 8pt, fill: cB.lighten(25%))[`x`])
    }
    let nd(x, y) = circle((x, y), radius: 0.1, stroke: 0.8pt + luma(60), fill: luma(235))
    let app(x, y) = {
      circle((x, y), radius: 0.13, stroke: 0.8pt + luma(60))
      content((x, y), text(size: 7.5pt)[\@])
    }
    // K row
    content((-1.5, -0.45), text(size: 10pt)[K:])
    line((0, 0), (-0.55, -0.6), stroke: 0.7pt + luma(120))
    line((0, 0), (0.55, -0.6), stroke: 0.7pt + luma(120))
    nd(0, 0)
    content((-0.62, -0.85), text(size: 8.5pt)[`△`])
    tri(0.55, -0.6, luma(60), `b`)
    content((1.3, -0.45), text(size: 10pt)[$•$])
    tri(2.0, -0.3, cB, `x`)
    line((2.75, -0.45), (3.65, -0.45), mark: (end: ">"), stroke: 0.9pt)
    tri(4.35, -0.15, luma(60), `b`)
    gtri(5.8, -0.3)
    content((6.0, -1.15), text(size: 8pt, fill: cR)[`x` discarded: weakening])
    // S row
    content((-1.5, -2.85), text(size: 10pt)[S:])
    line((0, -2.4), (-0.55, -3.0), stroke: 0.7pt + luma(120))
    line((0, -2.4), (0.55, -3.0), stroke: 0.7pt + luma(120))
    nd(0, -2.4)
    nd(-0.55, -3.0)
    line((-0.55, -3.1), (-0.55, -3.5), stroke: 0.7pt + luma(120))
    tri(-0.55, -3.5, luma(60), `c`)
    tri(0.55, -3.0, luma(60), `b`)
    content((1.3, -2.85), text(size: 10pt)[$•$])
    tri(2.0, -2.7, cB, `x`)
    line((2.75, -2.85), (3.65, -2.85), mark: (end: ">"), stroke: 0.9pt)
    app(5.2, -2.4)
    line((5.2, -2.52), (4.55, -3.0), stroke: 0.7pt + luma(120))
    line((5.2, -2.52), (5.85, -3.0), stroke: 0.7pt + luma(120))
    app(4.55, -3.0)
    app(5.85, -3.0)
    line((4.55, -3.12), (4.2, -3.5), stroke: 0.7pt + luma(120))
    line((4.55, -3.12), (4.95, -3.5), stroke: 0.7pt + luma(120))
    tri(4.2, -3.5, luma(60), `c`)
    tri(4.95, -3.5, cB, `x`)
    line((5.85, -3.12), (5.5, -3.5), stroke: 0.7pt + luma(120))
    line((5.85, -3.12), (6.25, -3.5), stroke: 0.7pt + luma(120))
    tri(5.5, -3.5, luma(60), `b`)
    tri(6.25, -3.5, cB, `x`)
    content((7.9, -3.0), text(size: 8pt, fill: cB)[`x` twice: \ contraction])
  }),
  caption: [The substrate performing the structural rules.
    K (`apply(fork(△,b), x) = b`) throws its argument away: weakening as a
    reduction. S (`apply(fork(stem c, b), x) = (c x)(b x)`) copies its argument
    into both branches: contraction as a reduction. Exchange has no rule; bracket
    abstraction compiles it away.],
) <fig-ks>

This is the old combinatory-logic fact in new clothes: a combinator basis is a
Hilbert system, K is the weakening axiom, S carries contraction, and *bracket
abstraction is the deduction theorem run as a compiler*. The elaborator
translates binder syntax into K/S trees, which is the classical proof that
contexts can be eliminated, executed: every use of exchange, weakening, and
contraction in a derivation becomes an explicit combinator in the term, and
hash-consing then shares the bookkeeping maximally. (Two compiler workarounds in
`CLAUDE.md`, the η/saturation rule and the free-var closure of match arms, are
engineering shadows of this translation.)

At the checking layer the invariances surface as behaviors: minting a credit and
ignoring it is fine (`bind_hyp Nat ({x} -> Ok 0)` passes); a hypothesis may be
used arbitrarily often (`occurs` checks presence, never multiplicity; nothing in
the type layer counts uses, and the coeffect design space disp deliberately did
not build is surveyed in `effects-and-coeffects.typ`); ambient credits are
unordered (the support scan builds a set) while telescopes order theirs by
dependency, with the admissible reorderings *computed*: `field_deps`
(`demand.disp`) extracts which earlier entries a field actually uses, and
`sub_record` builds the restricted context on a dependency closure, thinning and
strengthening as library functions returning types. One invariance is even a
checkable principle rather than an invisible freebie: effect-row weakening.
The `Eff` recognizer decides containment, so `m : Eff S X` inhabits `Eff R X`
for every $R ⊇ S$ definitionally, with no subtyping judgment and no coercion
(spec §15.9): monotonicity of the harvest, stated by a predicate.

== The declaration layer: the theory is nearly affine

Values are cartesian; *bindings* are not. A declaration is a request mediated by
the name's guard (`cut.disp`; SYNTAX.typ), and the guard policies are a
substructural discipline on the ambient theory itself:

- `default_guard`: a name is owned once, an unguarded duplicate is a driver
  error. Contraction of *definitions* is forbidden by default (weakening is
  fine: names may go unused). The declaration context is affine.
- `license_guard R`: rebinding demands credentials, `{ new; proof }` with
  `proof : R old new` and the relation explicit. Replacing a lemma requires a
  proof relating old and new, and opens replay these licenses across module
  boundaries with driver-stamped certificates: contraction, licensed.
- `freeze`: no rebind ever; a linear name.

Disp is substructural precisely where proof engineering is sloppiest: not in
terms, but in the theory. Redefining a lemma mid-development is the contraction
that actually breaks proof developments, and it is the one disp polices.

== The evaluator: linear logic as the cost model

The interaction-combinator substrate (`research/interaction-combinator/`,
`SPATIAL_IC.md`, `EMBEDDING_THEOREM.md`) runs the same programs on nets where the
invariances stop being free: contraction is an explicit duplicator cell (δ),
weakening an explicit eraser (ε), and the embedding inserts them. Operationally
this is Girard's decomposition of intuitionistic logic into linear logic: the
exponential $!A$ is exactly "this value may be duplicated and discarded, pay
here." The pricing is measured, not asserted: wire-RC is a weakening garbage
collector (doubly-dead δ cancellation), the E1 rent experiments price
structural-rule traffic, and the embedding theorem's polarity lemma is the proof
obligation that the translation respects who-drives. Recall @fig-cut-match: the subtree
deleted by a match's cut-elimination step is precisely what ε must erase. The
slogan the layers add up to: *disp keeps LJ at the type layer and rediscovers
linear logic at the evaluator layer; structural rules are logically free and
operationally priced.*

What disp does not have, for the record: linear or affine arrows in the surface
language, graded binders, any ordered-context former beyond dependency itself.
The one modal operator in the system is `Pub h R` (§3.4), and it polices scope,
not usage.

= When "the same type" means the same set

If a type is its harvest, type equality *ought* to mean same harvest. That
relation is $Π^0_2$-flavored and undecidable, so the filter needs the same kind
of move the membership problem needed, and disp makes a characteristically blunt
one: replace the extensional relation by an intensional proxy that move 2
already paid for.

*Judgmental equality is `tree_eq`.* Conversion is the silent rule

#pt(rule(
  name: [conv],
  [$Γ ⊢ v : B$],
  [$Γ ⊢ v : A$],
  [$A ≡ B$],
))

and in disp $A ≡ B$ is a pointer comparison: hash-consing interns every cut-free
form, deterministic elaboration makes "same spelling, same tree" hold across
re-checks, so convertible types are *the same object*, not two objects compared.
By §3.4's separation note this proxy is less arbitrary than it looks: once
harvests include credit members, same-tree and same-harvest agree, so the
intensional shortcut computes the extensional relation on the fragment where the
H-rule sets the terms of comparison. The cost is real even so: $≡$ is identity
of normal forms and nothing coarser (a stuck `row_union ρ S` compares
syntactically, spec §15.9), and no amount of semantic equality between two
differently-spelled types will make `tree_eq` say yes.

*Propositional equality is `Eq`.* The right rule is `refl`, gated by the
judgmental layer (`Eq A x y` accepts `refl` iff `tree_eq x y`; `refl = t` by
collision), and the left rule is J, implemented as `Eq`'s respond: eliminating a
stuck $p : mono("Eq") thin A thin x thin y$ at a motive answers
`Extend (motive rhs)`, the motive walked under the policed token. `sym`,
`trans`, `cong`, `subst` (`base.disp`) are the admissible rules derived from J.

*Observational equality is the honest relation, and it is frontier.* Negative
values have no shape to compare; their true equality is agreement under every
observation, the harvest-equality this section opened with. `lib/std/oeq.disp`
implements the pointwise Pi form: agreement during a neutral-probe run, one
minted argument standing for all. But move 4's fine print bites back here. The
mint is a sound test of *membership* because the walker forbids the checked body
to inspect the credit; it is a weaker test of *equality between two given
functions*, because those functions are not under the walker when real callers
run them, and a function can detect a probe (`is_neutral` through the sanctioned
readers) and behave differently on concrete input (@fig-probe):

#figure(
  canvas(length: 1cm, {
    import draw: *
    line((0, 0), (7.6, 0), mark: (end: ">"), stroke: 0.7pt + luma(90))
    line((0, 0), (0, 3.3), mark: (end: ">"), stroke: 0.7pt + luma(90))
    content((7.55, -0.32), text(size: 8pt, fill: luma(110))[inputs])
    content((-0.15, 3.45), anchor: "east", text(size: 8pt, fill: luma(110))[outputs])
    line((0.3, 0.5), (6.9, 2.5), stroke: 1pt + cB)
    content((7.15, 2.5), anchor: "west", text(size: 8.5pt, fill: cB)[`id`])
    line((0.3, 1.05), (1.0, 1.26), stroke: 1pt + cG)
    line((1.0, 1.26), (1.4, 0.83), stroke: 1pt + cG)
    line((1.4, 0.83), (1.8, 1.5), stroke: 1pt + cG)
    line((1.8, 1.5), (6.9, 3.05), stroke: 1pt + cG)
    content((7.15, 3.05), anchor: "west", text(size: 8.5pt, fill: cG)[`shift`])
    circle((1.4, 0.83), radius: 0.08, fill: black)
    line((1.4, 0), (1.4, 0.72), stroke: (dash: "dashed", paint: luma(150)))
    content((1.4, -0.35), text(size: 8pt)[the one minted probe `h`])
    content((3.35, 0.62), text(size: 8pt, fill: luma(110))[both answer the same stuck value here])
    line((5.0, 1.98), (5.0, 2.4), mark: (start: ">", end: ">"), stroke: 0.8pt + cR)
    content((5.2, 2.2), anchor: "west", text(size: 8pt, fill: cR)[disagree on every concrete input])
  }),
  caption: [The oeq gap, drawn. `shift` = `{n} -> if (is_neutral n) then n else
    succ n` detects the probe and dips to meet `id` there; everywhere concrete
    they differ. Probe-agreement sampled the observation space where the
    specification demands covering it.],
) <fig-probe>

The counterexample is live (`OEQ_INVESTIGATION.md`; `ACTIVE_BUGS.md` item 5):
probe-agreement certified a `license_guard` rebind between functions that differ
on every concrete number. In this document's terms: the specification quantifies
over all observations, the implementation *sampled* one, and sampling is only
sound under a restriction on observers that the kernel does not yet enforce. The
fix design (observer restriction: strict walker mode, first-order certificates,
the PER lift) lives in `OPTIMIZER.typ`. The same drift, one level up, is what
the subject-reduction ledger tracks: the spec set is closed under cut by
construction; the decided set may not be; `ACTIVE_BUGS.md` is the ledger of the
difference, with machine pins in `lib/tests/probe_*_sr.test.disp`.

= The ledger, and the other query

Collected honestly, the places where the story admits its limits:

- *No Hauptsatz.* The substrate is untyped and has `fix`; cut elimination is a
  budgeted attempt, normalization behavioral, `wf_fix`/`Total` deferred
  (spec §10). Everything downstream of move 2 inherits the asterisk.
- *The circularity lands on the universe.* §2's definition generates types and
  judgments mutually; set theory would stratify, disp instead closes the loop
  computationally: `Type := BehavioralType` is a telescope of observations that
  *inhabits itself* (`StrictType : StrictType`, no privileged kinding rule; the
  universe knot of `universe.disp`). The naive definition's circularity is not
  patched; it is adopted as an operating principle and pinned by tests.
- *`Err` is not a verdict.* `Ok false` refutes a sequent; `Err` reports that the
  *run* breached the bookkeeping (inverted an eigenvariable, forged an init
  leaf, escaped a scope, posed an open judgment at the boundary). The calculus
  has no analogue because on paper nobody tries.
- *Single succedent, one pole per type.* No $℘$, no classical dualization.
  Curien and Herbelin's symmetric cut has a critical pair whose global
  resolution is the CBV/CBN choice; disp resolves it per type, by polarity.
- *Proofs are quotiented harder than derivations.* The calculus identifies
  derivations up to permutations; disp evaluates and interns, so all
  derivations of one cut-free form are one tree. What survives of proof
  relevance is structural: right-rule traces in positive values, left-rule
  traces in spines.
- *Recognition is scoped, not universal.* By collision, a recognizer decides
  membership among type-respecting candidates, not over arbitrary trees
  (`Ok zero` is structurally `succ zero`). The discipline, not the predicate
  alone, carries soundness.

And the door the story opens. The harvest answers two queries (@fig-queries):

#figure(
  canvas(length: 1cm, {
    import draw: *
    circle((0, 0), radius: (1.75, 1.0), stroke: 0.9pt + cP, fill: cP.lighten(90%))
    content((0, 0.18), text(size: 9.5pt)[$sans("ext")(T)$])
    content((0, -0.32), text(size: 7.5pt, fill: luma(110))[the harvest])
    line((-4.35, 0), (-1.9, 0), mark: (end: ">"), stroke: 0.9pt + cB)
    content((-4.55, 0), anchor: "east", text(size: 9pt, fill: cB)[`v`])
    content((-3.1, 0.42), text(size: 8pt, fill: cB)[checking: is `v` inside?])
    content((-3.1, -0.42), text(size: 8pt, fill: luma(110))[decide; this document])
    line((1.9, 0), (4.35, 0), mark: (end: ">"), stroke: 0.9pt + cG)
    content((4.55, 0), anchor: "west", text(size: 9pt, fill: cG)[`v` ?])
    content((3.35, 0.45), text(size: 8pt, fill: cG)[synthesis: produce a member])
    content((3.35, -0.42), text(size: 8pt, fill: luma(110))[search; `GOALS.md`])
  }),
  caption: [One set, two queries. The checker decides membership from a given
    term; synthesis must find the term, the query no theorem makes cheap.],
) <fig-queries>

Term given, decide membership: that is the checker, and this document derived
it. Type given, *find* a member: that is inhabitation, proof search proper, the
query the filter deliberately refuses (it is the undecidable one). Everything
disp built for checking is infrastructure for searching: the focused phase
structure prunes the search space (that is what focusing was invented for), the
H-rule closes branches, telescopes enumerate the obligations a candidate must
meet, and spines are exactly the partial proof objects a search extends. The
missing ingredient is guidance about which branch to grow, and that is the
`GOALS.md` endgame in one line: neural-guided synthesis is a learned heuristic
for enumerating the harvest, the half of the naive filter that no theorem makes
cheap (`LOCAL_SYNTH.md` for the design). Checking and synthesis are one set,
queried from opposite ends.

= Appendix: the dictionary

The story in compressed form. "Where" points into this document.

#figure(
  table(
    columns: (auto, auto, auto),
    stroke: 0.4pt + gray, align: left, inset: 5.5pt,
    [*Sequent calculus*], [*Disp*], [*Where*],
    [judgment $v : A$], [an association a run can warrant], [§1],
    [context $Γ$], [credit: minted hyps carrying `stored_type`; reified as a telescope when needed as a value], [§1, §3.5],
    [sequent $Γ ⊢ v : A$], [a run: `param_apply A v` evaluating to `Ok true`], [§2],
    [the type $A$ itself], [$sans("ext")(A, Γ)$, the harvest of derivable judgments at $A$; in disp, its characteristic program], [§2],
    [rule presentation], [maker arguments: `Coproduct [(V,S)…]`, `Telescope cells`; makers compile rules to filters], [§2, §3.1],
    [init (axiom)], [the H-rule: `is_neutral` + `tree_eq` on `neutral_type`, O(1), at every type], [§3.4],
    [cut], [`apply`; the §2.6 value `cut`; `let`; module fills; `typecheck`/`verify` at the boundary], [§1, §3.2, §4],
    [cut elimination], [evaluation: principal pairs annihilate, subtrees splice or die; budgeted, not proven], [§3.2],
    [subformula property], [canonical forms: cut-free positives are constructor-rooted, so reading works], [§3.2],
    [invertible (async) phase], [recognition: `tele_walk true`, syntax-directed, no backtracking], [§3.3],
    [eigenvariable + freshness], [`bind_hyp` + `occurs`; `Pub h R`; the walker as the validity condition], [§3.4],
    [focused (sync) phase / stoup], [a neutral's spine; `Spine = Mint | Ext` is a stored left-focused derivation], [§4],
    [left rules], [`hyp_reduce` consulting the type's `respond`; `Extend` = stay in focus, `Reduce` = blur], [§4],
    [fixed rule set], [Σ routing: pinned sigs run the *registered* handler; rejections = attempted amendments], [§4],
    [deduction theorem], [bracket abstraction (terms); module functor readback (files)], [§5.1, §4],
    [weakening / contraction / exchange], [invariances of the harvest; K / S / bracket abstraction; priced as ε/δ in the net evaluator], [§5],
    [substructural discipline], [`default_guard` (affine names), `license_guard` (licensed rebinds), `freeze` (linear)], [§5.2],
    [conversion $A ≡ B$], [`tree_eq`, O(1); sound because credit members separate types], [§6],
    [identity type], [`Eq`: `refl` right rule, J left rule via respond], [§6],
    [harvest equality (extensional)], [`oeq`, currently probe-sampled; observer restriction is the open design], [§6],
    [subject reduction], [spec-set vs decided-set drift; `ACTIVE_BUGS.md` is the ledger], [§6],
    [inhabitation / proof search], [synthesis; the query the checker refuses; `GOALS.md`, `LOCAL_SYNTH.md`], [§7],
  ),
  caption: [The dictionary.],
) <tab-dictionary>

= Appendix: one full run of the filter

Check `({p} -> p.fst) : Pi (Sigma Nat ({_} -> Nat)) ({_} -> Nat)`; write $Σ$ for
the Sigma type. The derivation the run traces out:

#pt(rule(
  name: [$→$R, then occurs],
  [$⊢ mono("{p} -> p.fst") : Π(p : Σ). thin ℕ$],
  rule(
    name: [dep-$\&$L#sub[1], then init (H-rule)],
    [$p : Σ ⊢ p.mono("fst") : ℕ$],
    rule(name: [init (mint)], [$p : Σ ⊢ p : Σ$]),
  ),
))

The same events in execution order, one row per rule:

#figure(
  table(
    columns: (auto, 1fr),
    stroke: 0.4pt + gray, align: left, inset: 5.5pt,
    [*Derivation, bottom-up*], [*Kernel, in execution order*],
    [goal $⊢ mono("{p} -> p.fst") : Π(p:Σ). ℕ$], [`param_apply` routes the Pi recognizer; `tele_walk true` starts on `[mint p : Sigma ; apply out : Nat]` (move 3: the invertible walk)],
    [$→$R opens the scope], [the `mint` cell: `bind_hyp` mints the credit $p$ with `stored_type = Sigma Nat _` (move 4)],
    [body: $p : Σ ⊢ p.mono("fst") : ℕ$], [the `apply` cell observes `v p`, walking `p (acc "fst")` under the walker],
    [dep-$\&$L#sub[1]: focus $⟨p : Σ⟩$, project], [`hyp_reduce`: Sigma's respond (`tele_walk false`) walks to `fst`; opaque field, so `Extend Nat`: the neutral `p.fst`, spine `Ext(Mint p, acc fst)` (@fig-spine: the derivation grows forward)],
    [init closes the branch], [`Nat`'s recognizer, H-rule: `Ok (tree_eq Nat (neutral_type p.fst))` = `Ok true` (move 4's lookup)],
    [$→$R's side condition], [`bind_hyp`'s `occurs` scan on the result (`Ok true` is closed): pass; the run returns `Ok true`],
  ),
  caption: [The sequent proof and the trace are the same object read in opposite
    directions: right rules backward, left rules forward, init where they meet.],
) <tab-run>

= Reading list

Gentzen (1935) for LK/LJ, cut, and the Hauptsatz. Herbelin (1994), "A λ-calculus
structure isomorphic to Gentzen-style sequent calculus structure," for LJT, the
stoup, and spines. Andreoli (1992) for focusing; Liang and Miller (2009) for
LJF; Zeilberger (2008) and Munch-Maccagnoni (2013) for polarity as a foundation.
Miller, Nadathur, Pfenning, Scedrov (1991), "Uniform proofs as a foundation for
logic programming," for derivability-as-generated-set run as a machine. Girard
(1987) for linear logic and the exponential decomposition of the structural
rules; Girard (2001), "Locus solum," for ludics, types as sets closed under
testing. Curien and Herbelin (2000), "The duality of computation," and "Grokking
the Sequent Calculus" (ICFP 2024) as the readable entry. Levy (2001) for
call-by-push-value and the shifts. Abel, Pientka, Thibodeau, Setzer (2013) for
copatterns (a `respond` is a copattern table over frames). In-repo:
`TYPE_THEORY.typ` §2.6, §2.7, §5 through §7, §12 (especially §12.7 and §12.19),
§17; `NEGATIVE_TYPES.md`; `KERNEL_DESIGN.md` § Telescopes; `MODULES.md`;
`GOALS.md` and `LOCAL_SYNTH.md`;
`research/interaction-combinator/EMBEDDING_THEOREM.md`;
`research/effects-and-coeffects.typ`.
