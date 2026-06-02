// Effects and Coeffects — a field guide
// Compile with:  typst compile effects-and-coeffects.typ
// Typst 0.14.x

#set document(title: "Effects and Coeffects: A Field Guide", author: "research notes")
#set page(paper: "a4", margin: (x: 2.1cm, y: 2.4cm), numbering: "1")
#set text(size: 10.5pt, lang: "en")
#set par(justify: true, leading: 0.62em, spacing: 1.0em)
#set heading(numbering: "1.")

// A light callout box for intuition / warnings.
#let note(title, body) = block(
  fill: luma(245), inset: 9pt, radius: 4pt, width: 100%, stroke: 0.5pt + luma(200),
  [#text(weight: "bold")[#title] \ #body],
)
#let intuition(body) = note(text(fill: rgb("#1d6f42"))[Intuition], body)
#let warning(body) = note(text(fill: rgb("#a3431b"))[Watch out], body)

// monoid op (effects) and semiring ops (coeffects), kept visually distinct.
#let mon = math.dot.op // effect monoid sequencing
#let smul = math.times.o // semiring multiplication
#let sadd = math.plus // semiring addition

#align(center)[
  #text(19pt, weight: "bold")[Effects and Coeffects]
  #v(0.25em)
  #text(12.5pt)[The Two Sides of the Typing Judgment]
  #v(0.4em)
  #text(9.5pt, style: "italic")[
    What a computation #emph[causes] vs. what it #emph[demands] — \
    and what happens when you put both in one type system.
  ]
]

#v(0.6em)
#line(length: 100%, stroke: 0.5pt + luma(180))
#v(0.4em)

#outline(title: [Contents], depth: 2, indent: 1em)

#v(0.6em)
#line(length: 100%, stroke: 0.5pt + luma(180))

= The question, in one paragraph

Algebraic-effect languages such as *Koka* track what a computation #emph[does to] the
world — it throws, it mutates, it prints, it forks. *Coeffect* systems (Petříček, Orchard
& Mycroft) track what a computation #emph[needs from] its context — which implicit
parameters it reads, how many times it uses a variable, how sensitive its output is to its
input, what security clearance it requires. The two are formally *dual*: effects are
*monadic* and annotate the #emph[conclusion] of a typing judgment; coeffects are
*comonadic* and annotate the #emph[assumptions]. This document builds that distinction
from the ground up, catalogs the useful instances of each, and then works through the hard
and interesting part: what happens when a single computation is *both* effectful and
coeffectful at once.

#intuition[
  An *effect* is a function's _exhaust_ — the mess it makes on the way out.
  A *coeffect* is a function's _intake_ — the nutritional label of what it consumes from
  its environment before it can even run. Reading input from the world is exhaust (you
  performed an interaction), not intake — that is the trap we dismantle in @sec:confusion.
]

= Where the annotation lives <sec:judgments>

A plain typing judgment says "in context $Gamma$, expression $e$ has type $tau$":

$ Gamma tack.r e : tau $

A *graded effect system* decorates the *conclusion*. The annotation $f$ (from an effect
monoid) describes what $e$ produces or causes:

$ Gamma tack.r e : tau thin ! thin f quad quad #text(8.5pt)[("e.g." $f = {"throw", "io"}$)] $

A *coeffect system* decorates the *assumptions* instead. Each free variable carries a grade
$r_i$ (from a coeffect semiring) describing how $e$ demands it:

$ (x_1 :_(r_1) tau_1, thin ..., thin x_n :_(r_n) tau_n) tack.r e : tau $

A *combined* system does both at once — grades on the left, an effect on the right:

$ (x_1 :_(r_1) tau_1, thin ..., thin x_n :_(r_n) tau_n) tack.r e : tau thin ! thin f $

#intuition[
  Effects ride out on the *result*. Coeffects ride in on the *variables*. That single
  "which side of the turnstile" question is the whole distinction; everything else is a
  consequence of it.
]

A concrete contrast. In Koka, an effectful function wears its effect on the arrow's output:

```koka
fun get-line() : <console> string   // effect <console> on the result
```

In a usage-tracking coeffect language, a function wears its grade on the argument:

```granule
dup : a [2] -> (a, a)               // demands its argument 2 times
dup [x] = (x, x)
```

Nothing about `dup` touches the world. It performs no action. Its type is a statement about
*consumption*, not *causation* — that is what makes it a coeffect and not an effect.

= The categorical skeleton <sec:cats>

The shapes line up as exact duals.

#table(
  columns: (auto, 1fr, 1fr),
  inset: 7pt,
  align: (left, left, left),
  stroke: 0.5pt + luma(210),
  table.header([], [*Effects*], [*Coeffects*]),
  [structure], [(graded) *monad* $T$], [(graded) *comonad* $D$],
  [a computation is], [a Kleisli arrow $A arrow.r T B$], [a coKleisli arrow $D A arrow.r B$],
  [unit / counit], [$"return" : A arrow.r T A$], [$"extract" : D A arrow.r A$],
  [the "extra stuff" is on the], [#emph[output]], [#emph[input / context]],
  [grading algebra], [preordered *monoid* $(E, #mon, I)$], [preordered *semiring* $(R, lt.eq, 0, #sadd, 1, #smul)$],
  [families], [${T_e}_(e in E)$], [${D_r}_(r in R)$],
  [composition uses], [the monoid op $#mon$], [the semiring ops $#sadd$ and $#smul$],
)

A graded monad gives a family $T_e$ with $eta : "Id" arrow.r T_1$ and a multiplication
$mu : T_e (T_(e')) arrow.r T_(e #mon e')$ — sequencing two effectful steps *adds up*
(via the monoid) their annotations. A graded comonad gives $epsilon : D_1 arrow.r "Id"$
and a comultiplication $delta : D_(r #smul r') arrow.r D_r (D_(r')) $ — nesting two layers
of demand *multiplies* the grades.

#intuition[
  Monad = "I can keep going and the effects pile up." Comonad = "I came wrapped in context,
  and you can re-wrap me." Sequencing effects is associative bookkeeping (a monoid);
  managing demands needs richer arithmetic (a semiring), for the reason in @sec:asymmetry.
]

= Why the duality is *not* trivial <sec:asymmetry>

If effects and coeffects were perfect mirror images we would expect the same algebra on
both sides. They are not, and the reason is built into the $lambda$-calculus: a judgment has
*many inputs* (the free variables) but *one output*.

- The *output* side only ever needs to *sequence*: do this, then that. One associative
  operation with a unit — a *monoid*. There is no notion of "the same result shared between
  two consumers," so the effect algebra has no addition.
- The *input* side must handle two genuinely different situations, so it needs a *semiring*:
  - *Contraction* — a variable used in two subterms. Its grades must *combine*:
    $ "!"_(r #sadd s) A arrow.r "!"_r A times.o "!"_s A $
    This is where the semiring's *addition* $#sadd$ comes from.
  - *Nesting / scaling* — a demand sitting under another demand, or a whole context pulled
    under a modality. Grades *multiply*: $D_(r #smul s) = D_r (D_s)$. That is the semiring's
    *multiplication* $#smul$.
  - *Weakening* — an unused variable gets grade $0$, so $"!"_0 A arrow.r 1$.

#warning[
  This monoid-vs-semiring asymmetry (verified against Gaboardi et al., ICFP 2016) is not a
  cosmetic detail. It is *the* structural reason effects and coeffects behave differently
  under composition, and it dictates which combinations are even expressible (@sec:compat).
  Effects "lack contraction" precisely because a monoid has no $#sadd$.
]

= The classic confusion: is `readLine` a coeffect? <sec:confusion>

A natural first guess: input depends on context, so `readLine` is a *coeffect*, and output
affects the world, so `writeLine` is an *effect*. The mapping "input = coeffect, output =
effect" is *wrong*, and seeing why is the fastest route to understanding coeffects.

`readLine` is an *effect*. Reading a line is an *operation the computation performs*; at
runtime it interacts with the world and the world answers. In Koka it lives in the very
same `console` effect row as `writeLine`. Both are on the "what the computation does" side;
both are monadic. *Direction of data flow (in vs. out) is not the effect/coeffect axis.* The
axis is *causes/produces* (monad) vs. *demands-from-context* (comonad).

#intuition[
  `readLine` flows data *inward* but it is still exhaust: you *did something* (performed a
  read) and the world reacted. A coeffectful "input" would be resolved *statically from
  context* — an implicit parameter the caller must supply, or a usage constraint — not a
  runtime action.
]

The subtlety that makes this worth dwelling on: *environment-dependence can be modeled on
either side*, and the two models are categorical duals.

#table(
  columns: (1fr, 1fr),
  inset: 7pt, stroke: 0.5pt + luma(210),
  table.header([*Reader monad* (effect)], [*Coreader comonad* (coeffect)]),
  [$A arrow.r (R arrow.r B)$], [$(R times A) arrow.r B$],
  [the computation _consumes_ an environment to make a result], [the computation is _given_ the environment alongside its input],
  [`ask` is a textbook algebraic effect, handled by supplying the value], [the grade tracks _which_ context is demanded, per variable],
  [flexible dynamic reinterpretation (swap the handler)], [precise static, graded accounting (sum over a semiring)],
)

Same informal idea ("depends on an environment"), opposite structure. Algebraic effects buy
you *dynamic* power (handlers, continuations); coeffects buy you *static* precision (exact
usage, sensitivity, security levels). So even "reading" splits cleanly: a runtime
interaction is a monadic effect; a static context requirement is a comonadic coeffect.

= A catalog of effects <sec:effcat>

Effects are graded by a monoid; when the effect is a _set_ of things (exceptions thrown,
regions touched) that monoid is a join-semilattice (union, idempotent).

#[
#set text(size: 9pt)
#table(
  columns: (auto, 1fr, auto, auto),
  inset: 5.5pt, stroke: 0.5pt + luma(215),
  align: (left + top, left + top, left + top, left + top),
  table.header([*Effect*], [*Tracks*], [*Grading*], [*System*]),
  [State / mutation], [reads & writes; refined: which cells/regions], [monoid; read/write/region lattice], [mtl `State`, Koka `st⟨h⟩`],
  [Exceptions], [set of exceptions that may be thrown], [powerset (idempotent monoid)], [Koka `exn`, Eff, Java `throws`],
  [Nondeterminism], [may branch / many results], [powerset monoid], [list/`Set` monad, Koka `ndet`],
  [Probability], [sampling; refined: privacy budget $epsilon$], [monoid; $(RR_(>=0), #sadd)$ for budget], [distribution monad; Fuzz privacy monad],
  [IO / console / net / FS], [external interaction], [flat row of labels], [Koka `io`, Haskell `IO`],
  [Divergence], [may loop forever], [2-point monoid `total ≤ div`], [Koka `div`],
  [Partiality], [may produce no value], [`Maybe`], [`Maybe` / `Option`],
  [Delimited control], [captures & resumes the continuation], [— (the general mechanism)], [Eff, Koka, Frank, OCaml 5],
  [Generators / yield], [emits a stream of outputs], [row label], [Koka `yield`, OCaml 5],
  [Async / concurrency], [suspension, fibers, scheduling], [row label], [Koka `async`, OCaml 5 `eio`, Unison],
  [Reader (`ask`)], [reads an ambient value], [(often trivial) monoid], [Reader monad],
  [Writer / logging], [accumulates output], [*literally* the output monoid $(W, #mon, I)$], [Writer monad],
  [Free monad over a signature], ["these operations may be invoked"], [signature functor → free monad], [polysemy, fused-effects, `eff`],
  [STM], [atomic transactional memory], [monoid], [Haskell `STM`],
  [Regions / allocation], [which memory regions are touched], [*set of regions* (the original!)], [Tofte–Talpin; Lucassen–Gifford '88],
)
]

== Effect-handler languages, briefly

- *Koka* — _row-typed_ effects: `<exn, st⟨h⟩, console>` is a scoped multiset of labels that
  behaves like a monoid; the type of an expression names exactly the effects it may incur.
- *Eff* (Bauer & Pretnar), *Frank* (McBride & Lindley) — algebraic operations + handlers;
  Frank fuses operators and handlers so there is no separate `handle` keyword.
- *Unison* — _abilities_ are its effect system, row-like.
- *OCaml 5* — effect handlers via delimited continuations, but *unchecked*: there is no
  effect typing yet; that is open research.
- *Haskell* — `mtl` (transformer stacks; constraints-as-effects) and the free(r)-monad
  libraries `polysemy` / `fused-effects` / `eff`.

A tiny algebraic-effects example (Koka-flavored): the effect *is* the set of operations the
handler must interpret.

```koka
effect ask    { fun ask() : int }          // declares an operation
fun add-two() : <ask> int { ask() + ask() } // the type records the <ask> effect
fun main() {
  with handler { fun ask() resume(21) }     // a handler interprets it
  println( add-two() )                       // prints 42
}
```

= A catalog of coeffects <sec:coeffcat>

Coeffects are graded by a *semiring* — the column shows the natural resource algebra. The
three founding examples in the literature were *implicit/dynamic parameters*, *variable
liveness*, and *dataflow history depth*; all appear below.

#[
#set text(size: 9pt)
#table(
  columns: (auto, 1fr, auto, auto),
  inset: 5.5pt, stroke: 0.5pt + luma(215),
  align: (left + top, left + top, left + top, left + top),
  table.header([*Coeffect*], [*Tracks*], [*Semiring*], [*System*]),
  [Usage / linearity], [how many times a variable is used], [${0,1,omega}$ or $NN$], [QTT, Idris 2, Linear Haskell, Granule `Nat`],
  [Bounded reuse], [usage bounded by a count], [$(NN, #sadd, #smul, 0, 1)$], [Bounded Linear Logic; Petříček et al.],
  [Implicit params / dynamic scope], [which ambient params are required], [set lattice (idempotent)], [Scala `given`, Haskell implicit params],
  [Dataflow liveness / history], [how many _past_ values a stream needs (`prev`)], [$(NN, max, #sadd)$], [comonadic FRP; Petříček dataflow],
  [FRP causality], [depends on past, never future], [stream comonad], [comonadic FRP],
  [Information flow / security], [clearance required to read], [*security lattice* (Public ≤ Private)], [FlowCaml; Granule `Level`/`Sec`],
  [Sensitivity (privacy)], [output change per unit input change], [$(RR_(>=0) union {infinity}, #sadd, #smul)$], [Fuzz, DFuzz, Granule],
  [Hardware / capabilities], [required sensors / permissions], [capability-set lattice], [context-aware PLs (Petříček)],
  [Staging / binding-time], [metaprogramming stage of a value], [$NN$ levels (S4 $square$ = code)], [Davies–Pfenning modal staging],
  [Caching / memoization], [which past results to retain], [$NN$ (≈ history depth)], [dataflow coeffects],
  [Strictness / demand], [how much of the input is forced], [demand lattice (backwards)], [strictness analysis],
)
]

== Coeffects in working languages

*Quantitative Type Theory* (Atkey; McBride) grades every binding with a quantity from a
semiring; *Idris 2* uses the three quantities $0$ (erased), $1$ (linear), $omega$
(unrestricted). *Linear Haskell* uses multiplicities `One` / `Many` on the arrow:

```haskell
-- Linear Haskell: %1 means the argument is consumed exactly once
f :: a %1 -> (a, a)   -- TYPE ERROR: cannot use a linear value twice
f x = (x, x)
```

```idris
-- Idris 2: the 1 is a usage quantity (a coeffect), not a value
dup : (1 _ : a) -> (a, a)   -- rejected: linear var used twice
```

*Granule* is the language built around a configurable coeffect side. A grade is drawn from a
chosen *coeffect type* (semiring): `Nat` (exact usage), `Ext Nat` (with $infinity$),
`Interval` (usage ranges), `Level` / `Sec` (security lattices), rationals for *sensitivity*,
`Cartesian` (unrestricted), and *products* of any of these (track several at once).
Crucially Granule carries graded *necessity* $square_r$ (coeffects) *and* graded
*possibility* $diamond_f$ (effects, e.g. IO) in the same language — making it the practical
testbed for combining the two.

```granule
-- Granule: the [..] modality carries the usage grade; share is well-typed
-- only because the security lattice and Nat semiring track demand precisely.
push : forall {a : Type, n : Nat} . (a [n]) -> a [n]
```

= Combining them: graded distributive laws <sec:combine>

Now the substantial part. To put $T_e$ (effects) and $D_r$ (coeffects) in *one* calculus, a
term denotes a morphism that *consumes graded context and produces a graded effect*:

$ D_r thin Gamma arrow.r T_e thin A $

Composing two such morphisms forces you to *commute the comonad past the monad*. That move
is a *graded distributive law* — the central new idea of Gaboardi, Katsumata, Orchard,
Breuvart & Uustalu (ICFP 2016):

$ "dist"_(r,e,A) : D_(iota(r,e)) (T_e thin A) arrow.long T_(kappa(r,e)) (D_r thin A) $

It generalizes the ordinary $D T A arrow.r T D A$ by inserting two *monotone
grade-transforming functions*:

- $iota : R times E arrow.r R$ — how an effect modulates the *coeffect demand*;
- $kappa : R times E arrow.r E$ — how a coeffect modulates the *effect grade*.

(The canonical specialization takes $kappa = pi_2$, leaving the effect grade $e$ untouched.)

== Eight forms — this is the `□◇` vs `◇□` question

The framework enumerates *exactly eight* distributive-law formats,
$"FMT" = {"LL","LR","RL","RR"} times {"TD","DT"}$. The ${"TD","DT"}$ axis chooses the
*direction* (effects-over-coeffects vs. coeffects-over-effects); the
${"LL","LR","RL","RR"}$ axis fixes where $e$ and $r$ sit on the input vs. the output. This
is the formal home of the ordering question:

#table(
  columns: (1fr, 1fr),
  inset: 7pt, stroke: 0.5pt + luma(210),
  table.header([$square_n (diamond_f thin A)$], [$diamond_f (square_n thin A)$]),
  [$n$ copies of an $f$-effectful computation], [one effect yielding something usable $n$ times],
  [running all of them incurs $f$ #emph[n-fold]: $kappa(n,f) approx f #mon f #mon ... #mon f$], [$f$ is incurred *once*],
)

#intuition[
  *Replication multiplies effects.* If using a value $n$ times means running its effect $n$
  times, the coeffect grade $n$ shows up on the effect side as an $n$-fold monoid product.
  This is exactly what $kappa$ encodes — and why you cannot treat the two gradings
  independently.
]

== The soundness condition: a matched pair

A combination is sound *iff* $(iota, kappa)$ form an *$R,E$-matched pair* (a Zappa–Szép
product, borrowed from quantum-group theory), satisfying four coherence equations:

$
iota(r, e #mon f) &= iota(iota(r,e), f) \
iota(r #smul s, e) &= iota(r, kappa(s,e)) #smul iota(s,e) \
kappa(r #smul s, e) &= kappa(r, kappa(s,e)) \
kappa(r, e #mon f) &= kappa(r,e) #mon kappa(iota(r,e), f)
$

#warning[
  *This is where incompatibility lives.* A given effect/coeffect pairing combines soundly
  exactly when such a matched pair exists for your chosen $(R, E)$. When no $(iota, kappa)$
  satisfies these equations, there is *no* sound combined semantics. The obstruction is
  constructive: "no matched pair exists," not a single dramatic theorem.
]

= Compatibility and incompatibility <sec:compat>

== Pairings that compose cleanly

- *Usage / linearity $+$ state.* Linearity actively _helps_ state: a linear handle is freed
  exactly once, an array updated in place safely. This is Granule's bread and butter and the
  classic "linear types for resources" result.
- *Sensitivity $+$ probability (differential privacy).* The flagship combined system
  (@sec:dp). A coeffect (sensitivity) feeds an effect (privacy budget) through one
  controlled interaction point — the noise mechanism.
- *Information flow $+$ IO.* Combined in practice (FlowCaml-style); declassification and IO
  leaks are the delicate seam.

== Pairings that fight

- *Multi-shot continuations $+$ linearity.* A handler may invoke the captured continuation
  *0, 1, or many* times. That directly violates an "exactly once" usage coeffect unless the
  usage semiring is coarsened to absorb the multiplicity. A genuine, well-known tension
  between expressive handlers and linear resources.
- *Ordering is not free.* Because $square_n (diamond_f) eq.not diamond_f (square_n)$, you
  must commit to one of the eight formats; the "wrong" direction for your intended semantics
  simply has no matched pair.

== No-go results — read the fine print

The dramatic *impossibility* theorems are about *monad–monad* (effect–effect) composition,
not comonad–monad:

- *Probability does not distribute over nondeterminism* (Varacca–Winskel) — a famous,
  concrete effect $+$ effect incompatibility.
- Klin & Salamanca: iterated covariant powerset has *no* distributive law (MFPS 2018).
- Zwart & Marsden: broad classes of algebraic theories *cannot* be composed via a
  distributive law ("no-go theorems for distributive laws").

#warning[
  For the effect/coeffect (monad/comonad graded) case *specifically*, there is no published
  named no-go theorem I am aware of; the obstruction is characterized constructively by the
  matched-pair equations above. Treat Varacca–Winskel / Klin / Zwart as the cautionary
  backdrop — distributive laws are *not* guaranteed in general — and the matched-pair
  conditions as the concrete pass/fail test on the effect↔coeffect axis.
]

= Worked example: differential privacy end to end <sec:dp>

Differential privacy is the cleanest place where a coeffect and an effect *must* coexist and
*do* compose — the Fuzz lineage (Reed & Pierce, ICFP 2010; DFuzz, POPL 2013).

+ *Sensitivity is a coeffect.* "This query's output changes by at most $c$ units per unit
  change in one person's data" is a graded-comonadic property. In Fuzz it rides on the
  function arrow as a scaling factor; grades live in $(RR_(>=0) union {infinity}, #sadd,
  #smul)$ and compose by *adding* along data flow and *scaling* under function composition —
  a semiring, exactly as @sec:asymmetry predicts.

+ *Privacy cost is an effect.* Adding calibrated noise (sampling) is a *probabilistic
  effect* graded by the privacy budget $epsilon$ — a monoid $(RR_(>=0), #sadd, 0)$:
  sequential queries *add* their budgets.

+ *The mechanism is the interaction point.* The Laplace / Gaussian mechanism has, morally,
  the type

  $ "Mech"_epsilon : (c"-sensitive query") arrow.r diamond_(c #smul epsilon) ("noisy result") $

  It *converts a coeffect grade (sensitivity $c$) into an effect grade (privacy cost $c
  #smul epsilon$)*. That conversion is a concrete, sound instance of the grade-transforming
  $iota$ / $kappa$ — the demand-side measurement literally feeds the cost-side budget.

#intuition[
  You cannot express this in an effect-only system (no static handle on sensitivity) or a
  coeffect-only one (no probabilistic budget). It *needs* both, coupled — and the coupling
  is precisely a graded distributive law whose grade arithmetic is "multiply sensitivity by
  the per-query budget."
]

A Fuzz-flavored sketch (sensitivity annotations on arrows; the mechanism spends budget):

```text
-- counting query is 1-sensitive: changing one row changes the count by ≤ 1
count   :  Database ⊸₁ ℝ
-- the Laplace mechanism turns a c-sensitive query into an ε·c-private result
laplace :  (Database ⊸_c ℝ) -> Database -> ◇[ε·c] ℝ
-- composing two private releases ADDS budgets (effect monoid),
-- while sensitivity SCALES through the query (coeffect semiring)
```

= Cheat sheet

#table(
  columns: (auto, 1fr, 1fr),
  inset: 7pt, stroke: 0.5pt + luma(210),
  align: (left, left, left),
  table.header([], [*Effect*], [*Coeffect*]),
  [slogan], [what it _causes_ / produces], [what it _demands_ / consumes],
  [annotates], [the conclusion (result)], [the assumptions (variables)],
  [category], [graded monad $T_e$], [graded comonad $D_r$],
  [arrow], [$A arrow.r T_e B$], [$D_r A arrow.r B$],
  [algebra], [monoid (sequencing only)], [semiring (sharing $#sadd$ + nesting $#smul$)],
  [has contraction?], [no (monoid has no $#sadd$)], [yes ($"!"_(r #sadd s) arrow.r "!"_r times.o "!"_s$)],
  [example], [`readLine`, throw, mutate, fork], [usage count, sensitivity, security level],
  [poster language], [Koka (row effects + handlers)], [Granule / Idris 2 (graded modal types)],
  [combine via], [#table.cell(colspan: 2)[graded distributive law $D_(iota(r,e)) T_e A arrow.r T_(kappa(r,e)) D_r A$, sound iff $(iota,kappa)$ is a matched pair]],
)

#v(0.5em)
#line(length: 100%, stroke: 0.5pt + luma(180))

= Sources

Primary sources behind this note (the structural claims — grading algebras, the
distributive-law form, the eight formats, the matched-pair equations, the contraction
asymmetry — were cross-checked against the first two).

#set text(size: 9pt)
- Gaboardi, Katsumata, Orchard, Breuvart & Uustalu. *Combining Effects and Coeffects via
  Grading.* ICFP 2016. #link("https://kar.kent.ac.uk/57480/")
- Petříček, Orchard & Mycroft. *Coeffects: A Calculus of Context-Dependent Computation.*
  ICFP 2014. #link("https://www.doc.ic.ac.uk/~dorchard/publ/coeffects-icfp14.pdf")
- Petříček, Orchard & Mycroft. *Coeffects: Unified Static Analysis of Context-Dependence.*
  ICALP 2013. #link("https://link.springer.com/chapter/10.1007/978-3-642-39212-2_35")
- Petříček. *Context-Aware Programming Languages* (PhD thesis) /
  #link("https://tomasp.net/coeffects/")
- Orchard, Liepelt & Eades. *Quantitative Program Reasoning with Graded Modal Types.*
  ICFP 2019. #link("https://www.cs.kent.ac.uk/people/staff/dao7/publ/granule-icfp19.pdf")
- Granule project. *Deriving Distributive Laws for Graded Linear Types.*
  #link("https://granule-project.github.io/papers/distributive_laws_journal.pdf")
- Reed & Pierce. *Distance Makes the Types Grow Stronger* (Fuzz). ICFP 2010.
  #link("https://www.cis.upenn.edu/~bcpierce/papers/sensitivity.pdf")
- Gaboardi et al. *Linear Dependent Types for Differential Privacy* (DFuzz). POPL 2013.
  #link("https://inria.hal.science/hal-00909340")
- Leijen. *Koka: Programming with Row-Polymorphic Effect Types.*
  #link("https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/koka-effects-2013.pdf")
- Klin & Salamanca. *Iterated Covariant Powerset has no Distributive Law.* MFPS 2018.
  #link("https://www.cs.ox.ac.uk/people/bartek.klin/papers/mfps18.pdf")
- Zwart. *On the Non-Compositionality of Monads via Distributive Laws* (DPhil thesis).
  #link("https://www.cs.ox.ac.uk/files/12453/MaaikeZwartDPhilThesis.pdf")
