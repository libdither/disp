#set document(title: "Disp Optimizer — Unified Design (OPTIMIZER)")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set par(justify: true, leading: 0.62em)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 16pt, weight: "bold")
#show heading.where(level: 2): set text(size: 13pt)
#show heading.where(level: 3): set text(size: 11pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))
#show raw.where(block: true): set text(size: 9pt)

#let note(body) = block(
  breakable: false, above: 0.8em, below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")), inset: (left: 1em, y: 0.3em), body,
)
#let openq(body) = block(
  breakable: false, above: 0.6em, below: 0.6em,
  stroke: (left: 2pt + rgb("#cc6600")), inset: (left: 1em, y: 0.3em),
  text(weight: "bold")[Open question: ] + body,
)

#align(center, text(22pt, weight: "bold")[Disp Optimizer])
#v(0.3em)
#align(center, text(13pt)[Unified Design — a verified, self-improving program optimizer])
#v(0.6em)
#align(center)[Verified gradient descent on a graded cost over a materialized net.]
#v(1em)

#note[
  *Status.* Active design proposal. This document *consolidates* the prior optimizer stack
  into one coherent design and works out the connections between its layers. It folds and
  replaces, in full:
  `research/OPTIMIZER.md` (the reading-map + staging-axis framing),
  `research/EQUALITY_FOR_VERIFIED_OPTIMIZATION.md` (the equality layer),
  `research/OPERATIONAL_EQUIVALENCE_LICENSING.md` (the soundness/licensing relation),
  `research/VERIFIED_OPTIMIZER_IMPLEMENTATION.md` (the build-on-current-code plan), and
  `research/interaction-combinator/DISP_BACKPROP.typ` (synthesis-as-reverse-mode), plus the
  empirical `research/SUPERPOSITION_FINDINGS.md`. Those files are *deleted*; their detailed
  treatments — option-by-option equality analysis, the categorical/semiring development of
  reverse-mode, the per-test superposition results — remain recoverable in git history and are
  cited inline where this design compresses them. The *substrate* (the interaction-net
  reducer) keeps its own spec: `research/interaction-combinator/{tc-net.typ, RUST_IC_NET_DESIGN.md}`.
  Type-theory foundations stay in `TYPE_THEORY.typ` and `research/effects-and-coeffects.typ`.
  Open items are flagged inline and collected in §11.

  *None of this is built.* Most of the *substrate* exists (§3, §10); the optimizer is research.
]

= The design in one page

The goal (§2) is `GOALS.md`'s: write a specification as a type, turn the type checker into a
$0/1$ score, multiply by a cost/size score, and search the host calculus for a program that is
*both formally correct and efficient* — then turn that search on itself.

This design realizes it as one sentence: *the optimizer is verified gradient descent on a graded
cost over a materialized net.* Each word is a section.

- *Materialized net* (§3) — programs run on `rust-ic-net`, a materialized interaction-net
  reducer that drops hash-consing. That single choice — no automatic memo — is what makes cost
  *attributable per candidate* and reduction *provenance-preserving*; it is the substrate's reason
  to exist, and §5 shows it is forced.
- *Graded cost* (§4) — cost is one axis of a graded-coeffect/effect ledger it shares with usage,
  sharing, and staging. The cost *bound* is a coeffect (what a term demands); the cost *incurred*
  is an effect (what a run spends); the cost-soundness theorem links them — and that link *is* the
  guidance-vs-soundness split of §2.
- *Gradient descent* (§8) — synthesis by hole-filling is reverse-mode evaluation: a forward type-
  propagation DAG with a backward blame/credit functor, run by *superposition* over candidates.
  In the net's linear-logic reading the duplicator $delta$ is the exponential bang and the backward
  pass is its derivative — search is differentiating the cost over the net.
- *Verified* (§6, §7) — every accepted rewrite carries a *certificate* a tiny trusted checker
  re-validates, and is *licensed by an equality*. In a reflective calculus the only licensing
  relation that is not trivial is a deliberately coarse logical relation the *walker already
  defines*; cost-improving rewrites ride Sands' asymmetric improvement preorder.

One mechanism spans the whole timeline (§5): memoization, partial evaluation, JIT, AOT, and
code-generation are the *same* verified rewrite at different points of "how much input is fixed
when the rewrite is chosen." Self-applying it is the Futamura projection — the self-improvement
endgame. §9 takes the same mechanism *down* to effects and a modeled hardware target. §10 is the
build order; §11 the open problems.

*Trust model in one line.* The optimizer is arbitrarily clever and *untrusted*; a bad rewrite
either fails certificate-checking or fails an independent re-type-check. The trusted base is the
runtime's reduction relation, a ~30-line in-language checker, the rule library's soundness, one
binder-free congruence theorem, and the cost metric.

= The goal: specification → score → search

A *specification* is a type `T`. The type checker, applied to a candidate `e`, is already a
function returning a verdict; collapse it to $0/1$ (`param_apply T e == Ok true`). Multiply by a
*cost/size* score read from running `e` (§4). The product is the objective; the optimizer
searches the space of trees for an `e` maximizing it.

#note[
  $ "score"(e) = [#h(0.2em) "typechecks"(e, T) #h(0.2em)] times "cheapness"(e) $
  Correctness is a hard gate ($0$ kills the candidate); cost is the smooth signal that ranks the
  survivors. This is `GOALS.md`'s "external optimizer + scoring function," made precise.
]

Two uses of cost, kept rigidly separate (this distinction recurs everywhere):

- *Guidance (untrusted).* The *measured* cost of running candidates drives the search. Wrong
  measurements only make the optimizer pick a worse-but-still-correct program.
- *Soundness (trusted).* That a chosen rewrite *does not regress* comes from the rewrite's proven
  *improvement direction* (§6), never from a measurement.

The endgame is self-application: the optimizer is a disp program; given a meta-utility it can
optimize *itself*, then re-derive the calculus, the type checkers, and a hardware model
(`GOALS.md`). §5 identifies this precisely as the third Futamura projection.

= The substrate: a materialized net, and why it drops memo

The optimizer runs on `rust-ic-net` (spec: `tc-net.typ`; design: `RUST_IC_NET_DESIGN.md`), a
*materialized* interaction-net reducer — agents and ports are real arena nodes, active pairs sit
in a schedulable bag — not the default hash-consed reducer `rust-eager`. The difference is one
property: *ic-net does no automatic cross-occurrence memoization.* That looks like a 600× raw-speed
loss (measured); it is in fact the design's foundation. Two consequences:

- *Provenance.* Hash-consing merges equal-by-evaluation terms. The reverse-mode optimizer (§8)
  must attribute blame/credit *per candidate*; merging two candidates' shared sub-results
  *entangles* their costs (candidate B looks free because candidate A paid for the shared work).
  Keeping distinct-but-equal subterms distinct is the prerequisite for credit assignment.
- *Honest cost.* For the same reason, ic-net's interaction count is a *pure function of the
  program's structure*, not of session-global hash-cons state. The cost you minimize is the cost
  the program actually has, attributable to the decisions that caused it.

*The unification (the load-bearing claim).* The 600× slowdown, the preserved provenance, and the
honest, attributable cost are *one property seen three ways* — the absence of automatic memo, which
§5 reframes as "memo is the optimizer's job, not the substrate's." `rust-eager`'s hash-consing is
*borrowed* optimization the real system must make explicit and verified.

#note[
  *The honest cost boundary.* Interaction count is a *reasonable but space-biased proxy*, provably
  *not* wall-clock: token/GoI machines can be exponentially time-slower than an environment
  machine (Accattoli–Dal Lago, *The (In)Efficiency of Interaction*, POPL 2021). So (a) the net
  exposes *two* intrinsic readouts — interactions (a time proxy) and peak live nodes (space) —
  giving a 2-D cost vector for "efficient among the desired axes," and (b) *faithful hardware cost*
  is a *separate, coarser* axis — reached either by reducing a hardware *model* encoded in the
  calculus (§9), or, for the real thing, through `GOALS.md`'s measurement primitive — never read off
  the raw interaction count.
]

Strong confluence (`tc-net.typ`, Theorem 2) makes the net deterministic under any schedule — so
parallelism is free and the optimizer may reduce many candidates at once. *This is sacred:* §9
forbids any construct (a true nondeterministic `amb`/IO agent) that would break it.

= Cost as a graded coeffect (and its effect dual)

Cost is not a bolt-on metric; it is one axis of a single *graded ledger* the program already
carries. The frame (developed in `research/effects-and-coeffects.typ`): a computation's *effects*
(what it does *to* its context) form a graded *monad* on the conclusion; its *coeffects* (what it
demands *from* its context) form a graded *comonad/semiring* on the assumptions.

*Cost has both faces, and the optimizer needs both:*

- *The cost bound is a coeffect.* "This term runs in $≤ f(n)$ steps" is a resource grade on the
  *demand* side — a graded modality, exactly where *usage/linearity*, *sharing*, and *staging*
  ($square$ = code) also live (the coeffect catalog: QTT usage, Davies–Pfenning staging, Fuzz
  sensitivity, resource-bounded feasibility). They compose by a *semiring*: addition for
  contraction (a variable used in two places), multiplication for nesting/scaling under a modality.
- *The cost incurred is an effect.* "This run *spent* $N$ interactions" is a measurement on the
  *result* side — the net's interaction counter.

These are linked by a *cost-soundness theorem*: a term whose cost-bound coeffect is $≤ B$ has
incurred cost (effect) $≤ B$ (the Resource-Bounded Type Theory result). *This dual has the same
shape as §2's guidance/soundness split:* the incurred effect is the measured side that *guides*
(untrusted); a *proven* type-level cost fact is the trusted side that makes a guarantee *sound* — an
absolute bound here, or §6's relative improvement direction for no-regression. The formal
bridge between a coeffect grade and an effect grade is a *graded distributive law* (Gaboardi et
al., ICFP 2016) — a matched pair $(iota, kappa)$ whose archetype is the differential-privacy
mechanism "sensitivity-coeffect $c$ times per-query budget $epsilon$ gives privacy-effect
$c dot epsilon$."

*Why this matters operationally.* `RUST_IC_NET_DESIGN.md` §7 already names the consequence without
naming the frame: *one coeffect pair drives scheduler + sharing + GC.* The elaborator lowers the
grades onto the net's knobs — usage/sharing grade → duplicator species ($delta^0$ erase / linear /
$delta^n$ need / $delta^s$ structural); strictness grade → the strict/lazy seeding bit; and the
selective-RC. So cost, usage, sharing, staging, and the scheduler policy are *one graded structure*
the elaborator compiles to one set of net controls.

#note[
  *Use graded modalities, not cubical.* This whole ledger is *graded* (semiring-indexed), never
  *higher-dimensional*. Resist reaching for cubical/HOTT machinery here (§6): paths/transport are
  the wrong tool for a resource grade. The type theory you want is graded modal / resource-bounded.
]

= The staging axis: one rewrite, memo to assembly

Before the mechanism, the frame that unifies *when* it runs. *Memoization, partial evaluation,
JIT, ahead-of-time optimization, and code generation are the same verified rewrite* (§7's
`φ` + certificate) applied at different points on one axis: *how much of the input is fixed when
the rewrite is chosen.*

#table(
  columns: (auto, auto, 1fr),
  inset: 5pt, align: left, stroke: 0.4pt + rgb("#bbbbbb"),
  [*corner*], [*input fixed*], [*what it is*],
  [AOT optimization], [nothing (∀ inputs)], [the whole pipeline of this document],
  [partial evaluation], [some args], [a residual program specialized to the known args],
  [JIT], [the input, decided at runtime], [online specialization on observed data],
  [code generation], [the target stage], [a `φ`-rewrite whose object stage is a *hardware model* (§9)],
  [memoization], [the input, decided automatically + cached], [the *degenerate, zero-intelligence corner*],
)

`φ` is *stage-agnostic*: "swap `e` for a proven-equal cheaper `e'`." With no data → AOT; with some
inputs fixed → partial evaluation; with runtime facts → JIT; with the object stage a hardware
model → codegen. *Memoization is the same rewrite* — `f(x) ⤳ cached f(x)`, licensed by referential
transparency `f(x) ~ f(x)` — made *uniformly for every x, by the substrate, with no cost model and
no choice.* Three consequences:

- *Memo is the optimizer's dumbest rewrite — which is the deep justification for §3.* The substrate
  must not hardwire an optimization decision the optimizer should own. ic-net refuses the
  degenerate corner, so the optimizer owns the entire axis with honest, attributable cost.
- *The axis is closed under self-application.* Applying the optimizer to itself with the
  interpreter fixed is the *Futamura projection*; `GOALS.md`'s "have the optimizer produce an ideal
  version of itself" is exactly that — the *third Futamura projection*. JIT and self-improvement are
  one operation at different points.
- *It re-files two soundness questions as the online corner.* The AOT cost model (§4) measures on a
  *guessed* input distribution; the honest version measures the *runtime* distribution (the cost
  model *is* the online signal — `GOALS.md`'s measurement primitive). And a data-conditioned rewrite
  is valid only for *observed* data — a *refinement*, not an unconditional equivalence (§6, §11).

#note[
  *Honest scope.* In a pure calculus the memo *structure* (table-threading / self-memoizing code) is
  AOT-insertable as an ordinary verified rewrite; the genuinely-online part is the *decision* (where
  to specialize, informed by profile), not the mechanism. Running a full verified *search* per call
  is impractical — the realistic online form is staged, profiled decisions over the one `φ`-machinery,
  with the produced specialization itself cached. Staging's formal tool is the $square$=code coeffect
  (§4); the elegance is the unification, the engineering is staged decisions.
]

= Equality and licensing: what makes a rewrite sound

A rewrite `e ⤳ e'` is sound only if `e` and `e'` are *interchangeable*. The subtle part, specific
to disp: *which* equivalence licenses that.

== Contextual equivalence collapses to `tree_eq`

Tree calculus is *reflective*: a context `tree_eq [·] P` (or any `triage` on the hole) distinguishes
any two structurally-distinct trees. By Wand's "fexprs is trivial" theorem, sharpened to a
binder-free substrate, *contextual equivalence = literal structural identity = `tree_eq`*. This
*inverts* the textbook intuition: in a reflective calculus, contextual equivalence is the *floor*,
not the ceiling — every non-trivial rewrite is contextually *inequivalent*, so raw operational
equivalence licenses *nothing*.

== The walker is constitutive of the licensing relation

The licensing relation must therefore be a deliberately *coarser, observer-restricted* logical
relation `~_T` — one that quantifies only over *applicative* uses (observers may *apply* abstract
values, never *inspect* them). Disp already has the mechanism that defines exactly this observer
class: *the walker* (`param_walker`) rejects `triage`/structural inspection of kernel-minted
neutrals. That discipline *is* relational parametricity, so:

#note[
  *The walker is not merely a source of rewrite rules — it is constitutive of which equivalence
  exists at all.* Without the parametricity discipline restricting observers, intensionality would
  leave the optimizer nothing but `tree_eq`. Free theorems (parametricity) are then sound rewrite
  rules *for free*, and they supply the *white-box* half of representation-independence with no
  equality machinery — which is why cubical/univalence is mostly unneeded (below).
]

== Cost-aware: Sands' strong improvement

Equivalence preserves *behavior*; it does not certify *speed*. Three relations over (stripped)
trees:

#table(
  columns: (auto, 1fr), inset: 5pt, align: left, stroke: 0.4pt + rgb("#bbbbbb"),
  [`e ⊵~ e'`], [improvement preorder: in every sanctioned context `e'` reaches a value in ≤ as many steps],
  [`e ⊵~ₛ e'`], [*strong improvement*: `e ⊵~ e'` ∧ `e ≅ e'` — faster *and* behaviourally equal],
  [`e ◁▷~ e'`], [*cost-equivalence*: improvement both ways — provably same cost],
)

A symmetric *equational* calculus can change cost by at most a constant factor (Moran–Sands, POPL
1999). So a bag of cost-*equivalence* laws certifies only constant-factor speedups; to certify an
*asymptotic* win (O(n²)→O(n): accumulator/`reverse`, deforestation) the optimizer *must* use the
*asymmetric* improvement preorder *plus* improvement induction. This is non-negotiable for the
"produce an efficient program" goal.

#note[
  *Why asymmetry is forced (the counterexample).* `repeat x = x : repeat x` is equivalent to
  `tail (repeat x)`, but rewriting the recursive body via that equation yields `repeat' x = x :
  tail (repeat' x)`, which produces one element — productivity broken. Cost-awareness catches it:
  `tail (repeat x)` is *not an improvement*. Equivalence alone is unsafe for a recursive optimizer.
]

== Set-level by design

`~` / `⊵~` are *proof-irrelevant* (the witness is a meta-level bisimulation; in disp `refl := t`
erases). A proof-irrelevant equality is a mere proposition, giving definitional UIP, which
*refutes univalence*. So operational-equivalence licensing is inherently *set-level* (h-set,
0-truncated) — and this is *correct*: an optimizer's transformations are observational/relational,
never higher-dimensional. The dimensional ladder (if ever wanted) goes *up* from this floor; it is
not a competitor (2LTT framing, below).

== The equality stack, prioritized

The provable-equality space is enlarged in priority order; soundness rests on the *operational*
route (C), not on the logic (disp is `Type:Type`-inconsistent *as a logic*, which does not reach
the semantic relation that licenses rewrites):

+ *(A) Observational equality / funext* — a per-type `eq` meta-field (Bool/Nat structural, Π via
  `bind_hyp`-funext, Σ/Record componentwise). The single biggest enlargement, and a small local
  edit. Lets funext-level rewrites be *provable*.
+ *(B) The `φ` zero-cost cast* — the verified-rewrite primitive. Given `e : HEq T t1 t2` and
  `t1 : T`, `φ` types at `T` but *erases to t2*: `|phi T t1 t2 e| = |t2|`. Because disp erases and
  conversion is hash-cons, `φ` is operationally the identity onto `t2` — verified optimization in
  one combinator. It *requires* a richer (and ideally *heterogeneous* — the cheap form may inhabit
  a different predicate) equality than `tree_eq`; if `t1 ≃ t2` were `tree_eq` they'd be the same
  node with nothing to optimize.
+ *(C) Operational-equivalence licensing* — `~_T` (above) is the soundness floor that keeps the
  optimizer sound despite `Type:Type`. Rules are proven `~`/`⊵~ₛ` in a *total fragment*.
+ *(D) Cubical/HOTT — mostly not needed.* Representation-independence is *parametricity*
  (white-box), which the walker already enforces; univalence is only for *black-box transport along
  a named equivalence*, the rare case. If ever needed, *HOTT* (observational + univalence, no
  interval) — not full cubical — is the fit. (See §11: catalogue whether any target rewrite truly
  needs it.)

*Landed (2026-07-05).* (A) and (C) have first implementations in `lib/std/oeq.disp`: `setoid_of`
derives each type's equivalence from its cells (pointwise at Π = funext by definition,
componentwise at non-dependent records, `Eq` at data; a declared setoid in `behavioral_specs`
overrides = quotients) — it lives in the meta a type already carries rather than a new `eq` field,
and the pointwise licenses are exactly `~_T` restricted to applicative observers. The
motive-extensionality obligation is operational: the `ext_walker` probe
(`lib/tests/ext_gate_proto.test.disp`), a `param_walker` variant whose intensional carve-outs
refuse neutrals — the per-motive residue of the fundamental lemma. The licensed-*replacement*
mechanism exists as the elaborator's guard layer (COMPILATION.typ § Declarations as requests):
`license_guard R` makes redefinition of an owned name demand `proof : R old new`, re-verified at
every load. End-to-end walkthrough pinned in `lib/tests/oeq_tree_license.test.disp` (the
deep-recognizer-to-`Ok true` rewrite, licensed by tree induction). Still open here: (B) `φ` as a
term-level cast (replacement is definition-level today), cost-aware `⊵~ₛ`, and dependent-family
transport (the §13 coe rung).

#note[
  *2LTT is the equality layering, not staged compilation.* Disp is already two-level in spirit:
  the strict level = `tree_eq`/hash-cons definitional equality (the optimizer's fast path); the
  richer level = observational `~` where funext/representation-swaps need it. Keep `tree_eq` as the
  common-case fast path; pay for the richer equality only where a rewrite needs it. (Do not confuse
  this with the §5 staging axis, which is the *compile-time/runtime* axis.)
]

= The rule library and the certificate

== Rules are parametric programs

A rule is *a function from its metavariables to its `(lhs, rhs)` pair* — schematicity rides bracket
abstraction, instantiation is application, the two sides are projections. No separate pattern
language:

```disp
// map fusion: first-order in xs, higher-order in f,g
map_fusion := {f, g, xs} -> pair (map f (map g xs)) (map (compose f g) xs)
RuleEntry  := { name : String, rule : Tree, arity : Nat, kind : RuleKind, witness : Tree }
RuleKind   := CostEq | StrongImpr        // CostEq reversible; StrongImpr one-way
```

Under hash-consing in the *checker's* reducer, `map_fusion inc dbl xs` reconstructs the *exact*
tree of an occurrence of `map inc (map dbl xs)` (deterministic elaboration → identical tree), so
*matching the instantiated rule is `tree_eq`, not unification.* The optimizer must still *find* the
instantiation (e-matching, its untrusted problem); the checker only verifies a *given* one.
First-order + parametricity-free-theorem rules need no higher-order search and come first;
higher-order rules (`map_fusion`'s `f,g`) need Miller-pattern matching in the optimizer, where
incompleteness is harmless (missed rules = missed speedups, never unsoundness).

== Where rules live

- *Per-type laws → the inert `behavioral_specs` meta-field*, finally activated for the role the
  type theory imagines: `List`'s laws on `List`, `Nat`'s arithmetic on `Nat`.
- *Cross-type laws (β/η, `id`, `compose`) → a global `lib/opt/rulebook.disp`.*

== The certificate and the in-language checker

The pipeline is an untrusted producer feeding a trusted gate:

```
   e : T
     │
     ▼
   OPTIMIZER  (host · UNTRUSTED — may be neural, buggy, wrong)
     · e-graph over tree-ids · apply rule library · saturate · extract cheapest
     · cost = measured session stats           (guidance only)
     │
     ▼  emits (e', certificate)
   CHECKER  (TRUSTED · ~30 lines · in-language)
     1. check_cert      e ⤳* e'                (each cert step matches a rule)
     2. re-type-check   param_apply T e' = Ok true   (belt & suspenders)
     accept ⇒ φ emits the bare e'              (zero runtime residue)
```

A bad rewrite — from any bug in the optimizer, the e-matcher, or the neural policy — either
fails `check_cert` or fails the independent re-type-check. Nothing the optimizer does can
produce an unsound or ill-typed result; the optimizer is therefore *fully outside* the trusted base.

An egg-style flat explanation: navigate to a redex, apply a rule instance, repeat.

```disp
Dir  := DL | DR | DStem            // fork-left, fork-right, into-stem
Step := { rule : String, path : List Dir, subst : List Tree }
Cert := List Step

check_step := {book, t, s} ->
  let lr := apply_all (lookup book s.rule).rule s.subst in   // rule m1..mk -> (lhs,rhs)
  if (tree_eq (at s.path t) (pair_fst lr)) then Ok (replace s.path (pair_snd lr) t) else Err
check_cert := {book, e, cert} -> foldM check_step (Ok e) cert
verify_rewrite := {book, e, e', cert} ->
  match (check_cert book e cert) { Ok t => tree_eq t e'; Err => false }
```

(`at`/`replace` walk and splice a `Path` over the tree by folding over `Dir`; `apply_all`
instantiates the rule program on its substitution list.) It is decidable, total under budget, ~30 lines, and — being a disp program — itself
type-checkable, satisfying the metacircular discipline. The acceptance combinator `φ` (an
elaborator construct, wired like `Pi`/`tree_eq`): *verify the certificate*, *re-type-check the
result independently* (`param_apply T e' == Ok true` — belt and suspenders), *emit the bare `e'`*
(zero residue). The optimizer pipeline needs no surface `φ`; it hands `(e', cert)` to the build,
which runs the same two checks.

== The soundness witness — three tiers (a staging device)

+ *Tier 0 — reduction laws.* β, η, K/S, `id x ⤳ x` *are* the reduction relation; captured by
  `tree_eq`-after-reduction, no rule needed. `witness = reduction`.
+ *Tier 1 — test-certified.* A battery of `test` cases checked at elaboration. A strong filter, not
  a proof; *in the trusted base until promoted.* How M0/M1 ship rules fast.
+ *Tier 2 — proof-certified.* `witness : Eq_obs T lhs rhs` (observational equality, §6-A) and, for
  cost, `witness_cost : Improves T lhs rhs` (§6 Sands). Machine-checked by the existing type
  checker; the rule leaves the trusted base.

= The search: superposition as reverse-mode

Hole-filling synthesis *is* reverse-mode evaluation — the same shape as backpropagation and CDCL,
parameterized only by the carrier and the feedback semiring (the categorical development is in
git history — the former `DISP_BACKPROP.typ`). Disp already has the *forward* DAG (type propagation along application
spines); synthesis adds the *backward* slot: a functor carrying blame/credit from a goal failure
back to the responsible hole, accumulating *learned conflict clauses* (NeoGen-style).

*The loop.* (1) Enumerate per-hole candidate streams (type-correct by construction, ordered by a
prior — size, library frequency, an LLM's logits). (2) Superpose: replace hole $i$'s hypothesis
with nested `sup` agents over its candidates. (3) Reduce the goal recognizer over the superposition.
(4) *Collapse* — read back the candidate combinations whose leaf is `true`. (5) On empty collapse,
*reify the conflict* (an instrumented checker returns a witness — a tree position + expected/actual
type — the conflict graph), and learn a predicate that prunes the search.

== Superposition is the parallel, provenance-preserving search engine

`sup_λ(a,b)` is a labeled superposition — $n^k$ candidate combinations *shared* in one net, the
common structure reduced once, forking only at the holes; the non-hash-consed net keeps the
branches distinct for blame (§3). Categorically it is the *confluent* realization of the
nondeterminism effect ("run all branches, share the work"), *not* `amb` (§9).

In the net's linear-logic reading, $delta$ is the exponential bang `!` — and a *differential
category*'s derivative $partial$ acts on `!`. So the backward/blame pass is morally $partial$ of the
bang: *search is differentiating the graded cost over the net*, where the same `!` = $delta$ is
sharing, the effect modality, and the thing differentiated. (The comonoid-law obligation this
incurs is §11.)

== What testing the engine established

A prototype on the real substrate (branch `research/sup-prototype`; the empirical record was
`SUPERPOSITION_FINDINGS.md`) settled the two load-bearing conjectures:

- *Conjecture 1 (collapse soundness): holds.* `collapse(reduce(sup(a,b))) == {nf(a), nf(b)}`.
- *Conjecture 2 (the type checker distributes over superposition): holds only for AFFINE
  recognizers.* A recognizer that uses the candidate more than once *cross-products* — the
  $delta^n$ that duplicates the candidate *commutes* with the sup, so the two uses range
  independently over the branches, producing phantom "branch-1 × branch-2" combinations that
  correspond to no single candidate. *Neither rule gives the diagonal*: commute → cross-product,
  annihilate → positional unzip.

*The design's stance.* For the common case the cross-product is a *readback* cost (the NF stays
linear; the host enumeration is $2^k$), and *correlated collapse* — enumerate only the consistent
per-label assignments — recovers the diagonal `{f(v1), f(v2)}` *soundly, at readback, with no
net-level cells, keeping both worlds.* This is the default. An optional var-style *choice cell*
(a write-once register every duplicator of a label follows) prunes the cross-product *during*
reduction — useful only when it is reduction-materialized (chained uses) — at the cost of one
world per pass; it is deterministic/confluent but not needed for soundness. The genuinely open
piece is *label-coordinated duplication* for the non-affine case (§11).

#note[
  *E-graph vs. superposition — opposite ends of the merge↔distinct axis (§3), complementary not
  redundant.* Both pack many terms into one structure, and the structural map is real: an e-class is
  a "sup" of forms, and e-graph *congruence* (`a ≡ b ⟹ f(a) ≡ f(b)`) is exactly sup *distribution*
  (`f(sup(a,b)) → sup(f(a),f(b))`). But they sit at opposite poles. An *e-graph MERGES* semantic
  equivalents (union-find over e-classes, on the *hash-consed* backend) — maximal sharing is its
  power; a *superposition KEEPS candidates distinct* (no merge, on ic-net) — provenance is *its*.
  So an e-graph's three efficiency sources — *id-sharing*, *union-find merge*, and *cycles*
  (recursive/infinite equivalence classes held finitely) — are exactly what ic-net drops; you would
  *not* build an e-graph via sup over ic-net (it loses all three and re-materializes everything).
  And their *construction* differs: an e-graph grows by *applying rewrite rules* (equivalences); a
  sup is built by *enumerating candidates at holes* (choices). Different tools for different phases:
  the e-graph saturates *rewrites* (M0, rust-eager) to find the cheapest *equivalent*; superposition
  searches a *candidate space* with blame (ic-net) for reverse-mode synthesis.
]

#note[
  *Can they be merged?* ic-net cannot *natively be* an e-graph — linearity (one principal port,
  variables occur twice) forbids the structural id-sharing — but it can *hold* one as *data*: a
  flat `class-id → {e-nodes}` table where sharing and cycles become *logical* id-references in a
  linear tree (no structural pointer cycle). The obstruction is then not representation but
  *purity*: an in-language e-graph gets *persistent* structures, not the *mutable* union-find +
  hashcons an imperative e-graph runs on, so it is correct-but-slow — and ic-net (no-memo,
  sequential-hostile for these ops) is the *worst* host; the e-graph data belongs on the hash-consed
  backend. The real merge is the *bootstrap* (§10): the e-graph as a tree-program *spec* compiled
  to a fast native implementation (a verified pure→imperative rewrite), so "everything is a tree
  program" *without* ic-net being an e-graph. And the merge↔distinct tension is *only at the search
  frontier*: an e-graph *with explanations* (proof-producing saturation) merges equal terms *and*
  records why — and that explanation *is* the certificate (§7). So merge what is *proven equal*
  (compact, certificate-bearing) and keep distinct what is *still being chosen* (superposition,
  blame-bearing); the tightest hybrid — an e-graph with superposition-frontiers at its open leaves —
  is speculative but coherent.
]

= The low-level frontier: effects, machines, and the gradient

The same machinery extends *downward* to effectful programs and a modeled hardware target. The
through-line: *a program, an effect, and a machine model are the same kind of tree; running,
handling, and simulating-hardware are the same net interaction — a pure consumer agent reducing an
inert producer that reifies "program as data."* This is the Reynolds/Danvy chain (monadic
evaluator → CPS + defunctionalize → abstract machine → interaction rules) made structural;
`tc-net.typ` already states its premise ("the same constructors represent code and data"). Three
unifications carry this frontier: *one shape* (program = effect = machine, here), *one ledger* (cost
as a graded coeffect, §4), and *one derivative* (search as differentiation, §8).

== Effects are values, which makes effectful programs optimizable

`TYPE_THEORY.typ` §15: `postulate` is removed; effects are a library *free monad `Eff R X`*
interpreted by *deep handlers*, with *one impure driver at the program boundary*; the substrate's
purity forces effects to be *values, never dispatch targets.* Consequences for the optimizer, all
from "effects are inspectable data":

- *Superposable.* `sup` can range over effectful candidates; the optimizer searches effectful
  programs exactly as pure ones.
- *Attributable.* Provenance attaches to individual effect operations (no merge).
- *The driver is the collapse + measurement boundary.* You may superpose effect *plans* (pure
  data) and optimize over them, but you cannot *perform* superposed real effects (you would run
  both branches). The single impure driver — which is precisely `GOALS.md`'s "outsource execution
  to a faster external language, get output + time + memory back" — collapses to the winner and
  runs *once*. The single-driver design quarantines impurity at exactly the point where
  superposition must collapse.

#note[
  *Never make the net effectful.* A true nondeterministic `amb`/IO agent destroys strong confluence
  (`tc-net.typ` Theorem 2) — the whole parallelism and determinism guarantee. Effects stay values;
  superposition is the *confluent* share-all-branches dual of nondeterminism, which is sound only
  because the walker treats `sup` as neutral *and* effects are not dispatch targets (both guards).
]

== A hardware model is a tree program; its cost is its output

`GOALS.md` wants a deterministic hardware/assembly model encoded *in the calculus* — an ordinary
tree program `run_model : Asm → Input → (Output, Cost)`. Keep three levels distinct: the *host
evaluator* (`rust-eager`/`rust-ic-net`, the only thing "built in" — it reduces every tree), the
*model* (a *pure* library function that simulates the CPU), and the *assembly* (a tree of type `Asm`
the model interprets). Then:

- *Running assembly is pure reduction* — "run `asm` on `x`" = reduce `run_model(asm, x)` on the host
  evaluator; the simulation *is* the reduction. Not an effect, and not an evaluator feature — the
  model is library code the generic engine reduces like any other tree.
- *Cost is the model's explicit output* — the modeled cycle count `run_model` *computes*, *not* the
  §4 intrinsic interaction count, which measures the cost of *simulating*, not what the simulated
  hardware costs.
- *Correctness is a type* — `Refines spec asm := ∀ x. fst (run_model asm x) ≡ spec x`, checked
  symbolically (reduce the model, compare), never by running on silicon. This is *why* the model
  must exist: real hardware is opaque (measurable, never provable); a model is type-checkable.

Codegen is then the §5 axis's bottom corner: a `φ`-rewrite swapping a high-level term for an
assembly-model term proven equivalent via the model's semantics; AOT- and JIT-codegen differ only in
how much input is fixed.

*The payoff, and the loop closing.* Because ic-net keeps candidates distinct, provenance attributes
*modeled-hardware cost per decision* — "this design choice cost $N$ cycles on the encoded CPU." That
is reverse-mode credit assignment (§8) operating at the hardware level, and it *requires* §3's
no-memo design. The 600× slowness, the provenance, the cost-as-coeffect ledger, and the
hardware-cost gradient are one property.

#note[
  *Four ways to cost a candidate, and the invariant that keeps them safe.* (0) *Static grade* — the
  §4 cost coeffect, read off by type-checking, no run at all. (1) *Intrinsic* — interaction count to
  reduce it on the net: pure, crude. (2) *Modeled* — `run_model`'s explicit cycle output: pure and
  *verifiable* (a provable bound), but you pay to simulate. (3) *Measured* — run it on real silicon
  via the driver above (`GOALS.md`'s "outsource execution, get time + memory"): an *effect* — fast,
  real, but non-deterministic and *untrusted*. The invariant: *soundness* (correctness + no-regression)
  is always symbolic — it never runs a candidate, so it never needs an effect; only *guidance*
  (ranking) runs candidates, and guidance is untrusted, so rung 3 is safe there. Practically: rank by
  the cheap static grade in the inner loop; reserve simulation/measurement for finalists. The bootstrap
  (§5) makes the *verifiable* rung fast — the optimizer compiles `run_model` itself to native, so
  modeled cost runs at native speed; the real-silicon effect then only ground-truths the model or
  prices what it omits. *The net never becomes effectful:* real measurement is the library driver
  compiled to a syscall, never a net primitive.
]

= Build order

Each milestone is shippable and raises trust. The optimizer milestones (M0–M5) reuse the existing
codebase; the *substrate* and *search* tracks run alongside.

- *M0 — empirical superoptimizer. Zero kernel change; buildable now.* Host e-graph (TS, untrusted)
  over the runtime's canonical ids; a Tier-1 (tested) rule set; extract by *measured* cost via the
  Session ABI; validate output by re-running the program's tests + `param_apply`. Translation
  validation — sound w.r.t. tests and types, not yet `~`. Proves the pipeline + cost model.
  *Concrete build plan: `OPTIMIZER_DESIGN.md`* (the language decision is TypeScript; rationale there).
- *M1 — certificate checker + rule book (in-language).* `lib/opt/{rulebook,checker}.disp` (§7);
  rewrites carry certs the kernel validates. Soundness rests on rule-book soundness (Tier-0/1) +
  checker correctness + Howe congruence (assumed). *(Partially landed 2026-07-05: the license TYPE
  (`oeq`, std/oeq.disp), Tier-2 witnesses (the Q1 proofs), a prototype cert checker
  (`opt_q1_cert/m1real`), and the licensed-replacement mechanism (the guard layer — owned names
  whose redefinition demands equivalence proofs, enforced at every load) exist; the standing
  in-language rulebook/checker modules remain.)*
- *M2 — `φ` cast (elaborator).* Wire `phi`: verify cert + re-type-check + emit `e'`. User-facing
  zero-cost swaps; the optimizer gets its kernel-blessed accept step.
- *M3 — observational `Eq` (funext) + Tier-2 rule proofs.* Add the `eq` meta-field (§6-A); re-prove
  the rule book as `Eq_obs`. Rule soundness becomes machine-checked; rules leave the trusted base.
- *M4 — cost-aware improvement (Sands).* Port the Improvement Theorem to *eager* tree-calculus
  reduction (pick the metric — `sRules + triageForkRules` for eager, *interactions* for ic-net;
  re-derive the context lemma + improvement induction). Enables asymptotic, no-regression rewrites.
  *The single genuine theory port.*
- *M5 — Howe congruence for tree calculus* (binder-free ⇒ easy) into the trusted base; the
  dimensional upper deck (§6-D) only if ever needed.

*Search track.* `sup_λ` prototype + correlated collapse *done* (`research/sup-prototype`);
label-coordinated duplication next (gates non-affine recognizers); then the optimizer tiers
(`foldMany` → `sup`/`collapse` → `reduceWithWitness`) and the conflict-learning loop (§8).
*Substrate track.* ic-net through M2d done; the cost readouts (interactions + peak nodes) and the
graded-knob lowering (§4) are the next substrate work. *Staging track.* AOT is M0–M5; the online
corner (§5) is future, gated on the measurement primitive and the refinement relation (§11).

#note[
  *The bootstrap (no chicken-and-egg).* The first optimizer (M0) is untrusted *host* code — already
  native, not a tree program — so nothing slow optimizes itself to get started (the self-hosting-
  compiler seed). The optimizer-as-a-*tree-program* appears only later, for two reasons that are *not*
  speed: so it can *read and improve itself* (it operates on trees, so to optimize itself it must
  *be* one), and so the stack is verifiable. That tree form is never run interpreted in the loop —
  the *current native* optimizer compiles the *next* source to native (`O_native(O') = O_native'`), a
  sequence of build-time compiles; worst case without the host seed is a *single* slow self-compile
  (pay once, native forever). Trust is laundered through the *checker*, not the seed: the bootstrap
  producer may be an arbitrary hack, yet every output is certificate-checked, so an untrusted fast
  seed yields a *verified* fast optimizer. The optimizer thus lives in two corresponding forms — a
  tree-program *spec* (verifiable, self-readable, what is improved) and a native *implementation*
  (fast, what runs) — exactly a self-hosting compiler, plus type-checking and cost.
]

#note[
  *Incrementally, not monolithically — and it automates the project's core discipline.* Self-improvement
  is not one whole-optimizer recompile; it is a chain of *verified fragment swaps*. Each replaces one
  definition `f` with a proven-equivalent cheaper `f'` — an ordinary `φ`-rewrite (§7) pointed at the
  optimizer's *own* body, gated by the same checker, and *targeted* by self-profiling (the net's
  per-candidate cost attribution, §3, says which fragment is hot). Two kinds: *algorithmic* (a better
  algorithm, verified by observational equivalence, §6) and *compilation* (native codegen, verified by
  the hardware model, §9). In a pure system this is incremental *versioning*, not in-place mutation —
  each version is one cheaply-verified, behavior-preserving fragment better than the last, so
  improvements compose and cost falls monotonically (to a *local* optimum; the superposition search, §8,
  is for bigger jumps). This is disp's own discipline — "the object language is the spec; host
  implementations are optimizations" — *automated and proven*: today a human hand-writes a fast path
  (`tree_eq`, the elaborator's `compileExpr`) and differential-tests it; the optimizer *generates* the
  fragment and *certifies* the equivalence. The hand-built spec/fast-path elaborator is the manual
  rehearsal of this loop.
]

= Open questions

The genuinely unresolved core, ordered roughly by how load-bearing.

#openq[
  *Label-coordinated duplication (the load-bearing one).* Conjecture 2 fails for non-affine
  recognizers (§8). Can a recognizer's internal $delta$ be made to carry the hole's label so it
  *annihilates* (correlates) rather than *commutes* (cross-products)? Annihilation alone gives the
  positional unzip, not the diagonal — so the fix is subtler than "share the label." This decides
  whether tier-2/3 superposition is sound for the recognizers that matter (relational / project-
  twice). The `research/sup-prototype` harness is ready to test candidates. *Direction:* gate
  superposition to affine / readback-correlatable recognizers and *enumerate* candidates for the
  non-affine ones (correct, just slower) — that ships now; the full fix (a duplication discipline
  yielding the diagonal for non-affine recognizers) is the open research.
]
#openq[
  *The affine fragment as a (valuated-)matroid search island.* The boundary where Conjecture 2 fails —
  non-affine / project-twice recognizers, where the recognizer's $delta$ duplicates — is the *same*
  boundary where matroid exchange fails: shared subterms break the exchange property that makes
  greedy/local cost-extraction optimal (the reason e-graph extraction is NP-hard and non-submodular).
  On the *affine* (bang-free) side this cuts two ways. *Feasibility:* an affine, *first-order* recognizer
  lives in the decidable part of linear logic — inhabitation is NP-complete multiplicatively (Kanovich),
  PSPACE-complete with additives (Lincoln–Mitchell–Scedrov–Shankar); the bang `!` (= $delta$, §8) is the
  LL undecidability cliff, and dependent $Pi$-over-`Tree` is undecidable *independently* of affineness.
  So the affine-and-first-order fragment is a complete, terminating synthesis island (guess-and-check,
  verifier-cheap) on a boundary disp already computes (project-once vs -twice) — a principled split
  between a decision procedure and neural search. *Optimality:* among feasible affine candidates, if the
  cost coeffect (§4) is *valuated-matroid*-shaped (a tropical / min-plus Plücker vector — Dress–Wenzl,
  "a new look at the greedy algorithm"; Murota M-concavity), greedy extraction is optimal in poly time.
  *Open:* does disp's affine condition land *exactly* in the decidable LL fragment (needs the $delta$ =
  bang identification of §8 made exact), and does any wanted cost grading meet the valuated exchange
  axiom? Refs: Rado–Edmonds (greedy optimal for all weights iff matroid); a telescope's admissible
  binding orders are the antimatroid of linear extensions of its dependency DAG (Korte–Lovász–Schrader).
]
#openq[
  *The eager Improvement Theorem (M4).* Sands is call-by-name/need with a function-call metric;
  disp is eager. Pick the metric and re-derive the theorem + a context lemma + improvement induction
  for the eager strategy. Feasible (a CBV improvement theory exists, Sands 1997) but the one real
  theory port. *Direction:* fix the metric (`sRules + triageForkRules`) and re-derive the context
  lemma + improvement induction for eager reduction; deferrable to M4 — only *asymptotic* /
  no-regression guarantees need it.
]
#openq[
  *`~_T` concretely, and is it the Type:Type consistency relation?* Define the licensing logical
  relation over stripped trees so it (a) meshes with the walker's neutral/escape discipline, (b) is
  sound for the applicative observer class, (c) hosts the `behavioral_specs` rule library. The
  sealing-framework / step-indexed-LR conjecture (`TYPE_THEORY.typ` §11.6) suggests `~_T` and the
  Type:Type consistency argument may be *the same* logical relation — if so, soundness of the
  optimizer and consistency of the (semantic) kernel are one proof. *Direction:* build it as a
  step-indexed logical relation over the walker's observer class (the sealing recipe) — likely the
  *same* relation as the PER model below, so prove the two together.
]
#openq[
  *Howe congruence for tree calculus.* `~`/`⊵~` must be a *congruence* for step-wise rewriting to
  compose. Binder-free tree calculus makes the hard clause vanish, but the proof must be discharged
  to put it in the trusted base. *Direction:* run Howe's precongruence-candidate construction;
  binder-freeness deletes its one hard case — the most tractable item here, clearable soon.
]
#openq[
  *The differential-category comonoid laws.* The §8 reading ($delta$ = bang, backward = $partial$)
  needs $delta$ to satisfy the comonoid laws a differential category requires. Does the walker's
  parametricity discipline force them (the former DISP_BACKPROP's Open-Q 6, in git), or a counterexample?
  *Direction:* write the comonoid laws and check `δ` against them — verify or refute; *low stakes*,
  since if they fail "search = differentiation" is merely a metaphor and the optimizer is unaffected.
]
#openq[
  *Refinement vs equivalence vs improvement.* Some transforms are not equivalences: dead-code
  elimination of a *diverging* unused subterm changes termination — a *refinement*, not `~`. And a
  data-conditioned (online, §5) rewrite is valid only for observed data — also a refinement.
  Catalogue which transforms need which of {cost-equivalence, strong-improvement, refinement}; the
  architecture must carry all three. *Direction:* a taxonomy, not a proof — tabulate
  transform-kind → relation, adopt a standard approximation preorder for the divergence-changing
  cases, and surface the choice in the rule's `kind` field; clearable soon.
]
#openq[
  *Does a sound graded distributive law exist for disp's grades?* §4 couples cost, usage, and
  staging via a matched pair $(iota, kappa)$. Such a pair is *not* guaranteed (Gaboardi et al.);
  the matched-pair equations are the concrete pass/fail test. Work them out for disp's specific
  $("cost", "usage", "staging")$ semirings, or find the obstruction. *Direction:* a concrete algebra
  check; if no sound matched pair exists, keep the grades *separate* (lower each onto the net's knobs
  independently) — the unification is elegant, not required.
]
#openq[
  *A disp realizability / PER model.* The consistency anchor. Cedille's and Nuprl's proofs do not
  transfer mechanically; the sealing / step-indexed-LR recipe is the candidate. It must be carried
  out at least informally before the optimizer is "verified" rather than "empirically sound."
  *Direction:* the step-indexed realizability model from the sealing framework (= the `~_T` question
  above); deferrable, since M0–M3 are empirically sound (tests + certificates) before it exists.
]
#openq[
  *Heterogeneous `Eq` / `~` over stripped trees.* `φ` needs a *heterogeneous* equality (the cheap
  form may inhabit a different predicate). Exact formation + the strip clauses + non-leakage through
  the walker are unworked. And `φ`'s zero-cost story has a hard limit: when representations have
  *genuinely different runtime structure* (unary vs binary `Nat`, list vs tree) there is no identity
  coercion — the best is an ordinary coercion that does real work. Do not oversell zero-cost.
  *Direction:* define `HEq` over erased forms with the strip clauses, routed through `param_apply`
  for non-leakage; where runtime structures genuinely differ, accept an ordinary (non-identity) coercion.
]
#openq[
  *Modeled-hardware fidelity, and the online corner's feasibility.* How faithful must the encoded
  hardware model (§9) be before its modeled cost tracks real cost, given interaction count is only a
  space-biased proxy? And is the online corner (§5) ever worth the staged-decision machinery over
  just emitting self-memoizing code AOT? *Direction:* empirical — build a model, measure its
  divergence from real silicon and iterate fidelity; prototype profiled specialization and compare
  it to AOT self-memoizing code. Gated on the measurement primitive + a first model.
]
#openq[
  *Does the optimizer ever need black-box univalence?* Concretely: is there a target optimization
  expressible *neither* as a `~`-equation *nor* a white-box (parametric) free theorem, that *requires*
  transport along a named non-identity equivalence? If not, cubical/HOTT (§6-D) stays off the roadmap
  permanently. *Direction:* try to exhibit one such optimization; the parametricity argument predicts
  none, which *closes* the question (cubical off permanently).
]
