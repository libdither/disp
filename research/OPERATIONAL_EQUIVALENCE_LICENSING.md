# Operational-Equivalence Licensing for Disp's Optimizer

> *Part of disp's verified-optimizer stack — [`OPTIMIZER.md`](OPTIMIZER.md) is the reading map (this is layer 2, the soundness deep-dive on EQUALITY's §7).*

A deep dive into §7 of [`EQUALITY_FOR_VERIFIED_OPTIMIZATION.md`](EQUALITY_FOR_VERIFIED_OPTIMIZATION.md)
— "license rewrites by operational equivalence, in a total fragment." The
question: **is this the ideal soundness foundation for disp's verified
optimizer, and how far does it stretch toward univalence / higher-inductive
types (HITs)?**

The short answer is that the idea is right but my original §7 phrasing was
*naïve in a way that turns out to be the most important fact in the whole
analysis*, and once corrected it becomes a **stronger, more disp-native** story
— at the explicit cost of being **set-level by construction**, so univalence and
higher HITs are *principled non-goals* of this layer (reachable only by a
separate dimensional extension).

> **Status.** Design investigation, not a landed change. Builds on the four
> source reports behind the main doc plus two focused follow-up passes
> (operational equivalence + improvement theory; PER semantics ↔ univalence/HITs).
> Code claims checked against `lib/` on 2026-06-13; theory claims carry citations
> with uncertainty flagged inline.

---

## 0. Verdict up front

1. **Raw contextual/operational equivalence is *useless* in disp** — it collapses
   to `tree_eq`. Tree calculus is reflective: a context `tree_eq [·] P`
   distinguishes any two structurally-distinct trees, so contextual equivalence =
   syntactic identity (Wand's "fexprs is trivial" theorem, sharpened to
   binder-free structural identity). **In a reflective calculus, contextual
   equivalence is the *floor* (= `tree_eq`), not the *ceiling*.** This inverts the
   textbook intuition that "contextual equivalence is the gold standard," and it
   means every non-trivial rewrite is contextually *inequivalent*. §7 cannot mean
   *contextual* equivalence.

2. **The licensing relation must be a deliberately *coarser, observer-restricted*
   relation** — a **logical relation** that quantifies only over *applicative*
   uses (apply, don't inspect). It is **sound but incomplete** for the (now
   trivial) contextual equivalence. **Disp's walker is exactly the mechanism that
   defines the sanctioned observer class** (it forbids structural inspection of
   abstract values). So parametricity is not merely a *source* of rewrite rules —
   it is *constitutive of which equivalence exists at all.*

3. **The relation should be Sands' *strong improvement* / *cost-equivalence*, not
   plain equivalence.** Plain equivalence is *unsound under fold/unfold*
   transformations (the `repeat`/`tail` counterexample); and only the *asymmetric*
   improvement preorder + improvement induction can certify **asymptotic**
   (O(n²)→O(n)) speedups — a symmetric cost-equivalence algebra is provably limited
   to constant factors. Cost-awareness is exactly `GOALS.md`'s ask, so this is a
   feature, not a tax.

4. **Univalence / higher HITs: a principled NO at this layer.** Operational
   equivalence is proof-irrelevant ⇒ set-level (0-truncated) ⇒ forces UIP ⇒
   *refutes* univalence. What you *do* get free: **set-level quotients / QITs**
   (= a coarser PER, i.e. the OTT `eq`-field doing double duty) and
   **representation independence via parametricity** (the walker), which is *more
   general in reach* than univalent transport. Higher structure (S¹, suspensions,
   n-truncations, proof-relevant univalence) is **flattened** by a PER and is only
   reachable by **extending the substrate with dimensions** (computational higher
   type theory) — ideally, for disp, via **internal parametricity reusing the
   walker** rather than a bolted-on cubical interval.

5. **Net:** §7, corrected to *"strong-improvement licensing, defined as a logical
   relation over the walker's observer class, composed from certified rules"*, is
   close to **ideal for the optimizer** — and it is *deliberately* a set-level
   strict layer. Univalence/HITs, if ever wanted, are a *separate inner layer* (a
   2LTT-style upper deck), reached by a dimensional ladder whose **base case is
   §7**. It is not a dead end; it is the ground floor.

---

## 1. Why §7 needs sharpening: the intensionality trap

My original §7 said "license rewrites by operational equivalence (Howe `~`),
sound by construction." That is the right *instinct* but the wrong *relation*,
for a reason specific to disp's substrate.

**Tree calculus is reflective / intensional.** Its `triage` eliminator
(`prelude.disp:30`) branches on whether *any* value is a leaf / stem / fork, and
`tree_eq` (`prelude.disp:113`) decides structural identity of *any two trees,
including functions* (a function value is just a partial-application fork/stem).
Both are total over all trees. This is Barry Jay's whole point — tree calculus
can write its own `equal`, `size`, and self-evaluator because programs can
inspect the structure of other programs.

**Consequence (a theorem, not a worry):** in a reflective calculus, contextual
equivalence collapses to syntactic identity. This is

> M. Wand, *The Theory of Fexprs is Trivial*, Lisp & Symbolic Computation 10(3),
> 1998, DOI 10.1023/A:1007720632734.

Wand proves that adding reflective `fexpr`/`eval` (operators that can inspect
argument *syntax*) to the λ-calculus makes contextual equivalence coincide with
**α-congruence**. For disp's **binder-free** substrate, α-congruence degenerates
to **literal structural tree identity** — i.e. **contextual equivalence =
`tree_eq`**. The separating context is exactly `tree_eq [·] P` (or any `triage`
on the hole): it returns `TT` for one filling and `FF` for any structurally-
different one.

> *Attribution caveat.* Jay builds the operators (`triage`, the factorisation/`F`
> operator, an intensional `equal`) that *cause* the collapse, but as far as the
> research could verify he does **not** himself state the Wand-style triviality
> corollary. The collapse theorem is Wand's, transported to tree calculus (sound,
> standard, but his theorem, not Jay's).

So the standard picture is **inverted**. In an ordinary calculus, contextual
equivalence is the *coarsest* sound congruence — the relation an optimizer dreams
of. In disp it is the *finest* — equal to definitional `tree_eq`, the relation
the optimizer *already has* and gains nothing from. **Every interesting rewrite
changes the tree and is therefore contextually inequivalent.** Licensing by
contextual equivalence would license nothing.

The lattice, for disp:

```
   FINEST  ──────────────────────────────────────────────►  COARSEST
   tree_eq  =  contextual equivalence          Eq_obs     logical relation ~_T
   (definitional, O(1))   (USELESS: = tree_eq)  (checkable) (chosen, observer-restricted)
            └─── the optimizer is stuck here ───┘     └──── the optimizer wants to live here ────┘
```

The job of §7 is therefore **not** to find "the sound congruence" (that's
`tree_eq`, and it's trivial) but to **deliberately choose a coarser relation and
justify it relative to a restricted observer class.** This is forced by
intensionality, not optional.

---

## 2. The fix: a logical relation over the walker's observer class

How do you recover a *useful* (coarse) extensional equivalence in a calculus
where contexts can inspect structure? You **restrict the observers**.

**Why "restrict the syntactic context class" (CIU / context lemmas) is not
enough here.** In ordinary languages, the *context lemma* (Milner 1977) and **CIU
equivalence** (Mason–Talcott, JFP 1991) show that quantifying over a smaller
class of *use* contexts induces the *same* relation as full contextual
equivalence — a decidability/tractability win. But that move *preserves* the
relation; in a reflective calculus the full relation is already trivial
(`tree_eq`), and you cannot syntactically forbid `triage`/`tree_eq` from
appearing in a context (they're ordinary programs). So CIU-style restriction
alone won't manufacture a coarser relation.

**The right tool: a logical relation** (an operationally-based one):

> A. M. Pitts, *Parametric Polymorphism and Operational Equivalence*, MSCS 10(3),
> 2000, DOI 10.1017/S0960129500003066 — an operational logical relation that
> "yields an applicative characterisation of contextual equivalence" and a
> complete method for proving equivalences; the relation quantifies over
> *applicative behaviour*.

A logical relation `~_T` (indexed by a type `T`) relates values that behave the
same *when used according to `T`'s interface* — applied, projected, eliminated —
**not** when raw-inspected. In a reflective calculus `~_T` is necessarily a
**sound but incomplete** under-approximation of contextual equivalence (it can't
be complete — completeness would force it down to `tree_eq`). That incompleteness
is exactly right for an optimizer: you want the *largest relation you can prove
sound for the contexts you permit*, not the impossible "all contexts."

**The disp-specific punchline: the walker *is* the observer-class definition.**
`param_walker` (`engine.disp:109`) rejects `triage`/structural inspection of
kernel-minted neutrals (the stem-forge and triage-on-neutral rejections,
`engine.disp:75,91`). That discipline is precisely "observers may *apply* abstract
values but not *inspect* them" — i.e. **relational parametricity**. So:

> Disp already enforces the observer restriction that makes a coarse extensional
> equivalence *exist*. The walker is not just a *source* of rewrite rules (free
> theorems); it is **constitutive** of the licensing relation `~_T`. Without a
> parametricity discipline, intensionality would leave the optimizer with nothing
> but `tree_eq`.

This also pins down **where coarse `~_T` actually beats `tree_eq`**, and where it
doesn't:

- **At function types and abstract/sealed types** — interface = "apply / use via
  operations." `~_T` is genuinely extensional here (funext-style: two functions
  equal-on-all-inputs are `~`). **This is where the optimizer wins.**
- **At concrete inductive types** (Bool, Nat, the Scott/Tree-encoded data whose
  values are matched by `triage`) — the interface *includes* structural matching,
  so `~_T` collapses back to structural / `tree_eq`. No gain, but none needed: the
  canonical forms are already what you compare.

And it pins down **the soundness obligation**: a swap `e ⇝ e'` justified by
`e ~_T e'` is sound **only at positions where the surrounding program observes `e`
parametrically** (through `T`'s interface, not by raw inspection). **Typed
positions are exactly the parametric positions** — a well-typed program uses a
value of type `T` via `T`'s eliminators / `respond`, and the walker forbids the
rest. So the type discipline and the operational equivalence are *the same
mechanism viewed twice*: the type system is what makes `~_T` non-trivial, and
`~_T` is the semantic content of the type system's parametricity.

> *Sharper than "parametric contexts."* The follow-up research pushed back on my
> phrase "contextual equivalence w.r.t. parametric contexts": there is no such
> *syntactic* context class once `triage` exists. The correct object is the
> *logical relation* (a semantic, observer-restricted relation), with the walker
> as the discipline that guarantees programs stay inside the sanctioned observer
> class. Cf. Neis–Dreyer–Rossberg, *Non-Parametric Parametricity* (ICFP 2009),
> on what parametricity becomes when a language *can* inspect — directly analogous
> to disp's "can inspect structure."

---

## 3. The relation should be *improvement*, not equivalence

Even after restricting observers, **plain equivalence is the wrong relation for a
*transforming* optimizer.** Two reasons, both from Sands' improvement theory.

Primary sources:
- D. Sands, *Improvement Theory and its Applications*, in Gordon & Pitts (eds.),
  *Higher-Order Operational Techniques in Semantics*, CUP 1998, pp. 275–306.
- D. Sands, *Total correctness by local improvement in the transformation of
  functional programs*, ACM TOPLAS 18(2), 1996, 175–234, DOI 10.1145/227699.227716
  (POPL '95 conf. version DOI 10.1145/199448.199499) — the **Improvement Theorem**.
- A. Moran & D. Sands, *Improvement in a Lazy Context*, POPL '99,
  DOI 10.1145/292540.292547 — call-by-need theory + the constant-factor result.
- J. Hackett & G. Hutton, *Worker/Wrapper/Makes It/Faster* (ICFP 2014) &
  *Programs for Cheap!* (LICS 2015) — improvement-licensed transformation.

**Reason 1 — plain equivalence is unsound under fold/unfold.** Sands' canonical
counterexample: with `repeat x = x : repeat x`, one can *prove the equivalence*
`repeat x ≅ tail (repeat x)`. Use it to rewrite the recursive body and you get
`repeat' x = x : tail (repeat' x)`, which produces *one element* — **not**
equivalent to `repeat`. Folding turned a finite derivation into a non-productive
one. The diagnosis is *cost*: `tail (repeat x)` is **not an improvement** of
`repeat x` (it needs an extra unfolding to reach head-normal form), so an
improvement-based side-condition *rejects* the unsound step. **This is exactly
disp's "don't introduce non-termination / don't break productivity" requirement**,
and only a cost-sensitive relation enforces it.

**The relation you want — strong improvement / cost-equivalence (Sands Def 7):**

```
e ⊵~  e'   :  in every (sanctioned) context, e' reaches a value in ≤ as many steps as e   (preorder)
e ⊵~ₛ e'   :  e ⊵~ e'  ∧  e ≅ e'      (STRONG improvement: faster AND behaviourally equal)
e ◁▷~ e'   :  e ⊵~ e'  ∧  e' ⊵~ e     (COST-EQUIVALENCE: provably same cost)
```

- For "replace with something **faster**": license by **strong improvement
  `⊵~ₛ`**. (Bare `⊵~` is a preorder refining operational *approximation*, so it
  would permit replacing a terminating term with a faster *divergence-introducing*
  one — the very bug to avoid.)
- For "replace with something of **identical cost**" (e.g. a representation swap):
  license by **cost-equivalence `◁▷~`**.

**The Improvement Theorem (the fold/unfold licensing result).** If every local
rewrite step is an improvement (`eᵢ ⊵~ eᵢ'`), then the whole transformed program
improves the original (`fᵢ ⊵~ gᵢ`) **and is totally correct** — upgrading
unfold/fold's "partial correctness for free" to total correctness. The strong /
cost-equivalence variants compose likewise (Sands Cor 13, Prop 14). Improvement
is a **precongruence** and cost-equivalence a **congruence**, so it composes
locally: prove a rule once, apply it in any sub-context.

**Reason 2 — the asymptotic/constant-factor dichotomy (decisive for a
superoptimizer).** Moran–Sands prove (POPL '99, Cor 4.4) that a *symmetric
equational* calculus can change cost by **at most a constant factor**. So:

> A bag of cost-*equivalence* laws can only ever certify **constant-factor**
> speedups. To certify an **asymptotic** improvement (O(n²)→O(n) — e.g. the
> accumulator/`reverse` and deforestation transformations), the optimizer **must**
> use the *asymmetric* improvement preorder plus a **recursion principle**
> (improvement induction). This is non-negotiable for `GOALS.md`'s "produce an
> efficient program" goal.

**Tooling — the tick algebra.** Sands' practical device is `tick` (`✓e`, a marked
computation step): `✓e ≅ e` extensionally but costs one extra step, with algebraic
laws for propagating ticks through reduction. It lets you *calculate* improvements
like ordinary algebra — the proof technique for establishing `rule ⊆ ⊵~ₛ`.

**The honest porting obligation.** Sands' theory is developed for call-by-name /
call-by-need with a **function-call** cost metric; the Improvement Theorem *fails*
for other metrics and is sensitive to evaluation strategy (β is an improvement
for call-by-name, *not* for call-by-value where argument duplication changes
cost). Disp's substrate is **eager** (iterative `apply`). So disp must (i) pick a
tree-calculus reduction cost metric, and (ii) **re-derive the Improvement Theorem
and a context lemma for the eager strategy**. This is a *known-feasible port* — a
call-by-value improvement theory exists (Sands 1997) — but it is real work, not a
citation.

---

## 4. Architecture: certified rules + the (small) total fragment

The relations above are undecidable (they quantify over observers), but they are
**(pre)congruences closed under reduction**, which gives the standard, decidable
*architecture*:

```
prove each rewrite rule  lᵢ ⤳ rᵢ   to satisfy   lᵢ ⊵~ₛ rᵢ    ONCE   (the hard, offline part)
─────────────────────────────────────────────────────────────────────────────────────────────
optimizer composes rule instances  →  a finite derivation (chain of in-context rule applications)
checking a derivation is DECIDABLE  (match each step against a certified rule; transitivity)
```

- **Soundness of a chain** follows from transitivity + compatibility (congruence)
  — no need to re-prove anything per program.
- **Efficiency:** *bisimulation up-to improvement and context* (Sands 1998, Thm 5;
  the operational analogue of Sangiorgi's up-to techniques) lets you discharge a
  rule by a small relation closed up to known improvements + contexts.
- **Prior art for the pattern** (correctness-only): CompCert's per-pass simulation
  relations; Alive (Lopes et al., PLDI 2015) and Peek/verified-peephole (Mullen et
  al., PLDI 2016) — each rule certified once against a semantic relation, composed,
  chain-checking decidable. **Cost-aware** prior art: Hackett–Hutton's
  improvement-licensed worker/wrapper.

**What the "total fragment" really is — and how *small* it can be.** The trusted
base is **not** a big total sub-language. It is:

1. the **operational semantics** of tree calculus (defines reduction, hence the
   cost metric and `~`/`⊵~`);
2. a **congruence proof** for the chosen bisimilarity (Howe's method — see §5,
   *simplified* by binder-freeness);
3. the **cost metric** (and the eager Improvement Theorem derived for it);
4. the **rule-soundness lemmas** `lᵢ ⊵~ₛ rᵢ`, proven in a *consistent* logic.

Only (4) needs an in-language sound logic, and it only needs to be strong enough
to state and prove a *finite (or schematic) family* of rewrite-rule soundnesses —
many of which come *free from parametricity* (free theorems are `~_T`-facts the
walker already guarantees). So the "total fragment" is "the consistent layer where
rewrite-rule soundness is established," and it can be **the metatheory (Howe, once
per rule)** or a **small total in-language sub-logic** — crucially, it must be
*consistent* so `Type:Type` cannot leak a bogus rule in. **This is the precise
content of "operational-equivalence licensing + total fragment": the program layer
stays partial and `Type:Type`; the rule-soundness layer is small, total, and the
only thing that must be trusted.**

**Disp pieces that already fit:**
- `behavioral_specs` (the per-type runnable coherence slot) = **the certified
  rewrite-rule library** (map-fusion, β/η, `id`-elimination, accumulator laws),
  each tagged with its `⊵~ₛ`/`◁▷~` proof.
- the **walker** = both the observer-class definition (§2) *and* a generator of
  free-theorem rules.
- **`φ`** (the Cedille-style zero-cost cast, main doc §4) = the mechanism that
  performs the swap once a rule licenses it; its soundness is the rule's soundness.

---

## 5. Why this is well-matched to disp (and the obligations)

**Favourable substrate facts:**
- **Binder-free ⇒ Howe's method simplifies dramatically.** Howe's congruence proof
  has one hard clause — (Com2), closure *under binders* (the substitutivity
  argument). Tree calculus has **no binders in the substrate** (only the tree
  constructor + application), so (Com2) *vanishes*; congruence of bisimilarity
  reduces to "bisimilar operands ⇒ bisimilar applications," near-immediate from
  determinism. (The simplification is because the *substitutivity obligation*
  disappears, not because "contexts are simpler.")
- **Deterministic + pure ⇒ applicative bisimilarity coincides with contextual
  equivalence** (Pitts) — the favourable regime. (Caveat: the coincidence theorems
  are proven for λ-calculi, *not* tree calculus, and the reflective operators
  *break their premise* — which is exactly why you fall back to a logical-relation
  *under*-approximation rather than full abstraction.)
- **Immune to `Type:Type`.** `~`/`⊵~` are defined by the operational semantics, not
  by propositions in the object logic, so the logic's inconsistency cannot corrupt
  them. This is the property that lets disp keep `Type:Type` + general recursion in
  the *program* layer while the *optimizer* stays sound — the resolution of the
  main doc's §7 tension.
- **Cost-awareness is the actual goal.** `GOALS.md` wants programs scored on
  performance; improvement theory *is* the formal "this transform is an
  optimization" relation. The soundness relation and the objective function are
  the same currency.

**Obligations (honest):**
1. Port Sands to **eager** tree calculus: choose a cost metric, re-derive the
   Improvement Theorem + context lemma (feasible; CBV improvement theory exists).
2. Establish **Howe congruence for tree calculus** (easy direction — no binders).
3. Accept **incompleteness**: `~_T` will not prove every true equivalence (it
   *can't* — completeness = `tree_eq`). Fine for an optimizer; it just means some
   valid speedups aren't in the rule library yet.
4. Define the **logical relation `~_T`** in a way that meshes with the walker's
   neutral discipline (an `~`-proof must not itself leak hyp structure — the same
   escape-check the walker already runs).

---

## 6. How far toward univalence / higher-inductive types?

This is the load-bearing second half of the question. The answer is a **clean,
principled dichotomy**, and it is *good news framed correctly*.

### 6.1 The hard ceiling: operational equivalence is set-level

`~`/`⊵~` are **proof-irrelevant** (the "proof" that `a ~ b` is a meta-level
bisimulation; in disp the witness erases — `refl := t`). A proof-irrelevant
equality is a *mere proposition*, which gives **definitional UIP**, which
**refutes univalence**:

- Univalence requires `Id_U(A,B)` to *carry the data of the equivalence* `A ≃ B`
  (proof-*relevant*); e.g. `Bool` has *two distinct* self-identities (`refl` and
  the `not`-swap), so the universe is **not a set** (HoTT Book Ex 3.1.9). A
  proof-irrelevant `~` flattens those two to one.
- The canonical PER realization (Nuprl/CTT) doesn't merely *permit* UIP — it
  **proves** UIP/axiom K, via equality reflection. (Hofmann–Streicher groupoid
  model, 1998, is the converse: the first model where UIP is *not* forced, i.e. the
  seed of higher structure — precisely what a PER throws away.)

> **So: operational-equivalence licensing is inherently set-level (h-set,
> 0-truncated). It cannot host univalence or higher HITs. This is structural, not
> an implementation gap — and it is *correct* for an optimizer, whose
> transformations are observational/relational, never higher-dimensional.**

### 6.2 What you get *free* anyway (most of the practical payload)

- **Set-level quotients / quotient inductive types (QITs) = a coarser PER.** A
  quotient is literally "replace the carrier's PER by a coarser equivalence." This
  is the setoid story; Nuprl has had quotient/PER types for decades. In disp this
  is the **OTT `eq` meta-field (main doc §3) doing double duty**: a QIT is a type
  whose `eq`-field is the quotient relation. **The optimizer's `~` and a user's
  QITs share one mechanism.** Reachable set-level objects: integers-as-quotient
  (Altenkirch–Scoccola, LICS 2020), finite multisets / `Bag` (free commutative
  monoid), set-level free groups, the partiality monad (Altenkirch–Danielsson–Kraus,
  FoSSaCS 2017), the well-typed syntax of type theory as a QIIT (Altenkirch–Kaposi,
  POPL 2016). Propositional truncation `‖A‖₋₁` is the borderline case = Nuprl's
  "squash" (reachable). *Caveat:* honest dependent eliminators (the recursor's
  coherence) are fiddly in a bare setoid setting — the *expressive reach* is free,
  the *eliminator engineering* is not.
- **Representation independence via parametricity** (the walker) — and, importantly,
  this is **more general in reach than univalent transport**: parametricity moves
  programs across *relationally related* representations, including
  **non-isomorphic** ones, whereas univalence only moves across *equivalences*. The
  univalence literature confirms the asymmetry: Angiuli–Cavallo–Mörtberg–**Zeuner**,
  *Internalizing Representation Independence with Univalence* (POPL 2021,
  DOI 10.1145/3434293), must reach for **QITs to quotient non-isomorphic
  representations together** — i.e. it uses the set-level tool precisely where
  univalence alone is too weak. (Parametricity is *narrower* only in the
  proof-relevant *structure it carries*, not in reach.)

> Net: **a PER/operational-equivalence layer + the walker's parametricity gives
> set-level representation independence and set-level quotients for free — which is
> most of what an *optimizer* soundness story actually needs.** Program
> transformations are observational; they live at h-set.

### 6.3 The upgrade ladder (so it is a base case, not a dead end)

If disp ever wants genuine univalence / higher HITs (S¹, suspensions,
n-truncations for n≥1, Eilenberg–MacLane spaces — all of which a PER *flattens*),
the route **reuses the same PER/operational methodology over a richer computation
system**:

- **Computational Higher(-Dimensional) Type Theory** — Angiuli–Harper–Wilson
  (POPL 2017); Cavallo–Harper, *Higher Inductive Types in Cubical Computational
  Type Theory* (POPL 2019, DOI 10.1145/3290314, where S¹ etc. *compute*). Keep the
  Nuprl-style PER-over-an-operational-semantics, **index the computation system by
  cubical dimensions**, and univalence + HITs compute. **The family splits on
  decidability:** *RedPRL* keeps the Nuprl lineage (equality reflection, two-level
  "exact equality + paths", undecidable, tactic-driven); *cooltt/redtt* are a
  deliberate departure (NbE, **decidable** conversion, no equality reflection;
  normalization: Sterling–Angiuli, LICS 2021). Either is available — you are not
  forced back into undecidability to get univalence.
  *Refinement:* dimension-indexing is **not** a free add-on — you also need the
  **Kan/composition coherence** for the PERs to behave (transport, hcomp). "Same
  methodology over a richer system," not "same theory + an interval."

- **The disp-aligned route: internal parametricity, reusing the walker.** Because
  disp already enforces *external* relational parametricity (the walker), the
  natural upgrade is **internal** parametricity — a **bridge** modality (distinct
  from a path interval) that reflects relations into the theory:
  - Bernardy–Coquand–Moulin (presheaf model of parametric type theory); Altenkirch–
    Kaposi, *Towards a Cubical Type Theory without an Interval* (TYPES 2015):
    "extend internal parametricity to univalence by reinterpreting the universe" —
    **but** "we do not know how to compute in this theory" (non-computing in 2015).
  - Cavallo–Harper, *Internal Parametricity for Cubical Type Theory* (CSL 2020,
    arXiv:2005.11290): bridges give **"relativity" = the relational analogue of
    univalence**; paths (Kan) give univalence proper. (So bridges alone give the
    *relational* twin; full univalence still pairs them with a Kan interval.)
  - Altenkirch–Chamoun–Kaposi–Shulman, *Internal Parametricity, without an
    Interval* (POPL 2024, arXiv:2307.06448): internal parametricity as a *small
    MLTT extension* **with a canonicity proof** (it computes) — explicitly "a baby
    version of Higher Observational Type Theory."
  - **HOTT** (Altenkirch–Kaposi–Shulman, WIP): univalence *without an interval* by
    computing the identity type of each former. **Honest status: canonicity for the
    univalent fragment is conjectural, not proved.**

  > For disp this means: the upgrade that *reuses the walker* is internal
  > parametricity → **relativity (relational univalence) at set level for free**,
  > with full proof-relevant *computing* univalence being a bet on the HOTT
  > endpoint (not yet a closed theorem). Each added dimension of computation buys
  > one more h-level of reachable HIT.

### 6.4 The 2LTT framing: they coexist, they don't compete

The clean architecture is **two-level**: operational-equivalence/improvement is
the **strict, set-level outer layer** (the optimizer's substrate, the fast
`tree_eq`-backed world), and *if* univalence is ever wanted it is an **inner
fibrant layer** on top — which is **exactly RedPRL's design** ("an extensional,
proof-irrelevant *exact equality* coexisting with a proof-relevant notion of
*paths*"). So §7 and univalence are not rivals; §7 is the ground floor of a 2LTT
whose upper deck (if built) is dimensional.

### 6.5 HITs by h-level (what `~` reaches vs. flattens)

| Reachable as PER quotients / QITs (set-level) | Flattened by a PER (need dimensions) |
|---|---|
| set-quotients; integers-as-HIT (provably a set); finite multisets / `Bag`; set-level free groups; partiality monad (QIIT); type-theory syntax (QIIT); `‖A‖₋₁` (squash, borderline) | S¹ (`loop ≠ refl`, π₁ = ℤ); suspensions Σ, spheres Sⁿ, pushouts, torus, Hopf; n-truncations `‖A‖ₙ` for n ≥ 1; K(G,n); all synthetic homotopy theory |

---

## 7. Verdict: is operational-equivalence licensing ideal for disp?

**For the optimizer: yes — and more native to disp than I first claimed —
provided three sharpenings:**

1. **Not contextual equivalence** (= `tree_eq`, useless) but a **logical relation
   `~_T`** over a sanctioned applicative observer class — **which the walker already
   defines**. Parametricity is constitutive of the relation, not just a rule
   source. This is a place where disp's intensional substrate, which looks like a
   liability, is *rescued precisely by the discipline disp already has.*
2. **Not plain equivalence** but **strong improvement `⊵~ₛ` / cost-equivalence
   `◁▷~`** — sound under fold/unfold, and (via the asymmetric preorder +
   improvement induction) able to certify *asymptotic* speedups, which a symmetric
   equational theory provably cannot. Cost-awareness coincides with `GOALS.md`'s
   objective.
3. **A small total fragment** carrying only the rule-soundness lemmas (+ the
   operational semantics, Howe congruence, cost metric as the trusted base) —
   keeping the program layer free to be partial and `Type:Type`, and the optimizer
   sound regardless.

**For univalence / higher HITs: deliberately no** — operational equivalence is
set-level by construction, and that is the *right* level for an optimizer. The
practically valuable slice (set quotients, representation independence) comes free;
the higher slice is a *separate dimensional layer* (computational higher type
theory, ideally via internal parametricity reusing the walker), forming a ladder
whose **base case is exactly §7**. So §7 does not "support" univalence, but it is
the correct foundation *under* a future univalent layer, and nothing about it
forecloses that layer.

This is a **stronger and more coherent §7** than the original: the intensionality
trap forces the logical-relation form, the walker supplies it, improvement theory
supplies cost-soundness, and the set-level ceiling is reframed from "limitation"
to "correct altitude, with a known ladder up."

---

## 8. Open questions specific to this layer

1. **Eager Improvement Theorem for tree calculus.** Pick the cost metric (count
   `apply` steps? a sanctioned subset?) and re-derive Sands' theorem + a context
   lemma for the eager strategy. Which metric makes β-style rules improvements?
2. **The logical relation `~_T`, concretely.** Define it over stripped trees in a
   way that (a) meshes with the walker's neutral/escape discipline, (b) is sound
   for the typed observer class, (c) supports the `behavioral_specs` rule library.
   Is it the step-indexed logical relation hinted at by the sealing-framework
   conjecture (`TYPE_THEORY.typ` §11.6)? (`~_T` and the Type:Type consistency
   argument may be the *same* logical relation.)
3. **Improvement induction for a superoptimizer.** How does the asymmetric
   recursion principle integrate with equality saturation / neural search (the
   optimizer must *find* the improving rewrite *and* carry its improvement proof)?
4. **Howe congruence for tree calculus.** Discharge the (easy, binder-free)
   congruence proof to make `~` a certified congruence in the trusted base.
5. **Does the optimizer ever need cost-equivalence vs. strong-improvement vs. a
   refinement (improving termination)?** E.g. dead-code elimination of a diverging
   unused subterm is *not* an equivalence (changes termination) — it needs a
   *refinement* relation, not `~`. Catalogue which transforms need which relation.
6. **Bridge-modality upgrade.** If/when univalence is wanted: is internal
   parametricity (reusing the walker) genuinely cheaper to add to disp than a
   cartesian-cubical interval, given the HOTT endpoint is still conjectural for
   *computing* univalence? Prototype the bridge modality as a library type (à la
   the §13 interval) and see how much the walker already provides.

---

## 9. References (this layer; see the main doc for the rest)

### Operational equivalence, congruence, logical relations
- M. Wand. *The Theory of Fexprs is Trivial.* Lisp & Symbolic Computation 10(3), 1998. DOI 10.1023/A:1007720632734. — contextual equivalence collapses to syntactic identity in a reflective calculus.
- A. M. Pitts. *Operationally-Based Theories of Program Equivalence.* In Gordon & Pitts (eds.), *Semantics and Logics of Computation*, CUP 1997, 241–298. — contextual equiv, bisimilarity, operational extensionality, equational logic.
- A. M. Pitts. *Howe's method for higher-order languages.* In Sangiorgi & Rutten (eds.), *Advanced Topics in Bisimulation and Coinduction*, CUP 2011, ch. 5. — Howe's method; CIU; the determinism-dependent coincidence.
- A. M. Pitts. *Parametric Polymorphism and Operational Equivalence.* MSCS 10(3), 2000. DOI 10.1017/S0960129500003066. — the operational logical relation.
- D. J. Howe. *Equality in lazy computation systems.* LICS 1989, DOI 10.1109/LICS.1989.39174; *Proving congruence of bisimulation in functional programming languages.* Inf. & Comp. 124(2), 1996, DOI 10.1006/inco.1996.0008.
- S. Abramsky. *The Lazy Lambda Calculus.* In Turner (ed.), *Research Topics in Functional Programming*, Addison-Wesley 1990, 65–117.
- R. Milner. *Fully abstract models of typed λ-calculi.* TCS 4(1), 1977 (context lemma). I. Mason & C. Talcott. *Equivalence in functional languages with effects.* JFP 1(3), 1991 (CIU).
- G. Neis, D. Dreyer, A. Rossberg. *Non-Parametric Parametricity.* ICFP 2009 / JFP 21(4–5), 2011. — parametricity when the language can inspect.
- B. Jay. *Reflective Programs in Tree Calculus.* 2021 (+ Coq dev); Jay & Given-Wilson, *A Combinatory Account of Internal Structure*, JSL 76(3), 2011. — the intensional operators.

### Improvement theory (cost-aware)
- D. Sands. *Improvement Theory and its Applications.* In *Higher-Order Operational Techniques in Semantics*, CUP 1998, 275–306.
- D. Sands. *Total correctness by local improvement in the transformation of functional programs.* ACM TOPLAS 18(2), 1996, 175–234. DOI 10.1145/227699.227716 (POPL '95: DOI 10.1145/199448.199499).
- A. Moran & D. Sands. *Improvement in a Lazy Context.* POPL '99. DOI 10.1145/292540.292547. — constant-factor limit (Cor 4.4).
- J. Hackett & G. Hutton. *Worker/Wrapper/Makes It/Faster.* ICFP 2014, DOI 10.1145/2628136.2628142; *Programs for Cheap!* LICS 2015, DOI 10.1109/LICS.2015.21.
- D. Sangiorgi. *On the bisimulation proof method.* MSCS 8(5), 1998. — up-to techniques.

### Verified-rewriting / compiler prior art
- X. Leroy. *Formal verification of a realistic compiler.* CACM 52(7), 2009 (CompCert).
- N. Lopes et al. *Provably correct peephole optimizations with Alive.* PLDI 2015. E. Mullen et al. *Verified peephole optimizations for CompCert.* PLDI 2016.

### PER semantics, univalence, HITs (the §6 ladder)
- Univalent Foundations Program. *Homotopy Type Theory.* IAS 2013 (Ex 3.1.9, "U is not a set").
- M. Hofmann & T. Streicher. *The Groupoid Interpretation of Type Theory.* 1998 (Oxford Logic Guides 36).
- C. Angiuli, R. Harper, T. Wilson. *Computational Higher-Dimensional Type Theory.* POPL 2017. DOI 10.1145/3009837.3009861.
- E. Cavallo & R. Harper. *Higher Inductive Types in Cubical Computational Type Theory.* POPL 2019. DOI 10.1145/3290314.
- *The RedPRL Proof Assistant.* arXiv:1807.01869. cooltt/redtt (NbE, decidable). J. Sterling & C. Angiuli. *Normalization for Cubical Type Theory.* LICS 2021, arXiv:2101.11479.
- C. Angiuli, E. Cavallo, A. Mörtberg, M. Zeuner. *Internalizing Representation Independence with Univalence.* POPL 2021. DOI 10.1145/3434293, arXiv:2009.05547.
- J. Reynolds. *Types, Abstraction and Parametric Polymorphism.* IFIP 1983. J. Mitchell. *Representation Independence and Data Abstraction.* POPL 1986. DOI 10.1145/512644.512669.
- J.-P. Bernardy, T. Coquand, G. Moulin. *A Presheaf Model of Parametric Type Theory.* MFPS 2015 / ENTCS 319. T. Altenkirch & A. Kaposi. *Towards a Cubical Type Theory without an Interval.* TYPES 2015, DOI 10.4230/LIPIcs.TYPES.2015.3.
- E. Cavallo & R. Harper. *Internal Parametricity for Cubical Type Theory.* CSL 2020, DOI 10.4230/LIPIcs.CSL.2020.13, arXiv:2005.11290 (LMCS). A. Nuyts, A. Vezzosi, D. Devriese. *Parametric Quantifiers for Dependent Type Theory.* ICFP 2017, DOI 10.1145/3110276.
- T. Altenkirch, Y. Chamoun, A. Kaposi, M. Shulman. *Internal Parametricity, without an Interval.* POPL 2024, DOI 10.1145/3632920, arXiv:2307.06448. (+ HOTT, WIP; univalent-fragment canonicity conjectural.)
- T. Altenkirch & A. Kaposi. *Type Theory in Type Theory using QITs.* POPL 2016, DOI 10.1145/2837614.2837638. Kaposi, Kovács, Altenkirch. *Constructing QIITs.* POPL 2019, DOI 10.1145/3290315. T. Altenkirch & L. Scoccola. *The Integers as a HIT.* LICS 2020, DOI 10.1145/3373718.3394760. T. Altenkirch, N. A. Danielsson, N. Kraus. *Partiality, Revisited.* FoSSaCS 2017, arXiv:1610.09254.
