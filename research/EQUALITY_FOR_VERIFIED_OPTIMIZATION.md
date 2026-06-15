# Equality for Verified Optimization

Research notes on which equality / type theory best serves disp's north-star goal
(`GOALS.md`): a self-improving optimizer that **replaces a program with a
provably-equal cheaper one**. The investigation covers observational type theory
(OTT/SeTT), cubical type theory (CTT) and its lighter relatives (XTT, HOTT,
2LTT), Cedille-style zero-cost coercions, and computational/extensional type
theory (Nuprl, equality reflection), each assessed *specifically* for ease and
elegance of implementation on disp's tree-calculus substrate — and for what disp
can simplify away given its stance "drop the universe hierarchy, don't insist on
termination."

Companion documents (read alongside, not duplicated here):
- `research/CUBICAL_TYPE_THEORY_FOR_DUMMIES.typ` — pedagogical cubical walkthrough.
- `research/MODAL_TYPES_INVESTIGATION.md` — modal/wait-form typing; brief survey of HOTT/2LTT/etc.
- `TYPE_THEORY.typ` §13 — the cubical proposal already folded into the spec (interval `I`, `Path = Pi I`, transport via the per-type `functor` field, with a known univalence-dispatch gap).

> **Status.** Design investigation, not a landed change. Code sketches are
> illustrative (disp syntax) and untested. The load-bearing claims about disp's
> *current* behaviour (degenerate `Eq`, the four meta-fields, the walker's
> parametricity discipline) were checked against the sources on 2026-06-13.

---

## 0. Bottom line up front

1. **Disp's situation inverts the usual motivation for fancy equality.** In
   Coq/Agda the cost of an equality-driven rewrite is the *transport/coercion it
   inserts into the term*, which then blocks reduction ("transport hell"). Disp's
   terms are **erased untyped trees** and conversion is **O(1) hash-cons
   identity** — so a rewrite inserts *nothing* into the runtime term. The
   bottleneck is therefore **not "does transport compute?"** but **"is the
   equality provable at all, and is the equality fragment sound?"** Almost every
   conclusion below follows from this reframe.

2. **Disp already *is* a computational/extensional type theory** of the
   Nuprl/Cedille lineage (types-as-predicates ≈ types-as-PERs over an untyped,
   partial computation system). Its consistency can only be **semantic**
   (realizability/parametricity), never via normalization — which is *already*
   disp's bet (`TYPE_THEORY.typ` §11.6 / Appendix A: the Type:Type conjecture,
   "sealing preserves parametricity"). That forecloses the stratified-PER options
   and points at the impredicative-realizability ones.

3. **Recommended stack, in priority order:**
   - **(A) Observational equality** defined by recursion on the per-type
     meta-fields — gives function extensionality, the single biggest enlargement
     of the provable-equality space. *Elegant on disp:* it is a small, local edit
     to `Eq`'s recognizer plus one new meta-field; `funext` falls out of the
     existing `bind_hyp` machinery. **Best fit.**
   - **(B) A Cedille-style `φ` zero-cost cast** over a *heterogeneous* equality —
     turns a proven equation into a free runtime swap. This is the verified-rewrite
     primitive; nearly free in disp because terms are already erased.
   - **(C/§7) License rewrites by *operational equivalence*, not propositional
     equality**, proven in a *total* fragment. This is what keeps the optimizer
     sound even though disp is `Type:Type`-inconsistent as a logic. Nuprl did
     exactly this to verify real program optimizations (a Paxos implementation).
   - **(D) Cubical/univalence: mostly *not* needed.** Representation independence
     is **parametricity**, which disp's walker *already enforces*; univalence is
     only required to *compute on a single given equivalence opaquely*. Full
     cubical is the heaviest option and disp's instance already has a structural
     gap (§13.5). If higher structure is ever wanted, **HOTT** (observational +
     univalence, no interval) is the upgrade path that fits disp's per-type design
     — but it is research-grade and unfinished.

4. **The sharp tension to internalize:** "drop the universe hierarchy" and "keep
   a sound equality logic" are *in conflict* under the classical stratified
   models. The escape (Cedille's existence proof; Nuprl's operational route) is to
   make soundness rest on a **semantic model + operational equivalence**, not on
   stratification. Disp has already chosen this fork; this document just makes the
   consequence explicit.

---

## 1. What "verified optimization" actually asks of equality

The optimizer's core move: given `e : T`, find a cheaper `e' : T` with `e = e'`,
and justify the swap. Decompose into three independent levers:

| Lever | Question | What governs it |
|---|---|---|
| **(L1) Provable-equality space** | Can I even *state and prove* `e = e'`? | The equality type's strength (intensional vs observational vs extensional) |
| **(L2) Cost of the rewrite** | Does swapping `e→e'` add runtime overhead? | Erasure / coercion behaviour |
| **(L3) Soundness** | Does `e = e'` provable ⇒ `e` and `e'` truly interchangeable? | Consistency of the equality fragment |

In a conventional dependently-typed language all three are coupled and **L2 dominates**:
transport along a propositional equality is a real term that obstructs further
β-reduction, so the entire cubical enterprise exists to make L2 cheap (transport
*computes*). **Disp decouples L2 for free.**

### 1.1 Disp's current equality is the trivial OTT (L1 is maximally weak)

```disp
refl := t                                   // the unique proof value — already proof-irrelevant
Eq : {A:Type} -> A -> A -> Type := {A,x,y} -> wait (make_recognizer
  {meta, v} -> {
    let p = meta.recognizer_params          // { type, lhs, rhs }
    Ok (and (tree_eq v refl) (tree_eq p.lhs p.rhs))   // <-- inhabited IFF x,y already hash-cons-equal
  }) (make_meta { type:=A; lhs:=x; rhs:=y } eq_respond)
```

`Eq A x y` is inhabited **iff `tree_eq x y`** — i.e. iff `x` and `y` are *already*
definitionally equal. Consequences, all verified against the code:

- **`refl := t` is the only proof** → disp's `Eq` is *already* definitionally
  proof-irrelevant and satisfies UIP trivially. (The whole point of the modern
  OTT machinery — `SProp`, the `Ω` universe — is to *recover* this property inside
  a typed term language. Disp has it for free.)
- **`eq_subst : Eq A x y → motive x → motive y` is the identity.** Because `x ≡ y`
  (hash-cons), `motive x` and `motive y` are *the same tree*; `eq_J` computes to
  `base` on the concrete `refl` and stays stuck on a neutral proof (`elim` →
  `select_lazy` on `is_neutral target`). Transport is inert.
- **Function extensionality is unprovable.** `map f (map g xs)` and
  `map (f∘g) xs` are different trees, so no `Eq` between them is inhabitable. **Every
  interesting optimization is an equation between distinct trees, hence currently
  unprovable.** This is the binding constraint.

So disp sits at the *degenerate corner* of observational equality: propositional
equality = conversion, in the trivial direction. The work is to enlarge L1
**without** wrecking L2 or L3.

### 1.2 The reframe, stated precisely

Because terms are erased and conversion is hash-cons identity:

> If the optimizer can *prove* `e = e'` for distinct trees `e, e'` under a **sound**
> equality, it may replace the tree `e` with the tree `e'` and **re-verify against
> the same predicate `T`** — which succeeds because they are equal. The proof is
> the *justification*; the swap itself costs nothing (no transport is inserted, the
> result is literally the `e'` tree). L2 is free; the proof, being
> proof-irrelevant, erases (`refl := t`).

This means: **for disp, the value of a type theory is measured almost entirely by
how much of L1 it unlocks and how cheaply it preserves L3 — not by whether its
transport computes.** That single criterion reorders the whole field.

---

## 2. Where disp already sits: a tree-calculus computational type theory

Two established systems are disp's near-twins. Recognising this pins down which
consistency arguments are even *available*.

### 2.1 Nuprl / Computational Type Theory (the semantic-consistency precedent)

Nuprl (Constable et al. 1986; Allen 1987) is built on an **untyped, partial,
Turing-complete** term system; **types are PERs** (partial equivalence relations)
over it; `a : A` is the diagonal `a ~A a`; ill-behaved/divergent terms simply fall
outside every PER. This is disp's "types are predicates over untyped trees,"
sharpened: a predicate that also says *when two trees are equal members* is a PER.

The decisive precedent: **Nuprl is consistent despite a non-normalizing
substrate, because soundness is *semantic* — `False` denotes the empty PER, every
rule is proved validity-preserving in the model, so `⊢ False` is underivable.
Normalization of the term language is never invoked.** Anand & Rahli (ITP 2014)
mechanized exactly this in Coq ("a type is a PER à la Allen"), reducing Nuprl's
consistency to Coq's; the artifact literally contains `weak_consistency.v`.

- **Conversion ↔ disp's `tree_eq`.** Nuprl's definitional layer is **Howe's
  computational equivalence `~`** (a bisimulation congruence: if `t` steps to `t'`
  then `t ~ t'`, substitutable everywhere). Disp's hash-cons identity is a
  *strictly finer, O(1)* under-approximation of the same idea. **Crucial
  observation for §7:** `~` relates *distinct* trees that compute alike;
  `tree_eq` does not. The optimizer needs the former.
- **Equality reflection + undecidable checking** — Nuprl validates *derivations*,
  not bare terms. Disp's checker is "run the predicate, get true/false-or-diverge,"
  i.e. semi-decidable. **Disp has already pre-paid the price extensional theories
  are criticised for.**

### 2.2 Cedille (the erased-substrate, drop-the-hierarchy precedent)

Cedille / CDLE (Stump, JFP 2017) is a **Curry-style** dependent theory whose terms
**erase to plain untyped λ-terms**, with conversion modulo erasure — disp's
`strip` pass + `tree_eq` is the same discipline (and cheaper). Cedille is built
from exactly three ingredients, two of which disp already has:

| Cedille | Disp |
|---|---|
| Dependent intersection `ι x:A. B` (Kopylov, LICS 2003) | **`Intersection A P`** (`types.disp:128`) — identical: `v` inhabits iff `v:A` *and* `v:P v`, same underlying value |
| Erased implicit product `∀x:A. B` (Miquel) | erased binders / `strip` (`compile.ts:302` erases `ann`) |
| Primitive **heterogeneous** equality `{t1 ≃ t2}` over *untyped* terms + the `φ`/`ρ`/`δ`/`β` combinators | **missing** — disp's `Eq` is homogeneous and degenerate (§1.1) |

The gap is precisely the equality type and its cast — which is the subject of §3–4.

What Cedille proves that licenses disp's stance:
- **Consistent without a universe hierarchy.** CDLE has only `⋆ : □`; no `Type_i`
  tower. Impredicativity does the work, and even *large eliminations* are
  *simulated* via the equality type rather than needing a universe (Jenkins–Marmaduke–Stump,
  TYPES 2021). External evidence that **disp can drop the hierarchy and keep
  inductive power.**
- **Consistent without termination.** `Ω = (λx.x x)(λx.x x)` is typeable (at the
  `Top` equality type); Cedille is *non-normalizing* yet *logically sound*
  (realizability; explicitly analogized to extensional MLTT). Checking is
  undecidable, handled by a **step bound** on conversion. **This is disp's exact
  operating point.**
- **The catch:** the realizability consistency proof **does not transfer
  mechanically** — disp needs its own model argument. This is what disp's
  Type:Type conjecture ("sealing preserves parametricity", `TYPE_THEORY.typ`
  §11.6 / Appendix A) is for.

### 2.3 The consequence

Disp's consistency story *must* be a semantic model (realizability/PER) plus a
parametricity/sealing argument — it cannot be stratification (it's `Type:Type`) and
cannot be normalization (the substrate is partial). This is **the Cedille/Nuprl
fork**, and disp has already taken it. Every equality option below is judged on
whether it is compatible with that fork. (It rules out, e.g., leaning on Nuprl's
*stratified* PER construction for consistency, since that construction needs the
hierarchy disp discarded — see §7.2.)

---

## 3. Option A — Observational / Setoid Type Theory (the natural fit)

### 3.1 The mechanism

OTT (Altenkirch–McBride–Swierstra, "Observational Equality, Now!", PLPV 2007)
defines equality **by recursion on the structure of the type**, not as one
inductive family. The defining clauses (the ones that matter for optimization):

```
(f ∼_{Π A B} g)   :=  ∀ a. (f a ∼_{B a} g a)            -- THIS IS funext, by definition
(p ∼_{Σ A B} q)   :=  (fst p ∼_A fst q) ∧ (snd p ∼ snd q)
(x ∼_{Bool} y)    :=  structural (tt∼ff := ⊥, …)
(A ∼_{Type} B)    :=  (A → B) ∧ (B → A)   -- propext  (OTT);  or  A ≃ B  (HOTT, →univalence)
```

Plus `coe : (A B : Type) → (A ∼ B) → A → B` (transport) and `coh` (its coherence
proof), both defined by recursion on type structure. Equalities live in a
**proof-irrelevant** universe so they **erase**.

The modern line made this fully practical and machine-checked:
- **`SProp`** — definitionally proof-irrelevant propositions, *decidable*
  conversion (Gilbert–Cockx–Sozeau–Tabareau, POPL 2019).
- **TTobs** "Observational Equality: Now For Good" (Pujet–Tabareau, POPL 2022,
  Distinguished Paper) — `∼` as an *eliminator on the universe* (pattern-match the
  type's head), `cast` computing by recursion on type structure; **normalization,
  canonicity, decidable conversion all formally proved in Agda.**
- **CICobs** "Observational Equality Meets CIC" (Pujet–Leray–Tabareau, TOPLAS
  2025) — adds indexed inductives **and the rule `cast A A refl t ≡ t` extended to
  *neutral/open* `t`**, proven to keep conversion decidable, implemented in a
  Coq/Rocq extension. This is the result to copy if rewrites must fire under
  binders (see §3.4).

### 3.2 Mapping onto disp's meta-fields (the elegance argument)

Disp's meta record already has exactly the slots OTT needs. From `cut.disp:62`:

```disp
make_meta := {params, respond} ->
  { recognizer_params := params; functor := trivial_functor; respond; behavioral_specs := t }
```

| OTT concept | Disp home | Notes |
|---|---|---|
| per-type `∼` (the equality relation) | **new `eq` field** (5th meta slot), sibling to `functor` | the only addition |
| per-type `coe`/`cast` (transport) | **the existing `functor` field** | already earmarked for cubical transport — OTT puts a *simpler* occupant there (no interval) |
| coherence laws | **`behavioral_specs`** | already "runnable coherence laws"; under OTT they become `Eq`-proofs, not `Path`-proofs — *no interval needed to state them* |
| proof irrelevance | **free** | `refl := t` is already the unique proof |

This is the headline finding: **adopting OTT *reuses* the slot disp reserved for
cubical, with a strictly simpler function in it.** Cubical's `functor` is the
5-argument CCHM `comp` (type-path, cofibration, partial element, base, …; needs
`I`, `Partial`, `Glue`). OTT's `functor` is `cast : (A B:Type) → Eq Type A B → A →
B` — a plain recursion on type structure, **no `I`, no `Partial`, no cofibrations,
no `Glue`.** Same dispatch mechanism (look up the head type's meta), much less
machinery.

### 3.3 The minimal concrete change

Add an `eq` field and route `Eq`'s recognizer through it:

```disp
// per-type observational equality, read from the type's own `eq` meta-field
obs_eq := {A, x, y} -> (type_meta A).eq x y          // -> CheckerResult Bool

Eq : {A:Type} -> A -> A -> Type := {A, x, y} -> wait (make_recognizer
  {meta, v} -> {
    let p = meta.recognizer_params
    bind (obs_eq p.type p.lhs p.rhs) ({holds} -> Ok (and (tree_eq v refl) holds))
  }) (make_meta_eq { type:=A; lhs:=x; rhs:=y } eq_respond pi_or_struct_eq)

// the per-type clauses (the heart of OTT) — note funext rides bind_hyp:
bool_eq  := {x, y} -> Ok (tree_eq x y)               // discrete: structural
nat_eq   := {x, y} -> Ok (tree_eq x y)
pi_eq    := {A, B} -> {f, g} -> bind_hyp A ({a} -> obs_eq (B a) (f a) (g a))   // FUNEXT
sigma_eq := {A, B} -> {p, q} ->
  bind (obs_eq A p.fst q.fst) ({h} -> if h then (obs_eq (B p.fst) p.snd q.snd) else (Ok FF))
```

The funext clause `pi_eq` mints a fresh domain hypothesis with **`bind_hyp`** — the
*same* primitive disp already uses for Π-recognition — and the **walker
automatically polices it** (the body cannot inspect the abstract `a`). OTT's
`∀x. f x ∼ g x` is therefore not a bolt-on; it *is* disp's existing hypothesis
discipline applied to equality. That is as elegant a fit as this codebase offers.

For the degenerate types (Bool/Nat/String), `eq := structural` reproduces today's
behaviour exactly, so the change is conservative: existing tests keep passing; the
new power appears only at Π/Σ/Record.

### 3.4 Universes, termination, erasure — what disp can drop

- **Erasure: already won.** OTT works hard (the whole `Ω`/`SProp` apparatus) to
  make equality proofs erasable. Disp *starts* erased; proofs are `t`. The cost
  OTT can't erase is `cast` *on data* (it rebuilds the value, O(n)) — but in disp
  a *successful* rewrite produces the target tree directly, and `cast A A refl t`
  along an *identity* is where §3.4's CICobs rule (`≡ t` even on neutrals) lets the
  cast vanish under binders. Worth importing.
- **Decidability:** OTT keeps conversion **decidable**; disp doesn't need that
  guarantee (it tolerates divergence), so disp can adopt OTT's *equations* without
  paying for OTT's *normalization proof*.
- **Universe hierarchy:** OTT's published consistency uses a predicative `Ωᵢ`.
  The safe way to collapse it is **a single *impredicative, proof-irrelevant*
  prop universe** (CCobs, POPL 2023) — "never necessary to compute with
  impredicative proof terms," normalizing, consistent, decidable. A *proof-relevant*
  impredicative universe instead **breaks** decidability/normalization
  (Abel–Coquand 2020). **Lesson for disp:** keep the equality fragment
  proof-irrelevant (disp already does — `refl := t`) and impredicativity is safe;
  the danger is proof-*relevant* `Type:Type`, which is exactly the open disp
  Type:Type conjecture. **OTT-on-top-of-full-`Type:Type` is, per the literature, genuinely
  unstudied** — disp would be charting it.

### 3.5 Verdict

**Best fit, lowest risk, highest L1 payoff.** A local recognizer edit + one
meta-field + per-type `eq` clauses, with funext riding existing machinery and
proof-irrelevance free. Reuses the cubical `functor` slot with a simpler occupant.
The only foundational unknown (OTT atop `Type:Type`) is one disp already owns.

---

## 4. Option B — Cedille-style `φ` and zero-cost coercions (the rewrite primitive)

OTT gives the *proofs*; `φ` gives the *free swap*. They compose.

### 4.1 What `φ` is

Cedille's cast (Cedille Core, arXiv:1811.01318, Fig. 3), direction verified:

```
Γ ⊢ t : {t1 ≃ t2}      Γ ⊢ t1 : T
─────────────────────────────────────       |φ t - t1 {t2}|  =  |t2|
Γ ⊢ φ t - t1 {t2} : T
```

In words: given a proof that `t1 ≃ t2` and a typing `t1 : T`, the expression
**types at `T` but erases to (runs as) `t2`.** This is the optimizer's move
exactly:

> You already have `slow : T` (with its typing derivation). You found `fast`,
> structurally different, and proved `slow ≃ fast`. Then `φ proof - slow {fast} : T`
> keeps `slow`'s type and runs `fast`. Verified optimization in one combinator.

### 4.2 Why it's nearly free in disp — and what equality it needs

Because disp erases and conversion is hash-cons, `φ` in disp is *operationally* the
identity onto `t2`; the novelty is purely the **kernel admissibility rule** + a
**strip clause** `|φ ...| = t2`. Sketch (as a manifest-contract sibling of disp's
existing `checked`/`checked_apply`):

```disp
// phi e t1 t2 : T   when   e : HEq T t1 t2   and   t1 : T.   Strips to t2.
phi := {T, t1, t2, e} -> t2          // runtime = t2; the kernel admits the type T via e
// strip:  |phi T t1 t2 e| = |t2|     (drop the proof and the witness, keep the target)
```

**Critical:** `φ` is only *useful* with an equality over **distinct** trees. If
`t1 ≃ t2` were `tree_eq`, they'd be the same hash-cons node and there's nothing to
optimize. So `φ` *requires* the richer equality of §3 (observational) or §7
(operational) — and, ideally, a **heterogeneous** one (compare stripped trees
regardless of their predicates), because the cheap representation may inhabit a
different predicate. Disp's `Eq` is homogeneous; a `HEq` over stripped trees is the
deliberate design addition. `φ`'s soundness is exactly the soundness of that
equality (§7).

### 4.3 Representation independence without univalence — and its limit

Cedille's "univalence-lite": when two types' values share an **erased normal
form**, the back-and-forth coercions are *identity functions* (erase to `λx.x`), so
programs and proofs move between representations at **zero cost** (Diehl–Firsov–Stump,
"Zero-Cost Coercions," 2018; "Generic Zero-Cost Reuse," ICFP 2018). Examples:
`List`↔`Vec`, indexed↔non-indexed families, constructor subtyping, refinements.

The **limit, stated honestly:** when representations have *genuinely different
runtime structure* — unary vs binary `Nat`, list vs balanced tree, disp's own
`Nat` vs `HBin` — there is **no identity coercion**; the best you get is an ordinary
coercion that does real work, and zero-cost is lost. This is strictly weaker than
univalence (no transport across *arbitrary* equivalences) but strictly cheaper
where it applies. Disp should advertise this boundary rather than promise
representation independence in general.

### 4.4 What disp inherits, what it must build

- **Inherits:** the entire zero-cost story *in principle* — untyped substrate +
  `strip` + hash-cons + **`Intersection` already present** means even the famous
  "induction for λ-encodings via dependent intersection" is expressible today (a
  datum = `ι` of its fold and its own induction predicate, same underlying tree).
- **Must build:** the (heterogeneous) equality + `φ`/`δ` rules + their strip
  clauses; and its own consistency argument (Cedille's realizability proof does not
  port). The `β` "proof erases to a chosen witness" freedom (the Kleene trick) is
  what lets equality proofs vanish — disp's `refl := t` is the impoverished version.

### 4.5 Verdict

**The highest-leverage single addition for the optimization goal.** Small kernel
surface (`φ` + strip clause), composes with §3's equality, and "replace a program
with a provably-equal cheaper one at zero runtime cost" becomes a kernel idiom.
Build it *after* the equality it consumes exists.

---

## 5. Option C — Equality reflection / full extensional (maximum L1)

The endpoint of enlarging L1: the **reflection rule** `Γ ⊢ e : u =_A v ⟹ Γ ⊢ u ≡ v`
— *any proven equality becomes definitional*, usable in conversion with no
transport. Classic result (Hofmann 1995; Winterhalter–Sozeau–Tabareau, CPP 2019):
**ETT = ITT + funext + UIP**, with an effective derivation-to-derivation translation
that *inserts transports* where reflection was used.

- **Maximal optimizer power:** once `prog1 = prog2` is proved, the checker accepts
  `prog2` anywhere `prog1` fit, for free. This is precisely what an optimizer wants.
- **The standard objection — undecidable checking ("you can encode the halting
  problem as a conversion obligation") — costs disp essentially nothing**: disp's
  checking is *already* semi-decidable, and disp already validates *evidence* (run
  the predicate), not bare terms. ETT's downside is **pre-paid**.
- **The real friction:** reflection makes conversion **context-dependent** (it
  depends on the equalities currently in scope), which a global O(1) hash-cons
  check *cannot see*. Putting an equality oracle *inside* the conversion checker
  would also forfeit disp's cheap-conversion win.

**The reconciliation (and it is the same architecture §7 needs):** *don't* put
reflection in the kernel's conversion. Instead, keep conversion = cheap global
`tree_eq`, and have the **optimizer rewrite the term `prog1 ⇝ prog2` while emitting
a proof `prog1 = prog2`; the kernel checks the proof + cheap identity on the
rewritten term.** This is exactly the **egg / lean-egg** model:

- **egg** (Willsey et al., POPL 2021) — equality saturation over e-graphs, with a
  **proof-producing mode** emitting a step-by-step explanation (rule + direction +
  subterm position). egglog (PLDI 2023) adds Datalog-style *conditional* rules
  (≈ "rewrite valid when these typing facts hold").
- **lean-egg** (Rossel–Goens 2024) — egg rewrites *outside* Lean's kernel
  (untrusted), then **reconstructs a kernel-checkable proof term**; "neither egg
  nor the tactic is in the TCB." This is disp's verified-superoptimizer pattern,
  working today over a dependent kernel.
- lean-egg's dependent-type pains are **where disp is structurally better
  positioned**: it needs `funext`/`congr` traversal to rewrite under binders and
  hits the heterogeneous-congruence wall — but disp's bracket abstraction destroys
  `Lam`, conversion is global identity, and binder-occurrence info lives in the
  `Pi` type rather than the term (a disp design insight), so much of that pain is moot.

**No one has driven equality saturation into a Nuprl-style extensional/PER
kernel.** That combination is favourable (the emitted proof is consumed by
reflection/`φ`, avoiding transport gymnastics) and appears to be genuinely novel
territory for disp — and it is the concrete shape of the `GOALS.md` optimizer.

### 5.1 Verdict

**The right *operational* model for the optimizer** (rewrite + emit certificate +
cheap check), and ETT is the right *characterization* of its power. Do **not**
implement reflection inside conversion; implement it as "rewrite the tree, carry
the proof." Soundness of the proofs is §7.

---

## 6. Option D — Cubical / HOTT / XTT / 2LTT (the higher-dimensional axis)

### 6.1 The untangling that demotes cubical for disp

> **Representation independence = parametricity (white-box, structural).
> Univalence is only needed to compute on a *specific given equivalence*
> (black-box).**

This is the load-bearing result of the univalent-parametricity line
(Tabareau–Tanter–Sozeau, ICFP 2018 / JACM 2021): white-box transport "does not
rely on any type-level computation" and is *effective even in a non-univalent type
theory*; only opaque transport along an arbitrary supplied equivalence needs
univalence to compute (otherwise it gets stuck on the axiom).

**Disp's walker already enforces parametricity.** `param_walker` rejects
triage/inspection of kernel-minted neutrals — that *is* relational parametricity,
so every `Pi A B` is parametric in its `A`-argument and satisfies the free
theorems that *are* representation independence. **Disp therefore already owns the
white-box half of "univalence's benefits" without any equality machinery at all** —
the optimizer can use free theorems (derived from the walker discipline) as sound
rewrite rules directly. What disp lacks is only black-box transport along a *named*
equivalence, which is the rarer case.

### 6.2 The four points on the higher-dimensional axis

| Option | funext | Univalence | Repr. independence | Cost on disp |
|---|---|---|---|---|
| **OTT** (§3) | ✓ | ✗ | via parametricity (white-box) + manual `coe` along proven iso | **lowest**: `eq` field, no interval |
| **XTT** (Sterling–Angiuli–Gratzer, FSCD 2019) | ✓ | ✗ (definitional UIP, "cubical for sets") | same as OTT | low–moderate: cubical *form*, but "boundary separation" forces Kan rules (no per-type rules) and avoids stuck coercions; closed universe + `tycase` |
| **HOTT** (Altenkirch–Kaposi–Shulman) | ✓ | ✓ (by definition, **no interval**) | full (black-box too) | **high + unfinished**: per-type-former `Id`/`ap`/`coe`, higher-dim NbE; **the Kan layer that makes univalence compute is future work**; only the parametric substrate is published (POPL 2024); `narya` is experimental "Parametric OTT," no computing univalence |
| **Full cubical** CCHM/Cartesian (TYPE_THEORY.typ §13) | ✓ | ✓ (via `Glue`/`ua`) | full | **highest**: interval + face-lattice/cofibration solver + `comp`/`coe` at every former + `Glue` + partial elements; **plus disp's known §13.5 endpoint-dispatch gap** that already blocks `ua` transport |

Key reads:
- **Avoid full cubical for this goal.** It is the heaviest to implement, and disp's
  instance *already* hit the wall (endpoint-bound dispatch can't route `ua`/`Glue`).
  That wall is a *known fork in the road*, not a disp bug: XTT solves it by
  *abandoning* univalence (`tycase` + boundary separation); HOTT solves it by adding
  a Kan layer over a parametric substrate. The interval/Kan apparatus buys
  higher-dimensional structure disp does not need for optimization.
- **XTT** is the interesting compromise *if* a definitional, decidable extensional
  equality (UIP, funext, no univalence) is wanted with cleaner heterogeneous-equality
  handling than bare OTT — and it is explicitly engineered to be the **strict level
  of a 2LTT**.
- **HOTT** is the *best conceptual match* to disp (observational `Id` by recursion
  on type structure + per-type-former transport — exactly disp's meta-field design,
  no global interval) and the natural upgrade *if disp ever needs black-box
  univalent transport* — but it is an active research frontier with the univalent
  half unfinished. Not a near-term target.

### 6.3 2LTT — the architecture, not a competitor

Two-Level Type Theory (Annenkov–Capriotti–Kraus–Sattler) layers a **strict,
decidable-UIP** level under a **fibrant/rich** level, with conservativity. **Disp is
already two-level in spirit:** the strict level = `tree_eq` / hash-cons definitional
equality (the fast path for rewrites); a richer level = wherever observational or
(someday) univalent equality lives. RedPRL even realizes this computationally
("an extensional, proof-irrelevant *exact equality* coexisting with proof-relevant
*paths*"). **Takeaway:** treat §3's observational equality as a second level *over*
the existing `tree_eq` level, keep `tree_eq` as the optimizer's fast path, and the
richer equality only where funext/representation-swaps are needed. This is the
clean way to add power without slowing the common case.

### 6.4 Verdict

Cubical is the wrong default for disp's optimization goal. Parametricity (which the
walker already provides) + OTT covers representation independence in the
practically important cases. Keep HOTT in view as the eventual univalence upgrade
*because it fits the meta-field design*; keep 2LTT as the layering principle.

---

## 7. The soundness problem (L3) — the load-bearing section

This is where "drop the universe hierarchy / don't care about termination" collides
with "verified optimization," and where the design must be most careful.

> **Deep dive + two corrections.** A dedicated follow-up,
> [`OPERATIONAL_EQUIVALENCE_LICENSING.md`](OPERATIONAL_EQUIVALENCE_LICENSING.md),
> sharpens this section and supersedes the naïve phrasing below on two points:
> (1) **Raw operational/contextual equivalence collapses to `tree_eq` in disp**
> (tree calculus is reflective — Wand's theorem), so the licensing relation can't
> be contextual equivalence; it must be a **logical relation `~_T` over a
> restricted applicative observer class — which the walker already defines.**
> (2) The relation should be **Sands' *strong improvement* / *cost-equivalence*,
> not plain equivalence** (plain equivalence is unsound under fold/unfold, and only
> the asymmetric improvement preorder certifies asymptotic speedups). That doc also
> answers how far this stretches toward univalence/HITs (set-level by construction;
> a ladder up via dimensions / internal parametricity). Read it for the real story.

### 7.1 The tension

Disp is `Type : Type` (`TYPE_THEORY.typ` §11, with the Hurkens caveat flagged as
open). A `Type:Type` logic is **inconsistent** (Girard/Hurkens) — so the *equality
fragment can prove `prog1 = prog2` for unequal programs*, and an optimizer that
trusts such proofs **emits wrong programs**. **An inconsistent equality logic is an
unsound optimizer.** L3 is not optional for *verified* optimization; it is the
whole point.

### 7.2 Why "drop the hierarchy" makes this worse (the sharp constraint)

Nuprl stays consistent partly because its PER model is **stratified** — Allen's
construction builds `Uᵢ`'s PER before `Uᵢ₊₁`'s. **Dropping the hierarchy to
`Type:Type` removes exactly the stratification that makes the PER model
well-founded.** So the naive plan "Nuprl-style PER semantics + `Type:Type`" is
*self-defeating*: you cannot get the consistency proof from the stratified model
after discarding the strata. **Dropping universes and keeping a stratified-semantic
sound logic are in direct conflict.**

### 7.3 The escape: license rewrites by *operational* equivalence, in a *total* fragment

Two compatible moves, both with strong precedent:

**(a) Use operational/computational equivalence as the rewrite-licensing relation.**
Howe's `~` (bisimulation under the operational semantics) is **sound by
construction, independently of the object logic** — it is a statement about *how
trees compute*, not a proposition in a possibly-inconsistent theory. So even a
`Type:Type` object logic cannot corrupt it. Nuprl did *exactly this* to verify real
optimizations: **Rahli–Bickford–Anand, "Formal Program Optimization in Nuprl Using
Computational Equivalence and Partial Types" (ITP 2013)** — proving bisimulations
between untyped programs to optimize a verified Paxos. This is the closest existing
system to disp's goal, and it deliberately licenses by *operational* equivalence,
not raw propositional equality.

For disp this is natural: define a **`~` (operational equivalence) predicate / proof
system** (a coarser, *sound* over-approximation of `tree_eq`), let the optimizer
emit `~`-proofs, and let `φ` (§4) consume them. `tree_eq` (finest, syntactic) and
the full propositional `Eq` (potentially unsound under `Type:Type`) are both wrong
for licensing; **`~` is the right relation** — strictly coarser than `tree_eq`
(relates distinct-but-equivalent trees) and strictly *safer* than full `Eq` (sound
regardless of the logic). Disp's `behavioral_specs` slot is the natural home for a
*library of proven `~`-equations* (map-fusion, etc.) — i.e. the optimizer's rewrite
rules *are* behavioral specs.

**(b) Phase-separate total proofs from partial programs.** Nuprl's partial types
`A̅` (Constable–Smith, LICS 1987; Smith 1989) let divergent computations exist
while only *total* types carry logical force; fixpoint induction is sound only on
*admissible* types (Crary, CADE 1998). Disp's deferred **`Total` / `wf_fix`** is
exactly this gate. The discipline: **programs may be partial (general recursion,
don't-care termination); a rewrite-licensing proof must live in the total fragment
where `False` stays uninhabited.** The relation `~` lives in the consistent layer;
the *relata* (the programs) may be partial. This is the phase separation that
reconciles both halves of disp's stance.

### 7.4 What this means concretely for disp

- The optimizer's trusted core is **not** the full type theory; it is **(i) the
  operational semantics (defining `~`) + (ii) the kernel's proof checker**. Both are
  small and model-independent. `Type:Type` inconsistency in the *general* logic does
  not reach them.
- `φ`-swaps are sound **iff** backed by a `~`-proof (or an `Eq`-proof from a fragment
  known total/consistent), **not** an arbitrary `Eq`-proof.
- This is *also* the answer to §5's context-dependence worry: conversion stays cheap
  `tree_eq`; soundness rides the emitted `~`-certificate.

### 7.5 Verdict

L3 is the real work. The good news: it is **decoupled from the equality type
choice** — whether disp adopts OTT (§3) or stops at φ (§4), the *licensing relation*
should be operational equivalence proven in a total fragment. That choice is what
makes "drop universes + don't care about termination + sound optimizer"
*simultaneously* achievable, and it has a working precedent (Nuprl/Paxos).

---

## 8. Synthesis: the recommended stack and a staged roadmap

### 8.1 The layered design

```
   ┌─────────────────────────────────────────────────────────────────────┐
   │ OPTIMIZER (external; untrusted): equality saturation / neural search  │
   │   rewrites tree e ⇝ e', emits a ~-certificate                         │  §5, GOALS.md
   ├─────────────────────────────────────────────────────────────────────┤
   │ φ  zero-cost cast: present e' at e's type, strips to e'               │  §4 (Option B)
   ├─────────────────────────────────────────────────────────────────────┤
   │ ~  operational-equivalence proofs (TOTAL fragment) — licenses swaps   │  §7 (Option C/soundness)
   │     library of proven ~-equations  ==  behavioral_specs               │
   ├─────────────────────────────────────────────────────────────────────┤
   │ Eq observational: funext via bind_hyp; cast in `functor` field        │  §3 (Option A)
   ├─────────────────────────────────────────────────────────────────────┤
   │ tree_eq  O(1) hash-cons definitional equality — the fast path         │  current, kept
   ├─────────────────────────────────────────────────────────────────────┤
   │ parametric walker — free theorems = representation independence        │  current, already present
   └─────────────────────────────────────────────────────────────────────┘
```

Cubical/HOTT sit *off* this stack; pull HOTT in only if black-box univalent
transport is ever required (§6.4).

### 8.2 Elegance / payoff ranking (for disp specifically)

| Rank | Move | L1 gain | L2 | L3 risk | Impl. cost on disp |
|---|---|---|---|---|---|
| 1 | **Observational `Eq`** (§3) | large (funext) | free | low (proof-irrelevant already) | small: 1 meta-field + per-type `eq` clauses; funext reuses `bind_hyp` |
| 2 | **`φ` + heterogeneous `~`** (§4) | — (enables swaps) | free | governed by §7 | small kernel rule + strip clause |
| 3 | **Operational-equivalence licensing + total fragment** (§7) | — | — | **resolves it** | medium: define `~`, a totality gate (`Total`/`wf_fix`) |
| 4 | **Eq-saturation optimizer emitting certificates** (§5) | — | — | untrusted by design | large but external; the `GOALS.md` payload |
| 5 | **HOTT** (§6) | univalence (black-box transport) | free-ish | — | **high + unfinished**; only if needed |
| — | **Full cubical** (§6) | univalence | the §13 gap | — | **highest**; not recommended |

### 8.3 Staged roadmap

- **Phase 0 (today).** Degenerate `Eq` (= `tree_eq`); parametric walker gives free
  theorems; `Intersection` present; `strip` present. Representation independence
  available *externally* via the walker's parametricity.
- **Phase 1 — Observational `Eq` (Option A).** Add the `eq` meta-field; give
  Bool/Nat structural `eq` (conservative), Π `pi_eq` (funext via `bind_hyp`), Σ/Record
  componentwise. Re-state `behavioral_specs` coherence laws as `Eq`-proofs (drop the
  interval from them). Now funext-level optimizations are *provable*.
- **Phase 2 — `φ` + heterogeneous `~` (Options B + soundness).** Add `φ` (kernel rule
  + strip clause) and an operational-equivalence relation `~` over stripped trees,
  in a total fragment. Populate `behavioral_specs` with a starter library of
  `~`-equations (map fusion, `id`-elimination, β/η laws). Now "swap to a
  provably-equivalent cheaper tree" is a sound, zero-cost idiom.
- **Phase 3 — certificate-driven optimizer (Option C operationally).** Build (or
  bolt on) equality saturation that rewrites trees and emits `~`-certificates the
  kernel checks; this is the `GOALS.md` self-improving optimizer's verifiable core.
  Disp's erased trees + global conversion sidestep lean-egg's under-binder/transport
  pains.
- **Phase 4 (optional, research).** If black-box univalent transport is ever needed,
  adopt **HOTT**'s per-type-former observational identity (fits the meta-fields) — *not*
  full cubical.

---

## 9. Open questions

1. **OTT atop `Type:Type`.** No literature studies observational `∼`/`cast` over a
   *proof-relevant impredicative* universe. Disp would be charting it; the safe
   anchor is to keep the equality fragment proof-irrelevant (it already is) and rest
   soundness on §7's operational route, not on the logic. Ties directly to disp's
   Type:Type conjecture (`TYPE_THEORY.typ` §11.6 / Appendix A).
2. **A disp realizability/PER model.** Cedille's and Nuprl's consistency proofs do
   not transfer mechanically. What is disp's? The sealing/step-indexed-LR recipe is
   the candidate; it needs to be carried out at least informally before the optimizer
   can be called "verified" rather than "empirically sound."
3. **Heterogeneous `Eq` / `~` over stripped trees.** Exact formation + the `φ`/`δ`
   strip clauses; interaction with the walker (an `~`-proof must not leak hyp
   structure). Does the existing `checked`/`strip` machinery host `φ` cleanly?
4. **Defining `~` in-language.** Howe's congruence is a coinductive bisimulation up
   to tree-calculus reduction. What is its disp encoding, and how does the *total
   fragment* certify `~`-proofs without running the (partial) program? (Bounded
   bisimulation? `behavioral_specs` as the proof library?)
5. **`cast`-on-`refl`-on-neutrals (CICobs 2025).** Is the under-binder
   no-op rule worth importing given disp's terms are erased (where does a residual
   `cast` even survive)? Likely only relevant if `cast` is materialized in the
   non-stripped representation.
6. **Does the optimizer ever need black-box univalence?** Concretely: is there a
   target optimization that is *not* expressible as a `~`-equation or a white-box
   (parametric) free theorem, and *requires* transport along a named non-identity
   equivalence? If not, HOTT/cubical stay off the roadmap permanently.

---

## 10. References

### Observational / Setoid type theory
- Altenkirch, McBride, Swierstra. *Observational Equality, Now!* PLPV 2007. DOI 10.1145/1292597.1292608.
- Altenkirch. *Extensional Equality in Intensional Type Theory.* LICS 1999. DOI 10.1109/LICS.1999.782636.
- Altenkirch, Boulier, Kaposi, Tabareau. *Setoid Type Theory — A Syntactic Translation.* MPC 2019, LNCS 11825, 155–196. DOI 10.1007/978-3-030-33636-3_7.
- Altenkirch, Boulier, Kaposi, Sattler, Sestini. *Constructing a Universe for the Setoid Model.* FoSSaCS 2021, LNCS 12650. DOI 10.1007/978-3-030-71995-1_1.
- Gilbert, Cockx, Sozeau, Tabareau. *Definitional Proof-Irrelevance without K.* POPL 2019, PACMPL 3. DOI 10.1145/3290316. (SProp)
- Pujet, Tabareau. *Observational Equality: Now For Good (TTobs).* POPL 2022, PACMPL 6, Art. 32. DOI 10.1145/3498693.
- Pujet, Tabareau. *Impredicative Observational Equality (CCobs).* POPL 2023, PACMPL 7, Art. 74. DOI 10.1145/3571739.
- Pujet, Leray, Tabareau. *Observational Equality Meets CIC (CICobs).* ESOP 2024, LNCS 14576, 275–301 (DOI 10.1007/978-3-031-57262-3_12); journal TOPLAS 47(2) Art. 6, 2025 (DOI 10.1145/3719342). — `cast A A refl t ≡ t` on neutrals.
- Pujet. *Computing with Extensionality Principles in Type Theory.* PhD thesis, Nantes, 2022.
- Abel, Coquand. *Failure of Normalization in Impredicative Type Theory with Proof-Irrelevant Propositional Equality.* LMCS 16(2), 2020. arXiv:1911.08174.
- Sirman, Lennon-Bertrand, Krishnaswami. *Implementing OTT with NbE.* TYPES 2024, LIPIcs 336, 5:1–5:22. DOI 10.4230/LIPIcs.TYPES.2024.5.

### Cedille / dependent intersection / zero-cost coercions
- Stump. *The Calculus of Dependent Lambda Eliminations.* JFP 27 (2017) e14. DOI 10.1017/S0956796817000053.
- Stump. *From Realizability to Induction via Dependent Intersection.* APAL 169(7) (2018) 637–655. DOI 10.1016/j.apal.2018.03.002.
- Stump. *Syntax and Typing for Cedille Core.* arXiv:1811.01318 (2018). — the φ/ρ/δ/β/ι/∀/{≃} rules + erasure.
- Stump. *Syntax and Semantics of Cedille.* arXiv:1806.04709 (2018). — realizability; consistency (Thm 2); non-normalization (Obs 3); step-bounded conversion.
- Kopylov. *Dependent Intersection: A New Way of Defining Records in Type Theory.* LICS 2003, 86–95. DOI 10.1109/LICS.2003.1210048.
- Diehl, Firsov, Stump. *Zero-Cost Coercions for Program and Proof Reuse.* arXiv:1802.00787 (2018).
- Diehl, Firsov, Stump. *Generic Zero-Cost Reuse for Dependent Types.* ICFP 2018, PACMPL 2, Art. 104. DOI 10.1145/3236799.
- Jenkins, Marmaduke, Stump. *Simulating Large Eliminations in Cedille.* TYPES 2021, LIPIcs Art. 9 (2022). DOI 10.4230/LIPIcs.TYPES.2021.9. — large eliminations *without* a universe.
- Firsov, Blair, Stump. *Efficient Mendler-Style Lambda-Encodings in Cedille.* IFL 2018. arXiv:1803.02473. (constant-time predecessor)
- Jenkins. PhD thesis (U. Iowa, 2023); Marmaduke. *A Proof-Theoretic Redesign of CDLE* ("Cedille 2"), PhD thesis (U. Iowa, 2024).

### Computational / extensional type theory; equality reflection
- Constable, Allen, et al. *Implementing Mathematics with the Nuprl Proof Development System.* Prentice-Hall, 1986.
- Allen. *A Non-Type-Theoretic Semantics for Type-Theoretic Language.* PhD thesis, Cornell, 1987. (the PER model)
- Howe. *Equality in Lazy Computation Systems.* LICS 1989. (computational equivalence `~`)
- Anand, Rahli. *Towards a Formally Verified Proof Assistant.* ITP 2014, LNCS 8558, 27–44. DOI 10.1007/978-3-319-08970-6_3. (Nuprl-in-Coq; consistency = empty PER)
- Constable, Smith. *Partial Objects in Constructive Type Theory.* LICS 1987. Smith, PhD thesis, Cornell, 1989. Crary, *Admissibility of Fixpoint Induction over Partial Types*, CADE 1998 (DOI 10.1007/BFb0054265).
- Rahli, Bickford, Anand. *Formal Program Optimization in Nuprl Using Computational Equivalence and Partial Types.* ITP 2013, LNCS 7998, 261–278. DOI 10.1007/978-3-642-39634-2_20. — **the closest precedent to disp's goal.**
- Hofmann. *Extensional Concepts in Intensional Type Theory.* PhD thesis, Edinburgh, 1995 (Springer 1997). (ETT = ITT + funext + UIP)
- Winterhalter, Sozeau, Tabareau. *Eliminating Reflection from Type Theory.* CPP 2019. DOI 10.1145/3293880.3294095. (effective ETT→ITT translation; halting-problem-as-equality)
- Bauer, Gilbert, Haselwarter, Pretnar, Stone. *Design and Implementation of the Andromeda Proof Assistant.* TYPES 2016 (LIPIcs 2018). arXiv:1802.06217. (tiny trusted nucleus + external equality handlers)

### Equality saturation / proof-producing rewriting (optimizer side)
- Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha. *egg: Fast and Extensible Equality Saturation.* POPL 2021, PACMPL 5, Art. 23. DOI 10.1145/3434304.
- Zhang, Wang, Flatt, Cao, Zucker, Rosenthal, Tatlock, Willsey. *Better Together: Unifying Datalog and Equality Saturation (egglog).* PLDI 2023, PACMPL 7. DOI 10.1145/3591239.
- Flatt, Coward, Willsey, Tatlock, Panchekha. *Small Proofs from Congruence Closure.* FMCAD 2022. DOI 10.34727/2022/isbn.978-3-85448-053-2_9.
- Rossel, Goens, et al. *lean-egg* (MSc thesis + tooling, 2024). github.com/marcusrossel/lean-egg. — eq-sat emitting kernel-checkable proofs over a dependent kernel.
- Kœhler, Goens, et al. *Guided Equality Saturation.* POPL 2024, PACMPL 8. DOI 10.1145/3632900. (relevant to neural guidance)
- Joshi, Nelson, Randall. *Denali: A Goal-Directed Superoptimizer.* PLDI 2002. (e-graph superoptimization)

### Cubical / HOTT / 2LTT / parametricity
- Cohen, Coquand, Huber, Mörtberg. *Cubical Type Theory.* TYPES 2015, LIPIcs 69, 5:1–5:34. arXiv:1611.02108. (CCHM, De Morgan)
- Angiuli, Brunerie, Coquand, Harper, Hou (Favonia), Licata. *Syntax and Models of Cartesian Cubical Type Theory.* MSCS 31(4):424–468, 2021. DOI 10.1017/S0960129521000347. (ABCFHL)
- Sterling, Angiuli, Gratzer. *Cubical Syntax for Reflection-Free Extensional Equality (XTT).* FSCD 2019, LIPIcs 131, 31:1–31:25. arXiv:1904.08562.
- Angiuli, Harper, Wilson. *Computational Higher-Dimensional Type Theory.* POPL 2017. DOI 10.1145/3009837.3009861. RedPRL: arXiv:1807.01869 (two-level: exact equality + paths).
- Altenkirch, Chamoun, Kaposi, Shulman. *Internal Parametricity, without an Interval.* POPL 2024, PACMPL 8, Art. 78. arXiv:2307.06448. (HOTT substrate)
- Shulman. *Towards a Third-Generation HOTT* (n-Category Café, 2022, 3 parts); *narya* proof assistant (experimental "Parametric OTT").
- Tabareau, Tanter, Sozeau. *Equivalences for Free! Univalent Parametricity for Effective Transport.* ICFP 2018, PACMPL 2, Art. 92. DOI 10.1145/3236787. *The Marriage of Univalence and Parametricity.* JACM 68(1):5, 2021. arXiv:1909.05027. — **representation independence = parametricity; univalence only for black-box transport.**
- Angiuli, Cavallo, Mörtberg, Zeuner. *Internalizing Representation Independence with Univalence.* POPL 2021, PACMPL 5. DOI 10.1145/3434293.
- Annenkov, Capriotti, Kraus, Sattler. *Two-Level Type Theory and Applications.* arXiv:1705.03307; MSCS 2023. Kovács. *Staged Compilation with Two-Level Type Theory.* ICFP 2022. DOI 10.1145/3547641.
- Zhang (Tesla). *A Tutorial on Implementing De Morgan Cubical Type Theory.* arXiv:2210.08232. (the implementation-cost inventory)

### Disp-internal
- `GOALS.md`; `TYPE_THEORY.typ` (§11 Type:Type, §13 cubical); `KERNEL_DESIGN.md`; `lib/kernel/{cut,engine,types}.disp` (`make_meta`, `Eq`, `Intersection`, `param_walker`).
- `research/CUBICAL_TYPE_THEORY_FOR_DUMMIES.typ`; `research/MODAL_TYPES_INVESTIGATION.md`.
