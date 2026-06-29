# Singular Learning Theory — a reference, and its relation to disp

> **Status.** Research note (2026-06-29). External-theory survey + a disp-connection analysis,
> in the genre of `FOUNDATIONS.md` / `research/EFFECT_SYSTEMS_SURVEY.md`. Part I is settled
> mathematics (Watanabe); Parts III–V are *proposed* connections, tagged **[load-bearing]** /
> **[speculative]** / **[breaks]** with the same discipline `FOUNDATIONS.md` uses. Nothing here is
> built. Companion reading: `OPTIMIZER.typ` (§2 objective, §4 cost coeffect, §6 equality, §8
> search-as-differentiation), `research/effects-and-coeffects.typ`.

> **Thesis in one paragraph.** Singular Learning Theory (SLT) is the asymptotic theory of Bayesian
> inference for models that are *non-identifiable* — where many parameters compute the same function,
> so the set of optima is not a point but a *variety with singularities*. Its central result replaces
> the dimension count `d/2` of classical model selection with a birational invariant, the **Real Log
> Canonical Threshold (RLCT)** `λ`, that measures *effective* complexity and equals *compressibility*.
> disp is, in SLT's vocabulary, a *maximally singular* setting by construction: a reflective,
> intensional substrate where `tree_eq` is the finest equality and the walker's `~_T` is a coarse
> functional one, so the fibre "many trees → one behaviour" is enormous. SLT is therefore the natural
> statistical geometry of exactly the gap disp's optimizer lives on — between syntax and behaviour —
> and two 2025 results (Programs-as-Singularities; MDL-meets-SLT) build the discrete crossing. The
> payoff disp could take: a principled, degeneracy-aware `cheapness`, a complexity measure for
> *specifications*, and a precise name for the equivalence sitting *between* `tree_eq` and `~_T`.

---

## Contents

- **Part I — SLT proper** (§1 the regular assumption · §2 singular models · §3 the free energy · §4 the zeta function and the RLCT · §5 resolution of singularities, with worked examples · §6 the headline theorems · §7 estimating λ in practice · §8 the thermodynamic / phase-transition picture)
- **Part II — Crossing to discrete programs** (§9 the impedance mismatch · §10 Programs as Singularities · §11 MDL = SLT · §12 the algorithmic-information ancestor)
- **Part III — SLT in disp** (§13 the dictionary · §14 disp's objective is already a free energy · §15 the three nested equivalences)
- **Part IV — Potential applications** (§16 degeneracy-aware cheapness · §17 RLCT as synthesis difficulty · §18 estimation & the λ-proxy · §19 phase transitions and the search · §20 the shared differential structure & self-improvement)
- **Part V — The honest ledger** + a first experiment + glossary + sources

---

# Part I — Singular Learning Theory

## 1. The regular world and its one load-bearing assumption

Fix data `D_n = {x_1,…,x_n}` drawn i.i.d. from an unknown truth `q(x)`, a model family `p(x|w)`
with parameters `w ∈ W ⊆ ℝ^d`, and a prior `φ(w)`. The object that organizes everything is the
**KL divergence from the truth**

$$K(w) \;=\; \mathrm{KL}\big(q \,\|\, p(\cdot|w)\big) \;=\; \int q(x)\,\log\frac{q(x)}{p(x|w)}\,dx \;\ge\; 0,$$

and the **optimal set** `W_0 = { w : K(w) = 0 }` of parameters that realize the truth.

Classical statistics assumes the model is **regular**: `W_0` is a single point `w_0`, and the Hessian
of `K` there — which equals the **Fisher information matrix** `I(w_0)` — is positive-definite. Then
near `w_0`,

$$K(w) \;\approx\; \tfrac12\,(w-w_0)^{\mathsf T}\, I\, (w-w_0),$$

a non-degenerate paraboloid. *Every* familiar asymptotic result — asymptotic normality of the MLE,
the χ² likelihood-ratio law, the Laplace approximation, BIC's `(d/2)·log n` penalty, Cramér–Rao —
is a corollary of that one quadratic. Regularity is the assumption that the parameter-to-distribution
map `w ↦ p(·|w)` is, locally, an injective immersion.

## 2. Singular models: when the assumption dies

A model is **singular** when that fails: either `W_0` is positive-dimensional (a curve, a surface, a
variety — not a point), or `I(w)` is degenerate (non-invertible) somewhere on `W_0`. Equivalently,
`w ↦ p(·|w)` is not locally injective — distinct parameters give the same distribution.

This is not exotic; it is *generic* for expressive models:

- **Neural networks** — permutation of hidden units, weight scaling under homogeneous activations,
  and *dead/redundant units* all map many `w` to one function. The optimum is a high-dimensional
  variety riddled with singularities.
- **Mixture models** — label-switching, and components that vanish (weight → 0) or coincide.
- **Hidden Markov models, Bayesian networks, reduced-rank regression, Boltzmann machines, matrix
  factorization** — all singular.

For these the paraboloid is the *wrong* local model. Near `W_0`, `K(w)` looks like a higher-order or
anisotropic zero — `K ∼ w_1^4`, or `K ∼ w_1^2 w_2^2`, or worse — and the Gaussian integral that
underwrites classical asymptotics is simply incorrect. SLT is the replacement.

## 3. The free energy — the central object

The Bayesian evidence (marginal likelihood) is

$$Z_n \;=\; \int \prod_{i=1}^n p(x_i|w)\,\varphi(w)\,dw \;=\; \int e^{-n L_n(w)}\,\varphi(w)\,dw,
\qquad L_n(w) = -\tfrac1n\textstyle\sum_i \log p(x_i|w),$$

and the **free energy** is `F_n = −log Z_n`. This single quantity *is* the model's stochastic
complexity / Bayesian description length (Part II makes the "description length" reading literal).
Model selection = choosing the family with the smallest `F_n`.

Split the empirical loss `L_n(w) = S_n + K_n(w)`, where `S_n → S` is the irreducible empirical
entropy of the truth and `K_n(w) → K(w)` is the empirical KL. Then

$$F_n \;=\; n S_n \;-\; \log\!\int e^{-n K_n(w)}\,\varphi(w)\,dw.$$

Everything interesting is in that integral, and its `n → ∞` asymptotics are controlled entirely by
how the prior-weighted *volume* of near-optima shrinks as you approach `W_0` — i.e. by the local
geometry of the zero-set of `K`. In a regular model that volume shrinks like a Gaussian and you
recover BIC. In a singular model it shrinks like a *power law with log corrections*, and the exponent
is the RLCT.

## 4. The zeta function and the RLCT

Encode the near-optimum volume in the **state density** `v(t) = ∫_{K(w)=t} φ(w)/|∇K| dS` — the
prior mass at KL-level `t`. Its small-`t` behaviour is read off the **zeta function**

$$\zeta(z) \;=\; \int_W K(w)^{z}\,\varphi(w)\,dw, \qquad z\in\mathbb C.$$

`ζ` converges for `Re(z) > 0` and (Watanabe, via Hironaka — §5) extends **meromorphically** to all of
`ℂ` with poles only at *negative rationals*. Define:

> **The learning coefficient `λ` (RLCT)** is `−(largest pole of ζ)`; its **multiplicity `m`** is that
> pole's order.

Then `v(t) ∼ c · t^{λ−1} (−log t)^{m−1}` as `t → 0⁺`, the integral behaves as
`∫ e^{−nK}φ dw ∼ C · n^{−λ}(log n)^{m−1}`, and substituting gives **Watanabe's free energy formula**:

$$\boxed{\,F_n \;=\; n S_n \;+\; \lambda \log n \;-\; (m-1)\log\log n \;+\; O_p(1)\,}$$

Compare BIC's `nS_n + (d/2)\log n`: the dimension `d/2` is replaced by the birational invariant `λ`,
and a genuinely new `log log n` term appears with the multiplicity. **`λ` is the effective number of
parameters (over two); it is generally a rational `< d/2`.**

## 5. Resolution of singularities — how `λ` is actually computed

The meromorphy of `ζ` and the *computation* of `λ` both come from **Hironaka's resolution of
singularities** (1964). For a real-analytic `K(w) ≥ 0` there is a manifold `U` and a proper analytic
map (a finite composition of blow-ups) `g : U → W` that turns `K` into a **normal crossing**
(monomial) in every local chart:

$$K(g(u)) \;=\; u_1^{2k_1}\cdots u_d^{2k_d}, \qquad |g'(u)| \;=\; b(u)\,|u_1^{h_1}\cdots u_d^{h_d}|,\ \ b\neq 0.$$

In monomial coordinates the integral factorizes and the pole is immediate. The RLCT is

$$\lambda \;=\; \min_{i}\ \frac{h_i + 1}{2k_i}, \qquad m \;=\; \#\{\,i : \text{the minimum is attained}\,\}$$

(minimized over charts of the cover). This is literally the **log canonical threshold** of algebraic
geometry, ported to the real semianalytic setting — which is *why* `λ` is a birational invariant
(coordinate-free, intrinsic to the singularity, not to the parameterization).

**Worked example A (1-D, regular vs flat).** `K(w) = w^{2k}` with smooth prior `φ(0) > 0`. Already
monomial: `∫|w|^{2kz}φ dw` converges for `Re(z) > −1/(2k)`, simple pole at `z = −1/(2k)`. So
`λ = 1/(2k)`, `m = 1`. The regular case is `k = 1`: `λ = 1/2 = d/2`. As `k → ∞` (a *flatter*
minimum), `λ → 0` — flatter = lower effective complexity = more compressible. **Flatness is
degeneracy is low `λ`.**

**Worked example B (2-D, a true singularity).** `K(w_1,w_2) = w_1^2 w_2^2`. The optimal set
`W_0 = {w_1=0} ∪ {w_2=0}` is the *union of the two axes* — a 1-dimensional variety with a crossing at
the origin: classically non-identifiable. Already normal-crossing (`k_1=k_2=1`, `h_1=h_2=0`), so
`λ = min(1/2, 1/2) = 1/2` and **both coordinates attain it → `m = 2`**. Contrast the regular 2-D
model `K = w_1^2 + w_2^2`, which has `λ = 1 = d/2`, `m = 1`. The singular crossing is *effectively
half the model* and carries a multiplicity. (Keep this picture — §15 reads the two crossing
components as two algorithmic "families" meeting at a degenerate program.)

Computing RLCTs for real models (reduced-rank regression, two-layer tanh nets, naive Bayes, matrix
factorization) is hard and is its own active subfield; closed forms exist only for structured cases.
§7 is the practical escape.

## 6. The headline theorems

- **Free energy.** `F_n = nS_n + λ log n − (m−1) log log n + O_p(1)` (§4). Smaller `λ` ⇒ smaller free
  energy ⇒ a model the evidence prefers.
- **Generalization.** The expected Bayes generalization error (KL from truth to the Bayes predictive)
  is `𝔼[G_n] ≃ λ/n`. Compare a regular model's `d/2n`. **Lower `λ` ⇒ better generalization** — and
  since singular models have `λ < d/2`, *singularity is an inductive bias toward generalization.*
  Occam's razor is not added as a penalty; it falls out of the Bayesian geometry.
- **The two birational invariants.** `λ` governs the *mean*; a second invariant `ν` (the **singular
  fluctuation**) governs the *variance* and the train/test gap. Watanabe's "equations of state" relate
  Bayes/Gibbs × train/test errors through `(λ, ν)`.
- **Inequalities.** `0 < λ ≤ d/2`, equality iff regular. If `W_0` has dimension `d_0`, then
  `λ ≤ (d − d_0)/2`. `λ ∈ ℚ`, `m ∈ ℤ⁺`.

## 7. Estimating `λ` in practice (the bridge to applicability)

You cannot resolve singularities for a real model — but you can *estimate* `λ`, which is what makes
SLT an empirical tool rather than only a theorem:

- **WBIC** (Widely Applicable BIC; Watanabe, JMLR 2013). Sample the posterior at the **single special
  inverse temperature `β* = 1/log n`** and average the loss:
  `WBIC = 𝔼^{β*}_w[ n L_n(w) ]`. Then `𝔼[WBIC] = nS_n + λ log n + o(log n)` — so a tempered-posterior
  expectation *estimates `λ`* with no algebraic geometry.
- **WAIC** (Widely Applicable Information Criterion). A posterior-computable estimator of
  generalization error, asymptotically correct for *singular* models (where AIC's `d` is wrong);
  uses the functional variance (the `ν` channel).
- **LLC** — the **local learning coefficient** (Lau–Murfet–Wei–Hoogland et al.). A *local* RLCT around
  one parameter/basin, estimated at scale via SGLD on the tempered posterior. This is the workhorse of
  *developmental interpretability* — `λ̂` tracked over training to detect structure formation.

The practical takeaway for any prospective disp use (§18): **degeneracy/effective-complexity is
*measurable* by sampling a tempered posterior — you never need the resolution.**

## 8. The thermodynamic picture and phase transitions

The name "free energy" is literal. The tempered posterior `p_β(w) ∝ φ(w) e^{−nβ L_n(w)}` is a Gibbs
measure at inverse temperature `β`, and `F_n(β)` is its thermodynamic free energy. As the data count
`n` (or `β`) varies, the posterior's mass can shift *discontinuously* between competing singularities
of different `λ` — **first-order phase transitions in learning**. The generic pattern: with little
data the posterior sits in high-`λ` regions (easy to fit, "fat," low curvature); as data accumulates
the `λ log n` term bites and it transitions to lower-`λ`, better-generalizing regions. This is the SLT
account of **stagewise development** in neural networks and of **grokking** as a transition between
basins of different effective complexity (Carroll–Hoogland–Murfet; the devinterp programme). Hold
this picture for §19.

---

# Part II — Crossing to discrete programs

## 9. The impedance mismatch, stated plainly

SLT is built on *continuous* parameters `w ∈ ℝ^d`, a *real-analytic* `K(w)`, *Bayesian* posteriors,
and asymptotics in *sample size* `n`. disp is *discrete* tree-space, a *hard 0/1* type gate plus a
cost, *MAP/combinatorial* search, and a *fixed* spec `T`. None of the heavy machinery (zeta integrals,
blow-ups) transfers literally. The honest question is whether the *concepts* — non-identifiability,
effective complexity, degeneracy-as-Occam, the geometry that distinguishes algorithms — transfer. Two
2025 results say yes, by building the discrete crossing explicitly.

## 10. Programs as Singularities (the key bridge)

Murfet & Troiani, *Programs as Singularities* ([arXiv 2504.08075](https://arxiv.org/abs/2504.08075);
[Timaeus writeup](https://timaeus.co/research/2025-04-10-programs-as-singularities)), April 2025:

- **The construction.** Embed discrete Turing-machine codes into a smooth family of **"noisy codes"**
  forming a continuous parameter space, on which a potential function (a negative-log-likelihood) has
  the machines as **critical points**. Now SLT applies — each program *is* a singularity.
- **The bridge map.** The **Ehrhard–Regnier derivative** from *differential linear logic* relates the
  Taylor expansion of the potential at a program-critical-point to the combinatorics of "error
  syndromes," so that **local singularity geometry encodes the machine's internal structure.**
- **The headline.** *Two algorithms computing the same function can be singularities of different
  geometry — so the Bayesian posterior discriminates between distinct implementations, contrary to a
  purely functional view of inference.* Occam's razor becomes **geometric, not merely functional**:
  "simplicity" is a property of the singularity, not of the input/output map.

That headline is the disp insight (§15), and the *mechanism* — the Ehrhard–Regnier derivative — is
the **same differential-linear-logic object `OPTIMIZER.typ` §8 already invokes** (`δ` = exponential
bang `!`, the backward pass = its derivative `∂`). disp's optimizer and SLT's program-geometry are
reaching for one structure from opposite ends (§20).

## 11. MDL = SLT (effective complexity is compressibility)

*Compressibility Measures Complexity: MDL Meets SLT*
([arXiv 2510.12077](https://arxiv.org/abs/2510.12077), Oct 2025) makes the description-length reading
of the free energy literal: the `λ log n` term *is* the singular refinement of the Minimum Description
Length model-cost (the two-part code's "bits to encode the model"), and empirically the **local
learning coefficient is closely — sometimes linearly — correlated with compressibility** (measured by
quantization/factorization on the Pythia suite). So:

> `λ` = effective description length = compressibility. The thing SLT computes is the thing a good
> compressor finds.

disp's `cheapness(e)` is *already* a description-length / cost term (`OPTIMIZER.typ` §2, §4) — but the
*naive* (raw-size / raw-interaction-count) version. SLT says the *right* one is degeneracy-aware
(§16).

## 12. The algorithmic-information ancestor — and why disp needs the *parametric* refinement

The discrete idea long predates SLT, in algorithmic information theory. **Kolmogorov complexity**
`K(x) = min{ |p| : U(p) = x }` is the shortest program — the discrete "regular" Occam term, the
analogue of `d/2`. **Algorithmic (Solomonoff–Levin) probability** sums over *all* programs:

$$m(x) \;=\; \sum_{p:\,U(p)=x} 2^{-|p|},$$

the analogue of the *whole* evidence integral. **Levin's Coding Theorem** ties them:
`−log₂ m(x) = K(x) + O(1)`. The gap between "shortest program" and "total mass" is exactly a
*degeneracy/multiplicity* term — `m(x) ≈ N · 2^{−K(x)}` where `N` counts near-minimal programs, so
`−log m(x) ≈ K(x) − log N`. **That `log N` is the discrete avatar of SLT's `λ`/`m` correction.**

The crucial subtlety, and *why disp specifically needs SLT and not just AIT*:

> In **idealized** AIT (a *universal*, unbounded-length UTM), the coding theorem forces `log N = O(1)`
> — degeneracy is a constant, Occam collapses to "shortest program, full stop." But disp is **not**
> idealized: it is a *bounded, cost-weighted* search over a *fixed* calculus (a finite cost budget,
> the tree-calculus substrate, a size/logit prior). In any **parametric / resource-bounded** setting
> the degeneracy `N` is *large and grows*, and that growth is precisely what SLT's `λ` measures
> (and `Programs as Singularities` / `MDL = SLT` formalize for neural nets). **disp lives in the
> parametric regime where the multiplicity is the whole story** — so the SLT refinement, not the
> O(1)-degenerate universal limit, is the relevant tool.

---

# Part III — SLT in disp

## 13. The dictionary

| SLT (continuous, analytic) | disp (discrete, reflective) |
|---|---|
| parameter `w ∈ ℝ^d` | candidate tree `e` |
| prior `φ(w)` | `prior e` — tree size / library frequency / an LLM's logits (`OPTIMIZER.typ` §8) |
| model / truth `q` | the specification `T` (a predicate / dependent type) |
| KL loss `K(w) ≥ 0`, `= 0` on truth | hard gate + smooth cost: `Err ⇒ ∞`, `Ok ⇒ cost e` |
| optimal set `W_0 = {K=0}` (a *variety*) | the solution variety `{ e : param_apply T e == Ok TT }` |
| non-identifiability / degeneracy | the `~_T` fibre — many trees, one behaviour (§15) |
| Fisher information degenerate ⇒ singular | reflective/intensional substrate ⇒ **maximally** singular |
| free energy `F_n = nS_n + λ log n + …` | "free energy of a spec" `F(T) = −log Σ_e prior(e)·score(T,e)` |
| RLCT `λ` (≤ d/2), effective complexity | **effective** `cheapness` — degeneracy-aware, ≤ raw size (§16) |
| multiplicity `m` | # of "tightest" co-optimal fibres / components (cf. §5B) |
| singular fluctuation `ν` | (no studied analogue — open) |
| tempered posterior `p_β`, WBIC at `β*=1/log n` | `score = gate × exp(−β·cost)` Gibbs measure (§18) |
| resolution of singularities (Hironaka) | **— no analogue; where the mathematics stops (§V)** |
| phase transitions between singularities | superposition collapse across `~_T`-classes / staging (§19) |

## 14. disp's objective is already a free energy

Start from `OPTIMIZER.typ` §2 — the objective is a hard correctness gate times a smooth cost:

```disp
// the 0/1 correctness filter and the score, in disp.
gate  := {v}    -> match v { Ok _ => one ; Err _ => zero }
score := {T, e} -> mul (gate (param_apply T e)) (cheapness e)   // §2: [typechecks] × cheapness
```

SLT studies not `argmax score` (that is MAP — i.e. *synthesis*) but the whole **posterior over
candidates** and its free energy. Overlay a prior (§8 already calls for one) and an inverse
temperature on cost:

```disp
// SCHEMATIC (prior, the Σ over trees, β, infinity are not literal disp):
//   post(e)  ∝  prior e · score T e                        -- the synthesis posterior
//   F(T)      =  - log  Σ_e  prior e · score T e            -- the free energy of a spec
//   SLT:  F  ≈  n·(min loss)  +  λ·log n  + …   with  λ = RLCT  (NOT d/2, NOT raw |e|)
```

The optimal set and its degeneracy are first-class disp objects (`Refinement` and `param_apply` are
real; `equiv_T` is the walker's `~_T`, schematic here):

```disp
// SLT's W_0 = {K=0}  is disp's solution VARIETY — huge and structured:
Solutions := {T}    -> Refinement Tree ({e} -> param_apply T e == Ok TT)

// non-identifiable: quotient by the walker-defined licensing relation ~_T (OPTIMIZER.typ §6).
// the FIBRE over one behaviour b — the "~_T-class", the locus of degeneracy SLT scores:
class_T   := {T, b} -> Refinement (Solutions T) ({e} -> equiv_T b e)
```

## 15. The central insight: three nested equivalences, and SLT names the middle one

disp currently carries **two** equivalences on trees:

1. `tree_eq` — the **finest**, syntactic/intensional; the substrate's O(1) conversion (`KERNEL_DESIGN.md`).
2. `~_T` — the **coarsest**, extensional/functional; the walker-defined licensing relation that makes
   rewrites sound (`OPTIMIZER.typ` §6; `FOUNDATIONS.md` §7).

`Programs as Singularities` (§10) exhibits a **third, intermediate** equivalence: the singularity
geometry — captured by `λ` — **distinguishes algorithmically-distinct-but-functionally-equal
programs.** It is strictly coarser than `tree_eq` (it does not see every syntactic wiggle) and
strictly finer than `~_T` (it *does* see the difference between two implementations of the same
function). It is a *graded, real-valued* refinement where disp has only a binary one.

This is not an analogy bolted on — it is the **measure-theoretic justification for disp's whole
no-memo / provenance argument** (`OPTIMIZER.typ` §3). disp insists, *operationally*, that
functionally-equal-but-distinct candidates be kept distinct (drop hash-consing) so cost is
attributable per candidate. SLT supplies the *invariant* (`λ`, a birational quantity) that says those
candidates are *genuinely geometrically different objects*, and a Bayesian observer can tell them
apart. Two crossing components of Example 5B = two program families realizing one behaviour, meeting
at a degenerate program; `λ` and `m` quantify the meeting. **disp's intensional substrate is, in SLT
terms, the correct choice: the posterior is not a function of the function.**

---

# Part IV — Potential applications to disp

Each tagged by how much weight it can bear, `FOUNDATIONS.md`-style.

## 16. A degeneracy-aware `cheapness` — the *right* Occam term **[load-bearing if pursued]**

disp ranks surviving candidates by raw cost/size (`OPTIMIZER.typ` §2, §4). §11 says the quantity that
actually governs generalization and posterior mass is `λ` — the effective description length, ≤ raw
size, *smaller* for degenerate (heavily-shared, symmetric, reused) programs. A raw-size prior
therefore **systematically mis-prices exactly the programs disp most wants** (compressible, reuse-heavy
ones). The principled `cheapness` weights a candidate by the *degeneracy of its `~_T`-fibre*, and its
natural home is the **§4 cost coeffect**: the free-energy split `F = (fit) + λ log n` is *literally*
§4's guidance(`fit`) / coeffect(`complexity`) duality, with `λ` as the correct complexity grade. SLT
even agrees with §3's instinct that cost should be an *intrinsic, coordinate-free* invariant of the
program — `λ` is birational, a property of the singularity, not of the cache or the parameterization.

## 17. RLCT as a complexity measure for *specifications* **[speculative, high-value]**

Read `F(T)` (the free energy of a spec, §14) through SLT: a spec whose solution variety is "fat and
low-`λ`" (many near-solutions, lots of degeneracy) is *easy to satisfy but loosely determined*; a spec
with an "isolated, high-curvature" solution (high `λ`, small fibre) is *hard to find — a needle in a
haystack — but sharply determined.* So **the RLCT of a spec is a measure of its synthesis difficulty
that is not its answer's size** — it is the *geometry of the solution set*. This reframes "why is this
synthesis problem hard?" from "the answer is big" to "the solution variety is thin and curved," which
is both more accurate and directly actionable for search budgeting and for ordering a curriculum of
specs (easy/fat first). No prior art applies SLT to *specifications*; this is a genuinely new lens
disp is unusually well-placed to try, because it already has `Solutions T` and a measured cost.

## 18. WBIC-style estimation, and the concrete λ-proxy **[load-bearing — buildable now]**

You never need resolution of singularities (§7): WBIC estimates `λ` by a tempered-posterior
expectation. disp's `score = gate × exp(−β·cost)` *is* a tempered posterior; if disp ever does
Bayesian (not MAP) synthesis — and §8's "order candidates by a prior" is one step from it — WBIC's
`β* = 1/log n` trick imports directly. The discrete shadow of Watanabe's `vol{K < ε} ∼ ε^λ` is a
*counting* law, and it is measurable with the machinery disp already has — see the first experiment
below.

## 19. Phase transitions ↔ superposition collapse and the staging axis **[speculative]**

SLT's stagewise development (§8) — the posterior moving through singularities of *decreasing* `λ` as
evidence accumulates — resonates with two disp constructs: the **superposition search collapsing**
across `~_T`-classes (`OPTIMIZER.typ` §8), and the **§5 staging axis** (memo→PE→JIT→AOT as one rewrite
at increasing "input fixed"). A "developmental" view of synthesis would watch the search move through
plateaus (fat, low-`λ`, easy-to-reach regions) toward sharp solutions, and could *schedule* the
collapse / the staging by an estimated `λ̂` gradient. Evocative; unproven; would need §18 working first.

## 20. The shared differential structure, and self-improvement **[speculative, but structurally real]**

The Ehrhard–Regnier derivative is in *both* `OPTIMIZER.typ` §8 (`δ` = bang, backward = `∂`,
"search is differentiating the cost over the net") and `Programs as Singularities` (the bridge from
program structure to singularity geometry). This **elevates §11's open comonoid-laws question** from
"low-stakes metaphor" to "possibly the same derivative": if disp's `δ` satisfies the differential-
category laws, it is the very operator that, on the SLT side, reads out a program's singularity — i.e.
its effective complexity. Worth checking the laws with this stake in mind. And for the endgame
(`GOALS.md` self-improvement): a Bayesian self-improver with a prior over its *own* rewrites would, by
SLT, be biased toward *degenerate* (more-shared, more-reusable) fragments — which is exactly the
inductive bias you want for a system whose improvements must *compose*. SLT explains *why* a Bayesian
self-improver naturally finds compression.

---

# Part V — The honest ledger

**What transfers (concepts):** non-identifiability as the organizing principle; effective complexity
`λ` < raw size; degeneracy = compressibility = generalization (Occam from geometry); the
intermediate "algorithmic" equivalence between syntax and behaviour (§15); the free-energy framing of
the objective (§14); tempered-posterior *estimation* without resolution (§7, §18).

**What does not transfer (mathematics) [breaks]:**

- **Hard gate ≠ smooth likelihood.** SLT needs a real-analytic `K(w)`. disp's gate is `0/1`. The
  fix in `Programs as Singularities` is the *noisy-code* smooth embedding — disp has none, and
  building one (a smooth family over tree-space) may fight the intensional/reflective substrate
  rather than fall out of it. *This is the make-or-break obstruction*, the discrete analogue of
  `FOUNDATIONS.md`'s "the property that makes disp fast is the property that makes disp hard."
- **No resolution of singularities.** No disp analogue of Hironaka, and it is intractable in general.
  (Mitigated: §7/§18 estimation sidesteps it.)
- **Sample asymptotics in `n`.** SLT's entire content is the `n → ∞` expansion. disp's spec `T` is
  *fixed* — there is no obvious `n`. Candidate reinterpretations (each unworked): the number of
  behavioural `test` cases the spec carries; the bits of the behavioural specification; the
  proof-/search-depth. Picking and justifying disp's `n` is a prerequisite to any quantitative use.
- **MAP vs Bayesian.** disp *searches for the best* program; SLT is about the *posterior*. The
  connection only fully lights up under Bayesian synthesis (§8's prior is the on-ramp).

**Open questions, disp-specific:**

1. *What is disp's `n`?* (the asymptotic variable — blocks everything quantitative).
2. *Is there a smooth embedding of tree-space* compatible with the intensional substrate, or does the
   discrete counting law (§18) suffice without one?
3. *Does the §18 λ-proxy correlate with anything disp cares about* — generalization of a synthesized
   program to held-out tests? search difficulty? If yes, §16/§17 are real; if no, this whole note is a
   pretty analogy.
4. *Does disp's `δ` satisfy the differential-category comonoid laws* (§20, = `OPTIMIZER.typ` §11)? If
   so, the SLT differential structure and disp's search-differential are one object.

## A first experiment — the λ-proxy probe (buildable now, zero kernel change)

The discrete shadow of `vol{K < ε} ∼ ε^λ` is a *count* of near-optimal programs. Using only the
existing measured-cost metric (`ApplyStats.steps` / interaction count via the `Session` ABI) and the
differential harness:

1. Fix a behaviour `b` (or spec `T`) with a known cheap solution at cost `c_min`.
2. Estimate the **degeneracy count** `N(ε) = #{ e : equiv_T(b, e) ∧ cost(e) ≤ c_min + ε }`, enumerating
   `~_T`-equivalents within a cost band (the differential harness already decides `equiv_T` by
   cross-backend agreement; bound the enumeration and *log the cap*, per the no-silent-truncation rule).
3. Fit the growth `N(ε) ∼ ε^{λ̂}` (or the discrete-counting analogue); `λ̂` is the **local learning
   coefficient proxy**.
4. **The test:** take two `~_T`-equivalent programs of *equal raw cost* but different sharing/symmetry
   (explicitly-memoized vs naive `fib`; fused vs unfused `map`). Does the **more degenerate one get the
   lower `λ̂`** — i.e. does SLT's effective-complexity ordering disagree with the raw-cost ordering, in
   the direction SLT predicts? A yes is the first evidence that §16 (degeneracy-aware cheapness) buys
   disp anything real. A no bounds the connection to "conceptual only."

This is a one-afternoon probe and it is the smallest falsifiable version of the entire Part IV.

## Glossary

- **Singular model** — `w ↦ p(·|w)` not locally injective; optimal set is a variety / Fisher info
  degenerate. (Regular = single non-degenerate optimum.)
- **`K(w)`** — KL divergence from truth to model at `w`; the loss landscape; `≥ 0`, zero on `W_0`.
- **RLCT / learning coefficient `λ`** — `−(largest pole of ζ(z) = ∫ K^z φ)`; effective parameter
  count over two; `= min_i (h_i+1)/(2k_i)` after resolution; birational invariant; `≤ d/2`.
- **Multiplicity `m`** — order of that pole; the `log log n` coefficient; `# coords attaining the min`.
- **Free energy `F_n`** — `−log` evidence; stochastic complexity / description length;
  `= nS_n + λ log n − (m−1) log log n + O_p(1)`.
- **Singular fluctuation `ν`** — second birational invariant; governs variance / train-test gap.
- **WBIC / WAIC / LLC** — estimators (free energy / generalization / *local* `λ`) computable from a
  (tempered) posterior; the practical, resolution-free face of SLT.
- **Resolution of singularities** — Hironaka's blow-up to normal-crossing form; how `λ` is computed
  and why `ζ` is meromorphic.
- **Phase transition** — discontinuous shift of posterior mass between singularities of different `λ`
  as `n`/`β` varies; basis of developmental interpretability.

## Sources & further reading

**Start here (accessible explainers).**
- Liam Carroll, *Distilling Singular Learning Theory* (DSLT) — the canonical intro sequence, no
  algebraic geometry assumed: [LessWrong](https://www.lesswrong.com/posts/xRWsfGfvDAjRWXcnG/dslt-0-distilling-singular-learning-theory)
  · [Timaeus mirror](https://devinterp.com/blog/dslt/2023-06-16-dslt-0/) · [author's site](https://www.liamcarroll.au/mathematics/SLT/).
- Jesse Hoogland, high-level intro + interview: [The Inside View](https://theinsideview.ai/jesse).
- The [AI Alignment Forum SLT sequence](https://www.alignmentforum.org/s/mqwA5FcL6SrHEQzox) and
  [Developmental Interpretability hub (devinterp.com)](https://devinterp.com/) /
  [resources](https://singularlearningtheory.com/resources).

**Foundational (Watanabe).**
- S. Watanabe, *Algebraic Geometry and Statistical Learning Theory* (Cambridge, 2009) — the book.
- S. Watanabe, *Mathematical Theory of Bayesian Statistics* (CRC, 2018) — WAIC/WBIC-centric.
- S. Watanabe, *A Widely Applicable Bayesian Information Criterion*,
  [JMLR 14 (2013)](https://jmlr.csail.mit.edu/papers/v14/watanabe13a.html).
- [Watanabe's SLT homepage](https://sites.google.com/view/sumiowatanabe/home/singular-learning-theory).

**The disp-relevant bridges (read these for Parts II–IV).**
- Murfet & Troiani, *Programs as Singularities* — [arXiv 2504.08075](https://arxiv.org/abs/2504.08075)
  · [Timaeus](https://timaeus.co/research/2025-04-10-programs-as-singularities). **The central bridge:
  programs as singularities, Ehrhard–Regnier derivative, posterior-distinguishes-algorithms.**
- *Compressibility Measures Complexity: MDL Meets SLT* — [arXiv 2510.12077](https://arxiv.org/abs/2510.12077).
  `λ` ≈ compressibility.
- *Bridging Kolmogorov Complexity and Deep Learning* — [arXiv 2509.22445](https://arxiv.org/abs/2509.22445).
- Carroll, Hoogland, Murfet et al., *Loss Landscape Degeneracy and Stagewise Development in
  Transformers* — [arXiv 2402.02364](https://arxiv.org/pdf/2402.02364).
- *Using Degeneracy in the Loss Landscape for Mechanistic Interpretability* —
  [arXiv 2405.10927](https://arxiv.org/html/2405.10927v2).

**Algorithmic information ancestor (§12).**
- Li & Vitányi, *An Introduction to Kolmogorov Complexity and Its Applications* (Springer) — algorithmic
  probability, Levin's Coding Theorem `−log m(x) = K(x) + O(1)`.

**disp-internal cross-references.**
- `OPTIMIZER.typ` — §2 (objective), §3 (no-memo / provenance), §4 (cost as graded coeffect), §6
  (equality / `~_T`), §8 (search as differentiation / `δ` = bang), §11 (comonoid-laws open question).
- `FOUNDATIONS.md` — §7 (the equality crux), the load-bearing/decorative discipline this note mirrors.
- `research/effects-and-coeffects.typ` — the graded-ledger frame §16 plugs into.
