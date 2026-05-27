# Modal Types Investigation

Research notes from a design discussion about how to type-distinguish "wait-forms" from ordinary partial application in disp, the type-theoretic background around modal and cubical type theories, and a sketch of how Multimodal Type Theory (MTT) could be implemented in a disp-like substrate.

## 1. The motivating question

In disp, two kinds of values both behave like "functions waiting for an argument," but they differ structurally and semantically:

1. **Normal partial application.** A closure produced by bracket abstraction — e.g. `f a : B → C` where `f : A → B → C`. Opaque; the underlying tree is whatever the S/K combinators of bracket abstraction left behind. The only useful operation is to apply it to another argument.

2. **Wait-form / reified operation.** A tree of shape `fork(signature, meta)` where `signature` is a fixed kernel-handler signature (e.g. `kernel.predicate_frame`, `kernel.bind_hyp`) and `meta` is structured payload data. Applying it triggers the dispatcher to route to the corresponding kernel handler. Critically, its structure (`signature`, `meta`) is **observable** via `pair_fst` / `pair_snd` *without* applying it — that's what makes `Bool`'s recognizer readable as data, what makes type-form recognition O(1), and what makes the kernel's signature-based dispatch work at all.

Both currently inhabit `Pi A B`. The type system can't distinguish them. This forces the structural distinction to live entirely at the runtime layer (signature checks, recognizer dispatch), which works but loses an important property: forgery of wait-forms via raw `wait kernel.op garbage_meta` is structurally indistinguishable, only failing later when garbage propagates.

The investigation: what would a type that captures this distinction look like, and what's the right type-theoretic foundation for it?

## 2. Three design sketches

Three encodings of the distinction were considered:

### Sketch A: Refinement subtype

```
WaitForm σ μ A := { t : Pi MetaArg A | pair_fst t ≡ sig_tree σ ∧ pair_snd t ≡ μ }
```

A subtype of the function type characterized by an equational refinement on the underlying tree shape. Coerces freely upward to `Pi MetaArg A`; downward elimination is just structural signature inspection. In cubical type theory this desugars to `Σ t : (Pi MetaArg A). Path Tree (pair_fst t) (sig_tree σ)`.

**Pros:** Minimal — reuses existing function types plus a refinement. Integrates with §13's already-planned cubical extensions.
**Cons:** The refinement is structural, not behavioral. A function that *happens* to satisfy the refinement is treated identically (including dispatch). Doesn't strictly enforce that the value was *built* as a wait-form.

### Sketch B: Distinct type former (algebraic-effects style)

```
Op : (σ : Signature) → MetaShapeOf σ → Type → Type
```

`Op σ μ A` is the type of wait-forms for operation `σ` with meta `μ` that produce an `A` when applied. Defined as a TypeFormer (per §11) with:
- classifier: `t ∈ Op σ μ A` iff `pair_fst t = sig_tree σ ∧ pair_snd t = μ`
- applicable: yes (the eval morphism is the dispatcher)
- functor: transport along type-paths

Library types become typed instances. For example, `Bool : Op kernel.predicate_frame { recognizer = bool_rec, ... } (Tree_p → CheckerResult Bool)`.

**Pros:** The operational distinction is type-encoded. Aligns directly with §5's "closed indexed algebraic effect" framing. Forgery becomes a type error rather than runtime garbage-in-garbage-out.
**Cons:** One more bootstrap layer (meta-shape types need to exist before `Op` can be defined). More verbose for type-former authors.

### Sketch C: Modal qualifier

```
Pi : (A : Type) → (A → Type) → Type
Reified : Type → Type
```

`Reified A` is a modality saying "this value is reified data with observable structure." `Pi A B` is opaque; `Reified (Pi A B)` is a function known to be a wait-form. The dispatcher operates inside `Reified`; the walker operates outside.

In modal-logic terms, `Reified` is a comonadic modality (a reflective subuniverse, in HoTT vocabulary): there's a counit `ε : Reified A → A` (forget structure) but lifting requires the value to actually *be* a wait-form, not just any A.

**Pros:** Captures the construction-history distinction directly. Composable with other modalities (erasure, staging) if disp grows in that direction. Type-erasure friendly.
**Cons:** Modal machinery is heavier than refinement; requires modal type theory as foundation.

## 3. The `Reified` modality, expanded

Under Sketch C, `Reified` is operationally:

- A type former: `Reified : Type → Type`.
- Modal intro: `mod_reified M : Reified A` for `M : A` (requires `M` to actually be a wait-form structurally — checked at intro).
- Modal elim: `let mod_reified x ← N in body` exposes `x` with access to `pair_fst`/`pair_snd` introspection.
- Counit: `forget : Reified A → A` always available; just treat as opaque.

Concretely in disp:
- A wait-form `wait kernel.predicate_frame bool_meta` would be typed `Reified (Pi MetaArg (Tree_p → CheckerResult Bool))`.
- The dispatcher operates inside the `Reified` modality (it can introspect).
- The walker operates on `Pi` directly (it just applies, no introspection).
- User code at the outer `Pi` level can't introspect; library/dispatcher code under `Reified` can.

The soundness boundary becomes a *modal boundary*, type-enforced. Forgery (`wait kernel.op garbage_meta`) doesn't type-check if `garbage_meta` doesn't fit the operation's meta-shape type.

## 4. Background: intensional vs extensional type theory

The investigation initially used "intensional/extensional" loosely. The technical clarification:

The distinction is about **equality**, specifically whether the type theory has the **equality reflection** rule:

```
p : Id_A(x, y)
──────────────
    x ≡ y
```

- **Extensional Type Theory (ETT):** has equality reflection. Propositional equality collapses into judgmental equality. Type-checking is undecidable because deciding `m ≡ n` requires proof search.
- **Intensional Type Theory (ITT):** rejects equality reflection. Identity types stay propositional; judgmental equality is decidable βιη-convertibility.

**Hofmann's 1995 thesis** (Springer 1997) is the canonical reference. He proves that ETT can be translated into ITT + Funext + UIP, formalizing the slogan "ETT = ITT + Funext + UIP."

The intensional/extensional axis is **orthogonal** to the choice between refinement types and modal types. Both refinement and modal constructions live happily inside either ITT or ETT.

What the original framing was actually gesturing at:
- **Proof-relevant explicit-witness (refinement / Σ-with-prop):** carry the proof as a value. Constructing requires `(t, proof)`; using requires projection.
- **Proof-irrelevant implicit-coercion (modality):** type-judgmentally tracked; no witness materialized at runtime.

That's a real distinction but it's about *what gets carried* and *what discipline you're under*, not about extensional vs intensional equality.

## 5. Cubical type theory — what it actually does

**Cubical type theory is intensional** (Cohen-Coquand-Huber-Mörtberg [arXiv:1611.02108](https://arxiv.org/abs/1611.02108)). It preserves decidable conversion. It does *not* add equality reflection. What it adds is **path types** built from an interval `I`:

```
Γ, i : I ⊢ t : A
─────────────────────────────────
Γ ⊢ ⟨i⟩ t : Path A t(i0) t(i1)
```

The interval's algebra of de Morgan formulas has decidable equality, so path computation is symbolic. Conversion stays decidable. Cubical's contribution is **computational content for funext and univalence** — they're inhabitants of `Path` built from `comp` and `Glue`, not extra rules.

### The algorithm for checking a refinement

Cubical has no primitive refinement-type former. `{ t : A | P t }` desugars to `Σ t : A. P t`. To check a candidate `(a, p)` against `Σ t : Tree. Path Tree (pair_fst t) σ`:

1. Check `a : Tree`.
2. Compute expected type for `p`: substitute `a` for `t`, getting `Path Tree (pair_fst a) σ`.
3. Check `p` against that path type.
4. **For `p = refl`:** reduces to checking the path's two boundaries, both judgmental equalities `pair_fst a ≡ σ`. In disp's tree-calculus setup, that's `tree_eq` — one hash-cons comparison, O(1).
5. **For non-trivial `p`:** type-check `p` as a path; cubical's reduction system normalizes.

**For the wait-form case where the witness is always `refl`, cubical's machinery is dormant.** You pay one judgmental-equality check. The path/transport/Glue apparatus only fires if you transport across non-trivial equalities.

## 6. Multimodal Type Theory (MTT)

Gratzer-Kavvos-Nuyts-Birkedal ([arXiv:2011.15021](https://arxiv.org/abs/2011.15021), LMCS 17:3) parameterize the theory by a **2-category of modes**:

- objects = modes (kinds of contexts)
- 1-cells `µ : m → n` = modalities
- 2-cells = natural transformations

For each modality there is a type former `⟨µ | A⟩` and a **Fitch-style context lock** `Γ.{µ}`. The lock restricts which earlier variables are visible inside a modal subterm.

```
µ : m → n     Γ, 𝟁µ ⊢ M : A @ n
─────────────────────────────────
   Γ ⊢ mod_µ(M) : ⟨µ | A⟩ @ m
```

Elimination is `let mod_µ x ← N in body`.

### The algorithm for checking `t : ⟨R_σ | A⟩`

The check is **entirely syntactic and type-directed**. No runtime tag, no witness, no proof term:

1. If `t = mod_R(M)`: re-check `M : A` under the locked context. Locks restrict what variables can be used in `M`.
2. If `t = let mod x ← N in body`: walk inward.
3. If `t = x` for a variable: look up `x`'s type in the context.

Two terms that are extensionally identical can live in different modes — the distinction exists only at type-checking time. MTT is type-erasure friendly.

### Equivalence to Σ-with-prop for reflective subuniverses

Rijke-Shulman-Spitters ([arXiv:1706.07526](https://arxiv.org/abs/1706.07526)) prove that a reflective subuniverse modality is exactly a Σ-type of `Type` with a propositional predicate. nLab on reflective subuniverses: "If closed under dependent sum, this is the universe of modal types for a modal operator."

So `⟨R_σ | A⟩` and `Σ t : A. is_R_σ t` are **equivalent in inhabitation**. They differ in syntactic discipline (modal locks vs explicit pairs/projections), not in expressive power.

## 7. Comparing the algorithms for the wait-form case

| | Cubical refinement | MTT reflective modality |
|---|---|---|
| Type form | `Σ t : Pi MetaArg A. Path Tree (pair_fst t) σ` | `⟨R_σ | Pi MetaArg A⟩` |
| Introduction | `(wait σ μ, refl)` | `mod_R(wait σ μ)` (under lock) |
| Inhabitation check | Pair check + `refl` against path type + boundaries via `≡`; reduces to `tree_eq (pair_fst t) σ` | Walk `mod_R(...)` syntax; re-check under lock; no equality on trees |
| Runtime data | Pair `(t, refl)`; η-collapses to just `t` | Bare `t`; lock is type-checker bookkeeping only |
| Cost at type-check | One `tree_eq` per inhabitation | Lock-tracking in elaborator |

**For this single use case, they are operationally equivalent.** Both compile away to "the runtime value is the tree." Both type-check decidably.

## 8. Where they actually diverge

Three categories, all flagged in the MTT paper §1:

1. **Multi-mode.** MTT has objects and morphisms; refinements live in one context. If disp ever wants erasure × wait-form × guarded-recursion as orthogonal modalities, MTT scales — refinements proliferate as ad-hoc Σ-encodings.

2. **Non-reflective modalities.** Guarded recursion's later-modality `▶` has *no* canonical map `A → ▶A`. A reflective-subuniverse refinement always has the unit `A → R A`; non-reflective modalities can't be encoded as Σ-with-prop. Same for parametricity quantifiers, internal sheaf modalities.

3. **Substructural discipline.** Quantitative Type Theory's `0`-modality enforces erasure: a `0`-marked variable *must not* appear at runtime. Locks can enforce this; refinements live in cartesian contexts and can't.

Pfenning-Davies S4 (*MSCS* 11(4), 2001) is the prototype. Their introduction rule for `□` uses an **empty truth context**: `∆; · ⊢ A true → ∆; Γ ⊢ □A true`. The asymmetry between the valid-zone `∆` and truth-zone `Γ` is the whole point — Σ-types are unrestricted products and can't enforce "you may use `∆` but not `Γ`."

## 9. Other type theories investigated

### Higher Observational Type Theory (HOTT)

Altenkirch-Kaposi-Shulman. Defines equality observationally (recursing on type structure) rather than via cubical's interval. Recovers funext and a form of univalence without cubical machinery. Recent (2022 TYPES; Shulman's blog "Towards a Third-Generation HOTT," n-Café 2022). The full HOTT paper has been forthcoming for several years. Sometimes informally called the "categorical" approach to HoTT.

Does **not** currently incorporate MTT-style modalities; modal extensions speculative.

### Synthetic Tait Computability (STC)

Sterling ([arXiv:2210.00827](https://arxiv.org/abs/2210.00827), PhD CMU 2021), used in Sterling-Angiuli "Normalization for Cubical Type Theory" ([arXiv:2101.11479](https://arxiv.org/abs/2101.11479)). A **metatheoretic** framework using topos-theoretic modalities to prove properties about cubical. Genuinely fuses cubical and modal — at the meta level. The object theory it analyzes is plain cubical.

### Cohesive HoTT / Spatial Type Theory

Shulman ([arXiv:1509.07584](https://arxiv.org/abs/1509.07584)) adds cohesive modalities ʃ ⊣ ♭ ⊣ ♯ to HoTT. Spatial Type Theory (Licata-Shulman-Riley, LICS 2017) introduced the crisp-variable judgment that became the template for Fitch-style locks. Specific modal-cubical instance, parameterized for cohesion. Computational variants: Mitchell Riley's PhD thesis "A Bunched Homotopy Type Theory for Synthetic Stable Homotopy Theory" (Wesleyan 2022).

### Guarded cubical type theory

Birkedal et al. ([arXiv:1611.09263](https://arxiv.org/abs/1611.09263), CSL 2017 / JAR 2019). Adds Nakano's later-modality `▷` to cubical for productive recursion. Specific point-instance, not parameterized.

### Synthetic ∞-category theory

Riehl-Shulman ([arXiv:1705.07442](https://arxiv.org/abs/1705.07442)), Buchholtz-Weinberger ([arXiv:2105.01724](https://arxiv.org/abs/2105.01724)). Built on cubical, internalizes ∞-categorical structure via extension types. The extension types behave modally in spirit.

### Two-Level Type Theory (2LTT)

Annenkov-Capriotti-Kraus-Sattler ([arXiv:1705.03307](https://arxiv.org/abs/1705.03307), MSCS 2023). Layers a strict (set-truncated) theory under a fibrant (HoTT) one. The interaction between layers is modal in flavor; the modality is fixed (the fibrant inclusion), not parameterized.

### Internal parametricity in cubical

Cavallo-Harper ([arXiv:2005.11290](https://arxiv.org/abs/2005.11290)). Extends cubical with a "bridge" interval orthogonal to the path interval. Bridge variables behave modally — readable as instances of a parametricity modality. Mature; Cubical Agda implements `--bridges`. One fixed modality, not MTT-parameterized.

## 10. Can cubical and MTT be unified?

**Not in published literature, not in full generality.** Specific cubical + modal hybrids exist as point instances (cohesive, guarded, parametricity); a generic Cubical-MTT parameterized over a 2-category of modes is open work.

Gratzer's 2023 PhD thesis "Syntax and Semantics of Modal Type Theory" (Aarhus) explicitly names this as future work. Named obstacles:

1. **Lex modalities + univalence.** Rijke-Shulman-Spitters show only *left-exact* modalities preserve identity-types-up-to-equivalence. MTT modalities are not required to be lex. Naive MTT + univalence is unsound; restrictions are needed.

2. **Interval variables crossing locks.** Cubical's `i : I` lives in the ordinary context; MTT's locks restrict variable visibility. Whether `i` may cross a lock is unresolved.

3. **Joint decidability / normalization.** Cubical normalization (Sterling-Angiuli 2021) and MTT normalization (Gratzer 2022) use different machinery. Combining them requires re-proving normalization for the joint system; not done in general.

4. **Higher mode theories.** MTT's mode theory is a strict 2-category. Paths between mode terms would require an (∞,2)-category at minimum. Semantics largely unwritten.

The "categorical type theory" name doesn't correspond to a specific recent framework that unifies cubical + modal. The two strongest candidates for what the term might refer to are **HOTT** (recent, "categorical" pitch, alternative to cubical) and **STC** (topos-theoretic, fuses cubical and modal at the metalevel). Neither is the unified object theory.

## 11. How MTT could be implemented in disp

Following the §13 cubical playbook (interval is a library predicate_frame type; paths are sugar over `Pi I`; transport dispatches on per-type `morphism_action`; no kernel extension), MTT pieces map onto disp:

### The mode universe

```disp
let tag_opaque    = t t              // O — closures
let tag_reified   = t (t t)          // R — wait-forms (transparent)
let tag_erased    = t (t (t t))      // ε — QTT 0-modality
let tag_static    = t (t (t (t t)))  // s — compile-time-only

Mode := make_type_former
  Unit
  mode_recognizer
  none
  trivial_functor        // Mode is discrete — no continuous structure
  refl_identity_law
  refl_composition_law
```

A library `Mode` type, parallel to `I` in the cubical proposal.

### Modality records

```disp
Modality := record {
  source : Mode,
  target : Mode,
  classifier : (A : Type) -> Tree_p -> CheckerResult Bool,
  intro      : (A : Type) -> A -> CheckerResult (ModalValue self A),
  elim       : (A : Type) -> ModalValue self A -> CheckerResult A,
  identity_law : ...,
  composition_law : ...
}
```

A sibling to `TypeFormer`. The `classifier`/`intro`/`elim` fields play the role `Functor.morphism_action` plays for cubical — per-modality dispatch.

### Modal type former

```disp
Modal := {µ, A} -> make_type_former
  (pair µ A)
  ({_, v} -> µ.classifier A v)
  none
  (modal_functor µ A)
  refl_identity_law
  refl_composition_law
```

`⟨µ | A⟩` becomes `Modal µ A`. Modal intro/elim are library functions delegating to the modality's own `intro`/`elim`.

### The locks problem

MTT's context locks have no direct analog because disp has no first-class contexts. Two paths:

**Option A: Walker mode-awareness (small kernel extension).** Generalize `bind_hyp` to `bind_modal_hyp` that mints a hypothesis tagged with mode. Extend the walker to track current evaluation mode; reject when mode tags mismatch. Adds one new `CheckerError` variant (`ModeMismatch`). The current parametricity rejection becomes a special case: "opaque mode can't introspect transparent values."

This **unifies** parametricity discipline with modal discipline — both become aspects of one mechanism.

**Option B: Runtime mode predicates (library-only).** Encode modes as values threaded through computation, capability-passing style. Closer to QTT erasure than MTT locks. Sufficient for `Reified` and `Erased`; insufficient for guarded recursion or substructural disciplines.

### Inspired by `transp`: modal coerce

Just as cubical's `transp` dispatches on the target type's `Functor.morphism_action`, modal coercions dispatch on each modality's coerce function:

```disp
mod_coerce := fix ({self, α, A, modal_value} -> {
  apply (transformation_action α A) modal_value
})
```

2-cells (transformations between modalities) are library values — functions plus coherence proofs.

### The 2-category as library data

Modes, modalities, 2-cells all become library records. Composition (vertical and horizontal) becomes library operations with associativity/identity laws as `Path`-typed proofs (using §13's cubical machinery). Different mode theories are different `ModeTheory` records. You can have multiple in scope.

This would make disp arguably the **first dependent type theory where the mode theory itself is library data** — most MTT presentations bake the mode theory into the syntax.

## 12. Cubical × modal interaction in disp

In the disp sketch:
- `Path (Modal µ A) x y` reduces to `Pi I ({_} → Modal µ A)`.
- For lex modalities: `Path (Modal µ A) x y` is equivalent to `Modal µ (Path A x y)`. The `Functor.morphism_action` field of `Modal µ A`'s TypeFormer computes the isomorphism.
- For non-lex modalities: paths and modal types interact in ways that need careful coherence work. The `identity_law` or `composition_law` field wouldn't admit a `refl` proof.

Disp's approach: encode lex-ness as a predicate on `Modality` records. The `Path` library checks before allowing transport across a modal type-path. Non-lex modalities exist (intro/elim work) but path-equality on them is opaque.

## 13. Roadmap

- **Phase 0 (current):** Wait-forms recognized structurally via signature dispatch. One implicit "transparent" mode. No explicit modal machinery.
- **Phase 1 (no kernel change):** Add `Modality` and `Modal µ A` as library types. Implement `Reified A` = the wait-form modality. Library-only enforcement. Delivers Sketch B's `Op σ μ A` essentially.
- **Phase 2 (small kernel change):** Add `bind_modal_hyp` and walker mode-awareness. Unifies parametricity with modal discipline. Add `Erased` modality and refactor strip to use it.
- **Phase 3 (expansive):** Build the full 2-category as a library record. Add 2-cells, compose modalities, prove coherence laws using cubical `Path`s. Add `Static`, `Effect σ`, etc.
- **Phase 4 (research):** Non-lex modalities — guarded recursion via `▶`, parametricity bridges as a modality, internal sheaf modalities. Requires real coherence work.

## 14. Open questions

1. **Universe polymorphism × modality.** If `Mode` is in the universe, modalities are between `Mode`-indexed types, and `⟨µ | Type⟩` is a thing — universe stratification needs to be specified.

2. **Effect modalities unifying with the closed-effect framing.** §5 already describes the kernel as a "closed indexed algebraic effect" over `CheckerResult`. Each kernel handler could be re-cast as a specific modality (`Op σ`). Doing this would unify §5's algebraic-effects framing with the modal framing — meaningful refactor.

3. **Interaction with the I-shortcut.** Currently the walker has one carve-out: `apply(I_canonical, x) = Ok x` even for hypotheses. Under mode-aware walker, this generalizes to "identity-on-mode-µ" carve-outs — one per modality. Each needs its own justification.

4. **Bootstrap layering.** Modal machinery depends on `Mode`, `Modality`, `Modal µ A`, which depend on TypeFormer (§11), which depend on the kernel. Bootstraps after TypeFormer, which is fine. But typed-kernel ascription becomes modal-typed-kernel ascription. Even more circular, consistent with metacircular discipline.

5. **2-cell coherence as Path proofs.** Real cubical × modal interaction territory. Gratzer flagged this as open. Disp would inherit the openness.

## 15. Net assessment

For just distinguishing wait-form trees from closures, a refinement-style approach (Sketch A or Sketch B) is fully sufficient, type-checks decidably under any cubical or intensional setup, and the check reduces to a single hash-cons equality. The reflective-subuniverse MTT modality (Sketch C / `Reified`) is equivalent in expressive power for this case but gives nicer syntactic ergonomics — no `(t, refl)` pair to thread around — at the cost of importing modal machinery.

The cubical path machinery, `J`/`transport`, and the MTT lock discipline are all overkill if disp only needs `pair_fst t ≡ σ` and never transports across that equality. They become load-bearing the moment disp wants to:
- reason about wait-forms whose signatures are provably equal under non-trivial paths,
- compose multiple modal disciplines (erasure × wait-form × guarded × static),
- extend wait-form-ness to non-reflective notions (linear wait forms, partial wait forms).

Cubical and MTT solve **different problems**. Cubical is about *equality* (paths, transport, univalence). MTT is about *type-former structure* (modes, locks, modalities). For wait-form vs closure alone, both technologies converge on the same trivial check; the choice is about which scaffold to grow into.

The recommendation: stay with current structural signature dispatch + TypeFormer recognizer machinery for now. If disp grows toward erasure modes, staging, or guarded recursion, adopt MTT incrementally — starting with `Reified` as a single library modality and extending only as more modalities prove necessary. Cubical extensions (§13) remain on the roadmap orthogonally. A full Cubical-MTT unification is not available in print and would be ahead of the published literature.

## 16. References

### Foundations
- Cohen, Coquand, Huber, Mörtberg. *Cubical Type Theory: A Constructive Interpretation of the Univalence Axiom.* [arXiv:1611.02108](https://arxiv.org/abs/1611.02108) (2016).
- Hofmann, M. *Extensional Constructs in Intensional Type Theory.* Springer, 1997 (PhD thesis 1995).
- Winterhalter et al. *Extensional concepts in intensional type theory, revisited.* [arXiv:2310.05706](https://arxiv.org/abs/2310.05706).

### Modal type theory
- Gratzer, Kavvos, Nuyts, Birkedal. *Multimodal Dependent Type Theory.* [arXiv:2011.15021](https://arxiv.org/abs/2011.15021), LMCS 17:3, 2021.
- Gratzer. *Normalization for Multimodal Type Theory.* [arXiv:2106.01414](https://arxiv.org/abs/2106.01414), LICS 2022.
- Gratzer. *Syntax and Semantics of Modal Type Theory.* PhD thesis, Aarhus, 2023.
- Gratzer, Cavallo, Kavvos, Guatto, Birkedal. *Modalities and Parametric Adjoints.* [arXiv:2107.05912](https://arxiv.org/abs/2107.05912), ACM TOCL 2022.
- Pfenning, Davies. *A Judgmental Reconstruction of Modal Logic.* MSCS 11:4, 2001.
- Rijke, Shulman, Spitters. *Modalities in Homotopy Type Theory.* [arXiv:1706.07526](https://arxiv.org/abs/1706.07526), LMCS 2020.

### Cubical + modal hybrids
- Shulman. *Brouwer's fixed-point theorem in real-cohesive homotopy type theory.* [arXiv:1509.07584](https://arxiv.org/abs/1509.07584), MSCS 2018.
- Birkedal et al. *Guarded Cubical Type Theory.* [arXiv:1611.09263](https://arxiv.org/abs/1611.09263), JAR 2019.
- Cavallo, Harper. *Internal Parametricity for Cubical Type Theory.* [arXiv:2005.11290](https://arxiv.org/abs/2005.11290), CSL 2020.
- Licata, Shulman, Riley. *A Fibrational Framework for Substructural and Modal Logics.* LICS 2017.
- Riley. *A Bunched Homotopy Type Theory for Synthetic Stable Homotopy Theory.* PhD thesis, Wesleyan, 2022.

### Alternative foundations
- Altenkirch, Kaposi, Shulman. *Higher Observational Type Theory.* TYPES 2022 abstract; Shulman, "Towards a Third-Generation HOTT," n-Category Café 2022.
- Sterling. *First Steps in Synthetic Tait Computability.* [arXiv:2210.00827](https://arxiv.org/abs/2210.00827), PhD CMU 2021.
- Sterling, Angiuli. *Normalization for Cubical Type Theory.* [arXiv:2101.11479](https://arxiv.org/abs/2101.11479), LICS 2021.
- Riehl, Shulman. *A Type Theory for Synthetic ∞-Categories.* [arXiv:1705.07442](https://arxiv.org/abs/1705.07442), Higher Structures 2017.
- Buchholtz, Weinberger. *Synthetic fibered (∞,1)-category theory.* [arXiv:2105.01724](https://arxiv.org/abs/2105.01724), Higher Structures 2023.
- Annenkov, Capriotti, Kraus, Sattler. *Two-Level Type Theory and Applications.* [arXiv:1705.03307](https://arxiv.org/abs/1705.03307), MSCS 2023.

### nLab pages
- [extensional type theory](https://ncatlab.org/nlab/show/extensional+type+theory)
- [intensional type theory](https://ncatlab.org/nlab/show/intensional+type+theory)
- [propositional equality](https://ncatlab.org/nlab/show/propositional+equality)
- [cubical type theory](https://ncatlab.org/nlab/show/cubical+type+theory)
- [multimodal type theory](https://ncatlab.org/nlab/show/multimodal+type+theory)
- [reflective subuniverse](https://ncatlab.org/nlab/show/reflective+subuniverse)
- [refinement type](https://ncatlab.org/nlab/show/refinement+type)
