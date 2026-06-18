# Negative Types — design rationale and the frontier from local to global

**Status: design note (2026-06).** Why the telescope (one negative former for
`Pi`/`Sigma`/`Record`/`⊤`) is shaped the way it is, the ideal it approximates, and
the concrete improvements that remain. Companion to `KERNEL_DESIGN.md` § Telescopes
(implementation idioms) and `TYPE_THEORY.typ` §12.7 (spec). Landed history:
`archive/UNIFIED_FORMER_PROPOSAL.md` (the partial-cut unification + dead-ends).

## TL;DR

The telescope is close to a *global* optimum, not merely a local one — because
disp's substrate and foundations nearly **determine** it via a chain of forced
choices (§1). The current design is a faithful realization of the ideal in §2. The
remaining distance is three kinds of gap (§4): **(a) cosmetic** encoding, **(b)
completeness** (folding the rest of the negative lattice in), and **(c) one genuine
architectural frontier** — collapsing recognition and respond into a single
mode-polymorphic walk (the NbE move), whose payoff is *unvalidated*. A *genuinely
different* optimum requires breaking the chain — intrinsic typing — which means a
different substrate, i.e. a different project (§6).

## 1. Why the design is nearly determined (the forced-choice chain)

Each link leaves essentially no alternative given the prior:

1. **Substrate = tree calculus.** Fixed by `GOALS.md` (neural-guided synthesis over
   a tiny rewrite system).
2. ⟹ **Types are extrinsic predicates over `Tree`.** Bracket abstraction erases
   binders and annotations; a value is just a tree, so you cannot do syntax-directed
   checking. The only operation on a tree is to *run a predicate on it*. "Types are
   tree programs `Tree -> CheckerResult`" is forced.
3. ⟹ **Soundness over open terms = the walker / parametricity.** A predicate is an
   arbitrary tree program; on a hypothesis it must not inspect structure. The walker
   ("no raw triage on neutrals") is the only way to keep arbitrary predicates sound
   under binders.
4. ⟹ **∀-intro = mint a fresh neutral (`bind_hyp`); elimination = neutral + `respond`
   (NbE).** Forced by *dependent* checking over an *untyped* substrate.
5. ⟹ **Negative types are observation interfaces; recognition (concrete, all
   observations) and respond (neutral, one observation) are their two faces.** A
   `Π`/`Σ`/record is defined by how you eliminate it.
6. ⟹ **The interface is a telescope** (de Bruijn's dependent context) with
   dependency carried by real λ-tails — the one thing bracket abstraction keeps
   (substitution itself).

So "the global optimum" is not a rival design; it is the **ideal this chain
converges to**, and every "more elegant" probe bottoms out at cosmetics or
completeness.

## 2. The ideal (observation-interface / NbE statement)

> A **negative type is an observation interface**: a domain of frames `Q` and a
> dependent result `Cod : Q -> Type`.
> - **Recognition** = the *concrete-mode* universal check: `v : T` iff for every
>   frame, observing `v` there lands in `Cod(frame)`. The `∀` ranges by
>   **coverage** — *mint one* (Q infinite/parametric) or *enumerate* (Q finite).
> - **Respond** = the *neutral-mode* single frame: eliminating a hypothesis at a
>   frame yields `Cod(frame)`, stuck.
> - The **cut `v q`** is the universal observation (project / apply / observe-self
>   are one operation); the **walker** policing it on neutrals *is* parametricity
>   *is* soundness.
> - **`⊤` = the empty interface** (no observations ⟹ empty meet ⟹ all of `Tree`).
> - The concrete/neutral split (recognize vs respond) is **NbE's reify/reflect —
>   forced and correct, not redundant.** The H-rule bridges them (a neutral is a
>   member by hypothesis).

## 3. What's landed (the current realization)

`Telescope` is THE negative n-ary former, **guard-free**; `Pi A B = Telescope [mint
x:A ; qapp out:(B x)]` and `Sigma` are thin instances; `Record` shares
`neg_check`/`neg_respond` but keeps its own wait-form (lazy `fields_to_tele` lift,
for self-verification). A cell is a §2.6 record `{ name; ty; def; [src] }`; coverage
is the `src` tag (`mint`/`qacc`/`qapp`, default `qacc`), transparency is `def`
(opaque / `t recipe`). `{} = Telescope t = ⊤ = Tree`. See `KERNEL_DESIGN.md` §
Telescopes and `TYPE_THEORY.typ` §12.7.

## 4. The distance from local to global

### 4a. Cosmetic — the cell encoding (marginal)

Tags vs a stored *query value* vs a coproduct of cell-kinds vs stored closures. All
behavior-identical; measured ~5% recognition cost difference; the **tag is the
correct *defunctionalized* form** for disp's situation (few cell kinds, many
operations over cells — recognize/respond/`mk`/`constructor_type`/future `strip`).
Storing the operation (tagless-final) is open-to-new-kinds but closed-to-new-ops,
the wrong dual here; it also fattens cell literals and forces a behavior+data hybrid
(`mk` needs `name`/`def` as data). The one genuine tightening: **query-collapse** —
fold `src`+`name` into a single `query` (qacc's query = `acc name`, a real cut-value)
so `observe = honest-cut v query` mirrors the value-level cut. Real but cosmetic;
worth doing only if the cell encoding is being touched anyway.

### 4b. Completeness — fold the rest of the negative lattice in

The telescope currently covers `⊤`/`Π`/`Σ`/records. The rest of the *negative*
(observation-defined) lattice is reachable by new observation modes:

- **`qid` (observe `v` itself) → Intersection / Refinement.** `[qid:A ; qid:B]` =
  `A ∩ B`; dependent `qid` = `{v:A | v : B v}`. Recognition validated in scratch.
  *Open work:* the respond is **subsumption** (use the neutral *as* an A or *as* a
  B), which the current `Action` (`Extend`/`Reduce`) doesn't express — it's the same
  frame-routing as callable records below.
- **positional / `Fin` observations → tuples** (the prelude fork-pair idiom as a
  telescope instance, distinct from named §2.6 records).
- **The unified single-frame respond for *mixed*/callable interfaces.** Today
  `neg_respond` is a *router* (mint-lead ⟹ Pi-application; else ⟹ `tele_field_at`
  projection) — sound for *pure* formers. A value that is both projected *and*
  applied (a callable record = `Neg` over a sum index `Q = Tags ⊕ A`) needs a single
  walk that dispatches a frame by tag. Validated in scratch with **tagged queries**
  (`inj "apply" arg`); blocked on pure bare-application being unsound to unwrap (a
  function argument can itself be `inj "apply" x`). Defer until a consumer
  (dataflow/objects, `GOALS.md`) exists.
- **Metacircular self-typing of the telescope machinery.** Cells are §2.6 record
  *values*; a telescope-*type* can recognize them (validated), and the kernel already
  self-types `MetaShape`/`NeutralMeta`. Closing the loop — an `Entry` type for cells,
  a shape-type for the spine — completes self-hosting. Non-trivial: the spine is
  *dependent* (the λ-tails), so fully typing it needs dependent/higher-order
  machinery. Self-*description*, not simplification.

### 4c. The architectural frontier — unify recognition and respond (one walk)

The only non-cosmetic, non-completeness move. Today `neg_check` (concrete,
fold-all-observations) and `neg_respond`/`tele_field_at` (neutral, one-observation)
are **two walks that share** the spine-walk, the dependency-threading, and the
observe-dispatch, differing only at the **leaf** (check-and-continue vs extend-stuck)
and the **prior-feed** (concrete value vs projection-neutral). The ideal writes one
per-frame kernel and derives both:

```
at(tele, v, frame, mode)        // observe v at frame; mode = check (concrete) | stuck (neutral)
recognize v = ∀ frame. at(tele, v, frame, check)   // mint (infinite Q) or enumerate (finite Q)
respond   v frame = at(tele, v, frame, stuck)       // single application
```

This is NbE's "single evaluator, reflect/reify as modes," one level down. It would
deduplicate the two ~15-line walks into one. **But the payoff is unvalidated:** the
split it merges is partly forced — concrete vs neutral (reify/reflect), fold-all vs
answer-one, and mint vs enumerate vs given-frame (coverage). The unified `at` must
branch on all three, so the merge may *relocate* the duality into a mode parameter
rather than eliminate it. **The experiment:** build `at`, define
`neg_check`/`neg_respond` as its two specializations, and measure whether one walk is
genuinely simpler than the two it replaces (and survives the dependent-threading and
coverage cases). This is the one move that could actually shift the design.

## 5. The boundary that is *not* a gap (the positive dual)

Coproducts and inductives (`Nat`, `List`, `Ord`) are the **positive** dual — sums /
colimits, defined by *construction*, recognized by *recursion* (`Coproduct` +
`make_rec_recognizer`). They are **not** telescopes, by polarity: a telescope is a
conjunction of observations (`∀`/product, eliminate by one projection); a coproduct
is a disjunction (`∃`/sum, eliminate by total case analysis). `Nat` fails twice —
it's a sum *and* value-recursive (telescopes are finite cell-chains). A recursive
*product* (codata: streams) could be a self-referential telescope; a recursive *sum*
cannot. The real boundary is **polarity, not recursion.** The telescope is the
complete negative side; nothing on the positive side belongs in it.

## 6. A genuinely different optimum requires breaking the chain

The only breakable link in §1 is #2 — extrinsic predicates. **Intrinsic typing**
(terms carry their types, well-typed by construction) needs no recognizer at all —
but it requires a *typed* substrate, not erased tree calculus. That contradicts
`GOALS.md`'s tree-calculus commitment, so it is a different project, not an
improvement to this one. Given erased trees, everything from §1.2 onward follows.

## 7. Recommended order (if pursued)

1. **§4c** — prototype the NbE recognition/respond unification and *measure*. The
   only architectural move; do it first because a positive result reshapes everything
   below it.
2. **§4b `qid`** — Intersection/Refinement (recognition is validated; design the
   subsumption respond, shared with the callable-record frame-router).
3. **§4b metacircular self-typing** — the self-hosting `GOALS.md` win.
4. **§4a query-collapse** — only when the cell encoding is already being touched.
5. **Defer** callable/mixed respond until a dataflow/objects consumer exists.
