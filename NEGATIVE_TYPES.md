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
**(c) architectural move** — collapsing recognition and respond into a single
mode-polymorphic walk (the NbE move) — has now **landed** (one walker `tele_walk`, wait-form
cells; §4c), so the remaining distance is just **(a) cosmetic** encoding and **(b)
completeness** (folding the rest of the negative lattice in). The `qid`/Intersection
piece **landed (2026-06-29)**: Intersection/Refinement/Eq are now telescope hybrids
(`qid`/`mint` + `refine`/`imeet`/`eqends` cells) and `GoodRespond` a telescope of `probe`
cells — all pluggable as new wait-form ops. Frame-tagging for mixed/callable records
remains. A
*genuinely different* optimum requires breaking the chain — intrinsic typing — which
means a different substrate, i.e. a different project (§6).

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
x:A ; apply out:(B x)]` and `Sigma` are thin instances; `Record` shares the engine
but keeps its own wait-form (lazy `fields_to_tele` lift, for self-verification). The
recognition/respond split collapsed into ONE walker `tele_walk` (the §4c move, landed), and
a cell is now a **wait-form** `wait op meta` — inspectable (signature = which
observation, meta = its data) AND runnable. The op is the observation
(`mint`/`proj name`/`apply`/`deriv name recipe`) and returns a *Step* (`SMint`,
`SThread`, `SReject`, `SDone`) that `tele_walk` interprets, recursion + `bind_hyp` inline.
New observation modes plug in as new ops with no walker edit. `{} = Telescope t = ⊤ =
Tree`. See `KERNEL_DESIGN.md` § Telescopes and `TYPE_THEORY.typ` §12.7.

## 4. The distance from local to global

### 4a. Cosmetic — the cell encoding (mostly settled by §4c)

The §4c landing chose **wait-form cells** `wait op meta`: the op *is* the
defunctionalized observation (signature = kind, meta = data), so the old `src`/`def`
axes collapsed into the op identity, and `build`/`constructor_type` inspect cells by
op-signature (`is_derive`) + `type_meta`. This is the inspectable-AND-runnable point
the encoding debate (tag vs stored-query vs coproduct-of-kinds vs closures) converged
on — a closure would be a black box to `build`/`constructor_type`, but a wait-form is
not. The op carries the genuine duality (intro `mint` vs the observations
`proj`/`apply`/`deriv`); openness (new observation = new op) is what §4b builds on.
Residual tightening (a query value folding `proj`'s name into a single cut-value) is
marginal and behavior-identical.

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
- **The unified single-frame respond for *mixed*/callable interfaces.** `tele_walk false`
  routes a neutral elimination by the lead cell (mint-lead ⟹ Pi-application; else ⟹
  projection) — sound for *pure* formers. A value that is both projected *and*
  applied (a callable record = `Neg` over a sum index `Q = Tags ⊕ A`) needs the
  elimination *frame* to carry an accessor-vs-argument tag so one walk dispatches it.
  Validated in scratch with **tagged queries** (`inj "apply" arg`); blocked on `acc`
  being load-bearing for the §2.6 value cut (`cut` indexes records by the
  accessor's tag), so it cannot be naively retagged. This is now the live frontier
  (§7).
- **Metacircular self-typing of the telescope machinery.** Cells are now wait-forms
  `wait op meta` whose op signatures already make them partly self-describing; a
  telescope-*type* can recognize them, and the kernel already self-types
  `MetaShape`/`NeutralMeta`. **The recognize-side metacircle LANDED (2026-06):**
  a type is a wait-form observed two ways — applied (`T v` → `CheckerResult`) and
  projected (`type_meta T` → `MetaShape`) — so `StrictType` is a plain `Telescope`
  of two `qid` cells (the recognize-face `Tree -> CheckerResult` and the meta-face
  `MetaShape`), walked by the same `tele_walk`, and it **inhabits itself**
  (`StrictType : StrictType`) with no privileged kinding rule
  (`lib/tests/strict_type.test.disp`). What remains is the *respond* side: a strict
  `respond : RespondShape` needs a **dependent `MetaShape`** (respond typed
  `params_ty -> Self -> frame_ty -> Action` against the former's structured frame).
  That makes *projecting* responds (inductive/eq) checkable, but the negative-former
  respond (`tele_walk false`, which raw-walks the cell spine) needs an `Entry` type for cells
  + a shape-type for the spine — fully typing it needs dependent/higher-order
  machinery, since the spine is *dependent* (the λ-tails). Self-*description*, not
  simplification.

### 4c. The architectural move — unify recognition and respond (one walk) — **LANDED (2026-06)**

This is the one move that actually shifted the design, and it is done. Formerly
`neg_check` (concrete, fold-all-observations) and `neg_respond`/`tele_field_at`
(neutral, one-observation) were **two walks that shared** the spine-walk, the
dependency-threading, and the observe-dispatch, differing only at the **leaf**
(check-and-continue vs extend-stuck) and the **prior-feed** (concrete value vs
projection-neutral). They are now one walker:

```
at(mode, tele, source, frame, prior)               // mode = true (check / concrete) | false (stuck / neutral)
recognize v       = at(true, tele, v, t, t)          // ∀ frame, folding; mint a hyp at mint cells
respond   v frame = at(false, tele, v, frame, t)       // answer one frame
```

How the prediction played out (it was right): the merge **relocated** the duality
into the `mode`, it did not eliminate it — the leaf (fold-all vs answer-one) and the
prior-feed (observe vs reflect) genuinely differ by mode, as forced. But two real
wins landed anyway: (1) the spine traversal, dependency threading, and observe-
dispatch are written ONCE; (2) because each cell's check-and-stuck behavior is
co-located in one op, the recognizer and respond **cannot disagree** about a cell.

The realization went *further* than the §4c sketch by also taking the wait-form-cell
encoding (§4b's "cells carry their semantics"): the per-cell logic moved OUT of the
walker into a wait-form **op** that returns a `Step` (`SMint | SThread | SReject |
SDone`), and `tele_walk` is the interpreter. This is the recursion-schemes split (op =
algebra, `tele_walk` = harness), and it is what makes new observation modes (§4b) pluggable
without a walker edit. One non-obvious constraint forced this shape rather than the
naive "op calls `bind_hyp kont`": a `bind_hyp` continuation passed *through* a
function miscompiles under nested binders (the hyp leaks, trips the occurs-check), so
the recursion/`bind_hyp` had to stay inline in `tele_walk` and the op had to return data
(see CLAUDE.md § Compiler workarounds). See `KERNEL_DESIGN.md` § Telescopes.

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

0. **§4c — DONE.** The recognition/respond unification + wait-form-cell encoding
   landed (one walker `tele_walk`, ops returning `Step`s). This reshaped everything below: a
   new observation mode is now a new wait-form op, no walker edit.
1. **§4b `qid`** — Intersection/Refinement as a `qid_cell` op (observe `v` itself).
   Recognition is validated; design the subsumption respond, shared with the
   callable-record frame-router.
2. **frame-tagging / mixed-callable** — the live frontier (§4b): a value both
   projected *and* applied needs the elimination frame to carry an accessor-vs-
   argument tag. Blocked on `acc` being load-bearing for the §2.6 value cut, so it
   can't be naively retagged; needs its own design.
3. **§4b metacircular self-typing** — the self-hosting `GOALS.md` win (an `Entry`
   type for cells; the wait-form op signatures make cells partly self-describing
   already).
4. **§4a encoding tightening** — only when the cell encoding is already being touched
   (the wait-form op already absorbs the old `src`/`def` axes into the op identity).
