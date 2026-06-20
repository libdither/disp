# Canonical telescopes — strongly-normalizing types via elaboration-time normalization

**Status: design note (2026-06).** Why telescope *types* aren't yet unique-up-to-conversion,
what a canonical normal form looks like, where normalization must happen (the *elaborator*,
not a runtime combinator), exactly which laws to normalize (the decidable structural fragment)
and which to leave to transport, and the concrete work to get there. Companion to
[`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) (the telescope's shape) and
[`STRICTTYPE.md`](STRICTTYPE.md) (the metacircle); the `&` / Intersection-as-telescope thread
(`NEGATIVE_TYPES.md` §4b) lands here.

## TL;DR

Conversion is `tree_eq` (hash-cons identity), so types unify **iff they are the same tree**.
Deterministic elaboration gives "same *spelling* → same tree" but not "same *semantics* → same
tree": `And A B`, `And B A`, and `{a;b}` are three distinct trees for one type. The fix is an
**elaboration-time normalizer** that rewrites a telescope's *conjunctive* cell-list to a canonical
**flat, dependency-respecting, sorted spine** — the free-commutative-monoid (row) normal form.
That fragment is confluent and terminating, so it makes `tree_eq` *decide* type-equality there.
Stop exactly at the undecidable laws (dependent codomains, function extensionality, abstract
operands), which stay intensional + transport. Crucially this must be a **compile-time tree
rewrite**, not a runtime combinator — a runtime `append`/`And` produces a lazy closure that is
*value-equal but tree-distinct*, which is no good when conversion is tree identity.

## 0. The problem: conversion is intensional; types have many spellings

```
tree_eq (And {a:Nat} {b:Bool}) {a:Nat, b:Bool}        = FALSE   // nesting ≠ flat
tree_eq (And {a:Nat} {b:Bool}) (And {b:Bool} {a:Nat}) = FALSE   // order matters
```

(Both verified.) Each pair recognizes the *same values* but is a *different tree*, so they don't
unify: `f : And A B -> C` rejects `x : And B A`; a reordered record is a different type. Every
conjunctive type with more than one spelling then needs an explicit coercion — brittle, and it
defeats the point of having `&`/Intersection as a first-class operator.

The target: **normalize telescope types to a canonical form at elaboration, so `tree_eq` *is*
type-equality for the normalizable fragment.** That is "strongly-normalizing types": every (statically
known) telescope reduces to a unique normal form, and equality is identity of normal forms.

## 1. The hard constraint — normalize at elaboration, not at runtime

The natural first instinct, `And := {A,B} -> Telescope (append (cells A) (cells B))` with

```disp
append (t cell tail) B = t cell (λx. append (tail x) B)   // walk the λ-tail chain, splice at nil
append t B             = B
```

**does not give canonical types**, and the failure is the whole lesson. As a *runtime* function,
`append A B` reduces to `t cell (λx. append (tail x) B)` — a closure that lazily computes the rest
when the walker instantiates it. That closure is a *different tree* than the eager flat spine the
surface builds:

```
param_apply (And {a:Nat} {b:Bool}) {a:=2; b:=TT} = Ok TT      // same VALUES recognized
tree_eq     (And {a:Nat} {b:Bool}) {a:Nat,b:Bool} = FALSE     // different TREE
```

**Combinators don't produce canonical forms; only a normalizer does.** The surface `recType`
desugar (`rectype_to_telescope` in `lib/elab/ast.disp` + the `recType` case in `src/compile.ts`)
*already* normalizes — it emits the flat `t (proj_cell …) (λ. t (proj_cell …) …)` spine directly —
which is why `{a:Nat, b:Bool}` is canonical per spelling. So the missing piece is an
**elaboration-time tree-rewriting pass** that brings *derived* spellings (`&`, nested telescopes,
`Intersection`/`Refinement`) into the same canonical spine the surface record already yields.

### 1b. "At elaboration" is a *phase*, not a *module* — and the kernel/elab split is organizational

The real constraint is **reduction-vs-rewrite**, *not* where the code lives. The canonicalizer
must run as a **metalevel tree rewrite that forces eagerly when the type tree is built**, never as
a deferred object-level combinator that stalls under the spine's `λ`-tails (that is the whole §1
lesson, restated precisely). *Where* the rewrite is defined is orthogonal: it can be **kernel-native**,
sitting beside `Telescope` in `lib/kernel/types.disp` as a smart constructor `mk_tele := Telescope ∘
normalize_tele`. The `lib/kernel`↔`lib/elab` boundary is organizational — `lib/elab/bracket.disp`
opens `../kernel/prelude.disp`, so the kernel is *upstream* of the elaborator, not walled off from it.

The rewrite splits into two tiers, by whether a step has to cross a binder:

- **Tier A — the structural fragment** (flatten qid/`&`, drop `⊤`, sort *independent* cells, recurse
  into each cell's *type*). Independent cells carry the **constant tail** `t t rest` (the `K`-shape
  `fields_to_tele` emits at `types.disp:338`), so there is *no* binder to cross — Tier A is a pure
  structural tree rewrite with **zero** dependency on hyps or bracket abstraction. It is **kernel-native
  today**, and it covers the high-value cases (`&` of records, reordered records, nested-record field
  types). This is what "build it into the constructor" actually means, and it's the bulk of the value.
- **Tier B — under dependent binders** (canonicalize the continuation *after* a genuinely dependent
  cell, or normalize a Pi codomain `B x`). This is NbE **reflect/reify**: *reflect* by minting a fresh
  hyp — which the kernel already does in `at`'s `SMint → bind_hyp` (`types.disp:87`) — normalize the
  now-open body, then *reify* it back to a `λ`-tail. **Reify is bracket-abstraction-shaped** (abstract the
  spine body over the minted hyp), and a reifier is the one real structural fact behind "elaborator
  territory" — but note (§10d) it abstracts over a **neutral hyp** (find it by `tree_eq`, replace, S/K/I-
  abstract), so it is *new* substrate code (`reify_hyp`), related to but not the same as `lib/elab/bracket.disp`'s
  **named-variable** abstraction. Since that abstraction is *part of definitional equality* (it decides which
  tree a binder becomes — see `CLAUDE.md`), a small shared `reflect`/`reify` pair belongs **upstream of both**
  kernel and elab; then Tier B is kernel-expressible. Until then Tier B can run host-side (the host already
  bracket-abstracts every `.disp` file, the kernel's own λs included), validated bit-identically per §6f.

So: **Tier A goes into the kernel smart constructor now; Tier B waits only on hoisting reflect/reify
to shared substrate** (a cleanup — §10). Neither is an elaborator-*module* requirement; the original
framing of this note conflated the elaboration *phase* with the `lib/elab/` *directory*.

## 2. The normal form

A telescope is `t cell (λx. rest)`; each cell is a wait-form `wait op meta` whose op is one of
`mint` / `proj name` / `apply` / `deriv name recipe` / `qid` / `qid_meta`. The **canonical normal
form** of a telescope's *conjunctive* structure is a flat spine produced by these rewrites (applied
to fixpoint):

1. **Flatten qid-of-telescope** (the `&`/Intersection law): a cell `qid (Telescope S)` ⟶ splice `S`'s
   cells inline. (`qid A` with `A` a telescope means "v passes all of A's cells," which *is* having
   those cells.) Hence `And A B` = `cells(A) ++ cells(B)`.
2. **Drop the unit:** a `qid Tree` / empty sub-telescope (`⊤`) contributes nothing ⟶ delete the cell.
3. **Splice field-of-telescope into path cells:** a `proj "outer" (Telescope S)` whose *type* is a
   telescope ⟶ for each cell of `S`, a path cell observing `v` via `acc "outer" ∘ <S-observation>`
   (i.e. `{ outer : { inner : T } }` ⟶ `{ outer.inner : T }`). (Optional / second-tier — see §3.)
4. **Sort the independent cells:** within each *dependency class* (cells whose tails don't reference
   each other), order by a canonical key (the field name; positional index for tuples). Dependent
   cells keep their topological order. Hence `{a;b}` ≡ `{b;a}` but `{a; b:B a}` is fixed.

The result is a **flat, topologically-sorted DAG of `(observation-path, constraint)` cells** — the
free-commutative-monoid element on cells, equivalently a **row normal form**. Two types are
definitionally equal (in this fragment) iff their normal forms are `tree_eq`. Recursion: rewrites
1–4 also run inside each cell's *type* (a field whose type is itself a record normalizes), so the
normal form is fully canonical bottom-up.

## 3. The decidability boundary — what to normalize, what not

| normalize (decidable, confluent, terminating) | leave intensional (undecidable) |
|---|---|
| flatten qid/`&` nesting (associativity) | dependent codomains that *branch* on the value (`λa. if … then T1 else T2`) |
| drop `⊤` (unit) | function extensionality (`A→B` vs a 1-`apply`-cell record) |
| sort independent cells (commutativity) | **abstract operands** (`A & B` where `A` is a type *variable* — structure not statically known) |
| path-splice field-of-telescope (composition) | anything needing β/η at higher type, or a proof |

The first column is the **free commutative monoid / row** normal form: associativity + commutativity
+ unit is confluent and terminating (AC-rewriting with a canonical sort), so `tree_eq` decides it.
The second column is the **decidability frontier** — the same line `research/EQUALITY_FOR_VERIFIED_OPTIMIZATION.md`
draws (structural laws are free; the rest needs the licensing relation / transport). It stays
*intensional* (per-spelling, with explicit transport), by design (disp is set-level).

**Static/dynamic split.** Normalization needs the operands' cell-lists *statically*. So `A & B`
flattens when `A`/`B` reduce to known telescopes at elaboration; when an operand is abstract (a
universally-quantified type variable, an un-reduced application), the elaborator **falls back to the
lazy `qid` form** `Telescope [qid A ; qid B]` — correct but non-canonical. So canonicity is *partial*:
total on the concrete fragment, best-effort on the abstract one. That's acceptable — it's exactly the
decidability boundary, and the abstract case is where you'd reach for transport anyway.

## 4. The dependency analysis (the subtle prerequisite for sorting)

Sorting (rewrite 4) is only sound for *independent* cells. A cell `c_i` **depends** on a prior `c_j`
iff `c_i`'s type or recipe mentions `c_j`'s bound value — i.e. the λ-tail that binds `c_j`'s value is
*used* by `c_i`. Detection is already in the kernel: `occurs` / `support_set` (`lib/kernel/engine.disp`)
compute exactly "does this tree mention this hyp." Concretely, a cell is independent of its immediate
prior iff the prior's tail is *constant* (the `K`/`t t` shape — ignores the bound var; this is the
same `λ_. …` test `fields_to_tele` already uses for non-dependent records). So:

- partition the spine into a **DAG**: edge `c_j → c_i` iff `c_i` references `c_j`'s binder;
- **topologically sort**, breaking ties among independent siblings by the canonical key (field name).

The standard hazards: the analysis must be *exact* — over-conservative loses canonicity (misses a legal
reorder); over-eager is *unsound* (reorders a dependent cell past its dependency, changing the type).
Reuse `support_set` rather than re-deriving it.

## 5. The conjunction/arrow distinction — do NOT over-flatten

Only the **conjunctive** structure flattens. Pi's `mint` + `apply` chain does **not**: arrows are not
associative — `A -> (B -> C)` and `(A -> B) -> C` are different types, and currying is meaningful. A
`Pi A B = Telescope [mint x:A ; apply out:(B x)]` whose codomain `B x` is *itself* a telescope must
*not* have its mint/apply spliced into the outer one (`[mint a ; mint b ; apply]` can't even thread
`f a b` correctly — the apply cell sees only the last prior). So the normalizer:

- **flattens** `proj` / `qid` / `deriv` cells (the conjunctive `&` fragment — records, intersections);
- **recurses into** each cell's *type* (a field of record type normalizes internally), and into a Pi's
  *codomain* (`A -> {b:B}` normalizes the `{b:B}`);
- **never splices across** a `mint`/`apply` boundary (the functional fragment is structural, kept as-is).

In polarized-logic terms (`NEGATIVE_TYPES.md` discussion): flatten the additive conjunction `&` and the
multiplicative-ish record; leave `→`/`∀` (the negative *implication*) alone. The normalizer is the
**asynchronous phase running to its unique fixpoint on the `&`-fragment only**.

## 6. What currently needs to be done

Concrete checklist, smallest-to-largest:

- **6a. `normalize_tele : spine -> spine`** — the rewrite of §2. Tier A (flatten + unit + sort) is
  **kernel-native** beside `Telescope` (`lib/kernel/types.disp`) as the smart constructor `mk_tele`
  (§1b); Tier B (under-binder reify) lands once `reflect`/`reify` is shared substrate (§10). Either tier
  carries a host fast-path (`src/compile.ts`), validated bit-identical per the `tree_eq` native-fast-path
  discipline. Start with rewrites 1+2 (flatten + unit); add 4 (sort) behind §4's dependency analysis;
  3 (path-splice) last.
- **6b. The `&` / `And` operator** — surface parse `A & B` (right-assoc, `->`-priority) → at elaboration,
  if `A`/`B` reduce to known telescopes, `Telescope (normalize_tele (append (cells A) (cells B)))`; else
  the lazy `Telescope [qid A ; qid B]` fallback (§3).
- **6c. The anonymous `qid` clause** — extend `recType` (host + `tele_entries` mirror) so a clause with
  *no field name*, `: T`, emits a `qid_cell T` instead of `proj_cell`. Then `{ a:Nat, : Even }` is a
  refined record and `{ : A, : B }` is `A & B` — and both normalize via 6a.
- **6d. `Intersection`/`Refinement` as telescope instances** — make them thin `mint`+`qid` / `qid`+`refine`
  wrappers (validated extensionally identical to the bespoke ones), folding `NEGATIVE_TYPES.md` §4b's
  recognize side. (A small `refine_op` cell for Refinement's `A -> Bool` predicate.)
- **6e. Run `normalize_tele` in the `recType` desugar too** — so even hand-written records get sorted
  (rewrite 4), making `{a;b}` ≡ `{b;a}`. (Today the desugar is flat but order-preserving.)
- **6f. Determinism + host/in-language agreement** — the normalizer must be a pure deterministic function
  (so the elaboration discipline still holds: same semantics → same tree), and the host fast-path must be
  bit-identical to the in-language spec (the existing `desugar.test.ts`/`compile.test.ts` equivalence
  pattern extends to it).
- **6g. Tests** — pin `tree_eq (A & B) (B & A)`, `tree_eq (A & (B & C)) {a;b;c}`, `tree_eq {a;b} {b;a}`
  (all `= TT` post-normalization); randomized normalize-idempotence (`normalize ∘ normalize = normalize`)
  and confluence (different rewrite orders → same normal form); and that recognition is unchanged
  (normal form recognizes the same values as the original).

## 7. Staging

1. **Flatten + unit** (rewrites 1–2) + the `&` operator (6a/6b, fallback path) + anonymous-`qid` clause
   (6c). Makes `&`/nesting collapse to one tree; `Intersection`/`Refinement` become telescope instances
   (6d). *Highest value, lowest risk* — no dependency analysis needed (associativity + unit only).
2. **Sort** (rewrite 4 + §4 dependency analysis) + run it in `recType` (6e). Delivers full commutative
   canonicity (`{a;b} = {b;a}`). *The real engineering* — the dependency analysis must be exact.
3. **Path-splice** (rewrite 3). Canonicalizes nested-record field access. *Optional polish.*
4. **Defer** the undecidable column (§3): leave to transport / the verified-optimizer's licensing.

## 8. What it connects to

- **NbE for types.** The elaborator becomes a *type-normalizer*: the flat-sorted spine is the normal
  form (reify), `tree_eq` is conversion. This is reflect/reify at the type level.
- **AC-rewriting / free commutative monoid / row types.** `&`'s normal form is the sorted cell-multiset;
  flatten+sort is literally row-type canonicalization; `&` is row merge.
- **Polarized / focused logic.** The normalizer is the asynchronous phase normalizing the negative
  `&`-fragment to its unique form; the place it *halts* (the positive `Coproduct` phase, and the `→`
  boundary) is the polarity wall.
- **The decidability frontier.** Structural = free (normalize); deep = proof (transport). This is the
  set-level-intensional + OTT/licensing stance of `research/EQUALITY_FOR_VERIFIED_OPTIMIZATION.md`.
- **The deterministic-elaboration discipline.** Normalization *strengthens* it from "same spelling →
  same tree" to "same *semantics* → same tree" on the structural fragment — additive, no conflict.

## 9. Open problems / risks

- **Dependency analysis exactness** (§4): the load-bearing correctness condition for sorting. Over-eager
  reordering past a real dependency is *unsound* (silently changes a type). Must reuse/verify `support_set`.
- **Confluence + termination.** The rewrite system (flatten, unit, sort) must be confluent and
  terminating to *have* a unique normal form. AC-monoid + a total canonical order is standard, but it
  should be *pinned* (the idempotence/confluence tests in 6g), especially once path-splice (3) interacts
  with sort (4).
- **Cost.** A normalization pass per telescope type at elaboration. Mitigated by hash-consing (normalize
  once per distinct tree; memoize), but worth measuring against `APPLY_BUDGET`.
- **Partial canonicity** (§3 static/dynamic): abstract operands stay non-canonical (lazy `qid`). So
  `A & B` for a type *variable* `A` won't unify with a rearrangement until `A` is known. Acceptable (it's
  the decidability boundary), but means "strongly normalizing types" is a statement about the *concrete*
  fragment, not all types.
- **Don't over-flatten** (§5): the conjunction/arrow distinction must be respected, or you'll wrongly
  identify `A -> (B -> C)` with something flattened. The normalizer keys on the cell *op* (proj/qid/deriv
  flatten; mint/apply don't).
- **Duplicate-field policy.** `{a:Nat} & {a:Bool}` produces two `a` cells. Either reject (duplicate field,
  as the parser already does within one record), or define it as the *meet* of the field types (`a : Nat
  & Bool`) — a design choice the `&` operator must pin.

## 10. Prerequisite cleanup (do before the structural work)

**Status (2026-06-19 pass).** A cleanup pass investigated all five. **10a + 10b LANDED** (clean,
tested, on branch `cleanup/pre-normalization`). **10c, 10d, 10e were reclassified**: investigation
showed each is *not* a mechanical prerequisite but is **entangled with an open frontier** (the §4b
respond face, Tier-B reify, the §7B spine-typing wall). They are not safe blind swaps; the findings
below redefine each as scoped frontier work rather than pre-cleanup. So the *actual* prerequisites for
Tier-A normalization are only 10a + 10b — both done.

- **10a. (LANDED) Constant-tail vocabulary.** `const_tail := {rest} -> t t rest` and
  `is_const_tail := {tail} -> and (is_fork tail) (is_leaf (pair_fst tail))` (`types.disp`, by the cell
  constructors); `fields_to_tele` now builds via `const_tail`. `is_const_tail` is the §4 **structural
  (conservative) proxy** for "cell independent of its prior"; exact non-adjacent analysis stays with
  `occurs`/`support_set`. *Key finding the test pins:* the structural proxy and the exact analysis answer
  **different** questions — Sigma's generic former always threads `B a` through its tail (proxy = "dependent"
  even when the codomain ignores `fst`), while a surface record inlines the concrete field type and yields a
  genuine K-tail (proxy = "independent"). The sort keys on the proxy; over-eager sorting is guarded by the
  exact scan (§9).
- **10b. (LANDED) `tree_lt` structural total order** (`prelude.disp`, after `tree_eq`): `leaf < stem <
  fork`, then lexicographic; the deterministic sort key (NOT the allocation-order hash-cons id). Unannotated
  like `nat_le`/`tree_eq`'s siblings — it triages its argument, so a Pi annotation would mint a neutral and
  trip the walker's triage-on-neutral guard (only `tree_eq` is a sanctioned carve-out for that pattern; a
  *finding* of this pass — `tree_lt` cannot carry `Tree -> Tree -> Bool` until/unless it's added to the
  carve-out set, which it should not be).
- **10c. `Intersection`/`Refinement` as telescopes — RECLASSIFIED to §4b frontier (do with `&`).** The
  naive "`Telescope [mint A ; qid (P hyp)]`" swap is **unsound as a drop-in**: (i) the telescope respond is
  `at FF`, and a *mint-lead* spine responds **Pi-like** (instantiate binder, Extend codomain) — but
  `Intersection` is non-callable and must stay `inert` (`types.disp:199` uses `inert_respond`); **no test
  pins Intersection's respond**, so the change would be *silent* and wrong; (ii) the recognize side is *not*
  bit-identical — a failing predicate gives `Err` in the bespoke form but `Ok FF` through `qid_op`'s `Err →
  SReject` path. `Refinement` additionally needs a brand-new `refine_op` (its `P : A -> Bool` is a bare-Bool
  predicate, not a Type, so `qid_cell` doesn't fit). **Conclusion:** this is the §4b *recognize+respond*
  frontier, and its only consumer is the `&` operator (Stage 1). Do it *there*, building a telescope-recognizer
  wait-form that **keeps `inert` respond** and is validated extensionally bit-identical — not as standalone
  pre-cleanup.
- **10d. Kernel-native Tier-B reify — RECLASSIFIED as new code, deferred to Tier B.** Re-scoped on
  investigation: Tier B does **not** need the existing `lib/elab/bracket.disp` *hoisted* — that machinery
  abstracts over a **named Cir/AST variable**, whereas Tier B must abstract over a **neutral hyp** (find the
  minted neutral by `tree_eq`, replace with the bound position, S/K/I-abstract). That is a *new* substrate
  primitive (`reify_hyp`, expressible over `prelude` + `tree_eq`), not a relocation. The reflect half exists
  (`bind_hyp` mint, `types.disp:87`). Since it gates only the (not-yet-built) Tier B and has no consumer to
  validate end-to-end, **build it with Tier B**, not now. (Tier A needs none of it.)
- **10e. `StrictType` → `Type` — substantive part LANDED; literal name-collapse BLOCKED at the host
  level.** Worked this pass (2026-06-19). Outcome in two parts:
  - **LANDED (committed):** *(a)* the per-former **structured RespondShapes** are now defined and
    validated **in-tree** (not just PoC scratch): `InductiveFrame`, `InductiveRespondShape`, and the
    params-pinned `ProjectingRespondShape` — the projecting family (Unit/Ord, Eq's J-rule) and the Pi
    negative former all inhabit their shapes (`metashape.test.disp`). Residual pinned: recursive *gated*
    formers (Nat) need per-constructor `cases` structure (the branching residual); Record/Sigma
    projection-frame shapes not yet built. *(b)* **StrictType is now a real UNIVERSE, not just a
    recognizer** — its respond was a §4b *placeholder* (telescope `at FF` subsumption); swapped for
    `type_predicate_h_rule`, so a StrictType-hyp works as a polymorphic Pi domain (`∀(A:StrictType). A→A`
    recognizes the polymorphic identity — empirically confirmed: it *failed* before, passes now). This is
    the substantive content of "make the universe strict": StrictType can now actually serve as `Type`.
  - **BLOCKED — the literal `Type := StrictType` name-collapse:** an **irreducible host-level bootstrap
    cycle** (confirmed empirically, two distinct failures). *(1)* In-language: `Type` is used in VALUE
    position early (`Action`'s `Extend := Type`, `InductiveFrame`'s motive, `NeutralMeta`) before the
    strict machinery (MetaShape/qid/CheckerResult) exists, and within-file value refs are sequential —
    fixable by reordering those defs after a late `Type := StrictType`. *(2)* But then the **host
    elaborator** breaks: `compile.ts`'s `isUniverseTree(type_tree, Type)` (the positional type/value
    desugar) needs `Type` **in scope when compiling every `: Type` binding**; a late `Type` is invisible
    to the early ones, corrupting the type/value decision (`binderToPi` then fails on a value lambda).
    Breaking this needs a **host change** — forward-declaring `Type`'s tree before compiling the kernel,
    or a two-pass top-level compile — not just a `.disp` reshuffle. Reverted the collapse experiment; the
    universe stays two names (`Type` lean-early for bootstrap, `StrictType` the strict universe) until the
    host forward-declaration lands. So "universe is a telescope" (for 6d normalization) holds *via
    StrictType*, just not yet under the name `Type`.

**Test impact (no pre-cleanup needed, but update on landing).** The suite is not redundant; the pins to
revisit *when normalization lands* are: `telescope.test.disp:134,144` (surface `{a:Nat,b:Bool}` ==
hand-built — survives as long as `b` sorts after `a`, i.e. already-sorted spellings are fixed points),
`kernel.test.disp:121-126` (Pi `mint_cell` first — safe, §5 never sorts mint/apply; Sigma first cell
`name = "fst"` — safe, `f < s`), `record.test.disp:25`, and `ast.test.disp:54-76` (the *symbolic* desugar
— keep `normalize_tele` a **distinct post-desugar tree pass**, not folded into `rectype_to_telescope`, so
these stay valid). `soundness.test.disp` / `metashape.test.disp` are order-agnostic — unaffected. Add new
invariance tests (§6g) rather than editing these in place.

## References

- Verified facts (this note): `tree_eq (And {a:Nat} {b:Bool}) {a:Nat,b:Bool} = FALSE`,
  `tree_eq (And {a:Nat} {b:Bool}) (And {b:Bool} {a:Nat}) = FALSE`, and that runtime `append` gives a
  value-equal but tree-distinct telescope.
- Code: `lib/kernel/types.disp` — the telescope cells (`proj_cell`/`deriv_cell`/`mint_cell`/`apply_cell`/
  `qid_cell`), `at`, `fields_to_tele` (the constant-tail/non-dependent test reused in §4); `engine.disp`
  — `occurs`/`support_set` (the dependency analysis); `lib/elab/ast.disp` + `src/compile.ts` — the
  `recType` desugar to extend (6c/6e).
- Companions: `NEGATIVE_TYPES.md` (telescope shape, §4b `qid`/Intersection, the polarity boundary),
  `STRICTTYPE.md` (the metacircle — `StrictType` is itself `And(recognize-face, meta-face)`),
  `research/EQUALITY_FOR_VERIFIED_OPTIMIZATION.md` (the decidability frontier / transport).
