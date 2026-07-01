# ACTIVE_PLAN.md — the path to a verified, cell-derived kernel

**Status (2026-06-27):** active working plan, synthesized from the respond-verification
investigation. Supersedes the scattered "next steps" notes in `STRICTTYPE.md`/`KERNEL_SELF_TYPING.md`
for *sequencing* (those remain authoritative for their respective mechanisms). This file is the
map; the per-topic docs are the territory.

**Uncertainty legend:** ✅ done/proven · 🟢 low (mechanical, clear how) · 🟡 medium (clear approach,
untested details, plausible obstacles) · 🟠 high (real obstacles, conjectured resolution) ·
🔴 research (genuinely open; may be impossible in the strong form).

---

## 0. The destination — a type *is* its cells

A type `T = wait recognizer meta`. The end-state is that **every operation on a type is a uniform
walk over the same telescope of cells**, and the kernel verifies the cells:

| operation | mechanism | status | doc |
|---|---|---|---|
| build / recognize | `at TT` over the cells | ✅ | `NEGATIVE_TYPES.md`, `KERNEL_DESIGN.md §Telescopes` |
| eliminate — negative (Pi/Σ/Record) | `at FF` over the cells | ✅ (§7B) | `STRICTTYPE.md §7`, `NEGATIVE_TYPES.md` |
| eliminate — positive (gate the cases) | `coh_check` = `check cases (case-telescope)` — η-readback, ONE gate for ALL inductives | ✅ | `coproduct_gate.test.disp` |
| map / fold | `fmap_n` / `fold_value` over the cells | ✅ | `CELL_OPTICS.md` |
| "is this respond sound?" | `GoodRespond` = a TELESCOPE of `probe` cells (coh + per-case junk; reflect out-of-band) | ✅ landed; `verify_good` = `param_apply (GoodRespond T) R` composes under the walker (R6 mechanism unblocked) | `goodrespond.test.disp`, this file §T1-L2 |
| meet types (Intersection/Refinement/Eq) | telescope hybrids: `qid`/`mint` + `refine`/`imeet`/`eqends` cells + type-specific respond | ✅ migrated off hand-written recognizers (2026-06-29) | `types.test.disp` |
| the cell representation itself | one `(optic, role)` | 🟠 refactor (probe/refine/imeet/eqends/qid added — the role vocabulary is filling in) | `CELL_OPTICS.md §3-§5` |

"Full verified kernel" = the kernel checks that every type's cells are well-formed and **both faces
are derived from those cells**, on top of a *minimal, pinned trusted base*.

---

## 1. The trust ceiling — what stays trusted (read first)

"Full verified" ≠ "zero trust." Two parts of the kernel are trusted by construction:

- **The substrate floor — PERMANENT.** `pair_snd`, `triage`, `type_meta`, `checker_sig`, `is_fork`,
  `support_set`, … — anything that *raw-inspects* a tree — cannot self-verify, because verifying
  `f : Tree -> X` mints a `Tree`-hyp and `f` triages it, which the walker rejects by the *same*
  parametricity that makes it sound (pinned by `lib/tests/soundness.test.disp`). Certainty it's a
  ceiling: ✅. Goal = **minimize and pin** this set, not eliminate it. The 4 *sanctioned readers*
  (`pair_fst`, `neutral_type`, `tree_eq`, `I`) are the carve-outs; everything else payload-reading
  is irreducible TCB.
- **The walker / Σ-op core — RESEARCH (🔴).** `param_walker` triages its argument and *is* the
  policer — it cannot police itself without circularity. Same for `hyp_reduce`/`bind_hyp`/`make_hyp`
  (`make_hyp` literally forges a neutral = the stem-forge the walker rejects). The conjectured escape
  is a **sealing modality** (`KERNEL_SELF_TYPING.md`, `project_sealing_framework` memory). It may be
  unachievable; then this core stays trusted permanently. That's *fine* — it caps "full verified" at
  "everything above a small pinned TCB."

The honest target metric is **the size of the TCB**, and "everything above it verifies."

---

## 2. Established facts (proven this investigation — don't re-derive)

- The gate (`coh_nat`/`coh_bool` via `gated_inductive_respond`) enforces **type-coherence of
  recursor cases**: a `step` returning `TT:Bool` under a `Nat` motive is rejected (`kernel.test`
  #31-39). It also polices **parametricity** of cases (`adversarial.test:20`: a `step` doing
  `is_fork p` is rejected). It is the clause that gives the eliminator **subject reduction**.
- **Dropping the gate is unsound** — proven: yields a function accepted as `Nat->Nat` that returns
  `TT:Bool` (type confusion). Don't drop it.
- **Unit and the generic `Coproduct` are non-gated** (`inductive_respond`) → they carry the *same* latent
  type-coherence gap (**Ord ✅ and List ✅ were closed by R3**). PROVEN: `bad_fn = λo. ord_rec (const Nat)
  TT (…TT…) o` was accepted as `Ord->Nat` yet `bad_fn zero_ord = TT` (pre-gate); reproduced + closed for
  List (`gap_severity.test.disp`). The live analogue is now the generic `Coproduct` (arbitrary variants →
  needs R2). **The gate is the correct universal design; non-gated formers are the bug.** (Severity —
  **R0 RESOLVED**: a subject-reduction violation, **not** inconsistency; no closed `False` proof,
  defended by use-site re-checking. See R0.)
- **Sealing `coh_nat`'s internal `bind_hyp`** (`apply_policed` instead of raw `param_apply`) is
  **runtime-identical** (`apply_policed M x ≡ param_walker M x`, which `param_apply` unwraps) and
  makes the gate self-type / be behaviorally checkable under the walker. PROVEN (kernel/adversarial/
  soundness green with the seal). The "structured/dependent frame" was a **red herring** — the seal
  alone matters.
- **`RespondShape` inhabitance = totality = VACUOUS** for correctness: `vac_coh`(always accept),
  `rej_coh`(always reject), and the real gate all inhabit it. So inhabiting `RespondShape` is *not*
  the property worth proving.
- **`GoodRespond`** — a type whose check is `is_good R = (R accepts coherent cases → motive self)
  ∧ (R rejects unknown/`Tree`-cases → InvalidType)`, both via **`tree_eq` (conversion), not the `Eq`
  type** — *does* distinguish good from bad (gate=good, `inductive_respond`/vac/rej=bad), and
  condemns Ord/Unit/Coproduct (they share `inductive_respond`). PROTOTYPED. It needs the responds
  **sealed** to run under the walker (the bad ones already are; the gate needs R1).
- The **recognition-form** gate (`param_apply (case-telescope) cases`) **breaks `nat_rec`** —
  recognizing a `step`-HYP against the `Π` type uses the H-rule (tree-identity), which rejects the
  recursor's legitimately-typed hyp. The **application-form** gate (`coh_nat`: apply the case to
  fresh args, recognize the *result*) is robust. **`derive_gate` MUST be application-form.**

---

## 3. Track 1 — Verified responds

> **✅ UPDATE 2026-06-28 — the gate is now ONE generic, self-typing, η-readback `coh_check`.**
> `derive_gate` and the four hand-written `coh_nat`/`coh_bool`/`coh_ord`/`coh_list` are **deleted**;
> every inductive (Nat/Bool/Ord/List + Coproduct/Coproduct_p → Option/Result/Either/CheckerResult/
> Action) now routes through `coh_check params = {T, motive, cases} -> check_fields (dct_go params …) cases`.
> `check v T` is **η-readback** (bidirectional Π-introduction): a Π-headed type → mint the domain, check
> `(v h)` against the codomain; a neutral/base type → recognize raw `T v`. The combination of sig-dispatch
> (lead-cell-mints, via `pair_fst`) + **raw base** `T v` (never `param_apply` on a hyp) is what lets it
> **self-type** AND verify a recursor's **own type** (hyp cases) — the two things the recognition-form
> couldn't do. Shape-encoded types reconstruct the constructor value through the `encode` iso
> (`pair_snd (type_meta T).functor`; identity for inj-tagged). The case-telescope (`dct_go`) is the spec
> the gate checks against — recognize/respond/gate are now nearly one walk (Track 2's goal, partially
> realized). **Caveat:** `ProjectingRespondShape` self-typing remains *totality* (vacuous at `self:Tree`),
> the same as before — the *behavioral* GoodRespond (R4–R6) is unchanged and still open; `check` is its
> substrate. Commits: Stage A (inj-tagged) + Stage B (shape-encoded), full suite green (44 files).
> *The R0–R3 history below is the path that got here; the destination is `coh_check`.*

Two levels: **Level 1 makes the kernel SOUND; Level 2 PROVES it.** Level 1 is the critical path.

### Level 1 — close the gap (derive the gates)

- **R0 — how exploitable is the gap?** ✅ **RESOLVED 2026-06-27** (`gap_severity.test.disp`). The gap is
  a **subject-reduction / canonicity violation, NOT a logical inconsistency** — it does **not** cascade to
  a closed proof of `False`. Probed via List (still non-gated) with an `elim`-routed recursor (the route
  the closed `ord_rec` exploit used): `lying` is accepted as `NatList -> Nat` yet returns `TT:Bool`, and
  the mistyped `bad_eq : (xs) -> Eq NatList nil xs` is accepted — **but** the forged proof, used at its
  claimed type, is rejected (`param_apply (Eq nil (cons zero nil)) refl = Ok FF`), and the
  transport-to-`False` cascade is `Ok FF`. **Why structural:** disp re-checks every value against its type
  at the use site (`apply(T,v)=TT`), so gap-junk fails the target recognizer (Eq endpoints / False) and
  gap-neutrals are open (fail closedness). **Urgency verdict:** R2/R3 are needed for subject-reduction
  soundness (a real bug — `lying : NatList -> Nat` returns Bool), but the logic is intact, so this is not
  an emergency; the cheaper K0/K1 may precede R2. *Caveat: empirical over the Eq-transport + direct-False
  routes, not a proof — a cleverer exploit is not formally excluded.*
- **R1 — seal the gate's internal `bind_hyp`.** ✅ **LANDED 2026-06-27** (sealed `coh_nat`;
  runtime-identical; full suite green; flips the now-resolved `metashape:78`/`coh_gate_proto:32`
  residual assertions, which were updated). Replaced, in `coh_nat`:
  ```
  let step_coherent = (param_apply (bind_hyp T) ({n} -> …))
  ```
  with
  ```
  let step_coherent = (match (apply_policed (bind_hyp T) ({n} -> …)) { Ok v => v; Err _ => Err })
  ```
  Runtime-identical. `inductive_respond`/`coh_bool` have **no raw `param_apply`** so they need no
  seal (audit: only `coh_nat` and any new `coh_*` do). Required so the gate is checkable (Level 2)
  and self-types. *File: `lib/kernel/types.disp`.*
- **R2 — `derive_gate`: generate the application-form gate from a type's constructors.** ✅ **LANDED
  2026-06-27** (`derive_gate` in `lib/kernel/types.disp`; `coproduct_gate.test.disp`). For a type `T`
  with variants `[(tag, argspec)…]` and a motive:
  ```
  derive_gate variants params T motive cases = ∧ over (tag, argspec):
    mint args (bind_hyp T for Rec | params i for Param i | the type otherwise),
    mint IH per Rec arg (bind_hyp (motive arg)),
    recognize ((field cases tag) args… ihs…)  :  motive (inj tag (build_payload args))
  ```
  `coh_nat` generalized over (multiple constructors, multiple rec args + IHs, dependent motives,
  arbitrary tags). **The feared blocker did NOT materialize:** the recursive `dg_go` **EMITS** inline
  `bind_hyp` continuations (the bind_hyp-inline constraint is about *receiving* a passed kont, not
  emitting) — a 2-rec-arg body using *both* outer hyps does not trip the occurs-check (validated
  MyTree `[leaf, node:[Rec,Rec]]`). So **no** Step-based harness was needed. The outer mint is sealed
  (`apply_policed`, R1); inner mints run raw under the walker the seal opened. **Integration friction
  (the real work):** cases must be **field-readable** (`list_const`-wrapped, keyed by tag — the same
  convention `rec_value` uses), so `option_rec`/`result_rec`/`either_rec` neutral branches switched
  from ad-hoc positional pairs to `mk_record [tags] [list_const case…]`. Reuses `build_payload`
  (moved above `Coproduct`), `is_param_marker`/`is_recunder_marker`/`param_nth`. (`derive_case_telescope`
  is the recognition-form / recursor-*type*, NOT the gate.) Tested every recursor (nat/ord/list/option/
  result/either + generic MyNat/MyTree). *Docs: `POSITIVE_TYPES.md`
  §4 (case-telescope derivation), `coh_gate_proto.test.disp` (what self-types and what doesn't).*
- **R3 — gate Ord/Unit/Coproduct/List.** **Ord ✅ LANDED 2026-06-27** (`coh_ord`, application-form,
  sealed; gap closed — incoherent `ord_rec`→`Ok FF`, raw incoherent elim→`InvalidType`; `ord_rec`'s
  own type still verifies; suite green). This *proves the pattern extends* to a recursive, 2-rec-arg
  former without R2. **List ✅ LANDED 2026-06-27** (`coh_list`, std/list.disp; the demonstrated gap —
  `gap_severity.test.disp` — closed: lying recursor + dishonest proof-builder both → `Ok FF`; recursors
  + recognition intact, full suite green). List **disproved its own "needs R2" classification**: it has
  *fixed* constructors (`nil`/`cons`), so it's hand-writable like `coh_nat` — the new wrinkle (a
  PARAMETERIZED former: cons's head is at the element type `A`, not `T`) is handled by threading `A`
  via partial application (`gated_inductive_respond (coh_list A)`), which **de-risks R2's param story**.
  **Unit:** no recursor exists → gap unreachable via a recursor → deferred (gate it only if a raw-elim
  threat is shown). **Coproduct/Coproduct_p (generic) ✅ LANDED 2026-06-27** via **R2 `derive_gate`** —
  both convenience formers now carry `gated_inductive_respond (derive_gate variants params)`, so EVERY
  inductive built from them (incl. Option/Result/Either + the kernel's `Coproduct_of` →
  `CheckerResult`/`Action`) is gated with no per-type hand-written gate (`coproduct_gate.test.disp`;
  full suite 44 files green). The remaining non-gated surface is only the bespoke `Coproduct_viewed …`
  / `Coproduct_ctx` instances *that don't supply a gated respond* (none currently: Nat/Bool/Ord/List
  are gated). Pattern (proven for nat/ord/list and now generic): switch the former to a
  `gated_inductive_respond`. Verify: (a) recursors' *own types* still verify (kernel loads),
  (b) the gap-closing test (`bad_fn` now → `InvalidType`/`Ok FF`), (c) `metashape`/`adversarial` green.

### Level 2 — verify responds behave (the respond metacircle)

- **R4 — universal + generic `GoodRespond`.** ✅ **LANDED** (`lib/kernel/types.disp`; pinned
  `lib/tests/goodrespond.test.disp`). `verify_good`/`good_resp`/`good_resp_reflect`/`GoodRespond`,
  generic over `T`'s variants, ABSTRACT motive + self (so the obligations are ∀ by parametricity), test
  frames derived per-type from `derive_case_telescope`. Reuses `check` verbatim via `CONV x = MEM
  (Singleton x)` (conversion = singleton-membership → no new walker mode). Validated on MyNat/3-variant/
  Nat/Bool/Ord — incl. the **recursive eliminator** (the structural-self-typing RESIDUAL wall), which
  the *behavioral* check covers.
- **R5 — `GoodRespond` is itself sound.** ✅ **mechanism LANDED + characterized.** The soundness-
  critical direction is "reject incoherent," and incoherence is a **gradient of dimensions**, not one
  predicate: (1) **per-case** type-junk (NOT single-slot — the `R_lazy` adversary, which only checks the
  first case, passes a naive probe but is caught per-case); (2) **reflection/parametricity** — a separate
  dimension (the `R_unpoliced` adversary is type-sound yet reflection-unsound). **FUSION finding:**
  `check` polices its mints, so any check-based respond rejects reflection *for free* → reflect only
  matters for hand-rolled unpoliced responds (the out-of-band `good_resp_reflect`). The complete spec
  `R ≡ coh_check` is sound+complete but **circular** for self-verification, so an independent
  `GoodRespond` is necessarily the dimension-enumerating gradient. Resolves the TCB worry: `GoodRespond`
  is the **spec + a BOUNDARY checker**, not a kernel step (derived responds stay correct by construction).
- **R6 — wire respond-checking into `Type`.** 🟢 **composability UNBLOCKED** (telescope refactor,
  2026-06-29). The CONTAINMENT obstacle was self-inflicted by *hand-rolling* `good_resp` from nested
  `check`/`apply_policed`. Re-expressed as a **TELESCOPE** — `GoodRespond T` = `[mint case_tele ; probe
  coh ; (mint junk_i ; probe InvalidType)*]`, a `probe` cell = "apply R to a frame, check result ≡
  Singleton(outcome)" — it is ONE `at` walk, so `param_apply (GoodRespond T) R` (= `verify_good`)
  **composes under the walker** (validated: real responds → `Ok TT`, lazy/inert/over-permissive →
  `Ok FF`, all under `param_apply`). coh + per-case junk are cells; reflection stays out-of-band (its
  rejection is a non-local triage-abort, genuinely not a cell — but FUSION makes it redundant for
  check-based responds). **Remaining for the actual StrictType cell:** respond-KIND dispatch — only
  gated-inductive responds get `GoodRespond`-checked; non-inductive formers (Pi/Record/Eq/…) have
  inert/J/H-rule responds that need their own (cheaper) specs. **The right wiring is the MERGE
  (2026-06-29):** inline GoodRespond's cells as a dependent TAIL of StrictType's own telescope (deriving
  `R = resp_of v` in the probe) — no sub-`param_apply`, no nested walker. Validated top-level:
  `param_apply MergedST Nat = Ok TT` (recursive, ~1.4M steps, cross-validated 3 ways), discriminates bad
  responds, self-types (`lib/tests/r6_merge_proto.test.disp`). **The real blocker is COST, not a bug
  (2026-06-29, definitively diagnosed):** the only check-site is module auto-verify (`verify mod`), which
  checks `T : Type` *nested* under its walker — and running the merged check there is OUT-OF-MEMORY on
  EVERY backend (rust-eager OOM 3.4GiB → raw `unreachable` via `handle_alloc_error`; eager-TS 8GiB heap;
  naive ~4e9-node "Invalid array length"). The outer walker re-materializes the whole deep GoodRespond
  computation with no sharing. So earlier "stem-forge / route M/s through bind_hyp / nested-walker abort"
  framings are SUPERSEDED — there is NO backend bug and NO sealing needed. **Path:** run the self-check at
  **definition-time, top-level** (cheap, where it works) — NOT inline in the universe recognizer (which
  forces every nested `T:Type` to pay the blowup) — OR make GoodRespond dramatically cheaper. Authoritative:
  memory `project_telescope_meet_unification` FOLLOW-UP 4-6. *Doc: `STRICTTYPE.md §7`, `goodrespond.test`,
  `r6_merge_proto.test`.*

**Strategy verdict:** prefer **Level 1 (derive)** for the kernel's own formers — correct by
construction, sidesteps R5's trust question. `GoodRespond` (Level 2) **landed as the spec + boundary
checker**; promoting it to a StrictType recognizer cell is gated on the R6 containment carve-out.

---

## 4. Track 2 — Full optics (parallel enabler, NOT on the soundness path)

Unifies the cell machinery so recognize/respond/gate/map are one walk. *Doc: `CELL_OPTICS.md`.*

- **O0** `over`-half + `fmap`/`fold` + `Param`/`Coproduct_p`/`Record_p`. ✅ landed.
- **O1** add `{get, over}` optic under existing cells (pure addition). 🟢.
- **O2** re-seat extractive cells (`proj`/`pos`/`rec`/`qid`/…) on optics, byte-for-byte. 🟡 (touches
  every cell).
- **O3** collapse the 9 per-kind ops → one `cell_op` over `(optic, role)`. 🟠 — re-seats the `qid`
  cells carrying the **StrictType metacircle** (`metashape.test` must stay green) and adds an
  indirection on the **hot `at` path** (already ~2× the old walkers). The one refactor that could
  *break* the recognize-face metacircle we already have.
- **O4** migrate types onto `Param`; delete `build_payload`/`mk_record` duplication. 🟡.

**Role:** the substrate that makes "derive everything from cells" (R2, K-track) *one* machinery
instead of five traversals. Deferrable; its uncertainty is engineering, not research.

---

## 5. Track 3 — Full verified kernel (the umbrella)

- **K0 — declare + pin the substrate floor** as trusted axioms, pinned to `soundness.test`. ✅
  **LANDED 2026-06-27.** A "SUBSTRATE FLOOR (K0)" block in `soundness.test.disp` pins the boundary
  precisely: the 4 sanctioned readers (`pair_fst`/`neutral_type`/`tree_eq`/identity) verify on a hyp,
  while the floor readers (`pair_snd`/`is_fork`/`is_leaf`/`type_meta`/`is_closed`) are walker-REJECTED
  — that rejection *is* the proof they're trusted-by-construction. (`checker_sig` reads via the
  `pair_fst` carve-out, so it's trusted but not a direct-triage floor reader.) *Defines* the TCB and
  makes "everything else verifies" a checkable claim.
- **K1 — verify everything above the floor** (builders, formers, recursors, structural types). 🟢-🟡.
  Much validated (the Tier-1 sweep: `inductive_respond : InductiveRespondShape`,
  `type_predicate_h_rule : RespondShape`, `is_neutral : Tree -> Bool`,
  `extend_neutral_meta : NeutralMeta -> Type -> Frame -> NeutralMeta`, `InvalidType : Type`), made a
  checkable claim by the per-fragment `verify {cut,engine,types}_mod = Ok TT` (use_raw.test) + an
  "ABOVE THE FLOOR (K1)" block in `soundness.test` (former/structural-type/recursor reps). The
  arena/memo blocker is fixed (watermark memo, committed), so auto-verify load no longer OOMs.
  **Residual:** `list_rec` / list builders are not yet annotated → not in the fragment sweep (tracked).
  *Memory: `project_kernel_self_typing_metashape`, `project_rust_eager_shared_session_memo`.*
- **K2 — responds** = Track 1. 🟡/🟠.
- **K3 — self-type the walker / Σ-ops via the sealing modality.** 🔴 research (see §1).
- **K4 — metacircular closure** (`Type` certifies all of the above incl. itself). 🟠 (gated on K2+K3).

---

## 6. Critical path (recommended order)

1. **R1 + R2 + R3** — seal, derive the gates, close the gap. *Highest value:* turns a confirmed
   soundness bug into a sound, uniformly-derived elimination story. (Pre-step **R0** to set urgency.)
   *✅ COMPLETE 2026-06-27: R0 ✅ (gap = subject-reduction bug, not inconsistency), R1 ✅ (seal),
   R3-Ord ✅, R3-List ✅ (`coh_list`), **R2 ✅** (`derive_gate`, generic), **R3-rest ✅** (Coproduct/
   Coproduct_p gated). The type-coherence gap is CLOSED across every kernel + std inductive former.*
2. **K0 + K1** — pin the floor, sweep the above-floor verifications. Cheap, clarifies the TCB.
   *✅ LANDED 2026-06-27: K0 floor-boundary block + K1 above-floor block in `soundness.test`; residual =
   `list_rec`/list builders un-annotated.*
3. *Fork:* **Track 2 (O1–O3)** to unify the cell machinery *before* the harder verification, OR
   **R4–R6** for the in-language behavioral proof. Lean optics-first — a clean `(optic, role)`
   substrate makes R4–R6 and K4 far less hairy.
4. **K3 (sealing)** last — research; everything else delivers value without it.

## 7. Biggest unknowns (ranked)

1. 🔴 **Walker self-typing (sealing, K3).** May be unachievable → walker core stays trusted (fine,
   but caps "full verified").
2. 🟠 **Is `GoodRespond` a sound spec or a new trusted checker (R5)?** Derive-and-trust sidesteps it.
3. 🟢 **Optic collapse vs the StrictType metacircle (O3) — partly realized.** The η-readback `check`
   landed as ONE gate over the case-telescope without breaking the metacircle (metashape 35/35); it is
   the substrate for collapsing recognize/respond/gate into one walk. Remaining: re-seat the negative
   cells on the same `check`/role axis.
4. ✅ **One gate preserving every recursor (R2) — RESOLVED, then UNIFIED.** `derive_gate` generalized the
   hand gates; `coh_check` (η-readback) then *replaced* all of them with one self-typing gate that also
   verifies recursor own-types (hyp cases). All recursors preserved (44 files green).
5. ✅ **Severity of the gap (R0) — RESOLVED.** Subject-reduction violation, *not* inconsistency; no closed
   `False` proof (use-site re-checking defends). `gap_severity.test.disp`.

**Bottom line:** a **sound** kernel (gap closed, gates derived) is **ACHIEVED** — the type-coherence gap
is closed across every kernel + std inductive former by **ONE** self-typing, η-readback gate (`coh_check`,
2026-06-28), no hand-written per-type gates. What remains is **verification** in the strong sense: gated
on `GoodRespond`'s status (🟠, R4–R6 — *behavioral*, non-vacuous self-typing) and walker self-typing
(🔴, K3). Realistic
end-state: *"everything above a small, pinned, soundness-tested trusted base is verified, and both
faces of every type are derived from its cells."* — the soundness half now holds; the verified half is
the open frontier.

## 8. Doc index

- `GOALS.md` — north star. `TYPE_THEORY.typ` — authoritative spec (target).
- `KERNEL_DESIGN.md` — implementation idioms (`§Telescopes`, `§Signatures`, the bind_hyp-inline rule).
- `NEGATIVE_TYPES.md` — telescope/`tele_walk` design.
- (DELETED 2026-07-01, landed + superseded — recover via git: `TELESCOPE_FIXPOINT.md` recursion-as-cells; `POSITIVE_TYPES.md` inductives as coproduct-of-telescopes; `CELL_OPTICS.md` the optic factoring. Code comments still cite their § numbers as historical references.)
- `STRICTTYPE.md` — §7 respond self-typing (§7A `apply_policed`, §7B structured frames).
- `KERNEL_SELF_TYPING.md` — the sealing program (K3, §1 ceiling).
- `TYPE_NORMALIZATION.md` — canonical forms / `tree_lt` ordering.
- Tests that pin the facts in §2: `soundness.test.disp`, `metashape.test.disp`,
  `adversarial.test.disp`, `coh_gate_proto.test.disp`, `coh_why_proto.test.disp`, `kernel.test.disp`,
  `gap_severity.test.disp` (R0 — gap severity + the use-site-recheck defense),
  `coproduct_gate.test.disp` (R2/R3 — the generic `derive_gate`, gap closed end-to-end).
