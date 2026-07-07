# Self-Checking the Kernel — from the coherence gate to the sealing modality

**Status: provenance + research-direction note (2026-06-24).** This records *why* a
POSITIVE_TYPES investigation bottomed out at metacircular self-verification, what is
established empirically vs. conjectural, the destination criterion, and the open theory.
It is the synthesis doc that ties together [`POSITIVE_TYPES.md`](POSITIVE_TYPES.md) §8,
[`STRICTTYPE.md`](STRICTTYPE.md), [`TYPE_NORMALIZATION.md`](TYPE_NORMALIZATION.md) §10e, and
the sealing-framework thread. **It is not yet a design that's ready to build** — Track B
below is research-grade.

## 0. The forced chain (where this came from)

This was not a tangent; each link is a standing commitment of the system:

1. **Positive types** ([`POSITIVE_TYPES.md`](POSITIVE_TYPES.md)) — inductives as a coproduct
   of telescopes, deriving recognizer + dispatcher + coherence gate generically. The
   recognizer/dispatcher half is a clean win (PoC'd, §1 below). The **coherence gate** (§8)
   must *self-type*.
2. **Why the gate must self-type** — the universe is **strict** (`Type := StrictType`,
   TYPE_NORMALIZATION §10e): a value is a type only if its `respond : RespondShape`. The gate
   lives in the respond. Forced.
3. **Why the gate can't be dropped/weakened** — the elaborator inserts **no** checks (the
   2026-06-05 untrusting; `compile.ts` 1103→879). So the gate is the *only* coherence check
   for recursive definitions (neutral targets): `f : {n}->P n := {n}-> nat_rec P b s n` is
   verified by `param_apply ({n}->P n) f`, which on the neutral `n` routes through the gate.
   No call-site check exists. Load-bearing. Forced.
4. **Self-typing the gate** = type-checking a *hypothetical* `cases` = **reflecting on a hyp**.
5. **It generalizes** — reflecting on hyps coherently *is* self-checking the kernel
   (`hyp_reduce`, `param_walker`, `bind_hyp`). The gate is just where the need first surfaces.
6. **Doing it coherently** needs a principled, parametricity-preserving meta-interface — the
   **sealing modality** (already in the project notes: `bind_hyp` = generative dynamic sealing,
   escape-check = DCC noninterference, "Type:Type = sealing preserves parametricity").

So: positive types → strict universe → untrusted elaborator → reflect-on-hyps →
self-check-the-kernel → sealing. The tunnel exits at a pre-identified research program.

## 1. What is established (empirical anchors — this session)

Scratch suites, all green (negative results asserted as `≠ Ok TT`, so green = finding holds):

- **`lib/tests/positive_proto.test.disp`** — the recognizer/dispatcher MERGE works on RAW
  `inj` values with ZERO kernel edits: a `pos_cell` op (defined in the test) lets `at` walk a
  raw pair; a fresh recursive `NatList` is recognized end-to-end via the generic
  nested-telescope recognizer; surface `match` (= the cut function `cut`) is the eliminator.
- **`lib/tests/coh_gate_proto.test.disp`** — the gate-as-telescope (`coh = param_apply
  (Telescope (case_tele motive)) cases`) **runs correctly at runtime, incl. a DEPENDENT motive**
  (`P n = Eq Nat n n`). But **self-typing FAILS** (`param_apply ProjectingRespondShape
  (resp unit_witness) = Ok FF`), same as hand-rolled `coh_nat`.
- **`lib/tests/coh_why_proto.test.disp`** — the wall, isolated: applying a CONCRETE recognizer
  to *any* hyp inside a respond body breaks self-typing, even when the result is DISCARDED
  (R4), even with an EMPTY telescope. The sanctioned `apply_policed` of a NEUTRAL self-types
  (R2). So the wall is **concrete-recognizer-on-hyp**, NOT the IH/`bind_hyp` (correcting
  POSITIVE_TYPES §8's hypothesis).

Sketch (not yet built): the `hyp_reduce` self-typing decomposition (§5).

## 2. The destination and its criterion

**Goal:** the kernel typechecks its own checking machinery (`hyp_reduce`, `bind_hyp`,
`param_walker`, the responds/gate) in the object language, with a *minimal, characterized*
trusted seal-set.

**Criterion (the design law):** *a construction is well-formed iff its seal-sites equal its
semantically-necessary reflection points.* Seals must appear exactly where typechecking
genuinely needs to reflect on a hypothesis, and **must not appear as an artifact of the
construction/encoding.** This lens re-reads §1:

- The telescope-gate failure was a **seal-by-construction**: phrasing coherence as "recognize
  `cases` against `CaseTele`" manufactured a concrete-recognizer-on-a-hyp, although the
  semantics ("does this hyp have this type?") needs only a `neutral_type` comparison — already
  seal-free. The seal came from *how we wrote it*, not from the question. ANTI-PATTERN.
- `recognize_policed` (the H-rule exposed = `neutral_type` + `tree_eq`) is the **seal-minimal**
  form of the same check. GOOD.
- `hyp_reduce`'s read of the stored type's respond is a **genuine** seal — the semantics must
  reflect there. The `MetaShape ->` split (§5) concentrates it into one primitive.

## 3. The seal — what it is, and the admissibility test

- **Type, not structure.** The barrier we must never cross is reading a hyp's *structure*
  (which value it is) — that's the parametricity foundation. Reading a hyp's *type* is safe:
  it's information we introduced. The impl already draws this line — `neutral_type` is
  sanctioned (`param_walker`), raw `type_meta`/`pair_snd` into a neutral is not.
- **Admissibility criterion for a seal/carve-out:** an operation may touch a neutral iff its
  result is **invariant across the values that neutral could stand for** — it factors through
  the *type* (`neutral_type`, recognize-against-a-type) or through *identity only* (the
  `occurs`/escape check = generativity). Structure-extraction fails this; everything the kernel
  legitimately needs passes it.
- **The modality marks meta-level code.** A type like `Hypothesis | Concrete` (definable: a
  recognizer on `is_neutral`) classifies object terms by meta-status. Used at the object level
  it breaks parametricity; it is only sound behind a modality `□`. Giving a kernel op a type
  that mentions it is *declaring the op meta-level*. The modality is what forces seals to the
  right places and keeps object programs seal-transparent by construction.

## 4. Open research questions (Track B — not yet concrete)

1. **Characterize the minimal seal-set.** Which reflections are semantically necessary across
   `hyp_reduce` / `bind_hyp` / `param_walker` / the responds? (`neutral_type`,
   recognize-against-a-type, `metashape_of : Type -> MetaShape` on a type-hyp, identity/occurs —
   is that all?)
2. **Formulate the sealing modality.** Its intro/elim rules, interaction with the universe, and
   how it forces seals at exactly the §2 sites and nowhere else.
3. **Soundness.** Whether the residual reflection (esp. `metashape_of` on a type-hyp, i.e.
   the universe reflecting on itself) preserves parametricity — the Type:Type / sealing
   conjecture, candidate proof via step-indexed logical relations (the sealing-framework recipe).
4. **Decompose the kernel** into a seal-free core + named seal-sites (the `hyp_reduce_open`
   split is one instance; need the analogue for `bind_hyp` and `param_walker`).

## 5. Concrete next probes (research, framed as falsifiable experiments)

- **`recognize_policed`** — add the H-rule as a sanctioned walker token (sibling of
  `apply_policed`, keyed on signature to prevent forgery); confirm the coherence gate self-types
  (expected: `Extend InvalidType` on abstract cases — graceful — real check on concrete).
- **`hyp_reduce_open` split** — rewrite the body's `match` on the respond's `Action` as a typed
  `elim` (so an abstract `Action` routes through `Action`'s respond instead of the raw cut
  mis-projecting), and pass the stored type's `MetaShape` as an explicit argument:
  `hyp_reduce_open : MetaShape -> NeutralMeta -> Frame -> Tree`. Then
  `hyp_reduce nm f = hyp_reduce_open (type_meta nm.stored_type) nm f` isolates the ONE
  reflective read. Run `param_apply (MetaShape -> NeutralMeta -> Frame -> Tree) hyp_reduce_open`
  — either it self-types (first typed kernel primitive) or it reveals the next seal-site.
- **Map the seal-set** empirically by attempting each kernel op and recording where reflection
  is forced.

## 6. The road to FULL kernel self-verification (2026-06-26)

Self-verification today is **partial and opt-in**: a binding `name : T := v` auto-verifies at load
(`verify mod = param_apply mod.typ mod.record`); a bare `name := v` does not. Coverage now:

| fragment | typed (verified) | untyped |
|---|---|---|
| `cut.disp`    | 14 | 12 |
| `engine.disp` |  1 | 25 |
| `types.disp`  | 46 | 47 |

The untyped bindings are the **meta-level machinery**: a naive `: T` annotation mints a hyp for the
argument and runs the body, which the walker rejects the instant the body triages/reflects on that hyp.
They form a difficulty ladder; "full self-verification" = climbing it until the only untyped bindings
left are the irreducible seals (the §2 criterion):

1. **Pure data helpers** — constructors, the list/§2.6-cut accessors, the new `encode_*` / `iso_id` /
   iso plumbing. They don't inspect their argument as a hyp, so they have honest types and just need
   annotating. A mechanical sweep that shrinks the gap to the genuinely-reflective ops. **Do this first.**
2. **Inductive eliminators** — `fmap` / `fold_value` / `mk` / the cell ops / the walker `at`. They
   case/triage a value, so over an inductive argument they reflect on a hyp. The TYPED recursors
   (`nat_rec` / `ord_rec` / `rec_value`) already show the path: route through `elim`, so a neutral
   target goes via `hyp_reduce` + the coherence gate. Re-expressing `fmap`/`fold_value` over that typed
   path — and giving the kernel its first honest **functor types** (`F : Type -> Type` with an `fmap`) —
   is this rung; it depends on rung 3.
3. **The responds + the coherence gate.** §7A/§7B (PoC'd — branch `investigate/stricttype-7a-7b`,
   150/150, NOT merged): projecting + negative-former responds inhabit per-former `RespondShape`s via
   `apply_policed` + PIN params. Merging it types most of the respond surface. The residual is the GATED
   respond (Nat/Bool) — the §1 "concrete-recognizer-on-hyp" wall — addressed by the `recognize_policed`
   probe (§5).
4. **The Σ-ops** (`hyp_reduce` / `bind_hyp` / `param_walker`) — the GENUINE seals. `hyp_reduce_open` (§5)
   isolates the one reflective read → the first typed kernel primitive; beyond it, the sealing modality
   (§4, research).

So the order is: **(1) annotate the data helpers [mechanical, now] → (2) type the eliminators via `elim`
[+ honest functor types] → (3) merge §7A/§7B + the gate probe [engineering + small research] →
(4) `hyp_reduce_open` + sealing [research].** Each rung shrinks the untyped count and sharpens what the
irreducible trusted seal-set actually is.

**Where the CELL_OPTICS optic/fmap landing fits.** It added rung-1/2 surface — the shape↔inj iso in the
`functor` field, `fmap`/`fold_value` (now generic over inj-tagged AND shape-encoded), and `do_check`,
all currently untyped. Typing `fmap`/`fold_value` (rung 2) is the natural occasion to give the kernel
honest functor types, connecting self-verification to the container / polynomial-functor account that
[`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) and [`CELL_OPTICS.md`](CELL_OPTICS.md) gesture at. (Caveat from
that landing: the richer kernel terms diverge under the *weak* non-canonical naive reducer — see the
skipped `eval-naive-elaborate` test — a reproducibility limit of that reducer, orthogonal to typing.)

## Provenance / Tracks

- **Track A (engineering) — LANDED 2026-06-24.** The recognizer + eliminator merge from
  POSITIVE_TYPES §1–§5a is in the kernel: `pos_cell` (positional `at` op) + `Coproduct` as THE
  generic positive former (multi-arg + recursive via `REC`, recognized by `at`). Single-arg sums
  (`Action`/`CheckerResult`/`Option`/`Result`) are the degenerate case — no regression; full
  `.disp` suite green. Nat/Bool/Ord keep their existing recognizers + (gated) responds. Deferred
  inside Track A: §5b views (to migrate the shape-encoded types). Evidence:
  `lib/tests/positive_proto.test.disp` (one-line `NatList`/`BinTree` specs).
- **Track B (research, open):** everything in §2–§5. The coherence gate, the TYPE_NORMALIZATION
  §10e recursive-gated residual, the `hyp_reduce` typing question, and the sealing-framework
  notes are **the same wall**. Self-checking the kernel coherently is the unifying goal.

## References

- [`POSITIVE_TYPES.md`](POSITIVE_TYPES.md) §8 (the coherence-gate bet — its "IH is the problem"
  hypothesis is corrected here: the wall is concrete-recognizer-on-hyp).
- [`STRICTTYPE.md`](STRICTTYPE.md), [`TYPE_NORMALIZATION.md`](TYPE_NORMALIZATION.md) §10e (the
  strict universe + the recursive-gated-Nat respond residual = the same wall).
- [`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) (the telescope / `at` walker the gate reuses).
- Kernel: `engine.disp` (`hyp_reduce`, `param_walker` carve-outs incl. `neutral_type`/§7A
  `apply_policed`, `recognizer_wrap` H-rule, `gated_inductive_respond`), `types.disp`
  (`coh_nat`, `ProjectingRespondShape`, the per-former RespondShapes), `cut.disp` (`type_meta`,
  the §2.6 cut).
- Scratch evidence (this session): `lib/tests/{positive_proto,coh_gate_proto,coh_why_proto}.test.disp`.
