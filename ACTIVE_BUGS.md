# Active bugs: subject-reduction gaps

This tracks known subject-reduction (preservation) gaps in the kernel and standard
library. A subject-reduction gap is a function accepted at type `A -> B` whose
application can reduce to a value outside `B`. In Disp these are not, so far, logical
inconsistencies: they are all defended by use-site re-checking. This file records what is
broken, why it stays contained, and how each item is closed.

Items 1 and 2 are now fixed (see "Closed" below). Items 3-5 remain open (3 and 5 are
recorded as design mechanisms rather than defects).

## The defense model (why these are gaps, not inconsistencies)

Typing is `param_apply T v = Ok true`, decided by running `T`'s recognizer on `v`'s
normal form. A subject-reduction gap makes a *stuck* (neutral) computation carry a type
its eventual reduct will not satisfy. Three facts keep that from becoming an
inconsistency:

1. **Use-site re-check.** Closed values are re-run through the target recognizer, so a
   forged result is rejected where it is actually used (`param_apply Nat <junk> = Ok false`).
2. **`False` is unconditionally empty.** Its recognizer is `Ok false` for every input
   (`base.disp`), so nothing inhabits `False` regardless of a gate hole.
3. **Neutrals are open.** A forged proof built over a hypothesis fails `is_closed`, so it
   can never be discharged.

Consistency rests on recognizer soundness plus facts 2 and 3. The responds and gates
below are about subject reduction and canonicity, not consistency.

## Status table

| # | Gap | Location | Status |
|---|-----|----------|--------|
| 1 | Ungated eliminating responds (Coproduct_ctx / Unit / Eq base / HBin) | `positive.disp`, `base.disp`, `hbin.disp` | Fixed (gated, or inert where no recursor ships) |
| 2 | Coherence gate trusted an unchecked `view`/`encode` iso | `positive.disp` | Fixed (`iso_faithful` guard) |
| 3 | Neutral application skips the domain check (result-shape-only) | `cells.disp:171` | Open (design mechanism) |
| 4 | `Tree` respond with a non-function motive reduces case-dependently | `universe.disp` (`tree_app_or_elim`) | Open (minor) |
| 5 | Intensional neutrality-branching: a body observes `is_neutral` and diverges on concrete values; pointwise/`CaseRelation` licenses are neutral-face statements | `engine.disp` (the `pair_fst` carve-out) | Open (design invariant; membership defended; behavioral licensing gap pinned in `probe_license_sr`) |

Probe files live in `lib/tests/probe_*_sr.test.disp`. The item-1 and item-2 probes are now
regression pins (they assert the gates close); the item-3 probe still asserts its
(documented) mechanism.

## Closed

### 1. Ungated eliminating responds (fixed)

Every eliminating respond now gates. The fix lives at the respond, which is the choke
point: both the library `elim` (`engine.disp`) and a direct neutral application
(`p { motive; cases }`) funnel through `hyp_reduce`, so gating the respond closes both
routes (gating `elim` alone would leave the direct-application route open).

- **Eq** (`base.disp`, `eq_respond`): checks `cases.base : motive x` before J lands
  `motive y`. The safe API (`eq_J`/`eq_subst`/`eq_sym`/`eq_cong`/`eq_trans`) always passes
  `base : motive x`, so it is unaffected; only a hand-rolled `elim` with a lying base is
  now rejected.
- **Unit** (`base.disp`, `coh_unit` via `gated_inductive_respond`): checks the `unit` case
  inhabits `motive unit_val`. Unit keeps its lean recognizer and stays non-`is_gated`.
- **HBin** (`hbin.disp`): now `inert_respond`. It ships no recursor, so this over-rejects
  any HBin-neutral elimination (SR-safe). Swap for a bespoke `coh` gate when an HBin
  recursor is wanted.
- **Coproduct_ctx** (mutual recursion, `positive.disp`): now
  `gated_inductive_respond (coh_check t)`, with the sort-context `ctx` stored in
  `behavioral_specs.ctx` so the gate types a `RecAt i` argument at `ctx i` and gives a
  self-sort argument an induction hypothesis. The gate machinery (`cell_argty`,
  `wrap_ihs`, via the new `is_rec_at_cell` in `cells.disp`) is now `rec_at`-aware.

Pins: `probe_eq_unit_sr.test.disp`, `probe_mutual_sr.test.disp` (both now assert rejection).

Residual: the Coproduct_ctx gate is **single-motive**. Full mutual induction needs a
motive family (one motive per sort), which the current single-`motive` frame cannot
express. A cross-sort `RecAt` child is treated as opaque data (no IH), so a genuine
mutual recursor that needs a cross-sort IH is over-rejected, never accepted. Motive
families (frame + `elim` + gate) are the remaining work.

### 2. The coherence gate trusted an unchecked iso (fixed)

`coh_check` (`positive.disp`) is now guarded by `iso_faithful`: for each variant it checks
`view (encode (inj tag placeholders)) = Ok (inj tag placeholders)`, anchored on the sound
`view` side (recognition uses `view` and is sound). A `Coproduct_viewed` with a lying
`encode` (probe_gate_sr's `BadNat`) is rejected both by the gate (`liar_bad`) and by the
universe (`typecheck Type BadNat`, because the R6 coh probe now fails on it).

Pin: `probe_gate_sr.test.disp` (now asserts rejection).

Residual: `iso_faithful` uses opaque placeholder payloads, so it assumes a payload-agnostic
iso (the kernel's `Nat`/`Bool`/`Ord`/`Tree` shape isos and every `iso_id` sum qualify). A
faithful-but-payload-inspecting `encode` would be wrongly rejected. Only the `view . encode`
direction is checked, which is what the gate's soundness needs; the `encode . view`
direction (relevant to `fmap`/`fold`) is not checked.

## Open

### 3. Neutral application skips the domain check

Checking `v : T` runs `T`'s recognizer on the final result only. The Pi respond
(`cells.disp:171`) extends a function-typed neutral at its codomain without checking that
the argument inhabits the domain. So argument-position type errors inside a body are not
caught.

```
// [probe_argcheck_sr]
g := {k} -> succ (k false)                                  // k : Nat -> Nat applied to false
param_apply (Arrow (Arrow Nat Nat) Nat) g = Ok true         // accepted
k0 := {n} -> n
typecheck Nat (g k0) = Ok false                             // g k0 reduces to `succ false`, not a Nat
```

This is the general mechanism behind the defended-SR class: the checker validates the
shape of a result, not the well-typedness of subexpressions feeding neutral eliminations.
It is a consequence of the "shape-only, O(1) conversion" design. Tightening it means
checking arguments at neutral applications (a bidirectional-checking change). Recorded as
the mechanism, not necessarily a defect to fix.

### 4. Tree respond with a non-function motive

`Tree`'s `{motive;cases}` elimination path is soundly gated (a well-typed lie routes to
`InvalidType`). But a malformed `motive := t` (a leaf, not `Tree -> Type`) makes the
elimination on a Tree-neutral reduce case-dependently instead of staying stuck at a clean
`InvalidType`. It is defended because `t` fails `Pi Tree ({_} -> Type)` at any checked
call site, so it is reachable only by hand-routing a malformed frame past the checked
elim surface. Minor and currently unpinned; a comment or pin that the gate should reject a
non-function motive cleanly would close the rough edge.

### 5. Intensional neutrality-branching (certificates are neutral-face statements)

`is_neutral` is an O(1) root-signature read (`pair_fst` against `hyp_sig`), and `pair_fst`
is a sanctioned reader on every value including hyps (`engine.disp`). So a body can
observe which face it is on. Certification walks the body at a minted hyp — the checker's
only spelling of "arbitrary input" IS the neutral face — so it only ever sees the
`is_neutral = true` branch:

```
// [probe_intensional_sr]
evil := {n} -> if (is_neutral n) then n else false
param_apply (Pi Nat ({_} -> Nat)) evil = Ok true    // certified at the hyp
evil 3 = false                                      // a stem, not a Nat
param_apply Nat (evil 3) = Ok false                 // defense: use-site re-check
```

A Pi certificate is therefore a NEUTRAL-FACE statement, not a semantic forall over
members. This is not `is_neutral`-specific (any root-signature comparison reconstructs
the bit) and it is not removable: polarized application — `elim`, `case_value`, the
H-rule, every licensed `.opt` fast path — dispatches on exactly this bit. Unsanctioning
root-sig reads on hyps would take the eliminator architecture with it. The reverse attack
(a concrete value FORGING the signature to look neutral) is separately pinned
(`soundness.test.disp`, `forged_stem`).

Defended for membership and consistency by the standard model: the junk fails the next
membership check (fact 1), and theorems cannot be faked, since the neutral branch must
genuinely inhabit the dependent codomain at the abstract point, where junk cannot help
(pinned). What the defense model does not cover is behavior. A type-preserving variant
(`shift := {n} -> if (is_neutral n) then n else (succ n)`) certifies at `Pi Nat (_ -> Nat)`,
and because a pointwise license obligation is itself checked at a minted hyp, the same branch
collapses the license: `NatFnRelation.rel id shift` normalizes its codomain to
`Eq Nat h h`, `{n} -> refl` inhabits it, and `license_guard` accepts a rebind
from `id` to `shift` even though the two differ at every concrete point. Both outputs are
Nats, so use-site re-checking catches nothing; the replacement silently changes what
downstream programs compute. The hole reappears inside `CaseRelation`'s concrete-face
family (`std/case.disp`): the at_cut obligations make the tag concrete, but arms,
payloads and the type argument remain hyps, and a candidate can probe any of them
(delegate while an arm is neutral, junk when it is concrete; or dispatch on the two
licensed instance types and junk on fresh coproducts). All of this is pinned in
`probe_license_sr.test.disp`.

The licenses actually in tree survive on grounds the license does not check: delegating
fast faces (`nat_rec_fast`, `case_fast`) are tree-identical to their spec at the hyp and
carry hand differential pins for the concrete face (`guard_opt`, `case_opt`); genuine
replacements (`guards.test`'s `ident`, `relation_tree_license`'s `fast`) prove their Pi by
induction, whose cases instantiate at constructor-rooted values where the face bit reads
false (an induction proof of `id ~ shift` is impossible: its zero case demands
`Eq Nat 0 1`). The attack's doors are top-level refl at a bare hyp, and reflection
through residual hyps of concrete-face obligations. The closing design (a strict
certification walker mode, delegation by construction, first-order certificates for
staged dispatchers, the PER lift for quotients) lives in OPTIMIZER.typ under
"Certification is not yet observer-restricted".

Forward constraint: the defense model rests on use-site re-checking, and `strip`/erasure
(TYPE_THEORY §10) deletes exactly those checks — so item 5 upgrades from defended gap to
real unsoundness under naive erasure. Erasure must demand two-face (canonicity-backed)
certificates for anything it strips.

## What is sound (verified, do not re-investigate)

- **The coherence gates.** After the fixes above, every eliminating respond gates:
  `Coproduct`/`Coproduct_p`/`Coproduct_viewed`/`Coproduct_ctx`/`Tree` via
  `gated_inductive_respond` + `coh_check` (now `iso_faithful`-guarded and `rec_at`-aware),
  Eq via `eq_respond`, Unit via `coh_unit`. Inert-respond types over-reject.
- **Effects.** `eff_coh` and the derived `handler_sig` reject motive-lies and enforce the
  row deeply; no ungated eliminating respond is reachable; no unsound reflection leak.
  `spec_respond` (`tele_spec.disp`) is a test-only spec twin, never routed by `hyp_reduce`.
- **Streams.** `stream_respond` assigns correct head/tail observation types; recognition
  rejects a non-`A` head.
- **Inert-respond types.** `Refinement`, `Intersection`, `String`, `False`, `Singleton`,
  `Neutral`, `InvalidType`, `HBin`, and the shallow `Eff` use `inert_respond` (every frame
  to `InvalidType`), so a hand-rolled eliminator on their neutrals cannot claim the motive.
  Over-rejecting, hence SR-safe.
- **Conversion layer.** Bracket abstraction's eta and K-composition rewrites only identify
  beta-eta-equal terms (`cir.ts:55-108`); there is one reducer, so compile-time and
  runtime cannot disagree on results (only on budget/termination); elaboration is
  deterministic; native `tree_eq` (`tree.ts:314-319`) is pure structural identity with no
  false positives. Function eta holds definitionally; eta-long checking of negative types
  is the walker. Record/Unit eta on neutrals and type-level funext are absent by design
  (a completeness limit, offloaded to explicit relations / cubical `Path`), not soundness gaps.

## Priority

1. Item 3 is the design mechanism; document rather than fix unless bidirectional argument
   checking is wanted.
2. Item 4 is a minor rough edge.
3. Item 5 is a design invariant (the price of polarized application), and it is already
   a blocker for proof-certified replacement, not only for §10 erasure: pointwise licenses are
   neutral-face statements, and the two-face discipline (`CaseRelation`) is spoofable
   through residual hyps (`probe_license_sr.test.disp`). Until certification gets the
   strict observer-restricted mode (OPTIMIZER.typ, "Certification is not yet
   observer-restricted"), `license_guard`/`CaseRelation` rebinds are trusted on
   their differential pins, not on their proofs. Erasure additionally must not strip
   use-site checks behind a neutral-face-only certificate.
4. Remaining from item 1: motive families for full mutual induction (the Coproduct_ctx
   gate is currently single-motive).
