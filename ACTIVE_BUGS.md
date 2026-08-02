# Active bugs

This file covers two checkers, which have different defense models and should not be read
as one list.

The first part tracks subject-reduction (preservation) gaps in the **live kernel**
(`lib/kernel/`) and standard library. A subject-reduction gap is a function accepted at
type `A -> B` whose application can reduce to a value outside `B`. In Disp these are not,
so far, logical inconsistencies: they are all defended by use-site re-checking. Items 1
and 2 are fixed (see "Closed"); items 3-5 remain open (3 and 5 are recorded as design
mechanisms rather than defects).

The second part, at the end, tracks the **standalone kernel** (`lib/standalone/`), whose
defense model is different: hypotheses are ordinary trees rather than sealed values, and
legitimacy is established by provenance (a session ledger plus history replay) instead of
by unforgeable representation. Its roadmap is `OEQ_PLAN.md`.

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
(`tele_walk`'s `SMint` branch on the respond face, `cells.disp`) extends a function-typed
neutral at its codomain without checking that the argument inhabits the domain. So
argument-position type errors inside a body are not caught.

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
observe which face it is on. Certification walks the body at a minted hyp, and the
checker's only spelling of "arbitrary input" IS the neutral face, so it only ever sees the
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
the bit) and it is not removable: polarized application (`elim`, `case_value`, the
H-rule, every licensed `.opt` fast path) dispatches on exactly this bit. Unsanctioning
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
through residual hyps of concrete-face obligations. The closing design lives in
OPTIMIZER.typ under "Certification is not yet observer-restricted"; its no-kernel-change
layers have landed in `relation.disp` and `std/deriv.disp`. The top-level-refl door is
closed for rebinds that use them: `two_face_guard` + `PositiveFnCoverage` bind guard-built
delegation glue and demand per-constructor obligations (the shift pair fails its zero
case), `license_guard_deriv` COMPUTES its verdict over first-order derivations (nothing
runs at a hyp), and `Quotient`/`q_lift` demand the respect witness at a quotient neutral's
elimination (pins: `guard_faces`, `deriv`, `quotient`, `setoid` test files). The
residual-hyp door (payload/arm-face probes) remains open pending the strict certification
walker mode, and plain pointwise `license_guard` remains fully spoofable.

Forward constraint: the defense model rests on use-site re-checking, and `strip`/erasure
(TYPE_THEORY §10) deletes exactly those checks, so item 5 upgrades from defended gap to
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
   neutral-face statements, and residual hyps of concrete-face obligations stay spoofable
   (`probe_license_sr.test.disp`, `guard_faces.test.disp`). The landed no-kernel-change
   layers (`two_face_guard`+`PositiveFnCoverage`, `license_guard_deriv`, `Quotient`) move
   their rebinds onto constructor obligations, computed derivation verdicts, and
   consumption-side respect; plain pointwise `license_guard` rebinds remain trusted on
   their differential pins until the strict observer-restricted walker mode lands
   (OPTIMIZER.typ, "Certification is not yet observer-restricted"). Erasure additionally
   must not strip use-site checks behind a neutral-face-only certificate.
4. Remaining from item 1: motive families for full mutual induction (the Coproduct_ctx
   gate is currently single-motive).

# Standalone kernel (`lib/standalone/`)

A different checker with a different defense model, so the live kernel's items above do
not transfer. Here hypotheses are ordinary trees that anything can construct, and
legitimacy comes from provenance: a session ledger of root hypotheses, plus replaying a
derived hypothesis's recorded history through the shared respond face. Forgery and replay
are refused because a fabricated hypothesis roots nowhere, and that is pinned on both
walking tiers.

Everything below was found by review on 2026-08-01, after hypotheses became lazy records
of their own observation history. Pins live in `lib/standalone/kernel.test.disp`.

| # | Gap | Severity |
|---|-----|----------|
| S1 | One stuck term has two tree representations, depending on whether it was built under the walk or raw | Highest: a future edit turns into a silent wrong answer |
| S2 | A function type over a quotient does not mean the function respects the quotient | Latent; the semantic gap step 3 exists to close |
| S3 | Type-indexed projection loops forever on a key that is not in the tuple | Hang rather than error |
| S4 | Eliminator arity and construction read declaration slots unguarded | Garbage arity, and one defense goes vacuous |
| S5 | A declared member list is trusted as exhaustive but only checked for soundness | False disequality and false universal |
| S6 | The abstract tier reads a hypothesis's stored type directly rather than deriving it | Over-rejection only |
| S7 | Tuple keys deduplicate, so a pair of the same type collapses to a scalar | Surprising, arguably intended |
| S8 | One guard-family hook is vestigial after the three-valued equality change | Confusing, no behavior change |

### S1. Two representations of the same stuck term

Applying or eliminating a hypothesis produces different trees on different paths. Under
the walk it goes through the guard tier's eager helper, which computes the result type
immediately and stores it; there are three such producers (that helper, its collecting
shim, and the projection branch of the respond face). Raw, it goes through the marker,
which records the observation and leaves the type to be derived.

Both are correct and both report the same type, but this kernel compares by tree identity
nearly everywhere: ledger membership, type comparison in the judge, and congruence. Nothing
fails today only because every exercised path compares terms built the same way; the
induction proof works because its motive and its obligation are both built raw. That is a
property of the current call graph, not an invariant.

Measured: a walked and a raw elimination of the same hypothesis are not tree-equal, and
neither are a walked and a raw application.

The fix is a simplification rather than an addition. With lazy hypotheses the eager helper
duplicates the marker, and the collecting shim duplicates the marker's collector. The only
thing the helper still adds is early refusal when the respond face rejects, which under a
lazy discipline belongs at the point the type is demanded. Do this before building further
on the equality layer.

### S2. A function type over a quotient does not enforce respect

Membership at `Fn Q B` checks that results inhabit `B`. It does not check that the function
maps quotient-equal inputs to equal outputs. Measured: a function that compares its argument
structurally certifies at a parity-quotient function type while returning different results
for two values the quotient identifies, and the quotient itself agrees they are equal.

Not exploitable into a false equation today. Proofs erase to a single canonical value and
the equality type still checks its endpoints, so transport produces that value but no type
accepts it wrongly. Nothing consumes function-type membership as respect evidence either.

This is the setoid-respect gap the archived investigation described, reproduced here by the
quotient feature. Closing it is exactly what `OEQ_PLAN.md` step 4 (the cross-related
relation, where membership is self-relatedness) and step 3 (the capability that makes the
guarantee real) are for.

### S3. Type-indexed projection loops forever on an unknown key

`at` walks its key list without a base case. When the list runs out, the comparison keeps
failing and the recursion continues on the exhausted pair, so projecting with a key that is
not in the tuple hangs the checker instead of erroring. Reached only by a caller that passes
a key outside the tuple's key list, which the type system does not currently prevent.

### S4. Unguarded declaration reads in the eliminator layer

`elim_arity` reads a type's gate and `elim_of` reads its recursor without checking either is
present. For a type declaring neither, the arity computation returns an arbitrary count, and
worse, the respond face's defense that an eliminator carries the type's own recursor compares
two absent slots and passes. The defense is therefore vacuous exactly for the types that
declare nothing. Not currently reachable through any pinned path, because eliminators are
built by `elim_of` over declared inductives.

### S5. Member lists are trusted as exhaustive

Two consumers treat a declared member list as the complete domain: the three-valued equality
rule decides a disequality from it, and the enumerating tiers quantify over it. The coherence
suite only checks soundness, that every declared member is recognized, never the converse. A
type shipping a partial member list therefore yields both a false disequality and a false
universal. This is the one trusted declaration with no probe, and the probe is not obviously
constructible, since exhaustiveness is the half a finite battery cannot witness.

### S6. The abstract tier reads a stored type instead of deriving it

Its value checker compares a hypothesis's stored type key directly rather than going through
the derived accessor. A derived hypothesis carries no such key, so a re-lifted one would
compare a sentinel against the target type and be rejected. Unreached today, because that
tier builds its own derived values with the key present, and nested application is pinned
green on both tiers. Over-rejection only, but the read should go through the accessor.

### S7. Tuple keys deduplicate

Key insertion drops duplicates, so a tuple of two identical types collapses to that type
alone: a two-element tuple silently becomes a scalar. Defensible, since the keys are meant to
be distinct types and brands exist for when they are not, but it is silent.

### S8. A vestigial hook

Both call sites of the guard family's equality hook already test the predicate the hook
re-tests, so its rejection branch is unreachable and the two tiers that appear to differ
there are identical. Introduced when equality became three-valued and the real decision moved
to a different hook. No behavior change, but a reader will assume it is load-bearing.

## Closed here (2026-08-01)

- **A record type did not check field presence.** Membership read each declared field with
  the plain accessor, which answers the leaf sentinel when the key is absent, and that
  sentinel inhabits most types, so the empty record inhabited every record type. Fixed by
  reading the cell and rejecting an absent field, which is what the live kernel's honest
  lookup does and why it exists.
- **Ex falso leaked at the raw tier.** An elimination with zero obligations must fire on
  arrival; the collector otherwise waited for an argument that never comes and returned a
  partially applied collector as if it were a value. The walk had this case, the marker did
  not.
