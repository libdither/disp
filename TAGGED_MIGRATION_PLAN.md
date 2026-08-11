# Tagged / WaitForm migration plan

Goal: give the wait-form mechanism a first-class type in types.disp, so that
hypotheses, blessed eliminators, records, types, and invalids are typed
instances of one construction, and the record encoding (make_record, the cut)
can eventually be retired to one marker among many. This file is a living
plan: update stage statuses in place, delete the file when the work is done.

## Background: the untyped inventory

Every special structure in the standalone kernel is `wait marker payload`,
distinguished by signature (`sig_of marker`, checked with `has_sig`):

- hypotheses: `hyp_sig`, payload is a keyed record of stored type + history
  (readers: `neutral_type`, `neutral_history`)
- blessed eliminators: `elim_sig`, payload is `raw x ind x motive` pairs
  (readers: `elim_raw`, `elim_ind`, `elim_motive`)
- records: `cut_sig`, the make_record encoding (names header + list_const
  payloads; readers: `field_cell`, `field`, `field_put`, the `dot` hook)
- types: `make_type`'s tele/obs payload, recognized by `is_type`
- invalids: the error wrapper (`is_invalid`)
- faced records: face function + metadata rider

All readers are untyped today. `wait_payload := {w} => w.snd.snd`.

## Stage T1: descriptive formers (probe-sized, additive, zero drift)

Status: not started.

In types.disp:

    Tagged := {S, P} => ({ #recognize := {_self, v} => and (has_sig S v) (P (wait_payload v)) })
    WaitForm := {S, P, F} => ({ #recognize := {self, v} => and (Tagged S P v) (F v) })

WaitForm's F is the behavioral half: the function type the wait-form
implements, conjoined as a predicate (types are predicates, so intersection
is `and`).

Descriptive instances with membership pins against real specimens, riding the
existing forgery negatives:

    Hyp := Tagged hyp_sig AnyTree              // payload sharpened in T3
    BlessedElim := Tagged elim_sig AnyTree
    test Hyp (make_hyp Nat t) = true
    test BlessedElim (elim_of Nat (const_fn Nat)) = true
    // negatives: forge_hyp-style specimens, unblessed_rec

Gate: barrel + suite green with no tree changes anywhere (pure additions).

Open questions to resolve during the probe:
- which sigs the barrel exports (`cut_sig`, the type marker's sig)
- whether `wait` itself is reachable for library-tier construction pins

## Stage T2: observation rows (checker-facing)

Status: not started. Shared prerequisite with the Cases-record design.

Give `Tagged S P` declared rows so hypotheses of tagged types are usable under
Guard: payload projection as a declared observation (`Prj wait_payload` at P;
Sigma's `Prj pair_fst` is the precedent) plus a self observation for the sig
check. Records-as-hypotheses (per-field getter rows) are the same idea
specialized, which is why Cases and T2 should land together.

Gate: conduct pins. A checked function over Hyp/BlessedElim arguments that
projects payloads legally; a negative showing undeclared observations still
refuse; forgery pins stay green with sig checks running as observations.

Constraints to respect: the eager-refusal invariant (an undeclared observation
is an error, not a stuck term) and the S9 discipline (rows instantiate at
concrete constructors; codomains at abstract points lie).

## Stage T3: sharpen payload types, retype the readers

Status: not started. Rides behind T1/T2.

Replace the AnyTree payloads with real shapes: the eliminator payload as a
Sigma chain (raw : AnyTree x ind : ShallowType x motive), the hypothesis
payload as a keyed-record type (the Tuple/at machinery), records as
entries-shaped. Then the readers get spelled annotations (elim_ind :
BlessedElim -> ShallowType flavored, at whatever tier each supports). The
blessing metadata becomes checked structure instead of convention.

Gate: barrel re-verifies every retyped reader; no encoding changes, so pins
move only where annotations were added.

## Stage T4: retire the record encoding (big, gated, scope-local)

Status: not started. Do after T2.

Design: a `record_marker` whose application behavior is assoc-lookup dispatch;
records become `wait record_marker [pair "a" 1, ...]`. Field access stays
application-shaped, so the walker-mediation soundness anchor survives. The
payload becomes a plain pair spine (measured cheaper than the cut-structure
walk). prod/match dispatch becomes the marker's behavior. Host coupling
(recordFieldsFromTree, cut_sig/type_meta reads) simplifies to one sig plus a
spine walk.

The de-risking insight: record literals compile through the elab_settings
vocabulary, so the encoding is swappable per scope. A probe scope re-points
the record entries to the new builders while legacy and the standalone barrel
keep make_record until ready.

Order:
1. Build marker + readers in-language beside the old ones.
2. Probe scope with re-pointed vocabulary: semantic pins plus interleaved
   perf measurement (single cold-barrel timings on the dev box drift with
   background load; only interleaved ms-resolution A/B runs are trustworthy).
3. Flip the standalone barrel: migrate the ~59 reader sites and the pins,
   regenerate the snapshot and the playground record explorer.
4. make_record retires to legacy-only vocabulary.

Kill criteria, stated up front: if the assoc-dispatch marker measures worse
than the cut on the hot reads (field Guard "apply", recognizer-heavy paths),
T4 stops at the probe and the cut stays, with T1-T3's types describing it.
Precedent: the blessing-payload record refactor was tried and rejected on a
consistent +330ms over a ~3.5s cold barrel (2026-08-07).

## What never migrates

kernel.disp's own marker definitions and internal wait manipulation (the
substrate implementing what Tagged describes) and the raw prelude. The
primitive tier cannot use givens in values, settings-scoped sugar, or
accessors; probes established each of these boundaries.

## Sequencing

T1 any time (an afternoon, mostly pins). T2 when the Cases work starts. T3
behind T2. T4 after T2, gated on its measurement.
