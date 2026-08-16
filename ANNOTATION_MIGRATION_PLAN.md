# Annotation migration plan

Goal: total annotation of the standalone kernel. Every definition in
lib/standalone/ either carries a checked annotation at the strongest tier it
supports, or appears in the "never annotated" list with its stated reason.
This file is a living plan: update stage statuses in place, delete the file
when the work is done.

## Ground truth: tiers and encodings

Annotation tiers, strongest first:

- Guard tier (`Pi Guard`, `Fn`, `->`): the walker mints hypotheses and checks
  the body parametrically.
- Sampled tier (`Pi (Sampled [..])`): spot-checked at the listed samples only.
- Membership (`: ShallowType`, `: Table`, `: List AnyTree`): classifies the
  value's shape, says nothing about behavior.
- None.

Counts (2026-08-11, kernel.disp + types.disp, 203 defs): 27 Guard, 27
Sampled, 28 membership-only, 120 unannotated.

Three encodings underlie the special structures; they are not one mechanism:

- wait forms (`wait marker payload`: sig at `.fst`, a stem; payload at
  `.snd.snd`): hypotheses (hyp_sig), blessed eliminators (elim_sig), their
  collectors (elim_stuck_sig, open_sig), invalids (invalid_sig), suspended
  tree_eq partials (tree_eq_sig), and Sum's arm collector (per-instance
  marker, never sig_of'd).
- tag faces (`tag meta behavior`, the S/K transparency construction; meta at
  `.snd.snd`): types and anything `faced`. The head embeds the per-type
  behavior, so there is no constant sig a recognizer could key on.
- bare pairs: records (make_record = sorted names list x payload cells,
  recognized by RawRecord's length check, no marker) and inj variants
  (`pair tag payload`: frames, Pure/Op, sum values), which has_sig also
  matches by accident of shape.

Wait forms and tag faces share one structure (probe-verified 2026-08-11): a
suspended S-redex `fork(stem f, K x)` with the stored tree at `.snd.snd`,
where `K x` = `fork(leaf, x)` = the same box list_const uses for record
payload cells. Applied to an argument, the S rule fires `f c x`-shaped
computation: wait's stem child is a fixed function of the marker (so the head
is recognizable, that is the sig) and delivers the payload to it; tag's stem
child embeds the per-instance behavior and discards the meta. So a tag face
is a wait form whose head traded recognizability for behavior; one shape,
identity stored on opposite sides. Plain pairs (records, inj variants) are
forks with no stem head, which is what the unified shape check keys on.

Two facts shape everything below. First, the forgery defense is the walker
and its ledger (hyp_ok, the elim_raw-vs-T.elim comparison), never the sig:
sig-valid forgeries exist by construction (forge_hyp, unblessed_rec), so
sig-keyed types classify shape, not trust. Second, the checked fragment
refuses inspectors by design (raw triage / tree_eq / payload reads on a
hypothesis are errors), and that refusal is the sealed-core soundness. The
machinery that is made of those reads cannot be Guard-typed without new
mechanism (stages A8/A9); pretending otherwise weakens the check.

## Stage A1: descriptive formers (probe-sized, additive)

Status: LANDED (c5df5c5d, 2026-08-11). Sus/Tagged/Faced + the seven
instances (Hyp, ElimHead, Invalid, OpenChain, StuckElim, TeqPartial,
TypeFace) with 26 membership pins, all first-run green; liveness verified by
deliberate pin flip. BlessedElim was renamed ElimHead: shape-not-trust means
unblessed sig-valid specimens are members, so the name must not claim
blessing.

In types.disp, the unified suspended-form type plus two refinements keyed on
where the identity lives:

    Sus := Pred ({v} => ...)          // fork, stem head, K-boxed snd
    Tagged := {S, P} => ({ #recognize := {_self, v} =>
      if (Sus v) { if (has_sig S v) { P (wait_payload v) } else { false } }
      else { false } })
    Faced := {name, P} => ({ #recognize := {_self, v} =>
      if (Sus v) { ...meta fork, fst = name, P on snd... } else { false } })

(if/else, not `and`: `and` is eager and would run P on junk.) Sus is the
common super-type both refine; the payload accessor `.snd.snd` is shared.
The Sus guard is also the hardening that kills the has_sig collision family:
inj pairs (Acc/Elim frames, Pure/Op, sum values) have no stem head and no
K-boxed snd, so `Tagged "Acc" P` is false by shape instead of by luck of the
domain. Records already have their former: RawRecord.

Descriptive instances with membership pins: Hyp, BlessedElim, Invalid,
OpenChain, StuckElim, TeqPartial over Tagged; ShallowType is already the
Faced "recognize" case. Payloads start at AnyTree, sharpened in A4.

Pin design, corrected from the first draft of this plan: the forgery
specimens are positives, not negatives. `Hyp (forge_hyp Nat Nat)` is true by
construction and the pin should say so, with a comment that blessing is the
walker's judgment, not a shape. Real negatives: junk trees, cross-family
specimens (an inj "Acc" frame is not a Hyp even though has_sig "Acc" holds;
a record is not a type; a type is not a wait form), and the sig/payload depth
confusions (a bare pair whose fst happens to equal a sig).

Gate: barrel + suite green with no tree changes anywhere (pure additions).

## Stage A2: the mechanical surface sweep (biggest raw count win)

Status: first slice LANDED (4ce12724, 2026-08-11): 19 annotations in
types.disp, all probe-verified at Guard first, spelled Pi-Guard-literal so
the Meta differential does not re-tier them (upgrade individual ones to Fn
after Meta probes). Landed: see, self_is, done, Not, Refine, Quotient,
Named, Point, list_rows, tree_rows, bal_rows, ListR, TreeOf, plus
membership for node_like, False, near, MixedTree, Balanced, LenXs, xy,
list_samples. The eff_bind/see block moved below Eff for def-order
(tree-neutral). Annotation liveness verified by deliberate wrong-type flip.

Second slice LANDED (89ef76c8, 2026-08-12): the kernel.disp maximization
pass. Every kernel def now carries the strongest annotation reachable with
existing machinery (~35 additions/upgrades; see the commit). Notable: inj
and list_const carry exact Point-codomain specs (unique inhabitant up to
extensionality), the formers' codomains are TypeFace, Sum's is Refine
ShallowType Coherent, and the ElimHead readers landed at Guard tier via the
rows types and eight new given fills.

Third slice LANDED (980fb937, 2026-08-14): GoodGate and Coherent moved
above Nat and carry Sampled annotations over kernel-provided samples; Nat
and Tree upgraded from membership to Refine ShallowType Coherent (facts the
pins already proved, now claimed by the annotations). Probed refusal
recorded: the List/Eff codomains cannot carry the Coherent refinement,
because Coherent runs its battery through raw membership, which is junk at
an abstract element type.

Remaining candidates: the refused
set with diagnosed causes: all_rows/vec_rows-class (builder branches on its
parameter at construction, unlike list_rows whose branching hides in the
stored continuation; rewritable via list_rec/nat_rec = A5 candidates),
eff_bind (has_sig inspection on the subject), height (raw fix + triage, A5
candidate via tree_rec), key_insert (tree_lt, no carve-out), canon (dot on
an abstract type), eq_sym (Eq at abstract endpoints, the known eager-baking
wall), mixed_rows (its reject arm fires under the Eff recognizer's leaf
probe, so the row-program type is honestly not its codomain). Refused defs
cap at Sampled until their named blocker moves.

Meta parity: LANDED 57bead57 (2026-08-13). The probe had localized the
entire embedded-mark refusal family to one missing simulator rule
(pair_fst/pair_snd on a composite value poisoned; only bare mints were
special-cased), and the fix held: a Fork arm in the projection case (a
Fork value answers its own component, raw reduction verbatim, no
readback) unlocked the whole family. Fourteen annotations flipped to
arrows and now ride the Meta differential: Not, Refine, Quotient, Named,
Point, List, Eff, Tagged, Faced, TaggedRows, ListR, list_rows, tree_rows,
bal_rows. GuardNatToNat retired to test scaffolding (6c2f3162); the
literal opt-out list is now two entries, both diagnosed (2026-08-14):

- The blessed-eliminator trio: FIXED (6aadc810). The composite-head rule
  landed (a Fork value carrying elim_sig applied to a mint reads back and
  routes into Chain collection, gated by the rooting audit); all three
  certify all-Meta and ride arrows under the differential.
- TreeOf, the one literal left: an OPEN simulator pathology, not a design
  boundary. Bisection ledger (2026-08-14): building the mint-bearing
  branching program is instant (pair P 5 green), its RowProg membership
  is green (judged via readback + raw run), Tele over the LINEAR
  list_rows program is green, Tele over a CONCRETE branching program
  (bal_rows) is green; but CONSUMING the branching+mint program's value
  in simulation diverges, all the way down to direct
  `pair_snd (pair_snd P)` (so the composite-projection rule is not the
  culprit; it never gets a normal Fork value to answer on). Prime
  suspect: fix-unfolding during decomposition (the stored continuation
  embeds wait-omega self references that unfold when the surrounding
  expression is interpretively decomposed). Black-box probing is
  exhausted; next tool is host-side step/trace profiling of the
  simulator (the DISP_DEBUG_BATCH-style diagnostic the review notes
  wanted rebuilt).

Known residue beyond those: the multishot/Sigma-fan readback boundary
(pinned) and the shared dependent-application frontier. The deep
alternative stays recorded: converge Meta onto Guard's in-band provenance
design.

S9 differential (a479c478, 2026-08-14): with_probes conjoins the probe
battery only at neutral_free types now. Unguarded, a Fresh row at an
abstract type binder filtered probes through a mint-embedding type by raw
application; K-saturation junk read as present, bogus probes were kept, and
their obligations refused honest polymorphic bodies (cons, bool_case,
list_map, list_filter, and the blessed-eliminator trio all probed red,
all green with the guard). With the surface TwoFace-clean, kernel.test
carries `use "types.disp" { DefaultWalker := TwoFace }` beside the Meta
line: the whole arrow surface re-verifies under the concrete face, and the
lying-codomain pins stay refused (lies live at concrete domains, which keep
their battery). Flipping the default was measured and rejected: ~12x on
every load for a lie detector the differential already runs. S9 stays fully
open only at the barrier (raw type formation); the differential is the
probe-strength close.

Annotate everything data-shaped that today has nothing, at the strongest tier
that verifies:

- types.disp row builders (list_rows, vec_rows, record_rows, all_rows,
  unique_rows, tree_rows, mixed_rows, perfect_rows, bal_rows, len_xs_rows):
  functions from data to row programs; the result type exists
  (`Eff Row AnyTree AnyTree` recognizes teles, pinned).
- types.disp helpers: eff_bind, see, self_is, key_insert, key_sort, Tuple,
  at, canon, height, splits, bijection, eq_subst/sym/trans/cong, Not,
  Refine/Quotient/Named/Point as formers.
- kernel.disp combinators and injectors: inj, idx, const_fn, id_fn, stem_fn,
  fork_fn, perform, handle, Pred, PiCode, SigmaCode, IsectCode, record_mem,
  Rec, Coproduct.

Some of these will cap at Sampled (inspectors); record the tier reached next
to each. Type values that are membership-only today (`: ShallowType`) should
be probed for refinement annotations (Refine ShallowType Coherent-shaped);
def-order will block some, note which.

Gate: annotation batch green; per-def pins for anything that surprised.

## Stage A3: observation rows for tagged hypotheses (checker-facing)

Status: probed 2026-08-11, split into A3a (near-free, ready) and A3b (the
real mechanism). Probe findings, all first-run:

A3a: the wait form IS a nested Sigma (`fork(stem sig, fork(leaf, payload))`),
so `Sigma Guard (Point S) ({_} => Sigma Guard (Point t) ({_} => P))` gives
Tagged types working rows with ZERO walker changes: a checked function over
an abstract tagged argument projects sig and payload legally (`h.fst`,
`h.snd.snd` certify), undeclared observations refuse (application, too-deep
projection both pinned false), and the kernel's existing readers certify at
these types AS-IS (wait_payload, elim_raw, elim_ind : ElimHead -> TypeFace,
elim_motive — no respelling; composite accessor bodies decompose into the
mediated pair projections). To land: decide whether Tagged's instances gain
a tele beside #recognize or ship as parallel rows-variants (a tele changes
judge routing for these types everywhere), add S9/TwoFace pins for the new
rows, and pins scoping sig-projection legality to Tagged domains.

Control-marker caveat, completed by the maximization pass (2026-08-12): the
checker has exactly three control markers its judge claims before rows can
run, and all three refuse rows spellings categorically. Hyp marks are its
mints (admit + the is_neutral branch), invalid marks are its error
propagation (the is_invalid branch refuses them as values), open chains are
its pending completions (the is_open_chain branch routes them to complete).
Hyp, Invalid, OpenChain keep recognize spellings; ElimHead, StuckElim,
TeqPartial are rows-based (landed 89ef76c8) with conduct pins.

Payload-sharpening boundary, measured 2026-08-14 (9c1dcea1): rows
membership refuses any payload that embeds a live mint, because the judge
claims every neutral at its recorded type (a hyp of Nat inhabits no
AnyTree or Hyp slot). Real stuck collectors always carry a hypothesis
scrutinee, so StuckElim/TeqPartial describe the neutral-free shape only
(pinned at StuckElim); sharpening their payloads past the head slot is
dead until the judge grows a subsumption or shape route for neutrals.
RawRecordRows landed beside the rows family (9c1dcea1): the record pair
shape as a dependent row program (names observed, payload spine at the
observed length), fst-conduct certified, dependent side refusing; its
conduct pins are Pi-Guard-literal to keep the Meta differential off
branching-program consumption at a mint (the TreeOf pathology).

A3b, the genuinely new mechanism, now precisely scoped: walker-side
mediation for NAME-KEYED reads only (field/field_cell on abstract records),
needed by records-as-hypotheses (Cases, still no doc), the hypothesis
payload keyed record, and case_of/Tele table arguments. The walk's accessor
whitelist is literally pair_fst/pair_snd; respond_face already matches
arbitrary accessor trees, so only the walk-side conversion is missing.
Design options: rows scan at application time (hot-path perf risk),
a precompiled accessor slot on type records, or shape-recognition of
`field _ name` partials. Meta needs parity with whichever lands.

First form LANDED for TYPES (b7ed2239, 2026-08-15), Guard-only: the ask
head. Seven property formers (RecognizeP/GateP/ElimP/MembersP/NormP/EndsP/
UnderP, constructor-only kernel values), Cell (the record Option as a
declared sum), and one uniform op: ask dispatches to the named slot on
concrete types (agreement pinned) and is a licensed observation on type
hypotheses (walk grows the tree_eq-style partial pair; respond_face grows
an Ask branch answering pair [] (Cell (P self)) for ShallowType-typed
parents and licensed formers). The witness is a mint, so certificates hold
for every dictionary; unlicensed formers, non-type subjects, and
wrong-property claims refuse (pinned). Boundaries, both pinned: Meta has
no Ask rule (conduct pins are Pi-Guard-literal); and CONSUMING a witness
(case/projection/application) requires constructing the Sum-built answer
type inside the walked body, where scrutinizing formers refuse over an
embedded mint -- the unlock is walked type formation or a dedicated case
head. Kernel-local signatures keep GateP/MembersP AnyTree-weak (List and
TypeFace live above the kernel); identical weak signatures collide as
trees in the dispatch, so every former must be shaped distinctly. General
name-keyed reads on RECORD hypotheses remain open as scoped above.

Constraints to respect: eager-refusal invariant (undeclared observation is an
error, not a stuck term) and the S9 discipline (rows instantiate at concrete
constructors; codomains at abstract points lie).

Gate: conduct pins. A checked function over Hyp/BlessedElim arguments that
projects payloads legally; a negative showing undeclared observations still
refuse; the sig-mining pin family extended, not just kept green.

## Stage A4: sharpen payloads, retype the readers

Status: eliminator half unblocked by the A3a probe (2026-08-11): the
sharpened payload `Sigma AnyTree ({_} => Sigma TypeFace ({_} => AnyTree))`
recognizes real blessed eliminators, refuses hyps, and the readers certify
against it unchanged. Remaining for that half: def-order plumbing (kernel
annotations naming ElimHead/TypeFace need those as declared givens with
barrel fills, since the types live below the kernel; the alternative is
annotated re-exports in types.disp, but re-annotating an exported name is a
rebind and rides the guard mechanism). The hypothesis-payload half (keyed
record, history as inductive data, neutral_history/neutral_type readers)
stays Sampled until A3b lands name-keyed reads.

Known tension, decide before starting: Guard-tier reader types via
nested-Sigma spelling bake the encoding into the types, which fights A6's
encoding swap. If A6 is likely to proceed, keep reader types
encoding-agnostic (Sampled) until it lands.

Note the readers live in kernel.disp; retyping them there does not violate
"never annotated" below, which covers the markers' construction internals,
not descriptions of their output.

Gate: barrel re-verifies every retyped reader; no encoding changes, so pins
move only where annotations were added.

## Stage A5: eliminator rewrites of raw fix helpers

Status: first rewrite LANDED (0ac738f4, 2026-08-14): list_assoc folds with
the kernel list_rec (tree-identical to List.elim, so the open chain blesses
it) and branches through bool_case over three-valued tree_eq; annotation
upgraded Sampled -> Guard tier at the dependent-pair element domain
List (Sigma Guard AnyTree AnyTree); liveness probed (wrong bodies at the
same type refuse), junk-tailed spines now fall to the base case, cold
barrel timing unchanged. Walls hit probing the rest: an accumulator/CPS
respelling (list_apply-class) puts a function-typed carrier at an AnyTree
codomain, which refuses by nominal type identity (no subsumption; analyzed,
not landed); idx-class projection into an untyped spine has no rows at
abstract points. Remaining candidates below.

Second wave (f80750a1, 2026-08-15): vec_rows rides nat_rec at the
function-space motive (accessor to row program) and upgrades from nothing
to the full arrow annotation -- tail-position IH results are typed
neutrals the Eff check knot judges through the tier. perfect_rows stays
raw with its blocker named: its left-child program is sequenced
mid-program, and eff_bind inspects its subject, so a neutral IH refuses
there (the eff_bind class); only tail recursion certifies. height stays
raw too: its fork arm compares IHs with raw nat_le, which needs a blessed
comparison before the tree_rec spelling certifies.

The one existing typed door for recursion is open-chain blessing: a fix body
tree-identical to some type's own elim slot certifies. Rewrite the
single-scrutinee raw loops onto that door where the value change is
tolerable: list_zip and list_apply (dual/odd recursion, need restructuring),
list_assoc, key_insert/key_sort, at, eq_pairs/eq_reach fuel loops. Each
rewrite changes value trees (pin churn) and rides the hot path, so:
interleaved ms-resolution A/B only, and the blessing-payload precedent
(+330ms rejected 2026-08-07) is the kill criterion per function.

Open question flagged by the prelude: tree_lt has no carve-out the way
tree_eq does; comparator-using loops (key_insert, make_record's sort) may
need one or stay Sampled.

## Stage A6: the record encoding decision (big, gated)

Status: not started. Do after A3. Premises corrected from the first draft:

- Standalone field access today is function-shaped (`dot` -> field ->
  field_cell over a bare pair), not application-shaped; the cut encoding and
  its host coupling (recordFieldsFromTree, cut_sig) are the legacy tier and
  already dead code for standalone. A record_marker would move standalone
  records INTO wait form, adding one indirection on every hot field read.
  The gain is uniformity: records become Tagged-able, one recognizer family,
  and application-shaped access restores walker mediation for free.
- Scope is bigger than record literals: type literals compile through
  make_record (every `#recognize` record), and hypothesis payloads are
  make_record records. A probe scope via elab_settings re-pointing covers
  literals; the machinery's own records need explicit migration or exemption.
- Sort canonicalization (tree_lt insertion, `{b,a} = {a,b}` pinned) must
  survive whatever the new spine is.

Order: build marker + readers beside the old ones; probe scope with
re-pointed vocabulary + interleaved perf; flip the barrel (~60 field-family
sites plus ~100 dot-sugar sites); retire make_record to legacy-only.

Kill criteria: if the marker measures worse on hot reads, A6 stops at the
probe, records stay bare pairs, and their types stay RawRecord/NamedRec
descriptions of the pair encoding. That outcome is acceptable: A1-A5 do not
depend on A6.

## Stage A7: type the equation ledger

Status: LANDED (2026-08-15). Eq gained an `under := A` carrier slot;
eq_pairs harvests pair (carrier, ends) from the Eq-typed mints in scope
(no carrier = not harvested); the new eq_usable G E := tree_eq E G or
is_leaf E.norm decides whether an equation may serve a goal (same carrier,
or identity-fine: no norm slot means its equality is tree identity, which
implies every coarser one). eq_congr takes the goal and consults it ONCE,
at top-level reachability; every recursive position (spine decomposition,
derived-mark congruence, frames) runs at the finest grade, because
congruence through constructors or applications under a quotient goal
requires respect evidence no structural rule can supply. eq_holds takes
the goal; the judge's ends route passes T.under, Meta's vcheck likewise;
the Teq audit discharge and the open-comparison hook pass the finest
sentinel t (they answer tree-identity questions). Consequences, all
probed: the non-quotient world is behavior-identical (identity-carrier
equations are usable at every grade: direct, spine, function congruence,
and they discharge coarser goals soundly); a Parity assumption no longer
discharges a Nat goal or a raw tree_eq branch (the rel_probe reuse
unsoundness is closed and its pins flipped, rel_probe gaining the codomain
parameter); the parity normalizer's respect flips from the untyped
ledger's lucky true to an honest false, which is exactly the respect
evidence step 4's licenses will carry. Cold barrel timing unchanged.
rel_probe stays test-side pending the step-4 licensing design; the next
equality rungs this unblocks are the respect judgment as kernel surface
and a ledger route for type-level conversion.

Transport consumer LANDED (2933e84f, 2026-08-15): when tree identity of a
neutral's derived type against the expected type fails, Guard's judge asks
eq_holds at the finest grade before refusing. An assumed Eq ShallowType A B
converts A-hypotheses to B, including through constructor contexts by
spine decomposition (pinned: direct, symmetric, congruence through a Pi
context, refusal without the proof, and quotient-carried equations refused
for conversion -- the A7 grading protecting exactly this). Free on the
common path: two neutral-free distinct types answer false in teq_decide
before any ledger walk. Meta's judge keeps the nominal comparison; the
conversion pins are Pi-Guard-literal.

Tooling (ba6bedc0, 2026-08-15): a red module verification batch now names
its failing entries (the driver re-verifies each typed export as its own
single-entry batch on the failure path only); scripts/annotation_census.py
buckets every definition by annotation tier for re-audits.

## Stage A8: op-indexed effects, then annotate the interpreter

Status: not started. Investigation first.

`check` (the row-program interpreter) is resume-shaped, so an Eff-eliminator
rewrite is structurally plausible, but its five ops answer different types
(Mint a value, Judge a Bool, Read a value) and Eff has one answer parameter.
Investigate an op-indexed effect former (answer type as a function of the
op), which would let check and eff_bind carry real types and is the natural
meeting point with the Judge-elaboration idea (Judge is a scoped op; the
literature says elaborate it statically into algebraic ops). If the former
lands, Tele's handler tables get per-slot contracts instead of the
shape-only `: Table`.

## Stage A9: the machinery core (research stage, speculation only)

Status: not started. Do not schedule; investigate.

What remains after A1-A8 is the checker's own core: the walk, judge, hyp_ok,
respond_face_at/neutral_type, Meta's simulator, Sum's internals, teq_decide.
These are inspector code top to bottom, and the parametric fragment refuses
inspectors as its soundness mechanism, so Guard-typing them is not an effort
problem; it needs one of the following to pan out:

- Types-as-effects: recognizers and judges become effect programs the way
  teles already did (rows = Eff Row programs was the same move one level
  down). Then annotating the machinery means annotating handlers of a
  declared op vocabulary rather than raw tree readers, and the inspector
  reads hide behind ops the checker can reason about. A8's op-indexed former
  is the entry ramp; the open question is whether respond_face's
  tier-agnostic position survives being a program under interpretation.
- Hypothesis-tracing in-context: the meta-confusion (a minted hypothesis of
  type Hyp is itself neutral, so a checked judge body cannot distinguish
  "neutral because I am being checked" from "denotes a hypothesis") wants a
  provenance mechanism: mark which neutrals belong to the ongoing check
  (ledger membership as an in-language predicate?) so inspector ops on
  denoted-data neutrals become declarable observations while ops on the
  checker's own mints stay refused. This is the same provenance-not-
  representation move Meta already made once.
- What the legacy kernel does for self-typing: lib/kernel types its own
  fragments via given-fills, verify.disp, and checker_sig/license
  machinery. Survey which of those moves (licensed fast faces, per-fragment
  self-typed givens) translate to standalone before inventing new ones.
- The totality story: the prelude names wf_fix/Total as the missing honest
  type for fix itself. Any Guard-typing of the core's loops that are not
  eliminator-shaped bottoms out here.

Honest ceiling until one of these lands: Sampled annotations plus behavioral
certification (GoodGate, Coherent, the differential tables) for ~30 core
defs. That ceiling should be recorded per-def in the code, not treated as a
temporary gap.

## Never annotated (by principle, with reasons)

- make_hyp / make_mark: a checked annotation would certify hypothesis
  forgery; the ledger refuses it and the refusal is pinned.
- Marker construction internals (hyp_marker, elim_marker, elim_stuck,
  open_chain, invalid_marker bodies): the substrate implementing what the
  descriptive types describe; their spec is the walker's audit.
- Protocol vocabulary (let, given, default_guard, check_module,
  elab_settings): driver registration points with fixed-arity contracts.
- The raw prelude: the primitive tier cannot use givens in values,
  settings-scoped sugar, or accessors.

## Sequencing

A1 and A2 any time, independently (A2 is the big count win and is mostly an
afternoon of mechanical annotation plus surprises). A3 when the accessor-
mediation mechanism is designed (write the Cases doc first). A4 behind A3,
with the A6 tension decided up front. A5 per-function, perf-gated. A6 after
A3, measurement-gated, optional. A7 its own session. A8 investigation, then
implementation if the former works. A9 stays speculative until one of its
four directions survives a probe.
