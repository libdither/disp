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

Remaining candidates: kernel.disp's inj/const_fn/perform (probed green,
deferred because kernel.disp carries an unrelated uncommitted local change),
id_fn/stem_fn/fork_fn/Pred/PiCode/Coproduct (unprobed), and the refused
set with diagnosed causes: all_rows/vec_rows-class (builder branches on its
parameter at construction, unlike list_rows whose branching hides in the
stored continuation; rewritable via list_rec/nat_rec = A5 candidates),
eff_bind (has_sig inspection on the subject), height (raw fix + triage, A5
candidate via tree_rec), key_insert (tree_lt, no carve-out), canon (dot on
an abstract type), eq_sym (Eq at abstract endpoints, the known eager-baking
wall), mixed_rows (its reject arm fires under the Eff recognizer's leaf
probe, so the row-program type is honestly not its codomain). Refused defs
cap at Sampled until their named blocker moves.

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
- GoodGate and Coherent as membership predicates over ShallowType.

Some of these will cap at Sampled (inspectors); record the tier reached next
to each. Type values that are membership-only today (`: ShallowType`) should
be probed for refinement annotations (Refine ShallowType Coherent-shaped);
def-order will block some, note which.

Gate: annotation batch green; per-def pins for anything that surprised.

## Stage A3: observation rows for tagged hypotheses (checker-facing)

Status: not started. Shared prerequisite with the Cases-record design (which
has no doc yet; write it first or fold it in here).

Give `Tagged S P` declared rows so hypotheses of tagged types are usable
under Guard. Two corrections to the original spelling:

- The walker mediates only the literal pair_fst/pair_snd trees into Acc
  frames, so a `Prj wait_payload` row can never match. Either spell the rows
  as nested `.snd` projections (works today, hard-codes the encoding), or
  build the missing mechanism: walker-side declared-accessor mediation (the
  walk consults the subject type's rows to decide whether an applied function
  is a sanctioned accessor). The mechanism is the real work and also what
  records-as-hypotheses and case_of/Tele arguments need.
- The sig check is a `Prj pair_fst` observation, not Self, and declaring it
  legalizes sig-reading for Hyp-typed arguments. The sig-mining pins survive
  today only because they sit at Bool/Nat domains; add pins that scope the
  new legality to Tagged domains explicitly.

Constraints to respect: eager-refusal invariant (undeclared observation is an
error, not a stuck term) and the S9 discipline (rows instantiate at concrete
constructors; codomains at abstract points lie).

Gate: conduct pins. A checked function over Hyp/BlessedElim arguments that
projects payloads legally; a negative showing undeclared observations still
refuse; the sig-mining pin family extended, not just kept green.

## Stage A4: sharpen payloads, retype the readers

Status: not started. Rides behind A1/A3.

Replace the AnyTree payloads with real shapes: eliminator payload as a Sigma
chain (raw x ind : ShallowType x motive), hypothesis payload as the keyed
record (Tuple/at machinery), history as declared inductive data. Then the
readers (wait_payload, neutral_history, elim_raw/ind/motive, elim_arity,
neutral_type) get spelled annotations at whatever tier each supports, which
without A3's mediation means Sampled for most; record the tier honestly.

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

Status: not started. Independent of A3/A4; needs interleaved perf runs.

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

Status: not started. The audit's known hard stop, independent of everything
above; prerequisite for honest types on the equality machinery.

The ledger (eq_pairs output) stores untyped endpoint pairs; an equation
assumed at a coarse type is reusable at a fine one (rel_probe unsoundness,
pinned test-side). Change eq_pairs/eq_reach/eq_holds to carry and respect
the equation's type, then annotate them and teq_one/teq_decide/eq_congr at
the tier that verifies. This is a semantic change with its own pin sweep,
not an annotation pass; plan it as its own session.

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
