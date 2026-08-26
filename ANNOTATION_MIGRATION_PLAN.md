# Annotation migration plan

Goal: total annotation of the kernel (lib/kernel/, the promoted standalone checker). Every definition in
lib/kernel/ either carries a checked annotation at the strongest tier it
supports, or appears in the "never annotated" list with its stated reason.
This file is a living plan: update stage statuses in place, delete the file
when the work is done.

## Ground truth: tiers, encodings, trust

Annotation tiers, strongest first:

- Guard tier (`Pi Guard`, `Fn`, `->`): the walker mints hypotheses and checks
  the body parametrically. `->` rides DefaultWalker (Guard); the Meta and
  TwoFace differentials re-verify the arrow surface under other walkers.
- Sampled tier (`Pi (Sampled [..])`): spot-checked at the listed samples.
- Membership (`: ShallowType`, `: Space`, `: Table`, `: Refine Space
  Coherent`): classifies the value. Space is the structured sub-universe
  (tag form plus declared structure: rows, gate+elim, or members); it
  refines TypeFace, so it is the strongest shape-membership tier. The
  Coherent refinement adds probe-strength laws on top.
- None.

Census (scripts/annotation_census.py, 2026-08-25 post-formation, 241 defs):
52 none, 49 membership, 69 sampled, 71 guard-tier shape-precise;
exact/dependent content at inj, list_const, nat_rec, tree_rec, list_rec,
and the four Eq lemmas at the J tier (carrier sampled).
The seven new unannotated are the walker refactor's atoms (fix_sig,
teq_decide_at, walk_open_at, the four handler constants). The full
classification of what remains and why is the "Remaining surface" section
below.

Three encodings underlie the special structures: wait forms (sig at `.fst`,
payload at `.snd.snd`; markers, collectors, invalids, tree_eq partials), tag
faces (`tag meta behavior`; types and anything faced -- the head embeds
behavior, so no constant sig), and bare pairs (records = sorted names list x
payload cells; inj variants = pair tag payload, which is also every frame:
Acc, Elim, and now Ask).

Trust facts that shape everything:

- The forgery defense is the walker and its ledger (hyp_ok, elim_raw vs
  T.elim), never the sig; sig-keyed types classify shape, not trust.
- The checked fragment refuses inspectors by design; that refusal is the
  sealed-core soundness. Machinery made of inspector reads cannot be
  Guard-typed without new mechanism (A9).
- The licensing criterion is the plug square: an operation may act on a
  hypothesis iff walking it symbolically then plugging any closing value
  equals plugging first and running raw. License what commutes (frames),
  defer what is natural-but-undetermined (stuck Bools, chains), refuse
  inspection; quarantine deliberate non-naturality in Ambient. Every place a
  mint reaches RAW evaluation is a potential hole; the known classes are
  recognizers under the judge, the probe filter (neutral_free-guarded), and
  structure-probing check faces (codomain/motive instantiation left this
  list on 2026-08-25: formation is walked):
  a field read decided AT a mint (name, spine node, or slot head) is not
  instantiation-stable. Space's check face refuses those via its steady
  skeleton guard; Record's field reads are the pre-existing unguarded
  sibling of the same class.
- Laws attached to types must be INERT boolean batteries until they are
  proof-carrying. Now that judge-level transport is live, a ledger equation
  ACTS (it converts types); a false probe-passing law stated as an equation
  would rewrite types, while a false battery only misjudges. Batteries at
  probe strength, Pi-typed proofs per type where gates exist, ledger
  equations only for facts.

## Landed ledger (compact; details in git log and the audit memory)

- A1 descriptive formers (c5df5c5d): Sus/Tagged/Faced + seven instances.
- A2 in three slices (4ce12724, 89ef76c8, 980fb937): the full annotation
  maximization of both files; GoodGate/Coherent moved above Nat with Sampled
  tiers; Nat and Tree at Refine ShallowType Coherent.
- A3a rows types (89ef76c8, 9c1dcea1): ElimHead/StuckElim/TeqPartial rows
  spellings with conduct pins; RawRecordRows (dependent length); control
  markers (Hyp/Invalid/OpenChain) keep recognize spellings categorically.
- S9 differential (a479c478): TwoFace probe-conjunction is the lie detector;
  with_probes guards abstract types (neutral_free); whole-surface TwoFace
  re-open beside the Meta one in kernel.test; default flip measured and
  rejected (~12x per load).
- A7 typed ledger (0de0705d, f2eb88ed): equations carry their carrier;
  eq_usable grades discharge (same carrier or identity-fine); goal enters
  once at top-level reach, recursion at finest grade; rel_probe gained the
  codomain and the reuse unsoundness closed; respect_ty's normalizer pin
  flipped (it rode the untyped ledger, not the S9 vacuity its comment
  claimed -- when closing one hole, re-derive every pin in the family).
- Judge transport (2933e84f): failed tree-identity type comparison consults
  the ledger at finest grade; Eq ShallowType A B converts hypotheses,
  including through constructor contexts; quotient equations refused.
- Ask head, Guard-only (b7ed2239): seven property formers as kernel atoms,
  Cell, one uniform op; concrete face = slot read (agreement pinned);
  abstract face = licensed observation answering a mint typed Cell (P T)
  (an Ask frame `pair "Ask" P` in the history, replayed by respond_face).
- A5 waves (0ac738f4, f80750a1): list_assoc at Guard tier over the blessed
  recursor with Sigma element domain; vec_rows over nat_rec at the
  function-space motive.
- Tooling (ba6bedc0 + census): red verification batches name their failing
  entries; the census script.
- Space (28a47b49): the structured sub-universe as a kernel type (tag form
  plus rows, gate+elim, or members; steady guard on the check face refuses
  mint-decided field reads, two forge certificates pinned false). The ask
  face admits Space hypotheses beside ShallowType; ask's codomain is the
  dependent Cell (P T). Membership shifts: the Refine ShallowType Coherent
  nine (minus AnyTree) to Refine Space Coherent, code formers and
  Tele/Pi/Sigma/Isect returns to Space, eighteen library surfaces from
  ShallowType to Space. Formation certifies: PiCode/SigmaCode/Pi at an
  abstract type land in Space from both universes' binders; consumption
  stays boundary 2. The universe cycle Space ShallowType = ShallowType
  Space = true is pinned and inert while no universe carries gate/elim.
- Space maximization (f474c437): Space carries the predicate obs row, so
  Space hypotheses apply as membership tests and both universes
  self-member (Space : Space, ShallowType : Space; hierarchies deferred
  until a universe gains an eliminator). Property formers signed (six in
  Space, MembersP in ShallowType), prop_formers as a Guard-checked
  function list, ElimPayload/elim_ind in Space with the conduct pin,
  Space subjects for elim_of/case_of/children, Record entries shaped,
  annotations added across Eq, Fn, Tuple, Vec, NamedRec, TypeRec, AllOf,
  UniqueOf, Perfect, height, given, default_guard; Point and False at the
  audited tier. Probed refusal: Coherent cannot run GoodGate at a
  mint-parameterized gate, so List/Eff keep plain Space codomains.
- Tree absorbs AnyTree (52770c11): the bare universal predicate retires;
  Tree (identical membership, presents the fold) moves into kernel.disp
  and every reference follows, hypothesis-history keys included. Tree
  hypotheses license exactly the fold beyond what bare ones did;
  projections still refuse, so no certificate flipped except the two
  gateless-refusal pins (re-spelled at ShallowType) and three
  bare-predicate subject pins (re-spelled at Pred). Downstream: MembersP
  in Space, prop_formers as List (ShallowType -> Space), Cell/Sum at
  Space subjects, prop_slot annotated; Refine ShallowType Coherent has
  zero remaining sites. Probed refusals: tag cannot land at a rows
  codomain (S/K output puts mints at row decision spots); Sus keeps its
  recognize spelling (SusRows refuses neutrals at the admit gate while
  Sus deliberately covers hypothesis marks).

- Walked formation (4d8ad9f8, 7d45331b, d73450de, f904574b; the barrier
  decision, taken): the Guard walker detects eliminator-on-hypothesis
  itself; the walker is a top-level walk_with tied through neutral_type,
  its handler pairs named (honest_is_neutral/native_teq = walk_open, the
  checker's own walker; lying_is_neutral/minting_teq = what a candidate may
  learn); Read becomes Form, the tier applying checker-side code to its
  values -- raw by default, walk_open on Guard and Meta, the replay
  included; a closed application inside the walk is plain evaluation,
  which pays for it (cold suite 29.4 s vs 28.4 s); open_elim_start asks a
  neutral's type only at fix-shaped heads (formation and replay recurse
  through neutral_type -- a new intercept that asks it can loop). The
  eliminator marker's is_neutral branch goes: raw programs pay nothing
  (elim_id 3000: +65,769 steps vs +65,599 bare), the raw tier is pure tree
  calculus again, and the stuckness pins assert it of the walker.
  Boundary 8 narrows to equations inside families. Meta forms on the
  read-back mark through walk_open, so MP's formation half is done.
- Mint identity (bb6c7ffa): a hypothesis is keyed on (type, remaining
  telescope, prior mints) -- the in-context provenance A9 listed as
  needed; same-typed binders stop aliasing (the ih-is-m collapse that
  certified a non-Nat arrow is pinned false).
- Tooling: scripts/experiment.sh (a kernel edit tried cold, capped,
  restored on any exit), probe.sh reports cache and RSS, the census
  accepts -v.

- Harvest after walked formation (2026-08-25): computed motives beyond
  add_zero (a48aaae3: mul_zero, add_succ, both tiers); the Eq lemmas as J
  instances at the dependent tier (8e3cf5b8) behind two enabling changes
  -- canon tests neutral_free lazily (strict `or` ran it under the
  candidate walker, which refuses inspection), and Eq's recognizer keeps
  the endpoint comparison under the binder (`tree_eq (pair nx v) (pair ny
  refl)`: the hoisted `tree_eq nx ny` was baked with the constructing
  walker's answer, so the candidate's Eq and the hypothesis's type
  diverged by tree identity); commutativity of addition proved in-kernel
  (f32e55ef, Guard). elim_marker drops its self-threading (86395f6a): the
  walker supplies the marked head as self. Probed and refused: the
  open_chain family is live (the private list_rec is a bare fix identical
  to List's elim -- neutering it breaks nine combinators); merging check
  and respond_face_at (opposite jobs under different protocols).

## Probe-confirmed boundaries (canonical list; each caps a tier somewhere)

1. Raw structural inspection of a mint refuses (the naturality floor).
2. Scrutinizing formers (Sum, Tele/Pi) refuse CONSTRUCTION over an embedded
   mint inside walked bodies (pinned: `Cell (NormP T)` at a mint refuses);
   eliminator construction at mint-dependent types is the same class, so
   eliminators bind top-level and asked witnesses cannot yet be consumed.
3. eff_bind inspects its subject, so a neutral row program can END a
   program (tail position, vec_rows) but not be sequenced mid-program
   (perfect_rows stays raw).
4. A neutral in a judged slot accepts only at its recorded type (no
   subsumption); rows membership refuses any payload embedding a live mint
   (StuckElim describes the neutral-free shape only).
5. Kernel VALUES may only use kernel-local names: raw-pass loads leave
   givens absent (why GateP/MembersP signatures are AnyTree-weak in the
   kernel; honest signatures belong to the library registry).
6. Weak-signature atoms collide as trees: `{_T} => AnyTree` written twice is
   ONE tree, and both dispatch and licensing key on tree identity (probed:
   a naive RowsP aliased MembersP through ask end to end). Property identity
   cannot rest on fiber shapes.
7. GoodGate is plumbing-plus-frame, not beta: a JunkBool (correct
   recognizer/gate/members, eliminator that always takes one branch) passes
   GoodGate AND Coherent while eliminating wrongly. teq_one additionally
   trusts members-completeness beyond what any battery checks. The
   adversary is not yet pinned in-repo; pin it with the beta work (P1).
8. S9 residue, narrowed 2026-08-25: formation is walked, so a codomain or
   motive computing on a mint forms as the type that was written, and a
   family that INSPECTS its argument is refused (pinned on both tiers).
   What remains: an open tree_eq inside a family answers natively
   (walk_open runs native_teq at an empty ledger), so a family branching on
   an equation of mints still collapses -- the S9 vacuity pin stays true.
9. The Meta simulator lacks rules for the new heads (ask, conversion) and
   still diverges consuming branching row programs at a mint (TreeOf, the
   one Pi-Guard-literal annotation left on the old surface; the new ask and
   transport pin families are Pi-Guard-literal for the same reason).
   Formation and provenance are shared since 2026-08-25 (Meta forms
   through walk_open on the read-back mark and keys mints like Guard).
10. Blessing compares by tree identity end to end: a blessed eliminator
    face refuses spelled-arrow step domains (Tele-wrapped arrows are not
    gate codes; only the raw face accepts the doc spelling), and a
    same-name public rebind collides at the barrel open (privacy via
    top-level `let`). The blessing payload stays a bare pair: the record
    spelling measured +330ms on the cold barrel, dominated by the walker's
    elim_ind/elim_motive reads during stuck-elim checks.

## Remaining surface (census 2026-08-25, post-formation)

The 52 unannotated, by reason:

- Sig constants (9): tree_eq_sig, invalid_sig, open_sig, fix_sig, hyp_sig,
  elim_sig, elim_stuck_sig, ask_sig, Rec. Value atoms; a stem-shape
  membership type would cover them and say almost nothing.
- Handler constants (4): honest_is_neutral, native_teq, lying_is_neutral,
  minting_teq. The walker's named handler pairs; the same class.
- Checker machinery behind the inspector wall (14): hyp_marker,
  make_mark, make_hyp, open_chain, neutral_free, structured, tag_form,
  steady, teq_decide, teq_decide_at, walk_open_at, eq_usable, eq_congr,
  canon. Bodies read structure off arguments; Guard refuses by design (the
  sealed-core refusal); Sampled would only re-run constructions. Unblocked
  by A9 only.
- Re-tiered 2026-08-25 (2): elim_marker no longer inspects (projections on
  a concrete payload plus application) and elim_stuck only applies a
  hypothesis, which walk_open licenses; both are Sampled-writable now.
- Protocol and sugar targets (5): check_module (pinned := Record),
  elab_settings, eff_bind, reject, `let`. Trees are consumed by name or
  by the elaborator; annotations add nothing the pins do not.
- Trivia (3): arm (alias of idx), Sus (writable `: ShallowType`), refl
  (its honest type is every reflexive Eq at once; no single annotation).
- Row-program builders (9): raw_record_rows, record_rows, all_rows,
  splits, bijection, unique_rows, mixed_rows, perfect_rows, len_xs_rows.
  Probed blocker: Eff membership walks continuations at junk leaves, so
  any builder with a reject branch or computed dispatch is not an Eff
  member on samples. Needs an Eff membership that treats the abort
  convention as in-family, or per-builder sample values that avoid every
  branch.
- Private lets (list_mem, eff_mem, record_mem and friends): not
  exported, deliberately bare.

The 69 Sampled, by reason:

- The Eq lemmas (4, 2026-08-25): J instances at the dependent tier --
  sampled carrier (Nat), hypotheses for endpoints, families, proofs and
  bases; Pi-Guard-literal because the TwoFace differential's probe
  expansion over six binders measured past 6.5 GB.

- Inspector wall (the bulk): field/record readers, sig testers, dot,
  make_type, faced, handle, prod, the elim machinery accessors, ask,
  Sum, Cell, case_of, children, respond_face/neutral_type, check, Tele,
  the formers, the tables, teq/eq internals, with_trials/with_probes.
  Capped until A9. The escape that already works is the A5 recursor
  route: respell a body over a blessed recursor and the arrow tier
  opens (list_assoc precedent); list_zip, list_apply, and idx are the
  standing candidates.
- Honest partiality: Tuple (invalid branch), elim_arity (invalid
  escape), guard/given/default_guard/sig/base (record protocol),
  GoodGate/Coherent (audit predicates over any type), Eq (canon reads
  A.norm), Fn (a partial application), Vec/NamedRec/TypeRec/AllOf/
  UniqueOf/Perfect/height (row or fix bodies that inspect).

## Stage R: ask retires into dependent observation rows

LANDED (R1 5bc6f3d6 2026-08-25; R2-R5 089d73055c63 2026-08-26). What shipped, and
where it differs from the sketch it replaced:

- R1: Row grew ObserveAt (payload = pair accessor family); check judges
  the accessor's result at `family f`, the replay answers a matching Acc
  frame with `family self`, both formed through the tier.
- R2 landed as KEYED data, not closures: the definition-order and
  self-reference wall (GateP/UnderP mention the universe; Cell and ask
  live 200 lines below ShallowType) is closed by a fourth row kind,
  `Property k` -- both universes' obs carry seven Property rows, and
  check/respond resolve k late through the one kernel table `properties`
  (name x former; prop_slot and fiber read the same table). Property rows
  are transparent to non-matching frames (T 5 skips them to Fresh);
  generalizing that skip to Prj/ObserveAt DIVERGES (measured past 8 GB),
  so telescope semantics stay everywhere else -- a standing landmine.
- R3 landed stronger than sketched: the walk lost its whole accessor
  vocabulary, not just the ask case. A parametric refusal on a hypothesis
  retries once as an Acc frame against the rows; pair_fst/pair_snd and
  the reduced ask partial (`ask T P = slot_ask (prop_slot P) T`,
  S/K-walked down to the row's accessor) license identically.
- R4: Ask frame, respond branch, universe check, prop_formers -- deleted.
  A new property = one row per universe + one `properties` entry.
- R5: Meta retries a Poison from closure-applied-to-Neu as the same Acc
  replay; Fn/Meta/TwoFace ask spellings pinned in kernel.test. The
  types.disp ask pins keep their Pi-Guard-literal spelling (respelling to
  arrows is optional polish, not a blocker).
- Cost: kernel.test cold 198.6M -> 194.3M steps (-2.2%); the rows are
  ~free, the deleted whitelist pays for them. Measured cold-vs-cold; the
  warm cold_equiv protocol (AGENTS.md) replaces cold A/Bs going forward.
- Still does NOT retire: witness consumption (stage WC unchanged).

## Stage P: the property system (the current arc)

Decisions, from the 2026-08-16 design review, its adversarial reflection,
and the probes above:

- Properties are a CLOSED, kernel-owned set. The universal-property kinds
  are few (classifier, initial algebra via gate+elim, limit-of-observations
  via rows, enumeration, quotient, equalizer, plus plumbing: under, check,
  land, code); the set grows only when the type theory grows. No user
  extension; the kernel is the allocator.
- Identity is POSITIONAL over a DERIVED order: the dependency order of the
  property DAG (recognize/rows before the laws relating them, gate before
  elim, under before ends), ties broken by pinned choice. The order is not
  chosen, it is computed -- and verified by a load-time FIXPOINT PIN: the
  raw pass assumes the pinned order, the checked pass re-derives it from
  the built signature trees and compares. Order shifts become loud pin
  failures instead of silent tree_lt drift. Names become surface artifacts
  resolved at elaboration; no name reaches a tree. (Fallback, recorded: if
  P0 shows the fixpoint unstable under ordinary edits, tagged atoms --
  `inj "GateP" fiber` -- provide collision-proof identity at the cost of
  conceding nominal atoms; the reflection agent's recommendation.)
- The registry is library DATA (types.disp): per property -- index, honest
  signature former (List/TypeFace/Eff exist up there), law battery. The
  kernel keeps only the fiber table (positional list; respond_face computes
  `Cell (idx table i self)`) and the licensing check. The registry is on
  the certification side; the judge never reads it, and anything that would
  make the judge read it must move into kernel.disp at that moment.
- TypeOf is the stated normal form: the dependency-ordered telescope of
  Cell-wrapped fibers, written as rows over ask-projections, equal to a
  fold of the registry. It is honestly a LIMIT, not a unification: data
  fibers (members : List T, ends : Sigma T T, under, code) get real
  membership typing; function fibers (recognize, gate, elim) are
  law-charactered only, because sealed-core inspectors refuse Guard
  certification at their own signatures by design.
- Access semantics: the recognizer spec ("the entry with property P") is
  the meaning; storage lookup is the licensed fast face; physical layout
  stays free behind ask (the legacy string-keyed assoc remains bootstrap
  storage; a dense positional layout is a measured experiment, and the
  linear order is storage convenience -- the semantic content is only the
  DAG).
- tele/obs are one property (rows) at two strengths; the strength is a law
  (definitional: the recognizer agrees with running the rows, probe-
  checkable). The judge still routes on the slot name; that is a stated
  veneer. Judge-routes-on-law is its own hot-path stage, not scheduled.

Sub-stages:

- P0 (decisive, small): write the dependency-order derivation over the
  current slots, compute the fixpoint on today's kernel, pin it. Prototype
  the beta law (member-bearing types: run elim_of on each member with
  distinguishing concrete arms), confirm it refuses JunkBool with stock
  types green, and measure its declaration-time cost under the Sampled
  codomain annotations. P0's outcome selects positional-vs-tagged and
  decides whether laws-not-signatures is honest cheaply.
- P1: registry data + law extraction (Coherent decomposed into per-property
  batteries; the monolith untouched), the beta law included, JunkBool
  pinned as the adversary.
- P2: TypeOf + membership pins (stock types true; junk and hypotheses
  false) + registry-fold-vs-Coherent agreement pins.
- P3: positional identity: kernel fiber table, ask/licensing re-keyed,
  surface name-to-index at elaboration, the slot accessors carry indices. Gated on
  P0 fixpoint stability. Def-order landmine to plan around: the fold must
  serve annotations above Nat while honest signatures need List/Eff from
  below it -- split registry (below) from the fold's Nat-serving half, or
  keep the monolith for bootstrap annotations.
- P4: swap the Refine Space Coherent consumers to the registry fold
  via given-fill (zero kernel text edits), gated on P2's agreement pins
  holding across the Meta and TwoFace differentials plus cold timing;
  retire the monolith after.
- P5: storage experiments (dense telescope layout) behind ask, licensed
  against the recognizer spec, adopted on measurement only.

## Stage MP: Meta parity for the new heads

Stage R landed the ask route (the Poison-to-Acc retry), so MP is down to a
conversion route in Meta (mwalk/vcheck) plus the TreeOf branching-program
divergence (needs host-side step tracing of the simulator). Lifts the
remaining Guard-literal pin families (transport, RawRecordRows conduct) to
arrows under the differential. The deep
alternative stays recorded: converge Meta onto Guard's in-band provenance
design instead of accreting parity rules.

## Stage WC: witness consumption

Asked witnesses type, pass, and return; consuming one inside a checked body
(case, projection, application) was boundary 2 while formation was raw.
Probed 2026-08-25 now that formation is walked: APPLYING a plain function
hypothesis inside a checked body works, but case_of on an asked Cell (P T)
witness still refuses (even with constant arms over a NormP witness) --
eliminating a Sum-typed mint whose payload type embeds the mint is the
remaining wall, not formation. So WC is NOT unblocked by walked formation
alone; the dedicated case head (walker-recognized elimination of Cell-typed
witnesses against the respond_face-computed type) is back to the likely
route. Decide after the P-stage lands the property registry.

## Stage A3b-general: name-keyed reads on record hypotheses

The ask head covers TYPES. Records-as-hypotheses (the Cases idea, the
hypothesis payload record, case_of/Tele table arguments) still need
walker-side mediation for field/field_cell accessor shapes, or a Cases doc
deciding the encoding first. A4's remaining half (hypothesis-payload
sharpening, neutral_history/neutral_type reader types) waits on this.

## Stage A5 remainder: eliminator rewrites, per-function, perf-gated

With named blockers: list_zip/list_apply (dual/accumulator recursion; the
CPS respelling dies on nominal codomain identity), idx (untyped spine has
no rows at abstract points), key_insert/key_sort/make_record (tree_lt has
no carve-out), prod (undeclared variants), height (raw nat_le in the fork
arm; needs a blessed comparison), perfect_rows (boundary 3), eff_bind
itself and mixed_rows (diagnosed in A2). Each rewrite is tree-changing and
hot-path: interleaved timing, the +330ms precedent as kill criterion.

## Stage A6: the record encoding decision (gated, optional)

Unchanged from before: records-as-wait-forms would buy uniformity and
walker mediation at the cost of an indirection on every hot field read;
probe via elab_settings re-pointing; kill on measurement. Do after
A3b-general if at all; nothing else depends on it.

## Stage A8: op-indexed effects, then annotate the interpreter

Unchanged: investigate an op-indexed effect former (per-op answer types) so
check and eff_bind carry real types and Tele's tables get per-slot
contracts instead of shape-only `: Table`. Entry ramp for A9.

## Stage EQ: the equality remainder

- Step-4 respect licensing (unblocked by A7, wants the registry): the
  license design for quotient congruence evidence -- what discharges
  "f respects Q into B", carried per (function, quotient) pair; rel_probe
  becomes kernel surface behind it. The conservative refusals A7
  introduced (normalizer respect, constructor congruence under quotient
  goals) are exactly the holes these licenses fill.
- Beta probes (shared with P1) and, later, per-type Pi-typed law proofs
  via gates (the add-zero pattern generalized).
- FunEq packaging (a Pi-family of Eqs as a former); pointwise spellings
  suffice meanwhile.
- Type-quantified laws are the frontier (below), not a stage.

## Stage A9: the machinery core (research, do not schedule)

The walk, judge, hyp_ok, respond_face/neutral_type, Meta's simulator, Sum's
internals, teq_decide are inspector code; the parametric fragment refuses
inspectors as its soundness mechanism. The soundness story factors in two:
the walk's rules are natural GIVEN honest rows (a fixed meta-theorem,
provable once, externally, by the standard gluing method -- unwritten), and
each type's rows keep their promises (per-type, in-language: Coherent now,
registry laws in P). Internal self-typing still needs one of: types-as-
effects (A8 ramp), in-context hypothesis provenance (landed 2026-08-25,
bb6c7ffa), the legacy kernel's
licensed-fast-face moves, or the wf_fix/Total totality story. Honest
ceiling until then: Sampled plus behavioral certification for ~30 core
defs, recorded per-def.

## The frontier (research items the stages above stop at)

- Dependent application at abstract points: observations on x : A when A is
  itself a mint; `.gate`/`.tele` on abstract types beyond the ask
  vocabulary; polymorphic eliminator arms; Eq at abstract carriers (which
  is what type-quantified laws need).
- Walked type formation (the barrier): DECIDED and landed 2026-08-25 as
  candidate B in its honest form -- the tier forms through walk_open, the
  walker with the honest handlers. The 2024 cheap form failed because it
  used the CANDIDATE walker, which lies about is_neutral; type-forming code
  is checker vocabulary and must see hypotheses. Candidate A (marks stuck
  in the evaluator) is retired: the raw tier is pure tree calculus and the
  checker no longer needs it. Left of this wall: equations inside families
  (boundary 8) and the abstract-type item above. The plug-square rule
  still governs the raw sites that remain (recognizers under the judge,
  the probe filter, check faces).

## Never annotated (by principle, with reasons)

- make_hyp / make_mark: a checked annotation would certify hypothesis
  forgery; the ledger refuses it and the refusal is pinned.
- Marker construction internals (hyp_marker, open_chain, invalid_marker
  bodies; elim_marker and elim_stuck re-tiered 2026-08-25, see the
  census): the substrate implementing what the descriptive types describe;
  their spec is the walker's audit.
- Protocol vocabulary (let, given, default_guard, check_module,
  elab_settings): driver registration points with fixed-arity contracts.
- The raw prelude: the primitive tier cannot use givens in values,
  settings-scoped sugar, or accessors.

## Sequencing

P0 first: it is cheap and selects the identity scheme and the law story for
everything in P. MP and P1-P2 next, in either order (P1-P2 are additive
library work; MP lifts two pin families to the differential surface). WC
via the case head (direct probe 2026-08-25: case_of on an asked witness
still refuses; walked formation did not unblock it). P3-P4 once P0-P2 hold; P5 measured,
never scheduled. EQ's step-4 after P1 (it wants the registry and laws in
place). A5 opportunistic per function. A3b-general when the Cases doc is
written; A4's remainder behind it. A6 optional and measured. A8
investigation feeding A9; A9 stays research until one of its directions
survives a probe. The frontier items are the standing walls every stage
routes around; they move only by deliberate decision, not by drift.
