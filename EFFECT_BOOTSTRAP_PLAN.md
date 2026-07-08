# Effect bootstrap: types as protocols, the walker as a handler

Status: plan, 2026-07-07; the de-risk pass and STAGE 0 landed the same day.
Canonical rows and the deep `Eff R X` recognizer are in `lib/std/effect.disp`
(`EffAt` is marked as the shallow compatibility form); the hazard 0a mechanism
is corrected below (the elim idiom, not `match`) and pinned together with the
recognizer in `lib/tests/eff_deep_proto.test.disp` (38 pins, including the
branchy-continuation-through-Eff exit demonstration); hazard 1a was probed
empirically and does not reproduce (pins in
`lib/tests/tele_spec_proto.test.disp`). Stages 1 through 5 are not built. It
is written to be read cold: Part I gives the background a fresh session needs,
Part II is the staged plan, Part III is logistics and rules of engagement. Companion pieces:
[`REFLECT.md`](REFLECT.md) (reflection as an effect, the move this plan
generalizes), [`TOWER.md`](TOWER.md) (the self-checking architecture this plan
feeds), `lib/std/effect.disp` (the effects library this plan builds on, commit
9fe863c), and TYPE_THEORY.typ §15 (the effects spec). Prior art survey:
`research/EFFECT_SYSTEMS_SURVEY.md`.

Read order for a fresh session: this file, then REFLECT.md, then
`lib/std/effect.disp` and its two consumers (`lib/tests/effect_proto.test.disp`,
`lib/tests/reflect_proto.test.disp`), then the Step table and `tele_walk` in
`lib/kernel/cells.disp` (roughly lines 30 to 180), then `make_rec_recognizer`
and the walker in `lib/kernel/engine.disp`. Skim TOWER.md for the trust-ledger
framing. CLAUDE.md's "Compiler workarounds" section is load-bearing for stages
1 and 2.

---

## Part I: Background

### 1. Effects here, in sixty seconds

An effectful computation in disp is a value, a description of what to do, never
the doing of it. The substrate cannot perform effects: hash-consing shares
structurally equal subtrees, so a side-effecting reduction would run an effect a
sharing-dependent number of times. Purity is forced, and the §15 design follows
from it. There are two node shapes:

    Pure x            "done, the answer is x"
    Op op arg k       "perform op with arg; when you hand me the result r,
                       I continue as (k r)"

The continuation `k` is an ordinary function: the rest of the program, as a
function of the answer it is waiting for. A program is a chain of these nodes.

A handler is an interpreter that folds the chain. At `Pure` it runs its
`return` clause; at an `Op` it covers (run the clause, with `resume` = "keep
interpreting the tail under me") or forwards (re-emit the op, stay installed on
the tail). Because the program is a pure value, a clause may call `resume` zero
times (abort, this is how `catch` works), once (ordinary sequencing, state), or
many times (search, nondeterminism). All of this exists and is pinned:
`lib/std/effect.disp` (io_pure, io_op, eff_bind, mk_op, handle, catch, rows,
run_pure) with 18 pins in `effect_proto.test.disp`.

A row is the set of effects a value may perform. `eff_check R v` walks the
value and checks every op's label is in R, so weakening is free (a `[State]`
program checks at `[State, IO]` because containment is containment) and
handling an effect discharges its row entry. `EffAt R` lifts the check to an
ordinary type via Refinement. One known limit was central to this plan: that
check is shallow (it probes continuations with a leaf, follows one path, and
only certifies the first operations). Stage 0 closed it: the deep `Eff R X`
recognizer landed 2026-07-07, and `EffAt` remains as the compatibility form.

### 2. Telescopes here, in sixty seconds

A type in disp is a predicate: a tree program that answers whether a value
inhabits it. The negative formers (Pi, Sigma, Record, the meet types) are all
one shape, the telescope: a chain of cells `t cell (\x -> rest)`, where each
cell is one observation to make and the tail binds the observed value, so later
cells can depend on earlier answers. One walker, `tele_walk` in
`lib/kernel/cells.disp`, runs a chain in two modes: recognize (mode true, "does
this candidate satisfy every cell") and respond (mode false, "route this
elimination frame through the cells").

The key mechanical fact: a cell op never recurses, never binds, never rejects
by itself. It returns a `Step`, plain data saying what it wants, and the walker
interprets it:

    SMint ty     "bind me a fresh abstract inhabitant of ty"
    SThread x    "record x and continue"
    SReject      "not a member"
    SDone a      "finish with this Action"

The walker owns the recursion and the `bind_hyp` call (for a compiler reason
documented in CLAUDE.md: a continuation passed through a function into
`bind_hyp` miscompiles under nested binders, so the ops hand back data and the
harness does the binding inline).

### 3. The observation: these are the same shape

Both structures are chains of "emit a head, bind the answer, continue":

    telescope   t   cell (\x -> rest)      head = an observation to make
    program     Op  op arg (\r -> rest)    head = an operation to perform

And the Step vocabulary is an effect signature that never got named:

    SMint ty    = perform mint(ty)         answered by bind_hyp
    SThread x   = resume with x            the continuation, applied once
    SReject     = throw                    abort the walk
    SDone a     = early return
    do_check    = perform check(ty, v)     the policed sub-check (the §7A route)

Under this reading, `tele_walk` is a handler, and the mode boolean is a choice
of handler: recognition runs the type's program under the concrete-candidate
interpreter, respond runs the same program under the symbolic-frame
interpreter. A telescope is a straight-line program (the data never picks a
different spine); a general effectful program is the same thing with branching
allowed. So "type" and "effectful program" are the same protocol shape read
twice: as a classifier, and as a computation. Checking a value against a type
is running the type's protocol; eliminating a stuck value is running the same
protocol against a frame.

### 4. REFLECT.md's move, and why it generalizes

The walker enforces parametricity by rejecting inspection: checking
`f : A -> B` mints an opaque hypothesis and refuses any attempt to look inside
it (triage, payload reads). The cost is that inspection-shaped code is
untypeable as stated, including the walker itself, which is why kernel bodies
carry `SEALED(...)` comments instead of types. The two-horns pin in
`soundness.test.disp` shows this is forced, not incidental: the natural typed
facade needs a head guard that is simultaneously walkable and forge-proof, and
the two available guards each satisfy exactly one.

REFLECT.md's answer: distinguish performing an inspection from requesting one.
A program that asks "classify this tree, then continue here" is just building a
value, and building values over hypotheses is what the walker permits. The one
place that looks is the handler, one clause, at the floor. The slogan:

    looking is untyped; asking is typed; the row is the modality.

`walk_spec` (in `reflect_proto.test.disp`) already applies this to the walker's
own algorithm: the checker's dispatch logic exists as a typed object-language
value, differentially pinned against the live walker.

This plan is that move applied to the whole checking vocabulary. The telescope
Steps are the checking-side half of the same signature whose reading-side half
REFLECT.md started (classify, and eventually the other sanctioned readers).
One signature, one floor handler, and every sealed algorithm gets a typed spec
twin written against it.

### 5. What "closing the loop" can and cannot mean

Three things are conserved through this program. They do not block it; they
define what the end state honestly is. A fresh session should internalize these
before writing any code, because they are the difference between the plan and
an overclaim.

1. The floor is conserved. Asking relocates looking into the handler; someone
   still performs the triage, and the handler clause is that someone. Trying to
   type the handler reproduces the two horns (checking it behaviorally means
   running its body on a minted hypothesis, which is the inspection itself),
   and writing the handler as a program just demands a handler for that. An
   interpreter tower needs a machine at the bottom. What changes is the shape
   of the trusted surface: from "the walker's whole body plus diffuse
   carve-outs, pinned by comments" to one enumerable clause table with a
   per-clause soundness note. Narrow and listed, not zero. This is also the
   clean restatement of the spec's protection sets: seal(Σ) IS the floor
   handler, and forge(Σ) is its constructor ops (mint, extend).

2. Self-verification is behavioral, not foundational. The spec twins' types
   are checked by the live walker, so the achievable statement is "the checker
   accepts its own description": a fixed point with the same epistemic status
   as `Type : Type` and R6, which this project already embraces. Milawa (see
   TOWER.md) is the existence proof that a tower of levels with bridge
   theorems and an explicit ledger is worth everything anyway. Gödel does not
   grant more.

3. A row certifies capability, not correctness. `walk_spec` typed at
   `Eff [Reflect] CheckerResult` says what the walker may do, not that its
   dispatch implements parametricity soundly. The soundness theorem attaches
   to Reflect-free rows (the parametric fragment, where free theorems live);
   the walker is maximally Reflect-full, so its correctness is exactly the
   non-free residue, argued per handler clause. What rows buy the soundness
   story is the perimeter: everything outside the floor is machine-checkably
   not inspecting.

### 6. The maximal shape (the definition of done)

Every kernel operation ends in one of three buckets:

- (a) an ordinary typed program (structural recursion over concrete trees,
  the `tree_rec` clients);
- (b) typed at a row, with its algorithm as a spec program over the kernel
  signature, differentially pinned against its live fused form;
- (c) a one-clause entry in the enumerated floor handler, with a written
  soundness note.

Nothing left says `SEALED(<comment>)` alone; everything says either "checked"
or "trusted, here, one clause wide, for this reason." Concretely the prototype
is done when:

1. `Eff R X` has a deep recognizer (continuations checked under real minted
   answers, all branches). Landed 2026-07-07.
2. The kernel signature exists as ops with a single floor handler whose
   clauses wrap the existing sealed bodies.
3. `tele_spec` (the telescope walker as a program, modes as handlers) agrees
   with the live `tele_walk` on a differential battery, both faces.
4. `walk_spec` covers the full special-case table (no delegated routes), and
   `hyp_reduce` has a spec twin.
5. Every `SEALED` marker in `engine.disp`/`cut.disp` maps to a spec twin or a
   handler clause (enumerated in a ledger table).
6. At least three alternative handlers run the same spec programs (cost,
   explanation, synthesis), pinned.
7. The behavioral fixed-point pin: `verify` of the spec module returns
   `Ok true` under the live kernel.

Explicitly out of scope: rewriting the live fused kernel (the fused forms stay
the inner loop; a handler round per reduction step is not viable, the same
grounding reason the cut's list toolkit does not route through `elim`); the
spec-to-fused license proof (pinned differentially only; the license is the
OPTIMIZER.typ arc); open-row inference; the impure IO driver; scoped/weave
higher-order operations; any effect-typed user-facing surface syntax.

---

## Part II: The stages

Each stage lists: goal, work, hazards (with the exact trap and its
workaround), pins, exit criteria. Stages are ordered by dependency; 0 gates
everything.

### Stage 0: the deep `Eff R X` recognizer

Goal. Replace the shallow `EffAt` Refinement with a real type whose recognizer
checks, per node: `Pure x` has `x : X`; `Op op arg k` has `op`'s label in `R`,
`arg : op.param`, and `k : op.result -> Eff R X`, with the k-check walking a
real minted answer through every branch of the continuation.

Work, first route (the hand knot). `make_rec_recognizer` in `engine.disp`
already threads `self` (the reconstructed, fully-applied type) into a
recognizer body, which is how Nat ties its knot. Sketch:

    let eff_body := {self, m, v} -> {
      let R := (m.recognizer_params).row
      let X := (m.recognizer_params).result
      match v {
        Pure x      => param_apply X x
        Op op arg k => three checks, chained through the CheckerResult bind:
                       label_in_row (effect_of op) R,
                       param_apply (op.param) arg,
                       param_apply (Arrow (op.result) self) k
        _           => Ok false
      }
    }
    Eff := {R, X} -> wait (make_rec_recognizer eff_body)
                          (make_meta { row := R, result := X } inert_respond)

The H-rule comes free from `make_rec_recognizer` (an Eff-typed hypothesis is
recognized by its stored type). Respond stays inert for v1.

Work, second route (cleanup, optional in the prototype): `Eff` as a stock
Coproduct whose Op arm is a positional telescope `[op_cell R; arg_cell;
k_rec_cell]`, where `k_rec_cell` is recursion under a Pi, a `RecUnder` with a
reader functor (fmap = post-composition). This makes Eff an ordinary inductive
with the generic fold; pin tree-level agreement with route one if built.

Hazard 0a, the one-path certificate (this is the real fight). Checking `k`
mints `r : op.result` and walks `k r`. Continuations in the current prototypes
branch with tag tests: `if (tree_eq (sh_tag s) "fork") ...` where
`sh_tag = pair_fst`. On a minted `s`, `pair_fst` (a sanctioned reader) answers
the hypothesis signature, a concrete tree unequal to every tag string, so every
test answers false and the walk silently follows the all-else path. The
certificate covers one branch and claims all. Note that the deep recognizer
alone does not fix this, and cannot: a walk takes one concrete path by
construction, so an if-chain body stays one-path-certified under it. Depth
comes from changing the branching idiom. The fix is a chain (REFLECT.md's path
item 2 made load-bearing, with the mechanism named precisely; updated
2026-07-07, pinned in `lib/tests/eff_deep_proto.test.disp`):

  1. `ShapeR` (and any other op-response vocabulary) becomes a real Coproduct.
  2. Continuations eliminate the response through the type's gated respond:
     the hypothesis is applied to a `{ motive; cases }` frame (via `rec_value`,
     `elim`, or the frame directly), the motive spelling the continuation's
     result type. An earlier draft of this plan said "written with `match`";
     that was wrong. Surface `match` compiles to the consumer-driven cut (the
     tag read via the `pair_fst` carve-out answers the hyp signature, and the
     payload read via `pair_snd` is unsanctioned triage), so a match-written
     continuation fails closed on a minted answer; it never routes through the
     respond and cannot become the certified idiom at runtime.
  3. The frame application routes through `hyp_reduce` into the gated respond,
     and `coh_check` verifies every case against the motive under per-variant
     minted payloads. This is the machinery that already verifies `nat_rec`'s
     annotation at load, so nothing new has to exist; what is new in stage 0
     is only this composition running inside the Eff recognizer.

The seed file pins all four behaviors on a 4-variant response vocabulary
(`ShapeC`, the ShapeR shape, including a 2-arg variant and a Type payload):
the elim-written continuation deep-checks Ok true and its annotation verifies
at load (`shape_size`); flipping one arm ill-typed flips the verdict, so the
arms are demonstrably visited; a missing arm also fails; the match twin is
correct on concrete values and Errs under the check; the if-chain twin
certifies despite an ill-typed untaken branch (the conserved limit, pinned so
it is a documented boundary rather than a surprise).

Consequence: certified branchy continuations carry an explicit motive, which
is heavier ergonomics than `match`. A motive-carrying match (or a
match-to-elim lowering where the scrutinee type is known) would be elaborator
work; it stays out of scope for this prototype, and the elim idiom is the
certified spelling. Programs written with raw tag tests remain only shallowly
certified. Rewriting the reflect prototype's twins onto the elim idiom is
stage 3 work; the branchy program stage 0 needs is already pinned.

Hazard 0b. `param_apply (Arrow T self) k` needs `Arrow` and needs `self` to be
the applied type; confirm `recognizer_wrap`'s `self` is what lands in stored
types (it is: `wait (wait wrap body) meta`), so the recursive H-rule fires for
sub-computations.

Also in this stage: canonical rows. Landed 2026-07-07: `mk_row`/`row_insert`/
`row_union` in `lib/std/effect.disp`, over a raw structural label order
(leaf < stem < fork, children lexicographically); order- and
duplicate-insensitivity, nil canonicity, and verdict-blindness of the row
checks are pinned in `eff_deep_proto`. Open rows (a Row-typed hypothesis as
improper tail) can be declared and constructed here but their deep story is
not a blocker for later stages.

Pins. A branchy continuation checks Ok true at its row and the check demonstrably
visits both arms (make one arm ill-typed, watch it flip); wrong row rejects;
`Eff [State] Nat` hypothesis accepted by H-rule; weakening still free;
`eff_check` and the deep recognizer agree on the straight-line programs from
`effect_proto.test.disp`. The visits-both-arms half is already demonstrated at
the mechanism level in `eff_deep_proto` (the flip pin on `ShapeC`); the stage 0
version restates it through the Eff recognizer itself.

Exit. `EffAt` marked as the shallow compatibility form; new pins use the deep
type. Estimate: one to two sessions; the ShapeR/match chain is where it grows.

LANDED 2026-07-07, route one (the hand knot; route two stays open as optional
cleanup). `eff_body`/`Eff` live in `lib/std/effect.disp`, and
`Eff : Tree -> Type -> Type` is machine-verified at load. Pinned in
`eff_deep_proto` (38 pins, ~22s): the Pure payload checked at X; the
label/arg/k checks each with a rejection twin; the H-rule; weakening free;
agreement with `eff_check` on the composed prog1 (whose continuation computes
`double n` over the minted answer — walkable exactly because the std Nat
helpers are elim-routed); and the exit demonstration, a branchy continuation
whose answer is eliminated through ShapeC's gated respond, certified Ok true
at its row through the Eff recognizer, flipped to Ok false by one ill-typed
arm, and run concretely under a handler (one value, checked and executed).
Hazard 0b did not bite: recognizer_wrap's reconstructed self is
hash-cons-identical to `Eff R X`, so sub-computations land by one H-rule hit
each. Dividend: the deep arg check caught a real ill-typing the shallow form
passes (the M0 proto spelled get's Unit argument as the leaf; Unit's
inhabitant is `unit_val`), pinned as a shallow-vs-deep contrast and fixed in
`effect_proto`.

### Stage 1: the kernel signature and the floor handler

Goal. One op table for exactly the capabilities the walker cannot walk, one
handler interpreting them, clauses wrapping the existing sealed bodies
verbatim.

The design rule that decides membership: sanctioned-pure operations are not
ops. `tree_eq` and plain `pair_fst`-as-reader run natively under the walker
already; putting them in the signature would be noise. The signature is
exactly seal(Σ). Proposed table:

| op | answer | replaces | clause wraps |
|---|---|---|---|
| `classify v` | ShapeR: leaf, stem child, fork l r, hyp stored-type | the SEALED(reflection) reads | `is_hyp_fork` + triage (exists in reflect proto) |
| `mint ty` | a fresh inhabitant of ty | `bind_hyp`, SMint | `bind_hyp` |
| `check ty v` | CheckerResult | the policed sub-check, `do_check`'s route | `param_apply` |
| `extend n ty frame` | the extended neutral | the forge inside `hyp_reduce` | the `wait hyp_reduce {.. Ext ..}` build |

`mint` and `extend` are forge(Σ) as table rows; `classify` is the read side.
Note what is absent: a raw neutral-meta read. `meta_of : Type -> MetaShape` is
already typed and concrete-type metadata is public data, so stage 3's
`hyp_reduce` spec goes through the front door and the extraction leak never
becomes an op.

Hazard 1a, the mint clause meets the bind_hyp compiler wall (updated
2026-07-07: probed empirically, and the wall does not reproduce; ten pins in
`lib/tests/tele_spec_proto.test.disp`). The fear was that
`on mint := {ty, resume} -> param_apply (bind_hyp ty) resume` is the
documented miscompile (a continuation passed through a function into the
bind_hyp route leaks the fresh hypothesis under nested binders), that the
innocent wrapper `{h} -> resume h` eta-collapses right back to `resume`
(bracket abstraction's `[x](f x) = f`), and that the clause would need the
keep trick:

    let keep := {x} -> x        // top-level, so it arrives as a compiled literal
    on mint := {ty, resume} -> bind_hyp ty ({h} -> resume (keep h))

(the inner application blocks the eta, and `keep` arriving as a scope literal
rather than the CIR `I` sentinel blocks the `S (K p) I` collapse). The pins
now say: on the live compiler every tested shape passes, including the
helper-parameter pass (which is byte-for-byte the natural clause), the
eta-trap wrapper, a mid-body let-bound continuation closed over the outer
hypothesis, a continuation referencing a fix `self` reaching the route through
a helper (the walker's historical context), the record-field clause form, and
the keep spelling itself. So either an intervening compiler change fixed the
miscompile (candidates: the 2026-06-12 match desugar closing the cut over arm
free vars, the 2026-07-05 let rework) or it needs a context the minimal forms
still miss. Write the stage 1 clause in the natural shape; keep the keep
spelling in reserve; the seed file is the regression guard either way. Note
CLAUDE.md's inline-lambda rule remains the law for kernel code (tele_walk's
inline mint is not to be relaxed on the strength of these pins). Fallback if
an unpinned context still fights back: a mint-aware `handle` variant that
interprets the mint op inline in its fold, exactly as `tele_walk` does today.
That is doctrinally clean (mint is a floor op; the floor handler is allowed to
be fused) and costs only the claim "the interpreter is a completely generic
handle."

Deliverables. `KernelSig` ops via `mk_op`; `floor_h` via `mk_handler` plus the
mint clause; a ledger table in the file header: op, clause, soundness note,
which SEALED marker it replaces. Pins: each op's request builds over a
hypothesis and typechecks at its row; the handler reproduces the sealed
behaviors on concrete data (classify on all four shapes, mint freshness,
check agreeing with `param_apply`, extend agreeing with a walker-produced
extension tree).

### Stage 2: `tele_spec`, the telescope walker as a program

Goal. A parallel cell vocabulary (four or five cells suffice: mint, apply,
proj, qid, refine) where each cell is an Eff program over KernelSig, plus two
library handlers replacing the mode boolean:

- recognize-handler: interpret mint via the `mint` op, observations via
  `check`, rejection via throw; verdicts Ok true / Ok false.
- respond-handler: interpret mint by routing the frame and landing the
  codomain, observations against the frame, rejection as `Extend InvalidType`.

`tele_spec` itself is a short fold: run the head cell's program, bind, apply
the tail, recurse; generic `handle` or the mint-aware variant per stage 1's
outcome. Zero kernel edits: the parallel cells live in the proto file, built
from the same `wait op meta` shape.

Pins (the differential battery, both faces, against live
`param_apply`/`tele_walk`): Pi simple and dependent-codomain, acceptance and
rejection; Record present/missing/ill-typed field; Refinement; Intersection;
respond on application and projection frames; the adversarial soundness cases
(stem-forge rejection, triage-on-neutral rejection) reproduced through the
spec. Also the headline demonstration pin: the same telescope program run
under recognize-handler and respond-handler produces recognition and
elimination respectively, one program, two interpreters.

Exit. The sentence "a telescope is a free-monad program and tele_walk is its
handler" is a passing test, not an analogy.

### Stage 3: spec twins for the remaining sealed algorithms

Work items:

- `walk_spec` completed: its two delegated routes become the `mint` and
  `check` ops, so its row is the full signature and the walker's special-case
  table has no residue. Coordination note: `reflect_proto.test.disp` and
  REFLECT.md are the other agent's files as of 2026-07-07; extend in a new
  file or coordinate first.
- `hyp_reduce_spec`, the decomposition that makes the last Σ-op's algorithm
  typeable without a leaky meta op:

      hyp_reduce_spec := {n, frame} ->
        eff_bind (classify n) ({s} ->
          match s {
            Hyp storedT => {
              let m := meta_of storedT           // typed, public front door
              match (m.respond (m.recognizer_params) n frame) {
                Extend ty => perform (extend n ty frame)
                Reduce v  => io_pure v
              }
            }
            _ => io_pure (n frame)               // concrete head: plain application
          })

  (Spelled with `match` for readability; the certified form eliminates `s`
  through ShapeR's gated respond per hazard 0a.)

  Differential pins: Pi-hypothesis application, gated Nat elimination, record
  projection, both Action arms, against live `hyp_reduce`.
- `occurs_spec` / `is_closed_spec` as classify-programs (they are folds with
  one shape question per node); retires two more SEALED(reflection) markers
  cheaply. `elim_spec` falls out (concrete dispatch or `hyp_reduce_spec`).
- Rewrite the reflect prototype's tag-test twins onto the elim idiom over the
  ShapeR Coproduct so their certificates deepen (per hazard 0a; surface
  `match` is not the vehicle, motive-carrying eliminations are). For
  `walk_spec` this is a real rewrite of its dispatch if-chains: budget it as
  its own session, in a new file per the coordination rules.

Exit criterion, enumerable: list every `SEALED(...)` in `engine.disp` and
`cut.disp`; each maps to a spec twin or a stage-1 clause. No stragglers.

### Stage 4: rows as the ledger, and the fixed-point pin

Annotate every spec at its row. Add the parametric-fragment pin: one
Reflect-free polymorphic function plus a cheap empirical free-theorem witness
(naturality on a test instance), to make "rows without Reflect are where free
theorems live" a demonstrated sentence. Then the centerpiece, one test file:
`verify` of the spec module returns `Ok true` under the live kernel. Name it the
behavioral fixed-point pin, and write the honest header: this is a capability
certificate plus differential agreement, issued by the artifact being
described; it is the `Type : Type` epistemic shape, on purpose; it is not a
soundness proof of the floor.

### Stage 5: the handler zoo (the payoff demonstration)

Same spec programs, four interpreters, each about a page:

- floor handler (stage 1): the semantics.
- cost handler: parameter-passing counter over ops (the state pattern);
  pin "checking `Pi A B` on this candidate performed 2 mints and 3 checks."
  This is cost-as-coeffect attached to checking itself, empirically.
- explanation handler: thread a trace of (op, answer); on rejection the trace
  is the error report ("rejected: field b failed check at Nat"). The richer
  CheckerError vocabulary, nearly free.
- synthesis handler, the headline: interpret `mint ty` by enumerating
  candidates (multishot resume, the nondet pattern; small-type enumeration
  table: Bool, small Nat, pairs). `tele_spec`-recognize under it becomes a
  generator. Pin: synthesizing at `Pair Bool Bool` yields four values, each
  re-checking Ok true under the floor handler. This is GOALS.md's
  propose-and-check loop as one handler clause, and the strongest single piece
  of evidence the factoring is right.

Pin explicitly: one program, at least three interpreters, listed side by side.

### Stage 6: the bridge, scoped honestly

The spec-to-fused relation stays differential in this prototype. Write the
target down (`guard (license_guard (oeq WalkerT)) param_walker`, proof = a real
equivalence certificate) and stop. Pretending pins are a proof would poison the
ledger's honesty, which is its entire value. The license machinery is the
OPTIMIZER.typ arc.

---

## Part III: Logistics and rules of engagement

### Sequencing and effort

    stage 0   deep Eff recognizer          LANDED 2026-07-07 (route 2 optional)
    stage 1   signature + floor handler    1 session      mint hazard probed: stale
    stage 2   tele_spec + two handlers     1 session
    stage 3   walk/hyp_reduce/occurs specs 1 session
    stage 4+5 ledger, fixed point, zoo     1 session
    stage 6   note only

A convincing core (0, 1, 2, plus cost and synthesis handlers, plus the
fixed-point pin over what exists) is about three sessions.

### File plan (all additive, zero kernel edits)

- `lib/std/effect.disp`: deep `Eff`, canonical rows (rows landed 2026-07-07).
  Additive only; the reflect prototype now depends on this file.
- `lib/tests/eff_deep_proto.test.disp`: stage 0 pins (seeded 2026-07-07: row
  canonicity + the hazard 0a quartet on `ShapeC`; ~20s, 21 pins).
- `lib/tests/tele_spec_proto.test.disp`: stages 1 and 2 (signature, floor
  handler, tele_spec, differential battery), or split if it grows (seeded
  2026-07-07: the hazard 1a empirical pins; ~21s, 10 pins).
- `lib/tests/kernel_spec_proto.test.disp`: stage 3 twins, stage 4 ledger and
  fixed-point pin.
- `lib/tests/handler_zoo_proto.test.disp`: stage 5.
- Docs: a signature-and-ledger section, either in REFLECT.md (coordinate) or a
  new PROTOCOL.md; update this file's status line per stage.

### Coordination (two agents share this checkout)

- REFLECT.md, TOWER.md, `reflect_proto.test.disp`, and the tree_rec work
  belong to the other agent's arc (2026-07-07). Do not edit them without
  coordinating; extend in new files.
- `lib/std/effect.disp` is shared infrastructure now: additive changes only,
  run both `effect_proto` and `reflect_proto` after touching it.
- Commit with explicit pathspecs, always: `git commit -- <files>`. A bare
  `git commit` takes whatever the other agent has staged (this happened once;
  it was rolled back with a soft reset plus a pathspec commit).
- Work directly on main; push with `git push origin main`; never force-push
  without checking `git log origin/main` first.

### Compiler workarounds that will bite here (details in CLAUDE.md)

- Match arm bodies are single-line; wrap multi-line arms in parens.
- Every call in a match arm must carry an arm-bound variable in its spine
  (the eta/saturation rule); if-guarded bodies are safe.
- The `bind_hyp` continuation must be an inline lambda (stage 1 hazard; the
  eta rule can silently undo an inline wrapper, use the `keep` trick).
- No `select_lazy` around recursion; use `if`/`match`.
- Records with lambda fields project fine (`h.on op arg k`); this is how
  handlers already work.

### Budgets and test running

- Run one file: `npm run disp -- lib/tests/<file>.test.disp` (rust-eager-native
  by default).
- Full object-language suite, always solo (the full vitest run can exhaust
  8GB): `NODE_OPTIONS=--max-old-space-size=8192 npx vitest run test/disp.test.ts`.
- APPLY_BUDGET is 40M steps per compile-time evaluation; keep differential
  battery terms small (single-digit Nats, two-field records). Deep Eff checks
  nest `param_apply`; the §7A token flattening makes this viable, but watch
  the suite time and keep per-file pin counts near 25.
- The plain eager backend cannot load the kernel in budget; debug on
  rust-eager-native (see the getBackend pattern in `test/modules.test.ts`).

### Kill criteria (so the prototype can fail honestly)

- If deep continuation typing cannot get past one-path certificates even with
  ShapeR-as-Coproduct and match-routed branches, the row modality stays
  advisory; stages 3 and 4 lose their teeth. Stop, write up why, keep the
  shallow form.
- If the mint clause cannot be made safe in either form, `tele_spec` keeps
  mint fused forever and "modes are handlers" weakens to "non-binding modes
  are handlers." Still worth having; the docs must then say exactly that.
- If the differential battery finds a real spec/fused divergence that is not a
  spec bug, that is a kernel finding; stop and report before continuing.

### The one-paragraph summary to re-anchor on

Types and effectful programs are the same protocol shape: ask a question, bind
the answer, continue. The telescope walker was always a handler and the Step
type was always its effect signature. REFLECT.md showed that inspection
becomes typeable when you split asking (typed, a program) from looking (the
handler, one sealed clause at the floor). This plan finishes that split for
the whole kernel: a deep `Eff` recognizer so certificates mean something
(stage 0), one kernel signature with one floor handler (stage 1), the
telescope walker and the Σ-ops as typed spec programs differentially pinned
against their live fused forms (stages 2 and 3), rows as the trust ledger with
a behavioral fixed-point pin (stage 4), and alternative interpreters (cost,
explanation, synthesis) to prove the factoring pays (stage 5). What remains
trusted at the bottom is conserved but transformed: an enumerated table of
handler clauses instead of scattered SEALED comments. The fixed point is
behavioral, the rows certify capability rather than correctness, and the floor
never disappears; knowing its exact shape is the achievement.

### Glossary

- hyp / neutral: a fresh symbolic value standing for an arbitrary inhabitant
  of a type during checking; remembers its stored type; opaque by design.
- walker (`param_walker` / `param_apply`): the interpreter that runs user code
  over hyps while rejecting inspection of them; soundness lives there.
- respond: the per-type rule for what happens when a stuck value of that type
  is eliminated (stay stuck at a new type, or compute).
- H-rule: a neutral inhabits a type exactly when its stored type is that type
  (the O(1) shortcut that lets stuck values travel through checks).
- knot / `make_rec_recognizer`: how a recursive type's recognizer refers to
  the type being defined (`self` is threaded in by the wrapper).
- row: the set of effect labels a computation may perform; checking is
  containment, so weakening is free and handling discharges an entry.
- carve-out: an operation the walker runs natively on hyps because it is
  sanctioned (tree_eq, the root-signature read).
- seal(Σ) / forge(Σ): the spec's sets of trusted token producers and
  constructors; under this plan, the floor handler and its mint/extend ops.
- fused vs spec: the live optimized form (hand-written, sealed) vs the typed
  readable algorithm; related by differential pins now, a license later.
- pin: a test asserting a behavior so regressions and design claims are
  falsifiable; the unit of truth in this repo.
- floor: the code that must perform raw inspection for anything else to be
  checkable; the trusted residue; here, the handler clause table.
