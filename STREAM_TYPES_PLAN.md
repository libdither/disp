# Stream types: endings, spaces and telescopes, the contract as a claim

Status (2026-10-06): Part 1 is landed (`lib/stream.disp`, `lib/verdict.disp`,
`lib/tests/stream.test.disp`) on a general coalgebra core (see the end of Part 1). Part 2
(spaces and telescopes) is superseded: `research/CLAIMS_OVER_TREES.md` removed the
domain, the relation and the telescope as separate things (a claim is a test on bounded
runs under quantifiers over all trees), and its §9 landed `lib/claim.disp`, where a
checker is a lens into the honest claim and Part 3's contract is the lens law. What is
left of Part 3 is the symbolic machine. Delete this file once that part is code with
tests that say what its section says.

## What this is for

The stream layer (GRADUAL.disp, `lib/stream.disp`, the refuter and confirmer in
`lib/tests`) already has the pieces of a semantic type system: a type is a sameness
relation whose diagonal is the recognizer (TYPES.html §3), a quotient is a coarser
relation on the same trees (§4), the honest checker is the witness stream over an
enumeration (§5, GRADUAL §6), a grade is the algebra a stream is folded with (GRADUAL
§8), and every faster checker is a shortcut whose contract is agreement with that stream
(§9). Three things are missing, and each one is a place where today's code trusts a
caller or a comment instead of a value:

- A stream that ends says nothing about *why*. The refuter's `refute` takes an
  `exhaustive : Bool` the caller vouches for (`lib/tests/refuter.test.disp`), GRADUAL §7's
  `verdict` peeks at `bounded`'s fuel through `s_rest.snd.fst`, and IDEAS.disp's `Arrow`
  answers `true` both when it ran out of members and when it ran out of budget. "Proved"
  is only sound in the first case, so the ending has to be a fact the stream carries.
- The type formers are written one at a time: `witnesses_over` (GRADUAL) is a Pi over a
  diagonal, `refute` is a Pi over related pairs, `Arrow` is a Pi over a budget, and there is
  no Sigma, no Eq and no dependent binder at the stream level at all. The kernel already
  has the answer in another guise: `Tele table code` is its one negative former and
  `Pi`/`Sigma`/`Isect` are `Tele` at a code (`lib/kernel/kernel.disp:604-608`). The stream
  layer needs the same former, with a stream where the kernel's `Enum` table has a member
  list.
- The contract "every shortcut agrees with the honest run" is property-tested claim by
  claim (`agrees` and the `sweep` at the end of `lib/tests/confirmer.test.disp`). It is
  itself a claim over a stream of claims, and once the telescope exists it can be one
  witness stream whose verdict is read off like any other. The confirmer's search also
  re-runs a whole check under every case split; with the machine of `lib/machine.disp`
  generalised to partly-known values it resumes the paused run instead.

Each part is useful alone and the order matters: spaces need endings for their verdicts,
telescopes need spaces, the contract needs telescopes.

## Part 1: how a stream ended (landed)

As designed, with the encoding since replaced (below): `bounded` answers `spent` at
zero fuel without stepping the inner (so a finite source whose fuel is exactly its
length reads spent, pinned as the conservative reading) and every combinator passes an
ending through; the merge and its two policies live in `lib/stream.disp`, a policy
playing a `round` (found items, survivors, dropped_spent); `verdict` reads the ending;
the refuter's `refute` lost `exhaustive`; GRADUAL §7 reads the ending and §8's
`s_fold_absorb` folds a spent ending to its `unknown` element (`open` is a keyword). Deviations from the plan: the verdict vocabulary is its own module,
`lib/verdict.disp` (`refuted`, `open_on`, `is_proved`/`is_refuted`/`is_open`, `flat`,
`agrees`, and `agree` between two verdicts), and the shared recognizers, normalizers
and subject programs the roots kept re-spelling are `lib/tests/fixtures.disp`. Not
done: the confirmer's `agrees` lines and sweeps still compare against `Arrow`'s Bool;
switching them to `verdict` of a witness stream needs `witnesses_over` in the library,
which is Part 2's `pi_claim`, so that move belongs to step 3.

The coalgebra rework (2026-09-19): a stream is `Coalgebra F`, a step-shape map, a
stepper and a state (`c_new`), observed one layer at a time by `c_out` and consumed by
`c_fold`, whose algebra gets a `recurse` it may skip calling (the early exit). Two step
shapes: `Emit T` (an item and a successor) and `Step R` (`finished result | running
next`), and three named instances over them: `Stream T` (infinite: `nats`, IDEAS'
`Trees`), `Task R` (a run that finishes with an answer, sequenced by `task_then`) and
`Items V` (`Step Ending (Emit (Option V))`: a step finishes with `exhausted` or `spent`,
or runs on carrying `yield item` or `skip`). `as_items` lifts a `Stream` and `answers` a
`Task`.

Stopping is a combinator, not a shape. `c_until` reads a rule on the state before
stepping and `c_after` reads one on the step just taken; every ending in the library
comes from one of the two (`s_from_list` at nil, `c_bounded` at zero fuel, `task` when
the machine finishes, `answers` after its yield, the merge off a latch in its state), so
no stepper can mint an ending of its own. `bounded` is `c_bounded` plus reading the
budget's halt as `spent`; `s_map`/`s_filter` are per-layer maps (`c_hoist`), which leave
states alone and so cannot touch an ending.

Items do NOT promise to stop: the type says only that IF a stream finishes, it finishes
with an ending. A stopping claim is a `measure` on states that should drop each draw:
`stopping` carries one, `stopping_fuel` turns it into a budget and `settle` spends it, so
a wrong measure finishes spent (Open), never a false Proved. Because `c_hoist` never
changes a state, `stopping_map`/`stopping_filter` carry the same measure for free.
## Part 2: spaces and telescopes (superseded)

The design here (a space as a relation `Tree -> Tree -> Bool`, members and pairs derived
by tasks over a source, rows and environments, `pi_claim`/`eq_claim`) is kept in git
history (`git show e30c3cb75318:STREAM_TYPES_PLAN.md`). It was replaced before being
built: `research/CLAIMS_OVER_TREES.md` §1–§2 states a binder as "for every tree x and
budget n, if the recognizer accepts x within n", so a relation is a claim about
pair-shaped trees and a telescope is a prefix of quantifiers, with no second former. The
decisions that survive are in `lib/claim.disp`: the source of challenges is the claim's
(`honest` takes it as an argument), a finite source is a certificate (`over`, and the
cover lens whose law says every tree goes to a listed challenge that fails whenever it
does), and a normalizer or generator is a shortcut whose contract is a lens law, not an
adjoint. `research/STREAM_TYPES_ONTOLOGY.md` §6 lists the corrections to the old
section's claims, and they stand.

## Part 3: the contract as a claim, and the symbolic machine

### The contract as a claim

Landed in one form: `lens_holds` (`lib/claim.disp`) is the contract of one shortcut as a
claim the honest check can refute, and `lib/tests/claim.test.disp` runs it for the
certified rewrites. What follows is the per-checker version over a space of claims,
still written in Part 2's vocabulary (draft: not yet restated in claim terms, where it is
`lens_holds` quantified over the claims as well).

"Checker `C` is sound" says: no claim `C` proves has a witness, and every witness `C`
reports re-runs as one. Both halves are ∀ over pairs (claim, candidate), so soundness is
a Pi over the two-row telescope `[claims, candidates of the claim's domain]` with the
observation "not (`C claim == "Proved"` and the candidate refutes the claim)", and the
reported-witness half is decided per claim by re-running. The candidate under check is
the checker, and the claim space is finite where it can be (arrow claims over `Bool` with
IDEAS.disp's `candidates` as functions), so the verdict is a real `"Proved"` by exhaustion
there and `"Open"` with the sweep's counts over `Nat` claims drawn from `Trees`. This
replaces the `agrees` lines and the `sweep` of `lib/tests/confirmer.test.disp` and the
tree sweep with one witness stream per checker (`check_arrow`, `eq_arrow`, and the refuter
against `Arrow`), in a new root `lib/tests/contract.test.disp` that opens the confirmer
the way `types_doc.test.disp` does. The old lines stay until the new verdicts match them.

### The symbolic machine

The confirmer's evaluator `sapply_s` is GRADUAL's `run_s` over partly-known values: fuel
threaded through sub-runs, plus the clauses a symbol adds (a splittable symbol in a shape
position is a `need`, a hypothesis token applies the induction licence, a declared
normalizer on a sub-symbol makes a neutral term). Its search `search_r` re-runs the whole
check under each alternative of a split. The machine of `lib/machine.disp` over values
instead of trees gives the same evaluator as a paused state: the same three frames with
`Val` payloads, `Known` operands dispatching by the tree rules, `HalfStem`/`HalfFork`
operands by their shape, and a fourth terminal state:

```
sm_need :: {s : Val, f : Val, x : Val, stack : List MFrame} -> SMachine :=> pair "Need" (pair s (pair (pair f x) stack))
```

Resuming under an alternative substitutes it for the needed symbol in the registers and
every frame (`subst` from the confirmer, mapped over the state) and continues. The search
then costs one continuation per alternative instead of one full re-run per alternative
per depth, and the dispatch count comes out of the machine for free, which is the cost
unit a graded type will want later. The token, normalizer and neutral-term clauses become
dispatch cases of the symbolic step.

### The run lemma as a bisimulation, and why it is the adjoint

The symbolic machine and the concrete one are related by abstraction (a candidate becomes
a symbol) and concretization (`subst` puts a candidate back), and the abstract system is
sound exactly when the second undoes the first on every run: whatever the symbolic run
proves, every concrete run agrees with. That is the adjoint condition between the two
systems, and its unit is a family of PATHS, one per candidate — the lockstep trace of the
two machines. So state the run lemma as a bisimulation instead of an end-to-end
comparison, with "substitute the symbol" as the relation on states, the shape the
confirmer's `stream_eq` already takes (a relation between stream states, supplied as an
argument):

```
// the two machines as streams of states, related by subst: one step each, forever
sm_states :: {m : SMachine, quantum : Nat} -> Stream SMachine :=> stream_new ({st} => emit st (m_advance st quantum)) m
lockstep :: {sm : SMachine, s : Val, a : Tree} -> Tree -> Tree -> Bool :=>
	{x, y} => (subst s (known a) x) == y
```

Two claims replace the end-to-end differential, and together they are the proof the
property test only gathers evidence for:

1. **Per rule, by exhaustion.** For each of the machine's dispatch rules (K, S, triage,
   and the symbolic clauses), "if it fires on a symbolic state it fires identically after
   any substitution" is a `Pi` over a FINITE source of rule shapes — Part 2's finite
   spaces — so its verdict is a real `"Proved"`, not an `"Open"` with counts.
2. **Along the trace, by propagation.** If the states are related before a step they are
   related after it: the same "it propagates through every step" law Part 1 pinned for
   endings, one level up. The bisimulation is uniform in the candidate, which is what
   turns per-candidate evidence into a statement about every candidate — the placeholder
   trick applied to a PAIR of machines.

Trust bottoms out in one place, as before: the machine's step relation is the spec
(`test/machine.test.ts` pins `m_advance`'s dispatch count against the TypeScript
evaluator). Everything above it is a claim the stream layer can state. This is the first
place in the repo where a path does real work, and it needs no path-carrying space: these
paths are traces over `lib/machine.disp` states, already streams. A coinductive `Space`
would only be needed to quantify OVER such paths as members of a type ("the space of
sound abstractions"), which nothing here does.

Where it lives is a decision: the confirmer's value vocabulary (`Val`, `Res`, codes,
`subst`, `judge_as`) sits in a test root today. The machine over it wants a module, so the
proposal is to promote that vocabulary and the evaluator to `lib/symbolic.disp` with the
confirmer keeping its tests as the root; the alternative is to keep everything in the
root and accept that nothing else can use it.

### Hazards the kernel's checker already hit

The kernel (`lib/kernel/`) proves claims with a symbolic checker too, and its annotation
plan (`archive/ANNOTATION_MIGRATION_PLAN.md`, the trust facts and the boundaries) records
where that went wrong. The confirmer re-found two of them on its own: its hole G is the
kernel's same-typed-binder aliasing (bb6c7ffa), and its hole B is the kernel's typed
equation ledger, which refuses a coarse equation where a fine one is needed. The rest, as
a checklist for the symbolic machine:

- **A symbolic answer must survive substitution.** The kernel's rule: an operation may act
  on a stand-in only if running it symbolically and then substituting any value equals
  substituting first and running concretely. Every hole it found was a place a stand-in
  reached plain evaluation: a recognizer run by the judge, a sampling filter, a check that
  read a field off a stand-in. Here that rule is the run lemma's per-rule claim, checked by
  exhaustion instead of audited by hand, and the first places to look are the same: any
  user code (a recognizer, a normalizer) run on a value with a symbol in it.
- **Two unknowns are never unequal by encoding.** The kernel twice baked in an answer
  computed from stand-ins' own trees: an `Eq` recognizer that compared endpoints outside
  its binder, and a `tree_eq` inside a type family that still answers natively. The
  confirmer's canonical encoding makes equal `Val`s the same unknown, which licenses only
  `true`; its guard for the rest is that `"No"` is reserved for a ground counterexample,
  and the symbolic machine must keep that guard.
- **A supplied table is a certificate, and the kernel's was never checked.** The kernel's
  `teq_one` trusts that a type's member list is complete, and a Bool whose eliminator
  always takes one branch passed every coherence check it had (`GoodGate`, `Coherent`).
  Decision 2 already treats a finite source as a certificate the claim carries; the
  confirmer's split `table` is the same assertion, hand-written for four codes. Before
  tables come from spaces, make completeness a claim the stream layer can refute: a tree
  the recognizer admits that no alternative of the table matches.
- **A law the checker acts on must be proven, not sampled.** Once the kernel converted
  types along recorded equations, a false equation that passed its probes would rewrite
  types, where a false check only misjudges; so its laws stayed inert tests until they
  carried proofs. Here the laws that act are the certificates behind `.opt.disp`
  overwrites and, later, paths: a sampled one would swap the readable definition for a
  wrong fast one everywhere downstream.
- **Stand-ins must be unforgeable.** Kernel stand-ins were trees, so a candidate could
  build one, and the defense grew into a ledger of issued stand-ins plus two walkers (the
  checker's, which sees stand-ins, and the candidate's, which must not). The confirmer's
  symbols live outside the tree encoding (a candidate's tree is always `Known`), which
  removes the class; keep it that way when the vocabulary moves to `lib/symbolic.disp`.
- **A test can pass for the wrong reason.** When the kernel closed its equation-ledger
  hole, a pinned result flipped: it had held because of the hole, not for the reason its
  comment gave. When a hole closes here, re-derive every test in its family.

## Steps

1. Endings: landed (see Part 1).
2. (Superseded with Part 2.) Spaces. `lib/space.disp`: `data_space`, `quot_space`, `isect_space`, `s_bind`,
   `sp_members`/`sp_pairs` over a source; the refuter's pair streams become `sp_pairs`
   over `Trees.as_items.bounded(fuel)` and over `bools`. Verification: the refuter's tests with
   the same outcomes (budgets retuned), `sp_members LoopR Trees.as_items` yields `0`,
   `sp_pairs (quot_space NatR parity)` yields `(2, 0)` and `(0, 2)`, decision 3's
   ending tests for one space.
3. (Superseded with Part 2.) Telescopes. `lib/tele.disp`: `Env` and its accessors, rows, `s_bind`, `tele_envs`,
   `pi_check`, `tele_witnesses`, `eq_claim`, `pi_claim`, Sigma members and the
   `observe_row` fold, Isect; GRADUAL §6 and the refuter's `refute_arrow` become
   instances; the confirmer's `agrees` lines and sweeps switch to `verdict` of a
   `pi_claim` (deferred from step 1); the ∀∃ test; decision 3's merge-of-merges
   tests; the kernel-agreement root. Verification: GRADUAL and refuter outcomes
   unchanged; `boolfns` verdicts agree with `Tele Enum`.
4. The contract. `lib/tests/contract.test.disp`: the soundness claim per checker,
   `"Proved"` over the finite claim space, `"Open"` with pinned counts over `Nat`; then the
   old `agrees`/`sweep` lines retire. Verification: the harness.
5. The symbolic machine. `lib/symbolic.disp` (or the confirmer root): the machine over
   `Val`, `sm_need`, resumption in `search_r`, and the confirmer's whole test list
   unchanged. The run lemma as a bisimulation rather than an end-to-end differential:
   `sm_states`, `lockstep`, the per-rule claim over a finite source of rule shapes
   (`"Proved"` by exhaustion), and the propagation claim along the trace, with a failing
   lockstep reporting the first unrelated state pair instead of a differing final result.
   Verification: verdicts identical to the from-scratch search on every confirmer test;
   the per-rule claim `"Proved"`; the machine on all-`Known` inputs in lockstep with
   `lib/machine.disp`; dispatch counts before and after, reported.
6. Follow-ons, each its own plan: grades as semiring folds with the machine's count as the
   cost unit; records for spaces and rows once the stream layer can open the kernel; a
   native stepper for the symbolic machine; and paths, below.

   **Paths.** A `Space` here is a setoid: `same a b` is a bit, so `"Proved"` sameness
   carries nothing, the way a level-0 `"Proved"` carries nothing until a certificate
   does. A path is the re-runnable witness of sameness, the positive twin of a
   refutation: a transformation from `a` to `b` given as a STREAM of local steps, each
   step one cheap check (`s_all (path.bounded(fuel)) step_ok`), with the ending saying
   whether it arrived. Every type the three parts test is a set (any two paths `3 ~ 3`
   are the same), so paths are invisible there; they appear for quotients by a rewrite
   relation with no normalizer (sameness is "some rewrite path exists", a search
   stream), for stream sameness (the confirmer's `stream_eq` already takes the
   bisimulation relation as an argument: that relation is the path), for types up to
   equivalence (the path is the pair of round-trip programs, and claims transport along
   it), and above all for the optimizer of GOALS.md: the certificate of an `.opt.disp`
   overwrite is a path from the readable definition to the fast one, and it is what
   turns Part 3's "the shortcut agrees with the honest run" from a claim into a receipt
   whose checks never re-run the honest stream. The first real path is EARLIER than that
   certificate: it is Part 3's run lemma read as a lockstep bisimulation, the unit of the
   adjoint between the abstract checker and the stream one. It needs none of this
   follow-on's machinery, because a trace is already a stream; what starts this follow-on
   is the first type whose MEMBERS are paths. Its cost (the path's length, or the machine's
   dispatch count along it) is a grade, which is the distance between two programs at a
   type and the quantity the optimizer descends on. When it lands, `Space` becomes the
   `prop_space` case of the coinductive `Tree -> Tree -> Pair Bool Space` (the Bool is
   relatedness, the tail the space of paths, `triv` the contractible tail that makes
   today's spaces the 0-truncation); `sp_related` reads the Bool and nothing else in
   Parts 2 and 3 moves. Coherences (paths compose, composition is associative up to a
   2-path, two rewrite orders reconverge) are not part of the object: each is a ∀∃
   claim, `Open` at the stream level by construction and discharged only by the
   certificate rung, so a "homotopy space" is a space plus a bundle of such claims and
   truncation is the honest default. Order independence of rewrites (confluence, the
   interaction-net evaluator's licence) is the one 2-path claim expected to carry weight.

   Transport has one hard limit, found by the kernel's equality plan
   (`archive/OEQ_PLAN.md`, "The constraint that shapes everything"): a claim that may
   compare trees structurally cannot be transported along a path between two different
   trees. A path from `id` to a different tree that agrees with it on every input would
   carry the claim `f == id` from true to false. So transport is licensed only for claims
   that do not inspect the trees they are about, and deciding which claims those are
   comes before the first transport lands.

## Risks and non-goals

- `is_done` changes meaning for every consumer. "Not a fork" is the conservative reading
  (a stem was never a step before), and the propagation law is where a mistake hides: a
  combinator that answers a fresh `done` turns a budget into a proof. The stream root's
  laws are the gate, and every consumer that ends a stream is listed in step 1.
- The merge's policy contract changes (it must report a dropped spent inner). Both
  policies are in GRADUAL.disp today; moving them into the library is the moment to do it.
- Name clashes with the kernel (`cons`, `Space`, `Row`) block a single-root differential
  until the selective import is tried; the fallback is pinned lists.
- ∀∃ claims stay `Open` by construction. This is a property of stream verdicts, not a
  budget to raise, and the plan documents it rather than working around it.
- Resumption in the symbolic machine is only as sound as the run lemma, and the clauses
  `sapply_s` adds for symbols must all be mirrored; the from-scratch search is the oracle.
- No native acceleration of the symbolic machine, no records in the stream layer, no
  grade beyond the ending, and no path-carrying spaces or coherence claims in this plan:
  the setoid `Space` is exact for every type Parts 2 and 3 test.
