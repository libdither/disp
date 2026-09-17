# Stream types: endings, spaces and telescopes, the contract as a claim

Status (2026-09-16): plan, checked against the code it touches; nothing is landed. Its
three parts are the first three items of the roadmap that followed the resumable machine
(`RESUMABLE_INTERP_PLAN.md`): a grade on how a stream ended, the telescope as the one
stream former, and the checkers' contract written as a claim the stream layer can run.
Delete this file once each part is code with tests that say what its section says.

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

## Part 1: how a stream ended

### Today

A step is `done` (the leaf), `yield item state` (a fork whose left is `true`) or
`skip state` (a fork whose left is `false`); `is_done` is `is_leaf` (`lib/stream.disp`).
`bounded` answers `done` when its fuel is zero, and every combinator answers a fresh `done`
when its inner is done, so the reason is gone one combinator up.

### Design

Two ways to end, told apart by shape and nothing else:

```
done :: Step V St := t
// the budget ended the stream: nothing is known about the rest
spent :: Step V St := t t
is_done :: Step V St -> Bool := {s} => not (is_fork s)
is_exhausted :: Step V St -> Bool := is_leaf
is_spent :: Step V St -> Bool := is_stem
```

`spent` is a stem, so every consumer that checks `is_done` before reading `yielded`
(all of them, by the library's own rule) keeps working unchanged, and a consumer that
never asks why treats both endings alike. The ending is re-observable: a stream's rest
after `s_take` steps to the same ending again, so `taken.s_rest.s_step.is_spent` is the
question, on any stream, with no peek into anyone's state.

The one law: an ending propagates. `bounded` answers `spent` at zero fuel and passes an
inner's ending through (`if (step.is_done) { step }`, not `{ done }`); `s_map`,
`s_filter` and `s_take` pass it through the same way; `s_all` stays two-valued and is
documented as evidence (its `true` on a spent stream is "no counterexample seen"). The
fair merge is the only combinator that can hide an ending: a policy drops an inner that
is done, so a policy must report whether the inner it dropped was spent, and the merge
keeps a taint bit in its state and ends `spent` when the source ended spent or any inner
did. The merge moves from GRADUAL.disp into `lib/stream.disp` on this occasion, with its
two policies and `s_from_list` (defined identically in GRADUAL.disp and the refuter).

The three-valued verdict is then a library function, not a peek:

```
verdict :: {ws : Stream Tree} -> Tree :=> {
	let taken := ws.s_take(1)
	if (not taken.s_items.is_nil) { pair "Refuted" taken.s_items.head }
	else if (taken.s_rest.s_step.is_spent) { "Open" }
	else { "Proved" }
}
```

GRADUAL §8's `s_fold_absorb` gets the fourth element of the algebra, what an exhausted
ending and a spent ending fold to (`one` and a new `open`), and §7 reads the ending
instead of the fuel. The refuter's `refute` loses `exhaustive`: `diag_pairs` over
`Trees.bounded(fuel)` ends spent and answers `open_after`, `bool_pairs` over
`s_from_list` ends exhausted and answers `"Proved"`, which are the outcomes its tests
already pin. IDEAS.disp keeps `Arrow` as the Bool it is; the sweeps that compare
shortcuts with it (confirmer `agrees`) switch to `verdict` on the witness stream, so
that the comparison is three-valued on both sides.

Tests live in a new harness root `lib/tests/stream.test.disp` (a raw open runs none of a
module's inline tests, so `lib/stream.disp`'s own tests never run under `npm test`): a
finite list under `bounded` ends exhausted, `nats` under `bounded` ends spent, a merge
with one spent inner ends spent, `s_map`/`s_filter`/`s_take` preserve either ending, and
`verdict` on the three shapes.

## Part 2: spaces, and the telescope as the one stream former

### Today

GRADUAL's witnesses run a candidate over the diagonal of a domain; the refuter runs it
over related pairs so that a quotient domain checks respect (`quot_pairs` gives
`(a, nf a)`); both take the domain as a recognizer and the codomain as a relation, and
neither has a second binder. The kernel's `Tele` is a row program (`PiCode A B` is
"fresh an A, observe the candidate applied to it at `B x`"; `SigmaCode` observes the
candidate's shape and both projections) run under a table whose `fresh` says where
members come from: `Guard` mints a hypothesis (the placeholder run), `Enum` lists
`ty.members`.

### Design: a space

A space is what a binder ranges over, with the three faces the checkers use:

```
space :: {recognize : Tree -> Bool, same : Tree -> Tree -> Bool, pairs : Stream (Pair Tree Tree)} -> Space :=>
	pair recognize (pair same pairs)
sp_recognize :: Space -> Tree -> Bool := pair_fst
sp_same :: Space -> Tree -> Tree -> Bool := {sp} => sp.snd.fst
sp_pairs :: Space -> Stream (Pair Tree Tree) := {sp} => sp.snd.snd
// a data type: sameness is identity, the pairs are the diagonal of its members
data_space :: {recognize : Tree -> Bool, members : Stream Tree} -> Space :=>
	space recognize ({x, y} => ((recognize x) && (recognize y)) && (x == y)) (members.s_map({a} => pair a a))
// a quotient: the same trees, related through the normalizer
quot_space :: {base : Space, nf : Tree -> Tree} -> Space :=>
	space base.sp_recognize ({x, y} => nf x == nf y) (base.sp_pairs.s_map({p} => pair p.fst (nf p.fst)))
```

Members are the left components of the pairs. A finite type brings its own generator
(`data_space Bool (s_from_list [true, false])`, exhausted-ending); an infinite one is
`Trees.bounded(fuel).s_filter(recognize)`, spent-ending. This is the refuter's
`rel_of`/`diag_pairs`/`quot_pairs` as one value per type, and `Space` uses the same
field names as the kernel's (`#recognize`, `members`) so the two can be matched up later;
pairs rather than a record because the stream layer opens only the raw prelude, where
record literals are not available.

### Design: the telescope

A telescope is a list of rows, mirroring the kernel's: `fresh_row A` binds one related
pair from a space that may depend on the environment so far, `observe_row at T` judges a
value computed from the environment and the candidate at a space. Environments are lists
of pairs, newest first. Two folds:

- The **environment stream** (the ∃ face, what a Sigma's members are): every `fresh_row`
  extends the environments by the dependent fair merge, which is `s_merge_with` over a
  stream of streams. With `s_bind source inner policy := s_merge_with (source.s_map(inner)) policy`:

```
rec tele_envs :: {rows : List Row, envs : Stream Env, policy : Tree} -> Stream Env :=>
	if (rows.is_nil) { envs }
	else if (rows.head.fst == "Fresh") {
		let A := rows.head.snd
		tele_envs rows.tail (s_bind envs ({env} => (A env).sp_pairs.s_map({p} => cons p env)) policy) policy
	}
	else { tele_envs rows.tail envs policy }
```

- The **witness stream** (the ∀ face, what a claim is): one task per environment, the
  task running the observations two-sided, and the merge of the tasks filtered to the
  failures. The core check takes two candidates, because sameness at an arrow is judged
  on related inputs: `f` is run along the left components and `g` along the right ones,
  both outputs must be members of the codomain space and related by its sameness.
  Membership is the diagonal, `f` the same as itself (TYPES.html §3), and equality of
  two programs at a type is the same check with `g` different:

```
// the Pi check under one environment: outputs are members, and related
pi_check :: {cod : Env -> Space, f : Tree, g : Tree, env : Env} -> Bool :=> {
	let sp := cod env
	let out_l := reduce ({p, acc} => acc p.fst) f env
	let out_r := reduce ({p, acc} => acc p.snd) g env
	((sp.sp_recognize out_l) && (sp.sp_recognize out_r)) && (sp.sp_same out_l out_r)
}
tele_witnesses :: {rows : List Row, check : Env -> Bool, policy : Tree, quantum : Nat} -> Stream Env :=> {
	let envs := tele_envs rows (s_from_list [[]]) policy
	let tasks := envs.s_map({env} => (task ({e} => not (check e)) env quantum).s_map({b} => pair env b))
	(s_merge_with tasks policy).s_filter({p} => p.snd == true).s_map({p} => p.fst)
}
eq_claim :: {rows : List Row, cod : Env -> Space, f : Tree, g : Tree, policy : Tree, quantum : Nat} -> Stream Env :=>
	tele_witnesses rows ({env} => pi_check cod f g env) policy quantum
pi_claim :: {rows : List Row, cod : Env -> Space, f : Tree, policy : Tree, quantum : Nat} -> Stream Env :=>
	eq_claim rows cod f f policy quantum
```

`reduce` is the right fold of `lib/list.disp`; over a newest-first environment it applies
`f` to the oldest binder first. GRADUAL's `witnesses_over src A B fn policy quantum` is
`pi_claim [fresh_row ({_env} => data_space A src)] ...` with `B` folded into the codomain
space, and the refuter's `refute_arrow` is `verdict` of the same claim over a
`quot_space` when the domain is a quotient; their tests keep their outcomes. Sigma's
members are `tele_envs` of its two rows mapped to pairs; Isect is a space whose
recognizer is the conjunction and whose pairs are the first space's filtered by the
second; Eq at a type is `eq_claim`.

### The boundary this fixes

Every `fresh_row` is a ∀ in the witness reading, and stacking them stays a single ∀ over
the environment stream, so any number of dependent binders is one refutable claim. An ∃
enters only through a space whose recognizer is itself a search: a codomain "in the
image of g" makes each task look for a `b` it can never rule out, so the claim is ∀∃ and
the stream layer answers `Open` there by construction, however long it runs. That is the
honest limit of stream verdicts, refutable claims and confirmable members, and the
certificate rung (the confirmer) is the only way past it. One test pins it: a
surjectivity claim over `Trees` whose verdict is `"Open"` at every budget tried.

### Agreement with the kernel

At a finite space the kernel's `Tele Enum (PiCode A B)` and `verdict (pi_claim ...)` must
agree (`"Proved"` iff the recognizer answers true) on IDEAS.disp's `boolfns`. A single
root that opens the stream layer raw and the kernel checked collides on `cons` (the
kernel's takes a type argument) and on the annotation names `Space`/`Row`; the kernel's
barrel is opened with a selective name list (`lib/kernel/prelude.disp` does this for
`kernel.disp`), so the differential root imports only `Tele`, `PiCode`, `SigmaCode`,
`Enum` and the finite spaces. If the selective import does not isolate the clash, the
fallback is the same expected verdict list pinned in a kernel test root and in the
stream root, the way the confirmer's `sweep` pins its counts.

## Part 3: the contract as a claim, and the symbolic machine

### The contract as a claim

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
every frame (`subst` from the confirmer, mapped over the state) and continues. This is
sound by the run lemma TYPES.html §9 already relies on: every rule that fired examined a
known part, so it fires identically after substitution, and the paused state after
substitution is the state the from-scratch run reaches after the same dispatches. The
search then costs one continuation per alternative instead of one full re-run per
alternative per depth, and the dispatch count comes out of the machine for free, which
is the cost unit a graded type will want later. The token, normalizer and neutral-term
clauses become dispatch cases of the symbolic step, with the differential against the
from-scratch `search_r` as the gate.

Where it lives is a decision: the confirmer's value vocabulary (`Val`, `Res`, codes,
`subst`, `judge_as`) sits in a test root today. The machine over it wants a module, so the
proposal is to promote that vocabulary and the evaluator to `lib/symbolic.disp` with the
confirmer keeping its tests as the root; the alternative is to keep everything in the
root and accept that nothing else can use it.

## Steps

1. Endings. `lib/stream.disp`: `spent`, `is_exhausted`, `is_spent`, `is_done` as
   "not a fork"; `bounded`, `s_map`, `s_filter`, `s_take` pass endings through;
   `s_from_list`, the merge, its two policies (reporting a dropped spent inner) and
   `verdict` move in from GRADUAL.disp; `s_fold_absorb` gets its `open` element. The
   refuter drops `exhaustive`; GRADUAL §7 reads the ending; `agrees` in the confirmer
   compares against `verdict`. New root `lib/tests/stream.test.disp` with the propagation
   laws. Verification: every root's verdicts unchanged, the harness, GRADUAL.disp.
2. Spaces. `lib/space.disp`: `space`, `data_space`, `quot_space`, a finite space from a
   list, a `Trees`-backed space with fuel; the refuter's pair streams become `sp_pairs`
   of spaces. Verification: the refuter's tests with the same outcomes.
3. Telescopes. `lib/tele.disp`: rows, `s_bind`, `tele_envs`, `pi_check`,
   `tele_witnesses`, `eq_claim`, `pi_claim`, Sigma members, Isect; GRADUAL §6 and the
   refuter's `refute_arrow` become instances; the ∀∃ test; the kernel-agreement root.
   Verification: GRADUAL and refuter outcomes unchanged; `boolfns` verdicts agree with
   `Tele Enum`.
4. The contract. `lib/tests/contract.test.disp`: the soundness claim per checker,
   `"Proved"` over the finite claim space, `"Open"` with pinned counts over `Nat`; then the
   old `agrees`/`sweep` lines retire. Verification: the harness.
5. The symbolic machine. `lib/symbolic.disp` (or the confirmer root): the machine over
   `Val`, `sm_need`, resumption in `search_r`, a differential of the machine on all-`Known`
   inputs against `lib/machine.disp`, and the confirmer's whole test list unchanged.
   Verification: verdicts identical to the from-scratch search on every confirmer test;
   dispatch counts before and after, reported.
6. Follow-ons, each its own plan: grades as semiring folds with the machine's count as the
   cost unit; records for spaces and rows once the stream layer can open the kernel; a
   native stepper for the symbolic machine.

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
- No native acceleration of the symbolic machine, no records in the stream layer, and no
  grade beyond the ending in this plan.
