# Stream types: endings, spaces and telescopes, the contract as a claim

Status (2026-09-16): Part 1 is landed (`lib/stream.disp`, `lib/verdict.disp`,
`lib/tests/stream.test.disp`); Parts 2 and 3 are plan, checked against the code they
touch. The three parts are the first three items of the roadmap that followed the
resumable machine (`RESUMABLE_INTERP_PLAN.md`): a grade on how a stream ended, the
telescope as the one stream former, and the checkers' contract written as a claim the
stream layer can run. Delete this file once each part is code with tests that say what
its section says.

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

As designed: `done` is the leaf, `spent` a stem, `is_done` "not a fork"; `bounded`
answers `spent` at zero fuel without stepping the inner (so a finite source whose fuel
is exactly its length reads spent, pinned as the conservative reading) and every
combinator passes an ending through; the merge and its two policies live in
`lib/stream.disp`, a policy answering `(items, (survivors, dropped_spent))`; `verdict`
reads the ending; the refuter's `refute` lost `exhaustive`; GRADUAL §7 reads the shape
and §8's `s_fold_absorb` folds a spent ending to its `unknown` element (`open` is a
keyword). Deviations from the plan: the verdict vocabulary is its own module,
`lib/verdict.disp` (`refuted`, `open_on`, `is_proved`/`is_refuted`/`is_open`, `flat`,
`agrees`, and `agree` between two verdicts), and the shared recognizers, normalizers
and subject programs the roots kept re-spelling are `lib/tests/fixtures.disp`. Not
done: the confirmer's `agrees` lines and sweeps still compare against `Arrow`'s Bool;
switching them to `verdict` of a witness stream needs `witnesses_over` in the library,
which is Part 2's `pi_claim`, so that move belongs to step 3.

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

A space is what a binder ranges over. It carries the two programs the checkers judge
with, a partner map that says which pair a candidate is related to, and a stream of
CANDIDATES that is total per step by contract (a finite list, or raw `Trees` under a
budget). Nothing in a space runs a user program inside a stream step:

```
space :: {recognize : Tree -> Bool, same : Tree -> Tree -> Bool, partner : Tree -> Tree, candidates : Stream Tree} -> Space :=>
	pair recognize (pair same (pair partner candidates))
sp_recognize :: Space -> Tree -> Bool := pair_fst
sp_same :: Space -> Tree -> Tree -> Bool := {sp} => sp.snd.fst
sp_partner :: Space -> Tree -> Tree := {sp} => sp.snd.snd.fst
sp_candidates :: Space -> Stream Tree := {sp} => sp.snd.snd.snd
// a data type: sameness is identity, every member is its own partner (the diagonal)
data_space :: {recognize : Tree -> Bool, candidates : Stream Tree} -> Space :=>
	space recognize (rel_of recognize) ({a} => a) candidates
// a quotient: the same candidates, related through the normalizer
quot_space :: {base : Space, nf : Tree -> Tree} -> Space :=>
	space base.sp_recognize ({x, y} => nf x == nf y) nf base.sp_candidates
// an intersection: both recognizers, the first space's candidates and sameness
isect_space :: {A : Space, B : Space} -> Space :=>
	space ({a} => (A.sp_recognize a) && (B.sp_recognize a)) A.sp_same A.sp_partner A.sp_candidates
```

The related pairs of a space are DERIVED, by one task per candidate that recognizes it
and computes its partner inside the same closed program, merged fairly:

```
// the ∃ face: a candidate becomes a pair only when its own task says so
sp_pairs :: {sp : Space, policy : Tree, quantum : Nat} -> Stream (Pair Tree Tree) :=> {
	let relate := {a} => if (sp.sp_recognize a) { some (pair a (sp.sp_partner a)) } else { none }
	let tasks := sp.sp_candidates.s_map({a} => task relate a quantum)
	(s_merge_with tasks policy).s_filter({o} => not (is_none o)).s_map({o} => o.snd)
}
sp_members :: {sp : Space, policy : Tree, quantum : Nat} -> Stream Tree :=> (sp_pairs sp policy quantum).s_map(pair_fst)
```

A partial recognizer (`LoopR`) or a diverging normalizer stalls only its own task; the
other candidates keep their turns, which is GRADUAL §6's guarantee carried into the
member stream itself. The ending is right by Part 1: candidates from `Trees.bounded`
end spent, so do the pairs, so does any verdict over them (`Open`); a finite candidate
list with a total recognizer ends exhausted (`Proved` is reachable); a finite list with
a recognizer that loops on one candidate never ends on its own and reads spent under
any budget, which is the truth about it. `Option` here is the confirmer's
(`some`/`none`/`is_none`) and `rel_of` is `lib/tests/fixtures.disp`'s; both move into
the library when the space module is written. This is the refuter's `rel_of`/`diag_pairs`/`quot_pairs` as
one value per type; `Space` keeps the kernel's field name `#recognize` and its
`candidates` plays the role of the kernel `Enum` table's `members`, so the two can be
matched up later. Pairs rather than a record because the stream layer opens only the raw
prelude, where record literals are not available; the `sp_` accessors are the only way
in, so the later record migration is local.

### Design: the telescope

A telescope is a list of rows, mirroring the kernel's. An ENVIRONMENT is what the rows
have bound so far:

```
// Env := List (Pair Tree Tree), newest binder first; each pair is a related pair of
// the space its row ranged over (left and right components, identical at a data type)
env_empty :: Env := []
env_bind :: {p : Pair Tree Tree, env : Env} -> Env :=> cons p env
// the candidate applied along the environment, oldest binder first, on either side
env_apply_l :: {f : Tree, env : Env} -> Tree :=> reduce ({p, acc} => acc p.fst) f env
env_apply_r :: {f : Tree, env : Env} -> Tree :=> reduce ({p, acc} => acc p.snd) f env
// rows: `fresh_row A` binds one related pair from a space that may depend on the
// environment so far; `observe_row value at` judges a value computed from the
// environment and the candidate at a space
fresh_row :: {A : Env -> Space} -> Row :=> pair "Fresh" A
observe_row :: {value : Env -> Tree -> Tree, at : Env -> Space} -> Row :=> pair "Observe" (pair value at)
```

`reduce` is the right fold of `lib/list.disp`; over a newest-first list it reaches the
oldest binder last, so `env_apply_l f [(b, b'), (a, a')]` is `f a b`. Two folds over the
rows:

- The **environment stream** (the ∃ face, what a Sigma's members are): every `fresh_row`
  extends the environments by the dependent fair merge, which is `s_merge_with` over a
  stream of streams. With `s_bind source inner policy := s_merge_with (source.s_map(inner)) policy`:

```
rec tele_envs :: {rows : List Row, envs : Stream Env, policy : Tree, quantum : Nat} -> Stream Env :=>
	if (rows.is_nil) { envs }
	else if (rows.head.fst == "Fresh") {
		let A := rows.head.snd
		let extend := {env} => (sp_pairs (A env) policy quantum).s_map({p} => env_bind p env)
		tele_envs rows.tail (s_bind envs extend policy) policy quantum
	}
	else { tele_envs rows.tail envs policy quantum }
```

- The **witness stream** (the ∀ face, what a claim is): one task per environment, the
  task running the observations two-sided, and the merge of the tasks filtered to the
  failures. The core check takes two candidates, because sameness at an arrow is judged
  on related inputs: `f` is run along the left components and `g` along the right ones,
  both outputs must be members of the codomain space and related by its sameness.
  Membership is the diagonal, `f` the same as itself (TYPES.html §3), and equality of
  two programs at a type is the same check with `g` different. No domain guard is
  needed here: an environment only ever holds pairs that a recognition task admitted.

```
// the Pi check under one environment: outputs are members, and related
pi_check :: {cod : Env -> Space, f : Tree, g : Tree, env : Env} -> Bool :=> {
	let sp := cod env
	let out_l := env_apply_l f env
	let out_r := env_apply_r g env
	((sp.sp_recognize out_l) && (sp.sp_recognize out_r)) && (sp.sp_same out_l out_r)
}
tele_witnesses :: {rows : List Row, check : Env -> Bool, policy : Tree, quantum : Nat} -> Stream Env :=> {
	let envs := tele_envs rows (s_from_list [env_empty]) policy quantum
	let tasks := envs.s_map({env} => (task ({e} => not (check e)) env quantum).s_map({b} => pair env b))
	(s_merge_with tasks policy).s_filter({p} => p.snd == true).s_map({p} => p.fst)
}
eq_claim :: {rows : List Row, cod : Env -> Space, f : Tree, g : Tree, policy : Tree, quantum : Nat} -> Stream Env :=>
	tele_witnesses rows ({env} => pi_check cod f g env) policy quantum
pi_claim :: {rows : List Row, cod : Env -> Space, f : Tree, policy : Tree, quantum : Nat} -> Stream Env :=>
	eq_claim rows cod f f policy quantum
```

`pi_check` is what the fold of a one-observation telescope amounts to; the general
`observe_row` fold (the kernel's `SigmaCode`, judging the candidate's shape and both
projections) is written when Sigma is, in step 3. GRADUAL's
`witnesses_over src A B fn policy quantum` is `pi_claim [fresh_row ({_env} => data_space A src)] ...`
with `B` folded into the codomain space, and the refuter's `refute_arrow` is `verdict`
of the same claim over a `quot_space` when the domain is a quotient; their tests keep
their outcomes, though the budgets in their `bounded` calls will need retuning, since
recognition now costs a task and a merge layer of its own. Sigma's members are
`tele_envs` of its two rows mapped to pairs; Isect is `isect_space`; Eq at a type is
`eq_claim`.

### Decisions (2026-09-16, to review)

1. **Recognition is a task, on both faces.** No recognizer runs inside a stream step:
   a space's `candidates` are total per step by contract and `sp_pairs` admits a
   candidate only through its own task. The env stream then already holds members, so
   the witness face needs no guard of its own, and a partial recognizer stalls one task
   instead of the whole stream (the `LoopR` tests survive). Membership at a partial
   recognizer is semi-decidable on both faces, which is what it is. The cost is a
   second merge layer under every claim; GRADUAL's and the refuter's budgets get
   retuned, outcomes pinned unchanged.
2. **Normalizers are tasked the same way.** A quotient's partner map runs inside the
   candidate's task, next to its recognizer, and nothing is trusted total. Proving a
   recognizer or normalizer total, and then running it natively inside a step, is
   reserved for the optimization adjoints (an `.opt.disp` overwrite backed by a
   certificate), not for the honest layer.
3. **TODO, merge-of-merges endings:** `tele_envs` nests one merge per `fresh_row` and
   `sp_pairs` adds one under each; `lib/tests/stream.test.disp` (or the space root) must
   pin that a finite telescope of finite spaces with total recognizers ends exhausted
   under both policies, that one partial recognizer anywhere in it ends spent under any
   budget, and that a bounded candidate stream at any depth ends spent. Part 1's taint
   is what makes these fall out; the test is what says so.
4. **Record-free encoding stays.** `Space`, `Row` and `Env` are pairs and lists; the
   `sp_`/`env_` accessors and row constructors are the only way in.

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

1. Endings: landed (see Part 1).
2. Spaces. `lib/space.disp`: `space`, `data_space`, `quot_space`, `isect_space`,
   `sp_pairs`/`sp_members` (tasked recognition), a finite space from a list, a
   `Trees`-backed space with fuel; the refuter's pair streams become `sp_pairs` of
   spaces. Verification: the refuter's tests with the same outcomes (budgets retuned),
   the `LoopR` member stream yields `0`, decision 3's ending tests for one space.
3. Telescopes. `lib/tele.disp`: `Env` and its accessors, rows, `s_bind`, `tele_envs`,
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
