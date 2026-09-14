# Resumable computations: a disp machine, then a transparent native accelerator

Status (2026-09-14): Half 1 landed (steps 1–2 below, marked done); nothing of Half 2
exists yet. Delete this file once the accelerator exists and its tests say what this
file says.

## What this is for

Three consumers want to run a program for a bounded number of steps, keep its state, and
continue later:

- The stream layer (GRADUAL.disp, the walkthrough of the stream type checker). Its `task`
  re-runs an application from scratch with more fuel each turn, because nothing today can
  pause a run. That works, but it couples the fuel schedule to the scheduling policy:
  round-robin needs doubling fuel (`dbl`), the step-everything policy needs additive fuel.
  A resumable task advances by a constant quantum under any fair policy.
- The confirmer (the symbolic checker in `lib/tests/confirmer.test.disp`). Its `need s`
  result is a run paused on a symbol's shape. Today the search re-runs the whole function
  under each case; with a paused state it resumes the same run under each case.
- Any program that simulates another program's evaluation. Budget consumption is already
  observable to such programs, since a nested interpreter counts what it does. The machine
  here *is* such an interpreter, written once, in the library, with a fixed meaning of "one
  step". Nothing new becomes observable.

Both native evaluators already hold the whole state of a run in registers plus an explicit
frame stack (`src/core/tree.ts` `apply`, four frame kinds on a shared preallocated stack;
`evaluators/rust-eager/crate/src/reduce.rs` `reduce`, the same four kinds in a local `Vec`)
and drop it on budget exhaustion. The accelerator does not change those loops. It adds a
second, memo-free native stepper that computes exactly what the disp machine computes, and
intercepts the disp definition the way `tree_eq` is intercepted today.

The plan is in two halves on purpose: the disp machine is the specification and is useful
on its own; the accelerator only makes it fast and must be invisible.

## Half 1: the machine, in disp

### Names

Every binding is prefixed `m_` or `machine_`, because the obvious names collide:
`is_done` is the stream library's step predicate (`lib/stream.disp`), `Frame` is a kernel
binding (`lib/kernel/kernel.disp`), and `advance` and `start` are parameter names inside
GRADUAL.disp's `s_merge_with`, `witnesses_over` and `task`, which would shadow module
bindings of the same name inside those bodies. When GRADUAL.disp gains its resumable
section, those parameters are renamed (`policy`, `first_fuel`) at the same time.

`Machine` and `MFrame` are givens of `lib/machine.disp` (`open given { Machine : Tree,
MFrame : Tree }`, the kernel's spelling for type-valued givens). Every consumer opens
the stream layer raw, and a raw open binds no givens and compiles no annotations, so
today the annotations are documentation and the file behaves exactly like `list.disp`
and `stream.disp`; a checked consumer fills the two names. Two consequences: a
given-bearing file cannot be a root, and a raw open runs no inline tests
(`src/elab/driver.ts`, the Test propagation), so the machine's tests live in
`lib/tests/refuter.test.disp`, the harness root that already opens IDEAS.disp, next to
the conformance sweep. (`stream.disp`'s own inline tests have the same status: they run
only when that file is the root.)

### State

A machine is either finished or has one pending application and a stack of frames:

```
machine_done v            = pair "Done" v
machine_run f x stack     = pair "Run" (pair (pair f x) stack)
```

A frame says what to do with the value of the pending application:

```
fr_to arg      = pair "To" arg              // value r  ->  apply r arg
fr_res func    = pair "Res" func            // value r  ->  apply func r
fr_s b x       = pair "S" (pair b x)        // value cx ->  the tail of the S rule (below)
```

These are the native evaluator's `ApplyTo`, `ApplyResultTo` and `SAfterCx` frames. The
native `Memo` frame has no counterpart: the machine does not memoize, and that is what makes
its step count a definition rather than a measurement.

### Transitions

Applying `f` to `x` under stack `s`:

| head `f` | what happens | counted |
|---|---|---|
| leaf | deliver the stem `t x` | no |
| stem `t a` | deliver the fork `t a x` | no |
| fork `t t b` | K: deliver `b` | yes |
| fork `t (t c) b` | S: see below | yes |
| fork `t (t w v) b` | triage on `x`: leaf, deliver `w`; stem `t u`, run `v u`; fork `t u u'`, push `fr_to u'` and run `b u` | yes |

Delivering a value `r` pops the stack: empty, the machine is done with `r`; `fr_to arg`,
run `r arg`; `fr_res func`, run `func r`; `fr_s b x`, the S tail with `cx = r`.

The S rule `t (t c) b x = (c x) (b x)` and its tail carry the discard shortcuts the eager
backends implement. `EVALUATOR.md` records why: values agree across schedules by
confluence, but the termination domain does not, and the kernel's checking paths lean on
this discard. A machine without it would be stricter than the evaluators and would fail to
finish runs they finish. The two backends do not implement it identically, which matters
for the step unit below:

- Before scheduling anything (the TypeScript loop only, `src/core/tree.ts` at the S rule):
  if `c` is K-shaped (`t t v`), then `c x` is `v` with no application. If `v` is itself
  K-shaped (`t t w`), the answer is `w` and `b x` is never run. Otherwise if `b` is K-shaped
  (`t t u`), run `v u`. Otherwise push `fr_res v` and run `b x`.
- At the join (both backends; `reduce.rs` pushes the S frame directly and discards on
  delivery): push `fr_s b x` and run `c x`. On delivery of `cx`: if `cx` is K-shaped
  (`t t w`), deliver `w` and `b x` is never run. Otherwise if `b` is K-shaped (`t t u`),
  run `cx u`. Otherwise push `fr_res cx` and run `b x`.

The machine mirrors the TypeScript loop, both levels.

### One step, and what its count means

A step is one K, S or triage dispatch on a fork head, together with the delivery it causes.
Leaf and stem heads only build, so `m_step` first runs those for free until a fork head or a
finished machine appears (`m_settle`), then performs one dispatch. Settling terminates: every
free rule pops at least one frame or finishes, except the S-tail delivery, which replaces one
frame by another and can only happen once per S frame already on the stack.

The unit is defined against one backend: the TypeScript eager evaluator (`disp-eager`),
which `EVALUATOR.md` names as the canonical reference every other backend is validated
against. With this unit the machine's count equals that evaluator's `steps` counter on the
same application with its memo empty and the `tree_eq` fast path off (pinned by
`test/machine.test.ts`, step 2). It does not equal the
Rust reducer's count: on `S (K (K w)) b x` shapes Rust performs one more counted dispatch,
because it resolves `c x` by a K dispatch and discards only at the join. The native steppers
of both backends count by the machine's table, so they agree with the machine and with each
other, not with the Rust reducer. None of these is the warm `steps` figure from `--stats`,
and no program may turn "not done after k" into a verdict.

### Code (`lib/machine.disp`)

```
machine_done :: {v : Tree} -> Machine :=> pair "Done" v
machine_run :: {f : Tree, x : Tree, stack : List MFrame} -> Machine :=> pair "Run" (pair (pair f x) stack)
m_finished :: Machine -> Bool := {m} => m.fst == "Done"
m_result :: Machine -> Tree := pair_snd
m_start :: {f : Tree, x : Tree} -> Machine :=> machine_run f x []
fr_to :: {arg : Tree} -> MFrame :=> pair "To" arg
fr_res :: {func : Tree} -> MFrame :=> pair "Res" func
fr_s :: {b : Tree, x : Tree} -> MFrame :=> pair "S" (pair b x)
k_shaped :: Tree -> Bool := {v} => (is_fork v) && (is_leaf v.fst)

// m_deliver a value through the stack, popping until an application is pending
rec m_deliver :: {v : Tree, stack : List MFrame} -> Machine :=>
	if (stack.is_nil) { machine_done v }
	else {
		let fr := stack.head
		let rest := stack.tail
		if (fr.fst == "To") { machine_run v fr.snd rest }
		else if (fr.fst == "Res") { machine_run fr.snd v rest }
		else {
			let b := fr.snd.fst
			let x := fr.snd.snd
			if (k_shaped v) { m_deliver v.snd rest }
			else if (k_shaped b) { machine_run v b.snd rest }
			else { machine_run b x (cons (fr_res v) rest) }
		}
	}
// free work: leaf and stem heads only build; run them until a fork head or Done
rec m_settle :: {m : Machine} -> Machine :=>
	if (m_finished m) { m }
	else {
		let f := m.snd.fst.fst
		let x := m.snd.fst.snd
		let stack := m.snd.snd
		if (is_leaf f) { m_settle (m_deliver (t x) stack) }
		else if (is_stem f) { m_settle (m_deliver (t (stem_child f) x) stack) }
		else { m }
	}
// one counted m_step: one K, S or triage dispatch on a fork head, then its delivery
m_step :: {m : Machine} -> Machine :=> {
	let s := m_settle m
	if (m_finished s) { s }
	else {
		let f := s.snd.fst.fst
		let x := s.snd.fst.snd
		let stack := s.snd.snd
		let a := f.fst
		let b := f.snd
		if (is_leaf a) { m_deliver b stack }
		else if (is_stem a) {
			let c := stem_child a
			if (k_shaped c) {
				let v := c.snd
				if (k_shaped v) { m_deliver v.snd stack }
				else if (k_shaped b) { machine_run v b.snd stack }
				else { machine_run b x (cons (fr_res v) stack) }
			}
			else { machine_run c x (cons (fr_s b x) stack) }
		}
		else {
			if (is_leaf x) { m_deliver a.fst stack }
			else if (is_stem x) { machine_run a.snd (stem_child x) stack }
			else { machine_run b x.fst (cons (fr_to x.snd) stack) }
		}
	}
}
rec m_advance :: {m : Machine, steps : Nat} -> Machine :=>
	if (is_zero steps) { m } else if (m_finished m) { m } else { m_advance (m_step m) (nat_pred steps) }
```

Tests (`lib/tests/refuter.test.disp`, GRADUAL.disp §4): `succ 2` finishes with 3; the shape-only `NatR` answers on 3 and
on a stem; `LoopR 0` answers and `LoopR 1` is not done after 300 steps; `add 2 3` gives 5;
`{x} => ({_y} => 1) (LoopR x)` applied to 1 answers 1 both natively and in the machine
within 50 steps, which is the discard shortcut doing its job in both places; and a task
with a constant quantum yields exactly once for a finishing program and never for a looping
one while every stream step returns.

A fueled apply that returns a value is `m_advance (m_start f x) n` followed by `m_finished`.
That is the whole of "level 1" from the discussion that produced this plan; it needs no
primitive.

### Suspended values

The eager backend keeps `tree_eq a` as a suspended node and materializes it on any shape
observation. The machine observes shapes with `is_leaf`, `is_stem`, `.fst` and `.snd`, so
those nodes are forced under it exactly as they would be under any other program. No case
in the machine mentions suspensions, and none should.

## Half 2: the accelerator, transparent like `tree_eq`

### How `tree_eq` is intercepted today

`lib/prelude.disp` defines `tree_eq` as a recursive tree program. At boot the driver records
its compiled id (`recognizeNative("tree_eq", handle)` at `src/elab/driver.ts:904`,
`setTreeEqId` in `src/core/tree.ts`). Stage 1: `apply(tree_eq, a)` returns the honest
suspended partial application, a first-class value with no synthetic marker. Stage 2:
applying that partial to `b` answers with the hash-cons equality, one dispatch instead of a
walk. The Rust backend mirrors both stages in `native.rs`.

Two things the earlier draft of this plan described as existing are work items:

- `noNativeIntercept` is a field of `SessionOpts` (`src/eval/types.ts`) that no backend
  reads. The `tree_eq` differential in `test/tree.test.ts` bypasses the intercept by calling
  `setTreeEqId(-1)` around the in-language run. Step 3 implements the option in the
  TypeScript session and the Rust wrapper before anything relies on it.
- There is no session option for a memo-free run. `setApplyCacheLimit(0)` exists on the
  TypeScript core and the eager session but nothing calls it and it is not in `SessionOpts`.
  The Rust env var `RUST_EAGER_MEMO_LIMIT` treats 0 as unbounded, the opposite meaning, so it
  must not be reused for this. Step 2 adds an `applyCacheLimit` session option wired to the
  existing setter, TypeScript only.

### The same shape for `m_advance`

- Generalize the single `treeEqId` into a small table: compiled id, arity, native handler.
  `m_advance` is the second entry, arity 2, registered from `lib/machine.disp` at the same
  driver site as `tree_eq`.
- Stage 1: `apply(m_advance, m)` suspends as the partial `P(m_advance, m)`. Stage 2:
  applying it to `k` runs the native stepper and returns the resulting machine as a tree.
- The native stepper (`stepMachine(state, k)` in `src/core/tree.ts`; `step_machine` in
  `reduce.rs`, exposed as `tc_advance(m, k)` in `ffi.rs` next to `tc_apply` and `tc_equal`)
  decodes the state tree into registers and a frame vector, performs `k` dispatches with
  exactly the transition table above, and encodes the result back through the arena, so the
  returned tree is the same hash-consed node the disp definition would have built. It uses
  no memo and no `tree_eq` shortcut inside the run; it forces a suspended node only where the
  disp machine would observe a shape. Decoding walks the frame list, not the trees inside it,
  and `k` is a unary Nat, so one call costs the frame count plus `k` before any stepping. The
  frame count is the paused run's depth and has nothing to do with the quantum: a deep run
  advanced by a small quantum pays that decode on every call, which step 5 measures.

### The transparency contract

For every machine `m` and count `k`, the intercepted `m_advance m k` returns the tree the disp
definition returns. Nothing weaker is acceptable, because consumers read the state: a task
inspects `m_finished` and `m_result`, and a future confirmer will inspect the frames.

Enforced three ways once step 3 has made the toggle real: the `noNativeIntercept` path must
still pass every disp test that uses `m_advance`; a vitest differential over random total
terms and random `k`, including `k` past completion, compares the intercepted and
in-language results node for node, in the style of the 300-random-term loops in
`test/eval-abi.test.ts` and `test/eval-naive.test.ts`; and the Rust stepper is compared to
the TypeScript one through the existing differential-oracle harness.

## Consumers

- `task` in `lib/stream.disp` is resumable: the state is a machine, each stream step is
  `m_advance m quantum`, `m_result` is yielded once when done. Constant quantum, any fair
  policy. The restart form stays in GRADUAL.disp as `restart_task`, the teaching contrast,
  and the walkthrough has the resumable form beside it (`advance` → `policy`, `start` →
  `first_fuel`).
- `witnesses` and the fair merge are unchanged; the policy and the fuel schedule decouple,
  so the step-everything policy is correct with the same task as round-robin.
- The confirmer, later: the same frames over partly known values, plus a `need` terminal
  state, gives resumption at Need. Keep the frame encoding stable so that machine can share
  it. Not part of this plan's steps.

## Steps

1. Done. `lib/machine.disp` is the code above; its tests and the conformance sweep over
   IDEAS.disp's terminating candidates (`boolfns` on both bools, `idfns` on `probe`, each
   `m_result (m_advance (m_start fn a) 2000)` against `fn a` natively) are the machine
   section of `lib/tests/refuter.test.disp`. `task` in `lib/stream.disp` is resumable;
   GRADUAL.disp §4 shows both forms and §6–8 run the witness streams on quantum 64 under
   both policies with the budgets unchanged.
2. Done. `applyCacheLimit` is a `SessionOpts` field honoured by the TypeScript eager
   backend's `createSession`, and a fresh session has no `tree_eq` id. `test/machine.test.ts`
   steps the machine one dispatch at a time (settle, ask `m_finished`, step; `m_advance`
   reports no count) and compares with that session's `steps`: equal on all six programs,
   including the S-rule discard shape and `succ 2`, which is one free stem rule at 0 on both
   sides (`succ` eta-reduces to `t t`).
3. TypeScript stepper, the intercept table, registration at boot, a real `noNativeIntercept`,
   and the vitest differential. Adjacent fix in the same change: the TypeScript loop throws
   on budget exhaustion without resetting `stackTop`, so abandoned frames in the shared
   slot array are never reclaimed; add the reset. Verification: parser and evaluator suites,
   the differential, and every disp file that uses `m_advance` under both intercept settings.
4. Rust stepper, `tc_advance`, the TypeScript wrapper for `rust-eager`, the cross-backend
   differential. This is the default backend, so this step is what makes the streams fast.
5. Measure: GRADUAL.disp's witness tests and the LoopR cases, wall time and `cold_equiv`,
   intercept on and off. The expected result is that the interpreter overhead of `run_s`
   disappears and the machine's own cost becomes the frame traffic.
6. Follow-ons, each its own plan: the symbolic machine for the confirmer; running the
   step-everything policy on the interaction-net backend, where independent machines are
   disjoint subnets; the exhaustion-versus-budget grade on streams.

## Risks and non-goals

- Drift between three implementations of one transition table is the risk that matters.
  The differential tests are the gate, and the discard depth is the known hazard: a stepper
  stricter than its peers hangs on runs they finish, which has happened once already to the
  eager backend (`src/core/tree.ts`, the comment at the S rule).
- The count is a memo-free dispatch count defined against the TypeScript backend. It must
  never be compared with a warm run's `steps` or with the Rust reducer's counter, and no
  program may turn "not done after k" into a verdict; the stream layer does not, and that
  stays a rule.
- No scheduler moves into native code. Policies, fairness and buffering stay in disp; the
  accelerator only advances one machine by `k`.
- State trees embed `f`, `x` and every frame's trees, but those are shared hash-consed
  nodes; encoding and decoding cost the frame count, never the tree size.
- The intercepted `m_advance m k` is not memoized. The stage-2 path `tree_eq` uses today
  delivers its answer without touching the memo, a machine state rarely recurs, and a
  snapshot fact keyed on one would buy nothing. If that ever changes, the snapshot stamp
  already covers the evaluator binary, so a stepper change would invalidate it.
