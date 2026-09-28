# Claims over all trees: the most general form, and optimization as removing quantifiers

Design analysis, 2026-09-27. Nothing here is landed. This replaces two earlier versions of
this note: one built on type codes, and one with explicit domains and tags inside the
machine. Each version removed something the checker had to be told; this one follows that
pattern to its end.

Every code block is real disp. In order, the blocks form one file that was run raw against
`lib/prelude.disp`, `lib/list.disp`, `lib/machine.disp`, `lib/verdict.disp`,
`lib/stream.disp`, `lib/tests/fixtures.disp` and `IDEAS.disp`, and all 25 pins pass in
about 2.5 seconds.

## 1. The answer

Every generalization took something the checker had to be *told* and turned it into
something it *runs*:

| built in | became |
|---|---|
| type codes | recognizers, which are ordinary programs |
| a domain per type | every tree, filtered by a recognizer that is itself run |
| relations, arity, telescopes | recognizers on pair- and tuple-shaped trees (pairs are trees) |
| traces as a special observation | a quantifier over step budgets, plus runs cut off at that budget |
| tags inside the machine | origins: a function of a bounded run, computed from the machine's rules |
| kinds of aggregation | kinds of quantifier over trees |

Two things cannot be removed. **Running the machine for a given number of steps** is
always a finite computation, so any test that only looks at bounded runs finishes.
**Quantifying over all trees** is an infinite repetition, which can be described but never
run to the end. So:

> **A claim is a test on bounded runs of the machine, under a prefix of quantifiers over
> all trees.** With numeric scores, "largest", "smallest" and weighted sums take the place
> of "for all" and "exists".

Its meaning is the value of that formula. Nothing in it is specific to types: membership
in a function type, equality, linearity, cost bounds and losses are all formulas of this
shape (§2). Anything more specific is an optimization, and an optimization is a certified
way of removing or bounding a quantifier (§4).

## 2. Tests on bounded runs

All the infinity lives in the quantifiers; a test itself always finishes. A predicate that
may loop is still usable: "A accepts x" becomes "A accepts x within n steps", and the
quantifier over n takes up the slack. A domain is not a separate thing either: "for every x
in A" is "for every tree x and budget n, if A accepts x within n, then ...". Pairs are
trees, so a claim about two arguments, a relation, or a quotient is a claim about
pair-shaped trees.

```
// "for all" over tagged intervals: the upper end is the meet of everything seen; the lower end
// is the meet of the final intervals, and it only counts once the stream ran out
for_all :: {items : Bounded (Items (Pair Bool (Pair P P))), meet : P -> P -> P, top : P, bottom : P} -> Pair P P :=> c_fold ({recurse, step, acc} =>
	if (step.is_finished) { if (step.finished_result.is_spent) { pair bottom acc.snd } else { acc } }
	else if (not step.step_draw.is_yield) { recurse step.step_rest acc }
	else {
		let item := step.step_draw.yielded_item
		let upper := meet acc.snd item.snd.snd
		let lower := if (item.fst) { meet acc.fst item.snd.fst } else { acc.fst }
		if (upper == bottom) { pair bottom bottom } else { recurse step.step_rest (pair lower upper) }
	}
) items (pair top top)
exactly := {b} => pair b b

// budgets 4, 8, 16, ...: enough, since a budget that exposes a failure keeps exposing it; every
// tree's budgets advance together, so tree k meets budget j after about (k + j)^2 / 2 tests
budgets := (stream_new ({n} => emit n (add n n)) 4).as_items
everything := s_merge_with (Trees.as_items.s_map({x} => budgets.s_map({n} => pair x n))) advance_all
// for every tree and budget the test passes: a counterexample refutes it, and nothing ever proves it
every := {test, budget} => for_all ((everything.s_map({p} => pair true (exactly (test p.fst p.snd)))).bounded(budget)) and true false

// tests only look at bounded runs, so every test finishes
within := {f, x, n} => m_advance (m_start f x) n
accepts := {A, x, n} => { let m := within A x n; (m_finished m) && (m_result m == true) }
rec NatS :: {v : Tree} -> Bool :=> if (is_zero v) { true } else if (is_fork v) { if (is_leaf v.fst) { NatS v.snd } else { false } } else { false }
// v : A -> B, partially: if A accepted x within n and v finished within n, the result is in B
pi_partial := {A, B, v} => {x, n} => if (accepts A x n) { let m := within v x n; if (m_finished m) { B (m_result m) } else { true } } else { true }
// v : A -> B, totally, given a step bound: if A accepted x, v finishes within the bound, in B
pi_bounded := {A, B, v, bound} => {x, n} => if (accepts A x n) { let m := within v x (bound x); (m_finished m) && (B (m_result m)) } else { true }
// a claim about pairs is a claim about pair-shaped trees
respects := {related, v} => {z, n} =>
	if ((is_fork z) && (accepts related z n)) {
		let ma := within v z.fst n
		let mb := within v z.snd n
		if ((m_finished ma) && (m_finished mb)) { m_result ma == m_result mb } else { true }
	} else { true }
parity_related := {z} => ((NatS z.fst) && (NatS z.snd)) && (parity z.fst == parity z.snd)
rec mul :: {a : Nat, b : Nat} -> Nat :=> if (is_zero a) { zero } else { add b (mul (nat_pred a) b) }
bad := fix ({self, n} => if (is_zero n) { zero } else { succ (self n) })

test every (pi_partial NatS NatS ({n} => pair n n)) 100 = pair false false
test every (pi_partial NatS NatS succ) 45 = pair false true
test every (pi_partial LoopR NatS ({_n} => t t)) 45 = pair false false
test every (pi_partial NatS NatS bad) 45 = pair false true
test every (pi_bounded NatS NatS bad ({_x} => 400)) 100 = pair false false
test every (pi_bounded NatS NatS dbl ({_x} => 10)) 45 = pair false false
test every (pi_bounded NatS NatS dbl ({x} => mul 64 (succ x))) 45 = pair false true
test every (respects parity_related is_even) 150 = pair false true
```

Reading the pins:

- `pair n n` fails at 1, whose double copy is not a Nat. `succ` is never refuted.
- Filtering by a looping recognizer is honest: `LoopR` accepts only 0 (it loops on
  everything else), so the claim is refuted at 0 and constrains nothing else.
- `bad` never returns a wrong value, it just never returns on anything but 0, so the
  partial-correctness claim is *true* and stays Open. Stating a step bound turns it into a
  claim search can refute (§4).
- A step bound that is too small for `dbl` is refuted; a sufficient one stays Open.
- `is_even` respecting parity is a claim about pair-shaped trees whose components have the
  same parity, and it is never refuted.

## 3. The quantifier prefix measures difficulty

- **For all trees and budgets, a test (Π1).** Search refutes it, because a counterexample
  is a finite object, but never proves it over infinitely many trees. Partial correctness,
  "used at most once", and total correctness with a stated step bound are all Π1.
- **Exists a tree and a budget (Σ1).** Search confirms it and never refutes it:

```
// "exists" is "not for all not": negating swaps the ends of the interval
some := {test, budget} => {
	let none_pass := for_all ((everything.s_map({p} => pair true (exactly (not (test p.fst p.snd))))).bounded(budget)) and true false
	pair (not none_pass.snd) (not none_pass.fst)
}
// some Nat has successor 2; some Nat has successor 0
returns := {v, x, n, r} => { let m := within v x n; (m_finished m) && (m_result m == r) }
test some ({x, n} => (accepts NatS x n) && (returns succ x n 2)) 100 = pair true true
test some ({x, n} => (accepts NatS x n) && (returns succ x n 0)) 60 = pair false true
```

- **For all trees, exists a budget (Π2).** "Every input finishes": total correctness
  without a bound, and membership in a function type in general. Search can neither refute
  nor confirm it, except for runs that visibly cycle.

This is the safety/liveness split in quantifier form: "every property is the intersection
of a safety property and a liveness property"
([Alpern & Schneider](https://www.cs.cornell.edu/fbs/publications/DefLiveness.pdf#page=1)).
A safety part is "for every budget, nothing bad yet"; a liveness part is "some budget where
the good thing has happened".

## 4. Optimizations remove quantifiers

A witness makes a claim cheaper by removing or bounding a quantifier. Each witness is itself
a claim of lower complexity, checked by the same machinery:

- **A step bound** removes "exists a budget", taking total correctness from Π2 to Π1, where
  search can refute it. Cost types and termination certificates are the same thing: the
  `bad` pin in §2 goes from Open (partial) to refuted (bounded).
- **A generator** bounds "for every tree, if A accepts it": enumerate A's members directly
  instead of filtering all trees. Deriving generators from predicates is known as narrowing,
  and Beginner's Luck proves such generators sound and complete "with respect to the
  predicate semantics" ([Lampropoulos et al., p. 4](https://arxiv.org/pdf/1607.05443#page=4)).
- **A finite domain** bounds the quantifier completely, so the check decides and
  terminates. A checker that always terminates therefore needs exactly these constraints
  on its domain: finite or generated, with predicates of known cost.
- **A case table** covers every tree with finitely many symbolic cases. An earlier version
  of this note showed it: running Nat's recognizer on a placeholder, splitting only where it
  looks, derives `Nat = leaf | fork(leaf, Nat)`; and three differently written doublings
  produce identical tables, so their equality is one hash-cons comparison.
- **An invariant or a measure** replaces "for every budget" by one symbolic step
  (coinduction) or by a descent (induction).
- **A type system** is a syntactic sufficient condition. Its soundness is a claim over its
  derivations, which are trees.
- **A fused observer** (§6) computes a test faster without changing it.

The lattice of checkers is the quantifier hierarchy. At the bottom every quantifier is
bounded and the check decides; above that, search settles one quantifier block in one
direction; beyond one alternation only witnesses help. Optimizing a checker means pushing
claims down with witnesses. For GOALS.md this makes searching for programs and searching for
the witnesses that certify them one search.

## 5. Evaluating a quantifier

The honest evaluator searches the first quantifier block fairly. For "every tree and every
budget" there are two equivalent ways:

- **Restart**: test every (tree, budget) pair, as in §2. Simple, and it redoes work:
  reaching the parity counterexample at enumeration position 64 this way ran out of memory.
- **Resume**: run each tree's unbounded test once as a task, advanced a slice at a time.
  This is equivalent because each test only gets truer as its budget grows (a budget that
  exposes a failure keeps exposing it). It found the same counterexample in 1.5 seconds and
  135MB:

```
// the same claims with each tree's test run once, resumed, instead of restarted at every budget
every_task := {test, quantum, budget} => verdict ((s_merge_with (Trees.as_items.s_map({x} => (task test x quantum).answers.s_map({ok} => pair x ok))) advance_all).s_filter({p} => not p.snd).s_map(pair_fst).bounded(budget))
respects_check := {related, v} => {z} => if (is_fork z) { if (related z) { v z.fst == v z.snd } else { true } } else { true }
test every_task ({x} => if (NatS x) { NatS (pair x x) } else { true }) 64 100 = refuted 1
test every_task (respects_check parity_related id) 64 400 = refuted 3
test every_task (respects_check parity_related is_even) 64 300 = "Open"
```

Scheduling changes when an answer arrives, never what it is. Round-robin was the trap:
when a new task joins every round the queue keeps growing, and late candidates almost never
run. Advancing every live task each round fixes it.

Aggregations follow one rule: an early answer is safe exactly when the evaluator can bound
what the unfinished part could still contribute. For all is a running meet, whose upper
end moves. Exists is "not for all not", whose lower end moves. A sum with no bound on its
scores only ever gets a lower bound. A weighted sum over all trees needs a prior (weights
over trees with a known total); then the unseen weight bounds the remainder and the
interval closes from both sides.

## 6. Origins instead of tags

Linear and resource claims need to know where copies of an input go. That is
provenance, and rewriting theory calls it origin tracking: "Origins are relations between
subterms of intermediate terms and subterms of the initial term"
([van Deursen, Klint & Tip](https://www.franktip.org/pubs/jsc1993.pdf#page=1)). Origins are
defined by the *rules* (which piece of the new state came from which piece of the old one),
not by values: hash-consing makes every copy of `0` the same leaf as every other `0`. So an
observer that knows the rules can compute them from the plain run, outside the machine.

The machine below is the plain machine fused with such an observer: the rules of
`lib/machine.disp` over values that may carry a name, logging every step whose rule decided
something from a named value's shape. Fusing is only an optimization, and the pins check
that it is a faithful one: with the names erased, every state equals `lib/machine.disp`'s.

```
// a machine value: a plain tree, a watched tree (an input with a name), or a node built around one
plain := {tr} => pair "Plain" tr
watched := {name, tr} => pair "Watch" (pair name tr)
is_plain := {v} => v.fst == "Plain"
is_watched := {v} => v.fst == "Watch"
watch_name := {v} => v.snd.fst
watch_tree := {v} => v.snd.snd
vstem := {a} => if (is_plain a) { plain (t a.snd) } else { pair "Stem" a }
vfork := {a, b} => if ((is_plain a) && (is_plain b)) { plain (t a.snd b.snd) } else { pair "Fork" (pair a b) }
// the tree a value stands for, names forgotten
rec erase :: {v : Tree} -> Tree :=>
	if (is_plain v) { v.snd } else if (is_watched v) { watch_tree v }
	else if (v.fst == "Stem") { t (erase v.snd) } else { t (erase v.snd.fst) (erase v.snd.snd) }
// one layer of a value's shape; a watched tree opens into watched pieces named after it
shape_of := {v} =>
	if (is_plain v) { triage (pair "Leaf" t) ({c} => pair "Stem" (plain c)) ({l, r} => pair "Fork" (pair (plain l) (plain r))) v.snd }
	else if (is_watched v) { triage (pair "Leaf" t) ({c} => pair "Stem" (watched (pair (watch_name v) "child") c)) ({l, r} => pair "Fork" (pair (watched (pair (watch_name v) "left") l) (watched (pair (watch_name v) "right") r))) (watch_tree v) }
	else { v }
// the log: names whose shape decided a step (newest first), and the step count
note := {log, v} => if (is_watched v) { pair (cons (watch_name v) log.fst) log.snd } else { log }
tick := {log} => pair log.fst (succ log.snd)
// the discard shortcuts peek at shapes without deciding anything the program can see
k_shaped_v := {v} => { let s := shape_of v; (s.fst == "Fork") && ((shape_of s.snd.fst).fst == "Leaf") }
k_payload := {v} => (shape_of v).snd.snd

mm_run := {f, x, stack, log} => pair "Run" (pair (pair f x) (pair stack log))
mm_done := {v, log} => pair "Done" (pair v log)
mm_is_done := {m} => m.fst == "Done"
mm_log := {m} => if (mm_is_done m) { m.snd.snd } else { m.snd.snd.snd }
rec mm_deliver :: {v : Tree, stack : Tree, log : Tree} -> Tree :=>
	if (stack.is_nil) { mm_done v log }
	else {
		let fr := stack.head
		let rest := stack.tail
		if (fr.fst == "To") { mm_run v fr.snd rest log }
		else if (fr.fst == "Res") { mm_run fr.snd v rest log }
		else {
			let b := fr.snd.fst
			let x := fr.snd.snd
			if (k_shaped_v v) { mm_deliver (k_payload v) rest log }
			else if (k_shaped_v b) { mm_run v (k_payload b) rest log }
			else { mm_run b x (cons (fr_res v) rest) log }
		}
	}
rec mm_settle :: {m : Tree} -> Tree :=>
	if (mm_is_done m) { m }
	else {
		let f := m.snd.fst.fst
		let x := m.snd.fst.snd
		let stack := m.snd.snd.fst
		let log := m.snd.snd.snd
		let s := shape_of f
		if (s.fst == "Leaf") { mm_settle (mm_deliver (vstem x) stack (note log f)) }
		else if (s.fst == "Stem") { mm_settle (mm_deliver (vfork s.snd x) stack (note log f)) }
		else { m }
	}
mm_step := {m} => {
	let s0 := mm_settle m
	if (mm_is_done s0) { s0 }
	else {
		let f := s0.snd.fst.fst
		let x := s0.snd.fst.snd
		let stack := s0.snd.snd.fst
		let log := tick (note s0.snd.snd.snd f)
		let fs := shape_of f
		let b := fs.snd.snd
		let as := shape_of fs.snd.fst
		if (as.fst == "Leaf") { mm_deliver b stack log }
		else if (as.fst == "Stem") {
			let c := as.snd
			if (k_shaped_v c) {
				let v := k_payload c
				if (k_shaped_v v) { mm_deliver (k_payload v) stack log }
				else if (k_shaped_v b) { mm_run v (k_payload b) stack log }
				else { mm_run b x (cons (fr_res v) stack) log }
			}
			else { mm_run c x (cons (fr_s b x) stack) log }
		}
		else {
			let xs := shape_of x
			let seen := note log x
			if (xs.fst == "Leaf") { mm_deliver as.snd.fst stack seen }
			else if (xs.fst == "Stem") { mm_run as.snd.snd xs.snd stack seen }
			else { mm_run b xs.snd.fst (cons (fr_to xs.snd.snd) stack) seen }
		}
	}
}
rec mm_advance :: {m : Tree, steps : Nat} -> Tree :=>
	if (is_zero steps) { m } else if (mm_is_done m) { m } else { mm_advance (mm_step m) (nat_pred steps) }
// run f on its arguments, each watched under its position
mm_start := {f, args} => {
	let named := (fix ({go, xs, i} => if (xs.is_nil) { [] } else { cons (watched i xs.head) (go xs.tail (succ i)) })) args zero
	mm_run (plain f) named.head (named.tail.map(fr_to)) (pair [] zero)
}
rec count_watch :: {name : Tree, v : Tree} -> Nat :=>
	if (is_plain v) { zero } else if (is_watched v) { if (watch_name v == name) { 1 } else { zero } }
	else if (v.fst == "Stem") { count_watch name v.snd } else { add (count_watch name v.snd.fst) (count_watch name v.snd.snd) }
// uses of an input: steps its shape decided, plus copies of it in the result
uses_of := {name, m} => {
	let decided := (mm_log m).fst.filter({n} => n == name).length
	if (mm_is_done m) { add decided (count_watch name m.snd.fst) } else { decided }
}
finish := {f, args} => mm_advance (mm_start f args) 3000
result_of_run := {m} => erase m.snd.fst
```

```
test uses_of zero (finish ({x} => x) [5]) = 1
test uses_of zero (finish ({x} => t) [5]) = 0
test uses_of zero (finish ({x} => pair x x) [5]) = 2
test uses_of zero (finish ({x} => ({_y} => t) (pair x x)) [5]) = 0
test uses_of zero (finish is_leaf [5]) = 1
test uses_of zero (finish ({x} => pair (is_leaf x) x) [5]) = 2
test uses_of zero (finish ({n} => (t (t zero succ) id) n) [5]) = 1
test uses_of zero (finish ({f} => pair (f 1) (f 2)) [succ]) = 2
// the observer changes nothing: with names erased, every state is the plain machine's
erase_frame := {fr} => if (fr.fst == "To") { fr_to (erase fr.snd) } else if (fr.fst == "Res") { fr_res (erase fr.snd) } else { fr_s (erase fr.snd.fst) (erase fr.snd.snd) }
plain_state := {m} => if (mm_is_done m) { machine_done (erase m.snd.fst) } else { machine_run (erase m.snd.fst.fst) (erase m.snd.fst.snd) (m.snd.snd.fst.map(erase_frame)) }
rec upto :: {n : Nat} -> List Nat :=> if (is_zero n) { [0] } else { (upto (nat_pred n)).append([n]) }
in_lockstep := {f, x, k} => (upto k).all({i} => plain_state (mm_advance (mm_start f [x]) i) == m_advance (m_start f x) i)
test in_lockstep dbl 3 40 = true
test in_lockstep ({x} => pair (is_leaf x) x) 5 20 = true
// "at most one use" is one more test on bounded runs
at_most_once := {A, v} => {x, n} => if (accepts A x n) { nat_le (uses_of zero (mm_advance (mm_start v [x]) n)) 1 } else { true }
test every (at_most_once NatS ({x} => pair x x)) 45 = pair false false
test every (at_most_once NatS id) 45 = pair false true
```

Three consequences:

- **One mechanism, three uses.** A run is a path through the program's decisions (which
  rule fired, which way each inspection went). An observer replays that path on its own
  kind of value: origins, costs. A symbolic run explores all paths at once, which is a case
  table. Tags, external observers and symbolic execution are one idea with different values.
- **"A use" is defined by the machine**: its discard shortcuts mean some copies are never
  evaluated, so usage is relative to the evaluation strategy.
- **Abstract linear typing decides the same claim without running.** `at_most_once` is Π1;
  a linear type system is a witness that settles it syntactically.

## 7. Proofs, the other side

The contract between search and a checker: a bound a checker certifies is never crossed by
anything search observes. The search keeps testing that contract, so an unsound checker is
eventually refuted with a concrete counterexample.

Proving a checker sound is one case per checker rule (the "fundamental lemma"): each rule
preserves the meaning. A checker's derivations are trees, so this is a claim over trees,
proved by induction on derivations. It bottoms out in the run lemma (TYPES.html §9: a
symbolic run that never looks at a placeholder answers the same way for every tree in its
place), proved once by hand. Cyclic proofs, where a hypothesis is used again further down,
are sound when every infinite path makes progress, the "global trace condition"
([Berardi & Tatsuta, p. 10](https://arxiv.org/pdf/1712.09603#page=10)).

Liveness can also be refuted where a run cycles: the paused machine is deterministic, so a
whole state repeating, or the same pending application returning with the older stack
untouched underneath, proves the run never ends. An earlier version of this note caught
`spin` at step 10, `LoopR 1` at 43 and `bad 1` at 42 this way.

## 8. Where it would land, and decisions

- `within` and `accepts` belong beside `lib/machine.disp`; the quantifiers (`every`,
  `some`, `every_task`) in a stream-layer module. Part 2 of `STREAM_TYPES_PLAN.md` (spaces,
  telescopes, environments) is not needed. Part 3's symbolic machine is the fused observer
  above with placeholders in place of trees.
- **Decisions.** Which prior over trees weighted sums should use (geometric weights by
  enumeration position is the natural default). Whether the machine should expose
  single-rule steps for external observers (`m_step` bundles several rule applications;
  re-deriving them amounts to re-running the step function). Whether linear types default
  to strict (every piece of the input used once) or loose (the input decided once). And
  whether specs ask for total correctness by default, which in this form means asking for a
  witness: a step bound or a measure.
