# Checking as a search over a case tree

Written 2026-11-10. Sources fetch-verified with `scripts/verify-source.sh` the same day.
This note is the frame behind `lib/rules.disp` and the loop in `lib/confirm.disp`, the
literature each part of it has a name in, and what was built from it. The earlier survey,
[LENS_TYPE_THEORY_PRIOR_WORK.md](LENS_TYPE_THEORY_PRIOR_WORK.md), covers who else has the
individual pieces; [LOB_AND_CURRY.md](LOB_AND_CURRY.md) covers why the floor has to be
proved by hand.

## 1. The picture

A type check is a search over the claim's own case tree, and a lens is a certified step
that makes that tree smaller.

Brute force stays the meaning: enumerate every tree and keep the ones the claim fails on
(`honest` in `lib/claim.disp`). A run can only learn about its input through triage, the
one rule that looks at its argument. A run that finishes has looked a finite number of
times. So every claim has a case tree: each node is the next place the run looks, and each
leaf is a partly known input the run no longer looks at. Every tree falls under one leaf,
so checking the leaves is the same check as checking the trees. The lens is the map from a
tree to its leaf. Escardó puts the reason in one line: ["finite amounts of the output
depend only on finite amounts of the input"](https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/#content).

There are four steps. Each one is a reason not to enumerate.

| Step | "You don't have to iterate, because…" | What has to be true | In disp |
|---|---|---|---|
| **Case split** (look) | the run asked only a few questions about the input | a tree is a leaf, a stem or a fork | `alternatives` in the loop; as a lens, `split_lens` and `split_under` |
| **Generic run** (don't look) | the run never read this part, so one run answers for all of it | no rule but triage depends on what its argument is | the machine with `held` applications; law `run_law` |
| **Induction** (loop) | the same question comes back about a strictly smaller piece, or one step later | there is no endless chain of smaller pieces, or of earlier steps | `induct` for `fix(step)`; `stream_eq` for streams; law `ind_holds` |
| **Lemma** (cut) | a statement about this call is already Proved | the statement was Proved earlier | `lemma_apply`; given by `judging`, or guessed |

Three "you don't have to iterate" rules people usually reach for map onto these:

- *The predicate has finitely many options.* This is the case where split and generic run
  finish the tree on their own. The type need not be finite: `{n} => succ(n)` has one
  leaf, because it never looks at `n`.
- *The predicate defines an inductive datatype.* Induction, with the recursion in the
  type's recognizer.
- *The step function of a fixpoint has a property.* Induction again, with the recursion in
  the program. The tree is infinite but repeats.

Cyclic proof theory studies exactly this shape: case splits with back-edges, sound when on
every infinite path some inductive definition is ["unfolded infinitely
often"](https://era.ed.ac.uk/server/api/core/bitstreams/de6ecd6d-65fc-4b72-a0a1-f383b1748f5a/content#page=2).
Brotherston's thesis conjectures that ["proof by induction is equivalent to regular proof
by infinite descent"](https://era.ed.ac.uk/server/api/core/bitstreams/de6ecd6d-65fc-4b72-a0a1-f383b1748f5a/content#page=3).
disp has the smaller-piece kind of back-edge for `fix(step)` and the one-step-later kind
only for streams.

## 2. Where annotations start

Sometimes the tree neither ends nor repeats. Then the run is stuck on a value it can't
split, and only a stated claim about that value helps. That is a cut. It is known to be
unavoidable: cyclic proofs lose cut elimination, and the published counterexample is about
[two addition predicates](https://arxiv.org/pdf/2106.11798#page=1).

disp shows the same thing, with `add` from `lib/tests/fixtures.disp`:

```
rec times :: {a : Tree, b : Tree} -> Tree :=> if (is_zero(a)) { zero } else { add(b, times(nat_pred(a), b)) }
square := {k} => times(k, k)
```

Measured in `lib/tests/rules.test.disp`, with `nnn := ArrowC(NatR, ArrowC(NatR, NatR))`:

| Claim | Lemmas | Verdict |
|---|---|---|
| `add : nnn` | none | Proved: split, run, run on the hypothesis |
| `times : nnn` | none, no guessing | Open, "split depth exhausted" |
| `times : nnn` | `add : nnn` given | Proved; the successor case rests on induction and the lemma |
| `times : nnn` | none, guessing on | Proved the same way: `add` is found inside `times` and tried at `nnn` |
| `square : ArrowC(NatR, NatR)` | none, guessing on | Open: `times`'s type is not built from `Nat -> Nat` |
| `square : ArrowC(NatR, NatR)` | `times : nnn` given | Proved |

So the limit is precise. A checker can guess cuts from the types already in the goal. That
list is finite, so the finitely-many-options rule applies to it. A callee whose type is not
among the goal's own is where an annotation is needed. It cannot do without cuts.

## 3. The base point

Each step's law is a claim over all trees, so it needs checking too. The regress ends in
three places.

- **The machine's definition.** Split, generic run and induction rest on three facts: a
  tree has three shapes, a tree is finite, and only triage inspects. They mention no type
  and no program.
- **Running.** A type's case table comes from running its recognizer through those three
  steps. So a new type adds nothing to trust.
- **An earlier verdict.** This covers lemmas, given or guessed. It also covers any faster
  checker whose lens into the slow one was Proved by the slow one. The Milawa prover is
  built this way, as ["a sequence of reflective extensions from its
  kernel"](https://www.cl.cam.ac.uk/~mom22/2015-jar-milawa.pdf#page=1).

The weak spot was that disp had three evaluators: the native one, the pausable machine in
`lib/machine.disp`, and a separate symbolic one in `lib/confirm.disp`, with the generic-run
law in effect the claim that the third agrees with the first. There are now two. The
symbolic evaluator is the machine itself: `m_rule` steps both, and the only addition is
`held`, the applications that would read an unknown value's shape, each answered by
`answer_held`. What is still tested and not proved: that the native evaluator agrees with
the machine (`test/machine.test.ts`), and that `held` catches every read of a hole.

## 4. Self-consistency (Torre, Willard)

James Torre's [research programme](https://github.com/jpt4/sjas#sjas) is about Dan
Willard's self-justifying axiom systems. They are real and they don't break Gödel's second
theorem. The price is in Willard's abstract: the system ["views multiplication as a 3-way
relation"](https://arxiv.org/pdf/2006.01057#page=1) and not as a total function, and its
deduction is cut-free. With ordinary deduction, which has modus ponens, [a system that
recognizes even successor as total cannot recognize its own
consistency](https://arxiv.org/pdf/2006.01057#page=8). Two things matter here.

- The self-consistency is not derived. It is ["a builtin self-referencing axiom that
  declared 'I am consistent'"](https://arxiv.org/pdf/2006.01057#page=2). The proof that
  this axiom does no harm is done outside, in a stronger system. [Torre's
  post](https://jpt401.substack.com/p/on-the-strangeness-of-multiplication) says the same
  in a footnote.
- Willard's line is the line in section 2: cut-free deduction, addition total,
  multiplication not. disp's checker with no lemmas and no guessing lands in the same
  place. That is suggestive and not a result. disp's induction step is a rule Willard's
  systems lack, and nothing here shows the lemma-free checker is below his line.

What to take from it: a system that proves its own soundness adds no trust, because a
broken system would prove it too. Three things do add trust. The laws stay refutable by
brute force, which the tests do. Everything else is proved relative to the three facts,
stated once; MetaCoq does this for Coq, since ["there is no hope to prove completely the
correctness of the specification of Coq inside
Coq"](https://sozeau.gitlabpages.inria.fr/www/research/publications/Coq_Coq_Correct-POPL20.pdf#page=1),
so it assumes strong normalisation and proves the checker correct on top of that. And the
three facts can get a Lean proof outside.

## 5. The closest elegant wholes

- **Abstract interpretation.** [Cousot](https://cs.nyu.edu/~pmc309/publications.www/Cousot-POPL97-p316-331-1997.pdf#page=1)
  starts from one most precise semantics and gets "a hierarchy of type systems, which is
  part of the lattice of abstract interpretations". That is brute force on top and lenses
  below. His soundness map is the lens's backward map.
- **Semantic typing.** In [RustBelt](https://plv.mpi-sws.org/rustbelt/popl18/paper.pdf#page=4)
  a type is a predicate on programs and each typing rule is proved "as lemmas". Types are
  the tool laid over programs, and programs stay untyped.
- **Exhaustible sets.** [Escardó](https://martinescardo.github.io/papers/exhaustive.pdf#page=1)
  asks which infinite sets can be searched completely: "exhaustible sets are to compact
  sets as computable functions are to continuous maps". They are closed under images and
  products, which is the compositional way to build covers.
- **One evaluator.** [Abstracting Definitional Interpreters](https://arxiv.org/pdf/1707.04755#page=2)
  gets the abstract evaluator from the ordinary one by swapping a few operations. It also
  names the trap in the induction step: "a naive use of caching for fixed-points will
  guarantee termination but is inherently unsound".
- **The "later" operator.** In [Appel et al.](https://www.cs.princeton.edu/~appel/papers/modalmodel.pdf#page=2)
  a hypothesis holds only "strictly in the future". That one idea covers induction on data
  and induction on steps.

The earlier survey found nobody else with laws that the same brute-force search can
refute.

## 6. What was built from it (2026-11-10)

1. **One loop.** `settle_case` in `lib/confirm.disp` settles a case by the first of:
   the exact run answers Yes (and says whether the answer cites the hypothesis or a
   lemma); a known case fails (a counterexample); a later check, the same run with lemmas
   read by their types, answers Yes; the run needed a piece, which is split. `certify` is
   this loop with no splits. Every case is run exactly before a lemma is consulted, so a
   lemma no longer hides a counterexample.
2. **The work list is data and a lens.** `s_steps` in `lib/stream.disp` yields every case
   with what settled it; the verdict reads its counterexamples. `derivation` in
   `lib/rules.disp` reads the list back, for doubling:

   ```
   test derivation(plainly, nn, dbl) = [
   	pair([n], pair("split", n)),
   	pair([0], pair("run", [])),
   	pair([succ(below_n)], pair("run", ["induction"]))]
   ```

   `steps_lens` composes the splits with `lens_then`: a tree goes back to the case that
   settled it. `steps_law` is its law, and `lib/tests/rules.test.disp` shows search finds
   nothing against a real work list and refutes one that lost a case.
3. **One stepper.** Section 3. It also keeps the S rule's discard, which the separate
   evaluator lacked, and charges no fuel for builds, as the machine's step unit says.
4. **Guessed lemmas.** `judging` has a fourth setting, `guesses`: the types a value's
   recursive sub-programs are tried at when it is left Open. The default, `goal_types`, is
   the function types the goal's own type is built from.

5. **Functions as arguments (2026-11-11).** A program given where a function is expected
   is judged as one: `fn_member` in `lib/confirm.disp` runs it on a fresh piece of the
   domain and splits where it looks, which is the loop's generic run and case split,
   nested. Before, a lemma's function argument had to be an unknown piece.
6. **Eliminators come with the type (2026-11-11).** A type with cases has an eliminator,
   derived from its cases as the cases were derived from the type: `fold_of(T)` in
   `lib/confirm.disp`. Nothing is given beside the type. Programs written with it have no
   recursion of their own:

   ```
   plus_e := {a, b} => nat_fold(a, b, {_k, r} => succ(r))
   times_e := {a, b} => nat_fold(a, 0, {_k, r} => plus_e(b, r))
   square_e := {k} => times_e(k, k)
   ```

   The eliminator's tree says what it is, so on a hole the run keeps the use as a neutral
   term, and `judge_as` judges it by the type's own rule: what is eliminated is a member,
   and each handler takes its case's pieces, and the answer below each piece of the type,
   into the result type. The rule is case split and induction on a part of the program.
   It is not a lemma: no statement is looked up, and `judge` takes no list. Each of the
   three is Proved in one case resting on "elimination" (`lib/theories/types.test.disp`).
   The rule is the eliminator's statement for every family of types, and the same layer
   run as a plain recursive program is Proved at that statement by the three older steps,
   which is the check on the rule. A tree that only says it is an eliminator is compared
   with the derived one, and runs as the program it is.
7. **Result types that depend, and equations that compute (2026-11-11).** The result type
   of a stuck elimination may depend on what is eliminated: it is the type expected, with
   a value put for what is eliminated. A type that depends on values holds them apart
   (`given`) and says how one is put for another (`put`), so that step is exact and walks
   no code. An equation computes by shape (`places` in `lib/theories/types.disp`): where
   both sides are known they must agree, and what is left is the places where a side is
   unknown. So `succ(a) = succ(b)` is `a = b`, and the step of a proof by induction is its
   own hypothesis:

   ```
   plus_zero_t := pi_t(nat_t, {n} => eq_t([n], {m} => plus_e(m, 0), {m} => m))
   test judge(plus_zero_t, {n} => nat_fold(n, t, {_k, ih} => ih)) = "Proved"
   ```

   Equality has an eliminator too, `cast(e, d)`, which is d. Its rule is part of the
   equality type (`elim` on the behaviour record): d is judged at the type expected,
   rewritten by e. With it an equation already Proved is used in a later proof, and
   adding is commutative from `n + 0 = n` and `n + succ(m) = succ(n + m)`. That use is the
   one lemma left: the two equations are read by their types where they are called.
8. **A theory checked with no typing rule (2026-11-11).** `judges` in
   `lib/theories/pts.disp` puts a term's erased program to `judge` at its type's meaning,
   with nothing else. A theory is the types its formers mean, so the one `judge` is the
   judge of each theory. System T's addition, multiplication and Ackermann are Proved that
   way, and MLTT's list and tree programs, and its proofs of `n + 0 = n` and of appending
   the empty list, where `transport` runs as `cast`. A proof whose step is just its
   hypothesis is Proved too, though MLTT's written rules refuse it: they compare
   `S(k + 0) = S k` and `k + 0 = k` as written. What stays with the written rules is
   refusals about how a term is built (System T has no function over types; `judge`
   proves the identity over them).

## 7. Open

- **Equations, further.** Rewriting with an equation puts one side for the other as
  written and does not run the result again, so a use that needs a second step of
  computation after rewriting stays Open. An equation between functions is only "the same
  tree"; the type's own relation (the same answers on every argument) is not used. And
  `:` annotations cannot state an equation over two arguments: the type under the second
  binder is built inside a symbolic run, which cannot make pieces.
- **Wider guesses.** `square` above stays Open. Arrows over the goal's first-order types,
  up to two arguments, would find `times`. It is one function passed as `guesses`; the
  cost is one failed search per wrong guess.
- **Induction on a later argument.** The hypothesis is planted where a `fix` meets its
  first unknown argument, so a power function recursing on its second argument stays Open.
- **The run without the hypothesis as a fallback.** A function whose recursive call is on
  a known value, not a smaller piece, is stuck under the hypothesis and would run through
  without it.
- **One-step-later hypotheses for programs.** Only streams have them (`stream_eq`).
- **A native stepper that holds.** The machine's native stepper runs fully known
  applications. One that also stops at a hole would make the symbolic run native too.
- **The three facts in Lean.**
