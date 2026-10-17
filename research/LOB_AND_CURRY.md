# Löb and Curry over trees: what the paradox is, and where disp and Bend refuse it

Research note, 2026-10-17. It writes up a conversation that started from BendTT's
"no positivity restriction" and ended at disp's certified checkers. Nothing here is
landed code. The four Bend programs in §4 were run against Bend 2.0.4 (`bend2/main.ts`
in the bend2 checkout) and their output is quoted exactly. Everything about disp cites
the files as they stand on this date: `research/CLAIMS_OVER_TREES.md` §7 and §9,
`lib/claim.disp`, `lib/tests/claim.test.disp`, `lib/verdict.disp`, `GRADUAL.disp` and
`CHECK.disp`.

Three existing notes state the conclusion without the mechanism:
`FOUNDATIONS.md` [P3] "the Löbian ceiling", `research/THE_NATURE_OF_ABSTRACTION.typ`
"the Löbian wall", and `archive/TOWER.md` "the Gödel boundary". This note is the
computational account underneath them: what the self-referential program does when it
runs, why a checker is the only place it can do harm, and which line in disp's design
is the one Löb's theorem draws.

A naming warning. `research/REVERSIBLE_PIPELINE.md` uses "receipt" for the record an
irreversible rule leaves so a run can go backward. The conversation this note comes
from used "receipt" for checkable evidence of a claim. To keep the two apart, this
note says **certificate** for the second meaning.

## 1. The answer in six lines

1. Curry's paradox is a program that loops. At run time nothing contradictory happens.
2. It becomes a paradox only inside a checker that accepts a claim that a run succeeds
   before the run has succeeded.
3. There are three ways to refuse: forbid the type that allows the loop (positivity),
   forbid the copy the loop needs (affinity), or never believe a claim without evidence
   (disp's verdicts).
4. Löb's theorem is the same loop seen through a box: evidence that may be mentioned but
   never cashed in. Without a uniform way to cash the box in, the loop is harmless and
   the result is a theorem. With one, it is Curry.
5. In tree calculus, cashing in is not application. It is a total procedure from "this
   run succeeds" to the success, which is the halting problem. Open is the record that
   no such procedure exists.
6. In disp's current design, a certified checker is a lens into the honest claim, and its
   soundness is a claim over all trees. That soundness is the one thing that cannot be
   pulled inside. It is proved once per rule by hand, and those five rules are the floor.

## 2. Curry's paradox as a program

### 2.1 The term

Take a datatype with a field whose type mentions the datatype on the left of an arrow,
aimed at an arbitrary result type `A`:

```
type T:  Lam{f: T -> A}
omega(Lam{f}) = f(Lam{f})        -- omega : T -> A
omega(Lam{omega})                -- : A, for every A, Empty included
```

Reducing the last line gives the last line again. It is the Y combinator with a
constructor around it. The logical version is the sentence "if this sentence is true
then A". To use it you need it twice: once as the implication and once as its own
premise. That double use is contraction. Logics without contraction admit naive
comprehension safely for exactly this reason (Grishin 1982, Terui 2004, both cited by
the BendTT paper).

### 2.2 Where the harm actually is

The machine running `omega(Lam{omega})` is doing an ordinary thing: not stopping. The
harm exists only for a type checker, because a type checker's job is to promise facts
about programs it does not run. If it accepts the term at type `Empty`, it has counted a
run that never finished as a value. Every refusal below is a decision about which part
of that promise to withdraw.

### 2.3 Three refusals

| Refusal | Used by | What it forbids | What it costs |
|---|---|---|---|
| Positivity | Coq, Agda, Lean | declaring `T` at all | higher-order abstract syntax, many encodings |
| Affinity | Bend | using `f` twice | an explicit kind discipline for copying |
| Withheld belief | disp's stream verdicts | saying Proved without evidence | the checker is a semi-decision procedure |

**Positivity.** A field may not mention its own type left of an arrow. The type has no
initial algebra, so it is not admitted as a type.

**Affinity.** BendTT admits `T` and refuses the program. A live variable is used at
most once unless its binder is marked `+`, and a `+` binder forms only at kind `Data`.
No function type is `Data`. `omega` uses `f` once as the function and once inside the
argument, so the usage counter rejects it. The repo's test is
`tests/check/negative_data_reuse.bend`:

```
#|- expected : f
#|- observed : f (consumed more than once)
```

The paper's phrase for what survives is that a value of a negative type "is at worst an
inert closure". Physically, in the interaction-net runtime, the infinite unfolding needs
a duplicator feeding a closure back into itself, and Bend provides no duplicator for
closures.

**Withheld belief.** disp admits the type and the program, and does not promise
anything about the run. A type is a recognizer on trees. The honest checker is the
witness stream of `GRADUAL.disp` §6, or `honest` in `lib/claim.disp`, which searches for
a failing challenge. `lib/verdict.disp` has three answers. A refutation carries a witness
that re-runs. A Proved comes only from exhausting a finite source or from a certified
shortcut. Everything else is Open. The term `omega(Lam{omega})` never yields, so any
claim about it stays Open forever, and the paradox cannot be stated as a wrong verdict.
The cost is stated in `research/STREAM_TYPES_ONTOLOGY.md` §5: universal claims over
semidecidable members have no enumerable failures, so the checker decides nothing above
that level without help.

## 3. Löb's theorem as a program

### 3.1 The classical statement and what it hides

Löb's theorem says that if a theory proves "if A is provable then A", it proves A. In
the internal form it reads `□(□A → A) → □A`, where `□A` means "A has a proof". The
textbook proof spends most of its length on Gödel numbering and the diagonal lemma,
which together let a formula mention its own proof. What remains after that machinery is
Kleene's second recursion theorem: a program can be built that refers to its own code.

Written as a program, with `□A` meaning "a proof of A, held but not run":

```
loeb : (□A -> A) -> □A
loeb f = box (f (loeb f))
```

Given a function that turns a boxed proof of A into an actual A, build the program
`p = f(box p)` and return it boxed. Nothing is ever run.

### 3.2 Curry is Löb plus one operation

Now suppose there were also an operation `unbox : □A -> A` that works for every A. Then
`loeb unbox` is `box (unbox (loeb unbox))`, and unboxing it is `omega(Lam{omega})`. The
two constructions are one construction:

- **Löb's theorem** is the diagonal with no uniform unbox. It is a theorem.
- **Curry's paradox** is the diagonal with a uniform unbox. It proves anything.
- **"A theory cannot prove its own soundness"** says exactly that uniform unbox,
  `□A → A` for every A, is not available. It is not a limitation peculiar to set
  theory. It is the refusal of the one operation that would make the diagonal loop.

### 3.3 Why "unbox" is not "eval", and what tree calculus clarifies

In tree calculus a program is already a tree, triage can inspect it, and there is a fixed
point combinator. So quoting is the identity, the diagonal lemma is ordinary recursion,
and a function of type `□A -> A` may even read the syntax of the proof it is given.
Application is total on trees. So the box cannot mean "quoted and not yet run", because
every tree is both.

The box has to mean something about the run. Let `A` be the set of trees a checker
accepts, defined by "the check reduces to true", which may take any number of steps.
Then:

- a member of `A` is a tree on which that reduction lands;
- a member of `□A` is a tree that *says* the reduction lands.

Both are trees. The missing operation is a **total** tree that turns the second into the
first for every A. That is a procedure deciding whether runs land, which does not exist.
What does exist is a partial procedure that pursues the run and reports when it lands.
That procedure is the honest stream, and its third answer, Open, is the whole content of
"there is no `□A → A`".

So in a reflective calculus Löb's theorem reads: **no total tree turns a claim of
termination into the terminated value, and any checker that acts as if one existed can be
handed `p = f(p)` and made to accept anything.** Set theory obscures this because it has
no run time at all.

## 4. Bend: the dead level is the box, checked

BendTT has three quantities, spelled `-x`, `x` and `+x`: erased, live once, live many.
Erased code is checked "dead". The paper says it "may diverge or inhabit Empty by
design" and that "nothing promotes it to live". It also says descent applies only "at a
live self-reference", with erased columns skipped. Read against §3, an erased argument
`-p: A` is `□A`, and "nothing promotes it to live" is the absence of uniform unbox.

That predicts four behaviors. All four were run.

**Löb with the self-call in an erased argument checks and runs.**

```
law loeb:
  for f: @-p: U32 -> U32
  U32

def loeb(f):
  f(loeb(f))

def main():
  IO.print(U32.show(loeb(p => 7)))
```

```
7
```

The recursive call sits in `f`'s erased argument, so descent does not look at it and the
usage counter charges it zero. The erased argument is never built at run time, so the
self-call is never evaluated. The box is literally not computed.

**The same term with a live argument is refused, by descent.** Changing the law to
`for f: U32 -> U32`:

```
Error:
- expected : a decreasing self-call (arguments are read left to right: each passed unchanged until one shrinks)
- observed : loeb
```

**Skipping descent is not enough: affinity also refuses it.** Adding `@unsafe` above the
def:

```
Error:
- expected : f
- observed : f (consumed more than once)
```

`@unsafe` skips descent, but the live term still uses `f` twice: once called, once passed
to the recursive call. So the live Löb term is blocked by two independent walls. This
corrects a claim made during the conversation, which said `@unsafe` alone would make it
check and loop.

**With both walls lifted, it is Curry and it loops.** `@unsafe` also lets a `+` binder
form at a function type (`tests/check/unsafe_many.bend`), so the law becomes
`for +f: U32 -> U32`:

```
bend: memory fault (machine stack overflow?)
```

Even though `p => 7` ignores its argument, the argument is live, so it is evaluated, and
evaluating it is the loop.

**The box cannot be cashed in for Empty.** With the erased version at `A = Empty`, the
obvious attempt to get a live `Empty` is to pass the identity:

```
law bad:
  Empty

def bad():
  loeb(p => p)
```

```
Error:
- expected : -p
- observed : p
```

The function receives `p` dead and tries to return it live. That is uniform unbox, and
the checker refuses it. The paper's sentence "a dead `-e: Empty` in scope ... licenses
nothing" is Gödel's second theorem in one clause: holding `□⊥` proves nothing live.

So Bend realizes §3 exactly. The dead level is the box. Löb is a legal definition.
Curry needs the box to be cashed in, and the checker forbids that both by usage and by
the dead-to-live rule. Its consistency claims (subject reduction, progress, weak
normalization and no closed live `Empty`) are stated only at the live demand, which is
the same restriction provability logic makes.

## 5. disp: certificates, lenses and the floor

### 5.1 The question that started this section

A conjecture from the conversation: suppose a restricted checker is total and
under-accepts, for instance by abstract evaluation, but whatever it accepts is correct.
Can we have a general proof that the honest stream for a claim it accepts never refutes
at any finite sample count?

It splits into three parts.

- **"Never refutes at any n" means "holds for every member".** A statement about all
  finite prefixes is a statement about all members. Sampling can refute it and never
  establish it.
- **For a given checker, it is true exactly when the checker is sound, and soundness is
  proved per checker.** `CLAIMS_OVER_TREES.md` §7 states the method: "one case per
  checker rule (the fundamental lemma)", by induction on the checker's derivations,
  bottoming out in the run lemma, "proved once by hand". There is no general proof for an
  arbitrary total checker, because soundness of an arbitrary checker is itself a claim
  above one quantifier alternation. That is Löb again: no checker of all checkers, only a
  proof per checker, made one level up.
- **"Never refutes" is the weaker half.** If members may diverge, a checker that accepts
  a claim whose member loops is never refuted and still wrong in the sense that a
  reported witness must re-run. Abstract interpretation gives the weak half, partial
  correctness. Total correctness needs a step bound or a measure, which `CLAIMS_OVER_TREES.md`
  §4 describes as the witness that removes the "exists a budget" quantifier.

So the conjecture holds for the checkers disp builds, by a proof outside the stream that
is organized as induction on what the checker did.

### 5.2 What a certificate is

`lib/verdict.disp` already has half of the idea. A refutation is `refuted w`, and its
comment says `w` "re-runs concretely". That witness has the two properties that define a
certificate:

1. **Checking it is total.** Re-run once. No fuel policy and no Open outcome.
2. **Passing the check implies the honest verdict, by a theorem proved once.** For a
   refutation the theorem is trivial, since `w` is one of the honest stream's own
   challenges.

A positive certificate is the same shape in the other direction: finite evidence, a total
check, and a once-proved theorem that a passing check implies every challenge passes.
The conversation first described this as a transcript of a symbolic run plus a replay
validator. The current design in `CLAIMS_OVER_TREES.md` §9 says it more cleanly.

### 5.3 The current design: claims as games, checkers as lenses

From `lib/claim.disp`:

- **A claim** is a program `{witness, challenge} => Bool`. It holds when some witness
  passes every challenge. Every quantifier prefix has this shape once witnesses may be
  functions, which is Gödel's Dialectica form.
- **`honest`** searches a source of challenges for one the witness fails. It can only
  refute. **`over`** runs a listed source, which can run out, and running out is the
  only way search proves anything.
- **A lens** carries witnesses forward and challenges back. A certified checker is a lens
  from a small symbolic claim into the honest one. Its law, `lens_holds`, says a witness
  passing the pulled-back challenge is carried to a witness passing the challenge. Read
  backwards: every counterexample to the honest claim pulls back to a counterexample to
  the symbolic one.
- **`certify`** in `lib/tests/claim.test.disp` reads a finite symbolic claim as a
  certificate of the honest one. It answers Proved when every symbolic case settles, a
  refutation when a ground case fails, and Open with the unsettled case otherwise.

In this vocabulary the parts of §5.2 are named exactly.

| Role | In disp now |
|---|---|
| the honest checker | `honest`, or `over` on a finite source |
| the shortcut | `certify` applied to a symbolic claim and its cases |
| the certificate | the symbolic claim's finite case list, with the lens that maps every tree to its case |
| checking the certificate | `over` on that finite list, which always ends |
| the once-proved theorem | the lens law for each of the five rule lenses |

### 5.4 The floor is exactly Löb's line

`CLAIMS_OVER_TREES.md` §9 ends: "the lens laws are for-all claims, so trust stops at the
five rules, proved by hand". The five are finite cover, case split, generic run,
induction and composition. Each law is a claim over all trees. The honest checker can
refute a wrong law with a concrete tree. It cannot prove a right one. The test root pins
exactly this: searching the induction law `ind_holds` for the doubling pair answers
`"Open"`, while `certify` on the same pair answers Proved. Proving the law by search
would be the checker certifying its own acceptance in general, which is the uniform unbox
of §3.2.

So the hand-proved floor is not a temporary gap waiting for more automation. It is the
place Löb's theorem says must exist. `archive/TOWER.md` reached the same conclusion
from the other side: "not a system that proves itself right, which nothing can be, but a
system whose every unproved assumption fits on one page". The design question is only
how small that page can be kept. Five rules is small.

Read through the box, the three Hilbert–Bernays conditions Löb's proof uses become
properties of these lenses. This is an analogy, not a proved correspondence.

| Condition | Lens reading |
|---|---|
| a proof of A gives a proof of `□A` | a Proved by `over` is a certificate |
| `□(A→B)` and `□A` give `□B` | `lens_then` composes certified lenses |
| `□A` gives `□□A` | checking a certificate is a finite run, so it is itself certified by `over` |

### 5.5 Induction, descent, and a Curry-shaped hole in the old checker

`CHECK.disp`'s `check_by_induction` still carries this comment: "Missing certificate:
nothing checks the recursive call happens on something SMALLER (the decreasing-measure
debt)." Without that check the induction hypothesis may be applied at the root itself.
That is the live Löb term of §4: a self-reference that the checker believes before it has
any evidence. It is the same hole Bend's descent test closes.

The claim layer closes it by construction. In `lib/tests/claim.test.disp`, the
induction law `ind_holds` asks for the claim at `nat_pred x` and at the case of `x`, and
for nothing else. The back map hands the hypothesis only the predecessor. There is no way
to state the hypothesis at the root, so no separate measure check is needed. This is
Bend's rule, "the same constructor with a strictly smaller field", turned from a syntactic
test after the fact into a property of what the induction lens can ask for.

### 5.6 What under-acceptance looks like

A shortcut is sound and incomplete. The test root shows both sides.
`certify (sym_eq id shift) nat_cases` is Proved. `certify (sym_eq (fix dbl_step)
(fix dbl_step_pairs)) nat_cases` is Open, because one symbolic run does not settle a pair
that needs induction, and it becomes Proved only under `ind_eq`. A true claim with no
certificate in the current rule set stays Open. That gap between "has a certificate" and
"is true" is Gödel's first theorem in a checker short enough to read.

## 6. The agent-foundations reading

These points restate known results in the vocabulary above. They are not new results.

- **The Löbian obstacle.** An agent that trusts every proof it finds about itself has
  uniform unbox, so the diagonal makes it trust anything. That is Curry. Positivity and
  affinity are two static ways to deny the self-application. The induction lens's
  "only at the predecessor" is the well-founded version of "trust the next level from
  this one", with a shrinking index.
- **Bounded reflection.** Critch's parametric, resource-bounded Löb theorem keeps the
  diagonal alive when proofs have bounded length. Affinity charges a different resource on
  the same diagonal: the self-reference is allowed, but it is consumed.
- **Modal agents and benign fixed points.** In the modal-agent work of Barasz et al. on the prisoner's
  dilemma, "cooperate if I can prove you cooperate" is a fixed point that resolves to
  cooperation, by Löb. The difference between that and Curry is whether the loop bottoms
  out in evidence. In disp terms: act on what a certificate check returned, never on a
  claim about what a search would eventually return.
- **Self-improvement.** `THE_NATURE_OF_ABSTRACTION.typ` already freezes the trusted core
  and the license-checking machinery outside the optimization loop. §5.4 explains why that
  is forced rather than prudent: if optimization pressure could change the five rule
  lenses, it would be changing what counts as a certificate, which is uniform unbox by
  another route.

## 7. What stays open

- **The five laws are proved by hand.** They are refutable by search and not provable by
  it. A machine-checked proof would have to live in another system, as Bend's does in
  `bend.lean`.
- **Total correctness needs a witness.** Without a step bound or a measure, a certified
  claim is partial correctness only.
- **The backward maps are hand-written.** `CLAIMS_OVER_TREES.md` §9 names deriving them
  from a run's origins (§6 of that note) as the next step. Until then each shortcut's
  lens is written by its author.
- **`CHECK.disp`'s induction checker still has the hole of §5.5.** It is superseded by
  the claim layer's `ind_holds`, but it is still in the repo and still answers Proved.

## Sources

- The BendTT paper, `paper/BendTT.pdf` in the bend2 checkout, especially the sections
  "Negative Types Are Legal, and Harmless", "The License Cannot Be Forged" and "The
  Claims, and Their Price". Its bibliography carries Curry 1942, Grishin 1982 and Terui
  2004.
- Bend's tests `tests/check/negative_data_reuse.bend`, `tests/check/unsafe_loop.bend`
  and `tests/check/unsafe_many.bend`, and the four programs in §4.
- Löb, "Solution of a problem of Leon Henkin", 1955. Kleene's second recursion theorem.
- [Critch, "Parametric Bounded Löb's Theorem and Robust Cooperation of Bounded Agents"](https://arxiv.org/abs/1602.04184).
- [Barasz, Christiano, Fallenstein, Herreshoff, LaVictoire and Yudkowsky, "Robust Cooperation in the Prisoner's Dilemma: Program Equilibrium via Provability Logic"](https://arxiv.org/abs/1401.5577), 2014.
- de Paiva's Dialectica categories, linked from `CLAIMS_OVER_TREES.md` §9.

The two arXiv entries were resolved with `scripts/verify-source.sh` and their titles
match. Löb 1955 and Kleene's recursion theorem are cited from memory and were not
fetched.
