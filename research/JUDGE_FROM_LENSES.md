# Judge from lenses: proposed changes

Status: a proposal, revised after an independent review (section 8). Change 1 is implemented
(section 5); nothing else is. The table in section 3 shows the checker before change 1. Every
probe ran in a throwaway module that was deleted.

Words used here. `judge(T, w)` (lib/rules.disp:262) gives a verdict for a whole program `w` at a
type `T`: Proved, Refuted with a counterexample, or Open with a reason. `judge_as(T, v)`
(lib/confirm.disp:758) asks once whether a partly-known value is in a type. A claim is a
program `{witness, challenge} => Bool`. A lens between two claims carries a witness forward and
a challenge back (lib/claim.disp:19); its law says a witness that passes the pulled-back
challenge is carried to one that passes the challenge (`lens_holds`). A piece is an unknown
part of a value, named and typed. A neutral term is a call nobody can run, such as an unknown
function applied to an argument.

## 1. The goal

Two wishes, from the design discussion:

1. `judge` should be written with lenses whose laws are proved, since every shortcut it takes
   is one a lens allows.
2. A type whose check ends should be a lens composed with a stream, and a type former should be
   able to look at the lenses its parts bring and say whether they fit together.

## 2. Where the code stands

- The loop does each shortcut by hand (`settle_case`, lib/confirm.disp:872). The lens is
  written afterwards: `steps_lens` (lib/rules.disp:303) folds the case splits of a finished work
  list into one lens, and tests attack its law by search.
- Only the case split is ever a lens value. Induction and lemmas are a hypothesis piece and a
  lemma hole inside the run (`induct`, lib/rules.disp:101; `with_lemmas`, lib/rules.disp:196).
  What a Yes rests on is a list of strings (`cited`, lib/confirm.disp:845). Induction's law
  (`ind_holds`, lib/rules.disp:72) is used only by tests, and it is not of the `lens_holds` shape.
- A Proved already rests on little of the loop: that each settled case's check said Yes, and
  that each split's cases came from `alternatives` (lib/confirm.disp:621). The search order
  and the choice of what to split cannot make a wrong Proved. What is trusted lives in the
  check itself (`run_at`, lib/rules.disp:109): the evaluator on partly-known values, induction,
  the reading of lemmas, and a function type's own check.
- No law is proved by the checker by search: each is a claim over every tree, and search can
  only refute one. research/LOB_AND_CURRY.md §5.4 names five rules as taken on trust (finite
  cover, case split, generic run, induction, composition). The header of lib/rules.disp says
  three atoms "are proved by hand once" and points at that section, which holds no proofs.
- There are three loops that split. `judge` makes every argument up front (`generic_args`,
  lib/rules.disp:92; arguments past the eighth are left to the function type's check) and runs
  the work list. A function type's own check (`fn_member`, lib/confirm.disp:632) makes one
  argument, runs, and splits, nested per arrow. `explore` (lib/confirm.disp:338) derives a
  recognizer's cases.
- A type's record (`behaviour`, lib/confirm.disp) has a required check (`admits`) and optional
  fields: `cases` (it can be split), `generic` (how to make an unknown member; no type in lib/
  sets it), `elim`, `normal`. A function type reads its domain's and its result type's fields
  while it is being judged. A missing one shows up then, as Open.

## 3. What the probes showed

A law is also a type: "passes the pulled-back challenge" implies "passes the challenge" is a
function type, and a proof of it is a program. So a proved lens is one whose law `judge` says
is Proved.

One thing to know when writing such a type: `eq_t` gives a side its environment last entry
first. In `eq_t([x, v], {a, b} => ..., ...)`, `a` is `v`. The first draft of this document had
the two the wrong way round, and its results for the cover law were for the wrong statement.

```
bool_fn := {v, x} => if (Bool(x)) { Bool(v(x)) } else { true }
pull := {x} => if (Bool(x)) { x } else { true }
small_t := pi_t(bool_t, {x} => arrow_t(eq_t([x], {a} => a, {_a} => true), eq_t([x], {a} => a, {_a} => true)))
cover_t := pi_t(tree_t, {v} => pi_t(tree_t, {x} => arrow_t(eq_t([x, v], {vv, xx} => bool_fn(vv, pull(xx)), {_a, _b} => true), eq_t([x, v], {vv, xx} => bool_fn(vv, xx), {_a, _b} => true))))
bbb := arrow_t(arrow_t(bool_t, bool_t), arrow_t(bool_t, bool_t))
bool_fold := fold_of(bool_t)
```

`cover_t` is the law of the finite cover of `Bool -> Bool` by two challenges (`bool_cover` in
lib/tests/claim.test.disp). Its witness `v` has to range over every tree. Typed as
`Bool -> Bool` the statement is empty: `bool_fn(v, x)` is then true for every `x`, and the law
holds even for the bad cover that the claim form refutes (lib/tests/claim.test.disp:33).

| Statement | Proof | `judge` before change 1 |
|---|---|---|
| `small_t` | `{x, h} => if (x) { h } else { h }` | Open, reason "No" |
| `small_t`, by the function type's own check (`judge_as`) | the same | Yes |
| `cover_t` | `{_v, x, e} => if (Bool(x)) { e } else { t }` | Open, reason "No" |
| `bbb` | `{v, x} => v(not(x))` | Proved |
| `bbb` | `{v, x} => not(v(x))` | Open, "opaque value" |
| `bbb` | `{v, x} => bool_fold(v(x), false, true)` | Proved |
| "two lenses that hold compose to one that holds", over unknown claims of type `Tree -> Tree -> Bool` and unknown maps | the two hypotheses applied in turn | Proved; a proof that pulls back the wrong challenge is Open |

What this says:

- **The composition law is proved by the checker today.** It needs only the function rule on
  unknown functions. So composition can leave the list of rules taken on trust.
- **A later argument keeps an old type.** `judge` makes all arguments first, so the type of a
  hypothesis is computed with the earlier argument unknown. When that argument is split,
  `alternatives` puts each case in with `subst` (lib/confirm.disp:303), and `subst` left
  every hole as it was, so the hypothesis kept the type it had before the split. That is the
  "No" in the first and third rows. `fn_member` does not have the problem: it makes an argument
  only after the ones before it are in.
- **Looking at the result of an unknown function works through the eliminator and not through
  `if`.** A triage on a call is stuck as an "opaque value" (`inspect`, lib/confirm.disp:209).
  The same question asked with the type's eliminator is a stuck elimination, which is read by
  its type, and is Proved.
- **A law that calls an untyped witness is harder.** A call of a piece of type Tree asks for
  the piece's shape (`apply_hole`, lib/confirm.disp:518), and trees split without end. Whether
  With change 1 in, `cover_t` is still Open, now with "split depth exhausted": the stale type
  was not the only thing in its way.

## 4. A wrong Proved that exists today

Found by the review while attacking change 2, and confirmed by running. The finite function
type in lib/theories/finite.test.disp (`f_arrow`) lists lookup tables as its cases, but its
members are every tree that behaves like one. A program can read the tree of its argument, so
checking the tables does not cover the members:

```
peek := {v} => if (v(true)) { if (is_leaf(v.fst)) { pair(t, t) } else { true } } else { true }
```

With `bb := f_base([true, false])` and `bb2 := f_arrow(bb, bb)` as in that file,
`judge(f_arrow(bb2, bb), peek)` is Proved. But `{_x} => true` is a member of `bb2`, its first
part is the leaf, and `peek` of it is `pair(t, t)`, which is not in `bb`.

This is in a test file's example type, not in the checker. It still should be fixed or fenced:
either the type's check accepts only the tables themselves, or its cases are opaque (below).

## 5. The changes

### Change 1. A piece's type follows a split

Implemented. Where a value is put for a piece (`subst`, lib/confirm.disp), every other piece
it passes is the same piece at its type with that value put in too. One function
(`type_with`) rebuilds a type from its parts with each value passed through a substitution;
`type_subst` is that with one value for another. A family that ignores its argument stays one
that does, so a plain arrow is still one code after a split. `run_at` is left alone.

Why: a bug in dependent function types. It is safe today (Open, never a wrong Proved), but any
proof that takes a hypothesis and then splits an earlier argument fails, which is most proofs
of a law.

Evidence: `small_t` is Proved. That shows only that the hypothesis is typed for its case:
every program is in `small_t`, since a true equation holds every tree and a false one none.
What can go wrong shows at a type whose second argument is a Nat or a Bool by the first
(`picked_t` in lib/theories/types.test.disp): a program that goes on to look at the re-typed
argument is Proved, and a wrong one is Refuted with a concrete input, where both were Open
before. A type with no members (the hypothesis `x = true`, the goal `x = false`) stays Open.
A hypothesis whose type holds an argument that is not the one split is returned as before.
The full suite passes (265 tests).

Still Open, and safe: a program that is right only because its hypothesis is false at a
case, and does not say so with `cast`. `{_x, _h} => t` is in `small_t` and is Open.

Rejected: re-typing an unsplit argument inside `run_at` (the first draft). It is sound, but the
work list still holds the old piece, so a program that looks at the re-typed argument splits
nothing until the depth runs out, and induction on it fails the same way.

### Change 2. Look at the result of an unknown function

The first draft split a function piece `v` at an argument `a` by putting a real program in its
place, `{y} => if (tree_eq(y, a)) { c } else { rest(y) }`. **That is unsound**: a program can
read a function's tree, the wrapper has a shape, and so a triage on it runs where it must
stay stuck. Section 4 is the same mistake, already in the tree.

What instead, in order of preference:

1. Do nothing new for programs written with the eliminator: that works today.
2. Read a stuck `if` (a triage) on a call whose type has cases as that type's stuck
   elimination, so that `not(v(x))` is judged like `bool_fold(v(x), false, true)`. Not probed.
   The doubt: a triage's branches are not the handlers of an eliminator in general.
3. If a split is still wanted: the function stays a hole, and the case carries the fact
   "`v(a)` is `c`" (or the piece carries a table of known points). A new kind of thing in a
   case, so last.

Open points for 3, from the review: a wrong program could not be refuted while a piece of the
function is left (`ground_args` gives none); the hypothesis, lemma and eliminator pieces are
made inside the run, where a substitution in the case does not reach; the split depth is 3.

### Change 3. Rules as values

What is kept: each step of the work list holds the lens of the rule that made it, so
`steps_lens` folds what the steps hold. An optional tidy-up, with no effect on what is trusted.

What is dropped: saying Proved by re-checking the settled cases through `over`. The draft's
definition was wrong four ways (it proved from an empty list, refuted a case a lemma had
settled, refuted an Open work list, and never used the composed lens). More to the point, a
Proved already rests only on the checks and on `alternatives` (section 2), so there was little
to gain.

So the first wish, read as "restructure the loop around lens values", buys almost nothing.
Read as "the laws the loop relies on are proved", it is change 4.

### Change 4. Laws stated as types and checked

What: a checked module (annotations verified on load, like
lib/theories/checked_example.disp) that states a rule's law as a type and gives its proof.

1. The composition law, now: it is Proved today.
2. Cover laws over an untyped witness (`cover_t`), after change 1.
3. The lemma rule.

Why: this is what "proved" can mean here. A law written as a type is checked by the same rules
as everything else, where search could only fail to refute it.

Limits:
- Case split on a tree, the generic run and induction cannot be proved this way. They are what
  the proof would use.
- A law that talks about the checker itself (a recognizer's `cover_law`, which calls
  `judge_as`) needs the checker run on unknown values. Not tested; expected out of reach.
- A law must quantify its witness over every tree, or it says less than the claim it stands
  for. The equation-to-`true` form loses nothing for a claim that always answers.

### Change 5. Modes on a type's record

Deferred. The idea was to name what the optional fields say (finite, has cases, check only,
refute only) and have `fn_c` report a mismatch when a function type is built. Two problems: a
family's result type, and so its mode, is not known until its argument is in; and no lens is
built from a mode, so it is a diagnostic and not part of the checker. It also cannot say that
a program will be Proved: which splits are needed depends on where the program looks.

## 6. The one-loop change

Proposed earlier: the function type's own check becomes the work-list stream, with induction
and lemmas inside it; `judge_as` is renamed to something like `member`; a No carries its
counterexample. Change 1 no longer needs it. It stands on its own case: three loops that
split, and `judge` answering Open where `judge_as` answers Yes (first two rows of the table).
The open doubt is unchanged: a function checked inside another check sometimes needs a piece of
the outer one split, and hands that need up.

## 7. Order

1. Fix or fence `f_arrow` (section 4), with `peek` as the test. A wrong Proved comes first.
2. Change 1. Done.
3. Change 4, step 1: the composition law in a checked module. Then try `cover_t`.
4. Reword the header of lib/rules.disp and research/LOB_AND_CURRY.md §5.4: what is taken on
   trust, and that composition is now checked.
5. Change 2, option 2, as a probe first.
6. The one-loop change, if still wanted. Changes 3 and 5 wait.

## 8. The review

An independent reviewer read the first draft and the code, and ran about fifty probes. What
it changed:

- Change 2 as drafted was unsound, and the same mistake was already in `f_arrow` (section 4).
- The cover law in the draft had its two variables the wrong way round, and its typed witness
  made it empty. Section 3 is restated.
- The composition law, reported Open, is Proved once stated correctly.
- Change 3's re-check was wrong and gained little. Dropped.
- Change 1 moved from `run_at` to the split, which also fixes the cases the draft listed as
  holes.
- Change 5 deferred.
- Facts corrected: a call under a triage is stuck as an opaque value, not a need of the
  function; `admits` is required; no type sets `generic`; there is a third loop.

Checked and found right: the file and line references; that `subst` leaves holes alone and
that this is the cause of the stale type; that `fn_member` does not have the problem; that only
case splits are lens values.

Confirmed again by the author after the review, by running: the wrong Proved for `peek`, the
order of `eq_t`'s environment, the composition law Proved and a wrong proof of it Open,
`bool_fold(v(x), false, true)` Proved, and `cover_t` Open with reason "No". Change 1 was then
implemented the reviewer's way and run against the full suite.
