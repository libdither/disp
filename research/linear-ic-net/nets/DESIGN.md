# Reflective linear nets

A λ-calculus run as an interaction net, where function bodies stay inert until called, so a single
copy agent is enough. Programs can read any function's code, including their own. Variables are
wires, so there is no bracket abstraction and no routing combinators.

Prototype: `net.py` (the nets), `ref.py` (the same calculus as term rewriting, written separately),
`check.py` (differential tests), `selfinterp.py` (the calculus interpreting itself, on this table and on
two earlier ones), `linear.py` (the adds held to arithmetic: the linear ones erase nothing, the reversible
ones undo each other), `scaling.py` and `scaling.html` (how the adds scale, beside tree calculus),
`adders.py` and `adders.html` (every add compared), `blowup.py` and `costs.py` (measurements),
`audit.py` (which pairs meet),
`merge_probe.py` (a flawed variant run for real). `blowup.py` imports the 9-rule compiler from
`../tree-calculus/`. Theorems about this design are in `../lean/` (`PATHWAY.md` is the catalog).

One thing the Lean version found: copiers and erasers come in two orientations. Copying a function
puts a copier on its binder's x port, which hands values out, so that copier faces the opposite way
from one copying a value. `net.py` gets away with one `dup` and one `era` because its agents ignore
direction; the polarized table (`../lean/Tables.lean`) has `dupI`/`dupO` and `eraI`/`eraO`: 13 kinds of
agent, 33 rules, and 9 pairs left without a rule on purpose. One of the 9 is an out-facing eraser
meeting an in-facing copier. No run makes that pair (a body is only copied through its binder, so a
copier coming up from a use always meets its own other half first), and its rule was dropped.

Every program is closed: a name nothing binds is an error. There are no symbols. Where a program needs
a value to tell apart from others, or one that nothing may apply, it builds one: `ρ` is the only atom,
and `(^d. d) ρ` is a piece of code that is not a function.

## Why trees blow up

A tree has no pointers, so every place an argument goes has to be spelled out by routing nodes on
the path from the root to that use: B for "goes right", C for "goes left", W for "both". Each binder
pays about one node per application node above its uses. For λx1…xn. k xn … x1:

| n | term | net | 9-rule tree | tree calculus tree | 9-rule steps to apply |
|---|---|---|---|---|---|
| 4 | 14 | 14 | 72 | 165 | 19 |
| 8 | 26 | 26 | 282 | 965 | 85 |
| 16 | 50 | 50 | 1134 | 6533 | 361 |
| 32 | 98 | 98 | – | – | 1489 |

Random closed terms of 320 nodes average 455 agents as nets, 2422 nodes as 9-rule trees and 2591 as
tree-calculus trees. Kiselyov notes the same worst case (at least quadratic for λx1…xn. xn…x1) and
gets linear size only with an unbounded family of "bulk" combinators Bn, Cn, Sn.

## Agents

Principal port first. Code faces up: its principal port points at whoever uses it.

| agent | ports | what it is |
|---|---|---|
| `lam` | p; x, b | a function: x hands the argument to the uses, b holds the body |
| `Lam` | p; x, b | an opened function (from reading a function, or written out as `^x. body`): applying it returns the body as code, unrun |
| `app` | p; f, a | an application in code, frozen |
| `var` | p; s | a use of a variable; s leads back to the binder, or to a captured value (a splice) |
| `rho` | p | the reflection primitive |
| `O` | p; a, r | a running application: p faces the function |
| `T` | p; o | thaw: turns code into a running computation |
| `R` | p; o | reflection under way |
| `sig` | p; a, b | the split of a variable used twice, in code; it copies when a value arrives |
| `dup` | p; a, b | a copy in flight |
| `era` | p | the eraser |

The compiler gives each λ, application and variable use one agent, plus a `sig` per extra use and
an `era` per unused binder. That's at most twice the term's size, and about 1.4× on random terms.

## Rules (one per pair)

| pair | result |
|---|---|
| `O ⋈ lam` | argument to x; body → `T` → result (run) |
| `O ⋈ Lam` | argument to x; body → result (open: the body comes back as code) |
| `O ⋈ rho` | argument → `R` → result |
| `T ⋈ app` | `O` with a `T` on the function part and one on the argument part |
| `T ⋈ var` | the use resolves to whatever s leads to, which is not thawed |
| `T ⋈ lam / Lam / rho` | the value itself (a function's body stays inert) |
| `R ⋈ lam` | λk1…k5. k1 k2 k3 k4 k5 F, where F is the function opened (a `Lam`) |
| `R ⋈ Lam` | λk1…k5. k2 k1 k3 k4 k5 L, where L is the function closed again (a `lam`) |
| `R ⋈ app` | λk1…k5. k3 k1 k2 k4 k5 f a (f, a: the two parts, as code) |
| `R ⋈ var` | λk1…k5. k4 k1 k2 k3 k5 w (w: the captured value) |
| `R ⋈ rho` | λk1…k5. k5 k1 k2 k3 k4 |
| `sig ⋈ X`, `dup ⋈ X` | two copies of X; each auxiliary port of X gets a `dup` (the only copying) |
| `dup ⋈ dup` | annihilate: a–a, b–b |
| `era ⋈ X` | X goes; its auxiliary ports get erasers (the only dropping) |

Reflection works like the linear triage: the chosen arm also receives the arms that weren't chosen,
so nothing is dropped behind the program's back. Every kind of value has its own arm, so a program can
always tell what it holds. Reading a function opens it and reading the opened function closes it again,
so reading twice gives the function back: a program can look at a function and still run it.

An opened function can also be written out, `^x. body`. Applying it hands back the body as code, so
`(^d. p) u` is the code of `p`: a quote.

## Why one copy agent is enough here

With a single copier, `dup ⋈ dup` has to mean "the two ends of the same copy met", and it goes wrong
when two different copies meet. That's the classic trouble with sharing graphs. It happens when
something inside a function body gets reduced and the function is then copied. Here nothing inside
a body ever reduces before the call:

- code agents only meet `T`, `R`, `O`, copies and erasers through their principal port, which faces
  up, and inside a body there are only code agents and `sig`/`era` waiting on a binder's x port;
- values captured by an inner function attach to a `var`'s s port, which is auxiliary;
- a copy only starts when a value reaches a `sig`. It walks down what it copies, and the only things
  it can meet head-on are agents of that value, or its own other half across a variable wire.

That's an argument sketch, not a proof. The tests check it directly: every `dup` carries the id of
the copy it belongs to (bookkeeping only, the rules never read it), and every annihilation
is checked.

The proof doesn't have to follow overlapping copies at all. Runs are confluent and all the same length
(proven in `../lean/`), so if one order of steps ends with the right answer, every order does
(`every_order_of_one`). In call-by-value order each copy finishes before anything else moves, so it is
enough to prove that order right. That proof isn't written yet.

## It interprets itself

`interp` in `check.py` is an interpreter for the calculus, written in the calculus (11 lines and four
one-line helpers). It reads
code with `ρ`. A function, an opened function, a spliced value and `ρ` are values already. To apply a
function it opens it on the argument and interprets the body; an opened function hands back its body
as code; `ρ` reads. It never hands a call to the host.

- Every hand-written program gives the same answer interpreted as run directly, and so does the
  interpreter interpreting itself interpreting the program. `selfinterp.py` also compares 3,000 random
  programs: 2,970 agree (2,933 values, 37 stuck both ways), 30 were skipped for fuel, and 293 of them
  were also run as nets.
- The table as first designed couldn't do this. Reading an opened function gave the same view as
  reading a function, but the two behave differently when applied: one runs its body, the other hands
  it back as code. An interpreter has to do the same, and couldn't tell which it held. `selfinterp.py`
  runs that table too: its interpreter gets 6 of 14 programs wrong, the ones that apply an opened
  function. Giving opened functions their own arm fixes it (`views_tell_kinds_apart` in `../lean/`).
- Handing the opened function back closed, instead of as it is, is a choice: both pass every test. Closed
  is strictly more useful, since reading twice then changes nothing, and a program can look at a
  function and still run it. `net.VARIANT` switches between the three tables.
- An interpreter that only walks applications and hands every call to the host (`lazy` in
  `selfinterp.py`) worked on the first table too. It never interprets a function body.

## Linear programs

A program is linear here when every variable is used, so its net has no eraser and no run fires an erase
rule, and when the only copies are the ones the program asks for by using a variable twice. `linear.py`
checks both on every run. The programs are in `check.py` (`lzero` to `reversible_tree_sub_parallel`).

Each add is named for the kind of program it is, the shape of its numbers, and the way it adds:

| program | kind | numbers | written | hands back |
|---|---|---|---|---|
| `linear_unary_add` | linear | unary, a loop | `5u` | the sum, and its step |
| `ordinary_unary_add` | ordinary | unary, zero or a successor | `5s` | the sum |
| `linear_list_add` | linear | a list of bits, a loop | `5` | the sum, the carry, the end of b and its step |
| `linear_list_add_two_shapes` | linear | cells that pick an arm, and a chain of pairs | `5a`, `5p` | a, the sum as a chain, the carry, its two steps |
| `ordinary_list_add` | ordinary | a list of `true` and `false` | `5c` | the sum |
| `linear_tree_add` | linear | a tree of bits and its depth | `5t` | the sum and the carry |
| `ordinary_tree_add` | ordinary | the same trees | `5t` | the sum and the carry |
| `linear_tree_add_parallel` | linear | the same trees | `5t` | the sum and the carry |
| `ordinary_tree_add_parallel` | ordinary | the same trees | `5t` | the sum and the carry |
| `reversible_tree_add`, `reversible_tree_add_parallel` | reversible | the same trees | `5t` | a and the sum |
| `reversible_tree_sub`, `reversible_tree_sub_parallel` | reversible | the same trees | `5t` | a and s − a |

An ordinary program is written the usual way: a fixpoint or a test, and what is not picked is dropped.
A reversible one is linear, and its answer holds enough to get both numbers back (see "Reversible adds").

- **A number is its own loop.** `lzero = λs z. z s` and `lsuc n = λs z. s (n s z)`. Given a step and a
  base, a number runs the step once per successor: each `lsuc` uses `s` twice, so it copies the step
  once, and `lzero` hands the last copy to the base where a Church zero would drop it.
  `linear_unary_add m n = m (λk n. k (lsuc n)) (λs n k. k n s) n` gives the sum and the step handed
  back. There is no fixpoint, and nothing passes the loop around by hand: the data does. `3u` is
  written for `lsuc (lsuc (lsuc lzero))`.
- **A bit is a swap.** `b0 = λp q k. k p q` hands two things on in order, `b1` hands them on swapped.
  Either way both are used.
- **A list of bits is a loop over its bits**, lowest first: `wend = λf n. n f` and
  `cell x w = λf n. f x (w f n)`. It is a unary number with a bit on each successor. `5` is written for
  `cell b1 (cell b0 (cell b1 (cell b0 wend)))`: every list of bits in a program gets one bit more than
  the largest needs, so a sum of two fits. Run with `pair`, a number's loop gives a chain of pairs,
  which can be taken apart one bit at a time.
- **The list add sorts three bits, then swaps once.** `linear_list_add a b` runs a's loop with the step
  `addbit`, over `b` as a chain. For one bit, three compare-and-swaps put a's bit, b's bit and the
  carry in order (`x x y` lets `x` swap itself and `y`: it is used up, and its copy takes its place).
  Then the middle one swaps the outer two, and they come out as the sum bit and the next carry. Nothing
  is left over. It gives the sum, the carry out, and what is handed back: the end of b's chain and the
  step. The sum has the shape of the numbers that went in, so it can go into another add.
- **With a's bits as cells, add is cheaper, but has two shapes** (`linear_list_add_two_shapes`). If each
  cell of `a` picks an arm by its bit (`5a`) and `b` is a chain to start with (`5p`), one
  compare-and-swap and one more swap do, and no bit of `a` is copied. It builds `a` again as it goes and
  hands it back. The sum comes out as a chain, which cannot drive a loop, so it cannot be the `a` of
  another add.
- **A tree number is a tree of bits, and its depth.** `1014t` is `pair bits loop`. The bits are pairs
  of pairs with a bit at each leaf, the low half first: plain data, in which no node says what it is.
  The loop is the depth, `λs z. s (… (s z))` with one `s` for each level. Every tree number in a
  program gets the fewest levels whose leaves hold one bit more than the largest needs.
- **Tree add doubles an adder.** `add3 x y c ret` adds three bits: the sort and the swap of `addbit`.
  `double f` is an adder for trees twice as large as the ones `f` takes. It splits both trees, runs
  `f` on the low halves, then on the high halves with the carry, and pairs the two sums. It uses `f`
  twice, and that is its only copy. `linear_tree_add a b` runs a's loop with `double` on `add3`, which
  builds the adder for a's depth, and gives the sum (a tree, with b's depth beside it) and the carry
  out. Nothing is handed back. `ordinary_tree_add` adds the same trees the usual way: it tests each bit
  and drops the arm not taken, copies a bit it uses twice, uses a's depth twice and drops b's.
- **A parallel add looks the carries ahead, and forgets without erasing.** `linear_tree_add` runs one
  bit after another, since each waits for the carry before it: its rounds double when the tree does.
  `linear_tree_add_parallel` adds the same trees another way. Every part of a tree first works out its
  carry out for a carry in of 0 and for one of 1 (`g` and `h`): a leaf from its two bits, sorted; a
  pair from its halves, where the low half's answer picks one of the high half's two. None of that
  waits for a carry, so all the parts work at once. Then the carry goes down: a pair hands it to its
  low half and picks the low half's answer for its high half, and a leaf flips its sum bit by it.
  Picking one of two bits leaves the other, and a linear program cannot erase it. `forget v x` uses
  the bit up instead: `v` swaps two identity functions, both are then applied, and which way it
  swapped is lost. The parallel add copies eight bits for each leaf where the other copies three, and
  forgets four. `ordinary_tree_add_parallel` is the same add the usual way.

`linear.py`: 173 unary sums (every m, n up to 12, and four larger) and 159 sums on lists of bits (every
pair up to 3 bits, random ones up to 64 bits) are right, and match the reference where it was run. No
net has an eraser. `linear_unary_add m n` starts exactly m copies, a w-bit `linear_list_add` exactly 5w
(the step, the pair, three bits). The number of rewrites depends on the size of the input and not on
its bits. Two adds in a row work, the second on the first one's sum (75 cases, to 2a + b). 91 tree sums
on 2 to 64 leaves are right too, with two adds in a row on each: `linear_tree_add` on 2^d leaves starts
exactly 3·2^d + 2d − 1 copies (three bits a leaf, then the step of the loop and the adder at each
level), and `ordinary_tree_add` gives the same answers term for term. `linear_tree_add_parallel` gives
them too, on the same 91 pairs, and so does `ordinary_tree_add_parallel`. It starts exactly
8·2^d + 2d − 6 copies, and neither its rewrites nor its rounds depend on the bits.

Rewrites for one more successor, or one more bit:

| | rewrites | copy | cancel | erase | call | thaw |
|---|---|---|---|---|---|---|
| linear unary add | 43 | 17 | 6 | 0 | 5 | 15 |
| ordinary unary add (zero or a successor, a fixpoint) | 73 | 28 | 9 | 4 | 8 | 24 |
| linear list add | 287 | 103 | 36 | 0 | 37 | 111 |
| linear list add, two shapes | 206 | 79 | 27 | 0 | 25 | 75 |
| ordinary list add (lists of booleans, a fixpoint) | 390 | 138 | 43 | 61 | 37 | 111 |
| linear tree add | 300 | 110 | 37 | 0 | 38 | 114 |
| ordinary tree add (the same trees, bits tested) | 851 | 314 | 104 | 84 | 87 | 261 |
| linear parallel tree add | 770 | 291 | 99 | 0 | 95 | 285 |
| ordinary parallel tree add | 1,024 | 381 | 127 | 108 | 102 | 306 |
| reversible tree add | 316 | 122 | 41 | 0 | 38 | 114 |
| reversible tree subtract | 353 | 135 | 45 | 0 | 43 | 129 |
| reversible parallel tree add | 787 | 303 | 103 | 0 | 95 | 285 |
| reversible parallel tree subtract | 824 | 316 | 107 | 0 | 100 | 300 |

About half of a linear add is still copying (139 of 287, with the cancels). Nearly all of that is a copy
of frozen code, so sharing frozen code would take it away.

What it took:
- A choice between two pieces of code written in place always leaves the one not chosen, and dropping
  it is an erase. So a linear program never chooses that way. Either the data picks an arm it was
  handed and copies only that one, or a bit swaps two values that are both used.
- One shape for both numbers costs: 287 rewrites a bit against 206. The step has to take a's bit as
  data, which needs two more compare-and-swaps, and b's loop has to be run into a chain first.
- A tree cannot be asked whether it is a leaf: the arm for the other answer would be left over. And a
  tree written as its own loop, as the lists are, hands the step for a node down to every leaf, where
  it is left over too. So a tree number carries its depth, and the adder is built to that depth before
  it meets the tree. The whole adder is copied out first, 16 copies of `add3` for 16 leaves, which is
  most of what a run in the player shows.

### Reversible adds

Linear is not reversible. Linear is about the program: nothing is erased. Reversible is about what it
computes: different inputs give different answers, so the inputs can be had back.
`linear_tree_add` is not one-to-one (0 + 1 and 1 + 0 give the same sum), yet nothing is erased: on
trees of four bits its 256 pairs of numbers give 31 answers. A linear program forgets where a bit is
used up as a swap and no copy of it is kept. The sort does that: the middle bit swaps the outer two
and is gone. `forget` does it on purpose: a bit swaps two things that are the same.

- **The add hands a back.** `reversible_tree_add a b` gives `a` and the sum, in as many bits as the
  trees have leaves. It is `linear_tree_add` with `a` used twice, so copied once, and the carry out
  forgotten: `a` and the sum say what the carry was.
- **The subtract undoes it.** `reversible_tree_sub a s` gives `a` and `s − a`, so
  `reversible_tree_add a b reversible_tree_sub` is the pair of `a` and `b` again, and the other way
  round. `s − a` is `s` plus `a` with every bit flipped plus 1, so the subtract is the same adder with
  a carry in of 1 and the leaf `sub3`, which adds a's flipped bit.
- **A swap reads a bit and hands it back.** `x b1 b0` hands on x's flip and x itself, new ones each,
  with no copier and nothing forgotten. So `sub3` copies one bit less than `add3`: where `add3` sorts
  with `x x y`, which copies `x`, `sub3` sorts the flipped bit with `x2 y nx`.
- **In parallel too.** `reversible_tree_add_parallel` and `reversible_tree_sub_parallel` are the
  parallel add with `a` handed back, and with the leaf that flips a's bit.

`linear.py`: on trees of 2 to 64 leaves, 332 pairs of numbers for each of the two ways, every pair on
four leaves among them. Each add and subtract is right, each undoes the other, the 256 pairs on four
leaves give 256 answers, no net has an eraser, and the rewrites do not depend on the bits. The
reversible add costs 16 rewrites a bit more than the linear one (316 against 300), all of it the copy
of `a`. The subtract costs 353: its leaf is longer, and each leaf copies it out.

Only the answers can be had back. A run cannot: a net does not say what it was a step ago. Of the 33
rules, 5 make no agent, and two of those leave nothing at all. 14 leave a result that another rule
leaves too: a call on an opened function and a cancel both just join two pairs of wires, and a split
and a copy in flight copy a value to the same thing. In `linear_tree_add 1014t 2413t`, 1,210 of the
4,636 rewrites only join wires.

What a run that can be undone would take, going by other machines. None of it is built.
- **Keep a history.** A machine whose step [lacks a single-valued inverse](https://www.dna.caltech.edu/courses/cs191/paperscs191/bennett1973.pdf#page=1)
  can be made reversible at every step by saving what each step would lose. The history goes away
  again by [retracing the steps in backward order](https://www.dna.caltech.edu/courses/cs191/paperscs191/bennett1973.pdf#page=1)
  once the answer is copied out, which leaves the input beside the answer. For processes that run
  side by side, the history is [kept beside each process](https://www.pure.ed.ac.uk/ws/files/16870363/General_Reversibility.pdf#page=2).
  Here it would be a mark for each rewrite: 4,636 marks for the add above, whose answer is 252 agents.
- **Send a token through a net that stays as it is.** A machine that keeps closures gathers
  information that [has to be garbage-collected](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4).
  The token machine of the geometry of interaction does not, and [is reversible](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4):
  [deterministic backward as well as forward](https://arxiv.org/pdf/2010.12988#page=10). It
  [can be exponentially slower](https://arxiv.org/pdf/2010.12988#page=2), since no work is shared.
  Functional programs can be compiled to [automata that are reversible](https://arxiv.org/pdf/1111.7154#page=1)
  in this way.
- **Write programs as gates that are one-to-one.** A bit here is the control of a controlled swap,
  which [is its own inverse](https://www.cs.princeton.edu/courses/archive/fall05/frs119/papers/fredkin_toffoli82.pdf#page=6)
  as long as the control is kept. `add3` uses its middle bit up as a control, and that is where it
  forgets. A circuit of such swaps that keeps every control, and forgets a bit only where a copy of
  it is kept, is one-to-one by the way it is built. The adders of reversible circuits are of this kind: they
  give a and the sum [in place](https://arxiv.org/pdf/quant-ph/0410184#page=1), and there are
  [parallel ones](https://arxiv.org/pdf/quant-ph/0406142#page=1). Nothing here checks that a program
  is built this way, and the reversible adds are not: they forget a bit at each leaf that the answer
  still determines, and are held to being one-to-one by running them.

### How they scale, beside tree calculus

`scaling.py` runs m + m in unary up to m = 1,024 and a + b in binary up to 256 bits, in the nets and,
with the programs of `../tree-calculus` and `../reversible`, in tree calculus and in the 9-rule linear
tree calculus. `scaling.html` draws it. Every add is linear in the size of its numbers. For each more
successor, or bit:

| | nets, linear | nets, ordinary | tree calculus | linear tree calculus | the same, reversible |
|---|---|---|---|---|---|
| unary | 43 rewrites | 73 rewrites | 58 steps | 21 steps | 48 steps |
| binary | 287 rewrites | 390 rewrites | 182 steps | 66 steps | 44 steps |

Counted this way the tree calculi take fewer steps for a binary add than the nets take rewrites. But a
tree calculus step can copy a whole argument. With trees shared that is free. With trees copied for
real, tree calculus copies 11,700 nodes for each successor at m = 1,024 and 28,500 for each bit at 256
bits, and both keep growing with the numbers, so the add is no longer linear. A net's copies are
among its rewrites.

### All the adds, side by side

`adders.py` runs every add on lists and trees of bits on numbers of 2 to 1,024 bits, six pairs at each
size (0 + 0, all ones + 1, all ones + all ones, three random pairs), and `adders.html` draws it. A
round fires every pair that waits when it starts: with no limit on how many that is, the rounds are
the least time a run can take. On 64-bit numbers:

| | rewrites | rounds | rewrites a round | most agents at once |
|---|---|---|---|---|
| `linear_list_add` | 18,429 | 5,224 | 3.5 | 5,990 |
| `linear_list_add_two_shapes` | 13,245 | 3,499 | 3.8 | 4,899 |
| `ordinary_list_add` | 25,794 | 2,825 | 9.1 | 1,657 |
| `linear_tree_add` | 19,004 | 6,918 | 2.7 | 7,007 |
| `ordinary_tree_add` | 54,072 | 4,224 | 12.8 | 18,569 |
| `linear_tree_add_parallel` | 48,629 | 606 | 80.2 | 15,848 |
| `ordinary_tree_add_parallel` | 64,901 | 469 | 138.3 | 20,911 |
| `reversible_tree_add` | 20,091 | 6,935 | 2.9 | 7,815 |
| `reversible_tree_sub` | 22,431 | 7,895 | 2.8 | 9,160 |
| `reversible_tree_add_parallel` | 49,716 | 606 | 82.0 | 16,656 |
| `reversible_tree_sub_parallel` | 52,056 | 620 | 84.0 | 17,949 |

- **Time.** An add that goes one bit after another takes rounds in step with the bits: 43 to 108
  rounds a bit, so 44,105 to 110,598 rounds at 1,024 bits. The parallel adds take a fixed number more
  each time the numbers double, 85 for the linear one and 63 for the ordinary one: 946 and 718 rounds
  at 1,024 bits. `linear_tree_add_parallel` takes fewer rounds than `linear_tree_add` at every size,
  and fewer than every add that goes bit by bit from 8 bits.
- **Work.** Every add is linear in the bits. The parallel ones cost more: 770 rewrites a bit for
  `linear_tree_add_parallel` against 299 for `linear_tree_add` and 206 for the cheapest. About half of
  every add is copying (390 of the 770, with the cancels).
- **With a limit on how many pairs fire in a round**, at 256 bits: with 4 a round the parallel add is
  slower than `linear_tree_add` (49,142 rounds against 35,075), with 8 it is faster (24,588 against
  31,164), with 32 it is faster than every other add, and with 1,024 it takes 790 rounds against
  27,654. The adds that go bit by bit gain little past 16 a round: they do 3 to 13 rewrites a round,
  whatever the size.
- **Room.** The ordinary list add needs the least: its fixpoint unfolds one bit at a time, 20 agents
  for each bit of the numbers. A linear add copies out its step for every bit early in the run, and
  holds 74 to 111 agents a bit. The parallel adds and the ordinary tree add hold 250 to 330.
- **The bits change nothing.** Each add takes the same rewrites on 0 + 0, on the longest carry and on
  random numbers, and the same rounds (to within one, for `ordinary_tree_add_parallel`). A linear add
  has no choice. An ordinary one, called by value, works out both arms before it picks one.
- **Reversible.** Handing `a` back costs 16 rewrites a bit, the copy of `a`, and 17 rounds in all;
  in parallel it costs no round. The subtract costs 37 rewrites a bit more than the add, and one bit
  after another 15 rounds a bit more; in parallel it takes 14 rounds more in all.
- **Unary.** `linear_unary_add` takes 43 rewrites and 7 rounds for each successor. It beats the linear
  list add while the numbers have 5 bits or fewer, and from 6 bits on it loses.

## Tests

- 12,000 random closed programs of 3–40 nodes, in three families of 4,000: plain λ-terms; terms with
  `ρ` and written-out opened functions; and "readers", which apply `ρ` with an arm for every kind and
  use the parts. Each ran under the reference interpreter and as a net in three orders (queue order
  plus two shuffles). Every one of the 3,903 + 3,872 + 3,905 that ended in a value read back to the
  same term (up to renaming bound names). All 127 + 94 that got stuck in the reference got stuck in the
  net. 99 ran out of fuel and were skipped. Far fewer get stuck than when programs had symbols: the
  only error left is applying a piece of code.
- No annihilation ever joined two different copies. No `O`, `T` or `R` was ever copied or erased. Two
  `sig`s never met. No agent was left over once a run finished.
- 47 hand-written programs, all matching the reference: reading the kind of a value, sharing code, opened
  functions and spliced values, reading an opened function, closing one and running it, a quote, and
  `size`, a recursive program (through a call-by-value fixpoint) that counts the binders in any
  function's code. It runs on `suc`, on `add 2 2` and on itself: `size size`, 1,221 β-steps. Eight of
  the 47 run the interpreter, two of them on itself. Twenty are adds: three on unary numbers, five on
  lists of bits, seven on trees, and five that add and subtract reversibly.

## Costs

- Applying λx1…xn. k xn…x1 to n arguments: 4n + 1 interactions, against about 1.45·n² steps
  for the 9-rule calculus (129 against 1489 at n = 32).
- Copying and erasing are paid per agent. In `size size`, 97% of the 165,713 interactions are
  copies (69,388) and erasures (91,009): each recursive call copies the whole recursive function,
  and each read erases the four arms it didn't choose. Trees get this for free from
  hash-consing; a net copies for real. The fifth arm made this worse: with four arms the same program
  took 83,020 interactions. A program written linearly pays neither: see "Linear programs".
- Interpreting is dear for the same reason. A program of one β-step takes 11,502 interactions under the
  interpreter, and 942,777 under the interpreter interpreting itself: each level costs about 80×,
  nearly all of it copying and erasing the interpreter's own code.

## Limits and open questions

- Evaluation is weak (call by value, never under a binder). That's what makes one copier enough,
  and it's also what makes code readable as code.
- Inert code never changes, so an implementation could hash-cons it and make copying and erasing
  code O(1). That would give back the sharing trees have. It isn't built.
- Opening a function needs a marker value from the reader, and captured values show up as
  splices.
- Programs build new code only by opening templates and splicing values into the holes. Spliced
  code isn't run when the result runs; a program that wants that runs it through the interpreter.
- Data is λ-encoded. There is no tree constructor.
- Linear programs are written by hand. There is no compiler from ordinary programs to linear ones,
  and nothing checks that a program is linear before it runs.
