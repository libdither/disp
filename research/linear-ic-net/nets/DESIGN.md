# Reflective linear nets

A λ-calculus run as an interaction net, where function bodies stay inert until called, so a single
copy agent is enough. Programs can read any function's code, including their own. Variables are
wires, so there is no bracket abstraction and no routing combinators.

Prototype: `net.py` (the nets), `ref.py` (the same calculus as term rewriting, written separately),
`check.py` (differential tests), `blowup.py` and `costs.py` (measurements), `audit.py` (which pairs meet),
`merge_probe.py` (a flawed variant run for real). `blowup.py` imports the 9-rule compiler from
`../tree-calculus/`. Theorems about this design are in `../lean/` (`PATHWAY.md` is the catalog).

One thing the Lean version found: copiers and erasers come in two orientations. Copying a function
puts a copier on its binder's x port, which hands values out, so that copier faces the opposite way
from one copying a value. `net.py` gets away with one `dup` and one `era` because its agents ignore
direction; the polarized table (`../lean/Tables.lean`) has `dupI`/`dupO` and `eraI`/`eraO`.

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
| `Lam` | p; x, b | an opened function (from reflection): applying it returns the body as code, unrun |
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
| `R ⋈ lam / Lam` | λk1 k2 k3 k4. k1 k2 k3 k4 F, where F is the function as a `Lam` |
| `R ⋈ app` | λk1 k2 k3 k4. k2 k1 k3 k4 f a (f, a: the two parts, as code) |
| `R ⋈ var` | λk1 k2 k3 k4. k3 k1 k2 k4 w (w: the captured value) |
| `R ⋈ rho` | λk1 k2 k3 k4. k4 k1 k2 k3 |
| `sig ⋈ X`, `dup ⋈ X` | two copies of X; each auxiliary port of X gets a `dup` (the only copying) |
| `dup ⋈ dup` | annihilate: a–a, b–b |
| `era ⋈ X` | X goes; its auxiliary ports get erasers (the only dropping) |

Reflection works like the linear triage: the chosen arm also receives the arms that weren't chosen,
so nothing is dropped behind the program's back.

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

## Tests

- 8,000 random closed programs of 3–40 nodes, half using `ρ` and free symbols. Each ran under the
  reference interpreter and as a net in three orders (queue order plus two shuffles). Every one of the
  3,903 + 1,983 that ended in a value read back to the same term (up to renaming bound names). All
  2,017 that got stuck in the reference got stuck in the net. 97 ran out of fuel and were skipped.
- No annihilation ever joined two different copies. No `O`, `T` or `R` was ever copied or erased. Two
  `sig`s never met. No agent was left over once a run finished.
- 16 hand-written programs, all matching the reference: reading the kind of a value, sharing code, opened
  functions and spliced values, reading an opened function, and `size`, a recursive program (through
  a call-by-value fixpoint) that counts the binders in any function's code. It runs on `suc`, on
  `add 2 2` and on itself: `size size`, 891 β-steps.

## Costs

- Applying λx1…xn. k xn…x1 to n arguments: 4n + 1 interactions, against about 1.45·n² steps
  for the 9-rule calculus (129 against 1489 at n = 32).
- Copying and erasing are paid per agent. In `size size`, 95% of the 83,680 interactions are
  copies (36,496) and erasures (43,267): each recursive call copies the whole recursive function,
  and each reflection erases the three arms it didn't choose. Trees get this for free from
  hash-consing; a net copies for real.

## Limits and open questions

- Evaluation is weak (call by value, never under a binder). That's what makes one copier enough,
  and it's also what makes code readable as code.
- Inert code never changes, so an implementation could hash-cons it and make copying and erasing
  code O(1). That would give back the sharing trees have. It isn't built.
- A function can be run or read, not both: reading turns it into an opened `Lam`. Copy it first
  to do both.
- Opening a function needs a marker value from the reader, and captured values show up as
  splices.
- Programs build new code only by opening templates and splicing values into the holes. Spliced
  code isn't run when the result runs; a program that wants that writes an interpreter, as in tree
  calculus.
- Data is λ-encoded. There is no tree constructor.
- The reversible-programming discipline from the 9-rule work hasn't been tried here.
