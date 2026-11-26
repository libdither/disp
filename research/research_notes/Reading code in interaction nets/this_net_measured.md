# The net as built, and what a read costs (measured in this session)

Source: `research/linear-ic-net/nets/` in the repo (`net.py` is the reducer, `DESIGN.md` the design notes,
`check.py` holds the programs). Numbers below come from running `net.py` on programs from `check.py`.

## The mechanism

- A λ-calculus run as an interaction net. A function's body is kept as inert code agents (`lam`, `app`,
  `var`); nothing inside a body rewrites before the call. Evaluation is weak and call by value.
- `T` (thaw) is an agent that walks code and turns it into running applications (`O`). `T ⋈ app` makes an
  `O` with a `T` on each part; `T ⋈ lam` leaves the function as a value, body untouched.
- `ρ` is the one reflection primitive. `ρ c` puts a reader agent `R` on the value `c`. When `R` meets the
  value it builds a view: a five-argument function, one argument per kind of value.

| pair | the view that is built |
|---|---|
| `R ⋈ lam` (a function) | `λk1…k5. k1 k2 k3 k4 k5 F`, F = the same binder and body, relabelled as an opened function |
| `R ⋈ Lam` (an opened function) | `λk1…k5. k2 k1 k3 k4 k5 L`, L = the function closed again |
| `R ⋈ app` (an application, in code) | `λk1…k5. k3 k1 k2 k4 k5 f a` |
| `R ⋈ var` (a variable use holding a captured value) | `λk1…k5. k4 k1 k2 k3 k5 w` |
| `R ⋈ rho` | `λk1…k5. k5 k1 k2 k3 k4` |

- The chosen arm is handed the four arms not chosen, then the parts (so nothing is dropped behind the
  program's back; a linear program can use them).
- An opened function applied to a value hands back its body as code, with the value spliced in where the
  variable was used. Opening costs one rewrite (`O ⋈ Lam`: two wires joined). The body is not walked.
- Data is λ-encoded. There is no constructor agent and no case agent.

## What one read costs

`ρ (\x. x) (\o a v p F. F) ρ ρ ρ ρ`: read the identity function, keep the opened function, four arms that are
a single `ρ` each. 49 rewrites:

| part | rewrites |
|---|---|
| thawing the call `ρ c k1 … k5` (6 applications, `ρ`, 6 arguments) | 13 |
| `O ⋈ rho`, then `R ⋈ lam` (the view is built here) | 2 |
| running the view's five binders (5 calls, 15 thaws) | 20 |
| running the chosen arm's five binders (5 calls, 5 thaws) | 10 |
| erasing the four arms not chosen | 4 |

- `R ⋈ lam` makes 17 agents (5 `lam`, 5 `app`, 6 `var`, 1 `Lam`) whatever the size of the function read.
  The other views make 18 (application), 16 (variable), 14 (`ρ`).
- The view costs 21 rewrites (1 to build, 20 to run). A case agent that held the five arms would do the
  same in 1.
- Erasing the arms not chosen is paid for each agent in them: with four unused arms each holding 1, 4 or 16
  identity functions, the same read takes 97, 145 or 337 rewrites.

## Where the cost of `size` goes

`size` counts the binders in any function's code. It recurses through a call-by-value fixpoint and reads
with `ρ` at every node.

| program | rewrites | reads | copies | erases | thaws and calls |
|---|---|---|---|---|---|
| `size suc` | 20,803 | 10 | 8,647 (41.6%) | 11,498 (55.3%) | 635 |
| `size (add 2 2)` | 22,929 | 11 | 9,501 | 12,681 | 723 |
| `size size` | 165,713 | 80 (37 functions, 21 applications, 20 variables, 2 `ρ`) | 69,388 (41.9%) | 91,009 (54.9%) | 5,119 |

- Each read costs about 2,070 rewrites: about 865 copies, 1,140 erases, 64 thaws and calls.
- The view itself is 21 of those: 1.0%.
- The other 99% is the reading program's own code: each recursive call copies the whole recursive function
  (with everything it captured), and each read erases the four arms not chosen, agent by agent.
- With four arms instead of five the same `size size` took 83,020 rewrites (from `DESIGN.md`).

## Facts already in `DESIGN.md` that bear on the question

- "Variables are wires, so there is no bracket abstraction and no routing combinators."
- Random closed terms of 320 nodes average 455 agents as nets, 2,422 nodes as trees of a 9-rule tree
  calculus, 2,591 as tree-calculus trees. For λx1…xn. k xn…x1 at n = 16: 50 agents, 1,134 and 6,533 nodes.
- "Evaluation is weak (call by value, never under a binder). That's what makes one copier enough, and it's
  also what makes code readable as code."
- "Inert code never changes, so an implementation could hash-cons it and make copying and erasing code O(1).
  That would give back the sharing trees have. It isn't built."
- A self-interpreter written in the calculus costs about 80× for each level, nearly all of it copying and
  erasing the interpreter's own code.
- A first table gave an opened function the same view as a function; an interpreter could not tell them
  apart and got 6 of 14 programs wrong. The Lean theorem `views_tell_kinds_apart` covers the fix.

## What these measurements settle

- No whole-term quote happens. The code is already there; a read looks at one node and costs the same
  whatever the size of the function.
- The big λ seen in the player is the view: a five-way choice written as a five-argument function, built
  fresh at every read.
- That view is 1% of what a reading program pays. The rest is copying and erasing code that never changes.
