# From a rewrite function to a lattice, and a search for a linear base

Research note, 2026-10-15. Two parts:
1. **The design sequence as the repo does it** (sections 1–5): tree calculus's apply, a recursive function
   on two trees, becomes an interaction net (rust-ca-lattice `rules.rs`), which becomes the strand lattice
   (rust-ic-strands). Each step is a recipe with a price, and the agents the repo has are what the recipes
   produce. Facts come from the code; nothing here is new machinery.
2. **What a better base would look like** (sections 6–9): which properties of the base rewrite system set
   the price, what the literature has that pays less, and a sketch.

Earlier notes this builds on: research/interaction-combinator/tc-net.typ (the net, as a paper),
research/interaction-combinator/SPATIAL_IC.md and EMBEDDING_THEOREM.md (net to grid), research/REVERSIBLE_CORE.md
(reversibility; its literature map is not repeated here), research/LINEAR_TREE_NETS.md (reading code without
copying it). Delete once its conclusions live in code or in a `.typ` paper.

## 1. Stage 0: the rewrite function

Tree calculus has one constructor, △, and apply is a recursive function on two trees that returns a tree or
runs forever (research/reversible-core/residual.py `app`, rust-ca-lattice `oracle.rs`):

```text
apply(△, z)               = △z
apply(△x, z)              = △xz
apply(△△y, z)             = y                                   K
apply(△(△x)y, z)          = apply(apply(x, z), apply(y, z))     S
apply(△(△wx)y, △)         = w                                   triage on a leaf
apply(△(△wx)y, △u)        = apply(x, u)                         ... on a stem
apply(△(△wx)y, △uv)       = apply(apply(y, u), v)               ... on a fork
```

Four properties of this function decide everything downstream:
- **Pattern depth.** The function looks up to three levels into f (△(△wx)y: the root, its left child, that
  child's shape) and one level into z.
- **Linearity.** S uses z twice; K drops z; triage drops two of w, x, y. Those are the only places where a
  variable is not used exactly once.
- **Loss.** K and triage map many inputs to one output. The constructor cases, S and B/C-shaped S are
  one-to-one given the result and which case fired.
- **Calls.** S makes three calls, two of them independent; triage on a fork makes two, one after the other.
  Run as written, those calls live on a call stack.

## 2. Stage 1: the call stack made explicit

Turn the function into a machine whose every step looks at one thing. The standard route: write the
evaluator in continuation-passing style, then replace each continuation (a function) by a data constructor
holding what it captured (defunctionalization). Each place apply waits for a value to look at becomes a
**frame**, a record of what it captured and where its answer goes:

| frame | waits for | holds | where it comes from |
|---|---|---|---|
| `A` | the head of f | z | every call |
| `T1` | the head of a, where f = △a b | b, z | f was a fork |
| `Sel` | the head of z | w, x, b, where a = △wx | a was a fork |

Three things fall out:
- **A deep pattern becomes a chain of frames**, one constructor per step. △(△wx)y is matched by `A` (f is a
  fork), then `T1` (its left child is a fork), then `Sel` (the argument's shape). This is currying applied to
  pattern matching.
- **The stack is a tree.** S's two inner calls do not depend on each other, so their frames hang side by side
  under the outer call. A frame's "return address" is a pointer to its parent frame. The call stack of the
  recursive function becomes a tree of frames, which is where parallelism comes from.
- **Laziness picks which frames run.** Only frames whose answer someone reads should step; the rest wait.

## 3. Stage 2: the interaction net

An interaction net (Lafont) is a graph of agents. Each agent has one principal port and some others; two
agents interact only when their principal ports are wired together, and a rule replaces the pair by a small
net with the same loose ends. Every wire joins exactly two ports. Map the frames onto that:

- **Values are producers**: `L`, `S`, `F`, with the principal port as their output.
- **Frames are consumers**: `A`, `T1`, `Sel`, with the principal port facing the value they wait for and a
  result port pointing to their parent. A rule is "frame meets constructor": 3 frames × 3 constructors = 9
  rules carry tree calculus.
- **Linear wires force explicit copying and erasing.** A wire joins two ports, so a variable used twice
  needs a duplicator `Dn` and one dropped needs an eraser `Eps`. Each works one layer per rule (`Dn·L`,
  `Dn·S`, `Dn·F`, `Eps·L/S/F/Pair`), so copying and erasing are processes of linear agents, not primitives.
  At the net level everything is linear; tree calculus's non-linearity lives in `Dn` and `Eps`.
- **Reading consumes.** A consumer that meets a value destroys it. Anything read twice must be copied
  first, which is why S's z goes through `Dn`.
- **Frames with more captured values than ports carry them in pairs** (`Pair`, taken apart by `Unp`).
- **Laziness becomes demand**: a consumer fires only when wanted; a normalizer `Nrm` (`Nrm·L/S/F`) wants
  the answer whole and reads it out to the root `Out`.
- **Applications inside loaded values** load suspended (`P`), with a force rule per consumer.

Total (rules.rs `RULES`): 14 agent kinds, 26 rules. Each agent has one principal port, so active pairs never
overlap and every ready rule can fire at once: the net is strongly confluent, and as parallel as the
dependencies allow.

Two facts worth keeping from this stage:
- **A net can loop forever without copying.** If a rule's right side rebuilds an active pair, the net never
  stops; nothing has to be duplicated. Tree calculus loops through S (ω ω = S I I (S I I) copies itself
  every round), but a loop as such needs only a rule that re-creates its own redex, or a result wire fed
  back to an input (section 7).
- **The non-linear and lossy cases are where all the extra agents come from.** `Dn` and `Eps` exist
  because of S, K and triage; `Pair`/`Unp` exist because triage holds three arms.

## 4. Stage 3: the lattice

The strand lattice (rust-ic-strands README "The machine") puts the net in space with nothing addressed:
- **Agents sit in slots** of sites on a 2D or 3D grid (2 agents a site in the chip design).
- **Wires are paths of strands**: each link between neighbouring sites carries a few numbered strands, and
  each site's switchboard pairs up the wire ends present there (at most 8 pairings).
- **An interaction fires when the two principal ports are in one site or one strand apart**, inside a 2×2
  square (a 2×2×2 block on the chip). The new agents need free seats and free strands in that square; if
  there is no room, nothing happens and the pair waits.
- **Motion is thermal**: agents step, exchange, and wires fold and flip, accepted by an energy (wire
  tension, heavier on principal wires; crowding). Reactants find each other because their wire pulls them
  together.
- **Demand travels as pulses**: a wanted reader puts a pulse on its principal wire, which crosses a strand a
  clock and makes the computation it reaches wanted.
- **Garbage is collected by contact**: an eraser touching a computation's output kills it and erases its
  inputs instead.
- **Time is blocks**: each clock cuts the lattice into 2×2×2 blocks (Margolus), every site gets a turn, and
  every random choice is a hash of site and clock, so a chip reproduces the simulator bit for bit.

**The lattice pushes back on the net.** A rule must fit in a block: in practice at most 6 new agents and 3
ports (4 keeping history), since a wide agent costs switchboard pairings and a big rule needs room that a
crowded region does not have. Rules that need more are split into stage agents, each reading one more
thing. Keeping history shows this most: its forward side needs 12 walkers (`A+`, `T1+`, `Sel+`, `SL+`,
`SS+`, `SF+`, `T1s+` … `TSb+`) where the plain net has 3, and its biggest run-back rules (6 new agents)
stalled `rev [1, 2, 3]` for room until pressure was turned on.

## 5. The recipe, and what it costs

Read as a recipe, the sequence turns each property of the base function into agents:

| property of the base function | what it becomes | in the plain net | keeping history |
|---|---|---|---|
| each point where it looks at a value | a frame (consumer) | `A`, `T1`, `Sel` | +9 stage walkers |
| a pattern deeper than one level | a chain of frames | `T1`, `Sel` | `SL`/`SS`/`SF`, B/C peeks |
| a variable used twice | a duplicator, one layer per rule | `Dn` | merges `Dn-`, `DnL-`/`DnS-`/`DnF-` |
| a variable dropped | an eraser, one layer per rule | `Eps` | residuals `Rk`, `Rt` instead |
| captured values beyond the ports | pairs | `Pair`, `Unp` | `Arms3` |
| laziness and readout | demand, a normalizer, a root | `Nrm`, `Out`, wanted bits | + `Go`, `Back`, `E`, `E-`, `H` |
| information loss, made reversible | a residual and a mirror frame for each lossy step | none | `Rk` … `Rc`, `A-`, `Ar-`, `Sel-`, `SL-`/`SS-`/`SF-` |
| the lattice's limits | rules split into stages | none | most of the 38 kinds |

Plain: 14 kinds, 26 rules. Keeping history: 38 more kinds and 86 more rules (rules.rs `RESID_RULES` and
`FORGET_RULES`). The running-back half is mechanical: it is the same recipe applied to the inverse function
(residual.py `unapp`), so every forward frame gets a mirror.

So the size of the "pantheon" measures how far the base function is from three properties:
- **depth one**: every rule looks at one constructor of one value (no chains of frames);
- **linear**: every variable used exactly once on each side (no `Dn`, no `Eps`, except where a program asks);
- **one-to-one**: no two rules produce the same right side, so running back is reading the rules right to
  left (no residual, no mirror frames).

A base with all three would step down to an interaction net with nothing added, and to the lattice with
nothing split, as long as its rules are small. The rest of this note asks what such a base can be, and
whether it can keep what tree calculus has: one kind of data, programs that read programs, and loops.

## 6. What the three properties cost, and the trade they force

**Where the history comes from.** A rewriting system eats its program as it runs: `A` meets the function,
`T1` meets its left child, and those agents are gone. To run back, every eaten piece has to come back, so
every rule must leave enough to rebuild its left side. That is why keeping history costs a residual per
apply, even for the constructor cases, whose residual is just "a constructor fired here". Two ways out:
- **Pay per loss, not per step.** If a rule's right side determines its left side, nothing need be kept.
  Tree calculus's constructor, S, B and C cases are like that given which case fired; K and triage are not.
  The repo measured that 75–84% of the lattice's rewrites lose nothing (research/REVERSIBLE_PIPELINE.md),
  yet keeping history pays a record for every one of them, because a plain `S` node on the lattice does
  not say whether an apply made it.
- **Do not eat the program.** In a circuit, the program is fixed hardware and only data moves; running
  back is data moving the other way through the same gates, and nothing records anything (James and
  Sabry's Π, below, and the lattice's own resident machines in SPATIAL_IC.md Part II). The price is that the
  program is no longer data while it runs.

**Running back must be local too.** Reading a rule right to left means finding its right side, a small
subnet, intact. On the lattice those agents drift apart as soon as they are made, so the substrate's undo
waited for them to gather (10–12× the forward clocks). The repo's two working answers both make every
backward step two agents on one wire: footprints (research/REVERSIBLE_CORE.md: the used pair stays fused,
and products return to it along a back wire) and the residual walkers built here (`A-` meets a residual
and a result). The mirror walkers are the price of that locality, paid per kind of rule.

**The triangle.** Earlier notes found three things that pull against each other:
- **programs as data**: code is trees, copied and read like any value;
- **code not copied**: one copy of a function serves every call;
- **local and parallel**: no addresses, every step two agents on a wire.

Tree calculus on the lattice picks the first and third and copies code layer by layer (research/LINEAR_TREE_NETS.md:
6–33% of copies are ever needed at once). A code table picks the first two and is not a cellular automaton.
Circuits and resident machines pick the last two, and code is hardware. Riders (code read in place by
pointers) showed the cost of the middle corner in reverse: callers sharing code share its history, so
undo without ids becomes last-in-first-out across unrelated calls (research/REVERSIBLE_CORE.md §4). So
copies are not only a cost: they are what give each call a private history and its own place to run.

**Loops without copying.** A linear system can run forever: a Turing machine never copies a cell, and a
reversible one still loops. What a linear system cannot do is loop by self-application, since ω ω uses its
argument twice. The linear ways to loop are a rule whose right side rebuilds its own redex (a gate that
re-creates itself facing the next input), or a result wire fed back to an input (a trace, section 7). In
both, the loop reuses one piece of code without copying it, which is the "re-use itself" the question asks
about. Copying then becomes a program: walk a tree and build two.

## 7. What the literature has

Every quote below was found in the source with `scripts/verify-source.sh`; the links land on its page.

**The design sequence is a known recipe.**
- Stage 1 is Danvy's: an abstract machine is an evaluator transformed ["using closure conversion,
  transformation into continuation-passing style, and defunctionalization of continuations"](https://www.brics.dk/RS/03/13/BRICS-RS-03-13.pdf#page=3),
  where ["defunctionalizing a program amounts to replacing each of its function spaces by a data type"](https://www.brics.dk/RS/03/13/BRICS-RS-03-13.pdf#page=6).
  The frames of section 2 are that data type.
- Stage 2 is Lafont's: interaction nets ["generalise Girard's proof nets of linear logic"](https://web.archive.org/web/20250224075500id_/https://dl.acm.org/doi/pdf/10.1145/96709.96718#page=1),
  ["Agents interact through their principal port only"](https://web.archive.org/web/20250224075500id_/https://dl.acm.org/doi/pdf/10.1145/96709.96718#page=2),
  and ["explicit duplication and erasing symbols are required"](https://web.archive.org/web/20250224075500id_/https://dl.acm.org/doi/pdf/10.1145/96709.96718#page=1).
  "One frame meets one constructor" is exactly the shape a defunctionalized machine step has.
- A single walker does not make a parallel net: systems built from Turing machines
  ["are intrinsically sequential and cannot be universal in the above sense, even if they come from universal Turing machines"](https://dcreager.net/pdf/Lafont1997.pdf#page=2).
  This is the iso.rs result (one token, 9.4–51× the net's rounds) seen from the theory side.

**A linear system can loop, and copying can be derived.**
- Linear β alone cannot: ["The affine λ-calculus is strongly normalizing in a number of steps linear in the size of the initial term"](https://arxiv.org/pdf/1111.7154#page=22).
- Add one recursor and it is Turing complete, with ["erasing and duplication can only be done by the recursor"](https://arxiv.org/pdf/1001.3368#page=3)
  and ["only closed terms can be erased or duplicated in Lrec"](https://arxiv.org/pdf/1001.3368#page=3);
  the iterator alone already lets one ["erase any closed term, and moreover copy any closed term"](https://web.archive.org/web/20240423155651id_/https://nms.kcl.ac.uk/maribel.fernandez/papers/CSL06.pdf#page=9).
  The same discipline in nets: ["only erasing and duplicating substitutions when they are closed"](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=39).
  Tree-calculus code is always a closed tree, so it already fits.
- Tail loops need no copying at all: picture ["a token entering at the X wire, circulating k times around the feedback loop"](https://arxiv.org/pdf/1401.5113#page=5).
  A general fixpoint is that feedback plus copying: ["to have an abstract trace is to have a fixed point operator in the traditional sense, provided the monoidal product is cartesian"](https://publish.lfcs.inf.ed.ac.uk/reports/97/ECS-LFCS-97-360/ECS-LFCS-97-360.pdf#page=98)
  (cartesian meaning a copy map exists). So ω ω needs copying; a loop does not.
- The closest formal version of "rebuild your own duplicate" is the geometry of interaction's `!`, which is
  ["computationally universal, despite being entirely based around reversible primitives"](https://arxiv.org/pdf/1309.0361#page=1):
  a copy is a new address (a bijection ℕ ≅ ℕ⊎ℕ), not new matter. It runs as tokens walking a fixed graph.

**Reversible languages where every function is a bijection.**
- The syntax is simple: ["each variable occurring on one side must occur exactly once on the other side"](https://arxiv.org/pdf/1804.00952#page=4),
  and ["ensuring syntactical exhaustivity and non-overlapping of clauses is enough to ensure reversibility"](https://arxiv.org/pdf/2302.11887#page=1).
  The typed version corresponds to proofs in linear logic with fixed points, and there
  ["Every function that we can encode is reversible and total"](https://arxiv.org/pdf/2302.11887#page=3).
- Dropping exhaustiveness lets programs loop (["we drop this condition in order to allow non-terminating behaviour"](https://arxiv.org/pdf/2309.12151#page=2))
  and reaches reversible Turing completeness, and then copying and erasing come back as programs, partial
  ones: ["encode notions of duplication, erasure, and constant manipulation thanks to partiality"](https://arxiv.org/pdf/2309.12151#page=6).
  Copying run backward is an equality check, as R-CORE puts it: ["the value of E is duplicated, whereas in the latter case, the equality"](https://www.jstage.jst.go.jp/article/transinf/E100.D/5/E100.D_2016EDP7274/_pdf#page=3) is asserted.
- James and Sabry's Πᵒ is the point-free form: a dozen bijections plus a trace for loops. It
  ["is still a language of bijections but, because it can express infinite loops, the bijections may be partial"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=4),
  ["does not rely on any such history tracking"](https://web.archive.org/web/20170118093822id_/http://www.cs.indiana.edu/~sabry/papers/cat-rev.pdf#page=6),
  and inverts locally: ["The adjoint of trace c is trace c†"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=5).
  A translation from ordinary programs ["exposes the implicit erasure and duplication of information"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=2).
  Its high-level form, Theseus, requires that ["the number of iterations be recoverable from the output"](https://web.archive.org/web/20250602070806id_/https://legacy.cs.indiana.edu/~sabry/papers/theseus.pdf#page=9).
  The slogan: ["reversibility is a local phenomenon"](https://arxiv.org/pdf/1904.01679#page=10).
- **Code as data, reversibly.** There is ["a meta-circular interpreter for Π o written in Π o"](https://web.archive.org/web/20170118155739id_/http://www.cs.indiana.edu/~sabry/papers/iso-int.pdf#page=13),
  which walks the program with a zipper and re-enters a loop without copying it. R-WHILE's rule for any such
  interpreter: ["A universal RTM needs to preserve the machine it simulates in the output"](https://www.jstage.jst.go.jp/article/imt/11/0/11_160/_pdf#page=3).
  R-CORE does it on data with ["a single atom and a single constructor"](https://www.jstage.jst.go.jp/article/transinf/E100.D/5/E100.D_2016EDP7274/_pdf#page=1), binary trees.
  But in the pattern-matching languages code is a separate layer: an iso ["represents a closed computation, and can be duplicated or erased at will"](https://arxiv.org/pdf/2309.12151#page=4).

**When a rewrite rule needs no history.** Rewriting is not reversible by default: ["term rewriting is not reversible, even for injective functions"](https://arxiv.org/pdf/1710.02804#page=1).
A rule can skip its record when it drops no variable (["if a rule l → r is non-erasing, i.e., Var(l) = Var(r), then σ = id"](https://arxiv.org/pdf/1710.02804#page=8))
and ["the right-hand side r of the rule does not overlap with the right-hand side of any other rule"](https://arxiv.org/pdf/1710.02804#page=32).
Tree calculus fails the second test everywhere: every rule's right side is just a value (or an apply that
yields one), so the result of an S step and the result of a K step can be the same tree. **For tree calculus
a record per step is not a flaw of the encoding; it is forced by the base.** Only a base whose outputs say
which case made them (as Theseus requires of loops) can pay per loss instead.

**Reversible interaction nets are an open problem.** An interaction system runs backward deterministically
when ["two active pairs never reduce to overlapping nets and any net can be the result of at most one active pair"](https://arxiv.org/pdf/1806.07275#page=4)
(every rule connected, no clashes). Lafont's combinators fail it, and so does the linear λ rule:
["Not only the interaction rule is disconnected, but we also have a clash"](https://arxiv.org/pdf/1806.07275#page=6).
The question left is to ["find at least one universal reversible interaction system"](https://arxiv.org/pdf/1806.07275#page=7),
universal in Lafont's sense (able to simulate any interaction system), and no later paper found settles it.
Abramsky's reversible combinators are reversible as automata walked by a token,
["but not in the simplistic sense that application between combinators is itself reversible"](https://arxiv.org/pdf/1806.06759#page=4).

**Nothing found combines all four**: code as data, linear, reversible, Turing complete. No linear or reversible
variant of tree calculus, SF-calculus or any triage calculus turned up (Jay's book and blog, his newest
paper, treecalcul.us, arXiv).

## 8. Where that leaves disp

**Our own rules against Salikhmetov's test** (rule_audit.py on residual.py's tables):

| table | rules | connected | right sides alike | not connected |
|---|---|---|---|---|
| forward, history kept | 30 | 25 | none | `A·L`, `A·S` (the residual leaf floats free), `SL·Arms3` (w passes straight through), `Unp·Pair`, `Dn·L` |
| running back | 21 | 16 | none | `A⁻r·S`, `A⁻r·F`, `A⁻·Rk`, `SL⁻·Arms3`, `Unp·Pair` |

So the history-keeping net is close to reversible as a rule table, failing on pure rewiring (`Unp·Pair`), on
copying a leaf, and on steps whose record is one free-floating leaf. Even passing would only make it
Turing complete and reversible, not universal in Lafont's sense, and backward rewriting from the table alone
would also "un-make" loaded values (any loaded `S` looks like `A·L`'s right side), which is why running
back is steered by `E-` from the root.

Two directions follow, one incremental and one a new base.

### Sketch A: keep tree calculus, and stop pretending it can be history-free

Tree calculus pays per step (section 7), so the history-keeping net is the right shape; what can improve is
where its choices live and how many kinds it needs.
- **Local choice instead of a global switch.** Make "apply keeping its residual" and "un-apply" ordinary
  operations a program calls (a `trace`/`untrace` pair, names only), each residual a value the program uses
  or erases. The global mode becomes the default for applications nobody wrapped.
- **Copy only closed code**, in one place. Tree-calculus code is closed, so this is the Lrec and closed
  reduction discipline (above) with nothing to check; data copying can become a derived program.
- **Fewer kinds by connecting rules.** Fixing the five disconnected forward rules (a constructor step's
  record wired to its result, `SL`'s pass-through absorbed) would make the forward table nearly pass
  Salikhmetov's test, and might let some mirror walkers go, if the backward step can be found from the
  right side alone. Untested.
- **Tail loops as feedback wires**: a frame whose result wire leads back to its own input re-creates itself
  each round, with no copy of the loop body.

What it cannot do: run a program with no record. The record is the price of a lossy base.

### Sketch B: a reversible base with tree calculus as a guest

Swap the base for one whose rules are bijections, and run tree calculus inside it as one program among many.
- **Values and code are trees** (as R-CORE: one atom, one constructor; the △ trees as they are).
- **A program is a tree** listing clauses `pattern ↔ pattern`, each variable once on each side, left
  patterns non-overlapping and right patterns non-overlapping, and no exhaustiveness required (so it can
  loop). Composition, `both` (two programs on a pair's halves at once), calls, and loops as labels or a
  trace.
- **Inverting a program is a program on trees**: swap every clause's sides. Running a computation back is
  running its inverse forward. Nothing global, nothing recorded.
- **Copy is a partial program** (A ↔ A ⊗ A, read backward an equality check); **erasing** is of a known
  constant only, or an explicit garbage output, which the program keeps, reuses, or hands to an eraser that
  costs heat. Residuals are ordinary values.
- **Tree calculus as a guest**: an iso `((f, x), t) ↔ (f x, t')` that returns its trace t′, cleaned by
  Bennett's round trip (run, copy the answer, run back) where wanted. Programs written as bijections never
  make a trace.
- **Reflection**: `run` interprets a program given as a tree, handing the program back with the result
  (R-WHILE's rule); the meta-circular Πᵒ interpreter shows loops can re-enter it without copying.

The repo has built this language already: iso.rs (deleted in 2a32736, kept in eca0073) had rearrange,
copy, choose, loop, call, `run`, `both`, `dagger` and `forget`, with tree calculus derived as `tc` and `tck`.
What sank it was the step down: a single walker, 9.4–51× the net's rounds, which Lafont's remark above
predicts. **What is new here is the step down**, by the recipe of sections 2–3 applied to iso.rs's
interpreter:
- each point where the interpreter matches a pattern becomes a frame, one constructor at a time; a deep
  pattern becomes a chain of frames, as in tree calculus;
- `both`, and recursive calls on disjoint parts, give independent frames, which is where parallelism comes
  from (as S's two inner calls do now);
- because clauses are linear and non-overlapping on both sides, the frames that match a left pattern are
  the same frames that match a right pattern, so one family serves both directions and the mirror pantheon
  goes away;
- code read by a frame is consumed, so a program run many times is copied (closed code) or re-created
  through a feedback wire (tail loops).

Whether that net is parallel enough is the open question. Two risks:
- the triangle (section 6): reading program trees still consumes them, so code is copied as now;
- a clause interpreter reads two trees (pattern and value) where tree calculus reads one, so it may need more
  frames per step.

If its rules came out connected and clash-free, it would also be a candidate for Salikhmetov's open
question.

## 9. In brief, and what to try first

- **The sequence** is: a recursive function → frames (defunctionalized continuations) → interaction rules
  (one frame meets one constructor, explicit copy and erase) → the lattice (slots, strands, blocks, pulses),
  with the lattice's room and port limits splitting rules into stages. Every extra agent kind comes from a
  property of the base: deep patterns, variables not used once, lost information, or the lattice's limits.
- **Tree calculus forces a record per step**, because all its right sides are plain values (section 7).
  The residual design is the right shape for it; the inelegance is in the global switch and the mirror
  walkers, not in keeping records.
- **Linear systems loop fine.** Tail loops need only a feedback wire; general recursion needs copying of
  closed code, which tree calculus already restricts itself to.
- **A base that pays per loss** (every function a bijection, losses explicit and local, inversion a program)
  exists as a language (Πᵒ, isos, R-CORE, and the repo's own iso.rs), but nobody has a parallel interaction
  net for one; a universal reversible interaction system is an open problem.

Experiments, in order, all in the Python model first:
1. **Sketch B's net.** Turn iso.rs's interpreter into frames (section 2's recipe), run the repo's two-way
   programs (`add`, `tc` on the 21 compiled calls) with demand, and count parallel rounds against the tree
   calculus net. Pass: within 2× its rounds on `tc`. This decides between the sketches.
2. **Audit it.** Run rule_audit.py's connectivity and look-alike checks on the new table; count kinds.
3. **Sketch A's cleanups**, if B fails: connect the five forward rules and see which mirror walkers become
   unnecessary; add `trace`/`untrace` as per-call operations.
4. **Feedback loops on the lattice**: a tail loop whose frame re-creates itself, measured against the same
   loop by copying.
