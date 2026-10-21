# Searching for a reversible combinator net

A plan, not a result. The question: is there a small set of local rules that only reorganize, plus
one consistent way to forget, that together run useful programs? This note explains why the search
should be over interaction nets rather than over functions on pairs of trees, lays out the search,
and places it in the repo's thermodynamic picture: a reversible rule layer on a partly reversible
physical substrate, so that a program's run can trade time, space, heat and errors against each other
by choice. Counts marked *(measured)* come from research/combinator-search (`cargo run --release --
count 4`, `-- stage1 4` and `-- search`; §4, §5). Stage 1 is done, and it changed the target (§5,
"Stage 1, as run"). The search after it, with footprints as a cost, a varying signature and the
gadget test, is done for small signatures (§5, "After Stage 1"); Stages 4 and 5 are not built.

## 1. The premise, made exact

- **State:** one structure, in which program and data are the same kind of matter.
- **Reorganizing rules:** local rewrites that can be undone. They move, swap and rewire; they do not
  create or lose information. The two-way calculus's `swap`, `cross` and `sink`
  (website/src/lib/twoway/core.ts) are examples.
- **Forgetting:** at least one rule that maps many states to one.
- **Consistent:** the answer does not depend on the order rules fire in (confluence).

**Why forgetting is needed at all.** A system whose every rule can be undone permutes its states, so
every state lies on a path or a loop, and nothing settles. The two-way machine got around this with an
explicit direction (fire runs forward; flip turns the whole run around) and a mark for "this has run"
(`held`, the lid). Without the mark, finished branches oscillated: four branches took 12,927 rounds
against 25 with it *(measured in scratch, earlier in this line of work)*. Forgetting turns paths into
basins, and the answer is where a basin bottoms out.

**Two kinds of forgetting.**
1. **Forgetting time.** Run undoable rules in one direction and keep only where the path ends. The tree
   rule (△x, y) → (x, △y) can be undone, yet run forward it takes (3, 2) and (1, 4) both to (0, 5); only
   the number of steps is forgotten. It costs nothing beyond losing the clock, but it is limited: inputs
   with the same answer must lie on one path, as (3, 2) passes through (1, 4).
2. **Forgetting structure.** A rule that erases or merges: an eraser that takes apart whatever it meets,
   or an un-copy that merges two equal copies. Erasing a bit costs heat, Landauer's
   ["supplying 0.6931 kT per restored bit to the surroundings"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=5);
   un-copying is free in principle, since
   ["the generation (or erasure) of a duplicate copy of data requires no throwing away of information"](https://www.cs.princeton.edu/courses/archive/fall04/cos576/papers/bennett73.html#Eqn11).

**Where the two-way calculus sits.** It forgets neither, and that is where its constants come from:
frames, the lid, †, five combinators and 27 rule kinds are all ways of remembering where the run is. A
program counter is time written into the state; once the state records time, no two inputs share a
path, so only one-to-one functions remain. Measured on it: 60–66% of all moves are sequence, `fix`,
`self` and `id` bookkeeping (`addw`: 4,114 of 6,816 moves).

## 2. Why nets, and not functions from a tree pair to a tree pair

The two-way work searched for local rules on a (program, value) pair of trees. What it found, in order:

1. **Self-inverse moves exist, control does not come with them.** `swap` and `cross` reach every
   arrangement of up to 6 opaque parts, and adding `sink` reaches every arrangement with stems, but no
   single rule or pair of rules is complete once stems are present, and none of them sequences anything.
   Sequencing, branching and recursion had to be added as combinators with frames.
2. **Moving matter between program and value records the input.** 9,277 self-inverse rules move nodes
   between the two trees (up to 3 nodes a side). Conserving nodes turns the program into fuel, and the
   fuel left over depends on the input: the history ends up written into the program.
3. **A cursor costs either space or time.** Running a list backward needs its reverse. Storing every
   part's reverse halved the rounds (`not` 29 → 10, `addw` 2,633 → 1,210) but made `addw` a
   1,221,346-node tree (9,024 if equal parts were shared). Rewinding instead gave back the frame
   machine's rounds exactly; its four resting modes are the lid and † marks again.
4. **Deleting what has run is fast.** Dropping finished parts instead of keeping them for the way back
   took 30–50% fewer rounds and half the frames, with the same answers on 722 library runs; everything
   it dropped was a copy of program text. *(A scratch prototype, not in the repo.)*

What a tree lacks, and a net has:

| need | a pair of trees | a net |
|---|---|---|
| where the run is | a cursor: frames, lids, marks | the wires: work happens wherever two principal ports meet |
| sending a result somewhere | rearranging it there, a level at a time | a wire, already there |
| acting deep inside a value | the program descends (map, tri frames), and climbs back | an agent sits where its data is |
| order independence | a schedule, or critical-pair analysis of overlapping patterns | free: one rule per active pair, and active pairs never overlap |
| does a rule lose information | depends on the rest of the tree | decided per rule, from the rule alone (§3) |
| running on the lattice | compiled to agents first (the strand lattice's tree-net) | native |

On order independence, Lafont: ["if one abstracts from the irrelevant order of application of rules, there is only one possible reduction"](https://web.archive.org/web/20130509130326id_/http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.5761&rep=rep1&type=pdf#page=3).
So the "consistent" in the premise comes for free in nets.

What trees keep, and nets must recover:
- **A canonical form.** Nets have isomorphic copies; with ordered ports, a walk from the free ports in a
  fixed order numbers every agent, which gives a canonical form in linear time (§4 uses this).
- **Reading.** A net is read back as a tree (research/TREE_READBACK.md).
- **Programs as data.** A tree is a net of leaf, stem and fork agents (the strand lattice's encoding),
  so tree programs remain one encoding among others, and the two-way calculus a program style on top.

## 3. The target

An **interaction system** is a set of agent kinds, each with a principal port and a fixed number of
auxiliary ports, and at most one rule for each unordered pair of kinds: when two agents meet at their
principal ports (an active pair), they are replaced by a net wired to their auxiliary ports.

**The known point.** Lafont's interaction combinators,
["with only 3 symbols and 6 rules, is a universal"](https://web.archive.org/web/20130509130326id_/http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.5761&rep=rep1&type=pdf#page=1)
model: γ and δ (binary) undergo ["annihilation when they carry the same symbol"](https://web.archive.org/web/20130509130326id_/http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.5761&rep=rep1&type=pdf#page=8)
and pass through each other (commutation, four new agents) otherwise; ε erases. That is almost exactly
"reorganization plus one forgetting agent", except that annihilation also forgets.

**When a system can run backward.** Salikhmetov: ["An interaction system is reversible if and only if no two different active pairs clash and each rule is connected"](https://arxiv.org/pdf/1806.07275#page=4).
- Two different active pairs **clash** when they reduce to the same net. γγ and δδ annihilation both
  leave plain wires, so they ["clash in the system of interaction combinators"](https://arxiv.org/pdf/1806.07275#page=3).
- A rule is **connected** unless its right side falls into pieces sharing no wire. Annihilation rules
  ["are disconnected"](https://arxiv.org/pdf/1806.07275#page=4); commutation is connected.
- He leaves open whether ["there exists a reversible equivalent of interaction combinators"](https://arxiv.org/pdf/1806.07275#page=6).

**Our target relaxed that question:** the smallest universal interaction system in which every rule
is connected and clash-free *except the eraser's*, so that all forgetting sits in one agent kind.
Stage 1 showed that this is empty in the space searched, and why (§5, "Stage 1, as run"); the search
goes on with the fallback found there, undo with a footprint, which keeps the points below.
- **Heat becomes a decision.** Every erased bit is an eraser interaction, which a runtime can delay,
  batch, aim at copies (free), or avoid by running back (§6).
- **The ledger is exact.** In the smallest space (two binary kinds), a connected right side needs at
  least two agents, so non-eraser rules never shrink a net; only erasers do. Garbage is explicit and
  countable.
- **Either answer is useful.** A system found is a reversible core with one dial for forgetting. A
  proof that none exists within small sizes says which forgetting is unavoidable (annihilation, say),
  and that becomes the next thing to price.

## 4. The search space (measured)

Two binary kinds A and B, plus an eraser E with its fixed rules. A system is a rule for A A, one for
B B, and one for A B. A right side for a binary pair is a net on four free ports (the two agents'
auxiliary ports) whose ports are wired in pairs; nets that leave an agent unreachable from the free
ports (closed garbage) are dropped, and rules are counted up to renaming the right side's agents.
research/combinator-search enumerates them, writes each in a canonical form (the walk from the free
ports in a fixed order, numbering agents as they are met), and checks:
- **symmetric:** a rule for X X reads the same when its two agents trade places (needed for it to be
  well defined);
- **connected**, per Salikhmetov;
- **rigid and apart:** no renaming of a right side's free ports gives back the same net (apart from the
  trade above), and no two of a system's three right sides are the same net under any renaming of
  their free ports. This is a necessary condition for no clash, checked on the right sides alone: two
  active pairs whose right sides are the same net reduce to the same thing. A clash can also come from
  the terms in the ports filling in what tells two right sides apart, which Stage 1 must check on small
  terms.

It counts systems both as written and up to renaming (swapping A and B, and swapping a kind's two
auxiliary ports: eight renamings, counted by Burnside's lemma). The eraser appears only in right sides
of up to two agents. *(measured)*

| space | right sides | for X X / reversible-shaped | for A B / reversible-shaped | systems | up to renaming | reversible except the eraser, up to renaming |
|---|---|---|---|---|---|---|
| up to 2 agents | 2,001 | 65 / 24 | 2,001 / 360 | 8,454,225 | 1,069,266 | 21,992 |
| up to 2 agents, quiet | 1,785 | 49 / 16 | 1,785 / 336 | 4,285,785 | 539,176 | 8,128 |
| up to 4 agents | 1,278,561 | 2,117 / 696 | 1,278,561 / 495,936 | 5.73 × 10¹² | 7.17 × 10¹¹ | 2.99 × 10¹⁰ |
| up to 4 agents, quiet | 784,713 | 1,157 / 360 | 784,713 / 301,080 | 1.05 × 10¹² | 1.31 × 10¹¹ | 4.82 × 10⁹ |
| 4 agents, outward | 1,680 | 100 / 32 | 1,680 / 648 | 16,800,000 | 2,145,959 | 72,824 |

- **Quiet:** no active pair is left inside the right side, so a rule ends in a net that is already
  done. All six of Lafont's rules are quiet.
- **Outward:** every free port meets one of the new agents' principal ports, so the new agents face
  the rest of the net, as in Lafont's commutation. (Outward right sides have exactly four agents.)
- **Where interaction combinators sit:** their commutation rule is an outward 4-agent right side, and
  their annihilations are the 0-agent ones; so they are in the "up to 4 agents" row, but not in the
  outward row, whose same-kind rules have four agents. Annihilations are disconnected, so they are
  never reversible-shaped.
- **A consequence of connected:** with four free ports and binary agents, a connected right side has at
  least two agents, so in every reversible-shaped system the non-eraser rules never shrink a net.

The whole 4-agent space is far too big to run system by system: the next section works on the rows
that can be.

## 5. The pipeline

Cheapest filters first. Each stage writes its survivors to a file, so stages can be rerun with
different settings without redoing earlier ones.

**Stage 0: enumerate and canonicalize.** As in §4. Identify systems that differ only by renaming A and
B, or by permuting the auxiliary ports of a kind.

**Stage 1: static filters.** Symmetric same-kind rules; reversibility (connected, rigid, clash-free)
for every rule but the eraser's. The eraser's rules are fixed: E meeting an agent puts an E on each of
its auxiliary ports, and E meeting E leaves nothing.

**Stage 1, as run** (`cargo run --release -- stage1 4`, 52 s; survivors written to `out/`).
- **No rule here can be reversible, and no system.** Two different rules whose agents have the same
  number of auxiliary ports in total always clash: Salikhmetov proves it (his Proposition 7), and A A,
  B B and A B all have four. The same gluing argument makes a rule clash with itself. Glue a copy of
  its right side into its left side's ports through a renaming σ of the ports, and again through σ⁻¹.
  The two active pairs differ, yet both reduce to the same net: two copies of the right side glued
  through σ. Whenever σ is not its own inverse and no symmetry of the right side undoes it, that is a
  clash; with three or more auxiliary ports such a σ exists unless the right side is unusually
  symmetric. Checked, not just argued: of the 499,584 connected right sides (as A A, B B and A B rules), every one that leaves no
  active pair inside clashes this way, with the two reducts confirmed to be the same net. The only
  exceptions are 5 rules that just turn the pair into another pair, such as A⋈B → A⋈A. Lafont's
  commutation is among those that clash.
- **What it means.** In an interaction net, a step cannot be undone from the net alone: something
  must say where the step happened, which agents are the right side it made. That is the two-way
  calculus's lesson again (its frames and marks said where the run was), and it is what the strand
  lattice's records do (evaluators/rust-ic-strands/README.md, "Records", line 1498).
- **The fallback: undo with a footprint.** Each step leaves a footprint: a record of which rule fired,
  on one of the agents it made. A rule then needs to be connected, so the footprint on one agent finds
  the whole right side. Two rules may share a right side, since the footprint names the rule. Undoing
  a step reads its footprint and removes it; the only lasting irreversibility is forgetting
  footprints, and the eraser. It is Bennett's history at its smallest: one mark a step, heat when
  forgotten, cleared by running back.
- **Correction, found while pricing footprints:** the table below also asked for *rigid* rules (no
  renaming of the free ports gives back the same right side), so that the footprint need not say how
  the ports were assigned. That is not needed. If a rule always leaves its footprint on the same agent
  of its right side, the walk from that agent along numbered ports finds every agent and every wire
  out, so it fixes which outside wire was which port. The counts below are therefore an undercount of
  the systems that can be undone given a footprint.

| space (up to renaming) | systems | undoable given a footprint |
|---|---|---|
| right sides of up to 2 agents | 1,069,266 | 26,408 |
| up to 2 agents, quiet | 539,176 | 10,816 |
| 4 agents, outward | 2,145,959 | 83,384 |
| up to 4 agents, quiet | 1.31 × 10¹¹ | 4.88 × 10⁹ |

*(measured, with the rigidity condition that the correction above drops)*

**After Stage 1: footprints as a cost** (`cargo run --release -- search S G K N [wires]`;
research/combinator-search/src/search.rs, with the reducer in live.rs, signatures in space.rs, gadgets
in ic.rs and random nets in behave.rs). Stage 1 left the strict target empty, so the search changed
what it looks for rather than which rules it allows:
- **Footprints are a cost, not a filter.** A step leaves one mark for each part of its right side
  (a part with no agent, a bare wire, is marked on the wire). The mark names the rule, in log₂ of the
  number of rules bits.
- **The signature varies:** one binary kind (A), a binary and a unary kind (A U), two binary kinds
  (A B), and a binary and two unary kinds (A U V), each with the eraser E and its fixed rules. A pair
  of kinds gets any quiet right side of up to K agents, or any outward one.
- **Adequate means running the combinators through gadgets.** Small nets of the system's agents stand
  for γ and δ, E stands for ε, and each of the six combinator rules, both sides built from gadgets,
  must hold: the left side reduces to the right side. Annihilation may wire its ports straight (first
  to first) or crossed. A gadget's principal port must sit on an agent's principal port, its two
  auxiliary ports on auxiliary ports, with no active pair inside.
- **How this reaches tree calculus.** Not through an existing compiler: evaluators/tree-net is its own
  interaction system of 14 core kinds (leaf, stem, fork, apply, triage, pair, duplicator, eraser and
  so on, tree-net/crate/src/rules.rs), not built from γ, δ and ε. It reaches the combinators through
  Lafont's theorem that every interaction system translates into them. Gadgets for tree-net's own
  kinds would be the direct test, but they are not built: 14 kinds and their rules make a far larger
  gadget search than γ and δ.

**The footprint law.** Write Φ for the sum over a net's agents of (auxiliary ports − 1): +1 for a
binary agent, 0 for a unary one, −1 for an eraser. For a closed connected net, Φ is twice the
number of independent loops, less 2.
- **A step can lower Φ only by splitting.** If its right side has c parts, Φ falls by at most 2(c − 1).
  Proof: count the right side's k agents and f free ports as nodes. In c parts they need at least
  k + f − c wires. The wires have f + k + (auxiliary ports) ends in all, two each, so the auxiliary
  ports number at least k + f − 2c, which makes the right side's Φ at least f − 2c. The pair's Φ is
  f − 2.
- **Over a run, the marks beyond one a step are at least half of what Φ fell.** In other words,
  footprint ≥ steps + half the shrinkage. Checked on every right side searched: 5,385 across the four
  signatures, the eraser's included, of which 990 meet it with equality.
- **Any gadget for γ or δ has Φ = 1 + 2w** (w the wires joining two auxiliary ports inside it), so
  Φ ≥ 1. Annihilation (two gadgets → two wires) and erasure (ε and a gadget → two erasers) both lower
  Φ. So every system that runs the combinators has right sides in several parts, and so steps that
  cannot be undone locally. The fewest marks a simulated combinator rule can cost, if it takes one
  step (each further step adds at least one): annihilation 1 + Φ(gadget), commutation 1, erasure
  1 + (Φ(gadget) + 1)/2.
- **Lafont's combinators meet that bound on every rule:** 2 marks for γγ, δδ, γε and δε, 1 for γδ.
  No system can run them with fewer marks.

**What the search found** *(measured; 16 threads)*:

| signature | rules | systems | searched | gadgets (up to) | adequate | up to renaming | time |
|---|---|---|---|---|---|---|---|
| A, E | 3 | 59 | all | 202 (3) | 0 | 0 | 0 s |
| A U, E | 6 | 119,280 | all | 236 (3) | 0 | 0 | 6 s |
| A U, E, same-kind pairs only wires | 6 | 426 | all | 29,942 (5) | 0 | 0 | 600 s |
| A B, E | 6 | 105,658,344 | all | 4 (1) | 4 | 3 | 51 s |
| A B, E | 6 | 105,658,344 | 200,000 sampled | 1,476 (3) | 0 | 0 | 98 s |
| A B, E, same-kind pairs only wires | 6 | 36,234 | all | 1,476 (3) | 43 | 16 | 237 s |
| A U V, E | 10 | 6.4 × 10¹⁰ | 200,000 sampled | 318 (3) | 0 | 0 | 19 s |
| A U V, E, same-kind pairs only wires | 10 | 13,797,444 | 1,000,000 sampled | 318 (3) | 0 | 0 | 539 s |

- **With one-agent gadgets, the only systems are the combinators:** Lafont's commutation for A B,
  with each annihilation straight or crossed (3 up to renaming). This is the plan's validation check,
  passed: the search finds the known point.
- **With gadgets of up to three agents, 39 more turn up** (13 more up to renaming), all with a
  commutation-shaped A B rule: four new agents in a square, but wired or labelled differently. For
  example, A ⋈ B putting A agents on A's side and B agents on B's side. They need gadgets of 4 to 6
  agents per γ, δ pair (for instance γ = two A and an eraser, δ = B). Every one is beaten by the
  combinators on all three counts at once:

| gadgets γ + δ | agents | interactions | marks | floor (law above) |
|---|---|---|---|---|
| A + B (the combinators) | 2 | 5 | 9 | 9 |
| A A E + B | 4 | 11 | 18 | 15 |
| A A A + B | 4 | 13 | 22 | 20 |
| A B E + A B E | 6 | 21 | 33 | 25 |
| A A A + B B E | 6 | 25 | 40 | 32 |
| A A B + A B B | 6 | 29 | 47 | 39 |

  (Interactions and marks are summed over γγ, δδ, γδ, γε and δε. The floor is interactions plus the
  law's extra marks.)
- **One binary kind fails, as it must here:** its single rule would have to both shrink (for
  annihilation) and grow (for commutation), and no right side in the space does both.
- **A binary kind with one or two unary kinds: nothing.** A U was searched in full with gadgets of up
  to three agents, and of up to five when same-kind pairs only make wires; A U V was sampled.

**Stage 2, as run** (random closed nets of 30 agents, one in ten an eraser, 60 nets a system, capped
at 200 interactions and 50 agents per loaded agent):

| systems | halt | blow up | interactions an agent | peak | left at the end | eraser share | marks an interaction |
|---|---|---|---|---|---|---|---|
| the 43 adequate ones | 63% | 28% | 1.0 | 1.12× | 0.64× | 57% | 1.61 |
| 200 random, same-kind pairs only wires | 87% | 4% | 0.8 | 1.05× | 0.54× | 57% | 1.78 |
| 200 random, A B | 50% | 43% | 2.7 | 1.95× | 1.17× | 58% | 1.66 |

Random nets say little about adequacy: the adequate systems sit between the two random samples, and
mostly do short work before halting or growing without end. Stage 2 as a filter would not have found
the combinators; the gadget test did.

**What it means.**
- **The footprint is set by the computation, not by the rules.** One mark a step says where; beyond
  that, a run pays at least half of what it shrinks Φ. Rules can only add waste (bigger gadgets, more
  steps, more splitting than the shrinkage needs). The combinators add none, and nothing small beats
  them.
- **What is left to choose is time and place, not marks.** The steps that split, annihilation and
  erasure, are the ones whose marks end up apart, so undoing them is not local. Where those marks sit,
  and how far apart, is a question about the substrate's layout and schedule (the strand lattice's
  records; the spatial search in §7), not about the rule set.

**Stage 2: behaviour on random nets.** Build random closed nets of 10–100 agents (A, B and a few E,
wired at random), reduce each to normal form under a step cap, and record per system:
- whether runs halt, and how often;
- growth: peak agents against loaded agents (space);
- interactions and parallel rounds to normal form (time);
- eraser interactions (heat);
- how many different normal forms appear, and their sizes.

Drop systems that always collapse to a few wires, always blow up, or only loop. Keep long transients
with varied normal forms: the "interesting reorganization". Needs a fast reducer (Rust; the strand
lattice crate already has net types). Estimate, to be measured: about 1 µs an interaction, so each
system takes about 0.1 s (100 nets × 10³ interactions). Run system by system, that covers the
2-agent space (21,992 reversible systems up to renaming) and the outward 4-agent class (72,824) in
an hour or so on all cores. The rest of the 4-agent space (2.99 × 10¹⁰, or 4.82 × 10⁹ quiet) needs
another way in:
- **Sample it**, uniformly up to renaming, and see whether the survivors cluster;
- **Factor it**: score the A B rule on its own first (does it copy? does it let A pass through B?),
  then pair the few good ones with same-kind rules;
- **Grow it**: start from the outward and 2-agent survivors and allow one more agent where the
  survivors point.

**Stage 3: universality by translation.** Lafont calls a system universal when every interaction
system translates into it: each agent kind becomes a net (a gadget), and each rule's left side, so
translated, reduces to its right side translated. Interaction combinators are universal in this
sense, and translations compose, so finding gadgets for γ, δ and ε in a candidate shows the
candidate universal. The search: for each surviving system, enumerate gadgets of up to 4–6 agents
with the right interfaces (a principal port and two auxiliary ports for γ and δ, one port for ε) and
check all six combinator rules. A second, independent check: gadgets for the strand lattice's
tree-calculus agents (leaf, stem, fork, application).

**Stage 4: benchmarks.** Through the gadgets, or encoded by hand: `not`, `add`, `copy`, `mirror`,
`notw`, `cnotw` and `addw` from the two-way page. Measure time (rounds), space (peak agents), heat
(eraser interactions) and, against the two-way machine, how much bookkeeping disappears.

**Stage 5: the Pareto front.** Rules (count and size), gadget sizes, the reversible fraction of
interactions in typical runs, and the benchmark costs. Report every system not dominated on all of
them.

**Validation, before trusting any of it.**
- With the reversibility filter off, same-kind rules from the 2-agent space and A B rules from the
  outward class must turn up interaction combinators (up to renaming), with identity gadgets.
- Systems with one binary kind must fail Stage 3.
- The reducer must agree with the strand lattice's on interaction combinators.

**Order of work.** Stages 0–3 are research/combinator-search, run as described under "Stage 1, as
run" and "After Stage 1". Stage 2 turned out not to predict Stage 3, so the gadget test runs on whole
spaces where they are small enough, and on samples or the same-kind-wires subspace where not.

## 6. Why it matters: a reversible layer on a thermodynamic substrate

The picture the repo has been converging on, in layers:

1. **The physical substrate, partly reversible.** The strand lattice run as physics: every move is
   proposed as often as its reverse, a drive makes forward likelier, every site has a bank of heat, and
   errors come from the height of the barrier that holds the wiring
   (evaluators/rust-ic-strands/README.md, "The substrate" at line 1486 and "Run thermodynamically" at
   line 1562).
2. **A reversible rule layer.** The system this search looks for: every interaction except erasure can
   be undone, so it costs only the drive's dissipation, which in a Brownian computer
   ["approaches zero only in the limit of zero speed"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=1).
   (A ballistic computer could go fast at no cost in principle, but its positions
   ["would have to be corrected after every few collisions"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=6):
   the cost moves into error correction.)
3. **An execution policy.** For each part of a run: keep its history (space), run it back afterwards
   (time, Bennett's clean run), erase it (heat), recompute it later instead of keeping it (time for
   space), copy ahead for parallelism (space for time), or merge copies back (free while the twin is
   near).
4. **A goal.** The user, or a program, states a budget or prices for time, space, heat and errors, and
   the runtime picks the policy. Budgets and prices are two forms of one constraint
   (research/NATURE_OF_AGENTS.md §6, "Regularizers are prices").

**Why the middle layer has to be concrete.** If forgetting is built into ordinary rules (tree
calculus's K, combinator annihilation), every rewrite pays heat whether the program needed to forget
or not, and the runtime has nothing to trade. If every rule but erasure can be undone, each erased bit
is a separate, visible decision, and layer 3 has room to work.

**The trade-offs, with the knob and what the repo has measured:**

| trade | knob | measured or cited |
|---|---|---|
| time ↔ heat | the drive | drive 2 takes 14× the clocks of drive 8 for a sixth of the heat (strands README, line 1609) |
| space ↔ heat | keep history, or commit (forget) | keeping everything costs 2.2–2.6× the loaded agents; erasing every leaf leaves about what the plain lattice leaves, for heat (strands README, lines 1547–1553) |
| time ↔ space | pebbling: which intermediate results to keep | you may only ["remove a pebble from node i, provided node i - 1 is pebbled at the time"](https://arxiv.org/pdf/quant-ph/9703022#page=7); the range runs from ["the LMT-simulation using exponential time and no extra space"](https://arxiv.org/pdf/quant-ph/0101133#page=9) to Bennett's square space |
| errors ↔ energy | barrier height | every slip spoils the answer; under one slip a day needs about 55 kT (strands README, lines 1625–1643) |
| space ↔ time | copies for parallelism | copies "give each call a private history and its own place to run" (research/RULES_TO_LATTICE.md:184) |
| heat ↔ distance | merge copies near their twin, or erase them far away | 90–98% of erased agents are copies (research/REVERSIBLE_PIPELINE.md:28–29); a copy's refund needs its twin still matched (research/REVERSIBLE_CORE.md §2, at d7e3b96) |

On the two-way calculus, the same three corners show up *(measured in scratch)*: merging finished
parts back (the frame machine) pays time, `addw` 2,633 rounds; deleting them pays heat, 1,811 rounds
and 33,083 program nodes erased; storing reverses pays space, 1,210 rounds and a 1.2M-node tree. A
good substrate has a large region of such points reachable by independent knobs, and Stage 4's
measurements place each candidate in it.

**What the repo already says about this, and where:**
- research/NATURE_OF_AGENTS.md: the resource table (space as fuel, time as rounds, entropy, heat as
  commit), lines 58–66, and "Regularizers are prices", lines 148–170, which prices erasure among other
  resources and sets an exchange rate between them.
- research/REVERSIBLE_CORE.md, archived (read it with `git show d7e3b96:research/REVERSIBLE_CORE.md`):
  "What copying and erasing cost" (§2), and "History as a resource" (§3.6): a clean run, an inverse
  that erases the input, undo anywhere, measuring the true loss, and "Forget for heat".
- research/REVERSIBLE_PIPELINE.md: keeping history costs space and removing it costs time, lines
  102–115; "Forgetting is a separate act", lines 219–221.
- research/RULES_TO_LATTICE.md §6: what reversibility, locality and code-as-data cost, and the triangle
  among them (lines 151–186).
- evaluators/rust-ic-strands/README.md: the substrate as physics (line 1486), running
  thermodynamically (line 1562), the drive as a speed-for-heat knob, and errors against barrier
  height; `iso.rs` (line 1318), a two-way tree calculus whose instructions are rearrange, copy and
  forget; `twoway.rs` (line 1368), two-way programs on the lattice.
- research/COARSE_GRAINED_LATTICE.md: time–cost trade-offs in scheduling (line 55).
- research/interaction-combinator/reversible-ca.html: a 1D toy in which annihilation deposits its bits
  as heat instead of deleting them.
- research/PAST_CA_WORK.md, lines 174–182: interaction calculus has no rule for erasing one output of a
  duplicator; a projector agent is proposed. That is the un-copy this note's forgetting would use.

None of these states the layered design as one picture; this section is that picture, and the search
is its missing middle layer.

## 7. Open questions and risks

- **The relaxed target has no solution here** (§5, Stage 1): forgetting where a step happened is
  unavoidable in interaction nets. How cheap the footprint can be is now bounded (§5, "The footprint
  law"): at least a mark a step plus half of what the run shrinks Φ, which the combinators meet. What
  stays open is how many footprints a program can drop for free (un-copying) rather than for heat,
  and how far apart the marks of a splitting step end up.
- **Next: a spatial search.** A net cannot get below a mark a step, because nothing in it says where
  a step happened. A lattice with a fixed schedule can: if each clock applies one rule to disjoint
  blocks of cells, on a partition that alternates between clocks, the clock can be undone exactly
  when the rule is a bijection on a block, and "where" is the schedule's job, with no marks. The
  search there is over block rules that carry the strand lattice's agents and wires.
- **Gadget translation is sufficient for universality, not necessary.** A universal system with no
  small gadgets for γ and δ would be missed; gadgets for tree-net's own kinds would be the second
  chance, and are not built. The A B space with three-agent gadgets is only sampled outside the
  same-kind-wires subspace (200,000 of 10⁸ systems).
- **The right sides tried are narrow:** quiet ones of up to two agents, and outward ones. A rule that
  grows and leaves a bare wire at once (three or more agents, not outward) is outside the space, which
  is why one binary kind fails by construction.
- **Erasing propagates.** The eraser's own rules are the disconnected ones; whether erasure can be
  confined to copies (un-copy, free) or must also take originals (heat) is a property of the programs,
  measured in Stage 4.
- **Where the two-way calculus goes.** If a reversible core is found, the two-way calculus becomes a
  way of writing programs for it (a compiler target), not the machine.
