# Past work on running interaction nets as a cellular automaton

Literature survey, 2026-10-09. Nothing here is landed. Questions: has anything like the strand
lattice been built before? If not, how close did people get and where did they stop? What does
the literature suggest changing?

Terms used throughout:
- **the strand lattice**: `evaluators/rust-ic-strands`. disp's tree-calculus interaction net
  run on a 2D/3D grid where each wire is a physical path of strands through each site's
  switchboard, and nothing has an address. Agents walk, dragging their wires, and a rewrite fires
  when two partners meet. Moves are accepted by a Metropolis energy (tension, crowding, a
  temperature). Demand pulses along wires and a 2-bit demand field through space make it lazy, and
  erasers collect garbage by touching it. It has a bit-exact chip (`hw/`) and GPU version.
- **the mesh**: `evaluators/rust-ic-mesh`, the same net on tiles with global addresses and a
  router in every tile.
- **fork**: the strand lattice's S rule, which starts both halves wanted instead of only the
  first.

**Inference** marks a claim that no source makes: it is ours.

## 1. The answer

**No.** Nothing published runs a functional calculus as an interaction net whose wires are
physical paths on a lattice. One paper got about halfway: interaction automata (ia2d, 2016;
section 2). Everything else stops on one of three axes:
- **laid out in space, but computes nothing functional**: lattice chemistries, polymer models,
  Brownian cellular automata, Ackley's Movable Feast Machine;
- **functional, but not laid out**: chemlambda, AlChemy, HVM2;
- **hardware that keeps addresses**: Taelin's 2024 chip sketch, and probably Tendrils Compute.

Each evaluation idea has an ancestor that is not spatial (section 4): demand pulses come from
token passing, fork from branch-first and optimistic evaluation, superpositions from HVM's
interaction calculus. Not found anywhere: tree calculus as an interaction net, a demand field
through space, strand-and-switchboard wiring, a chip design for a net laid out on a lattice, or a
measured comparison of addresses against physical wires (the strand lattice against the mesh
seems to be the only one).

The "no" rests partly on searches that found nothing (section 6).

## 2. The closest attempt: interaction automata (ia2d)

Gimenez and Obwaller, FSCD 2016, pitch ia2d as ["a proof-of-concept piece of abstract
hardware"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=1)
for running functional programs in parallel. Its grid has cells ["each of which can hold up to two
nodes"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=6),
the same as ours. A node's pointers must stay within a small radius, and a one-port forwarder node
["can be used to lengthen the wires if
necessary"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=6).
Before each parallel step, nodes migrate toward emptier neighbourhoods, with random tie-breaks.
Rewrites that need new nodes are ["delayed until some space is made available
locally"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=7),
so parallel runs gradually go single-file, as ours do.

| | ia2d | the strand lattice |
|---|---|---|
| wire | pointer within a radius; long wires are forwarder nodes taking node seats | strand paths through switchboards; many strands a link; wires cross freely |
| motion | greedy migration toward empty space | Metropolis step, exchange, fold, flip |
| schedule | global synchronous passes | Margolus blocks or random turns |
| laziness | only when reading the input program | demand pulses and demand field |
| programs | arithmetic, merge sort, bitonic sort | tree calculus (26 rules) plus superpositions |
| platform | 2D, Haskell | 2D/3D, Rust, WebGPU, bit-exact Verilog |

A 32-leaf bitonic sort [took 1,740 steps](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=10) run one rewrite at a time against 52.8 in parallel. The
authors admit the hoped-for speed-up ["is not yet met on this example with a naive migration
strategy"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol052-fscd2016/LIPIcs.FSCD.2016.35/LIPIcs.FSCD.2016.35.pdf#page=10).
They list lambda encodings and 3D grids as future work. No follow-up turned up.

**Inference.** Both machines hit "a rewrite waits for room, so the run goes single-file". So the
hard problem in spatial reduction is making room, not rewriting fast.

## 3. Everything else, by where it stops

**Laid out in space, computes nothing functional.**
- Squirm3 (Hutton): atoms random-walk on a grid, and a move is allowed only ["if the atom remains
  within the neighbourhood of every atom it is bonded
  with"](https://faculty.cc.gatech.edu/~turk/bio_sim/articles/hutton_rep_molecules.pdf#page=4).
  It is the closest precedent for agents dragging their wires, but it only replicates templates.
- The bond-fluctuation model: lattice polymers moved by Metropolis moves, built so that ["polymer
  chains cannot cross each other"](https://en.wikipedia.org/wiki/Bond_fluctuation_model#mwJQ).
  Our wires drop that constraint.
- Brownian cellular automata (Peper, Lee): thermal noise pushes tokens around fixed circuits,
  ["sped up by the selective placement of so-called
  ratchets"](https://www.oldcitypublishing.com/journals/jca-home/jca-issue-contents/jca-volume-5-number-3-2010/jca-5-3-p-185-206/#:~:text=selective%20placement%20of%20so%2Dcalled%20ratchets).
- The Movable Feast Machine (Ackley; asynchronous tiles, local event windows) asks scalable
  machines to ["forego unique
  addresses"](https://www.cs.unm.edu/~ackley/papers/hotos-11.pdf#page=1). Its most general
  computation was ["a simple stack machine
  interpreter"](https://www.cs.unm.edu/~ackley/papers/hotos-11.pdf#page=3). Nothing lambda-like
  was found running on it.
- Nubots (Woods et al.; bonded monomers that move on a grid) show that ["asynchronous cellular
  automata are not capable of this parallelism"](https://arxiv.org/pdf/1405.0527#page=1) without a
  rigid push.

**Functional, but not laid out.**
- chemlambda and chemSKI (Buliga): random, local, asynchronous rewriting of graphs much like
  interaction nets, but edges have global names. chemSKI's tokens exist to solve ["the problem of
  new edge names"](https://arxiv.org/pdf/2306.00938#page=1). **Inference:** a physical wire
  solves that by construction, since a wire is its path.
- AlChemy (Fontana and Buss) is a [well-mixed lambda-calculus
  soup](https://arxiv.org/pdf/2408.12137#page=1).
- Graph-rewriting automata ([Tomita](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=5))
  and causal graph dynamics ([Arrighi and Dowek](https://arxiv.org/pdf/1202.1098#page=1)) treat the
  graph itself as space, with no geometry.
- Lafont's [Interaction
  Combinators](https://chorasimilarity.wordpress.com/wp-content/uploads/2024/01/ic-lafont-1.pdf#page=1)
  sets interaction nets against cellular automata, which have "a global synchronization of
  transitions". **Inference:** our Margolus-block mode brings that back; random turns do not.

**Hardware and runtimes that keep addresses.**
- HVM2 (HigherOrderCO's GPU interaction-net runtime) keeps the net in one memory and joins wires
  through ["a global, atomic substitution
  map"](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=9). Its
  biggest GPU gains came from locality: working in block-local memory [took it from 13,000 to 54,000
  million interactions a second](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=21). It has no lazy mode because that would need it to ["store
  uplinks"](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=23),
  which is what demand pulses avoid.
- Jiresch's GPU evaluator (2014) has no pointers and finds partners by sorting every connection.
  The rewriting itself was ["less than 0.1 percent of the total
  time"](https://arxiv.org/pdf/1404.0076#page=9).
- Kuszmaul's Connection Machine reducer (1986) put one combinator on each processor behind the
  router, and ["ran out of
  processors"](https://ftp.mirrorservice.org/sites/www.bitsavers.org/pdf/mit/lcs/tr/MIT-LCS-TR-0362.pdf#page=62)
  on fib 5 with 512 of them.
- Partain's [Graph Reduction Without Pointers](https://www.cs.unc.edu/techreports/89-045.pdf#page=4)
  (1989) does get rid of global pointers, but on a line of cells plus a tree network, not a
  lattice.
- Taelin's 2024 [chip
  sketch](https://api.fxtwitter.com/VictorTaelin/status/1806690584670679387) is "a 3D grid of
  interaction cores", but each core stores 48-bit pointers.
- Tendrils Compute (Vine's authors, Cambridge UK) is building an interaction-net chip and is
  [reported](https://www.newcomer.co/p/scoop-benchmark-backs-early-stage) to be raising a
  Benchmark-led round. It says only that cores keep data in [local
  SRAM](https://tendrils.co/background) with [no cache coherency](https://tendrils.co/about).
  **Inference:** that reads like the mesh, not the strand lattice. Its layout is private, so it is
  the one group that could have built something similar without publishing.

Bilardi and Preparata argue that under speed-of-light and volume limits ["scalability holds only
for neighborly
interconnections"](https://cs.brown.edu/research/pubs/techreports/reports/93/cs93-20.pdf#page=3). So the
lattice is in the right class; its limits come from the shape of the nets it runs.

## 4. Where the evaluation ideas come from

- **Demand pulses** are token passing with many tokens. Sinot's single token ["forces evaluation
  to proceed
  sequentially"](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=2).
  Salikhmetov's needed reduction has the same rule as ours: ["a needed term makes its parent agent
  needed"](https://arxiv.org/pdf/1702.06092#page=2). The dynamic geometry-of-interaction machine
  (Muroya and Ghica) runs ["three evaluations"](https://arxiv.org/pdf/1803.00427#page=1) (call-by-need and
  two call-by-value orders) on one token-guided rewriter by changing only the token's rules.
- **The demand field** is amorphous computing's [hop-count
  gradient](https://en.wikipedia.org/wiki/Amorphous_computing#mwJw), and close to Lin and Keller's
  [pressure gradient](https://scholarship.claremont.edu/hmc_fac_pub/258/#abstract) for spreading work over a
  mesh. **Inference:** a 2-bit max-minus-one field cannot hold itself up without a source, so it
  heals within 3 clocks. The slow "rising value" problem of real-valued gradients cannot happen.
- **One walker at a time** comes from laziness, not the lattice. HVM3's lazy mode lists
  ["Parallelism is still an open
  problem"](https://github.com/HigherOrderCO/HVM3/blob/main/MODES.md). Harris and Singh found
  parallelism in lazy Haskell only by running thunks ["as soon as they are
  allocated"](https://timharris.uk/papers/2007-fdip.pdf#page=1). Normalizing linear lambda terms is
  [P-complete](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/E8F3718983561B265BB6BAECC1974DDB/S0956796804005131a.pdf/functional-pearl-linear-lambda-calculus-and-ptime-completeness.pdf#page=1),
  so some nets have no parallel shortcut at all.
- **Fork** is branch-first evaluation at one rule. Jay warns that branch-first ["may fail on an
  argument that is not
  required"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=80) when
  that argument diverges; erasers should cover this, but it wants a test. Optimistic evaluation
  ([Ennals and Peyton
  Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/icfp2003.pdf#page=3)) bounds
  wasted speculation with a switch per site, a depth limit and an abort that pauses rather than
  discards, so that in the worst case "the entire program runs lazily". GHC's speculative sparks
  need [WEAK
  retention](https://www.eecs.northwestern.edu/~clk800/rand-test-study/_rsfmh/rsfmh-2009-10-8-12-02-00.pdf#page=2)
  so unneeded work can be collected. **Inference:** erasers give fork exactly that.
- **Superpositions.** HVM3's [interaction
  calculus](https://github.com/HigherOrderCO/HVM3/blob/main/IC.md) erases both outputs of a
  duplicator at once and has no rule for erasing one. That gap is why our eraser hands the whole
  superposition to the surviving copy. HVM1 clones [layer by
  layer](https://github.com/HigherOrderCO/HVM1/blob/master/guide/HOW.md) on demand, which is the fix
  for "a labelled duplicator forces what it copies". Taelin warns that rules firing on a secondary
  port ["might break strong
  confluence"](https://gist.github.com/VictorTaelin/93c327e5b4e752b744d7798687977f8a#user-content-edit-simpler-implementation-based-on-oportunistic-swaps); "a
  duplicator touched by an eraser becomes a wire" is such a rule.
- **Tree calculus** evaluators listed at
  [treecalcul.us](https://treecalcul.us/implementation/) are all ordinary ones. Its OCaml
  evaluator orders the S rule's arguments as disp does; Jay's book uses the other order.

## 5. Sara Achour and Unconventional AI

Related in spirit, not in substance. Achour (Stanford; cofounder of [Unconventional
AI](https://unconv.ai/blog/author/sara-achour/)) builds compilers and languages for analog
hardware. Her thesis compiles continuous dynamical systems onto [reconfigurable analog
devices](https://dspace.mit.edu/handle/1721.1/140167), coping with noise, limited operating range
and device variation. Ark (ASPLOS 2024) is a language for describing analog computing styles,
checked by simulating their differential equations. One case study is a cellular nonlinear
network, an analog cellular automaton of ["locally interconnected
cells"](https://arxiv.org/pdf/2309.08774#page=9). Unconventional AI's posts run neural networks
on physical dynamics: one trains [a hexagonal grid of gyroscopes joined by
springs](https://unconv.ai/blog/machine-learning-with-dynamics/) to classify pen strokes, on the
view that ["anything with interesting dynamics can be used as a
computer"](https://unconv.ai/blog/machine-learning-with-dynamics/).

Shared with the strand lattice: computing with local physics instead of addresses, and
treating hardware non-idealities as part of the design. Different: everything is continuous
dynamics on a fixed wiring, trained or compiled. Nothing moves, rewrites, or runs a discrete
program.

**Inference.** Two pieces carry over:
- The demand field is the kind of thing an analog cell array computes well. The README already
  notes it is a diode-OR mesh.
- Ark's habit of adding one non-ideality at a time and measuring its effect fits the chip work:
  RNG quality, and timing across tile boundaries.

## 6. Searched for and not found

- Lattice and CA work: "spatial" or "geometric" interaction nets, interaction nets on a grid,
  interaction combinators on a lattice, graph reduction or SKI combinators in a cellular automaton.
  Nothing beyond ia2d and section 3. Every lambda or Lisp in Game of Life emulates a stored-program
  machine with addresses. No artificial chemistry pairs short lattice bonds with general
  functional computation. Nothing lambda-like runs on the Movable Feast Machine.
- Hardware: no peer-reviewed FPGA or ASIC interaction-net processor, and no energy-per-rewrite
  figure for any interaction-net system. No older machine moved graph nodes (as opposed to tasks)
  toward their partners.
- Theory: no interaction-net encoding of tree calculus, no cost model for lambda calculus or
  interaction nets that charges for distance, and no measurements of how well real reduction nets
  cut in half (separator size).
- Not searchable: Discord (HigherOrderCO, Vine), Japanese- and French-language venues,
  unindexed theses, and Tendrils' private design.

## 7. Changes the literature suggests

All are untested inferences. They are ordered by how directly they attack the measured cost:
about 16 clocks per rewrite, mostly a walker closing a gap of 2–3 strands.

| change | basis | expected effect | cost |
|---|---|---|---|
| **Ratcheted zip**: when a block holds the end of a demanded principal wire and has room, always shorten that wire, without Metropolis sampling | Brownian ratchets (section 3); a lone walker's search time [grows quadratically](https://arxiv.org/pdf/2205.10735#page=8) with distance | gaps close straight instead of by chance; a rough guess is 16 → 4–6 clocks per rewrite | a priority rule; two ratcheted walkers can deadlock, so keep everything else thermal |
| **Pulses and field step every clock**, whatever the Margolus offset | CAM-8 moves data separately from block updates ([Margolus](https://arxiv.org/pdf/comp-gas/9509001#page=6)) | pulses never wait for the block alignment | tens of gates a site; skip if the RTL already does this |
| **Local heating near a stuck rewrite** instead of pressure | active particles ["accumulate where they move more slowly"](https://arxiv.org/pdf/1406.3533#page=1) | explains why the pressure field failed (it pulled matter into jams); heat spreads matter away from a stuck rewrite instead | 0–2 bits a site |
| **A projector agent** for a labelled duplicator with one output erased, and labelled duplicators that copy only when read | IC's missing rule; HVM1's lazy clone (section 4) | fixes both open superposition problems in the README | one agent kind, about 3 rules |
| **Bound fork** as optimistic evaluation does: a depth count in the pulse, stale speculation paused rather than erased, real demand outranking speculative demand | Ennals and Peyton Jones (section 4) | worst case falls back to lazy while keeping the 4.2× | a few bits a pulse |
| **Regression tests**: Jay's diverging-argument example under fork; random firing orders for confluence; count leaked agents at exit | Jay; Taelin's confluence warning | catches completeness, confluence and leak bugs | tests only |
| **Pick the dimension from the measured separator size**, or try a "2.5D" slab: agents on a few planes, extra layers only for wire | [Matsubayashi](https://arxiv.org/pdf/1402.7293#page=1) | predicts when a fixed number of strands a link jams | measuring snapshots |
| **Set tension against ln μ**, where μ counts the ways a wire can continue | the simple cubic lattice has [μ ≈ 4.684](https://arxiv.org/pdf/1302.2106#page=1) | swelling starts between ln 4.684 ≈ 1.54 and ln 5 ≈ 1.61, not at ln 5 | free |
| **Check that moves can reach every wire shape**; weight proposals toward wire ends | local fixed-length moves [can be nonergodic](https://arxiv.org/pdf/hep-lat/9405016#page=40) (unable to reach some shapes); worm updates have [near-zero critical slowing](https://arxiv.org/pdf/cond-mat/0103146#page=1) | no frozen shapes when links are full | 0–1 rule |
| **Test the hashed per-site random numbers** | [Random123](https://users.cs.utah.edu/~hari/teaching/bigdata/random123sc11.pdf#page=1) passes BigCrush | rules out hidden bias in moves | tests only |

**Why 2D jams** (inference from Matsubayashi). With at most L strands a link, a straight cut
across a 2D lattice k sites wide carries at most L·k wires. A net that keeps splitting into halves
joined by many wires then needs strands a link to grow like log N in 2D, but only a constant in
3D. Theory also says that forcing congestion down can force wire length up, so tension and
crowding cannot both be minimised. The energy weights are a real trade-off, not a tuning bug.

**Numbers worth publishing**, since nobody has them:
- clocks per rewrite, split into walking and the rest;
- rewrites per clock against the Reduceron's ["0.55 hand-reductions per
  clock-cycle"](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/9818E081664ADAFE9F61F1AEDAD0B043/S0956796812000214a.pdf/the-reduceron-reconfigured-and-re-evaluated.pdf#page=2)
  (an FPGA graph reducer);
- separator size at the jam point;
- energy per rewrite on the 130 nm process.

**Why sharing equal computations failed** (`memo=r`, `share=N`; inference). Interaction nets share
work only when the sharing is built at creation. Finding equal parts afterwards needs global
identity, which a lattice without addresses lacks by design. If it is tried again, a hash of a
value's children carried on each L/S/F agent would make equality a local check. No prior art
for this was found.
