# Signal fabrics: a self-configuring circuit for disp, costed in energy, time and error

Design exploration with a model and simulations, 2026-10-28. Code in `research/signal-fabric/`
(`model.mjs` the cost model, `sim.mjs` the Monte Carlo checks and the fault-routing simulation, `sweep.mjs`
the design-space sweep, `explorer.html` the results with the model live, `netlist.ts` disp's two-way
programs compiled to fixed circuits, `fabric.html` those circuits laid out and run cell by cell,
`tree.html` the programs run as signals over the value's own tree).
Nothing here is built in hardware; every number is a first-order model calibrated where a published
figure exists (Sources).

The question: the strand lattice (`evaluators/rust-ic-linear-strands`) moves data as matter, agents
walking and rewiring, and is far slower than a circuit. Is there a design closer to a circuit that
reconfigures itself, carries signals that may be analog and may err, and makes the trade between speed,
energy and error explicit, as a physical implementation would?

## Summary

- **Where the lattice sits.** Against standard cells at their best, the strand lattice takes about
  2,800 times the energy and 60 to 6,000 times the time per run (per primitive move: 2–3 rewrites of 3–28
  clocks, a clock being a whole site update). An FPGA takes [14 times the dynamic power, 3.4–4.6 times
  the delay and 35 times the area](https://www.eecg.toronto.edu/~jayar/pubs/kuon/kuontcad06.pdf#page=12)
  (measured at 90 nm, logic only). The
  lattice's cost is structural: a bit is a node, so moving a bit means moving matter. No tuning closes it.
- **What fails at low voltage is the devices, not the signals.** Threshold mismatch is about 12 times
  thermal noise at every size (both shrink as 1/√C: [noise variance kT/C](https://arxiv.org/pdf/1908.08099#page=2),
  [mismatch against the square root of the inverse area](https://class.ece.iastate.edu/djchen/ee501/2015/MOS%20TransistorMatching%20pelgrom89.pdf#page=5)), so near threshold some gates come out wrong for
  good, while transient thermal errors stay negligible until the margin is gone. A fixed chip must
  yield, so it runs with margin (0.8 V here for a 10⁷-site chip). A fabric that tests its cells and
  routes around the faulty ones can run at 0.38 V: routing around 3% faulty cells costs 9% more wire,
  10% faulty 35% (simulated); a fixed 400-gate circuit with 1% faulty sites works 2% of the time.
- **So a self-configuring cell array costs about 1.7 times standard cells' energy**, not an FPGA's 14,
  despite its cells being three times a gate: the low supply pays for the generality. It is 6–8 times
  slower at its fastest.
- **Tolerating errors buys little in digital logic.** Error tails are Gaussian and steep. The one
  transient error worth trading is timing (supply noise moving delays, steeply near threshold):
  catching late signals and replaying them shrinks the guardband from 1.9× to 1.3× at 0.4 V, a 1.4×
  speed-up; at nominal supply it is worth 10%.
- **Tolerating errors buys a lot when the value is analog.** An analog value needs about four times the
  energy per extra bit above threshold (noise must halve; [power grows as SNR in subthreshold and SNR²
  above](https://gwern.net/doc/psychology/neuroscience/1998-sarpeshkar.pdf#page=14)), stochastic
  bitstreams [exponentially longer streams](https://homes.cs.washington.edu/~armin/ACM_TECS_2013.pdf#page=2);
  digital grows [logarithmically](https://gwern.net/doc/psychology/neuroscience/1998-sarpeshkar.pdf#page=17).
  Where analog stops winning depends on what surrounds it: converting every result back to digital (an
  ADC at 10 fJ a step) with 30 times the kT floor in overhead, the crossover is about 4 bits; keeping
  values analog from stage to stage near the floor (3 times), it is about 8 bits, which is
  [Sarpeshkar's figure](https://gwern.net/doc/psychology/neuroscience/1998-sarpeshkar.pdf#page=15). So
  values should stay analog across stages and be [restored to discrete levels](https://gwern.net/doc/psychology/neuroscience/1998-sarpeshkar.pdf#page=21)
  only every few, not converted after each.
- **Energy recovery is the one lever below standard cells**, and it needs reversible programs. Adiabatic
  cells (a slow ramped power clock recovering most of C·V²: a ramp of time T loses
  [less than a step by a factor of 2RC/T](https://web.archive.org/web/20220308013237id_/https://dspace.mit.edu/bitstream/handle/1721.1/11620/32053038-MIT.pdf#page=20))
  trade time for energy along a smooth
  curve: the 32-bit add matches standard cells' least energy at about 19 times their time, is 1.7
  times lower at 40 times and 2.4 times lower at 130 times; the shallow 64-bit NOT is 7 times lower at
  0.8 ns. A function that is not a bijection must erase what it narrows, at C·V²/2 a bit (the floor
  beneath is Landauer's [0.6931 kT per restored bit](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=5),
  far below what circuits pay). disp's two-way programs are bijections: they get the whole saving, and
  running a computation back (flip) becomes reversing the power clock's phase order, and an error check
  ([inverse and compare](https://arxiv.org/pdf/1101.4222#page=1)).
- **Configuration is worth writing only for code that runs often.** The array writing its own
  configuration locally costs about 6 pJ for a 32-bit adder's region (plus 0.8 pJ testing it): 20,000
  times cheaper than an FPGA loading a bitstream from off chip, but still hundreds of runs' worth. The
  lattice is the extreme of reconfiguring for every single move.

## The design this points to

A **self-repairing reversible cell fabric with two signal tiers and a slow configuration layer**:

- **Cells**: a grid of identical bit-level cells, each a small reversible gate (a controlled swap, as in
  [conservative logic's Fredkin gate](https://cqi.inf.usi.ch/qic/82_Fredkin.pdf#page=5), a Toffoli,
  wire permutations) with neighbour routing, configured by a few dozen bits. Levels are
  restored in every cell (restoring is cheap and makes errors fall off a cliff).
- **Supply and repair**: the fabric runs near threshold. At set-up it tests every cell (64 test
  operations each: for the adder's region about 0.8 pJ, some 40 runs' worth, once) and configures
  routes around the faulty ones; it can re-test as it ages. This is what makes the low supply
  buildable.
- **Energy recovery**: regions running reversible code are clocked adiabatically. The ramp time is the
  knob, chosen per region: short ramps run near standard cells' energy at tens of times their time,
  long ramps several times below it at a hundred times. `flip` is the power clock's phases taken in
  the other order.
- **Values where precision is low**: tiles that carry numbers as analog charge or current (or
  stochastic streams) for work that tolerates 2–4 bits, their amplifier offsets trimmed by the
  configuration layer (trimming is configuration).
- **Errors**: faults are routed around; timing errors are caught late and replayed; anything else in a
  reversible region is caught by running it back and comparing (twice the work, only where wanted).
- **Configuration layer**: configuration travels cell to cell as a stream that each cell takes its bits
  from (cells can [read and write other cells' configuration information](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=4),
  as in the Cell Matrix), so a program held as data becomes a
  circuit by a local wave, without a trip off chip. It is written when code will run many times; code
  run once runs on a small configured interpreter instead.

How disp maps onto it:

- **A two-way program on words of fixed shape compiles to a fixed circuit, and the moves are wiring.**
  `netlist.ts` runs core.ts's semantics on a word whose bits are unknowns, splitting wherever the
  program looks at a bit's shape and merging the branches into gates (conditions as reduced decision
  diagrams, so impossible branches drop out exactly). On the library, every shape comes out fixed and
  no input gets stuck:

  | program | word | gates | depth |
  |---|---|---|---|
  | `notw` | 64 bits | 64 inverters | 1 |
  | `cnotw` | 8 pairs | 8 XOR (an inverter and a multiplexer each) | 2 |
  | `addw` | 4 bits | 25 | 8 |
  | `addw` | 8 bits | 61 (60 multiplexers, an inverter) | 16 |
  | `mirror` | 4 leaves | 0: wiring only | 0 |

  `addw` grows by about 8 gates and 2 levels a bit, a ripple adder; swap, cross, sink, stem and unit
  on a known shape cost nothing. As a reversible circuit the same gates become controlled swaps and
  Toffolis (not yet done: these counts are of the function, in 2:1 multiplexers).
- **Recursion becomes reuse in time or copies in space.** A recursion over the data's shape (map with
  self) is laid out once per data node, like the linear net's unrolling; a recursion of data-dependent
  depth reuses one configured region over successive passes (a loop), not new matter per call.
- **Programs as data stay cheap**: `eval` is a configuration wave over the program tree, each node
  becoming the gate its shape names (a form reads at most two or three levels down).
- **The strand lattice's role shrinks to the configuration layer**: rewriting structure, rarely,
  rather than carrying every signal.

## The model (research/signal-fabric/model.mjs)

- **Device**: one transistor curve, exponential below threshold and about square-law above
  (EKV-style), calibrated so a gate takes 15 ps at 1 V with 1 fF; Ion/Ioff 1.8·10⁵. Switching energy
  C·V²; leakage over the cycle. A 32-bit ripple add costs 112 fJ at 1 V, against
  [0.1 pJ at 45 nm, 0.9 V](https://gwern.net/doc/cs/hardware/2014-horowitz-2.pdf#page=5). The
  minimum-energy supply comes out at 0.2–0.3 V, below the 0.35 V threshold, as the
  [minimum energy point occurs below the threshold](https://web.archive.org/web/20260103042007id_/https://ee222-winter18-01.courses.soe.ucsc.edu/system/files/attachments/SubthresholdLogic_MIT_2005.pdf#page=2).
  A hop's 0.5 fF at a 2 µm pitch is about 160 fJ a bit a millimetre at 0.8 V, near the
  [240 fJ/bit/mm measured at 40 nm](https://www.cs.toronto.edu/~pekhimenko/courses/csc2231-f17/Papers/GPUs.pdf#page=3).
- **Noise and faults**: thermal noise √(kT/C) per node; threshold mismatch σ ∝ 1/√C (25 mV at 1 fF).
  A gate's noise margin is V/2 less 2·n·φt. Transient error per operation Q(margin/thermal); a
  gate is built faulty with chance 2Q(margin/mismatch).
- **Timing**: supply noise (1.5% of the supply) moves every gate on a path together; the clock's
  guardband is exp(Q⁻¹(allowed)·slope·σ), the slope being d ln(delay)/dV.
- **Designs**: standard cells; an FPGA as the published FPGA/ASIC ratios with an off-chip bitstream;
  the cell array (a cell three gates of energy, two of delay, neighbour hops, 32 configuration bits);
  the array with low-swing transport (sense amplifiers every k hops, offsets trimmed or not); adiabatic
  cells (loss max(2·delay/ramp, 5% clock loss)·C·V², erasure C·V²/2 a bit for non-bijections, four
  phases a stage); analog multiply-accumulate (capacitance for b bits against thermal noise and,
  untrimmed, mismatch; 30 times the kT floor in overhead; an ADC at 10 fJ a conversion step); stochastic
  streams (4^b bits, a generator per input); race logic; the strand lattice.
- **Fixes**: triple voting (von Neumann's [multiplexing](https://www.dna.caltech.edu/courses/cs191/paperscs191/VonNeumann56.pdf#page=28)
  in its simplest form), replaying late signals (shadow latches, 10% energy), running a bijection back
  and comparing (twice the work).
- **Constraints in the sweep**: wrong at most once in 10¹² runs; a fixed chip must yield 90% over 10⁷
  sites; an array is buildable while at most 15% of its cells are faulty (beyond about 20% routes stop
  existing).

What it leaves out: wire delay over long distances, process corners and temperature, aging, soft errors
from radiation, the area cost of the configuration memory in leakage, and the power clock's own
generator. Every ratio above is within a model; the published anchors are the FPGA ratios, the 45 nm
energies and the minimum-energy point, and the model lands near each.

## Results

Each design at its least-energy point, against standard cells at theirs (`node sweep.mjs`):

| workload | FPGA | cell array | adiabatic array | strand lattice |
|---|---|---|---|---|
| notw, 64 bits | 14× | 1.74× | 0.06× | 2,600× |
| add, 32 bits | 14× | 1.76× | 0.41× | 2,800× |
| multiply, 16 bits | 14× | 1.72× | 0.44× | 2,800× |
| dot product, 64 × 8 bits | 14× | 1.76× | 0.68× | 2,900× |

Multiply-accumulate, energy each, best supply and size:

| bits | digital | analog, trimmed | analog, untrimmed | stochastic |
|---|---|---|---|---|
| 2 | 4.8 fJ | 5.1 fJ | 5.9 fJ | 0.4 fJ |
| 4 | 17 fJ | 22 fJ | 34 fJ | 9.8 fJ |
| 6 | 37 fJ | 117 fJ | 301 fJ | 210 fJ |
| 8 | 65 fJ | 906 fJ | 3.9 pJ | 4.2 pJ |
| 10 | 101 fJ | 10.7 pJ | 58 pJ | 81 pJ |

Self-repair (`node sim.mjs`): a 400-gate pipeline on a 48 × 48 array, routed by shortest path through
good cells, 20 arrays each:

| faulty cells | routed | hops per net | a fixed 400-gate circuit works |
|---|---|---|---|
| 0.1% | 20/20 | 1.00 | 67% |
| 1% | 20/20 | 1.03 | 1.8% |
| 3% | 20/20 | 1.09 | 5·10⁻⁶ |
| 10% | 18/20 | 1.35 | 5·10⁻¹⁹ |
| 20% | 7/20 | 1.79 | – |
| 30% | 0/20 | – | – |

## The cell-level player (`fabric.html`, code in `fabric/`)

A two-way program laid out and run cell by cell. `fabric/circuit.ts` takes the symbolic run's decision
diagrams (`symbolic.ts`, shared with `netlist.ts`) and makes each node that is not a bare input a gate
reading its input bit and the one or two nodes under it (recognising xor, and, or). `fabric/layout.ts`
places the gates level by level, left to right, and routes the nets with negotiated congestion
(PathFinder) on a grid where each cell has one link to and one from each neighbour, as in the Cell
Matrix, leaving out the cells the self-test found faulty. `fabric/sim.ts` gives every cell its own
threshold offsets (the model's mismatch), so lowering the supply makes cells faulty, stuck at a value,
and slows the near-faulty ones; it follows each link's value and settling time, counts heat with the
model's numbers, ramps levels up and back down for adiabatic clocking, and checks each node against its
driver on the way down. `fabric/check.mts` checks it: every example matches the two-way interpreter on
every input tried, self-tested chips at 0.30 V are always right, untested ones go wrong (6 of 6 chips
for the 8-bit adder), and every injected noise flip is caught by flipping back.

What it shows that the formulas did not, on the 8-bit adder (34 gates, 15 levels):

- **Wires, not gates, are the cost**: about 900 wire cells for 34 gates, 26 a gate. Each input bit
  feeds gates at many levels, so it crosses most of the array.
- **The least energy is a static clock near threshold**: 57 fJ and 240 ns a run at 0.35 V, against
  187 fJ and 3 ns at 0.8 V. Below that, leakage over the slower run wins (87 fJ at 0.28 V).
- **Adiabatic clocking barely pays here**: 109 fJ at 0.8 V, 40 times slower, and worse at lower
  supplies. Each ramp must wait for the longest wire in its level to settle, and every configured cell
  leaks meanwhile. Adiabatic regions need short wires: pipelined into stages a few hops long, or
  placement that keeps each level's wires local.

## The signal tree (`tree.html`, code in `tree/`)

The fabric above places and routes gates from the outside. The signal tree runs a program with no
gates and no planned wiring. The value is a tree laid out on the lattice, one node a cell, each edge a
wire. The program travels as signals: a signal carrying a program fragment moves a cell a tick down a
wire, and where it reaches a node the fragment reduces there by core.ts's rules. `map` sends a signal
down every wire at once (the parallel active points), `tri` reads the left child and picks, and a node
that is done signals back up. Every change to the tree is a rule: a little tree whose loose ends are
holes (parts passed along unread) and the same nodes and holes rewired. The five moves and tri's two
rewrites (taking a stem off its left part and putting it back; rotating) are the machine's own rules.
Meanwhile matter relaxes: each tick
a node may step to a free neighbouring cell where its wires pull less (each wire wants its child two rows
under its parent). A small value (four nodes or fewer: a bit, a carry, a status) that is read climbs
its wire to its reader. A node never sends a fragment that does nothing (`map(id, swap, id)` touches
only one wire). `tree/engine.ts` simulates it event by event: a signal takes a tick per cell of wire, a
rewrite one tick, a read of a child a round trip. `tree/check.mts` runs every example forward and then
flipped back against the interpreter. The tables in this part are from the first version, which took
shortcuts no lattice could (the cheap physics; see **Honest physics** below for what they cost).

| program, same input | strand lattice (linear net) | signal tree (cheap) |
|---|---|---|
| `not` on a bit | 281 clocks | 29 ticks |
| `mirror`, 4 leaves | 551 | 27 |
| `notw`, 8 bits | 1,183 | 59 |
| `cnotw`, 8 bit pairs | 1,414 | 82 |
| `addw`, 2 bits | jams (two agents wait on each other) | 1,514 |

A clock and a tick are both one local update of the lattice, so the signal tree is 10 to 20 times
faster on these (the table is without learned rules), and the gap grows with size: on the strand lattice
every value and program agent walks.

**Learned rules.** A fragment that reads and makes at most 16 nodes is kept, when it finishes, as a rule
of its own: its left side is the nodes it read as they were, with what it never read as holes; its right
side is what it left there. A fragment counts every node its sub-fragments read, so `ann` at a word node
(which reads two statuses and passes the halves along) becomes one rule, and `up` on a whole word does
not. When the fragment reaches a node again, in the same run or one on other bits, the node reads what
the fragment's rules need (a round trip to the farthest node read) and, if one fits, rewrites in one
step (one tick under the cheap physics). `b†` run forward shares its rules with `b` run backward. The rules are sound because a fragment's
moves depend only on the kinds of the nodes it read; they are linear (each hole appears once on each
side), as reversibility needs. The machine's own rules are written the same way, so one rewrite
mechanism runs both (`MOVES` and `Net.rewrite` in `tree/engine.ts`).

**Layout.** Rows is the tidy tree. Packed gives each part its own share of a square, halves side by side
and their halves one above the other (an H-tree), so a signal from the root reaches any bit in about √n
cells; relaxing then just shortens wires.

`addw` under the cheap physics, ticks (`tree/scale.mts cheap`; other bits: mean of three runs with the
rules learned so far):

| bits | rows, no rules | rows, first | rows, other bits | packed, first | packed, other bits |
|---|---|---|---|---|---|
| 4 | 2,975 | 1,539 | 1,298 | 1,600 | 1,256 |
| 8 | 4,470 | 2,398 | 1,746 | 2,278 | 1,434 |
| 16 | 8,197 | 3,336 | 3,129 | 3,082 | 2,283 |
| 32 | 18,104 | 5,732 | 5,486 | 4,578 | 3,449 |

The first run already gains, since a rule learned at one bit fires at the next. On other bits a run
does about 100 own-rule rewrites and 140 learned ones (8 bits) where it did 2,700 moves. What is left is
mostly signals travelling: with every wire one cell long (a bound no layout reaches) later runs on 16
bits take about 660 ticks against 2,283 packed. The rest of the gap is the program's passes: `addw` goes
over the word nine times in a row (prep, up, down, sumz, negb, and back), and each pass waits for the
last one's done signals to come back to the root.

**Honest physics.** The numbers above lean on shortcuts: one rule book that every node reads at once, a
signal of any size costing only its wire's length, a 16-node rewrite landing in one tick however far its
nodes are, and wires drawn fresh from their two ends whenever either moves (so a node's whole wire moved
with it, and a node knew where its far ends were). `Physics` in `tree/engine.ts` now has a field for
each, and `HONEST` charges for all of them:
- **Rules carried**: a learned rule rides down a wire with the program's signal the first time a piece
  that could use it goes that way, and the node at the end keeps it; rules learned below come back up
  with done signals. (The alternatives: `cell`, a rule stays in the cell it was learned in; `shared`,
  the old book, kept to measure.)
- **Sizes**: a signal takes its wire's length plus its size in symbols, one signal at a time each way on a
  wire. A library name is one symbol, since every cell holds the library as it holds the machine's own
  rules; without that, `addw` alone is 4,354 symbols. A done signal also carries the record of what was
  read, while the piece above is still being learned.
- **Writes**: a rewrite waits until its writes reach the farthest node it changes; a lookup's answers
  share wires on the way back, one a tick.
- **Wires of cells**: a wire is a path of cells. A node sees only the first cell of each of its wires and
  steps where more of them lead than not (dragging its end of each one cell); a rewrite splices new wires
  along the old ones; doublings-back and 3-cell detours (both within a 2×2 block) straighten a few cells
  a tick. A node's step and a climb keep to 4 wires through a cell.

`CHEAP` reproduces the old numbers exactly (`tree/check.mts cheap`). At 16 bits, packed (`tree/ablate.mts`):

| physics | no rules | runs on other bits |
|---|---|---|
| cheap | 7,049 | 2,107 |
| cheap, plus sizes | 9,332 | 2,368 |
| cheap, plus writes | 8,141 | 2,638 |
| cheap, plus wires of cells | 10,048 | 3,507 |
| honest, but one shared book (not local) | 13,434 | 4,303 |
| honest, rules kept in their cell | 13,434 | 12,521 |
| honest (rules carried) | 13,434 | 42,531 |

And from 4 to 32 bits, packed (`tree/scale.mts honest,cell,shared packed`):

| bits | no rules | carried: first | carried: other bits | in cells: other bits | shared book: other bits |
|---|---|---|---|---|---|
| 4 | 6,830 | 16,426 | 20,926 | 4,233 | 1,850 |
| 8 | 11,402 | 25,069 | 31,654 | 7,417 | 2,783 |
| 16 | 16,898 | 30,230 | 43,705 | 11,792 | 5,044 |
| 32 | 29,020 | 38,375 | 51,373 | 21,731 | 7,349 |
| growth, 8 → 32 | n^0.67 | n^0.31 | n^0.35 | n^0.78 | n^0.70 |

What this says:
- Without rules, honest costs double the time at 32 bits, and the H-tree still sets the growth: n^0.67
  against the cheap n^0.61, with n^0.5 the least any 2D layout reached from one root can do.
- The 4× from learned rules was mostly the shared book. Carried, the rules cost more than they save at
  these sizes: about 150 rules (2,400 symbols) go down from the root, and every hop stores the whole
  signal before passing it on. That cost hardly grows with the word, so carried rules would pass no
  rules somewhere past 32 bits (in rows they already do on the first run at 32 bits: 44,226 against
  51,038). Sending every relevant rule with every piece, instead of once a wire, was far worse (46,127
  ticks on later 4-bit runs against 20,781).
- Rules kept in their cell are honest and cheap, but each cell sees only its own bit patterns, and
  matter moving shifts nodes into other cells: 1.3× at 32 bits.
- Still not charged: a rewrite's splices are not capped at 4 wires a cell, and splices along the same old
  wires bundle them (up to 75 through one cell at 32 bits); a node's step runs one node at a time within
  a tick; and with rules carried, a cell keeps up to about 2,400 symbols of rules.

**The frame (`frame/`, in Bend).** The tick is only a fair score if what a tick allows is fixed before a
design is, so the physics and the laws now live apart from any one design, written in Bend (a dependently
typed, affine language whose checker proves laws; checkout at `~/Repos/bend2`):
- `frame/lattice.bend`, the physics: a grid of cells all updated at once, each from itself and its four
  neighbours by one rule of type `Nb<C> -> C`. It is the only place a grid changes, so locality is the
  rule's type rather than a law to prove.
- `frame/twoway.bend`, the reference: a port of core.ts's interpreter (without eval), with fuel.
- `frame/swapnet.bend`, a first design: the tree in rows, wires as chains of cells, and programs made of
  swap, map, `;` and † sent in at the top as packets (a path, then X), each node passing a packet on or
  eating it with a latch. Packets keep their order on every wire, so the later swaps find the earlier ones
  done and no done signals are needed.
- `frame/LAWS.bend` (a draft for the human to edit) and `frame/PROOF.bend`: the Bend port answers as core.ts
  does on 6,052 cases (`frame/cases.mts` writes them from core.ts: library programs and swapnet's, both
  ways, every tree of at most 7 nodes); swap, cross and sink each undo themselves on every tree; running
  backward undoes running forward on every case; no tick makes, removes or changes the kind of a node or
  wire; and swapnet gives the reference's answer for all 21 of its programs on all 89 trees. A design that
  sends both paths of a fork the same way, or lets a stem pass a fork's path on, fails `sound_small`; with
  trees of at most 5 nodes the first slipped through, since those trees look the same either way round.
  Reversibility is written there as a candidate law, not a law: swapnet consumes a packet's X when it acts.
- `frame.html`, the viewer: `frame/view.bend` names what a page may call (lay a run out, tick, still,
  what the grid reads as, the design's answer and the reference's), Bend builds it into an ES module, and
  the page draws whatever cells come back, tick by tick, with LAWS.bend and the last proof run beside it. So
  the page shows the design as Bend runs it, not a copy. The loop is: edit a law, then the design, run
  `frame/check.sh`, and look; the page reloads itself when the check finishes.
- `frame/score.bend`: swapnet mirroring a full tree takes 1, 8, 24, 63, 157 ticks for 2 to 32 leaves,
  nearly all of it the program's 1 to 129 symbols going in at the top one a tick. That is the next thing
  for it to fix: one `map` packet copied at each fork instead of a packet per swap.
- `frame/loopnet.bend`, the second design and the one the laws now check: swapnet plus stems made and
  taken away, loops (enough for `add` and `add†`), recursion copied at forks, and chains that pack
  themselves. A stem is made out of the first plain wire cell its Sp reaches, and Sm turns one back into
  wire; where a chain has no plain wire left, its leaf grows into the empty cell past it (a stem anywhere in
  a chain being the same as one at its foot). A still plain wire just above a still free stem trades places
  with it, so stems float up and slack sinks to each chain's foot without changing what the grid reads as.
  No tick turns matter into Void (`matter_stays`, proved for every neighbourhood; it replaces swapnet's
  `keeps_matter`, since kinds now change, and the first version's `keeps_cells`, since leaves now grow).
  A loop (`fix(tri(a, j ; self ; k, id))`) is run by the node it reaches: a walker goes down its left wire
  and holds each stem (tri's lowering), so the recursion's stack is the left part itself; every wire has a
  second lane going up, and each level's burst (j going down, a at the bottom, k coming back) goes up it to
  the loop's node, which runs it. If a burst goes into the left part the node answers it with R down and
  the level waits for it; if none does (add's), nothing waits. `fix(map(x, y, z))` and `fix(map(x, y, z) ;
  swap)` with x, y, z each self or id go in as one symbol that a fork sends down both wires at once.
  `loops_small` checks 16 loop and stem programs on 21 trees, and the copied recursions (the library's
  `mirror` among them) are in `sound_small`'s 27 programs.
  **Binary add.** A fork runs what reaches it in order: while a loop of its own is out (from its Tri until
  the loop's last E comes back up) what comes from above waits in its queue, so steps can follow a loop,
  and a fast loop's burst may start a loop of its own further down. Any other `fix(b)` whose packets all
  go into a fork's halves goes in as one symbol carrying b; the fork it reaches queues b, each `self` in
  it sent on down as the same symbol. `not` on a bit (0 is △, 1 is △△) goes in as one symbol: the wire
  just above a leaf becomes its stem, or a stem turns back into wire. With these, `frame/binary.ts`'s
  `addb` runs: ripple-carry binary add on a list of positions (carry slot, a's bit, b's bit), lowest bit
  first, made of `tri(id, second(f), id)` (f when the left bit is 1), `not`, and a fix that recurs down
  the list; each position puts majority(c, a, b) in the next carry slot, the positions above add, the
  carry is taken back and b becomes its bit of a + b. The input has its carry slots already (no
  `unit`). `binary_small` checks it and nine smaller binary programs on bits and every pair of 1- and
  2-bit numbers; `cases.mts` first checks that `addb` adds (the laws only hold the design to the
  reference). 2^n − 1 + 1 takes 71, 116, 260, 692 ticks for n = 1, 2, 4, 8 bits, against 1,794 for
  255 + 1 in stems; it grows faster than linear because the rows layout puts each fork over the middle
  of its part, so the top positions' bits sit far from their forks.
  **Tree add.** On a list every bit's right part holds every later bit, so testing a bit and flipping
  something to its right (all `addb` does) reaches whatever the carry needs. In a balanced tree a bit's
  right part is only its partner, and a program run inside a part cannot reach outside it; the moves
  that carry anything from one part to another are cross, sink and tri's fork case (on ((l, r), x) it
  runs as on (l, (r, x)), a rotation). `frame/binary.ts`'s `addt` adds the library's words (<a, b>, high
  half left) with only tri's fork case past what `addb` uses: one carry bit sits beside the word and
  rides through it, lowest bit first and back, like
  [Cuccaro et al.'s ripple adder](https://arxiv.org/pdf/quant-ph/0410184#page=3) with the carry moving
  instead of the wires (no slots, unit, cross or copying); `cases.mts` checks that it adds every pair of
  1-, 2- and 4-bit words. The library's `addw` (carry-lookahead) needs unit, cross (rot, xch,
  transpose), tri's fork case and copying (dup, for each part's status). For loopnet to run `addt` it
  needs tri's fork case, and bursts that act on their loop's own node (the bit step swaps the bit pair
  and the carry there). In each rotation `addt` makes, one of the three parts is the carry bit, so the
  rotation can be a hop: the bit's fork is taken out where it is and a new fork holding the bit is put
  into the next wire, which needs forks made and taken away (unit's mechanism).
  What it does not do: c other than id, forks or leaves made (unit), growth past a fork (a chain that
  ends in one still needs slack left over it), leaves growing into the same cell (the rows layout gives
  each its own column, so it never comes up), and giving space back to Void. The score: mirror copied at
  forks takes 5, 15, 43 ticks for 2, 8, 32 leaves (swapnet's unrolled packets: 2, 25, 158); `add` on
  (n, n) takes 15, 23, 39, 71, 135 ticks for n = 1 to 16, and `add†` back 12 to 72, both about linear
  (the first version, every level waiting on a round trip: 57 to 3582).

**The block frame (`block/`, in Bend).** A second frame, in which reversibility is part of the physics and nothing is
checked against core.ts. The hierarchy: a rule on 2×2 blocks whose cut shifts each tick (Margolus's neighbourhood);
a reading from the grid to a tree; the two-way calculus as the spec's own definition (`block/calculus.bend`).
Reversibility of a general 2D automaton is
[undecidable (Kari, as restated by Toffoli and Margolus)](https://people.csail.mit.edu/nhm/ica.pdf#page=8), but a block
automaton is reversible [exactly when its block rule is a permutation](https://dmtcs.episciences.org/articles/2297/download#page=3),
and in 1D and 2D [every reversible automaton can be written as block permutations](https://arxiv.org/pdf/1201.5529#page=1).
- `block/blocks.bend`: a cell is a few small numbers, and a rule is a list of reactions, pairs of block patterns that
  turn into each other. So reversibility is a check on that list, whatever the number of cell states.
- `block/LAWS.bend`: each law says what it restricts and what it leaves free. On the rule, proved for every block:
  finitely many cells, no value copied or lost, the rule undoes itself, empty space stays empty. On runs, by cases:
  every tree lays out and reads back, the grid reads as a tree at every tick, that tree changes only by the five moves
  (the [coarse-graining idea of Israeli and Goldenfeld](https://arxiv.org/pdf/nlin/0508033#page=4), relaxed so a tick may
  leave the tree alone), and leftovers leave only through the port. A run must go exactly still before its answer is
  read, and a bijection reaches a still state only if what moved has left, so every signal goes back out.
- `block/design.bend`: 6 fields (8,064 cell states) and 89 reactions. Programs are packets of tokens, one out at a time,
  each coming back as one echo; a node cannot merge echoes from both parts without recording which part each came from.
  It runs swap, map, `;`, † , stem and unit, with stuck runs coming back as echoes and the tree unchanged. On a 4-leaf
  tree, `map(swap, swap, swap)` takes 244 ticks. **Its limit:** new matter comes from two spare nodes laid over the root
  and each fork's part, so a third stem in one place bounces forever. It cannot run `add`, and the laws do not catch
  this, since their cases ask for at most two stems in a place.
- `block.html` draws a run tick by tick, with the blocks the next tick acts on, and the laws beside it.

**The design search (`block/search/`, in Python).** The design above compiles a program and can't grow, so five design
families were tried under one set of laws, none of them seeing the others' code. `laws.py` holds the laws, each with what it restricts
and leaves free. New against `block/LAWS.bend`: no compile step (a program is a tree on the grid, and how a run starts sees only
each layout's size), the reading is a walk over a one-cell table, at most 6 cells per node on small trees, nothing kept
for growth, and everything leaving the grid is the same on every run. The rungs: swaps, maps, grow, moves, tri, fix, add, general.

| Family | Highest rung | Reactions | What it showed |
|---|---|---|---|
| ballistic: nodes and mirrors, sight lines through empty cells, one ball as head | add (general: 5,217 right, 148 refused, 0 wrong) | 382 | Grow costs 169. The return trip is the same reactions run backward |
| arcs: all nodes in one column, right arms as arcs | grow | 146 | Cheapest at grow; cross and rotate need adjacent forks |
| polymer: a tree as one Polish strand | grow | 244 | Control dominates the count, not geometry |
| split/join: parts of a `map` at once | maps | 154 | A wave runs only on full trees, and its ticks still grow with size |
| chem: redexes as local rewrites | none | 28 for one program | One head at a time turns into route walking |

- **A token can't wait.** While shut in a bounded region a token's states form a cycle through its entry, so it un-runs its work. A join
  of two echoes has to undo and redo, costing an lcm of their times, so `map` can't run its parts at once in general.
- **Slack can't be optional.** "Room here" and "no room here" would merge states or put a bit in the head, which breaks `clean`, so the
  layout has no gaps and every growth really shifts.
- **The best design's limit.** Swap is a flip bit on a fork, which makes the tree non-planar: 2 of the 4 rotation cases need a part to cross a
  part. So rotation is only read, not done, and nothing runs on a crossed fork; `seq(cross, stem)` is refused. The rungs through add pass
  anyway because they don't ask for this; `general` does. Forks with no flip bit and a physical swap (`ballistic/g1_attempt.py`, unfinished, about
  490 reactions by estimate) are the open route.
- These laws are checked in Python only, not ported to Bend, and `block.html` still shows the older design. `block/search/view.html` shows the best one
  (`python view.py ballistic/design.py` builds its data).

## Further along the design space (not modelled)

- **Superconducting single-flux-quantum logic**: picosecond gates at attojoules, cryogenic.
- **Photonics**: very cheap transport over distance, expensive conversion and memory.
- **Resistive memory crossbars**: analog multiply-accumulate in memory, writes slow and wearing:
  configuration that is costly to change, the opposite corner from the strand lattice.
- **Brownian / thermodynamic computing**: the strand lattice's `thermo=1` is a step there; at kT scale
  drift and error trade against each other directly.

## Next

1. Make `netlist.ts` emit reversible netlists (controlled swaps, Toffolis) rather than functions,
   and feed its counts to the model in place of the hand-set workloads.
2. In the player: wires pipelined into short adiabatic stages, and placement that keeps wires local,
   so the adiabatic corner can be judged fairly; then the run-back check on real reversible gates.
3. The signal tree, under honest physics: cap a rewrite's splices at the lanes a cell has (routing
   around), and pass a long signal on as its head arrives instead of after its tail, so a stream of rules
   costs its length once rather than at every hop; then let a pass follow the last one down the tree
   instead of waiting for its done signals, as swapnet's packets already do.
4. In the frame: unit and unit† (forks made and taken away; then `addb` could make its own carry slots),
   then tri's fork case as a hop and bursts that act on their loop's node, so `addt` runs; room past
   forks (a whole part moving down a row, as a wave) and leaves that grow into the same cell (one asks,
   the cell grants); giving space back to Void; a layout that keeps a list's bits next to their forks, so
   binary add costs a constant per bit; loops whose bursts go left without each level waiting on R; a
   reversible design if the law is wanted.
5. In the block frame: a law that a place can grow without a fixed limit (stems in a chain up to 4 or more), which the
   spare nodes fail, and a design that meets it (a leaf growing into empty cells, a part shifting down); per-law times
   and proofs in parallel, since the check now takes about 21 minutes. The search below `block/search/` has met the growth law in Python; left: forks with no flip bit so moves compose, then a port to Bend.

## Reproducing

`cd research/signal-fabric && node sim.mjs && node sweep.mjs && node build-explorer.mjs`; open
`explorer.html`. The sweep takes a few minutes. The player: `npx --prefix website tsx
research/signal-fabric/fabric/bundle.mts`, then serve `research/signal-fabric/` and open `fabric.html`;
`npx --prefix website tsx research/signal-fabric/fabric/check.mts` checks it (about a minute). The
signal tree: `tree/bundle.mts` builds `tree.js` for `tree.html` (the physics is chosen on the page, or with
`?phys=honest|cell|shared|cheap`); `tree/check.mts [cheap|honest|cell|all]` checks every example in both
layouts, first run, again, on other bits and flipped (all three: about 20 seconds); `tree/scale.mts
[physics] [layouts] [bits]` times `addw` (every physics, both layouts, 4 to 32 bits: about 5 minutes) and
`tree/ablate.mts [program] [input] [layout]` prices each shortcut alone (16 bits: about a minute). The
frame: `frame/check.sh` regenerates `frame/cases.bend`, checks `LAWS.bend`, builds the viewer and prints
the design's score (about 72 seconds, 40 of them `binary_small`); `frame/check.sh view` only rebuilds the design for the viewer (under a
second). Open `frame.html` on the always-on file server (`scripts/dev-server.sh` prints its address). The block frame: `block/check.sh`
checks its laws and builds `block.html` (about 21 minutes); `block/view.sh` only builds the page. The design search: from `block/search/`,
`python laws.py ballistic/design.py` (about 23 seconds, 12 of them `add`), `python general_check.py ballistic/design.py` counts the refused runs, `python every_tick.py ballistic/design.py` reads the tree at every tick.

## Sources

Every link above was fetched and its quote found with `scripts/verify-source.sh`. Not verified: the
alpha-power delay law (Sakurai & Newton 1990, no open copy); the model does not use it. Pelgrom's
mismatch constant comes from a scan whose numbers do not read cleanly, so the 25 mV at 1 fF here is an
assumption, not his figure. Also checked and not cited above: race logic, where
[a value is represented as a timing delay](https://sites.cs.ucsb.edu/~sherwood/pubs/ISCA-14-racelogic.pdf#page=1)
and [the AND gate passes the last arriving](https://sites.cs.ucsb.edu/~sherwood/pubs/ISCA-14-racelogic.pdf#page=5)
edge (max), OR the first (min); and that
[DRAM access is a couple of orders of magnitude](https://gwern.net/doc/cs/hardware/2014-horowitz-2.pdf#page=2)
dearer than an operation, which is why the array writes configuration locally instead of fetching it.
