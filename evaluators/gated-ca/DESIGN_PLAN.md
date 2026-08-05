# gated-ca: a hardware-constrained cellular substrate

Status: design plan, pre-implementation. This file should shrink and eventually die as
the code lands and becomes self-explanatory.

## 1. Background

The goal is unchanged from `rust-ca-lattice`: evaluate disp's tree-calculus interaction
net on a spatial substrate where scheduling is emergent and activity only happens where
signals change. The cascade substrate got far (event-driven signal plane, three
equivalent demand backends, blocklet rewrites, frontier 5/5 on the probe corpus) but its
cell rule grew as unconstrained Rust: ad-hoc priority rules (ring rule, relief ordering,
stamps) interacting in ways nobody could audit, and nothing in the language stopped a
rule design that could never map to hardware. The corridor census showed the substrate's
failures were self-imposed rule interactions, not resource limits. The conclusion is that
the rule design space itself was the problem.

gated-ca restarts with one constraint added: the cell update rule must live inside the
silicon circuit design space, by construction, enforced by the Rust type system. If a
rule can be written at all, it is a netlist. That buys:

- an auditable rule: every rule has an exact gate count, depth, and fanout, measured
  automatically, pinned in tests
- a real hardware story: the rule is already a circuit, so "could this be silicon" stops
  being speculation
- free simulation strategies: the same rule definition yields a bool simulator, a 64-way
  bit-parallel simulator, and an extracted netlist, with mechanical cross-checking

## 2. The constraint layer

One trait. Signals are opaque, NAND is the only constructor:

```rust
pub trait Gates {
    type S: Copy;
    fn nand(&mut self, a: Self::S, b: Self::S) -> Self::S;
    fn low(&mut self) -> Self::S;
}
```

A circuit is any function generic over `G: Gates` that only touches signals through the
trait. Because `G::S` is fully abstract, the body cannot branch on a signal, compare one,
or convert it: the only possible operations are duplicate (fanout), route (wires), and
`nand` (a transistor pair). Rust control flow in the body can only depend on non-signal
data, so it is elaboration-time structure, like a Verilog `generate` block. Parametricity
makes the function denote exactly one netlist per monomorphization.

The gate library is written once, generically (`not`, `and`, `or`, `xor`, `mux`, adders,
comparators, priority encoders), all reduced to NAND. Extra primitives (tristate,
pass-gate tricks) are excluded until a rule genuinely needs them; adding one is an
explicit fidelity decision.

### Interpreters

The same circuit function runs against different `Gates` implementations:

```rust
/// Plain simulation, one instance.
struct Sim;                 // type S = bool

/// 64 instances in lockstep, one word op per gate.
struct SimWide;             // type S = u64

/// Netlist extraction with hash-consing (CSE). Run once on symbolic
/// inputs, get the gate graph.
struct Netlist {
    n_inputs: u32,
    gates: Vec<(u32, u32)>,          // gate i = nand(a, b)
    cse: HashMap<(u32, u32), u32>,
}                           // type S = u32 node id

/// Depth in the signal, gate counter in the builder.
struct Cost { gates: usize }         // type S = u32 depth
```

Sharing is exact because the builder threads through `&mut`: `let x = ...; f(x, x)`
visibly reuses node `x`. A proptest harness pushes random input vectors through `Sim`,
`SimWide`, and the extracted netlist and asserts all three agree, so every new gate
library entry and every new rule is self-checking.

## 3. Complexity measurement

Two numbers per circuit, both automatic:

- as-written cost (the `Cost` interpreter): gate count as emitted, critical-path depth
- post-CSE netlist stats: live gate count (reachable from outputs, dead gates reported
  separately), depth per output, max fanout, per-output input cone sizes

Fanout and cone sizes matter beyond bragging rights: max fanout predicts wire congestion
if the rule is ever placed, and per-output cones feed change-driven skipping (small cone:
recompute eagerly; large cone: worth dirty-tracking).

Budgets get pinned in tests per rule, e.g. `assert!(s.gates <= 220 && s.depth <= 14)`.
A refactor that bloats the rule is a test failure, and comparing candidate rules is a
table of (gates, depth, max fanout) instead of reading code.

## 4. Cell architecture

The lattice is the same six-neighbor cubic lattice. Each cube exposes `N` bits per face
and holds `S` state bits:

```rust
pub const DIRS: usize = 6;

pub struct CellIo<Sig, const N: usize, const S: usize> {
    pub dirs: [[Sig; N]; DIRS],
    pub state: [Sig; S],
}

pub trait CellRule<const N: usize, const S: usize> {
    fn step<G: Gates>(&self, g: &mut G, io: CellIo<G::S, N, S>) -> CellIo<G::S, N, S>;
}
```

The discipline that makes everything downstream work:

- the rule is purely combinational: (neighbor ports, state) to (output ports, next state)
- all cross-cube communication is registered: a cube reads only what neighbors latched
  last tick; signals travel one cube per tick
- registers live in the simulator (double buffering), never inside the rule

This is the flops-between-combinational-clouds discipline from synchronous design, and it
is doing scheduling work, not just hygiene: because no signal crosses a cube boundary
within a tick, there is no global combinational settle, so a whole-lattice tick is one
uniform local update with no ordering constraints between cubes. That single property is
what makes the GPU story in section 7 a one-dispatch-per-tick kernel instead of a
levelized global logic simulation.

### Two layers, kept distinct

- Substrate rule: the fixed circuit above, identical in every cube. Written in the Gates
  DSL, measured, pinned. This is the candidate silicon.
- Contents: the mutable bits the rule processes. What those bits encode is a separate
  choice, and two encodings are on the table (section 5).

A fixed circuit cannot rewire itself, and disp reduction rewrites its graph. The
resolution is that topology lives in the contents: agents, wires, and routes are state
bits, and "rewiring" is the rule writing different route bits, exactly as in cascade.
The constraint layer changes how the rule is expressed, not what it can express.

## 5. Content encodings

### A. Cell-granular (transcribe cascade)

State bits encode the cascade matter kinds directly (empty, wire routes, agent with tag
and endpoints, seed) and the rule implements the transition relation as logic. This is a
transcription project: each cascade transition becomes a cone of gates. Value: an honest
hardware cost for the substrate we already believe in, plus the measurement pressure that
squeezes rule complexity (every special case is now visible gates). Risk: the cascade
rule set may be too big as a circuit; the gate count will tell us immediately, which is
itself the point.

### B. Gate-granular (the lattice as reconfigurable fabric)

State bits encode a small local circuit: a few gate descriptors (which function, which
input ports, which output ports) plus wire segments. The substrate rule is then a tiny
fixed interpreter circuit: evaluate my descriptors against my latched inputs, drive my
outputs. This makes the lattice an FPGA-like fabric, and the "panoply of different
circuits in different cubes under one universal rule" falls out. Reduction rewires by
having descriptor bits be writable by the rule itself (partial reconfiguration, driven
from within).

Circuits destined for the fabric are written in the same Gates DSL, extracted to a
netlist, and placed into cube descriptors by a placer. Wire length, placement area, and
congestion become measurable lattice-level metrics on top of the netlist metrics.

Plan: A first. B reuses everything A builds (the DSL, the metrics, the simulators) and
adds only the descriptor encoding and the placer, so it stays a cheap second experiment.

## 6. CPU simulation

Three tiers, all driven by the same extracted netlist, chosen per workload.

### Tier 0: functional (no lattice)

Levelize the rule netlist once (gates sorted by depth), evaluate as a straight-line loop
over `(dst, a, b)` word ops against a scratch value array. With `u64` values this
simulates 64 independent instances per pass. This is the circuit-design inner loop:
truth-table checks, property tests, rule candidates compared in milliseconds. No spatial
anything.

### Tier 1: dense spatial, bitplane layout

The workhorse. Represent the lattice as bitplanes: for each of the `S` state bits and
`6*N` port bits, one 3D bit array with one bit per cube. A tick is then: for each gate in
the levelized rule program, one bitwise word op over whole planes. Neighbor reads are
shifted loads of the port planes (the classic bitboard-Life trick lifted to 3D and to an
arbitrary compiled rule).

Cost model: a 200-gate rule is 200 word ops per 64 cubes per tick, so ~3 ops per cube.
Compute is nearly free; memory traffic dominates. Therefore:

- tile the lattice (8x8x8 cubes: each bitplane slice of a tile is 512 bits, a whole
  tile's full state is a few KB and lives in L1/L2)
- lay planes out tile-major so one tile's working set is contiguous
- halo exchange between tiles at tick boundaries (ports only, not full state)
- rayon over tiles; the per-tile inner loop autovectorizes (or use explicit u64x4)

Sparsity via dirty tiles, not dirty cubes: a tile whose input halo and own state did not
change is skipped wholesale. Change detection is one XOR-accumulate per plane during the
write pass, nearly free. This is the tile-aware loading lesson from the wire-RC work,
built in from the start instead of retrofitted.

The rule program is interpreted (a `Vec` of ops), which is fast enough to start; if it
ever matters, a build step can emit Rust source from the netlist for constant folding and
register allocation, an estimated 2-5x, deliberately deferred.

### Tier 2: sparse event-driven

When active fraction is tiny (long quiescent lattices with a small frontier), even
touching every tile's dirty flag wastes bandwidth. Keep a worklist of active tiles
(generation-ping-pong, like cascade's FIFO generations), enqueue neighbors whose halo
changed. Same tile kernel as tier 1, different driver. The crossover between tiers 1 and
2 is an empirical number to measure, not to guess; both drivers share the tile kernel so
the comparison is honest.

## 7. GPU simulation

The registered-boundary discipline is what makes this clean: no intra-tick global
dependencies means one dispatch per tick, no level-by-level global synchronization, no
divergence.

### Dense kernel

- thread block = tile (e.g. 8x8x8 cubes); the tile's bitplanes live in shared memory,
  each thread owns one or more 32-bit words of each plane
- the rule program (levelized `(dst, a, b)` triples) sits in constant memory; every
  thread executes the identical instruction sequence on its words: zero divergence by
  construction, since the program has no data-dependent control flow (the type system
  guaranteed that upstream)
- halos load from neighbor tiles' port planes at block start; outputs write back
  double-buffered
- intermediates live in registers, spilling to shared memory if the rule's live set at
  its widest level exceeds the register budget (the netlist stats predict this number
  before the kernel ever runs: max live width is a computable metric, add it to the
  stats pass)

### Sparse dispatch

Per-tile dirty flags in a device bitset, stream compaction builds the active tile list,
indirect dispatch runs only active tiles. Same ping-pong. This is the standard sparse CA
pattern and composes with the dense kernel unchanged.

### Multi-instance slicing

The bit dimension can pack 32/64 independent problem instances (different terms, seeds,
rule variants) instead of spatial neighbors: embarrassingly parallel corpus runs and
property tests on either backend.

### Backend choice

wgpu compute for portability first; the kernel is trivial enough (bitwise ops on shared
memory) that CUDA-specific features buy little until profiling says otherwise.

## 8. Verification spine

- proptest: Sim vs SimWide vs extracted netlist, per circuit
- tier equivalence: tier 0 vs tier 1 vs tier 2 on the same rule and corpus, exact state
  equality per tick (they share no evaluation machinery, like cascade's three signal
  backends, which caught real bugs)
- CPU vs GPU: same, exact bit equality (everything is integer ops, no tolerance needed)
- budgets: per-rule (gates, depth, max fanout, max live width) pinned in tests
- behavior: transcribed rules checked against cascade traces on the shared probe corpus

## 9. Open questions

- Clocking: global tick is assumed throughout (GPU-friendly, simple). A locally
  handshaked (asynchronous) fabric is more silicon-honest but costs state bits and
  complicates every simulator. Deferred until a global-tick rule exists to compare
  against.
- How much combinational depth per cube is reasonable? Depth bounds the hardware clock;
  the budget pins will force the conversation per rule.
- For encoding B: placement and routing quality, and whether self-reconfiguration (the
  rule writing descriptor bits) stays auditable.
- How large the transcribed cascade rule actually is. If the full transition relation is
  thousands of gates, that is a finding about the rule, and the measurement pressure
  should drive simplification rounds before any spatial work.
- Whether `N` (bits per face) can stay small. Cascade's u64-per-site suggests S around
  64; per-face bandwidth is the scarcer resource and the netlist stats will show which
  port bits are actually load-bearing.

## 10. Milestones

1. `gates` module: trait, NAND library, Sim/SimWide/Netlist/Cost, stats pass (live
   count, depth, fanout, cones, max live width), proptest harness. ~300 lines, useful
   standalone.
2. Toy rules under budget pins (adder, small state machine, a Life-in-3D rule) to shake
   out the DSL ergonomics and the stats.
3. Tier 0 + tier 1 CPU simulator with tiles and dirty flags, tier equivalence tests.
4. Transcribe the smallest honest slice of cascade (wire heat propagation alone) into a
   CellRule; measure it; compare against cascade behavior on wire-only corpora.
5. Tier 2 driver; measure the tier 1/2 crossover.
6. wgpu dense kernel; CPU/GPU equivalence gate.
7. Encoding B experiment: descriptor format, interpreter rule, placer, congestion
   metrics.
