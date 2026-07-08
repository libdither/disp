# Spatial IC — interaction combinators on a distance-aware substrate, and resident machines

Research note, 2026-07-07. Companion to `tc-net.typ` (the calculus), `RUST_IC_NET_DESIGN.md`
(the pointer-machine implementation), and `/OPTIMIZER.typ` (the cost ledger this note extends).
Status: design investigation, nothing built. Three independent literature sweeps grounded the
prior-art claims (2026-07-07); references in §8.

## 0. Claim and verdict

**The idea.** Run interaction-combinator reduction on a fully parallel cellular substrate, one
you could implement raw in a chip: a lattice of finite-state cells, agents as cell patterns,
wires as physical paths of cells, interactions firing only between adjacent principal ports.
Physical distance is real: information moves at most one cell per tick, and agents migrate
along their connections to shorten wires before reacting. Part II extends the picture with
**resident machines**: fixed-function circuits embedded in the fabric that consume and produce
agents, entered by a certified equivalence rewrite.

**Verdict.** Coherent, with one near-miss precedent (interaction automata / ia2d, FSCD 2016,
§8.1) whose two most interesting ingredients are unclaimed: attraction-driven migration and
distance as a first-class cost grade. Not competitive as a simulated runtime on today's
CPUs/GPUs; strong as (a) the honest hardware-cost model `OPTIMIZER.typ` §4/§9 explicitly
leaves open, (b) a scheduler insight (tiling + migration) applicable to `rust-ic-net` and
HVM2-class GPU reducers now, and (c) a long-range hardware thesis whose substrate class has
shipped silicon precedent (Cerebras-scale meshes). The whole bet is hostage to one unmeasured
number: the effective Rent exponent of real disp reduction traces (§4.3, experiment E1).

A finding worth flagging up front: **this repo's calculus is unusually suited to cellular
realization**. TC-Net's no-oracle theorem (`tc-net.typ` §Two Species Coexist) means the agent
alphabet is finite and label-free, so cells can be genuinely finite-state. HVM-style
lambda-calculus ICs carry duplication labels, which either blow the per-cell state budget or
cap sharing depth. Binderlessness pays again, this time in silicon.

# Part I — the distance-aware substrate

## 1. Why the fit is real, not aesthetic

Interaction combinators are already the most local universal model of computation available:
every rewrite touches exactly two agents joined at their principal ports; there is no
environment, no substitution at a distance, no global state. Three consequences line up:

- **Geometry can only be a scheduler.** The single-principal-port invariant gives strong
  confluence (`tc-net.typ` Theorem 2): any firing order yields the same result. A
  distance-aware machine cannot change what a net computes, only when and where. This is the
  entire soundness argument, and it comes for free.
- **GC is already spatially local.** Erasure is an ε wavefront; demand-before-copy duplication
  (δⁿ) copies one weak-head constructor at a time, at the point of demand. Laziness doubles as
  a transport minimizer: material is created adjacent to its consumer, just in time.
- **The redex bag dissolves into adjacency.** A redex is two agents whose principal ports
  touch, detectable by the two cells involved. No central queue exists. The bag's O(1) pop
  becomes O(distance) approach, which sounds like a regression until you notice the pointer
  machine also pays O(distance) when its two nodes are cold in DRAM; it just has no model of
  it. HVM2's paper is one long fight with exactly this: variables are "the only rule where
  nodes far apart can affect each other," and its two big wins were locality wins (96 KB
  per-SM scratchpads: 13,000 to 54,000 MIPS; warp-alignment scheduling: 1,300 to 12,000 MIPS
  on bitonic sort).

## 2. Where distance lives — the two design axes reconciled

Two candidate designs were on the table: distance as an integral part of the elimination
semantics, versus agents migrating automatically to minimize distance. The coherent answer
uses both, at different layers, and rejects one specific corner.

### 2.1 Rules stay distance-blind (the rejected corner)

If rewrite rules can observe distance (branch on near versus far, race, time out), the system
contains `amb` and strong confluence is destroyed, which `OPTIMIZER.typ` §3 correctly marks as
sacred: it is what makes parallelism free, results schedule-independent, and the
differential-oracle backend discipline possible. Rule *outcomes* never depend on geometry.

### 2.2 Distance in the semantics: an embedding, and transport as interactions

The clean formulation is a **geometric embedding**: map the abstract net to an embedded net in
which each wire becomes a chain of forwarder agents whose length is the physical path length.
(ia2d independently validates the representation: its wires are literally paths of forwarder
cells.) Transport is then forwarder elimination: the wire shortening by one cell is one local
interaction. This buys a tidy unification:

- Cost is still "count interactions," but on the embedded net the count automatically includes
  transport. The difference between embedded and abstract interaction counts is exactly the
  geometry term.
- Simulation shape: embedded reduction simulates abstract reduction with overhead equal to the
  transport term; VLSI lower bounds (§4) show that for some nets no embedding does better,
  so the model is asymptotically honest where bare interaction count is provably not.

In ledger terms (`OPTIMIZER.typ` §4): the graded semiring gains a **transport grade**
(bit-meters, the standard VLSI/energy metric) alongside interactions (work), peak live nodes
(space), and span. This lands squarely on the §3 note that interaction count is "a reasonable
but space-biased proxy, provably not wall-clock" and that faithful hardware cost is a separate
modeled axis. The cellular substrate is arguably the *minimal* honest model for that axis: it
prices the only two things silicon charges for, switching and transport, and abstracts
everything else. The stakes, quantitatively (Horowitz ISSCC 2014, 45 nm; Dally): a 32-bit add
is ~0.1 pJ; on-chip movement is ~0.1 pJ per bit per mm (medium confidence on the per-bit
decomposition); a DRAM access is 1.3–16 nJ. Four orders of magnitude between computing a word
and fetching it.

### 2.3 Migration in the implementation: tension physics as the scheduler

Wire tension, locally computed:

- **Principal-principal wires** (nascent redexes) contract with high priority; contracting one
  to length zero *is* scheduling that redex.
- **Auxiliary wires** exert weak tension: they must not grow unboundedly, nothing more.
- Each agent steps one cell per tick along the vector sum of its wire tensions; wires
  straighten peristaltically.

This is force-directed layout running concurrently with reduction, computed by purely local
rules. Because of §2.1 it is a pure policy layer: iterate on it empirically with zero
soundness risk. The programmable-matter literature contributes a hard-won lesson here
(§8.4): **deterministic greedy descent provably deadlocks in local minima; Metropolis-style
stochastic moves provably compress** (amoebot compression, PODC 2016, with a genuine phase
transition in the bias parameter), and dense lattices should move *vacancies* rather than
agents (Claytronics hole motion). So the migration layer is annealing, not greedy descent,
driven by a deterministic per-cell PRNG (seeded by position and tick) so runs stay
bit-reproducible for the differential oracle.

## 3. Sketch of the machine

- **Cell state**: a few tens of bits. ~4-bit agent tag (TC-Net alphabet: `L S F P E A T1 T2
  δs δn ε` + forwarder/wire + empty), port-orientation bits, wire-layer field, phase bit,
  PRNG counter.
- **Neighborhood**: T₂ has five ports (one principal + four aux), so single-cell agents need a
  Moore or hexagonal neighborhood; alternatively T₁/T₂ occupy two-cell blocks, echoing
  `RUST_IC_NET_DESIGN.md` §2's decision to store them as multi-slot blocks.
- **Crossings**: interaction nets are non-planar; give each cell 2–3 wire layers with via
  states (von Neumann's 29-state CA and Wireworld both have crossing organs; real chips have
  ~15 metal layers).
- **Clocking**: Margolus block partitioning gives deterministic, conflict-free synchronous
  updates; rewrites that allocate several agents unfold over staged micro-ticks. A GALS
  (locally clocked, handshake at borders) variant matches wafer-scale reality; confluence
  keeps results deterministic even when the trace is not.
- **Space metabolism**: the S-rule and commutations consume free cells; annihilation and
  erasure release them; vacancy diffusion carries free space toward allocation pressure.
  Physical memory exhaustion appears as sustained pressure with no relief, i.e. OOM is a
  local, observable condition.
- **GC**: ε waves work as-is. The δⁿ parked-agent leak that forces wire-RC in
  `RUST_IC_NET_DESIGN.md` §5 gets a geometric answer: the refcount back-channel becomes a
  physical pulse sent down the parked wire when both outputs die. Parked agents are idle
  cells, so laziness is literally clock gating.
- **Graded lowering unchanged**: the strictness bit injects `E` demands; the usage grade
  picks the δ species (`RUST_IC_NET_DESIGN.md` §7 transfers verbatim; the coeffect story is
  substrate-independent).
- **Superposition caveat**: if the optimizer's `sup` layer needs labels, label bits bound
  superposition depth per region. The base calculus needs none (the no-oracle theorem).

## 4. The physics you cannot negotiate away

Being honest about cost means inheriting real lower bounds.

### 4.1 The light cone

ia2d's one analytical result: on a d-dimensional grid the active-redex population after t
steps is at most polynomial of degree d. A program whose abstract redex set grows
exponentially (balanced fan-out recursion) is capped at polynomial parallelism by geometry
alone. Their suggested escape is multiscale long-link topologies (express channels), which is
also where real interconnects went.

### 4.2 Mesh laws

Any global rearrangement of N items on a 2D mesh costs Θ(√N) steps (Thompson–Kung 1977;
Schnorr–Shamir tightened the constant to ~3n). Thompson's AT² bounds make this fundamental:
any bisection of a chip of area A has width O(√A), and Ω(N) bits must cross it for
communication-heavy functions. Concretely for ICs: duplicating and then separating a size-s
subnet costs ~s^{3/2} transport in 2D against O(s) abstract interactions.

### 4.3 Rent's rule is the verdict-deciding number

Donath's result: after good hierarchical placement, average wire length stays bounded when the
Rent exponent p < 0.5 and grows as g^(p−0.5) above it. Real placed logic sits at p ≈ 0.5–0.7.
**Whether disp reduction traces have effective p below or above 0.5 decides whether the
spatial bet is sound, and nobody has measured it for interaction nets.** It is measurable now
on existing backends (experiment E1, §7). If traces come back expander-shaped, the pure mesh
dies and the design retreats to mesh + sparse express links, which real chips do anyway.

None of this is a defect. Every physical machine pays these costs; this design is the one
where the optimizer can *see* them. The no-memo substrate already gives per-decision cost
attribution (`OPTIMIZER.typ` §3); a spatial substrate extends attribution to data movement,
which is where the energy actually goes. Endgame reading: on hardware of this shape,
per-candidate cost becomes per-region power draw, and `GOALS.md`'s measurement primitive
becomes an energy meter.

## 5. As a CPU or GPU alternative, concretely

Simulating the literal CA on a GPU is not a performance play: agents crawl one cell per tick
where a pointer write teleports; most cells idle every tick; sparsifying the simulation
converges back to an event-driven pointer machine. The literal CA is an *instrument* (cost
model + dynamics validator), and can be the `OPTIMIZER.typ` §9 hardware model as a tree
program: `run_model : NetLayout -> (Result, Transport)`.

The version with near-term teeth is the **tiled middle ground** (the CA with big smart
cells): partition the net into regions, one per GPU SM's shared memory or per CPU tile;
reduce internal redexes at full local speed; make cross-region links explicit and charged;
migrate agents between neighboring regions down the tension gradient. HVM2's scratchpad +
LEAK design is a degenerate two-level instance, and its 4.2× win says the gradient is steep.
Cerebras WSE-3 shows the substrate class ships: 900k cores, 48 KB each (a thousand-plus net
nodes per tile), single-cycle neighbor links, no caches, no coherence. On such a machine this
design stops being exotic and becomes "how you would have to run an IC reducer anyway," with
tension-driven migration as the load balancer.

Ladder of spatiality (each level cheap relative to the next, each gating the next):

1. **Measure** the effective Rent exponent of real traces (E1).
2. **Ledger**: add the transport grade + the embedding formulation to the optimizer ledger.
3. **Tiled scheduler experiment** in `rust-ic-net` (region arenas + migration heuristic +
   traffic counters vs the current global-arena work stealing).
4. **CA simulator** as `run_model` (cost instrument; validate tension scheduling and jamming
   thresholds empirically).
5. **FPGA cell array** (existence proof at small scale; FPGA fabric is itself a 2D cellular
   substrate; the Vine community's WIP "Tendrils Compute" FPGA target suggests others smell
   the same opportunity).
6. **ASIC/wafer** only if 1 and 3–5 say yes.

# Part II — resident machines

The substrate so far is uniform. Real chips are not: they are mostly interconnect and memory,
with dedicated function units set about in space. This part develops the corresponding move
here: **special machines embedded in the fabric that consume and produce agents**, and the
claim that entering one is an act of *recognition*.

## 6. The three-layer resolution

The intuition "optimization = the net recognizing that one of its own processes is equivalent
to a special machine, swapping it for the dedicated circuit, and leaving the composition of
circuits to the agents" is coherent once split into three layers with different characters.
The initial unease traces to wanting layer 2 to be emergent like layer 3; it cannot be, and
the repo's architecture already says where it lives instead.

### 6.1 Machines are just agents (standard, already practiced)

A resident machine presents to the net as an ordinary agent: **one principal port**, a finite
set of aux ports, and a declared interaction contract. Internally it can be anything (a
pipelined multiplier, a matmul array, a whole CPU); externally it is one agent with large
latency, and latency is semantically invisible (interactions were never unit-time on the
spatial substrate anyway; confluence does not care about durations). The one-principal-port
constraint is load-bearing: it preserves the ownership argument (`RUST_IC_NET_DESIGN.md` §2,
"never introduce a multiport agent") and Theorem 2.

This is not exotic. It is how every practical IC runtime already smuggles arithmetic in: HVM2's
native number and operator agents are machines in exactly this sense (an "interaction" that is
an ALU op, not a net rewrite). `rust-ic-net` reserves a `Ref` tag for the same slot. Lafont's
own package/box construction (a subnet with free ports treated as one agent) is the formal
license. And the project already operates one resident machine today: **the `tree_eq` native
fast-path in `src/core/tree.ts`**, a host-native implementation semantically mirroring
in-language code, validated differentially against the in-language reference. The pattern's
first instance predates the hardware framing.

Multi-argument strictness is the standard currying dance (HVM2's OP agents): principal faces
the first argument, re-faces the second when it arrives; demand chains (`E` seeds at machine
inputs) supply the strict scheduling.

### 6.2 Recognition is staged and certified, never emergent (the fix)

The step "this subnet is equivalent to machine M" **cannot be an in-dynamics local rule**.
Extensional equivalence is not locally observable: a subnet's behavior is a global,
asymptotic property, and local rewrites only implement the calculus's own syntactic
reductions. A soup that mutates itself by local pattern-matching into *semantic* equivalences
is either unsound or is not doing local rewriting.

Where it lives instead is already built, in pieces, in this repo:

- **Machine-equivalence is a type.** Types are predicates here; so
  `Machine_M := { t : Tree | t ~ spec_M }` where `spec_M` is the machine's specification as a
  tree program (`OPTIMIZER.typ` §9: a hardware model is a tree program; `Refines spec asm`).
  Recognition is membership checking through the walker; the membership witness is the
  certificate. "Residence on dedicated hardware is a privilege granted by proving membership."
- **The swap is φ.** Replacing the subnet by a reference to the machine is exactly the
  `OPTIMIZER.typ` §5/§7 verified rewrite ("swap e for proven-equal cheaper e′") at the codegen
  corner of the staging axis, executed at a stage boundary (region quiescence / JIT pause),
  licensed by the certificate, checked by the trusted checker. The guard layer's licensed
  rebind (`license_guard` payload `{ new := machine_ref; proof := oeq_witness }`) is the
  module-level rehearsal of the same act.
- **Targeting is provenance.** The no-memo substrate attributes cost per region; a hot,
  recurring subnet is visible in the trace (on physical hardware: literally as a warm
  neighborhood). Untrusted search proposes candidates (pattern mining, e-graphs, neural
  proposal per `GOALS.md`); the checker disposes. Recognition need not be complete, only
  profitable where it fires.
- **Precedent stack** (§8.5): compiler *idiom recognition* (substituting library/hardware
  primitives for recognized code), *verified lifting* (lifting legacy code to a spec with a
  checked equivalence proof, then retargeting to accelerators), and *certified hardware
  compilation* (Vericert-style proven C-to-Verilog) are the software and hardware halves of
  exactly this step, none of them coupled to a rewriting substrate.

So "self-recognition" is right in the reflective sense (the optimizer is a tree program
recognizing tree programs; triage makes programs data) and wrong in the emergent-chemistry
sense. Staged self-recognition, not in-dynamics self-recognition.

### 6.3 Scheduling stays in the net (the part that is emergent)

Once machines exist in space, the residual computation is: route arguments to machines,
sequence dependent uses, arbitrate contention. That residual is a dataflow problem, and the
net *is* the dataflow graph: demand chains + wire tension + queuing at machine ports already
constitute a distributed, asynchronous scheduler, and confluence makes any arbitration order
sound (queueing affects latency, never results), while the transport grade prices it.

The interesting new constraint: **a physical machine is a linear resource**. A δ meeting a
machine reference copies the *reference* (both copies point at the same silicon, i.e.
contention), never the machine. Hardware is the ultimate linear type, and the usage coeffect
the ledger already carries is the correct accounting language: usage grades on machine refs
predict contention; grade-driven replication (a pool of machine instances) is the
type-directed response; queuing delay is a transport-grade cost. This also connects to the
`OPTIMIZER.typ` open question on the affine fragment as a decidable search island: hardware
linearity and type linearity meet in the same fragment.

Boundary discipline carries over unchanged: *pure* machines (arith, matmul, hash) live in the
net as agents; a machine that is a true effectful device (IO, real sensors) is exactly the
driver boundary of `TYPE_THEORY.typ` §15 and stays at the edge. And note the von Neumann
inversion: no "memory machine" is needed, because the fabric *is* the memory (the net is its
own store); machines are the exception, storage the default.

### 6.4 Grain, placement, and why the two ideas need each other

A machine pays off iff (invocations × per-call saving) exceeds (fixed area + transport to
reach it). Small machines (adders) are only worth having everywhere inline, which is HVM2's
native-number choice; large machines (matmul, FFT) are worth traveling to. Placement, count,
and grain are therefore optimization variables, and they are gradient-visible **only** with
the transport grade: without distance in the ledger, "call the machine" looks free and the
optimizer hallucinates speedups. Part I's cost model and Part II's machines are one design.

### 6.5 Existing chips, reread through this lens

A modern SoC/GPU is this architecture seen through a glass, darkly, with every layer present
but uncertified and frozen:

- **ALUs and the ISA** are machines whose recognition step ran once, at instruction-set design
  time: frozen idioms, certificates replaced by a specification document.
- **Caches** are automatic, heuristic, uncertified locality management: migration without a
  ledger (and with no way to say "this wire is principal-principal, contract it first").
- **The out-of-order window** is a tiny, hidden, energy-expensive dataflow net over a few
  hundred instructions: emergent scheduling, but trapped behind a sequential ISA and rebuilt
  from scratch every cycle.
- **SoC accelerators + drivers** are resident machines whose composition glue is imperative,
  effectful, and unverified: scheduling done by the layer least suited to it.
- **Cerebras-class meshes** are the substrate with the machines and the calculus left out.

The proposal, stated as a re-plumbing: make locality management explicit (tension/migration
under a ledger), make recognition ongoing and certified (φ with certificates, instead of
once-at-ISA-design), and let scheduling be the net itself at whole-program scale (instead of a
200-instruction window). Each layer exists in shipped silicon; what has never shipped is the
three of them made semantic.

### 6.6 The loop this closes

The repo's core discipline reads: *the object language is the specification; host
implementations are optimizations; every native fast-path is validated against the in-language
reference.* Resident machines are that discipline made physical. Every circuit is a
certificate-carrying replacement for a region of soft net; the soft net remains as the spec
and the fallback; hardware can be differentially validated against the very subnet it replaced
(the `tree_eq` discipline, at silicon level). Optimization becomes a monotone hardening chain
(recognize, certify, reside; repeat), which terminates locally when the residual glue is
cheaper than any further hardening. Pushed to the limit this is the third Futamura projection
with a hardware rung: the optimizer specializing itself down to circuits, certificates all the
way down. "Hardware design" stops being a separate discipline and becomes the coldest, most
amortized corner of the staging axis.

## 7. Experiments, cheapest first

- **E1 — Rent exponent of real traces (decisive, cheap).** On `rust-ic-net` (or `rust-eager`),
  log per interaction the address distance |addr(a) − addr(b)| of the two agents (log2
  buckets); additionally dump net snapshots every N interactions and compute boundary-vs-size
  scaling under recursive partitioning (fit `boundary ~ size^p`). Address distance is an
  allocator-polluted proxy; the partition fit is the real measurement. Run over representative
  workloads (kernel self-verification, arith/bench programs, list/stream code). Decision rule:
  p clearly below 0.5 supports the mesh bet; p above ~0.7 kills the pure mesh and points at
  express links.
- **E2 — the transport grade.** Write the embedding formulation (wires as forwarder chains,
  transport = forwarder interactions) into the `OPTIMIZER.typ` §4 ledger as the fourth grade
  (work, span, space, transport/bit-meters). Doc-level work; makes machine placement (§6.4)
  expressible at all.
- **E3 — tiled scheduler experiment.** Region-per-tile arenas in `rust-ic-net`, cross-tile
  links through mailboxes, a tension-driven migration pass, hardware traffic counters
  (cache misses, cross-socket traffic) vs the current global-arena work stealing. This is the
  practical distillation and a plausible real-performance win independent of any CA.
- **E4 — CA simulator as `run_model`.** Small, host-side (or in-language, per `OPTIMIZER.typ`
  §9); validates tension scheduling, jamming thresholds, and the stochastic-migration design
  (amoebot-style bias parameter); produces the transport readout for E2.
- **E5 — FPGA cell array.** Small lattice (10³–10⁵ cells direct, more time-multiplexed à la
  CAM-8); the raw-chip existence proof; only after E4 stabilizes the cell state machine.

## 8. Prior art map

Two web sweeps, 2026-07-07. Confidence noted where not high.

### 8.1 The direct precedent

- **Gimenez & Obwaller, "Interaction Automata and the ia2d Interpreter," FSCD 2016.**
  Interaction nets on Z^d grids ("webs" of locations); ≤2 nodes per cell; wires as paths of
  forwarder cells; asynchronous, purely local reduction; an explicit migration pass that is
  *density-driven* (spread toward empty cells, keep partners in range) and self-described as
  naive; framed as proof-of-concept abstract hardware; Haskell interpreter; bitonic sorter
  speedup 2.3×→33× (2→32 leaves). Their future-work list is this note's agenda: better
  migration, multiscale topologies, minimal symbol sets. One analytical result: redex
  population after time t is polynomial of degree d (the light cone, §4.1). No follow-up
  line of work found in the ten years since.
  https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.FSCD.2016.35

### 8.2 Unclaimed (verified absences, two sweeps)

Partner-attraction migration (force-directed, wire-shortening); IC/TC-specific spatial
instantiation; any FPGA/ASIC interaction-net hardware (only Lippi's clockless-circuit theory,
Fundamenta Informaticae 2009, and HigherOrderCO's stated chip ambitions; the Vine community
lists a WIP FPGA target "Tendrils Compute"); distance as a cost grade in any IN cost
semantics (the reasonable-space line, Accattoli–Dal Lago–Vanoni LICS 2022, is machine-based
and metric-free; Gimenez–Moser POPL 2016 is amortized but distance-blind).

### 8.3 Substrate-adjacent

- **Structurally dynamic CA** (Ilachinski & Halpern, Complex Systems 1987) and
  **graph-rewriting automata** (Tomita, Kurokawa, Murata, Physica D 2002 / Artificial Life
  2007; Arrighi et al. causal graph dynamics, 2012): the *substrate's own* topology rewrites;
  none embed a separate confluent calculus in a fixed metric space.
- **Chemlambda** (Buliga, 2013–14): purely local trivalent graph rewriting, explicitly
  topology-agnostic; no metric, no migration, no cost model. The delta of this note is
  exactly what it lacks.
- **Blob computing** (Gruau et al., ~2004–07): force-like CA physics (pressure, elasticity,
  "cellular gravity") hosting self-developing automata networks; the strongest substrate
  precedent, but the resident program model is not a confluent universal rewriting calculus.
- **HVM2** (Taelin/HigherOrderCO, 2024): 400 MIPS single-thread M3 Max; 5,200 MIPS 16-thread;
  74,000 MIPS RTX 4090; the paper's own optimization narrative (atomic substitution map,
  96 KB scratchpads +4.2×, warp alignment +9×, LEAK) is the locality fight, §1. Single-thread
  is 5×–100× behind GHC, so per-interaction useful work is low.
- **Mesh silicon**: Cerebras WSE-3 (900k cores, 48 KB/core, single-cycle 4-neighbor 32-bit
  wavelets, no caches/coherence); GA144 (144 async Forth cores, ~144 B RAM each); Adapteva
  Epiphany-V (1,024 cores, 64 KB each); UC Davis KiloCore; CAM-8 (Margolus/Toffoli, dedicated
  CA hardware, 1995: the lattice virtualized over DRAM, ~3G site-updates/s at 1 bit/site,
  i.e. the time-multiplexed alternative to one-cell-per-site).
- **Theory**: Thompson AT² (CMU thesis 1980); Thompson–Kung mesh sorting Θ(√N) (CACM 1977;
  Schnorr–Shamir 3n constant, STOC 1986); Leighton (1992) for bisection bounds; Rent's rule
  (Landman & Russo 1971) and Donath's wire-length law (IEEE TCAS 1979): average length bounded
  iff p < 0.5; placed logic measures p ≈ 0.5–0.7.
- **Energy**: Horowitz, ISSCC 2014 (45 nm: 32b add 0.1 pJ; 64b SRAM 10–100 pJ by size; DRAM
  64b 1.3–2.6 nJ); Dally (28 nm: 64b FP ~20 pJ; ~26 pJ per 256b×1mm on-chip; ~16 nJ DRAM;
  per-bit-per-mm decomposition medium confidence).
- **Migration algorithmics**: amoebot model (SPAA 2014; canonical amoebot 2021 with
  deadlock-free concurrency control); compression via Metropolis dynamics with a provable
  phase transition (Cannon, Daymude, Randall, Richa, PODC 2016); Claytronics hole motion
  (De Rosa et al., ICRA 2006: move vacancies, not modules, in dense lattices); Chirikjian's
  metamorphic systems (1994–97: earliest statement that greedy local reconfiguration
  deadlocks; simulated annealing as mitigation).

### 8.4 Machines-adjacent (Part II lineage)

*(Verified by a third sweep, 2026-07-07.)*

- **Dataflow machines**: Dennis & Misunas static dataflow (ISCA 1975); MIT Tagged-Token
  (TTDA), whose associative token-matching overhead was the classic failure; Monsoon's
  Explicit Token Store was the *fix* for that (Papadopoulos & Culler, ISCA 1990), and the
  Monsoon retrospective blames weak accumulator-style per-thread instruction power instead
  ("the beginning of the end of dataflow, as it demystified the approach"). The IC version
  answers the token-store problem structurally (point-to-point wires, no associative store)
  and makes locality first-class, which no dataflow machine did.
  https://dl.acm.org/doi/10.1145/325164.325117
- **Spatial dataflow / EDGE**: WaveScalar (Swanson, Michelson, Schwerin, Oskin, MICRO 2003;
  TOCS 2007), a dataflow graph cached across a grid of ALU-in-cache PEs with local
  scheduling; TRIPS/EDGE (Burger, Keckler et al., IEEE Computer 2004; ASPLOS 2009 eval).
  The closest architectural cousins to "scheduling left to the agents."
  https://cseweb.ucsd.edu/~swanson/papers/Micro2003WaveScalar.pdf
- **CGRAs**: survey Liu et al., ACM CSUR 2019 (https://dl.acm.org/doi/10.1145/3357375);
  Plasticine (Prabhakar et al., ISCA 2017) and its commercial descendant SambaNova SN40L
  (arXiv 2405.07518): meshes of word-level function units + routing fabric, compiler-placed
  dataflow; "machines + soft glue" with the glue imperative.
- **Idiom recognition**: IDL (Ginsbach, Remmelg, Steuwer, Bodin, Dubach, O'Boyle, ASPLOS
  2018): constraint-matched idioms over LLVM IR substituted by BLAS/cuSPARSE/Halide calls,
  60 idioms in NAS+Parboil, up to 20×; LiLAC (CC 2020); KernelFaRer (TACO 2021, matmul to
  BLAS/MMA). Recognize-and-substitute, syntactic rather than certificate-checked.
  https://dl.acm.org/doi/10.1145/3173162.3173182
- **Verified lifting**: STNG (Kamil, Cheung, Itzhaky, Solar-Lezama, PLDI 2016), Casper
  (Ahmad & Cheung, SIGMOD 2018), Tenspiler (ECOOP 2024, MetaLift lineage): CEGIS-synthesized
  summaries with per-program SMT-checked source-equals-summary proofs, then retargeting to
  Halide/Spark/tensor backends. Scope caveat: the *lift* is certificate-checked; the final
  summary-to-DSL codegen and both semantics axiomatizations are trusted. Still the closest
  match to §6.2's checked-swap, minus the substrate.
  https://arxiv.org/abs/2404.18249
- **Certified HLS**: Vericert (Herklotz, Pollard, Ramanathan, Wickerson, OOPSLA 2021):
  CompCert-based C-to-Verilog in Coq, 431-line Verilog-semantics TCB, ~2× behind unverified
  HLS. The certified-machine-construction half.
  https://johnwickerson.github.io/papers/vericert_oopsla21.pdf
- **Custom-instruction extraction**: Atasu, Pozzi, Ienne (DAC 2003, best paper): select
  maximal-speedup convex dataflow subgraphs to harden into function units; survey Galuzzi &
  Bertels, TRETS 2011. Recognition-at-design-time, the frozen ancestor of §6.2.
- **Anton** (Shaw et al., ISCA 2007 / CACM 2008): 32 hardwired interaction pipelines
  (~50 GP-ops/cycle each) + 12 programmable cores per ASIC; shipped proof that "dedicated
  pipelines + general cores on one die" wins big when the hot kernel is stable.
- **Graph-reduction hardware**: SKIM (Cambridge 1980, SKI in microcode), NORMA (Burroughs
  ~1986), GRIP (Peyton Jones et al., FPCA 1987), Reduceron (Naylor & Runciman, FPGA
  template instantiation, ICFP 2010). These harden the *reducer* into hardware, not
  *programs* into machines; complementary and mostly cautionary (they lost to commodity
  scaling; the wafer-scale era changes the terms).
- **Verified absence**: nothing found combining interaction nets or graph reduction with
  formally-checked substitution of subnets by fixed-function hardware. The composite in
  §6.2 (equivalence certificates licensing swap-for-resident-machine inside a net
  substrate) appears novel; the nearest neighbors are idiom recognition + verified lifting
  (recognition with proofs, von Neumann substrate) and Vericert (certified circuits, no
  recognition).

## 9. Risks, honestly ranked

1. **Online routing under congestion**: continuous place-and-route in hardware; the single
   hardest engineering unknown (crossing-layer capacity, rubber-band livelock between taut
   wires; needs damping/hysteresis and arbitration studied in E4).
2. **Jamming, deadlock, space exhaustion**: mitigations exist (stochastic moves, vacancy
   diffusion, density admission control) but need amoebot-style proofs or heavy simulation.
3. **The Rent exponent** (§4.3): if typical disp nets are expander-shaped, the pure mesh is
   dead; E1 settles it cheaply.
4. **The light cone** (§4.1): parallelism-hungry programs are polynomially capped regardless
   of cleverness; express links reintroduce non-uniformity and design complexity.
5. **Recognition economics** (§6.4): certified recognition is expensive per firing; it must
   amortize over stable hot structure. Programs that never stabilize never harden.
6. **Positioning**: like `rust-ic-net`, this is an optimizer/runtime substrate, not a checker
   speedup (it gives up hash-consed O(1) conversion; `tree_eq` stays king for checking).

## 10. Relation to the repo's roadmap

- `OPTIMIZER.typ` §4: transport is the missing fourth grade; §9: the CA is the minimal honest
  `run_model`; §3: per-region attribution extends to data movement and, on real hardware, to
  energy metering.
- `RUST_IC_NET_DESIGN.md`: E3 (tiled scheduler) is an M2-adjacent experiment; the wide-block
  T₁/T₂ decision and the `Ref` tag both prefigure Part II's shapes.
- `TYPE_THEORY.typ` §15: effectful machines stay at the driver boundary; pure machines are
  agents.
- Core discipline (`CLAUDE.md`): resident machines are "host implementations are
  optimizations" made physical; `tree_eq` is the first resident machine and its differential
  validation is the certification discipline in miniature.

# Part III — gap analysis toward implementation

Second round, 2026-07-07. Questions addressed: what exactly the scheduler is; how the
optimizer runs *on* the net (net in, net out); how linear resources are represented; whether
machines should be modeled as imperfect graph reducers in the same calculus; and the full
register of what must be ironed out before an implementation attempt.

## 11. How the scheduler actually works

### 11.1 It is not a picker; it is a rate allocator

A scheduler in the usual sense chooses *what to run next*. Confluence removes that question:
every active pair fires eventually and the result is fixed. What geometry controls is
*rates*: how fast each nascent redex approaches contact, and how fast free space reaches
allocation sites. Chemistry is the exact analogy: thermodynamics fixes what reacts,
concentration and diffusion fix how fast. Two consequences worth pinning:

- **Laziness is not the scheduler's job.** Which demands exist at all (which `E` agents are
  seeded) is the strictness policy, lowered from grades by the elaborator
  (`RUST_IC_NET_DESIGN.md` §7), geometry-independent. Geometry only sets latency and
  throughput of the demands that exist. Strict/lazy = which redexes exist; tension = how
  fast they close.
- Under the forwarder formulation (§2.2), "the scheduler" is precisely: *a policy over which
  forwarder eliminations to perform first.* Everything it does is ordinary interactions;
  confluence guarantees any policy is sound.

### 11.2 Three interleaved local processes

1. **Detection.** Two agent cells whose principal ports face each other across an edge are an
   active pair; both enter a committed state and the rewrite executes over staged micro-ticks
   (allocate adjacent free cells, splice wires). Commitment across Margolus block boundaries
   needs a two-tick propose/ack handshake (engineering, not research).
2. **Wire dynamics.** Reel-in: an agent may step into the adjacent cell of one of its wires,
   shortening it by one (one forwarder elimination = one unit of transport), at the price of
   lengthening its other wires by at most one each. Local decision rule: move iff the
   weighted shortening exceeds the weighted lengthening. Straightening: a kinked wire cell
   retracts diagonally into a free corner (length reduction with no agent motion). Slack from
   agent moves diffuses along wires by straightening.
3. **Drift.** Beyond reel-in, an agent computes a force (weighted sum of its wires' first-hop
   directions) and moves with Metropolis acceptance: always if it lowers local weighted wire
   length, with probability exp(-ΔE/T) otherwise, using the deterministic per-cell PRNG.
   Temperature can be locally adaptive (recently active regions run hotter and stay fluid).

Weights encode priority: principal-principal wires (redexes in waiting) high; a producer root
wired to a consumer aux slot (a value someone will eventually demand) medium; aux-aux `Var`
wires low (just bounded). Contracting a principal-principal wire to zero *is* dispatching
that redex.

### 11.3 Congestion, pressure, arbitration

Rewrites needing k free cells make all-or-nothing allocation requests with randomized backoff
(no hold-and-wait). Unmet requests increment a local pressure field; vacancies drift up
pressure gradients (Claytronics hole motion). Two committed rewrites contending for the same
vacancy are resolved by PRNG draw. This gives probabilistic liveness in the style of
randomized dining philosophers, deterministic per seed.

### 11.4 Unresolved scheduler items (the honest list)

1. **Liveness proof or watchdog.** A congestion-pinned redex wire could contract arbitrarily
   slowly; starvation-freedom wants either a potential-function argument (total weighted wire
   length plus pressure integral decreasing in expectation) or an impatience rule (age boosts
   weight). Open; E4's first job.
2. **The weight and temperature schedule** (numbers, adaptivity): pure empirics, E4 knobs.
3. **The commit protocol** written out as a micro-step table (bounded design work).
4. **Layer exhaustion / local rip-up-and-reroute** when straightening needs an occupied
   crossing layer: the continuous place-and-route problem, risk #1 in §9.

## 12. The optimizer on the net: net in, net out

This is `GOALS.md` verbatim ("an optimizer that takes this function and returns a new
function, in the same calculus"). The pieces and their status:

### 12.1 Suspended programs are already data

Tree calculus needs no quotation mechanism: an inert, producer-only subnet (`L/S/F/P`) *is* a
tree, δˢ copies it as syntax (`tc-net.typ` names reflective materialization as δˢ's purpose),
and triage dispatches on it. So for suspensions, `optimize : Tree -> Tree` is an ordinary
tree program and running it is ordinary reduction. No reflection gap exists on this fragment.

### 12.2 The optimizable frontier is the lazy frontier

A *running* net is not a tree: consumers (`E/A/T1/T2`), parked δⁿ, `Var` wires have no
producer reading, and must not (inspecting an in-flight consumer is a race; there is no rule
for it and there should never be). Consequence, which independently re-derives §6.2's
staged-recognition conclusion: **whatever is suspended is data; whatever is running is
opaque.** Online optimization operates on not-yet-demanded suspensions and on definitions
(`Ref`s), never on live activations, which is also what every real JIT does. Laziness is
thereby not just a cost policy but the reflection hook.

### 12.3 The loop as ordinary reduction

The optimizer is an ordinary consumer: receive a suspension root, δˢ-copy for analysis,
compute a rewritten tree plus certificate (analysis and synthesis are triage programs), have
the in-language checker validate the certificate (the walker already runs in-language), emit
the improved suspension on the result wire; the old copy is ε'd. Interposed on a wire, that
*is* the φ-cast; no new kernel mechanism. Interposition points are staging decisions.
Live-capture subtleties vanish by optimizing definitions rather than activations (a `Ref` is
stable and copyable without contending for a live principal wire).

### 12.4 Cost introspection, in-language

The inner loop needs candidate costs without leaving the calculus. The rung ladder
(`OPTIMIZER.typ` §9 note) instantiates as: rung 0, static grades (already type-level); rung
1, a **cost-instrumented self-interpreter** `eval_costed : Tree -> Tree -> (Tree, Nat)`
(instrument Jay's self-evaluator to thread a counter: pure, in-language, slow, and itself a
prime hardening target); rung 2, the spatial model `run_spatial : NetDesc -> (Result,
Transport)` (E4 as a tree program) for transport-aware cost; rung 3, measured, driver-side.
Missing artifact: the costed self-interpreter (a self-contained disp milestone).

### 12.5 The codomain decision

Sometimes the improved program *is a net, not a tree*: explicit sharing (a DAG) or machine
references that a tree cannot spell. v1: emit trees plus grade annotations and let
elaboration re-derive sharing (keeps purity, loses fine control). v2: a `NetDesc` data type
(agents + wires as data) with a `materialize` step at a stage boundary, carrying a small
semantics-preservation obligation. Decide at implementation time; v1 suffices to start.

### 12.6 The holy-grail shape is unchanged

Untrusted proposer + trusted checker + φ, per `OPTIMIZER.typ`; the spatial substrate changes
what "cheaper" means (transport in the ledger) and what "native" means (the optimizer as a
largely self-hardened region of fabric taking suspensions in and emitting suspensions plus
certificates out). `GOALS.md`'s third bootstrap item ("deterministic models of the base
hardware encoded into the type system") is §14 below.

## 13. Representing linear resources

Three layers, none needing new type theory:

- **Semantically: not at all.** In-net machines have pure contracts (a machine whose state is
  observable is an effect and lives at the driver boundary; a machine whose internal state is
  semantically invisible, like a pipeline or cache, is fine). Pure contract means machine
  references copy freely under δ; aliasing is harmless to results. Linearity never enters the
  semantics.
- **Economically: as grades.** Contention is priced, not typed away. Sharper: contention is a
  *derived* readout, roughly usage divided by span, and both grades are already in the ledger;
  worst-case queuing bounds (queue length bounded by usage, wait bounded by queue times
  service time) compose semiring-fashion, with queueing-theoretic refinement left to the
  measured rung. A 1-graded machine ref licenses pinning/dedication; ω-graded means pool,
  replicate, or accept queuing (§6.4 economics).
- **Physically: as area**, priced by the fixed-cost side of the §6.4 amortization inequality.

One genuinely new obligation surfaced here: **machine refs must be sealed.** The certificate
binds `spec_M` to physical machine M; a forgeable ref pointing at the wrong silicon would
break substitution soundness while type-checking fine. So refs are minted only by the φ-cast
(the checker), i.e. the kernel's one protection mechanism, `seal(Σ)`, reappears as the
hardware-residence permit. Machine refs join `hyp_reduce`'s trusted tokens as the second
member of the sealed family.

And the interface must be a five-tuple, not a spec alone:
**`Machine_M = (spec, codec isos, strictness signature, cost curve, area)`.** The codecs are
view/encode isos between the tree encoding and the wire format (the kernel's `functor`
view-iso pattern, reused); the strictness signature records that machines are hyper-strict
(a machine call induces deep demand plus serialization on its inputs, which the cost model
must charge or machines look artificially cheap); the cost curve is §14's model.

## 14. Machines as imperfect graph reducers (modeling silicon in the same calculus)

The question: for coherent cost models of CPU/GPU machines, must we model their internals,
and can the model be the same net formalism? Dissect an out-of-order core in net vocabulary:

- **Register renaming is wire minting.** Each destination register write allocates a fresh
  physical register and rewires consumers to it: exactly fresh `Var` substitution cells. The
  issue queue's tag-broadcast wakeup is the exchange linker's second-arriver-wins.
- **The issue window is a bounded redex bag** (instructions with ready operands = active
  pairs), with pick-any-ready-up-to-width scheduling.
- **Functional units are resident machines** with fixed inventory.
- **The ROB is a linearizer with no net analog**: it exists solely because the ISA promises
  sequential observable semantics (precise exceptions, memory ordering). A confluent net
  never makes that promise, which is precisely why it needs no ROB.
- **Speculation is reduction under an unresolved choice.** Branch prediction reduces one
  branch optimistically and squashes on mispredict; the net-native generalization is `sup`
  (reduce both, share work, collapse on resolution). Both are policies over the same
  structure; the model can express either.

So an OoO core is a small, resource-bounded, eagerly scheduled IC reducer, imperfect in four
ways: bounded window (cannot see far redexes), forced linearization (spurious ordering
edges), speculative (wasted work, squashes), fixed machine inventory. Which yields the
central modeling claim:

**A machine model = the same calculus + a grade profile + a scheduler policy.**
Grade profile: window size = space bound; issue width = span-per-tick bound; FU counts =
per-machine-kind usage-rate bounds; memory = a latency curve over the space grade (working
set), stepped at cache sizes, corrected by the measured rung. Scheduler policy: eager
bounded-window for CPU cores; warp-lockstep with divergence masking for GPU SMs (HVM2's warp
pain becomes a modelable policy); tension-CA for the fabric itself. Confluence is what makes
"one calculus, pluggable scheduler" coherent: every policy computes the same results,
differing only in cost, and cost is the thing being modeled.

Payoffs: (1) `run_model` stops being a foreign cycle simulator and becomes the same reducer
under a coeffect profile, so `GOALS.md`'s "deterministic models of the base hardware encoded
into the type system" gets a uniform construction; (2) whole-system cost = nested grading
(outer fabric transport composed with inner machine makespan, the semiring's multiplication
doing exactly its designed job); (3) the sequential-ISA tax becomes computable inside one
formalism, by comparing the constrained model (with ROB edges) against the unconstrained one
on the same fragment. The answer to "would it help in theory" is yes, and the answer to
"could we model them using the same scheduler" is: same calculus, same grades, *different
policy per silicon class*; the scheduler is the model.

## 15. Gap register

Beyond §9's risks and §11.4's scheduler items:

- **G1 — machine interface five-tuple** (§13): codec isos + hyper-strictness charging are the
  parts nobody has written down. Blocks any machine work, even simulated.
- **G2 — certificates are staging-time artifacts**: the fabric carries only sealed refs;
  spec + cert live in a boundary table for fallback/differential validation. Pin it, cheap.
- **G3 — transport grade: bound vs expectation.** Stochastic migration makes per-run
  transport a sample. Resolution already patterned by the ledger's coeffect/effect dual:
  worst-case-over-admissible-schedules bound for soundness, simulated/measured expectation
  for guidance. Decision, not research.
- **G4 — driver placement**: the one sequential point has a location; transport to it is
  priced; queue discipline outside confluence (already the §15-effects answer). Not blocking.
- **G5 — the embedding simulation theorem**, written out: forwarder ι (principal facing
  upstream) with one rule schema per producer (splice root to `out`); chains collapse
  serially, one elimination per tick = the transport cost; parked δⁿ and RC pulses must
  traverse chains. Routine but must be exact; gates E2/E4.
- **G6 — sup labels on finite-state cells**: bounded label bits = bounded superposition
  nesting per region; compounds with the open label-coordinated-duplication question.
- **G7 — what triggers hardening at runtime**: per-region interaction counters; threshold
  crossing emits an effect value naming the hot `Ref`; driver invokes the hardened optimizer;
  φ-swaps the definition. Optimize definitions, not activations (§12.2). Design settled,
  unbuilt.
- **G8 — substrate self-verification harness**: every micro-step preserves the abstract net
  (shadow-track it in test builds); normal forms bit-identical to rust-eager/rust-ic-net;
  Margolus determinism gives reproducibility. The E4 harness spec.
- **G9 — machine-friendly data representations**: unary Nat is transport poison; HBin/word
  reps via licensed rep-change (the arith.opt overlay pattern), which is a real coercion with
  real cost, not a φ-identity (the HEq caveat in `OPTIMIZER.typ`).
- **G10 — area grade vs fragmentation**: usable area < free area; first-order slack factor,
  refined empirically.
- **G11 — the loader**: compiled trees are trees, hence planar-embeddable; initial placement
  unfolds H-tree-fashion from a boundary pad. Engineering.

## 16. What blocks what

- **E1 (Rent measurement)**: nothing. Buildable today.
- **E2 (transport grade)**: G5 written carefully. Days of design writing.
- **E3 (tiled rust-ic-net)**: nothing conceptual; weight heuristics guessable ahead of E4.
- **E4 (CA simulator)**: the design-complete gate. Needs G5, the commit protocol (§11.4.3),
  pressure/vacancy rules (§11.3), and knob schedules (§11.4.2). Writing the full rule table
  is a bounded task about the size of `tc-net.typ`'s rules section, and forces every
  hand-wave; no unknown-unknowns visible from here.
- **E5 / machines (even simulated)**: G1 (interface five-tuple), sealed refs (§13), and the
  M1 rulebook/checker completion from `OPTIMIZER.typ`'s build order. The recognition search
  can be stubbed with hand-picked subnets; economics first, mining later.
- **The self-hosting loop (§12)**: the costed self-interpreter (new, self-contained disp
  milestone), the codomain decision (v1 trees), M1 completion, and honoring the lazy-frontier
  boundary (design done). Sequencing per `OPTIMIZER.typ`'s build order; nothing here blocks
  on Parts I–II beyond the ledger.

The overall shape: **no conceptual blocker stands; what remains is one theorem to write out
(G5), one protocol table (the commit protocol), one interface definition (G1), one liveness
argument or watchdog (§11.4.1), and empirics (weights, temperatures, Rent).** The largest
single unknown is not on this list at all: it is E1's number.
