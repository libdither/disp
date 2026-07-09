# Spatial IC — interaction combinators on a distance-aware substrate, and resident machines

Research note, 2026-07-07. Companion to `tc-net.typ` (the calculus), `RUST_IC_NET_DESIGN.md`
(the pointer-machine implementation), and `/OPTIMIZER.typ` (the cost ledger this note
extends). Status: design investigation, nothing built. Three literature sweeps ground the
prior-art claims; references in §15.

## 0. Claim and verdict

**The idea.** Run interaction-combinator reduction on a fully parallel cellular substrate,
one you could implement raw in a chip: a lattice of finite-state cells, agents as cell
patterns, wires as physical paths of cells, interactions firing only between adjacent
principal ports. Physical distance is real: information moves at most one cell per tick, and
agents migrate along their connections to shorten wires before reacting. Part II extends the
picture with **resident machines**: fixed-function circuits embedded in the fabric that
consume and produce agents, entered by a certified equivalence rewrite.

**Verdict.** Coherent, with one near-miss precedent (interaction automata / ia2d, FSCD 2016,
§15.1) whose two most interesting ingredients are unclaimed: attraction-driven migration and
distance as a first-class cost grade. Not competitive as a simulated runtime on today's
CPUs/GPUs; strong as (a) the honest hardware-cost model `OPTIMIZER.typ` §4/§9 explicitly
leaves open, (b) a scheduler insight (tiling + migration) applicable to `rust-ic-net` and
HVM2-class GPU reducers now, and (c) a long-range hardware thesis whose substrate class has
shipped silicon precedent (Cerebras-scale meshes). The whole bet hangs on one number, the
effective Rent exponent of real disp reduction traces (§4.3, §10), and the first rung C
measurements now exist (§10.3): four raw-tree-calculus disp workloads fit p ≈ 0.40–0.52, at
or below the 2D boundary; the kernel-elaborated class (checker walks, kernel arithmetic)
exceeds what the no-memo backend can trace today and stays open.

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

## 2. Where distance lives

Two candidate designs were on the table: distance as an integral part of the elimination
semantics, versus agents migrating automatically to minimize distance. The coherent answer
uses both, at different layers, and rejects one specific corner.

### 2.1 Rules stay distance-blind (the rejected corner)

If rewrite rules can observe distance (branch on near versus far, race, time out), the system
contains `amb` and strong confluence is destroyed, which `OPTIMIZER.typ` §3 correctly marks as
sacred: it is what makes parallelism free, results schedule-independent, and the
differential-oracle backend discipline possible. Rule *outcomes* never depend on geometry.

### 2.2 Distance in the semantics: the embedding and the transport grade

The clean formulation is a **geometric embedding**. Extend TC-Net with a unary forwarder
agent ι(principal `p`, aux `out`) and one rule schema, ι against any producer root C: rebuild
C with its principal on `out` (operationally a splice; under hash-consing a pointer move).
Chains of ι collapse serially, one interaction per element. An embedding is then a placement
π of agents onto cells of Z² (or Z³) plus layer-capacity-respecting paths for wires; the
embedded net E_π(N) replaces each wire by an oriented ι-chain of its path length. (ia2d
independently validates the representation: its wires are literally paths of forwarder
cells.)

**Theorem (simulation and cost decomposition)**, now written out in `EMBEDDING_THEOREM.md`
(the polarity lemma and created-short lemma proven by case analysis; the projection invariant
proven; drift charged via the ι-count accounting identity). The formal core of the design, in
outline:

1. *Simulation.* N → N′ in TC-Net iff E_π(N) →* E_π′(N′) in TC-Net+ι where the extra steps
   are ι-eliminations; readback (erase ι) is a bisimulation up to transport. Normal forms
   correspond exactly.
2. *Cost.* interactions(embedded run) = interactions(abstract run) + transport, where
   transport is the sum over value traversals of path lengths. The transport grade is thereby
   *defined*: it is the ι-count, an ordinary interaction count on the embedded net. One
   ledger, no second currency.
3. *Confluence.* ι has one principal port and the rule table stays functional, so Theorem 2's
   proof goes through unchanged; any firing policy (including §2.3's tension scheduling)
   computes the same normal form.
4. *Lower-bound counterpart.* For net families with bisection b(n), any embedding satisfies
   transport = Ω(b(n) · distance terms) (Thompson's cut argument, §4.2), so the model is
   tight: the grade cannot be defined away by a cleverer embedding.

Four points in the proof need actual care:

- **Orientation needs polarity.** ι's principal must face the end a value arrives from,
  which is well-defined only if every TC-Net wire has a value direction. Claim to verify:
  TC-Net is fully polarizable (producers emit at principal; E/A/T₁/T₂ consume at principal
  and emit at `res`; `arg`/branch slots consume; δ consumes at principal, emits at l/r; ε
  consumes). Every wire then connects one out-port to one in-port and chains orient uniformly
  at creation. A small typed-interaction-nets lemma (the linear-logic reading in `tc-net.typ`
  §Additives suggests it holds), and load-bearing: an unpolarizable wire would deadlock its
  chain.
- **Parked δⁿ across chains.** A δⁿ parks on a result wire that is now a chain; the arriving
  producer collapses the chain first, then meets the δⁿ. Fine, but the RC/GC back-pulse (§3)
  travels *against* polarity; cleanest is to make RC pulses substrate-level signals (like
  pressure), keeping the theorem about the agent layer only.
- **π is partial and dynamic.** Crossing/layer capacity makes some placements infeasible
  (where Rent enters, §4.3); migration re-embeds during reduction. The induction invariant:
  *at every tick the substrate state projects to a well-formed abstract net, and every
  micro-step projects to identity or exactly one abstract interaction.* That sentence is
  simultaneously the theorem's invariant and the simulator's test assertion (§13 item 6): the
  proof and the harness are the same statement.
- **Rewrite locality (created-short, grow-by-splice).** Each TC-Net rule's RHS wires only
  among the two dying agents' ports plus O(1) fresh agents, so a rewrite never creates a long
  wire; wires lengthen only by splicing (Var elimination fusing two chains) and by δ copying
  an existing long wire. Worth stating as a lemma, because it is why tension has a chance:
  geometry degrades only through identifiable, chargeable events.

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

### 2.3 Migration in the implementation: tension as a rate allocator

A scheduler in the usual sense chooses *what to run next*. Confluence removes that question:
every active pair fires eventually and the result is fixed. What geometry controls is
*rates*: how fast each nascent redex approaches contact, and how fast free space reaches
allocation sites. Chemistry is the exact analogy: thermodynamics fixes what reacts,
concentration and diffusion fix how fast. Two consequences worth pinning:

- **Laziness is not the scheduler's job.** Which demands exist at all (which `E` agents are
  seeded) is the strictness policy, lowered from grades by the elaborator
  (`RUST_IC_NET_DESIGN.md` §7), geometry-independent. Strict/lazy = which redexes exist;
  tension = how fast they close.
- Under §2.2's forwarder formulation, "the scheduler" is precisely a policy over which
  forwarder eliminations to perform first. Everything it does is ordinary interactions;
  confluence guarantees any policy is sound, so the whole layer can be iterated empirically
  with zero soundness risk.

Mechanically, three interleaved local processes:

1. **Detection.** Two agent cells whose principal ports face each other across an edge are an
   active pair; both enter a committed state and the rewrite executes over staged micro-ticks
   (allocate adjacent free cells, splice wires). Commitment across Margolus block boundaries
   needs a two-tick propose/ack handshake.
2. **Wire dynamics.** Reel-in: an agent may step into the adjacent cell of one of its wires,
   shortening it by one (one forwarder elimination = one unit of transport), at the price of
   lengthening its other wires by at most one each; move iff the weighted shortening exceeds
   the weighted lengthening. Straightening: a kinked wire cell retracts diagonally into a
   free corner. Slack from agent moves diffuses along wires by straightening.
3. **Drift.** Beyond reel-in, an agent computes a force (weighted sum of its wires' first-hop
   directions) and moves with Metropolis acceptance: always if it lowers local weighted wire
   length, with probability exp(-ΔE/T) otherwise, using a deterministic per-cell PRNG
   (hash of position, tick, seed — runs stay bit-reproducible for the differential oracle).
   Temperature can be locally adaptive (recently active regions run hotter and stay fluid).

Weights encode priority: principal-principal wires (redexes in waiting) high — contracting
one to zero *is* dispatching that redex; a producer root wired to a consumer aux slot
medium; aux-aux `Var` wires low (just bounded). The stochasticity is not optional: the
programmable-matter literature (§15.3) shows deterministic greedy descent provably deadlocks
in local minima while Metropolis-style moves provably compress (amoebot compression, PODC
2016, with a genuine phase transition in the bias parameter), and dense lattices should move
*vacancies* rather than agents (Claytronics hole motion).

Congestion and arbitration: rewrites needing k free cells make all-or-nothing allocation
requests with randomized backoff (no hold-and-wait); unmet requests increment a local
pressure field; vacancies drift up pressure gradients; contended vacancies resolve by PRNG
draw. This gives probabilistic liveness in the style of randomized dining philosophers,
deterministic per seed. What remains open in this layer is collected in §14: a liveness
argument or impatience watchdog, the weight/temperature schedule, the commit protocol table,
and layer-exhaustion rerouting.

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

Donath's result, generalized to substrate dimension d: after good hierarchical placement,
average wire length stays bounded iff the Rent exponent p < 1 − 1/d, and grows as g^(p−0.5)
above it in 2D. Real placed logic sits at p ≈ 0.5–0.7. The decision bands:

- **p < 0.5**: the 2D mesh thesis holds outright.
- **p in 0.5–0.65**: 2D is strained (at p = 0.6 and 10⁸ agents, a ~6× stretch factor);
  modest express-channel augmentation or 2.5D/3D absorbs it (3D is comfortable up to
  p < 2/3).
- **p > 0.7**: expander-shaped; the pure mesh dies and only the tiled/express designs
  survive.

**Whether disp reduction traces have effective p below or above 0.5 decides whether the
spatial bet is sound, and no one had measured it for interaction nets.** First measurements
landed 2026-07-08 (§10.3, rung C): four raw-tree-calculus workloads sit at p = 0.40–0.52,
two below 0.45 outright (fib, a self-application) and two at the boundary (exponentiation,
merge-sort at ≈ 0.51), none anywhere near the death band. The expected heterogeneity is real
(0.40 vs 0.52 across classes) but spans a tenth, not bands. The kernel-elaborated class
(checker walks, kernel arithmetic) is still unmeasured (§10.3 finding 4) and is where the
remaining empirical exposure concentrates; report per-workload stays the rule.

None of this is a defect. Every physical machine pays these costs; this design is the one
where the optimizer can *see* them. The no-memo substrate already gives per-decision cost
attribution (`OPTIMIZER.typ` §3); a spatial substrate extends attribution to data movement,
which is where the energy actually goes. Endgame reading: on hardware of this shape,
per-candidate cost becomes per-region power draw, and `GOALS.md`'s measurement primitive
becomes an energy meter.

# Part II — resident machines

The substrate so far is uniform. Real chips are not: they are mostly interconnect and memory,
with dedicated function units set about in space. This part develops the corresponding move
here: special machines embedded in the fabric that consume and produce agents, and the claim
that entering one is an act of *recognition*.

## 5. The three-layer resolution

The intuition "optimization = the net recognizing that one of its own processes is equivalent
to a special machine, swapping it for the dedicated circuit, and leaving the composition of
circuits to the agents" is coherent once split into three layers with different characters.
The unease it provokes traces to wanting layer 2 to be emergent like layer 3; it cannot be,
and the repo's architecture already says where it lives instead.

### 5.1 Machines are just agents (standard, already practiced)

A resident machine presents to the net as an ordinary agent: **one principal port**, a finite
set of aux ports, and a declared interaction contract. Internally it can be anything (a
pipelined multiplier, a matmul array, a whole CPU); externally it is one agent with large
latency, and latency is semantically invisible (interactions were never unit-time on the
spatial substrate anyway; confluence does not care about durations). The one-principal-port
constraint is load-bearing: it preserves the ownership argument (`RUST_IC_NET_DESIGN.md` §2,
"never introduce a multiport agent") and Theorem 2.

This is not exotic. It is how every practical IC runtime already smuggles arithmetic in:
HVM2's native number and operator agents are machines in exactly this sense (an "interaction"
that is an ALU op, not a net rewrite). `rust-ic-net` reserves a `Ref` tag for the same slot.
Lafont's package/box construction (a subnet with free ports treated as one agent) is the
formal license. And the project already operates one resident machine today: **the `tree_eq`
native fast-path in `src/core/tree.ts`**, a host-native implementation semantically mirroring
in-language code, validated differentially against the in-language reference. Multi-argument
strictness is the standard currying dance (HVM2's OP agents); demand chains supply the strict
scheduling.

Two obligations sharpen the interface:

- **Machine refs must be sealed.** The certificate binds `spec_M` to a specific physical
  machine; a forgeable ref pointing at the wrong silicon would break substitution soundness
  while type-checking fine. Refs are minted only by the φ-cast (the checker), i.e. the
  kernel's one protection mechanism, `seal(Σ)`, reappears as the hardware-residence permit.
- **The interface is a five-tuple**, not a spec alone:
  `Machine_M = (spec, codec isos, strictness signature, cost curve, area)`. Codecs are
  view/encode isos between the tree encoding and the wire format (the kernel's `functor`
  view-iso pattern, reused). The strictness signature records that machines are hyper-strict:
  a call induces deep demand plus serialization on its inputs, which the cost model must
  charge or machines look artificially cheap. The cost curve is §7's model. §6.1 shows this
  five-tuple is an effect signature plus four fields.

### 5.2 Recognition is staged and certified, never emergent

The step "this subnet is equivalent to machine M" **cannot be an in-dynamics local rule**.
Extensional equivalence is not locally observable: a subnet's behavior is a global,
asymptotic property, and local rewrites only implement the calculus's own syntactic
reductions. (§8.2 re-derives the same boundary from what reflection can even see.) Where it
lives instead is already built, in pieces, in this repo:

- **Machine-equivalence is a type.** Types are predicates here; so
  `Machine_M := { t : Tree | t ~ spec_M }` where `spec_M` is the machine's specification as a
  tree program (`OPTIMIZER.typ` §9: a hardware model is a tree program; `Refines spec asm`).
  Recognition is membership checking through the walker; the membership witness is the
  certificate. Residence on dedicated hardware is a privilege granted by proving membership.
- **The swap is φ.** Replacing the subnet by a reference to the machine is exactly the
  `OPTIMIZER.typ` §5/§7 verified rewrite at the codegen corner of the staging axis, executed
  at a stage boundary (region quiescence / JIT pause), licensed by the certificate, checked
  by the trusted checker. The guard layer's licensed rebind (`license_guard` payload
  `{ new := machine_ref; proof := oeq_witness }`) is the module-level rehearsal of the same
  act.
- **Targeting is provenance.** The no-memo substrate attributes cost per region; a hot,
  recurring subnet is visible in the trace (on physical hardware: literally as a warm
  neighborhood). Untrusted search proposes candidates (pattern mining, e-graphs, neural
  proposal per `GOALS.md`); the checker disposes. Recognition need not be complete, only
  profitable where it fires.
- **Precedent stack** (§15.4): compiler idiom recognition, verified lifting, and certified
  hardware compilation are the software and hardware halves of exactly this step, none of
  them coupled to a rewriting substrate.

So "self-recognition" is right in the reflective sense (the optimizer is a tree program
recognizing tree programs; triage makes programs data) and wrong in the emergent-chemistry
sense. Staged self-recognition, not in-dynamics self-recognition.

### 5.3 Scheduling stays in the net; linearity is priced, not typed

Once machines exist in space, the residual computation is: route arguments to machines,
sequence dependent uses, arbitrate contention. That residual is a dataflow problem, and the
net *is* the dataflow graph: demand chains + wire tension + queuing at machine ports already
constitute a distributed, asynchronous scheduler, sound under any arbitration order
(confluence: queueing affects latency, never results) and priced by the transport grade.

The interesting new constraint: **a physical machine is a linear resource**. A δ meeting a
machine reference copies the *reference* (both copies point at the same silicon, i.e.
contention), never the machine. Three layers of representation, none needing new type theory:

- *Semantically: not at all.* In-net machines have pure contracts (observable state makes a
  machine an effect, §6.3), so refs copy freely and aliasing is harmless to results.
- *Economically: as grades.* Contention is a derived readout, roughly usage divided by span,
  and both grades are already in the ledger; worst-case queueing bounds (queue length bounded
  by usage, wait by queue times service time) compose semiring-fashion, with real queueing
  behavior left to the measured rung. A 1-graded machine ref licenses pinning/dedication;
  ω-graded means pool, replicate, or accept queuing. This also connects to the
  `OPTIMIZER.typ` open question on the affine fragment as a decidable search island: hardware
  linearity and type linearity meet in the same fragment.
- *Physically: as area*, priced by the fixed-cost side of §5.4's amortization inequality.

Note the von Neumann inversion: no "memory machine" is needed, because the fabric *is* the
memory (the net is its own store); machines are the exception, storage the default.

### 5.4 Grain, placement, and why Parts I and II need each other

A machine pays off iff (invocations × per-call saving) exceeds (fixed area + transport to
reach it). Small machines (adders) are only worth having everywhere inline, which is HVM2's
native-number choice; large machines (matmul, FFT) are worth traveling to. Placement, count,
and grain are therefore optimization variables, and they are gradient-visible **only** with
the transport grade: without distance in the ledger, "call the machine" looks free and the
optimizer hallucinates speedups. Part I's cost model and Part II's machines are one design.

### 5.5 Existing chips, reread through this lens

A modern SoC/GPU is this architecture seen through a glass, darkly, with every layer present
but uncertified and frozen:

- **ALUs and the ISA** are machines whose recognition step ran once, at instruction-set design
  time: frozen idioms, certificates replaced by a specification document.
- **Caches** are automatic, heuristic, uncertified locality management: migration without a
  ledger (and with no way to say "this wire is principal-principal, contract it first").
- **The out-of-order window** is a tiny, hidden, energy-expensive dataflow net over a few
  hundred instructions: emergent scheduling, but trapped behind a sequential ISA and rebuilt
  from scratch every cycle. §7 makes this a formal statement, not a metaphor.
- **SoC accelerators + drivers** are resident machines whose composition glue is imperative,
  effectful, and unverified: scheduling done by the layer least suited to it.
- **Cerebras-class meshes** are the substrate with the machines and the calculus left out.

The proposal, stated as a re-plumbing: make locality management explicit (tension/migration
under a ledger), make recognition ongoing and certified (φ with certificates, instead of
once-at-ISA-design), and let scheduling be the net itself at whole-program scale (instead of a
200-instruction window). Each layer exists in shipped silicon; what has never shipped is the
three of them made semantic.

### 5.6 The loop this closes

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

## 6. Machines and effect systems

`TYPE_THEORY.typ` §15: effects are a free monad `Eff R X` over an operation-signature row R,
interpreted by deep handlers, one impure driver at the boundary. Machines connect to this at
every level.

### 6.1 A machine is a handler that got hardened

A machine call has the *shape* of an effect operation: an opaque, named, typed
request/response performed at a boundary, continuation resumed after. The difference is
discipline: an operation's meaning comes from whichever handler the context installs; a
machine's meaning is fixed by certificate. Read in both directions:

- **An effect row is the interface of a machine you have not built yet.** A program written
  against row R is parametric in the machines implementing R's operations.
- **Handlers are software machines; resident machines are hardened handlers.** The pipeline
  is the staging axis end to end: interpret (the handler runs as soft net) → specialize
  (handler fused into the program, the Futamura step) → reside (the handler is silicon,
  entered by sealed ref). Each step is the same φ-rewrite at a colder stage.

This means §5.1's machine interface five-tuple is not a new kind of thing: it is *an effect
signature plus codecs, strictness, cost curve, and area*. Effects are the natural seam along
which hardening cuts, because operations are exactly the program points with stable, named,
typed interfaces — which is what a physical port needs.

### 6.2 Rows are capability wiring; grades on rows are contention

At the fabric level, an entry in a region's row corresponds to a physical *route* to a
machine instance implementing that operation. Row containment (the effects arc's
weakening-is-containment) becomes reachability; a row is a wiring budget. Graded rows (how
often each operation fires) are precisely §5.3's contention economics: usage-per-span on the
op determines queueing at the machine, pool replication, or pinning. The effect system and
the machine economy share one accounting substrate.

### 6.3 The linearity bridge: when stateful silicon keeps a pure contract

§5.3 said observable state makes a machine an effect. The refinement: **a stateful machine is
pure iff its state is linearly threaded**, and the usage grades already express linearity.
A DRAM bank with an alias-free (1-graded) handle is semantically a pure store being passed
through — the classic uniqueness-typing move — so it can live in-net as a certified machine
despite being mutable silicon, with in-place update as the implementation of a pure
contract. Effects that can be linearized become machines; effects that cannot (true
nondeterminism, wall-clock time, the outside world) stay at the driver. The driver is then
just *the machine of last resort*: the unique, sequential, effectful cell at the boundary,
whose row is the ambient capability set.

### 6.4 Two closing symmetries

- The effects prototype's finding that `handle` needs a handler-supplied *forward clause*
  (operations outside the handled row pass through) has a literal fabric reading: the
  forward clause of a hardened handler is a wire — routing for ops this machine does not
  serve, onward to machines or driver that do.
- `GOALS.md`'s measurement primitive ("outsource execution, get output + time + memory
  back") is an effect op whose handler is the driver; incurred cost (`OPTIMIZER.typ` §4's
  effect face) flows back through the same row plumbing that routes the call. The coeffect
  face (the bound) rides the type; the effect face (the bill) rides the row.

## 7. Machine models: the same calculus under a grade profile

For coherent cost curves (§5.1's fifth field), machines with internal dynamics need models.
Dissect an out-of-order core in net vocabulary:

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
differing only in cost, and cost is the thing being modeled. The scheduler *is* the model.

Payoffs: (1) `run_model` stops being a foreign cycle simulator and becomes the same reducer
under a coeffect profile, so `GOALS.md`'s "deterministic models of the base hardware encoded
into the type system" gets a uniform construction; (2) whole-system cost = nested grading
(outer fabric transport composed with inner machine makespan, the semiring's multiplication
doing exactly its designed job); (3) the sequential-ISA tax becomes computable inside one
formalism, by comparing the constrained model (with ROB edges) against the unconstrained one
on the same fragment.

## 8. The optimizer on the net: net in, net out

This is `GOALS.md` verbatim ("an optimizer that takes this function and returns a new
function, in the same calculus"). The pieces and their status:

### 8.1 Suspended programs are already data

Tree calculus needs no quotation mechanism: an inert, producer-only subnet (`L/S/F/P`) *is* a
tree, δˢ copies it as syntax (`tc-net.typ` names reflective materialization as δˢ's purpose),
and triage dispatches on it. So for suspensions, `optimize : Tree -> Tree` is an ordinary
tree program and running it is ordinary reduction. No reflection gap exists on this fragment.

### 8.2 The optimizable frontier is the lazy frontier

A *running* net is not a tree: consumers (`E/A/T1/T2`), parked δⁿ, `Var` wires have no
producer reading, and must not (inspecting an in-flight consumer is a race; there is no rule
for it and there should never be). Consequence, which independently re-derives §5.2's
staged-recognition conclusion: **whatever is suspended is data; whatever is running is
opaque.** Online optimization operates on not-yet-demanded suspensions and on definitions
(`Ref`s), never on live activations, which is also what every real JIT does. Laziness is
thereby not just a cost policy but the reflection hook.

### 8.3 The loop as ordinary reduction

The optimizer is an ordinary consumer: receive a suspension root, δˢ-copy for analysis,
compute a rewritten tree plus certificate (analysis and synthesis are triage programs), have
the in-language checker validate the certificate (the walker already runs in-language), emit
the improved suspension on the result wire; the old copy is ε'd. Interposed on a wire, that
*is* the φ-cast; no new kernel mechanism. Interposition points are staging decisions.
Live-capture subtleties vanish by optimizing definitions rather than activations (a `Ref` is
stable and copyable without contending for a live principal wire). What triggers hardening at
runtime: per-region interaction counters; a threshold crossing emits an effect value naming
the hot `Ref`; the driver invokes the hardened optimizer; φ swaps the definition.

### 8.4 Cost introspection, in-language

The inner loop needs candidate costs without leaving the calculus. The rung ladder
(`OPTIMIZER.typ` §9 note) instantiates as: rung 0, static grades (already type-level); rung
1, a **cost-instrumented self-interpreter** `eval_costed : Tree -> Tree -> (Tree, Nat)`
(instrument Jay's self-evaluator to thread a counter: pure, in-language, slow, and itself a
prime hardening target); rung 2, the spatial model `run_spatial : NetDesc -> (Result,
Transport)` (§13 as a tree program) for transport-aware cost; rung 3, measured, driver-side.
Missing artifact: the costed self-interpreter (a self-contained disp milestone).

### 8.5 The codomain decision

Sometimes the improved program *is a net, not a tree*: explicit sharing (a DAG) or machine
references that a tree cannot spell. v1: emit trees plus grade annotations and let
elaboration re-derive sharing (keeps purity, loses fine control). v2: a `NetDesc` data type
(agents + wires as data) with a `materialize` step at a stage boundary, carrying a small
semantics-preservation obligation. v1 suffices to start.

### 8.6 The holy-grail shape is unchanged

Untrusted proposer + trusted checker + φ, per `OPTIMIZER.typ`; the spatial substrate changes
what "cheaper" means (transport in the ledger) and what "native" means (the optimizer as a
largely self-hardened region of fabric taking suspensions in and emitting suspensions plus
certificates out). Certificates and specs are staging-time artifacts: the fabric carries only
sealed refs, with spec and cert retained boundary-side for fallback and differential
validation.

# Part III — roadmap

## 9. Positioning: what to build on which hardware

Simulating the literal CA on a GPU is not a performance play: agents crawl one cell per tick
where a pointer write teleports; most cells idle every tick; sparsifying the simulation
converges back to an event-driven pointer machine. The literal CA is an *instrument* (§13).
The version with near-term teeth is the **tiled middle ground** (§12): the CA with big smart
cells, buildable on shipped silicon. The two artifacts sit on one axis (embedding
granularity) but answer different questions:

- **Tiled** is a *performance* artifact: coarse embedding (10³–10⁶ nodes per tile), pointer
  semantics inside a tile (intra-tile wiring is O(1) and unmodeled), transport exists only at
  tile boundaries, scheduling is allocation bias + queues. Goal: real speedup on shipped
  silicon plus coarse validation of the locality thesis. Failure is cheap and informative.
- **CA simulator** is a *semantics and cost* artifact: unit embedding, everything is
  transport, speed irrelevant. Goals: the reference `run_model` giving the transport grade
  its operational meaning, validation of the fine dynamics tiles abstract away (tension,
  jamming, vacancy diffusion, liveness — relevant only to real cellular hardware), and the
  differential oracle for any future device.
- What transfers: the allocation-bias policy is the coarse shadow of tension; tile-boundary
  traffic is the coarse shadow of bit-meters; §10 informs both. What does not: the CA's knob
  tuning lives in a regime the tiled reducer never enters.

The ladder, each level cheap relative to the next and gating it: (1) measure the Rent
exponent (§10); (2) formalize the transport grade (§11); (3) the tiled reducer (§12); (4) the
CA simulator (§13); (5) an FPGA cell array (small lattice, 10³–10⁵ cells direct or more
time-multiplexed à la CAM-8; the raw-chip existence proof, only after the simulator
stabilizes the cell state machine; the Vine community's WIP "Tendrils Compute" FPGA target
suggests others smell the same opportunity); (6) ASIC/wafer only if everything upstream says
yes. On such hardware the design stops being exotic: Cerebras WSE-3 (900k cores, 48 KB each,
single-cycle neighbor links, no caches or coherence) is the substrate class with the
machines and the calculus left out, and a thousand-plus net nodes fit per tile.

## 10. E1 — the Rent measurement

### 10.1 The right object is the canonical interaction DAG

Rent's rule is a property of a static circuit; a reducing net is a movie, not a frame. But
strong confluence hands us a canonical static object: by permutation equivalence, the *set*
of interactions performed in reducing a program to normal form is schedule-invariant, and so
are the produced/consumed dependencies between them. Define the **interaction DAG**: nodes =
interactions, edges = "interaction j consumed a port that interaction i produced." This DAG
is the computation's intrinsic communication structure, independent of any embedding,
schedule, or allocator, and it lower-bounds every physical realization. Its effective Rent
exponent is the number the whole design hangs on.

A care point when reading the DAG's p against §4.3's bands: Donath's law is a statement
about a static circuit laid out all at once, while the interaction DAG is a spacetime
object, and a physical mesh reuses each region for temporally separated interactions. The
exponent transfers as a per-level bound on the blocks any placement must form (Donath's
argument applies level by level), but the sharp physical constraint is bandwidth:
dependencies crossing a balanced spatial cut per unit of schedule depth (rung C's traffic
bisection) against the mesh's O(√A) cut capacity. So read two numbers per workload: p for
whether wire length stays bounded as blocks grow, and traffic/depth for whether required
bandwidth fits the cut. A workload with high p but low parallelism under-fills the mesh
rather than overloading it; the static bands are conservative in that direction, so a
band failure prompts the bandwidth check before a verdict, never instead of one.

### 10.2 Three measurement rungs

- **Rung A, hours: address-distance histogram.** In the reduce loop, log
  floor(log2(|addr(a) − addr(b)|)) per popped redex, plus the slot distance per `link()`.
  Caveat: this measures one particular embedding (allocation order), which a bump allocator
  makes meaningful, but reuse and frees pollute it. Asymmetric evidence: mostly-short
  distances *confirm* embeddability (allocation order is itself a witness embedding); long
  distances refute nothing. Cheap first signal only. Built: node-distance and var-chain
  histograms in `rust-ic-net`'s tracer, dumped as a JSON sidecar per run.
- **Rung B, days: snapshot Rent fit.** Every N interactions dump the live net as a graph;
  run recursive balanced partitioning (KL/FM or METIS-style); at each level record part size
  g and boundary count T; fit log T against log g for the slope p. Also compute bisection
  width growth across snapshots, and one 2D spectral/force layout per snapshot to read the
  realized wire-length distribution directly. Not built (rung C subsumed it for the first
  pass).
- **Rung C, the real one: partition the interaction DAG.** Log every interaction with its
  consumed/produced port ids (10⁷–10⁸ events is fine offline); build the DAG; same recursive
  partition and fit. Schedule-free and embedding-free, and it additionally yields the
  work/span profile (depth = span) and the *traffic bisection*: dependencies crossing a
  balanced cut per unit depth, which is the bandwidth a mesh must supply. Built and run
  (§10.3): tracer in `evaluators/rust-ic-net/crate/src/trace.rs` (opt-in on the sequential
  drain; `-trace` on `ic-net-cli`), analyzer in `crate/src/bin/rent.rs`, drivers in
  `bench/e1-workloads.ts` + `bench/e1-run.sh` + `bench/e1-summary.sh`.

Decision bands in §4.3. Workload set as originally scoped: a kernel self-verification run,
nat/HBin arithmetic benches, list and stream std tests, and a sup-prototype search trace. As
realized (§10.3): the raw-tree-calculus tier landed (recursive fib, exponentiation,
merge-sort, a self-application); the kernel-elaborated tier (membership checks, nat_rec
arithmetic, tree_eq conversion) exceeded the traceable range (finding 4), which also rules
out kernel self-verification and sup traces at this rung until the same unblocks land.

### 10.3 First measurements (rung C, 2026-07-08)

Method, compressed. The tracer records one event per interaction with in-edges at two
grains, both real communication in an embedding: *flow* (the interaction that last moved
each side of the redex into contact, threaded through the exchange linker's parked cells)
and *birth* (the interaction that allocated each consumed node's cells). Ids are assigned in
firing order by the sequential drain, so the log is a topological order and any prefix is a
downward-closed sub-DAG: big traces are fit on an exact prefix, never a sample. The analyzer
builds the deduped undirected dependency graph, recursively bisects it (multilevel
heavy-edge matching, BFS seed, FM refinement with rollback), counts each block's
full-graph external edges (the Landman–Russo T), and fits log2 mean T against log2 mean g
across levels. Calibration brackets the trust region: torus grid2 fits 0.523 (theory 0.5),
grid3 0.668 (theory 2/3), binary tree 0.10, chain −0.03, random 3-regular 0.90 (an
expander, theory ≈ 1). Near-exact inside the decision band, slightly compressed at the
extremes.

| workload | events (fit window) | span | avg par | traffic/depth | p | r² |
|---|---|---|---|---|---|---|
| size(size) | 439 K (full) | 894 | 491 | 1.20 | 0.413 | 0.973 |
| exp(5) | 2.9 M (full) | 2 055 | 1 435 | 2.42 | 0.518 | 0.999 |
| fib(14) | 40 M prefix of 69 M | 3 181 | 12 575 | 0.54 | 0.404 | 0.951 |
| merge-sort(32) | 6 M prefix of 85 M | 2 957 | 2 029 | 0.82 | 0.511 | 0.997 |

Findings:

1. **Every measurable workload sits at p = 0.40–0.52**: two below 0.45 outright, two at the
   2D boundary, none near the p > 0.7 death band. The heterogeneity §10.2 predicted is real
   but spans a tenth, not bands. First contact favors the mesh thesis.
2. **Bandwidth is nowhere near binding.** Traffic/depth is 0.5–2.4 crossing dependencies per
   unit of depth, orders of magnitude inside any plausible mesh's cut capacity at these
   scales; p is the operative readout so far (§10.1's two-number rule).
3. **The interaction economy is mostly transport-shaped already.** δⁿ + ε dominate every
   trace (97% of size(size); a 50/50 split of merge-sort's prefix), with dispatch (A/T1/T2)
   under 1%. Real programs spend the net's time on demand-copy and erasure wavefronts, both
   spatially local by construction (§1), consistent with the low exponents.
4. **The kernel-elaborated class is out of rung C's reach on this backend.** One
   `param_apply Bool true` membership check exceeds 2×10⁸ interactions (so do `param_apply
   Nat n` and 24-layer `nat_rec` addition), and in-language `tree_eq` over a 12.5 MB
   kernel term strands more than 2.7×10⁸ live cells (the parked-δⁿ leak; wire-RC is the
   standing M2 item, `RUST_IC_NET_DESIGN.md` §5). Two readings. As a measurement gap: the
   checker-shaped exponent, the one §4.3 guessed "plausibly tree-local", stays open until
   wire-RC lands plus either a native tree_eq machine on the net or a leaner check path. As
   positioning: §14.4 item 5's "not a checker speedup" is now an empirical bound (a
   membership check that costs ≥ 2×10⁸ interactions here is ~10⁴ steps on the hash-consed
   backend), quantifying the memo dividend by its absence.
5. **Read the numbers with their error bars.** Four workloads, 10⁵–10⁸ events, one seed,
   prefix windows on the two big traces, dependencies deduped per pair, and a partitioner
   that reads ~0.02 high at 0.5 and ~0.1 low at 1.0. The band verdict survives all of that;
   a p = 0.65 workload would not hide.

## 11. E2 — the transport grade in the ledger

The content is §2.2 (the embedding theorem defines the grade as an interaction count); the
task is integration: write the fourth grade (work, span, space, transport/bit-meters) into
`OPTIMIZER.typ` §4's semiring, with the bound/incurred split resolved as the ledger already
patterns it — the coeffect face is a worst-case over admissible schedules (composable, loose
under annealing), the effect face is the simulated or measured sample. Days of design
writing; makes §5.4's machine placement expressible at all.

## 12. E3 — the tiled reducer

### 12.1 CPU: placement at birth, not migration

A pointer arena cannot cheaply move a node (ports are one-directional; no back-pointers), so
literal migration is off the table. The realization that makes tiling implementable: **nets
churn**. Agents are short-lived; every rewrite kills two nodes and births up to four, and the
births are placeable. Tension becomes an *allocation policy*: place each new agent in the
tile its principal wire points into. The population then relocates by generational turnover,
zero copies moved. Concretely:

- Arena split into T tiles (one per core or L2 slice), each with its own bump region,
  freelist, and redex bag; worker threads pinned.
- `link()` unchanged (the exchange linker is address-agnostic); when it detects a redex,
  route it to the tile owning the consumer node. Same-tile redexes are the fast path;
  boundary redexes go to the neighbor's bag.
- In-place node reuse (a real rust-ic-net win) is kept for tile-internal rewrites and traded
  away at boundaries, where new nodes allocate in the *target* tile.
- Work stealing demoted to idle-fallback, and made mesh-shaped: steal from adjacent tiles
  first, and steal *boundary* redexes preferentially (stealing interior redexes imports cold
  cachelines).
- Metrics: cross-tile interaction fraction, LLC misses per interaction, throughput vs the
  work-stealing baseline, and tile-boundary traffic (a live coarse Rent readout
  cross-checking §10).

### 12.2 GPU: persistent regions

HVM2 §8.2 already proved the two-level version (block-local shared-memory buffers, a LEAK op
for escaping edges, 4.2× from locality alone) but its buffers are transient scratch. The
tiled generalization: persistent-kernel blocks each *own* a region (4–12k nodes in 48–228 KB
shared memory); intra-region redexes never touch global memory; boundary wires are global
Var cells; per-region boundary queues accumulate; periodically a region picks the neighbor it
shares the most boundary redexes with and processes the interface, and a low-frequency
repartition/compaction kernel re-tiles the net (coarse-grain annealing). The delta vs HVM2 is
persistence + boundary-minimizing placement, so the shared-memory hit rate compounds instead
of resetting per launch.

### 12.3 Combo CPU+GPU

Route *regions* by redex-density profile: bushy, duplication-heavy regions (wide parallel
fronts) to GPU blocks; deep sequential demand spines to CPU tiles (latency cores win there).
This is a two-cell coarse mesh whose transport cost is the PCIe/NVLink cliff, priced by the
same transport grade; only long-lived residency amortizes it, so the policy knob is
redexes-per-node over a trailing window with hysteresis. It is also the first live instance
of Part II's economics: the GPU is a resident machine good at wide fronts, the CPU one good
at deep chains, and the router is §5.3's scheduler in miniature. Build order: CPU tiles first
(pure crate work under the existing Session ABI + differential oracle), GPU tier after.

## 13. E4 — the CA simulator, build spec

What "effective" requires:

1. **A closed micro-step rule table** — but not ~40 hand-written cell-level cases. Committed
   pairs run a small uniform *rewrite executor* FSM (reserve k cells all-or-nothing → write
   new agents → splice wires → release), interpreting per-rule-pair *templates* (how many
   agents, which tags, the wiring permutation) from a template ROM. The CA stays microcoded
   and small; adding a rule is data, not logic.
2. **The commit handshake**: two-phase propose/ack across Margolus block boundaries with a
   deterministic tie-break; prove no orphaned commitments (a stuck half-committed pair is the
   CA analog of a torn write).
3. **Space reservation and pressure**: all-or-nothing reservation with randomized backoff; a
   diffusing pressure field; vacancy migration up-gradient. One number to find empirically:
   the jamming threshold (expect a traffic-model-like phase transition in fill fraction; run
   below it).
4. **Liveness**: an impatience rule (age raises weight) as the watchdog, plus an adversarial
   test battery (taut crossing wires, rings, dense clusters, pathological fan-outs); ideally
   a potential-function sketch (weighted wire length + pressure integral decreasing in
   expectation) for the deterministic core.
5. **Layers and rerouting**: fix the layer count (2–3), define via allocation, and a local
   detour rule for layer exhaustion; pressure must eventually clear blocked reroutes or the
   battery must show the frequency is negligible below the jamming threshold.
6. **The shadow harness** (= §2.2's induction invariant): the simulator co-maintains the
   abstract net; asserts every micro-step projects to identity or one interaction; asserts
   normal-form bit-equality against `rust-eager` over the corpus. The differential-oracle
   discipline, unchanged.
7. **Readouts**: interactions, transport (ι-count), peak area, makespan in ticks, per-region
   heat maps — exported as the ledger's cost vector.
8. **Knob sweep harness**: W_hot/W_warm/W_cold, temperature and adaptivity, against an oracle
   floor (critical path × ideal transport) on benchmark nets.
9. **Determinism**: hash(position, tick, seed) PRNG; bit-reproducible runs.
10. **Scale target**: 10⁵–10⁶ cells, 10⁴–10⁵ agents; Rust; single-threaded first (the sim
    validates dynamics, it does not race anything).

Deliberately out of scope for v1: sup/labels, machines, GALS clocking, real I/O (one root pad
at a fixed edge cell suffices; the loader unfolds a compiled tree H-tree-fashion from the
pad, trees being planar-embeddable).

## 14. Open problems and blockers

### 14.1 To write (theory)

- The **polarity lemma** (§2.2): TC-Net wires are orientable; gates the embedding theorem.
  Written: `EMBEDDING_THEOREM.md` §1, proven by full case analysis over the whole rule table
  (no rule fails; the calculus is uniformly polarizable).
- The **embedding simulation theorem** itself (§2.2), including the created-short lemma.
  Written: `EMBEDDING_THEOREM.md` §4 (projection invariant proven; bisimulation lift reduced
  to inherited metatheory) and §6 (created-short, proven); §5 charges drift and closes the
  ledger via the ι-count accounting identity.
- A **liveness argument** for the tension layer (potential function), or accept the watchdog.
  Still open.

### 14.2 To design (protocol and interfaces)

- The **commit protocol** as a micro-step table (§13 item 2).
- The **machine interface five-tuple** (§5.1) with the codec-iso obligation spelled out.
- Layer-exhaustion **rerouting** rules (§13 item 5) — the continuous place-and-route problem,
  the hardest engineering unknown in the design.
- The optimizer **codomain** (§8.5): trees v1, `NetDesc` + `materialize` v2.

### 14.3 To measure (empirics)

- **The Rent exponent of the kernel-elaborated tier** (§10.3 finding 4). The
  raw-tree-calculus tier measured p = 0.40–0.52 (§10.3); checker walks and kernel
  arithmetic exceed the traceable range until wire-RC lands (plus a native tree_eq machine
  on the net, or a leaner check path). The thesis's remaining empirical exposure sits
  there, plus scale (10⁸+ events per fit), seed variation, and workload breadth (streams,
  sup traces).
- The **jamming threshold** and the weight/temperature schedules (§13 items 3, 8).
- **Representation costs**: unary Nat is transport poison; HBin/word representations via
  licensed rep-change (the arith.opt overlay pattern), which is a real coercion with real
  cost, not a φ-identity (the HEq caveat in `OPTIMIZER.typ`).
- Area vs fragmentation slack (usable area < free area).

### 14.4 Research risks

1. Online routing under congestion (rubber-band livelock between taut wires, layer
   congestion); mitigations designed (§2.3, §13) but unproven.
2. The light cone (§4.1): parallelism-hungry programs are polynomially capped regardless of
   cleverness; express links reintroduce non-uniformity.
3. Recognition economics (§5.4): certified recognition must amortize over stable hot
   structure; programs that never stabilize never harden.
4. `sup` labels vs finite-state cells (§3): bounded label bits cap superposition nesting per
   region; compounds with the open label-coordinated-duplication question.
5. Positioning: like `rust-ic-net`, this is an optimizer/runtime substrate, not a checker
   speedup (it gives up hash-consed O(1) conversion; `tree_eq` stays king for checking).

### 14.5 What blocks what

§10 (Rent) ran first (§10.3) and the thesis survived contact on the measurable tier; its
kernel-class completion is blocked on `rust-ic-net` wire-RC. §11 (grade) needs only §14.1's
theorem written carefully. §12 (tiled) has no conceptual blocker; policy knobs are guessable.
§13 (CA sim) is the design-complete gate: it needs §14.1 and §14.2's first three items, and
writing its full rule table is a bounded task about the size of `tc-net.typ`'s rules section
that will force every remaining hand-wave. Machines (even simulated) need the five-tuple,
sealed refs, and the M1 rulebook/checker completion from `OPTIMIZER.typ`'s build order; the
recognition search can start as hand-picked subnets. The self-hosting loop (§8) needs the
costed self-interpreter, the codomain decision, and M1. No conceptual blocker stands
anywhere; what remains is one theorem, one protocol table, one interface definition, one
liveness argument, and empirics — with §10's number the biggest unknown of all.

## 15. Prior art map

Three web sweeps, 2026-07-07. Confidence noted where not high.

### 15.1 The direct precedent

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

### 15.2 Unclaimed (verified absences)

Partner-attraction migration (force-directed, wire-shortening); IC/TC-specific spatial
instantiation; any FPGA/ASIC interaction-net hardware (only Lippi's clockless-circuit theory,
Fundamenta Informaticae 2009, and HigherOrderCO's stated chip ambitions; the Vine community
lists a WIP FPGA target "Tendrils Compute"); distance as a cost grade in any IN cost
semantics (the reasonable-space line, Accattoli–Dal Lago–Vanoni LICS 2022, is machine-based
and metric-free; Gimenez–Moser POPL 2016 is amortized but distance-blind); and (from the
third sweep) any combination of interaction nets or graph reduction with formally-checked
substitution of subnets by fixed-function hardware — §5.2's composite appears novel, its
nearest neighbors being idiom recognition + verified lifting (recognition with proofs, von
Neumann substrate) and Vericert (certified circuits, no recognition).

### 15.3 Substrate-adjacent

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
  96 KB scratchpads +4.2×, warp alignment +9×, LEAK) is the locality fight of §1.
  Single-thread is 5×–100× behind GHC, so per-interaction useful work is low.
- **Mesh silicon**: Cerebras WSE-3 (900k cores, 48 KB/core, single-cycle 4-neighbor 32-bit
  wavelets, no caches/coherence); GA144 (144 async Forth cores, ~144 B RAM each); Adapteva
  Epiphany-V (1,024 cores, 64 KB each); UC Davis KiloCore; CAM-8 (Margolus/Toffoli, dedicated
  CA hardware, 1995: the lattice virtualized over DRAM, ~3G site-updates/s at 1 bit/site,
  i.e. the time-multiplexed alternative to one-cell-per-site).
- **Theory**: Thompson AT² (CMU thesis 1980); Thompson–Kung mesh sorting Θ(√N) (CACM 1977;
  Schnorr–Shamir 3n constant, STOC 1986); Leighton (1992) for bisection bounds; Rent's rule
  (Landman & Russo 1971) and Donath's wire-length law (IEEE TCAS 1979): average length
  bounded iff p < 0.5 in 2D; placed logic measures p ≈ 0.5–0.7.
- **Energy**: Horowitz, ISSCC 2014 (45 nm: 32b add 0.1 pJ; 64b SRAM 10–100 pJ by size; DRAM
  64b 1.3–2.6 nJ); Dally (28 nm: 64b FP ~20 pJ; ~26 pJ per 256b×1mm on-chip; ~16 nJ DRAM;
  per-bit-per-mm decomposition medium confidence).
- **Migration algorithmics**: amoebot model (SPAA 2014; canonical amoebot 2021 with
  deadlock-free concurrency control); compression via Metropolis dynamics with a provable
  phase transition (Cannon, Daymude, Randall, Richa, PODC 2016); Claytronics hole motion
  (De Rosa et al., ICRA 2006: move vacancies, not modules, in dense lattices); Chirikjian's
  metamorphic systems (1994–97: earliest statement that greedy local reconfiguration
  deadlocks; simulated annealing as mitigation).

### 15.4 Machines-adjacent (Part II lineage)

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
  match to §5.2's checked-swap, minus the substrate.
  https://arxiv.org/abs/2404.18249
- **Certified HLS**: Vericert (Herklotz, Pollard, Ramanathan, Wickerson, OOPSLA 2021):
  CompCert-based C-to-Verilog in Coq, 431-line Verilog-semantics TCB, ~2× behind unverified
  HLS. The certified-machine-construction half.
  https://johnwickerson.github.io/papers/vericert_oopsla21.pdf
- **Custom-instruction extraction**: Atasu, Pozzi, Ienne (DAC 2003, best paper): select
  maximal-speedup convex dataflow subgraphs to harden into function units; survey Galuzzi &
  Bertels, TRETS 2011. Recognition-at-design-time, the frozen ancestor of §5.2.
- **Anton** (Shaw et al., ISCA 2007 / CACM 2008): 32 hardwired interaction pipelines
  (~50 GP-ops/cycle each) + 12 programmable cores per ASIC; shipped proof that "dedicated
  pipelines + general cores on one die" wins big when the hot kernel is stable.
- **Graph-reduction hardware**: SKIM (Cambridge 1980, SKI in microcode), NORMA (Burroughs
  ~1986), GRIP (Peyton Jones et al., FPCA 1987), Reduceron (Naylor & Runciman, FPGA template
  instantiation, ICFP 2010). These harden the *reducer* into hardware, not *programs* into
  machines; complementary and mostly cautionary (they lost to commodity scaling; the
  wafer-scale era changes the terms).

## 16. Relation to the repo's roadmap

- `OPTIMIZER.typ` §4: transport is the missing fourth grade; §9: the CA is the minimal honest
  `run_model`, and §7 here makes every machine model the same calculus under a grade profile;
  §3: per-region attribution extends to data movement and, on real hardware, to energy
  metering.
- `RUST_IC_NET_DESIGN.md`: §12 here (tiled scheduler) is an M2-adjacent experiment; the
  wide-block T₁/T₂ decision and the `Ref` tag both prefigure Part II's shapes. E1's rung
  A/C instrumentation lives in that crate (`src/trace.rs`, `src/bin/rent.rs`, drivers in
  `bench/e1-*`): opt-in, sequential-only, one never-taken branch in normal runs; wire-RC
  (its §5) is now also what unblocks the kernel-tier Rent measurement.
- `TYPE_THEORY.typ` §15: effectful machines stay at the driver boundary; pure machines are
  agents; §6 here reads the whole effects arc as the machine economy's source-level face.
- `GOALS.md`: §8 here is its optimizer loop verbatim; §7 here is its "deterministic models of
  the base hardware" item given a uniform construction.
- Core discipline (`CLAUDE.md`): resident machines are "host implementations are
  optimizations" made physical; `tree_eq` is the first resident machine and its differential
  validation is the certification discipline in miniature.
