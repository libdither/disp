# Local CA substrate for TC-Net: a concrete build spec

Design doc. Companion to `SPATIAL_IC.md` (the theory note; this is its §13 "E4" expanded
into a buildable specification), `EMBEDDING_THEOREM.md` (the simulation proof this relies
on), `tc-net.typ` (the calculus), and `RUST_IC_NET_DESIGN.md` (the pointer machine whose
§2/§5/§7 decisions transfer). Status: design, with rungs 1 through 2 prototyped in
`local_ca_field.html` (a rung-2 evaluator: real reduction over the full agent alphabet, gated on
demand and adjacency with per-cell moves, checked live against an independent normalizer, with
four view modes; measured liveness findings in §12).

The existing `ca_substrate_viz.html` is a *shortcut* realization: it stores wires as
segment data on cells, re-embeds them with a global A* route on every move, propagates wire
type by an instant whole-chain walk, and schedules by whole-grid scans. Those four shortcuts
are exactly what a real cellular substrate cannot do, and this doc specifies the fully local
replacement for each. The shortcut viz remains useful as a fast picture and as a differential
oracle; this spec is the artifact that earns the "finite state per cell" claim.

A note carried forward from the perf work on the shortcut viz: reducing it to normal form is
fragile, because with global routing the wires grow long, converge on a cell, the route
jams, and a value blocks permanently. That is not a coding accident. It is the **jamming
phase transition** (§7) and the **liveness gap** (§9) showing up in miniature: the shortcut
viz was run above the jamming threshold with no watchdog. The local design below is where
those become first-class and addressable.


## 1. Invariants (the non-negotiables)

1. **Finite state per cell.** A cell holds a bounded number of bits (§3), independent of net
   size. No ids, no pointers, no per-cell lists that grow.
2. **Locality.** Every cell update reads only that cell and its four von Neumann neighbors
   (§2). Nothing reads a region, walks a chain, or searches a path.
3. **Rules stay distance-blind.** Rewrite outcomes never observe geometry (near vs far, race,
   timeout). This preserves strong confluence (`tc-net.typ` Theorem 2), which is what makes
   any schedule sound. Geometry sets rates, never results.
4. **Everything is an agent.** Values, consumers, dispatchers, wires (forwarders), and
   crossings (vias) are all cell-resident agents drawn from one finite alphabet (§4). There
   is no separate "wire" datatype.
5. **The projection invariant is the correctness spec.** At every tick the lattice projects
   (erase forwarders and vias, read agent connectivity) to a well-formed abstract TC-Net, and
   every micro-step projects to identity or to exactly one abstract interaction. This single
   sentence is both the theorem's invariant (`EMBEDDING_THEOREM.md` §4) and the simulator's
   test assertion (§10). Reduction quality (does it reach normal form, how fast) is a separate
   dynamics question; connectivity, hence the result, is fixed by this invariant regardless of
   the field, the schedule, or the routing.


## 2. Lattice and neighborhood

**Von Neumann (4-neighbor), not Moore.** Four faces per cell, directions encoded in 2 bits
(N, E, S, W). The choice is deliberate: a 4-neighbor lattice is what real silicon routes on
(no diagonal wires, which are second-class on a chip), and it forces every agent to fit in
four faces, which the shortcut viz dodged by using an 8-neighbor Moore lattice. The only
agent that does not fit four faces is `T2` (five ports); §4.3 splits it.

**Crossings via layers.** Interaction nets are non-planar, so wires sometimes cross. Give
each face 2 to 3 **wire layers** with via cells, matching real chips (roughly 15 metal
layers) and von Neumann's own 29-state CA and Wireworld, both of which carry crossing organs.
A crossing is an inert via agent (§4.4): two wires pass through on different layers without
interacting. Layer exhaustion is handled by a local detour rule (§8.3), the hardest
engineering unknown and one the pressure field (§7) is meant to relieve.


## 3. Cell state (bit-exact target)

A representative budget, in the "a few tens of bits" range of `SPATIAL_IC.md` §3:

- **tag**: which agent occupies the cell, from the ~15-kind alphabet (§4) including `empty`,
  `forwarder`, and `via`. 4 bits.
- **face map**: for each of the 4 faces, what attaches there and its role (unused, or port
  index 0 to 3, or wire layer A or B). About 3 bits per face, 12 bits. This subsumes port
  orientation and wire-layer occupancy.
- **phase / sleep**: 1 bit. A sleeping cell is clock-gated (demand-driven laziness): it does
  not fire, walk, or migrate until a demand signal wakes it.
- **signal bits**: demand and death pulses in flight on this cell's wire faces (§9). About 2
  to 4 bits. Signals ride the same faces as the wires they concern, so they need no dedicated
  face.
- **pressure**: the local scalar of the unified field (§7). 3 to 4 bits, bounded by diffusion
  with decay.
- **PRNG**: `hash(x, y, tick, seed)`, computed on demand, not stored (0 bits). Keeps runs
  bit-reproducible for the differential oracle.

Total on the order of 25 to 30 bits. Everything a cell needs to act is here; nothing it reads
lives outside its four neighbors' copies of the same.


## 4. The agent alphabet (all four-fittable)

Ports are listed principal-first. "aux" counts non-principal ports. Producers emit at their
principal; consumers consume at their principal and emit at `res`; this uniform polarity is
what orients every wire (`EMBEDDING_THEOREM.md` §1, proven by full case analysis).

### 4.1 Values (producers)

- `L` leaf, 1 port (principal). 0 aux.
- `S` stem, 2 ports (principal, child). 1 aux.
- `F` fork, 3 ports (principal, left, right). 2 aux.
- `P` suspension `(f a)`, 3 ports (principal, operator, argument). 2 aux. Inert until forced.

### 4.2 Consumers and dispatch

- `A` apply, 3 ports (principal faces the operator, argument, result). 2 aux.
- `T1` level-1 dispatch, 4 ports (principal faces the discriminant `a` of `△ a b c`, `b`,
  `c`, `res`). 3 aux. Fits four faces exactly.
- `T2` level-2 dispatch, 5 ports. Does not fit; see §4.3.
- `δⁿ` need-duplicator, 3 ports (principal, copy-left, copy-right). Demands a suspension once,
  copies only the resulting value.
- `δˢ` structural duplicator, 3 ports. Copies syntax blindly (suspensions included), the
  reflective-materialization operator.
- `ε` eraser, 1 port (principal). The death pulse (§9).
- `N` normalizer, 2 ports (principal, result). Drives full normal form.

### 4.3 Splitting T2 for the 4-neighbor lattice

`T2` dispatches on `z` (its principal): `z = leaf` returns the leaf-arm `w`; `z = stem u`
returns `x` applied to `u`; `z = fork u v` returns `((b u) v)`. Five ports: principal `z`,
arms `w`, `x`, `b`, and `res`. Two lowerings keep it on four faces:

- **(a) Two-cell block.** `T2` occupies two adjacent cells that jointly present five ports
  outward and commit atomically, the multi-slot-block pattern of `RUST_IC_NET_DESIGN.md` §2.
  Keeps the rule table verbatim; costs one intra-block handshake per fire.
- **(b) Binary lowering.** Replace `T2` by a 3-port select agent facing `z`, with its three
  arms carried as a nested pair `⟨w, ⟨x, b⟩⟩` built from 3-port pairing agents; the select
  unpacks the pair to the arm `z`'s arity chooses and erases the rest. Lafont-standard (every
  agent at most 2 aux), at the cost of a few extra local interactions per dispatch.

**Recommendation: split (lean toward (b), binary), because the depth-2 crossing model (§4.4)
gives a positive reason to.** Crossings are carried on a depth layer (a wire tucks behind the
front agent), and the clean division of labor is: the four in-plane faces are for agent ports
and primary wires, and the depth axis is a pure crossing/overflow channel, never a port. That
keeps depth meaning one thing. But it also means an agent may use only its four in-plane faces,
so `T2` (five ports) must split; letting `T2` spend its fifth port on the depth face would mix
logic into the crossing channel and is exactly the inelegance to avoid. Binary (b) is then
preferable to the two-cell block (a) for two compounding reasons: uniform at-most-3-port cells
make a microcoded rewrite executor's cell FSM and template ROM uniform (no special wide cell),
and the spare in-plane face on every binary cell carries the field/demand/commit signals.
Lowering is also a liveness lever (measured in §12): a cell with at most three ports bends at
most two wires per step, exactly the two-strand capacity of the cell it vacates, so the §8.2
local move becomes feasible by construction in the generic case. `T1`, the one four-port
survivor of the current alphabet, is the overflow case; that is an argument for lowering it
too. In
UNBOUNDED 3D (six faces) the split is instead neutral — `T2` fits with a face to spare — so
this recommendation is specific to the planar or depth-2 substrate. (The rung-1 prototype
already embodies the invariant: agents carry only in-plane ports; the depth layer, realized as
per-cell background crossing strands, is used for crossings alone.)

### 4.4 Wire agents

- `ι` forwarder, 2 ports (in, out), oriented along value polarity. A wire is a chain of `ι`.
  The single rule schema: `ι` against a producer root C rebuilds C with its principal on
  `out` (a splice; `SPATIAL_IC.md` §2.2). Chains collapse one interaction per element.
- `via` crossing, 4 ports on two layers (a straight-through on layer A, a straight-through on
  layer B), inert: it never forms an active pair, it only lets two wires cross.

**Depth-2 realization (front surface + background).** The two "layers" are made concrete as a
DEPTH-2 model: each cell has a front agent slot and a set of background crossing strands (the
wires tucked behind the agent). A forwarder is a cell with one background strand and no agent;
a via is a cell with two background strands; a value sitting on a crossing is an agent whose
cell also carries the crossed strand behind it. Faces stay unique (an agent port and a
background strand never share a face). This is what lets a value pass a crossing as an ordinary
ONE-CELL move: it reels onto the via cell, the crossed strand tucks behind it, and next tick it
reels off leaving that strand as a plain wire. No agent teleports two cells, and a value that
carries children passes a crossing the same way (each step is one cell), so multi-wire
through-via needs no special rule. An asymmetric field bias (prefer the front) keeps the front
a clean, mostly-2D surface and uses the back only when a crossing forces it. Two background
layers suffice for pairwise crossings, the way a 2-sided board routes anything by spreading
crossings across cells; a third is a fallback knob. This is the finite (depth-2) case of the
3D substrate: on a 6-neighbour lattice crossings become optional (route around in z) and `T2`
fits without splitting, but the depth-2 planar prototype is the honest worst case, and the one
that runs on today's 2.5D metal-stack silicon.

Every occupied cell holds exactly one of these agents. The shortcut viz's segment stack and
its `findPath` both disappear: a wire is forwarder agents, and re-embedding it is local agent
motion (§8).


## 5. The tick, as local cell programs

Each tick, under the Margolus schedule (§6), a cell runs whichever of five local rules its
state and neighbors enable. All five read only self plus four neighbors.

1. **Fire.** If my principal faces a neighbor whose principal faces me, and one of us is a
   producer and the other an awake consumer, we are an active pair. Enter the committed state
   and run the rewrite executor (§5.1). Local detection; the rewrite needs space, which is
   where the field enters (§7).
2. **Walk (reel-in transport).** If I am a producer or `ε` and my principal enters a wire
   whose far end is demanded, step one cell toward it, consuming one forwarder (one unit of
   transport). This is how a value travels to its consumer before firing.
3. **Straighten.** If I am a forwarder whose two neighbors have become adjacent (the wire
   kinks), retract: delete myself and let my neighbors connect directly. The local relaxation
   that keeps wires short.
4. **Drift.** Compute the field force on me (§7) and take a Metropolis step down it. Migration
   and space-making are the same move.
5. **Signal.** Advance any demand or death pulse on my wire faces by one cell (§9).

Rules 2 and 4 are separate mechanisms on purpose: Walk is directed transport (feasibility-first,
monotone toward the consumer), Drift is stochastic relaxation. The rung-2 prototype initially
asked Drift, with boosted wire weights, to do Walk's job, and redexes stranded; wiring Walk as
its own rule was load-bearing (§12). Walk's polarity matters equally: producers and `ε` travel,
a consumer parks and receives (its principal exerts no pull on it). Letting hot-wire tension
drag consumers toward their producers sent every fire site stepping toward the tree and the
whole computation walking off the fixed pad (§12).

### 5.1 The rewrite executor (microcoded, not hand-written)

Do not write ~40 cell-level fire cases. Committed pairs run one small uniform executor FSM
that interprets per-rule-pair **templates** from a ROM: reserve k free cells all-or-nothing
(§7) → write the new agents with their tags → splice the wires per the template's permutation
→ release the two dying cells. The template records only how many agents, which tags, and the
wiring permutation among (the two dying agents' surviving ports plus the fresh agents' ports).
Adding a rule is data, not logic, and the created-short lemma (`EMBEDDING_THEOREM.md` §6)
guarantees every template wires only among the two dying agents' ports plus O(1) fresh agents,
so a rewrite never creates a long wire. The tag transitions the templates encode are the
tree-calculus rules: `A·L→S`, `A·S→F`, `A·F→T1`; `T1·L→K` (return `b`, erase `c`),
`T1·S→S-rule` (a `δⁿ` sharing `c` plus three `A`), `T1·F→T2`; the `T2`, `δ`, `ε`, and `N`
families as in §4. A fire unfolds over several micro-ticks (reserve, write, splice), not one.


## 6. Scheduling: Margolus blocks

Partition the lattice into 2x2 blocks; each block applies its rules; alternate the partition
offset each tick. This gives deterministic, conflict-free synchronous updates with no global
scan and no central redex queue: a cell decides entirely from its block. Rewrites and moves
that touch a block boundary use a **two-phase propose/ack handshake** with a deterministic
tie-break (lower `hash(x, y, tick, seed)` wins), so two adjacent fires never clobber each
other and no pair is left half-committed (the CA analog of a torn write). This is both the
faithful local schedule and the performant one: block-partitioned CAs are embarrassingly
parallel and cache-local, which is the entire hardware ladder of `SPATIAL_IC.md` Part III
(tiled reducer, FPGA array, Cerebras-class mesh). Confluence makes any block order sound; the
schedule changes only rates.


## 7. The unified field: tension and pressure as one potential

The shortcut viz had two separate mechanisms, wire-tension migration (attractive, shorten
wires) and, on paper, allocation pressure (repulsive, make space). They are the two terms of
one potential, and unifying them is both simpler and the key to liveness:

    φ  =  Σ_wires ( w · length )   +   crowding(local density)

where `w` is the wire's weight (hot 12, principal 2, plumbing 1). Every agent takes a
Metropolis step that descends `φ` (accept always if it lowers `φ`, else with probability
`exp(-Δφ / T)`, `T` locally adaptive so recently active regions stay fluid). The first term
pulls connected agents together (tension); the second pushes agents out of crowded cells
(pressure). One field, one move (rule 5.4), no tug-of-war between separate mechanisms (a
failure mode the shortcut viz hit when a separate push lost to attraction every tick).

**Node insertion is the field making space, locally.** A fire that needs k free adjacent
cells and cannot get them does not search and does not fail. It raises the crowding term at
its site (an all-or-nothing reservation with randomized backoff; unmet requests increment
local pressure). Pressure diffuses to neighbors (a bounded, decaying heat-equation step, the
most local operation there is). The gradient pulls **vacancies** up toward the site: in a
dense region, move the holes, not the agents (Claytronics hole motion), which is far cheaper
because holes are the rare species there. When a vacancy arrives, the reservation completes
and the fire proceeds. Erasure (`ε`) is the vacancy source, the lattice boundary is the
infinite sink, and sustained pressure with no relief is exactly OOM, a local observable
condition. The boundary can grow (spawn empty cells) when pressure reaches it, giving a
substrate that expands as much as the computation demands.

**Liveness, and why φ is the candidate potential.** The open question `SPATIAL_IC.md` §14.1
leaves is a liveness argument: does the field always eventually make space, or can it deadlock
in a local minimum? The unified `φ` plus the pressure integral is precisely the potential
`SPATIAL_IC.md` §13 asks for ("weighted wire length + pressure integral, decreasing in
expectation"). Two ingredients make the descent live rather than stuck: the Metropolis noise
(deterministic greedy descent provably deadlocks in local minima; Metropolis moves provably
compress, amoebot PODC 2016, with a real phase transition in the bias), and an **impatience
rule** as watchdog (an agent's age raises its weight, so a starved reservation eventually
out-competes its neighbors). Whether `φ` is a true Lyapunov function for this dynamics, or the
watchdog is load-bearing, is the one theory item to settle or to accept. Temperature has a
scope limit the rung-2 prototype measured directly: it repairs energy minima only. When the
move set itself is infeasible (every candidate step refused for want of a free bend corner),
the acceptance rule never gets a vote and the pass rate is exactly flat in `T`; feasibility has
to be rebuilt into the move (§4.4, §12), it cannot be annealed in.

**The jamming threshold** is the one empirical number. Expect a traffic-model-like phase
transition in fill fraction: below it the field clears reservations quickly, above it it jams
(the shortcut viz's permanent stall). Measure it, run below it, and enforce a density cap
(refuse migration into a cell that would exceed the threshold locally). The shortcut viz's
fragility is the first data point that the transition is real.


## 8. Wire dynamics (local re-embedding)

### 8.1 Growth by splice only
Wires lengthen only by two chargeable events (`EMBEDDING_THEOREM.md` §6): a `Var` elimination
fusing two chains, and `δ` copying an existing long wire. No rewrite template creates a long
wire. So geometry degrades only through identifiable events, which is what gives tension a
chance to keep up.

### 8.2 Reel-in and straighten
An agent shortens a wire by stepping into its adjacent forwarder cell (rule 5.2), consuming
one `ι`, at the price of lengthening its other wires by at most one each; it moves when the
weighted shortening beats the weighted lengthening (this is the first term of `φ`). Slack from
any move diffuses away by straightening (rule 5.3): a kinked forwarder whose two neighbors are
adjacent retracts. Together these are the local replacement for the shortcut viz's global
route: an agent move is O(1), and the wire relaxes toward straight over subsequent ticks
rather than being re-solved at once.

### 8.3 Layer exhaustion
When both wire layers of a cell are occupied and a third wire must cross, a local detour rule
routes it one cell around, raising local pressure so the field opens room. If the detour rule
cannot place it, that is sustained pressure (OOM-like) and the adversarial battery (§10) must
show its frequency is negligible below the jamming threshold. This is the hardest engineering
unknown, flagged as such in `SPATIAL_IC.md` §14.2.


## 9. Signals: typing and GC as propagation

The shortcut viz's instant `retype` walk becomes two substrate signals, each advancing one
cell per tick along the forwarder chain, riding the wire's own faces (no dedicated face):

- **Demand** travels from a consumer toward the producer that feeds it, waking sleeping
  (clock-gated) cells as it goes. Laziness is literally clock gating; a region stays asleep
  and idle until demand reaches it. "This wire is hot" (carries a pending redex) becomes
  simply "a demand signal has reached this forwarder", a local bit, not a global classification.
- **Death** is an `ε` parked at a wire's far end flipping the wire's cells to a draining state
  one per tick, a back-pulse traveling against value polarity. A `δⁿ` keeps a one-dead bit
  (the entire refcount is one bit of local agent state); the second death pulse transmutes it
  into an `ε` facing its parked principal, cascading the drain up the demand chain. No refcount
  tables: the wire is the channel, a pulse can only race the value arriving from the other end,
  and both orders are sound (`SPATIAL_IC.md` §3 GC bullet, backed by `rust-ic-net`'s `rc.rs`).

Both pulses are priced as transport (one unit per cell crossed), so cancellation nets positive
exactly when the erased subnet exceeds the wire length.


## 10. Correctness: the shadow harness

The simulator co-maintains the abstract net and asserts the projection invariant (§1 item 5)
every micro-step: each step projects to identity or to exactly one abstract interaction. It
asserts normal-form bit-equality against `rust-eager` over a corpus. This is the
differential-oracle discipline the project already runs elsewhere (the `tree_eq` native
fast-path validated against its in-language reference), here at the substrate level. The proof
and the harness are the same statement, which is why the invariant is worth stating precisely.
Determinism comes from the `hash(x, y, tick, seed)` PRNG: runs are bit-reproducible, so the
oracle is exact, not statistical.


## 11. Readouts

Export the ledger's cost vector: interactions (work), transport (the `ι`-elimination count,
`SPATIAL_IC.md` §2.2 defines the transport grade as exactly this), peak area (space), makespan
in ticks (span), plus per-region heat maps, the pressure field, and fill fraction. A knob
sweep harness varies the wire weights (hot / principal / plumbing), the temperature schedule
and its adaptivity, the field diffusion rate, and the density cap, against an oracle floor
(critical path times ideal transport) on a benchmark set.


## 12. Build order and scope

Cheap-first, each rung gating the next:

1. **Cell state machine and the field.** The bit layout (§3), the Margolus tick loop (§6), and
   the unified field with diffusion and Metropolis drift (§7), on a lattice of `ι` and `via`
   only (wires with no live agents). Watch straightening and reel-in relax a tangle. This rung
   alone tests the jamming threshold and the liveness watchdog, the riskiest unknowns, before
   any reduction exists. Prototyped first in `local_ca_field.html` (since rebuilt as the rung-2
   evaluator below; the wire-only version had nodes as inert endpoints, wires as forwarder
   chains, O(1) local reel-in and bend with no path search, and the unified tension-plus-pressure
   field, kept in the current file as the `bead box` and `slack tangle` scenarios). Findings so far: the local move is integrity-clean and
   contracts a slack tangle to zero wire; the bead-box demand test confirms pressure evicts a
   cell faster than random diffusion, the gap widening with fill (the liveness half). Two
   honest results from building it: degree-2 structures (rings, chains) are frustrated because
   every single-node step is length-neutral (reel one wire, bend another), a genuine local
   minimum that coordinated moves or a better potential must solve; and on a 4-neighbor lattice
   a monotone staircase is already Manhattan-minimal, so straightening only removes true
   backtracks and reel-in is the primary relaxation. Since landed in the prototype: `via`
   crossings (a wire cell holds one or two strands; two wires share a cell on separate layers,
   rendered purple, integrity-clean, and a dragged wire forms one by crossing another), and the
   Margolus commit (the tick is now synchronous: every node proposes from the frozen pre-tick
   state, a conflict-free subset commits by rotating priority, footprints grown by one ring so
   no two committed moves share a cell or an edge; this both parallelizes and cuts the moves to
   converge, e.g. the two-node case 600 -> 44). Reel-in THROUGH a via also landed, and was then
   corrected to be genuinely local: the first version JUMPED the value two cells over the
   crossing, which is not a valid one-cell CA step. It was replaced by the DEPTH-2 model (§4.4):
   a value passes a crossing as an ordinary one-cell reel-onto the via cell (the crossed strand
   tucks behind it as the cell's background, rendered gold-node-over-dim-wire), then reels off
   the next tick leaving that strand a plain wire. This is strictly local, and multi-wire
   through-via now works for free because every step is one cell (the reason the jump version
   couldn't do it). The crossing scenarios contract fully (vias 1 -> 0, E -> 0), integrity clean
   every tick, and the prototype now embodies the §4.3 invariant that agents use only in-plane
   faces (depth is crossings-only). Still to add: a closed-region sweep for the hard jamming
   transition (contraction alone frees space, so it never jams; only a demand does), and the
   front-bias field term (currently the front/back split is structural, not yet an explicit
   pressure).
2. **The rewrite executor and template ROM** (§5.1) for the value and dispatch families, with
   `T2` split binary (§4.3) so cells stay uniform and depth stays crossings-only.
   NOW BUILT FOR REAL in `evaluators/rust-ca-lattice/` (see its README): the ROM as
   validated const data over the fully lowered ≤3-port alphabet (T1 lowered too, arms as
   pairs; 13 tags, 26 interactions), per-cell state with NO ids in the dynamics,
   footprint-atomic transitions with machine-checked footprints, a shadow net asserting
   the projection invariant per transition, and the two-stage differential (abstract
   4000/0; lattice: zero wrong, zero invariant hits, liveness measured per topology).
   The crate has since taken the TOPO LIFT: cells live at (x, y, z) with six faces, and
   §2's wire layers, via cells, and §4.4's tucked strands are DELETED — z is the capacity
   those mechanisms were simulating; a crossing is two wires at different z, and an agent
   cell holds only its agent. Two topologies run the same dynamics and the same
   differential: `Bilayer` (z ∈ {0,1}, the 2.5D chip, the honest worst case) and `Full3D`.
   Transitions are FIRE (search, precomputed stamp, or incremental dock-and-grow), REEL,
   SHOVE, and two polymer moves projecting to identity: RETRACT (a width-1 U-turn
   annihilates, the discrete curve-shortening step) and SLIDE (a strand relocates out of
   a shared cell through the shortest empty-cell route, the excluded-volume move; the
   kink-flip is its 1-cell case). The field rung has landed on top, as two strictly
   neighbor-local fields rather than §7's single potential (the unified potential stays
   the theory target): ψ demand is a per-strand hot bit pumped at live consumer
   principals and spread one cell per tick along wire continuations (it replaced the one
   unbounded read, the whole-wire trace, at zero measured liveness cost), and χ
   pressure is a per-cell scalar under a Jacobi relax with leak, pumped only by real
   frustration (failed clears, hot-but-unplannable walkers, unfireable adjacent pairs,
   waiting seeds). Under χ, parked agents shove downhill, crowded cells thin, and
   count-1 mats evaporate strictly downhill. TENSION (§8.2's straighten made real) rides
   the wires as a second bonded signal: each strand learns, one cell per tick per side,
   which travel directions its wire uses beyond that side; bends flip toward annihilating
   partners under a no-harm exclusive gate, U-turn retraction cashes the pairs out, and
   wires finish at chord scale (the deep-spine stall that froze holding 774 strands now
   completes holding 5). Two jurisdiction rules make the signals compose instead of
   fight: tension never moves wire into χ-positive cells (a slide and a flip are inverse
   operations; without the boundary they churn one-to-one forever), and blocked FIRES
   get licensed eviction like blocked walkers (a wire can be hot purely for a value only
   that fire creates — a circular wait nothing anonymous can break). Measured on the
   400-term corpus with all of it: full3d 366 to NF (201 bare), bilayer 271 (179);
   every named pin completes on full3d including chain4, and chain2 is the first chain
   to complete on bilayer; transport falls 30 to 55 percent on the named set.
   Substrate lessons folded back into this doc, each found by measurement: walkers move
   only when a fire awaits (parked values on crossings otherwise deadlock the crossed
   wires); coexistence states were a symptom of the missing dimension (with real z,
   exclusive faces suffice and the old "vias impassable to 3-port walkers" class
   dissolves); a blocked walker needs DISPLACEMENT, not coexistence, and fixed-shape
   displacement is not enough — the general shortest-reroute slide plus one level of
   knot-loosening is; slides must be strictly decongesting (empty-cell trails only) or
   the scheduler shuffles strands forever and stall detection dies; in-footprint
   place-and-route needs backtracking with most-constrained-first wires, and reel's aux
   re-anchoring wants that same router rather than bespoke bend shapes. The field-rung
   lessons on top: pressure needs precision pumping (pumping every frustrated pair
   measured as a regression, churn drowning the signal); walks must eat slack (§8.2's
   reel-in made real: extension-only aux re-anchoring nets +1 strand per step and paves
   the count-1 trail mats that block everything downstream, while truncation-first
   re-anchoring along the wire's own chain needs zero new wire exactly where the router
   has no room, and was the lever that unlocked the chain class and selF); and a field
   schedule needs a shadow-progress stall detector, because polymer churn keeps ticks
   busy while a hopeless knot cycles (the old tick-capped class measured 40/40 frozen
   over five times the budget). The remaining stalls are the bilayer seam class (the
   single overflow plane is that topology's geometric ceiling) and, on full3d, the
   deepest multi-walker spines: the §13 liveness residue in its current, sharpest form.
   The earlier JS prototype of this rung remains in `local_ca_field.html`, by a different
   route than the pure microcoded executor: the file co-maintains an abstract interaction net
   (authoritative for reduction, validated 3998/0 against an independent normalizer with full
   rule coverage over the value, `A`, `T1`, `T2`, `δ`, `ε` and `N` families) beside the spatial
   embedding. Each fire runs the abstract rule, then re-embeds its O(1) fresh agents locally with
   a bounded-window router rather than a global A*. `T2` is drawn as one agent with its depth-2
   non-fit flagged (red box), cleared on the 3D-substrate toggle, so the split is shown rather
   than structurally lowered. Firing is now gated strictly on 4-neighbor adjacency plus the
   demand front, with per-cell moves (no router on moves) and Walk as its own tick rule; the
   pure Margolus microcoded executor with no co-maintained oracle stays the target, the
   co-maintained net being the scaffold that got real reduction running and checkable end to
   end first.
   Liveness findings from making the gated dynamics actually reach normal form, measured by
   ablation over a fixed 120-term corpus (every completion bit-matches the oracle, so the whole
   gap is liveness, never correctness): (a) the original move primitive demanded a fresh forward
   corner for every bent wire and refused any step onto wire; under it the layout froze solid
   (0 of 6 curated examples, 70 of 120 random terms stuck), and the pass rate was flat in
   temperature from 0.05 to 2.0, the signature of an infeasible move set rather than an energy
   minimum. Restoring §4.4 fidelity in the move (reel onto a via with the strand tucking
   behind, step onto a single foreign strand, bends overflowing through the just-vacated cell)
   plus wiring Walk as its own rule recovered 100 of 120 and 4 of 6; neither ingredient
   suffices alone (walk-only: no change at all; move-fix-only: 2 of 6). (b) Tucks must stay
   transient: letting strands persist under agents silts the lattice until no bend fits
   anywhere (the pass rate drops by a third); until a local strand-slip rule exists, the
   bounded router is the janitor that keeps tucks transient, a known departure from full
   locality. (c) Consumers must wait (the §5 Walk polarity): with hot-wire tension pulling both
   endpoints, every fire site stepped toward the tree, the computation walked off the fixed pad
   at about a cell per tick, and past the router radius the pad wire went taut (weightless) and
   the net decoupled; parking consumers cut the measured drift from thousands of cells to about
   a hundred. The residue (wide and branch among the curated six, about a sixth of random
   terms) is pocket jamming near the fire site: pairs survive, demanded and adjacent-ish, but
   their moves stay refused. That is the §13 liveness gap made measurable, and the next lever
   on it is the §4.3 lowering (at most two bends per step, which the vacated cell can always
   absorb).
3. **The commit handshake** (§6) across block boundaries.
4. **Signals** (§9): demand waking, `ε` death pulses, the one-dead bit. Demand is prototyped:
   reduction is demand-driven from the root normalizer and undemanded agents render dim (the
   laziness face). Death pulses and the one-dead bit are not yet spatial; erasure runs as an
   abstract `ε` cascade.
5. **The shadow harness** (§10): projection invariant per step, NF bit-equality vs
   `rust-eager`. Prototyped as a live differential oracle: every run's read-back is checked
   in page against the independent normalizer. The demand-driven, adjacency-gated schedule
   reaches normal form on 100 of 120 random terms and four of the six curated examples (the
   rest pocket-jam; rung 2 findings above), and every completion bit-matches the oracle.
   Bit-equality against `rust-eager` specifically is still to wire, as is the strict per-step
   projection assertion (the current check is at quiescence).

Scope for v1: single-threaded simulation (it validates dynamics, it races nothing), one root
pad at a fixed boundary cell (the loader unfolds a compiled tree H-tree-fashion, trees being
planar-embeddable), 10^5 to 10^6 cells. Out of scope for v1: `sup` and superposition labels,
resident machines, GALS clocking, real I/O. These are `SPATIAL_IC.md` Part II and its §13
"deliberately out of scope" list, unchanged.


## 13. Open problems

- **Liveness** (§7): is `φ` plus the pressure integral a true Lyapunov function for the field
  dynamics, or is the impatience watchdog load-bearing? The one theory item.
- **The jamming threshold** (§7): the fill-fraction phase transition, an empirical number; run
  below it. The shortcut viz's stall is the first evidence it is real.
- **The commit protocol table** (§6): the propose/ack micro-steps in full, with a proof of no
  orphaned half-commitments.
- **Layer-exhaustion rerouting** (§8.3): the continuous place-and-route problem in local form,
  the hardest engineering unknown.
- **The T2 lowering choice** (§4.3): two-cell block versus binary, a measured tradeoff of
  interaction-step count against cell uniformity.

None of these blocks starting: rung 1 (field on a wire-only lattice) needs none of them and
tests the two riskiest directly.
