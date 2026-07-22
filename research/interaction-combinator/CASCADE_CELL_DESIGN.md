# Cascade cell design

Status: implemented 2026-07-21 in `evaluators/rust-ca-lattice` (`cascade.rs`, `blocklet.rs`,
`cascade_run.rs`, `cascade_par.rs`, `cascade_gather.rs`, `cascade_trace.rs`); this document
is the design it implements, with §15 recording what implementation changed and the open
frontier. The previous Cell64 substrate and its spec (`LOCAL_CA_DESIGN.md`) were removed 2026-07-22 once every scenario migrated here; git history holds both.

The question this answers: the Cell64 rewrite rules are too complicated to imagine in
silicon, the fair-random activation sweep is not physical, an agent should move one cell per
tick, and synchronization should only exist where two active regions actually meet. Is a
design with those properties possible? Yes. This document derives it.

## 1. Verdict

The 26-rule semantic ROM (`rules.rs`) is small and correct: at most 6 fresh agents, at most 9
wires, wiring as a perfect matching, no rule ever names a remote endpoint (the created-short
lemma, `EMBEDDING_THEOREM.md` §6). None of the complexity lives there.

The complexity lives in four spatial decisions, each replaceable:

| Root decision | What it forced |
|---|---|
| A. Atomic unit = one cell (write self only) | 4-phase, 3-role move handshake; request-tree reservation; ~60-80 control paths in `update_cell`; 28 of 64 bits spent on protocol state |
| B. Rewrites = rigid preplanned patches | `compile64.rs` (1045 lines of Dijkstra, rip-up, seat search, rotation); workshops up to 63 slots; whole-patch reservation; both-lifts-blocked failure class |
| C. Cable bundling as distinguished matter | Zip, Cross, GuestZip, GuestLink kinds; twist, aux_flip, lane-index adapters; the arity-three crossing frontier (tail-unzip wave) |
| D. Fairness via global random permutation | Every site plus halo shuffled and activated every round; moves take 1-4 sweeps; the non-physical twinkling in `lattice_player.html` |

The replacement, in one sentence: an event-driven asynchronous transition system whose atomic
primitive is a two-cell edge transaction, whose rewrites grow from a seed instead of stamping
a reserved patch, and whose matter is a single uniform route format. Every handshake protocol,
the workshop search, the reservation tree, and four of the seven matter kinds disappear. The
same rule runs serially, on N threads over `AtomicU64` cells, on a clocked activity-gated
array, and on self-timed asynchronous hardware.

What the current design got right and this proposal keeps: the one-u64-per-cell state bound,
delay insensitivity as the correctness bar, the shadow-net projection oracle, the semantic
ROM and its validation, demand bits (hot lanes), pressure as a hint rather than an authority,
and the suite-with-adversarial-schedules methodology.

## 2. The substrate as an asynchronous transition system

Discard the sweep. Define the dynamics as a set of transitions, each with:

- a scope: one cell, or one face-adjacent pair of cells (one lattice edge);
- an enabling predicate readable from the scope's words alone; and
- an effect that rewrites only the scope's words.

Scheduler contract: repeatedly pick any enabled transition and commit it atomically. Weak
fairness (an enabled transition is not ignored forever) is the only requirement. Confluence
obligation: two enabled transitions with disjoint scopes commute; overlapping scopes are
serialized by the commit mechanism. Given both, every schedule reaches the same normal form,
which is what the oracle gate already checks.

Activity is a wake set, not a shuffle. After a transition commits on cells {a, b}, the cells
whose enabling predicates could have changed are exactly {a, b} and their face neighbors.
Push those; process nothing else. Quiescent regions cost zero. Change propagates at most one
cell per step: a light cone. This is the cascade.

Time becomes observable. In the serial runner, use a FIFO worklist: the queue partitions into
BFS generations, and one generation is one tick. A walking agent re-enables itself one cell
forward each generation, so it moves exactly one cell per tick. The player's sweep counter
and next-activation marker become a generation counter and a wavefront overlay.

The current handshake protocols are already delay-insensitive, so this scheduler swap alone
is sound on today's code (see §12, rung 0). The rest of the proposal is about making the
transitions themselves small.

## 3. Edge transactions

The atomic primitive is: atomically read and rewrite the two 64-bit words across one face.

The discipline that makes this safe under true parallelism, checkable by the update
function's signature:

> A transition's correctness reads are limited to its own scope (the two claimed words plus
> the static ROM). Any read outside the scope is a heuristic and must be stale-safe
> (pressure gradients, emptiness hints, congestion estimates).

Today's `update_cell` reads six neighbors for correctness, which is exactly why it cannot be
run concurrently: the neighborhood is not a snapshot. The v2 update function receives two
words and returns two words.

Cross-cell geometric invariants (face reciprocity: if a cell exposes `(face, lane)`, the
neighbor exposes the opposite) are maintained inductively: every transition preserves them,
so no transition needs to re-verify them beyond its scope. One corollary needs stating
because it shapes growth: a transition may not write matter whose face claims the edge
toward a cell outside its scope, since that cell could be concurrently claimed by someone
else. Anything that grows into empty space (seed extrusion, trail detours) therefore first
commits a one-cell reserve mark into the empty target (a plain CAS, visible to every other
transaction), and only then places matter facing it. Reserves are held one at a time per
grower and only on empty cells, so there is no hold-and-wait cycle. This mark is the entire
v2 replacement for the request tree: a reservation of one cell, taken optimistically, instead
of a two-phase wave reserving up to 63.

Every core operation is naturally one edge:

- Move. Before: `tail ═ source(agent) ─ target(wire) ─ …`. After: `tail ═ trail ═
  target(agent) ─ …`. The tail cell never changes; in the current protocol its role is
  validation only (`packed_local.rs` `tail_next` holds and acknowledges but the move writes
  source and target). The source knows its own tail geometry and writes the trail into its
  own cell; the target word contains the wire's exit face. One transaction replaces
  offer/ack/ack/commit/write/write/done.
- Dock. Two agents principal-to-principal, both hot: one transaction replaces both with seed
  matter (§6). The semantic fire is observed when the seed resolves.
- Extrude. A seed and one empty neighbor: place the next blocklet cell.
- Retract, flip. The relaxation moves are two-cell transpositions already.

Single-cell transitions (field relaxation, hot-bit decay) degrade to plain CAS loops.

For hardware without a pair primitive, the edge transaction lowers to a two-phase
propose/commit handshake with the arbiter in the target cell (§10). The software claim
protocol (§9) is the same shape. The four-phase persistent-role protocols exist in v1 only
because the atomic unit was too small; move the atomicity into the execution substrate and
the control state dies.

## 4. Matter v2: uniform routes

One route = one unordered pair of `(face, lane)` endpoints. One cell holds at most two
routes. Everything the current five wire-ish kinds encode is a route configuration:

| v1 kind | v2 encoding |
|---|---|
| Link, one lane | one route |
| Link, two lanes (cable) | two routes with the same face pair, lanes distinguish |
| twist | gone: the pairing of endpoints is explicit per route |
| Zip | two routes sharing the trunk face on different lanes, exiting different branches |
| Cross | two routes on four distinct faces |
| GuestZip / GuestLink | agent plus one pass-through route in the same cell |
| aux_flip | gone: the agent's tail lane assignment is explicit in its trail routes |

The word (64 bits, generous slack because protocol control is gone):

| Field | Bits | Notes |
|---|---:|---|
| kind | 2 | Empty, Wire, Agent, Seed |
| Empty: reserved-by face + arm | 4 | growth reservation mark, zero when truly free |
| Wire: 2 × route (2 × (face 3 + lane 1)) + presence | 17 | |
| Agent: tag 4, principal 3, tail 4, lane order 1, pass-through route 9 | 21 | pass-through = the guest case |
| Seed: rule 5, pc 4, partner face 3, orientation 3 | 15 | |
| claim | 1 | freezes the word during a transaction |
| hot | 2 | demand per lane, unchanged semantics |
| cooldown | 2 | |
| χ, σ | 8 + 8 | unchanged |

Worst variant ≈ 40 bits used. v1 spends 28 bits on control phases; v2 spends one claim bit
plus the seed program counter, because atomicity moved out of cell state and into the commit
mechanism.

The projection tracer collapses to one case: enter at `(face, lane)`, find the route
containing that endpoint, exit at the other end; agents terminate. Zip, crossing, guest, and
twist tracing all become this.

## 5. Movement

An agent advances into the wire cell on its principal face when hot (demanded), calm, or
strictly downhill in χ: the same gate as today. The vacated cell becomes the trail:

- arity 1 (L, Eps: most traffic): empty. The agent is a pure particle, one cell per tick.
- arity 2 (S, Nrm): one route.
- arity 3 (F, A, T1, Sel, Unp, Dn, P, Pair): two parallel routes. A cable is not a kind; it
  is two routes that happen to share a face pair.

Crossing foreign wire: entering a cell that holds one foreign route is the guest
configuration (agent plus pass-through), uniform for every arity. The constraint is the
trail: if the vacated or entered cell would need three routes, the mover lays one aux trail
through a chosen free side cell first (a reserve plus one edge transaction), or waits
under χ. This closes the arity-three crossing frontier (the tail-unzip wave) with the
same primitive used everywhere else, instead of a new wave protocol over zip machinery.

Demand (ψ/hot) pumping, cooldown, and the calm/downhill gates carry over unchanged; they are
single-cell stale-safe transitions.

## 6. Rewrites: dock, seed, blocklet

The workshop model exists because a rigid 2-to-63-cell patch must be reserved before
placement and its internal wires must be routed around live boundary cables at search time.
Both needs disappear if the patch is grown.

Dock. When consumer and producer principals are adjacent and hot, one edge transaction
replaces both agents with two seed cells. The seed word stores the rule index and a program
counter. The dying agents' aux stubs (up to two cables on each side) remain attached to the
two seed cells' faces: the seed inherits them in place.

Patch panel. The rule's wiring names how consumer aux, producer aux, and fresh ports
interconnect. Boundary-to-boundary wires resolve immediately: the seed cells become the
routes. `Unp·Pair` (fuse both wire pairs, zero fresh) and `Eps·L` (both die) complete in the
dock transaction itself, one atomic step, versus a 2-slot workshop plus request tree today.
Boundary-to-fresh wires become routes through the seed cells (at most two per cell: exactly
capacity) that terminate at blocklet ports as they appear.

Blocklet. The fresh agents and their internal wires form a graph with at most 6 nodes.
At build time (replacing the 1045-line search), embed each rule's fresh graph into a small
fixed shape adjacent to the seed: a deterministic embedding of a 6-node graph, computed once,
no Dijkstra, no rip-up, no seat library, because the embedding contains only fresh matter and
never negotiates with live surroundings. Boundary splices, the thing that made workshops
large and search-dependent (A·F spent a 16-cell patch mostly on boundary zippers and an
overpass dodging the live Apply cable; T1·S hit the 63-slot cap), are not in the blocklet at
all: they stretch from the seed. Estimated blocklet sizes: A·F ≈ 2-4 cells (fresh T1 and Pair
are mutually adjacent), T1·S ≈ 8-10 (5 fresh plus 2-4 wire cells closing the Dn/A cycle).

Growth. Per step the seed commits two small transactions: a one-cell CAS reserving the next
cell of the blocklet's fixed order (§3 corollary), then one edge transaction writing its
final matter and advancing pc. Placement order is wires first, then agents leaves-up; a
placed agent whose partner is not yet placed has a port facing a reserved cell, which is
safe (agents act only through their principal, and the reserve mark keeps foreign matter off
the claimed edge). When pc reaches the end, the seed cells rewrite themselves into their
final route matter, the observer emits the one semantic `RewriteFire`, and the fire is
complete. That resolve step is the linearization point.

Blocked growth. If the wanted cell is occupied, the seed waits and pumps χ; the occupant
relaxes away under the ordinary ownership rule. No request tree and no lift fallback: the
failure granularity is one cell, and the grow direction is chosen at dock time from a
stale-safe emptiness hint. The current all-or-nothing failure class (both lifts blocked on a
reserved 63-cell region) becomes a per-cell wait.

Reversibility. Until resolve, everything placed is nursery matter: fresh agents carry the
seed's cooldown and do not move or dock. If a wanted cell is permanently lost (occupied by
matter that will never relax, for example another seed's resolved output), the seed retracts
its placements in reverse order, reverts to the docked pair, and re-docks in a different
orientation; between two competing seeds the lower cell address has priority and the other
retracts. Because the fire is observed at resolve, a retracted attempt never happened
semantically. Linearizing at dock instead, and dropping retraction entirely, is a later
optimization that becomes safe once pressure relief is measured strong enough to guarantee
completion.

## 7. What each rule costs

| | v1 workshop | v2 |
|---|---|---|
| Unp·Pair | 2 slots + request tree | dock transaction only |
| Eps·L | 2 slots + request tree | dock transaction only |
| Eps·S, Eps·F, Eps·Pair, Eps·P | small workshops | dock + 1-2 emissions |
| A·F | 16 slots, hand-built, overpass | dock + ≈2-4 emissions |
| T1·S (largest) | 63 slots at the U6 cap | dock + ≈8-10 emissions |

Erasure needs no special machinery: Eps walks (arity 1, pure particle) and docks; the death
pulse is literally a cascade of dock transactions.

## 8. Fields

χ (obstruction pressure) and σ (consumer shell) survive as hints. They gate approach and
choose grow directions; they are never correctness reads. Under parallel execution their
values are stale by construction and that is fine; relaxation runs as low-priority
single-cell CAS transitions, or piggybacks on transactions touching the cell. The ownership
rule is unchanged: pressure never grants authority to edit another occupant.

## 9. Concurrent execution over AtomicU64

Storage. Dense tiles of `AtomicU64` (8×8×8 = 512 cells, 4 KiB) in a sparse tile directory;
an absent tile reads as all-empty. Replaces `BTreeMap<Pos, CellWord>`; cell address is index
arithmetic. Adjacent cells share cache lines, and cascades are spatially coherent per thread,
so tile-to-thread affinity keeps line bouncing low (the tile discipline is already validated
by the eager evaluator's tiled-drain work).

Worklists. Per-thread deques of cell coordinates with work stealing (donation of whole
neighborhoods, as in the tiled reducer). Stealing migrates a cascade to an idle thread.

Transaction protocol, per popped cell `a`:

1. Load `a` (Acquire). If no transition is enabled by `a`'s word alone, drop.
2. Compute the partner `b` from `a`'s word (principal face, grow face, ...).
3. Claim in address order: CAS the claim bit into min(a,b), then max(a,b). If the second CAS
   fails: clear the first, randomized backoff, requeue `a`.
4. Both claimed: the words are frozen (claim gates every writer). Re-validate the enabling
   predicate against the claimed values; the world may have changed between 1 and 3. If
   stale: release, requeue.
5. Write `b'` then `a'` with claim bits cleared (Release).
6. Wake: push the face neighbors of `a` and `b` (after the release stores, so a concurrent
   pop that found them disabled gets re-queued rather than lost). Duplicate queue entries
   are idempotent because enabling is re-checked at pop.

Properties. ABA is harmless (the claim bit, not value equality, is the mutual exclusion).
Deadlock-free (address-ordered acquisition). Not strictly lock-free (a preempted claim holder
stalls that one edge briefly) which is acceptable for a simulator; claims span a few dozen
instructions and no allocation. Contention arises only when two threads' wake fronts touch
the same edge: two cascades meeting. On the probe corpus the expected steady state is zero
failed CAS per thousand transactions; measure it.

Where locks live is where the semantics says computation lives: a rewrite is by definition
two waves meeting at a dock edge, and that edge is exactly where the claim contention can
occur. Spatial traffic collisions (crossing cascades that are not a redex) are the only other
case, and they resolve by one side losing the claim race and retrying. The user's constraint
"locks only when cascades meet" is not just achievable; it is the design.

Observation. Per-thread event logs; semantic fires take a global atomic sequence number
(linearization). Projection checks run at quiescence (all worklists empty, no claims), not
per activation. Transport and rewrite counters are per-thread sums.

Determinism. Parallel runs are schedule-nondeterministic but confluent; the gate is oracle
equality of normal forms plus invariant checks, exactly the suite's existing shape. For
reproducible debugging, the serial runner with a seeded queue discipline replaces the four
adversarial seeds with four disciplines: FIFO, LIFO, seeded-random, address-ordered. Add a
deterministic interleaving fuzzer (simulate K logical threads with a seeded scheduler) for
race hunting, and a loom-style model check of the claim protocol on a 3-cell grid.

Memory ordering. Claims and commits Acquire/Release; heuristic reads Relaxed. No fences
beyond that: no correctness read crosses an unclaimed cell.

## 10. Silicon

Two lowerings of the same transition system.

Clocked, activity-gated. Each cell latches a dirty bit when any face neighbor commits; only
dirty cells evaluate (clock gating: power scales with active-front area, the same economics
as event-driven neuromorphic arrays). A tick is two phases: propose (each active cell asserts
at most one face request derived from its word) and commit (each cell arbitrates among
incoming requests by fixed face priority, winner's pair transaction commits). Race-free by
construction; a moving agent advances one cell per tick, literally.

Self-timed. Per-edge request/acknowledge wires and a mutual-exclusion element per cell;
quiescent cells draw nothing. This is the delay-insensitive circuits-on-asynchronous-CA
lineage (Lee and Peper's self-timed cellular automata), with arbiters exactly at conflict
points.

Per-cell logic. About ten transition families (move, trail detour, reserve, dock, extrude,
seed resolve, retract, flip, hot pump, field relax), each a comparator/mux network
over two 64-bit words, plus one shared ROM per tile for the unfold scripts: 26 rules × ≤10
steps × ~8 bits ≈ 300 bytes total, shared, versus per-cell access to workshop patches of up
to 63 slots × orientations today. No search hardware exists anywhere because nothing searches
at runtime.

## 11. Deleted and kept

Deleted: `compile64.rs` (search, seats, rip-up, rotation cache), the request-tree rewrite
control (roles, phases, slots, lift fallback, Place/Placed waves), the translate handshake
(3 roles × 5 phases), Zip/Cross/GuestZip/GuestLink as kinds, twist and aux_flip and the
lane-adapter rules, `sweep_permuted` and `activation_order` as the runner (optionally kept as
a legacy adversarial harness), 28-bit protocol control.

Kept: `rules.rs` (unchanged, still the validated spec), `net.rs` shadow net and `oracle.rs`,
projection methodology, suite scenarios and expectations (step counts change), the trace
schema and player (events become transactions; sweeps become generations), χ/σ and hot
semantics, Full3D as primary topology (Bilayer still works: capacity-two cells encode
crossings without a Cross kind).

## 12. Migration

Rung 0 (cheap, land anytime): swap the sweep for a worklist scheduler on the current code.
The current protocols are delay-insensitive, so correctness holds; sweeps become optional
adversarial tests. Buys the perf (O(active) instead of O(sites) per round) and makes the
player's time physical. Buys nothing on silicon complexity, latency, or threading.

Rung 1: `cascade.rs` with word v2 and the serial edge-transaction engine; port movement.
Gate: translate scenarios pass with one cell per generation and the projection oracle green
under all four queue disciplines.

Rung 2: dock/seed/blocklet for A·F, then generate blocklets for all 26 rules (build-time
embedder plus a test asserting every rule embeds within budget under the wires-first order).
Gate: rule-atlas parity, cascade scenarios, and the arity-three crossing scenario that
currently stalls (`Unp·Pair` after `T1·S`) completing via detours.

Rung 3: relaxation micro-rules (retract, flip) and χ tuning; run the probe corpus; compare
completion floors against the unpacked stack's bar (394/400 full3d). Judge on the aggregate,
not per-config pins (the chaotic-margin lesson).

Rung 4: the `AtomicU64` parallel runner, interleaving fuzzer, loom check, contention bench
(failed-CAS rate and scaling on the corpus at 1/2/4/8 threads).

Rung 5 (done 2026-07-22): delete the superseded modules and the old spec, retire
the old bundles, and regenerate the player on generations.

## 13. Risks

1. Seed liveness and retraction. Growth depends on pressure relief clearing transient
   squatters (the same dependency today's blocked workshops have, at one-cell granularity),
   and the reversibility machinery (nursery cooldown, reverse-order retraction, address
   priority between competing seeds) is new protocol surface that needs its own adversarial
   tests. Retraction is bounded (a seed retracts at most its own placements) but seed-vs-seed
   retry loops must be shown to terminate; address priority breaks symmetry, the fuzzer is
   the test.
2. Slack. Stretchy boundary wires and detours create more transient wire than preplanned
   patches; the design leans on retract/flip/tension to keep lengths bounded. The JS-era
   lesson said contraction without exhaustive routing lets wires run away, but that failure
   came from router-dragged long routes; v2 never routes, it extrudes and detours by one
   cell. Must be measured, not assumed (rung 3 gate).
3. Blocklet embeddability. Every rule's fresh graph must embed in the fixed budget with the
   wires-first order; enforced by a build-time test; cycles get elbow wire cells. Bilayer
   embeddings need their own check.
4. Livelock between colliding cascades. Address-ordered claims prevent deadlock, not
   ping-pong; randomized backoff plus χ waits should settle it; the fuzzer is the test.
5. Field staleness under threads may shift the approach/eviction tuning; re-tune on the
   corpus aggregate.
6. Trace nondeterminism in parallel mode; the player consumes serial-mode traces, parallel
   mode is for throughput and is gated by oracle equality.

## 14. Prior art

- HVM2 (Higher-order Virtual Machine): lock-free interaction-combinator runtime; atomic wire
  relinking with CAS and per-thread redex bags. The claim discipline here is the same shape
  with geometry added.
- Lee and Peper: delay-insensitive circuits embedded in asynchronous cellular automata;
  universality with small rule sets; arbitration only at explicit conflict points. The
  self-timed lowering is in this lineage.
- Margolus partitioned CA: the clocked two-cell-block degenerate case of edge transactions.
- Chip-firing / abelian networks: commuting local firings give schedule-independent results;
  the formal backbone of the confluence obligation in §2.
- Gimenez and Obwaller, ia2d (FSCD 2016): interaction nets on a lattice with forwarder-cell
  wires and density-driven migration; positioned in `SPATIAL_IC.md` §9. This design differs
  by demand-driven motion, transactional atomicity, and grown rewrites.
- Address-event neuromorphic arrays (Loihi, SpiNNaker): silicon precedent for activity-gated
  event-driven grids.

## 15. Implementation record (2026-07-21)

What landed matches §1 to §12 with these deviations, each forced by a measured failure:

1. Split aux endpoints. The single tail face of §4 was wrong: an arity-three walker needs
   both lanes of every edge it crosses, so one parallel foreign lane blocks it forever.
   Agents now store an independent (face, lane) endpoint per auxiliary, which makes the
   detour representable: aux two enters the moved agent through a side face. The word:
   kind 2, payload 36 (agent = tag 4, principal 4, aux 4+4, passthroughs 2+16, nursery 1,
   cooldown 1), cursor overlay 21, chi 4, claim 1.
2. Walks eat slack. When the walker's own aux cable already runs through the target cell,
   the walk absorbs that segment (truncation) instead of laying trail. Without it a
   walker strangles on its own detour zigzag.
3. Demand gating is load-bearing, not optional. Eager (calm) walking let undemanded
   values clog corridors into knots; per-route hot bits pumped from consumer principals,
   one cell per generation, are the only walk license (plus chi descent). Trails lay cold.
4. Growth merges. Blocklet placement absorbs pre-existing routes as passthroughs (the
   guest principle applied to growth) and wire cells carry reservations too. The two-cell
   fusion rules resolve in the dock transaction itself.
5. The cursor script is two-phase: place everything (nursery), resolve on the consumer
   seed while every fresh agent is still seated (so observer ids bind correctly), then a
   finalize pass. A cell can host a walker and the cursor at once; the agent acts first.
6. The dock's roll choice is a stale-safe heuristic ladder: prefer a roll whose whole
   footprint currently merges, fall back to a clear first ring (whose cells wake the pair
   on change), and never claim the axis row or the stub cells.
7. The loader routes every link through a dedicated corridor (dive, run on an exclusive
   row, climb), so initial wires cross only perpendicularly and never share edge lanes.
8. Retraction (the reverse cursor) exists and is exercised only by unit paths; the
   chi-threshold abort trigger was removed with the rest of the pressure pumping, which
   livelocked as pump-decay cycles. Blocked actors wait silently; pure event-driven
   waiting cannot livelock, and every waiting site is woken by its blocker's next change.
9. The relief rung landed: a growth-blocked cell evicts one cold route per activation
   (corner-cut through the bend's diagonal; a three-cell parallel shift for straights; a
   five-cell out-of-plane bracket when the diagonal is taken), preferring evictions that
   unblock the merge outright and otherwise making progress one route at a time. Only
   routes that are cold on both continuations and continue into plain wire cells move.
10. Demand crosses squatters: the heat predicate looks through chains of guests (and seed
    passthroughs), and a guest relays wakes across its pass routes, directionally, so the
    wave neither dies at a crossing nor ping-pongs between adjacent guests.
11. A consumer can be a swap partner (producers still initiate): a walker whose wire
    loops through its own consumer's cell would otherwise park one step from docking.
12. One-cell hairpins collapse by truncation: when the wire U-turns in the next cell and
    re-enters the walker's own cell as its passthrough, the principal re-anchors onto the
    passthrough's far end and both slack segments vanish (two cells, one commit).
13. The dock's roll ladder gained a middle tier: whole-footprint mergeable, then
    gate-ring mergeable, then gate-ring merely unclaimed (growth waits and evicts).
14. Relief recurses (2026-07-22). When every eviction shape is boxed in, the blockers
    themselves become depth-bounded eviction targets: full or lane-starved side cells,
    the blocked route's continuation cells, and agents shedding their own passthroughs.
    A blocked walker, dock, or detour relieves its own cell with the same primitive (an
    arity-three walker over a doubled foreign cable cannot vacate otherwise, and a seed
    inherits at most one passthrough). Two length-decreasing moves joined the set: the
    U-turn splice (a wire folding back through one neighbor splices out there, even
    under a reservation, since it strictly reduces occupancy) and the inverse shift (a
    cold three-cell detour pulls straight when the bypassed cell is eligible). A second
    eviction pass may move hot routes as the last resort in an all-hot pinch; heat rides
    along. Receivers of displaced routes carry a cooldown stamp that holds off the next
    few displacement attempts (attempt-count decay, not ticks, so parked pockets cannot
    freeze it); this damps displacement ping-pong.
15. Seed-vs-seed arbitration (2026-07-22). Colliding blocklets compare origins: a
    cursor folds its consumer-seed address back through its script hops, so no word bits
    are spent. The higher origin flips its cursor to reverse pre-resolve (the unwind
    restores the docked pair, which re-docks later); post-resolve cursors never yield
    because the fire is irrevocable. Pinned by a forced two-seed collision test and
    exercised organically by the deep terms.
16. Parallel growth (2026-07-22). Dock, place (reserve then merge), hop, resolve, and
    finalize are one- or two-cell claimed transactions in the threaded driver, sharing
    the dock's roll ladder with the serial runner through a read closure; the gather
    driver inherits growth through the shared decision function. The full atlas grows,
    resolves, and finalizes under claims, bit-identical across 1/2/4/8 threads.
    Retraction, arbitration, and relief stay serial: a blocked op waits, so congested
    runs park earlier in parallel than serially. Landing this exposed a latent executor
    hole: popping a transiently claimed cell used to drop its wake (the claimant can
    commit without changing the word, so nothing re-wakes it); such pops now requeue.

Verified: the full 26-rule atlas fires and projects under four adversarial queue
disciplines; `@(L,L)` normalizes end to end under all four (four chained fires) and
`@(F(L,L),L)` normalizes to `L` in six chained fires exercising growth, eviction, guest
crossings, and hairpin collapse; the threaded driver commits about 1.1 million transitions
per second with conflicts only where fronts touch (0 disjoint, about 0.5 percent crossing,
results bit-identical across 1, 2, 4, 8 threads); the gather driver is deterministic and
bit-identical run to run. Traces replay in `lattice_player.html` via `lattice_cascade.js`,
one frame per generation, so a displayed tick is the substrate's maximal simultaneous
step; each trace reports its parallel width (the crossing demo runs at mean width 9.4 and
peak 18; small sequential terms sit near width 1 with peaks of 9 where heat waves and
walks overlap).

Open frontier, pinned by `frontier_deep_reductions` (floor: 2 of 5 deep terms complete;
fork-dispatch and the K combinator reach their normal forms, and every parked term runs
several rewrites deeper than before the relief rungs). The remaining parked runs are
corridor knots: a demanded arity-three walker sits over a doubled hot foreign cable
whose relief chain bottoms out on cells nothing may touch (reserved targets, other
walkers, all-hot neighborhoods), and a growth chain wedges behind it. The candidate
levers are smarter trail-lane assignment during walks (avoid creating doubled cables
under future walkers) and relief that can reroute through a walker's own cell. The
substrate never computes a wrong answer; wedged runs park validly with the seed visible.
