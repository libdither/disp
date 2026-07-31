# AC_IDEA: the amorphous-chip direction for the cascade reducer

Where the cascade substrate (the 3D lattice of 64-bit cells running interaction-net
reduction) must end up to be siliconizable, and the path from today's shared-memory
drivers to a chip of identical message-passing cells. Revised 2026-07 after the design
review: the design is now organized around one invariant — every state change is either
monotone (a fact that never un-becomes true) or a change of mind — with a passive fabric
for the first and a tiny claimed-transaction machine for the second. The physics below is
validated by the current suite; the architecture is not yet built. Delete this file once
the chip exists and the code reads better than it does.

## The one invariant: three planes

- **Routing plane** — the road map: each cell's route table (12 half-edges = 6 faces × 2
  lanes, ≤ 3 pairings). Written only inside claimed commits; read only by the signal
  fixpoint. This is the bridge between the two worlds.
- **Signal plane** — monotone facts (heat, wakes): a least fixpoint over routing plus
  demand sources. No mutual exclusion anywhere, ever: raises commute. Platform contract:
  raise-only (no `clear` — state only grows), queries may err stale-cold but never
  stale-hot, invalidation by route-epoch death (signals die with their route; no sweeps).
- **Dynamic plane** — the occupants (agents, cursors, claims). Computation is claimed
  ≤2-cell commits, byte-identical on silicon/CPU/GPU; only the exclusion mechanism
  differs (arbiter grant / address-ordered CAS / domino phase).

The premise is unchanged and load-bearing: schedule-independence is proven
(`schedule_fuzz_never_wrong` — any local execution order may park but never answers
wrongly). That one property licenses races on chip, lazy signal evaluation on CPU, dense
idempotent passes on GPU, and every timing difference between platforms. Guard it above
everything. Its event-completeness dual is pinned beside it
(`kick_after_quiescence_is_a_no_op`): re-waking the whole grid after quiescence moves no
progress counter — parked means genuinely wedged, never forgotten.

## Constraints (the frozen contract)

- **Words are 64 bits, layout frozen.** On silicon the claim bit and the reservation
  marks are *arbiter state*, not word bits. Know the bit classes before spending:
  semantic core (kind, routes, agent endpoints, cursor pc) · correctness-of-mechanism
  (nursery — keeps un-resolved growth inert; its 2026-07-30 witnesses (Lifo/tree
  no-quiescence, 4-6/80) VANISHED once the reel landed, so the classification is
  currently unproven and the bit is a candidate capacity win pending an adversarial
  construction — the ablation pin tracks the count; AND cooldown — measured 2026-07-30:
  dropping the stamps livelocks 21/48 random terms in displacement ping-pong, so it
  guards quiescence, not comfort; answers stay right either way) · capacity (3rd route,
  pc width) · heuristics (χ only —
  and χ is currently inert in the serial driver: nothing pumps it since the pressure-wave
  removal; it re-enters with the demand-priority rung or its bits go to capacity).
  `tests/invariants.rs` pins all of this (ablation lanes + the negative control).
- **Commits touch ≤ 2 adjacent cells — as the target.** Current exceptions must be
  re-expressed as bounded token chains before silicon: the aux-detour walk (4 cells) and
  relief evictions (4–6 cells). Arity-1 teleport (below) is the one sanctioned amendment:
  a worm, not a commit. Measured per-activation write sets (2026-07-30, pinned as
  only-move-down ceilings in `tests/invariants.rs`): move 4 · growth 4 · fabric/relief 6
  · dock 2 · resolve 2 (the seated splice is already at the contract).
- **No combinational long reads.** One exception left: the ~60-cell dock-roll scan
  (host-assist preferred; else a scout token, hard ≤64-hop bound). The other two are
  gone (2026-07-31): the 5-hop demand lookahead is deleted — consumers RAISE, hot wires
  extend one cell per activation, guests relay one hop per generation, and the walk gate
  just reads its own edge; the every-5-generations contraction sweep is deleted — every
  matter-freeing commit already wakes its neighborhood, the whole corpus completes
  identically without it, and the kick invariant trips if a contraction ever goes
  un-woken. Measured read radii (Chebyshev, pinned): dock 11 — that IS the roll scan —
  everything else ≤ 2.
- **Frozen op alphabet and transition templates.** No runtime search on chip; growth runs
  compiled microcode in a per-tile ROM (cursor = program counter). Compile-time search
  stays host-side.
- **Small arithmetic only.** Compares, increments, saturating adds on ≤ 8-bit fields.
- **Bounded fan-in/fan-out.** Own state + 6 neighbors' presented lines; ≤ ~7 wakes per
  written cell. No floods, no broadcast.
- **Idle means dark — now structural.** Wire cells are unclocked (config + pass gates +
  hot latches); signals switch once per route lifetime.
- **One outstanding transaction per cell**, released by `done` or a bounded hold timer.
  Deadlock freedom: address-ordered claiming in software = a fixed cross-request
  tie-break in silicon.
- **Bounded array, host machinery off-chip, metastability budget** (2-flop synchronizers
  before the arbiter; per-tile-sync vs fully-async is an early decision).

## The cell

- **~30-bit semantic core**; the 64-bit word is explained by co-residency (a cell hosting
  a walker and a growth cursor at once: kind 2 + payload 36 + cursor 21 + χ 4 = 63).
- **Wire cell = fabric:** 66 pass gates (one per half-edge pair), 24 config bits
  selecting ≤ 3 pairings, 3 hot latches, decode. No clock, no FSM, no arbiter. A wire
  cell is a configurable piece of metal with three one-bit memories. The same pass gates
  are the data path for flits (teleport below).
- **Agent/seed cell:** a 6-input first-come mutex arbiter (request latches + grant
  register + metastability filter; direction-lock until `done`; losers get busy) and an
  FSM of op-decode → combinational two-word transform → write → forward/`done`, plus a
  ≤6-step sequencer for multi-commit ops. Sequencing lives in the cursor pc (growth) and
  the handshake pipeline (movement), not in the FSM.
- **The executor reads arity from live endpoints** (0/1/2 aux = arity 1/2/3); the tag is
  consulted only at dock rule lookup. The walk datapath never reads the tag.

## Reduction: three tiers (26 rules → 3 mechanisms + 2 generators + 9 scripts)

The ROM is 7 structural rules (the apply + two-level-triage semantics) plus 19 instances
of generic structural machinery (erasure = weakening, duplication = contraction, forcing
= a modality, fusion = annihilation) that is uniform in the producer's *shape*. The
generic half becomes mechanism:

- **Tier 0, in the dock commit (no growth, no cursor):**
  - *Fusion* (`Unp·Pair`, `Eps·L`): pure re-routing; already fires in the dock today.
  - *Erasure* (`Eps·{S,F,P,Pair}`): emit one eraser flit per producer aux down its cable;
    both cells die. With teleport the cascade runs at wire speed.
  - *Forcing* (`×P`, five rules with byte-identical wiring): a 2-cell in-place relabel —
    the producer cell becomes the fresh `A` (its aux cables are already on the right
    faces), the consumer cell is re-presented facing it. Zero new area.
- **Tier 1, two parameterized generators:** the Dn and Nrm traversals are the same loop
  (walk producer arity, push the agent) with different payloads — 6 rules → 2 loop
  programs.
- **Tier 2, the real ROM:** `A·{L,S,F}`, `T1·{L,S,F}`, `Sel·{L,S,F}` ≈ 9 blocklet
  scripts (the `T1·L` ≡ `Sel·L` shape is shared). Sel·F's 62-cell worst case stands:
  area is conserved, so the only lever is *when* the footprint is claimed (prefetch
  rung).

## Movement

- **Arity ≥ 2 walks** as today: 2-cell commits, trails laid cold, one cell per tick.
  Irreducible — the trails are the net's edges being re-anchored.
- **Arity-1 teleport (L, Eps — most traffic).** An arity-1 walker's intermediate
  positions project to nothing (the net edge is identity), so: source → empty, target →
  agent, dead cable → lazy slack erasure. Silicon: a 2-flit worm down the cable's own
  established route (the crossbar is the data path). CPU: a union-find far-end lookup +
  two writes + a free-list push. GPU: decomposes back to walking — a timing difference,
  licensed. Requires: an abort leg (the target may fire mid-flight) and the contract
  amendment above. The step-wise CPU precursor LANDED 2026-07-31: the reel — arity-1
  ERASERS walk their own hot cable, eating it cell by cell (no aux, no trail, so the
  cable self-erases behind them). This closed the standing-dead-matter hole: an
  eraser's cable could end in a terminal U-loop through its own cell, leaving the
  arriving producer parked on an undockable face forever; census discard-tree flipped
  from flat-cost dead matter to full erasure cascades (all seated, zero growth), and
  convoy(1) completes as a side effect. Polarity keeps the reel chase-free (consumer
  principals only ever face producer principals; a cable walked from both ends
  strictly shortens). Teleport proper replaces the multi-step reel with the
  components-backend far-end jump — same semantics, licensed timing.

## Demand as priority

- Heat is a **priority gradient, not a license**: demand owns the resource, speculation
  rents slack. Shoves are preemption (existing); cooldown stamps are damping (existing).
- **Classify every signal by monotonicity.** Identity (routes, heat, wakes, slack
  discovery) rides the fabric. Revocable state (χ, reservations, bids) stays clocked and
  local — a combinational revocable signal is a same-tick feedback loop (the pump-decay
  livelock, removed 2026-07, is the standing witness).

## Edge API (per face, per direction)

- **Fast:** 2 lane lines (bidirectional heat — the crossbar conductors) + 1 wake pulse.
- **Slow:** `req` + `op[3:0]` + `lane` · `ack` + `status[1:0]` · `data[7:0]` · `done` —
  ~16 wires. Flit sized by the largest atom (one route entry); agent state = 2 flits.

## Migration

0. **Signal-plane trait in software** (serial driver first): raise-only, epoch-keyed,
   three backends (worklist / union-find / dense bitmap); `decide()` generic over it.
   Gate: bit-exact under all queue disciplines; `hot_beyond` and the sweep deleted.
   LANDED IN FULL 2026-07-31 (`signal.rs`): worklist (in-word wave, one hop per
   activation, heat persists to route death — an over-approximation of demand),
   components (union-find over route reciprocity: whole cables heat in one instant and
   guest chains are exact — the model of the unclocked fabric, and the substrate for
   teleport's far-end lookup), dense (the same fixpoint by a deliberately independent
   iterative recompute). The derivational pair rebuild from matter whenever the grid's
   routing epoch moves (a structural-signature counter: kind/routes/endpoints/pass/
   nursery only); sync's diff delivers the wakes the wave used to carry, and the kick
   invariant polices that chain. Both scans deleted; raises commute (property-pinned).
   Cross-backend gate finding: completion is PER-CASE IDENTICAL across all three
   (28/54 gate corpus, zero verdict flips) — completion robustness extends across
   heat-persistence semantics, so the worklist's stale-hot bits are not load-bearing
   and the chip's exact-instant fabric loses nothing. Worklist stays the software default (fastest
   simulation, shortest rebuilds); clocking the signal costs it generations +13–35%
   on the census critical path, which components removes.
1. **Tier-0 mechanisms** (erasure emission, ×P relabel) in all drivers; ROM shrinks to 2
   generators + 9 scripts. Gate: atlas + stage1 + cascade_suite.
2. **cascade_msg**: the message-only driver, bit-exact against serial on the existing
   suite. The sufficiency test for the API before any Verilog.
3. **Arity-1 teleport**: CPU first (union-find makes it nearly free), then as a worm
   protocol in the message driver.
4. **Cycle-level simulator**: pins fabric cells-per-clock, synchronizer latency, arbiter
   behavior.
5. **Per-tile ROM** (now much smaller) **+ Verilog.**

## Open rungs

- **k-chain knot** (the stub-lock at crowded dock rings). Bounded ring relief LANDED
  2026-07-31: a declined dock picks the roll with the fewest blockers (≤ 2), relieves
  exactly one per activation with the existing primitives (evict for wires, sidestep
  for producer squatters), stamped, never docking in the same activation (the locality
  audit holds the dock commit at 2 writes; the blocker scan joins the roll scan's r=11
  read-exception family). Moved every parked frontier term deeper (s-rule 2→5 fires,
  k-chain 7→10) without regressing anything; unbounded ring clearing stays forbidden.
  The NEXT measured gap: routes threaded through AGENTS' passthrough lists refuse to
  swing (Dn·L declines on pass-shedding whose both continuations live inside adjacent
  agents' pass entries — relief needs an agent-hosted continuation move, a 3-cell
  claimed op, or the footprint-prefetch / single-lane levers). Still on the table:
  footprint prefetch (claim the roll ring during approach), the single-lane experiment
  (deletes the lane-conflict class at 2× cable width).
- **disp-t knot**: walker convoys; less characterized.
- **Relief as bounded token chains**, depth as a synthesis constant.
- **Teleport abort leg.**
- **Roll scan**: host-assist (preferred) or scout-token wave.
- **Dock-time clump fusion** (see below).
- **Chip-power audit** of every remaining multi-cell signal. **Word freeze stands.**

## Clump rules (a calculus-level direction, gated on measurement)

Today a rewrite locks exactly 2 cells and grows the result; a "clump rule" locks a larger
assembled structure and rewrites it in one transaction. Two different things share the
name; keep them separate:

- **Spatial clump** = claiming a bigger footprint around the dock (the prefetch rung
  generalized). Substrate-level, coherent, no semantics change.
- **Semantic clump** = multi-agent rules. The triage path is the target: fusing
  `A·F` + `T1·x` into one 3-agent rule recovers direct tree reduction
  (`F(S s) b` applied to `x` ⇒ `(s x)(b x)`; `F(L) b` applied to `x` ⇒ `b`) and skips an
  entire growth episode (the A·F blocklet) per triage — the congestion hot path. The
  preferred first form is **opportunistic dock-time fusion**: when `A·F` is about to
  fire, a 1-hop stale-safe check whether the discriminant's head is already docked on
  `F`'s aux face; if so, claim the third cell (address-ordered) and fire the fused rule;
  else fall back to the pair rule. Detection stays 1-hop; no waves. Full T1/Sel removal
  (always-fused, lazy `A·F`) is a calculus change: new spec, an orthogonality/confluence
  proof obligation (fused and pair rules overlap; the crown-jewel property is at stake),
  and a termination-domain re-check. Sandbox it in the abstract net (`net.rs` +
  `oracle.rs`) against the eager oracle before any lattice work; pursue only if the
  tracer shows triage-path growth episodes are a top cost on the frontier terms.

## Rejected (recorded so they stay rejected)

- **Standing multi-cell agents**: corridors are one cell wide; movement becomes a wave;
  worst-case sizing (Sel·F = 62 cells) is unaffordable at rest. (Footprint *prefetch*
  keeps the benefit without the standing cost.)
- **Tag-by-routing**: routing configuration is spatial accident, type is semantic;
  log₂(12) bits must live somewhere and the 4-bit tag is already minimal. What survives:
  arity from live endpoints; the tag read only at dock.
- **χ/pressure pump waves**: livelocked as pump-decay cycles (2026-07). Revocable state
  stays clocked or token-carried; only monotone facts ride the fabric.
