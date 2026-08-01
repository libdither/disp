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
  (nursery — keeps un-resolved growth inert; LOAD-BEARING, re-proven 2026-07-31 when
  endpoint swings landed: with the bit ablated a half-grown agent looks swingable and
  relief re-anchors its ports mid-growth — disp-t quiesces seed-free but no longer
  projects; the ablation pin holds the witness as a floor) · capacity (3rd route,
  pc width) · correctness-of-mechanism again (cooldown — its 2026-07-31 demotion to
  heuristic lasted a day: the order + edge sweep took undamped livelocks 21 → 6 → 0,
  and then the pays-for-itself order exemption put them at 3, because an exempted
  move's payment can be stolen before the placement it bought commits. The stamps
  carry that residue, so the bits stay spent. Answers stay right either way, which is
  all the ablation lane asserts now — the count is a ledger, printed not pinned, since
  it moved three times in one day and a control that needs hand re-pinning per
  mechanism change is measuring the mechanism) · heuristics (χ —
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
  RESOLVED 2026-07-31, k-chain COMPLETES (frontier 3/5): live-cable relief is the
  COMPOSITION of four disciplines, each killing one measured pump class — (1)
  guest-continuation swings (routes threaded through agents' passthrough lists bend
  like wire, one-word rewrite + guest stamp; nursery guests opaque) break the
  pass-threaded knot; (2) the requesting dock's ring is forbidden as a displacement
  receiver (relief strictly DRAINS rings — kills single-dock ping-pong); (3) only the
  address-lowest ready pair in the neighborhood runs ring relief (cycles need two
  pushers; the claim-deadlock order leaves one); (4) the ROUTE-LEVEL displacement
  order: every eviction shape's primary direction must ascend a fixed linear form
  (`Runner.relief_g`, components distinct powers of 3 so no face or diagonal sums to
  zero) — a cycle needs net-zero total displacement, so none exists for ANY pair of
  requesters, at move granularity (whole shifted segments never straddle the order
  the way cell-level receivers did; splice/truncation stay exempt as strictly
  shortening). One relief primitive per activation throughout (locality audit holds
  fabric at the single-bracket footprint). The form's sign choice trades terms
  (chaotic margin); g=(−1,−3,9) chosen by aggregate: zero livelocks under every form
  swept, soak 99/160 under all, cooldown-off livelocks 21→6 (the order independently
  damps ping-pong). Cell-level orders measured and REJECTED on the way: per-dock
  forbidding alone doesn't compose (cross-dock shuttle), universal beside-any-dock
  starves corridors, per-receiver address-monotone kills straddling shifts. Still on
  the table for disp-t's remaining park: footprint prefetch, the single-lane
  experiment.
- **disp-t knot — RESOLVED 2026-08-01. THE DEEP CORPUS IS COMPLETE: 5 of 5.** The
  declined dock is T1·F (the 56-cell comb); its best roll has 3 first-ring blockers
  against decline-time relief's bound of 2, which is why footprint prefetch was
  nominated. The bound was never the wall on its own: sweeping it showed every setting
  terminates — unbounded ring clearing no longer livelocks, the displacement order took
  that job — and, at the time, with identical completion everywhere. Raised past 3 the
  dock fired and the comb grew to pc 112 of 202, then wedged on the ROUTE-LEVEL ORDER:
  the one free receiver for the blocking route descends `relief_g`, so relief refused a
  cell that was otherwise empty (`debug-cascade --why` now says so in as many words,
  and prints each roll's blocker list; reporting that refusal as "diagonal busy" is
  what hid this for a session). So the wall was the order, and the bound was what kept
  the term from ever reaching it. Both had to go, in that order: once the last-blocker
  relief could pay its way past the order (below), the bound was the only thing left
  holding the comb dock shut, and 3/4/8/unbounded became indistinguishable — soak
  129-130/160, every deep term complete — so no constant is kept at all.
  What does NOT work: a
  per-request potential (push matter away from the requester) is acyclic for one
  requester and fails for two — precisely the cross-dock shuttle measured and rejected
  above, and relief requesters include un-arbitrated walkers and cursors, so the dock
  arbitration does not cover it. What DOES work, LANDED: a displacement may violate
  the order when it PAYS FOR ITSELF — the route being moved is the one whose removal
  lets a blocked placement's matter merge, so the violating move is the last one
  before real progress, and progress is monotone and bounded by the reduction. Payment
  must be verified per ROUTE, not assumed per request: exempting every eviction a
  blocked placement asks for (including ones shedding some other cold route) re-armed
  cycles outright. Even verified, the payment can be STOLEN — relief and placement sit
  in separate activations for the write budget, so the cell can be re-crowded in
  between — which is why the exemption costs the cooldown bit-class its brief
  demotion: undamped livelocks go 0 → 3, the stamps carry that residue. Worth it at
  +5 soak completions (107 → 112) for two stamp bits already spent in the frozen word.
  The same argument then applied to a declined dock's LAST blocker (clearing it makes
  the ring whole, so the fire it buys is next) and that was the big one: soak 112 →
  129-130, and with the bound gone, disp-t through. Prefetch is not dead, just unmotivated:
  no term parks for the reason it fixes.
  One pump surfaced on the way, again by the exact-instant heat backend and not the
  default one: shoving a guest off a walker's hot wire by SIDESTEP is self-defeating by
  construction — the guest leaves its trail in the very cell being contested and its
  principal re-anchors to point back into it, so the demand it was shoved out of is
  what marches it home (soak term 8, forever). The sidestep is gone from that one
  caller; it keeps the callers where the cleared cell is not the cell pulling the guest
  back. Measured cost: one soak term. Worth recording for the chip: an agent's
  cooldown is ONE bit, so "stamp it harder" is not available as a damper for agents —
  a stamp of 3 silently fails to pack and refuses the move instead.
- **Endpoint swings + termination redesign — LANDED 2026-08-01 (two commits):**
  the S-SHARING RULE COMPLETES (frontier 4/5; 6→9 fires and then through once the
  futile-shed guard landed), soak 99→107. The termination unit (stamps + sweep + order
  extensions, measured mutually dependent so landed together): the WEAR branch of
  stamped evictions was a pump motor (wear-as-progress + wake kept any adjacent
  requester's retry loop alive) — stamps now refuse plainly, self-decay on the
  stamped cell's own activations, and expiry wakes the neighborhood; contraction and
  sidesteps are displacements and obey the g-order (pull/step only ascending, else
  relief-vs-contraction and sidestep-duel pumps); slack enabled at radius 2 (anchor
  cells) is found by the QUIESCENCE-EDGE sweep — when the worklist drains, wake
  everything once, re-armed only by real commits, making the kick invariant true by
  construction; and a hopeless growth merge never reserves (pre-check + relief,
  mutation-free refusal), else the reserve/release retry re-arms the sweep forever
  (soak term 26's 20M-activation "livelock" was this — a genuine wedge that couldn't
  park). The capability unit: terminal-port re-anchoring (the same one-word swing as
  guest passes, agents' own principal/aux ends), consumer squatters sidestep off hot
  wires (consumers never walk), and an over-full squatter sheds its own passthrough
  as the shove fallback. Both earlier reds resolved: term 26 parks in 52 generations,
  competing-seeds fires both (the futile reserve cycle, not the order, was starving
  the loser's re-fit). The derivational backends then found one more hole the
  worklist's staggered heat had been masking: shoving a guest that has live demand of
  its own composes a sidestep (ordered) with its walk back (demand-driven, order-
  exempt) into a net-zero shuttle — under exact-instant heat the walk-back is always
  licensed, so two head-on erasers pumped forever. Fix per the rung's own design
  text: shove only guests with nothing of their own to act on; demanded guests are
  traffic and move themselves. Derivational sync also throttled (one O(grid) rebuild
  per max(64, cells/8) activations — the stale window is the worklist's own
  over-approximation, the interval a function of grid state so runs stay
  deterministic), and the components rebuild dropped its whole-grid re-unpacking;
  the backends gate went from stuck-for-an-hour to minutes.
- **Relief as bounded token chains**, depth as a synthesis constant.
- **Teleport abort leg.**
- **Roll scan**: host-assist (preferred) or scout-token wave.
- **Dock-time clump fusion** (see below).
- **Chip-power audit** of every remaining multi-cell signal. **Word freeze stands.**

## What is left, by measurement (`park-census`, 2026-08-01)

With every deep term complete, the capability frontier is the soak's 31 remaining parked
runs, and `cargo run --release --bin park-census` classifies them so the next rung is
chosen by distribution rather than by whichever term was looked at last. Today:

- **Walk wedges 15 → 13, the ASYMMETRIC SWAP LANDED** (`try_pass_guest`): a demanded
  walker exchanges cells with an undemanded stationary guest that hosts its cable, each
  one's cables becoming passthroughs of the other's new cell. Two cells, inside the move
  budget, carrying the guard all three of the session's pumps taught — only an
  UNDEMANDED guest may be displaced, or its own demand marches it back and the exchange
  is a shuttle. Measured: soak 129 → 130, blocked agents 31 → 24. Single digits, exactly
  as the ceiling below predicts; the point was to take the class's cheap half.
  The interesting correction came from the first draft's guards, which were too crude in
  a way worth remembering: refusing whenever the guest's principal pointed back at the
  walker excluded the commonest reason a guest is in the way at all — it is an argument
  DELIVERED into the walker's own aux. Those cables need no passthrough: both agents stay
  adjacent, so the paired ports simply swap ends. Cables crossing the shared face are now
  sorted into paired (a delivery, one lane, no passthrough), trails (my aux reaching back
  through the cell I left), and connectors (the guest's cables reaching forward through
  the cell it left), and the budget is `paired + trails + connectors ≤ 2`.
  **The residual is structural, not a gap in the rung.** An arity-3 agent has three
  cables that must all cross one face to move one cell, and a face carries two lanes — so
  it cannot be exchanged, and `try_sidestep` refuses it for the identical reason. An
  arity-3 agent at rest is IMMOVABLE; the only motion available to it is walking forward
  along its own principal, which consumes that cable and leaves only two trails to cross.
  So the remaining wedges of this class need the walker's cable routed AROUND the guest
  rather than the guest moved. That was tried the same day and REVERTED: simply letting
  the shed fire when the guest is immovable brings back soak term 95's pump. The
  plausible-sounding safety argument — an ordered shed and the ordered contraction that
  would undo it point opposite ways, so neither can cycle — is wrong, because walks and
  truncation are order-EXEMPT and cheerfully restore the geometry the shed just changed.
  The lesson generalizes past this rung: **an ordered move is only safe against a cycle
  if every move that could undo it is also ordered**, and this substrate deliberately
  exempts the shortening ones. So rerouting past an immovable guest needs the same shape
  as every loosening that has worked here — the shed must PAY FOR ITSELF, permitted only
  when the walker consumes the rerouted slack in the same breath, rather than permitted
  whenever nothing else can move.
- **13 declined docks** — see the ring-drain note in the disp-t entry: their traces are
  dominated by "in the requesting dock's ring", the per-roll relaxation unwedges 8 of
  them but they mostly re-park a step later and roll switching starts to oscillate. The
  safe version needs a receiver rule that provably worsens no roll.
- **1 growth wedge, 2 quiet parks** — tail, not worth aiming at yet.

Note the two classes are the same shape at different scales: something free sits where
matter needs to be, and the rule that keeps relief honest (the displacement order, the
ring drain) is what forbids using it. Both loosenings that have worked so far — the
pays-for-itself exemption, and this — take the same form: permit the otherwise-forbidden
move exactly when it is the last one before progress that the reduction bounds.

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
