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

## STEP 0 RESULT: the keystone below is REFUTED, and the premise was wrong

`cargo run --release --bin corridor-census` (2026-08-01) ran the falsification test the
plan called for, before any engine code. Two numbers, and the second one matters more:

- Corridors are not too short. They are enormous. Of the reactions that need growth, 92%
  vacated more cells on the way in than their blocklet needs (median corridor 161 cells
  against footprints of 6–24), and for the DECLINED docks — the 13-park class the whole
  rung was aimed at — it is **13 of 13 funded, median corridor 399 cells against
  footprints of 6–10**. Corridors are roughly forty times bigger than needed.
- **And they do not need the corridor at all: every stuck dock already sits in a
  neighbourhood that is 87% empty.** Median 299 free cells within radius 3, needing 6–10.
  All 13 have room for their footprint right now, without retaining anything.

So the premise underneath corridor-as-claim — and underneath "conserve reaction area",
and much of "make space" generally — is false. **Space is not scarce. It is abundant and
unusable.** A dock declines while several hundred empty cells sit within three steps of
it, because the handful of cells its footprint specifically requires are crossed by
cable, and because the rules that keep relief from cycling forbid relief from using the
empty cells that would clear them. The traces say this outright: term 11's refusals read
`shift D blocked: empty (ok), empty (ok), empty (in the requesting dock's ring)` — three
empty cells, refused, one of them purely because the ring rule reserves it.

The real constraint is therefore **rigidity plus self-imposed refusal**, not area:

1. A blocklet may only be placed in one of four rolls about a fixed dock axis. Four rigid
   shapes, and if a couple of cables cross all four, the dock declines with an ocean of
   free space beside it.
2. Relief could clear those cables into the free space, but the anti-cycle disciplines —
   the ring rule, the displacement order, the stamps — refuse exactly those receivers.
   Each was added to kill a measured pump and each is individually justified; together
   they have made abundant space unreachable.

That reframes the whole backlog and it is good news, because the cheapest fix is already
measured: narrowing the ring rule to the roll actually being cleared unwedged 8 of the 13
declined docks (see the untangling section), failing only because roll-switching then
oscillated. The specified safe version — a receiver rule that provably worsens no roll —
is a far smaller change than any allocator, and it attacks the constraint that actually
binds. Premoving keeps a role too, but a different one from the plan below: not to
accumulate claim, but to carry a pair to where a roll already fits.

Everything from here to the end of this section is kept as the record of a well-formed
idea killed cheaply by its own falsification test. Do not build it.

## The corridor is the claim (REFUTED at step 0 — see above; kept for the reasoning)

### The problem this exists to solve

Five mechanisms keep getting proposed and tried separately — agents premoving, wires
shortening locally, different agents' wires separating, one agent's wires zipping taut,
and reaction area being conserved. Three of them have now been built and reverted
(2026-08-01), each for a reason that names one of the others: untangling had nowhere to
put the cable *until cables are shorter*; freeing contraction fought relief, which
*lengthens*; the asymmetric swap only reaches small arities because there is no room to
route around. Read together, those are not three findings. They are one:

> **Every mechanism in the substrate today moves congestion around. Nothing creates
> space, and nothing owns space.**

Relief displaces, contraction shortens locally, the swap trades two cells. Congestion is
therefore always someone else's problem, resolved by ordering rules and damping stamps
because there is no notion of territory to arbitrate over. Every pump chased this week
was two parties disagreeing about who owns a cell.

### The idea

A hot cable between a producer and its consumer is *already a claim*. It is a contiguous
run of cells that provably belongs to exactly one pending reaction: nothing else may
legitimately occupy it, and both of its ends are the reacting pair. Today a walker eats
that cable as it advances and the vacated cells fall back into the commons — and then
the dock discovers it has no room and the whole relief apparatus starts up.

So: **stop surrendering it.** A walker advancing along its principal cable retains the
vacated cells as claimed area for the reaction it is walking toward, and does not dock
until the claim is big enough to grow the blocklet. Walking stops being consumption and
becomes conversion: cable length in, reaction area out. That is what "reaction area is
conserved" means operationally, and reduction funds it naturally, because cables shorten
as work proceeds.

This subsumes the other four rather than competing with them. **Premoving** is a walker
accumulating claim before it is needed, which is the only form of prefetch that helps a
dock that cannot yet fit. **Zipping** is what makes a footprint claimable at all, since
slack is what inflates the area a reaction needs. **Separation** keeps two reactions'
corridors from overlapping, so claims can be granted independently. **Local shortening**
becomes safe precisely because claimed space is off-limits to it — which is already
proven: the one rule that killed a shortening/relief cycle this week was "a shortening
move may not fill clearance a pending fire is waiting on," an accidental one-cell special
case of exactly this.

### Why it is affordable

The two pieces of state it needs are already in the design and currently idle:

- **Claim marks cost no word bits.** Empty and wire cells already carry a reservation
  field, and the frozen contract states that reservation marks are *arbiter state, not
  word bits*. The usual objection — the 64-bit word is full — does not apply.
- **χ is inert and this is its rung.** Four bits per cell that nothing has pumped since
  the pressure-wave removal, which the bit-class table says re-enter with a future rung
  or get demoted to capacity. A saturating claimed-area count is what they are shaped
  for. If the count needs more than 4 bits, that is a finding about χ, not a blocker:
  claims can be compared against a saturating ceiling rather than an exact total.

Footprint sizes are compile-time constants in the per-tile ROM, so "is the claim big
enough" is a comparison against a known number — small arithmetic on a small field,
inside the contract, no search.

### Silicon compatibility

A *centralized* allocator would be fatally incompatible: global free lists, manager
cells, and "is this region free" queries all violate no-broadcast, bounded fan-in, and
no-combinational-long-reads. None of that is required here, because the corridor form
never acquires territory — it declines to release territory it already holds. That is
the whole reason to prefer it over a claim-wave allocator.

What it does need is already sanctioned: bounded token chains (the sanctioned
re-expression of today's multi-cell relief), a hard hop bound (the scout-token
alternative is already specified at ≤64 hops, and the largest footprint in the rule table
is 62 cells), address-ordered acquisition as the deadlock-freedom rule, and bounded hold
timers for release. Note the prize: the ~60-cell dock roll scan is the last
combinational-long-read exception in the contract (it is why `dock` sits at read radius
11 while everything else is ≤2). A reaction that already owns its corridor does not need
to scan for room, so this rung is a candidate for *retiring* that exception rather than
adding one.

### The issues, and what to do about each

1. **Short cables.** Adjacent agents have no corridor to convert, and some of them need
   the largest footprints. Relief must remain as the fallback, unchanged, so the worst
   case is today's behaviour. The interesting sub-question is whether a walker should
   deliberately *stop one cell short* to preserve its corridor — an inversion of
   everything the substrate currently optimizes for, and cheap to test.
2. **Held claims starve everyone else.** Idle claimed area is area nobody can use, and a
   parked walker holding a corridor forever is a new deadlock shape. Release policy is
   where the first livelock will appear. Start with: a claim is released whenever its
   walker parks (no progress and no pending dock), and additionally on a bounded hold
   timer, which the contract already provides for outstanding transactions.
3. **Mutual starvation between two reactions.** A holds what B needs and vice versa.
   Address-ordered acquisition gives a fixed winner between two claims, the same
   tie-break that already breaks claim deadlocks and dock arbitration. The loser
   releases; it does not wait.
4. **The producer does not know its rule yet.** Which rule fires depends on both tags and
   it only learns its partner's on contact, so the budget must be its own tag's worst
   case over all partners — a compile-time constant, but an over-claim. Accept the
   conservatism; measure the wasted area in the census rather than guessing at it.
5. **Who may enter a claimed cell.** Claimed cells must be off-limits to relief
   receivers, contraction targets, and foreign growth — the generalization of the
   clearance rule that already works. They must remain traversable by the claiming
   reaction's own traffic, or the claim strangles its own dock.
6. **Interaction with the demand plane.** Claims must not become a second, competing
   notion of priority. Heat stays the priority gradient; a claim is a *reservation of
   space*, not a license to act. Nothing about raise-only monotonicity changes.
7. **Schedule independence must survive.** The load-bearing property is that any local
   execution order may park but never answers wrongly. A failed or released claim must
   only ever cause a park, never a different answer; `schedule_fuzz_never_wrong` is the
   gate and it must stay green under every discipline.
8. **The kick invariant must survive.** A claim that is released without waking its
   neighbourhood is a lost wake by construction. `kick_after_quiescence_is_a_no_op` is
   the gate; expect it to catch the first release-policy bug.
9. **Locality ceilings.** Claiming is per-cell and incremental (one cell per walk step),
   so per-activation write sets should not grow. If they do, the audit trips and the
   design is wrong — that is a feature.
10. **It may simply not pay.** If corridors are typically shorter than footprints, claims
    will rarely be big enough and the mechanism is dead weight. This is measurable before
    building anything: histogram cable length at dock time against the footprint the rule
    would need.

### Build order, each step measurable on existing gates

0. **Measure first** (no code in the engine): at every dock, record corridor length
   against the footprint the chosen rule needs. If corridors are systematically too
   short, stop here — the idea is refuted cheaply.
1. **Retain on walk.** Vacated cells stay claimed for the reaction. Success signal: peak
   cells in the cost census stops rising, and declined docks fall in `park-census`.
2. **Fund the dock.** Do not dock until the claim covers the footprint; fall back to
   today's relief when it cannot. Success signal: the declined-dock class shrinks
   sharply; watch the soak floor and the frontier for regressions.
3. **Release policy.** Park-release plus hold timer. This is where livelocks will appear;
   the cooldown ablation lane and the derivational backends are the detectors — the
   backends caught two of this week's three pumps before the default one noticed.
4. **Then the consumers**: zip (shorten toward the anchor so footprints fit smaller
   claims), separate (keep corridors from overlapping), premove (walk early to accumulate
   claim). Each measured separately, none of them load-bearing on its own.

### What would falsify it

Corridor lengths at dock time being systematically below footprint sizes (step 0). Or:
claims held long enough that total progress falls even as declined docks drop — visible
as soak completion falling while the park census improves, which would mean the mechanism
converts one park class into another rather than removing it.

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
  experiment. (Prefetch's useful form is now the premoving consumer of corridor-as-claim
  above — accumulating claim before the dock, rather than scanning for room at it.)
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
  safe version needs a receiver rule that provably worsens no roll. This class is the
  primary target of corridor-as-claim: every one of these pairs committed to reacting and
  only then discovered it had no room, which is the failure mode that funding a dock
  before it commits is meant to remove.
- **1 growth wedge, 2 quiet parks** — tail, not worth aiming at yet.

Note the two classes are the same shape at different scales: something free sits where
matter needs to be, and the rule that keeps relief honest (the displacement order, the
ring drain) is what forbids using it. Both loosenings that have worked so far — the
pays-for-itself exemption, and this — take the same form: permit the otherwise-forbidden
move exactly when it is the last one before progress that the reduction bounds.

## Untangling: measured, and it needs its other half first

*(Consumer of the corridor-as-claim keystone above. Read that first: the reason both
versions below failed is that neither had anywhere to put the cable, which is what
claimed area provides.)*

A cable threading an AGENT's cell is a tangle, and tangles are exactly what make a
walker's path unwalkable — it crosses wire all day and stops dead at an agent. They are
also rare and concentrated: counting parked grids, 2 or 3 agents host a foreign cable
regardless of whether the grid holds 11 agents or 52, and every walk wedge is one of
them. That makes "shed the cable instead of handling the collision" the obvious
prevention, and it has the one thing the other loosenings lacked — a real potential
function, since every eviction and contraction receiver is empty or wire (never an
agent), so no relief move can put a cable back into an agent's cell and the tangle count
falls monotonically, rising only when an agent walks onto a cable or growth places one.

Both versions were built and measured 2026-08-01, and both are reverted:

- **Untangle everywhere** (any agent sheds any foreign cable) is strictly WORSE: the
  cable has to go somewhere, and the somewhere is the scarce clearance around docks,
  which then decline. k-combinator and k-chain fell from complete to 3 rewrites.
- **Untangle only when a demanded walker is blocked on that agent** keeps the deep
  corpus complete (5/5) but revives soak term 95's pump — the SAME pump that the
  walker-side version of this shed produced. Two different trigger sites, one failure,
  which locates the problem in the move rather than in when it fires: the tangle count
  does fall monotonically, but that argument only bounds how often untangling happens,
  not what the freed cable does next, and in tight space a freed hot cable churns
  between relief, contraction and the walk that re-absorbs it.

The conclusion is about sequencing, and it rehabilitates the idea rather than killing
it: **untangling has nowhere to put the cable until cables are shorter.** The companion
half — zipping a cable taut from its agent anchor, so that slack is reclaimed rather
than parked in the fabric — is what creates the clearance untangling needs, and it is
also the half with the more interesting property, since shortening from an anchored end
gives a per-cable sequencing (a cable has one front; two contractions on it cannot
fight) instead of the global spatial direction whose sacrifices have cost the most.
Build zip-up first, measure whether free space around docks actually grows, and only
then re-try untangling on top of it.

**Zip-up's first measurement, and what it costs today.** The cheapest form of zip-up is
just to stop constraining the shortening we already do: contraction currently has to
ascend `relief_g` like any displacement. Removing that gate (2026-08-01) keeps every deep
term complete and makes them all about 10% faster — k-chain 4081 → 3717 generations,
disp-t 4652 → 4226 — which is direct evidence that shortening is what reclaims clearance
and that the direction rule is holding real slack in the fabric. It cannot go yet: soak
term 3 stops quiescing, and the churn is a dock's ring relief pushing a cable out while
free contraction pulls it straight back.

Note what that failure does and does not implicate. Shortening ALONE cannot cycle: cable
length is a decreasing potential, which is exactly the property that makes zip-up
attractive as a discipline. The cycle needs the LENGTHENING half, and the only thing that
lengthens is relief. So the price of freeing contraction is making every relief
pays-for-itself, the way a blocked placement's and a dock's last blocker's already are —
then each lengthening is followed by a fire the reduction bounds, contraction can be
freed in every direction, and the clearance it reclaims is what untangling was missing.
That is one specified change with three payoffs, and it is the most promising thread
left: ~10% on every deep term, the direction rule's remaining sacrifices, and untangling
becoming affordable.

**Second attempt, and it got halfway.** Freeing contraction and giving it the mirror of
relief's ring rule — *a shortening move may not fill clearance a pending fire is waiting
on* (a seed, a facing principal pair, or a cell a blocklet cursor is growing into) — is
the right shape and it works: soak term 3's cycle is gone, the deep corpus stays complete
and stays ~10% faster. A second, different cycle then surfaces at soak term 26, in open
fabric with no dock involved, where relief attempts are all failing yet three cells churn
tens of thousands of times. Two things are worth knowing before the next attempt: the
churn counter counts stamp decay as a mutation, so a cell being repeatedly re-stamped
looks identical to a cell being repeatedly rewritten and the two must be told apart
first; and every cell contraction touches is already stamp-gated, so this is not a
damping gap but a genuine second lengthener that has not been identified yet. Find what
lengthens cable in that region — a walk laying an aux detour is the prime suspect, since
walks are the one motion exempt from every order — and the pays-for-itself framing
applies to it exactly as it did to relief.

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
