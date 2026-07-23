# AC_IDEA: the amorphous-chip direction for the cascade reducer

Where the cascade substrate (the 3D lattice of 64-bit cells running interaction-net
reduction) must end up to be siliconizable, and the path from today's shared-memory
drivers to a chip of identical message-passing cells. Distilled from the 2026-07 design
sessions; the physics below is validated by the current suite, the architecture is not
yet built. Delete this file once the chip exists and the code reads better than it does.

## The premise that makes it possible

Schedule-independence is already proven: any local execution order may park but never
answers wrongly (schedule_fuzz). That is the license for races on a chip — first-come
state changes are safe, arbitration order never affects the answer. Guard this property
above everything; every mechanism below assumes it.

## Constraints (the frozen contract)

- **Words are 64 bits, layout frozen.** kind 2, payload 36, cursor 21 (roll 3, pc 8),
  chi 4, claim 1. New mechanisms must fit the frozen word or explicitly renegotiate it —
  the word is the chip.
- **Transactions touch ≤ 2 adjacent cells.** Every dynamic is already a 1–2 cell commit.
- **No combinational long reads.** Anything wider (the 5-hop demand lookahead, the ~60
  cell dock-ring scan) must be re-expressed as a bounded wave protocol: a clocked token
  with a hard hop bound, not a one-step reach. Enumerate every exception with its bound.
- **Frozen op alphabet and transition templates.** No runtime search on chip; growth runs
  a compiled microcode script (26 rules × ≤255 ops × ~12 bits) — the cursor is a program
  counter, the layout table is ROM. Compile-time search stays host-side.
- **Small arithmetic only.** Compares, increments, saturating adds on ≤ 8-bit fields.
  No multipliers, no floating point. True today.
- **Bounded fan-in/fan-out.** Each cell sees its own state plus the presented lines of
  its 6 neighbors; every op forwards ≤ ~7 activations. No floods, no broadcast.
- **Idle means dark.** Event-driven only; at equilibrium, zero switching. Signals are
  monotone where possible (heat is one flip per cable per lifetime). Global maintenance
  waves (the current every-5-generations contraction sweep) are a violation — scope them
  or drop them before silicon.
- **One outstanding transaction per cell, released by protocol.** A grant ends with a
  `done` in the same transaction or a bounded hold timer. No permanent locks.
- **Deadlock freedom by construction.** Address-ordered claiming (the parallel driver
  already does this) or bounded hold-and-retry; pick one as the arbiter's contract.
- **Bounded array.** Out-of-bounds is a permanent boundary cell (the parallel driver
  already treats it so).
- **Host machinery stays off-chip.** Shadow net, projection checks, traces, loaders,
  BFS compilers, verification (finals_correct) — never in the silicon datapath.
- **Metastability budget.** Async faces need 2-flop synchronizers before the arbiter;
  account the latency in protocol timing. Per-tile-synchronous vs fully-async is an
  early decision that changes the arbiter, not the API.

## The cell

- 64-bit state + cursor latch; anonymous (no ids — sids are host-only).
- A 6-input mutex arbiter: first request wins, the cell direction-locks ("listens only
  to the granted face") until `done`; losers get busy. This is wormhole
  circuit-switching — proven NoC idiom, 30 years of literature.
- **Fast path (wire cells are fabric):** a route crossbar (3 × 8-bit entries). A heat
  activation entering face/lane exits per the stored route, combinationally, no FSM,
  and sets the route's hot latch on the way. Cables are wormhole circuits.
- **Slow path (agent/seed/cursor cells):** an FSM of transition micro-ops (dock step,
  walk step, place, hop, retract). On grant: run the op, write state, forward the next
  activation or send `done`.
- Idle = clock-gated: no toggling edges, zero dynamic power.

## The edge API (per face, per direction; ~16 wires)

- `req` + `op[3:0]` + `lane`: activate with this op on this lane. Op enum is frozen
  (heat, probe, commit, place, hop, retract, wake — or similar).
- `ack` + `status[1:0]`: granted / busy (backpressure: hold the token) / refuse.
- `data[7:0]`: payload train, sized by the largest atom (one route entry); agent state
  (~15 bits) moves as a 2–3 flit train.
- `done`: transaction tail, releases the direction lock.

## Protocol mapping (current dynamics → messages)

- Heat wave: activation through the crossbar fast path. Trivial.
- Walk: probe principal channel, accept, agent state across as a flit train, trail left
  behind. A 4–6 flit wormhole.
- Dock/growth: the cursor is a roaming packet (rule, pc); each place is a reserve→write
  two-flit transaction with the target cell (already the current two-phase shape).
- Roll scan: the hard one. A scout token circulating the footprint ring collecting merge
  votes (bounded 64 hops) — or better, **host-assist**: roll diversity is a layout-time
  concern; silicon only needs attempt / decline-and-wait. Preferred.
- Relief/eviction: recursion becomes token chains with a hop bound (v2 protocol).
- Verification: host-side, unchanged.

## Migration strategy

1. **cascade_msg**: a third driver with the same semantics but no shared-memory reads —
   transactions communicate only via the message enum. Verified bit-exact against the
   serial runner on the existing suite. This is the sufficiency test for the API before
   any Verilog. The parallel driver is already the halfway point (claims ≈ locks,
   transactions ≈ protocols, atomic words ≈ shared state).
2. **Cycle-level simulator** of the message version: pins protocol timing, synchronizer
   latency, arbiter behavior.
3. **Per-tile ROM decision**: replicated-per-cell script ROM (~20 KB) is too fat; an
   8×8-cell tile sharing one ROM + a slow read bus fits (growth is already 2–3
   generations per placed cell).
4. Then Verilog.

## What is left (open rungs, in rough order)

- **Lane remapping at resolve**: shift a handover path's lane assignment (exit route →
  handover → corridor) to dodge an aux cable's immovable first segment. This is the
  k-chain knot's live blocker: a circular stub-lock where the pair's own aux cables box
  every orientation (2026-07 autopsy: every roll collides with a stub cell, and the
  planned handover's fixed lanes collide with the aux cable's lanes). Alternative rung:
  undock-and-reseat (break the pair, move one agent, re-dock) — bigger, livelock risk.
- **disp-t knot**: walker convoys; less characterized than k-chain's.
- **Sweep scope-or-drop** for the idle-means-dark contract.
- **Roll-scan**: host-assist (preferred) or scout-token wave protocol.
- **Relief as token chains**, with the depth bound as a synthesis constant.
- **Word freeze**: no more bit spending without an explicit renegotiation.
- **Chip-power audit** of every remaining multi-cell signal before tapeout.
