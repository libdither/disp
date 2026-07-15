# rust-ca-lattice

The lifted-lattice evaluator for disp's tree-calculus interaction nets. It combines a
projection-checked spatial engine with a strict cellular geometry kernel for selected
motions.

> **Status: hybrid engine, not yet a conforming cellular automaton.**
>
> The conformance target is one frozen, synchronous rule of the form
> `next_site(center, [N,E,S,W,U,D]) -> next(center)`. `crate/src/local.rs::next_site`
> meets that boundary for star translation, swept-square rewiring, pressure bulging, and
> center-emitted contention pressure. Field diffusion is still applied later by the host
> scheduler rather than as part of one unified state update. Fire,
> reel, agent step, flip, slide, retract, grow, and the phased host scheduler
> still use host plans or mutable scans. Passing the differential tests establishes semantic
> soundness; it does not erase those locality departures.

The normative design is
`research/interaction-combinator/LOCAL_CA_DESIGN.md`. This crate is not yet a `Session`
backend.

## Lattice and state

Cells occupy `(x,y,z)` positions and have six faces: N, E, S, W, U, D. `Full3D` permits
unbounded depth; `Bilayer` restricts z to `{0,1}` while running the same strict local rule.
Some host-planned routes and templates remain topology-specific. Agents,
wires, and crossings are represented directly in the three-dimensional cell geometry.

An occupied cell is either:

- a lowered agent with at most three ports, one attachment per face; or
- a wire switchbox with a fixed capacity of disjoint two-face strands.

Connectivity is positional. Runtime ids exist only for projection, diagnostics, and stable
visual coloring; they are excluded from the strict local view. The lowered alphabet keeps
every moving agent at arity at most three: `T1` arms use `Pair`, `T2` becomes `Sel`
over nested pairs, and `Unp·Pair` performs the corresponding fusion.

The crate supports the following bounded bonded state:

- `ψ`/`hot`, demand carried by each strand;
- the per-side tension survey and cooldown;
- `χ`, frustration pressure with local diffusion and leak;
- `σ`, the separate standing shell around consumers; and
- `MotionMark`, a finite relative role/phase for a strict local motion protocol.

`χ` and `σ` deliberately mean different things. `χ` is the sole clearance license, and each
occupant may move or reshape only itself; the shell stages calm approach/compression away from
reaction rooms.

### Canonical bit budget

The fixed seed-free local target is at most **592 bits**. These are packed-state cardinality
ceilings, not `size_of` measurements of the Rust structs:

| Component | Bits |
|---|---:|
| empty payload | 2 |
| arity-1 / arity-2 / arity-3 agent payload | 18 / 20 / 22 |
| one / two / three-strand wire payload | 116 / 223 / 330 |
| fields (`χ`, `σ`, local reservation) | 8 + 8 + 1 = 17 |
| protocol mark | 4–245 |

An agent payload comprises kind 2, tag 4, its ordered distinct face tuple (3, 5, or 7),
nascent 1, and frustration 8. A wire comprises kind 2, a 7-bit canonical matching of up to
three disjoint pairs among six faces, and 107 bits per strand: demand 1, cooldown 8, and two
49-bit survey sides. Protocol widths, including their four-bit role tag, are: none 4,
source/target 25, square 32, endpoint 31, back 29, bulge-source 22, bulge-spine 21,
bulge-corner 22, bulge-endpoint 27, debt 10, and shared-contest 245. Therefore a three-strand
cell with no protocol uses 351 bits, while `330 + 17 + 245 = 592` is the maximum.

The current Rust wire fields retain strand slot, order, and orientation, giving a 636-bit
seed-free fieldwise maximum when those distinctions are preserved. Canonicalizing to the
unordered face matching reaches the 592-bit target. Stable agent ids add 32 observer-only
bits when displayed and are not local dynamics.

Reservations and seeds still mark the finite-state boundary. The reservation map records a
97-bit present-plus-absolute-owner value where the local target has one locally witnessed
flag. `SeedCell` holds an `Rc<GrowPlan>` with absolute positions and variable vectors, so it
has no fixed local bit total. A conforming grow path replaces those host values with a bounded
ROM index and face-relative roles.

## Strict local kernel

`crate/src/local.rs` is the strict locality boundary. Its local decision receives no `Grid`,
position, clock, id, router, mutable neighbor, or distant endpoint. It sees a `Hood` containing
the center plus exactly six `SiteView`s and returns one `SiteNext` for the center.

The sparse runner may enumerate occupied sites and a one-cell halo, but it first gathers one
frozen snapshot, evaluates all sites, and then materializes the returned center edits
simultaneously. It deterministically shuffles that enumeration from a run seed and tick; all
seeds must produce the same next lattice because no evaluation sees an earlier evaluation's
output. This tests scan-order invariance, not asynchronous execution. Sparse enumeration is
therefore an optimization rather than extra dynamic information, and the shuffle seed is host
test metadata rather than a cell input. Event collection and observer ids are materialized
outside the local decision. The dynamic tension survey is still host-updated and remains an
input to host-planned flip.

One evaluator tick has exactly one read and one commit barrier. A payload may move only to a
face-neighbor, and a claim or signal advances only one face. A newly produced or newly arrived
state cannot participate in reel, step, fire, or another move until the following tick. A
multi-cell protocol may require many ticks; this preserves locality and is distinct from
putting several mutable mini-ticks inside one scheduler tick.

True live-read asynchronous activation requires a stronger protocol than shuffled snapshot
evaluation. Exact countdown moments and a synchronous final wave can be missed when centers
activate independently. The delay-insensitive path is persistent, idempotent
`request → acknowledge → commit → done`, with acknowledged abort/retry and a bounded
transit/ghost payload. Projection must map the connected handoff component to one logical
occupant while source and target activate at different times. Only then can a fair shuffled
live-read runner test asynchronous resilience without transient duplication or loss.

### Star translation

A producer at `p` may consume one exclusive principal strand cell at `q=p+d`. If that strand
enters `q` on `-d` and exits on `e`, the producer moves to `q`, points its principal along
`e`, and removes the intermediate forwarder:

```text
before: agent(p) --d--> q[-d,e] --> ...
after:  p            agent(q) --e--> ...
```

This moves the endpoint agent through one intermediate wire forwarder, shortening its
principal embedded edge by one instead of only straightening polymer between fixed endpoints.
It can be licensed by local demand, calm cold compression, or a sufficiently downhill
pressure gradient. The target must contain exactly the moving agent's principal strand, so
the move cannot displace a crossing wire.

For an arity-two producer, the auxiliary bond bends through the vacated source cell. For an
arity-three producer, one aux uses that back bend and the other uses a swept square. With
`u=p+f` and `c=q+f` for the swept aux face `f`:

- **extend:** `c` is empty, so it gains the corner `(-f,-d)` and `u` repoints from `-f`
  (the source cell) to `d` (the new corner);
- **truncate:** `u` already contains the exact corner `(-f,d)`, so that redundant strand is
  removed and `c` repoints from `-d` to `-f` directly toward the moved agent.

Counting all incident edges, an arity-one star lowers `L` by one, the back bend makes an
arity-two star length-neutral, and an arity-three extend/truncate changes `L` by +1/-1.
Thus the primitive always removes the principal forwarder, while auxiliary rerouting may
temporarily spend length that later tension can recover.

In calm space the arity-three source compares its two immediately adjacent aux endpoints.
An endpoint that can neither repoint toward the swept square nor supply the exact truncate
turn is forced onto the back route; the other aux becomes the square route. This is still a
six-neighbor choice—the diagonal square itself is learned only through the target role—and it
prevents a feasible contraction from repeatedly retrying an impossible aux orientation.
Once the source publishes that choice, the relative faces are part of the claim. Later phases
validate only those claimed faces and payloads; evolving `χ`, `σ`, or heat may license a new
offer, but cannot reselect an auxiliary inside an active handshake.

Source, target, square, endpoint, and unchanged back endpoint handshake using only reciprocal
face-neighbor marks. The protocol has a readiness/countdown phase and a second explicit commit
wave. Every role rechecks its adjacent phase on the final tick and writes only its own payload;
an abort or timeout before that wave leaves geometry unchanged. A short local debt mark keeps
a neutral degree-three parent from repeatedly outrunning both children. Incoming square,
endpoint, back, or bulge-endpoint roles may borrow that nonlocking payload, but restore its
debt afterward; the ratchet clears once neither stored adjacent reciprocal attachment remains.
Every handshake role except Debt and contested persistence is locking: host plans cannot enter
its footprint, reservations cannot arrive, and heat cannot change its payload before commit or
abort. The synchronous final wave relies on this enforced substrate invariant.

### Pressure bulge and relay

A straight strand has no movable corner. Under a strong downhill `χ` gradient, its owning
switchbox may replace the straight strand at `p` by a width-one detour on perpendicular side
`s`. For axis `±a` and `q=p+s`:

```text
before: (p-a) -- p -- (p+a)
after:  (p-a) -- (q-a) -- q -- (q+a) -- (p+a)
```

The source removes only its chosen strand (other crossing strands at `p` remain), the spine
and two corners write themselves, and the two original endpoints repoint themselves.
Embedded length rises by two while projection is unchanged. Ordinary strong pressure enables
this mainly in shared switchboxes; a lone strand is rate-limited to saturated pressure
(`χ >= 200`), which a local obstruction relay can produce. The endpoints do not move, so `C`
is unchanged, while `L` and `S` both rise by two. This avoids polymer bloom across a broad
halo.

The source cannot see either diagonal corner. The spine recruits the corners, each corner
recruits its adjacent endpoint, and readiness returns along the same faces. If a prospective
corner or endpoint is obstructed, that corner site locally pumps `χ`. A calm shared principal
switchbox uses a nonlocking finite counter. It records the exact seven-site payload, bounds,
and reservation signature. Detection stores age 0; after 64 completed unchanged updates have
stored age 64, the following local evaluation emits low pressure (`χ=16`). Every strand in
the patch must be cold: hot activity means the crossing is still in demanded transport, not
a static compression barrier. This filters transient transport while
ensuring a stable compression barrier eventually asks its own occupants to decongest. The
requester never reads a diagonal, names the obstruction, or moves it.

## Clearance ownership

`χ` is the sole license for clearing another occupant's space:

- a blocked fire pumps pressure at the reaction room and waits;
- a blocked hot walker pumps at itself/its target and waits;
- a grow reservation keeps its occupied claimed room pressurized until completion or abort;
  each incumbent remains free to depart, but no new occupant may arrive;
- a fully cold shared principal switchbox locally pumps when exclusivity persists; and
- an obstructed bulge corner relays pressure locally.

The occupant later sees the gradient and chooses its own projection-preserving star, bulge,
flip, slide, or agent-step response. Requesters do not select a foreign strand or route for it.
Trace schema v3 records the rising edge of each newly effective local pressure source as
observer-only evidence; the event is never an input to the dynamics.

Calm approach uses the strict local star. Some occupant-owned pressure responses are still
host-planned and are listed in the conformance ledger below.

## What is here

- `crate/src/rules.rs` — the validated 13-tag, 26-interaction template ROM over the lowered
  ≤3-port alphabet. Export it with `cargo run --bin export-rules`; a generated JSON copy lives
  at `research/interaction-combinator/rules.json`.
- `crate/src/oracle.rs` — an independent recursive normalizer.
- `crate/src/net.rs` — the table-driven abstract engine and shadow net.
- `crate/src/lattice.rs` — lifted cell state, topology, bonded fields/signals, loader, and
  executable `check_projection` invariant.
- `crate/src/local.rs` — strict seven-site views, center-only star/swept-square and
  pressure-bulge protocols, center-emitted pressure sources, and the frozen sparse runner.
- `crate/src/transitions.rs` — host-planned footprint transitions and apply functions.
- `crate/src/scheduler.rs` — the current deterministic hybrid schedule and event stream.
- `crate/src/fixtures.rs` — closed pedagogical fixtures, including one isolated row for
  every ROM interaction with exactly one wire cell between the two principal ports.
- `crate/tests/stage1.rs` — abstract engine vs independent normalizer, with rule coverage.
- `crate/tests/stage2.rs` — lattice vs oracle on both topologies, projection checks,
  determinism, liveness floors, and must-complete pins.
- `crate/src/bin/debug-stuck.rs`, `scan-pins.rs`, `probe-grow.rs`, `probe-cap.rs`, and
  `probe-corpus.rs` — liveness and geometry diagnostics.
- `crate/src/bin/dump-run.rs` — JSON traces consumed by
  `research/interaction-combinator/lattice_player.html`; the viewer replays engine state and
  never simulates it. The default `stride=1` records every evaluator tick; a larger explicit
  stride is an opt-in compact export with sampled states. `dump-run rules 120 bilayer 1`
  emits the complete rule-atlas preset with exact before/fire/after navigation metadata.

## Honest conformance ledger

The following active paths still violate the final center-plus-six, center-write-only model:

| Path | Current departure | Local replacement needed |
|---|---|---|
| fire | board inspection, placement/routing, atomic multi-cell plan | local reservation, template write, splice, and release roles |
| reel | aux-chain inspection and bounded-router extension fallback | star-like face protocols for every re-anchor case |
| agent step | planned multi-cell downhill agent re-tie | occupant-owned staged star roles |
| flip | diagonal fourth-corner inspection and atomic writes | corner claim relayed around the square |
| slide | bounded empty-cell trail search | pressure/vacancy relay one face per tick |
| retract | atomic multi-cell U-turn deletion | endpoint/corner acknowledgement protocol |
| grow | host `GrowPlan`, reservations, host-stepped placement/abort | finite per-site template roles |
| scheduler | deterministically permuted host scans and mutable subphases | one frozen `next_site` over all state components |

Some `ψ`, survey, `χ`, `σ`, and cooldown formulas are already neighbor-local. Pressure
diffusion/leak is radius one, but most current source injection (blocked fire, walker, grow,
or reservation) is still host-scheduled. Cold shared-principal and bulge-corner
pressure are emitted by `next_site`, but the χ diffusion update remains a later host phase.
Until these pieces are composed into the same frozen update as
geometry and rewrite state, the engine as a whole remains hybrid. Loader routing, projection,
metrics, and trace rendering may remain global because they are observers/initialization, not
cellular dynamics.

A mixed hot/cold shared switchbox deliberately does not start the cold persistence timer: the
hot strand's owner retains demanded-motion priority. Proving that this always clears the cold
requester, or adding a separate locally rate-limited mixed-contention state, remains part of
the clearance-liveness work.

## Geometry diagnostics and stopping

Normal form and compression are distinct. Traces should report:

- `L`: total embedded strand-cell length;
- `C`: the summed chord lower bound
  `max(Manhattan(endpoint₁, endpoint₂) - 1, 0)` for each abstract edge;
- `S = L - C`: excess embedded length above the chord lower bound (candidate slack).

`L` says how much wire exists; `C` says how much its endpoint separation forces; `S` is the
blue detour that contraction can plausibly remove. Per-edge maximum/quantiles are useful so a
single loose bond is not hidden by a small total.

There are likewise two stopping points:

1. **semantic NF** — the shadow net has no active interaction pairs; and
2. **geometric settling** — a later fixed point, or a documented bounded tail, in which
   payloads, motion marks, relevant fields, and `L/C/S` no longer change.

The trace player should mark semantic NF but continue through the settling tail. Stopping the
frame stream at NF makes an intermediate forwarder look like a permanent compression barrier even
when strict stars or host-planned settling transitions could still reduce slack. A bounded tail must report why
it stopped and the residual `S`/active marks; it is not equivalent to quiescence.

## Correctness and validation

Every completed geometry protocol must project to identity; a completed fire must project to
exactly one shadow interaction. Under `CheckLevel::Every`, the engine checks projection after
each applicable transition. `tests/stage1.rs` compares the abstract ROM engine with the
independent normalizer. `tests/stage2.rs` checks lattice values, projection, determinism, and
liveness on `Full3D` and `Bilayer`.

Useful commands from `crate/`:

```sh
cargo test --release local::tests -- --nocapture
cargo test --release pins_must_complete -- --nocapture
cargo test --release
```

The strict-kernel regression set should cover both swept-square modes on both topologies,
shared-target exclusion, pressure bulge projection identity, abort-before-commit safety, and
the locality property that remote state cannot affect a center decision.

## Path to full locality

1. Enforce the one-hop rule across every host fallback: no state written in a tick can be read
   or moved again in that tick.
2. Convert flip and retract to staged square/corner roles, completing local calm contraction.
3. Replace shortest-route slide and the host-planned agent step with pressure/vacancy relay protocols.
4. Express fire reservation, template extrusion, splice, and abort as finite face messages.
5. Fold bonded signals, fields, geometry, and rewrite state into one frozen `next_site` rule,
   and replace absolute reservation owners and `GrowPlan` with bounded local roles.
6. Make every transfer phase persistent through request/acknowledge/commit/done, extend
   projection over transit states, and then add fair live-read activation tests.
7. Run the full differential and liveness corpus, then compare semantic time and settling time
   with `L/C/S` rather than declaring conformance from final-value correctness alone.
