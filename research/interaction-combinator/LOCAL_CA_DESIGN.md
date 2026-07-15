# Local CA substrate for TC-Net: a concrete build spec

Design doc. Companion to `SPATIAL_IC.md` (the theory note; this is its §13 "E4" expanded
into a buildable specification), `EMBEDDING_THEOREM.md` (the simulation proof this relies
on), `tc-net.typ` (the calculus), and `RUST_IC_NET_DESIGN.md` (the pointer machine whose
§2/§5/§7 decisions transfer).

**Normative dynamics:** a lifted cubic, six-face, radius-one cellular automaton. At every
tick every site evaluates the same finite rule from one frozen seven-site snapshot (itself
plus N, E, S, W, U, D) and returns the next state of itself only. Coordinates, global
clocks, ids, routes, mutable neighbor references, region scans, and chain walks are not
inputs to that rule. Multi-cell geometry is implemented by bounded face-to-face protocols,
not by enlarging the read radius.

**Implementation conformance:** `evaluators/rust-ca-lattice/` is a hybrid evaluator. Its
`local.rs::next_site` implements strict radius-one star translation, swept-square auxiliary
rewiring, pressure bulging, and center-emitted pressure for locally witnessed contention.
The host still applies χ diffusion in a later scheduler phase rather than in that same update.
Several other active transitions are still host-planned and sequential; §12 states that
boundary explicitly.

Wires grow, converge on reaction rooms, and can jam. The dynamics therefore treats
compression, clearance, geometric settling, and semantic reduction as separate measurable
phenomena.


## 1. Invariants (the non-negotiables)

1. **Finite state per cell.** A cell holds a bounded number of bits (§3), independent of net
   size. No ids, no pointers, no per-cell lists that grow.
2. **Literal radius-one locality.** The dynamics has the shape
   `next(center, [N,E,S,W,U,D]) -> next(center)`. Every site reads the same frozen
   pre-tick state, and no site writes a neighbor. Nothing in the rule reads a coordinate,
   clock, region, path, wire identity, or distant endpoint.
3. **Rules stay distance-blind.** Rewrite outcomes never observe geometry (near vs far, race,
   timeout). This preserves strong confluence (`tc-net.typ` Theorem 2), which is what makes
   any schedule sound. Geometry sets rates, never results.
4. **Everything dynamic is cell-resident.** Values, consumers, dispatchers, wire strands,
   signals, and protocol roles are finite cell payload. There is no globally referenced wire
   object or crossing/via object in the lifted target.
5. **The projection invariant is the correctness spec.** At every tick the lattice projects
   (erase forwarders and protocol marks, read agent connectivity) to a well-formed abstract
   TC-Net, and
   every micro-step projects to identity or to exactly one abstract interaction. This single
   sentence is both the theorem's invariant (`EMBEDDING_THEOREM.md` §4) and the simulator's
   test assertion (§10). Reduction quality (does it reach normal form, how fast) is a separate
   dynamics question; connectivity, hence the result, is fixed by this invariant regardless of
   the field, the schedule, or the routing.
6. **Occupants own motion; pressure owns clearance.** A cell that can locally witness
   frustration—the requester or contested candidate—may raise `χ` at itself, but may never
   select, push, slide, or rewrite a foreign wire or agent. The obstructing cell sees the
   resulting pressure gradient and chooses its own locally legal transition. Calm tension may
   shorten a bond; it is not a clearance license.


## 2. Lattice and neighborhood

The authoritative substrate is the **lifted cubic lattice**. Each site has six named faces:
N, E, S, W in the surface plane and U, D in depth. A face is used by at most one attachment
in a cell. Three dimensions make crossings ordinary geometry: independent wires pass at
different z coordinates instead of sharing a special via or hidden layer state.

`Full3D` is unbounded in z. `Bilayer` restricts z to `{0,1}` and represents the harder 2.5D
case; an out-of-bounds neighbor is a finite boundary state supplied to the same local rule.
Topology changes which neighbor states can exist, not the update radius or protocol.

The six neighbors are not a convenient implementation approximation. They are the complete
dynamic information boundary. Diagonal cells such as `p+d+f` are learned through relayed
claims over multiple ticks; no rule reads them directly from `p`.


## 3. Cell state (finite-state target)

A site contains only bounded, locally meaningful state:

- **payload**: empty, one lowered agent with at most three ports, or a wire cell with a
  bounded number of disjoint two-face strands;
- **six-face map**: relative face directions and port roles, never endpoint ids;
- **bond signals**: bounded demand/heat, cooldown, and tension-survey values carried per
  strand;
- **fields**: bounded pressure `χ` and the distinct standing-shell field `σ`;
- **protocol mark**: one role, relative geometry spec, finite phase, and saturating age for
  a star or bulge handshake; and
- **bounded rewrite/reservation state** for the local fire executor.

The exact packing remains an implementation choice, but the cardinality must be fixed before
the net size is known. In particular, Rust observer ids, `Rc<GrowPlan>`, absolute positions,
event logs, sparse-map keys, and whole-wire survey oracles do not cross the local-rule API.
Tie-breaking must be derivable from bounded local state and face order; `hash(x,y,tick)` is
not part of the normative dynamics.


## 4. The agent alphabet (uniformly lowered)

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
- `T1` level-1 dispatch, lowered to the ≤3-port alphabet by carrying arms in `Pair` cells.
- `T2` level-2 dispatch, lowered to `Sel` over nested pairs.
- `δⁿ` need-duplicator, 3 ports (principal, copy-left, copy-right). Demands a suspension once,
  copies only the resulting value.
- `δˢ` structural duplicator, 3 ports. Copies syntax blindly (suspensions included), the
  reflective-materialization operator.
- `ε` eraser, 1 port (principal). The death pulse (§9).
- `N` normalizer, 2 ports (principal, result). Drives full normal form.

### 4.3 Why retain the binary lowering on the six-face lattice

Although six faces could physically hold the five ports of `T2`, the binary lowering keeps
the substrate alphabet uniform. A ≤3-port agent moves with at most two
auxiliary bonds, exactly the finite star handled in §8.2; every cell uses the same small face
map and rewrite executor; and neither a multi-cell logical agent nor an atomic wide move is
needed. `Pair`, `Sel`, and `Unp` make the lowering explicit in the rule ROM rather than hiding
it in geometry.

### 4.4 Wire cells

A wire cell contains up to a fixed capacity of disjoint strands. Each strand pairs two of the
six faces and carries its bounded bond signals. This is the geometric representation of a
chain of `ι` forwarders; it is not an addressable object and has no dynamic wire id. Multiple
strands may share a switchbox only when their face pairs are disjoint. An agent cell contains
only its agent, never a tucked foreign strand.

Crossings use depth: route one bond through U/D and the other through the surface plane, or
place them at different z. Agents and foreign strands never coexist in one cell.


## 5. The tick, as one local function

The normative dynamics is a synchronous map:

```text
for every site p, in parallel:
    out[p] = next_site(snapshot[p], snapshot[p+N], snapshot[p+E],
                       snapshot[p+S], snapshot[p+W],
                       snapshot[p+U], snapshot[p+D])
replace the lattice by out
```

`next_site` returns only `p`'s next payload, signals, fields, and protocol mark. Sparse
enumeration and halo construction are allowed host optimizations only when they materialize
exactly this dense frozen update. Projection, trace collection, color ids, and
normal-form checking are observers and cannot feed back into `next_site`.

Priority is encoded in the finite local rule, not in a host loop. A useful order is:
continue an already claimed protocol; propagate demand/survey/pressure; begin a demanded
owner-initiated star; begin a χ-licensed occupant response; perform calm bond maintenance;
otherwise stay.
Any operation touching more than one payload is a protocol whose intermediate ticks change
control state only and whose final tick consists of mutually verified center writes.

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

The same ownership rule applies during reservation: an occupied template cell declines the
claim. The pair pumps pressure and waits; it does not evict the occupant. Grow may place into
a cell only after that cell has become empty by its occupant's own dynamics. In the current
hybrid grow path, the occupied claimed room remains a host-scheduled pressure source until the
seed completes or aborts; this field grants no direct write and bars new arrivals while an
incumbent remains free to depart.


## 6. Scheduling and commit protocols

The semantic scheduler is the frozen radius-one map above. A block kernel may be a hardware
optimization only when it implements that map exactly.

A multi-site move uses relative roles such as source, target, square, endpoint, or back. A
claim advances one face per tick. Each role checks only reciprocal marks and unchanged local
geometry; conflicts lose by a fixed face-order rule or fail closed. After every participant
has reached ready, a countdown carries acknowledgement back through the same adjacency
graph. A second armed/commit wave is required before payload writes: without it, a remote
square could write on the same tick that the target learned the source had aborted. On the
commit tick every role again sees the expected adjacent phase and writes only itself. Missing,
changed, or timed-out roles clear their marks without touching geometry.

This staged discipline gives atomicity in projection without an atomic memory transaction.
Confluence still makes the choice among independently enabled interactions semantically
irrelevant; locality determines which protocols make progress.


## 7. `χ` is the sole clearance license

Compression and clearance are related geometrically but have different roles:

- **Tension/compression** removes slack from a cell's own bond in calm space. It may translate
  an endpoint along its exclusive principal strand or rearrange a bend while preserving the
  abstract edge. It never names a foreign occupant as a blocker.
- **Pressure `χ`** communicates that space is needed. A requester or contested candidate that
  can witness frustration pumps at itself. A bounded Jacobi step with leak relays that signal
  one face per tick. The gradient licenses an occupant to move or bulge *itself*.

This is an ownership boundary, not merely a scheduling preference. A requester never clears
an occupied cell directly: it cannot know which foreign abstract edge is safe or useful to
move from a radius-one view. It can only raise pressure. Each wire/agent cell then checks its
own connectivity and accepts a locally projection-preserving downhill response.

At a hidden diagonal obstruction, pressure itself is relayed locally. For example, the
prospective corner of a bulge can see the adjacent spine claim and its adjacent endpoint. If
the corner or endpoint is occupied/incompatible, that corner site pumps high `χ`; the source
never reads the diagonal and never selects that occupant. Diffusion carries the request until
the obstruction moves itself or the initiating protocol times out and retries another side.
A calm shared principal switchbox similarly sees the adjacent producer through one of its own
strands. Its nonlocking bounded counter stores the exact seven-site payload, bounds, and
reservation signature. Detection stores age 0; after 64 completed unchanged updates have
stored age 64, the following local evaluation emits low pressure at itself. Every strand in
the patch must be cold: a hot strand denotes active demanded transport, not a static
compression barrier. This local hysteresis ignores transient transport but
eventually activates a stable compression barrier without consulting semantic normal form. A
hot requesting strand instead uses the declared host blocked-walker pressure source in the
current hybrid engine. A cold requester sharing a switchbox with unrelated hot activity waits
for that owner-driven transport; a separately rate-limited local mixed-contention state remains
a clearance-liveness problem rather than being conflated with the cold timer.

A useful analysis potential is:

```text
φ = weighted wire length + crowding/pressure integral
```

but it does not grant cross-occupant writes. Calm tension descends the length term, while
pressure responses make vacancies travel toward frustrated rooms. Erasure and an expandable
boundary are vacancy sources. Whether these local moves are live below a measurable jamming
threshold, or require a bounded impatience/noise mechanism, remains an empirical and theory
question; correctness continues to come from projection preservation.


## 8. Wire dynamics (local re-embedding)

### 8.1 Growth by splice only

Wires lengthen only by two chargeable events (`EMBEDDING_THEOREM.md` §6): a `Var` elimination
fusing two chains, and `δ` copying an existing long wire. No rewrite template creates a long
wire. So geometry degrades only through identifiable events, which is what gives tension a
chance to keep up.

### 8.2 Star translation carries contraction through agents

The central compression primitive is a one-cell **star translation**, not merely deletion of
wire between fixed endpoints. Let producer cell `p` point along face `d` into `q=p+d`. The
target `q` must contain exactly one strand, entering on `-d` and exiting on `e`:

```text
before: agent(p) --d--> q[-d,e] --> ...
after:  p            agent(q) --e--> ...
```

Thus the moving endpoint agent consumes one intermediate wire forwarder and shortens its
principal embedded edge by one. A demanded hot bond, a calm cold bond, or a sufficiently
downhill pressure response may license the same geometry; none may enter a target containing
a foreign strand.

Auxiliary ports make the move a small star rather than a point translation:

- **One auxiliary.** Route it through the vacated source. If its old face is `b`, `p` becomes
  a turn `(d,b)` and the moved agent's aux at `q` points back through `-d`. The far endpoint at
  `p+b` does not move.
- **Two auxiliaries.** One uses that back turn. The other, on face `f`, uses the swept square
  with `u=p+f` and `c=q+f`. In **extend** mode, `c` is empty: write the turn `(-f,-d)` at `c`
  and repoint `u` from `-f` (toward old `p`) to `d` (toward `c`). In **truncate** mode, `u`
  already contains the exact turn `(-f,d)` into `c`: delete that redundant turn and repoint
  `c` from `-d` to `-f`, connecting it directly to the moved agent. Extend preserves the
  auxiliary edge with one extra segment; truncate consumes existing intermediate slack.

Across the whole star, arity one changes `L` by -1, arity two by 0, and arity-three
extend/truncate by +1/-1. The principal edge always loses its forwarder; auxiliary geometry
may pay length now so that later local tension can recover it.

Source, target, square, endpoint, and unchanged back endpoint are separate protocol roles.
The back endpoint is claimed even though its payload does not change, so an overlapping move
cannot invalidate the new turn. A short local debt/ratchet after a neutral degree-three move
prevents the parent from repeatedly outrunning both auxiliary subtrees. Debt is nonlocking:
an incoming square, endpoint, back, or bulge-endpoint role may borrow the payload, but carries
and restores the debt until neither stored adjacent reciprocal attachment remains.
All other handshake roles lock their sites: payload, heat, and reservation membership remain
fixed, and host-planned footprints must reject the site until commit or abort. The final
phase-mark barrier relies on this substrate invariant.

For a calm arity-three move, `p` can classify each adjacent aux endpoint `p+f` without seeing
the diagonal: it is square-capable if it can repoint `-f→d` (extend) or already supplies the
exact `(-f,d)` turn (truncate). An incapable aux must take the back route. If exactly one aux
is capable it takes the square route; a fixed face/port order breaks a two-way tie. The square
cell at `q+f` is still discovered by the target-to-square claim, never read by `p`.
Publishing the source claim latches these relative faces. Every later source phase checks the
claimed payload geometry rather than rerunning the licensing or tie-break policy, so changing
`χ`, `σ`, or demand cannot split a synchronous commit by changing the selected auxiliary.

### 8.3 Pressure bulge and relay

A straight strand has no corner that a one-cell bend flip can move. Under a strong downhill
`χ` gradient, its own cell may create a canonical width-one detour. Let the original strand
at `p` run along `±a`, and choose perpendicular side `s`; let `q=p+s`:

```text
before: (p-a) -- p -- (p+a)

after:  (p-a) -- (q-a) -- q -- (q+a) -- (p+a)
                         offset by s
```

The commit removes the `±a` strand from `p`, writes a straight `±a` spine at `q`, writes
corners at `q-a` and `q+a`, and repoints the two old endpoints from `p` toward those corners.
It changes one strand cell into three, so `L` and `S` rise by two while `C` and abstract
connectivity are unchanged. That elastic cost is intentional: a shared switchbox can give one
strand room to pass; a single strand is rate-limited to saturated pressure (`χ >= 200`), which
a local obstruction relay can produce, preventing every straight cell in a broad halo from
blooming simultaneously.

The source cannot see either diagonal corner. The spine recruits each corner, each corner
recruits its adjacent endpoint, and readiness returns over those same faces. An obstructed
prospective corner locally pumps pressure as described in §7. The bulge never overwrites or
pushes the obstruction.

### 8.4 Retraction and settling

Local bend shifts expose width-one U-turns; retraction removes those turns and is the primitive
that strictly lowers wire length. Star truncation also removes auxiliary slack while moving an
endpoint. These mechanisms must continue after semantic normal form: no active interaction
pair does not imply a geometrically settled embedding. See the readout and stopping rules in
§11.


## 9. Signals: typing and GC as propagation

Typing and collection use two substrate signals. Each advances one cell per tick along the
forwarder chain and rides the wire's own faces, with no dedicated signal face:

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

The simulator co-maintains the abstract net. Under `CheckLevel::Every`, it checks the
projection invariant (§1 item 5) after each applicable transition: geometry projects to
identity and a fire to exactly one abstract interaction. Stage tests compare normal forms
against the crate's independent recursive oracle. The proof obligation and the harness assert
the same statement, which is why the invariant is worth stating precisely.
The strict local kernel is deterministic from the frozen neighborhood. Host schedule
variations are useful adversarial tests for the hybrid evaluator, but are not inputs to the
local rule.


## 11. Readouts

Export the ledger's cost vector: interactions (work), transport (the `ι`-elimination count,
`SPATIAL_IC.md` §2.2 defines the transport grade as exactly this), peak area (space), makespan
in ticks (span), plus per-region demand, pressure, protocol marks, and fill fraction.

Geometry needs three explicit observer-only numbers:

- `L`: total embedded wire length, counted as strand cells (a shared switchbox contributes
  once per strand);
- `C`: chord lower bound for those same abstract edges, computed per edge as
  `max(Manhattan(endpoint₁, endpoint₂) - 1, 0)` and summed; and
- `S = L - C`: excess embedded length above the chord lower bound (candidate slack).

`L` alone is ambiguous: a long wire may be forced by distant endpoints. `S` isolates the blue
detour that should contract. Track the distribution as well as the total so one very slack edge
cannot hide behind many taut ones. Agent-cloud diameter and maximum per-edge slack are useful
secondary readouts.

There are two stopping times. **Semantic NF** is the first tick at which the shadow net has no
active pairs. **Geometric settling** is a later fixed point (or an explicitly bounded tail) in
which no payload, motion mark, relevant field, or `L/C/S` value changes. Trace generators and
the lattice viewer should mark the two phases and continue through the settling tail; otherwise
an intermediate forwarder can make a computation semantically done while blue bonds still visibly
contain slack. Production evaluation may stop at semantic NF, but it must not report that frame
as geometrically quiescent.


## 12. Implementation conformance and scope

### 12.1 Current conformance boundary

The Rust engine is a **hybrid evaluator**. Its strict kernel has the literal API
`next_site(center, six_neighbors) -> center`. Every participating site is evaluated from one
frozen snapshot, and all center edits are materialized together. The kernel owns:

- producer star translation for arities one through three;
- back-turn and swept-square extend/truncate auxiliary geometry;
- χ-licensed, strand-owned straight-strand bulging; and
- bounded role marks, expiry, reciprocal readiness, and the second commit wave.

The host may enumerate sparse active cells and collect observer events, but those operations do
not supply information to the local decision. The locality property is testable directly:
changing state outside the center's seven-site snapshot cannot change its `SiteNext`.
Cold shared-principal and bulge-corner contention also use a seven-site center-emitting
pressure output from `next_site`. Hot blocked-walker pressure and the later χ diffusion phase
remain host-run.

These active paths use host planning and therefore sit outside strict CA conformance:

- **fire** searches a board, routes fresh wiring, and atomically applies a multi-cell plan;
- **reel** may inspect an auxiliary chain and use a bounded router for extension fallback;
- **agent step** plans and atomically re-ties a χ-licensed, agent-owned downhill footprint;
- **flip** inspects and writes the fourth corner of a tension or `χ` square move;
- **slide** searches a bounded empty-cell trail for a χ-licensed, strand-owned move;
- **retract** detects and removes a U-turn as one multi-cell plan;
- **grow** uses host `GrowPlan` state, reservations, and host-stepped placement or abort; and
- **the scheduler** performs ordered whole-grid scans, fire rescans, and retract-to-fixpoint,
  making one host `tick` a sequence of mutable substeps.

Some bonded field and signal formulas are radius one but execute in host-ordered phases.
Pressure diffusion and leak are radius one. Source injection for blocked fire, walker, grow,
and reservation paths is host-scheduled. Shared-principal and bulge-corner
contention emit pressure from `next_site`; χ diffusion is still a later host phase. Blocked
requesters pump pressure and wait, while foreign occupants retain exclusive
authority over their own geometry.

Loaders, sparse storage, projection, diagnostic oracles, metrics, and rendering may be global
because they observe or initialize the dynamics without feeding values into `next_site`.

### 12.2 Conformance work plan

1. Express fire, reel, agent step, flip, slide, retract, and grow as bounded face-message protocols
   with reciprocal claims, abort propagation, and a second commit wave.
2. Evaluate fields, bonded signals, protocol advancement, and payload decisions from one frozen
   seven-site snapshot with a single explicit local priority rule.
3. Pack payloads, signals, fields, reservations, and protocol roles into a fixed finite-state
   encoding independent of net size.
4. Run the template ROM through the local reserve/write/splice executor described in §5.1.
5. Gate conformance on locality, per-tick projection, differential normal-form, protocol-abort,
   and post-normal-form settling tests in both `Bilayer` and `Full3D`.

Scope for v1 is a single-threaded simulator, one root pad at a fixed boundary cell, and
10^5–10^6 cells. Superposition labels, resident machines, GALS clocking, and real I/O are
outside v1.


## 13. Open problems

- **Protocol proof:** enumerate claim, abort, retry, and commit states and prove that every
  prefix projects to identity, a complete commit projects to identity or one interaction,
  and no participant can write after another has observed abort.
- **Clearance liveness:** determine whether occupant-owned pressure responses deliver a vacancy
  to every satisfiable room, especially in `Bilayer`.
- **Compression potential:** show that the calm star/flip/retract system drives `S=L-C` toward
  zero, or characterize its local minima, including intermediate-forwarder chains as well as
  polymer interiors.
- **Jamming threshold:** measure the fill-fraction transition and the effect of bounded
  impatience or noise while keeping every decision radius one.
- **Settling criterion:** choose the field tolerance and bounded tail used after semantic NF,
  and report unfinished protocol marks or residual slack rather than silently truncating them.
