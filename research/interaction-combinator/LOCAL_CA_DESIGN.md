# Local Cell64 substrate for TC-Net

This document specifies the six-neighbor cellular implementation of the tree-calculus
interaction net.

## 1. Dynamic boundary

The entire dynamic API is:

```rust
fn update_cell(center: &mut CellWord, adjacent: &[CellWord; 6]) -> UpdateEffect
```

`adjacent` is ordered N, E, S, W, U, D and is immutable for the duration of the call. The
function may replace only `center`. It receives no coordinate, global clock, endpoint id,
agent id, route, mutable neighbor, random value, or reference to the grid.

The host repeatedly activates cells in a fair order. The concrete runner forms the finite set
of stored cells plus their one-cell halo, deterministically shuffles that set from an observer
seed and round number, and activates each center once. It then repeats. This is a live-read
schedule: a later activation sees earlier word changes. Correctness therefore cannot depend on
simultaneous updates, exact countdowns, or scan order.

The random seed belongs to the runner, not to cell state. Multiple seeds are used to test that
persistent local protocols tolerate asynchronous interleavings.

## 2. Lattice

Cells occupy the lifted cubic lattice. Every cell has six face-neighbors. `Full3D` admits every
integer z coordinate. `Bilayer` admits z = 0 or 1; an unavailable U/D neighbor is supplied as a
non-state boundary sentinel.

All connectivity is face reciprocal. If a matter value exposes `(face, lane)`, the adjacent
cell must expose `(opposite(face), lane)`. Diagonals are never direct inputs. Information
reaches a diagonal by persistent messages relayed over two or more face activations.

## 3. Constant state

Every real site is one `CellWord(u64)`:

| Field | Width |
|---|---:|
| matter | 20 bits |
| control | 28 bits |
| obstruction pressure `χ` | 8 bits |
| standing shell `σ` | 8 bits |
| total | **64 bits** |

The sparse map and its coordinates are storage machinery, not cell state. Stable agent ids,
semantic events, projection diagnostics, and trace frame numbers are observer sidecars.

### 3.1 Matter field

The matter field begins with a 3-bit kind.

| Matter | Encoded data after kind | Maximum field use |
|---|---|---:|
| Empty | zero | 3 bits |
| Agent | tag 4, face geometry 5, auxiliary flip 1 | 13 bits |
| Link/cable | face pair 4, lane count 1, twist 1, hot mask 2, cooldown 3, pulls 4 | 18 bits |
| Zipper | ordered geometry 7, twist 1, hot mask 2, cooldown 3, pulls 4 | **20 bits** |
| Fixed crossing | two-route geometry 6, hot mask 2, cooldown 3, pulls 4 | 18 bits |

Invalid encodings are rejected when unpacked. The zipper determines the 20-bit width.

### 3.2 Control field

The control field begins with a 3-bit kind and can represent:

- idle;
- line translation;
- cable zip/unzip shift;
- pressure-owned relief;
- rewrite workshop coordination; and
- local contest persistence.

The largest entry is rewrite control:

```text
role 3 + phase 3 + rule 5 + axis 3 + side 2 + slot 6
+ fallback 1 + lift 1 + kind 3 = 27 bits
```

One control bit remains unused. A rewrite address is entirely face-relative. It stores no
origin coordinate or absolute transaction owner.

## 4. Matter geometry

### 4.1 Agents

Port 0 is principal. Arity-one agents have only the principal face. Arity-two agents have a
principal and a single-lane tail. Every arity-three agent has a principal and one two-lane
tail:

```text
                           lane 0 = auxiliary 1
principal ─ [ agent ] ═════════════════════════ tail
                           lane 1 = auxiliary 2
```

`aux_flip` exchanges the semantic meaning of physical lanes 0 and 1 without changing the
geometry. Both auxiliary wires therefore move as one cable behind the agent.

### 4.2 Links and cables

A link owns one unordered pair of distinct faces. A one-lane link is one ordinary wire
forwarder. A two-lane link is one geometric cable route. `twist` exchanges its lanes between
the two ends. Heat and pull state are lane-indexed; cooldown is shared by the local route.

A link never points a face at two destinations and never shares a face with another route.

### 4.3 Zippers

A zipper maps one double trunk to two ordered single branches:

```text
                       branch[0], lane 0
                      /
trunk lanes 0 + 1 ===□
                      \
                       branch[1], lane 0
```

With `twist = false`, trunk lane i maps to branch i. With `twist = true`, it maps to branch
`i xor 1`. Zippers are projection-transparent local matter. They keep auxiliary paths
compressed along shared runs while retaining distinct semantic endpoints.

### 4.4 Fixed crossings

A fixed crossing contains exactly two single-lane face-pairs whose four faces are distinct.
It represents two independent routes. It cannot join them, change either pair in place, or
displace a neighboring route. Depth-separated routing remains preferable when space allows;
the fixed crossing is a bounded local geometry for compact workshops.

## 5. Fields and ownership

`χ` represents pressure created by locally witnessed obstruction. `σ` is a separate standing
shell emitted by consumers. Each activation relaxes a field from the center and its six
neighbors with saturation and leak.

Pressure does not grant one occupant authority to edit another. The ownership rule is:

1. A requester may maintain or raise its own pressure source when its next locally required
   cell is unavailable.
2. The blocking occupant later sees the gradient.
3. That occupant may choose one of its own projection-preserving moves.
4. The requester waits and retries; it never pushes, swaps, or reroutes the blocker.

Active request or placement control temporarily locks the participating cell's matter. A
competing operation observes the control and declines the cell.

## 6. Cell-by-cell translation

A producer may advance only into the adjacent one-lane link on its principal face. If that
link enters from the source and exits through face `e`, the geometric change is:

```text
arity 1
before: source ─ target-link ─ e…
after:  empty    target/source ─ e…

arity 2
before: tail ─ source ─ target-link ─ e…
after:  tail ─ single-link ─ target/source ─ e…

arity 3
before: tail ═ source ─ target-link ─ e…
after:  tail ═ double-link ═ target/source ─ e…
```

Only one face-neighbor is traversed. The source, target, and tail roles coordinate with
persistent phases:

```text
source offer
  ├─ target acknowledges the exact entered/exited link
  └─ tail acknowledges the reciprocal one- or two-lane channel
source commit
  ├─ target writes the moved agent
  └─ tail holds its channel
source writes the vacated link and enters done
done clears after both neighbors have observed it
```

An invalidated offer enters abort and matter remains unchanged. A moved arity-three agent
leaves one double link, not two side-by-side detours. Moving through a zipper is a separate
cable-shift operation because it changes whether the cable is zipped or unzipped.

## 7. Rewrite protocol

Each semantic rule has a finite face-relative workshop ROM. A workshop contains:

- a connected set of slots;
- one rooted request tree over face adjacency;
- the final matter for every slot;
- the fresh-agent index, if any, assigned to a slot; and
- the control role used for rendering and validation.

Every participant can reconstruct the same workshop from `(rule, axis, side, lift)`. Its slot
number identifies its parent face, child faces, final matter, and fresh-agent index.

### 7.1 Direction request and response

The consumer is slot 0 and the driver. It verifies the docked producer through its principal
face, chooses a side relative to that active pair, and requests the preferred lift direction.
An idle cell accepts a request only when:

- the requesting neighbor's slot names this face as a request-tree child;
- the child's expected slot is unique;
- its matter is empty, except for the docked producer boundary slot; and
- its control is idle.

The child stores the same face-relative workshop address and its own slot, then relays the
request to its children. Leaves answer `Ready`; a parent answers `Ready` only after all its
children do.

If an expected child is occupied, outside the topology, or participating in a conflicting
request, the parent answers `Blocked`. The blocked response cancels that request outward.
Each participant clears only after its descendants have cleared, so no request mark is
orphaned. The driver then flips only the lift bit and requests the opposite direction. If that
direction is also blocked, matter remains unchanged and the driver returns idle.

Each local `Request → Blocked` transition also injects a `χ` pulse. This reports obstruction
without granting the requester any right to move or rewrite the refusing cell. Repeated fair
retries can reinforce the field; only the occupant may respond to its gradient.

### 7.2 Placement and completion

When the chosen direction is ready, the driver writes its own final matter, enters `Place`,
and emits exactly one observer `RewriteFire`. This is the semantic linearization point. The
observer fires the same rule in the shadow net and assigns stable ids to the workshop's
fresh-agent seats.

`Place` travels outward one face at a time. A participant writes only its own final matter
when it observes its parent in `Place`, while retaining control. Leaves answer `Placed`; each
parent answers `Placed` after all descendants have completed. The driver clears only after
both of its subtrees report completion, and remaining controls then clear locally. Projection
is checked again only after all controls are idle.

The protocol requires no timeout. Fair activation is sufficient because every nonterminal
phase remains visible until its dependent neighbor responds.

## 8. Apply–Fork workshop

For `A·F → T1 + Pair`, let the Apply cell be the origin, `a` point from Apply to Fork, `s` be
a perpendicular side, `d = opposite(s)`, and `l` be one of the remaining lift directions.

The main seats are:

```text
Apply boundary zipper       0
Fork boundary zipper        a
T1 agent                    a + 2s
T1 tail zipper              2s
Pair agent                  2d
Pair tail zipper            d
```

The two boundary zippers reuse the dying agents' double auxiliary cables. Short local links
connect three external interfaces. `C.aux1—Pair.aux2` is adjacent through the Pair zipper and
needs no link cell.

The internal `T1.args—Pair.principal` path must pass the live Apply cable without sharing its
cell. It follows:

```text
T1 zipper
  → -a
  → d
  → l
  → d
  → d
  → -l
  → Pair-a
  → Pair
```

This is a one-layer overpass, not a back-crossing or a remote route search. Every step is a
face adjacency fixed by the workshop ROM. The complete final patch contains 16 cells and no
fixed crossing. Before placement, all new seats contain only control. During placement, the
two dying agent cells become cable-boundary zippers, so the existing auxiliary cables are
respliced without expanding into separate long wires.

## 9. Projection invariant

Tracing begins at each agent port as `(face, lane)`:

- an agent principal uses lane 0;
- an arity-three tail maps semantic ports 1 and 2 through `aux_flip`;
- a link continues through its paired face and optional twist;
- a zipper maps trunk lanes to ordered branches; and
- a fixed crossing follows only the independent route containing the entered face.

At a control-free checkpoint, tracing every live port must produce exactly the corresponding
shadow-net port. The live agent tags and count must also match. Geometry motion projects to
identity. The driver's transition into `Place` projects to exactly one rule-table interaction.

Observer state may inspect coordinates and ids to perform this check. The local transition
function may not.

## 10. Trace and inspection

Packed trace schema 4 stores:

1. one initial full keyframe;
2. one frame for every center activation;
3. the activated coordinate and whether its word changed;
4. a one-cell delta containing the literal 64-bit word when state changes; and
5. semantic observer events at their linearization points.

Viewer reconstruction is replay-only and cannot affect the run. Selecting a cell displays its
matter geometry, control role and phase, fields, exact word, neighboring reciprocity, and bit
allocation. Two cable lanes, zipper mappings, and fixed crossing routes are drawn as distinct
geometric structures. The bundled Apply–Fork presets show preferred-lift placement, a forced
opposite-lift retry, and both directions blocked with matter unchanged.

## 11. Implementation status

Implemented and projection-tested:

- exact `CellWord` codec;
- sparse packed grid and loader;
- zipper/cable/crossing tracing;
- bonded heat and local `χ`/`σ` relaxation;
- cell-by-cell producer translation;
- the complete local Apply–Fork workshop;
- deterministic random live-read activation tests; and
- delta-compressed activation traces.

Remaining work:

- cable shift at zipper boundaries;
- pressure-owned link/cable relief and contraction;
- face-relative workshops for the other 25 semantic rules;
- conflict stress tests with overlapping workshops; and
- end-to-end normalization through the packed runner.
