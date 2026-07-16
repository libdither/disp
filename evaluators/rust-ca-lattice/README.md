# rust-ca-lattice

The six-neighbor cellular substrate for disp's tree-calculus interaction net.

The dynamic boundary is one mutable 64-bit center word plus six immutable neighbor words:

```rust
pub fn update_cell(center: &mut CellWord, adjacent: &[CellWord; 6]) -> UpdateEffect
```

An activation cannot read coordinates, stable ids, endpoint ids, routes, a clock, or any cell
beyond those six neighbors. It can replace only `center`. The runner repeatedly activates a
fair deterministic random permutation of the sparse lattice and its one-cell halo. Later
activations observe earlier writes, so protocols must be delay-insensitive rather than rely on
a global barrier.

## Cell word

Every site is exactly one `u64`:

| Field | Bits | Purpose |
|---|---:|---|
| matter | 20 | empty, agent, link/cable, zipper, or fixed crossing |
| control | 28 | translation, cable shift, pressure response, rewrite, or contest |
| `χ` | 8 | obstruction pressure |
| `σ` | 8 | standing consumer shell |
| total | **64** | constant for every site and every phase |

Stable display ids and semantic events are observer sidecars. They are updated from
`UpdateEffect` after a cell activation and never enter the local rule.

`cell64.rs` defines the field ADTs, validates their geometry, and packs/unpacks the word. Its
tests exhaust every agent orientation, all 15 link face-pairs, all 120 zipper orientations,
all 45 fixed crossing geometries, and every rewrite address.

## Matter geometry

An arity-three agent has one principal face and one tail face. Both auxiliary ports occupy
ordered lanes of the tail cable:

```text
                 aux 1 ─ lane 0 ─┐
agent tail ═══════════════════════╪═ two-lane cable
                 aux 2 ─ lane 1 ─┘
```

The matter variants are:

- `Agent`: tag, principal face, optional tail face, and auxiliary lane order;
- `Link`: one exclusive face-pair with either one or two lanes;
- `Zip`: one two-lane trunk mapped to two ordered single-lane branches;
- `Cross`: exactly two independent single-lane routes using four distinct faces; and
- `Empty`.

A zipper is a real local cell, not metadata. A cable stays doubled while an agent moves along
it and opens only where its two semantic wires need separate destinations. A crossing never
joins, repoints, or displaces either route. No requester moves another occupant; unavailable
space is represented by failed claims and pressure.

## Translation

A producer advances through one adjacent single-lane principal link. Translation uses three
persistent roles—source, target, and tail—with
`offer → acknowledge → commit → done` coordination. For an arity-three agent:

```text
before:  zipper ══ source ─ target-link ─ …
after:   zipper ══ double-link ══ source ─ …
```

The target becomes the agent. The vacated source becomes one two-lane link, so both auxiliary
wires remain overlapped behind it. There is no diagonal swept cell and the agent moves by
exactly one face-neighbor. The observer id moves only when the source commits.

## Rewrite workshop

`rewrite64.rs` is a face-relative workshop ROM. The implemented `A·F` entry occupies 16 cells
in a connected claim tree and creates `T1` and `Pair`. It uses the free side of the docked
pair and one bilayer lift for the internal `T1.args—Pair.principal` connection:

```text
                    T1
                     │
        lifted link ─zip
             │
    cable ══ [A]─[F] ══ cable       before
              │   │
             zip──┘
              │
             Pair
```

The exact shape rotates with `(axis, side, lift)`. A local cell stores only the rule id,
orientation, claim-tree slot, phase, and one epoch bit. It does not store the workshop origin
or an absolute owner.

The protocol is a distributed two-phase commit:

1. The driver offers to its adjacent claim-tree children.
2. Empty children claim themselves and relay offers by one face.
3. Acknowledgements return from leaves to the driver.
4. Any occupied or conflicting child propagates abort; matter is still unchanged.
5. Once all acknowledgements arrive, the driver commits the semantic interaction.
6. Commit propagates outward; each participant writes only its own final matter.
7. Done propagates outward and controls clear inward.

Random activation-order tests use four adversarial seeds. At control-free checkpoints the
lattice is traced port-by-port and checked against the shadow net. The static workshop
compiler is also checked independently by materializing the same 16 final matter words and
verifying the projection.

## Source layout

- `crate/src/cell64.rs` — exact word ADTs and codec.
- `crate/src/substrate.rs` — sparse `Grid64`, zipper-aware tracing, projection, and loader.
- `crate/src/packed_local.rs` — center-only update rule and fair live-read runner.
- `crate/src/rewrite64.rs` — face-relative workshop ROM and local rewrite geometry.
- `crate/src/rules.rs` — validated 26-rule semantic ROM.
- `crate/src/net.rs` — abstract shadow net.
- `crate/src/oracle.rs` — independent recursive normalizer.
- `crate/src/bin/dump-packed.rs` — activation-by-activation packed trace generator.

The broader semantic and geometry regression modules remain available while Cell64 workshop
coverage is extended across the ROM.

## Trace format and viewer

Schema 4 records one complete keyframe and then one-cell deltas. Each changed cell carries its
literal 16-hex-digit word. Unchanged activations are retained as frames without duplicating
the grid. The bundled 1,501-frame `A·F` trace is about 340 KB rather than repeating a full
snapshot 1,500 times.

`research/interaction-combinator/lattice_player.html` decodes the deltas for replay. It draws
two cable lanes separately, marks zippers as square split/join cells, shows fixed crossings as
two independent routes, and displays active control roles as cell outlines. Selecting a cell
shows its matter, control, fields, exact word, face reciprocity, and constant bit budget.

Regenerate the bundled trace from `crate/`:

```sh
cargo run --release --bin dump-packed -- \
  bilayer 0 10000 ../../../research/interaction-combinator/lattice_packed_trace.js
```

## Verification

From `crate/`:

```sh
cargo test --release --lib
cargo test --release rewrite64 --lib
cargo test --release packed_local --lib
```

Current execution coverage includes packed loading/projection, live-read cell-by-cell
translation, bonded heat/field relaxation, and the complete local `A·F` rewrite. Cable-shift
and pressure-relief controls are packed but their transition tables are pending. The remaining
25 semantic rules need face-relative workshops before Cell64 is a complete evaluator backend.
