# interaction-combinator

Notes and tooling for the interaction-net line of disp: the cascade cellular-automaton
reducer (a 3D lattice of 64-bit cells; agents and wires) and its trace player.

## The one gate

```sh
./check.sh
```

Crate tests, the model-cost census against `census-baseline.tsv`, and the bundle
regen+validate, in that order. The census measures substrate costs (rewrites, transport,
generations, peak live cells) on the corpus of pins plus size-knobbed term families
(`k-chain`, `discard-tree`, `convoy`, `share-tower` in `oracle.rs`), Fifo only, and fails
on a completion regression; cost drift prints but never gates. Wall-clock is deliberately
absent: until the message driver exists it would measure the simulator, not the design.
After an intentional improvement, re-pin with
`cargo run --release --bin census-cascade -- --write-baseline`.

The lattice-tier soak (`tests/soak.rs`, 160 seeded random terms against the oracle,
rotating disciplines and embeddings) runs as part of `cargo test`; the abstract-net tier
has its own 4000-term differential in `tests/stage1.rs`.

## Keeping the player bundle fresh

`lattice_player.html` replays `lattice_cascade.js` (schema v4), which is generated, not
hand-written. After ANY change to the engine —
`evaluators/rust-ca-lattice/crate/src/{blocklet,cascade,cascade_run,cascade_par,rules}.rs` —
run:

```sh
./regen.sh
```

It rebuilds the bundle via `dump-cascade` and validates it with node (syntax check,
scenario count, per-scenario smoke table). Failing loudly is the point: do not open the
player on a stale bundle and debug "engine" behavior that is really an old trace.

## Visualizing an ad-hoc run

`debug-cascade` (same crate) runs any preset or oracle term to quiescence and can record
the run as a schema-4 trace:

```sh
cd evaluators/rust-ca-lattice/crate
cargo run --release --bin debug-cascade -- "@(F(L,L),L)" --trace /tmp/t.js
```

Then open `lattice_player.html`, click the load button (⇧) in the top bar, and pick the
file. A parked/wedged run still writes a trace — the wedge is exactly what one wants to
look at. Presets: `identity`, `fork`, `k`, `s-rule`, `k-chain`, `disp-t`.

## Player controls

Shift+←/→ (or `[` / `]`) cycles scenarios; space plays, ←/→ step one generation, hover a
cell to inspect its 64-bit word, click to pin.
