# Tile-aware loading (SPATIAL_IC.md 12.1) — notes

Task: distribute the initial term across tiles at construction time so the layout starts
partitioned, instead of loading into one contiguous stripe and letting the drain cut it.

## Diagnosis (mechanism)

- `build_wide` / `load_fold` / `codec::parse` all run on a plain `Worker::new()` (untiled).
  Nodes bump from `net.node_top` starting at 0, contiguously into `[0, node_top)`.
- `TileShared::new` partitions `[0, cap)` into `tiles` stripes AFTER the load. stripe =
  cap.div_ceil(tiles). For wide-10-200, node_top << stripe (cap=2^21, stripe at 8 tiles =
  262144; the term is far smaller), so the WHOLE initial term lands in tile 0's stripe.
- Result: every initial node has tile 0. Births route to consumer tiles (good for folds),
  but for wide the 2^depth independent chains all sit in tile 0, so splitting them across
  tiles at drain time cuts adjacency; cross_frac grows with tile count.

## Plan

1. Baseline: -tiled -threads 8 -wide 10 200 -nodes <enough>; record cross_frac + births.
2. Tile-aware load: create the tiling BEFORE load, allocate initial nodes across T tiles.
   (a) round-robin by node.
   (b) subtree-aware: whole subtrees per tile (build_wide: the 2^depth chains; parse:
       depth-threshold cut).
3. Measure wide at 2/4/8; confirm fib-14 does not regress.
4. cargo test --release green (tiled_matches_sequential + race detector).

Status: WIP.
