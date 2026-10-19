# interaction-combinator

Notes on the interaction-net line of disp:
- `tc-net.typ` (and its PDF): disp's tree calculus as an interaction net, as a paper. The net itself
  is `evaluators/tree-net`.
- `SPATIAL_IC.md` and `EMBEDDING_THEOREM.md`: putting the net on a grid.
- `RUST_IC_NET_DESIGN.md`: the design behind `evaluators/rust-ic-net`.
- `reversible-ca.html`: a 1D toy of reversible erasure, every block update a permutation and
  annihilation leaving its bits as heat codewords, so running back is exact.

The cascade cellular-automaton reducer these notes led to (`rust-ca-lattice`, its design
`CASCADE_CELL_DESIGN.md`, its trace player and tooling) and the message-passing mesh (`rust-ic-mesh`)
are archived, in git at 81489e5. Their successor is `evaluators/rust-ic-strands`.
