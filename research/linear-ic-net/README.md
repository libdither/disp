# Linear tree calculus and linear interaction nets

Design analysis with prototypes, 2026-11-06 to 2026-11-08. Nothing here is in an evaluator. Every
number below was rerun from this folder on 2026-11-08.

Names used in file names:
- **TC**: ordinary tree calculus.
- **LTC**: linear tree calculus, a tree calculus where exactly one rule drops (K) and one copies (W).
- **V5**: the final 9-rule LTC design (earlier drafts are V1–V4, and the 7- and 8-rule variants).

## The threads

1. **`tree-calculus/`: the 9-rule linear tree calculus.** One node, △; a redex is △ a b x, and the shape of a
   plus one more node picks the rule. Each rule eats three △. Triage hands back the arms it didn't pick,
   so nothing is dropped except by K. `final.py` checks everything in one run. See its README for the
   rules, the compiler from λ-terms and a map of the files.
2. **`reversible/`: reversible programs in it.** Programs that drop nothing and hand themselves back.
   The best reversible binary adder runs at 29.3 steps per bit, against 67 for the ordinary LTC add and
   183 for TC. See `RESULTS.md`.
3. **`nets/`: reflective linear nets.** Trees blow up because every place an argument goes has to be
   spelled out by routing nodes (B, C, W) on the path to that use; λx1…xn. k xn…x1 compiles to 1,134
   nodes in LTC at n = 16, and 6,533 in TC. As an interaction net, variables are wires, and it's 50
   agents. The calculus here keeps function bodies frozen until called, which makes a single kind of
   copier correct, and lets programs read any function's code, their own included. See `DESIGN.md`.
4. **`lean/`: theorems that constrain net designs.** The nets' rule table is written as data in Lean, and
   the properties a good design needs are proven by evaluation: well formed, polarized, deterministic,
   covered, code inert, only erasing drops, only copying copies, every value readable, and the copy and
   erase rules forced by polarity. Flawed designs fail them. `Size.lean` proves compiled nets have at most
   2 agents per term node. See `PATHWAY.md` for the catalog, including the open theorems about reduction.

The visualizer for the 9-rule calculus lives in the website (`website/src/lib/evaluators/linear/`, page
`/evaluators/linear/`), not here.

Related: `../LINEAR_TREE_NETS.md` (earlier net work for disp's tree-net), and
`../interaction-combinator/EMBEDDING_THEOREM.md` (the hand-written polarity lemma for the tree-net,
which `lean/` could mechanize).

## Rerunning

Every run is memory-capped. From this folder:

```sh
cap() { systemd-run --user --scope -q -p MemoryMax=3G -p MemorySwapMax=1G timeout 1500 "$@"; }
(cd tree-calculus && cap python final.py)         # static checks, confluence, 3000 random programs, benchmarks
(cd reversible && ./run_all.sh)                   # every reversible measurement; addx6.py alone is the final adder
(cd nets && cap python check.py)                  # 8000 random programs against the reference, 16 reflective ones
(cd nets && cap python blowup.py && cap python costs.py)
(cd lean && PATH=~/.elan/toolchains/leanprover--lean4---v4.34.0/bin:$PATH cap lake build)   # every theorem; no errors means proven
```

Lean is core Lean 4.34 with no Mathlib, as a small Lake project; the toolchain isn't on PATH.

## Open

- Theorems about reduction (`lean/Net.lean`, `lean/Runs.lean`, `lean/Moves.lean`): polarity, the run
  invariants, confluence and unique results are proven for real runs, which fire pairs and remove
  forwarders in any order, and no order runs longer than a finished run. The per-design checks run inside
  Lean, and `lean/Runner.lean` ties them to the theorems: for every program up to size 9, every order of
  steps reads back the reference's answer. Simulation for all programs, as a proof, is open.
- Copies and erasures are 95% of the nets' work in recursive reflective programs. Frozen code never
  changes, so it could be hash-consed, which would make copying it O(1). Not built.
- The reversible discipline from `reversible/` hasn't been tried in the nets.
- The tree-net's own rule table (`evaluators/tree-net/crate/src/rules.rs`) could go through the same Lean checks.
