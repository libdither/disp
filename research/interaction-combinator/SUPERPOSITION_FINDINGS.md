# Superposition in rust-ic-net — empirical findings

**Status:** research prototype, 2026-06-28. Validates DISP_BACKPROP §4 (the `sup_λ` agent) +
its Conjectures on the *real* rust-ic-net substrate. Code is uncommitted in worktree
`agent-a89cb2373b162fca6`: the sup machinery in `evaluators/rust-ic-net/crate/src/{port,reduce,net}.rs`
+ the test suite `sup_tests.rs` (13 tests, all green via `cargo test --release sup_tests`).
Read [`DISP_BACKPROP.typ`](DISP_BACKPROP.typ) §4 first; this is what testing it found.

## TL;DR

- **Superposition works without special cells.** Conjecture 1 (collapse soundness) holds
  unconditionally. The **diagonal** — the per-candidate result synthesis wants — is recovered
  *soundly at readback* (correlated collapse), with **no net-level cells**, for the common case.
- **Conjecture 2 (type-check distributes through superposition) holds only for AFFINE
  recognizers.** A recognizer that uses the candidate more than once **cross-products** instead
  of distributing — the δ that duplicates the candidate *commutes* with the sup. This is a real
  soundness restriction, and real disp recognizers (relational / project-twice) are non-affine.
- **For side-by-side uses the cross-product is a READBACK cost, not a reduction cost** (the NF
  stays linear in the number of uses; the host enumeration is `2^k`). Correlated collapse
  selects the diagonal from it — sound, cheap, keeps **both** worlds.
- A var-style during-reduction **choice cell** is an *optional* optimization for the case where
  the cross-product is *reduction*-materialized (chained uses). It commits to **one world per
  pass**, is deterministic/confluent, and is **not needed for soundness**.

## The mechanism

`SUP` (tag 13) is a 2-aux producer `Σ(a,b)` — value-like, faces consumers at its principal.
The label is carried by the *agent kind* (the prototype has one synthesis label: `SUP` pairs
with the matched duplicator `DSUP`; the structural `DS`/`DN` are a different label).

| interaction | rewrite |
|---|---|
| `A`/`T1`/`T2` ⊗ `SUP(a,b)` | **distribute** — δˢ-dup the consumer's stored aux, run one consumer per branch, collect results under a fresh `SUP`. (`T2`'s three aux are the fiddly crux.) |
| `ε` ⊗ `SUP(a,b)` | erase both branches |
| `N` ⊗ `SUP(a,b)` | normalize both branches, **rebuild** the `SUP` — it survives into NF so `collapse` can read the alternatives |
| `DSUP` ⊗ `SUP(a,b)` (same label) | **annihilate** — `out1 ← a`, `out2 ← b` (positional routing, never mixing) |
| `DS`/`DN` ⊗ `SUP(a,b)` (different label) | **commute** — 4-way copy → independent → cross-product |

`collapse` (a host readback, *not* a net agent) reduces to full NF then walks it: constructors
push through SUPs (cartesian product on forks), SUPs union their branches.

## Findings

### Conjecture 1 (collapse soundness) — HOLDS
`collapse(reduce(sup(a,b))) == {nf(a), nf(b)}` for plain, nested (flattened), and
under-constructor sups. (`conj1_*` tests.)

### Conjecture 2 (type-check distributivity) — AFFINE ONLY
- **Affine** `T` (uses its arg once, e.g. `not`): `T(sup(v1,v2))` collapses to exactly
  `{T v1, T v2}` — distributes cleanly (`conj2_affine_not_distributes`,
  `conj2_apply_superposed_function`).
- **Non-affine** `T` (uses its arg ≥2×): **cross-products**, not the diagonal. With `op = F(S L,
  L)` (computes `op c = F(c, S c)`), `op(sup(v1,v2))` collapses to **4** elements including the
  *mixed* `F(v1, S v2)` / `F(v2, S v1)` — combinations corresponding to no single candidate
  (`conj2_nonaffine_via_real_s_rule`). Mechanism: every non-linear tree-calculus function
  duplicates its argument via the S-rule's δⁿ; `δⁿ ⊗ SUP` commutes → the two uses range
  *independently* over the branches. **Neither rule gives the diagonal**: commute → cross
  product, annihilate → positional unzip (one mixed term).

### Diagonal recovery WITHOUT cells — correlated collapse (Arm A)
One label ⇒ exactly the consistent global assignments (all-branch-a, all-branch-b). Walk the NF
once per assignment, taking the **same** branch at every SUP. `op(sup)` → the 2-element diagonal
`{op v1, op v2}`, a subset of the 4-element cross product, with **zero reduction change**
(`arm_a_correlated_collapse_recovers_diagonal`). **This is the cheap, sound, both-worlds fix for
the common case, and it needs no net cells.**

### The choice cell (Arm B) — a reduction-time optimization, not a soundness fix
`Net.choice` (a write-once `AtomicU32`: 0 unset / 1 branch-a / 2 branch-b) + `Net.correlate`.
Under `correlate`, `δⁿ ⊗ SUP` consults the cell via `compare_exchange`: the first use writes the
branch, every later use *follows* it ⇒ all uses agree ⇒ **one world**, and the unchosen branch
is erased before it is reduced. Properties measured:

```
 one sup used k× side-by-side    baseline (commute)        choice cell
 k=2    sups=2 readback=  4 ints=18      sups=0 readback=1 ints=10
 k=4    sups=4 readback= 16 ints=40      sups=0 readback=1 ints=16
 k=8    sups=8 readback=256 ints=84      sups=0 readback=1 ints=28
```

- Baseline reduction is **linear** in k (the blow-up is in readback); the choice cell prunes the
  SUPs from the NF (`k → 0`), trivializes readback (`→ 1`), and cuts a ~3× constant in
  interactions.
- It is **deterministic / confluent**: the fixed default + `compare_exchange` make every racer
  converge to the same branch, so NF *and* interaction count are schedule-independent. The cost
  is **losing a world** — the full diagonal needs a *seeded pass per candidate value*
  (`arm_b_both_worlds_need_two_seeded_passes`). A *data-driven* default would be non-confluent.
- **It earns its complexity only when the cross-product is REDUCTION-materialized** (superposed
  observations chain, each selected branch feeding further duplication). For side-by-side uses,
  Arm A already suffices and keeps both worlds — the cell is unnecessary there.

## The unifying frame: var / choice / memo cells

The substitution cell (`Net.vars`), the choice cell, and a content-memo cell are **the same
object — a shared write-once rendezvous — differing only in their key and read-cardinality**:

| cell | key | reads | gives |
|---|---|---|---|
| **var** (the linker) | wire identity | exactly 2 (linear) | substitution / connection |
| **choice** | synthetic *label* | many | branch correlation (the diagonal) |
| **memo** | content *fingerprint* | many | cross-occurrence sharing (≈ hash-consing) |

So a content-keyed cell *is* an approximate memo (this is exactly the bloom-fingerprint memo of
`RUST_IC_NET_DESIGN.md` §Fast-paths). But note the two **different kinds of sharing**:

- **Search-structure sharing** (sup + label cells): provenance-**safe** — distinct candidates
  stay distinct; the common structure of a search is reduced once. *This is what ic-net is for.*
- **Content-merge memo** (hash-consing / content cells): provenance-**destroying** — merges
  equal-by-evaluation terms, the blame attribution the reverse-mode optimizer needs. *This is
  what ic-net deliberately dropped*, and a cell can only reintroduce it with the non-linearity
  (refcounting GC) and provenance loss that motivated dropping it. Sound only for pure-reduction
  / benchmark mode, off by default for the optimizer.

So: the cells already give the *right* sharing (search-structure); approximating the *forbidden*
sharing (memo) is mechanically possible (same cell, content key) but self-defeating for the
optimizer — and superposition's own sharing does **not** generalize to arbitrary memo (sup shares
across the branches of a *deliberate* choice; it cannot *find* equal subterms — that is
hash-consing).

## Open

- **The chained / higher-order probe** — construct a case where superposed observations feed each
  other so the cross-product is *reduction*-materialized, and measure whether the choice cell
  turns exponential reduction into linear (the regime where it would actually beat readback
  correlation). The mechanism is in place; the workload is not yet built.
- **Full-recognizer differential** — drive Conjecture 2 through a real `lib/kernel` recognizer
  (`Nat`/`Bool`/`List` via `param_apply`) from `test/eval-ic-net.test.ts`. Prediction: passes for
  structurally-linear recognizers, fails for project-twice ones.
- **Label coordination for the general non-affine case** — make a recognizer's internal δ carry
  the hole's label so it correlates (see the `Labels` discussion); this is the genuine unbuilt
  research that decides whether tier-2/3 superposition is sound for the recognizers that matter.
