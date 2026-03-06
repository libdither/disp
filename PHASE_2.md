# Phase 2: Neural Program Synthesis — Research & Design

## Overview

Phase 2 adds a neural-guided MCTS engine that synthesizes tree calculus terms given a type specification. This document synthesizes research across three domains — search algorithms, communication architecture, and neural tree encodings — into a concrete implementation plan.

---

## 1. Search Architecture: AND-OR MCTS

### Why AND-OR, not flat MCTS

Type inhabitation is naturally an AND-OR problem:
- **OR nodes**: Which combinator/function to apply? (choose one)
- **AND nodes**: Once you pick `f : A -> B -> C`, you must provide terms of type A *and* type B (satisfy all)

This is exactly what [Canonical (ITP 2025)](https://arxiv.org/abs/2504.06239) implements for dependent type inhabitation in Lean, and what [AlphaProof (Nature 2025)](https://www.nature.com/articles/s41586-025-09833-y) uses for theorem proving (they call AND nodes "product nodes").

**Backpropagation rule**: At OR nodes, take max child value. At AND nodes, take min child value (difficulty = hardest subgoal). AlphaProof validates this at scale.

### MCTS variant: PUCT (AlphaZero-style)

Use PUCT selection with a learned policy prior:

```
UCT(parent, child) = W(child)/N(child) + C * pi(child) * sqrt(N(parent)) / (1 + N(child))
```

Where `pi(child)` is the policy network's predicted probability for that action. Start with `C = 1.0–1.4`, tune empirically.

**Virtual loss batching**: Run B concurrent simulations in one tree, using virtual loss to diversify (temporarily penalize visited nodes so parallel simulations explore different branches). Batch size B = 16–32. This is the standard approach (AlphaZero, KataGo, Leela Chess Zero).

### Action space: SExpr-level, not tree-level

**Critical design decision**: synthesis operates on typed SExpr (the surface language), not raw tree calculus combinators. The type checker guides the search — tree compilation happens only after a complete, well-typed term is found.

This avoids a representation mismatch: K/I/S are tree-level runtime constants (`src/tree.ts`), while type-directed pruning requires SExpr-level type information (`src/typecheck.ts`). Mixing the two creates an IR boundary problem with no clear type propagation.

**SExpr-level actions** (for a hole of type `T` in context `Γ`):

| Action | Effect | Typing |
|--------|--------|--------|
| `Var(name)` | Fill hole with a context variable | Check `Γ(name)` convertible with `T` |
| `App` | Fill hole with `f x`, creating two child holes | Hole for `f : ?A -> T`, hole for `x : ?A` |
| `Lam` | Fill hole with `{x} -> body` | Requires `T` = `(x : A) -> B`, creates hole for body of type `B` in `Γ, x:A` |
| `Type` | Fill hole with `Type` | Check `T` convertible with `Type` |
| `Library(name)` | Fill hole with a named definition from context | Check its type convertible with `T` |

No unification engine is needed — "convertible with" uses the existing `convertibleSExpr` function. For `App` holes, the function type `?A -> T` is enumerated from context entries whose return type matches `T`.

Growing the action space over time via library learning (Stitch/DreamCoder) is critical — it makes search shallower as the system discovers useful building blocks.

**Autoregressive decomposition** (from AlphaTensor): For very large action spaces, generate actions token-by-token with a decoder conditioned on state. This converts one huge-branching step into a sequence of small-branching steps.

### Partial terms with typed holes

Represent partial terms as SExpr trees with hole nodes:

```typescript
type PartialSExpr =
  | { tag: "hole", id: number, type: SExpr, ctx: Context }
  | { tag: "svar", name: string }
  | { tag: "sapp", func: PartialSExpr, arg: PartialSExpr }
  | ... // other SExpr variants
```

Each hole carries its expected type and available context. When filling a hole:
1. Choose a head symbol whose return type is *convertible* with the hole's type (uses existing `convertibleSExpr`, no unification engine needed)
2. Create child holes for each argument
3. Type-check the partial term bidirectionally; **prune immediately if ill-typed**

**Scoring partial terms** (before all holes filled):
- Simple heuristic: `0.9 ^ (remaining_holes)` — penalizes terms with many holes remaining
- Better: learned value network estimates expected completion probability
- Pruning signal: holes with uninhabitable types → dead end, score 0

### Handling non-termination

Tree calculus evaluation is eager and budget-bounded. During synthesis:
1. Cap tree size at N nodes (only allow PlaceLeaf after that)
2. Use existing `BudgetExhausted` mechanism when testing synthesized terms
3. Score budget-exhausted terms as 0
4. Progressive budgets: small during early training, larger as model improves

---

## 2. Communication: TypeScript ↔ Python Bridge

### Architecture

```
TypeScript Process              Python Process
├─ REPL / CLI                   ├─ Inference server
├─ Tree calculus evaluation     │  ├─ Receives batched positions
├─ Type checking                │  ├─ Converts to tensors
├─ MCTS game logic              │  ├─ Runs policy + value network
├─ Sends batched positions ───────→ ├─ Returns predictions
│  via stdin/stdout JSON        │  └─ Reloads checkpoints
└─ Writes game records ──────────→ Training script (offline)
                                   ├─ Reads game records
                                   ├─ Trains networks
                                   └─ Saves checkpoints
```

**Why this split**: Tree calculus eval is recursive data structure manipulation — V8/TS is 10–100x faster than Python for this. Neural network inference needs PyTorch/GPU. Keep each in its strength.

### Protocol: stdin/stdout JSON lines (prototype)

Simplest possible. TS spawns Python, communicates via newline-delimited JSON:

```
→ {"positions": [[2,1,0,0], [2,2,0,0,0]], "types": [...], "batch_id": 1}
← {"policies": [[0.1, 0.3, ...], ...], "values": [0.7, 0.2], "batch_id": 1}
```

**Latency budget** (per batch of 32):

| Component | Time |
|-----------|------|
| TS: 32 MCTS selections + virtual loss | ~0.5ms |
| TS: serialize 32 trees → JSON | ~0.1ms |
| IPC round-trip (pipe) | ~0.2ms |
| Python: deserialize + tensorize | ~0.3ms |
| Python: neural net inference (GPU) | ~1–5ms |
| Python: serialize response | ~0.1ms |
| TS: deserialize + backpropagate | ~0.3ms |
| **Total per batch** | **~2.5–6.5ms** |

For 800 simulations with batch 32 = 25 batches = **~125ms per move**. IPC is <10% of total — **the protocol choice barely matters** for the prototype.

### Tree serialization on the wire

Flat integer array, pre-order traversal: `leaf=0, stem=1, fork=2`.

Example: `fork(stem(leaf), leaf)` → `[2, 1, 0, 0]`

One byte per node. A 1000-node tree = 1KB. Batch of 32 = 32KB. Trivially fast.

**Tree→tensor conversion happens Python-side** (keeps TS architecture-agnostic; tensor shape depends on network architecture which is Python's concern).

### Training pipeline

Offline, file-based:
1. Self-play writes game records to JSONL files (one line per trajectory)
2. Python training script reads files, trains, saves checkpoint
3. TS (or Python inference server) loads new checkpoint
4. Repeat

This is what Leela Chess Zero and KataGo do. Simple, debuggable, good enough for single-machine.

### Future upgrades (defer)

- JSON → MessagePack (2–5x faster serialization)
- stdin/stdout → Unix domain sockets (multiple connections)
- ONNX export for inference-only in TS (eliminates IPC entirely)

---

## 3. Neural Network Architecture

### The key insight: hash-consing memoization

Tree calculus trees are hash-consed — every unique subtree has a unique integer ID. This is **the single most important optimization**:

- A tree from bracket abstraction may have O(N²) nodes but only O(N) *unique* subtrees (S, K, I are heavily shared)
- Cache `embed[hash_id]` for each unique subtree
- Any recursive encoding becomes practical even at depth 100 if most subtrees are cached
- During MCTS, most evaluations will be cache hits (small modifications to existing trees)

### Recommended: Pre-order traversal + Transformer + tree positional encoding

**Prototype architecture:**

1. **Serialize tree** in pre-order: emit `LEAF=0`, `STEM=1`, `FORK=2` tokens. Sequence length = node count.
2. **Pad/truncate** to max length (512 or 1024).
3. **Tree positional encoding**: Each token's position = binary path from root (sequence of left/right choices). Encode using sinusoidal functions over each path bit, following [Shiv & Quirk (NeurIPS 2019)](https://papers.nips.cc/paper/9376-novel-positional-encodings-to-enable-tree-based-transformers). This lets the Transformer capture tree structure in O(1) layers regardless of depth.
4. **Model**: 3–4 layer Transformer encoder (d_model=128, 4 heads). Mean-pool output for fixed-size embedding.
5. **Two heads**: Policy (softmax over actions) + Value (scalar expected score).

**Why this over alternatives:**

| Approach | Depth limit | Speed | Quality | Verdict |
|----------|------------|-------|---------|---------|
| Tree-LSTM | ~20–30 | Slow (sequential) | Medium-High | Depth is a showstopper |
| GNN (full tree) | ~5–10 rounds (over-smoothing) | Medium | Medium | Over-smoothing at depth 50+ |
| Pre-order + Transformer | ~1024 tokens | Fast | High | **Recommended for prototype** |
| DAG-aware GNN | Unlimited (DAG is shallow) | Medium-Fast | High | Production upgrade |
| Random recursive + memoization | Unlimited | Very fast | Low-Medium | Simplest possible baseline |

### Even-simpler fallback: random recursive embedding

If the Transformer feels too heavy for the first prototype:

```python
# No training needed for the encoder — only train the MLP heads
W_stem = random_matrix(d, d)
W_fork = random_matrix(d, 2*d)
leaf_embed = random_vector(d)

def embed(tree):
    if cached[tree.id]: return cached[tree.id]
    if tree == LEAF: return leaf_embed
    if tree.tag == "stem": return tanh(W_stem @ embed(tree.child))
    if tree.tag == "fork": return tanh(W_fork @ [embed(tree.left); embed(tree.right)])
    cached[tree.id] = result
    return result
```

Feed root embedding into MLP policy/value heads. Memoization by hash-consing ID makes this fast even for deep trees. Implementable in an hour. Provides a reasonable baseline.

### Production upgrade: DAG-aware GNN

When flat sequences hit their limits:
1. Convert tree to its minimal DAG using hash-consing IDs
2. Annotate nodes with: type (one-hot), combinator pattern (matches S/K/I?), subtree size (log-scaled), depth
3. Run 5–8 rounds of GATv2 or GIN on the DAG (DAG is much shallower than the tree)
4. Attention-weighted pooling → fixed-size embedding
5. Optional: 2–3 Transformer layers over top-K node embeddings for global structure

Use [PyTorch Geometric](https://github.com/pyg-team/pytorch_geometric) for GNN layers.

---

## 4. Training Loop

### Phase 2a: No ML — exhaustive type-directed search

Before adding neural networks, implement AND-OR search with iterative deepening:
- Action set: {Var(name), App, Lam, Type, Library(name)} — all at SExpr level
- Type-check partial terms bidirectionally; prune ill-typed branches using existing `convertibleSExpr`
- Score by `0.9^(remaining_holes)`
- Iterative deepening by term size
- Should solve: `id`, `K`, `not`, `succ`, simple Church-encoded functions

This validates the search infrastructure without ML complexity. [Canonical](https://github.com/chasenorman/Canonical) achieves 23M refinements/second with this approach.

### Phase 2b: Add learned guidance

**Option A (simpler): XGBoost policy/value** — Following [Parsert & Polgreen (AAAI 2024)](https://arxiv.org/abs/2307.09564):
- Features: bag-of-words over partial term tokens + type tokens
- Policy trained on MCTS visit proportions
- Value trained on discounted depth: `0.9^D` where D = distance to solution
- Sliding window of last 4 RL iterations for training data
- They found: 26 percentage point improvement over baseline enumerator

**Option B (recommended): Neural policy/value** — Train the Transformer described above:
- Policy target: MCTS visit counts (softmax temperature τ=1)
- Value target: binary outcome (1 if type eventually inhabited, 0 otherwise)
- Loss: `L = L_value + L_policy + c * L_reg` (standard AlphaZero)
- Optimizer: Adam, lr=1e-3, batch size 256
- Train on trajectories from self-play

### Phase 2c: Self-play and curriculum

Adapted from AlphaProof:

```
repeat:
  1. Generate target types (curriculum):
     - Easy: A -> A, A -> B -> A, Bool, Nat
     - Medium: (A -> B) -> (B -> C) -> A -> C
     - Hard: polymorphic compositions, dependent types
     - Adaptive: ~50% solve rate = maximum learning signal
  2. Run MCTS (800 simulations, batch 32) to search for inhabitant
  3. If found: record trajectory as positive
  4. If not: record as negative (with partial progress)
  5. Train policy/value on collected data
  6. Every K rounds: run Stitch to extract library abstractions
  7. Add discovered abstractions to action space
```

**Data generation** (solving the cold-start problem):
- Generate random well-typed terms, then use their types as synthesis targets
- Sample "fantasy" types from the type grammar (DreamCoder approach)
- Use the existing prelude as seed problems

**Online RL is critical**: [Concord](https://www.cs.utexas.edu/~isil/concord.pdf) found that online updates during synthesis dramatically outperform offline pretraining alone. Update the policy after each batch of solved problems.

---

## 5. Implementation Roadmap

### Step 1: Typed holes in the language (~2 days)

Add `?` syntax for holes in SExpr. The type checker infers the expected type and context for each hole. This is the foundation for synthesis — the synthesizer fills holes.

```
> :synth (A : Type) -> A -> A
Synthesized: {A x} -> x
```

### Step 2: AND-OR search without ML (~3 days)

- Partial term representation with typed holes (SExpr-level, not tree-level)
- Iterative deepening over term size
- Type-directed pruning via existing `convertibleSExpr` (no unification engine needed)
- Action set: {Var(name), App, Lam, Type, Library(name)}
- Benchmark: can it find `id`, `K`, `not`, `succ`?

### Step 3: Python inference server (~2 days)

- `inference_server.py`: reads JSON from stdin, runs model, writes JSON to stdout
- TS spawns Python process, manages communication
- Start with random policy/value (uniform policy, constant value 0.5)
- Verify the plumbing works end-to-end

### Step 4: MCTS with neural guidance (~3 days)

- PUCT selection with policy prior
- Virtual loss batching (batch size 16–32)
- AND-OR backpropagation (max at OR, min at AND)
- Game record writing (JSONL trajectories)

### Step 5: Neural network training (~3 days)

- Pre-order tree encoding with tree positional encoding
- Small Transformer (3 layers, d=128, 4 heads)
- Training script reads game records, trains, saves checkpoint
- Inference server reloads checkpoints

### Step 6: Self-play loop (~2 days)

- Type curriculum generator
- Self-play → train → reload cycle
- Basic metrics: solve rate vs. training iteration
- Benchmark: solve rate on held-out types

### Step 7: Library learning (~3 days)

- Integrate [Stitch](https://github.com/mlb2251/stitch) (Rust with Python bindings) for abstraction extraction
- Run Stitch on solved programs after each epoch
- Add discovered abstractions as new actions
- Benchmark: does the action space grow? Does solve rate improve?

**MVP (Steps 1–2): ~1 week** — typed holes + exhaustive search, no ML. Already synthesizes simple programs.

Steps 3–7 are iterative improvements, each taking 1–2 weeks. Full pipeline is months, not weeks.

**Known scaling concerns** (address when they become bottlenecks, not before):
- `extendCtx` copies arrays on every bind (`typecheck.ts:26`): fine for ~thousands of nodes, optimize with persistent maps later
- Hash-consing caches are unbounded (`tree.ts:20`): add `clearCaches()` between synthesis runs if memory grows
- Named substitution is recursive (`typecheck.ts:37`): WHNF is budget-bounded, fine for prototype

---

## 6. Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Search structure | AND-OR MCTS | Natural for type inhabitation; validated by AlphaProof |
| MCTS variant | PUCT with virtual loss | Industry standard (AlphaZero, KataGo) |
| Synthesis level | SExpr (typed), not tree combinators | Type information guides search; compile to tree only for eval |
| Action granularity | SExpr constructors (Var/App/Lam/Type) | Type-directed pruning eliminates ill-typed branches early |
| IPC protocol | stdin/stdout JSON lines | Simplest; IPC is <10% of latency |
| Tree serialization | Pre-order flat integers | Compact, unambiguous, trivial to implement |
| Neural encoding | Transformer + tree PE | Handles depth in O(1) layers; fast on GPU |
| Memoization | Hash-consing ID cache | Tree calculus's killer feature; O(N²) → O(N) |
| Training | Offline file-based + online updates | Simple; validated by Leela/KataGo |
| Library learning | Stitch (defer to Step 7) | 1000x faster than DreamCoder's approach |
| Partial term scoring | 0.9^(holes) then learned value | Simple heuristic bootstraps; neural improves |

---

## 7. Key References

**Search & synthesis:**
- [AlphaProof (Nature 2025)](https://www.nature.com/articles/s41586-025-09833-y) — AND-OR MCTS for theorem proving
- [Canonical (ITP 2025)](https://arxiv.org/abs/2504.06239) — Type inhabitation solver, 23M refinements/sec
- [Parsert & Polgreen (AAAI 2024)](https://arxiv.org/abs/2307.09564) — RL + MCTS for syntax-guided synthesis
- [AlphaTensor (Nature 2022)](https://www.nature.com/articles/s41586-022-05172-4) — Sampled MCTS, autoregressive policy
- [DreamCoder (PLDI 2021)](https://dl.acm.org/doi/10.1145/3453483.3454080) — Wake-sleep library learning
- [Stitch (POPL 2023)](https://arxiv.org/abs/2211.16605) — Fast anti-unification for library extraction
- [Concord](https://www.cs.utexas.edu/~isil/concord.pdf) — Deduction-guided RL; online updates critical

**Neural tree encodings:**
- [Shiv & Quirk (NeurIPS 2019)](https://papers.nips.cc/paper/9376-novel-positional-encodings-to-enable-tree-based-transformers) — Tree positional encodings for Transformers
- [CSA-Trans (2024)](https://arxiv.org/html/2404.05767) — Efficient code structure embedding
- [AST-Transformer](https://arxiv.org/pdf/2112.01184) — Pre-order traversal is sufficient

**Practical implementations:**
- [nanoproof](https://github.com/Kripner/nanoproof) — Minimal AlphaProof implementation
- [Canonical source](https://github.com/chasenorman/Canonical) — Rust type inhabitation solver
- [Stitch source](https://github.com/mlb2251/stitch) — Rust + Python library learning
- [Minimal MCTS in Python](https://gist.github.com/qpwo/c538c6f73727e254fdc7fab81024f6e1) — ~90 lines

---

## 8. Open Questions

1. **How deep should iterative deepening go before ML takes over?** Canonical solves 84% of Natural Number Game problems with pure search. If exhaustive search handles the easy cases, ML is only needed for hard ones.

2. **XGBoost vs. neural networks for the first policy?** Parsert & Polgreen got strong results with XGBoost (simpler, faster to train, no GPU needed). Neural nets are more expressive but harder to get right. Consider XGBoost first.

3. **When to run Stitch?** After every epoch? After every N solved problems? DreamCoder runs library learning after each wake phase. Stitch is fast enough to run frequently.

4. **How to generate the type curriculum?** Random type generation risks mostly uninhabitable types. Using types of known programs (from the prelude) as starting points, then mutating them, could work better.

5. **When does synthesis move to tree level?** The current plan synthesizes at SExpr level for type-directed search. Tree-level synthesis (building raw △ trees, checking types by running them) is the long-term reflective vision — the type system itself lives in tree calculus, so the synthesizer and checker are the same program. But this requires bootstrapping the type system first (see HIGH-LEVEL-PLAN Phase 5).
