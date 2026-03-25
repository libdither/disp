# Optimization Strategy for Disp

## The Two-Level Problem

Disp has an unusual optimization challenge: the tree-native type checker is a **program written in tree calculus**, executed by an **evaluator written in TypeScript**. Both layers can be optimized, and they interact in non-obvious ways.

```
                         ┌──────────────────────────┐
  Tree-level             │  Tree programs            │
  (algorithmic           │  (WHNF_STEP, TYPECHECK,   │  ← bracket abstraction,
   complexity)           │   treeEq, convertible)    │    data structures, encodings
                         └────────────┬─────────────┘
                                      │ calls apply()
                         ┌────────────▼─────────────┐
  Runtime-level          │  TypeScript evaluator     │
  (execution             │  (apply, hash-consing,    │  ← memoization, graph reduction,
   efficiency)           │   tree construction)      │    evaluation strategy
                         └──────────────────────────┘
```

The tree-native type checker is currently ~5000x slower than the TypeScript bootstrap. Closing this gap requires optimizing **both** levels — but optimizing them independently can lead to wasted effort or even regressions.

## What Each Level Controls

### Tree level: algorithmic complexity

Tree-level optimizations change the **structure** of the compiled tree programs. They affect how many `apply()` calls are needed to compute a result, regardless of how fast each call is.

Examples:
- **Unary → binary numbers**: Dispatching on fuel `n` goes from O(n) to O(log n) steps
- **Church list → balanced tree map**: Environment lookup goes from O(n) to O(log n)
- **B/C combinators**: Each composition takes 1 step instead of 3 S/K steps
- **Better bracket abstraction**: Smaller compiled trees, fewer intermediate applications

These are **algorithmic** improvements. No runtime optimization can replicate them — you can't cache your way out of O(n) when every call in the chain has different arguments.

### Runtime level: execution efficiency

Runtime optimizations change **how fast** each `apply()` call executes, without changing the tree programs at all.

Examples:
- **Memoization**: Cache `apply(f, x)` results so identical calls return in O(1)
- **Graph reduction**: Share subcomputations instead of re-evaluating them
- **Selective laziness**: Skip evaluating arguments that will be discarded (e.g., by K combinator)
- **Specialized fast paths**: Detect common patterns (arithmetic, triage) and use optimized code paths

These exploit **redundancy** in the evaluation. When the same `(f, x)` pair appears 100 times during evaluation, memoization makes 99 of those free.

## When Optimizations Interact Destructively

The dangerous case: **optimizing the same thing at both levels**.

### Concrete example: memoization at both levels

**Tree-level memoization**: Build a lookup table in tree calculus — a tree-encoded hash map. Every lookup costs ~50-200 apply steps (traversing the tree trie, comparing keys via treeEq, handling collisions). Every insertion restructures the tree.

**Runtime-level memoization**: Cache `(f.id, x.id) → result` in a TypeScript Map. Lookup is O(1) with no apply steps.

If both are active:
1. The tree-level memo code runs, generating apply calls for table operations
2. The runtime-level cache makes most of those calls free (since the same trie traversal patterns repeat)
3. But the tree-level memo adds **new** unique calls (for map manipulation) that the runtime cache doesn't help with
4. Net result: **slower than runtime memoization alone**, because the tree-level memo's overhead exceeds its benefit

### The general principle

> When a tree-level optimization reimplements what the runtime already provides,
> the tree level pays overhead for zero gain.

The overhead is especially bad in tree calculus, where even a hash table lookup costs hundreds of applies. The runtime can do the same thing with a single Map.get().

### When they're complementary

Tree-level and runtime-level optimizations are **complementary** when they target different things:

| Optimization | What it fixes | Can the other level do this? |
|---|---|---|
| Binary fuel encoding | O(n) → O(log n) algorithm | No — every call has different args |
| Balanced tree env | O(n) → O(log n) lookup | No — each list element is unique |
| B/C combinators | Fewer steps per composition | Partially — runtime can cache S/K chains |
| Apply memoization | Redundant recomputation | No — tree can't do O(1) caching |
| Graph reduction | Shared subexpression eval | No — tree can't control eval strategy |

**Rule of thumb**: The runtime handles **redundancy** (repeated subcomputations). The tree level handles **complexity** (cost per unique computation). Optimize each for what it's uniquely positioned to do.

## The Right Metrics

Because the two levels have different optimization targets, we need multiple metrics:

### 1. Total apply steps (tree program complexity)

Deterministic, reproducible. Measures the inherent algorithmic cost of the tree program independent of how clever the runtime is.

**Use for**: Evaluating tree-level optimizations (B/C combinators, binary numbers, better data structures). Good regression test: "WHNF_STEP on a leaf should take no more than N steps."

**Limitation**: Doesn't predict wall-clock time once the runtime has memoization.

### 2. Unique apply calls (information-theoretic minimum)

Count the number of distinct `(f.id, x.id)` pairs encountered during evaluation. This is the **theoretical minimum work** — even perfect memoization can't reduce it further.

**Use for**: Understanding how much redundancy exists. If total steps = 100,000 and unique calls = 5,000, then 95% of work is redundant and memoization will help enormously. If unique calls ≈ total steps, memoization won't help and you need tree-level improvements.

### 3. Wall-clock time (actual performance)

What actually matters for "can the type checker run in the REPL?"

**Use for**: End-to-end performance evaluation. The ultimate metric.

**Limitation**: Noisy (hardware variance, GC pauses, JIT compilation). Need multiple runs and statistical analysis.

### 4. Cache statistics (once memoization exists)

Hit rate, cache size, eviction count. Bridges the gap between step counts and wall-clock time.

**Use for**: Understanding *why* performance changed. "Wall-clock improved 10x because cache hit rate is 95%" vs. "Wall-clock improved 2x because we reduced unique calls by half."

### How they relate

```
wall_clock_time ≈ unique_calls × cost_per_unique_call
                + cache_hits × cost_per_cache_lookup
                + overhead (GC, hash-consing, etc.)
```

| Optimization type | Total steps | Unique calls | Wall-clock |
|---|---|---|---|
| B/C combinators | ↓ (fewer steps) | ↓ (fewer unique) | ↓ |
| Binary numbers | ↓↓ (log vs linear) | ↓↓ | ↓↓ |
| Apply memoization | unchanged | unchanged | ↓↓ (redundancy eliminated) |
| Graph reduction | unchanged | possibly ↓ | ↓ |

## Phase 1: Runtime Optimizations (Current Focus)

### Apply memoization

The simplest high-impact optimization. Tree calculus `apply()` is a **pure function** — same `(f, x)` always produces the same result (hash-consing guarantees tree identity). This means we can cache results in a `Map<(f.id, x.id), result>`.

**Why it works well here**: Hash-consing gives us O(1) equality on cache keys. We already pay for tree identity tracking; memoization leverages that investment.

**Budget interaction**: The budget object tracks remaining evaluation steps. For cache hits, we don't decrement the budget — the computation was already done. This is semantically correct: budget limits *new work*, and cache hits aren't new work.

**What to cache**: Only fork-case apply results (the cases with recursive calls). Leaf→stem and stem→fork are O(1) operations where cache lookup overhead exceeds the computation cost.

### Graph reduction

Beyond simple memoization, graph reduction changes **how** evaluation proceeds.

In standard tree calculus, the S combinator eagerly evaluates both branches:
```
apply(fork(stem(c), b), x):
  cx = apply(c, x)    ← always evaluated
  bx = apply(b, x)    ← always evaluated (even if discarded by cx)
  return apply(cx, bx)
```

If `cx` turns out to be a K combinator (`fork(LEAF, something)`), then `bx` was computed for nothing. In the tree-native type checker, triage dispatches frequently discard 2 of 3 branches, and the S combinator feeds into these dispatches.

**Selective laziness**: Instead of always evaluating both sides of S, defer `bx` until it's actually demanded (i.e., pattern-matched on in the recursive apply). This requires representing "pending applications" somehow — either via thunks or via a graph representation with indirection nodes.

**Trade-off**: Laziness adds per-node overhead (thunk allocation, indirection following) but saves wasted computation. Whether it's net positive depends on how often arguments are discarded. The benchmark suite should measure this.

## Phase 2: Tree-Level Optimizations (Future)

Once runtime optimizations are in place, measure the remaining gap. Focus on improvements the runtime can't provide:

- **Binary fuel encoding**: O(n) → O(log n) fuel dispatch
- **Balanced tree environments**: O(n) → O(log n) variable lookup
- **B/C combinators**: Reduce compiled tree sizes and step counts by ~3x for composition-heavy code
- **Specialized dispatch**: 2-way WHNF dispatch instead of 5-way TERM_CASE where only 2 cases are used

## Analogies from Other Systems

| System | "Tree level" | "Runtime level" | Interaction |
|---|---|---|---|
| **GHC/Haskell** | Core optimizations (inlining, fusion) | STG machine (thunks, sharing, GC) | Over-inlining defeats sharing |
| **HVM** | Lambda terms | Interaction nets (optimal sharing) | Runtime sharing makes manual sharing redundant |
| **SQL** | Query plan | Storage engine (buffer pool, indexes) | Query optimizer has cost model of engine |
| **JVM** | Java bytecode | JIT compiler (inlining, devirtualization) | Hand-optimization can confuse JIT heuristics |

The common lesson: **the optimizer at one level needs a cost model of the other level**. GHC's simplifier knows about the STG machine's sharing behavior. SQL query optimizers know about disk seek costs. Disp's tree-level optimizer should know about the runtime's memoization capabilities — and avoid reimplementing them in tree calculus.
