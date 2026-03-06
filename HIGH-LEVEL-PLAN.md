# Disp — High-Level Plan

## Vision

Disp is a reflective programming language built on tree calculus, paired with a neural
program synthesis engine. The end state: an ML model that takes a type (a specification)
and iteratively constructs a term (a program) satisfying it, using MCTS-guided search
with self-play to continuously expand the boundary of what it can synthesize.

Tree calculus is the foundation because it is *natively reflective* — terms ARE data,
so `quote` is trivial. Types live inside the term space (`[U,Q] ↪ U`), which means
the optimizer, the types it targets, and the programs it produces all inhabit the same
universe. This is the strange loop that enables self-improvement.

---

## Architecture Layers

### Layer 0: Tree Calculus Runtime
The computational substrate. Binary trees with 5 triage reduction rules.
Hash-consed for O(1) equality. Eager evaluation with budget.

**Status: Complete.** `src/tree.ts`

### Layer 1: The Language (disp)
Dependently-typed surface language (CoC) compiling to tree calculus via bracket
abstraction. Provides human-readable syntax for writing programs and types.

**Status: Core working.** Parse → typecheck → compile → eval pipeline.
Church-encoded booleans/naturals, prelude, REPL with file I/O.

**Remaining language work:**
- [x] Records / named arguments (product types as Church encodings)
- [x] Coproducts / sum types (tagged unions as Church encodings)
- [x] Recursive definitions (fixpoint combinator)
- [x] Better error messages (source positions, caret underlines)
- [ ] Stepping / interactive evaluation (low priority — eager eval limits usefulness)
- [ ] Browser UI (editor + tree visualization)

### Layer 2: Neural Program Synthesis (the optimizer)
An ML system that takes a type `τ : U → Q` and searches for a term `t` maximizing
`τ(t)`. Uses MCTS with a neural policy/value network. Self-improves through
adversarial self-play at the boundary of its capabilities.

**Status: Not started.**

### Layer 3: JS Compilation via Learned Equivalences
A neural-guided compilation layer that learns to translate tree calculus programs
into equivalent JavaScript, using tree-calculus-JS equivalence axioms as rewrite
rules. The network learns to apply these axioms to provably optimize programs.

**Status: Not started.** Depends on Layer 2 infrastructure.

---

## Scoring System (Q)

### Phase A: Binary (Q = {0, 1})
A term either type-checks or doesn't. The optimizer does classical proof search /
program synthesis against CoC types. This is the starting point.

### Phase B: Soft Scoring (Q = [0, 1])
The score reflects *degree* of type satisfaction:
- Base score: how much of the tree type-checks successfully (partial checking)
  - e.g. a term that satisfies 80% of a type's structure scores 0.8
- Bonuses (above the base):
  - Smaller terms score higher (Kolmogorov complexity pressure)
  - Fewer reduction steps to normalize score higher (runtime efficiency)

This gives the optimizer continuous gradient signal even when it can't fully
satisfy a type, and incentivizes elegant solutions.

### Phase C: Multi-Objective (Q = ℝ^n)
Eventually: separate axes for correctness, size, speed, readability.
Pareto-optimal search across objectives. Future work.

---

## Synthesis Engine Design

### Core Loop
```
repeat:
  1. Generate target type τ (from curriculum / self-play)
  2. Run MCTS to build candidate term t
     - Policy network: π(state, τ) → distribution over actions
     - Value network: V(state, τ) → expected score
     - Actions: PlaceLeaf (△), PlaceCombinator(name), Introduce (app node)
  3. Evaluate: score = τ(t) via bounded eval
  4. Update networks from MCTS statistics
  5. If t scores well, extract frequent subtrees → new library combinators
```

### Partial Terms
Terms with typed holes: `□_i` placeholders that MCTS fills incrementally.
```
PartialTerm ::= □_i | △ | PartialTerm PartialTerm
```
Placing `Introduce` creates an app node with two new holes (AND-node in game tree).
Placing a leaf or combinator fills one hole (OR-node in game tree).

### Combinator Library
Grows over time via Stitch-like abstraction. Frequent high-scoring subtrees
get named and added to the action space. The policy must handle a growing
action space (compatible family: new actions initialized near zero weight).

```
L₀ = {△}
L₁ = L₀ ∪ {K, I}  (discovered that K and I appear everywhere)
L₂ = L₁ ∪ {S, church_succ, ...}
...
```

### Self-Play / Curriculum
The meta-type `τ_meta` scores the optimizer itself:
- Generate types at the boundary of current ability
- Target ~50% solve rate (maximum learning signal)
- Diversity bonus to prevent mode collapse
- Track the frontier: which types are barely solvable?

---

## Neural Network Architecture

### Interface
External Python/PyTorch process communicating with the TypeScript runtime.

```
TypeScript (disp runtime)          Python (neural networks)
  ├─ tree calculus eval     ←──→   ├─ policy network π
  ├─ type checker           ←──→   ├─ value network V
  ├─ MCTS game logic               ├─ training loop
  └─ term serialization     ────→  └─ observe: Tree → Tensor
```

Communication: likely JSON over stdin/stdout or a local socket.
Trees serialized as sequences or adjacency lists for the network.

### observe: Tree → Tensor
Need a fixed-size representation of variable-size trees for the neural network.
Options to explore:
- Tree-LSTM / recursive neural network (natural but slow)
- Flattened traversal (pre-order/in-order) with padding
- Graph neural network on the tree structure
- Learned embedding via tree autoencoder

### Policy Network
Input: (partial_term_embedding, target_type_embedding)
Output: distribution over actions (PlaceLeaf, PlaceCombinator(i), Introduce)

### Value Network
Input: (partial_term_embedding, target_type_embedding)
Output: expected final score ∈ [0, 1]

---

## JS Compilation Layer

### The Idea
Tree calculus programs are correct but slow (one triage rule per reduction step).
JavaScript is fast but unverified. We want *provably correct* compilation:
every rewrite step is justified by an equivalence axiom.

### Equivalence Axioms (Tree Calculus ↔ JS)
```
F(△)           = null
F(△ a)         = [F(a)]                    -- stem as 1-element array
F(△ a b)       = [F(a), F(b)]              -- fork as 2-element array
F(△ △ y z)     = F(y)                      -- K rule
F(△ (△ x) y z) = F(x)(F(z))(F(y)(F(z)))   -- S rule
F(triage c d b △)       = F(c)             -- triage leaf
F(triage c d b (△ u))   = F(d)(F(u))       -- triage stem
F(triage c d b (△ u v)) = F(b)(F(u))(F(v)) -- triage fork
```

### Neural-Guided Rewriting
The synthesis engine (Layer 2) can learn to apply these axioms:
- Input: a tree calculus term
- Output: a sequence of rewrite steps producing equivalent JS
- Training signal: does the JS produce the same output? Is it faster?

This is itself a program synthesis problem — the optimizer optimizes the optimizer.
Requires Layer 2 infrastructure first.

### Incremental Approach
1. Hand-write a simple tree→JS compiler (pattern match on known combinators)
2. Verify equivalence by property-based testing (random inputs)
3. Use the neural system to discover new rewrite patterns
4. Eventually: the neural system writes the compiler

---

## Roadmap

### Phase 1: Language Foundation ✓
*Goal: disp is pleasant to use for writing small programs and types.*

- [x] Tree calculus runtime with hash-consing
- [x] CoC type checker (bidirectional, named variables)
- [x] Bracket abstraction with optimizations (eta, S(K p)(K q), S(K p)I)
- [x] Church-encoded booleans and numerals (true/false/number literals)
- [x] compileAndEval (eager evaluation during collapse)
- [x] REPL with :load/:save, prelude auto-load, CLI file execution
- [x] Type-guided tree recognition (disambiguates id/1, zero/false)
- [x] Records / named arguments (Church-encoded, {x : A, y : B} syntax)
- [x] Coproducts / sum types (Church-encoded, <Left : A | Right : B> syntax)
- [x] Recursive definitions (let rec with fixpoint combinator)
- [x] Better error messages (source positions, caret underlines)

### Phase 2: Synthesis Infrastructure ← CURRENT
*Goal: the MCTS engine can search for terms given a type.*

- [ ] `observe: Tree → Tensor` serialization format
- [ ] Python bridge (TypeScript ↔ PyTorch communication)
- [ ] Partial term representation (terms with holes)
- [ ] MCTS game logic (AND-OR tree, UCB exploration)
- [ ] Simple policy/value networks (even random at first)
- [ ] Training loop on hand-written types
- [ ] Benchmark: can it synthesize `id`, `K`, `S`, `not`, `succ`?

### Phase 3: Self-Play & Library Learning
*Goal: the system improves itself and grows its vocabulary.*

- [ ] Type generator (curriculum: random types near the frontier)
- [ ] Self-play loop (generate → solve → learn)
- [ ] Difficulty calibration (~50% solve rate targeting)
- [ ] Stitch-like combinator extraction from successful syntheses
- [ ] Growing action space with compatible policy updates
- [ ] Benchmark: does performance improve over time on held-out types?

### Phase 4: JS Compilation
*Goal: tree calculus programs compile to equivalent, faster JS.*

- [ ] Hand-written tree→JS compiler for known patterns
- [ ] Property-based equivalence testing
- [ ] Equivalence axiom representation as rewrite rules
- [ ] Neural rewrite search (synthesis engine applied to compilation)
- [ ] Benchmark: speedup factor on standard programs

### Phase 5: Closing the Loop
*Goal: self-hosting, self-improvement, the strange loop.*

- [ ] Meta-type τ_meta (score the optimizer on its own improvement)
- [ ] Optimizer discovers new compilation strategies
- [ ] Optimizer discovers new type system features
- [ ] Disp compiler partially written in disp
- [ ] The system proposes and validates its own extensions

---

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Calculus | Tree calculus | Native reflection (terms = data), no need for Gödel encoding |
| Type system | CoC (Type:Type) | Maximally expressive, inconsistent as logic but fine as language |
| Evaluation | Eager | Tree calculus trees are always in normal form |
| Encoding | Church | No inductive types needed; everything is functions |
| Q (initial) | {0,1} | Start simple; binary type checking |
| Q (target) | [0,1] | Partial checking + size/resource bonuses |
| ML framework | PyTorch (external) | Mature ecosystem, GPU support, separate process |
| Runtime | TypeScript | Fast iteration, browser-compatible, good enough for now |
| Bracket abstraction | Optimized S/K/I | Eta reduction + S(Kp)(Kq) + S(Kp)I prevents blowup |

---

## Open Questions

- **How to serialize trees for neural networks?** Tree-LSTM vs flattened vs GNN.
  Need to experiment. The encoding must handle variable-size trees efficiently
  and preserve structural information.

- **How to represent partial terms in MCTS?** Need efficient hole tracking,
  type propagation to holes (what type does each hole need?), and incremental
  evaluation (score what's filled so far).

- **What's the right granularity for MCTS actions?** PlaceLeaf + Introduce
  is minimal but deep. Allowing PlaceCombinator(K) is like giving the network
  a larger vocabulary. Too few actions = deep trees, too many = wide branching.

- **How to handle non-termination during synthesis?** The search might produce
  divergent terms. Budget-bounded eval handles this, but the score for ⊥ needs
  to be informative (not just 0).

- **When to add records/coproducts?** They're Church-encoded syntactic sugar,
  so they don't change the core. But they make types more readable, which
  matters for the neural network's input representation.

- **Communication protocol for Python bridge?** JSON-RPC? Protobuf?
  Needs to be fast enough for thousands of eval calls per MCTS batch.
  Batched evaluation likely necessary.
