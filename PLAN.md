# Disp — Master Plan

## Vision

Disp is a reflective programming language built on tree calculus, paired with a neural
program synthesis engine. Tree calculus is natively reflective — terms ARE data, so the
type checker, the optimizer, and the programs it produces all inhabit the same universe.

See `GOALS.md` for the full vision: a universal self-improving optimizer via reflective calculus.

---

## Status: What's Done

### Layer 0: Tree Calculus Runtime — COMPLETE

`src/tree.ts` — Hash-consed binary trees with eager evaluation. 5 triage reduction rules.
O(1) equality via hash-consing IDs. Apply memoization. FAST_EQ primitive.

### Layer 1: The Language — COMPLETE

Two-phase pipeline: **elaborate** (SExpr → TypedExpr) then **compile** (TypedExpr → annotated Tree).

- `src/parse.ts` — Tokenizer + recursive descent parser → SExpr
- `src/tree-native-elaborate.ts` — TypedExpr pipeline: `elaborate` + `compileTE`
- `src/tree-native-checker.ts` — `checkAnnotated(defs, ann, type)` on annotated trees
- `src/compile.ts` — Legacy compiler (still used for recursive defs, REPL eval, type-level computation)
- `src/tree-native.ts` — Tree-native builtins and step functions (TYPECHECK, etc.)
- `src/repl.ts` — REPL with `:load`/`:save`/`:type`/`:tree`/`:ctx`/`:help`
- `src/coc.ts` — Encoding library (used by tree-encoded step functions and bench tests)

**Test suite**: 483 tests, 481 passing. 2 `[POST]` aspirational failures (Church-encoded types).

### The TypedExpr Architecture

```
SExpr  ──elaborate──>  TypedExpr  ──compile──>  annotated Tree  ──check──>  bool
                                       │
                                       └──>  compiled Tree (for execution)
```

**Phase 1 — elaborate**: Walks SExpr, resolves names, computes types (evaluating
type-level trees for dependent codomains). Lambdas stay as `lam` nodes. No compilation.

**Phase 2 — compile**: Bracket-abstracts all lambdas inside-out, producing combI/combK/combS
nodes with types at every node. D annotations at S nodes come from types naturally.
Collapses to annotated tree (for checking) and raw tree (for execution).

```typescript
type TypedExpr =
  | { tag: "tree",  value: Tree, type: Tree }     // resolved constant
  | { tag: "var",   name: string, type: Tree }     // bound variable
  | { tag: "app",   func: TypedExpr, arg: TypedExpr, type: Tree }  // application
  | { tag: "lam",   param: string, domain: Tree, body: TypedExpr, type: Tree }  // lambda
  | { tag: "combI", type: Tree }                   // identity [x]x
  | { tag: "combK", inner: TypedExpr, type: Tree } // constant [x]c
  | { tag: "combS", c: TypedExpr, b: TypedExpr, D: Tree, type: Tree }  // sharing [x](f g)
```

Key design: combK/combS cases in bracket abstraction delegate to app-chain form,
getting S(Kp)(Kq) and eta optimizations for free. No manual formula derivation needed.

---

## Bootstrapping Path

*Goal: self-hosted type checker as a tree constant, enabling scoring functions and neural synthesis.*

### Step 1: Eliminate compile.ts from elaboration path (NEXT)

compile.ts is still used for:
1. **Recursive definitions** — `compileRecAndEval` wraps step function in FIX
2. **REPL expression evaluation** — `compileAndEval` for running expressions
3. **Type-level computation** — `collapseAndEval` + `bracketAbstract` in Pi codomain abstraction

To remove (1): implement FIX application in the TypedExpr pipeline — compile the step function
body via TypedExpr, then apply FIX to the collapsed raw tree.

To remove (2): use `collapseRaw` from TypedExpr for REPL expressions.

To remove (3): this is legitimate type-level evaluation. Keep Expr-based bracket abstraction
for type computation only, or implement it on TypedExpr.

### Step 2: Encode checkAnnotated as a tree constant

The checker has no external mutable state besides `defs: KnownDefs`. It dispatches
on tree structure via triage — exactly what tree calculus does natively.

Encode as a tree constant following the same pattern as TYPECHECK (in `src/tree-native.ts`):
build Expr tree with bracket abstraction, compile to tree combinator.

The `defs` map becomes a Church list or balanced tree with FAST_EQ for O(1) lookup.
`extract()` becomes a recursive tree function. Depth limit becomes fuel-based recursion.

### Step 3: Scoring functions

With a tree-encoded checker:
```
score(program) = treeCheck(annotate(program), type) * performance_weight(program) * size_weight(program)
```

The annotated tree IS the proof certificate. `extract(annotated)` gives the program.
`treeCheck` verifies it. Scoring combines correctness with size/speed metrics.

### Step 4: Neural synthesis (Phase 2)

MCTS engine searches for annotated trees that maximize the scoring function.
The search operates on TypedExpr (or SExpr with typed holes), compiling candidates
to annotated trees for scoring. See `PHASE_2.md` for detailed design.

### Step 5: Self-improvement

The type checker, the annotated tree format, and the optimizer itself are all trees.
The scoring function can score improvements to any of these components.
See `GOALS.md` and `category-theory-speculations-plan.md` for the theoretical foundations.

---

## Runtime Optimization

*Goal: make tree-encoded operations fast enough for interactive use.*

The tree-native type checker (step functions compiled to tree constants) is ~5000x slower
than the TypeScript bootstrap. See `OPTIMIZATION_INFO.md` for the two-level optimization
analysis (tree programs vs. runtime evaluator).

### Level 1: Runtime optimizations (DONE)
- Apply memoization — `(f.id, x.id) → result` cache
- WAIT combinator — demand-driven recursion
- FAST_EQ — O(1) tree equality via hash-consing
- B/C/K combinator fast-paths in evaluator

### Level 2: Tree-level optimizations (deferred)
- Binary fuel encoding (O(log n) vs O(n) dispatch)
- Balanced tree environments (O(log n) vs O(n) lookup)
- Specialized 2-way dispatch for WHNF (vs 5-way TERM_CASE)

---

## Phase 2: Neural Program Synthesis

*Goal: MCTS engine searches for terms given a type specification.*

### Architecture

```
TypeScript Process              Python Process
├─ REPL / CLI                   ├─ Inference server
├─ Tree calculus evaluation     │  ├─ Receives batched positions
├─ Type checking                │  ├─ Runs policy + value network
├─ MCTS game logic              │  ├─ Returns predictions
├─ Sends batched positions ───────→ └─ Reloads checkpoints
│  via stdin/stdout JSON
└─ Writes game records ──────────→ Training script (offline)
                                   ├─ Reads game records
                                   ├─ Trains networks
                                   └─ Saves checkpoints
```

### Implementation Steps

1. **Typed holes + exhaustive search** (MVP, no ML) — `PartialSExpr`, AND-OR search, type-directed pruning
2. **Python inference server** — stdin/stdout JSON, random policy/value baseline
3. **MCTS with neural guidance** — PUCT, virtual loss batching, AND-OR backprop
4. **Neural network + training** — Transformer or random recursive embedding
5. **Self-play loop** — type curriculum, ~50% solve rate targeting
6. **Library learning** — Stitch-like abstraction extraction

See `PHASE_2.md` for detailed research and design decisions.

---

## Future Phases

### Phase 3: Self-Play & Library Learning
- Adaptive difficulty calibration (~50% solve rate)
- Stitch-like combinator extraction
- Growing action space with compatible policy updates

### Phase 4: JS Compilation
- Hand-written tree→JS compiler for known patterns
- Property-based equivalence testing
- Neural rewrite search (synthesis engine applied to compilation)

### Phase 5: Closing the Loop
- Meta-type tau_meta (score the optimizer on its own improvement)
- Disp compiler partially written in disp
- The system proposes and validates its own extensions

---

## Reference Documents

| Document | Content |
|----------|---------|
| `GOALS.md` | Core vision — universal self-improving optimizer via reflective calculus |
| `NATIVE_IMPL_PLAN.md` | Engineering status — TypedExpr pipeline, checker, test coverage |
| `TREE_NATIVE_TYPE_THEORY.md` | Theory — annotated tree format, typing rules, structural correspondence |
| `OPTIMIZATION_INFO.md` | Two-level optimization strategy — runtime vs tree-level |
| `PHASE_2.md` | Phase 2 deep-dive — search, IPC, neural architecture |
| `category-theory-speculations-plan.md` | Categorical foundations — Q-enriched CCC, optimizer as Kan extension |
