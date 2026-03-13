# Disp — Master Plan

## Vision

Disp is a reflective programming language built on tree calculus, paired with a neural
program synthesis engine. Tree calculus is natively reflective — terms ARE data, so the
type checker, the optimizer, and the programs it produces all inhabit the same universe.

---

## Status: What's Done

### Layer 0: Tree Calculus Runtime — COMPLETE

`src/tree.ts` — Hash-consed binary trees with eager evaluation. 5 triage reduction rules.
O(1) equality via hash-consing IDs.

### Layer 1: The Language — COMPLETE

Unified pipeline: parse → type-check (CoC-on-trees) → compile → eval.

- `src/parse.ts` — Tokenizer + recursive descent parser → SExpr
- `src/compile.ts` — SExpr → Tree via bracket abstraction (S/K/I with optimizations)
- `src/coc.ts` — CoC-on-trees type checker + tree-native builtins
  - CoC terms encoded directly as trees (Type=leaf, Var=stem, App/Lam/Pi=fork variants)
  - Bodies bracket-abstracted — tree calculus `apply()` IS substitution
  - Wrapped values carry (data, type) as Church pairs
  - `buildWrapped`, `whnf`, `convertible`, `normalize` on tree-encoded terms
  - Declaration pipeline with recursive definitions via omega combinator
  - CoC prelude: Church-encoded Tree/leaf/stem/fork/triage, encoding constructors,
    wrap/unwrap, Bool/tt/ff
  - Tree-native builtins (injected triage trees): FST, SND, CHILD, ENC_APP_T,
    ENC_LAM_T, ENC_PI_T, TERM_CASE (5-way dispatch)
  - Tree-native step functions (all working): treeEqStep, abstractOutStep,
    whnfStep, convertibleStep, inferStep (full type inference as a tree constant)
- `src/repl.ts` — REPL with `:load`/`:save`/`:type`/`:tree`/`:ctx`/`:help`
- Church-encoded booleans/naturals, records, coproducts, recursive definitions
- Church literal recognition (add 3 4 → 7, not true → false)
- Pretty printer with generated binder names and reverse name lookup

**Test suite**: 292 tests across 6 files. Includes cross-pipeline equivalence
checks (boolean truth tables, arithmetic, nested expressions).

### Known Issue: collapse/apply mismatch

`collapse` (uses `treeApply`, structural) and `collapseAndEval` (uses `apply`, eager)
produce structurally different trees for partial applications of combinator trees.
This blocks building recursive tree programs (treeEq, abstractOut) via bracket
abstraction + omega combinator. The TypeScript implementations remain the working
bridge. Future work: runtime fixpoint primitive or different compilation strategy.

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

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Search structure | AND-OR MCTS | Natural for type inhabitation (AlphaProof) |
| Synthesis level | SExpr (typed) | Type information guides search; compile to tree only for eval |
| Action space | Var/App/Lam/Type/Library | Type-directed pruning eliminates ill-typed branches |
| IPC protocol | stdin/stdout JSON lines | Simplest; IPC is <10% of latency |
| Tree serialization | Pre-order flat integers | leaf=0, stem=1, fork=2. Compact, unambiguous |
| Neural encoding | Transformer + tree PE (or random recursive baseline) | Handles depth; hash-consing memoization is the key optimization |
| Training | Offline file-based (JSONL) + online updates | Simple; validated by Leela/KataGo |

### Implementation Steps

#### Step 1: Typed holes + exhaustive search (MVP, no ML)

Add `PartialSExpr` with typed holes. Implement AND-OR search with iterative deepening.
Type-check partial terms bidirectionally; prune ill-typed branches.

```typescript
type PartialSExpr =
  | { tag: "hole", id: number, type: SExpr, ctx: Context }
  | { tag: "svar", name: string }
  | { tag: "sapp", func: PartialSExpr, arg: PartialSExpr }
  | ... // other SExpr variants
```

Actions for a hole of type T in context Gamma:
- `Var(name)` — fill with context variable (check type convertible)
- `App` — fill with `f x`, creating two child holes
- `Lam` — fill with `{x} -> body` (requires T to be a Pi type)
- `Type` — fill with Type (check T convertible with Type)
- `Library(name)` — fill with a named definition

Score partial terms by `0.9^(remaining_holes)`.
Benchmark: synthesize `id`, `K`, `not`, `succ`.

**Files**: `src/synth.ts`, `test/synth.test.ts`

#### Step 2: Python inference server

`inference_server.py`: reads JSON from stdin, runs model, writes JSON to stdout.
TS spawns Python process. Start with random policy (uniform) and constant value (0.5).
Verify plumbing end-to-end.

Wire format: `{"positions": [[2,1,0,0], ...], "batch_id": 1}` →
`{"policies": [[0.1, ...], ...], "values": [0.7, ...], "batch_id": 1}`

**Files**: `python/inference_server.py`, `src/bridge.ts`

#### Step 3: MCTS with neural guidance

PUCT selection with policy prior. Virtual loss batching (batch 32).
AND-OR backpropagation (max at OR, min at AND). Game record writing (JSONL).

**Files**: `src/mcts.ts`, `test/mcts.test.ts`

#### Step 4: Neural network + training

Prototype: pre-order tree encoding + small Transformer (3 layers, d=128, 4 heads).
Or even simpler: random recursive embedding (no encoder training, only MLP heads).

Training script reads game records, trains policy (MCTS visit counts) +
value (binary outcome), saves checkpoint.

**Files**: `python/model.py`, `python/train.py`

#### Step 5: Self-play loop

Type curriculum generator (easy → hard, targeting ~50% solve rate).
Self-play → train → reload cycle. Basic metrics: solve rate vs iteration.

**Files**: `python/selfplay.py`, `src/curriculum.ts`

#### Step 6: Library learning (stretch)

Integrate Stitch for abstraction extraction. Run on solved programs.
Add discovered abstractions as new actions.

---

## Future Phases (from HIGH-LEVEL-PLAN.md)

### Phase 3: Self-Play & Library Learning
- Adaptive difficulty calibration (~50% solve rate)
- Stitch-like combinator extraction
- Growing action space with compatible policy updates

### Phase 4: JS Compilation
- Hand-written tree→JS compiler for known patterns
- Property-based equivalence testing
- Neural rewrite search (synthesis engine applied to compilation)

### Phase 5: Closing the Loop
- Meta-type τ_meta (score the optimizer on its own improvement)
- Disp compiler partially written in disp
- The system proposes and validates its own extensions

---

## Reference Documents

| Document | Content |
|----------|---------|
| `ROUGH-IDEA.md` | Original brainstorm — components, iteration philosophy |
| `COC-ON-TREES-PLAN.md` | Detailed plan for CoC-on-trees (Layer 1b) — IMPLEMENTED |
| `PHASE_2.md` | Detailed research for Phase 2 — search, IPC, neural architecture |
| `category-theory-speculations-plan.md` | Categorical foundations — Q-enriched CCC, optimizer as Kan extension |
| `HIGH-LEVEL-PLAN.md` | Full roadmap through Phase 5 |
