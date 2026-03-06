# ROUGH-IDEA: Tree Calculus Interactive Environment

## What We're Building

A minimal interactive environment where you can:
1. Write code in disp-like syntax
2. Compile it down to pure tree calculus
3. Execute/step through evaluation
4. See results pretty-printed back as readable code

The long-term vision: this becomes a self-hosting system where the compiler is itself written in disp, compiled via e-graphs to efficient JS, and runs in the browser.

---

## Components to Implement

### 1. Tree Calculus Evaluator

**What it does**: Takes a tree, applies reduction rules, returns the result.

**Key considerations**:
- Reduction is simple (3 triage rules + K/S behavior) but needs to handle deeply nested structures
- Need both "step once" and "run to completion" modes
- Must handle non-termination gracefully (step limit or lazy evaluation)
- Leftmost-outermost vs innermost reduction strategy matters for some programs

**Open questions**:
- Should we use an immutable tree structure (functional style, good for step history) or mutable (faster)?
- How to efficiently detect when no more reductions are possible?

### 2. Parser

**What it does**: Turns disp text → AST → tree calculus

**Strategy reflections**:
- Could use hand-written recursive descent (fast iteration, full control)
- Could use parser combinator library (more declarative, but adds dependency)
- For browser, want minimal bundle size—hand-written is probably better
- Need good error messages with source locations

**Hard parts**:
- **Named arguments**: `f {x := 1, y := 2}` needs to figure out argument ordering
- **Partial application**: When not all args provided, generate a closure
- **Abstraction elimination**: Converting λ-terms to pure combinators is non-trivial
  - Classic bracket abstraction produces huge output (exponential blowup risk)
  - Optimized algorithms exist (Turner's, Hughes') but add complexity
  - Bind-trees from dither-spec might avoid some issues but are less standard

**Syntax scope decision**:
- Start with just λ-calculus: `{x} -> body`, `f arg`, `let name := expr in body`
- Add records/coproducts later once core works
- Products `{a, b, c}` and coproducts `<A, B, C>` are syntactic sugar over functions

### 3. Pretty Printer

**What it does**: Turns tree calculus back into readable notation

**Problems to solve**:
- Tree calculus is "lossy"—variable names are gone after abstraction elimination
- Need to recognize common patterns (K, S, I, common data encodings)
- Heuristics for when to insert parentheses vs. use application precedence
- How do we preserve/reconstruct names?

**Approaches**:
1. **Don't try**: Just show the raw tree structure, let user learn to read it
2. **Pattern matching**: Recognize `△ △` as "K", etc., build up vocabulary
3. **Name tags**: Encode names *inside* trees as data, preserve through computation
4. **External annotations**: Keep a separate map of tree-hash → source-name

**Recommendation**: Start with approach 1+2, explore 3 later since it's philosophically interesting (fully reflective).

### 4. Browser UI

**What it might look like**:
- Left panel: code editor (Monaco or CodeMirror, or even textarea to start)
- Right panel: output/tree visualization
- Bottom: REPL for quick expressions
- Controls: Run, Step, Reset buttons

**Visualization ideas**:
- Show tree as actual tree diagram (nodes and edges)
- Highlight current redex during stepping
- Animate reduction (old structure fades, new appears)
- Maybe: side-by-side source ↔ tree view

**Iteration speed considerations**:
- Start with the absolute minimum: textarea + div for output
- Hot module reloading with Vite means fast feedback
- Save UI polish for later—get the semantics right first
- Consider: could the UI itself eventually be written in disp?

---

## Implementation Order

### Phase 1: Core Logic (TypeScript, no UI)
- Tree type definition
- Reduction rules
- Step/run functions
- Tests with known combinators (K, S, I, omega)

### Phase 2: Minimal Parser
- Tokenizer
- Expression parser (just λ and application)
- Abstraction elimination (naive version first, optimize if needed)

### Phase 3: Text Interface
- Simple REPL (node.js based)
- Parse → evaluate → print loop
- `:step` and `:tree` commands

### Phase 4: Browser
- Bundle with Vite
- Minimal HTML/CSS interface
- Hook up editor → evaluator → output

### Phase 5: Polish & Extend
- Better pretty printing
- Full disp syntax (records, coproducts)
- Tree visualization
- Step animation

---

## Iteration Speed Philosophy

The goal is **tight feedback loops**:

1. **Hot reload everything**: Vite + TypeScript means changes appear instantly
2. **Tests as documentation**: Small unit tests show how things work
3. **REPL-first development**: Build the REPL early, use it to test ideas
4. **Deferred complexity**: Don't add features until you need them
5. **Browser as playground**: Interactive experimentation beats printf debugging

Eventually, if we can get disp compiling to JS via e-graphs, we could:
- Write new language features *in* disp
- Compile them instantly
- Hot-reload the compiler itself
- Achieve true linguistic abstraction (language features are just libraries)

---

## Things I'm Uncertain About

1. **Bind-trees vs De Bruijn vs bracket abstraction**: Which approach is best for our goals? Bind-trees are novel but less documented. De Bruijn is standard but requires index arithmetic. Bracket abstraction produces pure trees but can explode in size.

2. **Name preservation**: Is it important that names survive computation? If yes, encoding them in-tree is the pure approach, but it means programs get bigger and we need conventions for what operations preserve vs. strip names.

3. **E-graph integration point**: When do we bring in e-graphs? They're for optimization, but could also unify "same meaning, different representation" trees. This might be Phase 6+.

4. **Self-hosting timeline**: How aggressively should we push toward writing the compiler in disp? Too early and we're blocked on missing features. Too late and we've invested too much in TypeScript tooling.

---

## References

- [treecalcul.us](https://treecalcul.us) — interactive demos, good for intuition
- [dither-spec/disp/monodoc.md](./dither-spec/disp/monodoc.md) — syntax design
- [dither-spec/disp/bind-trees.md](./dither-spec/disp/bind-trees.md) — alternative to De Bruijn
- Barry Jay, "Reflective Programs in Tree Calculus" — the academic foundation
