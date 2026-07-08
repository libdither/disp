<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Gloss from '../Gloss.svelte'
  import TreeVis from '$lib/components/TreeVis.svelte'

  const labPresets = [
    { expr: 'not false', tip: 'Booleans are the two smallest trees (true = t, false = t t); not is one triage: the F rule reads the argument\'s shape and picks a branch.' },
    { expr: 'K x y', tip: 'Any name that isn\'t a known definition parses as a named leaf — a free variable, drawn as a fruit. K keeps x and discards y; watch y drop.' },
    { expr: 'S f g x', tip: 'The S rule on paper: x is shared between f and g, giving (f x)(g x). Free variables have no shape, so the result is stuck — a symbolic normal form.' },
    { expr: 't (t a b) c (t t)', tip: 'The F rule in the raw: the argument t t is a stem, so it flies to branch b and hands it the stem\'s child; the a and c branches are pruned.' },
    { expr: 'S K K (t t)', tip: 'S K K is the identity, assembled from S and K: the S rule duplicates the argument, then two K rules collapse it back out.' },
    { expr: 'S K K (S K K (t t))', tip: 'Two identities, nested. Try the parallel toggle here: independent redexes fire in the same round, and confluence promises the same answer.' }
  ]
</script>

<section>
  <h2 id="trees"><span class="secnum">Section 2</span>From λ-calculus to trees</h2>
  <p>
    The untyped λ-calculus has terms <span class="math">M, N ::= x | λx. M | M N</span> and one
    rule, β-reduction: <span class="math">(λx. M) N →<sub>β</sub> M[x ≔ N]</span>. Its key
    limitation, for our purposes: λ-terms cannot inspect their own structure by reduction. Given
    a λ-term <span class="math">f</span>, there is no β-reducible context in which we can ask
    "is <span class="math">f</span> a function or an atom?". Everything is a function.
  </p>

  <h3 id="ski">2.1 · The SKI detour</h3>
  <p>
    Curry and Schönfinkel showed bound variables are dispensable: three combinators suffice.
  </p>
  <table>
    <thead><tr><th>Combinator</th><th>Definition</th><th>Behaviour</th></tr></thead>
    <tbody>
      <tr><td><code>I</code></td><td><span class="math">λx. x</span></td><td><code>I x = x</code></td></tr>
      <tr><td><code>K</code></td><td><span class="math">λx. λy. x</span></td><td><code>K x y = x</code></td></tr>
      <tr><td><code>S</code></td><td><span class="math">λx. λy. λz. (x z)(y z)</span></td><td><code>S x y z = (x z)(y z)</code></td></tr>
    </tbody>
  </table>
  <p>
    SKI is combinatorially complete — every closed λ-term compiles to it. But SKI inherits the
    same limitation from λ-calculus: there is no reducible operation that asks "is this an
    <code>S</code> or a <code>K</code>?"
  </p>

  <h3 id="tree-calculus">2.2 · Tree calculus</h3>
  <p>
    Tree calculus, due to Barry Jay, keeps combinatorial completeness and adds exactly the
    missing power. There is one base piece: <code>t</code> (written △ in the literature — the
    "leaf"). Trees are assembled by application, and every tree in
    <Gloss tip="in tree calculus, every tree is already in normal form — reduction only happens through apply">normal form</Gloss>
    is one of:
  </p>
  <p class="math-block">t (leaf) &nbsp;·&nbsp; t u (stem) &nbsp;·&nbsp; t u v (fork)</p>
  <p>
    Application is not a syntactic constructor; it's an external operation
    <span class="math">apply : Tree × Tree → Tree</span>. Trees by themselves don't reduce;
    reduction happens only through <code>apply</code>.
  </p>
  <Deep title="The host-side Tree type">
    <p>
      In the TypeScript runtime, trees are a discriminated union with an integer <code>id</code>
      for hash-consing (<code>src/core/tree.ts</code>):
    </p>
    <pre>{`type Tree =
  | { tag: "leaf"; id: number }
  | { tag: "stem"; child: Tree; id: number }
  | { tag: "fork"; left: Tree; right: Tree; id: number }`}</pre>
  </Deep>

  <h3 id="five-rules">2.3 · The five rules</h3>
  <p>
    <code>apply(f, x)</code> is defined by five rules that partition on the shape of
    <code>f</code> — and, for the last, on the shape of <code>x</code>:
  </p>
  <table>
    <thead><tr><th>Pattern of f</th><th>Shape of x</th><th>Result</th><th>Name</th></tr></thead>
    <tbody>
      <tr><td><code>t</code></td><td>any</td><td><code>t x</code></td><td>L (build a stem)</td></tr>
      <tr><td><code>t u</code></td><td>any</td><td><code>t u x</code></td><td>s (build a fork)</td></tr>
      <tr><td><code>t t b</code></td><td>any</td><td><code>b</code></td><td>K (constant)</td></tr>
      <tr><td><code>t (t c) b</code></td><td>any</td><td><code>(c x)(b x)</code></td><td>S (composition)</td></tr>
      <tr><td><code>t (t c d) b</code></td><td><code>t</code></td><td><code>c</code></td><td rowspan="3">F (triage on x)</td></tr>
      <tr><td></td><td><code>t u</code></td><td><code>d u</code></td></tr>
      <tr><td></td><td><code>t u v</code></td><td><code>b u v</code></td></tr>
    </tbody>
  </table>
  <p>
    The first two rules are passive — they just grow trees. K and S match their SKI
    counterparts. The fifth rule — F, the
    <Gloss tip="the third reduction rule: a function of shape t (t c d) b dispatches on the shape of its argument (leaf, stem, or fork) — the primitive reflection mechanism">triage</Gloss>
    — is the new ingredient: it is the only rule whose behavior depends on the shape of the
    <em>argument</em>. Reduction itself can inspect tree shape.
  </p>
  <Aside title="Why this matters.">
    <p>
      In λ-calculus there is no β-reducible predicate "x is a variable". In tree calculus we can
      write <code>is_leaf</code> as a closed tree program and reduce it against any value. This
      property is called <strong>structural completeness</strong>: every program can decide the
      structure of every other program by reduction alone. It's the property that makes a
      self-hosting type checker possible at all.
    </p>
  </Aside>
  <p>
    The lab below evaluates any expression you type. Names that aren't known definitions parse
    as <em>named leaves</em> — free variables with no shape of their own, drawn as fruit — so
    <code>K x y</code> and <code>S f g x</code> reduce exactly as they do on paper. Press
    <em>Step</em> to fire the next reduction and watch which rule carries it: the amber letter
    marks the next
    <Gloss tip="a reducible expression: an application whose function (and, for triage, argument) is in the right shape for a rule to fire">redex</Gloss>,
    and pruned leaves fall where all pruned leaves go. Click the grove and drive it from the
    keyboard — space runs and pauses, ←/→ step back and forward, shift+←/→ hop between the
    examples. The toggles are worth a poke:
    <em>parallel</em> fires every ready redex per round, which is legal because tree calculus is
    confluent, and <em>nature</em> off turns the grove back into a plain diagram.
  </p>
  <TreeVis variant="lab" presets={labPresets} exprs={[labPresets[0].expr]} height={330} />

  <h3 id="bracket">2.4 · Bracket abstraction: compiling λ to trees</h3>
  <p>
    Surface disp has binders; trees don't. The bridge is
    <em>bracket abstraction</em>: <span class="math">[x].M</span> ("abstract x out of M")
    produces a closed expression in <code>I</code>, <code>K</code>, <code>S</code> and
    application:
  </p>
  <table>
    <thead><tr><th>When M is…</th><th>Then [x].M =</th><th>Why</th></tr></thead>
    <tbody>
      <tr><td>x not free in M</td><td><code>K M</code></td><td><code>K M v = M</code></td></tr>
      <tr><td><code>x</code> itself</td><td><code>I</code></td><td><code>I v = v</code></td></tr>
      <tr><td><code>N P</code></td><td><code>S ([x].N) ([x].P)</code></td><td>substitute in both halves</td></tr>
    </tbody>
  </table>
  <p>
    Nested binders peel inside-out: <span class="math">[x].[y].y = [x].I = K I</span>. The
    compiler adds two semantics-preserving optimizations —
    <Gloss tip="the identity λx.(f x) = f when x is not free in f; the application and the bound variable cancel">η-reduction</Gloss>
    (<span class="math">[x].(N x) = N</span> when x ∉ N) and K-composition
    (<span class="math">S (K p) (K q) → K (p q)</span>) — which substantially shrink the output.
    These live in <code>src/elab/cir.ts</code> and are part of definitional equality: once
    compilation finishes, there is nothing in the runtime that is not a tree.
  </p>

  <h3 id="hash-consing">2.5 · Hash-consing and O(1) equality</h3>
  <p>
    The runtime stores every distinct tree exactly once
    (<Gloss tip="structural sharing: identical subtrees share one heap cell, so structural equality collapses to integer-identity comparison">hash-consing</Gloss>),
    so structural equality reduces to an identity check:
  </p>
  <pre>{`export function treeEqual(a, b) {
  return a.id === b.id   // O(1) — no traversal
}`}</pre>
  <p>
    Why does this matter so much? Because <em>conversion checking</em> in a dependent type
    system is structural equality of normal forms — the hot loop of every proof assistant.
    Making conversion O(1) keeps type-checking cheap. It also makes the kernel's H-rule (§5)
    tractable: "the stored type of this hypothesis equals the required type" is decided by
    hashed identity, because deterministic elaboration produces identical trees for identical
    types.
  </p>
</section>
