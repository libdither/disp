<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Gloss from '../Gloss.svelte'
  import Code from '../Code.svelte'
  import TraceWidget from '../TraceWidget.svelte'
  import { samples } from '../code-samples'

  const traceFrames = [
    {
      title: 'Goal.',
      html: 'The annotation <code>quadruple : Nat -> Nat</code> compiles to the check <code>param_apply (Pi Nat ({_} -> Nat)) quadruple</code>, expected verdict <code>Ok true</code>.'
    },
    {
      title: 'Route.',
      html: '<code>param_apply</code> reads the head signature of <code>Pi Nat …</code>: an ordinary wait-form type, not a Σ member — so its recognizer runs under the walker, with no raw privileges.'
    },
    {
      title: 'Cells.',
      html: '<code>Pi Nat B</code> is the two-cell telescope <code>[mint x : Nat ; apply out : B x]</code>. The telescope walker starts on the first cell.'
    },
    {
      title: 'Mint.',
      html: 'The <code>mint</code> cell requests a fresh hypothesis: <code>bind_hyp Nat ({h} -> …)</code> mints <code>h</code> — an unforgeable tree carrying "type Nat, id 0" and nothing else. The rest of the check runs under this binder.'
    },
    {
      title: 'Observe.',
      html: 'The <code>apply</code> cell observes <code>quadruple h</code>. The body runs: <code>double (double h)</code> — and <code>double</code> needs digits <code>h</code> does not have.'
    },
    {
      title: 'Stuck, safely.',
      html: '<code>double</code>&#8217;s recursor reaches the promise and parks: <code>hyp_reduce</code> reads <code>h</code>&#8217;s stored type and forwards the elimination to Nat&#8217;s <code>respond</code>, which answers <code>Extend Nat</code> — a stuck elimination <em>typed</em> Nat. Neutral evaluation, twice over.'
    },
    {
      title: 'Codomain.',
      html: 'The cell checks the observed value against <code>B h = Nat</code>: the stuck value is a neutral whose stored type IS Nat, so the H-rule answers by hash-cons identity — <code>Ok true</code>, no recognizer run.'
    },
    {
      title: 'Escape check.',
      html: '<code>bind_hyp</code> verifies <code>h</code> does not leak out of the verdict via any extractable path. <code>Ok true</code> contains no <code>h</code>.'
    },
    {
      title: 'Verdict.',
      html: '<code>Ok true</code> — the definition is accepted, for every Nat at once. ∎'
    }
  ]
</script>

<section>
  <h2 id="library"><span class="secnum">Section 6</span>Library types</h2>
  <p>
    <code>Pi</code>, <code>Sigma</code>, records, <code>Bool</code>, <code>Nat</code>,
    <code>Eq</code>, <code>Type</code>: none of these are kernel primitives. Each is a
    <Gloss tip="a type is wait(checker)(metadata): a deferred application whose head identifies the former and whose metadata record carries recognizer_params, respond, functor, behavioral_specs">wait-form</Gloss>
    built from cells, a metadata record, and a <code>respond</code> — and adding a new type
    former is library work, not kernel surgery. Two constructions cover nearly everything.
  </p>

  <h3 id="telescope">6.1 · The telescope: the one negative former</h3>
  <p>
    A <em>telescope</em> is a dependent chain of
    <Gloss tip="a cell is itself a wait-form `wait op meta` — inspectable (its signature says which observation) and runnable; the op is the cell's observation: mint a binder, project a field, check an application, pin a derived value">cells</Gloss>,
    each binding a value the later cells can mention. It is the single former behind functions,
    pairs, records, and the trivial type:
  </p>
  <table>
    <thead><tr><th>Type</th><th>As cells</th></tr></thead>
    <tbody>
      <tr><td><code>Pi A B</code></td><td><code>[mint x : A ; apply out : B x]</code></td></tr>
      <tr><td><code>&#123; a : Nat, b : Nat &#125;</code></td><td><code>[proj "a" : Nat ; proj "b" : Nat]</code></td></tr>
      <tr><td><code>Sigma A B</code></td><td><code>[proj "fst" : A ; proj "snd" : B fst]</code></td></tr>
      <tr><td><code>⊤</code> (trivial)</td><td><code>[]</code> — the empty list of obligations</td></tr>
    </tbody>
  </table>
  <Code code={samples['telescope-literal'].code} runnable context={samples['telescope-literal'].context} />
  <p>
    One walker serves every telescope, in two modes: <em>recognition</em> (walk the cells,
    check a candidate satisfies each observation) and <em>response</em> (drive a stuck
    elimination — this is what a telescope type's <code>respond</code> does when
    <code>hyp_reduce</code> forwards an observation). Because both faces read the same cells,
    the recognizer and the respond cannot disagree about what a field is. Derived fields show
    the payoff — the recipe is <em>data</em>, so checking runs it:
  </p>
  <Code code={samples['record-check'].code} runnable context={samples['record-check'].context} />

  <h3 id="coproduct">6.2 · Sums and recursion</h3>
  <p>
    The telescope's dual is <code>Coproduct</code> — a sum of per-variant telescopes, each
    tagged. Recursion is not a special former: a recursive constructor argument is one more cell
    kind (<code>Rec</code>), and the coproduct ties its own knot. From the cells alone, the
    library derives the recursor, the fold, and the functorial map — one implementation for
    every inductive, read off the type's structure.
  </p>
  <Code code={samples['coproduct-sample'].code} runnable context={samples['coproduct-sample'].context} />
  <Deep title="The coherence gate">
    <p>
      An inductive's <code>respond</code> claims to answer eliminations for hypotheses of the
      type. What keeps that claim honest? Every coproduct's respond is <em>gated</em>: an
      η-readback check (<code>coh_check</code>) verifies the case record it is handed actually
      covers the declared variants at the declared types, before any stuck elimination is
      issued. <code>Bool</code>, <code>Nat</code>, and <code>Ord</code> are ordinary gated
      coproducts — with a twist: their runtime values are the raw shape encodings of §4.2, and
      a <em>view isomorphism</em> in the metadata maps shapes to tagged variants. Data stays
      cheap; structure stays honest.
    </p>
  </Deep>

  <h3 id="equality">6.3 · Propositional equality</h3>
  <p>
    An equality type has exactly one proof value, and it is the leaf itself. Whether
    <code>refl</code> proves an equation is decided by <em>running both sides</em> — conversion
    is hash-cons identity (§2.5), so the check is cheap:
  </p>
  <Code code={samples['eq-sample'].code} runnable context={samples['eq-sample'].context} />
  <Aside title="For the mathematician.">
    <p>
      Propositional equality here is definitional: <code>Eq A x y</code> is inhabited iff x and
      y are convertible. The J eliminator and the usual lemmas (<code>eq_subst</code>,
      <code>eq_sym</code>, <code>eq_cong</code>, <code>eq_trans</code>) are library code in
      <code>kernel/base.disp</code>. Beyond it, the standard library adds <em>observational
      equivalence</em> (<code>std/oeq.disp</code>) as a type's fourth face — pointwise at
      functions (function extensionality by definition), componentwise at records — which is the
      equality the planned optimizer licenses rewrites with. The cubical path types of §7.2
      would replace both with something stronger.
    </p>
  </Aside>

  <h3 id="universe">6.4 · The universe checks itself</h3>
  <p>
    There is a single universe, and no stratification — no
    <span class="math">Type 0 : Type 1 : Type 2</span> tower. Membership in <code>Type</code> is
    a predicate like membership anywhere else, in two layers. The <em>structural</em> layer
    recognizes a well-formed wait-form type carrying MetaShape-conforming metadata. The
    <em>behavioral</em> layer aims the §5 promise machinery at the type itself: probes mint
    hypotheses and fire observations through <code>hyp_reduce</code> at the candidate's
    <code>respond</code>, comparing answers against the type's own constructors. A respond can
    lie in two directions — wave junk through, or refuse everything — and both are caught:
  </p>
  <Code code={samples['universe-sample'].code} runnable context={samples['universe-sample'].context} />
  <Code code={samples['verify-good'].code} runnable context={samples['verify-good'].context} />
  <p>
    So the checker checks the checkers, with the same two kernel operations it uses on
    everything else — the "self-verified" in disp's first sentence. As for Girard-style
    paradoxes: a Russell definition like <code>R := &#123;T&#125; -> not (T T)</code> is
    syntactically legal and simply diverges when run; the reduction budget reports failure, and
    failure proves nothing. Soundness by divergence-as-failure, not by hierarchy.
  </p>
  <TraceWidget
    title="Trace: checking quadruple : Nat -> Nat"
    intro="The whole machine, end to end — dispatcher, telescope cells, both kernel operations, the H-rule, and the escape check. (Schematic; lib/kernel/engine.disp is ground truth.)"
    frames={traceFrames}
  />
</section>
