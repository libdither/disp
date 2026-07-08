<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="predicates"><span class="secnum">Section 4</span>Types as predicates</h2>

  <h3 id="traditions">4.1 · Four traditions</h3>
  <table>
    <thead>
      <tr><th>Tradition</th><th>What T is</th><th>What v is</th><th>The relation v : T</th></tr>
    </thead>
    <tbody>
      <tr><td>Set theory</td><td>a set</td><td>an element</td><td><span class="math">v ∈ T</span> (primitive)</td></tr>
      <tr><td>Predicate logic</td><td>a formula φ(x)</td><td>an assignment</td><td>φ evaluates to true at v</td></tr>
      <tr><td>Martin-Löf type theory</td><td>a type</td><td>a term</td><td>a derivation <span class="math">Γ ⊢ v : T</span></td></tr>
      <tr><td><strong>disp</strong></td><td>a tree (a predicate)</td><td>a tree</td><td><code>T v</code> reduces to <code>Ok true</code></td></tr>
    </tbody>
  </table>
  <p>
    Disp's row is closest to the predicate-logic row, and we'll borrow Curry–Howard vocabulary
    informally throughout. The load-bearing difference from all three other rows: <code>T</code>
    and <code>v</code> live in <em>the same universe</em>. In predicate logic, formulas are
    syntactic objects and values are model-theoretic; you can't apply a formula to a value
    without an interpretation step. In disp both are trees, and applying one to the other is
    just <code>apply</code>. That is what makes the type-checker writable as a tree program
    rather than as an external interpreter.
  </p>
  <Deep title="Why this isn't 'set theory in disguise'">
    <p>
      Choosing predicates over propositions can look like discarding
      proof-relevance. It isn't, because disp's predicates are run on tree-shaped values that
      include proof terms. <code>Eq A x y</code> answers <code>Ok true</code> when the candidate
      has the shape of a reflexivity certificate and the two endpoints reduce to the same tree.
      Proof-relevance survives; it has just been relocated from the typing judgment into the
      predicate's body. What we gain is that there is no separate object called "the typing
      judgment" — there is only function application.
    </p>
  </Deep>

  <h3 id="booleans">4.2 · Booleans are shapes</h3>
  <p>
    For <code>T v = Ok true</code> to be a literal equation, the language needs booleans that
    are trees. Disp's are the two smallest distinguishable ones:
  </p>
  <p class="math-block">true = t &nbsp;·&nbsp; false = t t</p>
  <p>
    A boolean carries no eliminator of its own — it is raw data, and consuming it is a
    <em>triage</em>: the F rule reads the argument's shape (leaf or stem) and picks a branch.
    <code>if c then a else b</code> is sugar for exactly such a dispatch. Here is a hand-built
    <code>not</code>, one F-rule firing per call:
  </p>
  <Code code={samples['bool-shapes'].code} runnable context={samples['bool-shapes'].context} />
  <Aside title="A design that moved.">
    <p>
      Earlier disp used the Scott encoding — a boolean was its own eliminator, a function
      selecting one of its branches. The current design (TYPE_THEORY.typ §2.7's polarity
      discipline) keeps <em>data</em> raw and shape-encoded, and reserves function-like behavior
      for the negative types of §6. One consequence worth smiling at: <code>false</code>,
      <code>K</code>, and the successor constructor's partial application are literally the same
      tree, <code>t t</code>. Reflection means sharing.
    </p>
  </Aside>

  <h3 id="central-equation">4.3 · The central equation, running</h3>
  <Code code={samples['central-eq'].code} runnable context={samples['central-eq'].context} />
  <p>
    That is the entire type-check for a concrete value: an <code>apply</code> and a handful of
    reductions. No context, no judgment. Which sets up the real question — the one the kernel
    exists to answer. What happens when <code>v</code> is not a closed value but a variable
    bound by some surrounding lambda?
  </p>
</section>
