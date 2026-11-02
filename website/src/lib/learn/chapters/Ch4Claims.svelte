<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="claims"><span class="secnum">Section 4</span>Claims: a type is a test</h2>
  <p>
    Section 1 said a type is a program you run. This section makes that exact. In disp a type,
    a specification, a typing rule's conclusion and an equation between programs are all the
    same kind of thing: a <strong>claim</strong>, a program that takes a <em>witness</em> and a
    <em>challenge</em> and answers <code>true</code> or <code>false</code>. The claim holds when
    some witness passes every challenge. Nothing else is built in.
  </p>

  <h3 id="traditions">4.1 · Four traditions</h3>
  <table>
    <thead>
      <tr><th>Tradition</th><th>What T is</th><th>What v is</th><th>The relation v : T</th></tr>
    </thead>
    <tbody>
      <tr><td>Set theory</td><td>a set</td><td>an element</td><td><span class="math">v ∈ T</span> (primitive)</td></tr>
      <tr><td>Predicate logic</td><td>a formula φ(x)</td><td>an assignment</td><td>φ evaluates to true at v</td></tr>
      <tr><td>Martin-Löf type theory</td><td>a type</td><td>a term</td><td>a derivation <span class="math">Γ ⊢ v : T</span></td></tr>
      <tr><td><strong>disp</strong></td><td>a claim: a program <code>&#123;witness, challenge&#125; => Bool</code></td><td>a witness (any tree)</td><td>no challenge makes <code>T(v, c)</code> false</td></tr>
    </tbody>
  </table>
  <p>
    Disp's row is closest to predicate logic, with one difference that carries everything else:
    the claim, the witness and the challenge are all trees. A claim is not a formula that needs
    an interpretation before it can be applied; it <em>is</em> the function you apply. Its
    traditional name is <em>types as predicates</em>, and its nearest ancestor is NuPRL, whose
    types are also predicates over untyped terms. The difference is that NuPRL's terms are
    λ-terms you can only observe by running them, while disp's are trees a claim can take apart
    (§2.3).
  </p>
  <Deep title="Why a witness and a challenge, not just a value">
    <p>
      "For every x there is a y" is the common shape of a specification, and searching for the y
      separately at every x is hopeless. Allowing the witness to be a function fixes that: "for
      every x some y" becomes "some function f, for every x". Programs are trees, so a function
      is as good a witness as any other tree. This is Gödel's Dialectica reading, and it is why
      one fixed shape, <code>&#123;witness, challenge&#125; => Bool</code>, is enough for every
      claim in this walkthrough (<code>research/CLAIMS_OVER_TREES.md</code> §9).
    </p>
  </Deep>

  <h3 id="booleans">4.2 · Booleans are shapes</h3>
  <p>
    For a claim's answer to be a tree, the language needs booleans that are trees. Disp's are
    the two smallest distinguishable ones:
  </p>
  <p class="math-block">true = t &nbsp;·&nbsp; false = t(t)</p>
  <p>
    A boolean carries no eliminator of its own. It is raw data, and using it is a
    <em>triage</em>: the F rule reads the argument's shape (leaf or stem) and picks a branch.
    <code>if (c) &#123; a &#125; else &#123; b &#125;</code> is sugar for exactly that. Here is
    a hand-built <code>not</code>, one F-rule firing per call:
  </p>
  <Code code={samples['bool-shapes'].code} runnable context={samples['bool-shapes'].context} />

  <h3 id="honest">4.3 · Asking every tree</h3>
  <p>
    A claim talks about every tree, and there are infinitely many. Disp lists them anyway:
    <code>Trees</code> is a stream of every tree, smallest first. The <strong>honest</strong>
    check walks that stream looking for a challenge the witness fails, up to a budget of steps.
    It has three possible answers, and only two of them can come from search:
  </p>
  <ul>
    <li><strong>Refuted</strong>, with the challenge that failed. Final: anyone can re-run it.</li>
    <li><strong>Open</strong>: no failure within the budget. This proves nothing.</li>
    <li><strong>Proved</strong>: needs a shortcut that covers every tree at once (§5).</li>
  </ul>
  <Code code={samples['claim'].code} runnable context={samples['claim'].context} />
  <p>
    <code>sneaky</code> is the reason Open is not a verdict: it behaves until the 65th tree. A
    larger budget can turn Open into Refuted, never into Proved. Note also what the claim leaves
    out: <code>nat_fn</code> answers <code>true</code> on every challenge that is not a number,
    so it says nothing about them. That is how a claim restricts itself to a type.
  </p>
  <Aside title="A design that moved.">
    <p>
      Earlier versions of disp had a type <code>T</code> return a verdict when applied to a
      value, and a two-operation kernel that minted opaque placeholder values to check
      functions. Both are retired (<code>archive/live-kernel/</code> keeps them). Claims replace
      the placeholders with challenges: instead of running a function on "some unknown number",
      the check runs it on actual numbers, and Section 5 is how it gets from there to "every
      number".
    </p>
  </Aside>
</section>
