<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Gloss from '../Gloss.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="lenses"><span class="secnum">Section 5</span>Checkers are lenses</h2>
  <p>
    Search can refute but never prove, because the stream of trees never ends. Every checker
    that says Proved is a shortcut: it replaces "every tree" with something finite. This section
    is about how disp keeps shortcuts honest, and what is left that you have to trust.
  </p>

  <h3 id="finite">5.1 · Proved needs a finite list</h3>
  <p>
    <code>over</code> is the honest check run on a list instead of a stream. When the list runs
    out without a failure, the answer is Proved. That is only sound if the list really covers
    every challenge that matters, and saying so is a claim of its own.
  </p>

  <h3 id="lens">5.2 · Lenses and their law</h3>
  <p>
    A <strong>lens</strong> between two claims carries witnesses forward and challenges back:
    <code>lens(fwd, back)</code>. A checker, a typing rule and an optimization are all lenses.
    Its law, <code>lens_holds</code>, says: a witness that passes the pulled-back challenge is
    carried to a witness that passes the original. Read backwards, every counterexample to the
    target claim pulls back to a counterexample to the source. The law is itself a claim, so the
    honest check can refute a wrong shortcut with a concrete tree:
  </p>
  <Code code={samples['proved'].code} runnable context={samples['proved'].context} />
  <p>
    <code>bool_cover</code> sends each boolean challenge to itself, so passing the two booleans
    really does cover the claim. <code>bad_cover</code> sends everything to <code>true</code>,
    and search finds the tree it misses: <code>false</code>, where <code>half_bad</code> fails.
    Lenses compose (<code>lens_then</code>): forward maps run in order, backward maps in
    reverse, so a checker built from small certified steps is certified too.
  </p>
  <Deep title="Where lenses come from">
    <p>
      This is the morphism of de Paiva's Dialectica categories: a map of witnesses one way and of
      challenges the other, with a law tying them together. The same shape shows up as the
      backward pass of a learner, where goods go forward and prices come back, and as
      counterexample-guided synthesis, where refuting challenges are kept and reused (§6.4).
    </p>
  </Deep>

  <h3 id="rules">5.3 · The five rules you trust</h3>
  <p>
    A lens law is a claim over every tree, so search alone can't prove one either. Something has
    to be argued by hand, and in disp that is five
    <Gloss tip="lenses whose laws are argued on paper once, so every certificate built from them is trusted">rule lenses</Gloss>:
  </p>
  <ul>
    <li><strong>Finite cover</strong>: <code>over</code> exhausts a list.</li>
    <li><strong>Case split</strong>: a type's cases cover all its members (the confirmer's <code>table</code>).</li>
    <li><strong>Generic run</strong>: running a program on a symbolic case covers every concrete member of that case.</li>
    <li><strong>Induction</strong>: a claim that holds at zero, and holds at n+1 whenever it holds at n, holds everywhere.</li>
    <li><strong>Composition</strong>: <code>lens_then</code> of two lawful lenses is lawful.</li>
  </ul>
  <p>
    A <strong>certificate</strong> is a finite tree built from these rules, and
    <code>certify</code> checks one by running it. Everything else, including every clever
    optimizer that produces certificates, is untrusted: a bad certificate fails to check, and a
    refutation re-runs anywhere.
  </p>

  <h3 id="floor">5.4 · Why the floor can't go lower</h3>
  <p>
    Why not prove the five rules inside disp, too? Because a checker can't prove its own
    soundness. This is Löb's theorem, and in disp it has a concrete form: turning "this run
    succeeds" into the success itself would solve the halting problem, so no checker can cash in
    a promise about its own runs. Open is the honest record that no such procedure exists. The
    five hand-argued rules are the smallest base that can be trusted, and a successor checker
    can only be trusted one level down from the one that admitted it
    (<code>research/LOB_AND_CURRY.md</code>).
  </p>

  <h3 id="today">5.5 · What runs today</h3>
  <p>
    The claim layer lives in <code>lib/claim.disp</code>, with the rule lenses and
    <code>certify</code> in <code>lib/tests/claim.test.disp</code>. The checker that still verifies
    <code>:</code> annotations when a module is loaded is <code>lib/kernel/</code>, an earlier
    design the claim layer is replacing. If you meet it in the library, that is why.
  </p>
  <Aside kind="warn" title="Not yet proved.">
    <p>
      The five rules rest on short arguments on paper (the run lemma for generic runs, a
      decreasing measure for induction), not on a written proof a machine has checked. That is
      the project's trusted base, and it is still a promise.
    </p>
  </Aside>
</section>
