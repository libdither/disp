<script lang="ts">
  const entries: [string, string][] = [
    ['Tree calculus', 'An abstract rewriting system on values built from one nullary constructor (the leaf t), with binary application. The K, S, and triage rules make it Turing complete and structurally complete: every program can decide the shape of every value by reduction alone.'],
    ['Triage', 'The F rule: a function of shape t(t(c, d), b) dispatches on the shape of its argument (leaf, stem, or fork). The primitive reflection mechanism.'],
    ['Hash-consing', 'Storing each distinct tree exactly once and identifying trees by integer ID. Makes structural equality an O(1) identity check.'],
    ['Bracket abstraction', 'The algorithm [x].M that removes a free variable, leaving a closed combinator expression in I, K, S. Bridges the surface λ-syntax to tree calculus.'],
    ['Claim', 'A program {witness, challenge} => Bool. It holds when some witness passes every challenge. Types, specifications, typing rules and equations between programs are all claims.'],
    ['Witness / challenge', 'The two arguments of a claim: what is offered as evidence, and what it is tested on. A witness may be a function, which is how "for every x some y" becomes one witness.'],
    ['Honest check', 'honest: search a stream of challenges for one the witness fails, up to a budget. It can refute, or report Open; it can never prove.'],
    ['Refuted / Open / Proved', 'The three verdicts. Refuted carries the failing challenge and is final. Open means no failure was found, and proves nothing. Proved needs a finite cover or a certificate.'],
    ['Lens', 'A map between claims: witnesses go forward, challenges come back. Checkers, typing rules and optimizations are lenses. Its law, lens_holds, is itself a claim the honest check can refute.'],
    ['Rule lenses', 'The five lenses whose laws are argued by hand: finite cover, case split, generic run, induction and composition. They are the trusted base.'],
    ['Certificate', 'A finite tree built from the rule lenses that certify checks by running it. A bad certificate fails to check; nothing that produces certificates has to be trusted.'],
    ['cegis', 'Counterexample-guided synthesis: try candidates against a claim, keeping every refuting challenge to try first on the next candidate.'],
    ['Löb ceiling', 'A checker cannot prove its own soundness, so the rule lenses are argued outside disp, and a successor checker is trusted one level less than the one that admitted it.']
  ]
</script>

<section>
  <h2 id="glossary"><span class="secnum">Section 9</span>Glossary</h2>
  <dl>
    {#each entries as [term, def]}
      <dt>{term}</dt>
      <dd>{def}</dd>
    {/each}
  </dl>
  <p class="foot">
    This walkthrough is commentary. The ground truth is the code it runs:
    <code>lib/claim.disp</code>, <code>lib/tests/claim.test.disp</code> and
    <code>lib/tests/by_example.test.disp</code>, with the reasoning in
    <a href="https://github.com/libdither/disp/blob/main/research/CLAIMS_OVER_TREES.md">CLAIMS_OVER_TREES.md</a>
    and the syntax in
    <a href="https://github.com/libdither/disp/blob/main/SYNTAX.typ">SYNTAX.typ</a>.
  </p>
</section>

<style>
  dl {
    font-size: 0.94rem;
  }
  dt {
    font-weight: 650;
    color: var(--g2);
    margin-top: 0.9rem;
    font-family: var(--font-mono);
    font-size: 0.86rem;
  }
  dd {
    margin: 0.15rem 0 0 0;
    color: var(--fg-muted);
  }
  .foot {
    margin-top: 2.2rem;
    color: var(--fg-faint);
    font-style: italic;
    font-size: 0.9rem;
  }
</style>
