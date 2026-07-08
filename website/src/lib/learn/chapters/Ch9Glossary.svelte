<script lang="ts">
  const entries: [string, string][] = [
    ['Tree calculus', 'An abstract rewriting system on values built from one nullary constructor (the leaf t), with binary application. The K, S, and triage rules make it Turing complete and structurally complete: every program can decide the shape of every value by reduction alone.'],
    ['Triage', 'The F rule: a function of shape t (t c d) b dispatches on the shape of its argument (leaf, stem, or fork). The primitive reflection mechanism.'],
    ['Hash-consing', 'Storing each distinct tree exactly once and identifying trees by integer ID. Makes structural equality — and therefore conversion checking — an O(1) identity check.'],
    ['Bracket abstraction', 'The algorithm [x].M that removes a free variable, leaving a closed combinator expression in I, K, S. Bridges the surface λ-syntax to tree calculus.'],
    ['Types-as-predicates', 'The design principle: a type is a tree program, and v : T iff T v = Ok true.'],
    ['CheckerResult', 'The verdict coproduct Ok | Err. Failure is a value, not an exception in a meta-language.'],
    ['Hypothesis / neutral', 'A kernel-minted tree of shape wait (hyp_reduce) (meta), standing for "an unknown value of stored type T". How disp checks under binders. Unforgeable and uninspectable by construction.'],
    ['Signature', 'The identity of the function at a tree’s head, read by the sanctioned pair_fst projection. The kernel’s routing key.'],
    ['Σ (the routing table)', 'The fixed map from pinned signatures to registered handlers. param_apply routes a pinned signature to the REGISTERED handler, ignoring anything embedded in the value — so carrying a kernel signature grants no privilege.'],
    ['param_apply / the walker', 'The dispatcher and its policed reduction mode: the five rules minus reflective moves on hypotheses (no triage on a neutral, no forging kernel-signed forks), with reader carve-outs pair_fst, neutral_type, I, tree_eq.'],
    ['bind_hyp', 'Kernel operation #1: mint a fresh hypothesis, run the body under the walker, then escape-check that the hypothesis does not leak out of the result.'],
    ['hyp_reduce', 'Kernel operation #2: the single gateway for observing a hypothesis — reads the stored type and forwards the observation to that type’s respond.'],
    ['respond', 'The metadata field through which a type answers observations on its hypotheses. Constitutive: every type carries one, and the universe checks it behaviorally.'],
    ['Action', 'The two-armed answer of a respond: Extend type (stuck, at that type) or Reduce value (computes through). The canonical normalization-by-evaluation dichotomy.'],
    ['Spine', 'A neutral’s payload: Mint id, or Ext parent frame — the log of observations so far. Typed, so the kernel can reason about it; still unreadable by user code.'],
    ['H-rule', 'If the candidate is a hypothesis, compare its stored type against the required type by hash-cons identity instead of running the recognizer.'],
    ['Escape check', 'bind_hyp’s final gate: if the minted hypothesis is reachable in the result via an extractable path, the binder rejects. Prevents hypothetical values from leaking into closed proofs.'],
    ['Telescope', 'The one negative former: a dependent chain of cells. Pi, Sigma, records, and the trivial type are instances differing only in cell kind.'],
    ['Cell', 'One obligation in a telescope — itself a wait-form (inspectable AND runnable). Kinds include mint (bind a ∀-hypothesis), proj (a named field), apply (a codomain check), derive (a computed field), and Rec (a recursive position).'],
    ['Coproduct', 'The positive former: a tagged sum of per-variant telescopes. Bool, Nat, and Ord are instances (over raw shape encodings, via a view isomorphism).'],
    ['Coherence gate', 'The η-readback check (coh_check) every inductive respond runs on its case record before issuing a stuck elimination — the respond cannot wave junk through.'],
    ['Neutral evaluation', 'Running a program on a promise: eliminators park as stuck eliminations typed by their motives, and checking proceeds on the types of stuck things.'],
    ['Path (cubical, proposed)', 'A function from the interval into a type; equality with computational content. Univalence becomes a theorem via Glue.'],
    ['Verdict / verify_good', 'The behavioral universe check: probe a type’s respond with minted hypotheses and compare answers against its constructors. Catches responds that lie in either direction.']
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
    This walkthrough is pedagogical commentary. The repository's
    <a href="https://github.com/libdither/disp/blob/main/TYPE_THEORY.typ">TYPE_THEORY.typ</a>,
    <a href="https://github.com/libdither/disp/blob/main/SYNTAX.typ">SYNTAX.typ</a>, and the
    <code>lib/kernel/*.disp</code> sources are the ground truth — and unlike most language
    documentation, the kernel sources are the theory.
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
