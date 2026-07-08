<script lang="ts">
  import Aside from '../Aside.svelte'
</script>

<section>
  <h2 id="future"><span class="secnum">Section 7</span>Future directions</h2>

  <h3 id="landed">7.1 · What has landed</h3>
  <p>
    The original version of this walkthrough (mid-2026) closed with a roadmap; an unusual amount
    of it has since shipped. The seven-primitive kernel became the two-operation kernel of §5.
    Telescopes and cells unified the negative formers; the coproduct + coherence-gate story
    covered the positives. The universe grew its behavioral layer
    (<code>Type := BehavioralType</code>) — the §6.4 self-check is landed code, not a plan.
    Modules became hermetic functors with explicit givens; declarations became guard-mediated
    requests (a name can be owned by a policy, and rebinding it demands a machine-checked
    license — the seed of verified optimization). Five evaluator backends sit behind one
    session ABI and must agree byte-for-byte, including the Rust engine this website runs in
    your tab. And the first end-to-end optimizer slice exists: equality witnesses licensing real
    rewrites (map fusion among them) past syntactic equality, with zero kernel changes.
  </p>
  <p>Still design-stage: effects as a free-monad library with one impure driver at the program
    boundary (TYPE_THEORY.typ §15), cost as a typing-level resource, and the items below.</p>

  <h3 id="cubical">7.2 · Cubical type theory</h3>
  <p>
    Path-based equality — the source of computational univalence — appears implementable as
    library code over the existing kernel (TYPE_THEORY.typ §13). The interval
    <span class="math">I</span> is a library type; a path is a function from it; and the
    walker's parametricity is exactly the discipline paths require (one cannot triage on an
    abstract <span class="math">i : I</span>):
  </p>
  <pre>{`Path A x y  :=  Pi I ({_} -> A)

refl   := {A, x, i} -> x
sym    := {A, p, i} -> p (I_inv i)
funext := {A, B, h, i, a} -> h a i`}</pre>
  <p>
    Function extensionality, an axiom in intensional MLTT, drops out as a one-line definition.
    Transport dispatches through each type's <code>functor</code> metadata field — the same
    dispatch pattern the kernel already uses everywhere — and <code>Glue</code> would make
    univalence a <em>theorem</em>: equivalent types compute as equal.
  </p>

  <h3 id="long-arc">7.3 · The long arc</h3>
  <blockquote>
    I want to synthesize the best possible programs given a specification… turn the type
    checker into a function that takes a program and returns 1 or 0, combine that with
    functions that measure performance or program size, and somehow turn all that into a
    function represented purely in some low-level deterministic Turing-complete calculus.
    <span class="cite">— GOALS.md</span>
  </blockquote>
  <p>
    The pieces now in place: a reflective substrate (§2), a kernel small enough to trust (§5), a
    universe that can check its own citizens (§6.4), and licensing machinery that can say
    "these two programs are interchangeable, and here is the certificate". What remains is the
    optimizer itself — <a href="https://github.com/libdither/disp/blob/main/OPTIMIZER.typ">OPTIMIZER.typ</a>
    designs it as verified gradient descent on a graded cost over a materialized reduction
    net — and the neural proposer that searches where brute force cannot. An external generator
    proposes candidate trees; the type checker, itself a tree, scores them; and the loop
    refines itself.
  </p>
  <Aside title="Why the self-hosting discipline pays here.">
    <p>
      A synthesized program is only as trustworthy as the checker that admitted it. Because
      disp's checker is an object-language citizen, the optimizer can be pointed at the checker
      too — and every improvement it proposes for itself passes through the same verification
      it applies to everything else.
    </p>
  </Aside>
</section>
