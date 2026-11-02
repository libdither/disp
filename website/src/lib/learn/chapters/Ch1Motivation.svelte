<script lang="ts">
  import Aside from '../Aside.svelte'
  import Gloss from '../Gloss.svelte'
</script>

<section>
  <h2 id="motivation"><span class="secnum">Section 1</span>Motivation</h2>

  <h3 id="what-is-disp">1.1 · What is disp?</h3>
  <p>
    Disp is a dependently typed programming language based on the computational model of the
    <em>tree calculus</em>, rather than the λ-calculus. Its types are programs you can run. A
    type is a <strong>claim</strong>: a program that takes a witness and a challenge and answers
    true or false.
  </p>
  <p class="math-block">v : T &nbsp;⟺&nbsp; T(v, c) = true for every challenge c</p>
  <p>
    Traditional type theory keeps a separate judgment <span class="math">Γ ⊢ v : T</span>,
    derived by inference rules that live outside the language. Disp has no such machinery.
    Checking a value against a type is applying the type, and the verdicts are ordinary values
    too: Refuted with the challenge that failed, Open when nothing has failed yet, and Proved
    when a checker covers every challenge at once. §4 places this in the family tree of
    <em>types as predicates</em>, and §5 is how a checker earns the right to say Proved.
  </p>
  <Aside title="From the README.">
    <p>
      Tree calculus is natively
      <Gloss tip="programs can inspect the structure of other programs by reduction — no metaprogramming layer, no AST traversal">reflective</Gloss>
      — terms <em>are</em> data, so the type checker, the optimizer, and the programs it produces
      all inhabit the same universe. Disp's goal is a checker that lives in the object language as
      an ordinary program, not just as a host-language function.
    </p>
  </Aside>

  <h3 id="why-this-design">1.2 · Why does this design exist?</h3>
  <p>
    The long-term aspiration is a <em>self-improving optimizer</em>: a program that, given a
    specification of "good program" (encoded as a type — which is just a predicate), searches for
    programs satisfying it and improves the search program itself over time. For that loop to
    close, every component — the specification, the checker, the scoring function, the candidate
    programs — must live inside a single self-inspecting computational universe.
  </p>
  <p>
    Compare the mainstream proof assistants. Lean, Rocq, and Agda all descend from the
    <Gloss tip="Edinburgh LCF (Gordon, Milner & Wadsworth, 1979) established the 'small trusted kernel + everything else as a library' architecture: only the kernel can mint theorems, so soundness reduces to auditing it">LCF tradition</Gloss>,
    yet each still ships a trusted kernel written in its implementation language — Lean 4
    self-hosts its elaborator, but the trusted kernel is C++; Rocq's and Agda's checkers remain
    in OCaml and Haskell. Those kernels accumulate host-language machinery — mutable hash
    tables, exceptions, pattern-match compilation — with no counterpart in the language being
    implemented. Disp applies the LCF discipline from the start, and more literally: the checker
    is a tree program checking tree programs.
  </p>
  <p>
    The operating principle, and this walkthrough's spine: <strong>the object language is the
    specification; host implementations are optimizations.</strong> The TypeScript runtime
    (<code>src/core/tree.ts</code>) and the Rust evaluators accelerate the in-language
    definitions, and must produce identical decisions — they are validated against the
    in-language reference, byte for byte.
  </p>
</section>
