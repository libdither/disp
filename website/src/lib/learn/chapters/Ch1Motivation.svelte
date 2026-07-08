<script lang="ts">
  import Aside from '../Aside.svelte'
  import Gloss from '../Gloss.svelte'
</script>

<section>
  <h2 id="motivation"><span class="secnum">Section 1</span>Motivation</h2>

  <h3 id="what-is-disp">1.1 · What is disp?</h3>
  <p>
    Disp is a dependently typed programming language based on the computational model of the
    <em>tree calculus</em>, rather than the λ-calculus, where the type system is a first-class
    object. Type-checking is one equation:
  </p>
  <p class="math-block">T (v) = Ok true &nbsp;⟺&nbsp; v : T</p>
  <p>
    In words: the type <code>T</code>, applied to the value <code>v</code>, reduces to the
    verdict <code>Ok true</code> if and only if <code>v</code> inhabits <code>T</code>.
    Traditional type theory maintains a separate judgment <span class="math">Γ ⊢ v : T</span>,
    derived by inference rules that live outside the language. Disp has no such separate
    machinery. Types are themselves tree-calculus programs: predicates that, run against a
    candidate tree, return a verdict. Type-checking is, then, just function application on those
    predicates. (The verdict is a <code>CheckerResult</code> — <code>Ok</code> or
    <code>Err</code> — so failure is an ordinary value too, not an exception in some
    meta-language.)
  </p>
  <Aside title="From the README.">
    <p>
      Tree calculus is natively
      <Gloss tip="programs can inspect the structure of other programs by reduction — no metaprogramming layer, no AST traversal">reflective</Gloss>
      — terms <em>are</em> data, so the type checker, the optimizer, and the programs it produces
      all inhabit the same universe. Disp's goal is a fully self-hosting dependent type system
      whose checker lives as an inhabitant of the object language, not just as a host-language
      function.
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
