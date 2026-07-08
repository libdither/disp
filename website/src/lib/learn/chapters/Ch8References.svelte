<script lang="ts">
  interface Ref {
    id: string
    authors: string
    year: string
    title: string
    detail: string
    href?: string
  }
  const refs: Ref[] = [
    { id: 'schonfinkel', authors: 'Schönfinkel, M.', year: '1924', title: 'Über die Bausteine der mathematischen Logik', detail: 'Mathematische Annalen 92, 305–316; English translation in van Heijenoort, From Frege to Gödel, Harvard UP 1967.' },
    { id: 'curry', authors: 'Curry, H. B., & Feys, R.', year: '1958', title: 'Combinatory Logic, Vol. I', detail: 'North-Holland. The classical reference for the combinatory calculus and bracket abstraction.' },
    { id: 'howard', authors: 'Howard, W. A.', year: '1980', title: 'The formulae-as-types notion of construction', detail: 'In Seldin & Hindley (eds.), To H. B. Curry, 479–490, Academic Press; circulated 1969.' },
    { id: 'martinlof', authors: 'Martin-Löf, P.', year: '1984', title: 'Intuitionistic Type Theory', detail: 'Sambin notes, Padua lectures 1980. Bibliopolis.' },
    { id: 'scott', authors: 'Scott, D. S.', year: '1976', title: 'Data types as lattices', detail: 'SIAM J. Computing 5(3):522–587. The lattice-theoretic origin of the encoding now called "Scott encoding".' },
    { id: 'reynolds', authors: 'Reynolds, J. C.', year: '1983', title: 'Types, abstraction and parametric polymorphism', detail: 'Information Processing 83, 513–523. The foundational paper on relational parametricity.' },
    { id: 'jay', authors: 'Jay, B.', year: '2021', title: 'Reflective Programs in Tree Calculus', detail: 'Self-published; sources and PDFs on GitHub.', href: 'https://github.com/barry-jay-personal/tree-calculus' },
    { id: 'cubical', authors: 'Cohen, C., Coquand, T., Huber, S., & Mörtberg, A.', year: '2018', title: 'Cubical Type Theory: a constructive interpretation of the univalence axiom', detail: 'TYPES 2015, LIPIcs 69.', href: 'https://arxiv.org/abs/1611.02108' },
    { id: 'hott', authors: 'The Univalent Foundations Program', year: '2013', title: 'Homotopy Type Theory: Univalent Foundations of Mathematics', detail: 'IAS Princeton.', href: 'https://homotopytypetheory.org/book/' },
    { id: 'gordon', authors: 'Gordon, M., Milner, R., & Wadsworth, C. P.', year: '1979', title: 'Edinburgh LCF: A Mechanised Logic of Computation', detail: 'LNCS 78, Springer. The progenitor of the small-trusted-kernel tradition that informs disp’s discipline.' },
    { id: 'maclane', authors: 'Mac Lane, S., & Moerdijk, I.', year: '1994', title: 'Sheaves in Geometry and Logic', detail: 'Springer. Standard reference for elementary topos theory and the subobject classifier.' },
    { id: 'lambek', authors: 'Lambek, J., & Scott, P. J.', year: '1986', title: 'Introduction to Higher Order Categorical Logic', detail: 'Cambridge University Press.' },
    { id: 'lean', authors: 'de Moura, L., et al.', year: '', title: 'The Lean theorem prover & programming language', detail: '', href: 'https://lean-lang.org' },
    { id: 'rocq', authors: 'The Rocq Development Team', year: '', title: 'The Rocq Prover (formerly Coq)', detail: '', href: 'https://rocq-prover.org' },
    { id: 'agda', authors: 'The Agda Team', year: '', title: 'Agda — dependently typed functional programming language', detail: '', href: 'https://agda.readthedocs.io' }
  ]
</script>

<section>
  <h2 id="references"><span class="secnum">Section 8</span>References</h2>
  <ol class="refs">
    {#each refs as r}
      <li id="ref-{r.id}">
        <span class="authors">{r.authors}</span>
        {#if r.year}({r.year}).{/if}
        {#if r.href}
          <a href={r.href} target="_blank" rel="noopener"><em>{r.title}</em></a>{:else}<em>{r.title}</em>{/if}{#if r.detail}&nbsp;{r.detail}{/if}
      </li>
    {/each}
  </ol>
</section>

<style>
  .refs {
    padding-left: 1.4rem;
    font-size: 0.92rem;
  }
  .refs li {
    margin-bottom: 0.6rem;
    color: var(--fg-muted);
  }
  .authors {
    color: var(--fg);
    font-weight: 550;
  }
</style>
