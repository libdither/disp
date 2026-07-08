<script lang="ts">
  const REPO = 'https://github.com/libdither/disp'
</script>

<svelte:head>
  <title>Funding — disp</title>
</svelte:head>

<div class="container page">
  <h1>Funding</h1>
  <p class="lede">
    disp is independent research: a self-verifying language, an optimizer meant to improve
    itself, and every line of it dedicated to the public domain. There is no company behind it
    and no rights are reserved — which also means no revenue model. Support extends the runway
    directly.
  </p>

  <div class="grid">
    <div class="card">
      <h3>Right now</h3>
      <p>
        Formal sponsorship channels aren't set up yet. If you want to fund this work — as an
        individual, a lab, or a company interested in verified synthesis — open an issue or reach
        out through GitHub and we'll figure out the mechanics.
      </p>
      <a class="btn" href="{REPO}/issues" target="_blank" rel="noopener">Open an issue ↗</a>
    </div>
    <div class="card">
      <h3>Zero-dollar support</h3>
      <p>
        Stars and watchers genuinely help the project get seen. Reading
        <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener">FOUNDATIONS.md</a>
        and poking holes in its falsifiable questions helps even more.
      </p>
      <a class="btn" href={REPO} target="_blank" rel="noopener">Star on GitHub ↗</a>
    </div>
    <div class="card">
      <h3>Where money goes</h3>
      <p>
        Compute for the neural-guided synthesis experiments, AI-assisted development costs, and
        focused development time on the optimizer — the parts of
        <a href="{REPO}/blob/main/GOALS.md" target="_blank" rel="noopener">GOALS.md</a> that are
        designed but not yet built.
      </p>
    </div>
  </div>

  <p class="note">
    Everything produced stays under the <a href="{REPO}/blob/main/LICENSE">Unlicense</a> —
    funding buys the world more public-domain research, not access to it.
  </p>
</div>

<style>
  .page { padding-block: 3.5rem 2rem; max-width: 900px; }
  h1 { font-size: clamp(2.2rem, 5vw, 3.2rem); margin: 0 0 0.8rem; }
  .lede { color: var(--fg-muted); font-size: 1.08rem; max-width: 46rem; }
  .grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 1.1rem;
    margin-top: 2rem;
  }
  .card h3 { margin: 0 0 0.5rem; font-size: 1.1rem; }
  .card p { color: var(--fg-muted); font-size: 0.93rem; }
  .note { color: var(--fg-faint); font-size: 0.9rem; margin-top: 2.2rem; }
</style>
