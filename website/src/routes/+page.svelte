<script lang="ts">
  import { base } from '$app/paths'
  import NatureTree from '$lib/components/NatureTree.svelte'
  import RuleRail from '$lib/components/RuleRail.svelte'
  import { highlightDisp } from '$lib/editor/highlight'
  import { examples } from '$lib/disp/examples'
  import { snippetById } from '$lib/disp/landing-snippets'

  const REPO = 'https://github.com/libdither/disp'

  const snippetChecking = snippetById.checking
  const snippetKernel = snippetById.kernel
  const snippetEq = snippetById.eq
  const snippetUniverse = snippetById.universe

  let showcaseIdx = $state(0)
  const showcase = examples.filter((e) => ['trees', 'records', 'proofs', 'universe'].includes(e.id))

  const kernelChain = ['prelude', 'cut', 'engine', 'cells', 'base', 'positive', 'generic', 'universe']
</script>

<svelte:head>
  <title>disp — the language that checks itself</title>
</svelte:head>

<!-- ============================== hero ============================== -->
<section class="hero">
  <div class="aurora" aria-hidden="true"></div>
  <div class="container hero-grid">
    <div class="hero-copy">
      <h1>
        <span class="grad-text">disp</span>
      </h1>
      <p class="tagline">A language that <em>checks itself</em>.</p>
      <p class="sub">
        disp is a dependently-typed language built on the tree calculus. Types are ordinary
        programs, proofs are runs, and the whole type system — Π, records, induction, the universe
        — is library code re-verified from <strong>two trusted operations</strong>.
      </p>
      <div class="cta-row">
        <a class="btn primary" href="{base}/learn/">Get started</a>
        <a class="btn" href="{base}/playground/">Try it in your browser</a>
      </div>
      <div class="term">
        <span class="term-dollar">$</span>
        <code>git clone {REPO.replace('https://', '')} && npm i && npm test</code>
      </div>
    </div>
    <div class="hero-viz">
      <NatureTree />
    </div>
  </div>
</section>

<RuleRail />

<!-- ============================== stats ============================== -->
<section class="stats">
  <div class="container stats-row">
    <div class="stat"><b>2</b><span>trusted kernel operations</span></div>
    <div class="stat"><b>~1,200</b><span>machine-checked tests</span></div>
    <div class="stat"><b>5</b><span>evaluator backends, byte-agreeing</span></div>
    <div class="stat"><b>0</b><span>rights reserved — public domain</span></div>
  </div>
</section>

<!-- ============================== features ============================== -->
<section class="features container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <div class="feat-grid">
    <div class="card feat">
      <h3>Checking is running</h3>
      <p>
        A type is a predicate: apply it to a value and it answers. There is no separate judgment
        layer and no meta-language — the type checker is a tree program like everything else.
      </p>
      <pre class="feat-code">{@html highlightDisp(snippetChecking)}</pre>
    </div>
    <div class="card feat">
      <h3>Two operations to trust</h3>
      <p>
        The kernel mints unforgeable hypotheses and routes every observation through them —
        that's it. Functions, records, equality, induction, and the universe itself are readable
        library code, replaceable without kernel surgery.
      </p>
      <pre class="feat-code">{@html highlightDisp(snippetKernel)}</pre>
    </div>
    <div class="card feat">
      <h3>Equality is a pointer</h3>
      <p>
        Elaboration is deterministic and trees are hash-consed, so equal programs are literally
        the same tree. Conversion checking — the hot loop of every proof assistant — is O(1) here.
      </p>
      <pre class="feat-code">{@html highlightDisp(snippetEq)}</pre>
    </div>
    <div class="card feat">
      <h3>The universe answers for itself</h3>
      <p>
        <code>Type</code> is a behavioral check that probes a candidate type's responses — and it
        passes its own probe. The checker checks the checkers, using the same two kernel
        operations it uses on everything else.
      </p>
      <pre class="feat-code">{@html highlightDisp(snippetUniverse)}</pre>
    </div>
  </div>
  <p class="feat-foot">
    The long game: a <a href="{REPO}/blob/main/OPTIMIZER.typ">verified self-improving optimizer</a>
    — you write constraints as detailed as you need, and implementations are derived, with
    machine-checked equivalence certificates licensing every rewrite.
  </p>
</section>

<hr class="keyline container" />

<!-- ============================== showcase ============================== -->
<section class="showcase container">
  <h2 class="sect-title">See it, then run it</h2>
  <p class="sect-sub">
    Every block below passes the real compiler — and the playground runs that same compiler in
    your tab, kernel self-verification included.
  </p>
  <div class="tabs">
    {#each showcase as ex, i}
      <button class="tab" class:active={showcaseIdx === i} onclick={() => (showcaseIdx = i)}>
        {ex.label.replace(/ \(.*\)/, '')}
      </button>
    {/each}
    <div class="tab-space"></div>
    <a class="btn tab-run" href="{base}/playground/?example={showcase[showcaseIdx].id}">
      ▶ Open in playground
    </a>
  </div>
  <pre class="show-code">{@html highlightDisp(showcase[showcaseIdx].source.trim())}</pre>
</section>

<hr class="keyline container" />

<!-- ============================== self-verify band ============================== -->
<section class="selfcheck container">
  <h2 class="sect-title">No pre-checked kernel ships</h2>
  <p class="sect-sub">
    When the playground boots the type system, your browser re-elaborates the kernel's fragments
    in dependency order and re-verifies every typed export through the kernel itself — ending
    with the universe passing its own checker.
  </p>
  <div class="chain" role="img" aria-label="kernel bootstrap chain: {kernelChain.join(', ')}">
    {#each kernelChain as frag, i}
      <span class="chip" style="--d:{i}">{frag}</span>
      {#if i < kernelChain.length - 1}<span class="chev">→</span>{/if}
    {/each}
    <span class="loop" title="Type : Type">⟲ Type checks Type</span>
  </div>
  <div class="cta-row center">
    <a class="btn" href="{base}/playground/">Watch it happen</a>
    <a class="btn" href="{base}/learn/#who-checks">How can that be sound?</a>
  </div>
</section>

<hr class="keyline container" />

<!-- ============================== involvement ============================== -->
<section class="involve container">
  <h2 class="sect-title">Get involved</h2>
  <div class="inv-grid">
    <a class="card inv" href="{base}/learn/">
      <h3>Learn the theory</h3>
      <p>A first-principles walkthrough: from five rewrite rules to a self-verifying universe.</p>
      <span class="inv-cta">Start reading →</span>
    </a>
    <a class="card inv" href={REPO} target="_blank" rel="noopener">
      <h3>Read the source</h3>
      <p>
        The kernel is ~1k lines of literate disp. The docs rate every file's quality and origin,
        honestly.
      </p>
      <span class="inv-cta">github.com/libdither/disp ↗</span>
    </a>
    <a class="card inv" href="{base}/funding/">
      <h3>Support the work</h3>
      <p>Independent research, public domain output. Every bit of support extends the runway.</p>
      <span class="inv-cta">Funding →</span>
    </a>
  </div>
</section>

<style>
  /* ---------- hero ---------- */
  .hero {
    position: relative;
    overflow: hidden;
    padding: clamp(3rem, 9vh, 6.5rem) 0 2.5rem;
  }
  /* dappled midmorning light through a canopy */
  .aurora {
    position: absolute;
    inset: -20% -10%;
    background:
      radial-gradient(38% 45% at 18% 22%, color-mix(in oklab, var(--g1) 20%, transparent), transparent 70%),
      radial-gradient(42% 50% at 78% 18%, color-mix(in oklab, var(--g4) 16%, transparent), transparent 70%),
      radial-gradient(50% 55% at 60% 85%, color-mix(in oklab, var(--blossom) 14%, transparent), transparent 70%),
      radial-gradient(35% 40% at 38% 60%, color-mix(in oklab, var(--g3) 12%, transparent), transparent 70%);
    animation: drift 26s ease-in-out infinite alternate;
    pointer-events: none;
  }
  @keyframes drift {
    to { transform: translate3d(2.5%, -2%, 0) scale(1.06) rotate(1.2deg); }
  }
  .hero-grid {
    position: relative;
    display: grid;
    grid-template-columns: minmax(0, 7fr) minmax(0, 6fr);
    gap: 2.5rem;
    align-items: center;
  }
  h1 {
    font-size: clamp(4.2rem, 9vw, 6.8rem);
    margin: 0;
    line-height: 0.95;
    font-variation-settings: 'SOFT' 80, 'WONK' 1;
    font-weight: 640;
    letter-spacing: -0.02em;
  }
  .tagline {
    font-family: var(--font-display);
    font-size: clamp(1.5rem, 3vw, 2.1rem);
    margin: 0.7rem 0 0.9rem;
    color: var(--fg);
    font-variation-settings: 'SOFT' 50, 'WONK' 0;
  }
  .tagline em { font-style: italic; color: var(--g2); }
  .sub {
    color: var(--fg-muted);
    max-width: 34rem;
    font-size: 1.05rem;
    margin: 0 0 1.6rem;
  }
  .cta-row { display: flex; gap: 0.8rem; flex-wrap: wrap; margin-bottom: 1.4rem; }
  .cta-row.center { justify-content: center; margin-top: 1.6rem; }
  .term {
    display: inline-flex;
    align-items: center;
    gap: 0.6em;
    background: var(--bg-code);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 0.55em 1em;
    font-size: 0.8rem;
    color: var(--fg-muted);
    max-width: 100%;
    overflow-x: auto;
  }
  .term-dollar { color: var(--g1); font-family: var(--font-mono); }
  .term code { white-space: nowrap; }
  .hero-viz {
    min-width: 0;
    border: 1px solid var(--border);
    border-radius: 16px;
    background:
      radial-gradient(120% 130% at 50% 0%, color-mix(in oklab, var(--g3) 7%, transparent), transparent 60%),
      var(--bg-panel);
    padding: 1rem 0.6rem 0.4rem;
  }

  /* ---------- stats ---------- */
  .stats {
    border-block: 1px solid var(--border);
    background: var(--bg-elev);
  }
  .stats-row {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 1rem;
    padding-block: 1.1rem;
  }
  .stat { text-align: center; }
  .stat b {
    display: block;
    font-family: var(--font-display);
    font-size: 1.9rem;
    background: var(--grad-brand);
    -webkit-background-clip: text;
    background-clip: text;
    color: transparent;
  }
  .stat span { color: var(--fg-muted); font-size: 0.82rem; }

  /* ---------- features ---------- */
  .features { padding-top: 3.4rem; }
  .sect-title {
    font-size: clamp(1.9rem, 4vw, 2.6rem);
    margin: 0 0 0.5rem;
  }
  .sect-sub { color: var(--fg-muted); max-width: 44rem; margin-top: 0; }
  .feat-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1.1rem;
    margin-top: 1.6rem;
  }
  .feat h3 { margin: 0 0 0.5rem; font-size: 1.25rem; }
  .feat p { color: var(--fg-muted); font-size: 0.94rem; margin: 0 0 0.9rem; }
  .feat-code {
    font-size: 0.76rem;
    margin: 0;
    background: var(--bg-code);
  }
  .feat-foot {
    color: var(--fg-muted);
    margin-top: 1.6rem;
    font-size: 0.95rem;
    max-width: 52rem;
  }
  .features, .showcase, .selfcheck, .involve { padding-block: 3.2rem; }

  /* ---------- showcase ---------- */
  .tabs {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    flex-wrap: wrap;
    margin-bottom: 0.8rem;
  }
  .tab {
    background: none;
    border: 1px solid transparent;
    color: var(--fg-muted);
    font: inherit;
    font-size: 0.9rem;
    padding: 0.4em 0.9em;
    border-radius: 999px;
    cursor: pointer;
  }
  .tab:hover { color: var(--fg); background: var(--bg-panel-hover); }
  .tab.active {
    color: var(--fg);
    border-color: var(--border-strong);
    background: var(--bg-panel);
    box-shadow: inset 0 -2px 0 -0.5px var(--accent);
  }
  .tab-space { flex: 1; }
  .tab-run { font-size: 0.84rem; padding: 0.4em 1em; }
  .show-code {
    font-size: 0.8rem;
    max-height: 430px;
    overflow: auto;
    line-height: 1.6;
  }

  /* ---------- self-check band ---------- */
  .selfcheck { text-align: center; }
  .selfcheck .sect-sub { margin-inline: auto; }
  .chain {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    align-items: center;
    gap: 0.45rem;
    margin-top: 1.8rem;
    font-family: var(--font-mono);
    font-size: 0.82rem;
  }
  .chip {
    border: 1px solid var(--border-strong);
    border-radius: 999px;
    padding: 0.32em 0.85em;
    background: var(--bg-panel);
    animation: chipglow 6s ease-in-out calc(var(--d) * 0.75s) infinite;
  }
  @keyframes chipglow {
    0%, 100% { border-color: var(--border-strong); box-shadow: none; }
    8% {
      border-color: var(--g2);
      box-shadow: 0 0 14px -2px color-mix(in oklab, var(--g2) 55%, transparent);
    }
    16% { border-color: var(--border-strong); box-shadow: none; }
  }
  .chev { color: var(--fg-faint); }
  .loop {
    margin-left: 0.5rem;
    color: var(--g1);
    border: 1px dashed color-mix(in oklab, var(--g1) 55%, transparent);
    border-radius: 999px;
    padding: 0.32em 0.85em;
  }

  /* ---------- involve ---------- */
  .inv-grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 1.1rem;
    margin-top: 1.4rem;
  }
  a.inv { display: block; text-decoration: none !important; transition: transform 0.15s ease, border-color 0.15s ease; }
  a.inv:hover { transform: translateY(-3px); border-color: var(--fg-faint); }
  .inv h3 { margin: 0 0 0.4rem; font-size: 1.15rem; color: var(--fg); }
  .inv p { color: var(--fg-muted); font-size: 0.9rem; min-height: 3.2em; }
  .inv-cta { color: var(--accent); font-size: 0.9rem; }

  /* ---------- responsive ---------- */
  @media (max-width: 880px) {
    .hero-grid { grid-template-columns: 1fr; }
    .hero-viz { order: -1; }
    .stats-row { grid-template-columns: repeat(2, 1fr); row-gap: 1.2rem; }
    .feat-grid, .inv-grid { grid-template-columns: 1fr; }
  }
</style>
