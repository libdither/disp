<script lang="ts">
  import { base } from '$app/paths'
  import NatureTree from '$lib/components/NatureTree.svelte'
  import RuleRail from '$lib/components/RuleRail.svelte'
  import Gloss from '$lib/learn/Gloss.svelte'
  import { highlightDisp } from '$lib/editor/highlight'
  import { examples } from '$lib/disp/examples'

  const REPO = 'https://github.com/libdither/disp'
  const JAY = 'https://github.com/barry-jay-personal'
  const TREECALC = 'https://treecalcul.us/'

  let showcaseIdx = $state(0)
  const showcase = examples.filter((e) => ['trees', 'records', 'proofs', 'universe'].includes(e.id))

  const kernelChain = ['prelude', 'cut', 'engine', 'cells', 'base', 'positive', 'generic', 'universe']
</script>

<svelte:head>
  <title>disp — a decentralized lisp</title>
</svelte:head>

<!-- ============================== hero ============================== -->
<section class="hero">
  <div class="aurora" aria-hidden="true"></div>
  <div class="container hero-grid">
    <div class="hero-copy">
      <h1>
        <span class="grad-text">disp</span>
      </h1>
      <p class="abbr">
        <span class="abbr-label">abbr.</span>
        <Gloss
          tip="different programmers can build and adopt language features — syntax, types — independently, with automatic translation between dialects. No committee, no release train: features spread by being adopted."
          >decentralized</Gloss>
        &nbsp;<Gloss
          tip="like lisp, disp is homoiconic: programs are data you can compute on. Unlike lisp there is no quote/eval loop to cross — programs are trees, and reduction itself inspects tree shape — so program-rewriting programs (macros, optimizers, type checkers) are just ordinary programs."
          >lisp</Gloss>
      </p>
      <p class="sub">
        disp is an aspiring
        <Gloss
          tip="one substrate other languages can be re-created in and transpiled to — a language for absorbing languages, rather than another island."
          >universal</Gloss>
        general-purpose programming language with
        <Gloss
          tip="the surface grammar is a library concern, not a constitution: projects will define their own notations that elaborate to the same trees and interoperate."
          >user-definable parsers</Gloss>
        and
        <Gloss
          tip="the entire type system — Π, records, induction, the universe itself — is library code over a two-operation kernel, and re-verifies itself when it loads. Extend it or replace it without kernel surgery."
          >type systems</Gloss>, and a
        <Gloss
          tip="an optimizer that takes a specification (a type, a predicate, a loss function) and derives implementations — with machine-checked equivalence certificates licensing every rewrite. Being a disp program with a disp type, it is a candidate for its own improvement."
          >self-optimizing optimizer</Gloss>
        based on
        <Gloss
          tip="a model of computation where programs are graphs and reduction is local and parallel — so work is countable per rewrite, and cost is attributable to individual decisions."
          >interaction nets</Gloss>
        that models hardware as imperfect interaction-net reduction. Based on
        <a href={JAY} target="_blank" rel="noopener">Barry Jay</a>'s
        <a href={TREECALC} target="_blank" rel="noopener">tree calculus</a>.
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

<!-- ============================== features ============================== -->
<section class="features container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <p class="sect-sub">
    Some of this runs today — the type system, the self-verifying kernel, five byte-agreeing
    evaluators. Some is the design goal the rest is built toward.
    <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener">FOUNDATIONS.md</a>
    keeps score honestly.
  </p>
  <div class="feat-grid">
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Homoiconic, past lisp</h3>
      <p>
        Programs are binary trees, and trees are data — with no quote/eval boundary between the
        two. Case analysis over programs is a <em>rewrite rule</em> (the triage), so a program
        that rewrites programs — a macro, an optimizer, a type checker — is just a program.
        Elaboration is deterministic and trees are hash-consed, so equal programs are literally
        the same pointer.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Type systems you can replace</h3>
      <p>
        A type is a predicate: apply it to a value and it answers. The entire type system —
        Π, records, induction, the universe itself — is library code over a kernel of
        <strong>two trusted operations</strong>, and it re-verifies itself every time it loads.
        Want different formers, different logic? That's a library, not kernel surgery.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Your syntax, your dialect</h3>
      <p>
        The decentralized ambition: user-definable parsers and independently adoptable language
        features, with automatic translation between dialects — one universal substrate that
        other languages can be re-created in and transpiled to, instead of another island.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>An optimizer aimed at itself</h3>
      <p>
        Programs materialize as <a href="{REPO}/blob/main/OPTIMIZER.typ">interaction nets</a>,
        where reduction is local, parallel, and countable — hardware is modeled as imperfect net
        reduction, so cost attaches to individual decisions. Machine-checked equivalence
        certificates license every rewrite, and the optimizer's own disp type makes it a
        candidate for its own improvement.
      </p>
    </div>
  </div>
</section>

<hr class="keyline container" />

<!-- ============================== showcase ============================== -->
<section class="showcase container">
  <h2 class="sect-title">The features, in code</h2>
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
  .abbr {
    font-family: var(--font-display);
    font-size: clamp(1.15rem, 2.2vw, 1.5rem);
    font-style: italic;
    font-variation-settings: 'SOFT' 60, 'WONK' 1;
    color: var(--fg-muted);
    margin: 0.5rem 0 1rem;
  }
  .abbr-label {
    font-family: var(--font-mono);
    font-style: normal;
    font-size: 0.62em;
    color: var(--fg-faint);
    text-transform: uppercase;
    letter-spacing: 0.1em;
    margin-right: 0.35em;
  }
  .sub {
    color: var(--fg-muted);
    max-width: 36rem;
    font-size: 1.03rem;
    line-height: 1.75;
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
  .feat h3 {
    margin: 0 0 0.5rem;
    font-size: 1.25rem;
    display: flex;
    align-items: center;
    gap: 0.5em;
  }
  .feat p { color: var(--fg-muted); font-size: 0.94rem; margin: 0; }
  .feat-dot {
    width: 10px;
    height: 10px;
    border-radius: 50% 50% 50% 2px;
    background: var(--grad-brand);
    flex: none;
    transform: rotate(45deg);
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
    .feat-grid, .inv-grid { grid-template-columns: 1fr; }
  }
</style>
