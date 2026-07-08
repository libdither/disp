<script lang="ts">
  import { base } from '$app/paths'
  import NatureTree from '$lib/components/NatureTree.svelte'
  import RuleRail from '$lib/components/RuleRail.svelte'
  import { highlightDisp } from '$lib/editor/highlight'
  import { examples } from '$lib/disp/examples'

  const REPO = 'https://github.com/libdither/disp'
  const JAY = 'https://github.com/barry-jay-personal'
  const TREECALC = 'https://treecalcul.us/'

  // the field guide: one fixed card, entries swap on hover
  interface Entry {
    term: string
    pos: string
    text: string
  }
  const entries: Record<string, Entry> = {
    disp: {
      term: 'disp',
      pos: 'n.',
      text: 'A language grown from one leaf and five rewrite rules, currently teaching itself to check its own homework. The dotted words below have entries of their own.'
    },
    decentralized: {
      term: 'decentralized',
      pos: 'adj.',
      text: 'Features spread the way libraries do. Anyone can build new syntax or types, anyone can adopt them, and dialects translate into one another automatically. No committee, no release train, no waiting.'
    },
    lisp: {
      term: 'lisp',
      pos: 'n.',
      text: 'Programs are data here too, but there is no quote/eval loop to cross. Reduction itself reads tree shape, so a program that rewrites programs is simply a program. Lisp walked so trees could branch.'
    },
    universal: {
      term: 'universal',
      pos: 'adj.',
      text: 'One substrate that other languages can be rebuilt in and transpiled to. The ambition is a language that absorbs languages, not another island with good weather.'
    },
    parsers: {
      term: 'user-definable parsers',
      pos: 'n. pl.',
      text: 'The grammar is a library, not a constitution. A project can define its own notation, elaborate to the same trees underneath, and still talk to everything else.'
    },
    typesystems: {
      term: 'type systems',
      pos: 'n. pl.',
      text: 'Functions, records, induction, and the universe itself: all library code over two trusted kernel operations, re-verified on every load. Different logic is a library you write, not a compiler you fork.'
    },
    optimizer: {
      term: 'self-optimizing optimizer',
      pos: 'n.',
      text: 'Give it a specification: a type, a predicate, a loss function. It derives implementations, each rewrite licensed by a machine-checked certificate. It has a disp type of its own, so it qualifies for its own attention.'
    },
    nets: {
      term: 'interaction nets',
      pos: 'n. pl.',
      text: 'A model of computation where programs are graphs and every step is a small local rewrite. Steps become countable, so cost can be charged to the exact decision that spent it.'
    }
  }
  let entryKey = $state('disp')
  const entry = $derived(entries[entryKey] ?? entries.disp)
  const look = (k: string) => () => (entryKey = k)
  const lookAway = () => (entryKey = 'disp')

  let showcaseIdx = $state(0)
  const showcase = examples.filter((e) => ['trees', 'records', 'proofs', 'universe'].includes(e.id))

  const kernelChain = ['prelude', 'cut', 'engine', 'cells', 'base', 'positive', 'generic', 'universe']
</script>

<svelte:head>
  <title>disp · a decentralized lisp</title>
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
        <button class="dterm" onmouseenter={look('decentralized')} onmouseleave={lookAway} onfocus={look('decentralized')} onblur={lookAway}>decentralized</button>
        <button class="dterm" onmouseenter={look('lisp')} onmouseleave={lookAway} onfocus={look('lisp')} onblur={lookAway}>lisp</button>
      </p>
      <p class="sub">
        disp is an aspiring
        <button class="dterm" onmouseenter={look('universal')} onmouseleave={lookAway} onfocus={look('universal')} onblur={lookAway}>universal</button>
        general-purpose programming language with
        <button class="dterm" onmouseenter={look('parsers')} onmouseleave={lookAway} onfocus={look('parsers')} onblur={lookAway}>user-definable parsers</button>
        and
        <button class="dterm" onmouseenter={look('typesystems')} onmouseleave={lookAway} onfocus={look('typesystems')} onblur={lookAway}>type systems</button>, and a
        <button class="dterm" onmouseenter={look('optimizer')} onmouseleave={lookAway} onfocus={look('optimizer')} onblur={lookAway}>self-optimizing optimizer</button>
        based on
        <button class="dterm" onmouseenter={look('nets')} onmouseleave={lookAway} onfocus={look('nets')} onblur={lookAway}>interaction nets</button>
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
    <!-- the field guide: hover a dotted term and its entry appears here -->
    <aside class="defbox" class:looking={entryKey !== 'disp'} aria-live="polite">
      <span class="def-head">
        <span class="def-term">{entry.term}</span>
        <span class="def-pos">{entry.pos}</span>
      </span>
      <p class="def-text">{entry.text}</p>
      <span class="def-foot">field guide · entry {Object.keys(entries).indexOf(entryKey) + 1} of {Object.keys(entries).length}</span>
    </aside>
  </div>
</section>

<RuleRail />

<!-- ============================== features ============================== -->
<section class="features container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <p class="sect-sub">
    The type system, the self-verifying kernel, and five byte-agreeing evaluators run today.
    The rest is what they were built to reach.
    <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener">FOUNDATIONS.md</a>
    keeps honest score of which is which.
  </p>
  <div class="feat-grid">
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Homoiconic, past lisp</h3>
      <p>
        Programs are binary trees, and trees are plain data. There is no quote/eval border to
        smuggle code across: the rewrite rules themselves read tree shape, so a macro, an
        optimizer, or a type checker is an ordinary program that happens to eat programs. Equal
        programs are even the same pointer, courtesy of hash-consing.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Type systems you can replace</h3>
      <p>
        A type is a predicate. Apply it to a value and it answers. Functions, records,
        induction, and the universe itself are library code sitting on
        <strong>two trusted kernel operations</strong>, and the whole stack re-verifies itself
        every time it loads. If you want different logic, you write a library, not a fork of
        the compiler.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>Your syntax, your dialect</h3>
      <p>
        Parsers you can define, features that spread by adoption rather than by committee, and
        automatic translation between the dialects that result. The point is one substrate that
        other languages can be rebuilt in and transpiled to, not one more island with a nice
        standard library.
      </p>
    </div>
    <div class="card feat">
      <h3><span class="feat-dot" aria-hidden="true"></span>An optimizer aimed at itself</h3>
      <p>
        Programs materialize as <a href="{REPO}/blob/main/OPTIMIZER.typ" target="_blank" rel="noopener">interaction nets</a>,
        where every reduction is a local rewrite you can count, and hardware is modeled as
        imperfect net reduction. Cost lands on individual decisions. Rewrites need
        machine-checked equivalence certificates, and since the optimizer has a disp type too,
        nothing stops it from taking a pass at itself.
      </p>
    </div>
  </div>
</section>

<hr class="keyline container" />

<!-- ============================== showcase ============================== -->
<section class="showcase container">
  <h2 class="sect-title">The features, in code</h2>
  <p class="sect-sub">
    Every block below passes the real compiler. The playground runs that same compiler in your
    tab, kernel self-verification included, so the code and the claims can't drift apart.
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
    in dependency order and re-verifies every typed export through the kernel itself. The last
    thing checked is the universe, by the universe.
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

  /* ---- the field guide (one card, fixed size, pinned beside the wordmark) ---- */
  .dterm {
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    color: inherit;
    cursor: help;
    text-decoration: underline dotted var(--g2);
    text-decoration-thickness: 1.5px;
    text-underline-offset: 3px;
  }
  .dterm:hover,
  .dterm:focus-visible {
    color: var(--g2);
    outline: none;
  }
  .defbox {
    position: absolute;
    top: -1.2rem;
    right: 0;
    width: 306px;
    height: 218px;
    background: color-mix(in oklab, var(--g4) 5%, var(--bg-elev));
    border: 1px solid var(--border-strong);
    border-radius: 12px;
    padding: 0.9rem 1.05rem 0.7rem;
    box-shadow: var(--shadow-soft);
    transform: rotate(1.1deg);
    transition: transform 0.25s ease, border-color 0.25s ease;
    display: flex;
    flex-direction: column;
    z-index: 3;
    overflow: hidden;
  }
  .defbox.looking {
    transform: rotate(0deg);
    border-color: var(--g2);
  }
  .def-head {
    display: flex;
    align-items: baseline;
    gap: 0.5em;
    border-bottom: 1px solid var(--border);
    padding-bottom: 0.35rem;
  }
  .def-term {
    font-family: var(--font-display);
    font-variation-settings: 'SOFT' 60, 'WONK' 1;
    font-weight: 620;
    font-size: 1.12rem;
    color: var(--fg);
  }
  .def-pos {
    font-family: var(--font-display);
    font-style: italic;
    color: var(--fg-faint);
    font-size: 0.9rem;
  }
  .def-text {
    flex: 1;
    margin: 0.5rem 0 0;
    font-size: 0.83rem;
    line-height: 1.55;
    color: var(--fg-muted);
  }
  .def-foot {
    font-family: var(--font-mono);
    font-size: 0.62rem;
    color: var(--fg-faint);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    padding-top: 0.4rem;
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
    /* the field guide joins the flow on small screens */
    .defbox {
      position: static;
      width: 100%;
      height: auto;
      min-height: 0;
      transform: none;
      order: 3;
    }
  }
</style>
