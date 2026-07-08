<script lang="ts">
  import { base } from "$app/paths";
  import TreeVis from "$lib/components/TreeVis.svelte";
  import RuleRail from "$lib/components/RuleRail.svelte";
  import { highlightDisp } from "$lib/editor/highlight";
  import { examples } from "$lib/disp/examples";

  const REPO = "https://github.com/libdither/disp";
  const JAY = "https://github.com/barry-jay-personal";
  const TREECALC = "https://treecalcul.us/";

  // the field guide: one fixed card, entries swap on hover.
  // `html` is trusted author-written markup: links, <em>, <code> all work,
  // and plain \n newlines render as line breaks (white-space: pre-line).
  interface Entry {
    term: string;
    pos: string;
    html: string;
  }
  const entries: Record<string, Entry> = {
    disp: {
      term: "disp",
      pos: "n.",
      html: 'Ingredients: <ul><li>one leaf</li> <li>five rewrite rules</li> <li>types-as-predicates</li> <li>interaction nets</li> <li>and <a href="https://dither.link/">a dream</a>...</li></ul>',
    },
    decentralized: {
      term: "decentralized",
      pos: "adj.",
      html: "Is your favorite programming language is being sensible and not including your new pet feature into the language? Well in disp you can implement that feature yourself! Just need to formally prove it plays nice with everything else ofc :)",
    },
    lisp: {
      term: "lisp",
      pos: "n.",
      html: "Disp is like Lisp but no quote/eval on S-expressions required, just <code>triage</code> on a tree! Honestly, who even liked S-expressions anyway, too many parentheses...",
    },
    universal: {
      term: "universal",
      pos: "adj.",
      html: "The goal is for disp to be a singular substrate that other languages can be rebuilt in and transpiled to. Disp shall become the <em>grey goo of programming languages</em> mwahahahaHAHAHAHA",
    },
    parsers: {
      term: "user-definable parsers",
      pos: "n. pl.",
      html: '"A parser for things is a function from strings to potentially a pair of that thing and its string" and in disp, compilation is just a function man...',
    },
    typesystems: {
      term: "type systems",
      pos: "n. pl.",
      html: "A type system is just a system of types. Types are just predicates on programs. A type system is just a collection of predicates on programs. Why does no one teach it this way?!?",
    },
    optimizer: {
      term: "self-optimizing optimizer",
      pos: "n.",
      html: "Eliezer Yudkowsky called me and said this was probably a bad idea but idk man, I'd rather interpretable recursive self-improvement than whatever Anthropic and OpenAI be up to.",
    },
    nets: {
      term: "interaction nets",
      pos: "n. pl.",
      html: 'Okay, so imagine like feynman diagrams where particles are splitting apart and annihilating but in doing so they are doing computation, oh hi there <a href="https://github.com/VictorTaelin" target="_blank" rel="noopener">@VictorTaelin</a> didn\'t see you there',
    },
  };
  let entryKey = $state("disp");
  // clicking a term PINS its note (the tack goes in, hover stops mattering);
  // clicking the same term again, or anywhere outside the card, lets it go
  let pinned = $state<string | null>(null);
  const entry = $derived(entries[pinned ?? entryKey] ?? entries.disp);
  // leaving a term starts a short grace period, and hovering the box itself
  // holds the entry open, so links inside the card are reachable
  let resetTimer: ReturnType<typeof setTimeout> | undefined;
  const look = (k: string) => () => {
    clearTimeout(resetTimer);
    if (!pinned) entryKey = k;
  };
  const lookAway = () => {
    clearTimeout(resetTimer);
    if (pinned) return;
    resetTimer = setTimeout(() => (entryKey = "disp"), 650);
  };
  const holdEntry = () => clearTimeout(resetTimer);
  const togglePin = (k: string) => () => {
    clearTimeout(resetTimer);
    pinned = pinned === k ? null : k;
    entryKey = k;
  };
  const onWindowClick = (e: MouseEvent) => {
    if (!pinned) return;
    const t = e.target as Element | null;
    if (t?.closest(".defbox") || t?.closest(".dterm")) return;
    pinned = null;
    entryKey = "disp";
  };

  // the git-clone chip's copy button
  const CLONE_CMD = `git clone ${REPO.replace("https://", "")} && npm i && npm test`;
  let copied = $state(false);
  let copiedTimer: ReturnType<typeof setTimeout> | undefined;
  async function copyClone() {
    try {
      await navigator.clipboard.writeText(CLONE_CMD);
      copied = true;
      clearTimeout(copiedTimer);
      copiedTimer = setTimeout(() => (copied = false), 1600);
    } catch {}
  }

  let showcaseIdx = $state(0);
  const showcase = examples.filter((e) =>
    ["trees", "records", "proofs", "universe"].includes(e.id),
  );

  const kernelChain = [
    "prelude",
    "cut",
    "engine",
    "cells",
    "base",
    "positive",
    "generic",
    "universe",
  ];
</script>

<svelte:head>
  <title>disp · a decentralized lisp</title>
</svelte:head>

<svelte:window onclick={onWindowClick} />

<!-- ============================== hero ============================== -->
<section class="hero">
  <div class="aurora" aria-hidden="true"></div>
  <div class="container hero-grid">
    <div class="hero-copy">
      <div class="head-row">
        <div class="head-words">
          <h1>
            <span class="grad-text">disp</span>
          </h1>
          <p class="abbr">
            <span class="abbr-label">abbr.</span>
            <button
              class="dterm"
              onmouseenter={look("decentralized")}
              onmouseleave={lookAway}
              onfocus={look("decentralized")}
              onclick={togglePin("decentralized")}
              onblur={lookAway}>decentralized</button
            >
            <button
              class="dterm"
              onmouseenter={look("lisp")}
              onmouseleave={lookAway}
              onfocus={look("lisp")}
              onclick={togglePin("lisp")}
              onblur={lookAway}>lisp</button
            >
          </p>
        </div>
        <!-- one definition box: hover a dotted term and its entry appears -->
        <aside
          class="defbox"
          class:looking={entryKey !== "disp" || pinned !== null}
          class:pinned={pinned !== null}
          aria-live="polite"
          onmouseenter={holdEntry}
          onmouseleave={lookAway}
        >
          <span class="def-head">
            <span class="def-term">{entry.term}</span>
            <span class="def-pos">{entry.pos}</span>
          </span>
          <div class="def-text">{@html entry.html}</div>
        </aside>
      </div>
      <p class="sub">
        disp is an aspiring
        <button
          class="dterm"
          onmouseenter={look("universal")}
          onmouseleave={lookAway}
          onfocus={look("universal")}
          onclick={togglePin("universal")}
          onblur={lookAway}>universal</button
        >
        general-purpose programming language with
        <button
          class="dterm"
          onmouseenter={look("parsers")}
          onmouseleave={lookAway}
          onfocus={look("parsers")}
          onclick={togglePin("parsers")}
          onblur={lookAway}>user-definable parsers</button
        >
        and
        <button
          class="dterm"
          onmouseenter={look("typesystems")}
          onmouseleave={lookAway}
          onfocus={look("typesystems")}
          onclick={togglePin("typesystems")}
          onblur={lookAway}>type systems</button
        >, and a
        <button
          class="dterm"
          onmouseenter={look("optimizer")}
          onmouseleave={lookAway}
          onfocus={look("optimizer")}
          onclick={togglePin("optimizer")}
          onblur={lookAway}>self-optimizing optimizer</button
        >
        based on
        <button
          class="dterm"
          onmouseenter={look("nets")}
          onmouseleave={lookAway}
          onfocus={look("nets")}
          onclick={togglePin("nets")}
          onblur={lookAway}>interaction nets</button
        >
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
        <code>{CLONE_CMD}</code>
        <button
          class="copybtn"
          class:copied
          onclick={copyClone}
          title="copy to clipboard"
          aria-label="copy the clone command"
        >
          {#if copied}
            <svg viewBox="0 0 16 16" aria-hidden="true"
              ><path
                d="M3 8.5 L6.5 12 L13 4.5"
                fill="none"
                stroke="currentColor"
                stroke-width="2"
                stroke-linecap="round"
                stroke-linejoin="round"
              /></svg
            >
          {:else}
            <svg viewBox="0 0 16 16" aria-hidden="true"
              ><rect
                x="5.5"
                y="5.5"
                width="8"
                height="8"
                rx="1.5"
                fill="none"
                stroke="currentColor"
                stroke-width="1.5"
              /><path
                d="M10.5 3.5 H4 A1.5 1.5 0 0 0 2.5 5 v6.5"
                fill="none"
                stroke="currentColor"
                stroke-width="1.5"
                stroke-linecap="round"
              /></svg
            >
          {/if}
        </button>
      </div>
    </div>
    <div class="hero-viz">
      <TreeVis />
    </div>
  </div>
</section>

<RuleRail />

<!-- ============================== features ============================== -->
<section class="features container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <p class="sect-sub">
    The type system, the self-verifying kernel, and five byte-agreeing
    evaluators run today. The rest is what they were built to reach.
    <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener"
      >FOUNDATIONS.md</a
    >
    keeps honest score of which is which.
  </p>
  <div class="feat-grid">
    <div class="card feat">
      <h3>
        <span class="feat-dot" aria-hidden="true"></span>Programs are data are
        trees
      </h3>
      <p>
        Disp is homoiconic like lisp, except there is no quote/eval border to
        deal with. Programs can just take other programs as input and use the
        `triage` reduction rule to inspect them.
      </p>
    </div>
    <div class="card feat">
      <h3>
        <span class="feat-dot" aria-hidden="true"></span>Type-as-predicates
      </h3>
      <p>
        A type is a function that takes a tree and returns `true`, `false`, or
        `Err`. You can trivially implement new type theories by just writing new
        functions, but the best part is that you don't have to think about type
        systems in terms of confusing sequence calculus diagrams, <i
          >they're just programs</i
        >.
      </p>
    </div>
    <div class="card feat">
      <h3>
        <span class="feat-dot" aria-hidden="true"></span>You should be able to
        just define the parser
      </h3>
      <p>
        I've never understood why programming languages don't just allow you to
        entirely replace the parser. Well, I guess lisp and some ML languages
        (Haskell, Agda) allows you to do this kinda but not super well or not
        completely. Disp doesn't have this feature <i>yet</i>, but its pretty
        much just a matter of time at this point. The goal is to be able to
        literally change syntax or program representation with a simple dropdown
        menu. Users can make their own by defining a parser |- pretty-printer
        adjoint functor pair.
      </p>
    </div>
    <div class="card feat">
      <h3>
        <span class="feat-dot" aria-hidden="true"></span>Optimize
        <a href="https://www.youtube.com/watch?v=VtzvlXL9gXk">ZA WARUDO</a> (with
        a self-optimizing optimizer)
      </h3>
      <p>
        Idea: have tree programs compile to <a
          href="{REPO}/blob/main/OPTIMIZER.typ"
          target="_blank"
          rel="noopener">interaction nets</a
        >, and have another interaction net search the original interaction nets
        to find-and-replace certain nets with native operations. Possibly using
        <a href="https://egraphs-good.github.io/">e-graphs</a>.
      </p>
    </div>
  </div>
</section>

<hr class="keyline container" />

<!-- ============================== showcase ============================== -->
<section class="showcase container">
  <h2 class="sect-title">The features, in code</h2>
  <p class="sect-sub">
    Every block below passes the real compiler. The playground runs that same
    compiler in your tab, kernel self-verification included, so the code and the
    claims can't drift apart.
  </p>
  <div class="tabs">
    {#each showcase as ex, i}
      <button
        class="tab"
        class:active={showcaseIdx === i}
        onclick={() => (showcaseIdx = i)}
      >
        {ex.label.replace(/ \(.*\)/, "")}
      </button>
    {/each}
    <div class="tab-space"></div>
    <a
      class="btn tab-run"
      href="{base}/playground/?example={showcase[showcaseIdx].id}"
    >
      ▶ Open in playground
    </a>
  </div>
  <pre class="show-code">{@html highlightDisp(
      showcase[showcaseIdx].source.trim(),
    )}</pre>
</section>

<hr class="keyline container" />

<!-- ============================== self-verify band ============================== -->
<section class="selfcheck container">
  <h2 class="sect-title">No pre-checked kernel ships</h2>
  <p class="sect-sub">
    When the playground boots the type system, your browser re-elaborates the
    kernel's fragments in dependency order and re-verifies every typed export
    through the kernel itself. The last thing checked is the universe, by the
    universe.
  </p>
  <div
    class="chain"
    role="img"
    aria-label="kernel bootstrap chain: {kernelChain.join(', ')}"
  >
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
      <p>
        A first-principles walkthrough: from five rewrite rules to a
        self-verifying universe.
      </p>
      <span class="inv-cta">Start reading →</span>
    </a>
    <a class="card inv" href={REPO} target="_blank" rel="noopener">
      <h3>Read the source</h3>
      <p>
        The kernel is ~1k lines of literate disp. The docs rate every file's
        quality and origin, honestly.
      </p>
      <span class="inv-cta">github.com/libdither/disp ↗</span>
    </a>
    <a class="card inv" href="{base}/funding/">
      <h3>Support the work</h3>
      <p>
        Independent research, public domain output. Every bit of support extends
        the runway.
      </p>
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
    background: radial-gradient(
        38% 45% at 18% 22%,
        color-mix(in oklab, var(--g1) 20%, transparent),
        transparent 70%
      ),
      radial-gradient(
        42% 50% at 78% 18%,
        color-mix(in oklab, var(--g4) 16%, transparent),
        transparent 70%
      ),
      radial-gradient(
        50% 55% at 60% 85%,
        color-mix(in oklab, var(--blossom) 14%, transparent),
        transparent 70%
      ),
      radial-gradient(
        35% 40% at 38% 60%,
        color-mix(in oklab, var(--g3) 12%, transparent),
        transparent 70%
      );
    animation: drift 26s ease-in-out infinite alternate;
    pointer-events: none;
  }
  @keyframes drift {
    to {
      transform: translate3d(2.5%, -2%, 0) scale(1.06) rotate(1.2deg);
    }
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
  /* the wordmark and the definition box share a row: words left, box right */
  .head-row {
    display: flex;
    align-items: stretch;
    justify-content: space-between;
    gap: 1.2rem;
    margin-bottom: 0.4rem;
  }
  .head-words {
    flex: none;
  }
  .defbox {
    position: relative;
    flex: 1;
    max-width: 270px;
    min-height: 176px;
    align-self: center;
    background: color-mix(in oklab, var(--g4) 5%, var(--bg-elev));
    border: 1px solid var(--border-strong);
    border-radius: 12px;
    padding: 0.75rem 0.95rem;
    box-shadow: var(--shadow-soft);
    transform: rotate(1.1deg);
    transition:
      transform 0.25s ease,
      border-color 0.25s ease;
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }
  .defbox.looking {
    transform: rotate(0deg);
    border-color: var(--g2);
  }
  /* pinned: the tack goes in and the note tilts the other way */
  .defbox.pinned {
    transform: rotate(-1.3deg) scale(1.02);
    border-color: var(--g2);
    box-shadow: var(--shadow-lift);
  }
  .defbox.pinned::before {
    content: "";
    position: absolute;
    top: -6px;
    left: 50%;
    transform: translateX(-50%);
    width: 13px;
    height: 13px;
    border-radius: 50%;
    background: radial-gradient(
      circle at 35% 32%,
      #f0d9a8,
      #c99a3e 55%,
      #8a6414
    );
    border: 1px solid #8a6414;
    box-shadow: 0 2px 3px rgba(47, 74, 55, 0.35);
  }
  .def-head {
    display: flex;
    align-items: baseline;
    gap: 0.5em;
    border-bottom: 1px solid var(--border);
    padding-bottom: 0.3rem;
  }
  .def-term {
    font-family: var(--font-display);
    font-variation-settings:
      "SOFT" 60,
      "WONK" 1;
    font-weight: 620;
    font-size: 1.02rem;
    color: var(--fg);
    line-height: 1.2;
  }
  .def-pos {
    font-family: var(--font-display);
    font-style: italic;
    color: var(--fg-faint);
    font-size: 0.85rem;
  }
  .def-text {
    flex: 1;
    margin: 0.45rem 0 0;
    font-size: 0.77rem;
    line-height: 1.5;
    color: var(--fg-muted);
    white-space: pre-line; /* \n in an entry renders as a line break */
  }
  .def-text :global(a) {
    color: var(--accent);
  }
  .def-text :global(em) {
    color: var(--fg);
  }
  .def-text :global(code) {
    font-size: 0.92em;
    white-space: nowrap;
  }
  .def-text :global(p) {
    margin: 0 0 0.4rem;
  }
  .def-text :global(ul) {
    margin: 0.15rem 0 0;
    padding-left: 1.15em;
    white-space: normal;
  }
  .def-text :global(li) {
    margin: 0;
    line-height: 1.45;
  }
  h1 {
    font-size: clamp(4.2rem, 9vw, 6.8rem);
    margin: 0;
    line-height: 0.95;
    font-variation-settings:
      "SOFT" 80,
      "WONK" 1;
    font-weight: 640;
    letter-spacing: -0.02em;
  }
  .abbr {
    font-family: var(--font-display);
    font-size: clamp(1.15rem, 2.2vw, 1.5rem);
    font-style: italic;
    font-variation-settings:
      "SOFT" 60,
      "WONK" 1;
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
  .cta-row {
    display: flex;
    gap: 0.8rem;
    flex-wrap: wrap;
    margin-bottom: 1.4rem;
  }
  .cta-row.center {
    justify-content: center;
    margin-top: 1.6rem;
  }
  .copybtn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    flex: none;
    background: none;
    border: 1px solid var(--border);
    border-radius: 6px;
    color: var(--fg-faint);
    cursor: pointer;
    padding: 3px;
    transition: all 0.15s ease;
  }
  .copybtn:hover {
    color: var(--g2);
    border-color: var(--g2);
  }
  .copybtn.copied {
    color: var(--g2);
    border-color: var(--g2);
    background: color-mix(in oklab, var(--g1) 12%, transparent);
  }
  .copybtn svg {
    width: 100%;
    height: 100%;
  }
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
  .term-dollar {
    color: var(--g1);
    font-family: var(--font-mono);
  }
  .term code {
    white-space: nowrap;
  }
  .hero-viz {
    min-width: 0;
    border: 1px solid var(--border);
    border-radius: 16px;
    background: radial-gradient(
        120% 130% at 50% 0%,
        color-mix(in oklab, var(--g3) 7%, transparent),
        transparent 60%
      ),
      var(--bg-panel);
    padding: 1rem 0.6rem 0.4rem;
  }

  /* ---------- features ---------- */
  .features {
    padding-top: 3.4rem;
  }
  .sect-title {
    font-size: clamp(1.9rem, 4vw, 2.6rem);
    margin: 0 0 0.5rem;
  }
  .sect-sub {
    color: var(--fg-muted);
    max-width: 44rem;
    margin-top: 0;
  }
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
  .feat p {
    color: var(--fg-muted);
    font-size: 0.94rem;
    margin: 0;
  }
  .feat-dot {
    width: 10px;
    height: 10px;
    border-radius: 50% 50% 50% 2px;
    background: var(--grad-brand);
    flex: none;
    transform: rotate(45deg);
  }
  .features,
  .showcase,
  .selfcheck,
  .involve {
    padding-block: 3.2rem;
  }

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
  .tab:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .tab.active {
    color: var(--fg);
    border-color: var(--border-strong);
    background: var(--bg-panel);
    box-shadow: inset 0 -2px 0 -0.5px var(--accent);
  }
  .tab-space {
    flex: 1;
  }
  .tab-run {
    font-size: 0.84rem;
    padding: 0.4em 1em;
  }
  .show-code {
    font-size: 0.8rem;
    max-height: 430px;
    overflow: auto;
    line-height: 1.6;
  }

  /* ---------- self-check band ---------- */
  .selfcheck {
    text-align: center;
  }
  .selfcheck .sect-sub {
    margin-inline: auto;
  }
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
    0%,
    100% {
      border-color: var(--border-strong);
      box-shadow: none;
    }
    8% {
      border-color: var(--g2);
      box-shadow: 0 0 14px -2px color-mix(in oklab, var(--g2) 55%, transparent);
    }
    16% {
      border-color: var(--border-strong);
      box-shadow: none;
    }
  }
  .chev {
    color: var(--fg-faint);
  }
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
  a.inv {
    display: block;
    text-decoration: none !important;
    transition:
      transform 0.15s ease,
      border-color 0.15s ease;
  }
  a.inv:hover {
    transform: translateY(-3px);
    border-color: var(--fg-faint);
  }
  .inv h3 {
    margin: 0 0 0.4rem;
    font-size: 1.15rem;
    color: var(--fg);
  }
  .inv p {
    color: var(--fg-muted);
    font-size: 0.9rem;
    min-height: 3.2em;
  }
  .inv-cta {
    color: var(--accent);
    font-size: 0.9rem;
  }

  /* ---------- responsive ---------- */
  @media (max-width: 880px) {
    .hero-grid {
      grid-template-columns: 1fr;
    }
    .hero-viz {
      order: -1;
    }
    .feat-grid,
    .inv-grid {
      grid-template-columns: 1fr;
    }
    /* the definition box drops under the wordmark on small screens */
    .head-row {
      flex-direction: column;
      align-items: stretch;
    }
    .defbox {
      max-width: none;
      min-height: 0;
      transform: none;
    }
  }
</style>
