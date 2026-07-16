<script lang="ts">
  import { flushSync } from "svelte";
  import { base } from "$app/paths";
  import HeroCard from "$lib/components/HeroCard.svelte";
  import { highlightDisp } from "$lib/editor/highlight";
  import { examples } from "$lib/disp/examples";

  const REPO = "https://github.com/libdither/disp";
  const JAY = "https://github.com/barry-jay-personal";
  const TREECALC = "https://treecalcul.us/";

  // ---- hero card modes ----
  // theatre reflows the hero grid (headline | description | field guide across
  // the top, card full-width below); the swap is one class toggle animated by
  // the View Transitions API where available (instant elsewhere / under
  // prefers-reduced-motion), and the viewport follows: entering scrolls the
  // now-lower card into view, exiting returns to the hero top. flipped turns
  // the card over to the visualizer (HeroCard randomizes the initial face).
  let theatre = $state(false);
  let flipped = $state(false);
  let heroEl: HTMLElement | undefined = $state();
  let cardEl: HTMLDivElement | undefined = $state();

  function toggleTheatre(): void {
    const entering = !theatre;
    const reduce = window.matchMedia(
      "(prefers-reduced-motion: reduce)",
    ).matches;
    // Scroll the WINDOW explicitly — never scrollIntoView: the hero section is
    // overflow:hidden, which makes it a programmatically-scrollable ancestor,
    // and scrollIntoView would scroll the hero's own content out of view with
    // no user-visible way to scroll it back.
    const scroll = (behavior: ScrollBehavior) => {
      const el = entering ? cardEl : heroEl;
      if (!el) return;
      // entering leaves extra headroom for the face picker hanging above the card
      const offset =
        (document.querySelector("header")?.clientHeight ?? 64) +
        (entering ? 62 : 12);
      window.scrollTo({
        top: Math.max(
          0,
          el.getBoundingClientRect().top + window.scrollY - offset,
        ),
        behavior,
      });
    };
    const apply = () => flushSync(() => (theatre = entering));
    const doc = document as Document & {
      startViewTransition?: (cb: () => void) => unknown;
    };
    if (doc.startViewTransition && !reduce) {
      // scroll inside the transition callback: snapshots are captured after
      // the jump, so the morph carries the camera move in one motion
      doc.startViewTransition(() => {
        apply();
        scroll("auto");
      });
    } else {
      apply();
      scroll(reduce ? "auto" : "smooth");
    }
  }

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
      html: "Eliezer Yudkowsky called me and said this was probably a bad idea but idk man, I'd rather my recursive self-improvement loop be interpretable than whatever Anthropic and OpenAI be up to.",
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
<section class="hero" bind:this={heroEl}>
  <div class="aurora" aria-hidden="true"></div>
  <div class="container hero-grid" class:theatre>
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
    <div class="hero-sub">
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
        <a class="btn" href="{base}/learn/">Read the Docs</a>
        <!-- ghost of the longer label keeps the button width constant, so
             flipping never reflows the row (in theatre that reflow used to
             shift the card below) -->
        <button class="btn flip-cta" onclick={() => (flipped = !flipped)}>
          <span class="flip-label ghost" aria-hidden="true">
            Visualize the Tree Calculus <span class="btn-arrow">⟶</span>
          </span>
          <span class="flip-label">
            {flipped ? "See the code" : "Visualize the Tree Calculus"}
            <span class="btn-arrow" aria-hidden="true">⟶</span>
          </span>
        </button>
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
    <div class="hero-code" bind:this={cardEl}>
      <HeroCard {theatre} bind:flipped onToggleTheatre={toggleTheatre} />
    </div>
  </div>
</section>

<!-- ============================== features ============================== -->
<section class="features container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <p class="sect-sub">
    <i
      >Because when AGI comes and our code isn't formally verified yet, we're
      gonna be screwed.</i
    >
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
        <a href="https://egraphs-good.github.io/">e-graphs</a>. Then buy a
        dedicated hyper-parallel chip that basically runs a cellular automata
        specialized to simulate agent reduction where agents are connected by
        other agents through the physical space of the chip and reduce at a
        billion parallel reductions per second or something.
      </p>
    </div>
  </div>
</section>

<hr class="keyline container" />

<!-- ============================== showcase ============================== -->
<section class="showcase container">
  <h2 class="sect-title">Examples</h2>
  <p class="sect-sub">Click open in playground to check them out!</p>
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

<style>
  /* ---------- hero ---------- */
  .hero {
    position: relative;
    /* clip, NOT hidden: hidden makes the section a programmatically
       scrollable container, and any scroll-reveal (focus, find-in-page,
       automation) can wedge its content permanently out of view */
    overflow: clip;
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
  /* Four direct children (head-words, defbox, hero-sub, hero-code) on named
     areas, so theatre mode is ONE template swap — animated by the View
     Transitions API (each block carries a view-transition-name below). */
  .hero-grid {
    position: relative;
    display: grid;
    grid-template-columns: minmax(0, 4fr) minmax(0, 3fr) minmax(0, 6fr);
    grid-template-areas:
      "head def  card"
      "sub  sub  card";
    grid-template-rows: auto 1fr;
    gap: 1.1rem 2.5rem;
    align-items: start;
  }
  .hero-grid.theatre {
    grid-template-columns: minmax(0, auto) minmax(0, 1fr) minmax(0, 19rem);
    grid-template-areas:
      "head sub  def"
      "card card card";
    grid-template-rows: auto auto;
    /* wider row gap: the face picker hangs above the card in theatre */
    gap: 3.4rem 2.5rem;
  }
  .head-words {
    grid-area: head;
  }
  .defbox {
    grid-area: def;
    justify-self: end;
  }
  .hero-sub {
    grid-area: sub;
    min-width: 0;
  }
  .hero-code {
    grid-area: card;
    align-self: stretch;
    min-width: 0;
  }
  .theatre .hero-code {
    height: min(72vh, 46rem);
  }
  /* theatre trims the hero-sub column down to the pitch: no clone chip */
  .hero-grid.theatre .term {
    display: none;
  }
  .btn-arrow {
    font-size: 1.05em;
    line-height: 1;
    transform: translateY(0.02em);
  }
  /* both labels share the button's grid cell; the invisible long one sets the width */
  .flip-cta {
    display: inline-grid;
    justify-items: center;
  }
  .flip-label {
    grid-area: 1 / 1;
    display: inline-flex;
    align-items: center;
    gap: 0.5em;
    white-space: nowrap;
  }
  .flip-label.ghost {
    visibility: hidden;
  }
  .head-words {
    view-transition-name: hero-head;
  }
  .defbox {
    view-transition-name: hero-def;
  }
  .hero-sub {
    view-transition-name: hero-sub;
  }
  .hero-code {
    view-transition-name: hero-card;
  }
  :global(::view-transition-group(hero-head)),
  :global(::view-transition-group(hero-def)),
  :global(::view-transition-group(hero-sub)),
  :global(::view-transition-group(hero-card)) {
    animation-duration: 0.45s;
    animation-timing-function: cubic-bezier(0.35, 0.1, 0.22, 1);
  }
  /* the card morphs size: let both snapshots fill the group box */
  :global(::view-transition-old(hero-card)),
  :global(::view-transition-new(hero-card)) {
    height: 100%;
    object-fit: cover;
    overflow: hidden;
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
    position: relative;
    width: 100%;
    max-width: 270px;
    min-height: 176px;
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
  }
  .term-dollar {
    color: var(--g1);
    font-family: var(--font-mono);
    flex: none;
  }
  /* the command scrolls; the copy button stays put */
  .term code {
    white-space: nowrap;
    overflow-x: auto;
    min-width: 0;
    flex: 1;
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
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 1.1rem;
    margin-top: 1.6rem;
  }
  .feat h3 {
    margin: 0 0 0.5rem;
    font-size: 1.25rem;
  }
  .feat p {
    color: var(--fg-muted);
    font-size: 0.94rem;
    margin: 0;
  }
  .feat-dot {
    display: inline-block;
    width: 10px;
    height: 10px;
    margin-right: 0.45em;
    border-radius: 50% 50% 50% 2px;
    background: var(--grad-brand);
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
    grid-template-columns: repeat(3, minmax(0, 1fr));
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
    /* one column in either mode; theatre only adds height to the card */
    .hero-grid,
    .hero-grid.theatre {
      grid-template-columns: minmax(0, 1fr);
      grid-template-areas:
        "head"
        "def"
        "sub"
        "card";
      grid-template-rows: none;
    }
    .hero-code {
      height: min(70vh, 34rem);
    }
    .feat-grid,
    .inv-grid {
      grid-template-columns: minmax(0, 1fr);
    }
    /* the definition box drops under the wordmark on small screens */
    .defbox {
      max-width: none;
      min-height: 0;
      transform: none;
      justify-self: stretch;
    }
  }
</style>
