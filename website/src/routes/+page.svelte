<script lang="ts">
  import { flushSync } from "svelte";
  import { theme } from "$lib/theme.svelte";
  import { base } from "$app/paths";
  import HeroCard from "$lib/components/HeroCard.svelte";
  import { LEVEL_WORD } from "$lib/langs/types";
  import ClauseDots from "$lib/langs/ClauseDots.svelte";
  import summary from "virtual:awesome-langs/summary";

  const REPO = "https://github.com/libdither/disp";
  const AXES_URL = summary.surveyUrl.replace(/AWESOME-LANGS\.md$/, "_AXES.md");
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

  // the field guide: one fixed card, entries swap on hover. Each entry has a
  // serious definition and a joke; the appearance menu's "funny style" picks
  // (both render, :root[data-funny] shows one, so there is no flash on load).
  // Both are trusted author-written markup: links, <em>, <code> all work,
  // and plain \n newlines render as line breaks (white-space: pre-line).
  interface Entry {
    term: string;
    pos: string;
    serious: string;
    funny: string;
  }
  const entries: Record<string, Entry> = {
    disp: {
      term: "disp",
      pos: "n.",
      serious:
        "Inspired by: <ul><li>tree calculus</li> <li>types-as-predicates</li> <li>stream types</li> <li>interaction nets</li> <li>e-graphs</li> <li>library learning</li></ul>",
      funny:
        'Ingredients: <ul><li>one leaf</li> <li>five rewrite rules</li> <li>types-as-predicates</li> <li>interaction nets</li> <li>and <a href="https://dither.link/">a dream</a>...</li></ul>',
    },
    decentralized: {
      term: "decentralized",
      pos: "adj.",
      serious:
        "When there is no need for consensus on language features, since everyone can provably transpile other people's code into their own desired style.",
      funny:
        "Is your favorite programming language being sensible and not including your new pet feature? In disp you can implement that feature yourself! Just formally prove it can transpile to/from everything else :)",
    },
    lisp: {
      term: "lisp",
      pos: "n.",
      serious:
        "Like Lisp, programs are data a program can take apart. Unlike Lisp there is no quote or eval: every value is already a tree, and the F (triage) rule reads its shape directly.",
      funny:
        "Disp is like Lisp but no quote/eval on S-expressions required, just <code>triage</code> on a tree! Honestly, who even liked S-expressions anyway, too many parentheses...",
    },
    universal: {
      term: "universal",
      pos: "adj.",
      serious:
        "A language is universal (in the sense of a <a href='https://www.youtube.com/watch?v=V9tMzmlpuYo'>universal property</a>) if all other languages can be transpiled into it. Disp aims to be fully backwards compatible by subsuming all other languages",
      funny:
        "The goal is for disp to be a singular substrate that other languages can be rebuilt in and transpiled to. Disp shall become the <em>grey goo of programming languages</em> mwahahahaHAHAHAHA",
    },
    parsers: {
      term: "user-definable parsers",
      pos: "n. pl.",
      serious:
        "Parsers are just functions! And since in disp everything parses to a tree calculus program, you can just write your own parser and pretty-printer.",
      funny:
        '"A parser for things is a function from strings to potentially a pair of that thing and its string" and in disp, compilation is just a function man...',
    },
    typesystems: {
      term: "type systems",
      pos: "n. pl.",
      serious:
        "In disp a type annotation desugars to a predicate run on a program. A type system a set of predicates, thus you can create your own type systems just by defining new predicates.",
      funny:
        "A type system is just a system of types. Types are just predicates on programs. A type system is just a collection of predicates on programs. Why does no one teach it this way?!?",
    },
    optimizer: {
      term: "self-optimizing optimizer",
      pos: "n.",
      serious:
        "The endgame of disp is to make a general-purpose optimizer that produces code as small and fast as possible, and then make it generate versions of itself; an (ideally) interpretable RSI loop",
      funny:
        "Eliezer Yudkowsky called me and said this was probably a bad idea but idk man, I'd rather my recursive self-improvement loop be interpretable than whatever Anthropic and OpenAI be up to.",
    },
    nets: {
      term: "interaction nets",
      pos: "n. pl.",
      serious:
        "A computational model that is locally-rewriting and massively parallel. The goal is to use its local nature to efficiently model hardware layouts and execution cost.",
      funny:
        'Okay, so imagine like feynman diagrams where particles are splitting apart and annihilating but in doing so they are doing computation, oh hi there <a href="https://github.com/VictorTaelin" target="_blank" rel="noopener">@VictorTaelin</a> didn\'t see you there',
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
    // the path as dispatched, not the target: a click on something the card
    // re-renders (the face switch swaps its icon) must still count as inside
    const inside = e
      .composedPath()
      .some(
        (n) =>
          n instanceof Element &&
          (n.classList.contains("defbox") || n.classList.contains("dterm")),
      );
    if (inside) return;
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
      <!-- every entry in both variants, stacked in one grid cell: the hidden
           ones keep their height, so the card is as tall as the tallest entry at
           this width and never moves on hover or on the funny switch — and each
           entry keeps its own rule right under its own term -->
      <div class="def-stack">
        {#each Object.entries(entries) as [k, e] (k)}
          <div
            class="def-entry"
            class:shown={e === entry}
            aria-hidden={e !== entry}
          >
            <span class="def-head">
              <span class="def-term">{e.term}</span>
              <span class="def-pos">{e.pos}</span>
            </span>
            <div class="def-texts">
              <div class="def-text serious">{@html e.serious}</div>
              <div class="def-text funny">{@html e.funny}</div>
            </div>
          </div>
        {/each}
      </div>
      <!-- the jokes instead of the definitions: a faint switch at the card's corner -->
      <button
        class="def-funny"
        class:on={theme.funny}
        aria-pressed={theme.funny}
        title={theme.funny ? "you saw nothing" : "pssst"}
        aria-label="funny style"
        onclick={(e) => {
          e.stopPropagation(); // never reaches the unpin-on-click-outside
          theme.setFunny(!theme.funny);
        }}
      >
        <!-- the faces are Pixelarticons' smile and laugh (MIT, halfmage/pixelarticons), inlined -->
        <svg class="emo" viewBox="0 0 24 24" aria-hidden="true">
          {#if theme.funny}
            <path
              d="M6 20h12v2H6zM6 2h12v2H6zm12 2h2v2h-2zM4 4h2v2H4zm0 14h2v2H4zm14 0h2v2h-2zM2 6h2v12H2zm18 0h2v12h-2zM7 14h2v2H7zm0-2h10v2H7zm2 4h6v2H9zm6-2h2v2h-2zM8 8h2v2H8zm6 0h2v2h-2z"
            />
          {:else}
            <path
              d="M6 20h12v2H6zM6 2h12v2H6zm12 2h2v2h-2zM4 4h2v2H4zm0 14h2v2H4zm14 0h2v2h-2zM2 6h2v12H2zm18 0h2v12h-2zM7 13h2v2H7zm2 2h6v2H9zm6-2h2v2h-2zM8 8h2v2H8zm6 0h2v2h-2z"
            />
          {/if}
        </svg>
      </button>
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

<!-- ============================== why ============================== -->
<section class="why container">
  <h2 class="sect-title">Why <span class="grad-text">disp</span>?</h2>
  <p class="goal">
    The goal of disp is a language where you write only the specification and
    the implementation is derived from it. A specification is a type, and in
    disp a type is a predicate you can run, so the type checker is an ordinary
    function from programs to a yes or a no. Multiply that verdict by a cost
    score from a model of the hardware, and "find the best program" becomes a
    search with a fitness function. Because the checker, the cost model, and the
    search are all disp programs, the search can be aimed at itself: an
    optimizer that improves its own optimizer, and stays readable while it does.
  </p>
  <p class="goal">
    Everything else about disp exists to make that loop close: the tree calculus
    underneath (programs are data, with no quote/eval border), types as
    predicates, a trusted kernel small enough to audit, and the plan for
    user-definable syntax. A survey of the neighbouring languages reduces the
    goal to five requirements and scores each of them on the same scale, disp
    included.
  </p>
  <ol class="axes">
    {#each summary.axes as ax (ax.id)}
      {@const s = summary.disp[ax.id]}
      <li>
        <span class="ax-id">{ax.id}</span>
        <div class="ax-body">
          <h3>{ax.name}</h3>
          <p>{@html ax.requiresHtml}</p>
        </div>
        <span
          class="ax-pip lv{s.level ?? 'n'}"
          style:--fill={s.pct ?? 0}
          title="disp today: {s.pct != null
            ? `${s.pct}% of the requirement, `
            : ''}{s.raw} {s.level == null
            ? 'not scored'
            : LEVEL_WORD[s.level]}{s.tag ? ` (${s.tag})` : ''}"
        >
          {#if s.clauses}
            <ClauseDots clauses={s.clauses} labels={ax.clauses} size={10} />
          {/if}
          <span class="pip-score"
            >{#if !s.clauses}{s.raw}{/if}{#if s.pct != null}<b>{s.pct}%</b
              >{/if}</span
          >
          <small
            >{s.tag ??
              (s.level == null ? "not scored" : LEVEL_WORD[s.level])}</small
          >
        </span>
      </li>
    {/each}
  </ol>
  <p class="axes-key">
    The figure on each row is where disp stands today, one dot per clause of the
    requirement: filled = met, half-filled = halfway, empty = not met, dashed =
    still open. The percent is their mean, the word is how it's reached, and
    hovering a dot names its clause. Text and scores come from
    <a href={AXES_URL} target="_blank" rel="noopener">_AXES.md</a>.
  </p>
  <div class="teaser card">
    <p>
      {summary.count} neighbouring projects are scored on the same five axes. None
      of them combines native reflection with search, the pair disp is built around,
      and disp is last on equality, where several small projects already have answers.
      Per axis, these score higher than disp:
    </p>
    <ul class="ahead-list">
      {#each summary.axes as ax (ax.id)}
        <li>
          <span class="ax-id">{ax.id}</span>
          <b>{ax.short}</b>
          <span>
            {#if summary.ahead[ax.id].length}
              {#each summary.ahead[ax.id] as l, i (l.slug)}{i ? ", " : ""}<a
                  href="{base}/compare/?lang={l.slug}">{l.name}</a
                >{/each}
            {:else}
              <span class="nobody">nobody scores higher</span>
            {/if}
          </span>
        </li>
      {/each}
    </ul>
    <a class="btn" href="{base}/compare/">
      See the full comparison <span class="btn-arrow" aria-hidden="true">⟶</span
      >
    </a>
  </div>
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
  .def-entry.shown .def-text {
    visibility: visible;
  }
  /* the other variant stays laid out (it holds the height) but unseen */
  :global(:root[data-funny="1"]) .def-text.serious,
  :global(:root:not([data-funny="1"])) .def-text.funny {
    visibility: hidden;
  }
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
  .def-stack {
    flex: 1;
    display: grid;
  }
  .def-entry {
    grid-area: 1 / 1;
    visibility: hidden;
    display: flex;
    flex-direction: column;
  }
  .def-entry.shown {
    visibility: visible;
  }
  .def-head {
    display: flex;
    align-items: baseline;
    gap: 0.5em;
    padding-right: 1.4rem; /* room for the switch at the corner */
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
  .def-funny {
    position: absolute;
    top: 0.85rem;
    right: 0.95rem;
    width: 18px;
    height: 18px;
    padding: 0;
    border: none;
    background: none;
    color: var(--fg-faint);
    opacity: 0.4;
    cursor: pointer;
    transition:
      opacity 0.15s ease,
      color 0.15s ease;
  }
  .def-funny .emo {
    display: block;
    width: 16px;
    height: 16px;
    fill: currentColor;
    shape-rendering: crispEdges; /* keep the pixels square */
  }
  .def-funny:hover,
  .def-funny:focus-visible {
    opacity: 1;
    color: var(--fg);
    outline: none;
  }
  .def-funny.on {
    opacity: 1;
    color: var(--g2);
  }
  .def-texts {
    display: grid;
    margin-top: 0.45rem;
  }
  .def-text {
    grid-area: 1 / 1;
    visibility: hidden;
    margin: 0;
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
  /* ---------- why ---------- */
  .why {
    padding-block: 3.4rem 3.2rem;
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
  .goal {
    color: var(--fg-muted);
    max-width: 46rem;
    font-size: 1.02rem;
    line-height: 1.75;
    margin: 0 0 1rem;
  }
  .axes {
    list-style: none;
    margin: 1.8rem 0 0;
    padding: 0;
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.9rem 1.4rem;
  }
  .axes li {
    display: grid;
    grid-template-columns: auto minmax(0, 1fr) auto;
    gap: 0.8rem;
    align-items: start;
    padding: 0.85rem 1rem;
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-elev);
  }
  .ax-id {
    font-family: var(--font-mono);
    font-size: 0.78rem;
    font-weight: 600;
    color: var(--accent);
    padding-top: 0.2rem;
  }
  .ax-body h3 {
    margin: 0 0 0.25rem;
    font-size: 1.05rem;
  }
  .ax-body p {
    margin: 0;
    color: var(--fg-muted);
    font-size: 0.88rem;
    line-height: 1.55;
  }
  .ax-pip {
    display: inline-flex;
    flex-direction: column;
    align-items: center;
    gap: 0.1em;
    min-width: 4rem;
    max-width: 7rem;
    text-align: center;
    padding: 0.3em 0.45em;
    border-radius: 8px;
    font-family: var(--font-mono);
    font-size: 0.95rem;
    line-height: 1.1;
  }
  .ax-pip small {
    font-family: var(--font-body);
    font-size: 0.62rem;
    color: var(--fg-faint);
  }
  .ax-pip {
    background: color-mix(
      in oklab,
      var(--accent) calc(var(--fill, 0) * 0.45%),
      transparent
    );
  }
  .pip-score {
    display: inline-flex;
    align-items: baseline;
    gap: 0.3em;
  }
  .pip-score b {
    font-family: var(--font-body);
    font-size: 0.78rem;
    font-weight: 600;
  }
  .axes-key {
    margin: 0.9rem 0 0;
    font-size: 0.8rem;
    color: var(--fg-faint);
  }
  /* the comparison teaser: the one fact worth the front page, the rest is /compare */
  .teaser {
    margin-top: 1.6rem;
    padding: 1.1rem 1.3rem 1.2rem;
  }
  .teaser > p {
    margin: 0 0 0.8rem;
    color: var(--fg-muted);
    font-size: 0.95rem;
    line-height: 1.6;
    max-width: 46rem;
  }
  .ahead-list {
    list-style: none;
    margin: 0 0 1.1rem;
    padding: 0;
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.4rem 1.6rem;
    font-size: 0.86rem;
    color: var(--fg-muted);
    line-height: 1.5;
  }
  .ahead-list li {
    display: flex;
    gap: 0.5em;
    align-items: baseline;
  }
  .ahead-list .ax-id {
    padding-top: 0;
  }
  .ahead-list b {
    flex: none;
    color: var(--fg);
    font-weight: 600;
  }
  .nobody {
    color: var(--fg-faint);
    font-style: italic;
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
    .axes,
    .ahead-list {
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
