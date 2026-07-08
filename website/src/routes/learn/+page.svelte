<script lang="ts">
  import { onMount } from 'svelte'
  import { toc } from '$lib/learn/toc'
  import RuleRail from '$lib/components/RuleRail.svelte'
  import Ch1Motivation from '$lib/learn/chapters/Ch1Motivation.svelte'
  import Ch2Trees from '$lib/learn/chapters/Ch2Trees.svelte'
  import Ch3Source from '$lib/learn/chapters/Ch3Source.svelte'
  import Ch4Predicates from '$lib/learn/chapters/Ch4Predicates.svelte'
  import Ch5Kernel from '$lib/learn/chapters/Ch5Kernel.svelte'
  import Ch6Library from '$lib/learn/chapters/Ch6Library.svelte'
  import Ch7Future from '$lib/learn/chapters/Ch7Future.svelte'
  import Ch8References from '$lib/learn/chapters/Ch8References.svelte'
  import Ch9Glossary from '$lib/learn/chapters/Ch9Glossary.svelte'

  let activeId = $state('')
  let progress = $state(0)
  let navOpen = $state(false)

  onMount(() => {
    const headings = Array.from(document.querySelectorAll<HTMLElement>('.article [id]'))
    const onScroll = () => {
      const y = window.scrollY + 120
      let current = ''
      for (const h of headings) if (h.offsetTop <= y) current = h.id
      activeId = current
      const doc = document.documentElement
      const total = doc.scrollHeight - window.innerHeight
      progress = total > 0 ? Math.min(100, (window.scrollY / total) * 100) : 0
    }
    window.addEventListener('scroll', onScroll, { passive: true })
    onScroll()
    return () => window.removeEventListener('scroll', onScroll)
  })

  const isActiveSection = (id: string, children?: { id: string }[]) =>
    activeId === id || (children?.some((c) => c.id === activeId) ?? false)
</script>

<svelte:head>
  <title>Learn — disp</title>
  <meta
    name="description"
    content="A first-principles walkthrough of disp: from five rewrite rules over binary trees to a dependently-typed language whose universe passes its own checker."
  />
</svelte:head>

<!-- the leaf turns as you read: spring green → summer → gold → russet -->
<div class="progressbar" style="width:{progress}%" aria-hidden="true">
  <svg class="tipleaf" viewBox="0 0 16 16" style="transform: rotate({progress * 2.2 - 20}deg)">
    <path
      d="M8,1 C12.5,4 12.5,10 8,15 C3.5,10 3.5,4 8,1 Z"
      fill={progress < 40 ? '#58b368' : progress < 65 ? '#2f9e6e' : progress < 85 ? '#e2b04a' : '#d97b4f'}
    />
    <path d="M8,4 L8,12" stroke="rgba(255,255,255,0.6)" stroke-width="1" />
  </svg>
</div>

<div class="learn container">
  <button class="nav-toggle" onclick={() => (navOpen = !navOpen)} aria-expanded={navOpen}>
    {navOpen ? '✕ close' : '☰ contents'}
  </button>

  <nav class="sidebar" class:open={navOpen} aria-label="table of contents">
    <div class="side-inner">
      <span class="side-title">Contents</span>
      <ol>
        {#each toc as entry, i}
          <li class:active-branch={isActiveSection(entry.id, entry.children)}>
            <a
              href="#{entry.id}"
              class:active={activeId === entry.id}
              onclick={() => (navOpen = false)}
            >
              <span class="num">{i + 1}</span>
              {entry.title}
            </a>
            {#if entry.children && isActiveSection(entry.id, entry.children)}
              <ol class="sub">
                {#each entry.children as c}
                  <li>
                    <a href="#{c.id}" class:active={activeId === c.id} onclick={() => (navOpen = false)}>
                      {c.title}
                    </a>
                  </li>
                {/each}
              </ol>
            {/if}
          </li>
        {/each}
      </ol>
      <div class="side-foot">
        <span class="pct">{progress.toFixed(0)}%</span> read
      </div>
      <RuleRail inline />
    </div>
  </nav>

  <article class="article">
    <header class="art-head">
      <h1>disp, <span class="grad-text">from first principles</span></h1>
      <p class="subtitle">
        From five rewrite rules over binary trees to a dependently-typed language whose universe
        passes its own checker. Code blocks marked <span class="runmark">▶</span> run against the
        real compiler, in your tab.
      </p>
      <p class="provenance">
        Updated for the current two-operation kernel. This walkthrough is pedagogical commentary —
        <a href="https://github.com/libdither/disp/blob/main/TYPE_THEORY.typ">TYPE_THEORY.typ</a>
        and the <code>lib/kernel/*.disp</code> sources are the ground truth.
      </p>
    </header>

    <Ch1Motivation />
    <Ch2Trees />
    <Ch3Source />
    <Ch4Predicates />
    <Ch5Kernel />
    <Ch6Library />
    <Ch7Future />
    <Ch8References />
    <Ch9Glossary />
  </article>
</div>

<style>
  .progressbar {
    position: fixed;
    top: 0;
    left: 0;
    height: 3px;
    background: var(--grad-seasons);
    background-size: 100vw 100%;
    z-index: 200;
    transition: width 0.1s linear;
  }
  .tipleaf {
    position: absolute;
    right: -7px;
    top: 0.5px;
    width: 15px;
    height: 15px;
    transition: transform 0.2s ease;
  }

  .learn {
    display: grid;
    grid-template-columns: 250px minmax(0, 1fr);
    gap: clamp(1.5rem, 4vw, 3.5rem);
    align-items: start;
    padding-top: 2rem;
  }

  /* ---- sidebar (LessWrong-style) ---- */
  .sidebar {
    position: sticky;
    top: calc(var(--nav-h) + 1rem);
    max-height: calc(100dvh - var(--nav-h) - 2rem);
    overflow-y: auto;
    font-size: 0.84rem;
  }
  .side-title {
    display: block;
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 0.12em;
    color: var(--fg-faint);
    margin-bottom: 0.7rem;
  }
  .sidebar ol {
    list-style: none;
    margin: 0;
    padding: 0;
  }
  .sidebar li { margin: 0.1rem 0; }
  .sidebar a {
    display: flex;
    gap: 0.5em;
    color: var(--fg-muted);
    text-decoration: none !important;
    padding: 0.24rem 0.5rem;
    border-radius: 6px;
    border-left: 2px solid transparent;
    line-height: 1.35;
    transition: color 0.12s ease, background 0.12s ease;
  }
  .sidebar a:hover { color: var(--fg); background: var(--bg-panel-hover); }
  .sidebar a.active {
    color: var(--fg);
    background: var(--bg-panel);
    border-left-color: var(--accent);
  }
  .active-branch > a { color: var(--fg); }
  .num { color: var(--fg-faint); font-family: var(--font-mono); font-size: 0.76em; padding-top: 0.15em; }
  .sub { padding-left: 1.5rem !important; margin: 0.1rem 0 0.35rem !important; }
  .sub a { font-size: 0.8rem; padding: 0.18rem 0.5rem; }
  .side-foot {
    margin-top: 1.2rem;
    padding-top: 0.8rem;
    border-top: 1px solid var(--border);
    color: var(--fg-faint);
    font-size: 0.75rem;
  }
  .pct { color: var(--g2); font-family: var(--font-mono); }
  .nav-toggle { display: none; }

  /* ---- article ---- */
  .article {
    max-width: 760px;
    padding-bottom: 4rem;
    font-size: 1.02rem;
  }
  .art-head { margin-bottom: 2.5rem; }
  .art-head h1 {
    font-size: clamp(2rem, 5vw, 3rem);
    margin: 0 0 0.6rem;
    line-height: 1.1;
  }
  .subtitle { color: var(--fg-muted); font-size: 1.08rem; margin: 0 0 0.6rem; }
  .runmark { color: var(--accent); font-weight: 700; }
  .provenance { color: var(--fg-faint); font-size: 0.85rem; font-style: italic; }

  .article :global(section) { margin-bottom: 3.2rem; }
  .article :global(h2) {
    font-size: 1.75rem;
    margin: 2.8rem 0 0.8rem;
    padding-bottom: 0.35rem;
    border-bottom: 1px solid var(--border);
    scroll-margin-top: calc(var(--nav-h) + 1rem);
  }
  .article :global(h2 .secnum) {
    color: var(--fg-faint);
    font-size: 0.95rem;
    font-family: var(--font-mono);
    display: block;
    letter-spacing: 0.06em;
    margin-bottom: 0.2rem;
  }
  .article :global(h3) {
    font-size: 1.22rem;
    margin: 2rem 0 0.5rem;
    color: var(--g2);
    scroll-margin-top: calc(var(--nav-h) + 1rem);
  }
  .article :global(p) { color: var(--fg); }
  .article :global(p), .article :global(li) { line-height: 1.72; }
  .article :global(table) {
    border-collapse: collapse;
    width: 100%;
    margin: 1.2rem 0;
    font-size: 0.9rem;
  }
  .article :global(th) {
    text-align: left;
    color: var(--fg-muted);
    font-weight: 600;
    font-size: 0.8rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
  }
  .article :global(th), .article :global(td) {
    border-bottom: 1px solid var(--border);
    padding: 0.5rem 0.7rem;
  }
  .article :global(td:first-child), .article :global(th:first-child) { padding-left: 0; }
  .article :global(.math) {
    font-family: 'STIX Two Math', 'Cambria Math', 'Times New Roman', serif;
    font-style: italic;
  }
  .article :global(.math-block) {
    text-align: center;
    font-family: 'STIX Two Math', 'Cambria Math', serif;
    font-size: 1.15em;
    margin: 1.4rem 0;
    color: var(--fg);
  }
  .article :global(blockquote) {
    border-left: 3px solid var(--g3);
    margin: 1.2rem 0;
    padding: 0.2rem 0 0.2rem 1.1rem;
    color: var(--fg-muted);
    font-style: italic;
  }
  .article :global(blockquote .cite) {
    display: block;
    font-style: normal;
    font-size: 0.82rem;
    color: var(--fg-faint);
    margin-top: 0.4rem;
  }

  @media (max-width: 980px) {
    .learn { grid-template-columns: 1fr; }
    .nav-toggle {
      display: block;
      position: fixed;
      bottom: 1.2rem;
      right: 1.2rem;
      z-index: 150;
      background: var(--bg-elev);
      border: 1px solid var(--border-strong);
      color: var(--fg);
      border-radius: 999px;
      padding: 0.6em 1.1em;
      font: inherit;
      font-size: 0.85rem;
      cursor: pointer;
      box-shadow: var(--shadow-lift);
    }
    .sidebar {
      display: none;
      position: fixed;
      inset: auto 1rem 4.5rem 1rem;
      max-height: 60dvh;
      background: var(--bg-elev);
      border: 1px solid var(--border-strong);
      border-radius: 14px;
      padding: 1rem;
      z-index: 140;
      box-shadow: var(--shadow-lift);
    }
    .sidebar.open { display: block; }
  }
</style>
