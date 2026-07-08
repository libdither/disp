<script lang="ts">
  import '@fontsource-variable/fraunces'
  import '@fontsource-variable/inter'
  import '@fontsource-variable/jetbrains-mono'
  import '../app.css'
  import { base } from '$app/paths'
  import { page } from '$app/state'

  let { children } = $props()

  const REPO = 'https://github.com/libdither/disp'

  const links = [
    { label: 'Learn', href: `${base}/learn/` },
    { label: 'Playground', href: `${base}/playground/` },
    { label: 'Visualizer', href: `${base}/visualizer/` },
    { label: 'Git', href: REPO, external: true },
    { label: 'Funding', href: `${base}/funding/` }
  ]

  const isActive = (href: string) =>
    !href.startsWith('http') && page.url.pathname === href
</script>

<div class="shell">
  <header class="nav">
    <nav class="container navbar">
      <a class="brand" href="{base}/" aria-label="disp home">
        <svg class="glyph" viewBox="0 0 32 32" aria-hidden="true">
          <defs>
            <linearGradient id="bg1" x1="0" y1="0" x2="1" y2="1">
              <stop offset="0" stop-color="var(--g1)" />
              <stop offset="0.5" stop-color="var(--g2)" />
              <stop offset="1" stop-color="var(--g4)" />
            </linearGradient>
          </defs>
          <g stroke="url(#bg1)" stroke-width="2.4" stroke-linecap="round" fill="none">
            <path d="M16 7 L8 24 M16 7 L24 24" />
          </g>
          <circle cx="16" cy="7" r="3.4" fill="url(#bg1)" />
          <circle cx="8" cy="24" r="3.4" fill="url(#bg1)" />
          <circle cx="24" cy="24" r="3.4" fill="url(#bg1)" />
        </svg>
        <span class="wordmark">disp</span>
      </a>
      <div class="navlinks">
        {#each links as l}
          {#if l.external}
            <a class="navlink" href={l.href} target="_blank" rel="noopener">
              {l.label}<span class="ext" aria-hidden="true">↗</span>
            </a>
          {:else}
            <a class="navlink" class:active={isActive(l.href)} href={l.href}>{l.label}</a>
          {/if}
        {/each}
      </div>
    </nav>
  </header>

  <main class="content">
    {@render children()}
  </main>

  <footer class="footer">
    <div class="container footgrid">
      <div>
        <span class="wordmark grad-text" style="font-size:1.3rem">disp</span>
        <p class="foot-tag">
          A self-verifying language built on tree calculus.<br />
          Public domain, under the <a href="{REPO}/blob/main/LICENSE">Unlicense</a>.
        </p>
      </div>
      <div class="footcol">
        <h4>Explore</h4>
        <a href="{base}/learn/">Learn</a>
        <a href="{base}/playground/">Playground</a>
        <a href="{base}/visualizer/">Visualizer</a>
      </div>
      <div class="footcol">
        <h4>Project</h4>
        <a href={REPO} target="_blank" rel="noopener">Source ↗</a>
        <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener">Foundations ↗</a>
        <a href="{REPO}/blob/main/GOALS.md" target="_blank" rel="noopener">Goals ↗</a>
        <a href="{base}/funding/">Funding</a>
      </div>
    </div>
  </footer>
</div>

<style>
  .shell { min-height: 100dvh; display: flex; flex-direction: column; }

  .nav {
    position: sticky;
    top: 0;
    z-index: 100;
    backdrop-filter: blur(14px);
    background: color-mix(in oklab, var(--bg) 78%, transparent);
    border-bottom: 1px solid var(--border);
  }
  .navbar {
    height: var(--nav-h);
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
  }
  .brand {
    display: inline-flex;
    align-items: center;
    gap: 0.55rem;
    text-decoration: none !important;
  }
  .glyph { width: 26px; height: 26px; }
  .wordmark {
    font-family: var(--font-display);
    font-variation-settings: 'SOFT' 70, 'WONK' 1;
    font-weight: 620;
    font-size: 1.45rem;
    color: var(--fg);
    letter-spacing: -0.01em;
  }
  .navlinks { display: flex; align-items: center; gap: clamp(0.2rem, 1.6vw, 0.9rem); }
  .navlink {
    color: var(--fg-muted);
    font-size: 0.95rem;
    font-weight: 520;
    padding: 0.4rem 0.65rem;
    border-radius: 8px;
    text-decoration: none !important;
    transition: color 0.15s ease, background 0.15s ease;
    white-space: nowrap;
  }
  .navlink:hover { color: var(--fg); background: var(--bg-panel-hover); }
  .navlink.active {
    color: var(--fg);
    background: var(--bg-panel);
    box-shadow: inset 0 -2px 0 0 var(--accent);
  }
  .ext { font-size: 0.75em; margin-left: 0.25em; opacity: 0.7; }

  .content { flex: 1; display: flex; flex-direction: column; }

  .footer {
    border-top: 1px solid var(--border);
    padding: 2.6rem 0 3rem;
    margin-top: 4rem;
    background:
      radial-gradient(60% 120% at 15% 100%, color-mix(in oklab, var(--g3) 7%, transparent), transparent 70%),
      var(--bg-elev);
  }
  .footgrid {
    display: grid;
    grid-template-columns: 2fr 1fr 1fr;
    gap: 2rem;
  }
  .foot-tag { color: var(--fg-muted); font-size: 0.88rem; margin: 0.6rem 0 0; }
  .footcol { display: flex; flex-direction: column; gap: 0.35rem; }
  .footcol h4 {
    margin: 0 0 0.4rem;
    font-family: var(--font-body);
    font-size: 0.78rem;
    text-transform: uppercase;
    letter-spacing: 0.09em;
    color: var(--fg-faint);
  }
  .footcol a { color: var(--fg-muted); font-size: 0.92rem; }
  .footcol a:hover { color: var(--fg); }

  @media (max-width: 720px) {
    .footgrid { grid-template-columns: 1fr; }
    .navlink { padding: 0.35rem 0.45rem; font-size: 0.88rem; }
    .wordmark { font-size: 1.25rem; }
  }
</style>
