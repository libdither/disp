<script lang="ts">
  import "@fontsource-variable/fraunces";
  import "@fontsource-variable/inter";
  import "@fontsource-variable/jetbrains-mono";
  import "../app.css";
  import { base } from "$app/paths";
  import { page } from "$app/state";

  let { children } = $props();

  const REPO = "https://github.com/libdither/disp";

  const links = [
    { label: "Learn", href: `${base}/learn/` },
    { label: "Playground", href: `${base}/playground/` },
    { label: "Visualizer", href: `${base}/visualizer/` },
    { label: "Git", href: REPO, external: true },
    { label: "Funding", href: `${base}/funding/` },
  ];

  const isActive = (href: string) =>
    !href.startsWith("http") && page.url.pathname === href;
</script>

<div class="shell">
  <header class="nav">
    <nav class="container navbar">
      <a class="brand" href="{base}/" aria-label="disp home">
        <svg class="glyph" viewBox="0 0 32 32" aria-hidden="true">
          <defs>
            <linearGradient id="bg1" x1="0" y1="0" x2="1" y2="1">
              <stop offset="0" stop-color="var(--g1)" />
              <stop offset="0.55" stop-color="var(--g2)" />
              <stop offset="1" stop-color="var(--g3)" />
            </linearGradient>
          </defs>
          <g
            stroke="var(--bark)"
            stroke-width="2.2"
            stroke-linecap="round"
            fill="none"
          >
            <path
              d="M16 25 L16 15 M16 17 C16 13 11 12 9 9 M16 19 C16 15 21 14 23 11"
            />
          </g>
          <circle cx="9" cy="9" r="3.2" fill="url(#bg1)" />
          <circle cx="23" cy="11" r="3.2" fill="url(#bg1)" />
          <circle cx="16" cy="13" r="2.6" fill="url(#bg1)" />
        </svg>
        <span class="wordmark">disp</span>
      </a>
      <div class="navlinks">
        {#each links as l}
          {#if l.external}
            <a class="navlink" href={l.href} target="_blank" rel="noopener">
              {l.label}
            </a>
          {:else}
            <a class="navlink" class:active={isActive(l.href)} href={l.href}>
              {l.label}
              <svg class="sprout" viewBox="0 0 24 12" aria-hidden="true">
                <path class="stem" d="M12 12 C12 8 12 7 12 5" />
                <path
                  class="leaf l1"
                  d="M12 7 C9.5 6.5 8 4.5 7.6 2.4 C10 2.8 11.7 4.4 12 7 Z"
                />
                <path
                  class="leaf l2"
                  d="M12 6 C14.3 5.4 15.7 3.8 16.2 1.6 C13.9 2 12.3 3.6 12 6 Z"
                />
              </svg>
            </a>
          {/if}
        {/each}
      </div>
    </nav>
  </header>

  <main class="content">
    {@render children()}
  </main>

  <footer class="footer">
    <svg
      class="treeline"
      viewBox="0 0 1200 46"
      preserveAspectRatio="none"
      aria-hidden="true"
    >
      <path
        d="M0,46 L0,34 Q20,30 32,20 Q44,30 60,32 L70,26 Q84,32 100,33 Q114,12 128,10 Q142,12 152,32 L170,34 Q186,28 198,16 Q210,28 228,32 L250,33 Q268,24 278,8 Q290,22 306,30 L330,33 Q348,29 360,22 Q374,30 392,32 L410,30 Q424,14 436,12 Q450,16 462,31 L484,33 Q500,26 512,18 Q526,28 544,32 L566,33 Q580,24 592,10 Q606,22 620,30 L644,32 Q660,28 672,20 Q686,29 702,32 L724,33 Q740,16 752,13 Q766,17 776,31 L798,33 Q814,27 826,18 Q838,28 856,32 L878,33 Q894,25 904,9 Q918,21 934,30 L958,32 Q974,28 986,21 Q1000,29 1018,32 L1038,30 Q1052,14 1064,12 Q1078,16 1090,31 L1112,33 Q1128,26 1140,18 Q1154,28 1172,32 L1200,34 L1200,46 Z"
        fill="color-mix(in oklab, var(--g2) 16%, var(--bg-elev))"
      />
    </svg>
    <div class="container footgrid">
      <div>
        <span class="wordmark grad-text" style="font-size:1.3rem">disp</span>
        <p class="foot-tag">
          An aspiring universal language built on tree calculus.<br />
          Public domain, under the
          <a href="{REPO}/blob/main/LICENSE">Unlicense</a>.
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
        <a href={REPO} target="_blank" rel="noopener">Source</a>
        <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener"
          >Foundations</a
        >
        <a href="{REPO}/blob/main/GOALS.md" target="_blank" rel="noopener"
          >Goals</a
        >
        <a href="{base}/funding/">Funding</a>
      </div>
    </div>
  </footer>
</div>

<style>
  .shell {
    min-height: 100dvh;
    display: flex;
    flex-direction: column;
  }

  .nav {
    position: sticky;
    top: 0;
    z-index: 100;
    /* solid tint, no backdrop-filter: blur on a sticky bar makes Chrome's
       rasterizer crawl (screenshots time out, scrolling pays for it too) */
    background: color-mix(in oklab, var(--bg-elev) 96%, var(--g1));
    border-bottom: 1px solid var(--border);
    /* own view-transition layer: page morphs (theatre mode) must never
       animate OVER the nav — see the group z-index below */
    view-transition-name: site-nav;
  }
  :global(::view-transition-group(site-nav)) {
    z-index: 100;
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
  .glyph {
    width: 27px;
    height: 27px;
  }
  .wordmark {
    font-family: var(--font-display);
    font-variation-settings:
      "SOFT" 70,
      "WONK" 1;
    font-weight: 620;
    font-size: 1.45rem;
    color: var(--fg);
    letter-spacing: -0.01em;
  }
  .navlinks {
    display: flex;
    align-items: center;
    gap: clamp(0.2rem, 1.6vw, 0.9rem);
  }
  .navlink {
    position: relative;
    color: var(--fg-muted);
    font-size: 0.95rem;
    font-weight: 520;
    padding: 0.4rem 0.65rem;
    border-radius: 8px;
    text-decoration: none !important;
    transition:
      color 0.15s ease,
      background 0.15s ease;
    white-space: nowrap;
  }
  .navlink:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .navlink.active {
    color: var(--fg);
  }

  /* the active tab grows a little sprout */
  .sprout {
    position: absolute;
    left: 50%;
    bottom: -7px;
    width: 22px;
    height: 11px;
    transform: translateX(-50%) scale(0);
    transform-origin: 50% 100%;
    transition: transform 0.35s cubic-bezier(0.34, 1.56, 0.64, 1);
    pointer-events: none;
  }
  .sprout .stem {
    stroke: var(--g2);
    stroke-width: 1.6;
    fill: none;
    stroke-linecap: round;
  }
  .sprout .leaf {
    fill: var(--g1);
  }
  .sprout .l2 {
    fill: var(--g2);
  }
  .navlink:hover .sprout {
    transform: translateX(-50%) scale(0.6);
  }
  .navlink.active .sprout {
    transform: translateX(-50%) scale(1);
  }

  .content {
    flex: 1;
    display: flex;
    flex-direction: column;
  }

  .footer {
    position: relative;
    border-top: 1px solid var(--border);
    padding: 2.6rem 0 3rem;
    margin-top: 4rem;
    background: radial-gradient(
        60% 120% at 15% 100%,
        color-mix(in oklab, var(--g1) 9%, transparent),
        transparent 70%
      ),
      color-mix(in oklab, var(--g2) 5%, var(--bg-elev));
  }
  .treeline {
    position: absolute;
    left: 0;
    right: 0;
    top: -45px;
    width: 100%;
    height: 46px;
    pointer-events: none;
  }
  .footgrid {
    display: grid;
    grid-template-columns: 2fr 1fr 1fr;
    gap: 2rem;
  }
  .foot-tag {
    color: var(--fg-muted);
    font-size: 0.88rem;
    margin: 0.6rem 0 0;
  }
  .footcol {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }
  .footcol h4 {
    margin: 0 0 0.4rem;
    font-family: var(--font-body);
    font-size: 0.78rem;
    text-transform: uppercase;
    letter-spacing: 0.09em;
    color: var(--fg-faint);
  }
  .footcol a {
    color: var(--fg-muted);
    font-size: 0.92rem;
  }
  .footcol a:hover {
    color: var(--fg);
  }

  @media (max-width: 720px) {
    .footgrid {
      grid-template-columns: 1fr;
    }
    .navbar {
      flex-wrap: wrap;
      height: auto;
      min-height: var(--nav-h);
      padding-block: 0.4rem;
      justify-content: center;
      row-gap: 0.1rem;
    }
    .navlinks {
      flex-wrap: wrap;
      justify-content: center;
    }
    .navlink {
      padding: 0.3rem 0.45rem;
      font-size: 0.88rem;
    }
    .wordmark {
      font-size: 1.25rem;
    }
  }
</style>
