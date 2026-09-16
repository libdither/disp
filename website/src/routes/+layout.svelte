<script lang="ts">
  import "@fontsource-variable/fraunces";
  import "@fontsource-variable/inter";
  import "@fontsource-variable/jetbrains-mono";
  import "../app.css";
  import { base } from "$app/paths";
  import { page } from "$app/state";
  import { theme, type ThemePref } from "$lib/theme.svelte";

  let { children } = $props();

  // adopt the stored preference once hydrated, then follow OS changes
  $effect(() => theme.sync());

  const THEME_LABEL = {
    system: "Theme: follows your system",
    light: "Theme: light",
    dark: "Theme: dark",
  };
  const themeLabel = (p: ThemePref) =>
    typeof p === "number" ? `Theme: ${Math.round(p * 100)}% of the way to dark` : THEME_LABEL[p];
  const THEME_OPTIONS: { value: ThemePref; label: string }[] = [
    { value: "system", label: "System" },
    { value: "light", label: "Light" },
    { value: "dark", label: "Dark" },
  ];
  // the appearance menu: a thin column of icons that appears under the theme
  // button on hover (or keyboard focus, CSS-driven). Clicking the button
  // cycles the theme in menu order; the simple style is only ever picked by
  // hand, from its own icon in the menu.
  const cycleTheme = () => theme.cycle();
  // the theme icons are a tape (the visualizer's cassette, sideways): a thumb
  // rides to the chosen icon with a bounce, and pressing anywhere on the
  // column jumps it there, then scrubs. The top cell (system) snaps; between
  // the sun and the moon the thumb is continuous and the page blends live
  // under the drag, and the thumb squares off to say so.
  // Geometry, in px from the tape's top: each icon is 30px with a 2px gap,
  // and a 64px track sits between the sun and the moon so the blend has room.
  const SUN_Y = 32;
  const MOON_Y = 130;
  const thumbY = $derived(theme.pref === "system" ? 0 : SUN_Y + (MOON_Y - SUN_Y) * theme.mix);
  const continuous = $derived(typeof theme.pref === "number");
  let scrubbing = $state(false);
  // squished while pressed on the track, a little squat while an intermediate holds
  const thumbTransform = $derived(
    `translateY(${thumbY}px) scale(${scrubbing && continuous ? "1.2, 0.72" : continuous ? "1.08, 0.86" : "1, 1"})`,
  );
  let themesEl: HTMLDivElement | undefined = $state();
  let hot = $state<number | null>(null); // the icon under the pointer
  const tapeY = (clientY: number) => clientY - (themesEl?.getBoundingClientRect().top ?? clientY);
  // the system cell snaps; from the sun's centre (0) to the moon's (1) the
  // thumb is continuous, and the last few percent snap to the words
  const prefAt = (clientY: number): ThemePref => {
    if (!themesEl) return theme.pref;
    const u = tapeY(clientY);
    if (u < SUN_Y - 1) return "system";
    const m = Math.max(0, Math.min(1, (u - (SUN_Y + 15)) / (MOON_Y - SUN_Y)));
    return m < 0.03 ? "light" : m > 0.97 ? "dark" : m;
  };
  const cellAt = (clientY: number) => {
    if (!themesEl) return null;
    const u = tapeY(clientY);
    return u < SUN_Y ? 0 : u < SUN_Y + 32 ? 1 : u >= MOON_Y - 2 ? 2 : null;
  };
  // a snap to one of the words eases the page like a click would; only the
  // continuous stretch follows the pointer instantly
  function scrubTo(clientY: number) {
    const p = prefAt(clientY);
    theme.setScrubbing(typeof p === "number");
    theme.set(p);
  }
  function themesDown(e: PointerEvent) {
    if (e.button !== 0) return;
    scrubbing = true;
    themesEl?.setPointerCapture(e.pointerId);
    scrubTo(e.clientY);
  }
  function themesMove(e: PointerEvent) {
    hot = cellAt(e.clientY);
    if (scrubbing) scrubTo(e.clientY);
  }
  // the inks cross polarity around the middle of the track and contrast
  // dips there; a release inside that stretch settles just outside it
  const DIP = 0.08;
  function themesUp() {
    if (!scrubbing) return;
    scrubbing = false;
    theme.setScrubbing(false);
    const p = theme.pref;
    if (typeof p === "number" && Math.abs(p - 0.5) < DIP) theme.set(p < 0.5 ? 0.5 - DIP : 0.5 + DIP);
  }

  const REPO = "https://github.com/libdither/disp";

  const links = [
    { label: "Learn", href: `${base}/learn/` },
    { label: "Playground", href: `${base}/playground/` },
    { label: "Compare", href: `${base}/compare/` },
    { label: "Git", href: REPO, external: true },
    { label: "Funding", href: `${base}/funding/` },
  ];

  const isActive = (href: string) =>
    !href.startsWith("http") && page.url.pathname === href;

  // the playground is an APP surface: it owns the whole viewport below the
  // nav — no footer, no page scroll
  const isApp = $derived(page.url.pathname.startsWith(`${base}/playground`));

  let warnDismissed = $state(false);
</script>

<!-- the appearance glyphs: sun, moon, half-and-half for "follow the system", plain lines for the simple style -->
{#snippet sun()}
  <svg class="tglyph" viewBox="0 0 24 24" aria-hidden="true">
    <circle cx="12" cy="12" r="4.4" />
    <g class="rays">
      <path d="M12 1.8v2.6M12 19.6v2.6M1.8 12h2.6M19.6 12h2.6" />
      <path d="M4.8 4.8l1.9 1.9M17.3 17.3l1.9 1.9M19.2 4.8l-1.9 1.9M6.7 17.3l-1.9 1.9" />
    </g>
  </svg>
{/snippet}
{#snippet moon()}
  <svg class="tglyph" viewBox="0 0 24 24" aria-hidden="true">
    <path d="M20.5 14.6A8.6 8.6 0 0 1 9.4 3.5a8.6 8.6 0 1 0 11.1 11.1Z" />
  </svg>
{/snippet}
{#snippet auto()}
  <svg class="tglyph" viewBox="0 0 24 24" aria-hidden="true">
    <circle cx="12" cy="12" r="8.2" fill="none" stroke="currentColor" stroke-width="1.9" />
    <path d="M12 3.8a8.2 8.2 0 0 1 0 16.4Z" />
  </svg>
{/snippet}
{#snippet themeItem(o: { value: ThemePref; label: string }, i: number)}
  <button
    class="amenu-item"
    class:hot={hot === i}
    role="menuitemradio"
    aria-checked={theme.pref === o.value}
    title={o.label}
    aria-label={o.label}
    onclick={() => theme.set(o.value)}
  >
    {#if o.value === "system"}{@render auto()}{:else if o.value === "light"}{@render sun()}{:else}{@render moon()}{/if}
  </button>
{/snippet}
{#snippet plain()}
  <svg class="tglyph" viewBox="0 0 24 24" aria-hidden="true">
    <g class="rays"><path d="M4 6h16M4 12h11M4 18h14" /></g>
  </svg>
{/snippet}

<div class="shell" class:app={isApp}>
  {#if !warnDismissed}
    <div class="warnbar" role="status">
      <span>
        disp is under heavy development and much of the codebase is
        AI-written — explore this website at your peril :)
      </span>
      <button
        class="warn-dismiss"
        onclick={() => (warnDismissed = true)}
        aria-label="Dismiss warning"
      >
        ✕
      </button>
    </div>
  {/if}
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

        <div class="appearance">
          <button
            class="themetoggle"
            class:auto={theme.pref === "system"}
            title="Appearance — {themeLabel(theme.pref)} (click cycles, hover for more)"
            aria-label="Appearance — {themeLabel(theme.pref)}. Click to cycle the theme."
            onclick={cycleTheme}
          >
            {#if theme.resolved === "dark"}{@render moon()}{:else}{@render sun()}{/if}
          </button>
          <div class="amenu" role="menu" aria-label="appearance">
              <!-- svelte-ignore a11y_no_static_element_interactions -->
              <div
                class="amenu-tape"
                class:scrubbing={scrubbing && continuous}
                bind:this={themesEl}
                onpointerdown={themesDown}
                onpointermove={themesMove}
                onpointerup={themesUp}
                onpointercancel={themesUp}
                onpointerleave={() => (hot = null)}
              >
                <span
                  class="athumb"
                  class:cont={continuous}
                  class:pressed={scrubbing && continuous}
                  style="transform: {thumbTransform}"
                  aria-hidden="true"
                ></span>
                {@render themeItem(THEME_OPTIONS[0], 0)}
                {@render themeItem(THEME_OPTIONS[1], 1)}
                <!-- the track between the sun and the moon: a wedge shaded from
                     the light page to the dark one, the room there is to blend -->
                <svg class="atrack" viewBox="0 0 10 64" aria-hidden="true">
                  <defs>
                    <linearGradient id="atrack-shade" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="0" style="stop-color: var(--l-bg)" />
                      <stop offset="1" style="stop-color: var(--d-bg)" />
                    </linearGradient>
                  </defs>
                  <path d="M3.5 0.5 H6.5 L9.5 63.5 H0.5 Z" fill="url(#atrack-shade)" stroke="var(--border-strong)" stroke-width="1" stroke-linejoin="round" />
                  <!-- the crossover: where the inks swap sides -->
                  <path d="M1.5 32 H8.5" stroke="var(--border-strong)" stroke-width="1" stroke-dasharray="1 1" />
                </svg>
                {@render themeItem(THEME_OPTIONS[2], 2)}
              </div>
              <span class="amenu-sep"></span>
              <button
                class="amenu-item"
                role="menuitemcheckbox"
                aria-checked={theme.style === "simple"}
                title="Simple style"
                aria-label="Simple style"
                onclick={() => theme.setStyle(theme.style === "simple" ? "original" : "simple")}
              >
                {@render plain()}
              </button>
            </div>
        </div>
      </div>
    </nav>
  </header>

  <main class="content">
    {@render children()}
  </main>

  {#if !isApp}
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
        <a href="{base}/compare/">Compare</a>
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
  {/if}
</div>

<style>
  .shell {
    min-height: 100dvh;
    display: flex;
    flex-direction: column;
  }

  .warnbar {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.75rem;
    padding: 0.3rem 0.9rem;
    font-size: 0.82rem;
    text-align: center;
    color: color-mix(in oklab, var(--warn) 65%, var(--fg));
    background: color-mix(in oklab, var(--warn) 12%, var(--bg-elev));
    border-bottom: 1px solid color-mix(in oklab, var(--warn) 35%, transparent);
  }
  .warn-dismiss {
    flex-shrink: 0;
    border: none;
    background: none;
    padding: 0 0.2rem;
    cursor: pointer;
    font-size: 0.78rem;
    line-height: 1;
    color: inherit;
    opacity: 0.65;
  }
  .warn-dismiss:hover {
    opacity: 1;
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

  /* sun/moon toggle; the dot marks "following your system" */
  .appearance {
    position: relative;
    display: inline-flex;
  }
  .amenu {
    position: absolute;
    top: calc(100% + 6px);
    right: 0;
    z-index: 40;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 2px;
    padding: 3px;
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: 999px;
    box-shadow: var(--shadow-lift);
    opacity: 0;
    visibility: hidden;
    transform: translateY(-4px);
    /* the close waits a beat, so crossing the gap below the button keeps it */
    transition:
      opacity 0.15s ease 0.12s,
      transform 0.15s ease 0.12s,
      visibility 0s linear 0.27s;
  }
  .amenu::before {
    content: "";
    position: absolute;
    left: 0;
    right: 0;
    top: -8px;
    height: 8px;
  }
  .appearance:hover .amenu,
  .appearance:focus-within .amenu {
    opacity: 1;
    visibility: visible;
    transform: none;
    transition-delay: 0s;
  }
  .amenu-tape {
    position: relative;
    display: flex;
    flex-direction: column;
    gap: 2px;
    cursor: pointer;
    touch-action: none; /* a drag on touch scrubs, never scrolls */
    user-select: none;
    -webkit-user-select: none;
  }
  /* the tape takes the pointer; the icons stay keyboard-operable */
  .amenu-tape .amenu-item {
    position: relative;
    z-index: 1;
    pointer-events: none;
  }
  .athumb {
    position: absolute;
    left: 0;
    top: 0;
    width: 30px;
    height: 30px;
    box-sizing: border-box;
    border-radius: 50%;
    border: 1.5px solid var(--g2);
    background: color-mix(in oklab, var(--g1) 16%, transparent);
    box-shadow: 0 1px 6px -2px color-mix(in oklab, var(--g2) 70%, transparent);
    pointer-events: none;
    z-index: 1;
    /* sticky + bouncy, the cassette's ride; squares off on an intermediate */
    transition:
      transform 0.24s cubic-bezier(0.34, 1.7, 0.5, 1),
      border-radius 0.3s ease;
  }
  .athumb.cont {
    border-radius: 35%;
  }
  .athumb.pressed {
    border-radius: 40%;
  }
  .atrack {
    display: block;
    flex: none;
    width: 10px;
    height: 64px;
    align-self: center;
  }
  /* under a scrub the thumb sits exactly where the pointer is */
  .amenu-tape.scrubbing .athumb {
    transition: border-radius 0.3s ease;
  }
  .amenu-item {
    width: 30px;
    height: 30px;
    display: grid;
    place-items: center;
    padding: 0;
    border: none;
    border-radius: 50%;
    background: none;
    color: var(--fg-muted);
    cursor: pointer;
  }
  .amenu-item:hover,
  .amenu-item:focus-visible,
  .amenu-item.hot {
    background: var(--bg-panel-hover);
    color: var(--fg);
    outline: none;
  }
  .amenu-item[aria-checked="true"] {
    color: var(--accent);
  }
  .amenu-sep {
    width: 16px;
    height: 1px;
    margin: 2px 0;
    background: var(--border);
  }

  .themetoggle {
    position: relative;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
    width: 32px;
    height: 32px;
    margin-left: 0.15rem;
    padding: 0;
    border: 1px solid transparent;
    border-radius: 8px;
    background: none;
    color: var(--fg-muted);
    cursor: pointer;
    transition:
      color 0.15s ease,
      background 0.15s ease,
      border-color 0.15s ease;
  }
  .themetoggle:hover,
  .appearance:hover .themetoggle,
  .appearance:focus-within .themetoggle {
    color: var(--fg);
    background: var(--bg-panel-hover);
    border-color: var(--border);
  }
  .themetoggle.auto::after {
    content: "";
    position: absolute;
    right: 3px;
    bottom: 3px;
    width: 4px;
    height: 4px;
    border-radius: 50%;
    background: var(--accent);
  }
  .tglyph {
    width: 17px;
    height: 17px;
    fill: currentColor;
  }
  .tglyph .rays {
    fill: none;
    stroke: currentColor;
    stroke-width: 1.9;
    stroke-linecap: round;
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

  /* app pages (playground): the shell IS the viewport — content flexes to
     the space under the nav, nothing scrolls at the page level */
  .shell.app {
    height: 100dvh;
    overflow: hidden;
  }
  .shell.app .content {
    min-height: 0;
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
