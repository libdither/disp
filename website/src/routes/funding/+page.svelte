<script lang="ts">
  import { base } from "$app/paths";
  import stats from "$lib/dono-stats.json";
  const REPO = "https://github.com/libdither/disp";
  const SPONSOR = "https://github.com/sponsors/zyansheep";

  // the runway, measured — by scripts/build-dono-stats.mts at every deploy
  // (GitHub Sponsors recurring + XMR received in the last 30 days, in USD)
  const CURRENT_MONTHLY = Math.round(stats.totals.last30Usd);
  const GOALS = [
    { label: "claude code", amount: 200 },
    { label: "rent and food", amount: 2000 },
    { label: "self-optimizing optimizer compute", amount: 3000 },
    { label: "median individual US income", amount: 3761 },
  ];
  const MAX = GOALS[GOALS.length - 1].amount;
  const pct = Math.min(1, CURRENT_MONTHLY / MAX);

  // bar geometry (svg viewbox coordinates)
  const BW = 800;
  const BX = 24; // track left
  const TW = BW - 48; // track width
  const TY = 64; // track top
  const TH = 26; // track height
  const xOf = (amount: number) => BX + (amount / MAX) * TW;
  const fmt = (n: number) => "$" + n.toLocaleString("en-US");
</script>

<svelte:head>
  <title>Funding · disp</title>
</svelte:head>

<div class="container page">
  <h1>Funding</h1>
  <p class="lede">
    Hello!!! Disp is a personal project, and I'm slowly running out of money on
    rent and my 200$/month claude code subscription... can hav monez plz? 🥺
  </p>

  <div class="grid">
    <div class="card">
      <h3>Support for free!</h3>
      <p>
        Star, watch, use, open issues, contribute, talk about disp in my dms:
        these are are all Very Cool™ things to do.
      </p>
      <a class="btn" href={REPO} target="_blank" rel="noopener"
        >Star on GitHub</a
      >
    </div>
    <div class="card">
      <h3>GitHub Sponsors</h3>
      <p>
        Monthly or one-time, card-payment easy. Recurring pledges water the
        runway garden below directly.
      </p>
      <a class="btn primary" href={SPONSOR} target="_blank" rel="noopener"
        >Sponsor on GitHub</a
      >
    </div>

    <div class="card">
      <h3>Monero</h3>
      <p>For Very Cool People who Like Privacy: fund this work with Monero!</p>
      <a class="btn" href="{base}/funding/monero/">Donate XMR</a>
    </div>
  </div>

  <!-- ================= the runway, as a garden ================= -->
  <section class="runway">
    <h2>Donation goals</h2>
    <p class="runway-sub">
      Monthly support so far: <strong>{fmt(CURRENT_MONTHLY)}</strong> of
      {fmt(MAX)}.
    </p>
    <div class="runway-scroll">
    <svg
      viewBox="0 0 {BW} 150"
      class="gardenbar"
      role="img"
      aria-label="funding progress: {fmt(
        CURRENT_MONTHLY,
      )} per month toward milestones at {GOALS.map(
        (g) => `${g.label} ${fmt(g.amount)}`,
      ).join(', ')}"
    >
      <defs>
        <!-- the infill: a hedge of little leaves -->
        <pattern
          id="leafpat"
          width="26"
          height="26"
          patternUnits="userSpaceOnUse"
        >
          <rect width="26" height="26" fill="#bfe0c4" />
          <path
            d="M6,13 C10,9 10,4 6,1 C2,4 2,9 6,13 Z"
            fill="#58b368"
            transform="rotate(18 6 7)"
          />
          <path
            d="M19,25 C23,21 23,16 19,13 C15,16 15,21 19,25 Z"
            fill="#2f9e6e"
            transform="rotate(-14 19 19)"
          />
          <path d="M20,8 C23,5 23,2 20,0 C17,2 17,5 20,8 Z" fill="#8fce9d" />
          <path d="M4,24 C7,21 7,18 4,16 C1,18 1,21 4,24 Z" fill="#3d9b74" />
        </pattern>
        <clipPath id="trackclip">
          <rect x={BX} y={TY} width={TW} height={TH} rx={TH / 2} />
        </clipPath>
      </defs>

      <!-- track -->
      <rect x={BX} y={TY} width={TW} height={TH} rx={TH / 2} class="track" />
      <!-- leafy fill -->
      {#if pct > 0}
        <g clip-path="url(#trackclip)">
          <rect
            x={BX}
            y={TY}
            width={pct * TW}
            height={TH}
            fill="url(#leafpat)"
          />
        </g>
      {/if}

      <!-- the seed/sprout at the current mark -->
      <g style="transform: translate({xOf(CURRENT_MONTHLY)}px, {TY - 4}px)">
        <path d="M0,0 C0,-6 0,-8 0,-11" class="sproutstem" />
        <path
          d="M0,-8 C-4,-9 -6,-12 -6.5,-15 C-3,-14.5 -0.5,-11.5 0,-8 Z"
          class="sproutleaf a"
        />
        <path
          d="M0,-9.5 C3.5,-10.5 5.5,-13 6,-16 C2.5,-15.5 0.5,-12.5 0,-9.5 Z"
          class="sproutleaf b"
        />
      </g>

      <!-- milestones: little signposts along the path -->
      {#each GOALS as g, i}
        {@const x = xOf(g.amount)}
        {@const above = i % 2 === 0}
        {@const reached = CURRENT_MONTHLY >= g.amount}
        <g class="milestone" class:reached>
          <line
            x1={x}
            y1={above ? TY - 14 : TY + TH + 2}
            x2={x}
            y2={above ? TY : TY + TH + 14}
            class="post"
          />
          {#if reached}
            <!-- a bloom for every goal reached -->
            <g
              style="transform: translate({x}px, {above
                ? TY - 18
                : TY + TH + 18}px)"
            >
              {#each [0, 72, 144, 216, 288] as a}
                <circle
                  r="2.4"
                  cx={5 * Math.cos((a * Math.PI) / 180)}
                  cy={5 * Math.sin((a * Math.PI) / 180)}
                  class="petal"
                />
              {/each}
              <circle r="2" class="pollen" />
            </g>
          {:else}
            <circle
              cx={x}
              cy={above ? TY - 17 : TY + TH + 17}
              r="3"
              class="bud"
            />
          {/if}
          <text
            {x}
            y={above ? TY - 28 : TY + TH + 34}
            class="mlabel"
            text-anchor={g.amount / MAX > 0.9
              ? "end"
              : g.amount / MAX < 0.1
                ? "start"
                : "middle"}>{g.label}</text
          >
          <text
            {x}
            y={above ? TY - 41 : TY + TH + 47}
            class="mamount"
            text-anchor={g.amount / MAX > 0.9
              ? "end"
              : g.amount / MAX < 0.1
                ? "start"
                : "middle"}>{fmt(g.amount)}/mo</text
          >
        </g>
      {/each}

      <!-- ground dressing -->
      <g class="grass">
        <path
          d="M {BX + 40} {TY + TH + 8} q 2 -9 6 -12 M {BX + 46} {TY +
            TH +
            8} q 3 -6 7 -8"
        />
        <path
          d="M {BX + TW * 0.38} {TY + TH + 10} q 2 -9 6 -12 M {BX +
            TW * 0.38 +
            6} {TY + TH + 10} q 3 -6 7 -8"
        />
        <path d="M {BX + TW * 0.68} {TY + TH + 8} q 2 -9 6 -12" />
      </g>
      <circle cx={BX + TW * 0.3} cy={TY + TH + 6} r="2" class="daisy" />
      <circle cx={BX + TW * 0.86} cy={TY + TH + 8} r="2" class="daisy alt" />
    </svg>
    </div>
    <p class="runway-note">
      The last post is the U.S. median individual income, at which point disp
      becomes a real job!
    </p>
  </section>

  <!-- ================= the books ================= -->
  <section class="books">
    <h2>The books</h2>
    <div class="stat-grid">
      <div class="stat">
        <span class="stat-num">${stats.totals.last30Usd.toLocaleString("en-US")}</span>
        <span class="stat-label">last 30 days</span>
      </div>
      <div class="stat">
        <span class="stat-num">${stats.totals.lifetimeUsd.toLocaleString("en-US")}</span>
        <span class="stat-label">lifetime</span>
      </div>
      <div class="stat">
        <span class="stat-num">${stats.totals.avgMonthlyUsd.toLocaleString("en-US")}</span>
        <span class="stat-label">per month, amortized over the project's life</span>
      </div>
      <div class="stat">
        <span class="stat-num">${stats.totals.perCommitUsd.toLocaleString("en-US")}</span>
        <span class="stat-label">per commit ({stats.repo.commits.toLocaleString("en-US")} commits)</span>
      </div>
    </div>
    <p class="books-note">
      Computed at every deploy by
      <a href="{REPO}/blob/main/website/scripts/build-dono-stats.mts" target="_blank" rel="noopener"
        >a build script</a
      >: GitHub Sponsors ({stats.github.sponsorCount} sponsor{stats.github.sponsorCount === 1
        ? ""
        : "s"}, ${stats.github.monthlyUsd}/mo recurring; lifetime accrued as an estimate) plus a
      view-only scan of the
      <a href="{base}/funding/monero/">Monero wallet</a>
      ({stats.xmr.txs.length} donation{stats.xmr.txs.length === 1 ? "" : "s"} on-chain, converted at
      ${stats.xmrUsd.toLocaleString("en-US")}/XMR) — the wallet's view key is published, so you can
      audit that number yourself. Last refreshed {new Date(stats.updatedAt).toISOString().slice(0, 10)}.
    </p>
  </section>
</div>

<style>
  .page {
    padding-block: 3.5rem 2rem;
    max-width: 900px;
  }
  h1 {
    font-size: clamp(2.2rem, 5vw, 3.2rem);
    margin: 0 0 0.8rem;
  }
  .lede {
    color: var(--fg-muted);
    font-size: 1.08rem;
    max-width: 46rem;
  }
  .grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 1.1rem;
    margin-top: 2rem;
  }
  .card h3 {
    margin: 0 0 0.5rem;
    font-size: 1.1rem;
  }
  .card p {
    color: var(--fg-muted);
    font-size: 0.93rem;
  }

  /* ---- the runway garden ---- */
  .runway {
    margin-top: 3rem;
  }
  /* on a phone the garden scrolls sideways instead of shrinking to moss */
  .runway-scroll {
    overflow-x: auto;
    max-width: 100%;
    padding-bottom: 0.3rem;
  }
  .runway h2 {
    font-size: 1.6rem;
    margin: 0 0 0.3rem;
  }
  .runway-sub {
    color: var(--fg-muted);
    font-size: 0.95rem;
    margin: 0 0 0.6rem;
  }
  .gardenbar {
    width: 100%;
    min-width: 560px;
    overflow: visible;
  }
  .track {
    fill: color-mix(in oklab, var(--g1) 10%, var(--bg-elev));
    stroke: var(--border-strong);
    stroke-width: 1;
  }
  .sproutstem {
    stroke: var(--g2);
    stroke-width: 1.8;
    fill: none;
    stroke-linecap: round;
  }
  .sproutleaf {
    fill: var(--g1);
  }
  .sproutleaf.b {
    fill: var(--g2);
  }
  .milestone .post {
    stroke: var(--bark);
    stroke-width: 1.6;
    stroke-linecap: round;
  }
  .milestone .bud {
    fill: none;
    stroke: var(--g4);
    stroke-width: 1.4;
    stroke-dasharray: 2.5 2.5;
  }
  .milestone .petal {
    fill: var(--blossom);
  }
  .milestone .pollen {
    fill: var(--g4);
  }
  .mlabel {
    font-family: var(--font-body);
    font-size: 11.5px;
    fill: var(--fg-muted);
  }
  .mamount {
    font-family: var(--font-mono);
    font-size: 11px;
    font-weight: 600;
    fill: var(--g2);
  }
  .milestone.reached .mlabel {
    fill: var(--fg);
  }
  .grass path {
    stroke: color-mix(in oklab, var(--g2) 55%, transparent);
    stroke-width: 1.4;
    fill: none;
    stroke-linecap: round;
  }
  .daisy {
    fill: var(--blossom);
  }
  .daisy.alt {
    fill: var(--g4);
  }
  .runway-note {
    color: var(--fg-faint);
    font-size: 0.82rem;
    font-style: italic;
    margin-top: 0.4rem;
  }

  /* ---- the books ---- */
  .books {
    margin-top: 3rem;
  }
  .books h2 {
    font-size: 1.6rem;
    margin: 0 0 0.9rem;
  }
  .stat-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
    gap: 1rem;
  }
  .stat {
    background: var(--bg-elev);
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 0.9rem 1rem;
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  .stat-num {
    font-family: var(--font-mono);
    font-size: 1.5rem;
    font-weight: 650;
    color: var(--g2);
  }
  .stat-label {
    font-size: 0.8rem;
    color: var(--fg-muted);
  }
  .books-note {
    color: var(--fg-faint);
    font-size: 0.82rem;
    margin-top: 0.9rem;
    line-height: 1.6;
  }
</style>
