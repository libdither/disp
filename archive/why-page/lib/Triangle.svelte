<script lang="ts">
  // Scene 1: fast, proven, written for you. Nothing sits in the middle.
  const DOTS = [
    { id: 'rust', name: 'Rust', x: 92, y: 240, gives: 'Fast, but nothing proves it does what you meant, and you write every line.' },
    { id: 'lean', name: 'Lean', x: 408, y: 240, gives: 'Proofs are checked by machine, but they take real effort, and the result is usually not fast.' },
    { id: 'hacl', name: 'HACL*', x: 250, y: 262, gives: 'Verified crypto code that is also fast, written by experts at great cost, one algorithm at a time.' },
    { id: 'agents', name: 'Coding agents', x: 250, y: 58, gives: 'Writes the code for you, but nothing guarantees it is right, and it gets expensive at scale.' },
    { id: 'center', name: "Disp's bet", x: 250, y: 178, gives: 'Fast, proven and written for you, all at once. Nothing is here yet, disp included.' }
  ]
  let picked = $state('center')
  const dot = $derived(DOTS.find((d) => d.id === picked)!)
</script>

<div class="why-widget">
  <svg viewBox="0 0 500 310" role="group" aria-label="A triangle with corners fast, proven and written for you">
    <path class="edge" d="M250 28 L470 282 L30 282 Z" />
    <text class="corner" x="250" y="17" text-anchor="middle">written for you</text>
    <text class="corner" x="22" y="302">fast</text>
    <text class="corner" x="478" y="302" text-anchor="end">proven correct</text>
    {#each DOTS as d (d.id)}
      <g
        class="dot"
        class:center={d.id === 'center'}
        class:on={d.id === picked}
        role="button"
        tabindex="0"
        aria-label={d.name}
        onmouseenter={() => (picked = d.id)}
        onfocus={() => (picked = d.id)}
        onclick={() => (picked = d.id)}
        onkeydown={(e) => e.key === 'Enter' && (picked = d.id)}
      >
        <rect x={d.x - 44} y={d.y - 20} width="88" height="30" fill="transparent" />
        <circle cx={d.x} cy={d.y} r={d.id === 'center' ? 11 : 6} />
        <text x={d.x} y={d.y - 12} text-anchor="middle">{d.name}</text>
      </g>
    {/each}
  </svg>
  <p class="why-note" aria-live="polite"><strong>{dot.name}.</strong> {dot.gives}</p>
</div>

<style>
  svg {
    display: block;
    width: 100%;
    max-width: 30rem;
    margin: 0 auto;
  }
  .edge {
    fill: color-mix(in oklab, var(--g1) 5%, transparent);
    stroke: var(--border-strong);
  }
  .corner {
    font-size: 14px;
    font-weight: 600;
    fill: var(--fg-muted);
  }
  .dot {
    cursor: pointer;
    outline: none;
  }
  .dot text {
    font-size: 12.5px;
    fill: var(--fg);
  }
  .dot circle {
    fill: var(--g2);
    stroke: var(--bg-elev);
    stroke-width: 2;
  }
  .dot.center circle {
    fill: none;
    stroke: var(--fg-faint);
    stroke-dasharray: 3 3;
  }
  .dot.on circle {
    stroke: var(--g4);
    stroke-width: 3;
  }
</style>
