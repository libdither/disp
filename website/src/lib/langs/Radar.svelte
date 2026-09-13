<script lang="ts">
  // A six-axis radar: disp's profile against the picked languages. Levels are
  // the survey's 0/1/2 (✗/◐/✅); the zero ring sits off-centre so an all-✗
  // profile is still a visible hexagon. A dashed ring on a vertex marks the
  // survey's "ahead of disp". Colours arrive as CSS variables so a series
  // keeps its hue across theme changes.
  import type { Axis } from './types'

  export interface RadarSeries {
    key: string
    name: string
    color: string
    levels: (number | null)[]
    ahead: boolean[]
  }
  interface Props {
    axes: Axis[]
    series: RadarSeries[]
    onhover?: (hit: { series: RadarSeries; axis: number } | null, ev: MouseEvent) => void
  }
  let { axes, series, onhover }: Props = $props()

  const W = 380
  const H = 336
  const CX = 190
  const CY = 170
  const R = 112
  const R0 = 20
  const RING_SYM = ['✗', '◐', '✅']

  const angle = (i: number) => -Math.PI / 2 + (i * 2 * Math.PI) / axes.length
  const radius = (level: number | null) => R0 + ((R - R0) * (level ?? 0)) / 2
  const pt = (i: number, r: number) => [CX + r * Math.cos(angle(i)), CY + r * Math.sin(angle(i))] as const
  const ring = (level: number) => axes.map((_, i) => pt(i, radius(level)).join(',')).join(' ')
  const poly = (s: RadarSeries) => axes.map((_, i) => pt(i, radius(s.levels[i])).join(',')).join(' ')
  // labels hang off the spoke ends: anchored by side, nudged up on the top half
  const anchor = (i: number) => {
    const c = Math.cos(angle(i))
    return c > 0.1 ? 'start' : c < -0.1 ? 'end' : 'middle'
  }
  const lift = (i: number) => {
    const s = Math.sin(angle(i))
    return s < -0.5 ? -14 : s > 0.5 ? 8 : -6
  }
</script>

<figure class="radar">
  <svg viewBox="0 0 {W} {H}" role="img" aria-label="disp and the picked languages on the six axes">
    {#each [0, 1, 2] as lv (lv)}
      <polygon class="ring" class:outer={lv === 2} points={ring(lv)} />
      <text class="ring-lbl" x={CX + 5} y={CY - radius(lv) - 3}>{RING_SYM[lv]}</text>
    {/each}
    {#each axes as ax, i (ax.id)}
      {@const [x, y] = pt(i, R)}
      {@const [lx, ly] = pt(i, R + 16)}
      <line class="spoke" x1={CX} y1={CY} x2={x} y2={y} />
      <text class="ax-lbl" x={lx} y={ly + lift(i)} text-anchor={anchor(i)} dominant-baseline="middle">
        <tspan class="ax-id">{ax.id}</tspan>
        <tspan class="ax-name" x={lx} dy="1.2em">{ax.short}</tspan>
      </text>
    {/each}
    {#each series as s (s.key)}
      <polygon class="area" points={poly(s)} style:fill={s.color} style:stroke={s.color} />
    {/each}
    {#each series as s (s.key)}
      {#each axes as ax, i (ax.id)}
        {@const [x, y] = pt(i, radius(s.levels[i]))}
        {#if s.ahead[i]}
          <circle class="ahead" cx={x} cy={y} r="8.5" style:stroke={s.color} />
        {/if}
        <circle class="dot" class:hollow={s.levels[i] == null} cx={x} cy={y} r="4.5" style:fill={s.color} style:stroke={s.levels[i] == null ? s.color : null} />
        <circle
          class="hit"
          role="graphics-symbol"
          aria-label="{s.name} on {ax.short}: {s.levels[i] == null ? 'not scored' : RING_SYM[s.levels[i] ?? 0]}"
          cx={x}
          cy={y}
          r="12"
          onmouseenter={(e) => onhover?.({ series: s, axis: i }, e)}
          onmouseleave={(e) => onhover?.(null, e)}
        />
      {/each}
    {/each}
  </svg>
  <figcaption class="legend">
    {#each series as s (s.key)}
      <span class="lg"><i class="sw" style:background={s.color}></i>{s.name}</span>
    {/each}
  </figcaption>
</figure>

<style>
  .radar {
    margin: 0;
  }
  svg {
    display: block;
    width: 100%;
    height: auto;
  }
  .ring {
    fill: none;
    stroke: var(--border);
    stroke-width: 1;
  }
  .ring.outer {
    stroke: var(--border-strong);
  }
  .spoke {
    stroke: var(--border);
    stroke-width: 1;
  }
  .ring-lbl {
    font: 9px var(--font-mono);
    fill: var(--fg-faint);
  }
  .ax-lbl {
    font: 600 11px var(--font-body);
    fill: var(--fg);
  }
  .ax-name {
    font-weight: 400;
    font-size: 10.5px;
    fill: var(--fg-muted);
  }
  .area {
    fill-opacity: 0.16;
    stroke-width: 2;
    stroke-linejoin: round;
  }
  .dot {
    stroke: var(--bg-elev);
    stroke-width: 2;
  }
  .dot.hollow {
    fill: var(--bg-elev) !important;
    stroke-width: 1.5;
  }
  .ahead {
    fill: none;
    stroke-width: 1.5;
    stroke-dasharray: 2 2;
  }
  .hit {
    fill: transparent;
    cursor: help;
  }
  .legend {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    gap: 0.35rem 1rem;
    margin-top: 0.3rem;
    font-size: 0.8rem;
    color: var(--fg-muted);
  }
  .sw {
    display: inline-block;
    width: 10px;
    height: 10px;
    border-radius: 3px;
    margin-right: 0.4em;
    vertical-align: -1px;
  }
</style>
