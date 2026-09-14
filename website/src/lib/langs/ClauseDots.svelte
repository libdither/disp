<script module lang="ts">
  // Shared by the dots and the compare page's tooltips: one entry per clause
  // of the survey files' "1 · ½† · 0 · ?" strings.
  const WORD: Record<string, string> = {
    '1': 'met',
    '½': 'halfway',
    '0': 'not met',
    '?': 'open, scored 0',
    '—': 'not applicable'
  }
  export interface Clause {
    v: string // '1' | '½' | '0' | '?' | '—'
    route?: string // a ½ names the half-credit condition it takes: '½(contracts)'
    word: string // the state in words, the ½ route and † qualifier spelled out
  }
  /// A clause definition reads `(a) **short label** — full criterion`; split it.
  export function splitClause(text: string): { key: string; label: string; detail: string } {
    const m = text.match(/^\((\w)\)\s*\*\*(.+?)\*\*\s*—\s*(.*)$/)
    return m ? { key: `(${m[1]})`, label: m[2], detail: m[3] } : { key: '', label: text, detail: '' }
  }

  export function parseClauses(clauses: string): Clause[] {
    return clauses
      .split('·')
      .map((t) => t.trim())
      .filter(Boolean)
      .map((t) => {
        const knowledge = t.includes('†')
        const m = t.replace('†', '').trim().match(/^(.+?)(?:\(([^)]+)\))?$/)
        const v = m?.[1].trim() ?? t
        const route = m?.[2]
        const word =
          (WORD[v] ?? v) + (route ? ` (${route})` : '') + (knowledge ? ' (from general knowledge)' : '')
        return { v, route, word }
      })
  }
</script>

<script lang="ts">
  // One dot per grading clause, so a glance shows how much of an axis is met
  // and a hover shows which clause is missing. Input is the survey files'
  // clause string ("1 · ½† · 0 · ?"): filled = met, half-filled = halfway,
  // empty = not met, dashed = open (?, scored 0), a dash = not applicable.
  interface Props {
    clauses: string
    labels?: string[] // the axis's clause definitions, one native tooltip each
    size?: number // dot diameter in px
    titles?: boolean // off where a custom tooltip already covers the figure
  }
  let { clauses, labels = [], size = 9, titles = true }: Props = $props()

  const dots = $derived(
    parseClauses(clauses).map((c, i) => ({
      ...c,
      title: labels[i] ? `${splitClause(labels[i]).label} — ${c.word}` : c.word
    }))
  )
  const summary = $derived(`clauses: ${dots.map((d) => WORD[d.v] ?? d.v).join(', ')}`)

  // one 12×12 cell per dot; r leaves room for the stroke
  const U = 12
  const GAP = 3.5
  const R = 4.9
</script>

<svg
  class="clause-dots"
  width={((dots.length * (U + GAP) - GAP) * size) / U}
  height={size}
  viewBox="0 0 {dots.length * (U + GAP) - GAP} {U}"
  role="img"
  aria-label={summary}
>
  {#each dots as d, i (i)}
    {@const x = i * (U + GAP) + U / 2}
    <g class="dot {d.v === '1' ? 'met' : d.v === '½' ? 'half' : d.v === '?' ? 'open' : d.v === '—' ? 'na' : 'no'}">
      {#if titles}<title>{d.title}</title>{/if}
      {#if d.v === '—'}
        <line x1={x - R + 1.5} y1={U / 2} x2={x + R - 1.5} y2={U / 2} />
      {:else}
        <circle cx={x} cy={U / 2} r={R} />
        {#if d.v === '½'}
          <path d="M {x} {U / 2 - R} A {R} {R} 0 0 0 {x} {U / 2 + R} Z" class="fillhalf" />
        {/if}
      {/if}
    </g>
  {/each}
</svg>

<style>
  .clause-dots {
    display: inline-block;
    vertical-align: middle;
    overflow: visible;
  }
  .dot circle {
    fill: none;
    stroke: var(--fg-faint);
    stroke-width: 1.6;
  }
  .dot.met circle {
    fill: var(--accent);
    stroke: var(--accent);
  }
  .dot.half circle {
    stroke: var(--accent);
  }
  .dot .fillhalf {
    fill: var(--accent);
    stroke: none;
  }
  .dot.open circle {
    stroke-dasharray: 2.4 2;
    stroke-linecap: round;
  }
  .dot.na line {
    stroke: var(--fg-faint);
    stroke-width: 1.6;
    stroke-linecap: round;
  }
</style>
