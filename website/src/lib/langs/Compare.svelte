<script lang="ts">
  // Progress & Comparisons: the survey's master table as a sortable score
  // matrix, a radar of disp against up to two picked languages, and each
  // pick's scorecard notes plus write-up excerpts. All of it comes from
  // research/awesome-langs through the virtual:awesome-langs module.
  import { onMount } from 'svelte'
  import { replaceState } from '$app/navigation'
  import Radar, { type RadarSeries } from './Radar.svelte'
  import { AXIS_IDS, LEVEL_WORD, type AxisId, type Lang, type LangsData, type Score } from './types'

  interface Props {
    data: LangsData
  }
  let { data }: Props = $props()

  // colour follows the entity: a pick keeps its slot until it is dropped
  const SLOT_COLORS = ['var(--cmp-1)', 'var(--cmp-2)']
  let picks = $state<{ slug: string; slot: number }[]>([])
  let sortAxis = $state<AxisId | null>(null)
  let sortDesc = $state(true)
  let onlyAhead = $state(false)
  let query = $state('')

  const scored = $derived(data.langs.filter((l) => l.scored))
  const unscored = $derived(data.langs.filter((l) => !l.scored))
  const rank = (l: Lang, id: AxisId) => l.scores[id].pct ?? (l.scores[id].level == null ? -1 : l.scores[id].level * 50)
  const rows = $derived.by(() => {
    const q = query.trim().toLowerCase()
    let out = scored.filter((l) => !q || l.name.toLowerCase().includes(q))
    if (onlyAhead) out = out.filter((l) => (sortAxis ? [sortAxis] : AXIS_IDS).some((id) => l.scores[id].ahead))
    if (sortAxis) {
      const id = sortAxis
      out = [...out].sort((a, b) => (sortDesc ? rank(b, id) - rank(a, id) : rank(a, id) - rank(b, id)))
    }
    return out
  })

  // click a column: ahead-first, then reversed, then back to survey order
  function sortBy(id: AxisId): void {
    if (sortAxis !== id) {
      sortAxis = id
      sortDesc = true
    } else if (sortDesc) sortDesc = false
    else sortAxis = null
  }

  const pickOf = (slug: string) => picks.find((p) => p.slug === slug)
  const langOf = (slug: string) => data.langs.find((l) => l.slug === slug)
  function toggle(slug: string, sync = true): void {
    const i = picks.findIndex((p) => p.slug === slug)
    if (i >= 0) picks.splice(i, 1)
    else {
      if (picks.length >= SLOT_COLORS.length) picks.shift()
      const used = new Set(picks.map((p) => p.slot))
      picks.push({ slug, slot: SLOT_COLORS.findIndex((_, s) => !used.has(s)) })
    }
    if (sync) syncUrl()
  }
  // ?lang=a,b deep-links a comparison
  function syncUrl(): void {
    try {
      const url = new URL(location.href)
      if (picks.length) url.searchParams.set('lang', picks.map((p) => p.slug).join(','))
      else url.searchParams.delete('lang')
      replaceState(url, {})
    } catch {
      /* before router init / no history: the picks still show */
    }
  }
  onMount(() => {
    const want = new URLSearchParams(location.search).get('lang')?.split(',') ?? []
    for (const slug of want) if (langOf(slug)?.scored && !pickOf(slug)) toggle(slug, false)
  })

  // a cell without a percentage (none today) falls back to its level
  const value = (s: Score) => s.pct ?? (s.level == null ? null : s.level * 50)
  const series = $derived<RadarSeries[]>([
    {
      key: 'disp',
      name: 'disp',
      color: 'var(--cmp-0)',
      values: AXIS_IDS.map((id) => value(data.disp[id])),
      ahead: AXIS_IDS.map(() => false)
    },
    ...picks.flatMap((p) => {
      const l = langOf(p.slug)
      if (!l) return []
      return [
        {
          key: l.slug,
          name: l.name,
          color: SLOT_COLORS[p.slot],
          values: AXIS_IDS.map((id) => value(l.scores[id])),
          ahead: AXIS_IDS.map((id) => l.scores[id].ahead)
        }
      ]
    })
  ])

  // one tooltip for matrix cells and radar vertices alike
  let tip = $state<{ x: number; y: number; title: string; html: string } | null>(null)
  function showTip(ev: MouseEvent, name: string, id: AxisId, s: Score): void {
    const axis = data.axes.find((a) => a.id === id)
    const word = s.level == null ? 'not scored' : LEVEL_WORD[s.level]
    const head = `<b>${s.raw}${s.pct != null ? ` ${s.pct}%${s.provisional ? '?' : ''}` : ''}</b> ${word}${s.tag ? ` · ${s.tag}` : ''}${s.ahead ? ' · <b>ahead of disp</b>' : ''}`
    const clauses = s.clauses ? `<br><span class="tip-clauses">${s.clauses}</span>${s.whyHtml ? ` — ${s.whyHtml}` : ''}` : ''
    tip = {
      x: Math.min(ev.clientX + 14, window.innerWidth - 350),
      y: ev.clientY + 14,
      title: `${name} · ${id} ${axis?.short ?? ''}`,
      html: `${head}${clauses}${s.noteHtml ? `<br>${s.noteHtml}` : ''}`
    }
  }
  const hideTip = () => (tip = null)
  function radarHover(hit: { series: RadarSeries; axis: number } | null, ev: MouseEvent): void {
    if (!hit) {
      hideTip()
      return
    }
    const id = AXIS_IDS[hit.axis]
    const s = hit.series.key === 'disp' ? data.disp[id] : langOf(hit.series.key)?.scores[id]
    if (s) showTip(ev, hit.series.name, id, s)
  }

  const lv = (level: number | null) => (level == null ? 'lvn' : `lv${level}`)
  const pctText = (s: Score) => (s.pct == null ? '' : `${s.pct}%${s.provisional ? '?' : ''}`)
</script>

<div class="compare">
  <div class="top">
    <div class="left">
      <div class="toolbar">
        <input type="search" placeholder="filter by name" bind:value={query} aria-label="filter languages" />
        <label class="chk">
          <input type="checkbox" bind:checked={onlyAhead} />
          only ahead of disp {sortAxis ? `on ${sortAxis}` : '(any axis)'}
        </label>
        <span class="hint">click an axis to sort · click a name to compare (up to two)</span>
      </div>
      <div class="matrix-wrap">
        <table class="matrix">
          <thead>
            <tr>
              <th class="name">Project</th>
              {#each data.axes as ax (ax.id)}
                <th class="axh">
                  <button class="sortbtn" class:on={sortAxis === ax.id} onclick={() => sortBy(ax.id)} title="sort by {ax.name}. 100% = {ax.clauses.join(' · ')}">
                    {ax.id}
                    <small>{ax.short}</small>
                    {#if sortAxis === ax.id}<span class="arrow" aria-hidden="true">{sortDesc ? '↓' : '↑'}</span>{/if}
                  </button>
                </th>
              {/each}
            </tr>
          </thead>
          <tbody>
            <tr class="disp-row">
              <th scope="row"><span class="rowname"><i class="sw" aria-hidden="true"></i>disp <small class="self">self-assessed</small></span></th>
              {#each data.axes as ax (ax.id)}
                {@const s = data.disp[ax.id]}
                <td class="cell {lv(s.level)}" style:--fill={value(s) ?? 0} onmouseenter={(e) => showTip(e, 'disp', ax.id, s)} onmouseleave={hideTip}>
                  <span class="score">{s.raw}<span class="pct">{pctText(s)}</span></span>
                  {#if s.tag}<small class="how">{s.tag}</small>{/if}
                </td>
              {/each}
            </tr>
            {#each rows as lang (lang.slug)}
              {@const pick = pickOf(lang.slug)}
              <tr class:picked={!!pick} style:--row-color={pick ? SLOT_COLORS[pick.slot] : null}>
                <th scope="row">
                  <button class="rowbtn" aria-pressed={!!pick} onclick={() => toggle(lang.slug)}>
                    <i class="sw" aria-hidden="true"></i>{lang.name}
                  </button>
                </th>
                {#each data.axes as ax (ax.id)}
                  {@const s = lang.scores[ax.id]}
                  <td class="cell {lv(s.level)}" class:ahead={s.ahead} style:--fill={value(s) ?? 0} onmouseenter={(e) => showTip(e, lang.name, ax.id, s)} onmouseleave={hideTip}>
                    <span class="score">{s.raw}<span class="pct">{pctText(s)}</span></span>
                    {#if s.tag}<small class="how">{s.tag}</small>{/if}
                  </td>
                {/each}
              </tr>
            {/each}
            {#if !rows.length}
              <tr><td class="empty" colspan={data.axes.length + 1}>nothing matches</td></tr>
            {/if}
          </tbody>
        </table>
      </div>
      <p class="key">
        <span>percent of what disp needs on the axis, three clauses each (hover an axis)</span>
        <span>✗ under 25%</span><span>◐ 25–79%</span><span>✅ 80%+</span>
        <span><i class="ring-key" aria-hidden="true"></i> ahead of disp</span>
        <span>small text: how</span>
        <span>? a clause the write-up leaves open</span>
        <span>— not scored</span>
        {#each Object.entries(data.footnotes) as [k, v] (k)}<span><sup>{k}</sup> {v}</span>{/each}
      </p>
      {#if unscored.length}
        <p class="also">
          Also surveyed, unscored:
          {#each unscored as l, i (l.slug)}{i ? ', ' : ''}<a href={l.sourceUrl} target="_blank" rel="noopener">{l.name}</a>{/each}.
        </p>
      {/if}
    </div>
    <aside class="right">
      <Radar axes={data.axes} {series} onhover={radarHover} />
      <p class="radar-note">
        The outer ring is 100% of what disp needs on that axis. A dashed ring marks a project ahead of disp there.
      </p>
    </aside>
  </div>

  <div class="details">
    {#if !picks.length}
      <p class="hint-card">Pick a language in the table to see its scorecard notes, where disp differs, and the survey's verdict.</p>
    {/if}
    {#each picks as p (p.slug)}
      {@const lang = langOf(p.slug)}
      {#if lang}
        <article class="card detail" style:--row-color={SLOT_COLORS[p.slot]}>
          <header>
            <h3><i class="sw" aria-hidden="true"></i>{lang.name}</h3>
            {#if lang.author}<span class="author">{lang.author}</span>{/if}
            <span class="links">
              {#if lang.repoUrl}<a href={lang.repoUrl} target="_blank" rel="noopener">repo</a>{/if}
              <a href={lang.sourceUrl} target="_blank" rel="noopener">full write-up</a>
            </span>
            <button class="close" onclick={() => toggle(lang.slug)} aria-label="remove {lang.name} from the comparison">✕</button>
          </header>
          {#if lang.closestHtml}
            <p class="closest-line"><b>Closest to disp on:</b> {@html lang.closestHtml}</p>
          {/if}
          <dl class="scorecard">
            {#each data.axes as ax (ax.id)}
              {@const s = lang.scores[ax.id]}
              <div class="sc">
                <dt>
                  <span class="sym {lv(s.level)}">{s.raw} {pctText(s)}</span>
                  <span class="sc-ax">{ax.id} {ax.short}</span>
                  {#if s.tag}<span class="sc-how">{s.tag}</span>{/if}
                  {#if s.ahead}<span class="tag">ahead of disp</span>{/if}
                </dt>
                <dd>
                  {@html s.noteHtml ?? ''}
                  {#if s.clauses}
                    <div class="clauses"><code>{s.clauses}</code> {@html s.whyHtml ?? ''}</div>
                  {/if}
                </dd>
              </div>
            {/each}
          </dl>
          {#if lang.differsHtml}
            <h4>Where disp differs</h4>
            <div class="prose">{@html lang.differsHtml}</div>
          {/if}
          {#if lang.verdictHtml}
            <h4>Verdict</h4>
            <div class="prose">{@html lang.verdictHtml}</div>
          {/if}
        </article>
      {/if}
    {/each}
  </div>

  {#if tip}
    <div class="tip" style:left="{tip.x}px" style:top="{tip.y}px" role="tooltip">
      <div class="tip-title">{tip.title}</div>
      <div class="tip-body">{@html tip.html}</div>
    </div>
  {/if}
</div>

<style>
  /* three series at most: disp, then the two pick slots (validated for
     colour-vision separation on both surfaces) */
  .compare {
    --cmp-0: #2f9e6e;
    --cmp-1: #7c5cbf;
    --cmp-2: #c95f6d;
  }
  :global(:root[data-theme='dark']) .compare {
    --cmp-0: #3d9e70;
    --cmp-1: #9a80d8;
    --cmp-2: #d56f7c;
  }
  .top {
    display: grid;
    grid-template-columns: minmax(0, 1fr) 310px;
    gap: 1.6rem;
    align-items: start;
  }
  .right {
    position: sticky;
    top: calc(var(--nav-h) + 1rem);
  }
  .radar-note {
    margin: 0.4rem 0 0;
    font-size: 0.78rem;
    color: var(--fg-faint);
    text-align: center;
  }

  /* ---- toolbar ---- */
  .toolbar {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 0.5rem 1rem;
    margin-bottom: 0.7rem;
    font-size: 0.84rem;
    color: var(--fg-muted);
  }
  .toolbar input[type='search'] {
    font: inherit;
    padding: 0.35em 0.7em;
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    background: var(--bg-elev);
    color: var(--fg);
    min-width: 11rem;
  }
  .chk {
    display: inline-flex;
    align-items: center;
    gap: 0.4em;
    cursor: pointer;
  }
  .hint {
    color: var(--fg-faint);
  }

  /* ---- the matrix ---- */
  .matrix-wrap {
    overflow: auto;
    max-height: 34rem;
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-elev);
  }
  table {
    border-collapse: separate;
    border-spacing: 0;
    width: 100%;
    font-size: 0.84rem;
  }
  th,
  td {
    padding: 0.36rem 0.45rem;
    border-bottom: 1px solid var(--border);
    text-align: left;
    vertical-align: middle;
  }
  thead th {
    position: sticky;
    top: 0;
    z-index: 1;
    background: var(--bg-elev);
    font-size: 0.76rem;
    font-weight: 600;
    color: var(--fg-muted);
    white-space: nowrap;
  }
  th.axh {
    text-align: center;
    padding-inline: 0.2rem;
  }
  .sortbtn {
    display: inline-flex;
    flex-direction: column;
    align-items: center;
    gap: 0.05em;
    background: none;
    border: 1px solid transparent;
    border-radius: 6px;
    padding: 0.2em 0.45em;
    font: inherit;
    color: inherit;
    cursor: pointer;
  }
  .sortbtn small {
    font-weight: 400;
    font-size: 0.68rem;
    color: var(--fg-faint);
  }
  .sortbtn:hover {
    border-color: var(--border-strong);
    color: var(--fg);
  }
  .sortbtn.on {
    color: var(--accent);
    border-color: var(--accent);
  }
  .arrow {
    font-size: 0.75rem;
    line-height: 1;
  }
  tbody th {
    font-weight: 500;
    max-width: 11rem;
  }
  .rowbtn,
  .rowname {
    display: inline-flex;
    align-items: center;
    gap: 0.45em;
    background: none;
    border: none;
    padding: 0.1em 0.2em;
    margin: -0.1em -0.2em;
    border-radius: 6px;
    font: inherit;
    color: var(--fg);
    text-align: left;
    cursor: pointer;
  }
  .rowbtn:hover {
    color: var(--accent);
    background: var(--bg-panel-hover);
  }
  /* the swatch: an empty ring until the row is picked */
  .sw {
    flex: none;
    width: 10px;
    height: 10px;
    border-radius: 3px;
    border: 1.5px solid var(--border-strong);
    background: var(--row-color, transparent);
  }
  tr.picked .sw,
  .detail .sw {
    border-color: var(--row-color);
  }
  tr.picked th {
    box-shadow: inset 3px 0 0 var(--row-color);
  }
  .disp-row {
    --row-color: var(--cmp-0);
  }
  .disp-row th,
  .disp-row td {
    border-bottom: 2px solid var(--border-strong);
    font-weight: 600;
  }
  .disp-row th {
    box-shadow: inset 3px 0 0 var(--row-color);
  }
  td.cell {
    text-align: center;
    font-family: var(--font-mono);
    width: 5rem;
    min-width: 5rem;
    padding-inline: 0.25rem;
    line-height: 1.2;
    /* the 2px surface gap between adjacent fills */
    border-left: 2px solid var(--bg-elev);
    cursor: help;
  }
  /* the how-tag: body face, small, wraps inside the cell */
  td.cell .how {
    display: block;
    margin-top: 0.15rem;
    font-family: var(--font-body);
    font-size: 0.62rem;
    font-weight: 400;
    line-height: 1.15;
    color: var(--fg-muted);
    white-space: normal;
  }
  .self {
    font-weight: 400;
    font-size: 0.7rem;
    color: var(--fg-faint);
  }
  /* a sequential fill: the percentage sets the tint */
  td.cell {
    background: color-mix(in oklab, var(--accent) calc(var(--fill, 0) * 0.45%), transparent);
  }
  td.lvn {
    color: var(--fg-faint);
    background: none;
  }
  .score {
    display: inline-flex;
    align-items: baseline;
    gap: 0.3em;
    white-space: nowrap;
  }
  .pct {
    font-family: var(--font-body);
    font-size: 0.72rem;
    font-weight: 600;
    color: var(--fg);
  }
  td.ahead {
    box-shadow: inset 0 0 0 2px var(--accent);
    font-weight: 700;
  }
  td.empty {
    text-align: center;
    color: var(--fg-faint);
    padding-block: 1rem;
  }
  .key,
  .also {
    display: flex;
    flex-wrap: wrap;
    gap: 0.3rem 1rem;
    margin: 0.6rem 0 0;
    font-size: 0.76rem;
    color: var(--fg-faint);
  }
  .also {
    display: block;
  }
  .ring-key {
    display: inline-block;
    width: 0.9em;
    height: 0.9em;
    border: 2px solid var(--accent);
    border-radius: 3px;
    vertical-align: -0.15em;
  }

  /* ---- the picks ---- */
  .details {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(min(100%, 30rem), 1fr));
    gap: 1.1rem;
    margin-top: 1.6rem;
  }
  .hint-card {
    color: var(--fg-faint);
    font-size: 0.9rem;
    margin: 0;
  }
  .detail {
    border-top: 3px solid var(--row-color);
    min-width: 0;
  }
  .detail header {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    gap: 0.3rem 0.8rem;
  }
  .detail h3 {
    display: inline-flex;
    align-items: center;
    gap: 0.45em;
    margin: 0;
    font-size: 1.2rem;
  }
  .author {
    color: var(--fg-muted);
    font-size: 0.85rem;
  }
  .links {
    display: inline-flex;
    gap: 0.8em;
    font-size: 0.82rem;
    margin-left: auto;
  }
  .close {
    background: none;
    border: 1px solid var(--border);
    border-radius: 6px;
    color: var(--fg-faint);
    font: inherit;
    font-size: 0.8rem;
    padding: 0.1em 0.45em;
    cursor: pointer;
  }
  .close:hover {
    color: var(--fg);
    border-color: var(--border-strong);
  }
  .closest-line {
    font-size: 0.88rem;
    color: var(--fg-muted);
    margin: 0.7rem 0 0;
  }
  .scorecard {
    margin: 0.9rem 0 0;
    display: grid;
    gap: 0.35rem;
  }
  .sc {
    display: grid;
    grid-template-columns: 9.5rem minmax(0, 1fr);
    gap: 0.6rem;
    align-items: baseline;
    font-size: 0.83rem;
  }
  .sc dt {
    display: flex;
    align-items: center;
    gap: 0.4em;
    flex-wrap: wrap;
    font-weight: 600;
  }
  .sc dd {
    margin: 0;
    color: var(--fg-muted);
  }
  .clauses {
    margin-top: 0.2rem;
    font-size: 0.78rem;
    color: var(--fg-faint);
  }
  .clauses code {
    font-size: 0.85em;
    color: var(--fg-muted);
  }
  .sym {
    font-family: var(--font-mono);
    padding: 0.05em 0.35em;
    border-radius: 5px;
  }
  .sym.lv1 {
    background: color-mix(in oklab, var(--accent) 18%, transparent);
  }
  .sym.lv2 {
    background: color-mix(in oklab, var(--accent) 42%, transparent);
  }
  .sc-how {
    font-weight: 400;
    color: var(--fg-muted);
  }
  .tag {
    font-size: 0.68rem;
    font-weight: 600;
    color: var(--accent);
    border: 1px solid var(--accent);
    border-radius: 999px;
    padding: 0 0.5em;
  }
  .detail h4 {
    margin: 1.1rem 0 0.3rem;
    font-size: 0.95rem;
  }
  .prose {
    font-size: 0.88rem;
    color: var(--fg-muted);
    line-height: 1.6;
  }
  .prose :global(p) {
    margin: 0 0 0.6rem;
  }
  .prose :global(ul),
  .prose :global(ol) {
    margin: 0 0 0.6rem;
    padding-left: 1.2em;
  }
  .prose :global(strong) {
    color: var(--fg);
  }
  .prose :global(code) {
    font-size: 0.9em;
  }

  /* ---- tooltip ---- */
  .tip {
    position: fixed;
    z-index: 50;
    max-width: 22rem;
    padding: 0.5rem 0.7rem;
    border: 1px solid var(--border-strong);
    border-radius: 10px;
    background: var(--bg-elev);
    box-shadow: var(--shadow-lift);
    font-size: 0.78rem;
    line-height: 1.45;
    color: var(--fg-muted);
    pointer-events: none;
  }
  .tip-title {
    font-weight: 600;
    color: var(--fg);
    margin-bottom: 0.2rem;
  }
  .tip-body :global(b) {
    color: var(--fg);
  }
  .tip-body :global(.tip-clauses) {
    font-family: var(--font-mono);
    font-size: 0.9em;
  }

  @media (max-width: 960px) {
    .top {
      grid-template-columns: minmax(0, 1fr);
    }
    .right {
      position: static;
      max-width: 26rem;
      margin: 0 auto;
    }
  }
  /* the simple reading style keeps the table (it is the chart's table view)
     and drops the radar */
  :global(:root[data-style='simple']) .right {
    display: none;
  }
  :global(:root[data-style='simple']) .top {
    grid-template-columns: minmax(0, 1fr);
  }
</style>
