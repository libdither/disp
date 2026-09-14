<script lang="ts">
  // Progress & Comparisons: the survey's master table as a sortable score
  // matrix, a radar of disp against up to two picked languages, and each
  // pick's scorecard notes plus write-up excerpts. All of it comes from
  // research/awesome-langs through the virtual:awesome-langs module.
  import { onMount } from 'svelte'
  import { replaceState } from '$app/navigation'
  import Radar, { type RadarSeries } from './Radar.svelte'
  import ClauseDots, { parseClauses, splitClause } from './ClauseDots.svelte'
  import { AXIS_IDS, LEVEL_WORD, type Axis, type AxisId, type Lang, type LangsData, type Score } from './types'

  interface Props {
    data: LangsData
  }
  let { data }: Props = $props()

  // colour follows the entity: a pick keeps its slot until it is dropped
  const SLOT_COLORS = ['var(--cmp-1)', 'var(--cmp-2)']
  let picks = $state<{ slug: string; slot: number }[]>([])
  // an axis, or 'frontier': Pareto layers, the average breaking ties inside one
  type SortKey = AxisId | 'frontier'
  let sortAxis = $state<SortKey | null>(null)
  let sortDesc = $state(true)
  let onlyAhead = $state(false)
  let query = $state('')

  const scored = $derived(data.langs.filter((l) => l.scored))
  const unscored = $derived(data.langs.filter((l) => !l.scored))
  const value = (s: Score) => s.pct ?? (s.level == null ? null : s.level * 50)
  const average = (scores: Record<AxisId, Score>) =>
    Math.round(AXIS_IDS.reduce((sum, id) => sum + (value(scores[id]) ?? 0), 0) / AXIS_IDS.length)
  const dispAverage = $derived(average(data.disp))

  // ---- Pareto dominance among the surveyed languages (disp is a reference, not a member).
  // Layer 0 is the frontier: nobody matches-or-beats it on every axis at once. Peeling
  // the frontier off repeatedly layers the rest — the sort an average can't give,
  // because a single exceptional axis keeps a project on an early layer.
  const vec = (l: Lang) => AXIS_IDS.map((id) => value(l.scores[id]) ?? 0)
  const dominates = (a: number[], b: number[]) => a.every((v, i) => v >= b[i]) && a.some((v, i) => v > b[i])
  const layerOf = $derived.by(() => {
    const out = new Map<string, number>()
    let pool = scored.map((l) => ({ slug: l.slug, v: vec(l) }))
    for (let layer = 0; pool.length; layer++) {
      const front = pool.filter((p) => !pool.some((q) => q !== p && dominates(q.v, p.v)))
      for (const p of front) out.set(p.slug, layer)
      pool = pool.filter((p) => !out.has(p.slug))
    }
    return out
  })
  const frontier = $derived(scored.filter((l) => layerOf.get(l.slug) === 0))

  const rank = (l: Lang, key: SortKey) =>
    key === 'frontier' ? -(layerOf.get(l.slug) ?? 99) * 1000 + average(l.scores) : (value(l.scores[key]) ?? -1)
  const rows = $derived.by(() => {
    const q = query.trim().toLowerCase()
    let out = scored.filter((l) => !q || l.name.toLowerCase().includes(q))
    if (onlyAhead) {
      const axes = sortAxis && sortAxis !== 'frontier' ? [sortAxis] : AXIS_IDS
      out = out.filter((l) => axes.some((id) => l.scores[id].ahead))
    }
    if (sortAxis) {
      const key = sortAxis
      out = [...out].sort((a, b) => (sortDesc ? rank(b, key) - rank(a, key) : rank(a, key) - rank(b, key)))
    }
    return out
  })

  // click a column: highest first, then reversed, then back to survey order
  function sortBy(id: SortKey): void {
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

  // one tooltip for matrix cells, axis headers and radar vertices alike
  let tip = $state<{ x: number; y: number; flip: boolean; title: string; html: string } | null>(null)
  const esc = (t: string) => t.replace(/&/g, '&amp;').replace(/</g, '&lt;')
  const dotCls = (v: string) => (v === '1' ? 'met' : v === '½' ? 'half' : v === '?' ? 'open' : v === '—' ? 'na' : 'no')
  // the itemized tips are tall, so open upward from the lower half of the screen
  const place = (ev: MouseEvent) => {
    const flip = ev.clientY > window.innerHeight * 0.5
    return { x: Math.min(ev.clientX + 14, window.innerWidth - 350), y: ev.clientY + (flip ? -14 : 14), flip }
  }
  function showTip(ev: MouseEvent, name: string, id: AxisId, s: Score): void {
    const axis = data.axes.find((a) => a.id === id)
    const word = s.level == null ? 'not scored' : LEVEL_WORD[s.level]
    const head = `<b>${s.raw}${s.pct != null ? ` ${s.pct}%${s.provisional ? '?' : ''}` : ''}</b> ${word}${s.tag ? ` · ${s.tag}` : ''}${s.ahead ? ' · <b>ahead of disp</b>' : ''}`
    // each grading clause's short label with this project's verdict; a ½ names
    // its route. Full criteria live on the axis header's tooltip; the scorecard
    // note below carries the narrative, so the one-line why is omitted here
    const parts = s.clauses ? parseClauses(s.clauses) : []
    const clauses = parts.length
      ? `<div class="tip-clauses">${parts
          .map((c, i) => {
            const cl = axis ? splitClause(axis.clauses[i] ?? `clause ${i + 1}`) : { label: `clause ${i + 1}` }
            return `<span class="tip-clause"><i class="cd ${dotCls(c.v)}"></i><span>${esc(cl.label)} — <b>${esc(c.word)}</b></span></span>`
          })
          .join('')}</div>${!s.noteHtml && s.whyHtml ? `<div class="tip-why">${s.whyHtml}</div>` : ''}`
      : ''
    tip = {
      ...place(ev),
      title: `${name} · ${id} ${axis?.short ?? ''}`,
      html: `${head}${clauses}${s.noteHtml ? `<br>${s.noteHtml}` : ''}`
    }
  }
  function showAxisTip(ev: MouseEvent, ax: Axis): void {
    const list = ax.clauses
      .map((c) => {
        const cl = splitClause(c)
        return `<span class="tip-clause"><i class="cd head"></i><span>${cl.key} <b>${esc(cl.label)}</b>${cl.detail ? ` — ${esc(cl.detail)}` : ''}</span></span>`
      })
      .join('')
    tip = {
      ...place(ev),
      title: `${ax.id} ${ax.name} — the clauses`,
      html: `<div class="tip-clauses">${list}</div><div class="tip-why">each clause scores 0, ½ or 1; the percentage is their mean · click to sort</div>`
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

  // ---- the Pareto scatter: any two axes, dots grouped on shared coordinates
  let px = $state<AxisId>('G2')
  let py = $state<AxisId>('G4')
  const PW = 560
  const PH = 340
  const PL = 42
  const PR = 110
  const PT = 16
  const PB = 34
  const sx = (v: number) => PL + (v / 100) * (PW - PL - PR)
  const sy = (v: number) => PH - PB - (v / 100) * (PH - PT - PB)
  interface PGroup {
    x: number
    y: number
    langs: Lang[]
    onFrontier: boolean // someone at this point is on the five-axis frontier
  }
  const pgroups = $derived.by(() => {
    const m = new Map<string, PGroup>()
    for (const l of scored) {
      const x = value(l.scores[px]) ?? 0
      const y = value(l.scores[py]) ?? 0
      const k = `${x},${y}`
      const g = m.get(k) ?? { x, y, langs: [], onFrontier: false }
      g.langs.push(l)
      g.onFrontier ||= layerOf.get(l.slug) === 0
      m.set(k, g)
    }
    return [...m.values()]
  })
  // maximal points for the picked pair, left to right; the stair joins them
  const pairFront = $derived(
    pgroups
      .filter((g) => !pgroups.some((h) => h.x >= g.x && h.y >= g.y && (h.x > g.x || h.y > g.y)))
      .sort((a, b) => a.x - b.x)
  )
  const stairPath = $derived.by(() => {
    if (!pairFront.length) return ''
    let d = `M ${sx(pairFront[0].x)} ${sy(pairFront[0].y)}`
    for (let i = 1; i < pairFront.length; i++) d += ` H ${sx(pairFront[i].x)} V ${sy(pairFront[i].y)}`
    return d
  })
  const dxy = $derived({ x: value(data.disp[px]) ?? 0, y: value(data.disp[py]) ?? 0 })
  // colour follows the entity: a picked language keeps its slot colour here too
  const groupColor = (g: PGroup) => {
    for (const l of g.langs) {
      const p = pickOf(l.slug)
      if (p) return SLOT_COLORS[p.slot]
    }
    return null
  }
  const plabel = (g: PGroup) =>
    (g.langs[0].name.length > 18 ? g.langs[0].name.slice(0, 17) + '…' : g.langs[0].name) +
    (g.langs.length > 1 ? ` +${g.langs.length - 1}` : '')
  function showGroupTip(ev: MouseEvent, g: PGroup): void {
    tip = {
      ...place(ev),
      title: g.langs.length > 1 ? `${g.langs.length} projects share this point` : g.langs[0].name,
      html: g.langs
        .map(
          (l) =>
            `<b>${l.name}</b> — ${px} ${g.x}% · ${py} ${g.y}%${layerOf.get(l.slug) === 0 ? ' · <b>five-axis frontier</b>' : ''}`
        )
        .join('<br>')
    }
  }
</script>

<div class="compare">
  <div class="top">
    <div class="left">
      <div class="toolbar">
        <input type="search" placeholder="filter by name" bind:value={query} aria-label="filter languages" />
        <label class="chk">
          <input type="checkbox" bind:checked={onlyAhead} />
          only ahead of disp {sortAxis && sortAxis !== 'frontier' ? `on ${sortAxis}` : '(any axis)'}
        </label>
        <span class="hint">click an axis to sort, or Project for the Pareto sort · click a name to compare (up to two)</span>
      </div>
      <div class="matrix-wrap">
        <table class="matrix">
          <thead>
            <tr>
              <th class="name">
                <button class="sortbtn name-sort" class:on={sortAxis === 'frontier'} onclick={() => sortBy('frontier')} title="Pareto sort: first the frontier (no surveyed project matches or beats them on every axis at once), then each successive peel; the average breaks ties inside a layer">
                  Project
                  <small>Pareto sort</small>
                  {#if sortAxis === 'frontier'}<span class="arrow" aria-hidden="true">{sortDesc ? '↓' : '↑'}</span>{/if}
                </button>
              </th>
              {#each data.axes as ax (ax.id)}
                <th class="axh">
                  <button class="sortbtn" class:on={sortAxis === ax.id} onclick={() => sortBy(ax.id)} onmouseenter={(e) => showAxisTip(e, ax)} onmouseleave={hideTip}>
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
              <th scope="row">
                <span class="namecell">
                  <span class="rowname"><i class="sw" aria-hidden="true"></i>disp <small class="self">self-assessed</small></span>
                  <span class="avg" title="average of the five">{dispAverage}%</span>
                </span>
              </th>
              {#each data.axes as ax (ax.id)}
                {@const s = data.disp[ax.id]}
                <td class="cell {lv(s.level)}" style:--fill={value(s) ?? 0} onmouseenter={(e) => showTip(e, 'disp', ax.id, s)} onmouseleave={hideTip}>
                  <span class="score">{#if s.clauses}<ClauseDots clauses={s.clauses} size={8} titles={false} />{:else}{s.raw}{/if}<span class="pct">{pctText(s)}</span></span>
                  {#if s.tag}<small class="how">{s.tag}</small>{/if}
                </td>
              {/each}
            </tr>
            {#each rows as lang (lang.slug)}
              {@const pick = pickOf(lang.slug)}
              <tr class:picked={!!pick} style:--row-color={pick ? SLOT_COLORS[pick.slot] : null}>
                <th scope="row">
                  <span class="namecell">
                    <button class="rowbtn" aria-pressed={!!pick} onclick={() => toggle(lang.slug)}>
                      <i class="sw" aria-hidden="true"></i>{lang.name}
                    </button>
                    <span class="avg" title="average of the five">{average(lang.scores)}%</span>
                    {#if layerOf.get(lang.slug) === 0}<small class="fr" title="on the five-axis Pareto frontier: no surveyed project matches or beats it on every axis at once">frontier</small>{/if}
                  </span>
                </th>
                {#each data.axes as ax (ax.id)}
                  {@const s = lang.scores[ax.id]}
                  <td class="cell {lv(s.level)}" class:ahead={s.ahead} style:--fill={value(s) ?? 0} onmouseenter={(e) => showTip(e, lang.name, ax.id, s)} onmouseleave={hideTip}>
                    <span class="score">{#if s.clauses}<ClauseDots clauses={s.clauses} size={8} titles={false} />{:else}{s.raw}{/if}<span class="pct">{pctText(s)}</span></span>
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
        <span>percent of what disp needs on the axis; one dot per clause (hover an axis for them)</span>
        <span><ClauseDots clauses="1" titles={false} size={9} /> met</span>
        <span><ClauseDots clauses="½" titles={false} size={9} /> halfway</span>
        <span><ClauseDots clauses="0" titles={false} size={9} /> not met</span>
        <span><ClauseDots clauses="?" titles={false} size={9} /> open</span>
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
      <section class="pareto">
        <div class="pareto-head">
          <h2>The Pareto frontier</h2>
          <label>x <select bind:value={px}>{#each data.axes as ax (ax.id)}<option value={ax.id}>{ax.id} {ax.short}</option>{/each}</select></label>
          <label>y <select bind:value={py}>{#each data.axes as ax (ax.id)}<option value={ax.id}>{ax.id} {ax.short}</option>{/each}</select></label>
        </div>
        <p class="pareto-note">
          A project is on the frontier when no other surveyed project matches or beats it on every
          axis at once — the exceptional projects an average buries. On all five axes that is
          {#each frontier as l, i (l.slug)}{i ? ', ' : ''}<b>{l.name}</b>{/each}. The table's Pareto
          sort orders by how many frontier peels a project survives. The stair below is the frontier
          for just the two picked axes; disp is drawn for reference, not as a member.
        </p>
        <svg class="pareto-plot" viewBox="0 0 {PW} {PH}" role="img" aria-label="the surveyed languages on {px} against {py}, with the Pareto stair for the pair">
          {#each [0, 25, 50, 75, 100] as t (t)}
            <line class="grid" x1={sx(t)} y1={sy(0)} x2={sx(t)} y2={sy(100)} />
            <line class="grid" x1={sx(0)} y1={sy(t)} x2={sx(100)} y2={sy(t)} />
            <text class="tick" x={sx(t)} y={PH - PB + 14} text-anchor="middle">{t}</text>
            <text class="tick" x={PL - 6} y={sy(t) + 3} text-anchor="end">{t}</text>
          {/each}
          <text class="axlab" x={(PL + PW - PR) / 2} y={PH - 2} text-anchor="middle">{px} {data.axes.find((a) => a.id === px)?.short}</text>
          <text class="axlab" x={PL} y={PT - 6}>{py} {data.axes.find((a) => a.id === py)?.short} ↑</text>
          <path class="stair" d={stairPath} />
          {#each pgroups as g (`${g.x},${g.y}`)}
            <g class="pt" class:front={g.onFrontier} onmouseenter={(e) => showGroupTip(e, g)} onmouseleave={hideTip} role="img" aria-label="{g.langs.map((l) => l.name).join(', ')} at {px} {g.x}%, {py} {g.y}%">
              <circle cx={sx(g.x)} cy={sy(g.y)} r={4 + Math.min(3, g.langs.length - 1)} style:fill={groupColor(g) ?? undefined} />
            </g>
          {/each}
          {#each pairFront as g (`${g.x},${g.y}`)}
            <text class="plab" x={sx(g.x) + 9} y={sy(g.y) + 3}>{plabel(g)}</text>
          {/each}
          <g class="disp-pt">
            <path d="M {sx(dxy.x)} {sy(dxy.y) - 6} l 6 6 l -6 6 l -6 -6 Z" />
            <text class="plab disp-lab" x={sx(dxy.x) + 10} y={sy(dxy.y) - 6}>disp</text>
          </g>
        </svg>
        <p class="pareto-key">
          <span>● a project · a bigger dot is several sharing the point — hover them</span>
          <span><i class="fr-ring" aria-hidden="true"></i> on the five-axis frontier</span>
          <span>the stair joins the frontier for this pair</span>
          <span>◆ disp, reference</span>
        </p>
      </section>
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
                  <span class="sym {lv(s.level)}">{#if s.clauses}<ClauseDots clauses={s.clauses} labels={ax.clauses} size={9} /> {pctText(s)}{:else}{s.raw} {pctText(s)}{/if}</span>
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
    <div class="tip" style:left="{tip.x}px" style:top="{tip.y}px" style:transform={tip.flip ? 'translateY(-100%)' : undefined} role="tooltip">
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
  .name-sort {
    flex-direction: row;
    align-items: baseline;
    gap: 0.4em;
    padding-left: 0.3em;
  }
  .namecell {
    display: flex;
    align-items: baseline;
    justify-content: space-between;
    gap: 0.6rem;
  }
  .avg {
    flex: none;
    font-family: var(--font-body);
    font-size: 0.72rem;
    font-weight: 500;
    color: var(--fg-faint);
    font-variant-numeric: tabular-nums;
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
    align-items: center;
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
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
    margin-top: 0.25rem;
  }
  .tip-body :global(.tip-clause) {
    display: flex;
    gap: 0.45em;
    align-items: baseline;
  }
  .tip-body :global(i.cd) {
    flex: none;
    width: 0.6em;
    height: 0.6em;
    border-radius: 50%;
    border: 1.5px solid var(--fg-faint);
    transform: translateY(0.05em);
  }
  .tip-body :global(i.cd.met) {
    background: var(--accent);
    border-color: var(--accent);
  }
  .tip-body :global(i.cd.half) {
    border-color: var(--accent);
    background: linear-gradient(90deg, var(--accent) 50%, transparent 50%);
  }
  .tip-body :global(i.cd.open) {
    border-style: dashed;
  }
  .tip-body :global(i.cd.na) {
    border-style: dotted;
  }
  .tip-body :global(i.cd.head) {
    border-color: var(--fg-muted);
    width: 0.45em;
    height: 0.45em;
  }
  .tip-body :global(.tip-why) {
    margin-top: 0.3rem;
    color: var(--fg-faint);
    font-style: italic;
  }

  /* ---- pareto sort + scatter ---- */
  .fr {
    font-size: 0.62rem;
    color: var(--accent);
    border: 1px solid color-mix(in oklab, var(--accent) 45%, transparent);
    border-radius: 999px;
    padding: 0 0.45em;
    white-space: nowrap;
  }
  .pareto {
    margin-top: 1.6rem;
  }
  .pareto-head {
    display: flex;
    align-items: baseline;
    gap: 0.9rem;
    flex-wrap: wrap;
  }
  .pareto-head h2 {
    font-size: 1.05rem;
    margin: 0;
  }
  .pareto-head label {
    font-size: 0.78rem;
    color: var(--fg-muted);
    display: inline-flex;
    gap: 0.35em;
    align-items: center;
  }
  .pareto-head select {
    font: inherit;
    font-size: 0.78rem;
    color: var(--fg);
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    padding: 0.15em 0.3em;
  }
  .pareto-note {
    margin: 0.4rem 0 0.7rem;
    font-size: 0.85rem;
    color: var(--fg-muted);
    max-width: 46rem;
  }
  .pareto-plot {
    width: 100%;
    max-width: 40rem;
    height: auto;
    display: block;
  }
  .pareto-plot .grid {
    stroke: var(--border);
    stroke-width: 1;
  }
  .pareto-plot .tick,
  .pareto-plot .axlab,
  .pareto-plot .plab {
    font-size: 10px;
    fill: var(--fg-faint);
    font-family: var(--font-body);
  }
  .pareto-plot .axlab {
    fill: var(--fg-muted);
    font-weight: 600;
  }
  .pareto-plot .stair {
    fill: none;
    stroke: var(--accent);
    stroke-width: 1.5;
    opacity: 0.7;
  }
  .pareto-plot .pt circle {
    fill: color-mix(in oklab, var(--fg-muted) 55%, transparent);
    stroke: var(--bg);
    stroke-width: 1;
  }
  .pareto-plot .pt.front circle {
    stroke: var(--accent);
    stroke-width: 1.8;
  }
  .pareto-plot .plab {
    font-size: 9.5px;
    fill: var(--fg-muted);
  }
  .pareto-plot .disp-pt path {
    fill: var(--cmp-0);
  }
  .pareto-plot .disp-lab {
    fill: var(--cmp-0);
    font-weight: 700;
  }
  .pareto-key {
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem 1rem;
    margin: 0.4rem 0 0;
    font-size: 0.75rem;
    color: var(--fg-faint);
  }
  .fr-ring {
    display: inline-block;
    width: 0.65em;
    height: 0.65em;
    border-radius: 50%;
    border: 1.8px solid var(--accent);
    vertical-align: middle;
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
