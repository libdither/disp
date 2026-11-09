<script lang="ts">
  // The nine-rule linear tree calculus, step by step, on a clone of the playground's tree view. A program is
  // written with binders; bracket abstraction takes them off one step at a time (the binder steps), then the
  // tree runs one rule at a time. The reel under the tree is the whole run; drag it to scrub.
  import { onDestroy, untrack } from 'svelte'
  import TreeCanvas, { type CanvasApi, type TreeLook } from './TreeCanvas.svelte'
  import Timeline from './Timeline.svelte'
  import { Store, R_BIND, R_LEAF, R_STEM, R_I, R_K, R_C, R_W, R_B, R_B2, R_T0, R_T1, R_T2, RULE_TEXT, BIND_TEXT, type Id } from './store.ts'
  import { Trace, type Mode } from './trace.ts'
  import { layoutScene, keyPath, K_APP, K_POD, K_VAR, K_LAM, type Scene, type VNode } from './layout.ts'
  import { reachOf } from './motion.ts'
  import { stepGuide } from './guides.ts'
  import { EXAMPLES, loadProgram } from './examples.ts'

  const VAR_COLORS = ['#1971c2', '#e03131', '#9c36b5', '#2b8a3e', '#c2255c', '#0b7285', '#b35c00', '#5f3dc4']
  const LOOKS: TreeLook[] = ['bold', 'quiet', 'garden', 'moss', 'ink', 'tree']

  let exampleId = $state('s')
  let source = $state(EXAMPLES.find((e) => e.id === 's')!.source)
  let error = $state('')
  let store = new Store()
  let trace = $state.raw<Trace | null>(null)
  let version = $state(0)
  let at = $state(0)
  let playing = $state(false)
  let speed = $state(4)
  let mode = $state<Mode>('eager')
  let sk = $state(true)
  let labels = $state(true)
  let hideBuild = $state(true)
  let look = $state<TreeLook>('bold')
  let scene = $state.raw<Scene | null>(null)
  let animMs = $state(0)
  /// the caption sits over the canvas's bottom, taller once it wraps on a narrow stage
  let stageW = $state(1000)
  let hover = $state('')
  let canvas: CanvasApi | null = null
  const open = new Set<Id>()
  const closed = new Set<Id>()
  const varIndex = new Map<string, number>()
  const varColor = (name: string) => VAR_COLORS[(varIndex.get(name.replace(/′+$/, '')) ?? 0) % VAR_COLORS.length]

  const example = $derived(EXAMPLES.find((e) => e.id === exampleId))

  // ---- loading and computing the run -------------------------------------------
  let computing = 0
  function load() {
    playing = false
    error = ''
    store = new Store()
    open.clear()
    closed.clear()
    varIndex.clear()
    let root: Id
    try {
      root = loadProgram(store, source).root
    } catch (e) {
      error = (e as Error).message
      trace = null
      scene = null
      return
    }
    trace = new Trace(store, root, mode)
    at = 0
    drawnAt = -1
    reachKey = ''
    computeMore()
    draw(0)
    queueMicrotask(() => canvas?.fit())
  }
  /// frames come in a slice at a time, so a long run never blocks the page
  function computeMore() {
    cancelAnimationFrame(computing)
    const t = trace
    if (!t || t.end) return
    const tick = () => {
      if (trace !== t) return
      const more = t.advance(12)
      version++
      if (more) computing = requestAnimationFrame(tick)
    }
    computing = requestAnimationFrame(tick)
  }
  onDestroy(() => {
    // also runs when the page renders on the server, where there are no frames to cancel
    if (typeof window === 'undefined') return
    cancelAnimationFrame(computing)
    clearTimeout(timer)
  })

  function pick(id: string) {
    const ex = EXAMPLES.find((e) => e.id === id)
    if (!ex) return
    exampleId = id
    source = ex.source
    load()
  }
  $effect(() => {
    void mode
    untrack(() => load())
  })

  // ---- drawing --------------------------------------------------------------------
  let drawnAt = -1
  let lastNearOpen = new Set<Id>()
  let reachKey = ''
  let reach: Scene['reach'] = undefined
  /// how far every frame of a short finished run reaches, so the view holds one zoom for all of them
  function runReach(): Scene['reach'] {
    if (!trace?.end || trace.last > 400) return undefined
    const key = `${trace.frames[0]}:${trace.last}:${open.size}:${closed.size}:${sk}:${labels}`
    if (key === reachKey) return reach
    reachKey = key
    let nodes = 0
    const t = trace
    function* scenes() {
      for (const f of t.frames) {
        const sc = layoutScene(store, [f], { open: new Set(open), closed: new Set(closed) }, { labels, sk, foldData: labels ? 40 : undefined })
        if ((nodes += sc.nodes.length) > 150_000) return
        yield sc
      }
    }
    const out = reachOf(scenes())
    return (reach = nodes > 150_000 ? undefined : out)
  }
  /// a finished answer is data, so reading its shapes as combinators would only mislead
  const readsSk = () => sk && !(trace?.end === 'normal' && at === trace.last)
  function draw(ms: number) {
    if (!trace) {
      scene = null
      return
    }
    animMs = ms
    const was = drawnAt
    drawnAt = at
    const root = trace.frames[at]
    const keepOpen = ms > 0 && at > was && at - was <= 4 ? lastNearOpen : undefined
    const next = layoutScene(store, [root], { open, closed }, {
      nextPath: trace.nextPath(root),
      alsoNext: mode === 'parallel' ? trace.readyPaths(root) : undefined,
      labels,
      sk: readsSk(),
      foldData: labels ? 40 : undefined,
      keepOpen,
      reads: trace.readsFrom(at, 3),
      repeats: 60,
      repeatsKeep: 'near'
    })
    lastNearOpen = next.nearOpen
    if (ms > 0 && was >= 0 && at > was && at - was <= 4) next.guide = stepGuide(store, trace, was)
    next.reach = runReach()
    for (const n of next.nodes) if ((n.kind === K_VAR || n.kind === K_LAM) && n.text) {
      const base = n.text.replace(/′+$/, '')
      if (!varIndex.has(base)) varIndex.set(base, varIndex.size)
    }
    scene = next
  }
  $effect(() => {
    void sk
    void labels
    untrack(() => draw(0))
  })

  // ---- transport --------------------------------------------------------------------
  /// how many binder steps come first (by default every binder comes off before anything runs)
  const bindEnd = $derived.by(() => {
    void version
    if (!trace) return 0
    let f = 0
    while (f < trace.last && trace.rules[f + 1] === R_BIND) f++
    return f
  })
  const absorbed = (f: number) => !!trace && trace.folds(f)
  function seek(i: number, ms = 0) {
    at = Math.max(0, Math.min(trace?.last ?? 0, i))
    draw(ms)
  }
  const stops = $derived.by(() => {
    void version
    if (!trace) return null
    const out = [0]
    for (let f = 0; f < trace.last; ) {
      f++
      if (hideBuild) while (f < trace.last && f !== bindEnd && absorbed(f)) f++
      out.push(f)
      if (out.length > 400) return null
    }
    return out
  })
  function snap(i: number): number {
    if (!hideBuild || !trace) return i
    let f = i
    while (f < trace.last && f !== bindEnd && absorbed(f)) f++
    return f
  }
  function stepForward(ms = 220): boolean {
    if (!trace) return false
    let f = at + 1
    if (f > trace.last) return false
    if (hideBuild) while (f < trace.last && f !== bindEnd && absorbed(f)) f++
    // a binder rewrite moves more than a rule firing, and a drop, a copy or a triage pick needs a beat to see
    const r = trace.rules[at + 1]
    const path = r === R_BIND ? trace.nextPath(trace.frames[at]) : null
    const knocks = path ? ['id', 'eta'].includes(store.bindKind(trace.subterm(trace.frames[at], path)) ?? '') : false
    const beat = r === R_K || r === R_W || r === R_T0 || r === R_T1 || r === R_T2
    seek(f, ms > 0 && knocks ? Math.max(ms, 460) : ms > 0 && r === R_BIND ? Math.max(ms, 340) : ms > 0 && !playing && beat ? Math.max(ms, 280) : ms)
    return true
  }
  function stepBack() {
    if (!trace || at === 0) return
    let f = at - 1
    if (hideBuild) while (f > 0 && f !== bindEnd && absorbed(f)) f--
    seek(f, 260)
  }
  let timer = 0
  function tick() {
    if (!playing) return
    const ms = Math.max(50, Math.min(900, 1000 / speed))
    if (!stepForward(Math.round(Math.min(ms * 0.92, 520)))) {
      if (trace?.end) {
        playing = false
        return
      }
    }
    timer = window.setTimeout(tick, ms)
  }
  function togglePlay() {
    if (!trace) return
    if (!playing && at >= trace.last && trace.end) seek(0)
    playing = !playing
    clearTimeout(timer)
    if (playing) tick()
  }
  function stop() {
    playing = false
    clearTimeout(timer)
  }
  function onKey(e: KeyboardEvent) {
    if ((e.target as HTMLElement).closest('textarea, input, select')) return
    if (e.key === 'ArrowRight') stepForward()
    else if (e.key === 'ArrowLeft') stepBack()
    else if (e.key === ' ') togglePlay()
    else if (e.key === 'Home') seek(0)
    else if (e.key === 'End') seek(trace?.last ?? 0)
    else return
    if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') stop()
    e.preventDefault()
  }

  // ---- clicking: open a fold, or fire an application early -----------------------------
  function handleClick(n: VNode) {
    if (!trace) return
    if (n.kind === K_POD) {
      open.add(n.id)
      closed.delete(n.id)
      draw(220)
      return
    }
    if (n.tag) {
      closed.add(n.id)
      open.delete(n.id)
      draw(220)
      return
    }
    if (n.kind === K_APP && n.rule) {
      stop()
      const from = at
      trace.fireAt(at, keyPath(n.key))
      version++
      computeMore()
      drawnAt = from
      seek(from + 1, 300)
    }
  }
  function handleHover(n: VNode | null) {
    if (!n) return void (hover = '')
    if (n.kind === K_APP && n.rule) hover = n.rule === R_BIND ? 'a binder step' : RULE_TEXT[n.rule]
    else if (n.kind === K_POD) hover = `${n.text}: click to open`
    else if (n.comb) hover = { I: 'I: gives its argument back', K: 'K q: drops its argument, gives q', C: 'C q r: x goes left, q x r', W: 'W b: copies its argument, b x x', B: 'B f b: x goes right, f (b x)', 'B′': 'B′ f g b: x goes right twice, f (g (b x))', T: 'a triage: the argument picks an arm' }[n.comb] ?? ''
    else if (n.kind === K_LAM) hover = `the binder of ${n.text}`
    else if (n.kind === K_VAR) hover = `the symbol ${n.text}`
    else hover = ''
  }

  // ---- what is about to happen, and the counts so far ------------------------------------
  const caption = $derived.by(() => {
    void version
    if (!trace) return ''
    if (at >= trace.last) {
      if (!trace.end) return 'computing…'
      return trace.end === 'normal' ? 'Done: the answer is a tree.' : trace.end === 'stuck' ? 'Done: what is left waits on a symbol.' : 'Stopped at the step limit.'
    }
    const r = trace.rules[at + 1]
    if (r === R_BIND) {
      const path = trace.nextPath(trace.frames[at])
      const kind = path ? store.bindKind(trace.subterm(trace.frames[at], path)) : null
      return kind ? `Binder: ${BIND_TEXT[kind]}` : 'A binder comes off'
    }
    return mode === 'parallel' ? `Next: every ready redex fires (${RULE_TEXT[r]} among them)` : `Next: ${RULE_TEXT[r]}`
  })
  const term = $derived.by(() => {
    void version
    void at
    return trace ? store.pretty(trace.frames[at], 70, readsSk()) : ''
  })
  const COUNTED: [number[], string, string][] = [
    [[R_I], 'I', 'ok'],
    [[R_K], 'K drops', 'warn'],
    [[R_C], 'C', 'left'],
    [[R_W], 'W copies', 'kw'],
    [[R_B, R_B2], 'B, B′', 'acc'],
    [[R_T0, R_T1, R_T2], 'triage', 'err'],
    [[R_LEAF, R_STEM], 'build', 'faint']
  ]
  const counts = $derived.by(() => {
    void version
    const hist = new Array(13).fill(0)
    if (trace) for (let f = 1; f <= at; f++) hist[trace.rules[f]]++
    return hist
  })
  const totals = $derived.by(() => {
    void version
    const hist = new Array(13).fill(0)
    if (trace) for (let f = 1; f <= trace.last; f++) hist[trace.rules[f]]++
    return hist
  })
  const sum = (h: number[], rs: number[]) => rs.reduce((a, r) => a + h[r], 0)
  const phaseBind = $derived(Math.min(at, bindEnd))
  const phaseRun = $derived(Math.max(0, at - bindEnd))
</script>

<svelte:window onkeydown={onKey} />

<div class="vis">
  <div class="top">
    <label class="ex">
      <span>Example</span>
      <select value={exampleId} onchange={(e) => pick((e.target as HTMLSelectElement).value)}>
        {#each EXAMPLES as ex (ex.id)}<option value={ex.id}>{ex.title}</option>{/each}
      </select>
    </label>
    {#if example && example.source === source}<p class="blurb">{example.blurb}</p>{/if}
  </div>
  <div class="src">
    <textarea bind:value={source} spellcheck="false" rows={Math.min(9, source.split('\n').length + 1)} aria-label="program"></textarea>
    <div class="srcside">
      <button class="run" onclick={load} title="Load the program and start from its first frame">Load</button>
      {#if error}<p class="err">{error}</p>{/if}
    </div>
  </div>

  <div class="stage" bind:clientWidth={stageW}>
    <TreeCanvas {scene} {animMs} {varColor} {look} steady={playing} falling={true} inset={stageW < 560 ? 44 : 18} onHover={handleHover} onClick={(n) => handleClick(n)} api={(a) => (canvas = a)} />
    <div class="caption">
      <div class="what">{caption}</div>
      <div class="term">{term}</div>
      {#if hover}<div class="hover">{hover}</div>{/if}
    </div>
  </div>

  <Timeline {trace} {at} lead={0} {version} {stops} onScrub={(i) => { stop(); seek(snap(i)) }} />
  <div class="transport">
    <button class="tb" onclick={() => { stop(); seek(0) }} title="Start (Home)" aria-label="start">⏮</button>
    <button class="tb" onclick={() => { stop(); stepBack() }} title="Step back (←)" aria-label="step back">◀</button>
    <button class="tb play" onclick={togglePlay} title="Play (space)" aria-label={playing ? 'pause' : 'play'}>{playing ? '⏸' : '▶'}</button>
    <button class="tb" onclick={() => { stop(); stepForward() }} title="Step (→)" aria-label="step">▶|</button>
    <button class="tb" onclick={() => { stop(); seek(trace?.last ?? 0) }} title="End (End)" aria-label="end">⏭</button>
    {#if bindEnd > 0}
      <button class="phase" class:on={at < bindEnd} onclick={() => { stop(); seek(0) }} title="Bracket abstraction: binders coming off, one step each">Binders <span>{phaseBind}/{bindEnd}</span></button>
    {/if}
    <button class="phase" class:on={at >= bindEnd} onclick={() => { stop(); seek(bindEnd) }} title="The tree running, one rule at a time">Run <span>{phaseRun}/{(trace?.last ?? 0) - bindEnd}{trace?.end ? '' : '…'}</span></button>
    <label class="speed">Speed <input type="range" min="1" max="40" bind:value={speed} aria-label="speed" /> <span>{speed}/s</span></label>
  </div>

  <div class="counts">
    {#each COUNTED as [rs, name, cls] (name)}
      <span class="count c-{cls}"><i></i>{name} <b>{sum(counts, rs)}</b><small>/{sum(totals, rs)}</small></span>
    {/each}
  </div>

  <div class="options">
    <label><input type="checkbox" bind:checked={sk} /> Show combinators</label>
    <label><input type="checkbox" bind:checked={labels} /> Fold named parts and numbers</label>
    <label title="Steps that only build a stem or a fork are taken in with the step before"><input type="checkbox" bind:checked={hideBuild} /> Skip steps that only build</label>
    <label>Order <select bind:value={mode}><option value="eager">one at a time</option><option value="parallel">all ready at once</option></select></label>
    <label>Look <select bind:value={look}>{#each LOOKS as l (l)}<option value={l}>{l}</option>{/each}</select></label>
  </div>
</div>

<style>
  .vis {
    display: flex;
    flex-direction: column;
    gap: 10px;
    margin: 18px 0 28px;
  }
  .top {
    display: flex;
    gap: 14px;
    align-items: baseline;
    flex-wrap: wrap;
  }
  .ex {
    display: flex;
    gap: 8px;
    align-items: baseline;
    font-size: 0.9rem;
  }
  .blurb {
    margin: 0;
    color: var(--fg-muted);
    font-size: 0.9rem;
    flex: 1 1 320px;
  }
  .src {
    display: flex;
    gap: 10px;
    align-items: flex-start;
  }
  textarea {
    flex: 1;
    font-family: 'JetBrains Mono Variable', monospace;
    font-size: 0.82rem;
    line-height: 1.45;
    padding: 8px 10px;
    border-radius: 8px;
    border: 1px solid var(--border-strong);
    background: var(--bg-code);
    color: var(--fg);
    resize: vertical;
    min-height: 2.6em;
  }
  .srcside {
    display: flex;
    flex-direction: column;
    gap: 6px;
    max-width: 260px;
  }
  .run {
    padding: 6px 14px;
    border-radius: 8px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg);
    cursor: pointer;
  }
  .err {
    margin: 0;
    color: var(--err);
    font-size: 0.82rem;
  }
  .stage {
    position: relative;
    height: min(64vh, 620px);
    min-height: 340px;
    border: 1px solid var(--border-strong);
    border-radius: 10px;
    overflow: hidden;
    background: var(--bg-code);
  }
  .caption {
    position: absolute;
    left: 10px;
    right: 10px;
    bottom: 8px;
    pointer-events: none;
    font-size: 0.82rem;
    display: flex;
    flex-direction: column;
    gap: 2px;
  }
  .caption .what {
    color: var(--fg);
    background: color-mix(in oklab, var(--bg-code) 82%, transparent);
    align-self: flex-start;
    padding: 2px 6px;
    border-radius: 6px;
  }
  .caption .term {
    font-family: 'JetBrains Mono Variable', monospace;
    color: var(--fg-muted);
    font-size: 0.74rem;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    background: color-mix(in oklab, var(--bg-code) 82%, transparent);
    align-self: flex-start;
    max-width: 100%;
    padding: 1px 6px;
    border-radius: 6px;
  }
  .caption .hover {
    color: var(--accent);
    align-self: flex-start;
    padding: 1px 6px;
  }
  .transport {
    display: flex;
    gap: 6px;
    align-items: center;
    flex-wrap: wrap;
  }
  .tb,
  .phase {
    padding: 4px 10px;
    border-radius: 7px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg);
    cursor: pointer;
    font-size: 0.85rem;
  }
  .tb.play {
    min-width: 2.6em;
  }
  .phase span {
    color: var(--fg-muted);
    font-variant-numeric: tabular-nums;
  }
  .phase.on {
    border-color: var(--accent);
    color: var(--accent);
  }
  .speed {
    display: flex;
    gap: 6px;
    align-items: center;
    font-size: 0.82rem;
    color: var(--fg-muted);
    margin-left: auto;
  }
  .counts {
    display: flex;
    gap: 12px;
    flex-wrap: wrap;
    font-size: 0.8rem;
    color: var(--fg-muted);
  }
  .count i {
    display: inline-block;
    width: 9px;
    height: 9px;
    border-radius: 3px;
    margin-right: 5px;
    background: var(--c);
  }
  .count b {
    color: var(--fg);
    font-variant-numeric: tabular-nums;
  }
  .count small {
    opacity: 0.7;
  }
  .count.c-ok {
    --c: var(--hl-number);
  }
  .count.c-warn {
    --c: var(--warn);
  }
  .count.c-left {
    --c: var(--hl-atom);
  }
  .count.c-kw {
    --c: var(--hl-kw);
  }
  .count.c-acc {
    --c: var(--accent);
  }
  .count.c-err {
    --c: var(--err);
  }
  .count.c-faint {
    --c: var(--fg-faint);
  }
  .options {
    display: flex;
    gap: 16px;
    flex-wrap: wrap;
    font-size: 0.82rem;
    color: var(--fg-muted);
  }
  .options label {
    display: flex;
    gap: 5px;
    align-items: center;
  }
  @media (max-width: 640px) {
    .src {
      flex-direction: column;
      align-items: stretch;
    }
    .srcside {
      max-width: none;
    }
    .speed {
      margin-left: 0;
    }
  }
</style>
