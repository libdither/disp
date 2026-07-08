<script lang="ts">
  // The interactive tree-calculus evaluator (a Svelte port of the original
  // walkthrough's in-page reducer). Type an expression, Step through
  // reductions, watch the rule letters fire.
  import { onDestroy } from 'svelte'
  import {
    parseTree,
    stepOnce,
    nextApplyToFire,
    childrenOf,
    pretty,
    natValue,
    treeEq,
    nodeCount,
    DEFS,
    type T
  } from '$lib/treecalc/treecalc'

  interface Preset {
    expr: string
    tip: string
  }
  const presets: Preset[] = [
    { expr: 'not false', tip: 'Booleans are the two smallest trees (true = t, false = t t); not is one triage — the F rule reads the argument\'s shape and picks a branch.' },
    { expr: 'shape_code (t t t)', tip: 'The F rule as a value: shape_code answers 0 for a leaf, 1 for a stem, 2 for a fork. Case analysis over arbitrary data is a rewrite rule.' },
    { expr: 'K t (t t)', tip: 'K discards its second argument — one K-rule firing after the stem rule assembles the redex.' },
    { expr: 'S K K (t t)', tip: 'S K K is the identity, assembled from S and K: the S rule duplicates the argument, then two K rules collapse it back out.' },
    { expr: 'I (S K K)', tip: 'The identity applied to itself-in-disguise: I x = x, and S K K IS I extensionally — but they are different trees. Equality of programs is not equality of behavior.' }
  ]

  let input = $state(presets[0].expr)
  let activeTip = $state(presets[0].tip)
  let cur = $state<T | null>(null)
  let ruleMsg = $state('')
  let stepCount = $state(0)
  let err = $state('')
  let autorun = $state(false)
  let timer: ReturnType<typeof setInterval> | undefined

  function load() {
    err = ''
    ruleMsg = ''
    stepCount = 0
    stopAuto()
    try {
      cur = parseTree(input)
    } catch (e) {
      cur = null
      err = e instanceof Error ? e.message : String(e)
    }
  }

  function step() {
    if (!cur) load()
    if (!cur) return
    const s = stepOnce(cur)
    if (!s) {
      ruleMsg = `normal form — ${pretty(cur)}`
      stopAuto()
      return
    }
    cur = s.next
    stepCount++
    ruleMsg = s.rule
  }

  function toggleAuto() {
    if (autorun) {
      stopAuto()
      return
    }
    if (!cur) load()
    autorun = true
    timer = setInterval(() => {
      if (!cur) return stopAuto()
      const s = stepOnce(cur)
      if (!s || stepCount > 900) {
        ruleMsg = cur ? `normal form — ${pretty(cur)}` : ''
        stopAuto()
        return
      }
      cur = s.next
      stepCount++
      ruleMsg = s.rule
    }, 160)
  }

  function stopAuto() {
    autorun = false
    if (timer) clearInterval(timer)
    timer = undefined
  }

  function usePreset(p: Preset) {
    input = p.expr
    activeTip = p.tip
    load()
  }

  onDestroy(stopAuto)

  // initial load
  load()

  // ---- rendering ----------------------------------------------------------

  interface VNode {
    path: string
    tree: T
    tag: T['tag']
    x: number
    y: number
    hot: boolean
    label: string | null
  }

  function nameOf(t: T): string | null {
    if (t.tag === 'apply') return null
    const n = natValue(t)
    if (n !== null && n >= 1) return String(n)
    if (nodeCount(t) < 3) return null
    for (const [name, d] of Object.entries(DEFS)) if (treeEq(t, d)) return name
    return null
  }

  const XS = 40
  const YS = 46
  let W = $state(620)
  let H = $state(300)

  const laid = $derived.by(() => {
    if (!cur) return { nodes: [] as VNode[], edges: [] as { id: string; x1: number; y1: number; x2: number; y2: number }[] }
    const tree = cur
    const out: { path: string; tree: T; x: number; depth: number }[] = []
    function go(t: T, path: string, depth: number): { rootX: number; width: number } {
      const kids = childrenOf(t)
      if (kids.length === 0) {
        out.push({ path, tree: t, x: 0, depth })
        return { rootX: 0, width: 1 }
      }
      let cum = 0
      let first: number | null = null
      let last = 0
      for (let i = 0; i < kids.length; i++) {
        const before = out.length
        const sub = go(kids[i], path + i, depth + 1)
        for (let j = before; j < out.length; j++) out[j].x += cum
        const r = sub.rootX + cum
        if (first === null) first = r
        last = r
        cum += sub.width
      }
      const myX = ((first ?? 0) + last) / 2
      out.push({ path, tree: t, x: myX, depth })
      return { rootX: myX, width: Math.max(cum, 1) }
    }
    go(tree, 'r', 0)
    const maxX = Math.max(...out.map((n) => n.x), 1)
    const maxD = Math.max(...out.map((n) => n.depth), 1)
    const width = Math.max(340, Math.min(860, maxX * XS + 80))
    const height = Math.max(180, Math.min(460, maxD * YS + 80))
    W = width
    H = height
    const sx = (width - 60) / Math.max(maxX, 1)
    const sy = (height - 64) / Math.max(maxD, 1)
    const hotSite = nextApplyToFire(tree)
    const hotSet = new Set<T>()
    if (hotSite) hotSet.add(hotSite)
    const px = (n: { x: number }) => 30 + n.x * Math.min(XS, sx)
    const py = (n: { depth: number }) => 30 + n.depth * Math.min(YS, sy)
    const byPath = new Map(out.map((n) => [n.path, n]))
    const nodes: VNode[] = out.map((n) => ({
      path: n.path,
      tree: n.tree,
      tag: n.tree.tag,
      x: px(n),
      y: py(n),
      hot: hotSet.has(n.tree),
      label: nameOf(n.tree)
    }))
    const edges: { id: string; x1: number; y1: number; x2: number; y2: number }[] = []
    for (const n of out) {
      const kids = childrenOf(n.tree)
      for (let i = 0; i < kids.length; i++) {
        const c = byPath.get(n.path + i)
        if (c) edges.push({ id: n.path + '>' + i, x1: px(n), y1: py(n), x2: px(c), y2: py(c) })
      }
    }
    return { nodes, edges }
  })
</script>

<div class="lab">
  <div class="lab-head">
    <span class="lab-title">Tree lab</span>
    <span class="lab-sub">the five rules, one step at a time — it speaks for the trees</span>
  </div>
  <div class="controls">
    <input
      type="text"
      bind:value={input}
      onkeydown={(e) => e.key === 'Enter' && load()}
      spellcheck="false"
      aria-label="tree-calculus expression"
    />
    <button class="btn sm primary" onclick={step} disabled={!cur && !!err}>Step</button>
    <button class="btn sm" onclick={toggleAuto}>{autorun ? 'Pause' : 'Run'}</button>
    <button class="btn sm" onclick={load}>Reset</button>
  </div>
  <div class="presets">
    {#each presets as p}
      <button class="preset" class:active={input === p.expr} onclick={() => usePreset(p)}>{p.expr}</button>
    {/each}
  </div>
  {#if activeTip}<p class="tip-line">{activeTip}</p>{/if}

  {#if err}
    <p class="err">{err}</p>
  {:else if cur}
    <svg viewBox="0 0 {W} {H}" style="aspect-ratio: {W} / {H}">
      <defs>
        <linearGradient id="labg" x1="0" y1="0" x2="1" y2="1">
          <stop offset="0" stop-color="var(--g1)" />
          <stop offset="0.55" stop-color="var(--g2)" />
          <stop offset="1" stop-color="var(--g4)" />
        </linearGradient>
      </defs>
      {#each laid.edges as e (e.id)}
        <line x1={e.x1} y1={e.y1} x2={e.x2} y2={e.y2} />
      {/each}
      {#each laid.nodes as n (n.path)}
        <g class="node {n.tag}" class:hot={n.hot} style="transform: translate({n.x}px, {n.y}px)">
          {#if n.tag === 'apply'}
            <circle r="10" class="ring" />
            <text class="at" y="1">@</text>
          {:else}
            <circle r={n.tag === 'leaf' ? 5 : 6.5} />
          {/if}
          {#if n.label}
            <text class="badge" y="-13">{n.label}</text>
          {/if}
        </g>
      {/each}
    </svg>
    <div class="readout">
      <div class="rule-line">
        {#if ruleMsg}<span class="rule">{ruleMsg}</span>{:else}<span class="rule dim">press Step to fire the next reduction</span>{/if}
        <span class="count">{stepCount} step{stepCount === 1 ? '' : 's'}</span>
      </div>
      <div class="forms">
        <span class="form-label">named</span>
        <code>{pretty(cur)}</code>
      </div>
      <div class="forms">
        <span class="form-label">raw</span>
        <code>{pretty(cur, { names: false })}</code>
      </div>
    </div>
  {/if}
</div>

<style>
  .lab {
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-panel);
    padding: 1rem 1.1rem;
    margin: 1.4rem 0;
  }
  .lab-head { display: flex; align-items: baseline; gap: 0.7em; margin-bottom: 0.7rem; flex-wrap: wrap; }
  .lab-title {
    font-family: var(--font-display);
    font-size: 1.1rem;
    font-weight: 620;
    background: var(--grad-brand);
    -webkit-background-clip: text;
    background-clip: text;
    color: transparent;
  }
  .lab-sub { color: var(--fg-faint); font-size: 0.8rem; font-style: italic; }
  .controls { display: flex; gap: 0.5rem; flex-wrap: wrap; }
  input {
    flex: 1;
    min-width: 180px;
    background: var(--bg-code);
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    padding: 0.45em 0.7em;
    outline: none;
  }
  input:focus { border-color: var(--accent); }
  .btn.sm { padding: 0.32em 0.85em; font-size: 0.82rem; }
  .presets { display: flex; gap: 0.4rem; flex-wrap: wrap; margin-top: 0.6rem; }
  .preset {
    background: none;
    border: 1px solid var(--border);
    border-radius: 999px;
    color: var(--fg-muted);
    font-family: var(--font-mono);
    font-size: 0.72rem;
    padding: 0.25em 0.7em;
    cursor: pointer;
  }
  .preset:hover { color: var(--fg); border-color: var(--border-strong); }
  .preset.active { color: var(--g2); border-color: color-mix(in oklab, var(--g2) 50%, transparent); }
  .tip-line { color: var(--fg-faint); font-size: 0.8rem; margin: 0.5rem 0 0; font-style: italic; }
  svg { width: 100%; margin-top: 0.6rem; }
  line { stroke: color-mix(in oklab, var(--g3) 30%, transparent); stroke-width: 1.3; }
  .node { transition: transform 0.4s cubic-bezier(0.4, 0.1, 0.2, 1); }
  .node circle { fill: url(#labg); }
  .node.apply .ring {
    fill: none;
    stroke: var(--warn);
    stroke-width: 1.5;
    stroke-dasharray: 3 3;
  }
  .node .at {
    fill: var(--warn);
    font-size: 11px;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }
  .node.hot .ring { animation: labpulse 0.9s ease-in-out infinite; }
  .node.hot circle { filter: drop-shadow(0 0 6px color-mix(in oklab, var(--g2) 75%, transparent)); }
  @keyframes labpulse { 50% { opacity: 0.35; } }
  .badge {
    fill: var(--g1);
    font-size: 10px;
    text-anchor: middle;
    font-family: var(--font-mono);
  }
  .err { color: var(--err); font-family: var(--font-mono); font-size: 0.82rem; }
  .readout { margin-top: 0.5rem; font-size: 0.8rem; }
  .rule-line { display: flex; justify-content: space-between; gap: 1rem; margin-bottom: 0.35rem; }
  .rule { color: var(--g2); font-family: var(--font-mono); }
  .rule.dim { color: var(--fg-faint); }
  .count { color: var(--fg-faint); white-space: nowrap; }
  .forms { display: flex; gap: 0.6em; align-items: baseline; overflow-x: auto; padding: 0.1rem 0; }
  .form-label {
    color: var(--fg-faint);
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    flex: none;
    width: 3.2em;
  }
  .forms code { white-space: nowrap; background: none; border: none; padding: 0; }
</style>
