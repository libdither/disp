<script lang="ts">
  // The hero's living diagram: real tree-calculus programs reducing, one rule
  // at a time, on loop. Node positions morph via CSS transforms keyed by tree
  // path, so shared subtrees glide instead of teleporting.
  import { onMount } from 'svelte'
  import { parseTree, stepOnce, nextApplyToFire, childrenOf, pretty, type T } from '$lib/treecalc/treecalc'

  interface Props {
    exprs?: string[]
    stepMs?: number
    height?: number
  }
  let {
    exprs = ['not false', 'S K K (t t)', 'shape_code (t t t)', 'K t (t t)'],
    stepMs = 950,
    height = 340
  }: Props = $props()

  interface VNode {
    path: string
    tag: T['tag']
    x: number
    y: number
    hot: boolean
  }
  interface VEdge {
    id: string
    x1: number
    y1: number
    x2: number
    y2: number
  }

  let nodes = $state<VNode[]>([])
  let edges = $state<VEdge[]>([])
  let ruleChip = $state('')
  let exprLabel = $state('')
  let resultLabel = $state('')
  let W = 560

  const XSTEP = 46
  const YSTEP = 52

  function layout(tree: T): { nodes: VNode[]; edges: VEdge[] } {
    // tidy layout with path identity
    type Lay = { path: string; tree: T; x: number; depth: number }
    const out: Lay[] = []
    const hotSite = nextApplyToFire(tree)
    const hotSet = new Set<T>()
    if (hotSite) {
      hotSet.add(hotSite)
      hotSet.add(hotSite.f)
      hotSet.add(hotSite.x)
    }
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
        // shift the child's freshly appended slice by the cumulative width
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
    const sx = Math.min(XSTEP, (W - 60) / Math.max(maxX, 1))
    const sy = Math.min(YSTEP, (height - 70) / Math.max(maxD, 1))
    const ox = (W - maxX * sx) / 2
    const vnodes: VNode[] = out.map((n) => ({
      path: n.path,
      tag: n.tree.tag,
      x: ox + n.x * sx,
      y: 34 + n.depth * sy,
      hot: hotSet.has(n.tree)
    }))
    const byTree = new Map<string, Lay>()
    for (const n of out) byTree.set(n.path, n)
    const vedges: VEdge[] = []
    for (const n of out) {
      const kids = childrenOf(n.tree)
      for (let i = 0; i < kids.length; i++) {
        const child = byTree.get(n.path + i)
        if (!child) continue
        vedges.push({
          id: n.path + '>' + i,
          x1: ox + n.x * sx,
          y1: 34 + n.depth * sy,
          x2: ox + child.x * sx,
          y2: 34 + child.depth * sy
        })
      }
    }
    return { nodes: vnodes, edges: vedges }
  }

  onMount(() => {
    let cur: T
    let exprIdx = 0
    let timer: ReturnType<typeof setTimeout>
    let alive = true

    const show = (t: T) => {
      const l = layout(t)
      nodes = l.nodes
      edges = l.edges
    }

    const startExpr = () => {
      const src = exprs[exprIdx % exprs.length]
      exprIdx++
      cur = parseTree(src)
      exprLabel = src
      resultLabel = ''
      ruleChip = ''
      show(cur)
      timer = setTimeout(tick, stepMs)
    }

    const tick = () => {
      if (!alive) return
      const s = stepOnce(cur)
      if (!s) {
        resultLabel = pretty(cur)
        ruleChip = 'normal form'
        timer = setTimeout(startExpr, stepMs * 2.2)
        return
      }
      cur = s.next
      ruleChip = s.rule
      show(cur)
      timer = setTimeout(tick, stepMs)
    }

    startExpr()
    return () => {
      alive = false
      clearTimeout(timer)
    }
  })
</script>

<div class="wrap" style="height:{height}px">
  <svg viewBox="0 0 {W} {height}" preserveAspectRatio="xMidYMid meet" aria-hidden="true">
    <defs>
      <linearGradient id="tg" x1="0" y1="0" x2="1" y2="1">
        <stop offset="0" stop-color="var(--g1)" />
        <stop offset="0.5" stop-color="var(--g2)" />
        <stop offset="1" stop-color="var(--g4)" />
      </linearGradient>
    </defs>
    {#each edges as e (e.id)}
      <line x1={e.x1} y1={e.y1} x2={e.x2} y2={e.y2} />
    {/each}
    {#each nodes as n (n.path)}
      <g class="node {n.tag}" class:hot={n.hot} style="transform: translate({n.x}px, {n.y}px)">
        {#if n.tag === 'apply'}
          <circle r="11" class="ring" />
          <text y="1">·</text>
        {:else}
          <circle r={n.tag === 'leaf' ? 5.5 : 7} />
        {/if}
      </g>
    {/each}
  </svg>
  <div class="caption">
    <span class="expr">{exprLabel}</span>
    {#if resultLabel}
      <span class="arrow">⟶*</span><span class="result">{resultLabel}</span>
    {/if}
    <span class="rule">{ruleChip}</span>
  </div>
</div>

<style>
  .wrap {
    position: relative;
    width: 100%;
  }
  svg {
    width: 100%;
    height: calc(100% - 34px);
    overflow: visible;
  }
  line {
    stroke: color-mix(in oklab, var(--g3) 34%, transparent);
    stroke-width: 1.4;
  }
  .node {
    transition: transform 0.55s cubic-bezier(0.4, 0.1, 0.2, 1);
  }
  .node circle {
    fill: url(#tg);
    opacity: 0.92;
  }
  .node.apply .ring {
    fill: none;
    stroke: var(--warn);
    stroke-width: 1.6;
    stroke-dasharray: 3 3;
    opacity: 0.85;
  }
  .node.apply text {
    fill: var(--warn);
    font-size: 15px;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }
  .node.hot circle {
    filter: drop-shadow(0 0 7px color-mix(in oklab, var(--g2) 80%, transparent));
  }
  .node.hot .ring {
    animation: spin-pulse 1s ease-in-out infinite;
  }
  @keyframes spin-pulse {
    50% {
      opacity: 0.3;
    }
  }
  .caption {
    display: flex;
    align-items: baseline;
    gap: 0.6em;
    justify-content: center;
    font-family: var(--font-mono);
    font-size: 0.82rem;
    color: var(--fg-muted);
    height: 34px;
  }
  .expr {
    color: var(--fg);
  }
  .arrow {
    color: var(--g2);
  }
  .result {
    color: var(--g1);
    font-weight: 600;
  }
  .rule {
    font-size: 0.72rem;
    color: var(--fg-faint);
    font-style: italic;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 45%;
  }
</style>
