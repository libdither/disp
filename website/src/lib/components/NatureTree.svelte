<script lang="ts">
  // The grove's living diagram: real tree-calculus programs reducing, drawn
  // as an actual tree — rooted at the bottom, bark branches, leaf-shaped
  // leaves. When a rule fires, pruned subtrees FALL (drift down, rotate,
  // fade) and new growth scales in; shared subtrees glide to their new spot.
  import { onMount } from 'svelte'
  import { backOut, quadIn } from 'svelte/easing'
  import { parseTree, stepOnce, nextApplyToFire, childrenOf, pretty, type T } from '$lib/treecalc/treecalc'

  interface Props {
    exprs?: string[]
    stepMs?: number
    height?: number
  }
  let {
    exprs = ['not false', 'S K K (t t)', 'shape_code (t t t)', 'K t (t t)'],
    stepMs = 1050,
    height = 360
  }: Props = $props()

  interface VNode {
    path: string
    tag: T['tag']
    x: number
    y: number
    depth: number
    hot: boolean
    tint: string
    tilt: number
  }
  interface VEdge {
    id: string
    d: string
    w: number
  }

  let nodes = $state<VNode[]>([])
  let edges = $state<VEdge[]>([])
  let ruleChip = $state('')
  let exprLabel = $state('')
  let resultLabel = $state('')
  const W = 560

  const XSTEP = 46
  const YSTEP = 50
  const GROUND = 46 // px reserved below the root for the hill

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']

  const hash = (s: string) => {
    let h = 0
    for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0
    return Math.abs(h)
  }

  function layout(tree: T): { nodes: VNode[]; edges: VEdge[] } {
    type Lay = { path: string; tree: T; x: number; depth: number }
    const out: Lay[] = []
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
    // small trees grow TALLER and WIDER (up to ~2× the base step) so a
    // three-node sapling still fills its clearing
    const sx = Math.min(XSTEP * 2, (W - 90) / Math.max(maxX, 1))
    const sy = Math.min(YSTEP * 1.7, (height - GROUND - 70) / Math.max(maxD, 1))
    const ox = (W - maxX * sx) / 2
    // nature flip: the ROOT sits at the bottom; depth climbs toward the sky
    const px = (n: Lay) => ox + n.x * sx
    const py = (n: Lay) => height - GROUND - 14 - n.depth * sy
    const hotSite = nextApplyToFire(tree)
    const vnodes: VNode[] = out.map((n) => ({
      path: n.path,
      tag: n.tree.tag,
      x: px(n),
      y: py(n),
      depth: n.depth,
      hot: hotSite !== null && n.tree === hotSite,
      tint: LEAF_TINTS[hash(n.path) % LEAF_TINTS.length],
      tilt: (hash(n.path) % 70) - 35
    }))
    const byPath = new Map(out.map((n) => [n.path, n]))
    const vedges: VEdge[] = []
    for (const n of out) {
      const kids = childrenOf(n.tree)
      for (let i = 0; i < kids.length; i++) {
        const c = byPath.get(n.path + i)
        if (!c) continue
        const x1 = px(n)
        const y1 = py(n)
        const x2 = px(c)
        const y2 = py(c)
        // a gentle branch curve: bow outward, sag toward the parent
        const mx = x1 + (x2 - x1) * 0.5
        const my = y1 + (y2 - y1) * 0.62
        vedges.push({
          id: n.path + '>' + i,
          d: `M ${x1} ${y1} Q ${mx} ${my} ${x2} ${y2}`,
          w: Math.max(1.6, 5.4 - n.depth * 0.85)
        })
      }
    }
    return { nodes: vnodes, edges: vedges }
  }

  // new growth unfurls; pruned leaves fall
  const grow = (_n: Element, p: { delay?: number } = {}) => ({
    delay: p.delay ?? 0,
    duration: 480,
    easing: backOut,
    css: (t: number) => `transform: scale(${t}); opacity: ${Math.min(1, t * 1.6)}`
  })
  const fall = (_n: Element) => ({
    duration: 620,
    easing: quadIn,
    css: (t: number, u: number) =>
      `transform: translateY(${u * 58}px) translateX(${u * 12}px) rotate(${8 + u * 74}deg); opacity: ${t * t}`
  })
  const fadeEdge = (_n: Element) => ({
    duration: 480,
    css: (t: number) => `opacity: ${t * 0.8}`
  })

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
      timer = setTimeout(tick, stepMs * 1.4)
    }

    const tick = () => {
      if (!alive) return
      const s = stepOnce(cur)
      if (!s) {
        resultLabel = pretty(cur)
        ruleChip = 'normal form'
        timer = setTimeout(startExpr, stepMs * 2.4)
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

<div class="wrap" style="height:{height + 34}px">
  <svg viewBox="0 0 {W} {height}" preserveAspectRatio="xMidYMid meet" aria-hidden="true">
    <!-- ground -->
    <ellipse cx={W / 2} cy={height - 22} rx={W * 0.36} ry="16" class="hill back" />
    <ellipse cx={W / 2 - 30} cy={height - 16} rx={W * 0.3} ry="13" class="hill front" />
    <g class="grass">
      <path d="M {W / 2 - 118} {height - 24} q 2 -12 7 -16 M {W / 2 - 112} {height - 24} q 3 -8 9 -10" />
      <path d="M {W / 2 + 104} {height - 20} q 2 -12 7 -16 M {W / 2 + 111} {height - 20} q 3 -8 9 -10" />
      <path d="M {W / 2 - 40} {height - 12} q 2 -10 6 -13 M {W / 2 - 34} {height - 12} q 3 -7 8 -9" />
    </g>
    <circle cx={W / 2 - 128} cy={height - 30} r="2.3" class="daisy" />
    <circle cx={W / 2 + 122} cy={height - 26} r="2.3" class="daisy alt" />

    <g class="tree">
      <!-- trunk stub under the root -->
      {#if nodes.length}
        <path
          d="M {W / 2} {height - GROUND + 12} Q {W / 2} {height - GROUND} {nodes.find((n) => n.path === 'r')?.x ?? W / 2} {nodes.find((n) => n.path === 'r')?.y ?? 0}"
          class="trunk"
        />
      {/if}
      {#each edges as e (e.id)}
        <path d={e.d} class="branch" style="stroke-width:{e.w}" transition:fadeEdge />
      {/each}
      {#each nodes as n (n.path)}
        <g class="node" style="transform: translate({n.x}px, {n.y}px)">
          <g in:grow={{ delay: n.depth * 60 }} out:fall>
            {#if n.tag === 'apply'}
              <!-- a bud: growth waiting to happen -->
              <circle r="8.5" class="bud-ring" />
              <circle r="3" class="bud-core" />
            {:else if n.tag === 'leaf'}
              <path
                class="leafshape"
                d="M0,-9 C5.8,-5.5 5.8,3 0,9 C-5.8,3 -5.8,-5.5 0,-9 Z"
                style="fill:{n.tint}; transform: rotate({n.tilt}deg)"
              />
              <path class="vein" d="M0,-6 L0,6" style="transform: rotate({n.tilt}deg)" />
            {:else}
              <circle r="4" class="knot" />
            {/if}
            {#if n.hot}
              <g class="blossom">
                {#each [0, 72, 144, 216, 288] as a}
                  <circle r="3.2" cx={7 * Math.cos((a * Math.PI) / 180)} cy={7 * Math.sin((a * Math.PI) / 180)} />
                {/each}
                <circle r="2.6" class="pollen" />
              </g>
            {/if}
          </g>
        </g>
      {/each}
    </g>
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
  .hill.back { fill: color-mix(in oklab, var(--g1) 16%, var(--bg-elev)); }
  .hill.front { fill: color-mix(in oklab, var(--g2) 13%, var(--bg-elev)); }
  .grass path {
    stroke: color-mix(in oklab, var(--g2) 55%, transparent);
    stroke-width: 1.5;
    fill: none;
    stroke-linecap: round;
  }
  .daisy { fill: var(--blossom); }
  .daisy.alt { fill: var(--g4); opacity: 0.8; }

  .tree {
    transform-origin: 50% 100%;
    animation: sway 9s ease-in-out infinite alternate;
  }
  @keyframes sway {
    from { transform: rotate(-0.7deg); }
    to { transform: rotate(0.7deg); }
  }

  .trunk, .branch {
    stroke: var(--bark);
    fill: none;
    stroke-linecap: round;
    opacity: 0.8;
  }
  .trunk { stroke-width: 5; }

  .node {
    transition: transform 0.6s cubic-bezier(0.4, 0.1, 0.2, 1);
  }
  .leafshape {
    stroke: color-mix(in oklab, var(--fg) 18%, transparent);
    stroke-width: 0.5;
  }
  .vein {
    stroke: rgba(255, 255, 255, 0.55);
    stroke-width: 0.8;
    fill: none;
  }
  .knot { fill: var(--bark); }
  .bud-ring {
    fill: none;
    stroke: var(--g4);
    stroke-width: 1.5;
    stroke-dasharray: 3 3;
  }
  .bud-core { fill: var(--g4); }

  .blossom circle { fill: var(--blossom); opacity: 0.9; stroke: #fff; stroke-width: 0.5; }
  .blossom .pollen { fill: var(--g4); }
  .blossom {
    animation: bloom 1.15s ease-in-out infinite;
    transform-origin: 0 0;
  }
  @keyframes bloom {
    50% { transform: scale(1.22); opacity: 0.75; }
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
  .expr { color: var(--fg); }
  .arrow { color: var(--g2); }
  .result { color: var(--g2); font-weight: 600; }
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
