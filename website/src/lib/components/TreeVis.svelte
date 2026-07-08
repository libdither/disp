<script lang="ts">
  // The shared tree-calculus visualizer. One component, two temperaments:
  //   ambient — self-driving scenery for the landing page and visualizer
  //   lab     — the Learn page's instrument, controls out and honest
  // Either way it renders real reductions: pruned subtrees fall to the
  // ground, rest a while, and fade; surviving branches glide to their new
  // positions; ready redexes wear the letter of the rule about to fire.
  import { onMount, onDestroy } from 'svelte'
  import { backOut } from 'svelte/easing'
  import {
    parseTree,
    stepOnce,
    stepParallel,
    nextApplyToFire,
    readyRule,
    childrenOf,
    pretty,
    natValue,
    treeEq,
    nodeCount,
    DEFS,
    type T
  } from '$lib/treecalc/treecalc'

  export interface Preset {
    expr: string
    tip: string
  }

  interface Props {
    variant?: 'ambient' | 'lab'
    exprs?: string[]
    presets?: Preset[]
    stepMs?: number
    height?: number
  }
  let {
    variant = 'ambient',
    exprs = ['not false', 'S K K (t t)', 'shape_code (t t t)', 'S K K (S K K (t t))'],
    presets = [],
    stepMs = 1050,
    height = 360
  }: Props = $props()

  // ---- instrument state ----------------------------------------------------
  // props are read once here on purpose: variant/exprs/presets are static at
  // every usage site, they just pick the instrument's starting posture
  // svelte-ignore state_referenced_locally
  let running = $state(variant === 'ambient')
  // svelte-ignore state_referenced_locally
  let autoCycle = $state(variant === 'ambient') // ambient rolls through exprs on its own
  let parallel = $state(false)
  let styled = $state(true) // nature styling vs plain diagram
  let motion = $state(true) // transitions + debris vs instant
  // svelte-ignore state_referenced_locally
  let labels = $state(variant === 'lab') // name badges on recognized subtrees
  // svelte-ignore state_referenced_locally
  let showControls = $state(variant === 'lab')

  // svelte-ignore state_referenced_locally
  let input = $state(exprs[0] ?? 'not false')
  // svelte-ignore state_referenced_locally
  let activeTip = $state(presets.find((p) => p.expr === input)?.tip ?? '')
  let cur = $state<T | null>(null)
  let ruleMsg = $state('')
  let stepCount = $state(0)
  let err = $state('')

  // ---- layout ---------------------------------------------------------------

  interface VNode {
    path: string
    tree: T
    tag: T['tag']
    x: number
    y: number
    depth: number
    ready: 'L' | 's' | 'K' | 'S' | 'F' | null
    next: boolean
    tint: string
    tilt: number
    label: string | null
  }
  // edges carry coordinates (not a baked path string) so they can be tweened
  interface VEdge {
    id: string
    x1: number
    y1: number
    cx: number
    cy: number
    x2: number
    y2: number
    w: number
    trunk?: boolean
  }
  // a pruned SUBTREE falls as one piece: branches with their leaves attached
  interface DebrisGroup {
    id: number
    ox: number // rotation origin (group centroid)
    oy: number
    dx: number
    dy: number
    rot: number
    nodes: { x: number; y: number; tag: T['tag']; tint: string; tilt: number }[]
    edges: { d: string; w: number }[]
  }

  let nodes = $state<VNode[]>([])
  let edges = $state<VEdge[]>([])
  let debris = $state<DebrisGroup[]>([])
  let debrisId = 0
  const W = 560
  const XSTEP = 46
  const YSTEP = 50
  const GROUND = 46

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']
  const hash = (s: string) => {
    let h = 0
    for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0
    return Math.abs(h)
  }

  function nameOf(t: T): string | null {
    if (!labels || t.tag === 'apply') return null
    const n = natValue(t)
    if (n !== null && n >= 1) return String(n)
    if (nodeCount(t) < 3) return null
    for (const [name, d] of Object.entries(DEFS)) if (treeEq(t, d)) return name
    return null
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
    const sx = Math.min(XSTEP * 2, (W - 90) / Math.max(maxX, 1))
    const sy = Math.min(YSTEP * 1.7, (height - GROUND - 70) / Math.max(maxD, 1))
    const ox = (W - maxX * sx) / 2
    const px = (n: Lay) => ox + n.x * sx
    const py = (n: Lay) => height - GROUND - 14 - n.depth * sy
    const nextSite = parallel ? null : nextApplyToFire(tree)
    const vnodes: VNode[] = out.map((n) => ({
      path: n.path,
      tree: n.tree,
      tag: n.tree.tag,
      x: px(n),
      y: py(n),
      depth: n.depth,
      ready: readyRule(n.tree),
      next: nextSite !== null && n.tree === nextSite,
      tint: LEAF_TINTS[hash(n.path) % LEAF_TINTS.length],
      tilt: (hash(n.path) % 70) - 35,
      label: nameOf(n.tree)
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
        // curved when styled; control on the segment midpoint = straight line
        vedges.push({
          id: n.path + '>' + i,
          x1,
          y1,
          cx: x1 + (x2 - x1) * 0.5,
          cy: styled ? y1 + (y2 - y1) * 0.62 : y1 + (y2 - y1) * 0.5,
          x2,
          y2,
          w: styled ? Math.max(1.6, 5.4 - n.depth * 0.85) : 1.4
        })
      }
    }
    if (styled) {
      // the trunk is an edge too, so it glides with the root
      const root = out[out.length - 1]
      vedges.push({
        id: '__trunk',
        x1: W / 2,
        y1: height - GROUND + 12,
        cx: W / 2,
        cy: height - GROUND,
        x2: px(root),
        y2: py(root),
        w: 5,
        trunk: true
      })
    }
    return { nodes: vnodes, edges: vedges }
  }

  const edgeD = (e: VEdge) => `M ${e.x1} ${e.y1} Q ${e.cx} ${e.cy} ${e.x2} ${e.y2}`

  // ---- edge tweening: branches glide with their nodes ------------------------

  let edgeAnim: number | undefined
  function setEdges(target: VEdge[]) {
    cancelAnimationFrame(edgeAnim!)
    if (!motion || edges.length === 0) {
      edges = target
      return
    }
    const from = new Map(edges.map((e) => [e.id, e]))
    const pairs = target.map((te) => {
      const f = from.get(te.id)
      // a NEW branch grows out of its parent endpoint
      const start: VEdge = f ?? { ...te, x2: te.x1, y2: te.y1, cx: te.x1, cy: te.y1 }
      return { start, te }
    })
    const t0 = performance.now()
    const D = 520
    const frame = (now: number) => {
      const t = Math.min(1, (now - t0) / D)
      const k = 1 - Math.pow(1 - t, 3)
      edges = pairs.map(({ start, te }) => ({
        ...te,
        x1: start.x1 + (te.x1 - start.x1) * k,
        y1: start.y1 + (te.y1 - start.y1) * k,
        cx: start.cx + (te.cx - start.cx) * k,
        cy: start.cy + (te.cy - start.cy) * k,
        x2: start.x2 + (te.x2 - start.x2) * k,
        y2: start.y2 + (te.y2 - start.y2) * k
      }))
      if (t < 1) edgeAnim = requestAnimationFrame(frame)
    }
    edgeAnim = requestAnimationFrame(frame)
  }

  function show(tree: T) {
    const prevNodes = nodes
    const prevEdges = edges
    const l = layout(tree)
    if (motion && styled && prevNodes.length > 0) {
      // Anything whose tree REFERENCE survives into the next state merely
      // moved (rewrites share untouched subtrees), so it glides. Anything
      // whose reference is gone was truly pruned, and a pruned subtree
      // breaks off as one piece: branches with their leaves attached.
      const survivors = new Set(l.nodes.map((n) => n.tree))
      const removedByPath = new Map(prevNodes.filter((n) => !survivors.has(n.tree)).map((n) => [n.path, n]))
      // group roots: removed nodes whose old parent is not itself removed
      const groups: DebrisGroup[] = []
      for (const [path, n] of removedByPath) {
        const parentPath = path.slice(0, -1)
        if (path !== 'r' && removedByPath.has(parentPath)) continue
        const members = [...removedByPath.values()].filter((m) => m.path.startsWith(path))
        const memberPaths = new Set(members.map((m) => m.path))
        const gedges = prevEdges
          .filter((e) => !e.trunk)
          .filter((e) => {
            const child = e.id.replace('>', '')
            return memberPaths.has(child)
          })
          .map((e) => ({ d: edgeD(e), w: e.w }))
        const cx = members.reduce((s, m) => s + m.x, 0) / members.length
        const cy = members.reduce((s, m) => s + m.y, 0) / members.length
        const maxY = Math.max(...members.map((m) => m.y))
        groups.push({
          id: debrisId++,
          ox: cx,
          oy: cy,
          dx: ((hash(path) % 44) - 22) * 1.1,
          dy: height - GROUND + 2 - maxY + (hash(path) % 8),
          rot: (hash(path) % 64) - 32,
          nodes: members.map((m) => ({ x: m.x, y: m.y, tag: m.tag, tint: m.tint, tilt: m.tilt })),
          edges: gedges
        })
      }
      if (groups.length) {
        debris.push(...groups)
        // keep the ground tidy: cap total debris nodes, oldest first
        let total = debris.reduce((s, g) => s + g.nodes.length, 0)
        while (total > 140 && debris.length > 1) {
          total -= debris[0].nodes.length
          debris.shift()
        }
        const ids = groups.map((g) => g.id)
        setTimeout(() => {
          debris = debris.filter((g) => !ids.includes(g.id))
        }, 6400)
      }
    }
    nodes = l.nodes
    setEdges(l.edges)
  }

  // ---- the run loop ----------------------------------------------------------

  let timer: ReturnType<typeof setTimeout> | undefined
  let exprIdx = 0
  let atNormalForm = $state(false)

  function load(src: string, opts: { keepAuto?: boolean } = {}) {
    clearTimeout(timer)
    if (!opts.keepAuto) autoCycle = false
    err = ''
    ruleMsg = ''
    stepCount = 0
    atNormalForm = false
    debris = []
    input = src
    activeTip = presets.find((p) => p.expr === src)?.tip ?? ''
    try {
      cur = parseTree(src)
      show(cur)
      if (running) schedule()
    } catch (e) {
      cur = null
      err = e instanceof Error ? e.message : String(e)
    }
  }

  function fire(): boolean {
    if (!cur) return false
    if (parallel) {
      const s = stepParallel(cur)
      if (!s) {
        ruleMsg = `normal form: ${pretty(cur)}`
        atNormalForm = true
        return false
      }
      cur = s.next
      stepCount++
      ruleMsg =
        s.fired.length === 1
          ? `1 redex fired (${s.fired[0]})`
          : `${s.fired.length} redexes fired in parallel (${s.fired.join(' ')})`
      show(cur)
      return true
    }
    const s = stepOnce(cur)
    if (!s) {
      ruleMsg = `normal form: ${pretty(cur)}`
      atNormalForm = true
      return false
    }
    cur = s.next
    stepCount++
    ruleMsg = s.rule
    show(cur)
    return true
  }

  function schedule() {
    clearTimeout(timer)
    timer = setTimeout(() => {
      if (!running) return
      const went = fire()
      if (went) {
        schedule()
      } else if (autoCycle) {
        timer = setTimeout(nextExpr, stepMs * 2.2)
      } else {
        running = false
      }
    }, stepMs)
  }

  function nextExpr() {
    exprIdx = (exprIdx + 1) % exprs.length
    running = true
    load(exprs[exprIdx], { keepAuto: true })
  }

  const step = () => {
    running = false
    autoCycle = false
    clearTimeout(timer)
    if (!cur) load(input)
    fire()
  }
  const toggleRun = () => {
    autoCycle = false
    running = !running
    if (running) {
      if (!cur || atNormalForm) load(input)
      schedule()
    } else clearTimeout(timer)
  }
  const reset = () => {
    running = variant === 'ambient'
    load(input, { keepAuto: variant === 'ambient' })
  }

  onMount(() => {
    load(input, { keepAuto: autoCycle })
  })
  onDestroy(() => clearTimeout(timer))
</script>

<div class="vis" class:plain={!styled} class:lab={variant === 'lab'}>
  {#if showControls}
    <div class="controls">
      <input
        type="text"
        bind:value={input}
        onkeydown={(e) => e.key === 'Enter' && load(input)}
        spellcheck="false"
        aria-label="tree-calculus expression"
      />
      <button class="cbtn primary" onclick={step}>Step</button>
      <button class="cbtn" onclick={toggleRun}>{running ? 'Pause' : 'Run'}</button>
      <button class="cbtn" onclick={reset}>Reset</button>
    </div>
    {#if presets.length}
      <div class="presets">
        {#each presets as p}
          <button class="preset" class:active={input === p.expr} onclick={() => load(p.expr)}>{p.expr}</button>
        {/each}
      </div>
      {#if activeTip}<p class="tip-line">{activeTip}</p>{/if}
    {/if}
    <div class="toggles">
      <button class="tog" class:on={parallel} onclick={() => { parallel = !parallel; if (cur) show(cur) }}
        title="fire every ready redex per step (confluence says the answer agrees)">parallel</button>
      <button class="tog" class:on={styled} onclick={() => { styled = !styled; if (cur) show(cur) }}
        title="nature styling vs plain diagram">nature</button>
      <button class="tog" class:on={motion} onclick={() => (motion = !motion)}
        title="glide + falling leaves vs instant updates">motion</button>
      <button class="tog" class:on={labels} onclick={() => { labels = !labels; if (cur) show(cur) }}
        title="name badges on recognized subtrees">labels</button>
    </div>
  {/if}

  {#if err}
    <p class="err">{err}</p>
  {:else}
    <svg viewBox="0 0 {W} {height}" preserveAspectRatio="xMidYMid meet" aria-hidden="true">
      {#if styled}
        <ellipse cx={W / 2} cy={height - 22} rx={W * 0.36} ry="16" class="hill back" />
        <ellipse cx={W / 2 - 30} cy={height - 16} rx={W * 0.3} ry="13" class="hill front" />
        <g class="grass">
          <path d="M {W / 2 - 118} {height - 24} q 2 -12 7 -16 M {W / 2 - 112} {height - 24} q 3 -8 9 -10" />
          <path d="M {W / 2 + 104} {height - 20} q 2 -12 7 -16 M {W / 2 + 111} {height - 20} q 3 -8 9 -10" />
          <path d="M {W / 2 - 40} {height - 12} q 2 -10 6 -13 M {W / 2 - 34} {height - 12} q 3 -7 8 -9" />
        </g>
        <circle cx={W / 2 - 128} cy={height - 30} r="2.3" class="daisy" />
        <circle cx={W / 2 + 122} cy={height - 26} r="2.3" class="daisy alt" />
      {/if}

      <!-- pruned subtrees: they break off whole, land, rest, and let go -->
      {#each debris as g (g.id)}
        <g
          class="dgroup"
          style="--dy: {g.dy}px; --dx: {g.dx}px; --rot: {g.rot}deg; transform-origin: {g.ox}px {g.oy}px"
        >
          {#each g.edges as e}
            <path class="branch fallen" d={e.d} style="stroke-width:{e.w}" />
          {/each}
          {#each g.nodes as n}
            <g style="transform: translate({n.x}px, {n.y}px)">
              {#if n.tag === 'leaf'}
                <path
                  class="leafshape"
                  d="M0,-8 C5.2,-5 5.2,2.5 0,8 C-5.2,2.5 -5.2,-5 0,-8 Z"
                  style="fill:{n.tint}; transform: rotate({n.tilt}deg)"
                />
              {:else if n.tag === 'apply'}
                <circle r="3" class="bud-core faded" />
              {:else}
                <circle r="3.4" class="knot" />
              {/if}
            </g>
          {/each}
        </g>
      {/each}

      <g class="tree" class:swaying={styled && motion}>
        {#each edges as e (e.id)}
          <path d={edgeD(e)} class="branch" class:trunk={e.trunk} style="stroke-width:{e.w}" />
        {/each}
        {#each nodes as n (n.path)}
          <g class="node" class:gliding={motion} style="transform: translate({n.x}px, {n.y}px)">
            <g class="inner" class:growing={motion && styled} style="--gd: {n.depth * 55}ms">
              {#if n.tag === 'apply'}
                <circle r={styled ? 8.5 : 8} class="bud-ring" />
                {#if !styled}<text class="at">@</text>{:else}<circle r="3" class="bud-core" />{/if}
              {:else if n.tag === 'leaf'}
                {#if styled}
                  <path
                    class="leafshape"
                    d="M0,-9 C5.8,-5.5 5.8,3 0,9 C-5.8,3 -5.8,-5.5 0,-9 Z"
                    style="fill:{n.tint}; transform: rotate({n.tilt}deg)"
                  />
                  <path class="vein" d="M0,-6 L0,6" style="transform: rotate({n.tilt}deg)" />
                {:else}
                  <circle r="4.5" class="dot leafdot" />
                {/if}
              {:else}
                <circle r={styled ? 4 : 5.5} class="knot" />
              {/if}
              {#if n.ready && (parallel || n.next)}
                <g class="firemark" class:next={n.next || parallel}>
                  <circle r="7.5" class="fire-ring" />
                  <text class="fire-letter" y="0.5">{n.ready}</text>
                </g>
              {/if}
              {#if n.next && styled}
                <g class="blossom">
                  {#each [36, 108, 180, 252, 324] as a}
                    <circle r="2.6" cx={9.5 * Math.cos((a * Math.PI) / 180)} cy={9.5 * Math.sin((a * Math.PI) / 180)} />
                  {/each}
                </g>
              {/if}
              {#if n.label}
                <text class="badge" y="-14">{n.label}</text>
              {/if}
            </g>
          </g>
        {/each}
      </g>
    </svg>

    <div class="readout">
      <div class="rline">
        <span class="expr">{input}</span>
        <span class="rule" class:nf={atNormalForm}>{ruleMsg || (variant === 'lab' ? 'press Step to fire the next reduction' : '')}</span>
        <span class="count">{stepCount} step{stepCount === 1 ? '' : 's'}{parallel ? ' · parallel' : ''}</span>
      </div>
      {#if variant === 'lab' && cur}
        <div class="forms"><span class="flabel">named</span><code>{pretty(cur)}</code></div>
        <div class="forms"><span class="flabel">raw</span><code>{pretty(cur, { names: false })}</code></div>
      {/if}
    </div>
  {/if}

  {#if variant === 'ambient'}
    <button
      class="instrument"
      onclick={() => {
        showControls = !showControls
        if (showControls) {
          autoCycle = false
        }
      }}
      title={showControls ? 'put the instruments away' : 'instrument this tree'}
      aria-expanded={showControls}
    >
      {showControls ? '×' : '⚘'}
    </button>
  {/if}
</div>

<style>
  .vis {
    position: relative;
    width: 100%;
  }
  .vis.lab {
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-panel);
    padding: 1rem 1.1rem;
    margin: 1.4rem 0;
  }
  svg {
    width: 100%;
    overflow: visible;
    display: block;
  }

  /* ---- controls ---- */
  .controls {
    display: flex;
    gap: 0.5rem;
    flex-wrap: wrap;
    margin-bottom: 0.5rem;
  }
  input {
    flex: 1;
    min-width: 170px;
    background: var(--bg-code);
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    padding: 0.42em 0.7em;
    outline: none;
  }
  input:focus { border-color: var(--accent); }
  .cbtn {
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg);
    border-radius: 8px;
    font: inherit;
    font-size: 0.82rem;
    font-weight: 550;
    padding: 0.32em 0.9em;
    cursor: pointer;
  }
  .cbtn:hover { border-color: var(--accent); }
  .cbtn.primary {
    background: linear-gradient(118deg, #4caf6d, #2f9e6e 70%);
    color: #f7fcf5;
    border: none;
  }
  .presets { display: flex; gap: 0.4rem; flex-wrap: wrap; margin-top: 0.35rem; }
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
  .tip-line { color: var(--fg-faint); font-size: 0.8rem; margin: 0.45rem 0 0; font-style: italic; }
  .toggles { display: flex; gap: 0.4rem; flex-wrap: wrap; margin-top: 0.55rem; }
  .tog {
    background: none;
    border: 1px dashed var(--border-strong);
    border-radius: 999px;
    color: var(--fg-faint);
    font-family: var(--font-mono);
    font-size: 0.7rem;
    padding: 0.22em 0.75em;
    cursor: pointer;
    transition: all 0.15s ease;
  }
  .tog.on {
    border-style: solid;
    border-color: var(--g2);
    color: var(--g2);
    background: color-mix(in oklab, var(--g1) 10%, transparent);
  }

  /* ---- scenery ---- */
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

  .tree { transform-origin: 50% 100%; }
  .tree.swaying { animation: sway 9s ease-in-out infinite alternate; }
  @keyframes sway {
    from { transform: rotate(-0.7deg); }
    to { transform: rotate(0.7deg); }
  }
  .trunk,
  .branch {
    stroke: var(--bark);
    fill: none;
    stroke-linecap: round;
    opacity: 0.8;
  }
  .plain .trunk, .plain .branch { stroke: var(--fg-faint); opacity: 0.6; }
  .trunk { stroke-width: 5; }

  /* ---- nodes ---- */
  .node.gliding { transition: transform 0.55s cubic-bezier(0.34, 1.2, 0.4, 1); }
  .inner.growing { animation: grow 0.5s var(--gd) cubic-bezier(0.34, 1.56, 0.64, 1) backwards; }
  @keyframes grow {
    from { transform: scale(0); opacity: 0; }
  }
  .leafshape { stroke: color-mix(in oklab, var(--fg) 18%, transparent); stroke-width: 0.5; }
  .vein { stroke: rgba(255, 255, 255, 0.55); stroke-width: 0.8; fill: none; }
  .knot { fill: var(--bark); }
  .plain .knot { fill: var(--fg-muted); }
  .dot.leafdot { fill: var(--g2); }
  .bud-ring { fill: none; stroke: var(--g4); stroke-width: 1.5; stroke-dasharray: 3 3; }
  .plain .bud-ring { stroke: var(--warn); }
  .bud-core { fill: var(--g4); }
  .at {
    fill: var(--warn);
    font-size: 10px;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }

  /* rule letters at redexes about to fire */
  .fire-ring {
    fill: color-mix(in oklab, var(--g4) 22%, var(--bg-elev));
    stroke: var(--g4);
    stroke-width: 1.2;
  }
  .fire-letter {
    font-family: var(--font-mono);
    font-size: 8.5px;
    font-weight: 700;
    fill: #8a6414;
    text-anchor: middle;
    dominant-baseline: middle;
  }
  .firemark.next { animation: firepulse 1s ease-in-out infinite; transform-origin: 0 0; }
  @keyframes firepulse {
    50% { transform: scale(1.25); }
  }
  .blossom circle { fill: var(--blossom); opacity: 0.85; stroke: #fff; stroke-width: 0.5; }

  .badge {
    fill: var(--g2);
    font-size: 10px;
    text-anchor: middle;
    font-family: var(--font-mono);
  }

  /* ---- pruned subtrees: fall (1s), rest (~2.4s), let go (3.4s) ---- */
  .dgroup {
    animation:
      dgfall 1.05s cubic-bezier(0.45, 0.05, 0.75, 0.6) forwards,
      dgfade 3.4s 2.5s linear forwards;
  }
  @keyframes dgfall {
    to {
      transform: translate(var(--dx), var(--dy)) rotate(var(--rot));
    }
  }
  @keyframes dgfade {
    to {
      opacity: 0;
    }
  }
  .branch.fallen {
    opacity: 0.6;
  }
  .bud-core.faded {
    opacity: 0.5;
  }

  /* ---- readout ---- */
  .readout { margin-top: 0.3rem; font-size: 0.8rem; }
  .rline {
    display: flex;
    align-items: baseline;
    gap: 0.8em;
    justify-content: center;
    font-family: var(--font-mono);
    color: var(--fg-muted);
    flex-wrap: wrap;
  }
  .lab .rline { justify-content: flex-start; }
  .expr { color: var(--fg); }
  .rule {
    font-size: 0.74rem;
    color: var(--fg-faint);
    font-style: italic;
    max-width: 55%;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .rule.nf { color: var(--g2); font-style: normal; }
  .count { color: var(--fg-faint); font-size: 0.72rem; white-space: nowrap; }
  .forms { display: flex; gap: 0.6em; align-items: baseline; overflow-x: auto; padding: 0.12rem 0; }
  .flabel {
    color: var(--fg-faint);
    font-size: 0.68rem;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    flex: none;
    min-width: 3.6em;
  }
  .forms code { white-space: nowrap; background: none; border: none; padding: 0; }
  .err { color: var(--err); font-family: var(--font-mono); font-size: 0.82rem; }

  /* ---- the ambient instrument button ---- */
  .instrument {
    position: absolute;
    right: 0.4rem;
    bottom: 2.2rem;
    width: 30px;
    height: 30px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--g2);
    font-size: 1rem;
    cursor: pointer;
    opacity: 0.45;
    transition: opacity 0.2s ease, transform 0.2s ease;
  }
  .vis:hover .instrument { opacity: 1; }
  .instrument:hover { transform: rotate(20deg); }
</style>
