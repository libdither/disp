<script lang="ts">
  // The shared tree-calculus visualizer. One component, two temperaments:
  //   ambient — self-driving scenery for the landing page and visualizer
  //   lab     — the Learn page's instrument, controls out and honest
  // Either way it renders real reductions: pruned subtrees fall to the
  // ground, rest a while, and fade; surviving branches glide to their new
  // positions; ready redexes wear the letter of the rule about to fire.
  import { onMount, onDestroy } from 'svelte'
  import {
    parseTree,
    stepOnce,
    stepParallel,
    nextApplyToFire,
    readyRule,
    readySites,
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

  type ExprSpec = string | { expr: string; stepMs?: number }

  interface Props {
    variant?: 'ambient' | 'lab'
    exprs?: ExprSpec[]
    presets?: Preset[]
    stepMs?: number
    height?: number
  }
  let {
    variant = 'ambient',
    // the ambient tour: one demo per rule (on named leaves, so the reductions
    // read like they do on paper), then programs that mean something
    exprs = [
      'K x y', // K keeps x, discards y
      'K t x y', // …after K resolves, the leaf rule (△) grows a stem: t y
      'K (t t) x y', // …and fork-construction (f) finishes a triple: t t y
      'S f g x', // S shares x between f and g: (f x)(g x)
      't (t a b) c (t t)', // triage: the F rule reads the argument's shape
      'not true',
      'and false true',
      { expr: 'add 2 3', stepMs: 90 } // the computation storm finale
    ],
    presets = [],
    stepMs = 1050,
    height = 360
  }: Props = $props()

  const exprOf = (e: ExprSpec) => (typeof e === 'string' ? e : e.expr)
  const speedOf = (e: ExprSpec) => (typeof e === 'string' ? undefined : e.stepMs)

  // the letters redexes wear: △ builds a stem from the leaf, f builds a fork
  const RULE_GLYPH = { L: '△', s: 'f', K: 'K', S: 'S', F: 'F' } as const

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
  let input = $state(exprOf(exprs[0] ?? 'not false'))
  // svelte-ignore state_referenced_locally
  let activeTip = $state(presets.find((p) => p.expr === input)?.tip ?? '')
  // the dropdown offers the presets when there are any, else the cycle list
  const exampleOptions = $derived(
    presets.length ? presets : exprs.map((e) => ({ expr: exprOf(e), tip: '' }))
  )

  // ---- the example picker: a combobox panel under the input ---------------
  const uid = $props.id()
  let pickerOpen = $state(false)
  let activeIdx = $state(-1) // keyboard/hover highlight in the open panel
  let inputEl: HTMLInputElement | undefined = $state()
  let wrapEl: HTMLDivElement | undefined = $state()
  const matchIdx = () => exampleOptions.findIndex((o) => o.expr === input)
  function openPicker() {
    if (!exampleOptions.length) return
    const m = matchIdx()
    activeIdx = m >= 0 ? m : 0
    pickerOpen = true
  }
  const closePicker = () => (pickerOpen = false)
  function pickIdx(i: number) {
    const o = exampleOptions[i]
    closePicker()
    if (o) load(o.expr)
  }
  // shift+←/→ hops through the examples (also bound in the widget's onKey)
  function cyclePreset(dir: 1 | -1) {
    const opts = exampleOptions
    if (!opts.length) return
    const cur = matchIdx()
    const next = cur === -1 ? (dir === 1 ? 0 : opts.length - 1) : (cur + dir + opts.length) % opts.length
    load(opts[next].expr)
  }
  function onInputKey(e: KeyboardEvent) {
    if (e.key === 'Enter') {
      if (pickerOpen && activeIdx >= 0) pickIdx(activeIdx)
      else load(input)
    } else if (e.key === 'ArrowDown') {
      e.preventDefault()
      if (!pickerOpen) openPicker()
      else if (activeIdx < exampleOptions.length - 1) activeIdx++
    } else if (e.key === 'ArrowUp' && pickerOpen) {
      e.preventDefault()
      if (activeIdx > 0) activeIdx--
    } else if (e.key === 'Escape' && pickerOpen) {
      e.stopPropagation()
      closePicker()
    }
  }
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
    vname: string | null // a named leaf's name — always shown
    fresh?: boolean // no prior position: genuinely new growth
    gdelay?: number // extra grow-in delay (staged triage: new joints wait for docking)
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
  // a pruned leaf falls on its own; pruned branches disintegrate in place.
  // `delay` staggers the exit: triage joins the chosen branch FIRST, and only
  // then do the rejected branches let go.
  interface Debris {
    id: number
    x: number
    y: number
    dy: number
    dx: number
    rot: number
    tint: string
    delay: number
    label?: string // a discarded named leaf falls with its name on
  }
  interface GhostEdge {
    id: number
    d: string
    w: number
    delay: number
    fx?: number // staged triage: the ghost rides the argument's flight…
    fy?: number
    flyMs?: number // …for this long before dissolving
  }
  // consumed pattern pieces shrink away where they stood
  interface Vanishing {
    id: number
    x: number
    y: number
    tag: T['tag']
    tint: string
    tilt: number
    delay: number
    fx?: number // staged triage: consumed argument shells fly to the branch…
    fy?: number
    flyMs?: number // …and dissolve on arrival
  }

  // ---- staged triage -------------------------------------------------------
  // The F rule gets a two-beat animation: the ARGUMENT physically flies to the
  // branch its shape selects (everything else holds still and the consumed
  // spine dissolves), then the docked pair reflows into the new tree — the new
  // root, when the redex was the root — while the rejected branches let go.
  // A flight is a rigid translation of one on-screen subtree, keyed by path
  // prefix (paths are /[01]*/ so prefix matching is exact).
  interface Flight {
    prefix: string
    dx: number
    dy: number
  }
  interface Stage {
    flights: Flight[]
    p1: number // flight duration; phase 2 (reflow) follows
  }
  const flightFor = (stage: Stage | undefined, path: string | undefined): Flight | null => {
    if (!stage || !path) return null
    for (const fl of stage.flights) if (path.startsWith(fl.prefix)) return fl
    return null
  }
  // where the argument's flight lands, relative to the chosen branch: a leaf
  // is absorbed dead-on; stems and forks dock just beside it, where the new
  // application will place them
  const LAND = { leaf: { dx: 0, dy: 0 }, beside: { dx: 20, dy: 14 } }
  function stageFor(sites: (T & { tag: 'apply' })[]): Stage | undefined {
    const prevByPath = new Map(nodes.map((n) => [n.path, n]))
    const flights: Flight[] = []
    for (const site of sites) {
      if (readyRule(site) !== 'F') continue
      // a shared redex ref rewrites at every occurrence; fly each one
      for (const occ of nodes) {
        if (occ.tree !== site) continue
        const xPath = occ.path + '1'
        const xN = prevByPath.get(xPath)
        const bPath =
          site.x.tag === 'leaf' ? occ.path + '000' : site.x.tag === 'stem' ? occ.path + '001' : occ.path + '01'
        const bN = prevByPath.get(bPath)
        if (!xN || !bN) continue
        const land = site.x.tag === 'leaf' ? LAND.leaf : LAND.beside
        flights.push({ prefix: xPath, dx: bN.x + land.dx - xN.x, dy: bN.y + land.dy - xN.y })
      }
    }
    return flights.length ? { flights, p1: 340 } : undefined
  }

  let nodes = $state<VNode[]>([])
  let edges = $state<VEdge[]>([])
  let debris = $state<Debris[]>([])
  let ghosts = $state<GhostEdge[]>([])
  let vanishing = $state<Vanishing[]>([])
  let debrisId = 0
  const W = 560
  const XSTEP = 46
  const YSTEP = 50
  const GROUND = 46

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']
  const VAR_TINT = '#dcaa5e' // named leaves are the grove's autumn leaves
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
      tint: n.tree.tag === 'var' ? VAR_TINT : LEAF_TINTS[hash(n.path) % LEAF_TINTS.length],
      tilt: (hash(n.path) % 70) - 35,
      label: nameOf(n.tree),
      vname: n.tree.tag === 'var' ? n.tree.name : null
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

  // ---- one tween for everything: nodes and branches share the same clock
  // and the same easing, so a branch endpoint never drifts from its leaf ----

  let anim: number | undefined
  // ease-out with a small overshoot; both nodes and edges ride this curve
  const ease = (t: number) => {
    const c = 0.9
    const u = t - 1
    return 1 + (c + 1) * u * u * u + c * u * u
  }
  // the flight's curve: accelerate, arrive softly (no overshoot mid-air)
  const easeInOut = (t: number) => (t < 0.5 ? 2 * t * t : 1 - (-2 * t + 2) ** 2 / 2)
  function animateTo(l: { nodes: VNode[]; edges: VEdge[] }, oldByRef: Map<T, VNode>, stage?: Stage) {
    cancelAnimationFrame(anim!)
    if (!motion || nodes.length === 0) {
      nodes = l.nodes
      edges = l.edges
      return
    }
    const nFrom = new Map(nodes.map((n) => [n.path, n]))
    const eFrom = new Map(edges.map((e) => [e.id, e]))
    // Reference correspondence is the general rewrite-animation rule: a
    // target node whose path survived stays put and glides; failing that, a
    // node whose TREE REFERENCE existed before glides (or splits, when a rule
    // duplicated it) from the reference's old position; only genuinely new
    // structure grows in from nothing.
    const fromFor = (tn: VNode): VNode | undefined => nFrom.get(tn.path) ?? oldByRef.get(tn.tree)
    // Staged (triage) tween: phase 1 flies the argument's subtree to a mid
    // waypoint (everything else holds), phase 2 settles the world into the
    // new layout. mid = from + its flight, or from itself (hold).
    const nPairs = l.nodes.map((tn) => {
      const f = fromFor(tn)
      const fl = flightFor(stage, f?.path)
      return {
        tn: { ...tn, fresh: !f, gdelay: !f && stage ? stage.p1 : 0 },
        f,
        mid: f ? (fl ? { x: f.x + fl.dx, y: f.y + fl.dy } : { x: f.x, y: f.y }) : null
      }
    })
    const pairByPath = new Map(nPairs.map((p) => [p.tn.path, p]))
    const newByPath = new Map(l.nodes.map((n) => [n.path, n]))
    const ePairs = l.edges.map((te) => {
      let f = eFrom.get(te.id)
      if (!f && !te.trunk) {
        // the child node may have moved here from elsewhere: bring its old
        // incoming branch along instead of growing a new one
        const childPath = te.id.replace('>', '')
        const child = newByPath.get(childPath)
        const old = child && !nFrom.get(childPath) ? oldByRef.get(child.tree) : undefined
        if (old && old.path !== 'r') {
          f = eFrom.get(`${old.path.slice(0, -1)}>${old.path.slice(-1)}`)
        }
      }
      // otherwise a NEW branch grows out of its parent endpoint
      const from = f ?? { ...te, x2: te.x1, y2: te.y1, cx: te.x1, cy: te.y1 }
      // an edge belongs to its child's subtree: it rides the child's flight,
      // so branches stay glued to the trees they carry
      const cPath = te.trunk ? 'r' : te.id.replace('>', '')
      const chFl = flightFor(stage, pairByPath.get(cPath)?.f?.path)
      const mid = chFl
        ? {
            x1: from.x1 + chFl.dx,
            y1: from.y1 + chFl.dy,
            cx: from.cx + chFl.dx,
            cy: from.cy + chFl.dy,
            x2: from.x2 + chFl.dx,
            y2: from.y2 + chFl.dy
          }
        : { x1: from.x1, y1: from.y1, cx: from.cx, cy: from.cy, x2: from.x2, y2: from.y2 }
      return { te, f: from, mid }
    })
    const t0 = performance.now()
    const D = stage ? stage.p1 + 420 : 550
    const split = stage ? stage.p1 / D : 0
    const lerpN = (tn: VNode, a: { x: number; y: number }, b: { x: number; y: number }, k: number) => ({
      ...tn,
      x: a.x + (b.x - a.x) * k,
      y: a.y + (b.y - a.y) * k
    })
    const lerpE = (te: VEdge, a: Omit<VEdge, 'id' | 'w' | 'trunk'>, b: Omit<VEdge, 'id' | 'w' | 'trunk'>, k: number) => ({
      ...te,
      x1: a.x1 + (b.x1 - a.x1) * k,
      y1: a.y1 + (b.y1 - a.y1) * k,
      cx: a.cx + (b.cx - a.cx) * k,
      cy: a.cy + (b.cy - a.cy) * k,
      x2: a.x2 + (b.x2 - a.x2) * k,
      y2: a.y2 + (b.y2 - a.y2) * k
    })
    const frame = (now: number) => {
      const t = Math.min(1, (now - t0) / D)
      if (stage && t < split) {
        const k = easeInOut(t / split)
        nodes = nPairs.map(({ tn, f, mid }) => (f && mid ? lerpN(tn, f, mid, k) : tn))
        edges = ePairs.map(({ te, f, mid }) => lerpE(te, f, mid, k))
      } else {
        const k = ease(stage ? (t - split) / (1 - split) : t)
        nodes = nPairs.map(({ tn, f, mid }) => {
          const a = stage ? mid : f && { x: f.x, y: f.y }
          return a ? lerpN(tn, a, tn, k) : tn
        })
        edges = ePairs.map(({ te, f, mid }) => lerpE(te, stage ? mid : f, te, k))
      }
      if (t < 1) anim = requestAnimationFrame(frame)
    }
    anim = requestAnimationFrame(frame)
  }

  // The redex's own pattern (the apply node, the rule's matched spine, and
  // for triage the argument's shell) is CONSUMED by a rule: it shrinks away.
  // Everything else that disappears was DISCARDED data: leaves fall, branches
  // disintegrate. This split is derivable from any pattern-matching rule, not
  // just these five.
  function spineOf(site: T & { tag: 'apply' }): T[] {
    const out: T[] = [site]
    const f = site.f
    if (f.tag === 'leaf' || f.tag === 'stem') out.push(f)
    else if (f.tag === 'fork') {
      out.push(f)
      const a = f.l
      if (a.tag === 'leaf' || a.tag === 'stem') out.push(a) // K, S
      else if (a.tag === 'fork') {
        out.push(a) // F
        out.push(site.x) // triage consumes the argument's shell
      }
    }
    return out
  }

  function show(
    tree: T,
    opts: { spine?: Set<T>; allShrink?: boolean; exitDelay?: number; stage?: Stage } = {}
  ) {
    const { spine, allShrink = false, exitDelay = 0, stage } = opts
    const prevNodes = nodes
    const prevEdges = edges
    const l = layout(tree)
    const oldByRef = new Map<T, VNode>()
    for (const n of prevNodes) if (!oldByRef.has(n.tree)) oldByRef.set(n.tree, n)
    if (motion && styled && prevNodes.length > 0) {
      const survivors = new Set(l.nodes.map((n) => n.tree))
      const removed = prevNodes.filter((n) => !survivors.has(n.tree))
      const removedPaths = new Set(removed.map((n) => n.path))
      const prevByPath = new Map(prevNodes.map((n) => [n.path, n]))
      const fresh: Debris[] = []
      const shrunk: Vanishing[] = []
      for (const n of removed) {
        const consumed = allShrink || (spine?.has(n.tree) ?? false)
        // consumed pattern pieces exit with the join; discarded data waits
        const delay = consumed ? 0 : exitDelay
        const fl = consumed ? flightFor(stage, n.path) : null
        if ((n.tag === 'leaf' || n.tag === 'var') && !consumed) {
          // fruit drops heavier than leaves: less drift, less spin
          const damp = n.tag === 'var' ? 0.35 : 1
          fresh.push({
            id: debrisId++,
            x: n.x,
            y: n.y,
            dy: height - GROUND + 4 - n.y + (hash(n.path) % 8),
            dx: ((hash(n.path) % 36) - 18) * 1.1 * damp,
            rot: ((hash(n.path) % 160) - 80) * damp,
            tint: n.tint,
            delay,
            label: n.vname ?? undefined
          })
        } else if (fl) {
          // a consumed argument shell rides the triage flight, then dissolves
          shrunk.push({
            id: debrisId++,
            x: n.x,
            y: n.y,
            tag: n.tag,
            tint: n.tint,
            tilt: n.tilt,
            delay: stage!.p1,
            fx: fl.dx,
            fy: fl.dy,
            flyMs: stage!.p1
          })
        } else {
          shrunk.push({ id: debrisId++, x: n.x, y: n.y, tag: n.tag, tint: n.tint, tilt: n.tilt, delay })
        }
      }
      if (fresh.length) {
        debris.push(...fresh)
        while (debris.length > 90) debris.shift()
        const ids = fresh.map((d) => d.id)
        setTimeout(() => {
          debris = debris.filter((d) => !ids.includes(d.id))
        }, 6400 + exitDelay)
      }
      if (shrunk.length) {
        vanishing.push(...shrunk)
        const ids = shrunk.map((v) => v.id)
        setTimeout(() => {
          vanishing = vanishing.filter((v) => !ids.includes(v.id))
        }, 460 + exitDelay + (stage?.p1 ?? 0))
      }
      // branches that lost their subtree fade out in place
      const newIds = new Set(l.edges.map((e) => e.id))
      const gone = prevEdges.filter(
        (e) => !e.trunk && !newIds.has(e.id) && removedPaths.has(e.id.replace('>', ''))
      )
      if (gone.length) {
        const fading: GhostEdge[] = gone.map((e) => {
          const childPath = e.id.replace('>', '')
          const child = prevByPath.get(childPath)
          const consumed = allShrink || (child !== undefined && (spine?.has(child.tree) ?? false))
          const fl = consumed ? flightFor(stage, childPath) : null
          return {
            id: debrisId++,
            d: edgeD(e),
            w: e.w,
            delay: consumed ? (fl ? stage!.p1 : 0) : exitDelay,
            fx: fl?.dx,
            fy: fl?.dy,
            flyMs: fl ? stage!.p1 : undefined
          }
        })
        ghosts.push(...fading)
        const ids = fading.map((g) => g.id)
        setTimeout(() => {
          ghosts = ghosts.filter((g) => !ids.includes(g.id))
        }, 500 + exitDelay + (stage?.p1 ?? 0))
      }
    }
    animateTo(l, oldByRef, stage)
  }

  // ---- the run loop ----------------------------------------------------------

  let timer: ReturnType<typeof setTimeout> | undefined
  let exprIdx = 0
  let atNormalForm = $state(false)
  // some expressions bring their own clock (add 2 3 runs a few hundred steps)
  // svelte-ignore state_referenced_locally
  let curStepMs = stepMs
  const speedFor = (src: string) => {
    const e = exprs.find((x) => exprOf(x) === src)
    return (e && speedOf(e)) || stepMs
  }

  function load(src: string, opts: { keepAuto?: boolean } = {}) {
    clearTimeout(timer)
    if (!opts.keepAuto) autoCycle = false
    pickerOpen = false
    err = ''
    ruleMsg = ''
    stepCount = 0
    atNormalForm = false
    curStepMs = speedFor(src)
    debris = []
    ghosts = []
    vanishing = []
    history = []
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

  // each fired step remembers where it came from, so ◀ can walk back
  let history: { tree: T; msg: string; steps: number }[] = []
  let historyLen = $state(0)
  const remember = () => {
    if (!cur) return
    history.push({ tree: cur, msg: ruleMsg, steps: stepCount })
    if (history.length > 400) history.shift()
    historyLen = history.length
  }

  function fire(): boolean {
    if (!cur) return false
    // gather the pattern spines BEFORE stepping: these pieces are consumed
    // (they shrink); anything else that vanishes was discarded (it falls).
    // Triage sites also get a stage plan (the argument's flight to its branch),
    // computed against the CURRENT on-screen positions.
    const sites = (parallel ? readySites(cur) : [nextApplyToFire(cur)].filter((s) => s !== null)) as (T & {
      tag: 'apply'
    })[]
    const spine = new Set(sites.flatMap((s) => spineOf(s)))
    const stage = stageFor(sites)
    if (parallel) {
      const s = stepParallel(cur)
      if (!s) {
        ruleMsg = `normal form: ${pretty(cur)}`
        atNormalForm = true
        return false
      }
      remember()
      cur = s.next
      stepCount++
      ruleMsg =
        s.fired.length === 1
          ? `1 redex fired (${s.fired[0]})`
          : `${s.fired.length} redexes fired in parallel (${s.fired.join(' ')})`
      // triage in two beats: the argument flies to its branch, and only once
      // it docks do the rejected branches let go
      show(cur, { spine, exitDelay: stage?.p1 ?? 0, stage })
      return true
    }
    const s = stepOnce(cur)
    if (!s) {
      ruleMsg = `normal form: ${pretty(cur)}`
      atNormalForm = true
      return false
    }
    remember()
    cur = s.next
    stepCount++
    ruleMsg = s.rule
    show(cur, { spine, exitDelay: stage?.p1 ?? 0, stage })
    return true
  }

  function back() {
    const h = history.pop()
    if (!h) return
    historyLen = history.length
    running = false
    autoCycle = false
    clearTimeout(timer)
    cur = h.tree
    stepCount = h.steps
    ruleMsg = h.msg || 'stepped back'
    atNormalForm = false
    // undoing a rewrite: the rule's products shrink away, its inputs regrow
    show(cur, { allShrink: true })
  }

  function schedule() {
    clearTimeout(timer)
    timer = setTimeout(() => {
      if (!running) return
      const went = fire()
      if (went) {
        schedule()
      } else if (autoCycle) {
        timer = setTimeout(nextExpr, Math.max(curStepMs * 2.2, 2000))
      } else {
        running = false
      }
    }, curStepMs)
  }

  function nextExpr() {
    exprIdx = (exprIdx + 1) % exprs.length
    running = true
    load(exprOf(exprs[exprIdx]), { keepAuto: true })
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

  // keyboard drive when the widget itself is focused (clicking it focuses it):
  // space runs/pauses, ◀ steps back, ▶ steps forward, shift+◀/▶ hops through
  // the examples. Keys landing on the input or buttons keep their normal
  // meaning — except the shift-hop, which works anywhere but the text input
  // (where shift+arrows select text).
  function onKey(e: KeyboardEvent) {
    if (
      e.shiftKey &&
      (e.key === 'ArrowLeft' || e.key === 'ArrowRight') &&
      (e.target as HTMLElement).tagName !== 'INPUT'
    ) {
      e.preventDefault()
      cyclePreset(e.key === 'ArrowRight' ? 1 : -1)
      return
    }
    if (e.target !== e.currentTarget) return
    if (e.key === ' ') {
      e.preventDefault()
      toggleRun()
    } else if (e.key === 'ArrowLeft') {
      e.preventDefault()
      back()
    } else if (e.key === 'ArrowRight') {
      e.preventDefault()
      step()
    }
  }

  onMount(() => {
    load(input, { keepAuto: autoCycle })
  })
  onDestroy(() => clearTimeout(timer))
</script>

<svelte:document
  onpointerdown={(e) => {
    if (pickerOpen && wrapEl && !wrapEl.contains(e.target as Node)) closePicker()
  }}
/>

<!-- the widget is a keyboard-drivable instrument: focus it, then space
  runs/pauses and the arrow keys step — hence the tabindex on a div -->
<!-- svelte-ignore a11y_no_noninteractive_tabindex, a11y_no_noninteractive_element_interactions -->
<div
  class="vis"
  class:plain={!styled}
  class:lab={variant === 'lab'}
  tabindex="0"
  role="application"
  aria-label="tree-calculus visualizer — space runs or pauses, arrow keys step back and forward"
  onkeydown={onKey}
>
  {#if showControls}
    <div class="controls">
      <!-- input and example picker are ONE control: the caret opens a panel
           attached under the input; picking overrides the input, and whatever
           you type highlights its own entry when it matches one -->
      <div class="inputwrap" bind:this={wrapEl}>
        <input
          bind:this={inputEl}
          type="text"
          class:open={pickerOpen}
          bind:value={input}
          onkeydown={onInputKey}
          spellcheck="false"
          autocomplete="off"
          role="combobox"
          aria-expanded={pickerOpen}
          aria-controls="{uid}-droplist"
          aria-autocomplete="list"
          aria-label="tree-calculus expression"
        />
        <button
          type="button"
          class="caretbtn"
          class:open={pickerOpen}
          onclick={() => {
            if (pickerOpen) closePicker()
            else openPicker()
            inputEl?.focus()
          }}
          aria-label="show example expressions"
          aria-expanded={pickerOpen}
          title="examples — shift+←/→ cycles them"
        >
          ▾
        </button>
        {#if pickerOpen}
          <div class="droplist" id="{uid}-droplist" role="listbox" tabindex="-1">
            {#each exampleOptions as o, i}
              <button
                type="button"
                class="dropitem"
                class:active={i === activeIdx}
                class:current={o.expr === input}
                role="option"
                aria-selected={o.expr === input}
                onpointerenter={() => (activeIdx = i)}
                onclick={() => pickIdx(i)}
              >
                <span class="dexpr">{o.expr}</span>
                {#if o.tip}<span class="dtip">{o.tip}</span>{/if}
              </button>
            {/each}
          </div>
        {/if}
      </div>
      <button class="cbtn" onclick={back} disabled={historyLen === 0} title="step backwards (←)">◀</button>
      <button class="cbtn primary" onclick={step} title="fire the next reduction (→)">Step</button>
      <button class="cbtn" onclick={toggleRun} title="run/pause (space)">{running ? 'Pause' : 'Run'}</button>
      <button class="cbtn" onclick={reset}>Reset</button>
    </div>
    {#if activeTip}<p class="tip-line">{activeTip}</p>{/if}
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

      <!-- pruned leaves fall; their branches disintegrate where they stood -->
      {#each debris as d (d.id)}
        <g style="transform: translate({d.x}px, {d.y}px)">
          <g class="debris" style="--dy: {d.dy}px; --dx: {d.dx}px; --xdelay: {d.delay}ms">
            <g class="dspin" style="--rot: {d.rot}deg; animation-delay: {d.delay}ms">
              {#if d.label}
                <circle r="9" class="fruitbody" />
                <text
                  class="vname"
                  y="0.5"
                  style="font-size: {d.label.length === 1 ? 9.5 : d.label.length === 2 ? 8 : 6.5}px"
                  >{d.label.length <= 3 ? d.label : d.label.slice(0, 3)}</text
                >
              {:else}
                <path
                  class="leafshape"
                  d="M0,-7 C4.5,-4 4.5,2.5 0,7 C-4.5,2.5 -4.5,-4 0,-7 Z"
                  style="fill:{d.tint}"
                />
              {/if}
            </g>
          </g>
        </g>
      {/each}
      {#each ghosts as g (g.id)}
        {#if g.flyMs}
          <g class="vfly" style="--fx: {g.fx}px; --fy: {g.fy}px; --fms: {g.flyMs}ms">
            <path class="branch ghost" d={g.d} style="stroke-width:{g.w}; animation-delay: {g.delay}ms" />
          </g>
        {:else}
          <path class="branch ghost" d={g.d} style="stroke-width:{g.w}; animation-delay: {g.delay}ms" />
        {/if}
      {/each}
      <!-- consumed pattern pieces shrink away where they stood (or, on a
           triage flight, where they landed) -->
      {#each vanishing as v (v.id)}
        <g style="transform: translate({v.x}px, {v.y}px)">
          <g
            class="vfly"
            style={v.flyMs ? `--fx: ${v.fx}px; --fy: ${v.fy}px; --fms: ${v.flyMs}ms` : '--fx: 0px; --fy: 0px; --fms: 1ms'}
          >
            <g class="vshrink" style="animation-delay: {v.delay}ms">
              {#if v.tag === 'leaf'}
                <path
                  class="leafshape"
                  d="M0,-8 C5.2,-5 5.2,2.5 0,8 C-5.2,2.5 -5.2,-5 0,-8 Z"
                  style="fill:{v.tint}; transform: rotate({v.tilt}deg)"
                />
              {:else if v.tag === 'var'}
                <circle r="9" class="fruitbody" />
              {:else if v.tag === 'apply'}
                <circle r="6" class="bud-ring" />
              {:else}
                <circle r="3.6" class="knot" />
              {/if}
            </g>
          </g>
        </g>
      {/each}

      <g class="tree" class:swaying={styled && motion}>
        {#each edges as e (e.id)}
          <path d={edgeD(e)} class="branch" class:trunk={e.trunk} style="stroke-width:{e.w}" />
        {/each}
        {#each nodes as n (n.path)}
          <g class="node" style="transform: translate({n.x}px, {n.y}px)">
            <g
              class="inner"
              class:growing={motion && styled && n.fresh !== false}
              style="--gd: {(n.gdelay ?? 0) + n.depth * 55}ms"
            >
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
              {:else if n.tag === 'var'}
                <!-- a named leaf is FRUIT: rounder, bigger, unmistakably not foliage -->
                <g class="fruit" style="transform: rotate({n.tilt * 0.12}deg)">
                  {#if styled}<path class="fstalk" d="M0,-9 Q1,-13 3.5,-14.5" />{/if}
                  <circle r="10" class="fruitbody" />
                  {#if styled}<ellipse cx="-3.4" cy="-3.8" rx="2.9" ry="1.8" class="fshine" />{/if}
                  {#if n.vname && n.vname.length <= 3}
                    <text
                      class="vname"
                      y="0.5"
                      style="font-size: {n.vname.length === 1 ? 10 : n.vname.length === 2 ? 8.5 : 7}px"
                      >{n.vname}</text
                    >
                  {:else}
                    <text class="vname above" y="-17">{n.vname}</text>
                  {/if}
                </g>
              {:else}
                <circle r={styled ? 4 : 5.5} class="knot" />
              {/if}
              {#if n.ready}
                <g class="firemark" class:next={n.next || parallel}>
                  <circle r="7.5" class="fire-ring" />
                  <text class="fire-letter" y="0.5">{RULE_GLYPH[n.ready]}</text>
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
        pickerOpen = false
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
    outline: none;
    border-radius: var(--radius, 12px);
  }
  /* keyboard drive: a visible ring only when focus came from the keyboard —
     clicking to grab space/arrow control stays visually quiet */
  .vis:focus-visible {
    box-shadow: 0 0 0 2px color-mix(in oklab, var(--accent) 55%, transparent);
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
  .inputwrap {
    position: relative;
    flex: 1;
    min-width: 170px;
    display: flex;
  }
  input {
    flex: 1;
    width: 100%;
    min-width: 0;
    background: var(--bg-code);
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    padding: 0.42em 2em 0.42em 0.7em;
    outline: none;
  }
  input:focus { border-color: var(--accent); }
  /* open state: the input and the panel fuse into one combobox */
  input.open {
    border-color: var(--accent);
    border-radius: 8px 8px 0 0;
  }
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
  /* the example picker: a caret button inside the input opens a panel
     attached flush under it — input + dropdown as one control */
  .caretbtn {
    position: absolute;
    top: 1px;
    right: 1px;
    bottom: 1px;
    width: 2em;
    border: none;
    background: none;
    color: var(--fg-faint);
    font-size: 0.8rem;
    cursor: pointer;
    border-radius: 0 7px 7px 0;
    transition: transform 0.15s ease, color 0.15s ease;
  }
  .caretbtn:hover { color: var(--accent); }
  .caretbtn.open {
    color: var(--accent);
    transform: rotate(180deg);
    border-radius: 7px 0 0 7px;
  }
  .droplist {
    position: absolute;
    top: 100%;
    left: 0;
    right: 0;
    z-index: 40;
    display: flex;
    flex-direction: column;
    background: var(--bg-code);
    border: 1px solid var(--accent);
    border-top: none;
    border-radius: 0 0 8px 8px;
    box-shadow: 0 14px 28px -14px color-mix(in oklab, var(--fg) 35%, transparent);
    max-height: 264px;
    overflow-y: auto;
  }
  .dropitem {
    display: flex;
    align-items: baseline;
    gap: 0.7em;
    padding: 0.42em 0.75em;
    border: none;
    background: none;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.82rem;
    text-align: left;
    cursor: pointer;
    width: 100%;
  }
  .dropitem.active { background: color-mix(in oklab, var(--accent) 13%, transparent); }
  .dropitem.current .dexpr { color: var(--accent); font-weight: 650; }
  .dexpr { flex: none; white-space: nowrap; }
  .dtip {
    flex: 1;
    min-width: 0;
    color: var(--fg-faint);
    font-size: 0.7rem;
    font-style: italic;
    font-family: var(--font-body);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
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

  /* named leaves (free variables) are FRUIT: big, round, name worn on the skin */
  .fruitbody {
    fill: #e79a3f;
    stroke: color-mix(in oklab, #8a5410 55%, transparent);
    stroke-width: 0.8;
  }
  .plain .fruitbody { fill: #dfa050; stroke: var(--fg-faint); }
  .fshine { fill: rgba(255, 255, 255, 0.42); }
  .fstalk {
    stroke: var(--bark);
    stroke-width: 1.5;
    fill: none;
    stroke-linecap: round;
  }
  .vname {
    fill: #45290a;
    font-size: 8px;
    font-weight: 700;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }
  .vname.above {
    fill: var(--warn);
    font-size: 10px;
  }

  /* ---- pruned leaves: fall (1s), rest (~2.4s), let go (3.4s) ----
     --xdelay staggers the whole exit (triage's rejected branches wait for
     the chosen one to join before they drop) */
  .debris {
    animation:
      dfall 1s cubic-bezier(0.45, 0.05, 0.75, 0.6) forwards,
      dfade 3.4s linear forwards;
    animation-delay: var(--xdelay, 0ms), calc(2.4s + var(--xdelay, 0ms));
  }
  .dspin {
    animation: dspin 1s cubic-bezier(0.45, 0.05, 0.75, 0.6) forwards;
  }
  @keyframes dfall {
    to {
      transform: translate(var(--dx), var(--dy));
    }
  }
  @keyframes dspin {
    to {
      transform: rotate(var(--rot));
    }
  }
  @keyframes dfade {
    to {
      opacity: 0;
    }
  }
  /* consumed pattern pieces shrink away (absorbed by the rewrite) */
  .vshrink {
    animation: vshrink 0.42s ease-in forwards;
  }
  @keyframes vshrink {
    to {
      transform: scale(0);
      opacity: 0;
    }
  }
  /* a triage flight: consumed argument shells (and their branches) ride to
     the chosen branch before dissolving there */
  .vfly {
    animation: vfly var(--fms, 1ms) cubic-bezier(0.45, 0.05, 0.55, 0.95) forwards;
  }
  @keyframes vfly {
    to {
      transform: translate(var(--fx, 0px), var(--fy, 0px));
    }
  }

  /* pruned branches disintegrate where they stood */
  .branch.ghost {
    animation: eghost 0.45s ease-out forwards;
  }
  @keyframes eghost {
    to {
      opacity: 0;
      transform: translateY(5px);
    }
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
