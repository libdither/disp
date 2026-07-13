<script lang="ts">
  // The shared tree-calculus visualizer. One component, two temperaments:
  //   ambient — self-driving scenery for the landing page and visualizer
  //   lab     — the Learn page's instrument, controls out and honest
  // Either way it renders real reductions: pruned subtrees fall to the
  // ground, rest a while, and fade; surviving branches glide to their new
  // positions; ready redexes wear the letter of the rule about to fire.
  import { onMount, onDestroy } from 'svelte'
  import { fade, scale } from 'svelte/transition'
  import { backOut } from 'svelte/easing'
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
    strValue,
    treeEq,
    nodeCount,
    replaceNode,
    stuckSpine,
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
    // the dictionary names parse against (REPLACES the built-in toy defs —
    // an embedding with real definitions must not let `add`-the-teaching-tree
    // shadow `add`-the-library-value). Substituted defs enter as collapsed
    // PODS: real structure, drawn folded until a reduction consumes them or
    // they're clicked open.
    defs?: Record<string, T>
    // async fallback for names outside `defs` (the playground resolves them
    // against its live elaboration session). A hit re-parses the input if no
    // step has fired yet; a miss leaves the name a free variable (fruit).
    resolveDef?: (name: string) => Promise<T | null>
    // embedding chrome: hide the prev/next tree cycling (and shift+arrow)
    minimal?: boolean
    // replaces the edit box ⓘ's combinator list when the host brings its own
    // name universe
    namesHint?: string
    // two-way fold sync: BEFORE any step has fired, clicking a labeled pod
    // offers the unfold to the host (the playground unfolds its buffer value
    // and re-seeds this panel). Return true = handled externally; false =
    // reveal locally as usual.
    onPodOpen?: (label: string) => boolean
    // engine macro-step: when local stepping is stuck on a named head whose
    // definition never shipped (too large for a pod), the host may perform
    // the application on the REAL evaluator and return the result tree —
    // the reduction the elaborator pre-applies, shown as one step. null =
    // the engine couldn't (unbound name, divergent, or oversized result).
    engineStep?: (name: string, args: T[]) => Promise<T | null>
    // parse with EVERY application held as an apply node: expressions arrive
    // carrying their construction work, so the reducer always has visible
    // steps (the playground panel's posture) instead of a pre-built tree
    lazyParse?: boolean
    // imperative handle: re-seed the instrument IN PLACE (new program + defs)
    // — the tree animates from its current drawing instead of remounting
    api?: (a: { setProgram: (expr: string, newDefs?: Record<string, T>) => void }) => void
  }

  // The tree-calculus pieces — the ONE source shared by the cassette (symbol
  // tape) AND the example list / ambient auto-cycle, so the two match precisely:
  // stem (△), fork (s), K, S, the three triage variations (△ subscripted by the
  // argument shape it reads — leaf t / stem s / fork f), then not / and / add.
  const PIECES: { sym: string; sub?: string; expr: string; tip: string; sep?: boolean; stepMs?: number; lazyTop?: boolean }[] = [
    { sym: '△', expr: 't x', tip: 'stem · a leaf applied builds a stem (△ x)', lazyTop: true },
    { sym: 's', expr: 't x y', tip: 'fork · a stem applied builds a fork (△ x y)', lazyTop: true },
    { sym: 'K', expr: 'K x y', tip: 'K · keep the first argument, discard the second' },
    { sym: 'S', expr: 'S f g x', tip: 'S · share x between both sides: (f x) (g x)' },
    { sym: '△', sub: 't', expr: 't (t a b) c t', tip: 'triage · argument is a leaf → take the first branch (a)' },
    { sym: '△', sub: 's', expr: 't (t a b) c (t u)', tip: 'triage · argument is a stem → second branch on its child (b u)' },
    { sym: '△', sub: 'f', expr: 't (t a b) c (t u v)', tip: 'triage · argument is a fork → third branch on its children (c u v)' },
    { sym: '¬', expr: 'not true', tip: 'not · negation, built from triage', sep: true },
    { sym: '∧', expr: 'and false true', tip: 'and · conjunction' },
    { sym: '+', expr: 'add 2 3', tip: 'add · the recursion storm', stepMs: 90 }
  ]

  let {
    variant = 'ambient',
    // the ambient tour rolls through the pieces above, in palette order
    exprs = PIECES.map((p) => (p.stepMs ? { expr: p.expr, stepMs: p.stepMs } : p.expr)),
    presets = [],
    stepMs = 1050,
    height = 360,
    defs = undefined,
    resolveDef = undefined,
    minimal = false,
    namesHint = undefined,
    onPodOpen = undefined,
    engineStep = undefined,
    lazyParse = false,
    api = undefined
  }: Props = $props()

  // the live name dictionary: the host's defs (or the built-in toy set) —
  // replaceable via api.setProgram — plus whatever resolveDef has supplied.
  // Initial value read-once like the other posture props.
  // svelte-ignore state_referenced_locally
  let hostDefs = $state.raw<Record<string, T>>(defs ?? DEFS)
  let resolved = $state<Record<string, T>>({})
  const activeDefs = $derived({ ...hostDefs, ...resolved })

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
  let parallel = $state(true) // fire every ready redex per step — on by default
  let styled = $state(true) // nature styling vs plain diagram
  let motion = $state(true) // transitions + debris vs instant
  // svelte-ignore state_referenced_locally
  let labels = $state(variant === 'lab') // name badges on recognized subtrees

  // svelte-ignore state_referenced_locally
  let input = $state(exprOf(exprs[0] ?? 'not false'))
  // svelte-ignore state_referenced_locally
  let activeTip = $state(presets.find((p) => p.expr === input)?.tip ?? '')
  // the dropdown offers the presets when there are any, else the PIECES list —
  // the same source the cassette tape uses, so the two match precisely
  const exampleOptions = $derived(
    presets.length ? presets : PIECES.map((p) => ({ expr: p.expr, tip: p.tip }))
  )

  // ---- the cassette: a chip that toggles open into a stationary tape; a
  // draggable selector rides the tape and snaps to the nearest piece. Shares
  // PIECES with the example list, so cassette and prev/next agree. -----------
  const matchPaletteIdx = (s: string) => PIECES.findIndex((it) => it.expr === s)
  // svelte-ignore state_referenced_locally
  let selectedIdx = $state(Math.max(0, matchPaletteIdx(input)))
  $effect(() => {
    const i = matchPaletteIdx(input)
    if (i >= 0) selectedIdx = i
  })
  const clampIdx = (i: number) => Math.max(0, Math.min(PIECES.length - 1, i))
  const CELLW = 34 // tape cell width
  const CELLH = 28 // tape cell height — the pieces stack VERTICALLY
  let cassetteOpen = $state(false) // chip ⇄ expanded tape
  let showSelectors = $state(false) // the ⚘ tree toggle reveals the view selectors
  let cassetteWrapEl: HTMLDivElement | undefined = $state()
  let selectorsWrapEl: HTMLDivElement | undefined = $state()
  // press anywhere on the (vertical) tape and the selector jumps there, then
  // scrubs. Each landing snaps to a cell (sticky, bouncy via CSS) and previews
  // that piece frozen — the tree changes as you drag, nothing plays.
  let tapeEl: HTMLDivElement | undefined = $state()
  let selDragging = $state(false)
  // the selector rides UP from the bottom cell (leaf △) → sticky; CSS bounces it
  const selY = $derived(selectedIdx * CELLH)
  // which cell the pointer is over — measured from the tape's bottom (3px pad)
  const cellAtY = (clientY: number) => {
    if (!tapeEl) return selectedIdx
    const r = tapeEl.getBoundingClientRect()
    return clampIdx(Math.floor((r.bottom - 3 - clientY) / CELLH))
  }

  // select a piece: load and RUN it (tape release and prev/next land here)
  function pickPalette(i: number) {
    const t = clampIdx(i)
    selectedIdx = t
    running = true
    load(PIECES[t].expr)
  }
  // preview a piece WITHOUT playing — used live while scrubbing, so the tree
  // changes as you drag but nothing runs. Cleared nodes = instant swap.
  function loadFrozen(i: number) {
    selectedIdx = clampIdx(i)
    running = false
    autoCycle = false
    clearTimeout(timer)
    nodes = []
    edges = []
    load(PIECES[selectedIdx].expr)
  }
  // the selector rises out of the chip (y = 0) up to its cell as the tape unfurls
  const selectorIn = (_n: Element, { duration = 300 } = {}) => ({
    duration,
    easing: backOut,
    css: (t: number) => `opacity: ${t}; transform: translateY(${-selY * t}px)`
  })
  // opening the tape freezes the ambient cycle so the selector holds still
  function openCassette() {
    cassetteOpen = true
    autoCycle = false
    running = false
    clearTimeout(timer)
  }
  function tapeDown(e: PointerEvent) {
    selDragging = true
    tapeEl?.setPointerCapture(e.pointerId)
    const li = cellAtY(e.clientY)
    if (li !== selectedIdx) loadFrozen(li) // jump to the pressed cell
  }
  function tapeMove(e: PointerEvent) {
    if (!selDragging) return
    const li = cellAtY(e.clientY)
    if (li !== selectedIdx) loadFrozen(li) // scrub
  }
  function tapeUp() {
    if (!selDragging) return
    selDragging = false
    pickPalette(selectedIdx) // land + run
  }

  const matchIdx = () => exampleOptions.findIndex((o) => o.expr === input)
  // prev/next switch trees; stop at the ends rather than wrapping
  function cyclePreset(dir: 1 | -1) {
    const opts = exampleOptions
    if (!opts.length) return
    const cur = matchIdx()
    const next = cur === -1 ? (dir === 1 ? 0 : opts.length - 1) : cur + dir
    if (next < 0 || next >= opts.length) return
    load(opts[next].expr)
  }
  // focusing the edit box pauses the reduction so it holds still while you type
  function pauseForEdit() {
    autoCycle = false
    running = false
    clearTimeout(timer)
  }
  // the edit box parses AS YOU TYPE: each keystroke that yields a valid
  // expression swaps the tree in immediately (frozen, the cassette-scrub
  // posture); mid-edit gibberish holds the last good tree and just tints the
  // underline. Enter restarts the loaded tree and drops focus, so the arrow
  // keys step it straight away; Escape only drops focus.
  let editInvalid = $state(false)
  function liveParse() {
    let t: T
    try {
      const lazyTop = PIECES.find((p) => p.expr === input)?.lazyTop ?? false
      t = parseTree(input, activeDefs, { lazyTop, lazyAll: lazyParse })
    } catch {
      editInvalid = true
      return
    }
    editInvalid = false
    if (cur && treeEq(t, cur)) return // cosmetic edit — same tree, keep it
    nodes = []
    edges = []
    load(input)
  }
  function onEditKey(e: KeyboardEvent) {
    if (e.key === 'Enter') {
      e.preventDefault()
      load(input)
      // hand the keyboard to the instrument: arrows step, space runs
      ;((e.target as HTMLElement).closest('.vis') as HTMLElement | null)?.focus()
    } else if (e.key === 'Escape') {
      ;(e.target as HTMLElement).blur()
    }
  }
  // RAW state on purpose: the whole animation pipeline (oldByRef, survivors,
  // podNames, next-site marks) matches subtrees by OBJECT IDENTITY, and a
  // deep $state proxy would wrap nodes on read — the same subtree seen once
  // through `cur` and once from a fresh parse would no longer be ===. These
  // are only ever reassigned wholesale, so raw loses nothing (and spares the
  // proxy machinery a thousand-node tree per frame).
  let cur = $state.raw<T | null>(null)
  let ruleMsg = $state('')
  let stepCount = $state(0)
  let err = $state('')

  // ---- pods: subtrees drawn folded ---------------------------------------
  // Display-only folding over the real tree (reduction always computes on
  // full structure), keyed by object identity, mirroring the playground's
  // inline unfolds: a click reveals ONE budgeted level (big children re-fold
  // as new pods), and a reduction step never explodes a pod — data it newly
  // exposes above the size threshold folds shut on arrival, named when the
  // dictionary recognizes it, '…' otherwise. Only active with host defs
  // (podMode); the built-in tour keeps drawing everything open. Not reactive
  // state: every change is followed by show(cur), which relayouts.
  // svelte-ignore state_referenced_locally
  const podMode = !!(defs || resolveDef)
  const AUTOFOLD_MIN = 9 // unseen non-apply data this big folds shut
  const UNFOLD_BUDGET = 16 // nodes revealed per pod click
  let folded = new Map<T, string | null>() // root -> label (null = anonymous)
  let seen = new WeakSet<T>() // every node that has ever been drawn
  const sizeMemo = new WeakMap<T, number>()
  function sizeOf(t: T): number {
    const hit = sizeMemo.get(t)
    if (hit !== undefined) return hit
    const n = 1 + childrenOf(t).reduce((s, c) => s + sizeOf(c), 0)
    sizeMemo.set(t, n)
    return n
  }
  // a folded region's label: numeral, string, dictionary match, or anonymous
  function autoName(t: T): string | null {
    const n = natValue(t)
    if (n !== null && n >= 1) return String(n)
    const s = strValue(t)
    if (s !== null) return JSON.stringify(s)
    if (sizeOf(t) >= 3) {
      for (const [name, d] of Object.entries(activeDefs)) if (treeEq(t, d)) return name
    }
    return null
  }
  // reveal one budgeted level from a pod root: children beyond the budget
  // re-fold — but ONLY when they have a recognized name (a numeral, a
  // string, a dictionary match). An unnameable subtree is not a pod: hiding
  // structure behind an ellipsis reads as noise, so it draws instead.
  function reveal(root: T, budget: number) {
    let left = budget
    const visit = (t: T) => {
      seen.add(t)
      left--
      for (const c of childrenOf(t)) {
        if (seen.has(c) || folded.has(c)) continue
        if (left > 0 || sizeOf(c) <= 3) {
          visit(c)
          continue
        }
        const name = autoName(c)
        if (name !== null) folded.set(c, name)
        else visit(c)
      }
    }
    visit(root)
  }
  function openPod(t: T) {
    // before any step, a labeled pod is a host atom — the host may prefer to
    // unfold its own value (and re-seed this panel) instead of a local reveal
    const label = folded.get(t)
    if (onPodOpen && stepCount === 0 && label && onPodOpen(label)) return
    if (!folded.delete(t) || !cur) return
    reveal(t, UNFOLD_BUDGET)
    show(cur)
  }
  // after a step: fold big DATA the rewrite newly exposed — but only when it
  // has a recognized NAME, and never at the root (the answer always draws;
  // apply nodes — the computation's own machinery — always draw too)
  function refreshFolds(t: T) {
    if (!podMode) return
    // a rewrite can SELECT an existing pod as the whole result (K keeping a
    // folded branch) — the root always opens
    folded.delete(t)
    const visit = (n: T, isRoot: boolean) => {
      if (folded.has(n)) return
      if (!seen.has(n)) {
        if (!isRoot && n.tag !== 'apply' && sizeOf(n) > AUTOFOLD_MIN) {
          const name = autoName(n)
          if (name !== null) {
            folded.set(n, name)
            return
          }
        }
        seen.add(n)
      }
      for (const c of childrenOf(n)) visit(c, false)
    }
    visit(t, true)
  }

  // names outside activeDefs, asked of the host once each; a hit swaps the
  // fruit for the real definition — but only before any step has fired
  // (swapping a tree mid-reduction under the viewer is worse than fruit)
  const attempted = new Set<string>()
  function maybeResolve(t: T) {
    if (!resolveDef) return
    const missing = new Set<string>()
    const walk = (n: T): void => {
      if (n.tag === 'var' && !(n.name in activeDefs) && !attempted.has(n.name)) missing.add(n.name)
      for (const c of childrenOf(n)) walk(c)
    }
    walk(t)
    for (const name of missing) {
      attempted.add(name)
      void resolveDef(name).then((tree) => {
        if (!tree) return
        resolved = { ...resolved, [name]: tree }
        if (stepCount === 0 && cur) load(input)
      })
    }
  }

  // a normal form has no apply nodes left; a tree that still carries them is
  // STUCK — waiting on a free variable's shape (an honest distinction the
  // readout should make)
  const hasApply = (t: T): boolean => t.tag === 'apply' || childrenOf(t).some(hasApply)
  function restMsg(t: T): string {
    return hasApply(t)
      ? `stuck on free variables: ${pretty(t, { names: true, defs: activeDefs })}`
      : `normal form: ${pretty(t, { names: true, defs: activeDefs })}`
  }

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
    pod: string | null // a folded defs splice: real structure, drawn shut
    fresh?: boolean // no prior position: genuinely new growth
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
    label?: string // a discarded named leaf falls with its name on
  }
  interface GhostEdge {
    id: number
    d: string
    w: number
  }
  // consumed pattern pieces dissolve in place — the collapse marks the site;
  // gliding them toward the joint read as clutter, so they stay put
  interface Vanishing {
    id: number
    x: number
    y: number
    tag: T['tag']
    tint: string
    tilt: number
  }

  // raw like `cur` (VNode.tree must stay ===-comparable across frames)
  let nodes = $state.raw<VNode[]>([])
  let edges = $state.raw<VEdge[]>([])
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
    for (const [name, d] of Object.entries(activeDefs)) if (treeEq(t, d)) return name
    return null
  }

  function layout(tree: T): { nodes: VNode[]; edges: VEdge[] } {
    type Lay = { path: string; tree: T; x: number; depth: number }
    const out: Lay[] = []
    function go(t: T, path: string, depth: number): { rootX: number; width: number } {
      // a pod draws as a single closed node — its real children stay unshown
      const kids = folded.has(t) ? [] : childrenOf(t)
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
    const vnodes: VNode[] = out.map((n) => {
      const pod = folded.has(n.tree) ? (folded.get(n.tree) ?? '…') : null
      return {
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
        label: pod ? null : nameOf(n.tree), // a pod already wears its name
        vname: n.tree.tag === 'var' ? n.tree.name : null,
        pod
      }
    })
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
  // returns the ids of the old edges the tween carries forward (as a same-id
  // morph or as a moved child's travelling branch) — every other old edge is
  // the caller's to fade out, so no branch ever pops off screen
  function animateTo(
    l: { nodes: VNode[]; edges: VEdge[] },
    oldByRef: Map<T, VNode>,
    sitePaths: string[] = []
  ): Set<string> {
    cancelAnimationFrame(anim!)
    const used = new Set<string>()
    if (!motion || nodes.length === 0) {
      nodes = l.nodes
      edges = l.edges
      return used
    }
    const nFrom = new Map(nodes.map((n) => [n.path, n]))
    const eFrom = new Map(edges.map((e) => [e.id, e]))
    // Correspondence, the general rewrite-animation rule: a node that kept
    // BOTH its path and its tree didn't move — it stays and glides with the
    // layout. Otherwise find the tree wherever it stood before: a kept
    // subtree glides down from there to its place in the result (this is
    // the K rule's keep and triage's selected branch), and a duplicated ref
    // splits from one origin. Otherwise inherit the position of whatever
    // stood at your path — a joint grown by the rewrite emerges from the
    // redex site it replaced. Only then is a node genuinely new growth.
    const fromFor = (tn: VNode): VNode | undefined => {
      const byPath = nFrom.get(tn.path)
      if (byPath && byPath.tree === tn.tree) return byPath
      return oldByRef.get(tn.tree) ?? byPath
    }
    const nPairs = l.nodes.map((tn) => {
      const f = fromFor(tn)
      return { tn: { ...tn, fresh: !f }, f }
    })
    const newByPath = new Map(l.nodes.map((n) => [n.path, n]))
    const ePairs = l.edges.map((te) => {
      const cPath = te.trunk ? 'r' : te.id.replace('>', '')
      const pPath = te.trunk ? '' : te.id.split('>')[0]
      const child = newByPath.get(cPath)
      let f = eFrom.get(te.id)
      let fId = f ? te.id : null
      if (f && !te.trunk) {
        const oldChild = nFrom.get(cPath)
        const sameChild = oldChild && child && oldChild.tree === child.tree
        // The port rule: an edge whose parent survives OUTSIDE the rewritten
        // region is a stable port — the branch stays and receives the
        // arriving child (K's kept leaf lands on the waiting branch). An
        // edge whose parent is part of the RESULT re-wires: it travels with
        // its child instead (triage's selected branch and S's shared
        // argument bring their branches along).
        const inResult = sitePaths.some((sp) => pPath.startsWith(sp))
        if (!sameChild && inResult) {
          f = undefined
          fId = null
        }
      }
      if (!f && !te.trunk && child) {
        // the child moved here from elsewhere: bring its old incoming
        // branch along instead of growing a new one
        const old = oldByRef.get(child.tree)
        if (old && old.path !== 'r') {
          fId = `${old.path.slice(0, -1)}>${old.path.slice(-1)}`
          f = eFrom.get(fId)
          if (!f) fId = null
        }
      }
      if (fId) used.add(fId)
      // otherwise a NEW branch grows out of its parent endpoint
      return { te, f: f ?? { ...te, x2: te.x1, y2: te.y1, cx: te.x1, cy: te.y1 } }
    })
    const t0 = performance.now()
    const D = 550
    const frame = (now: number) => {
      const t = Math.min(1, (now - t0) / D)
      const k = ease(t)
      nodes = nPairs.map(({ tn, f }) =>
        f ? { ...tn, x: f.x + (tn.x - f.x) * k, y: f.y + (tn.y - f.y) * k } : tn
      )
      edges = ePairs.map(({ te, f }) => ({
        ...te,
        x1: f.x1 + (te.x1 - f.x1) * k,
        y1: f.y1 + (te.y1 - f.y1) * k,
        cx: f.cx + (te.cx - f.cx) * k,
        cy: f.cy + (te.cy - f.cy) * k,
        x2: f.x2 + (te.x2 - f.x2) * k,
        y2: f.y2 + (te.y2 - f.y2) * k
      }))
      if (t < 1) anim = requestAnimationFrame(frame)
    }
    anim = requestAnimationFrame(frame)
    return used
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

  function show(tree: T, opts: { spine?: Set<T>; allShrink?: boolean; sitePaths?: string[] } = {}) {
    const { spine, allShrink = false, sitePaths = [] } = opts
    const prevNodes = nodes
    const prevEdges = edges
    const l = layout(tree)
    const oldByRef = new Map<T, VNode>()
    for (const n of prevNodes) if (!oldByRef.has(n.tree)) oldByRef.set(n.tree, n)
    const usedEdges = animateTo(l, oldByRef, sitePaths)
    if (motion && styled && prevNodes.length > 0) {
      const survivors = new Set(l.nodes.map((n) => n.tree))
      const removed = prevNodes.filter((n) => !survivors.has(n.tree))
      const fresh: Debris[] = []
      const shrunk: Vanishing[] = []
      for (const n of removed) {
        const consumed = allShrink || (spine?.has(n.tree) ?? false)
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
            label: n.vname ?? undefined
          })
        } else {
          // consumed machinery dissolves where it stood — the shrinking ring
          // and core mark the reduction point while the result takes its place
          shrunk.push({
            id: debrisId++,
            x: n.x,
            y: n.y,
            tag: n.tag,
            tint: n.tint,
            tilt: n.tilt
          })
        }
      }
      if (fresh.length) {
        debris.push(...fresh)
        while (debris.length > 90) debris.shift()
        const ids = fresh.map((d) => d.id)
        setTimeout(() => {
          debris = debris.filter((d) => !ids.includes(d.id))
        }, 6400)
      }
      if (shrunk.length) {
        vanishing.push(...shrunk)
        const ids = shrunk.map((v) => v.id)
        setTimeout(() => {
          vanishing = vanishing.filter((v) => !ids.includes(v.id))
        }, 460)
      }
      // any branch the tween didn't carry forward dissolves where it stood —
      // whether its subtree fell, its machinery was consumed, or its id was
      // claimed by a re-wired edge travelling in with a different child
      const gone = prevEdges.filter((e) => !e.trunk && !usedEdges.has(e.id))
      if (gone.length) {
        const fading: GhostEdge[] = gone.map((e) => ({ id: debrisId++, d: edgeD(e), w: e.w }))
        ghosts.push(...fading)
        const ids = fading.map((g) => g.id)
        setTimeout(() => {
          ghosts = ghosts.filter((g) => !ids.includes(g.id))
        }, 500)
      }
    }
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
    err = ''
    editInvalid = false
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
      // the stem/fork pieces parse lazily so their construction rule fires visibly
      const lazyTop = PIECES.find((p) => p.expr === src)?.lazyTop ?? false
      folded = new Map()
      seen = new WeakSet()
      // pods are a HOST-defs behavior: the built-in tour's whole point is
      // showing the toy combinators' structure, so it keeps drawing them open
      cur = parseTree(src, activeDefs, {
        lazyTop,
        lazyAll: lazyParse,
        onDefSplice: podMode ? (node, name) => folded.set(node, name) : undefined
      })
      // the root is never a pod — a program that IS one atom draws in full
      folded.delete(cur)
      // everything visible at load is 'seen': later auto-folds only ever
      // apply to data a reduction newly exposes
      const markSeen = (t: T): void => {
        if (folded.has(t)) return
        seen.add(t)
        childrenOf(t).forEach(markSeen)
      }
      markSeen(cur)
      maybeResolve(cur)
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
    // (they shrink); anything else that vanishes was discarded (it falls or
    // fades) — concurrently with the kept trees gliding into the result
    const sites = (parallel ? readySites(cur) : [nextApplyToFire(cur)].filter((s) => s !== null)) as (T & {
      tag: 'apply'
    })[]
    const spine = new Set(sites.flatMap((s) => spineOf(s)))
    // every on-screen occurrence of a fired site: edges inside those regions
    // re-wire, travelling with their subtrees instead of holding their port
    const sitePaths = nodes.filter((n) => sites.some((s) => s === n.tree)).map((n) => n.path)
    if (parallel) {
      const s = stepParallel(cur)
      if (!s) {
        ruleMsg = restMsg(cur)
        atNormalForm = true
        return false
      }
      remember()
      cur = s.next
      stepCount++
      refreshFolds(cur)
      ruleMsg =
        s.fired.length === 1
          ? `1 redex fired (${s.fired[0]})`
          : `${s.fired.length} redexes fired in parallel (${s.fired.join(' ')})`
      show(cur, { spine, sitePaths })
      return true
    }
    const s = stepOnce(cur)
    if (!s) {
      ruleMsg = restMsg(cur)
      atNormalForm = true
      return false
    }
    remember()
    cur = s.next
    stepCount++
    refreshFolds(cur)
    ruleMsg = s.rule
    show(cur, { spine, sitePaths })
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

  // Engine macro-step: the local rules are stuck, but the head is a NAME the
  // host's evaluator knows — hand it the spine, splice the result back, and
  // show the whole thing as one step (the pattern dissolves, the result
  // grows, folds apply to whatever arrives).
  let engineBusy = false
  async function tryEngine(): Promise<boolean> {
    if (!engineStep || !cur || engineBusy) return false
    const s = stuckSpine(cur)
    if (!s) return false
    engineBusy = true
    ruleMsg = `${s.name} is opaque here — asking the engine…`
    try {
      const result = await engineStep(s.name, s.args)
      if (!result || !cur) {
        ruleMsg = `stuck: the engine couldn't reduce ${s.name} here (or the result is too large to draw)`
        return false
      }
      remember()
      // the spine (applies + the named head) is the consumed pattern
      const spine = new Set<T>()
      let f: T = s.site
      while (f.tag === 'apply') {
        spine.add(f)
        f = f.f
      }
      spine.add(f)
      const sitePaths = nodes.filter((n) => n.tree === s.site).map((n) => n.path)
      cur = replaceNode(cur, s.site, result)
      stepCount++
      refreshFolds(cur)
      atNormalForm = false
      ruleMsg = `engine step: ${s.name} applied by the real evaluator`
      show(cur, { spine, sitePaths })
      return true
    } finally {
      engineBusy = false
    }
  }

  function schedule() {
    clearTimeout(timer)
    timer = setTimeout(() => {
      if (!running) return
      void (async () => {
        const went = fire() || (await tryEngine())
        if (went) {
          schedule()
        } else if (autoCycle) {
          timer = setTimeout(nextExpr, Math.max(curStepMs * 2.2, 2000))
        } else {
          running = false
        }
      })()
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
    if (!fire()) void tryEngine()
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
  // keyboard drive. The arrows work no matter which control inside the widget
  // holds focus (so tabbing to a transport button doesn't strand them): ←/→
  // step the reduction, shift+←/→ hop between trees. Only the text edit box is
  // exempt — there the arrows move the caret and shift+arrows select. Space
  // runs/pauses, but only when the widget itself (not a button) is focused, so
  // it never hijacks a focused button's own activation.
  function onKey(e: KeyboardEvent) {
    const inEdit = (e.target as HTMLElement).tagName === 'INPUT'
    if (!inEdit && (e.key === 'ArrowLeft' || e.key === 'ArrowRight')) {
      e.preventDefault()
      if (e.shiftKey) {
        if (!minimal) cyclePreset(e.key === 'ArrowRight' ? 1 : -1)
      } else if (e.key === 'ArrowLeft') back()
      else step()
      return
    }
    if (e.key === ' ' && e.target === e.currentTarget) {
      e.preventDefault()
      toggleRun()
    }
  }

  onMount(() => {
    api?.({
      setProgram: (expr, newDefs) => {
        if (newDefs) hostDefs = newDefs
        running = false
        autoCycle = false
        clearTimeout(timer)
        load(expr)
      }
    })
    load(input, { keepAuto: autoCycle })
  })
  onDestroy(() => clearTimeout(timer))
</script>

<svelte:document
  onpointerdown={(e) => {
    const t = e.target as Node
    if (cassetteOpen && cassetteWrapEl && !cassetteWrapEl.contains(t)) cassetteOpen = false
    if (showSelectors && selectorsWrapEl && !selectorsWrapEl.contains(t)) showSelectors = false
  }}
/>

<!-- transport + selector icons (fill = triangles, .stroke = outlined) -->
{#snippet icoPrev()}<svg class="ico" viewBox="0 0 24 24"><rect x="6" y="6" width="2.2" height="12" rx="0.6" /><path d="M18 6 L10 12 L18 18 Z" /></svg>{/snippet}
{#snippet icoBack()}<svg class="ico" viewBox="0 0 24 24"><path d="M15.5 6 L8 12 L15.5 18 Z" /></svg>{/snippet}
{#snippet icoPlay()}<svg class="ico" viewBox="0 0 24 24"><path d="M8 5.5 L18.5 12 L8 18.5 Z" /></svg>{/snippet}
{#snippet icoPause()}<svg class="ico" viewBox="0 0 24 24"><rect x="7.5" y="5.5" width="3.2" height="13" rx="0.8" /><rect x="13.3" y="5.5" width="3.2" height="13" rx="0.8" /></svg>{/snippet}
{#snippet icoFwd()}<svg class="ico" viewBox="0 0 24 24"><path d="M8.5 6 L16 12 L8.5 18 Z" /></svg>{/snippet}
{#snippet icoNext()}<svg class="ico" viewBox="0 0 24 24"><path d="M6 6 L14 12 L6 18 Z" /><rect x="15.8" y="6" width="2.2" height="12" rx="0.6" /></svg>{/snippet}
{#snippet icoReset()}<svg class="ico stroke" viewBox="0 0 24 24"><polyline points="3 5 3 10.5 8.5 10.5" /><path d="M5.2 15.5 a8 8 0 1 0 1.9 -8.4 L3 10.5" /></svg>{/snippet}
{#snippet icoParallel()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M4 8.5 h9" /><path d="M10.5 5.5 L13.5 8.5 L10.5 11.5" /><path d="M4 15.5 h9" /><path d="M10.5 12.5 L13.5 15.5 L10.5 18.5" /></svg>{/snippet}
{#snippet icoLeaf()}<svg class="ico" viewBox="0 0 24 24"><path d="M12 3.5 C6.5 7 6 15 11.6 20.6 C17.5 15 17.5 7 12 3.5 Z" /></svg>{/snippet}
{#snippet icoMotion()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M9.6 4.6 A2 2 0 1 1 11 8 H2.5 M12.6 19.4 A2 2 0 1 0 14 16 H2.5 M17.7 7.7 A2.5 2.5 0 1 1 19.5 12 H2.5" /></svg>{/snippet}
{#snippet icoLabels()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M20.4 13.4 l-7 7 a2 2 0 0 1 -2.8 0 L2.5 12 V2.5 h9.5 l8.4 8.4 a2 2 0 0 1 0 2.5 Z" /><circle cx="7.3" cy="7.3" r="1.15" fill="currentColor" stroke="none" /></svg>{/snippet}

<!-- the four view selectors — shared by the lab's inline row and the ambient
     ⚘ pop-out; each is an on/off icon toggle -->
{#snippet selectorButtons()}
  <button class="mpi sel" class:on={parallel} aria-pressed={parallel} title="parallel — fire every ready redex per step" aria-label="parallel reduction"
    onclick={() => { parallel = !parallel; if (cur) show(cur) }}>{@render icoParallel()}</button>
  <button class="mpi sel" class:on={styled} aria-pressed={styled} title="nature — styling vs plain diagram" aria-label="nature styling"
    onclick={() => { styled = !styled; if (cur) show(cur) }}>{@render icoLeaf()}</button>
  <button class="mpi sel" class:on={motion} aria-pressed={motion} title="motion — glide + falling leaves vs instant" aria-label="motion"
    onclick={() => (motion = !motion)}>{@render icoMotion()}</button>
  <button class="mpi sel" class:on={labels} aria-pressed={labels} title="labels — name badges on recognized subtrees" aria-label="labels"
    onclick={() => { labels = !labels; if (cur) show(cur) }}>{@render icoLabels()}</button>
{/snippet}

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
  onmousedown={(e) => {
    // clicking a control shouldn't leave it focused (no stuck selection ring);
    // keeps keyboard focus on the widget so the arrow keys keep working
    if ((e.target as HTMLElement).closest('button')) e.preventDefault()
  }}
>
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
          <g class="debris" class:fruit={!!d.label} style="--dy: {d.dy}px; --dx: {d.dx}px">
            <g class="dspin" style="--rot: {d.rot}deg">
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
        <path class="branch ghost" d={g.d} style="stroke-width:{g.w}" />
      {/each}
      <!-- consumed pattern pieces dissolve in place at the reduction point -->
      {#each vanishing as v (v.id)}
        <g style="transform: translate({v.x}px, {v.y}px)">
          <g class="vshrink">
            {#if v.tag === 'leaf'}
              <path
                class="leafshape"
                d="M0,-8 C5.2,-5 5.2,2.5 0,8 C-5.2,2.5 -5.2,-5 0,-8 Z"
                style="fill:{v.tint}; transform: rotate({v.tilt}deg)"
              />
            {:else if v.tag === 'var'}
              <circle r="9" class="fruitbody" />
            {:else if v.tag === 'apply'}
              <!-- ring AND core, at live size — the whole apply marker
                   collapses into the joint that replaces it -->
              <circle r="8.5" class="bud-ring" />
              <circle r="3" class="bud-core" />
            {:else}
              <circle r="3.6" class="knot" />
            {/if}
          </g>
        </g>
      {/each}

      <g class="tree" class:swaying={styled && motion}>
        {#each edges as e (e.id)}
          <path d={edgeD(e)} class="branch" class:trunk={e.trunk} style="stroke-width:{e.w}" />
        {/each}
        {#each nodes as n (n.path)}
          <g class="node" style="transform: translate({n.x}px, {n.y}px)">
            <g class="inner" class:growing={motion && styled && n.fresh !== false} style="--gd: {n.depth * 55}ms">
              {#if n.pod != null}
                <!-- a seed pod: a real subtree folded shut. Reduction computes
                     THROUGH it (results re-fold on arrival); a click reveals
                     one budgeted level, like the buffer's inline unfolds.
                     Green (grown from real structure), unlike the orange
                     fruit (free names with no insides at all). -->
                <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
                <g class="pod" onclick={() => openPod(n.tree)}>
                  <title>unfold one level</title>
                  {#if styled}<path class="fstalk" d="M0,-9 Q1,-13 3.5,-14.5" />{/if}
                  <circle r="10" class="podbody" />
                  <path class="podseam" d="M0,-6.5 Q2.8,0 0,6.5" />
                  {#if n.pod.length <= 3}
                    <text
                      class="vname"
                      y="0.5"
                      style="font-size: {n.pod.length === 1 ? 10 : n.pod.length === 2 ? 8.5 : 7}px"
                      >{n.pod}</text
                    >
                  {:else}
                    <text class="vname above" y="-17">{n.pod}</text>
                  {/if}
                </g>
              {:else if n.tag === 'apply'}
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
                <!-- eased: the mark settles onto its node after the glide
                     lands, instead of standing pre-planted at the endpoint -->
                <g class="firemark" class:eased={motion} class:next={n.next || parallel}>
                  <circle r="7.5" class="fire-ring" />
                  <text class="fire-letter" y="0.5">{RULE_GLYPH[n.ready]}</text>
                </g>
              {/if}
              {#if n.next && styled}
                <g class="blossom" class:eased={motion}>
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

    {#if variant === 'lab' && !minimal}
      <!-- minimal embeddings (the playground panel) show no readout text at
           all — the term input below is the only prose -->
      <div class="readout">
        <div class="rline">
          <span class="expr">{input}</span>
          <span class="rule" class:nf={atNormalForm}>{ruleMsg || 'press ▶ (or →) to fire the next reduction'}</span>
          <span class="count">{stepCount} step{stepCount === 1 ? '' : 's'}{parallel ? ' · parallel' : ''}</span>
        </div>
        {#if cur}
          <div class="forms"><span class="flabel">named</span><code>{pretty(cur, { names: true, defs: activeDefs })}</code></div>
          <div class="forms"><span class="flabel">raw</span><code>{pretty(cur, { names: false })}</code></div>
        {/if}
      </div>
    {/if}
  {/if}

  <!-- the edit box, sitting on top of the transport (Enter loads it), with an
       (i) that explains what names you can type -->
  <div class="editrow">
    <input
      class="editbox"
      class:invalid={editInvalid}
      type="text"
      bind:value={input}
      oninput={liveParse}
      onkeydown={onEditKey}
      onfocus={pauseForEdit}
      spellcheck="false"
      autocomplete="off"
      aria-label="tree-calculus expression — parses as you type"
    />
    <span class="info">
      <button type="button" class="info-btn" aria-label="what can I type here?">i</button>
      <span class="info-pop" role="tooltip">
        {#if namesHint}
          Edits parse as you type; <kbd>Enter</kbd> (re)starts the tree. {namesHint}
        {:else}
          Edits parse as you type; <kbd>Enter</kbd> (re)starts the tree. Combinators:
          <code>t</code> (△), <code>K</code>, <code>S</code>, <code>not</code>, <code>and</code>,
          <code>or</code>, <code>add</code>, and plain numbers. Any other name — <code>x</code>,
          <code>y</code>, <code>f</code> … — is a free variable: a named leaf the reductions carry
          along symbolically.
        {/if}
      </span>
    </span>
  </div>
  {#if activeTip && !minimal}<p class="tip-line">{activeTip}</p>{/if}

  <!-- the bottom bar: play/pause dead-centre with prev/back and fwd/next/reset
       clustered tight around it (thin buttons, no circles); the cassette sits
       at the far left and the ⚘ view toggle at the far right, both absolute so
       they never shift the centring -->
  <div class="bottombar">
    <div class="tside tleft">
      {#if !minimal}
        <button class="tbtn" onclick={() => cyclePreset(-1)} title="previous tree (shift+←)" aria-label="previous tree">{@render icoPrev()}</button>
      {/if}
      <button class="tbtn" onclick={back} disabled={historyLen === 0} title="step back (←)" aria-label="step back">{@render icoBack()}</button>
    </div>

    <button class="tbtn play" onclick={toggleRun} title="run / pause (space)" aria-label={running ? 'pause' : 'play'}>{#if running}{@render icoPause()}{:else}{@render icoPlay()}{/if}</button>

    <div class="tside tright">
      <button class="tbtn" onclick={step} title="step forward (→)" aria-label="step forward">{@render icoFwd()}</button>
      {#if !minimal}
        <button class="tbtn" onclick={() => cyclePreset(1)} title="next tree (shift+→)" aria-label="next tree">{@render icoNext()}</button>
      {/if}
      <button class="tbtn" onclick={reset} title="reset to the start" aria-label="reset">{@render icoReset()}</button>
    </div>

    {#if variant === 'ambient'}
      <!-- cassette (far left): the chip toggles a VERTICAL tape that expands
           upward — leaf △ at the bottom, + at the top. Press anywhere on it and
           the selector jumps there, then scrubs. -->
      <div class="cassette-wrap corner-left" bind:this={cassetteWrapEl}>
        <button
          type="button"
          class="cassette-chip"
          class:open={cassetteOpen}
          onclick={() => (cassetteOpen ? (cassetteOpen = false) : openCassette())}
          aria-haspopup="true"
          aria-expanded={cassetteOpen}
          aria-label={`tree pieces — now: ${PIECES[selectedIdx].tip}`}
          title={`${PIECES[selectedIdx].tip} — pick a piece`}
        >{PIECES[selectedIdx].sym}{#if PIECES[selectedIdx].sub}<sub>{PIECES[selectedIdx].sub}</sub>{/if}</button>
        {#if cassetteOpen}
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <div
            class="cassette-tape"
            class:grabbing={selDragging}
            bind:this={tapeEl}
            role="slider"
            aria-orientation="vertical"
            tabindex="-1"
            aria-label="tree pieces"
            aria-valuemin="0"
            aria-valuemax={PIECES.length - 1}
            aria-valuenow={selectedIdx}
            aria-valuetext={PIECES[selectedIdx].tip}
            style="width: {CELLW + 6}px"
            out:fade={{ duration: 150 }}
            onpointerdown={tapeDown}
            onpointermove={tapeMove}
            onpointerup={tapeUp}
            onpointercancel={tapeUp}
          >
            {#each PIECES as it, i}
              <div class="tcell" class:on={i === selectedIdx} style="height: {CELLH}px; --ci: {i}" title={it.tip}>{it.sym}{#if it.sub}<sub>{it.sub}</sub>{/if}</div>
            {/each}
            <div
              class="selector"
              style="width: {CELLW}px; height: {CELLH}px; transform: translateY({-selY}px)"
              aria-hidden="true"
              in:selectorIn={{ duration: 300 }}
            ></div>
          </div>
        {/if}
      </div>

      <!-- the ⚘ view toggle (far right): reveals the selectors, popping upward -->
      <div class="selectors-wrap corner-right" bind:this={selectorsWrapEl}>
        {#if showSelectors}
          <div class="selectors-pop" transition:scale={{ duration: 200, start: 0.8, opacity: 0, easing: backOut }}>{@render selectorButtons()}</div>
        {/if}
        <button
          class="tree-toggle"
          onclick={() => (showSelectors = !showSelectors)}
          aria-expanded={showSelectors}
          aria-label={showSelectors ? 'hide view options' : 'view options'}
          title={showSelectors ? 'hide view options' : 'view options'}
        >{showSelectors ? '×' : '⚘'}</button>
      </div>
    {:else}
      <div class="selectors corner-right">{@render selectorButtons()}</div>
    {/if}
  </div>
</div>

<style>
  .vis {
    position: relative;
    width: 100%;
    outline: none;
    border-radius: var(--radius, 12px);
  }
  /* the widget grabs keyboard focus for space/arrow drive, but stays visually
     quiet — no selection ring around the whole thing */
  .vis:focus,
  .vis:focus-visible {
    outline: none;
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

  /* ---- controls: a bottom bar with the edit box on top of it ---- */
  /* the grid keeps play/pause at the widget's exact centre (equal 1fr flanks);
     the transport clusters tight around it, the cassette / ⚘ sit absolute in
     the corners so they don't disturb the centring */
  .bottombar {
    position: relative;
    display: grid;
    grid-template-columns: 1fr auto 1fr;
    align-items: center;
    gap: 0.15rem;
    margin-top: 0.3rem;
    min-height: 30px;
  }
  .tside {
    display: flex;
    align-items: center;
    gap: 0.1rem;
  }
  .tleft { justify-self: end; } /* hug the right, next to play */
  .tright { justify-self: start; } /* hug the left, next to play */
  .corner-left {
    position: absolute;
    left: 0;
    top: 50%;
    transform: translateY(-50%);
  }
  .corner-right {
    position: absolute;
    right: 0;
    top: 50%;
    transform: translateY(-50%);
    display: flex;
    align-items: center;
    gap: 0.2rem;
  }
  /* thin transport buttons — bare icons, no circle */
  .tbtn {
    flex: none;
    border: none;
    background: none;
    color: var(--fg-muted);
    display: inline-grid;
    place-items: center;
    padding: 0.18em;
    border-radius: 6px;
    cursor: pointer;
    transition: opacity 0.18s ease, color 0.18s ease, background 0.18s ease;
  }
  .vis:not(.lab) .tbtn { opacity: 0.5; }
  .vis:not(.lab):hover .tbtn { opacity: 1; }
  .tbtn:hover { color: var(--accent); background: color-mix(in oklab, var(--accent) 12%, transparent); }
  .tbtn:disabled { opacity: 0.28; cursor: default; color: var(--fg-muted); background: none; }
  .vis:not(.lab):hover .tbtn:disabled { opacity: 0.32; }
  .tbtn .ico { width: 18px; height: 18px; display: block; fill: currentColor; }
  .tbtn .ico.stroke { fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round; stroke-linejoin: round; }
  /* play/pause is the primary — larger and greener */
  .tbtn.play { color: var(--g2); padding: 0.1em; }
  .tbtn.play:hover { color: var(--accent); }
  .tbtn.play .ico { width: 24px; height: 24px; }
  /* the edit box: bare editable text, centred, sitting above the transport */
  .editrow {
    position: relative;
    display: flex;
    justify-content: center;
    align-items: center;
    width: 100%;
    min-width: 0;
  }
  .editbox {
    width: 100%;
    max-width: min(24rem, calc(100% - 3rem));
    min-width: 0;
    background: none;
    border: none;
    outline: none;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    text-align: center;
    padding: 0.14em 0.5em;
    border-radius: 7px;
    caret-color: var(--accent);
    /* a quiet affordance that this is editable: a faint dashed underline that
       firms up to the accent on hover / focus */
    border-bottom: 1px dashed color-mix(in oklab, var(--fg-faint) 50%, transparent);
    transition: border-color 0.15s ease, background 0.15s ease;
  }
  .editbox:hover {
    border-bottom-color: var(--fg-muted);
    background: color-mix(in oklab, var(--bg-code) 45%, transparent);
  }
  .editbox:focus {
    border-bottom: 1px solid var(--accent);
    background: color-mix(in oklab, var(--bg-code) 60%, transparent);
  }
  /* mid-edit gibberish: the tree holds still, only the underline says so */
  .editbox.invalid {
    border-bottom-color: color-mix(in oklab, var(--err) 65%, transparent);
  }
  .editbox::selection { background: color-mix(in oklab, var(--accent) 28%, transparent); }
  /* the (i) that explains what names you can type, at the row's right edge */
  .info {
    position: absolute;
    right: 0.1rem;
    top: 50%;
    transform: translateY(-50%);
    display: inline-flex;
  }
  .info-btn {
    width: 16px;
    height: 16px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: none;
    color: var(--fg-faint);
    font-family: var(--font-body);
    font-size: 10px;
    font-style: italic;
    font-weight: 700;
    line-height: 1;
    display: inline-grid;
    place-items: center;
    cursor: help;
    padding: 0;
    transition: color 0.15s ease, border-color 0.15s ease;
  }
  .info-btn:hover,
  .info-btn:focus-visible { color: var(--accent); border-color: var(--accent); outline: none; }
  .info-pop {
    position: absolute;
    bottom: calc(100% + 9px);
    right: -0.35rem;
    width: 250px;
    padding: 0.55rem 0.7rem;
    border-radius: 9px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    box-shadow: 0 12px 28px -14px color-mix(in oklab, var(--fg) 42%, transparent);
    color: var(--fg-muted);
    font-family: var(--font-body);
    font-size: 0.72rem;
    line-height: 1.5;
    text-align: left;
    z-index: 50;
    opacity: 0;
    visibility: hidden;
    transform: translateY(4px);
    transition: opacity 0.15s ease, transform 0.15s ease, visibility 0.15s;
    pointer-events: none;
  }
  .info:hover .info-pop,
  .info-btn:focus-visible + .info-pop {
    opacity: 1;
    visibility: visible;
    transform: none;
  }
  .info-pop code {
    background: color-mix(in oklab, var(--fg) 9%, transparent);
    padding: 0 0.25em;
    border-radius: 3px;
    font-size: 0.92em;
  }
  .info-pop kbd {
    background: var(--bg-code);
    border: 1px solid var(--border-strong);
    border-radius: 4px;
    padding: 0 0.3em;
    font-size: 0.88em;
    font-family: var(--font-mono);
  }
  /* the lab's inline selector row (ambient uses the ⚘ pop-out instead) */
  .selectors {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.3rem;
  }
  /* media + selector icon buttons — circular, dimmed like the corner buttons */
  .mpi {
    flex: none;
    width: 30px;
    height: 30px;
    display: inline-grid;
    place-items: center;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg-muted);
    border-radius: 50%;
    padding: 0;
    cursor: pointer;
    transition: opacity 0.18s ease, border-color 0.18s ease, color 0.18s ease, background 0.18s ease;
  }
  /* ambient greys the whole control set out until the pointer enters the widget
     (the lab instrument stays full); a direct hover always lifts a button */
  .vis:not(.lab) .mpi { opacity: 0.45; }
  .vis:not(.lab):hover .mpi { opacity: 1; }
  .mpi:hover { opacity: 1; border-color: var(--accent); color: var(--accent); }
  .mpi:disabled { opacity: 0.22; cursor: default; border-color: var(--border-strong); color: var(--fg-muted); }
  .vis:not(.lab):hover .mpi:disabled { opacity: 0.3; }
  .mpi .ico { width: 16px; height: 16px; display: block; fill: currentColor; }
  .mpi .ico.stroke {
    fill: none;
    stroke: currentColor;
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
  /* play/pause is the primary — greener than the rest */
  .mpi.play { color: var(--g2); }
  .mpi.play:hover { color: var(--accent); }
  /* a selector that is ON lights up like the old solid toggle pill */
  .mpi.sel.on {
    border-color: var(--g2);
    color: var(--g2);
    background: color-mix(in oklab, var(--g1) 12%, var(--bg-elev));
  }

  .tip-line { color: var(--fg-faint); font-size: 0.8rem; margin: 0.1rem 0 0; font-style: italic; }

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
  /* when the layout is animating, a newly-ready mark waits for its node to
     land (the glide is ~settled by 0.3s) and then grows in on it — never
     pre-planted at the endpoint while the tree is still travelling */
  .firemark.eased,
  .blossom.eased { animation: markin 0.26s ease-out 0.34s backwards; transform-origin: 0 0; }
  .firemark.eased.next {
    animation:
      markin 0.26s ease-out 0.34s backwards,
      firepulse 1s ease-in-out 0.75s infinite;
  }
  @keyframes markin {
    from {
      transform: scale(0.3);
      opacity: 0;
    }
  }
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

  /* a folded defs splice is a POD: green (real structure grown inside, unlike
     the orange fruit's opaque nothing), a seam hinting it opens. Click opens. */
  .pod { cursor: pointer; }
  .podbody {
    fill: #79ae74;
    stroke: color-mix(in oklab, #2e5e33 60%, transparent);
    stroke-width: 0.9;
  }
  .pod:hover .podbody {
    fill: #8abd85;
  }
  .plain .podbody { fill: #8fbb8a; stroke: var(--fg-faint); }
  .podseam {
    stroke: rgba(255, 255, 255, 0.55);
    stroke-width: 1.1;
    fill: none;
    stroke-linecap: round;
    pointer-events: none;
  }
  .pod .vname {
    fill: #1c3a20;
  }
  .pod .vname.above {
    fill: var(--g2);
  }

  /* ---- pruned leaves: fall (1s), rest (~2.4s), let go (3.4s) ---- */
  .debris {
    animation:
      dfall 1s cubic-bezier(0.45, 0.05, 0.75, 0.6) forwards,
      dfade 3.4s linear forwards;
    animation-delay: 0s, 2.4s;
  }
  /* fruit drops quicker and dissolves mid-air — it never reaches the ground */
  .debris.fruit {
    animation:
      dfall 0.85s cubic-bezier(0.5, 0.05, 0.8, 0.6) forwards,
      dfade 0.45s ease-out forwards;
    animation-delay: 0s, 0.22s;
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
  /* consumed pattern pieces dissolve in place — collapsing where they stood
     keeps the eye on the surviving trees, which carry all the real motion */
  .vshrink {
    animation: vshrink 0.42s ease-in forwards;
  }
  @keyframes vshrink {
    to {
      transform: scale(0);
      opacity: 0;
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

  /* ---- the cassette (ambient): a chip in the bar; the tape pops up above ---- */
  /* position comes from .corner-left (absolute); being positioned makes it the
     containing block for the upward tape pop-out */
  .cassette-wrap {
    display: inline-flex;
  }
  /* the collapsed chip wears the current piece's symbol */
  .cassette-chip {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--g2);
    font-family: var(--font-mono);
    font-size: 1rem;
    line-height: 1;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    cursor: pointer;
    opacity: 0.5;
    transition: opacity 0.2s ease, border-color 0.2s ease, color 0.2s ease, transform 0.2s ease;
  }
  .vis:hover .cassette-chip { opacity: 1; }
  .cassette-chip:hover { border-color: var(--accent); color: var(--accent); transform: scale(1.06); }
  .cassette-chip sub { font-size: 0.6em; margin-left: 0.03em; }
  /* the expanded tape: a stationary VERTICAL column of every piece + a draggable
     selector, floating just ABOVE the chip and expanding upward — leaf △ at the
     bottom, + at the top. The whole tape is the slider track — press anywhere
     and the selector jumps there. */
  .cassette-tape {
    position: absolute;
    bottom: calc(100% + 8px);
    left: 0;
    z-index: 30;
    display: flex;
    flex-direction: column-reverse; /* index 0 (leaf △) at the bottom, + on top */
    align-items: stretch;
    box-sizing: border-box;
    padding: 3px;
    border-radius: 10px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    box-shadow: 0 12px 26px -14px color-mix(in oklab, var(--fg) 48%, transparent);
    animation: tapeReveal 0.2s ease;
    cursor: pointer;
    touch-action: none;
    user-select: none;
    -webkit-user-select: none;
  }
  .cassette-tape.grabbing { cursor: grabbing; }
  @keyframes tapeReveal { from { opacity: 0; transform: translateY(8px); } }
  /* the cells stagger in bottom-to-top as the tape unfurls up out of the chip */
  @keyframes cellIn { from { opacity: 0; transform: translateY(6px) scale(0.6); } }
  .tcell {
    flex: none;
    width: 100%;
    color: var(--fg-muted);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    line-height: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    pointer-events: none; /* the tape handles all the pointer work */
    animation: cellIn 0.3s calc(var(--ci, 0) * 24ms) both cubic-bezier(0.34, 1.6, 0.5, 1);
  }
  .tcell sub { font-size: 0.62em; margin-left: 0.02em; }
  .tcell.on { color: var(--g2); font-weight: 650; transition: color 0.15s ease; }
  /* the selector thumb — purely visual; the tape drives it. It rests on the
     bottom cell and rides UP (translateY) to the selected piece. */
  .selector {
    position: absolute;
    left: 3px;
    bottom: 3px;
    border-radius: 7px;
    border: 1.5px solid var(--g2);
    background: color-mix(in oklab, var(--g1) 16%, transparent);
    box-shadow: 0 1px 6px -2px color-mix(in oklab, var(--g2) 70%, transparent);
    pointer-events: none;
    /* sticky + bouncy: it snaps to a cell and rides there with an overshoot */
    transition: transform 0.24s cubic-bezier(0.34, 1.7, 0.5, 1);
  }

  /* ---- the tree toggle (⚘) + its view-selector pop-out, in the bar ---- */
  /* the wrap holds only the toggle in flow (the pop floats above, absolute), so
     revealing the selectors never nudges the toggle sideways */
  .selectors-wrap {
    display: inline-flex;
  }
  .selectors-pop {
    position: absolute;
    bottom: calc(100% + 0.34rem);
    right: 0;
    transform-origin: bottom right; /* grows out of the toggle */
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
    padding: 0.32rem;
    border-radius: 999px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    box-shadow: 0 10px 24px -16px color-mix(in oklab, var(--fg) 45%, transparent);
  }
  .tree-toggle {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--g2);
    font-size: 1rem;
    line-height: 1;
    display: inline-grid;
    place-items: center;
    padding: 0;
    cursor: pointer;
    opacity: 0.5;
    transition: opacity 0.2s ease, border-color 0.2s ease, color 0.2s ease, transform 0.2s ease;
  }
  .vis:hover .tree-toggle { opacity: 1; }
  .tree-toggle:hover { border-color: var(--accent); color: var(--accent); transform: rotate(20deg); }
</style>
