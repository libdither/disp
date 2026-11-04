<script lang="ts">
  // The tree drawing: a canvas with pan, zoom and fit that glides between
  // scenes. Nodes keep identity by path and by subtree id, so a kept subtree
  // visibly travels to its new place; what a step removes fades out on its way
  // to where the step sends it.
  import { onMount, untrack } from 'svelte'
  import { theme } from '$lib/theme.svelte'
  import { K_LEAF, K_STEM, K_FORK, K_APP, K_VAR, K_POD, K_LAM, type Scene, type VNode } from './layout.ts'
  import { R_K, R_S, R_F, RULE_GLYPH, type Id } from './store.ts'

  /// how the tree is drawn: bold is the original, the rest are calmer
  export type TreeLook = 'bold' | 'quiet' | 'garden' | 'moss' | 'ink'

  export interface CanvasApi {
    fit: () => void
    zoom: (factor: number) => void
    focus: (key: string) => void
  }

  interface Props {
    scene: Scene | null
    /// tween length for the next scene change; 0 snaps
    animMs?: number
    /// labels the editor's cursor is on: their pods glow
    glow?: string | null
    /// removed nodes fade out between scenes instead of vanishing
    falling?: boolean
    /// the colour a variable or its binder is drawn in
    varColor?: (name: string) => string
    look?: TreeLook
    onHover?: (n: VNode | null) => void
    onClick?: (n: VNode, e: MouseEvent) => void
    api?: (a: CanvasApi) => void
  }
  let { scene, animMs = 0, glow = null, falling = false, varColor = () => '#1971c2', look = 'bold', onHover, onClick, api }: Props = $props()

  let host: HTMLDivElement
  let cv: HTMLCanvasElement
  let ctx: CanvasRenderingContext2D
  let w = 0
  let h = 0
  let dpr = 1

  // camera: screen = world * k + (x, y)
  const cam = { x: 0, y: 0, k: 1 }
  const target = { x: 0, y: 0, k: 1 }
  let follow = true

  let shown: Scene | null = null
  let fromX = new Float64Array(0)
  let fromY = new Float64Array(0)
  let curX = new Float64Array(0)
  let curY = new Float64Array(0)
  let fresh = new Uint8Array(0)
  let t0 = 0
  let dur = 0
  let hover: VNode | null = null
  let hoverId: Id = -1

  /// a node the last step removed, fading on its way to `to` (a new node's index), in place, or dropping
  type Ghost = { n: VNode; x: number; y: number; to: number; drop: boolean; parent: number }
  let ghosts: Ghost[] = []
  /// every alpha below is scaled by this, so a ghost or a new node can be drawn faded with the usual helpers
  let fade = 1
  let raf = 0
  let lastFrame = 0

  const tiltCache = new Map<string, number>()
  function tiltOf(key: string): number {
    let t = tiltCache.get(key)
    if (t === undefined) {
      let h = 7
      for (let i = 0; i < key.length; i++) h = (h * 31 + key.charCodeAt(i)) | 0
      t = (((h >>> 0) % 70) - 35) * (Math.PI / 180)
      if (tiltCache.size > 20000) tiltCache.clear()
      tiltCache.set(key, t)
    }
    return t
  }

  // ---- colours: resolve the theme's CSS variables to values canvas accepts --
  let C: Record<string, string> = {}
  function readColors() {
    const probe = document.createElement('span')
    host.appendChild(probe)
    const get = (v: string) => {
      probe.style.color = v.startsWith('--') ? `var(${v})` : v
      return getComputedStyle(probe).color
    }
    C = {
      edge: get('--fg-faint'),
      fg: get('--fg'),
      muted: get('--fg-muted'),
      accent: get('--accent'),
      ok: get('--ok'),
      err: get('--err'),
      warn: get('--warn'),
      leaf: get('--accent'),
      num: get('--hl-number'),
      str: get('--hl-string'),
      name: get('--hl-fn'),
      kw: get('--hl-kw'),
      atom: get('--hl-atom'),
      bg: get('--bg-code'),
      panel: get('--bg-elev'),
      border: get('--border-strong'),
      // the original tree view's palette, so moss matches it in both themes
      bark: get('--bark'),
      g2: get('--g2'),
      g4: get('--g4'),
      g4soft: get('color-mix(in oklab, var(--g4) 22%, var(--bg-code))')
    }
    probe.remove()
    requestDraw()
  }

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']
  const ruleColor = (r: number) => (look === 'moss' ? flower().deep : r === R_K ? C.warn : r === R_S ? C.kw : r === R_F ? C.err : C.accent)
  /// garden and its all-green twin
  const planty = $derived(look === 'garden' || look === 'moss')
  const combColor = (op: string | null) => (look === 'moss' ? garden().step : op === 'S' ? C.kw : op === 'K' ? C.warn : op === 'I' ? C.ok : C.err)
  const podColor = (k: VNode['podKind']) =>
    k === 'nat' ? C.num : k === 'str' ? C.str : k === 'name' ? C.name : k === 'same' ? C.edge : C.muted

  const textW = new Map<string, number>()
  function podWidth(text: string): number {
    const shown = text.length > 28 ? text.slice(0, 27) + '…' : text
    let tw = textW.get(shown)
    if (tw === undefined) {
      ctx.font = "11px 'JetBrains Mono Variable', monospace"
      tw = ctx.measureText(shown).width
      textW.set(shown, tw)
    }
    return tw + 12
  }

  /// A variable stays readable when the view zooms out: it grows as everything else shrinks.
  const pillScale = (k: number) => Math.max(1, Math.min(1.4, 0.55 / k))

  function drawPill(x: number, y: number, text: string, col: string, k: number, binder = false, lit = true) {
    const s = pillScale(k)
    const shown = text.length > 16 ? text.slice(0, 15) + '…' : text
    if (look === 'ink') {
      // a name written in the margin: italic, with the lines knocked out behind it
      ctx.font = `italic 600 ${14 * s}px Georgia, 'Times New Roman', serif`
      const tw = ctx.measureText(shown).width
      ctx.fillStyle = C.bg
      ctx.fillRect(x - tw / 2 - 3, y - 9 * s, tw + 6, 18 * s)
      if (binder) {
        ctx.strokeStyle = col
        ctx.lineWidth = 1
        ctx.beginPath()
        ctx.moveTo(x - tw / 2 - 2, y + 8 * s)
        ctx.lineTo(x + tw / 2 + 2, y + 8 * s)
        ctx.stroke()
      }
      if (k * s > 0.2) {
        ctx.fillStyle = col
        ctx.fillText(shown, x, y + 0.5)
      }
      return
    }
    ctx.font = `${look === 'moss' ? 500 : 700} ${12 * s}px 'JetBrains Mono Variable', monospace`
    const pw = Math.max(22 * s, ctx.measureText(shown).width + 14 * s)
    const tint = look === 'quiet' || look === 'moss' || (planty && !lit)
    if (look === 'moss' && !lit) col = garden().ink
    ctx.beginPath()
    ctx.roundRect(x - pw / 2, y - 11 * s, pw, 22 * s, 11 * s)
    if (tint) {
      ctx.fillStyle = C.bg
      ctx.fill()
      ctx.globalAlpha = fade * (0.16)
      ctx.fillStyle = col
      ctx.fill()
      ctx.globalAlpha = fade
      ctx.strokeStyle = col
      ctx.lineWidth = 1.3
      ctx.setLineDash(binder ? [3, 2] : [])
      if (planty && !lit) ctx.globalAlpha = fade * (0.5)
      ctx.stroke()
      ctx.globalAlpha = fade
      ctx.setLineDash([])
    } else {
      ctx.fillStyle = col
      ctx.strokeStyle = C.bg
      ctx.lineWidth = 2
      ctx.fill()
      ctx.stroke()
      if (planty) {
        // a berry's shine
        ctx.globalAlpha = fade * (0.28)
        ctx.fillStyle = '#fff'
        ctx.beginPath()
        ctx.roundRect(x - pw / 2 + 4 * s, y - 9 * s, pw - 8 * s, 6 * s, 3 * s)
        ctx.fill()
        ctx.globalAlpha = fade
      }
    }
    if (k * s > 0.2) {
      ctx.fillStyle = tint ? col : '#fff'
      ctx.fillText(shown, x, y + 0.5)
    }
  }

  function fitTarget(s: Scene) {
    const pad = 34
    const sw = Math.max(1, s.maxX - s.minX)
    const sh = Math.max(1, s.maxY)
    const k = Math.min(1.5, (w - pad * 2) / sw, (h - pad * 2 - (s.titles.length ? 18 : 0)) / sh)
    target.k = Math.max(0.02, k)
    target.x = w / 2 - ((s.minX + s.maxX) / 2) * target.k
    const contentH = sh * target.k
    target.y = Math.max(pad + (s.titles.length ? 18 : 0), (h - contentH) / 2 - 8)
  }

  function setScene(next: Scene | null) {
    const prev = shown
    shown = next
    hover = null
    hoverId = -1
    if (!next) {
      requestDraw()
      return
    }
    const n = next.nodes.length
    const tween = animMs > 0 && prev && n < 2500 && prev.nodes.length < 2500
    const nx = new Float64Array(n)
    const ny = new Float64Array(n)
    fresh = new Uint8Array(n)
    ghosts = []
    if (tween && prev) {
      const byKey = new Map<string, number>()
      const byId = new Map<Id, number[]>()
      prev.nodes.forEach((p, i) => {
        byKey.set(p.key, i)
        const list = byId.get(p.id)
        if (!list) byId.set(p.id, [i])
        else if (list.length < 64) list.push(i)
      })
      const guide = next.guide
      const sites = guide?.sinks.map(([key]) => key)
      // an old node carries on when something takes its place or its subtree; anything else is a ghost
      const used = new Uint8Array(prev.nodes.length)
      const src = new Int32Array(n).fill(-1)
      for (let i = 0; i < n; i++) {
        const v = next.nodes[i]
        const k = byKey.get(v.key)
        const par = v.parent
        let from = -1
        // a node of a subtree that moved comes along with it
        if (par >= 0 && src[par] >= 0) {
          const c = byKey.get(prev.nodes[src[par]].key + v.key.slice(next.nodes[par].key.length))
          if (c !== undefined && prev.nodes[c].id === v.id) from = c
        }
        // the same spot holding the same thing (the spine above a step changes ids, not looks)
        if (from < 0 && k !== undefined) {
          const p = prev.nodes[k]
          if (p.id === v.id || (p.kind === v.kind && p.text === v.text && p.comb === v.comb)) from = k
        }
        if (from < 0) {
          const o = guide?.origin.get(v.key)?.flatMap((key) => byKey.get(key) ?? [])
          if (o?.length) {
            nx[i] = o.reduce((a, j) => a + curX[j], 0) / o.length
            ny[i] = o.reduce((a, j) => a + curY[j], 0) / o.length
            fresh[i] = 1
            continue
          }
          // a kept subtree that moved: the same tree from where the step happened
          const cands = (byId.get(v.id) ?? []).filter((j) => !sites || sites.some((key) => prev.nodes[j].key.startsWith(key)))
          from = cands.find((j) => !used[j]) ?? cands[0] ?? -1
        }
        if (from >= 0) {
          src[i] = from
          used[from] = 1
          nx[i] = curX[from] ?? v.x
          ny[i] = curY[from] ?? v.y
          continue
        }
        // something new grows out of its parent
        fresh[i] = 1
        if (par >= 0) {
          nx[i] = nx[par]
          ny[i] = ny[par]
        } else {
          nx[i] = k !== undefined ? (curX[k] ?? v.x) : v.x
          ny[i] = k !== undefined ? (curY[k] ?? v.y) : v.y
        }
      }
      if (falling && prev.nodes.length < 1500) {
        const nextKey = new Map<string, number>()
        next.nodes.forEach((v, i) => nextKey.set(v.key, i))
        const sinks = [...(guide?.sinks ?? [])].sort((a, b) => b[0].length - a[0].length)
        const ghostOf = new Map<number, number>()
        prev.nodes.forEach((p, j) => {
          if (used[j] || ghosts.length >= 400) return
          const sink = sinks.find(([pre]) => p.key.startsWith(pre))?.[1] ?? 'stay'
          let to = -1
          if (sink !== 'stay' && sink !== 'drop') {
            // the spot it was sent to may have folded away: the nearest one above it
            for (let key = sink; key.length >= 2 && to < 0; key = key.slice(0, -1)) to = nextKey.get(key) ?? -1
          }
          ghostOf.set(j, ghosts.length)
          ghosts.push({ n: p, x: curX[j], y: curY[j], to, drop: sink === 'drop', parent: ghostOf.get(p.parent) ?? -1 })
        })
      }
      fromX = nx
      fromY = ny
      dur = animMs
      t0 = performance.now()
    } else {
      for (let i = 0; i < n; i++) {
        nx[i] = next.nodes[i].x
        ny[i] = next.nodes[i].y
      }
      fromX = nx
      fromY = ny
      dur = 0
    }
    curX = Float64Array.from(nx)
    curY = Float64Array.from(ny)
    if (follow) {
      fitTarget(next)
      if (!tween) Object.assign(cam, target)
    }
    requestDraw()
  }

  const ease = (t: number) => 1 - (1 - t) * (1 - t) * (1 - t)

  function requestDraw() {
    if (!raf && ctx) raf = requestAnimationFrame(frame)
  }

  function frame(now: number) {
    raf = 0
    const dt = Math.min(0.05, (now - lastFrame) / 1000)
    lastFrame = now
    let again = false
    if (shown) {
      const nodes = shown.nodes
      if (dur > 0) {
        const t = Math.min(1, (now - t0) / dur)
        const k = ease(t)
        for (let i = 0; i < nodes.length; i++) {
          curX[i] = fromX[i] + (nodes[i].x - fromX[i]) * k
          curY[i] = fromY[i] + (nodes[i].y - fromY[i]) * k
        }
        if (t < 1) again = true
        else dur = 0
      }
      if (follow) {
        const d = Math.abs(cam.k - target.k) / target.k + Math.abs(cam.x - target.x) / 400 + Math.abs(cam.y - target.y) / 400
        if (d > 0.002) {
          const f = 1 - Math.pow(0.002, dt)
          cam.k += (target.k - cam.k) * f
          cam.x += (target.x - cam.x) * f
          cam.y += (target.y - cam.y) * f
          again = true
        } else Object.assign(cam, target)
      }
    }
    if (ghosts.length > 0 && dur === 0) ghosts = []
    draw()
    if (again) raf = requestAnimationFrame(frame)
  }

  function drawLeaf(x: number, y: number, tilt: number, color: string, k: number) {
    if (k < 0.45) {
      ctx.fillStyle = color
      ctx.beginPath()
      ctx.arc(x, y, 5, 0, Math.PI * 2)
      ctx.fill()
      return
    }
    const c = Math.cos(tilt)
    const s = Math.sin(tilt)
    const px = (u: number, v: number) => x + u * c - v * s
    const py = (u: number, v: number) => y + u * s + v * c
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.moveTo(px(0, -9), py(0, -9))
    ctx.bezierCurveTo(px(5.8, -5.5), py(5.8, -5.5), px(5.8, 3), py(5.8, 3), px(0, 9), py(0, 9))
    ctx.bezierCurveTo(px(-5.8, 3), py(-5.8, 3), px(-5.8, -5.5), py(-5.8, -5.5), px(0, -9), py(0, -9))
    ctx.fill()
  }

  const INK_RED = '#d6336c'
  /// garden keeps to soft plant colours; full colour is saved for the step about to happen
  const GARDEN = {
    light: { bark: '#9c8f7d', knot: '#7a6e5e', vine: '#a7b39c', bud: '#e2e8d8', paper: '#f6f2e8', ink: '#8a8072', step: '', leaves: ['#9fbf98', '#8fb38a', '#a9c6a2', '#94b590'] },
    dark: { bark: '#7c7264', knot: '#5c5448', vine: '#56634f', bud: '#3a4436', paper: '#2e2b25', ink: '#a59a8a', step: '', leaves: ['#6c8c67', '#5f7f5b', '#779772', '#66865f'] },
  }
  /// moss takes the site's own tree colours, which already have matching light and dark versions
  const garden = () => {
    if (look === 'moss') return { bark: C.bark, knot: C.bark, vine: C.bark, bud: C.g4soft, paper: C.bg, ink: C.bark, step: C.g2, leaves: LEAF_TINTS }
    return GARDEN[theme.resolved === 'dark' ? 'dark' : 'light']
  }
  /// moss's reduction points are flowers, so they're the warm colour on a green tree
  /// the original tree view's gold: a ring around a soft core, petals when it's next
  const flower = () => ({ petal: C.g4, deep: C.g4, heart: C.g4soft, letter: theme.resolved === 'dark' ? C.g4 : '#8a6414' })
  /// nodes inside the next step (the application about to fire, and what it's applied to)
  let inStep = new Uint8Array(0)

  function drawLeafAs(x: number, y: number, tilt: number, color: string, k: number, lit = false) {
    if (look === 'ink') {
      // the leaf is △, so draw one
      ctx.strokeStyle = C.fg
      ctx.fillStyle = C.bg
      ctx.lineWidth = 1.2
      ctx.beginPath()
      ctx.moveTo(x, y - 6)
      ctx.lineTo(x + 6, y + 4.5)
      ctx.lineTo(x - 6, y + 4.5)
      ctx.closePath()
      ctx.fill()
      ctx.stroke()
    } else if (look === 'quiet') {
      ctx.save()
      ctx.translate(x, y)
      ctx.scale(0.75, 0.75)
      drawLeaf(0, 0, tilt, LEAF_TINTS[1], k)
      ctx.restore()
    } else if (planty) {
      const g = garden()
      ctx.save()
      ctx.translate(x, y + 3)
      ctx.scale(1.35, 1.35)
      if (look === 'moss' && !lit) ctx.globalAlpha *= 0.55
      drawLeaf(0, 0, tilt * 1.4, lit || look === 'moss' ? color : g.leaves[Math.abs(Math.round(tilt * 17)) % g.leaves.length], Math.max(k, 0.5))
      // a vein
      ctx.strokeStyle = 'rgba(255,255,255,0.45)'
      ctx.lineWidth = 0.8
      ctx.beginPath()
      ctx.moveTo(-Math.sin(tilt * 1.4) * -7, Math.cos(tilt * 1.4) * -7)
      ctx.lineTo(-Math.sin(tilt * 1.4) * 6, Math.cos(tilt * 1.4) * 6)
      ctx.stroke()
      ctx.restore()
    } else drawLeaf(x, y, tilt, color, k)
  }

  /// a stem (one child) or a fork (two)
  function drawJoint(x: number, y: number, fork: boolean) {
    if (planty) {
      ctx.fillStyle = fork ? garden().knot : garden().bark
      ctx.beginPath()
      ctx.ellipse(x, y, fork ? 4.5 : 3.4, fork ? 3.6 : 3.4, 0, 0, Math.PI * 2)
      ctx.fill()
      return
    }
    const ink = look === 'ink'
    const col = ink ? C.fg : C.muted
    ctx.globalAlpha = fade * (look === 'quiet' ? 0.75 : 1)
    ctx.beginPath()
    ctx.arc(x, y, ink ? (fork ? 2.8 : 2.4) : fork ? 4.6 : 4.2, 0, Math.PI * 2)
    if (fork || ink) {
      ctx.fillStyle = col
      ctx.fill()
    } else {
      ctx.fillStyle = C.bg
      ctx.strokeStyle = col
      ctx.lineWidth = 1.6
      ctx.fill()
      ctx.stroke()
    }
    ctx.globalAlpha = fade
  }

  /// gold petals around whatever is about to change; `wide` stretches the ring to fit a name
  function petals(x: number, y: number, dist: number, rx: number, ry: number, wide = 0) {
    ctx.fillStyle = flower().petal
    ctx.globalAlpha = fade * (0.85)
    const count = wide > 0 ? 8 : 5
    for (let p = 0; p < count; p++) {
      const a = (p / count) * Math.PI * 2 - Math.PI / 2
      const ex = Math.cos(a) * (dist + wide)
      const ey = Math.sin(a) * dist
      ctx.beginPath()
      ctx.ellipse(x + ex, y + ey, rx, ry, Math.atan2(ey, ex), 0, Math.PI * 2)
      ctx.fill()
    }
    ctx.globalAlpha = fade
  }

  function drawApp(x: number, y: number, n: VNode, showText: boolean) {
    const col = look === 'ink' ? (n.next ? INK_RED : C.fg) : n.rule ? ruleColor(n.rule) : C.kw
    if (look === 'moss') {
      // as in the original tree view: a gold bud while it waits, a gold ring with the rule's
      // letter once it can fire, and petals around the one that fires next
      const fl = flower()
      if (n.next) petals(x, y, 13, 7.5, 5)
      if (n.rule || n.next) {
        const r = n.next ? 10 : 7.5
        ctx.fillStyle = fl.heart
        ctx.strokeStyle = fl.deep
        ctx.lineWidth = n.next ? 2.2 : 1.2
        ctx.beginPath()
        ctx.arc(x, y, r, 0, Math.PI * 2)
        ctx.fill()
        ctx.stroke()
        if (showText) {
          ctx.fillStyle = fl.letter
          ctx.font = `600 ${n.next ? 11 : 8.5}px 'JetBrains Mono Variable', monospace`
          ctx.fillText(RULE_GLYPH[n.rule] || '@', x, y + 0.5)
        }
      } else {
        ctx.strokeStyle = fl.deep
        ctx.fillStyle = fl.deep
        ctx.lineWidth = 1.3
        ctx.globalAlpha = fade * (0.75)
        ctx.beginPath()
        ctx.arc(x, y, 6, 0, Math.PI * 2)
        ctx.stroke()
        ctx.beginPath()
        ctx.arc(x, y, 2.3, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = fade
      }
      return
    }
    if (n.next) {
      if (planty) {
        // the next step blossoms
        ctx.fillStyle = col
        for (let p = 0; p < 5; p++) {
          const a = (p / 5) * Math.PI * 2 - Math.PI / 2
          ctx.globalAlpha = fade * (0.55)
          ctx.beginPath()
          ctx.ellipse(x + Math.cos(a) * 15, y + Math.sin(a) * 15, 7.5, 5, a, 0, Math.PI * 2)
          ctx.fill()
        }
        ctx.globalAlpha = fade
      } else if (look === 'bold') {
        ctx.fillStyle = col
        ctx.globalAlpha = fade * (0.22)
        ctx.beginPath()
        ctx.arc(x, y, 21, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = fade
      }
      ctx.fillStyle = C.bg
      ctx.strokeStyle = col
      ctx.lineWidth = look === 'ink' ? 1.6 : 2.6
      ctx.beginPath()
      if (look === 'ink') ctx.ellipse(x, y, 13, 10.5, -0.2, 0, Math.PI * 2)
      else ctx.arc(x, y, look === 'quiet' ? 10.5 : 12, 0, Math.PI * 2)
      ctx.fill()
      ctx.stroke()
      if (showText) {
        ctx.fillStyle = col
        ctx.font = look === 'ink' ? "italic 700 13px Georgia, serif" : "700 12px 'JetBrains Mono Variable', monospace"
        ctx.fillText(RULE_GLYPH[n.rule] || '@', x, y + 0.5)
      }
      return
    }
    if (planty) {
      // a bud: ready ones are a little fuller
      const g = garden()
      ctx.fillStyle = n.rule ? g.vine : g.bud
      ctx.beginPath()
      ctx.ellipse(x, y, 4.6, 5.8, 0, 0, Math.PI * 2)
      ctx.fill()
      ctx.strokeStyle = g.vine
      ctx.lineWidth = 1
      ctx.stroke()
      return
    }
    ctx.fillStyle = C.bg
    ctx.strokeStyle = look === 'quiet' ? (n.rule ? col : C.muted) : col
    ctx.lineWidth = look === 'ink' ? 1.2 : 1.4
    ctx.globalAlpha = fade * (look === 'quiet' ? (n.rule ? 0.8 : 0.55) : n.rule ? 1 : 0.65)
    ctx.beginPath()
    ctx.arc(x, y, look === 'ink' ? 4 : look === 'quiet' ? 4.5 : 5.5, 0, Math.PI * 2)
    ctx.fill()
    ctx.stroke()
    ctx.globalAlpha = fade
  }

  /// the top node of a subtree that acts as S, K, I or F
  function drawComb(x: number, y: number, op: string, called: boolean, showText: boolean, lit = true) {
    const col = planty && !lit ? garden().ink : combColor(op)
    if (look === 'bold') {
      ctx.fillStyle = col
      ctx.strokeStyle = C.bg
      ctx.lineWidth = 2
      ctx.beginPath()
      ctx.roundRect(x - 10, y - 10, 20, 20, 6)
      ctx.fill()
      ctx.stroke()
      if (showText) {
        ctx.fillStyle = '#fff'
        ctx.font = "700 12px 'JetBrains Mono Variable', monospace"
        ctx.fillText(op, x, y + 0.5)
      }
      return
    }
    if (planty) {
      // a paper tag hanging off the branch
      const tx = x - (look === 'moss' ? 13 : 17)
      const ty = y - (look === 'moss' ? 6 : 8)
      if (look !== 'moss') {
        ctx.strokeStyle = garden().bark
        ctx.lineWidth = 0.9
        ctx.beginPath()
        ctx.moveTo(x, y)
        ctx.lineTo(tx + 5, ty)
        ctx.stroke()
      }
      if (look === 'moss') {
        // just the letter, beside the branch
        if (showText) {
          ctx.font = `${called ? 15 : 13}px Georgia, serif`
          ctx.fillStyle = col
          ctx.fillText(op, tx, ty + 0.5)
        }
      } else {
        ctx.save()
        ctx.translate(tx, ty)
        ctx.rotate(-0.18)
        ctx.fillStyle = garden().paper
        ctx.strokeStyle = col
        ctx.lineWidth = called ? 2 : 1.2
        ctx.beginPath()
        ctx.roundRect(-8, -8, 16, 16, 3)
        ctx.fill()
        ctx.stroke()
        if (showText) {
          ctx.fillStyle = col
          ctx.font = "700 11px Georgia, serif"
          ctx.fillText(op, 0, 0.5)
        }
        ctx.restore()
      }
      ctx.fillStyle = col
      ctx.beginPath()
      ctx.arc(x, y, 3.6, 0, Math.PI * 2)
      ctx.fill()
      return
    }
    const ink = look === 'ink'
    ctx.fillStyle = C.bg
    ctx.strokeStyle = col
    ctx.lineWidth = called ? 2.2 : 1.3
    ctx.beginPath()
    if (ink) ctx.arc(x, y, 9, 0, Math.PI * 2)
    else ctx.roundRect(x - 9, y - 9, 18, 18, 5)
    ctx.fill()
    if (!ink) {
      ctx.globalAlpha = fade * (called ? 0.3 : 0.12)
      ctx.fillStyle = col
      ctx.fill()
      ctx.globalAlpha = fade
    }
    ctx.stroke()
    if (showText) {
      ctx.fillStyle = col
      ctx.font = ink ? "700 13px Georgia, serif" : "700 11.5px 'JetBrains Mono Variable', monospace"
      ctx.fillText(op, x, y + 0.5)
    }
  }

  type Landing = 'gets' | 'drop' | 'returns' | 'looks'
  /// Where an argument goes once the combinator at `root` is called on it: S hands it to both
  /// operands, K drops it, I gives it back, anything else is applied to it.
  function argFlow(nodes: VNode[], at: number): { root: number; land: Map<number, Landing>; route: Set<number>; label: string } | null {
    const kids: number[][] = nodes.map(() => [])
    for (let i = 0; i < nodes.length; i++) if (nodes[i].parent >= 0) kids[nodes[i].parent].push(i)
    let root = at
    let label = 'x'
    const n = nodes[at]
    if (n.kind === K_APP) {
      const [f, x] = kids[at]
      root = f
      const t = nodes[x]?.text
      if (t && t.length <= 8 && (nodes[x].kind === K_VAR || nodes[x].kind === K_POD)) label = t
    } else if (n.group >= 0) root = n.group
    if (root === undefined || !nodes[root]?.comb) return null
    const operands = (g: number) => {
      const out: number[] = []
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p >= 0 && nodes[i].group !== g && nodes[p].group === g) out.push(i)
      }
      return out.sort((a, b) => (nodes[a].key < nodes[b].key ? -1 : 1))
    }
    const land = new Map<number, Landing>()
    const visit = (i: number, depth: number) => {
      const c = nodes[i].comb
      const ops = c ? operands(i) : []
      if (c === 'S' && ops.length === 2 && depth < 40) {
        visit(ops[0], depth + 1)
        visit(ops[1], depth + 1)
      } else if (c === 'K' && ops.length === 1) land.set(i, 'drop')
      else if (c === 'I') land.set(i, 'returns')
      else if (c === 'F') land.set(i, 'looks')
      else land.set(i, 'gets')
    }
    visit(root, 0)
    const route = new Set<number>()
    for (const d of land.keys()) for (let i = d; i !== root && i >= 0; i = nodes[i].parent) route.add(i)
    return { root, land, route, label }
  }

  function draw() {
    if (!ctx) return
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    ctx.clearRect(0, 0, w, h)
    if (!shown) return
    const k = cam.k
    ctx.setTransform(dpr * k, 0, 0, dpr * k, dpr * cam.x, dpr * cam.y)
    const nodes = shown.nodes
    const minWX = (-40 - cam.x) / k
    const maxWX = (w + 40 - cam.x) / k
    const minWY = (-40 - cam.y) / k
    const maxWY = (h + 40 - cam.y) / k
    const vis = (x: number, y: number) => x >= minWX && x <= maxWX && y >= minWY && y <= maxWY

    const lw = Math.max(1.1, 1.5 / Math.sqrt(k)) / Math.max(1, 1 / k / 2)
    const dark = theme.resolved === 'dark'
    void dark
    const { bark, vine } = garden()
    if (inStep.length !== nodes.length) inStep = new Uint8Array(nodes.length)
    // breadth-first order: a parent always comes before its children
    for (let i = 0; i < nodes.length; i++) inStep[i] = nodes[i].next || (nodes[i].parent >= 0 && inStep[nodes[i].parent]) ? 1 : 0
    const pen = look === 'ink' ? C.fg : C.edge
    const wobble = look === 'ink' ? 5 : 0
    /// one edge, as the same gentle curve everywhere (ink wobbles it a little, steadily per node)
    const curve = (p: number, i: number) => {
      const x1 = curX[p]
      const y1 = curY[p]
      const x2 = curX[i]
      const y2 = curY[i]
      const j = wobble ? (tiltOf(nodes[i].key) / 0.61) * wobble : 0
      ctx.moveTo(x1, y1)
      ctx.quadraticCurveTo(x1 + (x2 - x1) * 0.5 + j, y1 + (y2 - y1) * 0.62 - j * 0.4, x2, y2)
    }
    // garden tapers like a plant; moss keeps one width so both phases read the same
    const branchW = (p: number) => (look === 'moss' ? Math.max(lw, 2.4) : Math.max(lw, 6.5 * Math.pow(0.8, curY[p] / 54)))
    const calledComb = (g: number) => {
      const p = nodes[g].parent
      return p >= 0 && nodes[p].next && nodes[p].kind === K_APP && nodes[g].side === 0
    }
    /// inside a combinator: the edge from its own node p to its own node i
    const inComb = (p: number, i: number) => nodes[i].group >= 0 && nodes[i].group === nodes[p].group

    if (look === 'bold') {
      // a combinator's own nodes sit in one tinted band; the one about to be called is darker
      for (let g = 0; g < nodes.length; g++) {
        const head = nodes[g]
        if (!head.comb) continue
        ctx.strokeStyle = combColor(head.comb)
        ctx.globalAlpha = fade * (calledComb(g) ? 0.38 : 0.2)
        ctx.lineWidth = 24
        ctx.lineJoin = 'round'
        ctx.lineCap = 'round'
        ctx.beginPath()
        ctx.moveTo(curX[g], curY[g])
        ctx.lineTo(curX[g], curY[g])
        for (let i = g + 1; i < nodes.length; i++) if (nodes[i].group === g && nodes[i].parent >= 0) curve(nodes[i].parent, i)
        ctx.stroke()
      }
      ctx.globalAlpha = fade
    }

    // edges: the tree's own, then the applications' (function side solid, argument dotted)
    ctx.lineCap = 'round'
    ctx.lineJoin = 'round'
    if (planty) {
      // branches taper with depth, so the eye reads the trunk first
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || nodes[p].kind === K_APP) continue
        if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
        ctx.strokeStyle = bark
        ctx.lineWidth = branchW(p)
        ctx.beginPath()
        curve(p, i)
        ctx.stroke()
      }
    } else {
      ctx.lineWidth = look === 'ink' ? 1.2 * lw : lw
      ctx.strokeStyle = pen
      ctx.globalAlpha = fade * (look === 'ink' ? 0.8 : look === 'quiet' ? 0.75 : 1)
      ctx.beginPath()
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || nodes[p].kind === K_APP) continue
        if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
        curve(p, i)
      }
      ctx.stroke()
    }
    ctx.strokeStyle = look === 'bold' ? C.kw : planty ? vine : look === 'ink' ? C.fg : C.muted
    ctx.globalAlpha = fade * (look === 'bold' ? 0.55 : look === 'quiet' ? 0.4 : look === 'moss' ? 0.55 : 0.7)
    ctx.lineWidth = planty ? lw * 1.3 : lw
    for (const side of [0, 1]) {
      ctx.setLineDash(side === 1 ? (planty ? [1, 4] : look === 'ink' ? [5, 3] : [3, 3]) : [])
      ctx.beginPath()
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || nodes[p].kind !== K_APP || nodes[i].side !== side) continue
        if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
        curve(p, i)
      }
      ctx.stroke()
    }
    ctx.setLineDash([])
    ctx.globalAlpha = fade
    // the calmer looks mark a combinator by colouring its own edges instead of a band
    if (look !== 'bold')
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || !inComb(p, i)) continue
        const g = nodes[i].group
        if (planty && !calledComb(g) && !inStep[g]) continue
        ctx.strokeStyle = combColor(nodes[g].comb)
        ctx.globalAlpha = fade * (calledComb(g) ? 1 : planty ? 0.6 : 0.75)
        ctx.lineWidth = planty ? branchW(p) : lw * (calledComb(g) ? 3 : 2.2)
        ctx.beginPath()
        curve(p, i)
        ctx.stroke()
      }
    ctx.globalAlpha = fade
    for (let i = 0; i < nodes.length; i++) {
      const p = nodes[i].parent
      if (p < 0 || !nodes[p].next || nodes[p].kind !== K_APP) continue
      ctx.strokeStyle = look === 'ink' ? INK_RED : ruleColor(nodes[p].rule)
      ctx.lineWidth = lw * 2
      ctx.beginPath()
      curve(p, i)
      ctx.stroke()
    }

    if (ghosts.length > 0 && dur > 0) {
      const p = Math.min(1, (performance.now() - t0) / dur)
      const e = ease(p)
      const gx = new Float64Array(ghosts.length)
      const gy = new Float64Array(ghosts.length)
      ghosts.forEach((g, j) => {
        const tx = g.to >= 0 ? curX[g.to] : g.x
        const ty = g.to >= 0 ? curY[g.to] : g.y + (g.drop ? 46 : 0)
        gx[j] = g.x + (tx - g.x) * e
        gy[j] = g.y + (ty - g.y) * e
      })
      // what merges into a new node stays visible until it gets there; the rest just fades
      const alphaOf = (g: Ghost) => (g.to >= 0 ? (p < 0.5 ? 1 : 1 - (p - 0.5) / 0.5) : g.drop ? 1 - p * p : 1 - e) * 0.9
      ctx.strokeStyle = planty ? bark : pen
      ctx.lineWidth = planty ? 2.4 : lw
      ghosts.forEach((g, j) => {
        if (g.parent < 0) return
        ctx.globalAlpha = fade * (alphaOf(g) * 0.7)
        ctx.beginPath()
        ctx.moveTo(gx[g.parent], gy[g.parent])
        ctx.lineTo(gx[j], gy[j])
        ctx.stroke()
      })
      ghosts.forEach((g, j) => {
        const x = gx[j]
        const y = gy[j]
        if (!vis(x, y)) return
        const n = g.n
        ctx.save()
        fade = alphaOf(g)
        ctx.globalAlpha = fade
        const sc = g.to >= 0 ? 1 - 0.3 * e : 1
        ctx.translate(x, y)
        ctx.scale(sc, sc)
        if (n.comb) drawComb(0, 0, n.comb, false, true, false)
        else if (n.kind === K_LAM) drawPill(0, 0, look === 'ink' ? `λ${n.text ?? ''}` : `{${n.text ?? ''}}`, varColor(n.text ?? ''), k, true)
        else if (n.kind === K_VAR) drawPill(0, 0, n.text ?? '', varColor(n.text ?? ''), k)
        else if (n.kind === K_LEAF) drawLeafAs(0, 0, tiltOf(n.key), LEAF_TINTS[1], k)
        else if (n.kind === K_STEM || n.kind === K_FORK) drawJoint(0, 0, n.kind === K_FORK)
        else if (n.kind === K_APP) drawApp(0, 0, { ...n, next: false }, false)
        else if (n.kind === K_POD) {
          ctx.strokeStyle = C.muted
          ctx.lineWidth = 1.2
          ctx.beginPath()
          ctx.roundRect(-podWidth(n.text ?? '') / 2, -10, podWidth(n.text ?? ''), 20, 9)
          ctx.stroke()
        }
        ctx.restore()
        fade = 1
      })
      ctx.globalAlpha = 1
    }

    const hi = hover ? nodes.indexOf(hover) : -1
    const flow = hi >= 0 ? argFlow(nodes, hi) : null
    const flowCol = look === 'ink' ? INK_RED : planty ? flower().deep : C.accent
    if (flow) {
      ctx.strokeStyle = flowCol
      ctx.lineWidth = lw * 2.4
      ctx.setLineDash([6, 4])
      ctx.globalAlpha = fade * (0.9)
      ctx.beginPath()
      for (const i of flow.route) curve(nodes[i].parent, i)
      ctx.stroke()
      ctx.setLineDash([])
      ctx.globalAlpha = fade
    }

    const showText = k >= 0.38
    ctx.textAlign = 'center'
    ctx.textBaseline = 'middle'
    for (let i = 0; i < nodes.length; i++) {
      const n = nodes[i]
      const x = curX[i]
      const y = curY[i]
      if (!vis(x, y)) continue
      const hot = n.id === hoverId
      fade = fresh[i] && dur > 0 ? Math.max(0.15, ease(Math.min(1, (performance.now() - t0) / dur))) : 1
      ctx.globalAlpha = fade
      if (n.diff && n.kind !== K_STEM && n.kind !== K_FORK) {
        ctx.fillStyle = C.err
        ctx.globalAlpha = fade * (0.22)
        ctx.beginPath()
        ctx.arc(x, y, 16, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = fade
      }
      if (n.next && n.kind !== K_APP && look === 'moss') {
        const named = n.kind === K_LAM || n.kind === K_VAR
        const text = n.kind === K_LAM ? `{${n.text ?? ''}}` : (n.text ?? '')
        if (named) petals(x, y, 17, 6, 4, Math.max(0, (podWidth(text) * pillScale(k)) / 2 - 10))
        else petals(x, y, 13, 8, 5.5)
      }
      else if (n.next && n.kind !== K_APP) {
        ctx.fillStyle = C.accent
        ctx.globalAlpha = fade * (0.22)
        ctx.beginPath()
        ctx.arc(x, y, 19, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = fade
      }
      if (n.comb) drawComb(x, y, n.comb, calledComb(i), showText, calledComb(i) || inStep[i] === 1)
      else switch (n.kind) {
        case K_LAM: {
          if (n.rule && !n.next && look !== 'ink') {
            // a binder ready to come off is a reduction point like any other: a gold ring
            const s = pillScale(k)
            const pw = Math.max(22 * s, podWidth(`{${n.text ?? ''}}`) * s + 2)
            ctx.strokeStyle = look === 'moss' ? flower().deep : C.accent
            ctx.lineWidth = 1.6
            ctx.beginPath()
            ctx.roundRect(x - pw / 2 - 3, y - 11 * s - 3, pw + 6, 22 * s + 6, 13 * s)
            ctx.stroke()
          }
          drawPill(x, y, look === 'ink' ? `λ${n.text ?? ''}` : `{${n.text ?? ''}}`, varColor(n.text ?? ''), k, true, inStep[i] === 1)
          break
        }
        case K_LEAF: {
          const key = n.key
          let hh = 7
          for (let c = key.length - 1; c >= Math.max(0, key.length - 6); c--) hh = (hh * 31 + key.charCodeAt(c)) | 0
          drawLeafAs(x, y, tiltOf(key), LEAF_TINTS[(hh >>> 0) % LEAF_TINTS.length], k, inStep[i] === 1)
          break
        }
        case K_STEM:
        case K_FORK:
          drawJoint(x, y, n.kind === K_FORK)
          break
        case K_APP:
          drawApp(x, y, n, showText)
          break
        case K_VAR:
          drawPill(x, y, n.text ?? '', varColor(n.text ?? ''), k, false, inStep[i] === 1)
          break
        case K_POD: {
          const text = n.text ?? ''
          const pw = podWidth(text)
          const col = look === 'ink' ? C.fg : planty && !inStep[i] ? garden().ink : podColor(n.podKind)
          ctx.fillStyle = C.bg
          ctx.strokeStyle = col
          ctx.lineWidth = glow && glow === text ? 2.6 : look === 'ink' ? 1 : 1.3
          ctx.beginPath()
          ctx.roundRect(x - pw / 2, y - 10, pw, 20, look === 'ink' ? 3 : 9)
          ctx.fill()
          ctx.globalAlpha = fade * (glow && glow === text ? 0.3 : look === 'ink' ? 0 : 0.1)
          ctx.fillStyle = col
          ctx.fill()
          ctx.globalAlpha = fade
          ctx.stroke()
          if (showText) {
            ctx.fillStyle = col
            ctx.font = "11px 'JetBrains Mono Variable', monospace"
            ctx.fillText(text.length > 28 ? text.slice(0, 27) + '…' : text, x, y + 0.5)
          }
          break
        }
      }
      if (hot) {
        ctx.strokeStyle = C.accent
        ctx.lineWidth = n === hover ? 2.2 : 1.2
        ctx.beginPath()
        if (n.kind === K_POD) {
          const pw = podWidth(n.text ?? '')
          ctx.roundRect(x - pw / 2 - 3, y - 13, pw + 6, 26, 12)
        } else if (n.kind === K_VAR || n.kind === K_LAM) {
          const s = pillScale(k)
          const pw = Math.max(22 * s, podWidth(n.text ?? '') * s + 2)
          ctx.roundRect(x - pw / 2 - 3, y - 11 * s - 3, pw + 6, 22 * s + 6, 13 * s)
        } else ctx.arc(x, y, (n.kind === K_FORK || n.kind === K_STEM) && !n.comb ? 9 : 14, 0, Math.PI * 2)
        ctx.stroke()
      }
    }
    fade = 1
    ctx.globalAlpha = 1
    if (flow)
      for (const [i, how] of flow.land) {
        const x = curX[i]
        const y = curY[i]
        ctx.strokeStyle = flowCol
        ctx.fillStyle = flowCol
        ctx.lineWidth = 2
        if (how === 'drop') {
          // K throws the argument away here
          ctx.beginPath()
          ctx.moveTo(x + 9, y - 19)
          ctx.lineTo(x + 17, y - 11)
          ctx.moveTo(x + 17, y - 19)
          ctx.lineTo(x + 9, y - 11)
          ctx.stroke()
          continue
        }
        ctx.setLineDash(how === 'looks' ? [3, 3] : [])
        ctx.beginPath()
        ctx.arc(x, y, 16, 0, Math.PI * 2)
        ctx.stroke()
        ctx.setLineDash([])
        const tag = how === 'looks' ? `${flow.label}?` : flow.label
        ctx.font = "italic 700 12px Georgia, serif"
        const tw = ctx.measureText(tag).width + 8
        ctx.beginPath()
        ctx.roundRect(x + 10, y - 26, tw, 15, 7)
        ctx.fill()
        ctx.fillStyle = '#fff'
        ctx.fillText(tag, x + 10 + tw / 2, y - 18)
      }
    for (const t of shown.titles) {
      ctx.fillStyle = t.bad ? C.err : C.muted
      ctx.font = "600 12px 'Inter Variable', sans-serif"
      ctx.fillText(t.text, t.x, -28)
    }
  }

  // ---- input ----------------------------------------------------------------
  function hitTest(sx: number, sy: number): VNode | null {
    if (!shown) return null
    const wx = (sx - cam.x) / cam.k
    const wy = (sy - cam.y) / cam.k
    const slop = Math.max(12, 6 / cam.k)
    let best: VNode | null = null
    let bestD = Infinity
    const nodes = shown.nodes
    for (let i = 0; i < nodes.length; i++) {
      const dx = wx - curX[i]
      const dy = wy - curY[i]
      const n = nodes[i]
      if (n.kind === K_POD && Math.abs(dy) < 12 && Math.abs(dx) < podWidth(n.text ?? '') / 2 + 2) return n
      if ((n.kind === K_VAR || n.kind === K_LAM) && Math.abs(dy) < 11 * pillScale(cam.k) && Math.abs(dx) < (podWidth(n.text ?? '') / 2 + 4) * pillScale(cam.k)) return n
      const d = dx * dx + dy * dy
      if (d < bestD && d < slop * slop) {
        bestD = d
        best = n
      }
    }
    return best
  }

  const pointers = new Map<number, { x: number; y: number }>()
  let dragged = 0
  let pinch = 0

  function localXY(e: { clientX: number; clientY: number }) {
    const r = cv.getBoundingClientRect()
    return { x: e.clientX - r.left, y: e.clientY - r.top }
  }
  function onDown(e: PointerEvent) {
    host.focus({ preventScroll: true })
    cv.setPointerCapture(e.pointerId)
    pointers.set(e.pointerId, localXY(e))
    dragged = 0
    pinch = 0
  }
  function onMove(e: PointerEvent) {
    const p = localXY(e)
    const prev = pointers.get(e.pointerId)
    if (!prev) {
      const n = hitTest(p.x, p.y)
      if (n !== hover) {
        hover = n
        hoverId = n?.id ?? -1
        cv.style.cursor = n ? 'pointer' : 'grab'
        onHover?.(n)
        requestDraw()
      }
      return
    }
    if (pointers.size === 2) {
      pointers.set(e.pointerId, p)
      const [a, b] = [...pointers.values()]
      const d = Math.hypot(a.x - b.x, a.y - b.y)
      if (pinch > 0) zoomAt((a.x + b.x) / 2, (a.y + b.y) / 2, d / pinch)
      pinch = d
      dragged = 99
      return
    }
    dragged += Math.abs(p.x - prev.x) + Math.abs(p.y - prev.y)
    if (dragged > 3) {
      follow = false
      cam.x += p.x - prev.x
      cam.y += p.y - prev.y
      requestDraw()
    }
    pointers.set(e.pointerId, p)
  }
  function onUp(e: PointerEvent) {
    const wasClick = pointers.size === 1 && dragged <= 3
    pointers.delete(e.pointerId)
    if (wasClick) {
      const p = localXY(e)
      const n = hitTest(p.x, p.y)
      if (n) onClick?.(n, e)
    }
  }
  function zoomAt(sx: number, sy: number, f: number) {
    follow = false
    const nk = Math.max(0.01, Math.min(4, cam.k * f))
    const r = nk / cam.k
    cam.x = sx - (sx - cam.x) * r
    cam.y = sy - (sy - cam.y) * r
    cam.k = nk
    requestDraw()
  }
  function onWheel(e: WheelEvent) {
    e.preventDefault()
    const p = localXY(e)
    // pinches and mouse wheels zoom; a trackpad's two-finger scroll pans
    const isPan = !e.ctrlKey && e.deltaMode === 0 && (Math.abs(e.deltaX) > 0.5 || Math.abs(e.deltaY) < 40)
    if (isPan) {
      follow = false
      cam.x -= e.deltaX
      cam.y -= e.deltaY
      requestDraw()
    } else zoomAt(p.x, p.y, Math.exp(-e.deltaY * (e.ctrlKey ? 0.012 : 0.0016)))
  }

  function resize() {
    const r = host.getBoundingClientRect()
    w = r.width
    h = r.height
    dpr = window.devicePixelRatio || 1
    cv.width = Math.round(w * dpr)
    cv.height = Math.round(h * dpr)
    if (shown && follow) {
      fitTarget(shown)
      Object.assign(cam, target)
    }
    requestDraw()
  }

  onMount(() => {
    ctx = cv.getContext('2d', { alpha: true })!
    readColors()
    resize()
    const ro = new ResizeObserver(resize)
    ro.observe(host)
    api?.({
      fit: () => {
        follow = true
        if (shown) fitTarget(shown)
        requestDraw()
      },
      zoom: (f) => zoomAt(w / 2, h / 2, f),
      focus: (key) => {
        if (!shown) return
        const i = shown.nodes.findIndex((n) => n.key === key)
        if (i < 0) return
        follow = false
        const k = Math.max(cam.k, 0.8)
        cam.k = k
        cam.x = w / 2 - curX[i] * k
        cam.y = h / 2 - curY[i] * k
        requestDraw()
      }
    })
    setScene(scene)
    return () => {
      ro.disconnect()
      cancelAnimationFrame(raf)
    }
  })

  $effect(() => {
    const next = scene
    if (ctx) untrack(() => setScene(next))
  })
  $effect(() => {
    void glow
    requestDraw()
  })
  // the page flips data-theme; colours follow on the next tick
  $effect(() => {
    void theme.resolved
    if (ctx) queueMicrotask(readColors)
  })
  $effect(() => {
    void look
    untrack(requestDraw)
  })
</script>

<div class="wrap" bind:this={host} tabindex="-1" role="presentation">
  <canvas
    bind:this={cv}
    onpointerdown={onDown}
    onpointermove={onMove}
    onpointerup={onUp}
    onpointercancel={onUp}
    onpointerleave={() => {
      if (hover) {
        hover = null
        hoverId = -1
        onHover?.(null)
        requestDraw()
      }
    }}
    onwheel={onWheel}
  ></canvas>
</div>

<style>
  .wrap {
    position: relative;
    width: 100%;
    height: 100%;
    min-height: 0;
    overflow: hidden;
    outline: none;
    touch-action: none;
  }
  canvas {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
    cursor: grab;
  }
</style>
