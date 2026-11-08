<script lang="ts">
  // The tree drawing: a canvas with pan, zoom and fit that glides between
  // scenes. Nodes keep identity by path and by subtree id, so a kept subtree
  // visibly travels to its new place; what a step removes fades out on its way
  // to where the step sends it.
  import { onMount, untrack } from 'svelte'
  import { theme } from '$lib/theme.svelte'
  import { followTheme } from './theme-follow.ts'
  import { K_LEAF, K_STEM, K_FORK, K_APP, K_VAR, K_POD, K_LAM, type Scene, type VNode } from './layout.ts'
  import { R_K, R_S, R_F, RULE_GLYPH, type Id } from './store.ts'
  import type { Roles, RoleHit } from './roles.ts'
  import { planTween, stillTween, nodesAt, ghostAt, fitCam, reachCam, trunkCam, pinAt, camAt, ease, actT, orient, yRange as yRangeOf, knockedOff, knockTime, fallAt, fallAlpha, fallLife, type Fall, type Ghost, type Tween, type Pin, type View } from './motion.ts'

  /// how the tree is drawn: bold is the original, the rest are calmer
  export type TreeLook = 'tree' | 'bold' | 'quiet' | 'garden' | 'moss' | 'ink'

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
    /// while playing: the view only zooms out, so a tree that grows and shrinks doesn't pump; it refits once this goes false
    steady?: boolean
    /// `onTag`: the pointer is on the node's name tag, not the node
    onHover?: (n: VNode | null, onTag?: boolean) => void
    onClick?: (n: VNode, e: MouseEvent, onTag?: boolean) => void
    /// what each part of a finished function does to which input, drawn over it
    roles?: Roles | null
    /// the one input whose roles stand out
    roleFocus?: string | null
    onRoleHover?: (r: RoleHit | null) => void
    onRoleClick?: (r: RoleHit) => void
    api?: (a: CanvasApi) => void
  }
  let { scene, animMs = 0, glow = null, falling = false, varColor = () => '#1971c2', look = 'bold', steady = false, onHover, onClick, roles = null, roleFocus = null, onRoleHover, onRoleClick, api }: Props = $props()

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
  let curX = new Float64Array(0)
  let curY = new Float64Array(0)
  /// the step being drawn: where each node starts, and what fades out (see motion.ts)
  let tw: Tween = { fromX: curX, fromY: curY, fresh: new Uint8Array(0), ghosts: [], acts: null, anchor: new Int32Array(0), root: -1 }
  let fresh = tw.fresh
  let ghosts: Ghost[] = []
  /// knocked-off variables lying on the ground after their step, fading
  let falls: { n: VNode; f: Fall; t: number; scale: number; parent: number; turns: boolean }[] = []
  /// how far the fruit being drawn has rolled, in radians
  let spin = 0
  /// films slow every animation down by this, to look at it frame by frame
  const animScale = () => (globalThis as { __dispAnimScale?: number }).__dispAnimScale ?? 1
  let t0 = 0
  let dur = 0
  let pin: Pin | null = null
  let hover: VNode | null = null
  let hoverId: Id = -1
  /// the hovered node's name tag is under the pointer, rather than the node
  let hoverTag = false
  /// where each name tag was drawn last, in world coordinates, for the pointer
  let tagBoxes: { i: number; x: number; y: number; w: number; h: number }[] = []

  /// every alpha below is scaled by this, so a ghost or a new node can be drawn faded with the usual helpers
  let fade = 1
  /// how far the next step's marks have come in: they wait for the current step's motion to land
  let bloom = 1
  const isNext = (n: VNode) => n.next && bloom > 0
  const reduced = typeof matchMedia === 'function' ? matchMedia('(prefers-reduced-motion: reduce)') : null
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
    // reduced motion gives every element a tiny transition, which would hand back the old colour
    probe.style.transition = 'none'
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
      g4soft: get('color-mix(in oklab, var(--g4) 22%, var(--bg-code))'),
      g1: get('--g1'),
      blossom: get('--blossom'),
      hillBack: get('color-mix(in oklab, var(--g1) 16%, var(--bg-elev))'),
      hillFront: get('color-mix(in oklab, var(--g2) 13%, var(--bg-elev))'),
      grass: get('color-mix(in oklab, var(--g2) 55%, transparent)')
    }
    probe.remove()
    requestDraw()
  }

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']
  const ruleColor = (r: number) => (look === 'moss' ? flower().deep : r === R_K ? C.warn : r === R_S ? C.kw : r === R_F ? C.err : C.accent)
  /// garden and its all-green twin
  const planty = $derived(look === 'garden' || look === 'moss')
  /// the home page's tree view: it grows up out of a hill
  const up = $derived(look === 'tree')
  /// the scene's height, root at 0: down from it, or up from it with the hill below
  const yRange = (s: Scene) => yRangeOf(s, up)
  const FRUIT = '#e79a3f'
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

  /// the width of the pill drawPill draws for `text`
  function pillWidth(text: string, k: number): number {
    const s = pillScale(k)
    ctx.font = `${look === 'moss' ? 500 : 700} ${12 * s}px 'JetBrains Mono Variable', monospace`
    return Math.max(22 * s, ctx.measureText(text.length > 16 ? text.slice(0, 15) + '…' : text).width + 14 * s)
  }
  /// a ring round a binder's pill, touching its edge
  function pillRing(x: number, y: number, text: string, k: number, lw: number) {
    const s = pillScale(k)
    const pw = pillWidth(text, k) + lw
    const ph = 22 * s + lw
    ctx.lineWidth = lw
    ctx.beginPath()
    ctx.roundRect(x - pw / 2, y - ph / 2, pw, ph, ph / 2)
    ctx.stroke()
  }

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
    if (up && !binder) return drawFruit(x, y, text, k, col)
    ctx.font = `${look === 'moss' ? 500 : 700} ${12 * s}px 'JetBrains Mono Variable', monospace`
    const pw = Math.max(22 * s, ctx.measureText(shown).width + 14 * s)
    const tint = look === 'quiet' || look === 'moss' || up || (planty && !lit)
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
      ctx.fillStyle = up ? inkOf(col) : tint ? col : '#fff'
      ctx.fillText(shown, x, y + 0.5)
    }
  }

  const view = (): View => ({ w, h, up })
  function fitTarget(s: Scene, minX = s.minX, maxX = s.maxX, top = yRange(s)[0], bottom = yRange(s)[1]) {
    Object.assign(target, fitCam(view(), s, minX, maxX, top, bottom))
  }
  /// fit the view to the run's box when it has one, else to this frame
  const refit = (sc: Scene) => Object.assign(target, reachCam(view(), sc) ?? fitCam(view(), sc))

  /// While playing, the view holds still: it moves only when the tree leaves it or shrinks well inside it, and
  /// then leaves room to spare. A tree too big to read whole is followed at a readable zoom instead, panning only
  /// when the next step nears an edge.
  function steadyTarget(s: Scene) {
    const pad = 34
    const floor = 0.45
    const slack = (s.maxX - s.minX) * 0.12
    fitTarget(s)
    const sx = (x: number) => cam.x + x * cam.k
    const sy = (y: number) => cam.y + y * cam.k
    if (target.k >= Math.min(floor, cam.k)) {
      const [top, bottom] = yRange(s)
      const fits = target.k >= cam.k * 0.999 && sx(s.minX) >= pad && sx(s.maxX) <= w - pad && sy(top) >= pad && sy(bottom) <= h - pad - 26
      const extra = (bottom - top) * 0.12
      if (fits && target.k < cam.k * 1.6) Object.assign(target, cam)
      else fitTarget(s, s.minX - slack, s.maxX + slack, up ? top - extra : top, up ? bottom : bottom + extra)
      return
    }
    target.k = Math.min(cam.k, floor)
    const site = s.nodes.filter((n) => n.next)
    const cx = site.length ? site.reduce((a, n) => a + n.x, 0) / site.length : (s.minX + s.maxX) / 2
    const cy = site.length ? site.reduce((a, n) => a + n.y, 0) / site.length : 0
    const px = cam.x + cx * target.k
    const py = cam.y + cy * target.k
    target.x = px > w * 0.25 && px < w * 0.75 && target.k === cam.k ? cam.x : w / 2 - cx * target.k
    target.y = py > h * 0.2 && py < h * 0.75 && target.k === cam.k ? cam.y : up ? h * 0.55 - cy * target.k : Math.min(pad, h * 0.45 - cy * target.k)
  }

  function setScene(next: Scene | null) {
    if (next) next = orient(next, up)
    const prev = shown
    shown = next
    // what was under the pointer may be gone; its hover text goes with it
    if (hover) onHover?.(null)
    hover = null
    hoverId = -1
    if (!next) {
      requestDraw()
      return
    }
    const n = next.nodes.length
    const tween = !!(animMs > 0 && !reduced?.matches && prev && n < 2500 && prev.nodes.length < 2500)
    // a variable still falling when the next step starts lands where it was going
    launch()
    ghosts = []
    if (!tween) falls = []
    tw = tween && prev ? planTween(prev, next, curX, curY, falling) : stillTween(next)
    fresh = tw.fresh
    ghosts = tw.ghosts
    dur = tween ? animMs * animScale() : 0
    t0 = performance.now()
    curX = Float64Array.from(tw.fromX)
    curY = Float64Array.from(tw.fromY)
    pin = null
    if (follow) {
      const v = view()
      // one zoom for the whole run, with the trunk where it always is; else the trunk held where it was while it can be
      const whole = reachCam(v, next)
      const held = whole ? null : tween && prev && !steady ? trunkCam(v, target, prev, next) : null
      if (whole || held) {
        Object.assign(target, whole ?? held)
        if (tween && prev) pin = pinAt(target, cam, next)
      } else if (steady && tween) steadyTarget(next)
      else fitTarget(next)
      if (!tween) Object.assign(cam, target)
    }
    requestDraw()
  }

  /// Hand what a binder knocks off to its fall, on its own clock from here on: at the knock, or at once when the step
  /// is cut short.
  function launch() {
    if (!ghosts.some((g) => g.hit >= 0 && !g.launched)) return
    const now = performance.now()
    const knocked = knockedOff(tw, curX, curY, up)
    const of = new Map<number, number>()
    for (const { ghost, fall } of knocked) {
      const g = ghosts[ghost]
      if (g.launched) continue
      g.launched = true
      of.set(ghost, falls.length)
      // a lone variable rolls as it goes; a bigger tree keeps its shape
      falls.push({ n: g.n, f: fall, t: now, scale: animScale(), parent: of.get(g.parent) ?? -1, turns: knocked.length === 1 })
    }
  }

  /// eases the view toward its target; true while it is still moving
  function glide(dt: number) {
    const d = Math.abs(cam.k - target.k) / target.k + Math.abs(cam.x - target.x) / 400 + Math.abs(cam.y - target.y) / 400
    if (d <= 0.002) {
      Object.assign(cam, target)
      return false
    }
    const f = 1 - Math.pow(0.002, dt)
    cam.k += (target.k - cam.k) * f
    cam.x += (target.x - cam.x) * f
    cam.y += (target.y - cam.y) * f
    return true
  }

  function requestDraw() {
    if (!raf && ctx) raf = requestAnimationFrame(frame)
  }

  function frame() {
    // the same clock the step's start was read from, so a test's fake clock drives both
    const now = performance.now()
    raf = 0
    const dt = Math.min(0.05, (now - lastFrame) / 1000)
    lastFrame = now
    let again = false
    if (shown) {
      const nodes = shown.nodes
      if (dur > 0) {
        const t = Math.min(1, (now - t0) / dur)
        nodesAt(tw, shown, t, curX, curY)
        if (t >= knockTime(tw)) launch()
        if (follow && pin) {
          Object.assign(cam, camAt(pin, t, curX, curY))
        } else if (follow && steady) again = glide(dt) || again
        else if (follow) {
          // the view fits where the nodes are right now, so nothing leaves it and it lands when they do;
          // the scene's bounds sit a little outside its nodes' centres, and that margin carries over
          let lo = Infinity
          let hi = -Infinity
          let top = 0
          let bottom = 0
          let lo1 = Infinity
          let hi1 = -Infinity
          let top1 = 0
          let bottom1 = 0
          for (let i = 0; i < nodes.length; i++) {
            lo = Math.min(lo, curX[i])
            hi = Math.max(hi, curX[i])
            top = Math.min(top, curY[i])
            bottom = Math.max(bottom, curY[i])
            lo1 = Math.min(lo1, nodes[i].x)
            hi1 = Math.max(hi1, nodes[i].x)
            top1 = Math.min(top1, nodes[i].y)
            bottom1 = Math.max(bottom1, nodes[i].y)
          }
          const [yt, yb] = yRange(shown)
          fitTarget(shown, lo - (lo1 - shown.minX), hi + (shown.maxX - hi1), top + (yt - top1), bottom + (yb - bottom1))
          Object.assign(cam, target)
          if (t >= 1) fitTarget(shown)
        }
        if (t < 1) again = true
        else dur = 0
      } else if (follow && reduced?.matches) Object.assign(cam, target)
      else if (follow) again = glide(dt) || again
    }
    if (ghosts.length > 0 && dur === 0) {
      launch()
      ghosts = []
    }
    if (falls.length) {
      falls = falls.filter((b) => (now - b.t) / 1000 / b.scale < fallLife(b.f))
      if (falls.length) again = true
    }
    draw()
    if (again) raf = requestAnimationFrame(frame)
  }

  /// a colour mixed toward the background's opposite, for writing in it
  function inkOf(hex: string, f = 0.42): string {
    const m = /^#([0-9a-f]{6})$/i.exec(hex)
    if (!m) return hex
    const v = parseInt(m[1], 16)
    const to = theme.resolved === 'dark' ? 255 : 0
    const g = (sh: number) => Math.round(((v >> sh) & 255) * (1 - f) + to * f)
    return `rgb(${g(16)}, ${g(8)}, ${g(0)})`
  }

  /// a name as fruit, in its own colour: its name on the skin when short, above it when not
  function drawFruit(x: number, y: number, text: string, k: number, col = FRUIT) {
    // a rolling fruit turns its stalk; the shine stays where the light is, and the name stays upright
    const c = Math.cos(spin)
    const sn = Math.sin(spin)
    const at = (u: number, v: number): [number, number] => [x + u * c - v * sn, y + u * sn + v * c]
    ctx.strokeStyle = C.bark
    ctx.lineWidth = 1.5
    ctx.beginPath()
    ctx.moveTo(...at(0, -9))
    ctx.quadraticCurveTo(...at(1, -13), ...at(3.5, -14.5))
    ctx.stroke()
    ctx.fillStyle = col
    ctx.strokeStyle = 'rgba(60, 30, 10, 0.4)'
    ctx.lineWidth = 0.8
    ctx.beginPath()
    ctx.arc(x, y, 10, 0, Math.PI * 2)
    ctx.fill()
    ctx.stroke()
    ctx.fillStyle = 'rgba(255, 255, 255, 0.42)'
    ctx.beginPath()
    ctx.ellipse(x - 3.4, y - 3.8, 2.9, 1.8, 0, 0, Math.PI * 2)
    ctx.fill()
    if (k < 0.3) return
    const short = text.length <= 3
    ctx.font = `700 ${short ? [10, 10, 8.5, 7][text.length] : 10 * pillScale(k)}px 'JetBrains Mono Variable', monospace`
    ctx.fillStyle = short ? '#fff' : inkOf(col, 0.25)
    ctx.fillText(short ? text : text.length > 16 ? text.slice(0, 15) + '…' : text, x, short ? y + 0.5 : y - 21)
  }

  /// what a fork does to an input: a split for a copy, an arrow to the one side it sends it, a cross for a drop, a question for a look at its shape.
  /// Filled when it happens in the step about to fire, an outline while it is still to come.
  function drawRoleMark(x: number, y: number, kind: string, col: string, hot: boolean, now: boolean) {
    ctx.beginPath()
    ctx.arc(x, y, now ? 10 : 9, 0, Math.PI * 2)
    ctx.fillStyle = now ? col : C.bg
    ctx.fill()
    ctx.strokeStyle = col
    ctx.lineWidth = hot ? 2.6 : 1.8
    ctx.stroke()
    if (now) ctx.strokeStyle = '#fff'
    ctx.lineWidth = 1.9
    ctx.lineCap = 'round'
    ctx.lineJoin = 'round'
    ctx.beginPath()
    if (kind === 'copy') {
      ctx.moveTo(x, y + 5)
      ctx.lineTo(x, y + 1.5)
      ctx.lineTo(x - 4, y - 2.5)
      ctx.moveTo(x, y + 1.5)
      ctx.lineTo(x + 4, y - 2.5)
      ctx.stroke()
    } else if (kind === 'left' || kind === 'right') {
      // toward that child: up the tree in the tree look, down it otherwise
      const dx = kind === 'left' ? -1 : 1
      const dy = up ? -1 : 1
      ctx.moveTo(x - dx * 3.5, y - dy * 3.5)
      ctx.lineTo(x + dx * 3.5, y + dy * 3.5)
      ctx.moveTo(x + dx * 3.5 - dx * 4.5, y + dy * 3.5)
      ctx.lineTo(x + dx * 3.5, y + dy * 3.5)
      ctx.lineTo(x + dx * 3.5, y + dy * 3.5 - dy * 4.5)
      ctx.stroke()
    } else if (kind === 'drop') {
      ctx.moveTo(x - 3.5, y - 3.5)
      ctx.lineTo(x + 3.5, y + 3.5)
      ctx.moveTo(x + 3.5, y - 3.5)
      ctx.lineTo(x - 3.5, y + 3.5)
      ctx.stroke()
    } else {
      ctx.fillStyle = now ? '#fff' : col
      ctx.font = "700 11px 'Inter Variable', sans-serif"
      ctx.fillText('?', x, y + 0.5)
    }
  }
  /// the fruit an input will be where it lands, as an outline on a dashed stalk from the node it lands on
  function drawHeld(nx: number, ny: number, x: number, y: number, col: string, hot: boolean) {
    ctx.strokeStyle = col
    ctx.setLineDash([2, 2.5])
    ctx.lineWidth = 1.3
    ctx.beginPath()
    ctx.moveTo(nx, ny)
    ctx.lineTo(x, y)
    ctx.stroke()
    ctx.setLineDash([])
    ctx.beginPath()
    ctx.arc(x, y, 7.5, 0, Math.PI * 2)
    ctx.fillStyle = C.bg
    ctx.fill()
    ctx.lineWidth = hot ? 2.6 : 1.8
    ctx.stroke()
  }

  /// a folded subtree as its name on a green pill, as a binder wears its name on a gold one
  function drawNamePod(x: number, y: number, text: string, showText: boolean, lit = false) {
    const pw = podWidth(text) + 6
    ctx.beginPath()
    ctx.roundRect(x - pw / 2, y - 11, pw, 22, 11)
    ctx.fillStyle = C.bg
    ctx.fill()
    ctx.globalAlpha = fade * (lit ? 0.3 : 0.16)
    ctx.fillStyle = C.g2
    ctx.fill()
    ctx.globalAlpha = fade
    ctx.strokeStyle = C.g2
    ctx.lineWidth = lit ? 2.4 : 1.3
    ctx.stroke()
    if (!showText) return
    ctx.fillStyle = C.g2
    ctx.font = "700 11px 'JetBrains Mono Variable', monospace"
    ctx.fillText(text.length > 28 ? text.slice(0, 27) + '…' : text, x, y + 0.5)
  }

  /// a named subtree drawn open wears its name on a small tag up and to its left; clicking the tag folds it
  function drawTag(i: number, x: number, y: number, text: string, hot: boolean) {
    ctx.font = "600 10px 'JetBrains Mono Variable', monospace"
    const shown = text.length > 20 ? text.slice(0, 19) + '…' : text
    const w = ctx.measureText(shown).width + 10
    const h = 15
    const bx = x - 10 - w
    const by = y - 26
    const col = up ? C.g2 : C.name
    ctx.strokeStyle = col
    ctx.lineWidth = 1
    ctx.globalAlpha = fade * 0.6
    ctx.beginPath()
    ctx.moveTo(bx + w, by + h)
    ctx.lineTo(x - 4, y - 4)
    ctx.stroke()
    ctx.globalAlpha = fade
    ctx.beginPath()
    ctx.roundRect(bx, by, w, h, 7)
    ctx.fillStyle = C.bg
    ctx.fill()
    ctx.globalAlpha = fade * (hot ? 0.3 : 0.14)
    ctx.fillStyle = col
    ctx.fill()
    ctx.globalAlpha = fade
    ctx.lineWidth = hot ? 2 : 1
    ctx.stroke()
    ctx.fillStyle = col
    ctx.fillText(shown, bx + w / 2, by + h / 2 + 0.5)
    tagBoxes.push({ i, x: bx, y: by, w, h })
  }

  /// the hill the tree stands on, its trunk, and a little grass
  function drawGround(s: Scene) {
    // under the trunk wherever it is mid-step, as wide as the tree is right now, so the two never drift apart
    let lo = Infinity
    let hi = -Infinity
    let root = -1
    let roots = 0
    for (let i = 0; i < s.nodes.length; i++) {
      lo = Math.min(lo, curX[i])
      hi = Math.max(hi, curX[i])
      if (s.nodes[i].parent < 0) {
        root = i
        roots++
      }
    }
    if (!(hi >= lo)) return
    const cx = roots === 1 ? curX[root] - 8 : (lo + hi) / 2
    const rx = Math.max(120, (hi - lo) / 2 + 40)
    ctx.fillStyle = C.hillBack
    ctx.beginPath()
    ctx.ellipse(cx, 30, rx, 16, 0, 0, Math.PI * 2)
    ctx.fill()
    ctx.fillStyle = C.hillFront
    ctx.beginPath()
    ctx.ellipse(cx - rx * 0.08, 36, rx * 0.83, 13, 0, 0, Math.PI * 2)
    ctx.fill()
    ctx.strokeStyle = C.grass
    ctx.lineWidth = 1.5
    ctx.lineCap = 'round'
    ctx.beginPath()
    for (const [f, dy] of [[-0.9, 28], [0.8, 32], [-0.3, 40]]) {
      const gx = cx + rx * f
      const gy = dy
      ctx.moveTo(gx, gy)
      ctx.quadraticCurveTo(gx + 2, gy - 12, gx + 7, gy - 16)
      ctx.moveTo(gx + 6, gy)
      ctx.quadraticCurveTo(gx + 9, gy - 8, gx + 15, gy - 10)
    }
    ctx.stroke()
    ctx.fillStyle = C.blossom
    ctx.beginPath()
    ctx.arc(cx - rx * 0.98, 24, 2.3, 0, Math.PI * 2)
    ctx.fill()
    ctx.fillStyle = C.g4
    ctx.globalAlpha = fade * 0.8
    ctx.beginPath()
    ctx.arc(cx + rx * 0.92, 28, 2.3, 0, Math.PI * 2)
    ctx.fill()
    ctx.globalAlpha = fade
    // each root grows out of the ground
    ctx.strokeStyle = C.bark
    ctx.globalAlpha = fade * 0.8
    ctx.lineWidth = 5
    for (let i = 0; i < s.nodes.length; i++) {
      if (s.nodes[i].parent >= 0) continue
      const x = curX[i]
      const y = curY[i]
      ctx.beginPath()
      ctx.moveTo(x - 16, 34)
      ctx.quadraticCurveTo(x - 16, y + 14, x, y)
      ctx.stroke()
    }
    ctx.globalAlpha = fade
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
    if (look === 'moss' || look === 'tree') return { bark: C.bark, knot: C.bark, vine: C.bark, bud: C.g4soft, paper: C.bg, ink: C.bark, step: C.g2, leaves: LEAF_TINTS }
    return GARDEN[theme.resolved === 'dark' ? 'dark' : 'light']
  }
  /// moss's reduction points are flowers, so they're the warm colour on a green tree
  /// the original tree view's gold: a ring around a soft core
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
    } else if (up) {
      drawLeaf(x, y, tilt, color, k)
      if (k < 0.45) return
      ctx.strokeStyle = 'rgba(255,255,255,0.55)'
      ctx.lineWidth = 0.8
      ctx.beginPath()
      ctx.moveTo(x + Math.sin(tilt) * 6, y - Math.cos(tilt) * 6)
      ctx.lineTo(x - Math.sin(tilt) * 6, y + Math.cos(tilt) * 6)
      ctx.stroke()
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
    if (up) {
      ctx.fillStyle = C.bark
      ctx.beginPath()
      ctx.arc(x, y, 4, 0, Math.PI * 2)
      ctx.fill()
      return
    }
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

  function drawApp(x: number, y: number, n: VNode, showText: boolean) {
    if (n.held) {
      // inside a binder still in place: a grey bud, like an application still waiting, since it can't run until the binder is gone
      ctx.strokeStyle = C.muted
      ctx.fillStyle = C.muted
      ctx.globalAlpha = fade * 0.55
      ctx.lineWidth = 1.3
      ctx.setLineDash([3, 3])
      ctx.beginPath()
      ctx.arc(x, y, up ? 8.5 : 6.5, 0, Math.PI * 2)
      ctx.stroke()
      ctx.setLineDash([])
      ctx.beginPath()
      ctx.arc(x, y, up ? 3 : 2.2, 0, Math.PI * 2)
      ctx.fill()
      ctx.globalAlpha = fade
      return
    }
    if (up) {
      // as on the home page: a dashed bud round a gold core, and once it can fire, a badge with its rule's letter
      ctx.strokeStyle = C.g4
      ctx.fillStyle = C.g4
      ctx.lineWidth = n === hover && n.rule ? 2.6 : 1.5
      ctx.setLineDash([3, 3])
      ctx.beginPath()
      ctx.arc(x, y, 8.5, 0, Math.PI * 2)
      ctx.stroke()
      ctx.setLineDash([])
      ctx.beginPath()
      ctx.arc(x, y, 3, 0, Math.PI * 2)
      ctx.fill()
      if (!n.rule && !isNext(n)) return
      const r = 7.5 * (isNext(n) ? 1 + 0.15 * bloom : 1)
      ctx.fillStyle = C.g4soft
      ctx.lineWidth = isNext(n) ? 2 : 1.2
      ctx.beginPath()
      ctx.arc(x, y, r, 0, Math.PI * 2)
      ctx.fill()
      ctx.stroke()
      if (showText) {
        ctx.fillStyle = flower().letter
        ctx.font = `700 ${8.5 * (r / 7.5)}px 'JetBrains Mono Variable', monospace`
        ctx.fillText(RULE_GLYPH[n.rule] || '@', x, y + 0.5)
      }
      return
    }
    const col = look === 'ink' ? (isNext(n) ? INK_RED : C.fg) : n.rule ? ruleColor(n.rule) : C.kw
    if (look === 'moss') {
      // as in the original tree view: a gold bud while it waits, a gold ring with the rule's
      // letter once it can fire, and a bigger, bolder ring on the one that fires next
      const fl = flower()
      if (n.rule || isNext(n)) {
        const r = isNext(n) ? 10 : 7.5
        ctx.fillStyle = fl.heart
        ctx.strokeStyle = fl.deep
        ctx.lineWidth = isNext(n) ? 2.2 : 1.2
        ctx.beginPath()
        ctx.arc(x, y, r, 0, Math.PI * 2)
        ctx.fill()
        ctx.stroke()
        if (showText) {
          ctx.fillStyle = fl.letter
          ctx.font = `600 ${isNext(n) ? 11 : 8.5}px 'JetBrains Mono Variable', monospace`
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
    if (isNext(n)) {
      if (look === 'bold') {
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
        // upright, so a rule's letter never reads as an (italic) variable
        ctx.font = look === 'ink' ? "700 11px Georgia, serif" : "700 12px 'JetBrains Mono Variable', monospace"
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
    if (up) {
      // a knot wearing its name, as the home page labels a named subtree
      ctx.fillStyle = C.bark
      ctx.beginPath()
      ctx.arc(x, y, 4, 0, Math.PI * 2)
      ctx.fill()
      if (showText) {
        ctx.fillStyle = called ? C.g4 : C.g2
        ctx.font = `${called ? 700 : 400} 10px 'JetBrains Mono Variable', monospace`
        ctx.fillText(op, x, y - 14)
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
    bloom = dur > 0 ? Math.max(0, Math.min(1, ((performance.now() - t0) / dur - 0.55) / 0.45)) : 1
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
    for (let i = 0; i < nodes.length; i++) inStep[i] = isNext(nodes[i]) || (nodes[i].parent >= 0 && inStep[nodes[i].parent]) ? 1 : 0
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
      return p >= 0 && isNext(nodes[p]) && nodes[p].kind === K_APP && nodes[g].side === 0
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
    // an edge to a new node comes in late, once its ends are nearly in place, so it never cuts across the old picture
    const late = dur > 0 ? Math.max(0, (ease(Math.min(1, (performance.now() - t0) / dur)) - 0.4) / 0.6) : 1
    const edgeA = (p: number, i: number) => (fresh[i] || fresh[p] ? late : 1)
    const keyAt = new Map<string, number>()
    if (roles) for (let i = 0; i < nodes.length; i++) keyAt.set(nodes[i].key, i)
    /// the drawn node for a key; a part folded to its name keeps what happens inside it to itself
    const nodeOf = (key: string) => keyAt.get(key) ?? -1
    const roleA = (input: string) => (roleFocus && roleFocus !== input ? 0.15 : 1)
    if (up) drawGround(shown)
    if (roles) {
      // the branches of the input in focus, the one being routed now unless another is picked, in its colour under the bark
      const shade = roleFocus ?? roles.active
      for (const [key, input] of roles.tint) {
        const i = keyAt.get(key)
        if (i === undefined || input !== shade) continue
        const p = nodes[i].parent
        ctx.strokeStyle = varColor(input)
        ctx.globalAlpha = fade * 0.32
        ctx.lineWidth = (up ? Math.max(lw, 1.6, 5.4 - (key.length - 2) * 0.85) : lw) + 8
        ctx.beginPath()
        if (p >= 0) curve(p, i)
        else {
          ctx.moveTo(curX[i], curY[i])
          if (up) ctx.quadraticCurveTo(curX[i] - 16, curY[i] + 14, curX[i] - 16, 30)
          else ctx.lineTo(curX[i], curY[i] - 26)
        }
        ctx.stroke()
      }
      ctx.globalAlpha = fade
    }
    if (up) {
      // every edge is a branch, thinning as it climbs
      ctx.strokeStyle = C.bark
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0) continue
        if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
        ctx.lineWidth = Math.max(lw, 1.6, 5.4 - (nodes[i].key.length - 2) * 0.85)
        ctx.globalAlpha = fade * 0.8 * edgeA(p, i)
        ctx.beginPath()
        curve(p, i)
        ctx.stroke()
      }
      ctx.globalAlpha = fade
    } else if (planty) {
      // branches taper with depth, so the eye reads the trunk first
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || nodes[p].kind === K_APP) continue
        if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
        ctx.strokeStyle = bark
        ctx.lineWidth = branchW(p)
        ctx.globalAlpha = fade * edgeA(p, i)
        ctx.beginPath()
        curve(p, i)
        ctx.stroke()
      }
    } else {
      ctx.lineWidth = look === 'ink' ? 1.2 * lw : lw
      ctx.strokeStyle = pen
      const a = fade * (look === 'ink' ? 0.8 : look === 'quiet' ? 0.75 : 1)
      for (const fresher of [false, true]) {
        ctx.globalAlpha = fresher ? a * late : a
        ctx.beginPath()
        for (let i = 0; i < nodes.length; i++) {
          const p = nodes[i].parent
          if (p < 0 || nodes[p].kind === K_APP || (edgeA(p, i) < 1) !== fresher) continue
          if (!vis(curX[p], curY[p]) && !vis(curX[i], curY[i])) continue
          curve(p, i)
        }
        ctx.stroke()
      }
    }
    if (!up) {
      ctx.strokeStyle = look === 'bold' ? C.kw : planty ? vine : look === 'ink' ? C.fg : C.muted
      const appA = fade * (look === 'bold' ? 0.55 : look === 'quiet' ? 0.4 : look === 'moss' ? 0.55 : 0.7)
      ctx.lineWidth = planty ? lw * 1.3 : lw
      for (const side of [0, 1])
        for (const fresher of [false, true]) {
          ctx.setLineDash(side === 1 ? (planty ? [1, 4] : look === 'ink' ? [5, 3] : [3, 3]) : [])
          ctx.globalAlpha = fresher ? appA * late : appA
          ctx.beginPath()
          for (let i = 0; i < nodes.length; i++) {
            const p = nodes[i].parent
            if (p < 0 || nodes[p].kind !== K_APP || nodes[i].side !== side || (edgeA(p, i) < 1) !== fresher) continue
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
          ctx.globalAlpha = fade * edgeA(p, i) * (calledComb(g) ? 1 : planty ? 0.6 : 0.75)
          ctx.lineWidth = planty ? branchW(p) : lw * (calledComb(g) ? 3 : 2.2)
          ctx.beginPath()
          curve(p, i)
          ctx.stroke()
        }
      ctx.globalAlpha = fade
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || !isNext(nodes[p]) || nodes[p].kind !== K_APP) continue
        ctx.strokeStyle = look === 'ink' ? INK_RED : ruleColor(nodes[p].rule)
        ctx.lineWidth = lw * 2
        ctx.beginPath()
        curve(p, i)
        ctx.stroke()
      }
    }

    /// a ghost or a falling variable, drawn at the origin
    const shape = (n: VNode) => {
      if (n.comb) drawComb(0, 0, n.comb, false, true, false)
      else if (n.kind === K_LAM) drawPill(0, 0, look === 'ink' ? `λ${n.text ?? ''}` : `{${n.text ?? ''}}`, varColor(n.text ?? ''), k, true)
      else if (n.kind === K_VAR) drawPill(0, 0, n.text ?? '', varColor(n.text ?? ''), k)
      else if (n.kind === K_LEAF) drawLeafAs(0, 0, tiltOf(n.key), LEAF_TINTS[1], k)
      else if (n.kind === K_STEM || n.kind === K_FORK) drawJoint(0, 0, n.kind === K_FORK)
      else if (n.kind === K_APP) drawApp(0, 0, { ...n, next: false }, false)
      else if (n.kind === K_POD && up) drawNamePod(0, 0, n.text ?? '', true)
      else if (n.kind === K_POD) {
        const text = n.text ?? ''
        ctx.strokeStyle = C.muted
        ctx.lineWidth = 1.2
        ctx.beginPath()
        ctx.roundRect(-podWidth(text) / 2, -10, podWidth(text), 20, 9)
        ctx.stroke()
        ctx.fillStyle = C.muted
        ctx.font = "11px 'JetBrains Mono Variable', monospace"
        ctx.fillText(text.length > 28 ? text.slice(0, 27) + '…' : text, 0, 0.5)
      }
    }
    if (ghosts.length > 0 && dur > 0) {
      const all = Math.min(1, (performance.now() - t0) / dur)
      const p = tw.acts ? actT(all) : all
      const at = ghosts.map((_, j) => ghostAt(tw, j, all, curX, curY, up))
      ctx.strokeStyle = planty || up ? bark : pen
      ctx.lineWidth = planty || up ? 2.4 : lw
      ghosts.forEach((g, j) => {
        if (g.parent < 0) return
        // what falls away lets go of its parent at once, so no line trails after it
        // an edge goes as soon as either end does
        if (g.hit >= 0 && (g.launched || ghosts[g.parent].hit < 0)) return
        const a = Math.min(at[j].a, at[g.parent].a)
        ctx.globalAlpha = fade * (g.drop && !ghosts[g.parent].drop ? Math.min(a, Math.max(0, 1 - p / 0.25)) : a) * 0.7
        ctx.beginPath()
        ctx.moveTo(at[g.parent].x, at[g.parent].y)
        ctx.lineTo(at[j].x, at[j].y)
        ctx.stroke()
      })
      ghosts.forEach((g, j) => {
        const { x, y, a, scale } = at[j]
        if (g.launched || !vis(x, y)) return
        ctx.save()
        fade = a
        ctx.globalAlpha = fade
        ctx.translate(x, y)
        ctx.scale(scale, scale)
        shape(g.n)
        ctx.restore()
        fade = 1
      })
      ctx.globalAlpha = 1
    }
    if (falls.length) {
      const now = performance.now()
      const secs = falls.map((b) => (now - b.t) / 1000 / b.scale)
      // falls are measured from the trunk, which the view holds still
      const root = nodes.findIndex((n) => n.parent < 0)
      const ox = root >= 0 ? curX[root] : 0
      const oy = root >= 0 ? curY[root] : 0
      const at = falls.map((b, j) => {
        const p = fallAt(b.f, secs[j])
        return { ...p, x: p.x + ox, y: p.y + oy }
      })
      ctx.strokeStyle = planty || up ? bark : pen
      ctx.lineWidth = planty || up ? 2.4 : lw
      falls.forEach((b, j) => {
        if (b.parent < 0) return
        ctx.globalAlpha = fallAlpha(b.f, secs[j]) * 0.7
        ctx.beginPath()
        ctx.moveTo(at[b.parent].x, at[b.parent].y)
        ctx.lineTo(at[j].x, at[j].y)
        ctx.stroke()
      })
      falls.forEach((b, j) => {
        const p = at[j]
        if (!vis(p.x, p.y)) return
        ctx.save()
        fade = fallAlpha(b.f, secs[j])
        ctx.globalAlpha = fade
        ctx.translate(p.x, p.y)
        spin = b.turns ? p.angle : 0
        shape(b.n)
        spin = 0
        ctx.restore()
        fade = 1
      })
      ctx.globalAlpha = 1
    }

    const hi = hover ? nodes.indexOf(hover) : -1
    const flow = hi >= 0 && !hoverTag ? argFlow(nodes, hi) : null
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
      if (isNext(n) && n.kind !== K_APP && (look === 'moss' || up)) {
        // the same gold ring the next application gets, stretched round a name
        const named = n.kind === K_LAM || n.kind === K_VAR
        const text = n.kind === K_LAM ? `{${n.text ?? ''}}` : (n.text ?? '')
        ctx.strokeStyle = flower().deep
        ctx.globalAlpha = fade * bloom
        if (n.kind === K_LAM) pillRing(x, y, text, k, up ? 2.4 : 2.6)
        else {
          const half = named ? (podWidth(text) * pillScale(k)) / 2 + 5 : 11
          ctx.lineWidth = up ? 2 : 2.2
          ctx.beginPath()
          ctx.roundRect(x - half, y - 14, half * 2, 28, 14)
          ctx.stroke()
        }
        ctx.globalAlpha = fade
      }
      else if (isNext(n) && n.kind !== K_APP) {
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
          if (n.rule && !isNext(n) && look !== 'ink') {
            // a binder ready to come off is a reduction point like any other: a gold ring
            ctx.strokeStyle = look === 'moss' || up ? flower().deep : C.accent
            pillRing(x, y, `{${n.text ?? ''}}`, k, up ? 1.4 : 1.8)
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
          if (up) {
            drawNamePod(x, y, text, showText, !!glow && glow === text)
            break
          }
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
        if (up && n.kind === K_VAR) ctx.arc(x, y, 13, 0, Math.PI * 2)
        else if (n.kind === K_POD) {
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
    roleBoxes = []
    if (roles) {
      const per = new Map<number, number>()
      for (const m of roles.marks) {
        const i = nodeOf(m.key)
        if (i < 0) continue
        const j = per.get(i) ?? 0
        per.set(i, j + 1)
        const x = curX[i] + 15 + j * 21
        const y = curY[i] + 7
        if (!vis(x, y)) continue
        ctx.globalAlpha = roleA(m.input)
        drawRoleMark(x, y, m.kind, varColor(m.input), hoverRole?.mark === m, !!m.now)
        roleBoxes.push({ x, y, r: 10, hit: { mark: m } })
      }
      for (const [key, all] of roles.holds) {
        const i = nodeOf(key)
        if (i < 0) continue
        // an input already hanging there has landed
        const inputs = all.filter((input) => !nodes.some((n) => n.parent === i && n.kind === K_VAR && n.text === input))
        if (inputs.length === 0) continue
        inputs.forEach((input, j) => {
          const x = curX[i] + 16 + j * 17
          const y = curY[i] - 22 - j * 4
          ctx.globalAlpha = roleA(input)
          drawHeld(curX[i], curY[i], x, y, varColor(input), hoverRole?.hold?.key === key)
        })
        roleBoxes.push({ x: curX[i] + 16 + ((inputs.length - 1) * 17) / 2, y: curY[i] - 22, r: 9 + inputs.length * 8, hit: { hold: { key, inputs } } })
      }
      ctx.globalAlpha = 1
    }
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
    tagBoxes = []
    if (showText) {
      for (let i = 0; i < nodes.length; i++) {
        const n = nodes[i]
        if (!n.tag || !vis(curX[i], curY[i])) continue
        fade = fresh[i] && dur > 0 ? Math.max(0.15, ease(Math.min(1, (performance.now() - t0) / dur))) : 1
        drawTag(i, curX[i], curY[i], n.tag, n === hover && hoverTag)
      }
      fade = 1
    }
    for (const t of shown.titles) {
      ctx.fillStyle = t.bad ? C.err : C.muted
      ctx.font = "600 12px 'Inter Variable', sans-serif"
      ctx.fillText(t.text, t.x, up ? 62 : -28)
    }
  }

  // ---- input ----------------------------------------------------------------
  /// the last hit test landed on a name tag
  let tagHit = false
  let roleBoxes: { x: number; y: number; r: number; hit: RoleHit }[] = []
  let hoverRole: RoleHit | null = null
  function roleAt(sx: number, sy: number): RoleHit | null {
    const wx = (sx - cam.x) / cam.k
    const wy = (sy - cam.y) / cam.k
    for (const b of roleBoxes) if (Math.hypot(wx - b.x, wy - b.y) <= b.r + 2 / cam.k) return b.hit
    return null
  }
  function hitTest(sx: number, sy: number): VNode | null {
    if (!shown) return null
    const wx = (sx - cam.x) / cam.k
    const wy = (sy - cam.y) / cam.k
    const slop = Math.max(12, 6 / cam.k)
    let best: VNode | null = null
    let bestD = Infinity
    const nodes = shown.nodes
    tagHit = false
    for (const b of tagBoxes) {
      if (wx >= b.x - 2 && wx <= b.x + b.w + 2 && wy >= b.y - 2 && wy <= b.y + b.h + 2 && nodes[b.i]) {
        tagHit = true
        return nodes[b.i]
      }
    }
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
      const r = roleAt(p.x, p.y)
      if (r !== hoverRole && (r?.mark !== hoverRole?.mark || r?.hold?.key !== hoverRole?.hold?.key)) {
        hoverRole = r
        onRoleHover?.(r)
        requestDraw()
      }
      if (r) {
        cv.style.cursor = r.mark ? 'pointer' : 'default'
        if (hover) {
          hover = null
          hoverId = -1
          onHover?.(null)
        }
        return
      }
      const n = hitTest(p.x, p.y)
      if (n !== hover || tagHit !== hoverTag) {
        hover = n
        hoverTag = tagHit
        hoverId = n?.id ?? -1
        cv.style.cursor = n && (tagHit || !n.held) ? 'pointer' : n ? 'default' : 'grab'
        onHover?.(n, tagHit)
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
      const r = roleAt(p.x, p.y)
      if (r) return void onRoleClick?.(r)
      const n = hitTest(p.x, p.y)
      if (n) onClick?.(n, e, tagHit)
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
      refit(shown)
      Object.assign(cam, target)
      // a step under way keeps the trunk where the new view puts it, rather than finishing on the old view's zoom
      if (pin) pin = pinAt(target, target, shown)
    }
    // resizing clears the canvas; drawing now (not next frame) avoids a blank frame
    draw()
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
        if (shown) refit(shown)
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
    void roles
    void roleFocus
    requestDraw()
  })
  // the page eases its palette over --mix's transition; the canvas follows it frame by frame
  $effect(() => {
    void theme.mix
    if (ctx) return untrack(() => followTheme(readColors))
  })
  // turning to or from the home page's look flips the tree over, so it comes in fresh
  let wasUp = false
  $effect(() => {
    const flip = up !== wasUp
    wasUp = up
    untrack(() => {
      if (flip && ctx) {
        shown = null
        follow = true
        setScene(scene)
      } else requestDraw()
    })
  })
  $effect(() => {
    if (steady) return
    untrack(() => {
      if (follow && shown && ctx) {
        refit(shown)
        requestDraw()
      }
    })
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
      if (hoverRole) {
        hoverRole = null
        onRoleHover?.(null)
        requestDraw()
      }
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
