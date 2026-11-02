<script lang="ts">
  // The tree drawing: a canvas with pan, zoom and fit that glides between
  // scenes. Nodes keep identity by path and by subtree id, so a kept subtree
  // visibly travels to its new place and discarded leaves fall away.
  import { onMount, untrack } from 'svelte'
  import { theme } from '$lib/theme.svelte'
  import { K_LEAF, K_STEM, K_FORK, K_APP, K_VAR, K_POD, K_LAM, type Scene, type VNode } from './layout.ts'
  import { R_K, R_S, R_F, RULE_GLYPH, type Id } from './store.ts'

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
    /// discarded leaves fall away between scenes
    falling?: boolean
    /// the colour a variable or its binder is drawn in
    varColor?: (name: string) => string
    onHover?: (n: VNode | null) => void
    onClick?: (n: VNode, e: MouseEvent) => void
    api?: (a: CanvasApi) => void
  }
  let { scene, animMs = 0, glow = null, falling = false, varColor = () => '#1971c2', onHover, onClick, api }: Props = $props()

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

  type Debris = { x: number; y: number; vx: number; vy: number; rot: number; vr: number; life: number; kind: number; text: string | null; tilt: number }
  let debris: Debris[] = []
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
      probe.style.color = `var(${v})`
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
      border: get('--border-strong')
    }
    probe.remove()
    requestDraw()
  }

  const LEAF_TINTS = ['#6dbd7e', '#4aa96c', '#8fce9d', '#3d9b74', '#5cb787']
  const ruleColor = (r: number) => (r === R_K ? C.warn : r === R_S ? C.kw : r === R_F ? C.err : C.accent)
  const combColor = (op: string | null) => (op === 'S' ? C.kw : op === 'K' ? C.warn : op === 'I' ? C.ok : C.err)
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

  function drawPill(x: number, y: number, text: string, col: string, k: number) {
    const s = pillScale(k)
    ctx.font = `700 ${12 * s}px 'JetBrains Mono Variable', monospace`
    const shown = text.length > 16 ? text.slice(0, 15) + '…' : text
    const pw = Math.max(22 * s, ctx.measureText(shown).width + 14 * s)
    ctx.fillStyle = col
    ctx.strokeStyle = C.bg
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.roundRect(x - pw / 2, y - 11 * s, pw, 22 * s, 11 * s)
    ctx.fill()
    ctx.stroke()
    if (k * s > 0.2) {
      ctx.fillStyle = '#fff'
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
    if (tween && prev) {
      const byKey = new Map<string, number>()
      const byId = new Map<Id, number>()
      prev.nodes.forEach((p, i) => {
        byKey.set(p.key, i)
        if (!byId.has(p.id)) byId.set(p.id, i)
      })
      const survivors = new Set<Id>()
      for (let i = 0; i < n; i++) {
        const v = next.nodes[i]
        survivors.add(v.id)
        const k = byKey.get(v.key)
        let from = k !== undefined && prev.nodes[k].id === v.id ? k : byId.get(v.id)
        from ??= k
        if (from !== undefined) {
          nx[i] = curX[from] ?? v.x
          ny[i] = curY[from] ?? v.y
        } else if (v.parent >= 0) {
          nx[i] = nx[v.parent]
          ny[i] = ny[v.parent]
          fresh[i] = 1
        } else {
          nx[i] = v.x
          ny[i] = v.y
        }
      }
      if (falling && prev.nodes.length < 900) {
        prev.nodes.forEach((p, i) => {
          if (survivors.has(p.id) || (p.kind !== K_LEAF && p.kind !== K_VAR && p.kind !== K_POD)) return
          debris.push({
            x: curX[i],
            y: curY[i],
            vx: (tiltOf(p.key) || 0.1) * 26,
            vy: -20,
            rot: tiltOf(p.key),
            vr: tiltOf(p.key) * 3,
            life: 1,
            kind: p.kind,
            text: p.text,
            tilt: tiltOf(p.key)
          })
        })
        if (debris.length > 240) debris = debris.slice(-240)
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
    if (debris.length > 0) {
      for (const d of debris) {
        d.vy += 620 * dt
        d.x += d.vx * dt
        d.y += d.vy * dt
        d.rot += d.vr * dt
        d.life -= dt * 1.3
      }
      debris = debris.filter((d) => d.life > 0)
      again = true
    }
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

    // a combinator's own nodes sit in one tinted band; the one about to be called is darker
    for (let g = 0; g < nodes.length; g++) {
      const head = nodes[g]
      if (!head.comb) continue
      const p = head.parent
      const called = p >= 0 && nodes[p].next && nodes[p].kind === K_APP && head.side === 0
      ctx.strokeStyle = combColor(head.comb)
      ctx.globalAlpha = called ? 0.38 : 0.2
      ctx.lineWidth = 24
      ctx.lineJoin = 'round'
      ctx.lineCap = 'round'
      ctx.beginPath()
      ctx.moveTo(curX[g], curY[g])
      ctx.lineTo(curX[g], curY[g])
      for (let i = g + 1; i < nodes.length; i++) {
        const q = nodes[i].parent
        if (nodes[i].group !== g || q < 0) continue
        ctx.moveTo(curX[q], curY[q])
        ctx.quadraticCurveTo(curX[q] + (curX[i] - curX[q]) * 0.5, curY[q] + (curY[i] - curY[q]) * 0.62, curX[i], curY[i])
      }
      ctx.stroke()
    }
    ctx.globalAlpha = 1

    // edges: the application's function side is solid, its argument dotted
    const lw = Math.max(1.1, 1.5 / Math.sqrt(k)) / Math.max(1, 1 / k / 2)
    ctx.lineCap = 'round'
    ctx.lineWidth = lw
    ctx.strokeStyle = C.edge
    ctx.beginPath()
    for (let i = 0; i < nodes.length; i++) {
      const p = nodes[i].parent
      if (p < 0 || nodes[p].kind === K_APP) continue
      const x1 = curX[p]
      const y1 = curY[p]
      const x2 = curX[i]
      const y2 = curY[i]
      if (!vis(x1, y1) && !vis(x2, y2)) continue
      ctx.moveTo(x1, y1)
      ctx.quadraticCurveTo(x1 + (x2 - x1) * 0.5, y1 + (y2 - y1) * 0.62, x2, y2)
    }
    ctx.stroke()
    ctx.strokeStyle = C.kw
    ctx.globalAlpha = 0.55
    for (const side of [0, 1]) {
      ctx.setLineDash(side === 1 ? [3, 3] : [])
      ctx.beginPath()
      for (let i = 0; i < nodes.length; i++) {
        const p = nodes[i].parent
        if (p < 0 || nodes[p].kind !== K_APP || nodes[i].side !== side) continue
        const x1 = curX[p]
        const y1 = curY[p]
        const x2 = curX[i]
        const y2 = curY[i]
        if (!vis(x1, y1) && !vis(x2, y2)) continue
        ctx.moveTo(x1, y1)
        ctx.quadraticCurveTo(x1 + (x2 - x1) * 0.5, y1 + (y2 - y1) * 0.62, x2, y2)
      }
      ctx.stroke()
    }
    ctx.setLineDash([])
    ctx.globalAlpha = 1
    for (let i = 0; i < nodes.length; i++) {
      const p = nodes[i].parent
      if (p < 0 || !nodes[p].next || nodes[p].kind !== K_APP) continue
      ctx.strokeStyle = ruleColor(nodes[p].rule)
      ctx.lineWidth = lw * 2
      ctx.beginPath()
      ctx.moveTo(curX[p], curY[p])
      ctx.quadraticCurveTo(curX[p] + (curX[i] - curX[p]) * 0.5, curY[p] + (curY[i] - curY[p]) * 0.62, curX[i], curY[i])
      ctx.stroke()
    }

    for (const d of debris) {
      ctx.globalAlpha = Math.max(0, d.life)
      if (d.kind === K_LEAF) drawLeaf(d.x, d.y, d.rot, LEAF_TINTS[1], k)
      else if (d.kind === K_VAR && d.text) drawPill(d.x, d.y, d.text, varColor(d.text), k)
      else {
        ctx.fillStyle = C.muted
        ctx.beginPath()
        ctx.arc(d.x, d.y, 6, 0, Math.PI * 2)
        ctx.fill()
      }
    }
    ctx.globalAlpha = 1

    const showText = k >= 0.38
    ctx.textAlign = 'center'
    ctx.textBaseline = 'middle'
    for (let i = 0; i < nodes.length; i++) {
      const n = nodes[i]
      const x = curX[i]
      const y = curY[i]
      if (!vis(x, y)) continue
      const hot = n.id === hoverId
      if (n.diff && n.kind !== K_STEM && n.kind !== K_FORK) {
        ctx.fillStyle = C.err
        ctx.globalAlpha = 0.22
        ctx.beginPath()
        ctx.arc(x, y, 16, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = 1
      }
      if (n.next && n.kind !== K_APP) {
        ctx.fillStyle = C.accent
        ctx.globalAlpha = 0.22
        ctx.beginPath()
        ctx.arc(x, y, 19, 0, Math.PI * 2)
        ctx.fill()
        ctx.globalAlpha = 1
      }
      if (n.comb) {
        const col = combColor(n.comb)
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
          ctx.fillText(n.comb, x, y + 0.5)
        }
      } else switch (n.kind) {
        case K_LAM: {
          drawPill(x, y, `{${n.text ?? ''}}`, varColor(n.text ?? ''), k)
          break
        }
        case K_LEAF: {
          const key = n.key
          let hh = 7
          for (let c = key.length - 1; c >= Math.max(0, key.length - 6); c--) hh = (hh * 31 + key.charCodeAt(c)) | 0
          drawLeaf(x, y, tiltOf(key), LEAF_TINTS[(hh >>> 0) % LEAF_TINTS.length], k)
          break
        }
        case K_STEM:
          ctx.fillStyle = C.bg
          ctx.strokeStyle = C.muted
          ctx.lineWidth = 1.6
          ctx.beginPath()
          ctx.arc(x, y, 4.2, 0, Math.PI * 2)
          ctx.fill()
          ctx.stroke()
          break
        case K_FORK:
          ctx.fillStyle = C.muted
          ctx.beginPath()
          ctx.arc(x, y, 4.6, 0, Math.PI * 2)
          ctx.fill()
          break
        case K_APP: {
          const col = n.rule ? ruleColor(n.rule) : C.kw
          ctx.fillStyle = C.bg
          ctx.strokeStyle = col
          if (n.next) {
            ctx.fillStyle = col
            ctx.globalAlpha = 0.22
            ctx.beginPath()
            ctx.arc(x, y, 21, 0, Math.PI * 2)
            ctx.fill()
            ctx.globalAlpha = 1
            ctx.fillStyle = C.bg
            ctx.lineWidth = 2.6
            ctx.beginPath()
            ctx.arc(x, y, 12, 0, Math.PI * 2)
            ctx.fill()
            ctx.stroke()
            if (showText) {
              ctx.fillStyle = col
              ctx.font = "700 12px 'JetBrains Mono Variable', monospace"
              ctx.fillText(RULE_GLYPH[n.rule] || '@', x, y + 0.5)
            }
          } else {
            ctx.lineWidth = 1.4
            ctx.globalAlpha = n.rule ? 1 : 0.65
            ctx.beginPath()
            ctx.arc(x, y, 5.5, 0, Math.PI * 2)
            ctx.fill()
            ctx.stroke()
            ctx.globalAlpha = 1
          }
          break
        }
        case K_VAR:
          drawPill(x, y, n.text ?? '', varColor(n.text ?? ''), k)
          break
        case K_POD: {
          const text = n.text ?? ''
          const pw = podWidth(text)
          const col = podColor(n.podKind)
          ctx.fillStyle = C.bg
          ctx.strokeStyle = col
          ctx.lineWidth = glow && glow === text ? 2.6 : 1.3
          ctx.beginPath()
          ctx.roundRect(x - pw / 2, y - 10, pw, 20, 9)
          ctx.fill()
          ctx.globalAlpha = glow && glow === text ? 0.3 : 0.1
          ctx.fillStyle = col
          ctx.fill()
          ctx.globalAlpha = 1
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
