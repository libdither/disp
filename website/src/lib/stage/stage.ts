// A WebGL stage for drawings made of keyed nodes: shapes, text and curved strokes, each placed by the node
// it belongs to. `show` hands it a whole drawing; nodes it has seen before glide from where they are, new
// ones grow from the nearest node they came from, and nodes that are gone shrink where they stand, all in
// the shaders (shaders.ts), so a frame costs the CPU nothing between drawings. A drawing is only
// positions for keys, so a tree, a graph or a lattice of the same keys is a new set of targets, and the
// move between them is the same glide.

import {
  DataTexture,
  FloatType,
  Group,
  InstancedBufferAttribute,
  InstancedBufferGeometry,
  Float32BufferAttribute,
  Mesh,
  NearestFilter,
  OrthographicCamera,
  RGBAFormat,
  Scene,
  ShaderMaterial,
  Vector3,
  WebGLRenderer,
  type IUniform
} from 'three'
import { Atlas, ATLAS_PX } from './atlas'
import { GLYPH_FRAG, GLYPH_VERT, LABEL_FRAG, LABEL_VERT, STROKE_FRAG, STROKE_VERT } from './shaders'

export type RGBA = [number, number, number, number]

export interface NodeSpec {
  key: string
  x: number
  y: number
  z?: number
  /** The node a new one grows from: the nearest of these up the chain that was on stage before. */
  parent?: string | null
  /** Seconds a new node waits before it grows in. */
  delay?: number
  /** How near the pointer must come to pick it; 0 or none, not pickable. */
  hit?: number
}
export interface GlyphSpec {
  node: string
  /** 0 a disc, 1 a leaf (shaders.ts SHAPE). */
  shape?: number
  r: number
  dx?: number
  dy?: number
  fill: RGBA
  stroke?: RGBA
  sw?: number
  /** Dashes around the stroke; 0 for a solid one. */
  dashes?: number
  rot?: number
  flags?: number
}
export interface LabelSpec {
  node: string
  text: string
  size: number
  weight?: number
  color: RGBA
  dx?: number
  dy?: number
  anchor?: 'start' | 'middle' | 'end'
  baseline?: 'alphabetic' | 'middle'
  flags?: number
}
export interface StrokeSpec {
  a: string
  b: string
  width: number
  color: RGBA
  /** Dash period and the part of it drawn. */
  dash?: [number, number]
  /** Where the curve's control point sits between its ends, as a fraction of each axis. */
  bend?: [number, number]
}
export interface Drawing {
  nodes: NodeSpec[]
  glyphs: GlyphSpec[]
  labels: LabelSpec[]
  strokes: StrokeSpec[]
  /** The box that is fitted to the canvas, whole and centred. */
  view: { x0: number; y0: number; x1: number; y1: number }
}
/** The part of the drawing's box the canvas covers, in CSS pixels of the box (which is fullW by fullH),
 * and how many device pixels it gets for each: the whole box, or only the part a pinch zoom shows,
 * drawn as sharp as the zoom makes it. */
export interface Pane {
  fullW: number
  fullH: number
  x: number
  y: number
  w: number
  h: number
  ratio: number
}
/** Seconds each change takes; 0 for none. */
export interface Timing {
  move: number
  grow: number
  shrink: number
}

const SEGS = 10
const TEX_W = 1024
const easeMove = (t: number) => {
  const u = t - 1
  return 1 + 1.9 * u * u * u + 0.9 * u * u
}

interface Live {
  from: [number, number, number]
  to: [number, number, number]
  /** 0 staying, 1 growing in, 2 shrinking away. */
  mode: number
  /** Growing: its delay; shrinking: when it began, from the last drawing's start. */
  t: number
  hit: number
  parent: string | null
  glyphs: GlyphSpec[]
  labels: LabelSpec[]
  stroke: StrokeSpec | null
}

export class Stage {
  readonly renderer: WebGLRenderer
  readonly scene = new Scene()
  readonly camera = new OrthographicCamera(-1, 1, 1, -1, -1000, 1000)
  private pivot = new Group()
  private inner = new Group()
  private atlas: Atlas
  private uniforms: Record<string, IUniform>
  private glyphMat: ShaderMaterial
  private labelMat: ShaderMaterial
  private strokeMat: ShaderMaterial
  private meshes: Mesh[] = []
  private nodeTex: DataTexture | null = null
  private live = new Map<string, Live>()
  private index = new Map<string, number>()
  private keys: string[] = []
  private moveStart = 0
  private timing: Timing = { move: 0, grow: 0, shrink: 0 }
  private view = { x0: 0, y0: 0, x1: 1, y1: 1 }
  private t0 = performance.now()
  private raf = 0
  private running = false
  private visible = true
  private hovered: string | null = null
  private win: Pane = { fullW: 1, fullH: 1, x: 0, y: 0, w: 1, h: 1, ratio: 1 }
  private interObs: IntersectionObserver
  /** The angle (radians) the whole drawing leans by at a time, about `swayAt`; none, upright. */
  sway: ((t: number) => number) | null = null
  swayAt: [number, number] = [0, 0]

  constructor(readonly canvas: HTMLCanvasElement, fontFamily: string) {
    this.renderer = new WebGLRenderer({ canvas, antialias: false, alpha: true, premultipliedAlpha: true })
    this.renderer.setClearColor(0x000000, 0)
    this.atlas = new Atlas(fontFamily)
    this.uniforms = {
      uNodes: { value: null },
      uNodesW: { value: TEX_W },
      uNow: { value: 0 },
      uMoveStart: { value: 0 },
      uMoveDur: { value: 0 },
      uInDur: { value: 0 },
      uOutDur: { value: 0 },
      uHover: { value: -1 },
      uBg: { value: new Vector3(1, 1, 1) },
      uAtlas: { value: this.atlas.texture }
    }
    const mat = (vertexShader: string, fragmentShader: string) =>
      new ShaderMaterial({ vertexShader, fragmentShader, uniforms: this.uniforms, transparent: true, depthTest: false, depthWrite: false, premultipliedAlpha: true })
    this.strokeMat = mat(STROKE_VERT, STROKE_FRAG)
    this.glyphMat = mat(GLYPH_VERT, GLYPH_FRAG)
    this.labelMat = mat(LABEL_VERT, LABEL_FRAG)
    this.scene.add(this.pivot)
    this.pivot.add(this.inner)
    this.camera.position.z = 10
    this.interObs = new IntersectionObserver((es) => {
      this.visible = es.some((e) => e.isIntersecting)
      this.wake()
    })
    this.interObs.observe(canvas)
    document.addEventListener('visibilitychange', this.wake)
  }

  private now = () => (performance.now() - this.t0) / 1000

  /** Strokes fade toward this colour (sRGB, 0 to 1): what they are drawn over. */
  setBackground(rgb: [number, number, number]) {
    ;(this.uniforms.uBg.value as Vector3).set(rgb[0], rgb[1], rgb[2])
    this.wake()
  }

  /** The font changed or loaded: every string is drawn again. */
  refont(family: string) {
    this.atlas.texture.dispose()
    this.atlas = new Atlas(family)
    this.uniforms.uAtlas.value = this.atlas.texture
  }

  setHover(key: string | null) {
    this.hovered = key
    this.uniforms.uHover.value = key == null ? -1 : (this.index.get(key) ?? -1)
    this.wake()
  }

  /** Where a node is now, and how grown in, worked out as the shaders do. */
  private place(l: Live, now: number): { p: [number, number, number]; life: number } {
    const k = this.timing.move > 0 ? Math.min(1, Math.max(0, (now - this.moveStart) / this.timing.move)) : 1
    const e = easeMove(k)
    const p: [number, number, number] = [0, 1, 2].map((i) => l.from[i] + (l.to[i] - l.from[i]) * e) as [number, number, number]
    const cubic = (t: number) => (t - 1) ** 3 + 1
    const clamp = (t: number) => Math.min(1, Math.max(0, t))
    let life = 1
    if (l.mode === 1) life = this.timing.grow > 0 ? cubic(clamp((now - this.moveStart - l.t) / this.timing.grow)) : 1
    else if (l.mode === 2) life = this.timing.shrink > 0 ? 1 - cubic(clamp((now - this.moveStart - l.t) / this.timing.shrink)) : 0
    return { p, life }
  }

  show(d: Drawing, timing: Timing) {
    const now = this.now()
    const before = new Map<string, { p: [number, number, number]; life: number; l: Live }>()
    for (const [k, l] of this.live) before.set(k, { ...this.place(l, now), l })
    const next = new Map<string, Live>()
    const specs = new Map(d.nodes.map((n) => [n.key, n]))
    const strokeTo = new Map(d.strokes.map((s) => [s.b, s]))
    const glyphsOf = new Map<string, GlyphSpec[]>()
    const labelsOf = new Map<string, LabelSpec[]>()
    for (const g of d.glyphs) (glyphsOf.get(g.node) ?? glyphsOf.set(g.node, []).get(g.node)!).push(g)
    for (const t of d.labels) (labelsOf.get(t.node) ?? labelsOf.set(t.node, []).get(t.node)!).push(t)
    const origin = (n: NodeSpec): [number, number, number] | null => {
      for (let p = n.parent ?? null; p != null; p = specs.get(p)?.parent ?? null) {
        const b = before.get(p)
        if (b && b.l.mode !== 2) return b.p
      }
      return null
    }
    for (const n of d.nodes) {
      const to: [number, number, number] = [n.x, n.y, n.z ?? 0]
      const b = before.get(n.key)
      const common = { to, hit: n.hit ?? 0, parent: n.parent ?? null, glyphs: glyphsOf.get(n.key) ?? [], labels: labelsOf.get(n.key) ?? [], stroke: strokeTo.get(n.key) ?? null }
      if (b && b.life > 0 && (b.l.mode !== 2 || b.life > 0.05)) next.set(n.key, { ...common, from: b.p, mode: 0, t: 0 })
      else next.set(n.key, { ...common, from: origin(n) ?? to, mode: 1, t: n.delay ?? 0 })
    }
    // what is gone shrinks where it stands, keeping what it drew
    for (const [k, b] of before) {
      if (next.has(k) || b.life <= 0.001) continue
      const t = b.l.mode === 2 ? b.l.t + this.moveStart - now : 0
      next.set(k, { ...b.l, from: b.p, to: b.p, mode: 2, t, hit: 0 })
    }
    this.live = next
    this.moveStart = now
    this.timing = timing
    this.view = d.view
    this.build(now)
    this.fit()
    this.wake()
  }

  private build(now: number) {
    this.keys = [...this.live.keys()]
    this.index = new Map(this.keys.map((k, i) => [k, i]))
    const n = this.keys.length
    const rows = Math.max(1, Math.ceil((2 * n) / TEX_W))
    const data = new Float32Array(TEX_W * rows * 4)
    this.keys.forEach((k, i) => {
      const l = this.live.get(k)!
      data.set([l.from[0], l.from[1], l.from[2], l.mode, l.to[0], l.to[1], l.to[2], l.t], 8 * i)
    })
    this.nodeTex?.dispose()
    this.nodeTex = new DataTexture(data, TEX_W, rows, RGBAFormat, FloatType)
    this.nodeTex.magFilter = this.nodeTex.minFilter = NearestFilter
    this.nodeTex.needsUpdate = true
    this.uniforms.uNodes.value = this.nodeTex
    this.uniforms.uMoveStart.value = now
    this.uniforms.uMoveDur.value = this.timing.move
    this.uniforms.uInDur.value = this.timing.grow
    this.uniforms.uOutDur.value = this.timing.shrink
    this.uniforms.uHover.value = this.hovered == null ? -1 : (this.index.get(this.hovered) ?? -1)

    for (const m of this.meshes) {
      this.inner.remove(m)
      m.geometry.dispose()
    }
    this.meshes = []
    const lives = this.keys.map((k) => this.live.get(k)!)
    const strokes = lives.flatMap((l) => (l.stroke && this.index.has(l.stroke.a) ? [l.stroke] : []))
    const glyphs = lives.flatMap((l) => l.glyphs)
    let gen = this.atlas.generation
    let labels = this.labelData(lives.flatMap((l) => l.labels))
    if (this.atlas.generation !== gen) labels = this.labelData(lives.flatMap((l) => l.labels))
    this.atlas.flush()
    this.add(this.strokeData(strokes), this.strokeMat)
    this.add(this.glyphData(glyphs), this.glyphMat)
    this.add(labels, this.labelMat)
  }

  private add(g: InstancedBufferGeometry, m: ShaderMaterial) {
    const mesh = new Mesh(g, m)
    mesh.frustumCulled = false
    mesh.renderOrder = this.meshes.length
    this.inner.add(mesh)
    this.meshes.push(mesh)
  }

  private quad(xs: number[], ys: number[]): InstancedBufferGeometry {
    const g = new InstancedBufferGeometry()
    g.setAttribute('position', new Float32BufferAttribute([xs[0], ys[0], 0, xs[1], ys[0], 0, xs[1], ys[1], 0, xs[0], ys[1], 0], 3))
    g.setIndex([0, 1, 2, 0, 2, 3])
    return g
  }
  private attr(g: InstancedBufferGeometry, name: string, data: Float32Array, size: number) {
    g.setAttribute(name, new InstancedBufferAttribute(data, size))
  }

  private glyphData(gs: GlyphSpec[]): InstancedBufferGeometry {
    const g = this.quad([-1, 1], [-1, 1])
    const n = gs.length
    const node = new Float32Array(n), off = new Float32Array(2 * n), r = new Float32Array(n), shape = new Float32Array(n)
    const fill = new Float32Array(4 * n), stroke = new Float32Array(4 * n), param = new Float32Array(4 * n)
    gs.forEach((s, i) => {
      node[i] = this.index.get(s.node)!
      off[2 * i] = s.dx ?? 0
      off[2 * i + 1] = s.dy ?? 0
      r[i] = s.r
      shape[i] = s.shape ?? 0
      fill.set(s.fill, 4 * i)
      stroke.set(s.stroke ?? [0, 0, 0, 0], 4 * i)
      param.set([s.sw ?? 0, s.dashes ?? 0, s.rot ?? 0, s.flags ?? 0], 4 * i)
    })
    this.attr(g, 'aNode', node, 1)
    this.attr(g, 'aOff', off, 2)
    this.attr(g, 'aR', r, 1)
    this.attr(g, 'aShape', shape, 1)
    this.attr(g, 'aFill', fill, 4)
    this.attr(g, 'aStroke', stroke, 4)
    this.attr(g, 'aParam', param, 4)
    g.instanceCount = n
    return g
  }

  private labelData(ts: LabelSpec[]): InstancedBufferGeometry {
    const g = this.quad([0, 1], [0, 1])
    const n = ts.length
    const node = new Float32Array(n), off = new Float32Array(2 * n), size = new Float32Array(2 * n)
    const uv = new Float32Array(4 * n), color = new Float32Array(4 * n), flags = new Float32Array(n)
    ts.forEach((t, i) => {
      const e = this.atlas.get(t.text, t.weight ?? 400)
      const s = t.size / ATLAS_PX
      const w = (e.width + 2 * e.pad) * s
      const h = (e.ascent + e.descent + 2 * e.pad) * s
      const anchor = t.anchor ?? 'middle'
      const penX = (t.dx ?? 0) - (anchor === 'middle' ? (e.width * s) / 2 : anchor === 'end' ? e.width * s : 0)
      // the baseline at dy, or the middle of the ascent and descent there
      const base = (t.dy ?? 0) - (t.baseline === 'middle' ? ((e.ascent - e.descent) * s) / 2 : 0)
      node[i] = this.index.get(t.node)!
      off[2 * i] = penX - e.pad * s
      off[2 * i + 1] = base - (e.descent + e.pad) * s
      size[2 * i] = w
      size[2 * i + 1] = h
      uv.set([e.u0, e.v0, e.u1, e.v1], 4 * i)
      color.set(t.color, 4 * i)
      flags[i] = t.flags ?? 0
    })
    this.attr(g, 'aNode', node, 1)
    this.attr(g, 'aOff', off, 2)
    this.attr(g, 'aSize', size, 2)
    this.attr(g, 'aUV', uv, 4)
    this.attr(g, 'aColor', color, 4)
    this.attr(g, 'aFlags', flags, 1)
    g.instanceCount = n
    return g
  }

  private strokeData(ss: StrokeSpec[]): InstancedBufferGeometry {
    const g = this.quad([0, 1], [-1, 1])
    const n = ss.length * SEGS
    const a = new Float32Array(n), b = new Float32Array(n), t = new Float32Array(2 * n), w = new Float32Array(n)
    const color = new Float32Array(4 * n), dash = new Float32Array(2 * n), bend = new Float32Array(2 * n)
    ss.forEach((s, j) => {
      for (let k = 0; k < SEGS; k++) {
        const i = j * SEGS + k
        a[i] = this.index.get(s.a)!
        b[i] = this.index.get(s.b)!
        t[2 * i] = k / SEGS
        t[2 * i + 1] = (k + 1) / SEGS
        w[i] = s.width
        color.set(s.color, 4 * i)
        dash.set(s.dash ?? [0, 0], 2 * i)
        bend.set(s.bend ?? [0.5, 0.62], 2 * i)
      }
    })
    this.attr(g, 'aA', a, 1)
    this.attr(g, 'aB', b, 1)
    this.attr(g, 'aT', t, 2)
    this.attr(g, 'aWidth', w, 1)
    this.attr(g, 'aColor', color, 4)
    this.attr(g, 'aDash', dash, 2)
    this.attr(g, 'aBend', bend, 2)
    g.instanceCount = n
    return g
  }

  /** The view box, whole and centred in the drawing's box, of which the camera sees the window. */
  private fit() {
    const { fullW, fullH, x, y, w, h } = this.win
    const { x0, y0, x1, y1 } = this.view
    const s = Math.min(fullW / (x1 - x0), fullH / (y1 - y0))
    const left = (x0 + x1) / 2 - fullW / s / 2
    const top = (y0 + y1) / 2 + fullH / s / 2
    Object.assign(this.camera, { left: left + x / s, right: left + (x + w) / s, top: top - y / s, bottom: top - (y + h) / s })
    this.camera.updateProjectionMatrix()
  }

  setWindow(win: Pane) {
    const o = this.win
    if (o.fullW === win.fullW && o.fullH === win.fullH && o.x === win.x && o.y === win.y && o.w === win.w && o.h === win.h && o.ratio === win.ratio) return
    this.win = win
    this.renderer.setPixelRatio(win.ratio)
    this.renderer.setSize(Math.max(1, win.w), Math.max(1, win.h), false)
    this.fit()
    this.wake()
  }

  /** The node under a point of the screen, if one is near enough to pick. */
  pick(clientX: number, clientY: number): string | null {
    const rect = this.canvas.getBoundingClientRect()
    const nx = ((clientX - rect.left) / rect.width) * 2 - 1
    const ny = 1 - ((clientY - rect.top) / rect.height) * 2
    const c = this.camera
    let x = c.left + ((nx + 1) / 2) * (c.right - c.left)
    let y = c.bottom + ((ny + 1) / 2) * (c.top - c.bottom)
    // undo the lean
    const ang = this.pivot.rotation.z
    if (ang) {
      const [px, py] = this.swayAt
      const dx = x - px, dy = y - py
      x = px + dx * Math.cos(-ang) - dy * Math.sin(-ang)
      y = py + dx * Math.sin(-ang) + dy * Math.cos(-ang)
    }
    const now = this.now()
    let best: string | null = null
    let bestD = Infinity
    for (const [k, l] of this.live) {
      if (!l.hit || l.mode === 2) continue
      const { p, life } = this.place(l, now)
      const d = Math.hypot(p[0] - x, p[1] - y)
      if (d < l.hit * life && d < bestD) {
        best = k
        bestD = d
      }
    }
    return best
  }

  private wake = () => {
    if (this.running || !this.visible || document.hidden) return
    this.running = true
    this.raf = requestAnimationFrame(this.frame)
  }

  private frame = () => {
    const now = this.now()
    this.uniforms.uNow.value = now
    const [px, py] = this.swayAt
    const ang = this.sway ? this.sway(now) : 0
    this.pivot.position.set(px, py, 0)
    this.inner.position.set(-px, -py, 0)
    this.pivot.rotation.z = ang
    this.renderer.render(this.scene, this.camera)
    // keep drawing while anything moves: a glide, a growth, a fade-in, a lean, a pulse
    const settle = this.moveStart + Math.max(this.timing.move, this.timing.grow + 0.3, this.timing.shrink, 0.6)
    const busy = now < settle || !!this.sway || this.pulsing
    if (busy && this.visible && !document.hidden) this.raf = requestAnimationFrame(this.frame)
    else this.running = false
  }

  /** Something on stage pulses, so frames keep coming. */
  pulsing = false

  dispose() {
    cancelAnimationFrame(this.raf)
    this.interObs.disconnect()
    document.removeEventListener('visibilitychange', this.wake)
    for (const m of this.meshes) m.geometry.dispose()
    this.nodeTex?.dispose()
    this.atlas.texture.dispose()
    this.glyphMat.dispose()
    this.labelMat.dispose()
    this.strokeMat.dispose()
    this.renderer.dispose()
  }
}
