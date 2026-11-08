// The tree drawing's motion as plain functions: where each node starts a step,
// where it is a moment into the step, where the ghosts of what the step removed
// go, and where the camera is. TreeCanvas draws with these, and
// scripts/motion-check.mts runs them without a browser to check the motion's rules.

import type { Id } from './store.ts'
import type { Scene, VNode } from './layout.ts'

export interface View {
  w: number
  h: number
  /// the tree look: it grows up out of the ground
  up: boolean
}
/// screen = world * k + (x, y)
export interface Cam {
  x: number
  y: number
  k: number
}
/// while stepping, the base of the trunk stays at (sx, sy) on screen, and the view zooms about it from k0 to k
export interface Pin {
  sx: number
  sy: number
  k0: number
  k: number
  root: number
}

/// A node the step removed, fading on its way to `to` (a new node's index), in place, or dropping.
/// `aim`: the ghost a binder flies into to knock it off; `hit`: the binder that knocks this one off the tree.
export interface Ghost {
  n: VNode
  x: number
  y: number
  to: number
  drop: boolean
  quick: boolean
  parent: number
  aim: number
  hit: number
  /// a knocked-off ghost that has been handed over to a Fall
  launched?: boolean
}

/// A step's motion: where each node of the new scene starts, and what fades out.
export interface Tween {
  fromX: Float64Array
  fromY: Float64Array
  /// 1 for a node that wasn't there before
  fresh: Uint8Array
  ghosts: Ghost[]
  /// in a binder step, 1 for the nodes of the part that changes
  acts: Uint8Array | null
  /// for a node of that part, the nearest node above it that isn't: it moves relative to that one, so the change happens in place while the tree carries it
  anchor: Int32Array
  /// the new scene's root when it has just one, which the camera holds still; -1 otherwise
  root: number
}

/// how far through a step a binder reaches the variable it knocks off
export const HIT = 0.3
/// the top of the ground in the tree look, where a knocked-off variable lands
export const GROUND = 26
/// how far from the trunk a knocked-off variable lands at most: the ground is at least 120 either side of it
export const LANDS = 100
/// a knocked-off variable falls under this, in world units a second squared
export const GRAVITY = 2400
/// how much of its speed it keeps when it bounces off the ground
const BOUNCE = 0.28
/// how fast the binder's knock sends it up, in world units a second
const KICK = 300
/// a variable's radius, for how far it turns as it rolls
const RADIUS = 10
/// how long a knocked-off variable lies still on the ground before it fades, and how long it fades for, in ms
export const REST_MS = 450
export const FADE_MS = 550

/// A knocked-off variable in flight: where it was knocked from, how fast, and the ground it falls to.
export interface Fall {
  x: number
  y: number
  vx: number
  vy: number
  ground: number
  /// on the ground in the tree look it bounces; elsewhere it falls out of sight
  bounces: boolean
}

/// The knock: a little up, and sideways so it lands on the ground under the tree, within LANDS of the trunk.
export function knock(x: number, y: number, trunk: number, up: boolean): Fall {
  const ground = up ? GROUND : y + 260
  const vy = -KICK
  const land = trunk + Math.max(-LANDS * 0.8, Math.min(LANDS * 0.8, x - trunk))
  // aimed at where it comes to rest, bounces included: how far it would go at one unit a second
  const reach = fallAt({ x: 0, y, vx: 1, vy, ground, bounces: up }, 1000).x
  // even right over its resting spot it goes a little sideways, so it reads as knocked rather than dropped
  const vx = Math.abs(land - x) < 8 ? (x >= trunk ? -30 : 30) : (land - x) / reach
  return { x, y, vx, vy, ground, bounces: up }
}

/// Where a knocked-off variable is `s` seconds after the knock: flying, bouncing (twice at most, losing most of its
/// speed each time), then still on the ground. `angle` is how far it has rolled; `still` is how long it has been at
/// rest, or -1 while it moves.
export function fallAt(f: Fall, s: number): { x: number; y: number; angle: number; still: number } {
  let { x, y, vx, vy } = f
  for (let bounce = 0; ; bounce++) {
    const flight = (-vy + Math.sqrt(vy * vy + 2 * GRAVITY * Math.max(0, f.ground - y))) / GRAVITY
    if (s < flight) {
      const px = x + vx * s
      return { x: px, y: y + vy * s + (GRAVITY * s * s) / 2, angle: (px - f.x) / RADIUS, still: -1 }
    }
    s -= flight
    x += vx * flight
    y = f.ground
    vy = -(vy + GRAVITY * flight) * BOUNCE
    vx *= 0.5
    if (!f.bounces || bounce >= 2 || -vy < 90) return { x, y, angle: (x - f.x) / RADIUS, still: s }
  }
}
/// seconds from the knock until it lies still
export const restsAt = (f: Fall) => 1000 - fallAt(f, 1000).still
/// how opaque a knocked-off variable is `s` seconds after the knock: whole until it has lain still a moment, then fading
export function fallAlpha(f: Fall, s: number): number {
  const rest = restsAt(f)
  if (!f.bounces) return 0.9 * Math.max(0, 1 - s / rest)
  return 0.9 * Math.max(0, Math.min(1, 1 - (s - rest - REST_MS / 1000) / (FADE_MS / 1000)))
}
/// seconds from the knock until it is gone
export const fallLife = (f: Fall) => restsAt(f) + (f.bounces ? (REST_MS + FADE_MS) / 1000 : 0)
const PAD = 34
/// the hover bar along the bottom gets its own strip, so it never covers the lowest nodes
const BAR = 26

export const ease = (t: number) => 1 - (1 - t) * (1 - t) * (1 - t)
/// in a binder step the part that changes is done by 70% of the way, while the rest of the tree takes the whole step
export const actT = (t: number) => Math.min(1, t / 0.7)
const smooth = (t: number) => t * t * (3 - 2 * t)

/// The scene as drawn: in the tree look y grows upward.
export const orient = (s: Scene, up: boolean): Scene => (up ? { ...s, nodes: s.nodes.map((n) => ({ ...n, y: -n.y })) } : s)
export const yRange = (s: Scene, up: boolean): [number, number] => (up ? [-s.maxY - 24, s.titles.length ? 76 : 56] : [0, s.maxY])
const roots = (s: Scene) => s.nodes.flatMap((n, i) => (n.parent < 0 ? [i] : []))
const soleRoot = (s: Scene) => {
  const rs = roots(s)
  return rs.length === 1 ? rs[0] : -1
}

/// How far a run's frames reach from their root, left, right and down (as laid out); undefined when a frame is empty.
export function reachOf(scenes: Iterable<Scene>): Scene['reach'] {
  const out = { left: 0, right: 0, depth: 0 }
  for (const sc of scenes) {
    const r = sc.nodes.find((n) => n.parent < 0)
    if (!r) return undefined
    out.left = Math.max(out.left, r.x - sc.minX)
    out.right = Math.max(out.right, sc.maxX - r.x)
    out.depth = Math.max(out.depth, sc.maxY - r.y)
  }
  return out
}

/// No motion: every node starts where it ends.
export function stillTween(next: Scene): Tween {
  const n = next.nodes.length
  return { fromX: Float64Array.from(next.nodes, (v) => v.x), fromY: Float64Array.from(next.nodes, (v) => v.y), fresh: new Uint8Array(n), ghosts: [], acts: null, anchor: new Int32Array(n).fill(-1), root: soleRoot(next) }
}

/// Where each node of `next` comes from in `prev`, whose nodes are now at (curX, curY), and what of `prev` is left to fade.
export function planTween(prev: Scene, next: Scene, curX: ArrayLike<number>, curY: ArrayLike<number>, falling: boolean): Tween {
  const n = next.nodes.length
  const nx = new Float64Array(n)
  const ny = new Float64Array(n)
  const fresh = new Uint8Array(n)
  const ghosts: Ghost[] = []
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
  const dropped = guide?.sinks.flatMap(([key, to]) => (to === 'drop' ? [key] : [])) ?? []
  // an old node carries on when something takes its place or its subtree; anything else is a ghost
  const used = new Uint8Array(prev.nodes.length)
  const src = new Int32Array(n).fill(-1)
  /// where a node may have come from: under its own key, or under the top of the new part it sits in
  const scope: string[] = []
  for (let i = 0; i < n; i++) {
    const v = next.nodes[i]
    const k = byKey.get(v.key)
    const par = v.parent
    scope.push(par >= 0 && fresh[par] ? scope[par] : v.key)
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
    // an old node the step carries to a new place, as a binder S copies keeps going as one of the copies
    const moved = guide?.moves?.get(v.key)
    if (from < 0 && moved !== undefined) from = byKey.get(moved) ?? -1
    if (from < 0) {
      const o = guide?.origin.get(v.key)?.flatMap((key) => byKey.get(key) ?? [])
      if (o?.length) {
        nx[i] = o.reduce((a, j) => a + curX[j], 0) / o.length
        ny[i] = o.reduce((a, j) => a + curY[j], 0) / o.length
        fresh[i] = 1
        continue
      }
      // a kept subtree that moved: the same tree from where the step happened, never one the step threw away
      const cands = (byId.get(v.id) ?? []).filter((j) => {
        const key = prev.nodes[j].key
        return !dropped.some((d) => key.startsWith(d)) && (key.startsWith(scope[i]) || sites?.some((s) => key.startsWith(s)))
      })
      from = cands.find((j) => !used[j] && prev.nodes[j].key.startsWith(scope[i])) ?? cands.find((j) => !used[j]) ?? cands[0] ?? -1
    }
    if (from >= 0) {
      src[i] = from
      used[from] = 1
      nx[i] = curX[from] ?? v.x
      ny[i] = curY[from] ?? v.y
      continue
    }
    // something new takes over the spot of what was there (the spine a step rebuilds), or grows out of its parent
    fresh[i] = 1
    if (k !== undefined && (!used[k] || par < 0)) {
      nx[i] = curX[k] ?? v.x
      ny[i] = curY[k] ?? v.y
    } else if (par >= 0) {
      nx[i] = nx[par]
      ny[i] = ny[par]
    } else {
      nx[i] = v.x
      ny[i] = v.y
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
      if (sink !== 'stay' && sink !== 'drop' && sink !== 'fade') {
        // the spot it was sent to may have folded away: the nearest one above it
        for (let key = sink; key.length >= 2 && to < 0; key = key.slice(0, -1)) to = nextKey.get(key) ?? -1
      }
      ghostOf.set(j, ghosts.length)
      ghosts.push({ n: p, x: curX[j], y: curY[j], to, drop: sink === 'drop', quick: sink === 'fade', parent: ghostOf.get(p.parent) ?? -1, aim: -1, hit: -1 })
    })
    for (const [binder, apple] of guide?.knock ?? []) {
      const b = ghosts.findIndex((g) => g.n.key === binder)
      const a = ghosts.findIndex((g) => g.n.key === apple)
      if (b < 0 || a < 0) continue
      ghosts[b].aim = a
      ghosts.forEach((g) => {
        if (g.n.key.startsWith(apple)) g.hit = b
      })
    }
  }
  const stage = guide?.stage
  const acts = stage?.length ? Uint8Array.from(next.nodes, (v) => (stage.some((key) => v.key.startsWith(key)) ? 1 : 0)) : null
  const anchor = new Int32Array(n).fill(-1)
  if (acts)
    next.nodes.forEach((v, i) => {
      const p = v.parent
      if (acts[i] && p >= 0 && p < i) anchor[i] = acts[p] ? anchor[p] : p
    })
  return { fromX: nx, fromY: ny, fresh, ghosts, acts, anchor, root: soleRoot(next) }
}

/// Every node's place `t` (0 to 1) of the way through the step, into (curX, curY).
export function nodesAt(tw: Tween, next: Scene, t: number, curX: Float64Array, curY: Float64Array) {
  const { fromX, fromY, acts, anchor, root } = tw
  const nodes = next.nodes
  const k = ease(t)
  const ka = ease(actT(t))
  /// how far the anchor moves against the still trunk, and how far the node moves from its anchor
  const along = (from: ArrayLike<number>, to: (j: number) => number, i: number, a: number) => {
    const drift = to(a) - from[a] - (root >= 0 ? to(root) - from[root] : 0)
    const own = to(i) - to(a) - (from[i] - from[a])
    // both ways at once on different clocks could turn the node back, so then it keeps the tree's clock
    return drift * own < 0 ? k : ka
  }
  for (let i = 0; i < nodes.length; i++) {
    const a = acts?.[i] ? anchor[i] : -1
    if (a >= 0) {
      // where it sits from its anchor goes from old to new early; the anchor brings it along as the tree settles
      const kx = along(fromX, (j) => nodes[j].x, i, a)
      const ky = along(fromY, (j) => nodes[j].y, i, a)
      curX[i] = curX[a] + fromX[i] - fromX[a] + (nodes[i].x - nodes[a].x - fromX[i] + fromX[a]) * kx
      curY[i] = curY[a] + fromY[i] - fromY[a] + (nodes[i].y - nodes[a].y - fromY[i] + fromY[a]) * ky
      continue
    }
    const ki = acts?.[i] ? ka : k
    curX[i] = fromX[i] + (nodes[i].x - fromX[i]) * ki
    curY[i] = fromY[i] + (nodes[i].y - fromY[i]) * ki
  }
}

/// Where ghost `j` is `t` of the way through the step, how opaque, and how big.
export function ghostAt(tw: Tween, j: number, t: number, curX: ArrayLike<number>, curY: ArrayLike<number>, up: boolean): { x: number; y: number; a: number; scale: number } {
  const ghosts = tw.ghosts
  const g = ghosts[j]
  const p = tw.acts ? actT(t) : t
  const e = ease(p)
  const after = Math.max(0, (p - HIT) / (1 - HIT))
  if (g.aim >= 0) {
    // a binder speeds up into the variable it cancels with, and is gone once it has knocked it off
    const q = Math.min(1, p / HIT)
    const top = ghosts[g.aim]
    return { x: g.x + (top.x - g.x) * q * q, y: g.y + (top.y - g.y) * q * q, a: Math.max(0, 1 - after / 0.35) * 0.9, scale: 1 }
  }
  // waiting to be knocked off; from the knock on it falls on its own clock (see knockedOff and fallAt)
  if (g.hit >= 0) return { x: g.x, y: g.y, a: 0.9, scale: 1 }
  const tx = g.to >= 0 ? curX[g.to] : g.x
  const ty = g.to >= 0 ? curY[g.to] : g.y + (g.drop ? 46 : 0)
  // what merges into a new node stays visible until it gets there; the rest just fades
  const a = g.quick ? Math.max(0, 1 - p / 0.4) : g.to >= 0 ? (p < 0.5 ? 1 : 1 - (p - 0.5) / 0.5) : g.drop ? 1 - p * p : 1 - e
  return { x: g.x + (tx - g.x) * e, y: g.y + (ty - g.y) * e, a: a * 0.9, scale: g.to >= 0 ? 1 - 0.3 * e : 1 }
}

/// how far through the step the binder knocks its variable off, as the step's own time
export const knockTime = (tw: Tween) => (tw.acts ? HIT * 0.7 : HIT)

/// The falls of what a binder knocks off, from where it hangs, aimed under the tree. They are measured from the trunk
/// (whose place at the moment of the knock `curX`, `curY` hold): the view holds the trunk still, so a fall measured from
/// it is a true fall on screen however the tree moves meanwhile.
export function knockedOff(tw: Tween, curX: ArrayLike<number>, curY: ArrayLike<number>, up: boolean): { ghost: number; fall: Fall }[] {
  const out: { ghost: number; fall: Fall }[] = []
  const tx = tw.root >= 0 ? curX[tw.root] : 0
  const ty = tw.root >= 0 ? curY[tw.root] : 0
  tw.ghosts.forEach((g, j) => {
    if (g.hit < 0) return
    const top = tw.ghosts[tw.ghosts[g.hit].aim]
    const f = knock(top.x - tx, top.y - ty, 0, up)
    // a variable that is a bigger tree falls as one piece
    out.push({ ghost: j, fall: { ...f, x: g.x - tx, y: g.y - ty, ground: f.ground + (g.y - top.y) } })
  })
  return out
}

/// The view that fits a box of the scene (by default all of it).
export function fitCam(view: View, s: Scene, minX = s.minX, maxX = s.maxX, top = yRange(s, view.up)[0], bottom = yRange(s, view.up)[1]): Cam {
  const titled = s.titles.length && !view.up ? 18 : 0
  const sw = Math.max(1, maxX - minX)
  const sh = Math.max(1, bottom - top)
  const k = Math.max(0.02, Math.min(1.5, (view.w - PAD * 2) / sw, (view.h - PAD * 2 - BAR - titled) / sh))
  return { k, x: view.w / 2 - ((minX + maxX) / 2) * k, y: Math.max(PAD + titled, (view.h - BAR - sh * k) / 2 - 8) - top * k }
}

/// The view that fits the box every frame of the run fits in, placed at this frame's root, so the zoom and the trunk's
/// place on screen are the same for every frame; null without a reach, or with several trees.
export function reachCam(view: View, next: Scene): Cam | null {
  const rs = roots(next)
  const reach = next.reach
  if (!reach || rs.length !== 1) return null
  const r = next.nodes[rs[0]]
  return view.up ? fitCam(view, next, r.x - reach.left, r.x + reach.right, r.y - reach.depth - 24, yRange(next, true)[1]) : fitCam(view, next, r.x - reach.left, r.x + reach.right, r.y, r.y + reach.depth)
}

/// The trunk of `next`, held where the view `cam` shows it.
export function pinAt(cam: Cam, now: Cam, next: Scene): Pin | null {
  const rs = roots(next)
  if (rs.length !== 1) return null
  const r = next.nodes[rs[0]]
  return { sx: cam.x + r.x * cam.k, sy: cam.y + r.y * cam.k, k0: now.k, k: cam.k, root: rs[0] }
}

/// Keep the base of the trunk where the settled view showed it for the step from `prev` to `next`, zooming about it only
/// as far as the tree needs; null when the view should refit instead (several trees, the trunk out of view, or a tree
/// left far too small).
export function trunkCam(view: View, settled: Cam, prev: Scene, next: Scene): Cam | null {
  const a = roots(prev)
  const b = roots(next)
  if (a.length !== 1 || b.length !== 1) return null
  const { w, h } = view
  const sx = settled.x + prev.nodes[a[0]].x * settled.k
  const sy = settled.y + prev.nodes[a[0]].y * settled.k
  if (sx < PAD || sx > w - PAD || sy < PAD || sy > h - PAD) return null
  const r = next.nodes[b[0]]
  const fit = fitCam(view, next).k
  const [top, bottom] = yRange(next, view.up)
  const room = (space: number, need: number) => (need > 1e-6 ? space / need : Infinity)
  const most = Math.min(1.5, room(sx - PAD, r.x - next.minX), room(w - PAD - sx, next.maxX - r.x), room(sy - PAD, r.y - top), room(h - PAD - BAR - sy, bottom - r.y))
  // the zoom changes rarely: out with room to grow, so the next steps fit too, and in only once the tree is well inside the view
  const k = settled.k <= most && fit < settled.k * 1.8 ? settled.k : settled.k > most ? Math.max(most * 0.85, fit * 0.6) : Math.min(most, fit) * 0.9
  if (k < fit * 0.6 || k > most) return null
  return { k, x: sx - r.x * k, y: sy - r.y * k }
}

/// The camera `t` of the way through a pinned step: the zoom eases in and out, and the trunk stays put.
export function camAt(pin: Pin, t: number, curX: ArrayLike<number>, curY: ArrayLike<number>): Cam {
  const k = pin.k0 + (pin.k - pin.k0) * smooth(t)
  return { k, x: pin.sx - curX[pin.root] * k, y: pin.sy - curY[pin.root] * k }
}
