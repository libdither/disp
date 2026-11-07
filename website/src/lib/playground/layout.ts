// Turns a term into a drawable scene: decide what's folded (a breadth-first
// budget keeps a huge tree cheap), then a tidy layout, root on top.

import { Store, LEAF, STEM, FORK, APP, VAR, CUT, LAM, HELD_K, SPLIT, ATOM, type Id, type LabelKind } from './store.ts'

export const K_LEAF = 0
export const K_STEM = 1
export const K_FORK = 2
export const K_APP = 3
export const K_VAR = 4
export const K_POD = 5
/// a binder not abstracted yet; text = its name
export const K_LAM = 7

export const X_UNIT = 30
export const Y_UNIT = 54

export interface VNode {
  key: string // the path from the root; a node's identity across frames
  id: Id
  x: number
  y: number
  parent: number
  side: number // which child of its parent
  kind: number
  text: string | null
  podKind: LabelKind | 'anon' | 'same' | null
  rule: number
  next: boolean
  diff: boolean
  /// on the top node of a subtree that reads as a combinator: S, K, I or F
  comb: string | null
  /// the top node of the combinator this node is part of; its operands start their own
  group: number
  /// an application inside a binder still in place: it can't run until the binder is gone
  held?: boolean
  /// a named subtree drawn open: its name, on a tag that folds it back when clicked
  tag?: string
}

/// Where a step sends things, so the drawing can show it instead of jumping.
export interface Guide {
  /// a new node starts where these old nodes were (between them)
  origin: Map<string, string[]>
  /// old nodes under a key that are gone: they glide to a new key as they fade, stay put, drop away,
  /// or fade quickly where they are so what replaces them has the stage
  sinks: [string, string | 'stay' | 'drop' | 'fade'][]
}

export interface Scene {
  nodes: VNode[]
  guide?: Guide
  minX: number
  maxX: number
  maxY: number
  titles: { x: number; text: string; bad?: boolean }[]
  /// visible apart from what the budget had to cut
  cut: number
  /// big data left open because the next step is right at it
  nearOpen: Set<Id>
}

export interface FoldState {
  open: Set<Id>
  closed: Set<Id>
}

export interface LayoutOptions {
  maxVisible?: number
  nextPath?: number[] | null
  /// in parallel mode, every other site that fires along with it
  alsoNext?: number[][]
  same?: Set<Id>
  diff?: Set<string>
  titles?: (string | undefined)[]
  badTitles?: boolean[]
  /// false draws named subtrees open instead of as labelled pods
  labels?: boolean
  /// pods that hold a mismatch deeper down
  diffInside?: Set<string>
  /// finished data bigger than this folds, except right at the next redex
  foldData?: number
  /// big data the previous frame left open, kept open one more step so it doesn't fold and unfold in turn
  keepOpen?: Set<Id>
  /// mark the subtrees that read as S, K, I and F
  sk?: boolean
  /// draw numerals as the trees they are
  plainNats?: boolean
}

/// how many X_UNITs a name's pill needs
const pillWidth = (text: string) => Math.max(1, Math.min(text.length, 16) * 0.3 + 0.75)
const fmtCount = (n: number) => (n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e4 ? Math.round(n / 1e3) + 'k' : String(n))

export function layoutScene(store: Store, roots: Id[], folds: FoldState, opts: LayoutOptions = {}): Scene {
  const maxVisible = opts.maxVisible ?? 1800
  const nodes: VNode[] = []
  const kidsOf: number[][] = []
  const width: number[] = []
  let cut = 0
  const nearOpen = new Set<Id>()
  const titles: Scene['titles'] = []
  const nextKey = opts.nextPath ? 'r0' + opts.nextPath.join('') : null
  const alsoKeys = new Set(opts.alsoNext?.map((p) => 'r0' + p.join('')))
  const res = (id: Id) => {
    let cur = id
    for (let r = store.resolved.get(cur); r !== undefined; r = store.resolved.get(cur)) cur = r
    return store.tag[cur] === ATOM ? store.a[cur] : cur
  }

  // something being called is code, even when its shape spells a number
  const isHead = (n: VNode) => !!opts.sk && n.side === 0 && n.parent >= 0 && nodes[n.parent].kind === K_APP
  const operands = new Map<number, string[]>()
  /// inside a binder that hasn't come off: no application there can fire yet
  const held: boolean[] = []
  const rootStart: number[] = []
  roots.forEach((raw, ri) => {
    rootStart.push(nodes.length)
    const push = (id: Id, parent: number, key: string, side: number) => {
      const g = parent >= 0 ? nodes[parent].group : -1
      const group = g >= 0 && !operands.get(g)!.includes(key) ? g : -1
      nodes.push({ key, id, x: 0, y: 0, parent, side, kind: K_LEAF, text: null, podKind: null, rule: 0, next: false, diff: false, comb: null, group })
      held.push(parent >= 0 && (held[parent] || store.tag[nodes[parent].id] === LAM || store.tag[nodes[parent].id] === SPLIT))
      kidsOf.push([])
      width.push(1)
    }
    push(res(raw), -1, 'r' + ri, 0)
    for (let i = rootStart[ri]; i < nodes.length; i++) {
      const n = nodes[i]
      const id = n.id
      const t = store.tag[id]
      n.diff = (opts.diff?.has(n.key) || opts.diffInside?.has(n.key)) ?? false
      if (t === LEAF) {
        n.kind = K_LEAF
        continue
      }
      if (t === VAR) {
        n.kind = K_VAR
        n.text = store.strs[store.a[id]]
        width[i] = pillWidth(n.text)
        continue
      }
      if (n.key === nextKey || alsoKeys.has(n.key)) n.next = true
      const isRoot = n.parent < 0
      const inside = n.group >= 0
      const label = inside ? null : store.label(id)
      let pod = false
      if (t === CUT) pod = true
      else if (inside) pod = false
      else if (opts.same?.has(id) && !(opts.diff?.has(n.key) ?? false)) pod = true
      else if (folds.closed.has(id)) pod = true
      else if (folds.open.has(id) || isRoot) pod = false
      else if (label && t !== APP && opts.labels !== false && !(label.kind === 'nat' && (opts.plainNats || isHead(n)))) pod = true
      if (!pod && !inside && opts.foldData && t !== APP && !store.hasApp(id) && store.size[id] > opts.foldData) {
        const nearNext = nextKey !== null && n.key.startsWith(nextKey) && n.key.length <= nextKey.length + 2
        if (nearNext) nearOpen.add(id)
        else if (!folds.open.has(id) && !opts.keepOpen?.has(id)) pod = true
      }
      if (!pod && nodes.length >= maxVisible) {
        pod = true
        cut++
      }
      if (pod) {
        n.kind = K_POD
        if (opts.same?.has(id) && !(opts.diff?.has(n.key) ?? false)) {
          n.podKind = 'same'
          n.text = label?.text ?? `= ${fmtCount(store.size[id])}`
        } else if (label) {
          n.podKind = label.kind
          n.text = label.text
        } else if (t === CUT) {
          n.podKind = 'anon'
          n.text = store.names.get(id) ?? '…'
        } else {
          n.podKind = 'anon'
          n.text = '…' + fmtCount(store.size[id])
        }
        width[i] = Math.max(1, Math.min(n.text.length, 28) * 0.28 + 0.5)
        continue
      }
      if (!isRoot && !inside && ((label?.kind === 'name' && t !== APP) || folds.open.has(id))) n.tag = label?.text ?? '…' + fmtCount(store.size[id])
      const comb = !inside && (opts.sk || t === HELD_K) ? store.readComb(id) : null
      if (comb) {
        n.comb = comb.op
        n.group = i
        operands.set(i, comb.kids.map(([, suffix]) => n.key + suffix))
      }
      if (t === HELD_K) {
        n.kind = K_FORK
        kidsOf[i].push(nodes.length)
        push(store.leaf, i, n.key + '0', 0)
        kidsOf[i].push(nodes.length)
        push(res(store.a[id]), i, n.key + '1', 1)
        continue
      }
      if (t === LAM) {
        n.kind = K_LAM
        n.rule = store.rule[id]
        n.text = store.strs[store.a[id]]
        kidsOf[i].push(nodes.length)
        push(res(store.b[id]), i, n.key + '0', 0)
        continue
      }
      if (t === STEM) n.kind = K_STEM
      else if (t === FORK || t === SPLIT) {
        n.kind = K_FORK
        n.rule = t === SPLIT ? store.rule[id] : 0
      } else {
        n.kind = K_APP
        n.rule = held[i] ? 0 : store.rule[id]
        n.held = held[i]
      }
      const childIds = t === STEM ? [store.a[id]] : t === SPLIT ? [store.stem(store.a[id]), store.b[id]] : [store.a[id], store.b[id]]
      childIds.forEach((c, side) => {
        kidsOf[i].push(nodes.length)
        push(res(c), i, n.key + side, side)
      })
    }
  })

  // bottom-up widths (breadth-first order reversed visits children first)
  for (let i = nodes.length - 1; i >= 0; i--) {
    const k = kidsOf[i]
    if (k.length > 0) {
      let w = nodes[i].kind === K_LAM ? pillWidth(nodes[i].text ?? '') : 0
      if (nodes[i].kind === K_LAM) w = Math.max(w, width[k[0]])
      else for (const c of k) w += width[c]
      width[i] = w
    }
  }
  // top-down placement, then parents centre over their children
  const left = new Float64Array(nodes.length)
  let offset = 0
  roots.forEach((_, ri) => {
    const r = rootStart[ri]
    left[r] = offset
    offset += width[r] + 3
  })
  for (let i = 0; i < nodes.length; i++) {
    let cum = left[i]
    for (const c of kidsOf[i]) {
      left[c] = cum
      cum += width[c]
    }
    nodes[i].y = nodes[i].parent < 0 ? 0 : nodes[nodes[i].parent].y + Y_UNIT
  }
  let minX = Infinity
  let maxX = -Infinity
  let maxY = 0
  for (let i = nodes.length - 1; i >= 0; i--) {
    const k = kidsOf[i]
    const u = k.length === 0 ? left[i] + width[i] / 2 : (nodes[k[0]].x / X_UNIT + nodes[k[k.length - 1]].x / X_UNIT) / 2
    const x = u * X_UNIT
    nodes[i].x = x
    if (x < minX) minX = x
    if (x > maxX) maxX = x
    if (nodes[i].y > maxY) maxY = nodes[i].y
  }
  roots.forEach((_, ri) => {
    const t = opts.titles?.[ri]
    if (t) titles.push({ x: nodes[rootStart[ri]].x, text: t, bad: opts.badTitles?.[ri] })
  })
  return { nodes, minX, maxX, maxY, titles, cut, nearOpen }
}

/// Where two trees differ: everything they share folds away, and the keys of the mismatched positions light up.
export function diffTrees(store: Store, a: Id, b: Id, limit = 4000): { same: Set<Id>; diff: Set<string>; inside: Set<string>; firstDiff: string | null } {
  const same = new Set<Id>()
  const diff = new Set<string>()
  const inside = new Set<string>()
  let firstDiff: string | null = null
  const stack: [Id, Id, string][] = [[a, b, '']]
  let budget = limit
  while (stack.length > 0 && budget-- > 0) {
    const [x, y, path] = stack.pop()!
    if (x === y) {
      same.add(x)
      continue
    }
    const tx = store.tag[x]
    const ty = store.tag[y]
    if (tx === ty && (tx === STEM || tx === FORK)) {
      if (tx === FORK) stack.push([store.b[x], store.b[y], path + '1'])
      stack.push([store.a[x], store.a[y], path + '0'])
      continue
    }
    diff.add('r0' + path)
    diff.add('r1' + path)
    for (let i = 0; i < path.length; i++) {
      inside.add('r0' + path.slice(0, i))
      inside.add('r1' + path.slice(0, i))
    }
    firstDiff ??= path
  }
  return { same, diff, inside, firstDiff }
}

/// Parse a scene key back to a child-index path.
export const keyPath = (key: string): number[] => [...key.slice(2)].map(Number)
