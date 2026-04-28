// Tree Calculus: Leaf | Stem(child) | Fork(left, right)
// Trees are always in normal form. Application is an operation.

export type Tree = {
  readonly tag: "leaf"
  readonly id: number
} | {
  readonly tag: "stem"
  readonly child: Tree
  readonly id: number
} | {
  readonly tag: "fork"
  readonly left: Tree
  readonly right: Tree
  readonly id: number
}

// --- Hash-consing ---

let nextId = 0
const stemCache = new Map<number, Tree>()
const forkCache = new Map<number, Map<number, Tree>>()

export const LEAF: Tree = { tag: "leaf", id: nextId++ }

export function stem(child: Tree): Tree {
  const cached = stemCache.get(child.id)
  if (cached) return cached
  const node: Tree = { tag: "stem", child, id: nextId++ }
  stemCache.set(child.id, node)
  cacheStats.uniqueNodes++
  return node
}

export function fork(left: Tree, right: Tree): Tree {
  let inner = forkCache.get(left.id)
  if (inner) {
    const cached = inner.get(right.id)
    if (cached) return cached
  } else {
    inner = new Map()
    forkCache.set(left.id, inner)
  }
  const node: Tree = { tag: "fork", left, right, id: nextId++ }
  inner.set(right.id, node)
  cacheStats.uniqueNodes++
  return node
}

// --- Predicates ---

export function isLeaf(t: Tree): t is Tree & { tag: "leaf" } {
  return t.tag === "leaf"
}
export function isStem(t: Tree): t is Tree & { tag: "stem" } {
  return t.tag === "stem"
}
export function isFork(t: Tree): t is Tree & { tag: "fork" } {
  return t.tag === "fork"
}

// --- Tree equality (O(1) via hash-consing) ---

export function treeEqual(a: Tree, b: Tree): boolean {
  return a.id === b.id
}

// --- treeApply: constructive application for the compiler ---
// Used during compilation (bracket abstraction collapse).
// Leaf x       = Stem(x)
// Stem(a) x    = Fork(a, x)
// Fork(a,b) x  = Fork(Fork(a,b), x)   (no evaluation)

export function treeApply(f: Tree, g: Tree): Tree {
  if (isLeaf(f)) return stem(g)
  if (isStem(f)) return fork(f.child, g)
  return fork(f, g)
}

// --- Apply memoization ---
// apply(f, x) is a pure function: same (f, x) always produces the same result.
// Hash-consing guarantees tree identity, so (f.id, x.id) is a complete key.
// Only fork-case results are cached (leaf→stem and stem→fork are O(1)).

const applyMemo = new Map<number, Map<number, Tree>>()
let applyMemoEntries = 0
let applyMemoEntryLimit = 500_000

// --- Cache instrumentation ---
export const cacheStats = { hits: 0, misses: 0, memoWrites: 0, uniqueNodes: 0 }
export function resetCacheStats(): void {
  cacheStats.hits = 0; cacheStats.misses = 0; cacheStats.memoWrites = 0; cacheStats.uniqueNodes = 0
}

export function clearApplyCache(): void {
  applyMemo.clear()
  applyMemoEntries = 0
}

export function setApplyCacheLimit(entries: number): void {
  if (!Number.isFinite(entries) || entries < 0) throw new Error("apply cache limit must be a non-negative finite number")
  applyMemoEntryLimit = entries
  trimApplyMemo()
}

export function applyCacheSize(): number {
  return applyMemoEntries
}

function memoGet(f: Tree, x: Tree): Tree | undefined {
  return applyMemo.get(f.id)?.get(x.id)
}

function memoSet(f: Tree, x: Tree, result: Tree): void {
  if (applyMemoEntryLimit === 0) return
  let m = applyMemo.get(f.id)
  if (!m) { m = new Map(); applyMemo.set(f.id, m) }
  if (!m.has(x.id)) applyMemoEntries++
  m.set(x.id, result)
  cacheStats.memoWrites++
  trimApplyMemo()
}

function trimApplyMemo(): void {
  while (applyMemoEntries > applyMemoEntryLimit) {
    const firstOuter = applyMemo.keys().next()
    if (firstOuter.done) { applyMemoEntries = 0; return }
    const outerKey = firstOuter.value
    const inner = applyMemo.get(outerKey)
    if (!inner) { applyMemo.delete(outerKey); continue }
    const firstInner = inner.keys().next()
    if (firstInner.done) { applyMemo.delete(outerKey); continue }
    inner.delete(firstInner.value)
    applyMemoEntries--
    if (inner.size === 0) applyMemo.delete(outerKey)
  }
}

export type ApplyStats = {
  calls: number
  steps: number
  leafRules: number
  stemRules: number
  kRules: number
  sRules: number
  triageLeafRules: number
  triageStemRules: number
  triageForkRules: number
  fastEqRules: number
  memoHits: number
  memoMisses: number
  memoWrites: number
  maxStack: number
  cacheEntries: number
  uniqueNodes: number
}

const applyStats: ApplyStats = {
  calls: 0,
  steps: 0,
  leafRules: 0,
  stemRules: 0,
  kRules: 0,
  sRules: 0,
  triageLeafRules: 0,
  triageStemRules: 0,
  triageForkRules: 0,
  fastEqRules: 0,
  memoHits: 0,
  memoMisses: 0,
  memoWrites: 0,
  maxStack: 0,
  cacheEntries: 0,
  uniqueNodes: 0,
}
let applyTraceLimit = 0
const applyTrace: string[] = []

export function setApplyTraceLimit(limit: number): void {
  if (!Number.isFinite(limit) || limit < 0) throw new Error("apply trace limit must be a non-negative finite number")
  applyTraceLimit = limit
  applyTrace.length = 0
}

export function getApplyTrace(): string[] {
  return [...applyTrace]
}

function traceApply(event: string, f: Tree, x: Tree, stackDepth: number): void {
  if (applyTraceLimit === 0) return
  const fShape = isFork(f) ? `fork:${f.id}/${f.left.id}/${f.right.id}` : `${f.tag}:${f.id}`
  const xShape = isFork(x) ? `fork:${x.id}/${x.left.id}/${x.right.id}` : `${x.tag}:${x.id}`
  if (applyTrace.length >= applyTraceLimit) applyTrace.shift()
  applyTrace.push(`${event} f=${fShape} x=${xShape} stack=${stackDepth}`)
}

export function resetApplyStats(): void {
  applyStats.calls = 0
  applyStats.steps = 0
  applyStats.leafRules = 0
  applyStats.stemRules = 0
  applyStats.kRules = 0
  applyStats.sRules = 0
  applyStats.triageLeafRules = 0
  applyStats.triageStemRules = 0
  applyStats.triageForkRules = 0
  applyStats.fastEqRules = 0
  applyStats.memoHits = 0
  applyStats.memoMisses = 0
  applyStats.memoWrites = 0
  applyStats.maxStack = 0
  applyStats.cacheEntries = 0
  applyStats.uniqueNodes = 0
}

export function getApplyStats(): ApplyStats {
  return {
    ...applyStats,
    memoHits: cacheStats.hits,
    memoMisses: cacheStats.misses,
    memoWrites: cacheStats.memoWrites,
    cacheEntries: applyMemoEntries,
    uniqueNodes: cacheStats.uniqueNodes,
  }
}

// --- apply: tree calculus application (execution) ---
// This IS the evaluator. Trees are always in normal form.
// Application follows the 5 triage rules eagerly.
//
// apply(△, x)             = △ x                    (stem construction)
// apply(△ a, x)           = △ a x                  (fork construction)
// apply(△ △ b, x)         = b                      (Rule 1: K)
// apply(△ (△ c) b, x)     = apply(apply(c,x), apply(b,x))  (Rule 2: S)
// apply(△ (△ c d) b, △)   = c                      (Rule 3a: triage leaf)
// apply(△ (△ c d) b, △ u) = apply(d, u)            (Rule 3b: triage stem)
// apply(△ (△ c d) b, △ u v) = apply(apply(b,u), v) (Rule 3c: triage fork)

export class BudgetExhausted extends Error {
  constructor(budget: number) {
    super(`Evaluation budget exhausted (${budget} steps)`)
  }
}

// Fully iterative apply with explicit continuation stack.
// No recursive calls — all evaluation driven by the main loop.

const enum ContKind { ApplyTo, ApplyResultTo, Memo, SAfterCx }
type Cont =
  | { kind: ContKind.ApplyTo, arg: Tree }
  | { kind: ContKind.ApplyResultTo, func: Tree }
  | { kind: ContKind.Memo, f: Tree, x: Tree }
  | { kind: ContKind.SAfterCx, origX: Tree, b: Tree }

export function apply(fInit: Tree, xInit: Tree, budget = { remaining: 10000 }): Tree {
  applyStats.calls++
  let curF = fInit, curX = xInit
  const stack: Cont[] = []

  // Deliver a result: pop continuations until we find one that needs more work
  function deliver(result: Tree): Tree | null {
    while (stack.length > 0) {
      const c = stack[stack.length - 1]
      switch (c.kind) {
        case ContKind.ApplyTo:
          stack.pop(); curF = result; curX = c.arg; return null  // continue eval
        case ContKind.ApplyResultTo:
          stack.pop(); curF = c.func; curX = result; return null
        case ContKind.Memo:
          stack.pop()
          memoSet(c.f, c.x, result)
          break  // keep popping
        case ContKind.SAfterCx: {
          stack.pop()
          const cx = result
          if (isFork(cx) && isLeaf(cx.left)) {
            result = cx.right; break  // K(v): result=v, keep popping
          }
          if (isFork(c.b) && isLeaf(c.b.left)) {
            curF = cx; curX = c.b.right; return null  // C fast-path
          }
          // General: compute b(origX), then apply cx to it
          stack.push({ kind: ContKind.ApplyResultTo, func: cx })
          curF = c.b; curX = c.origX; return null
        }
      }
    }
    return result  // stack empty, final result
  }

  while (true) {
    if (budget.remaining <= 0) throw new BudgetExhausted(0)
    if (stack.length > applyStats.maxStack) applyStats.maxStack = stack.length

    // Leaf/Stem: immediate result
    if (isLeaf(curF)) { traceApply("leaf", curF, curX, stack.length); applyStats.leafRules++; const r = deliver(stem(curX)); if (r !== null) return r; continue }
    if (isStem(curF)) { traceApply("stem", curF, curX, stack.length); applyStats.stemRules++; const r = deliver(fork(curF.child, curX)); if (r !== null) return r; continue }

    // Memo check
    const c = memoGet(curF, curX)
    if (c !== undefined) { cacheStats.hits++; const r = deliver(c); if (r !== null) return r; continue }
    cacheStats.misses++

    // FAST_EQ
    if (curF.left.id === FAST_EQ_MARKER.id) {
      traceApply("fast_eq", curF, curX, stack.length)
      applyStats.fastEqRules++
      const v = treeEqual(curF.right, curX) ? LEAF : stem(LEAF)
      memoSet(curF, curX, v)
      const r = deliver(v); if (r !== null) return r; continue
    }

    budget.remaining--
    applyStats.steps++
    if (budget.remaining <= 0) throw new BudgetExhausted(0)

    const a = curF.left, b = curF.right
    stack.push({ kind: ContKind.Memo, f: curF, x: curX })

    if (isLeaf(a)) {
      // K rule
      traceApply("K", curF, curX, stack.length)
      applyStats.kRules++
      const r = deliver(b); if (r !== null) return r; continue
    }

    if (isStem(a)) {
      traceApply("S", curF, curX, stack.length)
      applyStats.sRules++
      const c = a.child
      if (isFork(c) && isLeaf(c.left)) {
        if (isFork(b) && isLeaf(b.left)) { curF = c.right; curX = b.right; continue }
        stack.push({ kind: ContKind.ApplyResultTo, func: c.right })
        curF = b; continue  // curX unchanged
      }
      // General S: compute c(x), then SAfterCx handles the rest
      stack.push({ kind: ContKind.SAfterCx, origX: curX, b })
      curF = c; continue  // curX unchanged
    }

    // Triage
    const tc = a.left, td = a.right
    if (isLeaf(curX)) { traceApply("T_leaf", curF, curX, stack.length); applyStats.triageLeafRules++; const r = deliver(tc); if (r !== null) return r; continue }
    if (isStem(curX)) { traceApply("T_stem", curF, curX, stack.length); applyStats.triageStemRules++; curF = td; curX = curX.child; continue }
    traceApply("T_fork", curF, curX, stack.length)
    applyStats.triageForkRules++
    stack.push({ kind: ContKind.ApplyTo, arg: curX.right })
    curF = b; curX = curX.left; continue
  }
}

// Convenience wrapper with a simple numeric budget
export function applyTree(f: Tree, x: Tree, maxSteps = 10000): Tree {
  const budget = { remaining: maxSteps }
  return apply(f, x, budget)
}

// --- Constants ---

// K = △ △ = stem(LEAF). K b x → b (returns first argument, discards second).
export const K = stem(LEAF)

// Structural identity via triage: I = △ (△ △ △) △ = fork(fork(LEAF, LEAF), LEAF)
//   I(leaf):      Rule 3a → LEAF = △                           ✓
//   I(stem(u)):   Rule 3b → apply(LEAF, u) = stem(u)           ✓
//   I(fork(u,v)): Rule 3c → apply(apply(LEAF, u), v) = fork(u,v) ✓
export const I = fork(fork(LEAF, LEAF), LEAF)

// --- FAST_EQ: O(1) tree equality via hash-consing identity ---
// apply(FAST_EQ, a) = fork(FAST_EQ_MARKER, a) via stem rule [O(1)]
// apply(fork(FAST_EQ_MARKER, a), b) = TT if a.id === b.id, FF otherwise [O(1)]
// This exposes hash-consing identity to tree programs, replacing the O(n)
// structural comparison of tree-level treeEq with an O(1) check.
const FAST_EQ_MARKER = fork(fork(stem(stem(LEAF)), LEAF), stem(stem(LEAF)))
export const FAST_EQ: Tree = stem(FAST_EQ_MARKER)

// --- Pretty printer ---

export function prettyTree(tree: Tree): string {
  if (isLeaf(tree)) return "△"
  if (isStem(tree)) return `(△ ${prettyTree(tree.child)})`
  return `(△ ${prettyTree(tree.left)} ${prettyTree(tree.right)})`
}
