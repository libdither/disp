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
let applyMemoEntryLimit = 5_000_000

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

// Evict entries until under the limit. Eviction is FIFO in JavaScript
// Map insertion order (`Map.keys().next()` always returns the oldest
// key first), not LRU — there is no recency tracking on memoGet.
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
  treeEqRules: number
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
  treeEqRules: 0,
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
  applyStats.treeEqRules = 0
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

// One stable shape for all continuation kinds (V8 keeps a single hidden
// class). Only the fields relevant to the kind are read.
//
// `innerMap` is cached on Memo frames so memoSet on unwind doesn't have
// to re-do `applyMemo.get(f.id)` — the outer-map lookup we already did
// at the memoGet check is reused.
interface ContSlot {
  kind: number
  f: Tree
  x: Tree
  arg: Tree
  func: Tree
  origX: Tree
  b: Tree
  innerMap: Map<number, Tree> | null
}

// Module-level continuation stack — preallocated slots, reused across all
// apply() calls (including recursive ones via nativeDispatch). Each
// apply() captures stackTop at entry and only pops down to that
// `baseTop`, so nested calls don't interfere. No per-push allocation:
// pushes overwrite slot fields in place.
const STACK_INITIAL_CAP = 4096
const stack: ContSlot[] = []
let stackTop = 0
function fillSlots(n: number): void {
  // Slots are populated lazily so they share a single hidden class created
  // from the first writes. Initialize with placeholder LEAF children to
  // pin the shape at construction time.
  for (let i = 0; i < n; i++) {
    stack.push({ kind: 0, f: LEAF, x: LEAF, arg: LEAF, func: LEAF, origX: LEAF, b: LEAF, innerMap: null })
  }
}
fillSlots(STACK_INITIAL_CAP)
function ensureStackSlot(): void {
  if (stackTop >= stack.length) fillSlots(stack.length)
}

export function apply(fInit: Tree, xInit: Tree, budget = { remaining: 10000 }): Tree {
  applyStats.calls++
  let curF = fInit, curX = xInit
  const baseTop = stackTop  // isolation: only pop frames pushed by THIS call

  // Deliver a result: pop continuations until we find one that needs more work
  function deliver(result: Tree): Tree | null {
    while (stackTop > baseTop) {
      const i = stackTop - 1
      const slot = stack[i]
      const kind = slot.kind
      if (kind === ContKind.ApplyTo) {
        const arg = slot.arg
        stackTop = i
        curF = result; curX = arg; return null  // continue eval
      }
      if (kind === ContKind.ApplyResultTo) {
        const func = slot.func
        stackTop = i
        curF = func; curX = result; return null
      }
      if (kind === ContKind.Memo) {
        const sf = slot.f, sx = slot.x
        let m = slot.innerMap
        stackTop = i
        // Inline memoSet, using the innerMap captured at memoGet time
        // to skip a redundant applyMemo.get(sf.id) lookup.
        if (applyMemoEntryLimit !== 0) {
          if (!m) { m = new Map(); applyMemo.set(sf.id, m) }
          if (!m.has(sx.id)) applyMemoEntries++
          m.set(sx.id, result)
          cacheStats.memoWrites++
          if (applyMemoEntries > applyMemoEntryLimit) trimApplyMemo()
        }
        continue  // keep popping
      }
      // SAfterCx
      const sb = slot.b, sOrigX = slot.origX
      stackTop = i
      const cx = result
      if (isFork(cx) && isLeaf(cx.left)) {
        result = cx.right; continue  // K(v): result=v, keep popping
      }
      if (isFork(sb) && isLeaf(sb.left)) {
        curF = cx; curX = sb.right; return null  // C fast-path
      }
      // General: compute b(origX), then apply cx to it
      ensureStackSlot()
      const next = stack[stackTop++]
      next.kind = ContKind.ApplyResultTo; next.func = cx
      curF = sb; curX = sOrigX; return null
    }
    return result  // stack empty (for this call), final result
  }

  while (true) {
    if (budget.remaining <= 0) throw new BudgetExhausted(0)
    if (stackTop > applyStats.maxStack) applyStats.maxStack = stackTop

    // Native dispatcher fast-path: fires on `apply(checked_apply, f)`.
    // Must precede the leaf/stem rule because `checked_apply` compiles
    // to a stem (S (K Ok)) — without this the stem rule preempts.
    if (DISPATCHER_TREE_ID !== -1 && curF.id === DISPATCHER_TREE_ID) {
      const r = fork(DISPATCHER_PARTIAL_MARKER, curX)
      const d = deliver(r); if (d !== null) return d; continue
    }
    if (curF.tag === "fork" && curF.left.id === DISPATCHER_PARTIAL_MARKER.id) {
      const result = nativeDispatch(curF.right, curX, budget)
      const d = deliver(result); if (d !== null) return d; continue
    }

    // Leaf/Stem: immediate result
    if (isLeaf(curF)) { traceApply("leaf", curF, curX, stackTop); applyStats.leafRules++; const r = deliver(stem(curX)); if (r !== null) return r; continue }
    if (isStem(curF)) { traceApply("stem", curF, curX, stackTop); applyStats.stemRules++; const r = deliver(fork(curF.child, curX)); if (r !== null) return r; continue }
    if (!isFork(curF)) throw new Error("apply: impossible non-fork function")

    // Memo check (inlined so we can capture the inner map for memoSet
    // reuse if this turns out to be a miss).
    const innerMap = applyMemo.get(curF.id)
    const c = innerMap?.get(curX.id)
    if (c !== undefined) { cacheStats.hits++; const r = deliver(c); if (r !== null) return r; continue }
    cacheStats.misses++

    // tree_eq host fast path: O(1) hash-cons identity check.
    // Canonical definition lives in lib/prelude.disp as a recursive triage
    // form. The host captures its compiled tree id at boot (via
    // `setTreeEqId`); the fast-path fires whenever an apply step is about
    // to reduce `apply(tree_eq, a)` (recognized by `curF.id === TREE_EQ_ID`)
    // — it synthesizes `fork(TREE_EQ_PARTIAL_MARKER, a)` so the next apply
    // can do an O(1) compare via the partial-marker branch below.
    // The fast-path is an optimization; removing it yields identical
    // answers from the recursive spec.
    if (TREE_EQ_ID !== -1 && curF.id === TREE_EQ_ID) {
      traceApply("tree_eq", curF, curX, stackTop)
      applyStats.treeEqRules++
      const r = fork(TREE_EQ_PARTIAL_MARKER, curX)
      memoSet(curF, curX, r)
      const d = deliver(r); if (d !== null) return d; continue
    }
    if (curF.left.id === TREE_EQ_PARTIAL_MARKER.id) {
      traceApply("tree_eq", curF, curX, stackTop)
      applyStats.treeEqRules++
      const v = treeEqual(curF.right, curX) ? SCOTT_TT : SCOTT_FF
      memoSet(curF, curX, v)
      const r = deliver(v); if (r !== null) return r; continue
    }

    // Native dispatcher fast-path: `apply(checked_apply, f)` synthesizes
    // a partial-marker form, then `apply(marker, x)` runs nativeDispatch
    // (signature recognition + parametric walker) in TS — orders of
    // magnitude faster than executing the in-language fix-form chain.

    budget.remaining--
    applyStats.steps++
    if (budget.remaining <= 0) throw new BudgetExhausted(0)

    const a = curF.left, b = curF.right

    // K rule is O(1) and produces a value constant in curX (just b).
    // Caching (curF, curX) -> b would mostly pollute the cache with
    // entries that won't repeat. Skip the Memo frame entirely.
    if (isLeaf(a)) {
      traceApply("K", curF, curX, stackTop)
      applyStats.kRules++
      const r = deliver(b); if (r !== null) return r; continue
    }

    // Push Memo frame (in-place; no allocation). Carry the innerMap
    // captured during memoGet so the unwind-time memoSet skips a Map.get.
    ensureStackSlot()
    {
      const s = stack[stackTop++]
      s.kind = ContKind.Memo; s.f = curF; s.x = curX; s.innerMap = innerMap ?? null
    }

    if (isStem(a)) {
      traceApply("S", curF, curX, stackTop)
      applyStats.sRules++
      const c = a.child
      if (isFork(c) && isLeaf(c.left)) {
        if (isFork(b) && isLeaf(b.left)) { curF = c.right; curX = b.right; continue }
        ensureStackSlot()
        const s = stack[stackTop++]
        s.kind = ContKind.ApplyResultTo; s.func = c.right
        curF = b; continue  // curX unchanged
      }
      // General S: compute c(x), then SAfterCx handles the rest
      ensureStackSlot()
      const s = stack[stackTop++]
      s.kind = ContKind.SAfterCx; s.origX = curX; s.b = b
      curF = c; continue  // curX unchanged
    }

    // Triage
    if (!isFork(a)) throw new Error("apply: impossible non-fork branch")
    const tc = a.left, td = a.right
    if (isLeaf(curX)) { traceApply("T_leaf", curF, curX, stackTop); applyStats.triageLeafRules++; const r = deliver(tc); if (r !== null) return r; continue }
    if (isStem(curX)) { traceApply("T_stem", curF, curX, stackTop); applyStats.triageStemRules++; curF = td; curX = curX.child; continue }
    if (!isFork(curX)) throw new Error("apply: impossible non-fork argument")
    traceApply("T_fork", curF, curX, stackTop)
    applyStats.triageForkRules++
    ensureStackSlot()
    {
      const s = stack[stackTop++]
      s.kind = ContKind.ApplyTo; s.arg = curX.right
    }
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

// Scott-encoded Bool constants (per spec §4.5). These are the exact
// hash-cons-identity trees produced when the prelude compiles
// `TT := {m,ct,cf} -> ct` (= K K) and `FF := {m,ct,cf} -> cf`
// (= K (K I)). Captured here so the tree_eq fast-path can return them
// directly without needing a runtime hook from compile.ts.
export const SCOTT_TT = fork(LEAF, K)               // K K
export const SCOTT_FF = fork(LEAF, fork(LEAF, I))   // K (K I)

// --- Native walker / dispatcher fast path ---
// The kernel's `q_checked_apply_fn` is the security perimeter: it routes
// each application via signature recognition (matching kernel handlers
// run raw) or the parametric walker (default). The in-language version
// is correct but ~400× slower than raw apply, because every apply step
// traverses the recq dispatcher chain.
//
// This native implementation runs the same dispatch + parametric rules
// in TypeScript, returning bit-identical CheckedResults. Wired by
// compile.ts after kernel.disp loads:
//   - setNativeDispatcherTreeId(checked_apply.id)
//   - setNativeKernelSig("hyp_reduce", kernel_hyp_reduce_sig.id)
//   - ... one call per kernel handler signature
//   - setNativeICanonicalId(I_canonical.id)
//
// Until those are registered the fast-path is dormant and apply runs
// the in-language stub (currently `Ok (f x)` from kernel.disp).

let DISPATCHER_TREE_ID: number = -1
const DISPATCHER_PARTIAL_MARKER = fork(fork(stem(stem(stem(LEAF))), LEAF), stem(stem(stem(LEAF))))

const nativeKernelSigs = new Map<string, number>()
let HYP_REDUCE_SIG_ID: number = -1
let NATIVE_I_CANONICAL_ID: number = -1

export function setNativeDispatcherTreeId(id: number): void { DISPATCHER_TREE_ID = id }
export function getNativeDispatcherTreeId(): number { return DISPATCHER_TREE_ID }

export function setNativeKernelSig(name: string, sigTreeId: number): void {
  nativeKernelSigs.set(name, sigTreeId)
  if (name === "hyp_reduce") HYP_REDUCE_SIG_ID = sigTreeId
}
export function setNativeICanonicalId(id: number): void { NATIVE_I_CANONICAL_ID = id }

export function clearNativeDispatcher(): void {
  DISPATCHER_TREE_ID = -1
  HYP_REDUCE_SIG_ID = -1
  NATIVE_I_CANONICAL_ID = -1
  nativeKernelSigs.clear()
}

// CheckedResult constructors (mirror lib/kernel.disp):
//   Ok v = `t t v`        = fork(LEAF, v)
//   Fail = `t (t t)`      = stem(stem(LEAF))
//   is_ok r              = treeEqual(pair_fst r, LEAF) — true iff r = fork(LEAF, _)
const NATIVE_FAIL = stem(stem(LEAF))
function nativeOk(v: Tree): Tree { return fork(LEAF, v) }
function nativeIsOk(r: Tree): boolean { return r.tag === "fork" && r.left.tag === "leaf" }
function nativeOkValue(r: Tree): Tree {
  // r = Ok v = fork(LEAF, v); pair_snd r = v.
  return r.tag === "fork" ? r.right : LEAF
}

// pair_fst native (matches lib/prelude.disp):
//   pair_fst LEAF       = LEAF      (triage on_leaf branch)
//   pair_fst (stem c)   = c         (on_stem applied to inner c)
//   pair_fst (fork l r) = l         (on_fork applied to (l, r))
function nativePairFst(p: Tree): Tree {
  if (p.tag === "leaf") return LEAF
  if (p.tag === "stem") return p.child
  return p.left
}

// is_neutral native: fork-shaped value with kernel.hyp_reduce signature.
function nativeIsNeutralRoot(v: Tree): boolean {
  if (HYP_REDUCE_SIG_ID === -1) return false
  return v.tag === "fork" && v.left.id === HYP_REDUCE_SIG_ID
}

// Native dispatcher: signature check first, walker default last.
// Returns a Tree that is either nativeOk(value) or NATIVE_FAIL.
function nativeDispatch(f: Tree, x: Tree, budget: { remaining: number }): Tree {
  // hyp_reduce gets first-priority signature check (most frequent).
  if (HYP_REDUCE_SIG_ID !== -1 && nativePairFst(f).id === HYP_REDUCE_SIG_ID) {
    return nativeOk(apply(f, x, budget))
  }
  // Other kernel handlers route raw too.
  const fSig = nativePairFst(f).id
  for (const sigId of nativeKernelSigs.values()) {
    if (sigId === fSig && sigId !== HYP_REDUCE_SIG_ID) {
      return nativeOk(apply(f, x, budget))
    }
  }
  // Default: parametric walker.
  return nativeWalkerStep(f, x, budget)
}

function nativeWalkerStep(f: Tree, x: Tree, budget: { remaining: number }): Tree {
  // I-shortcut (only soundness carve-out per §6.3).
  if (NATIVE_I_CANONICAL_ID !== -1 && f.id === NATIVE_I_CANONICAL_ID) {
    return nativeOk(x)
  }
  // Triage f.
  if (f.tag === "leaf") {
    // Leaf rule: apply(LEAF, x) = stem(x). Stems are never neutrals.
    return nativeOk(stem(x))
  }
  if (f.tag === "stem") {
    // Stem rule: apply(stem(c), x) = fork(c, x). Reject if neutral-rooted.
    const r = fork(f.child, x)
    if (nativeIsNeutralRoot(r)) return NATIVE_FAIL
    return nativeOk(r)
  }
  // Fork f = fork(a, b). Triage on a to pick K / S / triage rule.
  const a = f.left, b = f.right
  if (a.tag === "leaf") {
    // K rule: apply(fork(LEAF, b), x) = b.
    return nativeOk(b)
  }
  if (a.tag === "stem") {
    // S rule: apply(fork(stem(c), b), x) = apply(apply(c, x), apply(b, x)).
    const c = a.child
    const cx = nativeDispatch(c, x, budget)
    if (!nativeIsOk(cx)) return NATIVE_FAIL
    const bx = nativeDispatch(b, x, budget)
    if (!nativeIsOk(bx)) return NATIVE_FAIL
    return nativeDispatch(nativeOkValue(cx), nativeOkValue(bx), budget)
  }
  // a = fork(tc, td) — triage rule on x.
  const tc = a.left, td = a.right
  if (nativeIsNeutralRoot(x)) return NATIVE_FAIL
  if (x.tag === "leaf") return nativeOk(tc)
  if (x.tag === "stem") return nativeDispatch(td, x.child, budget)
  // x = fork(l, r): apply(fork(fork(tc, td), b), fork(l, r)) = apply(apply(b, l), r).
  const bl = nativeDispatch(b, x.left, budget)
  if (!nativeIsOk(bl)) return NATIVE_FAIL
  return nativeDispatch(nativeOkValue(bl), x.right, budget)
}

// --- tree_eq host fast path ---
// `tree_eq` is defined as a recursive tree program in lib/prelude.disp.
// The host captures the id of its compiled tree at boot (via setTreeEqId)
// and short-circuits two-arg applications to an O(1) hash-cons identity
// check. This optimization must produce answers identical to the spec.
//
// TREE_EQ_PARTIAL_MARKER is a synthetic, host-internal tree shape used
// only as the left child of the partial-application intermediate. It does
// not collide with any canonical kernel checker signature, so user code
// inspecting the partial form via pair_fst will see the marker (an
// observable but harmless artifact of the optimization).
const TREE_EQ_PARTIAL_MARKER = fork(fork(stem(stem(LEAF)), LEAF), stem(stem(LEAF)))
let TREE_EQ_ID: number = -1
export function setTreeEqId(id: number): void { TREE_EQ_ID = id }
export function getTreeEqId(): number { return TREE_EQ_ID }

// --- Pretty printer ---

export function prettyTree(tree: Tree): string {
  switch (tree.tag) {
    case "leaf": return "△"
    case "stem": return `(△ ${prettyTree(tree.child)})`
    case "fork": return `(△ ${prettyTree(tree.left)} ${prettyTree(tree.right)})`
  }
}
