// Tree Calculus: Leaf | Stem(child) | Fork(left, right)
// Trees are always in normal form. Application is an operation.
//
// Evaluator state (apply memo, stats, the tree_eq id, the suspension toggle)
// lives in an `EagerState` bag reached through the module-level `active`
// pointer; an `EagerSession` owns one bag and swaps it in for the duration of
// its calls (see the EvalBackend section at the bottom). Node identity
// (hash-consing) and the unique-node counter stay GLOBAL — trees built under
// any session share one identity space, which is what lets the elaborator's
// cross-session/cross-file invariants (verifiedModules, the run.ts name
// registry) keep working. This is the stateful evaluator architecture described
// in EVALUATOR.md: no module-global *evaluator state*, only a global arena.
//
// This is core/tree.ts: the PRIVATE internals of the eager backend. The engine
// (compile.ts/run.ts) never imports it — it goes through the Session ABI; only
// eval/eager.ts (the backend) consumes these internals.

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
} | {
  // P(f, a): a suspended application `f a` (TC-Net's P node). It is the honest,
  // first-class representation of a partially-applied native primitive — today
  // only `tree_eq a`, awaiting its second operand — with NO synthetic marker.
  // It behaves exactly like the genuine reduct of `f a` under every observation:
  // applied to one more argument it takes the native fast-path; inspected
  // structurally (triaged, compared, printed) it is `force`d to its real reduct.
  // `forced` memoizes that reduct the first time it is demanded.
  readonly tag: "susp"
  readonly f: Tree
  readonly a: Tree
  forced: Tree | null
  readonly id: number
}

// --- Hash-consing (global arena: node identity is process-wide) ---

let nextId = 0
// The intern tables are SHARDED 64 ways (by low id bits): a single V8 Map caps
// at ~16.7M entries (RangeError: Map maximum size exceeded), and a full suite on
// the eager backend interns more stems than that in one process (the grow-only
// arena has no GC). Sharding lifts the ceiling to ~1B per node kind; the extra
// array index is noise next to the Map op.
const SHARDS = 64
const stemCache: Map<number, Tree>[] = Array.from({ length: SHARDS }, () => new Map())
const forkCache: Map<number, Map<number, Tree>>[] = Array.from({ length: SHARDS }, () => new Map())

// Global node-creation counter. Unique nodes are a property of the shared arena,
// not of any one session, so this stays module-global (incremented by the
// constructors below, read back through getApplyStats).
export const nodeStats = { uniqueNodes: 0 }

export const LEAF: Tree = { tag: "leaf", id: nextId++ }

export function stem(child: Tree): Tree {
  const shard = stemCache[child.id & (SHARDS - 1)]
  const cached = shard.get(child.id)
  if (cached) return cached
  const node: Tree = { tag: "stem", child, id: nextId++ }
  shard.set(child.id, node)
  nodeStats.uniqueNodes++
  if (scopeStack.length > 0) scopeLog.push(node)
  return node
}

export function fork(left: Tree, right: Tree): Tree {
  const shard = forkCache[left.id & (SHARDS - 1)]
  let inner = shard.get(left.id)
  if (inner) {
    const cached = inner.get(right.id)
    if (cached) return cached
  } else {
    inner = new Map()
    shard.set(left.id, inner)
  }
  const node: Tree = { tag: "fork", left, right, id: nextId++ }
  inner.set(right.id, node)
  nodeStats.uniqueNodes++
  if (scopeStack.length > 0) scopeLog.push(node)
  return node
}

// susp(f, a) = the suspended application `f a` (P node). Hash-consed like the
// constructors, so two identical partials share one node (O(1) treeEqual) and a
// forced value memoized on it is seen by every reference.
// Sharded by the ARGUMENT id: the operator set is tiny (today only tree_eq), so
// per-operator inner maps are what would hit the V8 entry ceiling.
const suspCache: Map<number, Map<number, Tree>>[] = Array.from({ length: SHARDS }, () => new Map())
export function susp(f: Tree, a: Tree): Tree {
  const shard = suspCache[a.id & (SHARDS - 1)]
  let inner = shard.get(f.id)
  if (inner) {
    const cached = inner.get(a.id)
    if (cached) return cached
  } else {
    inner = new Map()
    shard.set(f.id, inner)
  }
  const node: Tree = { tag: "susp", f, a, forced: null, id: nextId++ }
  inner.set(a.id, node)
  nodeStats.uniqueNodes++
  if (scopeStack.length > 0) scopeLog.push(node)
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

// --- Scoped reclamation (the eager side of Session.beginScope/endScope) ---
//
// V8 cannot collect an interned Tree: the intern tables above hold strong
// references to every node ever built, so a long shared-session run accumulates
// its whole workload (a full test suite exceeds the heap). Scoped reclamation is
// the GC-world mirror of rust-eager's watermark arena: beginScope starts logging
// newly interned nodes; endScope(keep) un-interns every logged node NOT
// reachable from `keep` and drops the active session's memo entries touching
// them, releasing the objects to V8. Ids are never reused (unlike the rust
// arena's slots), so a stale reference to a freed node cannot alias a NEW node;
// the soundness contract is the ABI's: the host must list every tree it keeps
// live past the scope — a kept-but-unlisted tree stays alive as a JS object but
// loses its intern slot, so a later structural rebuild gets a fresh id and
// id-equality (tree_eq) between the two copies wrongly answers false.
// collectSessionRoots (elab/state.ts) is the engine-side inventory. The memo
// prune touches only the ACTIVE session's memo, so scoping assumes one scoping
// session per process (the shared-session harness).

interface ScopeFrame { watermarkId: number; logStart: number }
const scopeStack: ScopeFrame[] = []
const scopeLog: Tree[] = []
// Cumulative count of un-interned nodes, reported via stats().free.
export const scopeStats = { freed: 0 }

export function beginScope(): void {
  scopeStack.push({ watermarkId: nextId, logStart: scopeLog.length })
}

export function endScope(keep: Tree[]): void {
  // Three phases: (1) mark everything reachable from `keep`; (2) sweep this
  // scope's intern log, un-interning unmarked nodes; (3) prune the apply memo
  // of entries touching freed ids and clear the recycled stack slots, so
  // nothing pins the freed objects against V8's collection.
  const frame = scopeStack.pop()
  if (!frame) return
  const wm = frame.watermarkId

  // Mark every node reachable from `keep`. The walk cannot skip pre-scope nodes:
  // `susp.forced` is assigned after construction, so an OLD susp can point at an
  // in-scope reduct (the one edge that breaks the children-are-older invariant).
  const marked = new Set<number>()
  const work: Tree[] = []
  const visit = (n: Tree): void => { if (!marked.has(n.id)) { marked.add(n.id); work.push(n) } }
  for (const r of keep) if (r != null) visit(r)
  while (work.length > 0) {
    const n = work.pop()!
    if (n.tag === "stem") visit(n.child)
    else if (n.tag === "fork") { visit(n.left); visit(n.right) }
    else if (n.tag === "susp") { visit(n.f); visit(n.a); if (n.forced !== null) visit(n.forced) }
  }

  // Sweep this scope's log: un-intern unmarked nodes. Under a nested scope the
  // survivors are re-logged (still candidates for the enclosing scope).
  const nested = scopeStack.length > 0
  const survivors: Tree[] = []
  for (let i = frame.logStart; i < scopeLog.length; i++) {
    const n = scopeLog[i]
    if (marked.has(n.id)) { if (nested) survivors.push(n); continue }
    if (n.tag === "stem") {
      stemCache[n.child.id & (SHARDS - 1)].delete(n.child.id)
    } else if (n.tag === "fork") {
      const shard = forkCache[n.left.id & (SHARDS - 1)]
      const inner = shard.get(n.left.id)
      if (inner !== undefined) { inner.delete(n.right.id); if (inner.size === 0) shard.delete(n.left.id) }
    } else if (n.tag === "susp") {
      const shard = suspCache[n.a.id & (SHARDS - 1)]
      const inner = shard.get(n.f.id)
      if (inner !== undefined) { inner.delete(n.a.id); if (inner.size === 0) shard.delete(n.f.id) }
    }
    scopeStats.freed++
  }
  scopeLog.length = frame.logStart
  for (const s of survivors) scopeLog.push(s)

  // Prune the active session's memo of entries touching freed nodes. Freed ids
  // are never reused, so a stale entry could never be looked up again — but it
  // would pin the freed objects against V8's collection. deadId is exact:
  // in-this-scope and unreachable = freed just now (ids freed by an inner scope
  // were pruned by that scope's own endScope).
  const st = active
  const deadId = (id: number): boolean => id >= wm && !marked.has(id)
  for (const [fid, inner] of st.applyMemo) {
    if (deadId(fid)) { st.applyMemoEntries -= inner.size; st.applyMemo.delete(fid); continue }
    let removed = 0
    for (const [xid, res] of inner) {
      if (deadId(xid) || deadId(res.id)) { inner.delete(xid); removed++ }
    }
    if (removed > 0) st.applyMemoEntries -= removed
    if (inner.size === 0) st.applyMemo.delete(fid)
  }

  // The continuation stack's recycled slots retain the last run's Tree refs;
  // reset them so they don't pin freed nodes (apply never runs across endScope).
  for (let i = 0; i < stack.length; i++) {
    const s = stack[i]
    s.f = LEAF; s.x = LEAF; s.arg = LEAF; s.func = LEAF; s.origX = LEAF; s.b = LEAF; s.innerMap = null
  }
}

// --- Per-session evaluator state ---
// Everything that a `clearApplyCache()` / `resetApplyStats()` used to touch lives
// here. The module-level `active` pointer names the state the free functions
// (apply/force/treeEqual/…) and the apply loop read; an EagerSession swaps it in.

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
  // Scoped-reclamation view (harness [scope] log): nodes = cumulative created,
  // free = cumulative un-interned; live = nodes - free.
  nodes: number
  free: number
}

// The mutable counters carried per session (the ApplyStats fields that are
// genuinely accumulated during reduction; the rest are derived in statsOf).
interface RuleCounters {
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
  maxStack: number
}

export interface EagerState {
  applyMemo: Map<number, Map<number, Tree>>
  applyMemoEntries: number
  applyMemoEntryLimit: number
  hits: number
  misses: number
  memoWrites: number
  counters: RuleCounters
  // `tree_eq`'s compiled tree id for this session's native fast-path (-1 = unset).
  treeEqId: number
  // While `force` runs, stage-1 suspension is disabled so the partial reduces to
  // its genuine combinator reduct instead of re-suspending.
  suspEnabled: boolean
}

export function freshCounters(): RuleCounters {
  return {
    calls: 0, steps: 0, leafRules: 0, stemRules: 0, kRules: 0, sRules: 0,
    triageLeafRules: 0, triageStemRules: 0, triageForkRules: 0, treeEqRules: 0, maxStack: 0,
  }
}

export function freshState(): EagerState {
  return {
    applyMemo: new Map(),
    applyMemoEntries: 0,
    applyMemoEntryLimit: 5_000_000,
    hits: 0,
    misses: 0,
    memoWrites: 0,
    counters: freshCounters(),
    treeEqId: -1,
    suspEnabled: true,
  }
}

// The currently-active evaluator state. Defaults to a throwaway bag; the eager
// backend (eval/eager.ts) swaps in a session's state via setActive for the
// duration of a call. These accessors exist because an imported `let` binding is
// read-only across modules, so eager.ts cannot reassign `active` directly.
let active: EagerState = freshState()
export function getActive(): EagerState { return active }
export function setActive(st: EagerState): void { active = st }

// --- Tree equality (O(1) via hash-consing) ---
// Reads `active` only via force() (suspEnabled); the comparison itself is
// identity + top-level susp forcing, both session-independent in result.

export function treeEqual(a: Tree, b: Tree): boolean {
  if (a.id === b.id) return true                       // identical (incl. shared susps)
  if (a.tag !== "susp" && b.tag !== "susp") return false
  // A suspended application is observationally its genuine reduct: force, compare.
  return force(a).id === force(b).id
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
// The memo is per-session: it lives on `active`.

export function clearApplyCache(): void {
  active.applyMemo.clear()
  active.applyMemoEntries = 0
}

export function setApplyCacheLimit(entries: number): void {
  if (!Number.isFinite(entries) || entries < 0) throw new Error("apply cache limit must be a non-negative finite number")
  active.applyMemoEntryLimit = entries
  trimApplyMemo()
}

export function applyCacheSize(): number {
  return active.applyMemoEntries
}

// --- Cache instrumentation ---
// `cacheStats` exposes the GLOBAL unique-node count (arena-wide). The hit/miss/
// write counters are per-session and read through getApplyStats; this object is
// kept (uniqueNodes only) for back-compat with callers that read it directly.
export const cacheStats = {
  get hits() { return active.hits },
  get misses() { return active.misses },
  get memoWrites() { return active.memoWrites },
  get uniqueNodes() { return nodeStats.uniqueNodes },
}
export function resetCacheStats(): void {
  active.hits = 0; active.misses = 0; active.memoWrites = 0
  nodeStats.uniqueNodes = 0
}

function memoGet(f: Tree, x: Tree): Tree | undefined {
  return active.applyMemo.get(f.id)?.get(x.id)
}

function memoSet(f: Tree, x: Tree, result: Tree): void {
  if (active.applyMemoEntryLimit === 0) return
  let m = active.applyMemo.get(f.id)
  if (!m) { m = new Map(); active.applyMemo.set(f.id, m) }
  if (!m.has(x.id)) active.applyMemoEntries++
  m.set(x.id, result)
  active.memoWrites++
  trimApplyMemo()
}

// Evict entries until under the limit. Eviction is FIFO in JavaScript Map
// insertion order (iteration always yields oldest-inserted first), not LRU —
// there is no recency tracking on memoGet.
//
// Batch eviction: drop to a low-water mark (87.5% of the limit) in ONE pass over
// the live Map iterators, rather than one entry per call. The old one-at-a-time
// form allocated TWO iterators (`applyMemo.keys().next()`, `inner.keys().next()`)
// PER eviction; at the cap that fired on every memoSet, so a verify whose working
// set just exceeds the limit paid ~5x in iterator-alloc + GC churn for ZERO extra
// reduction steps (a hard performance cliff at the cap). Evicting a batch under one
// pair of iterators, then sitting idle until the next 12.5% refills, removes it.
export function trimApplyMemo(): void {
  if (active.applyMemoEntries <= active.applyMemoEntryLimit) return
  const applyMemo = active.applyMemo
  const target = active.applyMemoEntryLimit - (active.applyMemoEntryLimit >> 3) // 87.5%
  for (const [outerKey, inner] of applyMemo) {
    for (const innerKey of inner.keys()) {
      inner.delete(innerKey)
      active.applyMemoEntries--
      if (active.applyMemoEntries <= target) {
        if (inner.size === 0) applyMemo.delete(outerKey)
        return
      }
    }
    applyMemo.delete(outerKey) // inner fully drained; remove the empty outer bucket
  }
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
  active.counters = freshCounters()
  active.hits = 0
  active.misses = 0
  active.memoWrites = 0
}

export function statsOf(st: EagerState): ApplyStats {
  return {
    ...st.counters,
    memoHits: st.hits,
    memoMisses: st.misses,
    memoWrites: st.memoWrites,
    cacheEntries: st.applyMemoEntries,
    uniqueNodes: nodeStats.uniqueNodes,
    nodes: nodeStats.uniqueNodes,
    free: scopeStats.freed,
  }
}

export function getApplyStats(): ApplyStats {
  return statsOf(active)
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

// The argument is the exhausted budget's LIMIT (for the message); 0 = unknown
// (a caller that built its budget without a `limit` field).
export class BudgetExhausted extends Error {
  constructor(budget: number) {
    super(budget > 0 ? `Evaluation budget exhausted (${budget}-step limit)` : `Evaluation budget exhausted`)
  }
}

// Fully iterative apply with explicit continuation stack.
// No recursive calls — all evaluation driven by the main loop.
//
// The loop is a defunctionalized CPS machine: a rule that needs a sub-result
// pushes a frame and jumps to evaluating the sub-problem; `deliver` pops
// frames until one wants more work (or the stack drains to baseTop = done).
//   ApplyTo(arg)        — result is the operator: continue with result(arg)
//   ApplyResultTo(func) — result is the operand:  continue with func(result)
//   Memo(f, x)          — write result into the apply memo for (f, x); keep popping
//   SAfterCx(b, origX)  — the S rule's join point after c(x); see deliver

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
// apply() calls (including recursive ones). It is transient scratch (empty
// between top-level calls, baseTop-isolated within them), NOT session state, so
// it stays global: JS is single-threaded and a session's apply runs to
// completion before another's starts. Each apply() captures stackTop at entry
// and only pops down to that `baseTop`, so nested calls don't interfere. No
// per-push allocation: pushes overwrite slot fields in place.
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

export function apply(fInit: Tree, xInit: Tree, budget: { remaining: number; limit?: number } = { remaining: 10000 }): Tree {
  const st = active
  const counters = st.counters
  counters.calls++
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
        if (st.applyMemoEntryLimit !== 0) {
          if (!m) { m = new Map(); st.applyMemo.set(sf.id, m) }
          if (!m.has(sx.id)) st.applyMemoEntries++
          m.set(sx.id, result)
          st.memoWrites++
          if (st.applyMemoEntries > st.applyMemoEntryLimit) trimApplyMemo()
        }
        continue  // keep popping
      }
      // SAfterCx: result = c(x) for the S rule (fork(stem(c), b) · x).
      //   K-discard : c(x) = K(w) → answer is w, (b x) is dead and never evaluated
      //   C fast-path: b = K(u)   → answer is c(x)(u), no (b x) application
      //   general    : evaluate b(origX), then ApplyResultTo feeds it to c(x)
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
    if (budget.remaining <= 0) throw new BudgetExhausted(budget.limit ?? 0)
    if (stackTop > counters.maxStack) counters.maxStack = stackTop

    // Leaf/Stem: immediate result
    if (isLeaf(curF)) { traceApply("leaf", curF, curX, stackTop); counters.leafRules++; const r = deliver(stem(curX)); if (r !== null) return r; continue }
    if (isStem(curF)) { traceApply("stem", curF, curX, stackTop); counters.stemRules++; const r = deliver(fork(curF.child, curX)); if (r !== null) return r; continue }

    // Suspended application as the operator — the only non-fork tag left, so it is
    // checked HERE (after leaf/stem, which therefore skip it) and stands in for the
    // old non-fork guard, leaving the fork path's check count unchanged. (f a)
    // applied to curX; tree_eq stage 2: a `tree_eq a` partial meeting its second
    // operand is the O(1) hash-cons equality. Other suspensions: force, then apply.
    if (curF.tag === "susp") {
      if (st.treeEqId !== -1 && curF.f.id === st.treeEqId) {
        traceApply("tree_eq", curF, curX, stackTop)
        counters.treeEqRules++
        const r = deliver(treeEqual(curF.a, curX) ? TREE_TRUE : TREE_FALSE)
        if (r !== null) return r; continue
      }
      curF = force(curF); continue
    }
    if (!isFork(curF)) throw new Error("apply: impossible non-fork function")

    // tree_eq host fast path (stage 1): apply(tree_eq, a) suspends as the honest
    // P(tree_eq, a) — a first-class value, no synthetic marker. Canonical
    // definition lives in lib/prelude.disp as a recursive triage form; the host
    // captures its compiled tree id at boot (via setTreeEqId). Checked BEFORE the
    // memo so a natural reduct cached under (tree_eq, a) during `force` can never
    // shadow it. Stage 2 (the partial meeting its second operand → O(1) compare)
    // is the `curF.tag === "susp"` branch above. `suspEnabled` is cleared only
    // while `force` materializes the genuine reduct, so forcing is non-recursive.
    if (st.suspEnabled && st.treeEqId !== -1 && curF.id === st.treeEqId) {
      traceApply("tree_eq", curF, curX, stackTop)
      counters.treeEqRules++
      const r = deliver(susp(curF, curX))
      if (r !== null) return r; continue
    }

    // Memo check (inlined so we can capture the inner map for memoSet
    // reuse if this turns out to be a miss).
    const innerMap = st.applyMemo.get(curF.id)
    const c = innerMap?.get(curX.id)
    if (c !== undefined) { st.hits++; const r = deliver(c); if (r !== null) return r; continue }
    st.misses++

    budget.remaining--
    counters.steps++
    if (budget.remaining <= 0) throw new BudgetExhausted(budget.limit ?? 0)

    const a = curF.left, b = curF.right

    // A suspension in operator position (fork(susp, b) applied) needs its real
    // shape to pick the K/S/triage rule. Rare (tree_eq partials are saturated),
    // but forced here for totality. Re-dispatch on the materialized operator.
    if (a.tag === "susp") { curF = fork(force(a), b); continue }

    // K rule is O(1) and produces a value constant in curX (just b).
    // Caching (curF, curX) -> b would mostly pollute the cache with
    // entries that won't repeat. Skip the Memo frame entirely.
    if (isLeaf(a)) {
      traceApply("K", curF, curX, stackTop)
      counters.kRules++
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
      counters.sRules++
      const c = a.child
      if (isFork(c) && isLeaf(c.left)) {
        // (c x) = c.right =: v without an application. BEFORE scheduling (b x),
        // apply the same K-discard SAfterCx uses: if v = K(w), the result is w and
        // (b x) is never evaluated. Skipping this check here made the JS schedule
        // STRICTER than rust-eager's on S (K (K w)) b x — nested constant thunks,
        // the K-composition shapes bracket abstraction emits — and a discarded
        // divergent (b x) then hung the eager backend where rust terminated
        // (the 2026-07 kernel leans on that discard; found via the R6-check regress).
        const v = c.right
        if (isFork(v) && isLeaf(v.left)) {
          const r = deliver(v.right); if (r !== null) return r; continue
        }
        if (isFork(b) && isLeaf(b.left)) { curF = v; curX = b.right; continue }
        ensureStackSlot()
        const s = stack[stackTop++]
        s.kind = ContKind.ApplyResultTo; s.func = v
        curF = b; continue  // curX unchanged
      }
      // General S: compute c(x), then SAfterCx handles the rest
      ensureStackSlot()
      const s = stack[stackTop++]
      s.kind = ContKind.SAfterCx; s.origX = curX; s.b = b
      curF = c; continue  // curX unchanged
    }

    // Triage. The scrutinee's structure is observed here — so a suspended
    // application is forced to its genuine reduct first (this is the transparency
    // point: "if it is ever triaged on, do the real computation").
    if (!isFork(a)) throw new Error("apply: impossible non-fork branch")
    const tc = a.left, td = a.right
    if (curX.tag === "susp") curX = force(curX)
    if (isLeaf(curX)) { traceApply("T_leaf", curF, curX, stackTop); counters.triageLeafRules++; const r = deliver(tc); if (r !== null) return r; continue }
    if (isStem(curX)) { traceApply("T_stem", curF, curX, stackTop); counters.triageStemRules++; curF = td; curX = curX.child; continue }
    if (!isFork(curX)) throw new Error("apply: impossible non-fork argument")
    traceApply("T_fork", curF, curX, stackTop)
    counters.triageForkRules++
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

// Bool constants (raw shapes per TYPE_THEORY §2.7). These are the exact
// hash-cons-identity trees the prelude's `true := t` and `false := t t`
// compile to. Captured here so the tree_eq fast-path can return them
// directly without needing a runtime hook from compile.ts. (Scott-encoded
// K K / K (K I) until 2026-07-07 — the §2.7 polarity migration.)
export const TREE_TRUE = LEAF                       // △
export const TREE_FALSE = K                         // △ △ = stem(LEAF)

// --- tree_eq host fast path ---
// `tree_eq` is defined as a recursive tree program in lib/prelude.disp. The host
// captures its compiled tree id at boot (via setTreeEqId) and short-circuits
// two-arg applications to an O(1) hash-cons identity check, in two stages:
//   apply(tree_eq, a)         → susp(tree_eq, a)            (P node, stage 1)
//   apply(susp(tree_eq, a), b) → treeEqual(a, b) ? true : false  (stage 2)
// The intermediate is the honest suspended application P(tree_eq, a): no synthetic
// marker, and fully transparent — `force` materializes the genuine recursive-triage
// reduct the moment the partial is inspected structurally instead of applied. The
// optimization yields answers identical to the spec. The id is per-session (the
// registry handle in Phase-2 terms), read from `active`.
export function setTreeEqId(id: number): void { active.treeEqId = id }
export function getTreeEqId(): number { return active.treeEqId }

const FORCE_BUDGET = 10_000_000

// Materialize a suspended application to its genuine reduct, memoized on the node.
// Non-susp inputs pass through, so `force` doubles as a transparent deref. The
// suspEnabled toggle lives on `active`; tree_eq's partial never re-applies tree_eq
// to a single argument, so this does not nest; saved/restored for robustness.
export function force(t: Tree): Tree {
  if (t.tag !== "susp") return t
  if (t.forced !== null) return t.forced
  const prev = active.suspEnabled
  active.suspEnabled = false
  try {
    t.forced = apply(t.f, t.a, { remaining: FORCE_BUDGET })
  } finally {
    active.suspEnabled = prev
  }
  return t.forced
}

// --- Pretty printer ---

// Greedy top-down naming: `names` maps a hash-consed tree id to a source-level
// name (built by the driver from the program's definitions). At each node we try
// the lookup FIRST — so the largest named subtree wins — and only descend into
// children that aren't themselves named, bottoming out at △. A node that *is* a
// named definition (a type like `Nat`, a value like `Err`/`true`) prints as that
// name instead of △-spam. The lookup runs before `force` so a named wait-form
// (e.g. `Nat`, a stuck susp) matches as-is rather than being expanded. With no
// `names` map this degrades to the raw △ representation (back-compat).
export function prettyTree(tree: Tree, names?: Map<number, string>): string {
  const named = names?.get(tree.id)
  if (named !== undefined) return named
  switch (tree.tag) {
    case "leaf": return "△"
    case "stem": return `(△ ${prettyTree(tree.child, names)})`
    case "fork": return `(△ ${prettyTree(tree.left, names)} ${prettyTree(tree.right, names)})`
    case "susp": return prettyTree(force(tree), names)   // unnamed: print the genuine reduct (transparent)
  }
}

// --- Ternary serialization (preorder arity encoding) ---
// Each node is its arity digit in preorder: leaf "0", stem "1"+child, fork
// "2"+left+right. Canonical (one string per tree) — the recognition hash and the
// dump-comparison observation bridge both rely on that. dumpTernary forces
// suspensions (it is a forcing op). NOTE: this is *a* preorder arity encoding;
// reconciling its exact bytes with the lambada batch-tier convention is Phase 4
// and a deep-tree iterative encoder avoids recursive dump blowups
// — the recursive form here is fine for the small terms dump is used on so far.
export function encodeTernary(t: Tree): string {
  const parts: string[] = []
  const go = (n: Tree): void => {
    n = force(n)
    switch (n.tag) {
      case "leaf": parts.push("0"); return
      case "stem": parts.push("1"); go(n.child); return
      case "fork": parts.push("2"); go(n.left); go(n.right); return
      case "susp": go(force(n)); return  // unreachable after force; exhaustiveness
    }
  }
  go(t)
  return parts.join("")
}

export function decodeTernary(s: string): Tree {
  let i = 0
  const go = (): Tree => {
    if (i >= s.length) throw new Error("loadTernary: unexpected end of input")
    const c = s[i++]
    if (c === "0") return LEAF
    if (c === "1") return stem(go())
    if (c === "2") { const l = go(); const r = go(); return fork(l, r) }
    throw new Error(`loadTernary: bad character '${c}' at ${i - 1}`)
  }
  const t = go()
  if (i !== s.length) throw new Error("loadTernary: trailing input after one term")
  return t
}
