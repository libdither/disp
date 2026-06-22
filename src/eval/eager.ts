// The eager reference backend (EVALUATOR_PLAN.md §4.1 / §5 eval/eager.ts).
//
// EagerSession wraps one EagerState bag from core/tree.ts and swaps it in as the
// active state (via setActive) for the duration of each call, so core's free
// functions and the apply loop operate on it. The hash-cons arena stays global,
// so handles are canonical across sessions. This is the ONLY module that reaches
// into core/tree.ts's internals; the engine talks to it (and the Session ABI),
// never to core directly.

import {
  type Tree, LEAF, stem, fork, apply, applyTree, force, treeEqual,
  encodeTernary, decodeTernary, prettyTree,
  type EagerState, type ApplyStats, freshState, freshCounters, statsOf, trimApplyMemo, nodeStats,
  getActive, setActive,
} from "../core/tree.js"
import type { Session, EvalBackend, SessionOpts, Budget, EvalStats, Classification } from "./types.js"

// Re-export the bits the engine legitimately needs: the handle TYPE (it uses
// Session<Tree>) and the pretty-printer (error messages over Tree handles). The
// engine imports these from here, not from core/tree.ts.
export type { Tree } from "../core/tree.js"
export { prettyTree } from "../core/tree.js"

export class EagerSession implements Session<Tree> {
  // Not marked #private: withActiveSession reads it.
  readonly st: EagerState = freshState()

  // Handles are hash-consed Tree pointers, so identity coincides with equality.
  // The engine may use this only as an optimization gate (run.ts name registry).
  readonly canonicalHandles = true

  // ── term algebra (hash-consing is global; these just expose the arena) ──
  leaf(): Tree { return LEAF }
  stem(child: Tree): Tree { return stem(child) }
  fork(left: Tree, right: Tree): Tree { return fork(left, right) }

  // ── bulk ops / interchange ──
  loadTernary(s: string): Tree { return decodeTernary(s) }
  dumpTernary(h: Tree, _budget?: Budget): string {
    const prev = getActive(); setActive(this.st)
    try { return encodeTernary(h) } finally { setActive(prev) }
  }

  // ── observations (native overrides of the engine-side derivations) ──
  equal(a: Tree, b: Tree, _budget?: Budget): boolean { return this.treeEqual(a, b) }
  classify(h: Tree, _budget?: Budget): Classification<Tree> {
    const prev = getActive(); setActive(this.st)
    try {
      let n = force(h)
      while (n.tag === "susp") n = force(n)   // force returns the NF; loop is defensive + narrows
      if (n.tag === "leaf") return { tag: "leaf" }
      if (n.tag === "stem") return { tag: "stem", child: n.child }
      return { tag: "fork", left: n.left, right: n.right }
    } finally { setActive(prev) }
  }
  stats(): EvalStats { return statsOf(this.st) }

  // ── computation ──
  apply(f: Tree, x: Tree, budget = { remaining: 10000 }): Tree {
    const prev = getActive(); setActive(this.st)
    try { return apply(f, x, budget) } finally { setActive(prev) }
  }
  applyTree(f: Tree, x: Tree, maxSteps = 10000): Tree {
    const prev = getActive(); setActive(this.st)
    try { return applyTree(f, x, maxSteps) } finally { setActive(prev) }
  }
  force(t: Tree): Tree {
    const prev = getActive(); setActive(this.st)
    try { return force(t) } finally { setActive(prev) }
  }
  treeEqual(a: Tree, b: Tree): boolean {
    const prev = getActive(); setActive(this.st)
    try { return treeEqual(a, b) } finally { setActive(prev) }
  }

  // ── tree_eq native fast-path registration ──
  setTreeEqId(id: number): void { this.st.treeEqId = id }
  getTreeEqId(): number { return this.st.treeEqId }
  // ABI hook: idempotently register the tree_eq handle for the fast path.
  recognizeNative(name: string, handle: Tree): void {
    if (name === "tree_eq" && this.st.treeEqId === -1) this.st.treeEqId = handle.id
  }

  // ── stats / cache (read this session's own state) ──
  getApplyStats(): ApplyStats { return statsOf(this.st) }
  resetApplyStats(): void {
    this.st.counters = freshCounters()
    this.st.hits = 0; this.st.misses = 0; this.st.memoWrites = 0
  }
  resetCacheStats(): void {
    this.st.hits = 0; this.st.misses = 0; this.st.memoWrites = 0
    nodeStats.uniqueNodes = 0
  }
  clear(): void { this.st.applyMemo.clear(); this.st.applyMemoEntries = 0 }
  setApplyCacheLimit(entries: number): void {
    if (!Number.isFinite(entries) || entries < 0) throw new Error("apply cache limit must be a non-negative finite number")
    this.st.applyMemoEntryLimit = entries
    const prev = getActive(); setActive(this.st)
    try { trimApplyMemo() } finally { setActive(prev) }
  }
  applyCacheSize(): number { return this.st.applyMemoEntries }

  // ── lifecycle ──
  dispose(): void { this.clear() }
}

// Run `fn` with `session` active, restoring the previous active state after.
export function withActiveSession<T>(session: EagerSession, fn: () => T): T {
  const prev = getActive()
  setActive(session.st)
  try { return fn() } finally { setActive(prev) }
}

// The default session backs callers that don't manage their own (the elaborator
// default, standalone helpers). Make it core's default active state too, so the
// free functions (apply/treeEqual/… and the tree_eq fast path registered on it)
// operate on defaultSession rather than core's throwaway bag — restoring the
// pre-split behavior for anything that loads this module.
export const defaultSession = new EagerSession()
setActive(defaultSession.st)

export const eagerBackend: EvalBackend<Tree> = {
  name: "eager",
  // Recognition-by-hash (natives() + the engine-side hash assertion, §3.1) is a
  // later sub-step; today tree_eq is registered via recognizeNative at
  // kernel-load time, so none are reported here.
  natives(): ReadonlyMap<string, readonly string[]> { return new Map() },
  createSession(_opts?: SessionOpts): EagerSession { return new EagerSession() },
}
