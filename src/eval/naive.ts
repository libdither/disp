// The naive "honesty" backend (EVALUATOR_PLAN.md Phase 3).
//
// Deliberately the opposite of the eager reference where it could hide a latent
// assumption: NO hash-consing (every leaf/stem/fork is a FRESH object, so two
// structurally-equal trees are DISTINCT handles), structural recursive `equal`,
// and `canonicalHandles: false`. If the engine or compile.ts secretly relied on
// canonical handles or O(1) id-equality, running on this backend breaks; that it
// doesn't is the cheap proof the ABI is real.
//
// It KEEPS a memo — but keyed by *structure* (a content hash + collision check),
// not by canonical id — because "memoization is not a canonical-handle
// assumption" (plan, Phase 3): without it the no-hash-consing re-reduction of
// shared subterms makes the conversion-heavy kernel exponential (measured: pure
// no-memo exhausts 1e9 steps in ~22s on nat.test.disp). The memo shares computed
// RESULTS; constructed handles stay non-canonical, so the assumption-flushing
// property is intact. This is the "memoized-naive variant" the plan calls for.
//
// Handles are `Tree`-shaped (satisfying compile.ts's Session<Tree>) but minted
// fresh here, never through the global hash-cons constructors.

import type { Session, EvalBackend, SessionOpts, Budget, Classification, EvalStats } from "./types.js"
import type { Tree } from "../tree.js"

// Fresh, non-deduplicated constructors. The id is a throwaway uniqueness stamp,
// NOT a canonical structural identity — `equal` ignores it and walks structure.
let nid = 1
const mkLeaf = (): Tree => ({ tag: "leaf", id: nid++ })
const mkStem = (child: Tree): Tree => ({ tag: "stem", child, id: nid++ })
const mkFork = (left: Tree, right: Tree): Tree => ({ tag: "fork", left, right, id: nid++ })

export class NaiveBudgetExhausted extends Error {
  constructor() { super("Evaluation budget exhausted (naive backend)") }
}

// Structural equality — an explicit-stack walk (recursion would overflow on deep
// kernel trees). The whole point: NO id shortcut.
function structEqual(a: Tree, b: Tree): boolean {
  if (a === b) return true
  const stack: [Tree, Tree][] = [[a, b]]
  while (stack.length) {
    const [x, y] = stack.pop()!
    if (x === y) continue
    if (x.tag !== y.tag) return false
    if (x.tag === "stem" && y.tag === "stem") stack.push([x.child, y.child])
    else if (x.tag === "fork" && y.tag === "fork") stack.push([x.left, y.left], [x.right, y.right])
    else if (x.tag === "susp" || y.tag === "susp") return false   // naive never produces susps
  }
  return true
}

// Structural content hash, WeakMap-memoized, computed by explicit-stack
// post-order (deep kernel trees would overflow a recursive hash). Used only as
// a memo key; collisions are resolved by structEqual, so it need not be perfect.
const hashCache = new WeakMap<Tree, number>()
function mix(a: number, b: number): number {
  return (((a ^ (b + 0x9e3779b9 + (a << 6) + (a >>> 2))) >>> 0))
}
function hashOf(root: Tree): number {
  const seen = hashCache.get(root)
  if (seen !== undefined) return seen
  const stack: Tree[] = [root]
  while (stack.length) {
    const t = stack[stack.length - 1]
    if (hashCache.has(t)) { stack.pop(); continue }
    if (t.tag === "leaf") { hashCache.set(t, 1); stack.pop(); continue }
    if (t.tag === "stem") {
      const hc = hashCache.get(t.child)
      if (hc !== undefined) { hashCache.set(t, mix(2, hc)); stack.pop() }
      else stack.push(t.child)
      continue
    }
    if (t.tag === "fork") {
      const hl = hashCache.get(t.left), hr = hashCache.get(t.right)
      if (hl !== undefined && hr !== undefined) { hashCache.set(t, mix(hl, hr) ^ 0x55) ; stack.pop() }
      else { if (hr === undefined) stack.push(t.right); if (hl === undefined) stack.push(t.left) }
      continue
    }
    stack.pop()   // susp: unreachable
  }
  return hashCache.get(root)!
}

// Structural memo: key -> bucket of (f, x, result). Per session. Capped with
// FIFO eviction (like the eager backend's trimApplyMemo) — otherwise the kernel's
// millions of distinct redexes overflow JS's max Map size. Eviction only costs
// re-reduction, never correctness.
type Memo = Map<number, [Tree, Tree, Tree][]>
const MEMO_KEY_LIMIT = 4_000_000
function memoGet(m: Memo, f: Tree, x: Tree): Tree | undefined {
  const bucket = m.get(mix(hashOf(f), hashOf(x)))
  if (!bucket) return undefined
  for (const e of bucket) if (structEqual(e[0], f) && structEqual(e[1], x)) return e[2]
  return undefined
}
function memoSet(m: Memo, f: Tree, x: Tree, r: Tree): void {
  const k = mix(hashOf(f), hashOf(x))
  const bucket = m.get(k)
  if (bucket) bucket.push([f, x, r]); else m.set(k, [[f, x, r]])
  if (m.size > MEMO_KEY_LIMIT) {
    const target = m.size - (MEMO_KEY_LIMIT >> 3)   // drop ~12.5% oldest in one pass
    for (const key of m.keys()) { m.delete(key); if (m.size <= target) break }
  }
}

// Iterative tree-calculus reducer, ported from the eager loop's continuation
// stack, minus the susp/tree_eq fast paths. The memo is structural (above).
const enum Cont { ApplyTo, ApplyResultTo, SAfterCx, Memo }
interface Frame { kind: Cont; arg?: Tree; func?: Tree; b?: Tree; origX?: Tree; f?: Tree; x?: Tree }

function naiveApply(fInit: Tree, xInit: Tree, budget: Budget, memo: Memo): Tree {
  let curF = fInit, curX = xInit
  const stack: Frame[] = []

  const deliver = (result: Tree): Tree | null => {
    while (stack.length) {
      const fr = stack.pop()!
      if (fr.kind === Cont.ApplyTo) { curF = result; curX = fr.arg!; return null }
      if (fr.kind === Cont.ApplyResultTo) { curF = fr.func!; curX = result; return null }
      if (fr.kind === Cont.Memo) { memoSet(memo, fr.f!, fr.x!, result); continue }
      // SAfterCx: `result` is c(x); now compute apply(result, b(origX)).
      stack.push({ kind: Cont.ApplyResultTo, func: result })
      curF = fr.b!; curX = fr.origX!; return null
    }
    return result
  }

  for (;;) {
    if (budget.remaining <= 0) throw new NaiveBudgetExhausted()
    if (curF.tag === "leaf") { const r = deliver(mkStem(curX)); if (r) return r; continue }
    if (curF.tag === "stem") { const r = deliver(mkFork(curF.child, curX)); if (r) return r; continue }
    if (curF.tag === "susp") throw new Error("naive backend: unexpected suspension")

    const a = curF.left, b = curF.right
    if (a.tag === "leaf") { const r = deliver(b); if (r) return r; continue }   // K: O(1), no memo

    const hit = memoGet(memo, curF, curX)
    if (hit !== undefined) { const r = deliver(hit); if (r) return r; continue }
    budget.remaining--
    stack.push({ kind: Cont.Memo, f: curF, x: curX })   // store the NF of apply(curF,curX) on the way back

    if (a.tag === "stem") {                  // S: △(△c)b x → (c x)(b x)
      stack.push({ kind: Cont.SAfterCx, b, origX: curX })
      curF = a.child                         // evaluate c x (curX stays = x)
      continue
    }
    if (a.tag === "susp") throw new Error("naive backend: unexpected suspension (operator)")
    if (curX.tag === "susp") throw new Error("naive backend: unexpected suspension (argument)")
    if (curX.tag === "leaf") { const r = deliver(a.left); if (r) return r; continue }   // △(△c d)b △ → c
    if (curX.tag === "stem") { curF = a.right; curX = curX.child; continue }            // → d u
    // △(△c d)b (△u v) → (b u) v
    stack.push({ kind: Cont.ApplyTo, arg: curX.right })
    curF = b; curX = curX.left
    continue
  }
}

// Ternary codec with the fresh constructors (mirrors tree.ts's codec semantics).
function naiveEncode(t: Tree): string {
  const parts: string[] = []
  const stackT: (Tree | string)[] = [t]
  // iterative preorder to avoid recursion overflow on deep trees
  while (stackT.length) {
    const n = stackT.pop()!
    if (typeof n === "string") { parts.push(n); continue }
    if (n.tag === "leaf") parts.push("0")
    else if (n.tag === "stem") { parts.push("1"); stackT.push(n.child) }
    else if (n.tag === "fork") { parts.push("2"); stackT.push(n.right, n.left) }
    else throw new Error("naive backend: cannot dump a suspension")
  }
  return parts.join("")
}
function naiveDecode(s: string): Tree {
  let i = 0
  const go = (): Tree => {
    if (i >= s.length) throw new Error("loadTernary: unexpected end of input")
    const c = s[i++]
    if (c === "0") return mkLeaf()
    if (c === "1") return mkStem(go())
    if (c === "2") { const l = go(); const r = go(); return mkFork(l, r) }
    throw new Error(`loadTernary: bad character '${c}' at ${i - 1}`)
  }
  const t = go()
  if (i !== s.length) throw new Error("loadTernary: trailing input after one term")
  return t
}

const DEFAULT_BUDGET = 2_000_000_000

export class NaiveSession implements Session<Tree> {
  readonly canonicalHandles = false
  private readonly memo: Memo = new Map()
  private steps = 0

  leaf(): Tree { return mkLeaf() }
  stem(child: Tree): Tree { return mkStem(child) }
  fork(left: Tree, right: Tree): Tree { return mkFork(left, right) }

  loadTernary(s: string): Tree { return naiveDecode(s) }
  dumpTernary(h: Tree, _budget?: Budget): string { return naiveEncode(h) }

  apply(f: Tree, x: Tree, budget?: Budget): Tree {
    // Budgets are backend-declared and non-portable (EVALUATOR_PLAN §8/dec. 9):
    // naive steps are finer-grained, so a caller's eager-tuned budget would
    // spuriously reject — use our own generous divergence bound.
    const b: Budget = { remaining: Math.max(budget?.remaining ?? 0, DEFAULT_BUDGET) }
    const r = naiveApply(f, x, b, this.memo)
    this.steps += DEFAULT_BUDGET - b.remaining
    return r
  }
  equal(a: Tree, b: Tree, _budget?: Budget): boolean { return structEqual(a, b) }
  classify(h: Tree, _budget?: Budget): Classification<Tree> {
    if (h.tag === "leaf") return { tag: "leaf" }
    if (h.tag === "stem") return { tag: "stem", child: h.child }
    if (h.tag === "fork") return { tag: "fork", left: h.left, right: h.right }
    throw new Error("naive backend: cannot classify a suspension")
  }
  stats(): EvalStats { return { steps: this.steps, memoEntries: this.memo.size } }

  // No native fast path — tree_eq runs as the in-language program.
  recognizeNative(_name: string, _handle: Tree): void { /* no-op */ }

  dispose(): void { this.memo.clear() }
}

export const naiveBackend: EvalBackend<Tree> = {
  name: "naive",
  natives(): ReadonlyMap<string, readonly string[]> { return new Map() },
  createSession(_opts?: SessionOpts): NaiveSession { return new NaiveSession() },
}
