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
const forkCache = new Map<string, Tree>()

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
  const key = `${left.id},${right.id}`
  const cached = forkCache.get(key)
  if (cached) return cached
  const node: Tree = { tag: "fork", left, right, id: nextId++ }
  forkCache.set(key, node)
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

// --- Cache instrumentation ---
export const cacheStats = { hits: 0, misses: 0, memoWrites: 0, uniqueNodes: 0 }
export function resetCacheStats(): void {
  cacheStats.hits = 0; cacheStats.misses = 0; cacheStats.memoWrites = 0; cacheStats.uniqueNodes = 0
}

export function clearApplyCache(): void {
  applyMemo.clear()
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
          let m = applyMemo.get(c.f.id)
          if (!m) { m = new Map(); applyMemo.set(c.f.id, m) }
          m.set(c.x.id, result)
          cacheStats.memoWrites++
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

    // Leaf/Stem: immediate result
    if (isLeaf(curF)) { const r = deliver(stem(curX)); if (r !== null) return r; continue }
    if (isStem(curF)) { const r = deliver(fork(curF.child, curX)); if (r !== null) return r; continue }

    // Memo check
    const mi = applyMemo.get(curF.id)
    if (mi) { const c = mi.get(curX.id); if (c !== undefined) { cacheStats.hits++; const r = deliver(c); if (r !== null) return r; continue } }
    cacheStats.misses++

    // FAST_EQ
    if (curF.left.id === FAST_EQ_MARKER.id) {
      const v = treeEqual(curF.right, curX) ? LEAF : stem(LEAF)
      let m = applyMemo.get(curF.id); if (!m) { m = new Map(); applyMemo.set(curF.id, m) }; m.set(curX.id, v)
      const r = deliver(v); if (r !== null) return r; continue
    }

    budget.remaining--
    if (budget.remaining <= 0) throw new BudgetExhausted(0)

    const a = curF.left, b = curF.right
    stack.push({ kind: ContKind.Memo, f: curF, x: curX })

    if (isLeaf(a)) {
      // K rule
      const r = deliver(b); if (r !== null) return r; continue
    }

    if (isStem(a)) {
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
    if (isLeaf(curX)) { const r = deliver(tc); if (r !== null) return r; continue }
    if (isStem(curX)) { curF = td; curX = curX.child; continue }
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
