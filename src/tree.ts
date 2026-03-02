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
  return node
}

export function fork(left: Tree, right: Tree): Tree {
  const key = `${left.id},${right.id}`
  const cached = forkCache.get(key)
  if (cached) return cached
  const node: Tree = { tag: "fork", left, right, id: nextId++ }
  forkCache.set(key, node)
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

export function apply(f: Tree, x: Tree, budget = { remaining: 10000 }): Tree {
  if (budget.remaining <= 0) throw new BudgetExhausted(0)
  budget.remaining--

  if (isLeaf(f)) return stem(x)       // △ applied to x → stem(x)
  if (isStem(f)) return fork(f.child, x) // stem(a) applied to x → fork(a, x)

  // f is fork(a, b)
  const a = f.left
  const b = f.right

  if (isLeaf(a)) {
    // Rule 1: △ △ b x → b (K combinator: return first arg)
    return b
  }
  if (isStem(a)) {
    // Rule 2: △ (△ c) b x → c x (b x) (S combinator)
    const c = a.child
    const cx = apply(c, x, budget)
    const bx = apply(b, x, budget)
    return apply(cx, bx, budget)
  }

  // a is fork(c, d): triage rules
  const c = a.left
  const d = a.right

  if (isLeaf(x)) {
    // Rule 3a: triage leaf → c
    return c
  }
  if (isStem(x)) {
    // Rule 3b: triage stem → d u (where x = stem(u))
    return apply(d, x.child, budget)
  }
  // Rule 3c: x is fork(u, v): triage fork → b u v
  return apply(apply(b, x.left, budget), x.right, budget)
}

// Convenience wrapper with a simple numeric budget
export function applyTree(f: Tree, x: Tree, maxSteps = 10000): Tree {
  const budget = { remaining: maxSteps }
  return apply(f, x, budget)
}

// --- Constants ---

// K = △ △ = stem(LEAF). K b x → b (returns first argument, discards second).
export const K = stem(LEAF)

// I = fork(LEAF, LEAF) = △ △ △.
// I x: fork(LEAF, LEAF) is fork(a, b) with a=LEAF → Rule 1: return b = LEAF.
// Wait, that always returns LEAF, not x. Let me re-derive.
//
// Actually, there is no simple "I combinator" tree that returns its argument
// structurally. But we CAN build one using triage:
//
// I = △ (△ △ △) △  (triage: leaf→△, stem→△ u, fork→△ u v)
//   = fork(fork(LEAF, LEAF), LEAF)
//
// I(leaf):    Rule 3a → c = LEAF = △ ✓
// I(stem(u)): Rule 3b → apply(d, u) = apply(LEAF, u) = stem(u) ✓
// I(fork(u,v)): Rule 3c → apply(apply(b, u), v) = apply(apply(LEAF, u), v)
//             = apply(stem(u), v) = fork(u, v) ✓
//
// This is the STRUCTURAL identity!
export const I = fork(fork(LEAF, LEAF), LEAF)

// --- Pretty printer ---

export function prettyTree(tree: Tree): string {
  if (isLeaf(tree)) return "△"
  if (isStem(tree)) return `(△ ${prettyTree(tree.child)})`
  return `(△ ${prettyTree(tree.left)} ${prettyTree(tree.right)})`
}
