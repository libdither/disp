import { describe, it, expect } from "vitest"
import { resolve } from "node:path"
import { apply, treeEqual, TREE_TRUE, LEAF, stem, fork, getNativeId, getTreeEqId, getApplyStats, encodeTernary, type Tree } from "../src/core/tree.js"
import { EagerSession, eagerBackend } from "../src/eval/eager.js"
import { rustEagerNativeBackend, rustEagerNativeAvailable } from "../src/eval/rust-eager-native.js"
import type { Session } from "../src/eval/types.js"
import { parseProgram } from "../src/compile.js"

// lib/machine.disp defines one step as one K, S or triage dispatch and claims the
// count equals this evaluator's `steps` on the same application with the memo off
// and no tree_eq intercept (RESUMABLE_INTERP_PLAN.md, "One step"). m_advance reports
// no count, so the machine is stepped one dispatch at a time: settle, ask, step.
const src = `open use raw "../prelude.disp" {}
open use raw "../list.disp" {}
open use raw "../machine.disp" {}
rec add :: {a : Nat, b : Nat} -> Nat :=> if (is_zero a) { b } else { succ (add (nat_pred a) b) }
rec NatR :: {v : Tree} -> Bool :=> if (is_zero v) { true } else if (is_fork v) { if (is_leaf v.fst) { NatR v.snd } else { false } } else { false }
rec LoopR :: {v : Tree} -> Bool :=> if (is_zero v) { true } else { LoopR v }
discard := {x} => ({_y} => 1) (LoopR x)
add2 := add 2
n0 := 0
n1 := 1
n2 := 2
n3 := 3
stem1 := t (t t)
// the root's decls carry only its own definitions, so name what the test reads
h_succ := succ
h_m_start := m_start
h_m_settle := m_settle
h_m_step := m_step
h_m_finished := m_finished
h_m_result := m_result
h_m_advance := m_advance
h_tree_eq := tree_eq
`
const decls = parseProgram(src, resolve("lib/tests/_machine_host.disp"))
const tree = (name: string): Tree => (decls.find((d: any) => d.kind === "Def" && d.name === name) as any).tree
const big = () => ({ remaining: 20_000_000 })

// The machine itself runs on the default session, memo and tree_eq on: that only
// speeds up its own == tests and never changes what it builds.
function machineRun(f: Tree, x: Tree): { steps: number; result: Tree } {
  const m_start = tree("h_m_start"), m_settle = tree("h_m_settle"), m_step = tree("h_m_step")
  const m_finished = tree("h_m_finished"), m_result = tree("h_m_result")
  let m = apply(apply(m_start, f, big()), x, big())
  let steps = 0
  for (;;) {
    m = apply(m_settle, m, big())
    if (treeEqual(apply(m_finished, m, big()), TREE_TRUE)) return { steps, result: apply(m_result, m, big()) }
    m = apply(m_step, m, big()); steps++
  }
}
// The reference: a fresh session with the memo off; a fresh session has no tree_eq id.
function nativeRun(f: Tree, x: Tree): { steps: number; result: Tree } {
  const s = eagerBackend.createSession({ applyCacheLimit: 0 }) as EagerSession
  expect(s.getTreeEqId()).toBe(-1)
  const result = s.apply(f, x, big())
  return { steps: s.getApplyStats().steps, result }
}

// [label, f, x, counted steps when the run is free rules only]
const cases: [string, string, string, number?][] = [
  ["succ 2: eta-reduced to the stem t t, one free stem rule", "h_succ", "n2", 0],
  ["add 2 3", "add2", "n3"],
  ["NatR 3", "NatR", "n3"],
  ["NatR (t (t t))", "NatR", "stem1"],
  ["LoopR 0", "LoopR", "n0"],
  ["the S-rule discard: ({_y} => 1) (LoopR 1)", "discard", "n1"],
]
describe("lib/machine.disp step unit", () => {
  for (const [label, f, x, free] of cases) it(label, () => {
    const native = nativeRun(tree(f), tree(x))
    const machine = machineRun(tree(f), tree(x))
    expect(treeEqual(machine.result, native.result)).toBe(true)
    if (free !== undefined) expect(native.steps).toBe(free); else expect(native.steps).toBeGreaterThan(0)
    expect(machine.steps).toBe(native.steps)
  })
})

// The intercept: `m_advance m k` answered by the native stepper (src/core/tree.ts
// stepMachine) must return the node the disp definition builds, for every machine and
// count — consumers read the state. Loading lib/machine.disp registered m_advance on
// the default session; the reference session below has tree_eq but no m_advance.
let seed = 0x2468ace
const rnd = (n: number): number => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n }
function genTree(depth: number): Tree {
  if (depth <= 0 || rnd(3) === 0) return LEAF
  if (rnd(2) === 0) return stem(genTree(depth - 1))
  return fork(genTree(depth - 1), genTree(depth - 1))
}
const nat = (n: number): Tree => { let r: Tree = LEAF; for (let i = 0; i < n; i++) r = fork(LEAF, r); return r }

describe("m_advance intercept", () => {
  const m_advance = tree("h_m_advance"), m_start = tree("h_m_start")
  const ref = new EagerSession()
  ref.setTreeEqId(getTreeEqId())
  // [intercepted, in-language] for m_advance (m_start f x) k; asserts the intercept fired
  function both(f: Tree, x: Tree, k: number): [Tree, Tree] {
    const m = apply(apply(m_start, f, big()), x, big())
    const before = getApplyStats().machineRules
    const fast = apply(apply(m_advance, m, big()), nat(k), big())
    expect(getApplyStats().machineRules).toBeGreaterThan(before)
    const slow = ref.apply(ref.apply(m_advance, m, big()), nat(k), big())
    return [fast, slow]
  }
  it("is registered by loading lib/machine.disp", () => {
    expect(getNativeId("machineRules")).toBe(m_advance.id)
    expect(ref.getTreeEqId()).toBe(getTreeEqId())
  })
  it("returns the definition's node on random terms and counts", () => {
    for (let i = 0; i < 300; i++) {
      const [fast, slow] = both(genTree(6), genTree(6), rnd(40))
      expect(fast.id).toBe(slow.id)
    }
  })
  it("agrees past completion, on the discard shape, and on a suspended operand", () => {
    const [a, b] = both(tree("h_succ"), tree("n2"), 60)
    expect(a.id).toBe(b.id)
    const [c, d] = both(tree("discard"), tree("n1"), 30)
    expect(c.id).toBe(d.id)
    const partial = apply(tree("h_tree_eq"), tree("n2"), big())
    expect(partial.tag).toBe("susp")
    const [e, g] = both(partial, tree("n2"), 80)
    expect(e.id).toBe(g.id)
    const [h, i] = both(partial, tree("n2"), 1500)
    expect(h.id).toBe(i.id)
    expect(treeEqual(apply(tree("h_m_finished"), h, big()), TREE_TRUE)).toBe(true)
    expect(treeEqual(apply(tree("h_m_result"), h, big()), TREE_TRUE)).toBe(true)
  })
  it("answers a zero count and a finished machine with the same node", () => {
    const m = apply(apply(m_start, tree("h_succ"), big()), tree("n2"), big())
    expect(apply(apply(m_advance, m, big()), nat(0), big()).id).toBe(m.id)
    const done = apply(apply(m_advance, m, big()), nat(5), big())
    expect(apply(apply(m_advance, done, big()), nat(5), big()).id).toBe(done.id)
  })
})

// The Rust stepper (evaluators/rust-eager/crate/src/machine.rs) against the TypeScript one,
// through the dump bridge the backend differentials use: the same program elaborated on a
// native session, the same random terms loaded from one ternary string, dumps compared.
// A second native session with the intercept off is the in-language reference on that
// backend and shows the intercept fired (fewer interactions for the same answer).
describe.skipIf(!rustEagerNativeAvailable())("m_advance intercept, rust-eager-native", () => {
  const fast = rustEagerNativeBackend.createSession() as unknown as Session<number>
  const slow = rustEagerNativeBackend.createSession({ noNativeIntercept: true }) as unknown as Session<number>
  const handles = (s: Session<number>) => {
    const d = parseProgram(src, resolve("lib/tests/_machine_host.disp"), { session: s as any }) as any[]
    const h = (name: string): number => d.find(x => x.kind === "Def" && x.name === name).tree
    return { m_advance: h("h_m_advance"), m_start: h("h_m_start"), succ: h("h_succ"), add2: h("add2"), n3: h("n3") }
  }
  const hf = handles(fast), hs = handles(slow)
  const load = (s: Session<number>, t: Tree): number => s.loadTernary(encodeTernary(t))
  const advance = (s: Session<number>, h: { m_advance: number; m_start: number }, f: number, x: number, k: number): number =>
    s.apply(s.apply(h.m_advance, s.apply(s.apply(h.m_start, f), x)), load(s, nat(k)))
  it("agrees with the TypeScript stepper on random terms and counts", () => {
    const m_advance = tree("h_m_advance"), m_start = tree("h_m_start")
    for (let i = 0; i < 200; i++) {
      const f = genTree(6), x = genTree(6), k = rnd(40)
      const ts = apply(apply(m_advance, apply(apply(m_start, f, big()), x, big()), big()), nat(k), big())
      const native = advance(fast, hf, load(fast, f), load(fast, x), k)
      expect(fast.dumpTernary(native)).toBe(encodeTernary(ts))
    }
  })
  it("agrees with the definition run in-language on that backend, and is cheaper", () => {
    const before = [fast.stats!().steps, slow.stats!().steps]
    const a = advance(fast, hf, hf.add2, hf.n3, 40)
    const b = advance(slow, hs, hs.add2, hs.n3, 40)
    expect(fast.dumpTernary(a)).toBe(slow.dumpTernary(b))
    expect(fast.stats!().steps - before[0]).toBeLessThan(slow.stats!().steps - before[1])
  })
})
