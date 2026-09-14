import { describe, it, expect } from "vitest"
import { resolve } from "node:path"
import { apply, treeEqual, TREE_TRUE, type Tree } from "../src/core/tree.js"
import { eagerBackend, type EagerSession } from "../src/eval/eager.js"
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
