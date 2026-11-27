// Tests for Stage 0 Canonical Core IR (`src/core/ir.ts`), out-of-band source spans,
// `line:col` error attribution, and `elab_settings` routing for `==` (`tree_eq`).

import { describe, it, expect } from "vitest"
import { parseExpr, parseItems, tokenize } from "../src/parse.js"
import { parseProgram, spanOf, formatSpan, type CoreExpr } from "../src/compile.js"
import { eliminateLams, cirToTree } from "../src/elab/cir.js"
import { defaultSession } from "../src/eval/eager.js"

describe("Core IR & out-of-band source spans", () => {
  it("stamps 1-based line and column spans on tokens and parsed expressions", () => {
    const toks = tokenize("foo\n  bar(1)", "sample.disp")
    const bar = toks.find(t => t.t === "id" && t.v === "bar")!
    expect(spanOf(bar)).toEqual({ line: 2, col: 3, endLine: 2, endCol: 6, source: "sample.disp" })

    const e = parseExpr("f(x,\n  y)", "sample.disp")
    expect(spanOf(e)).toEqual({ line: 1, col: 1, endLine: 2, endCol: 5, source: "sample.disp" })
    expect(e.tag).toBe("app")
    if (e.tag === "app") {
      expect(spanOf(e.x)).toEqual({ line: 2, col: 3, endLine: 2, endCol: 4, source: "sample.disp" })
    }
  })

  it("stamps spans on top-level items and nested projections/infix expressions", () => {
    const items = parseItems("a := t\nb := x.foo == y", "mod.disp")
    expect(spanOf(items[0])).toEqual({ line: 1, col: 1, endLine: 1, endCol: 7, source: "mod.disp" })
    expect(spanOf(items[1])).toEqual({ line: 2, col: 1, endLine: 2, endCol: 16, source: "mod.disp" })
    if (items[1].tag === "field" && items[1].value?.tag === "app") {
      expect(spanOf(items[1].value)).toEqual({ line: 2, col: 6, endLine: 2, endCol: 16, source: "mod.disp" })
      expect(formatSpan(spanOf(items[1].value)!)).toBe("mod.disp:2:6")
    }
  })

  it("attributes unresolved free variables to exact line:col", () => {
    expect(() => parseProgram("a := t\nb := {x} => missing_var(x)\n", "err.disp"))
      .toThrow(/unresolved free variable missing_var at .*err\.disp:2:13/)
  })

  it("attributes expression elaboration errors (hole, missing if cond) with line:col", () => {
    expect(() => parseProgram("a := _\n", "hole.disp"))
      .toThrow(/hole '_' cannot appear in untyped compilation \(at .*hole\.disp:1:6\)/)
    expect(() => parseProgram("a := if (t) { t } else { t }\n", "if.disp"))
      .toThrow(/if: 'cond' must be in scope \(import prelude\) \(at .*if\.disp:1:6\)/)
  })

  it("CoreExpr lowers directly through eliminateLams + cirToTree", () => {
    const idCore: CoreExpr = { tag: "lam", x: "x", body: { tag: "var", name: "x" } }
    const tree = cirToTree(eliminateLams(idCore))
    const leaf = defaultSession.leaf()
    expect(defaultSession.equal!(defaultSession.apply(tree, leaf), leaf)).toBe(true)
  })

  it("routes `==` through `elab_settings` (`tree_eq`) without shadowing user `tree_eq`", () => {
    const decls = parseProgram(`
      zero := t
      succ := {n} => t(t, n)
      tree_eq := {_a, _b} => t
      custom_eq := {_a, _b} => t(t)
      elab_settings := [t("tree_eq", custom_eq)]
      via_op := t == t
      via_name := tree_eq(t, t)
    `)
    const viaOp = decls.find(d => d.kind === "Def" && d.name === "via_op")
    const viaName = decls.find(d => d.kind === "Def" && d.name === "via_name")
    if (!viaOp || viaOp.kind !== "Def" || !viaName || viaName.kind !== "Def") throw new Error("missing defs")
    const leaf = defaultSession.leaf()
    expect(defaultSession.equal!(viaOp.tree, defaultSession.stem(leaf))).toBe(true)
    expect(defaultSession.equal!(viaName.tree, leaf)).toBe(true)
  })
})
