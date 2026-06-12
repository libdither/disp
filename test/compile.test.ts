// Randomized end-to-end equivalence for Stages 2+3: the in-language
// compile_expr / compile_type (lib/elab/compile.disp — scope resolution +
// value desugars + bracket abstraction) must produce BIT-IDENTICAL trees to
// the host elaborator. Host path: real parser + compileExpr (`qi := …`) or
// the type-mode pipeline (`Ti : Type := …`). Spec path: encode the same AST,
// run the in-language pipeline over an env assembled from the same kernel
// scope, compare tree ids. The in-language code is the spec; the host is the
// validated optimization.

import { describe, it, expect } from "vitest"
import { resolve } from "node:path"
import { parseProgram, stringToTree } from "../src/compile.js"
import { fork, stem, LEAF, applyTree, force, type Tree } from "../src/tree.js"

type Param = { name: string | null; type: Expr | null }
type Expr =
  | { tag: "leaf" }
  | { tag: "num"; value: number }
  | { tag: "str"; value: string }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Expr; x: Expr }
  | { tag: "ann"; expr: Expr; type: Expr }
  | { tag: "binder"; params: Param[]; body: Expr }
  | { tag: "recType"; fields: { name: string; type: Expr | null; value: Expr | null }[] }
  | { tag: "recValue"; fields: { name: string; value: Expr }[] }
  | { tag: "proj"; target: Expr; field: string }
  | { tag: "if"; cond: Expr; thenBody: Expr; elseBody: Expr }
  | { tag: "match"; cond: Expr; arms: { pat: string; binders: string[]; body: Expr }[] }

let seed = 0xBEEF01
const rnd = (n: number): number => {
  seed = (seed * 1103515245 + 12345) & 0x7fffffff
  return seed % n
}
const pick = <T>(xs: T[]): T => xs[rnd(xs.length)]

const v = (name: string): Expr => ({ tag: "var", name })
const ap = (f: Expr, x: Expr): Expr => ({ tag: "app", f, x })

const VAL_NAMES = ["va", "vb", "vf"]
const FIELD_NAMES = ["fa", "fb"]
const PATS = ["Ok", "Err", "Some"]

// ── value generator (compile-time-safe: applications either have lit-safe
// heads (succ/inj) or a BOUND-VAR head, which stays open under abstraction) ─
function genVal(env: string[], d: number): Expr {
  const cs = ["atom", "num"]
  if (env.length > 0) cs.push("bound", "bound")
  if (d > 0) {
    cs.push("succ", "lam", "lam", "if", "match", "rec", "proj", "inj")
    if (env.length > 0) cs.push("appBound")
    if (rnd(6) === 0) cs.push("selectLazy")
    if (rnd(8) === 0) cs.push("ann")
  }
  switch (pick(cs)) {
    case "atom": return v(pick(["zero", "TT", "FF"]))
    case "num": return { tag: "num", value: rnd(4) }
    case "bound": return v(env[rnd(env.length)])
    case "succ": return ap(v("succ"), genVal(env, d - 1))
    case "inj": return ap(ap(v("inj"), { tag: "str", value: pick(PATS) }), genVal(env, d - 1))
    case "appBound": return ap(v(env[rnd(env.length)]), genVal(env, d - 1))
    case "ann": return { tag: "ann", expr: genVal(env, d - 1), type: v("Nat") }
    case "lam": {
      const n = 1 + rnd(2)
      const params: Param[] = []
      let inner = env
      for (let i = 0; i < n; i++) {
        const name = rnd(4) === 0 ? null : pick(VAL_NAMES)
        params.push({ name, type: null })
        if (name && !inner.includes(name)) inner = [...inner, name]
      }
      return { tag: "binder", params, body: genVal(inner, d - 1) }
    }
    case "if": {
      const cond = env.length > 0 && rnd(2) === 0 ? v(env[rnd(env.length)]) : v(pick(["TT", "FF"]))
      return { tag: "if", cond, thenBody: genVal(env, d - 1), elseBody: genVal(env, d - 1) }
    }
    case "match": {
      const cond = env.length > 0 && rnd(2) === 0
        ? v(env[rnd(env.length)])
        : ap(ap(v("inj"), { tag: "str", value: pick(PATS) }), genVal(env, 0))
      const pats = [...PATS]
      const arms: { pat: string; binders: string[]; body: Expr }[] = []
      const nArms = 1 + rnd(2)
      for (let i = 0; i < nArms; i++) {
        const pat = pats.splice(rnd(pats.length), 1)[0]
        const nB = rnd(3)
        const binders: string[] = []
        let inner = env
        for (let k = 0; k < nB; k++) {
          const b = rnd(4) === 0 ? "_" : pick(VAL_NAMES)
          binders.push(b)
          if (b !== "_" && !inner.includes(b)) inner = [...inner, b]
        }
        arms.push({ pat, binders, body: genVal(inner, d - 1) })
      }
      if (rnd(2) === 0) arms.splice(rnd(arms.length + 1), 0, { pat: "_", binders: [], body: genVal(env, d - 1) })
      return { tag: "match", cond, arms }
    }
    case "rec": {
      const names = [...FIELD_NAMES]
      const fields: { name: string; value: Expr }[] = []
      let inner = env
      const n = 1 + rnd(2)
      for (let i = 0; i < n && names.length > 0; i++) {
        const name = names.splice(rnd(names.length), 1)[0]
        fields.push({ name, value: genVal(inner, d - 1) })
        inner = [...inner, name]
      }
      return { tag: "recValue", fields }
    }
    case "proj": {
      const r = genVal(env, 1)
      if (r.tag === "recValue" && r.fields.length > 0) return { tag: "proj", target: r, field: r.fields[rnd(r.fields.length)].name }
      return { tag: "proj", target: { tag: "recValue", fields: [{ name: "fa", value: genVal(env, 0) }] }, field: "fa" }
    }
    case "selectLazy": {
      const thunk = (): Expr => ({ tag: "binder", params: [{ name: rnd(2) === 0 ? null : "vu", type: null }], body: genVal(env, d - 1) })
      const cond = env.length > 0 && rnd(2) === 0 ? v(env[rnd(env.length)]) : v(pick(["TT", "FF"]))
      let out: Expr = ap(ap(ap(v("select_lazy"), thunk()), thunk()), cond)
      if (rnd(3) === 0) out = ap(out, genVal(env, 0)) // select-then-apply passthrough
      return out
    }
    default: return v("zero")
  }
}

// ── type generator (Stage-1 shapes; first recType field must be `name : T`) ─
const BINDER_NAMES = ["na", "nb"]
const TFIELD_NAMES = ["fa", "fb", "fc"]
function genType(env: string[], d: number): Expr {
  const cs = d > 0 ? ["atom", "atom", "binder", "binder", "rec", "eq"] : ["atom"]
  switch (pick(cs)) {
    case "eq": return ap(ap(ap(v("Eq"), v("Nat")), genVal(env, 1)), genVal(env, 1))
    case "binder": {
      const n = 1 + rnd(2)
      const params: Param[] = []
      let inner = env
      for (let i = 0; i < n; i++) {
        const name = rnd(3) === 0 ? null : pick(BINDER_NAMES)
        params.push({ name, type: genType(inner, d - 1) })
        if (name && !inner.includes(name)) inner = [...inner, name]
      }
      return { tag: "binder", params, body: genType(inner, d - 1) }
    }
    case "rec": {
      const names = [...TFIELD_NAMES]
      const fields: { name: string; type: Expr | null; value: Expr | null }[] = []
      let inner = env
      const n = 1 + rnd(3)
      for (let i = 0; i < n && names.length > 0; i++) {
        const name = names.splice(rnd(names.length), 1)[0]
        const shape = i === 0 ? 0 : rnd(3)
        if (shape === 0) fields.push({ name, type: genType(inner, d - 1), value: null })
        else if (shape === 1) fields.push({ name, type: null, value: genVal(inner, 1) })
        else fields.push({ name, type: genType(inner, d - 1), value: genVal(inner, 1) })
        inner = [...inner, name]
      }
      return { tag: "recType", fields }
    }
    default: return v(pick(["Nat", "Bool"]))
  }
}

function render(e: Expr): string {
  switch (e.tag) {
    case "leaf": return "t"
    case "num": return String(e.value)
    case "str": return `"${e.value}"`
    case "var": return e.name
    case "app": return `(${render(e.f)} ${render(e.x)})`
    case "ann": return `(${render(e.expr)} : ${render(e.type)})`
    case "binder": {
      const ps = e.params.map(p => (p.name ?? "_") + (p.type ? ` : ${render(p.type)}` : "")).join(", ")
      return `({${ps}} -> ${render(e.body)})`
    }
    case "recType": {
      const fs = e.fields.map(f => {
        if (f.type && f.value !== null) return `${f.name} : ${render(f.type)} := ${render(f.value)}`
        if (f.type) return `${f.name} : ${render(f.type)}`
        return `${f.name} := ${render(f.value!)}`
      }).join(", ")
      return `{ ${fs} }`
    }
    case "recValue": return `{ ${e.fields.map(f => `${f.name} := ${render(f.value)}`).join("; ")} }`
    case "proj": return `${render(e.target)}.${e.field}`
    case "if": return `(if ${render(e.cond)} then ${render(e.thenBody)} else ${render(e.elseBody)})`
    case "match": {
      const arms = e.arms.map(a => `${a.pat}${a.binders.map(b => " " + b).join("")} => ${render(a.body)}`).join("; ")
      return `(match ${render(e.cond)} { ${arms} })`
    }
  }
}

// ── encoding (lib/elab/ast.disp layout) ───────────────────────────────────
const str = stringToTree
const inj = (tag: string, pay: Tree): Tree => fork(str(tag), pay)
const opt = (x: Tree | null): Tree => (x === null ? LEAF : stem(x))
const consList = (xs: Tree[]): Tree => xs.reduceRight<Tree>((acc, h) => fork(h, acc), LEAF)
const natT = (n: number): Tree => { let r: Tree = LEAF; for (let i = 0; i < n; i++) r = fork(LEAF, r); return r }

function enc(e: Expr): Tree {
  switch (e.tag) {
    case "leaf": return inj("leaf", LEAF)
    case "num": return inj("num", natT(e.value))
    case "str": return inj("str", str(e.value))
    case "var": return inj("var", str(e.name))
    case "app": return inj("app", fork(enc(e.f), enc(e.x)))
    case "ann": return inj("ann", fork(enc(e.expr), enc(e.type)))
    case "binder": return inj("binder", fork(
      consList(e.params.map(p => fork(opt(p.name === null ? null : str(p.name)), opt(p.type === null ? null : enc(p.type))))),
      enc(e.body)))
    case "recType": return inj("recType", consList(e.fields.map(f =>
      fork(str(f.name), fork(opt(f.type === null ? null : enc(f.type)), opt(f.value === null ? null : enc(f.value)))))))
    case "recValue": return inj("recValue", consList(e.fields.map(f => fork(str(f.name), enc(f.value)))))
    case "proj": return inj("proj", fork(enc(e.target), str(e.field)))
    case "if": return inj("if", fork(enc(e.cond), fork(enc(e.thenBody), enc(e.elseBody))))
    case "match": return inj("match", fork(enc(e.cond), consList(e.arms.map(a =>
      fork(str(a.pat), fork(consList(a.binders.map(str)), enc(a.body)))))))
  }
}

const ENV_NAMES = [
  "zero", "succ", "cond", "prod", "pair_fst", "pair_snd", "mk_record",
  "list_const", "select", "select_lazy", "inj", "TT", "FF",
  "Pi", "Telescope", "Tree", "Nat", "Bool", "Eq",
]

const BUDGET = 100_000_000

describe("Stages 2+3: in-language compile_expr/compile_type vs host", () => {
  it("compiles random value and type ASTs bit-identically to the host", () => {
    // One driver load: the spec pipeline + the kernel names the env needs.
    const driverPath = resolve("lib/tests/__compile_driver.disp")
    const decls = parseProgram(
      ['open use "../kernel/prelude.disp"', 'open use "../elab/compile.disp"'].join("\n"),
      driverPath)
    const byName = new Map<string, Tree>()
    for (const d of decls) if (d.kind === "Def" && !byName.has(d.name)) byName.set(d.name, d.tree)
    const compileExprT = byName.get("compile_expr")!
    const compileTypeT = byName.get("compile_type")!
    expect(compileExprT, "compile_expr export").toBeDefined()
    expect(compileTypeT, "compile_type export").toBeDefined()

    // env assoc list: fork(pair(name, stem(tree)), rest), same kernel scope
    // the host program compiles against.
    let envTree: Tree = LEAF
    for (let i = ENV_NAMES.length - 1; i >= 0; i--) {
      const n = ENV_NAMES[i]
      const tr = byName.get(n)
      expect(tr, `env name ${n}`).toBeDefined()
      envTree = fork(fork(str(n), stem(tr!)), envTree)
    }

    const vals: Expr[] = []
    for (let i = 0; i < 120; i++) vals.push(genVal([], 3))
    const typs: Expr[] = []
    for (let i = 0; i < 60; i++) typs.push(genType([], 2))

    // Host: ONE program over the same kernel.
    const src = ['open use "../kernel/prelude.disp"',
      ...vals.map((e, i) => `q${i} := ${render(e)}`),
      ...typs.map((e, i) => `T${i} : Type := ${render(e)}`)].join("\n")
    const hostDecls = parseProgram(src, resolve("lib/tests/__compile_host.disp"))
    const host = new Map<string, Tree>()
    for (const d of hostDecls) if (d.kind === "Def") host.set(d.name, d.tree)

    for (let i = 0; i < vals.length; i++) {
      const spec = applyTree(applyTree(compileExprT, envTree, BUDGET), enc(vals[i]), BUDGET)
      expect(force(spec).id, `value ${i}: ${render(vals[i])}`).toBe(force(host.get(`q${i}`)!).id)
    }
    for (let i = 0; i < typs.length; i++) {
      const spec = applyTree(applyTree(compileTypeT, envTree, BUDGET), enc(typs[i]), BUDGET)
      expect(force(spec).id, `type ${i}: ${render(typs[i])}`).toBe(force(host.get(`T${i}`)!).id)
    }
  })
})
