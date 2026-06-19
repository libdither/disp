// Randomized equivalence for Stage-1 type-position desugars: the in-language
// binder_to_pi / rectype_to_telescope (lib/elab/ast.disp) must produce
// desugared ASTs that the host compiles to BIT-IDENTICAL trees vs the host's
// own type-mode pipeline (binderToPi + the recType→Telescope fold). Per term:
//   host:  `Ti : Type := <surface term>`        (compileType path)
//   spec:  encode AST → run in-language desugar → decode → render →
//          `Si := <rendered desugared term>`    (plain value compile)
// and Ti.id must equal Si.id. This is the native-fast-path discipline
// (cf. tree_eq / bracket.test.ts): the in-language code is the spec, the
// host is the validated optimization.

import { describe, it, expect } from "vitest"
import { resolve } from "node:path"
import { parseProgram, stringToTree } from "../src/compile.js"
import { fork, stem, LEAF, applyTree, force, type Tree } from "../src/tree.js"

type Param = { name: string | null; type: Expr | null }
type TField = { name: string; type: Expr | null; value: Expr | null }
type Expr =
  | { tag: "leaf" }
  | { tag: "str"; value: string }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Expr; x: Expr }
  | { tag: "binder"; params: Param[]; body: Expr }
  | { tag: "recType"; fields: TField[] }

// Deterministic seeded LCG so failures are reproducible.
let seed = 0xD15B
const rnd = (n: number): number => {
  seed = (seed * 1103515245 + 12345) & 0x7fffffff
  return seed % n
}

const TYPE_ATOMS = ["Nat", "Bool"]
const BINDER_NAMES = ["na", "nb"]
const FIELD_NAMES = ["fa", "fb", "fc"]

const v = (name: string): Expr => ({ tag: "var", name })
const ap = (f: Expr, x: Expr): Expr => ({ tag: "app", f, x })

// Value expressions (derived-field recipes, Eq operands): zero/succ chains
// and references to bound names (binder params / prior fields).
function genValue(env: string[], depth: number): Expr {
  const c = rnd(env.length > 0 ? 4 : 2)
  if (c === 0) return v("zero")
  if (c === 1 && depth > 0) return ap(v("succ"), genValue(env, depth - 1))
  if (env.length > 0) return v(env[rnd(env.length)])
  return v("zero")
}

function genBinder(env: string[], depth: number): Expr {
  const nParams = 1 + rnd(2)
  const params: Param[] = []
  let innerEnv = env
  for (let i = 0; i < nParams; i++) {
    const name = rnd(3) === 0 ? null : BINDER_NAMES[rnd(BINDER_NAMES.length)]
    params.push({ name, type: genType(innerEnv, depth - 1) })
    if (name && !innerEnv.includes(name)) innerEnv = [...innerEnv, name]
  }
  return { tag: "binder", params, body: genType(innerEnv, depth - 1) }
}

// The first field must be `name : T` — a leading `:=` (or `name : T := e`)
// classifies the braces as a record VALUE in the surface grammar.
function genRec(env: string[], depth: number): Expr {
  const n = 1 + rnd(3)
  const fields: TField[] = []
  let innerEnv = env
  const names = [...FIELD_NAMES]
  for (let i = 0; i < n && names.length > 0; i++) {
    const name = names.splice(rnd(names.length), 1)[0]
    const shape = i === 0 ? 0 : rnd(3)
    if (shape === 0) fields.push({ name, type: genType(innerEnv, depth - 1), value: null })
    else if (shape === 1) fields.push({ name, type: null, value: genValue(innerEnv, 1) })
    else fields.push({ name, type: genType(innerEnv, depth - 1), value: genValue(innerEnv, 1) })
    innerEnv = [...innerEnv, name]
  }
  return { tag: "recType", fields }
}

function genType(env: string[], depth: number): Expr {
  const choices = depth > 0 ? ["atom", "atom", "binder", "binder", "rec", "eq"] : ["atom"]
  switch (choices[rnd(choices.length)]) {
    case "binder": return genBinder(env, depth)
    case "rec": return genRec(env, depth)
    case "eq": return ap(ap(ap(v("Eq"), v("Nat")), genValue(env, 1)), genValue(env, 1))
    default: return v(TYPE_ATOMS[rnd(TYPE_ATOMS.length)])
  }
}

function render(e: Expr): string {
  switch (e.tag) {
    case "leaf": return "t"
    case "str": return `"${e.value}"`
    case "var": return e.name
    case "app": return `(${render(e.f)} ${render(e.x)})`
    case "binder": {
      const ps = e.params.map(p => (p.name ?? "_") + (p.type ? ` : ${render(p.type)}` : "")).join(", ")
      return `({${ps}} -> ${render(e.body)})`
    }
    case "recType": {
      const fs = e.fields.map(f => {
        if (f.type && f.value !== null) return `${f.name} : ${render(f.type)} := ${render(f.value!)}`
        if (f.type) return `${f.name} : ${render(f.type)}`
        return `${f.name} := ${render(f.value!)}`
      }).join(", ")
      return `{ ${fs} }`
    }
  }
}

// ── encode / decode against lib/elab/ast.disp's coproduct layout ──────────
// inj tag pay = fork(str(tag), pay); Param = fork(nameOpt, tyOpt);
// recType field = fork(name, fork(tyOpt, valOpt)); option: stem(x) / LEAF;
// lists are cons-chains (fork(h, tl) / LEAF).

const str = stringToTree
const inj = (tag: string, pay: Tree): Tree => fork(str(tag), pay)
const opt = (x: Tree | null): Tree => (x === null ? LEAF : stem(x))
const consList = (xs: Tree[]): Tree => xs.reduceRight<Tree>((acc, h) => fork(h, acc), LEAF)

function enc(e: Expr): Tree {
  switch (e.tag) {
    case "leaf": return inj("Leaf", LEAF)
    case "str": return inj("Str", str(e.value))
    case "var": return inj("Var", str(e.name))
    case "app": return inj("App", fork(enc(e.f), enc(e.x)))
    case "binder": return inj("Binder", fork(
      consList(e.params.map(p => fork(opt(p.name === null ? null : str(p.name)), opt(p.type === null ? null : enc(p.type))))),
      enc(e.body)))
    case "recType": return inj("RecType", consList(e.fields.map(f =>
      fork(str(f.name), fork(opt(f.type === null ? null : enc(f.type)), opt(f.value === null ? null : enc(f.value)))))))
  }
}

const NAME_POOL = [
  ...TYPE_ATOMS, ...BINDER_NAMES, ...FIELD_NAMES,
  "zero", "succ", "Eq", "Pi", "Tree", "Telescope", "proj_cell", "deriv_cell",
  "name", "ty", "def",
]
const nameById = new Map<number, string>()
for (const n of NAME_POOL) nameById.set(str(n).id, n)
const tagById = new Map<number, string>()
for (const n of ["Leaf", "Str", "Var", "App", "Binder", "RecType"]) tagById.set(str(n).id, n)

const asFork = (t: Tree): Tree & { tag: "fork" } => {
  const f = force(t)
  if (f.tag !== "fork") throw new Error(`dec: expected fork, got ${f.tag}`)
  return f
}
const lookupName = (t: Tree): string => {
  const n = nameById.get(force(t).id)
  if (n === undefined) throw new Error("dec: unknown interned name")
  return n
}

function dec(tr: Tree): Expr {
  const node = asFork(tr)
  const tag = tagById.get(force(node.left).id)
  const pay = force(node.right)
  switch (tag) {
    case "Leaf": return { tag: "leaf" }
    case "Str": return { tag: "str", value: lookupName(pay) }
    case "Var": return { tag: "var", name: lookupName(pay) }
    case "App": {
      const p = asFork(pay)
      return { tag: "app", f: dec(p.left), x: dec(p.right) }
    }
    case "Binder": {
      const p = asFork(pay)
      const params: Param[] = []
      let cur = force(p.left)
      while (cur.tag === "fork") {
        const pr = asFork(cur.left)
        const nOpt = force(pr.left), tOpt = force(pr.right)
        params.push({
          name: nOpt.tag === "stem" ? lookupName(nOpt.child) : null,
          type: tOpt.tag === "stem" ? dec(tOpt.child) : null,
        })
        cur = force(cur.right)
      }
      return { tag: "binder", params, body: dec(p.right) }
    }
    case "RecType": {
      const fields: TField[] = []
      let cur = pay
      while (cur.tag === "fork") {
        const f = asFork(cur.left)
        const opts = asFork(f.right)
        const tOpt = force(opts.left), vOpt = force(opts.right)
        fields.push({
          name: lookupName(f.left),
          type: tOpt.tag === "stem" ? dec(tOpt.child) : null,
          value: vOpt.tag === "stem" ? dec(vOpt.child) : null,
        })
        cur = force(asFork(cur).right)
      }
      return { tag: "recType", fields }
    }
    default: throw new Error("dec: unknown tag")
  }
}

const BUDGET = 50_000_000

describe("Stage-1 type-position desugars (in-language vs host)", () => {
  it("desugars random binder/recType ASTs bit-identically to the host", () => {
    // Load the in-language desugars once (pulls in prelude + kernel).
    const driverPath = resolve("lib/tests/__desugar_driver.disp")
    const decls = parseProgram('open use "../elab/ast.disp"', driverPath)
    const get = (n: string): Tree => {
      const d = decls.find(d => d.kind === "Def" && d.name === n)
      expect(d, `${n} export`).toBeDefined()
      return (d as { tree: Tree }).tree
    }
    const b2p = get("binder_to_pi")
    const r2t = get("rectype_to_telescope")

    const terms: { e: Expr; spec: Tree }[] = []
    for (let i = 0; i < 80; i++) terms.push({ e: genBinder([], 3), spec: b2p })
    for (let i = 0; i < 50; i++) terms.push({ e: genRec([], 2), spec: r2t })

    // Host path: the real type-mode pipeline (`Ti : Type := …` triggers
    // compileType → binderToPi; recTypes desugar inside exprToCir).
    const srcHost = ['open use "../kernel/prelude.disp"',
      ...terms.map((t, i) => `T${i} : Type := ${render(t.e)}`)].join("\n")
    const hostDecls = parseProgram(srcHost, resolve("lib/tests/__desugar_host.disp"))
    const hostTrees = new Map<string, Tree>()
    for (const d of hostDecls) if (d.kind === "Def") hostTrees.set(d.name, d.tree)

    // Spec path: in-language desugar of the encoded AST, decoded and rendered
    // back to surface, then VALUE-compiled (the desugared form has no
    // type-position sugar left at the root; the host handles inner nodes the
    // same way on both sides).
    const rendered = terms.map(t => render(dec(applyTree(t.spec, enc(t.e), BUDGET))))
    const srcSpec = ['open use "../kernel/prelude.disp"',
      ...rendered.map((r, i) => `S${i} := ${r}`)].join("\n")
    const specDecls = parseProgram(srcSpec, resolve("lib/tests/__desugar_spec.disp"))
    const specTrees = new Map<string, Tree>()
    for (const d of specDecls) if (d.kind === "Def") specTrees.set(d.name, d.tree)

    for (let i = 0; i < terms.length; i++) {
      const host = hostTrees.get(`T${i}`)!
      const spec = specTrees.get(`S${i}`)!
      expect(spec.id, `term ${i}: ${render(terms[i].e)}\n  desugared: ${rendered[i]}`).toBe(host.id)
    }
  })
})
