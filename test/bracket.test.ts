// Randomized equivalence: the in-language bracket abstraction
// (lib/elab/bracket.disp) must produce BIT-IDENTICAL trees to the host
// elaborator on closed lambda terms. The host path goes through the real
// parser + compileExpr; the in-language path encodes the same term as a Cir
// coproduct value and runs bracket_compile on the substrate. This is the
// native-fast-path discipline (cf. tree_eq): the in-language code is the
// spec, the host is the validated optimization.

import { describe, it, expect } from "vitest"
import { resolve } from "node:path"
import { parseProgram, stringToTree } from "../src/compile.js"
import { eagerBackend } from "../src/eval/eager.js"
import type { Tree } from "../src/eval/eager.js"

// Ported to the Session ABI (EVALUATOR_PLAN decision 8): build/apply/compare go
// through a Session, not the raw core/tree runtime, and equivalence is equal()
// rather than `.id ===`. Runs on the eager backend; the in-language elaborator is
// thereby exercised through the same surface a non-eager backend would use.
const session = eagerBackend.createSession()

type Term =
  | { k: "var"; n: string }
  | { k: "leaf" }
  | { k: "app"; f: Term; x: Term }
  | { k: "lam"; n: string; b: Term }

// Deterministic seeded LCG so failures are reproducible.
let seed = 0xC0FFEE
const rnd = (n: number): number => {
  seed = (seed * 1103515245 + 12345) & 0x7fffffff
  return seed % n
}

const POOL = ["va", "vb", "vc"] // small pool → shadowing occurs naturally

function gen(depth: number, env: string[]): Term {
  const choices: string[] = []
  if (env.length > 0) choices.push("var", "var")
  choices.push("leaf")
  if (depth > 0) choices.push("app", "app", "lam", "lam")
  switch (choices[rnd(choices.length)]) {
    case "var": return { k: "var", n: env[rnd(env.length)] }
    case "leaf": return { k: "leaf" }
    case "app": return { k: "app", f: gen(depth - 1, env), x: gen(depth - 1, env) }
    default: {
      const n = POOL[rnd(POOL.length)]
      return { k: "lam", n, b: gen(depth - 1, env.includes(n) ? env : [...env, n]) }
    }
  }
}

function render(t: Term): string {
  switch (t.k) {
    case "var": return t.n
    case "leaf": return "t"
    case "app": return `(${render(t.f)} ${render(t.x)})`
    case "lam": return `({${t.n}} -> ${render(t.b)})`
  }
}

// Encode a Term as the Cir coproduct value lib/elab/bracket.disp consumes:
// inj tag pay = fork(stringToTree(tag), pay); pair = fork.
const inj = (tag: string, pay: Tree): Tree => session.fork(stringToTree(tag), pay)
function enc(t: Term): Tree {
  switch (t.k) {
    case "var": return inj("Var", stringToTree(t.n))
    case "leaf": return inj("Lit", session.leaf())
    case "app": return inj("App", session.fork(enc(t.f), enc(t.x)))
    case "lam": return inj("Lam", session.fork(stringToTree(t.n), enc(t.b)))
  }
}

const BUDGET = 50_000_000

describe("in-language bracket abstraction", () => {
  it("compiles random closed lambda terms bit-identically to the host", () => {
    // Load the in-language compiler once (pulls in prelude + kernel).
    const driverPath = resolve("lib/tests/__bracket_driver.disp")
    const decls = parseProgram('open use "../elab/bracket.disp"', driverPath, { session })
    const bc = decls.find(d => d.kind === "Def" && d.name === "bracket_compile")
    expect(bc, "bracket_compile export").toBeDefined()
    const bcTree = (bc as { tree: Tree }).tree

    // Generate closed terms, host-compile them all in ONE parse (fast: the
    // source is pure lambdas over `t`, no imports needed).
    const terms: Term[] = []
    for (let i = 0; i < 300; i++) {
      const n = POOL[rnd(POOL.length)]
      terms.push({ k: "lam", n, b: gen(4, [n]) })
    }
    const src = terms.map((t, i) => `let q${i} = ${render(t)}`).join("\n")
    const hostDecls = parseProgram(src, resolve("lib/tests/__rand.disp"), { session })
    const hostTrees = new Map<string, Tree>()
    for (const d of hostDecls) if (d.kind === "Def") hostTrees.set(d.name, d.tree)

    for (let i = 0; i < terms.length; i++) {
      const host = hostTrees.get(`q${i}`)!
      const inLang = session.apply(bcTree, enc(terms[i]), { remaining: BUDGET })
      expect(session.equal!(inLang, host), `term ${i}: ${render(terms[i])}`).toBe(true)
    }
  })
})
