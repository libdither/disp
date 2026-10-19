// Compile programs.disp (and the prelude and list library it opens) with origin tracking, and write
// what every emitted tree came from: ORIGINS.json = { origins: ternary -> [{def, vars, body} | {def, app}],
// names: ternary -> name }. Runs on a scratch copy of src/ with cir-origins.patch applied (run.sh).
import { readFileSync, writeFileSync } from "node:fs"
import { parseProgram } from "./src/compile.js"
import { ORIGINS, originOf, type Cir } from "./src/elab/cir.js"
import { defaultSession } from "./src/eval/eager.js"
import { moduleCacheBySession } from "./src/elab/state.js"
import { force } from "./src/core/tree.js"

const [file, out] = process.argv.slice(2)
/// Ternary preorder, iteratively; null past `max` nodes.
const tern = (t: any, max = 50000): string | null => {
  const s: string[] = [], stack = [t]
  while (stack.length) {
    const n = force(stack.pop())
    if (s.length > max) return null
    if (n.tag === "leaf") s.push("0")
    else if (n.tag === "stem") { s.push("1"); stack.push(n.child) }
    else { s.push("2"); stack.push(n.right, n.left) }
  }
  return s.join("")
}

const session = defaultSession as any
ORIGINS.on = true
// The definition each origin belongs to: the item compiled when it was recorded.
const owner = new Map<unknown, string>()
let seen = 0
const decls = parseProgram(readFileSync(file, "utf-8"), file, { session, onItem: it => {
  const keys = [...ORIGINS.map.keys()]
  for (; seen < keys.length; seen++) owner.set(keys[seen], it.name ?? it.kind)
} })
// Names: the definitions here and every export of the modules in scope, first name wins.
const names = new Map<unknown, string>()
for (const d of decls) if (d.kind === "Def" && !names.has(d.tree)) names.set(d.tree, d.name)
for (const [, entry] of moduleCacheBySession.get(session) ?? new Map()) {
  const fs = (entry as any).fields as string[] | undefined, ts = (entry as any).fieldTrees as unknown[] | undefined
  fs?.forEach((f, i) => { const t = ts?.[i]; if (t && !names.has(t) && !f.startsWith("__")) names.set(t, f) })
}
// A lowered term: ["var", x] | ["name", n] | ["lit", ternary] | ["app", f, x] | ["lam", x, body] | ["S"|"K"|"I"]
// | ["orig", vars, body] (an abstraction result inside, by its own origin).
const struct = (c: Cir, top = true): unknown => {
  const o = originOf(c)
  if (o && !top) return ["orig", o.vars, struct(o.body)]
  switch (c.tag) {
    case "var": return ["var", c.name]
    case "lit": { const n = names.get(c.t); return n ? ["name", n] : ["lit", tern(c.t, 4000) ?? "?"] }
    case "S": case "K": case "I": return [c.tag]
    case "lam": return ["lam", c.x, struct(c.body)]
    case "app": return ["app", struct(c.f, false), struct(c.x, false)]
  }
}
const origins: Record<string, unknown[]> = {}
for (const [t, os] of ORIGINS.map) {
  const k = tern(t); if (k === null) continue
  for (const o of os) (origins[k] ??= []).push({ def: owner.get(t) ?? "?", ...("app" in o ? { app: struct(o.app, false) } : { vars: o.vars, body: struct(o.body) }) })
}
const nameOut: Record<string, string> = {}
for (const [t, n] of names) { const k = tern(t); if (k !== null && !(k in nameOut)) nameOut[k] = n }
writeFileSync(out, JSON.stringify({ origins, names: nameOut }))
console.log(`origins for ${Object.keys(origins).length} trees, ${Object.keys(nameOut).length} names`)
