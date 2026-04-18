import { parseProgram } from "./src/parse.js"
import { apply } from "./src/tree.js"
import { readFileSync } from "node:fs"

const src = readFileSync(process.argv[2], "utf-8")
const decls = parseProgram(src) as any[]
const globals = new Map<string, any>()
for (const d of decls) {
  if (d.kind === "Def") globals.set(d.name, d.tree)
}

const fn = globals.get(process.argv[3])!
const args = process.argv.slice(4).map(n => globals.get(n)!)
const budget = { remaining: 100_000_000 }
let r: any = fn
for (const a of args) r = apply(r, a, budget)
console.log("steps used:", 100_000_000 - budget.remaining)
console.log("result tag:", r.tag, "id:", r.id)
