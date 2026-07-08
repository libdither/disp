// E1 workload dumper (research/interaction-combinator/SPATIAL_IC.md §10). Writes
// bench/e1/<name>.terms — whitespace-separated ternary terms that `ic-net-cli -trace X
// -file <name>.terms` left-folds into one application and reduces under the rung C
// tracer. Two sources:
//   - lambada benchmark programs (already ternary): fib, exp, merge-sort, size
//   - elaborated disp values (bench/e1_defs.disp): kernel add via nat_rec, the checker
//     walk param_apply Nat n, tree_eq as the conversion workload
// Compiled defs are dumped one value per term (each is an elaboration-time normal form);
// the CLI's fold re-creates the application, so the trace covers exactly the reduction.
//
// Run: npx tsx bench/e1-workloads.ts

import { readFileSync, writeFileSync, mkdirSync } from "node:fs"
import { resolve, join } from "node:path"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"

const OUT = resolve("bench/e1")
mkdirSync(OUT, { recursive: true })

const programs: Record<string, string> = JSON.parse(
  readFileSync(resolve("bench/programs/lambada-benchmarks.json"), "utf8"),
).programs

// nat → ternary (LSB-first) + nat list — the lambada programs' own conventions
// (copied from bench/ic-net-bench.ts).
function encodeNat(n: number): string {
  const bits: number[] = []
  for (let m = n; m > 0; m >>= 1) bits.push(m & 1)
  let r = "0"
  for (let i = bits.length - 1; i >= 0; i--) r = (bits[i] === 1 ? "210" : "20") + r
  return r
}
function makeNatList(start: number, stop: number, step: number): string {
  let r = "0"
  for (let k = stop; k !== start - step; k -= step) r = "2" + encodeNat(k) + r
  return r
}

function write(name: string, terms: string[]): void {
  writeFileSync(join(OUT, `${name}.terms`), terms.join("\n") + "\n")
  const bytes = terms.reduce((a, t) => a + t.length, 0)
  console.log(`${name}.terms  (${terms.length} terms, ${bytes} bytes)`)
}

// ── lambada-native workloads ──
write("fib-14", [programs["recursive-fib"], encodeNat(14)])
write("fib-17", [programs["recursive-fib"], encodeNat(17)])
write("exp-5", [programs["silly-exp"], encodeNat(5)])
write("sort-32", [programs["merge-sort"], makeNatList(32, 1, -1)])
write("sort-64", [programs["merge-sort"], makeNatList(64, 1, -1)])
write("size-self", [programs["size"], programs["size"]])

// ── elaborated disp workloads ──
type H = number
interface Sess {
  dumpTernary(h: H, budget?: { remaining: number }): string
}
const session = getBackend(defaultBackendName).createSession() as unknown as Sess
const defsPath = resolve("bench/e1_defs.disp")
console.log("elaborating bench/e1_defs.disp (kernel load — takes a while cold)...")
const t0 = performance.now()
const decls = parseProgram(readFileSync(defsPath, "utf-8"), defsPath, { session: session as never }) as {
  kind: string
  name?: string
  tree?: H
}[]
console.log(`elaborated in ${((performance.now() - t0) / 1000).toFixed(1)}s`)
const env = new Map<string, H>()
for (const d of decls) if (d.kind === "Def" && d.name && d.tree !== undefined) env.set(d.name, d.tree)
const dump = (n: string): string => {
  const h = env.get(n)
  if (h === undefined) throw new Error(`missing binding: ${n}`)
  return session.dumpTernary(h, { remaining: 4_000_000_000 })
}

const add = dump("e1_add")
const paramApply = dump("e1_param_apply")
const natTy = dump("e1_nat_ty")
const treeEq = dump("e1_tree_eq")
const n = (k: number): string => dump(`e1_n${k}`)

console.log(
  `sizes: add=${add.length}B param_apply=${paramApply.length}B Nat=${natTy.length}B tree_eq=${treeEq.length}B`,
)

write("kadd-24", [add, n(24), n(24)])
write("kadd-96", [add, n(96), n(96)])
write("check-nat-6", [paramApply, natTy, n(6)])
write("check-nat-12", [paramApply, natTy, n(12)])
write("treeq-add", [treeEq, add, add])

const boolTy = dump("e1_bool_ty")
const tru = dump("e1_true")
console.log(`sizes: Bool=${boolTy.length}B true=${tru.length}B`)
write("check-bool", [paramApply, boolTy, tru])
