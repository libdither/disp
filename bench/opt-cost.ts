// Q1 cost experiment — the "cheapness" half of score = typechecks × cheapness (OPTIMIZER.typ §4,
// OPTIMIZER_DESIGN.md §5). For each LICENSED rewrite (proven sound in opt_q1_main/cert), measure that
// the rhs is actually CHEAPER: reduction-step count (the §4 intrinsic cost) on inputs of growing size.
//
// Runs on rust-eager (Session<number> handles): build inputs via session.apply, reduce, and read the
// DELTA of session.stats().steps. Discipline: the hash-cons memo stays ON (clearing it makes the kernel
// recursors re-reduce cold and blow up — the §3 no-memo phenomenon); lhs/rhs run on DISTINCT input
// lists so neither free-rides on the other's memoised result; the kernel is pre-warmed by compilation.
//
// Run: npx tsx bench/opt-cost.ts

import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"

type H = number
const session = getBackend(defaultBackendName).createSession() as unknown as {
  apply(f: H, x: H): H; classify(h: H): unknown; stats(): { steps: number }
}
const path = resolve("bench/opt_cost_defs.disp")
const decls = parseProgram(readFileSync(path, "utf-8"), path, { session: session as never }) as { kind: string; name?: string; tree?: H }[]
const env = new Map<string, H>()
for (const d of decls) if (d.kind === "Def" && d.name && d.tree !== undefined) env.set(d.name, d.tree)
const g = (n: string): H => { const t = env.get(n); if (t === undefined) throw new Error(`missing binding: ${n}`); return t }

const succ = g("e_succ"), zero = g("e_zero"), consH = g("e_cons"), nilH = g("e_nil")
const ap = (f: H, x: H): H => session.apply(f, x)
const nat = (k: number): H => { let t = zero; for (let i = 0; i < k; i++) t = ap(succ, t); return t }
// length-n list, element offset `off` → distinct trees so lhs/rhs cannot share a memoised result.
const mkList = (n: number, off: number): H => { let t = nilH; for (let i = n - 1; i >= 0; i--) t = ap(ap(consH, nat(off + i)), t); return t }

// reduction steps to normalize (f arg). arg is pre-built, so only the rewrite's work is counted.
function cost(f: H, arg: H): number {
  const before = session.stats().steps
  const r = session.apply(f, arg)
  session.classify(r)                     // force NF (rust-eager is eager, but ensure all steps counted)
  return session.stats().steps - before
}

type RW = { name: string; lhs: H; rhs: H; mkArg: (n: number, off: number) => H }
const rewrites: RW[] = [
  { name: "add n zero  ⤳  n", lhs: g("addzero_lhs"), rhs: g("addzero_rhs"), mkArg: (n) => nat(n) },
  { name: "len (map f xs)  ⤳  len xs", lhs: g("lenmap_lhs"), rhs: g("lenmap_rhs"), mkArg: mkList },
  { name: "map f (map g xs)  ⤳  map (f∘g) xs   [generic compose]", lhs: g("fusion_lhs"), rhs: g("fusion_rhs"), mkArg: mkList },
  { name: "map f (map g xs)  ⤳  map (λx.f(g x)) xs   [fused + specialized]", lhs: g("fusion_lhs"), rhs: g("fusion_spec_rhs"), mkArg: mkList },
]
// pre-warm every kernel path so fixed setup isn't charged to the first measurement.
for (const rw of rewrites) { session.classify(session.apply(rw.lhs, rw.mkArg(3, 900))); session.classify(session.apply(rw.rhs, rw.mkArg(3, 950))) }

const sizes = [4, 8, 16, 32, 64]
const pad = (s: string | number, n: number) => String(s).padStart(n)
console.log("Q1 cost — licensed rewrites, reduction-step counts (§4 intrinsic cost), lhs/rhs on distinct inputs\n")
rewrites.forEach((rw, ri) => {
  console.log(rw.name)
  console.log(`  ${pad("n", 4)} ${pad("lhs", 10)} ${pad("rhs", 10)} ${pad("saved", 10)} ${pad("rhs/lhs", 8)}`)
  for (const n of sizes) {
    const base = (ri + 1) * 100000 + n              // globally unique input per (rewrite, side, size)
    const lc = cost(rw.lhs, rw.mkArg(n, base)), rc = cost(rw.rhs, rw.mkArg(n, base + 50000))
    console.log(`  ${pad(n, 4)} ${pad(lc, 10)} ${pad(rc, 10)} ${pad(lc - rc, 10)} ${pad((rc / lc).toFixed(3), 8)}`)
  }
  console.log()
})

// ── §3 confound, demonstrated: map-fusion rhs on a FRESH list vs on the SAME list lhs just built.
//    The memo makes the same-list rhs near-free (lhs prepaid the shared result) — the exact reason
//    OPTIMIZER.typ §3 wants no-memo ic-net for honest, attributable per-candidate cost.
const shared = mkList(32, 77), fresh = mkList(32, 88)
const freshCost = cost(g("fusion_rhs"), fresh)
const b = session.stats().steps; session.classify(session.apply(g("fusion_lhs"), shared))
const mid = session.stats().steps; session.classify(session.apply(g("fusion_rhs"), shared))
const sameCost = session.stats().steps - mid
console.log("§3 memo confound — map-fusion rhs (n=32):")
console.log(`  rhs on a FRESH list = ${freshCost} steps;  rhs on the SAME list lhs just built = ${sameCost} steps (entangled: lhs prepaid it)`)
