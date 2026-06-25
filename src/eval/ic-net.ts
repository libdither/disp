// rust-ic-net Session backend — the MATERIALIZED interaction-net evaluator (M0:
// sequential). Unlike rust-eager (a hash-consed tree reducer), the agents/ports are
// real arena nodes and active pairs sit in a redex bag — the substrate M2 parallelizes.
// See research/interaction-combinator/RUST_IC_NET_DESIGN.md.
//
// NOT a checker backend (no hash-consing ⇒ no O(1) conversion); registers only when
// built, never the default. Validated DIFFERENTIALLY against rust-eager / disp-eager
// (strong confluence ⇒ identical NFs). Handle = u32 producer Port (tag + node index),
// opaque to the host. apply is LAZY (builds P(f,x)); dumpTernary/equal force full NF.

import { readFileSync, existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import type { Session, EvalBackend, SessionOpts, Budget, EvalStats } from "./types.js"

const ARTIFACT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "evaluators", "rust-ic-net", "artifacts", "rust_ic_net.wasm")

export function icNetArtifactPath(): string { return ARTIFACT }
export function icNetAvailable(): boolean { return existsSync(ARTIFACT) }

interface IcNetExports {
  memory: WebAssembly.Memory
  tc_leaf(): number
  tc_stem(child: number): number
  tc_fork(left: number, right: number): number
  tc_apply(f: number, x: number, budget: number): number      // LAZY: builds P(f,x)
  tc_alloc(len: number): number
  tc_free(ptr: number, len: number): void
  tc_load_ternary(ptr: number, len: number): number
  tc_dump_ternary(h: number, budget: number): bigint          // (ptr<<32)|len; forces full NF
  tc_equal(a: number, b: number, budget: number): number      // 0=false 1=true 2=exhausted
  tc_interactions(): bigint
  tc_nodes(): bigint                                          // live node-cell count
  tc_dnp(): bigint                                            // δⁿ⊗P firings (shared suspensions)
}

const DEFAULT_BUDGET = 5_000_000_000
const U32_MAX = 0xffff_ffff
const clampBudget = (n: number): number => (n > U32_MAX ? U32_MAX : n)
// The materialized net allocates per interaction (no hash-cons sharing), so a forcing
// observation runs many more interactions than rust-eager's step count — floor the
// per-call budget generously (decision 9: budget is a non-portable per-backend bound).
const MIN_BUDGET = 4_000_000_000

class IcNetSession implements Session<number> {
  readonly canonicalHandles = false
  #x: IcNetExports
  #budget: number

  constructor(opts?: SessionOpts) {
    const bytes = readFileSync(ARTIFACT)
    const mod = new WebAssembly.Module(bytes)
    const inst = new WebAssembly.Instance(mod, {})
    this.#x = inst.exports as unknown as IcNetExports
    this.#budget = opts?.defaultBudget ?? DEFAULT_BUDGET
  }

  #bud(budget?: Budget): number {
    return clampBudget(Math.max(budget?.remaining ?? this.#budget, MIN_BUDGET))
  }

  leaf(): number { return this.#x.tc_leaf() }
  stem(child: number): number { return this.#x.tc_stem(child) }
  fork(left: number, right: number): number { return this.#x.tc_fork(left, right) }

  // LAZY: builds the suspended application P(f,x) in O(1); reduction happens in the
  // forcing observations (dumpTernary/equal), so apply itself cannot exhaust.
  apply(f: number, x: number, _budget?: Budget): number {
    return this.#x.tc_apply(f, x, 0)
  }

  loadTernary(s: string): number {
    const bytes = new TextEncoder().encode(s)
    const ptr = this.#x.tc_alloc(bytes.length)
    new Uint8Array(this.#x.memory.buffer, ptr, bytes.length).set(bytes)
    const h = this.#x.tc_load_ternary(ptr, bytes.length)
    this.#x.tc_free(ptr, bytes.length)
    return h
  }

  // Forces full NF (where deferred reduction runs). ptr = u32::MAX ⇒ budget exhausted.
  dumpTernary(h: number, budget?: Budget): string {
    const packed = this.#x.tc_dump_ternary(h, this.#bud(budget))
    const ptr = Number(packed >> 32n)
    if (ptr === U32_MAX) throw new Error("rust-ic-net: evaluation budget exhausted (dump)")
    const len = Number(packed & 0xffffffffn)
    const out = new TextDecoder().decode(new Uint8Array(this.#x.memory.buffer, ptr, len).slice())
    this.#x.tc_free(ptr, len)
    return out
  }

  equal(a: number, b: number, budget?: Budget): boolean {
    const r = this.#x.tc_equal(a, b, this.#bud(budget))
    if (r > 1) throw new Error("rust-ic-net: evaluation budget exhausted (equal)")
    return r === 1
  }

  // Interaction count is the backend-declared unit (never compared to eager's steps).
  // `nodes` is reported too — a materialized net allocates agents, proving it is not a
  // recompiled tree reducer (an M0-gate signal; see the design doc).
  stats(): EvalStats {
    return { steps: Number(this.#x.tc_interactions()), nodes: Number(this.#x.tc_nodes()), dnp: Number(this.#x.tc_dnp()) }
  }

  dispose(): void { this.#x = undefined as unknown as IcNetExports }
}

export const icNetBackend: EvalBackend<number> = {
  name: "ic-net",
  natives(): ReadonlyMap<string, readonly string[]> { return new Map() },
  createSession(opts?: SessionOpts): IcNetSession {
    if (!icNetAvailable())
      throw new Error(`rust-ic-net artifact missing: ${ARTIFACT} (run evaluators/rust-ic-net/build.sh)`)
    return new IcNetSession(opts)
  },
}
