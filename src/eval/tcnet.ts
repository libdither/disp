// Rooted TC-Net Session backend (EVALUATOR_PLAN.md §4.2, Phase 6 / TC_NET_PLAN.md).
//
// **M0 SCAFFOLD.** The seam is complete — WASM instantiation, the u32-arena handle
// scheme, and every Session method mapped to its C-ABI export — but the engine
// behind those exports is `todo!()` until TC_NET_PLAN milestones land. The backend
// registers ONLY when evaluators/tc-net/build.sh has produced the artifact (like
// the lambada peer), so `npm test` and the default loop never need a Rust toolchain.
//
// Handle = u32 index into the WASM instance's arena (opaque to the engine).
// `dispose()` drops the instance ⇒ the arena's backing ArrayBuffer is GC'd
// wholesale — the session-arena / no-per-handle-free model is literally how WASM
// memory works. canonicalHandles = false: with live suspensions, id≠id no longer
// implies ≠, so equality is demand-then-compare (decision 1), not an id check.

import { readFileSync, existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import type { Session, EvalBackend, SessionOpts, Budget, EvalStats, Classification } from "./types.js"

const ARTIFACT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "evaluators", "tc-net", "artifacts", "tcnet.wasm")

export function tcnetArtifactPath(): string { return ARTIFACT }
export function tcnetAvailable(): boolean { return existsSync(ARTIFACT) }

// The C-ABI exports tc_net.wasm provides (see evaluators/tc-net/crate/src/lib.rs).
interface TcNetExports {
  memory: WebAssembly.Memory
  tc_leaf(): number
  tc_stem(child: number): number
  tc_fork(left: number, right: number): number
  tc_apply(f: number, x: number, budget: number): number      // u32::MAX (-1) ⇒ budget exhausted
  tc_alloc(len: number): number
  tc_free(ptr: number, len: number): void
  tc_load_ternary(ptr: number, len: number): number
  tc_dump_ternary(h: number, budget: number): bigint          // (ptr << 32) | len
  tc_classify(h: number, budget: number): number              // 0=leaf 1=stem 2=fork, -1 exhausted
  tc_child0(h: number): number
  tc_child1(h: number): number
  tc_equal(a: number, b: number, budget: number): number      // 0=false 1=true 2=exhausted
  tc_interactions(): bigint                                    // total interactions this session
  tc_recognize_tree_eq(handle: number): void                  // register tree_eq fast-path
}

const DEFAULT_BUDGET = 5_000_000_000
const EXHAUSTED = -1 // tc_* return u32::MAX on budget exhaustion; WASM i32→JS Number is signed ⇒ -1
// The FFI budget is a single u32 (interactions). Clamp so a >u32 default (5e9)
// passes as the full u32::MAX rather than silently truncating mod 2³².
const U32_MAX = 0xffff_ffff
const clampBudget = (n: number): number => (n > U32_MAX ? U32_MAX : n)
// Floor for the per-call interaction budget (see #bud); ~u32 ceiling.
const TCNET_MIN_BUDGET = 4_000_000_000

class TcNetSession implements Session<number> {
  readonly canonicalHandles = false
  #x: TcNetExports
  #budget: number

  constructor(opts?: SessionOpts) {
    const bytes = readFileSync(ARTIFACT)
    const mod = new WebAssembly.Module(bytes)
    const inst = new WebAssembly.Instance(mod, {})
    this.#x = inst.exports as unknown as TcNetExports
    this.#budget = opts?.defaultBudget ?? DEFAULT_BUDGET
  }

  leaf(): number { return this.#x.tc_leaf() }
  stem(child: number): number { return this.#x.tc_stem(child) }
  fork(left: number, right: number): number { return this.#x.tc_fork(left, right) }

  // The host passes an eager-tuned per-call budget (compile.ts APPLY_BUDGET=40M
  // steps); budget is a non-portable per-backend divergence bound (decision 9 /
  // EVALUATOR_PLAN §8), and tcnet's interaction unit differs, so we floor it at a
  // generous interaction budget. The total conformance corpus terminates well
  // under this, so it never becomes a tight accept/reject boundary.
  #bud(budget?: Budget): number {
    return clampBudget(Math.max(budget?.remaining ?? this.#budget, TCNET_MIN_BUDGET))
  }

  // apply is LAZY: it builds the suspended application P(f,x) in O(1) and never
  // reduces, so it cannot exhaust (the EXHAUSTED check stays as a defensive guard
  // against a future eager-mode export). Forcing — and exhaustion — happens in
  // dumpTernary / classify / equal.
  apply(f: number, x: number, budget?: Budget): number {
    const h = this.#x.tc_apply(f, x, this.#bud(budget))
    if (h === EXHAUSTED) throw new Error("TC-Net: evaluation budget exhausted")
    return h
  }

  loadTernary(s: string): number {
    const bytes = new TextEncoder().encode(s)
    const ptr = this.#x.tc_alloc(bytes.length)
    new Uint8Array(this.#x.memory.buffer, ptr, bytes.length).set(bytes)
    const h = this.#x.tc_load_ternary(ptr, bytes.length)
    this.#x.tc_free(ptr, bytes.length)
    return h
  }

  // Forces full NF (this is where deferred reduction runs). On budget exhaustion
  // the engine returns ptr = u32::MAX (len 0); detect it before reading memory.
  dumpTernary(h: number, budget?: Budget): string {
    const packed = this.#x.tc_dump_ternary(h, this.#bud(budget))
    const ptr = Number(packed >> 32n)
    if (ptr === U32_MAX) throw new Error("TC-Net: evaluation budget exhausted (dump)")
    const len = Number(packed & 0xffffffffn)
    const out = new TextDecoder().decode(new Uint8Array(this.#x.memory.buffer, ptr, len).slice())
    this.#x.tc_free(ptr, len)
    return out
  }

  equal(a: number, b: number, budget?: Budget): boolean {
    const r = this.#x.tc_equal(a, b, this.#bud(budget))
    if (r > 1) throw new Error("TC-Net: evaluation budget exhausted (equal)")
    return r === 1
  }

  classify(h: number, budget?: Budget): Classification<number> {
    const tag = this.#x.tc_classify(h, this.#bud(budget))
    if (tag === 0) return { tag: "leaf" }
    if (tag === 1) return { tag: "stem", child: this.#x.tc_child0(h) }
    if (tag === 2) return { tag: "fork", left: this.#x.tc_child0(h), right: this.#x.tc_child1(h) }
    throw new Error("TC-Net: classify failed (budget exhausted?)")
  }

  // Interaction count is the backend-declared unit (never compared to eager's
  // steps — decision 9). Reported under the standard `steps` key.
  stats(): EvalStats { return { steps: Number(this.#x.tc_interactions()) } }

  // The engine pushes the compiled tree_eq handle at kernel-load; registering it
  // lets reduce intercept saturated tree_eq applications with the O(1) hash-cons
  // compare (the two-stage susp scheme), so conversion checking is cheap — without
  // it, in-language tree_eq blows the host's per-call apply budget on kernel verify.
  recognizeNative(name: string, handle: number): void {
    if (name === "tree_eq") this.#x.tc_recognize_tree_eq(handle)
  }

  // dispose drops the only references to the WASM instance/exports; the JS GC
  // then reclaims the instance and its linear-memory ArrayBuffer wholesale.
  dispose(): void { this.#x = undefined as unknown as TcNetExports }
}

export const tcnetBackend: EvalBackend<number> = {
  name: "tcnet",
  natives(): ReadonlyMap<string, readonly string[]> { return new Map() },
  createSession(opts?: SessionOpts): TcNetSession {
    if (!tcnetAvailable())
      throw new Error(`tc-net artifact missing: ${ARTIFACT} (run evaluators/tc-net/build.sh)`)
    return new TcNetSession(opts)
  },
}
