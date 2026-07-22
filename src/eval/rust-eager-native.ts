// rust-eager-native Session backend — the Rust hash-consed eager reducer as an in-process
// Node N-API addon (NOT wasm). Same engine as the rust-eager wasm backend (one crate, one
// reducer), but native: the Session runs in host RAM instead of the wasm32 4 GiB
// linear-memory ceiling (the wall the auto-verify check hits), with -O3 and no wasm sandbox
// tax on the reduce hot loop. See evaluators/rust-eager/crate/src/native.rs.
//
// Each Session = one `EagerSession` napi object owning its own arena (the per-instance
// analogue of the wasm thread_local — a `.node` is one shared dylib per process, so it
// CAN'T lean on instance-per-session isolation the way the wasm backend does). dispose()
// drops the reference; napi's finalizer frees the arena when the JS object is collected.
// canonicalHandles = false: with live suspensions id≠id no longer implies ≠, so equality is
// demand-then-compare, not an id check.
//
// The backend registers ONLY when evaluators/rust-eager/build-native.sh has produced the
// artifact, so `npm test` and the default loop never need a native toolchain.

import { createRequire } from "node:module"
import { existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import type { Session, EvalBackend, SessionOpts, Budget, EvalStats, Classification } from "./types.js"

const ARTIFACT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "evaluators", "rust-eager", "artifacts", "rust_eager.node")

export function rustEagerNativeArtifactPath(): string { return ARTIFACT }
export function rustEagerNativeAvailable(): boolean { return existsSync(ARTIFACT) }

// The napi class the addon exports (see evaluators/rust-eager/crate/src/native.rs).
// napi-derive lowercases snake_case method names → camelCase.
interface EagerSessionNative {
  leaf(): number
  stem(child: number): number
  fork(left: number, right: number): number
  apply(f: number, x: number, budget: number): number       // u32::MAX (4294967295) ⇒ exhausted
  applyLazy(f: number, x: number): number
  loadTernary(s: string): number
  dumpTernary(h: number, budget: number): string | null     // null ⇒ exhausted
  classify(h: number, budget: number): number               // 0=leaf 1=stem 2=fork, -1 exhausted
  child0(h: number): number
  child1(h: number): number
  equal(a: number, b: number, budget: number): number       // 0=false 1=true 2=exhausted
  interactions(): number
  nodeCount(): number
  freeCount(): number
  beginScope(): void
  endScope(keep: number[]): void
  recognizeTreeEq(handle: number): void
  setMemoLimit(n: number): void
  clearCaches(): void
  loadSnapshot(path: string, stamp: string): boolean
  saveSnapshot(path: string, stamp: string, minCost: number): number  // 1 saved, 0 unchanged, -1 error
  frozenHits(): number
}
interface RustEagerNativeAddon {
  EagerSession: new () => EagerSessionNative
}

const DEFAULT_BUDGET = 5_000_000_000
// napi returns the apply sentinel as an UNSIGNED u32 (u32::MAX), not the wasm path's
// signed -1 (which is how a WASM i32 u32::MAX reads in JS).
const EXHAUSTED = 0xffff_ffff
const U32_MAX = 0xffff_ffff
const clampBudget = (n: number): number => (n > U32_MAX ? U32_MAX : n)
const RUST_EAGER_MIN_BUDGET = 4_000_000_000

// The addon is a process-wide singleton dylib; load it once, lazily (only when a native
// session is actually created, so importing this module is free if the artifact is absent).
let addon: RustEagerNativeAddon | undefined
function loadAddon(): RustEagerNativeAddon {
  if (!addon) addon = createRequire(import.meta.url)(ARTIFACT) as RustEagerNativeAddon
  return addon
}

class RustEagerNativeSession implements Session<number> {
  readonly canonicalHandles = false
  #s: EagerSessionNative
  #budget: number

  constructor(opts?: SessionOpts) {
    this.#s = new (loadAddon().EagerSession)()
    this.#budget = opts?.defaultBudget ?? DEFAULT_BUDGET
    // Memory knob: cap the apply memo for smaller-footprint / long-lived sessions
    // (RUST_EAGER_MEMO_LIMIT=<entries>). Off by default — per-file sessions are bounded
    // by dispose() anyway.
    const lim = Number(process.env.RUST_EAGER_MEMO_LIMIT ?? 0)
    if (Number.isFinite(lim) && lim > 0) this.#s.setMemoLimit(lim >>> 0)
  }

  // Not part of the Session ABI: drop re-derivable caches to relieve memory pressure
  // mid-session (the shared-session test harness calls this per file); live trees untouched.
  clearCaches(): void { this.#s.clearCaches() }

  // Persistent reduction cache: memo entries are calculus-level facts, so only an
  // evaluator change invalidates — callers stamp with a hash of the addon binary.
  // Load is refused (false) on a non-pristine session or stamp/format/checksum
  // mismatch. Save compacts (holes dropped), keeps entries costing ≥ minCost
  // fork-dispatches, and returns 1 saved / 0 unchanged-skipped / -1 error.
  loadSnapshot(path: string, stamp: string): boolean { return this.#s.loadSnapshot(path, stamp) }
  saveSnapshot(path: string, stamp: string, minCost = 0): number { return this.#s.saveSnapshot(path, stamp, minCost) }
  // Frozen-memo hits this session — the snapshot's realized value.
  frozenHits(): number { return this.#s.frozenHits() }

  leaf(): number { return this.#s.leaf() }
  stem(child: number): number { return this.#s.stem(child) }
  fork(left: number, right: number): number { return this.#s.fork(left, right) }

  // Same eager-tuned per-call budget shape as the wasm backend (compile.ts APPLY_BUDGET);
  // floored at a generous interaction budget so it never becomes a tight accept boundary.
  #bud(budget?: Budget): number {
    return clampBudget(Math.max(budget?.remaining ?? this.#budget, RUST_EAGER_MIN_BUDGET))
  }

  // EAGER: fully normalizes apply(f,x). Can exhaust — the EXHAUSTED check is load-bearing.
  apply(f: number, x: number, budget?: Budget): number {
    const h = this.#s.apply(f, x, this.#bud(budget))
    if (h === EXHAUSTED) throw new Error("rust-eager-native: evaluation budget exhausted")
    return h
  }

  // Lazy apply (M1): build P(f,x) = Susp(f,x) in O(1), reduced on demand by the next
  // forcing observation.
  applyLazy(f: number, x: number): number { return this.#s.applyLazy(f, x) }

  // No ptr/len marshalling: napi copies the JS string in and the result String out.
  loadTernary(s: string): number { return this.#s.loadTernary(s) }

  dumpTernary(h: number, budget?: Budget): string {
    const out = this.#s.dumpTernary(h, this.#bud(budget))
    if (out === null) throw new Error("rust-eager-native: evaluation budget exhausted (dump)")
    return out
  }

  equal(a: number, b: number, budget?: Budget): boolean {
    const r = this.#s.equal(a, b, this.#bud(budget))
    if (r > 1) throw new Error("rust-eager-native: evaluation budget exhausted (equal)")
    return r === 1
  }

  classify(h: number, budget?: Budget): Classification<number> {
    const tag = this.#s.classify(h, this.#bud(budget))
    if (tag === 0) return { tag: "leaf" }
    if (tag === 1) return { tag: "stem", child: this.#s.child0(h) }
    if (tag === 2) return { tag: "fork", left: this.#s.child0(h), right: this.#s.child1(h) }
    throw new Error("rust-eager-native: classify failed (budget exhausted?)")
  }

  // Interaction count (the backend-declared unit, never compared to eager's steps).
  // `nodes` exposes the live arena high-water (for observing scoped reclamation).
  stats(): EvalStats { return { steps: this.#s.interactions(), nodes: this.#s.nodeCount(), free: this.#s.freeCount() } }

  // Scoped reclamation: beginScope() marks a point; endScope(keep) frees everything
  // allocated since, except nodes reachable from `keep`. See types.ts / src/native.rs.
  beginScope(): void { this.#s.beginScope() }
  endScope(keep: number[]): void { this.#s.endScope(keep) }

  recognizeNative(name: string, handle: number): void {
    if (name === "tree_eq") this.#s.recognizeTreeEq(handle)
  }

  // Drop the only reference to the napi object; its finalizer then frees the owned arena.
  dispose(): void { this.#s = undefined as unknown as EagerSessionNative }
}

export const rustEagerNativeBackend: EvalBackend<number> = {
  name: "rust-eager-native",
  natives(): ReadonlyMap<string, readonly string[]> { return new Map() },
  createSession(opts?: SessionOpts): RustEagerNativeSession {
    if (!rustEagerNativeAvailable())
      throw new Error(`rust-eager-native artifact missing: ${ARTIFACT} (run evaluators/rust-eager/build-native.sh)`)
    return new RustEagerNativeSession(opts)
  },
}
