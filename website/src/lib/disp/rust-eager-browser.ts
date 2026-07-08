// Browser port of the rust-eager WASM Session backend
// (mirrors src/eval/rust-eager.ts — the node version reads the artifact from
// disk; here the caller fetches the bytes and hands them over. The module has
// zero WASM imports, so instantiation needs no glue).
//
// Handle = u32 index into the WASM instance's arena. The arena only grows;
// dispose() drops the instance so the whole linear memory is GC'd at once.

import type { Session, SessionOpts, Budget, EvalStats, Classification } from '../../../../src/eval/types.ts'

interface RustEagerExports {
  memory: WebAssembly.Memory
  tc_leaf(): number
  tc_stem(child: number): number
  tc_fork(left: number, right: number): number
  tc_apply(f: number, x: number, budget: number): number
  tc_apply_lazy(f: number, x: number, budget: number): number
  tc_alloc(len: number): number
  tc_free(ptr: number, len: number): void
  tc_load_ternary(ptr: number, len: number): number
  tc_dump_ternary(h: number, budget: number): bigint
  tc_classify(h: number, budget: number): number
  tc_child0(h: number): number
  tc_child1(h: number): number
  tc_equal(a: number, b: number, budget: number): number
  tc_interactions(): bigint
  tc_recognize_tree_eq(handle: number): void
  tc_set_memo_limit(n: number): void
  tc_clear_caches(): void
}

const DEFAULT_BUDGET = 5_000_000_000
const EXHAUSTED = -1
const U32_MAX = 0xffff_ffff
const clampBudget = (n: number): number => (n > U32_MAX ? U32_MAX : n)
const RUST_EAGER_MIN_BUDGET = 4_000_000_000

export class RustEagerBrowserSession implements Session<number> {
  readonly canonicalHandles = false
  #x: RustEagerExports
  #budget: number

  constructor(wasmBytes: ArrayBuffer | Uint8Array, opts?: SessionOpts) {
    const mod = new WebAssembly.Module(wasmBytes as BufferSource)
    const inst = new WebAssembly.Instance(mod, {})
    this.#x = inst.exports as unknown as RustEagerExports
    this.#budget = opts?.defaultBudget ?? DEFAULT_BUDGET
  }

  clearCaches(): void { this.#x.tc_clear_caches() }
  memoryBytes(): number { return this.#x.memory.buffer.byteLength }

  leaf(): number { return this.#x.tc_leaf() }
  stem(child: number): number { return this.#x.tc_stem(child) }
  fork(left: number, right: number): number { return this.#x.tc_fork(left, right) }

  #bud(budget?: Budget): number {
    return clampBudget(Math.max(budget?.remaining ?? this.#budget, RUST_EAGER_MIN_BUDGET))
  }

  apply(f: number, x: number, budget?: Budget): number {
    const h = this.#x.tc_apply(f, x, this.#bud(budget))
    if (h === EXHAUSTED) throw new Error('rust-eager: evaluation budget exhausted')
    return h
  }

  applyLazy(f: number, x: number): number {
    return this.#x.tc_apply_lazy(f, x, 0)
  }

  loadTernary(s: string): number {
    const bytes = new TextEncoder().encode(s)
    const ptr = this.#x.tc_alloc(bytes.length)
    new Uint8Array(this.#x.memory.buffer, ptr, bytes.length).set(bytes)
    const h = this.#x.tc_load_ternary(ptr, bytes.length)
    this.#x.tc_free(ptr, bytes.length)
    return h
  }

  dumpTernary(h: number, budget?: Budget): string {
    const packed = this.#x.tc_dump_ternary(h, this.#bud(budget))
    const ptr = Number(packed >> 32n)
    if (ptr === U32_MAX) throw new Error('rust-eager: evaluation budget exhausted (dump)')
    const len = Number(packed & 0xffffffffn)
    const out = new TextDecoder().decode(new Uint8Array(this.#x.memory.buffer, ptr, len).slice())
    this.#x.tc_free(ptr, len)
    return out
  }

  equal(a: number, b: number, budget?: Budget): boolean {
    const r = this.#x.tc_equal(a, b, this.#bud(budget))
    if (r > 1) throw new Error('rust-eager: evaluation budget exhausted (equal)')
    return r === 1
  }

  classify(h: number, budget?: Budget): Classification<number> {
    const tag = this.#x.tc_classify(h, this.#bud(budget))
    if (tag === 0) return { tag: 'leaf' }
    if (tag === 1) return { tag: 'stem', child: this.#x.tc_child0(h) }
    if (tag === 2) return { tag: 'fork', left: this.#x.tc_child0(h), right: this.#x.tc_child1(h) }
    throw new Error('rust-eager: classify failed (budget exhausted?)')
  }

  stats(): EvalStats { return { steps: Number(this.#x.tc_interactions()) } }

  recognizeNative(name: string, handle: number): void {
    if (name === 'tree_eq') this.#x.tc_recognize_tree_eq(handle)
  }

  dispose(): void { this.#x = undefined as unknown as RustEagerExports }
}
