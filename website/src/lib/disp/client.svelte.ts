// Main-thread client for the disp compiler worker. One module-level singleton
// so the (expensive) kernel session survives SPA navigation — Learn-page
// examples and the Playground share it.

import { base } from '$app/paths'
import type { WorkerRequest, WorkerResponse, ItemEvent, RunOutcome } from './protocol.ts'

export type DispStatus =
  | 'cold'
  | 'booting'
  | 'restoring-kernel'
  | 'loading-kernel'
  | 'ready'
  | 'running'
  | 'dead'

// Omit that distributes over the request union (a plain Omit collapses the
// variants and loses their distinct payload keys)
type RequestBody = WorkerRequest extends infer R ? (R extends WorkerRequest ? Omit<R, 'id'> : never) : never

type Pending = {
  resolve: (o: RunOutcome) => void
  reject: (e: Error) => void
}

const KERNEL_PRELUDE = 'open use "../kernel/prelude.disp"\n'

class DispClient {
  #worker: Worker | null = null
  #nextId = 1
  #pending = new Map<number, Pending>()
  #initPromise: Promise<void> | null = null
  #kernelPromise: Promise<RunOutcome> | null = null

  status: DispStatus = $state('cold')
  kernelLoaded = $state(false)
  memBytes = $state(0)
  // live feed of elaboration items (drained by whoever renders progress)
  onItem: ((e: ItemEvent) => void) | null = null

  // module-depth items seen this session; once a successful run has streamed
  // a kernel's worth, the session module cache holds it — later runs are warm
  #moduleItems = 0

  #spawn(): Worker {
    const w = new Worker(new URL('./worker.ts', import.meta.url), { type: 'module' })
    w.onmessage = (e: MessageEvent<WorkerResponse>) => {
      const msg = e.data
      if (msg.type === 'item') {
        if (msg.depth > 0) this.#moduleItems++
        this.onItem?.(msg)
        return
      }
      const p = this.#pending.get(msg.id)
      if (!p) return
      this.#pending.delete(msg.id)
      if (msg.type === 'fatal') p.reject(new Error(msg.error))
      else if (msg.type === 'result') {
        this.memBytes = msg.outcome.memBytes
        if (!msg.outcome.error && this.#moduleItems > 100) this.kernelLoaded = true
        p.resolve(msg.outcome)
      } else p.resolve({ ok: true, defs: [], tests: [], elapsedMs: 0, steps: 0, memBytes: 0 })
    }
    w.onerror = (e) => {
      for (const [, p] of this.#pending) p.reject(new Error(`worker error: ${e.message}`))
      this.#pending.clear()
      this.status = 'dead'
    }
    return w
  }

  #request(msg: RequestBody): Promise<RunOutcome> {
    const id = this.#nextId++
    return new Promise((resolve, reject) => {
      this.#pending.set(id, { resolve, reject })
      this.#worker!.postMessage({ ...msg, id })
    })
  }

  /**
   * Spawn the worker + instantiate the WASM engine, then install the shipped
   * precompiled kernel (static/kernel.snap — seconds, not the ~1-minute
   * self-verification). A missing or stale snapshot downgrades gracefully:
   * the session starts cold and the first kernel-opening run elaborates from
   * source exactly as before.
   */
  init(): Promise<void> {
    if (this.#initPromise) return this.#initPromise
    this.status = 'booting'
    this.#worker = this.#spawn()
    this.#initPromise = (async () => {
      await this.#request({
        type: 'init',
        wasmUrl: new URL(`${base}/rust_eager.wasm`, location.href).href
      })
      await this.#restore()
      this.status = 'ready'
    })()
    this.#initPromise.catch(() => {
      this.status = 'dead'
    })
    return this.#initPromise
  }

  /** Install the precompiled kernel; resolves false when unavailable/stale. */
  async #restore(): Promise<boolean> {
    this.status = 'restoring-kernel'
    try {
      await this.#request({
        type: 'restore',
        snapshotUrl: new URL(`${base}/kernel.snap`, location.href).href
      })
      this.kernelLoaded = true
      return true
    } catch (e) {
      console.warn('[disp] precompiled kernel unavailable, will elaborate from source:', e)
      return false
    }
  }

  /**
   * Run `open use kernel/prelude` on the session. With the precompiled
   * kernel restored (the default after init) this is a warm cache hit and
   * returns in milliseconds; after `reset({ fromSource: true })` it is the
   * genuine ~1-minute elaborate-and-self-verify, streaming progress through
   * onItem — you are watching the type system check itself.
   */
  loadKernel(): Promise<RunOutcome> {
    if (this.#kernelPromise) return this.#kernelPromise
    this.#kernelPromise = (async () => {
      await this.init()
      this.status = 'loading-kernel'
      try {
        const out = await this.#request({ type: 'run', source: KERNEL_PRELUDE })
        this.kernelLoaded = out.ok
        this.status = 'ready'
        return out
      } catch (e) {
        // the worker's onerror may have flipped us to 'dead' concurrently
        if ((this.status as DispStatus) !== 'dead') this.status = 'ready'
        this.#kernelPromise = null
        throw e
      }
    })()
    return this.#kernelPromise
  }

  async run(source: string, opts?: { wantDefPretty?: boolean }): Promise<RunOutcome> {
    await this.init()
    this.status = 'running'
    try {
      return await this.#request({
        type: 'run',
        source,
        wantDefPretty: opts?.wantDefPretty
      })
    } finally {
      if (this.status === 'running') this.status = 'ready'
    }
  }

  async evalExpr(context: string, expr: string, opts?: { wantDefPretty?: boolean }): Promise<RunOutcome> {
    await this.init()
    this.status = 'running'
    try {
      return await this.#request({ type: 'eval', context, expr, wantDefPretty: opts?.wantDefPretty })
    } finally {
      if (this.status === 'running') this.status = 'ready'
    }
  }

  /**
   * Soft reset: fresh arena + session. By default the precompiled kernel is
   * re-installed into the fresh session; `fromSource: true` leaves it cold so
   * the next kernel load genuinely re-elaborates and self-verifies from
   * source (the don't-trust-the-snapshot path).
   */
  async reset(opts?: { fromSource?: boolean }): Promise<void> {
    if (!this.#worker) return
    this.#kernelPromise = null
    this.kernelLoaded = false
    this.#moduleItems = 0
    await this.#request({ type: 'reset' })
    if (!opts?.fromSource) await this.#restore()
    this.status = 'ready'
  }

  /** Hard stop: kill the worker mid-run (runaway elaboration), respawn cold. */
  interrupt(): void {
    this.#worker?.terminate()
    for (const [, p] of this.#pending) p.reject(new Error('interrupted'))
    this.#pending.clear()
    this.#worker = null
    this.#initPromise = null
    this.#kernelPromise = null
    this.kernelLoaded = false
    this.#moduleItems = 0
    this.status = 'cold'
  }
}

export const disp = new DispClient()
