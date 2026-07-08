// The disp compiler worker: hosts the elaborator + rust-eager WASM session off
// the main thread. The compiler's driver expects a node-ish `process` global
// (cwd for module resolution, env for feature flags), so shim it BEFORE the
// compiler modules load — hence the dynamic import of the runner.

;(globalThis as unknown as { process: unknown }).process ??= {
  env: {},
  cwd: () => '/',
  argv: [] as string[]
}

import type { WorkerRequest, WorkerResponse, ItemEvent } from './protocol.ts'
import type { DispRunner } from './runner.ts'

let runner: DispRunner | null = null

const post = (msg: WorkerResponse) => (self as unknown as Worker).postMessage(msg)

self.onmessage = async (e: MessageEvent<WorkerRequest>) => {
  const msg = e.data
  try {
    switch (msg.type) {
      case 'init': {
        const [{ DispRunner }, bytes] = await Promise.all([
          import('./runner.ts'),
          fetch(msg.wasmUrl).then((r) => {
            if (!r.ok) throw new Error(`failed to fetch ${msg.wasmUrl}: ${r.status}`)
            return r.arrayBuffer()
          })
        ])
        runner = new DispRunner(bytes)
        post({ type: 'ready', id: msg.id })
        break
      }
      case 'run': {
        if (!runner) throw new Error('worker not initialized')
        const onItem = (ev: ItemEvent) => post(ev)
        const outcome = runner.run(msg.source, msg.path, msg.wantDefPretty ?? false, onItem)
        post({ type: 'result', id: msg.id, outcome })
        break
      }
      case 'eval': {
        if (!runner) throw new Error('worker not initialized')
        const onItem = (ev: ItemEvent) => post(ev)
        const outcome = runner.evalExpr(msg.context, msg.expr, onItem, msg.path)
        post({ type: 'result', id: msg.id, outcome })
        break
      }
      case 'reset': {
        runner?.reset()
        post({ type: 'ready', id: msg.id })
        break
      }
    }
  } catch (err) {
    post({ type: 'fatal', id: msg.id, error: err instanceof Error ? err.message : String(err) })
  }
}
