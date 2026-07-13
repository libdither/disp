// Message protocol between the playground UI and the disp compiler worker.

export type WorkerRequest =
  | { id: number; type: 'init'; wasmUrl: string }
  // Restore the shipped precompiled kernel (static/kernel.snap) into the
  // session's module cache. Bytes are fetched once and kept for re-restores
  // after 'reset'. Fails (fatal for this id, worker stays alive) when the
  // snapshot is missing or was built against a different lib/ than the
  // bundle serves — callers fall back to elaborating from source.
  | { id: number; type: 'restore'; snapshotUrl: string }
  | {
      id: number
      type: 'run'
      source: string
      // virtual path for module resolution; defaults to /lib/tests/playground.disp
      path?: string
      // pretty-print top-level def values in the response (REPL cell mode)
      wantDefPretty?: boolean
    }
  | { id: number; type: 'eval'; context: string; expr: string; path?: string; wantDefPretty?: boolean }
  | { id: number; type: 'reset' }

// Streaming progress: one per elaborated item (defs, tests, opens — including
// everything the kernel loads underneath a `use`).
export interface ItemEvent {
  type: 'item'
  kind: string
  name?: string
  testIndex?: number
  sourcePath?: string
  depth: number
  // 1-based source line for depth-0 (root buffer) items, when known
  line?: number
  steps: number
}

export interface TestResult {
  index: number
  line?: number
  endLine?: number
  pass: boolean
  lhs?: string
  rhs?: string
}

export interface RunOutcome {
  ok: boolean
  error?: string
  errorLine?: number
  // Every top-level binding of the run — exported fields AND private lets —
  // in definition order, with its source extent and (when requested) the
  // pretty-printed value. Survives a mid-file error: bindings elaborated
  // before the failure are still reported.
  defs: { name: string; pretty?: string; line?: number; endLine?: number }[]
  tests: TestResult[]
  // eval mode: the pretty-printed value of the requested expression
  value?: string
  // decoded hints for the eval value (nat / string / named constant)
  valueHint?: string
  elapsedMs: number
  steps: number
  memBytes: number
}

export type WorkerResponse =
  | { type: 'ready'; id: number }
  | ItemEvent
  | { type: 'result'; id: number; outcome: RunOutcome }
  | { type: 'fatal'; id: number; error: string }
