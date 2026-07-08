// Message protocol between the playground UI and the disp compiler worker.

export type WorkerRequest =
  | { id: number; type: 'init'; wasmUrl: string }
  | {
      id: number
      type: 'run'
      source: string
      // virtual path for module resolution; defaults to /lib/tests/playground.disp
      path?: string
      // pretty-print top-level def values in the response (REPL cell mode)
      wantDefPretty?: boolean
    }
  | { id: number; type: 'eval'; context: string; expr: string; path?: string }
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
  steps: number
}

export interface TestResult {
  index: number
  line?: number
  pass: boolean
  lhs?: string
  rhs?: string
}

export interface RunOutcome {
  ok: boolean
  error?: string
  errorLine?: number
  defs: { name: string; pretty?: string }[]
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
