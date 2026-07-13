// Message protocol between the playground UI and the disp compiler worker.

// A structured, budget-capped rendering of a tree value. Decoded atoms
// (nat / string / bound name) and budget cuts ('more') carry the session
// HANDLE of the subtree they stand for, so the UI can ask the worker to
// re-render them raw or deeper (the playground's click-to-unfold). Handles
// die with the session — the client guards requests with a generation
// counter and the UI drops its values on reset/interrupt.
export type ValueNode =
  | { k: 'leaf' }
  | { k: 'nat'; n: number; h: number }
  | { k: 'str'; s: string; h: number }
  | { k: 'name'; name: string; h: number }
  | { k: 'stem'; c: [ValueNode]; h: number }
  | { k: 'fork'; c: [ValueNode, ValueNode]; h: number }
  | { k: 'more'; h: number }

// A complete structural tree, compactly: 0 = leaf, [c] = stem, [l, r] = fork.
// No decoding, no cuts — the visualizer's food (null when over budget).
export type RawTree = 0 | [RawTree] | [RawTree, RawTree]

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
  // Re-render a subtree by session handle: raw (skip nat/string/name
  // decoding at the root) and/or deeper (fresh node budget from here).
  | { id: number; type: 'render'; handle: number; budget: number; rawRoot?: boolean }
  // The COMPLETE structure of a tree, by handle or by in-scope name — the
  // reduction visualizer's real-context feed. null when the tree exceeds
  // maxNodes or the name isn't bound.
  | { id: number; type: 'raw'; handle?: number; name?: string; maxNodes: number }
  // the bundled library (the worker's virtual filesystem): list + read, for
  // the playground's file tabs and jump-to-import
  | { id: number; type: 'ls' }
  | { id: number; type: 'read'; path: string }
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
  // structured got/want for failing tests (interactive unfold in the UI)
  lhsNode?: ValueNode
  rhsNode?: ValueNode
}

export interface RunOutcome {
  ok: boolean
  error?: string
  errorLine?: number
  // Every top-level binding of the run — exported fields AND private lets —
  // in definition order, with its source extent and (when requested) the
  // pretty-printed value. Survives a mid-file error: bindings elaborated
  // before the failure are still reported.
  defs: { name: string; pretty?: string; value?: ValueNode; line?: number; endLine?: number }[]
  tests: TestResult[]
  // eval mode: the pretty-printed value of the requested expression
  value?: string
  // structured form of the eval value (interactive unfold in the UI)
  valueNode?: ValueNode
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
  | { type: 'rendered'; id: number; node: ValueNode }
  | { type: 'raw'; id: number; tree: RawTree | null }
  | { type: 'ls'; id: number; paths: string[] }
  | { type: 'read'; id: number; text: string | null }
  | { type: 'fatal'; id: number; error: string }
