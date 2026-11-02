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
  // Apply an in-scope value (by handle or name) to concrete argument trees on
  // the REAL evaluator and return the result's structure — the visualizer's
  // engine macro-step for definitions too large to ship as pods. Responds
  // with 'raw'; null when unbound, the apply diverges past its budget, or
  // the RESULT exceeds maxNodes.
  | { id: number; type: 'applySpine'; handle?: number; name?: string; args: RawTree[]; maxNodes: number }
  // the bundled library (the worker's virtual filesystem): list + read, for
  // the playground's file tabs and jump-to-import. write/rm mutate the vfs —
  // written files are immediately `use`-able (stale module-cache entries for
  // the path are dropped); the bundled originals return on worker respawn.
  | { id: number; type: 'ls' }
  | { id: number; type: 'read'; path: string }
  | { id: number; type: 'write'; path: string; text: string }
  | { id: number; type: 'rm'; path: string }
  // jump-to-definition: where is `name` defined, as seen from fromPath?
  // fromText is the (possibly unsaved) buffer content of fromPath.
  | { id: number; type: 'def'; name: string; fromPath: string; fromText: string }
  // Autocomplete and hover: every top-level name the opened files bring into
  // scope (the origin buffer's own names are indexed on the main thread).
  | { id: number; type: 'scope'; fromPath: string; fromText: string }
  // The inspector's food. 'graph' dumps trees by handle or name; 'term' compiles
  // an expression (a member at a line, or free text) with the real elaborator,
  // keeping every application held so the inspector can step through it.
  | { id: number; type: 'graph'; handles: number[]; names?: string[]; maxNodes: number }
  | { id: number; type: 'term'; expr?: string; path?: string; text?: string; line?: number; side?: 'lhs' | 'rhs'; value?: number; maxNodes: number }
  // Replays bracket abstraction for the same kinds of term, one rewrite per frame.
  | { id: number; type: 'bracket'; expr?: string; path?: string; text?: string; line?: number; side?: 'lhs' | 'rhs'; maxNodes: number }
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

// A hash-consed dump of tree handles: node i's children have smaller indices.
// Three ints per node (tag, a, b); labels name the indices the session knows.
export const G_LEAF = 0
export const G_STEM = 1
export const G_FORK = 2
export const G_APP = 3
export const G_VAR = 4 // a = index into strs
export const G_CUT = 5 // over budget: a = the session handle, expandable on demand
export const G_LAM = 6 // a binder not yet abstracted away: a = the name's string index, b = its body
export const G_HELD_K = 7 // K(a) kept visibly K, so K(2) doesn't read as the numeral 3

export interface Graph {
  nodes: Int32Array
  strs: string[]
  labels: [number, string][]
  roots: number[]
}

export type BracketRule = 'start' | 'split' | 'const' | 'id' | 'eta' | 'eta-after' | 'k-comp' | 'lam' | 'skip'

/// One rewrite of bracket abstraction. Its frame is the whole term after the rewrite.
export interface BracketStep {
  rule: BracketRule
  name: string
  /// child-index path to the rewritten spot
  at: number[]
  before: string
  after: string
}

export interface BracketDump {
  graph: Graph // roots[i] = the term after steps[i]
  steps: BracketStep[]
  /// rewrites left out of a very long replay
  skipped: number
  /// the replay failed to land on the compiler's own output, which is a bug in this view
  mismatch: boolean
  /// the leading binders' names: what a run can call this with
  params: string[]
  error?: string
}

export interface TermDump {
  graph: Graph // roots[0] = the held term, roots[1] = the real evaluator's value (when it closed)
  source: string
  error?: string
}

/// A top-level name reachable through the buffer's opens.
export interface ScopeItem {
  name: string
  path: string
  line: number
  sig?: string
  params?: string[]
  doc?: string
}

export interface TestResult {
  index: number
  line?: number
  endLine?: number
  pass: boolean
  lhsH?: number
  rhsH?: number
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
  defs: { name: string; pretty?: string; value?: ValueNode; line?: number; endLine?: number; h?: number }[]
  tests: TestResult[]
  // evaluator steps each root item spent, for the cost display
  costs?: { line: number; steps: number }[]
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
  | { type: 'def'; id: number; site: { path: string; line: number } | null }
  | { type: 'scope'; id: number; items: ScopeItem[] }
  | { type: 'graph'; id: number; graph: Graph }
  | { type: 'term'; id: number; dump: TermDump }
  | { type: 'bracket'; id: number; dump: BracketDump }
  | { type: 'fatal'; id: number; error: string }
