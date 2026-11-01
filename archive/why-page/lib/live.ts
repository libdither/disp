// Runs the page's programs on the shared in-browser compiler and reads the answers back as trees.
import { disp } from '$lib/disp/client.svelte'
import type { RawTree } from '$lib/disp/protocol'
import { CONTEXT } from './programs'

let queue: Promise<unknown> = Promise.resolve()

/** Runs `defs` under the page's imports and reads back `names`. One run at a time: a name only means something in the run that bound it. */
export function ask(defs: string, names: string[], maxNodes = 200000): Promise<Record<string, RawTree | null>> {
  const job = queue.then(async () => {
    const out = await disp.run(CONTEXT + defs)
    if (out.error) throw new Error(out.error)
    const got: Record<string, RawTree | null> = {}
    for (const n of names) got[n] = await disp.raw({ name: n }, maxNodes)
    return got
  })
  queue = job.catch(() => {})
  return job
}

export const listOf = (x: RawTree | null): RawTree[] => {
  const out: RawTree[] = []
  let cur = x
  while (cur !== null && cur !== 0 && cur.length === 2) {
    out.push(cur[0])
    cur = cur[1]
  }
  return out
}

export const natOf = (x: RawTree): number | null => {
  let n = 0
  let cur = x
  while (cur !== 0) {
    if (cur.length !== 2 || cur[0] !== 0) return null
    n++
    cur = cur[1]
  }
  return n
}

export const strOf = (x: RawTree): string | null => {
  let out = ''
  let cur = x
  while (cur !== 0) {
    if (cur.length !== 2) return null
    const code = natOf(cur[0])
    if (code === null || code < 32) return null
    out += String.fromCodePoint(code)
    cur = cur[1]
  }
  return out || null
}

export type Verdict = { kind: 'Proved' | 'Open' } | { kind: 'Refuted'; at: RawTree }

export const verdictOf = (x: RawTree | null): Verdict => {
  if (x !== null && x !== 0 && x.length === 2 && strOf(x[0]) === 'Refuted') return { kind: 'Refuted', at: x[1] }
  return { kind: x !== null && strOf(x) === 'Proved' ? 'Proved' : 'Open' }
}

export const nodesOf = (x: RawTree): number => (x === 0 ? 1 : 1 + x.reduce((n: number, c) => n + nodesOf(c), 0))

export const showTree = (x: RawTree): string => {
  const n = natOf(x)
  if (n !== null && n > 0) return String(n)
  if (x === 0) return 't'
  const arg = (y: RawTree) => (y === 0 ? 't' : `(${showTree(y)})`)
  return x.length === 1 ? `t ${arg(x[0])}` : `t ${arg(x[0])} ${arg(x[1])}`
}
