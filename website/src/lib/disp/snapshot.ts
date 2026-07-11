// Kernel snapshot: serialize the elaborator's per-session module cache — the
// state that makes `open use ".../kernel/prelude.disp"` a cache hit — so the
// site can ship a PRECOMPILED kernel and a fresh browser session skips the
// ~1-minute self-verification. driver.ts's resolveUse returns a cache hit
// before any elaboration or verification, so restoring this map IS the warm
// state. Generated at build time by scripts/build-kernel-snapshot.mts (which
// runs the real compiler through Vite SSR against the same virtual fs the
// browser bundles), restored by the worker on 'restore'.
//
// Sound to restore: entries keyed by path / raw / default-fills / the functor
// sentinel only. Entries whose cache key embeds a session-scoped fill intern
// id (`#n` — driver.ts fillKey) are EXCLUDED: a fresh session's intern counter
// would alias them. Excluded instantiations just re-elaborate on demand.
//
// Container (little-endian):
//   "DSNP" | u8 version | u32 json byte length | json header | node table
// Node table: nodes in children-first order, one varint tag (0=leaf, 1=stem,
// 2=fork) then a varint back-reference (index delta, >= 1) per child.
// Hash-consing makes handle identity coincide with structural identity, so
// every shared subtree is emitted exactly once — never serialize these trees
// through the ternary encoding, which re-expands sharing exponentially.

import type { Session } from '../../../../src/eval/types.ts'
import type { Tree } from '../../../../src/eval/eager.ts'
import {
  moduleCacheBySession,
  pristineTest,
  type ScopeEntry,
  type LicenseCert
} from '../../../../src/elab/state.ts'

const MAGIC = 0x504e5344 // "DSNP" LE
const VERSION = 1

// The wire shape of one module-cache entry: tree fields become node indices.
interface JEntry {
  k: string // cache key (`abs\0raw?\0fillKey`)
  t?: number // tree
  ty?: number | null // type (null = untyped is meaningful; undefined = absent)
  f?: string[] // fields
  ft?: number[] // fieldTrees
  fy?: (number | null)[] // fieldTypes
  fg?: (number | null)[] // fieldGuards
  fc?: ([number, number] | null)[] // fieldCerts as [old, payload]
  g?: number // guard
  c?: [number, number] // cert
}

interface Header {
  v: number
  hash: string // vfs content hash the snapshot was built against
  n: number // node count
  pt?: number // pristineTest root
  entries: JEntry[]
}

export class SnapshotStaleError extends Error {}

// ---- vfs content hash ------------------------------------------------------
// Shared by the builder (stamps it) and the worker (compares against its own
// bundled vfs) so "the snapshot matches the library the browser will serve"
// is one implementation.
export async function vfsHash(vfs: Map<string, string>): Promise<string> {
  const paths = [...vfs.keys()].sort()
  const enc = new TextEncoder()
  const parts: Uint8Array[] = []
  for (const p of paths) {
    parts.push(enc.encode(p), new Uint8Array([0]), enc.encode(vfs.get(p)!), new Uint8Array([0]))
  }
  let len = 0
  for (const b of parts) len += b.length
  const all = new Uint8Array(len)
  let off = 0
  for (const b of parts) {
    all.set(b, off)
    off += b.length
  }
  const digest = await globalThis.crypto.subtle.digest('SHA-256', all)
  return [...new Uint8Array(digest)].map((b) => b.toString(16).padStart(2, '0')).join('')
}

// ---- varint byte writer ----------------------------------------------------

class ByteWriter {
  buf = new Uint8Array(1 << 16)
  len = 0
  #grow(need: number): void {
    if (this.len + need <= this.buf.length) return
    let cap = this.buf.length * 2
    while (cap < this.len + need) cap *= 2
    const next = new Uint8Array(cap)
    next.set(this.buf.subarray(0, this.len))
    this.buf = next
  }
  byte(b: number): void {
    this.#grow(1)
    this.buf[this.len++] = b
  }
  varint(n: number): void {
    // LEB128 unsigned
    while (n >= 0x80) {
      this.byte((n & 0x7f) | 0x80)
      n = Math.floor(n / 128)
    }
    this.byte(n)
  }
  bytes(b: Uint8Array): void {
    this.#grow(b.length)
    this.buf.set(b, this.len)
    this.len += b.length
  }
  out(): Uint8Array {
    return this.buf.slice(0, this.len)
  }
}

class ByteReader {
  #buf: Uint8Array
  pos = 0
  constructor(buf: Uint8Array) {
    this.#buf = buf
  }
  byte(): number {
    if (this.pos >= this.#buf.length) throw new Error('snapshot: truncated')
    return this.#buf[this.pos++]
  }
  varint(): number {
    let n = 0
    let shift = 1
    for (;;) {
      const b = this.byte()
      n += (b & 0x7f) * shift
      if ((b & 0x80) === 0) return n
      shift *= 128
    }
  }
}

// ---- dump ------------------------------------------------------------------

const asTreeSession = (s: Session<number>): Session<Tree> => s as unknown as Session<Tree>
const asTree = (h: number): Tree => h as unknown as Tree
const asHandle = (t: Tree): number => t as unknown as number

// A cache key is restorable iff its fill component carries no session-scoped
// intern ids (`#n`). Plain files (""), raw ("d"/"u" per given), and the
// functor sentinel are all per-content-stable.
const restorableKey = (key: string): boolean => !(key.split('\0')[2] ?? '').includes('#')

// `mapKey` translates cache keys at the serialization boundary: the builder
// elaborates against the real filesystem (real abs paths) but ships keys in
// the browser's virtual '/lib/...' namespace. Identity when omitted.
export function dumpSnapshot(
  session: Session<number>,
  hash: string,
  mapKey: (key: string) => string = (k) => k
): Uint8Array {
  if (!session.classify) throw new Error('dumpSnapshot: session lacks classify')
  const cache = moduleCacheBySession.get(asTreeSession(session))
  if (!cache || cache.size === 0) throw new Error('dumpSnapshot: session has no module cache — elaborate the kernel first')

  const kept: [string, ScopeEntry][] = []
  for (const [key, e] of cache) {
    if (!restorableKey(key)) continue
    if (e.params) throw new Error(`dumpSnapshot: module entry ${key.split('\0')[0]} carries params (unexpected for a module record)`) // Expr payloads are not serialized
    kept.push([mapKey(key), e])
  }

  const nodes = new ByteWriter()
  const idx = new Map<number, number>() // handle -> node index
  let count = 0
  // Children-first iterative walk; hash-consing means a handle revisit is a
  // pure table hit.
  const emit = (root: number): number => {
    const have = idx.get(root)
    if (have !== undefined) return have
    const stack: number[] = [root]
    while (stack.length > 0) {
      const h = stack[stack.length - 1]
      if (idx.has(h)) {
        stack.pop()
        continue
      }
      const c = session.classify!(h)
      if (c.tag === 'leaf') {
        stack.pop()
        idx.set(h, count++)
        nodes.varint(0)
      } else if (c.tag === 'stem') {
        const ci = idx.get(c.child)
        if (ci === undefined) {
          stack.push(c.child)
        } else {
          stack.pop()
          idx.set(h, count)
          nodes.varint(1)
          nodes.varint(count - ci)
          count++
        }
      } else {
        const li = idx.get(c.left)
        const ri = idx.get(c.right)
        if (li === undefined || ri === undefined) {
          if (ri === undefined) stack.push(c.right)
          if (li === undefined) stack.push(c.left)
        } else {
          stack.pop()
          idx.set(h, count)
          nodes.varint(2)
          nodes.varint(count - li)
          nodes.varint(count - ri)
          count++
        }
      }
    }
    return idx.get(root)!
  }

  const ref = (t: Tree | null | undefined): number | null | undefined =>
    t == null ? (t as null | undefined) : emit(asHandle(t))
  const certRef = (c: LicenseCert): [number, number] => [emit(asHandle(c.old)), emit(asHandle(c.payload))]

  const entries: JEntry[] = []
  for (const [k, e] of kept) {
    const j: JEntry = { k }
    if (e.tree != null) j.t = emit(asHandle(e.tree))
    if (e.type !== undefined) j.ty = ref(e.type) as number | null
    if (e.fields) j.f = e.fields
    if (e.fieldTrees) j.ft = e.fieldTrees.map((t) => emit(asHandle(t)))
    if (e.fieldTypes) j.fy = e.fieldTypes.map((t) => (t == null ? null : emit(asHandle(t))))
    if (e.fieldGuards) j.fg = e.fieldGuards.map((t) => (t == null ? null : emit(asHandle(t))))
    if (e.fieldCerts) j.fc = e.fieldCerts.map((c) => (c == null ? null : certRef(c)))
    if (e.guard != null) j.g = emit(asHandle(e.guard))
    if (e.cert) j.c = certRef(e.cert)
    entries.push(j)
  }

  const pt = pristineTest.get(asTreeSession(session))
  const header: Header = { v: VERSION, hash, n: count, entries }
  if (pt != null) header.pt = emit(asHandle(pt))

  const json = new TextEncoder().encode(JSON.stringify(header))
  const out = new ByteWriter()
  const fixed = new Uint8Array(9)
  new DataView(fixed.buffer).setUint32(0, MAGIC, true)
  fixed[4] = VERSION
  new DataView(fixed.buffer).setUint32(5, json.length, true)
  out.bytes(fixed)
  out.bytes(json)
  out.bytes(nodes.out())
  return out.out()
}

// ---- restore ---------------------------------------------------------------

export function peekSnapshotHash(bytes: Uint8Array): string {
  return readHeader(bytes).header.hash
}

function readHeader(bytes: Uint8Array): { header: Header; nodeBytes: Uint8Array } {
  if (bytes.length < 9) throw new Error('snapshot: truncated header')
  const dv = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
  if (dv.getUint32(0, true) !== MAGIC) throw new Error('snapshot: bad magic')
  if (bytes[4] !== VERSION) throw new SnapshotStaleError(`snapshot: format v${bytes[4]}, expected v${VERSION}`)
  const jsonLen = dv.getUint32(5, true)
  const header = JSON.parse(new TextDecoder().decode(bytes.subarray(9, 9 + jsonLen))) as Header
  return { header, nodeBytes: bytes.subarray(9 + jsonLen) }
}

export interface RestoreStats {
  entries: number
  nodes: number
}

export function restoreSnapshot(
  session: Session<number>,
  bytes: Uint8Array,
  expectHash?: string,
  mapKey: (key: string) => string = (k) => k
): RestoreStats {
  const { header, nodeBytes } = readHeader(bytes)
  if (expectHash !== undefined && header.hash !== expectHash)
    throw new SnapshotStaleError(
      'snapshot: built against a different lib/ than this bundle serves (rebuild: npm run snapshot)'
    )

  const r = new ByteReader(nodeBytes)
  const handles = new Array<number>(header.n)
  for (let i = 0; i < header.n; i++) {
    const tag = r.varint()
    if (tag === 0) handles[i] = session.leaf()
    else if (tag === 1) handles[i] = session.stem(handles[i - r.varint()])
    else if (tag === 2) {
      const l = handles[i - r.varint()]
      handles[i] = session.fork(l, handles[i - r.varint()])
    } else throw new Error(`snapshot: bad node tag ${tag}`)
  }

  const at = (i: number): Tree => asTree(handles[i])
  const cert = (c: [number, number]): LicenseCert => ({ old: at(c[0]), payload: at(c[1]) })
  const map = new Map<string, ScopeEntry>()
  for (const j of header.entries) {
    const e: ScopeEntry = {}
    if (j.t !== undefined) e.tree = at(j.t)
    if (j.ty !== undefined) e.type = j.ty === null ? null : at(j.ty)
    if (j.f) e.fields = j.f
    if (j.ft) e.fieldTrees = j.ft.map(at)
    if (j.fy) e.fieldTypes = j.fy.map((i) => (i === null ? null : at(i)))
    if (j.fg) e.fieldGuards = j.fg.map((i) => (i === null ? null : at(i)))
    if (j.fc) e.fieldCerts = j.fc.map((c) => (c === null ? null : cert(c)))
    if (j.g !== undefined) e.guard = at(j.g)
    if (j.c) e.cert = cert(j.c)
    map.set(mapKey(j.k), e)
  }
  moduleCacheBySession.set(asTreeSession(session), map)
  if (header.pt !== undefined) pristineTest.set(asTreeSession(session), at(header.pt))

  // Re-register the native tree_eq fast-path: normally fired when the binding
  // elaborates, which a restore skips. Hash-consing means every entry that
  // re-exports the name holds the same handle; the first hit is THE handle.
  if (session.recognizeNative) {
    outer: for (const e of map.values()) {
      if (!e.fields || !e.fieldTrees) continue
      const i = e.fields.indexOf('tree_eq')
      if (i >= 0 && e.fieldTrees[i] != null) {
        session.recognizeNative('tree_eq', asHandle(e.fieldTrees[i]))
        break outer
      }
    }
  }

  return { entries: map.size, nodes: header.n }
}

// ---- structural differential -----------------------------------------------
// Compare a dump-session tree against its restored twin in ANOTHER session by
// parallel classify-walk (memo on handle pairs — hash-consing bounds the walk
// by unique-node count). The builder runs this over every restored entry so a
// codec bug can never ship silently.
export function structurallyEqual(
  sa: Session<number>,
  a: number,
  sb: Session<number>,
  b: number,
  seen: Set<string> = new Set()
): boolean {
  const stack: [number, number][] = [[a, b]]
  while (stack.length > 0) {
    const [ha, hb] = stack.pop()!
    const key = `${ha}:${hb}`
    if (seen.has(key)) continue
    seen.add(key)
    const ca = sa.classify!(ha)
    const cb = sb.classify!(hb)
    if (ca.tag !== cb.tag) return false
    if (ca.tag === 'stem' && cb.tag === 'stem') stack.push([ca.child, cb.child])
    else if (ca.tag === 'fork' && cb.tag === 'fork') {
      stack.push([ca.left, cb.left], [ca.right, cb.right])
    }
  }
  return true
}

// Every root pair (same iteration order on both sides) for the differential.
export function snapshotRootPairs(
  sa: Session<number>,
  sb: Session<number>
): [number, number][] {
  const ca = moduleCacheBySession.get(asTreeSession(sa))
  const cb = moduleCacheBySession.get(asTreeSession(sb))
  if (!ca || !cb) throw new Error('snapshotRootPairs: missing module cache')
  const pairs: [number, number][] = []
  for (const [k, ea] of ca) {
    if (!restorableKey(k)) continue
    const eb = cb.get(k)
    if (!eb) throw new Error(`snapshotRootPairs: restored cache lacks ${k.split('\0')[0]}`)
    const push = (x: Tree | null | undefined, y: Tree | null | undefined): void => {
      if (x == null || y == null) {
        if (x != y) throw new Error(`snapshotRootPairs: null/absence mismatch at ${k.split('\0')[0]}`)
        return
      }
      pairs.push([asHandle(x), asHandle(y)])
    }
    push(ea.tree, eb.tree)
    push(ea.type, eb.type)
    push(ea.guard, eb.guard)
    if (ea.cert || eb.cert) {
      push(ea.cert?.old, eb.cert?.old)
      push(ea.cert?.payload, eb.cert?.payload)
    }
    const zip = (
      xs: (Tree | null)[] | undefined,
      ys: (Tree | null)[] | undefined
    ): void => {
      if (!xs && !ys) return
      if (!xs || !ys || xs.length !== ys.length) throw new Error(`snapshotRootPairs: field arity mismatch at ${k.split('\0')[0]}`)
      for (let i = 0; i < xs.length; i++) push(xs[i], ys[i])
    }
    zip(ea.fieldTrees, eb.fieldTrees)
    zip(ea.fieldTypes, eb.fieldTypes)
    zip(ea.fieldGuards, eb.fieldGuards)
    if (ea.fieldCerts || eb.fieldCerts) {
      const xs = ea.fieldCerts ?? []
      const ys = eb.fieldCerts ?? []
      if (xs.length !== ys.length) throw new Error(`snapshotRootPairs: cert arity mismatch at ${k.split('\0')[0]}`)
      for (let i = 0; i < xs.length; i++) {
        push(xs[i]?.old ?? null, ys[i]?.old ?? null)
        push(xs[i]?.payload ?? null, ys[i]?.payload ?? null)
      }
    }
  }
  return pairs
}
