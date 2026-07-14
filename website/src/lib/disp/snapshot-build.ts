// Node-side kernel snapshot builder, driven by scripts/build-kernel-snapshot.mts
// under plain tsx (the same way validate-examples.mts runs the compiler).
// Vite SSR can't apply the node:fs->vfs shim (builtins are externalized before
// plugin resolveId), so the builder elaborates against the REAL lib/ files and
// translates cache keys into the browser's virtual '/lib/...' namespace at the
// dump boundary — node and the path shim resolve posix paths identically, and
// the vfs hash is computed over the same file set the browser's
// import.meta.glob bundles.
//
// After dumping it proves the round-trip before anything ships: restore into a
// fresh session, structurally diff every restored tree against its original,
// and run the landing-card example asserting it passes without re-elaborating
// any module.

import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { parseProgram } from '../../../../src/compile.ts'
import type { Session } from '../../../../src/eval/types.ts'
import type { Tree } from '../../../../src/eval/eager.ts'
import { RustEagerBrowserSession } from './rust-eager-browser.ts'
import { manifest } from './examples.ts'
import {
  dumpSnapshot,
  restoreSnapshot,
  vfsHash,
  snapshotRootPairs,
  structurallyEqual
} from './snapshot.ts'

// One parseProgram call per module (separate root programs sidestep any
// open-collision semantics between them; the session's module cache is what
// accumulates). The kernel barrel is the ~1-minute self-verification; the
// rest cache-hit their way through in seconds.
const WARMUP = [
  'open use "../kernel/prelude.disp"\n',
  'open use "../std/nat.disp"\n',
  'open use raw "../prelude.disp" {}\n'
]

const asTree = (s: RustEagerBrowserSession): Session<Tree> => s as unknown as Session<Tree>

// libRoot: real absolute path of the repo's lib/ directory (no trailing slash).
// vfs: path -> contents in the browser namespace ('/lib/...'), for the hash.
export async function buildSnapshot(
  wasmBytes: Uint8Array,
  libRoot: string,
  vfs: Map<string, string>
): Promise<Uint8Array> {
  const rootPath = `${libRoot}/tests/playground.disp`
  const toVirtual = (key: string): string => {
    const [abs, ...rest] = key.split('\0')
    if (!abs.startsWith(libRoot + '/')) throw new Error(`snapshot: cache key outside lib/: ${abs}`)
    return ['/lib' + abs.slice(libRoot.length), ...rest].join('\0')
  }
  const toReal = (key: string): string => {
    const [abs, ...rest] = key.split('\0')
    if (!abs.startsWith('/lib/')) throw new Error(`snapshot: unexpected virtual key ${abs}`)
    return [libRoot + abs.slice(4), ...rest].join('\0')
  }

  const t0 = performance.now()
  const sa = new RustEagerBrowserSession(wasmBytes)
  let items = 0
  for (const src of WARMUP) {
    const t = performance.now()
    parseProgram(src, rootPath, {
      session: asTree(sa),
      onItem: () => {
        items++
        if (items % 100 === 0) process.stdout.write(`\r  elaborated ${items} items…`)
      }
    })
    process.stdout.write(`\r  ${src.trim()}  (${((performance.now() - t) / 1000).toFixed(1)}s, ${items} items total)\n`)
  }

  const hash = await vfsHash(vfs)
  const tDump = performance.now()
  const bytes = dumpSnapshot(sa, hash, toVirtual)
  console.log(`  dumped ${(bytes.length / 1024).toFixed(0)} KiB in ${(performance.now() - tDump).toFixed(0)}ms (vfs ${hash.slice(0, 12)}…)`)

  // ---- round-trip proof ----
  // Restore under REAL keys (toReal) so the landing-example run below — which
  // elaborates against the real filesystem — hits the restored cache the same
  // way the browser (virtual keys, virtual fs) will.
  const sb = new RustEagerBrowserSession(wasmBytes)
  const tRestore = performance.now()
  const stats = restoreSnapshot(sb, bytes, hash, toReal)
  console.log(`  restored ${stats.entries} modules / ${stats.nodes} nodes in ${(performance.now() - tRestore).toFixed(0)}ms`)

  const pairs = snapshotRootPairs(sa, sb)
  const seen = new Set<string>()
  for (const [a, b] of pairs) {
    if (!structurallyEqual(sa, a, sb, b, seen)) {
      throw new Error('snapshot self-check: restored tree differs structurally from the original')
    }
  }
  console.log(`  differential: ${pairs.length} roots structurally identical`)

  // Example sources are files in repo examples/ (the glob in examples.ts is
  // Vite-only and inert here) — read the landing card's from disk.
  const hero = manifest.find((m) => m.id === 'hello')
  if (!hero) throw new Error('snapshot self-check: landing example "hello" missing from the examples manifest')
  const heroSource = readFileSync(
    fileURLToPath(new URL(`../../../../examples/${hero.file}`, import.meta.url)),
    'utf-8'
  )
  let moduleItems = 0
  const tHero = performance.now()
  const decls = parseProgram(heroSource, rootPath, {
    session: asTree(sb),
    onItem: (i) => {
      if (i.depth > 0) moduleItems++
    }
  })
  let tests = 0
  for (const d of decls) {
    if (d.kind === 'Def') continue
    tests++
    const lhs = d.lhs as unknown as number
    const rhs = d.rhs as unknown as number
    if (!sb.equal(lhs, rhs)) throw new Error(`snapshot self-check: landing example test ${tests} fails after restore`)
  }
  const heroMs = performance.now() - tHero
  if (tests === 0) throw new Error('snapshot self-check: landing example has no tests')
  if (moduleItems > 50)
    throw new Error(`snapshot self-check: landing example re-elaborated ${moduleItems} module items — the cache did not hit`)
  console.log(`  landing example: ${tests} tests pass in ${(heroMs / 1000).toFixed(2)}s (${moduleItems} module items re-elaborated)`)

  sa.dispose()
  sb.dispose()
  console.log(`  total ${(Math.round(performance.now() - t0) / 1000).toFixed(1)}s`)
  return bytes
}
