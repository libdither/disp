// Build the precompiled-kernel snapshot the site ships (static/kernel.snap).
// Runs under plain tsx against the real repo (see snapshot-build.ts for why
// not Vite SSR); the vfs handed to the hash is the same file set the browser
// bundle globs (lib/**/*.disp + archive/live-kernel/**/*.disp, keyed
// '/lib/...' / '/archive/...'). Always rebuilds — the snapshot depends on the
// compiler as well as the library, so there is deliberately no content-hash
// skip; the cost is the kernels genuinely self-verifying once per build
// instead of once per visitor.
import { readFileSync, writeFileSync, readdirSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { join } from 'node:path'
import { buildSnapshot } from '../src/lib/disp/snapshot-build.ts'

const repoRoot = fileURLToPath(new URL('../..', import.meta.url)).replace(/\/$/, '')

// Mirror the browser's import.meta.glob roots: every .disp file under lib/
// and archive/live-kernel/, keyed by its virtual (repo-relative) path.
const vfs = new Map<string, string>()
const walk = (dir: string): void => {
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    if (e.name.startsWith('__site_example_')) continue // validate-examples.mts temp files (excluded from the vfs glob too)
    const p = join(dir, e.name)
    if (e.isDirectory()) walk(p)
    else if (e.isFile() && e.name.endsWith('.disp'))
      vfs.set(p.slice(repoRoot.length), readFileSync(p, 'utf-8'))
  }
}
walk(join(repoRoot, 'lib'))
walk(join(repoRoot, 'archive', 'live-kernel'))
console.log(`building kernel snapshot over ${vfs.size} library files (the kernels self-verify once, ~a minute)…`)

const wasm = readFileSync(new URL('../static/rust_eager.wasm', import.meta.url))
const bytes = await buildSnapshot(wasm, repoRoot, vfs)
const out = new URL('../static/kernel.snap', import.meta.url)
writeFileSync(out, bytes)
console.log(`wrote static/kernel.snap (${(bytes.length / 1024).toFixed(0)} KiB)`)
