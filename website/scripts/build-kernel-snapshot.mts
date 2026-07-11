// Build the precompiled-kernel snapshot the site ships (static/kernel.snap).
// Runs under plain tsx against the real lib/ (see snapshot-build.ts for why
// not Vite SSR); the vfs handed to the hash is the same file set the browser
// bundle globs (lib/**/*.disp, keyed '/lib/...'). Always rebuilds — the
// snapshot depends on the compiler as well as lib/, so there is deliberately
// no content-hash skip; the ~1-minute cost is the kernel genuinely
// self-verifying once per build instead of once per visitor.
import { readFileSync, writeFileSync, readdirSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { join } from 'node:path'
import { buildSnapshot } from '../src/lib/disp/snapshot-build.ts'

const libRoot = fileURLToPath(new URL('../../lib', import.meta.url))

// Mirror the browser's `import.meta.glob('../../../../../lib/**/*.disp')`:
// every .disp file under lib/, keyed by its virtual path.
const vfs = new Map<string, string>()
const walk = (dir: string): void => {
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    if (e.name.startsWith('__site_example_')) continue // validate-examples.mts temp files (excluded from the vfs glob too)
    const p = join(dir, e.name)
    if (e.isDirectory()) walk(p)
    else if (e.isFile() && e.name.endsWith('.disp'))
      vfs.set('/lib' + p.slice(libRoot.length), readFileSync(p, 'utf-8'))
  }
}
walk(libRoot)
console.log(`building kernel snapshot over ${vfs.size} lib files (the kernel self-verifies once, ~a minute)…`)

const wasm = readFileSync(new URL('../static/rust_eager.wasm', import.meta.url))
const bytes = await buildSnapshot(wasm, libRoot, vfs)
const out = new URL('../static/kernel.snap', import.meta.url)
writeFileSync(out, bytes)
console.log(`wrote static/kernel.snap (${(bytes.length / 1024).toFixed(0)} KiB)`)
