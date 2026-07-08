// Headless check of the browser shims: load the virtual-fs module through
// Vite (so import.meta.glob resolves exactly as in the real bundle) and
// verify the path shim + vfs agree on every module path the driver will
// compute (dirname of the playground path + relative use specifiers).
import { createServer } from 'vite'

const server = await createServer({
  configFile: new URL('../vite.config.ts', import.meta.url).pathname,
  root: new URL('..', import.meta.url).pathname,
  logLevel: 'error'
})

try {
  const fs = (await server.ssrLoadModule('/src/lib/disp/shims/fs.ts')) as {
    vfs: Map<string, string>
    readFileSync: (p: string) => string
  }
  const path = (await server.ssrLoadModule('/src/lib/disp/shims/path.ts')) as {
    resolve: (...p: string[]) => string
    dirname: (p: string) => string
  }

  console.log(`vfs entries: ${fs.vfs.size}`)
  const expected = [
    '/lib/prelude.disp',
    '/lib/kernel/prelude.disp',
    '/lib/kernel/engine.disp',
    '/lib/std/nat/ops.disp',
    '/lib/std/nat/arith.disp'
  ]
  let bad = 0
  for (const p of expected) {
    if (!fs.vfs.has(p)) {
      console.error(`MISSING: ${p}`)
      bad++
    }
  }

  // simulate the driver: playground buffer at /lib/tests/playground.disp,
  // opens resolve relative to its dirname
  const dir = path.dirname(path.resolve('/lib/tests/playground.disp'))
  const cases: [string, string][] = [
    ['../kernel/prelude.disp', '/lib/kernel/prelude.disp'],
    ['../std/nat/ops.disp', '/lib/std/nat/ops.disp'],
    ['../prelude.disp', '/lib/prelude.disp']
  ]
  for (const [rel, want] of cases) {
    const got = path.resolve(dir, rel)
    const ok = got === want && fs.vfs.has(got)
    console.log(`${ok ? 'OK  ' : 'FAIL'} ${rel} -> ${got}`)
    if (!ok) bad++
  }

  // the kernel fragments raw-open siblings + the parent prelude; simulate one
  const kdir = path.dirname('/lib/kernel/cells.disp')
  const g1 = path.resolve(kdir, '../prelude.disp')
  console.log(`${g1 === '/lib/prelude.disp' && fs.vfs.has(g1) ? 'OK  ' : 'FAIL'} kernel sibling -> ${g1}`)
  if (g1 !== '/lib/prelude.disp') bad++

  process.exit(bad ? 1 : 0)
} finally {
  await server.close()
}
