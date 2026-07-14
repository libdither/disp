// Playground presets — real files in examples/ at the repo root, one per
// manifest entry below (the dropdown, in order; the FIRST entry is the
// buffer's initial doc). Their `use` paths are written for the playground's
// scratch residence (/lib/tests/playground.disp): "../kernel/…", never
// "../lib/…" — the driver-run examples in the same folder (hello.disp,
// echo.disp) use the repo-relative convention and are deliberately NOT
// listed here.
//
// `import.meta.glob` is a compile-time Vite transform and must stay a
// syntactically literal call. Under plain node (validate-examples.mts via
// tsx) it throws — caught below, `examples` then carries empty sources, and
// the validator reads the same files from disk via `manifest[].file`.
// Dev-server gotcha: ADDING a file to examples/ won't re-run a long-lived
// vite's glob until this module is touched.

export interface Example {
  id: string
  label: string
  kernel: boolean // opens the kernel (first run pays the self-verification load)
  source: string
}

// id is the stable key (localStorage choice, ?example= deep links, the
// landing card's lookup) — filenames can differ (hello-card.disp keeps the
// driver POC at examples/hello.disp untouched).
export const manifest = [
  {
    id: 'walkthrough',
    file: 'walkthrough.disp',
    label: 'Walkthrough — a type system from scratch (no kernel)',
    kernel: false
  },
  {
    id: 'walkthrough-presentation',
    file: 'walkthrough (presentation).disp',
    label: 'Walkthrough — presentation script (comments only)',
    kernel: false
  },
  { id: 'welcome', file: 'welcome.disp', label: 'Welcome tour', kernel: true },
  { id: 'trees', file: 'trees.disp', label: 'Raw tree calculus (no kernel — instant)', kernel: false },
  { id: 'typesystem', file: 'typesystem.disp', label: 'Build a type system from scratch', kernel: false },
  { id: 'records', file: 'records.disp', label: 'Records & derived fields', kernel: true },
  { id: 'proofs', file: 'proofs.disp', label: 'Proofs & hypotheses', kernel: true },
  { id: 'universe', file: 'universe.disp', label: 'Who checks the types?', kernel: true },
  { id: 'hello', file: 'hello-card.disp', label: 'Hello (landing card)', kernel: true }
]

let raw: Record<string, string> = {}
try {
  raw = import.meta.glob('../../../../examples/*.disp', {
    query: '?raw',
    import: 'default',
    eager: true
  }) as Record<string, string>
} catch {
  /* not under Vite — the validator reads examples/ from disk instead */
}
const byFile = new Map(Object.entries(raw).map(([path, text]) => [path.split('/').pop()!, text]))

export const examples: Example[] = manifest.map((m) => ({
  id: m.id,
  label: m.label,
  kernel: m.kernel,
  source: byFile.get(m.file) ?? ''
}))
