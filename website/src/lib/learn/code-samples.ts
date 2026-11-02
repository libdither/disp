// Every runnable code block on the Learn page is a region of
// lib/tests/by_example.test.disp, the file the test suite runs and the README
// copies from. A region starts at a line `//@ name`; a block shows its region and
// runs with everything above it in the file as hidden context.
//
// `import.meta.glob` is a Vite transform; under plain node it throws and the
// samples are empty, which is fine: the test suite checks the file itself.

export interface Sample {
  id: string
  context: string
  code: string
}

let text = ''
try {
  text = Object.values(
    import.meta.glob('../../../../lib/tests/by_example.test.disp', { query: '?raw', import: 'default', eager: true })
  )[0] as string
} catch {
  /* not under Vite */
}

export const samples: Record<string, Sample> = {}
const lines = text.split('\n')
let start = -1
let name = ''
const close = (end: number) => {
  if (!name) return
  samples[name] = {
    id: name,
    context: lines.slice(0, start).join('\n'),
    code: lines.slice(start + 1, end).join('\n').trim()
  }
}
lines.forEach((line, i) => {
  const m = line.match(/^\/\/@ (\S+)\s*$/)
  if (!m) return
  close(i)
  start = i
  name = m[1]
})
close(lines.length)

export const sampleList = Object.values(samples)
