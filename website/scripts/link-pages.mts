// Every HTML page tracked in the repo outside website/, with the local files it loads, linked into
// static/repo/ at its repo path (a symlink each: live in dev, copied as files by the build), and
// static/repo/pages.json listing them for the /pages/ route.
import { execFileSync } from 'node:child_process'
import { mkdirSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { dirname, join, normalize, relative } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = fileURLToPath(new URL('../..', import.meta.url))
const out = fileURLToPath(new URL('../static/repo', import.meta.url))
const tracked = new Set(execFileSync('git', ['ls-files'], { cwd: root, encoding: 'utf8' }).split('\n').filter(Boolean))
const pages = [...tracked].filter((f) => f.endsWith('.html') && !f.startsWith('website/')).sort()

rmSync(out, { recursive: true, force: true })
const link = (file: string) => {
  const to = join(out, file)
  mkdirSync(dirname(to), { recursive: true })
  try { symlinkSync(relative(dirname(to), join(root, file)), to) } catch {} // already linked by another page
}
const list = pages.map((page) => {
  const html = readFileSync(join(root, page), 'utf8')
  link(page)
  // src="…" and href="…" naming a tracked file beside the page (no scheme, query or fragment)
  for (const [, ref] of html.matchAll(/(?:src|href)="([^"#?:]+)[^"]*"/g)) {
    const file = normalize(join(dirname(page), ref))
    if (tracked.has(file)) link(file)
  }
  const title = html.match(/<title>([^<]*)<\/title>/)?.[1].trim()
  return { path: page, title: title || page.split('/').pop() }
})
writeFileSync(join(out, 'pages.json'), JSON.stringify(list, null, 1))
console.log(`linked ${list.length} pages into static/repo`)
