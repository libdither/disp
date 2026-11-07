// Filmstrip of the playground's tree viewer: for each step, shots taken during its animation,
// tiled one row per step. Runs against the dev server (scripts/dev-server.sh).
// usage: node scripts/playground-film.mjs <name> <line-regex> [steps] [times ms, comma] [program file] [parallel|dark|play]
// play: presses Play and takes <steps> shots, one every <first time> ms, tiled into one grid
// env: FILM_OUT (default /tmp/playground-film), PLAYGROUND_URL, CHROMIUM (headless shell binary)
import { chromium } from 'playwright-core'
import { readFileSync, mkdirSync, readdirSync } from 'node:fs'
import { execFileSync } from 'node:child_process'

const [name, lineRe, stepsArg, timesArg, progFile, mode] = process.argv.slice(2)
if (!name || !lineRe) {
  console.error('usage: node scripts/playground-film.mjs <name> <line-regex> [steps] [times] [program] [parallel|dark|play]')
  process.exit(1)
}
const out = process.env.FILM_OUT ?? '/tmp/playground-film'
const url = process.env.PLAYGROUND_URL ?? 'http://functor.ts.zyancraft.net:5174/playground/'
const steps = Number(stepsArg ?? 6)
const times = (timesArg ?? '0,120,240,380,620').split(',').map(Number)
const prog = progFile ? readFileSync(progFile, 'utf8') : 'open use raw "../prelude.disp" {}\nswap := {f, x, y} => f(y, x)\n'
mkdirSync(out, { recursive: true })

const shells = `${process.env.HOME}/.cache/ms-playwright`
const shell = process.env.CHROMIUM ?? (() => {
  const dir = readdirSync(shells).filter((d) => d.startsWith('chromium_headless_shell')).sort().pop()
  return `${shells}/${dir}/chrome-headless-shell-linux64/chrome-headless-shell`
})()
const browser = await chromium.launch({ executablePath: shell, args: ['--no-sandbox'] })
const theme = mode === 'dark' ? 'dark' : 'light'
const ctx = await browser.newContext({ viewport: { width: 1500, height: 1000 }, colorScheme: theme })
await ctx.addInitScript((t) => {
  localStorage.setItem('disp-playground-welcomed', '1')
  localStorage.setItem('disp-theme', t)
}, theme)
const page = await ctx.newPage()
const errors = []
page.on('pageerror', (e) => errors.push(e.message))
page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()) })
await page.goto(url, { waitUntil: 'load' })
await page.waitForFunction(() => document.querySelector('.status')?.textContent.match(/Checked|Ready/), null, { timeout: 60000 })
await page.locator('.cm-content').click()
await page.keyboard.press('Control+A')
await page.keyboard.insertText(prog)
await page.waitForTimeout(1800)
// the editor only draws the lines on screen, so walk the cursor down to the line instead of clicking it
const lineNo = prog.split('\n').findIndex((l) => new RegExp(lineRe).test(l))
if (lineNo < 0) throw new Error(`no line matches ${lineRe}`)
await page.evaluate((n) => {
  const view = document.querySelector('.cm-content').cmTile.root.view
  const line = view.state.doc.line(n + 1)
  view.focus()
  view.dispatch({ selection: { anchor: line.to }, scrollIntoView: true })
}, lineNo)
await page.waitForTimeout(1800)
if (mode === 'parallel') {
  await page.locator('.optbtn').click()
  await page.locator('label', { hasText: 'Fire all at once' }).locator('input').check()
  await page.waitForTimeout(1200)
  await page.locator('.optbtn').click()
}
const stage = page.locator('.stagewrap')
if (mode === 'play') {
  await page.locator('.tb.play').click()
  const shots = []
  for (let i = 0; i < steps; i++) {
    await page.waitForTimeout(times[0] || 400)
    shots.push(`${out}/${name}-play-${i}.png`)
    await stage.screenshot({ path: shots[i] })
  }
  execFileSync('montage', [...shots, '-tile', '8x', '-geometry', '300x+3+3', '-background', '#888', `${out}/${name}-play.png`])
  console.log(`grid: ${out}/${name}-play.png`)
  console.log('errors', errors)
  await browser.close()
  process.exit(0)
}
await page.locator('.insp').focus()
const caption = async () => (await page.locator('.now').first().innerText().catch(() => '')).replace(/\s+/g, ' ').slice(0, 150)
const rows = [`0: ${await caption()}`]
await stage.screenshot({ path: `${out}/${name}-0-start.png` })
const files = []
for (let s = 1; s <= steps; s++) {
  await page.keyboard.press('ArrowRight')
  const t0 = Date.now()
  const row = []
  for (const t of times) {
    const wait = t - (Date.now() - t0)
    if (wait > 0) await page.waitForTimeout(wait)
    const f = `${out}/${name}-${s}-${t}.png`
    await stage.screenshot({ path: f })
    row.push(f)
  }
  files.push(row)
  rows.push(`${s}: ${await caption()}`)
  await page.waitForTimeout(250)
}
for (const [i, row] of files.entries())
  execFileSync('montage', [...row, '-tile', `${row.length}x1`, '-geometry', '420x+4+4', '-background', '#888', `${out}/${name}-row${i + 1}.png`])
console.log(rows.join('\n'))
console.log(`rows: ${out}/${name}-row<N>.png`)
console.log('errors', errors)
await browser.close()
