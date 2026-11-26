// shots of the dial mock (served by scripts/dev-server.sh on :5180): node archive/playground-dial/shoot.mjs <example> <out-prefix> <actions json>
// actions: ["fwd", "back", ["level", def, n], ["all", d], ["hoverNode", text], ["film", times...], ["filmAs", label, times...],
//   ["shot", name], ["wait", ms], "caption", "side" (prints each definition's setting and reason),
//   "walk" (prints every beat's caption under the current settings, without playing anything)]
// env FILM_W: width of each film frame in the montage (default 520)
import { createRequire } from 'node:module'
import { readdirSync } from 'node:fs'
const require = createRequire(new URL('../../website/package.json', import.meta.url))
const { chromium } = require('playwright-core')
const [exKey, out, actsJson] = process.argv.slice(2)
const acts = JSON.parse(actsJson)
const shells = `${process.env.HOME}/.cache/ms-playwright`
const dir = readdirSync(shells).filter((d) => d.startsWith('chromium_headless_shell')).sort().pop()
const browser = await chromium.launch({ executablePath: `${shells}/${dir}/chrome-headless-shell-linux64/chrome-headless-shell`, args: ['--no-sandbox'] })
const page = await browser.newPage({ viewport: { width: 1400, height: 820 } })
const errors = []
page.on('pageerror', (e) => errors.push(e.message))
page.on('console', (m) => m.type() === 'error' && errors.push(m.text()))
await page.goto(`${process.env.DISP_PAGES_URL ?? 'http://localhost:5180'}/archive/playground-dial/index.html?ex=${exKey}&t=${Date.now()}`)
await page.waitForTimeout(500)
let n = 0
for (const a of acts) {
  const [op, ...args] = Array.isArray(a) ? a : [a]
  if (op === 'fwd') { await page.evaluate(() => window.__dial.forward()); await page.waitForTimeout(args[0] ?? 2600) }
  else if (op === 'back') { await page.evaluate(() => window.__dial.back()); await page.waitForTimeout(args[0] ?? 1800) }
  else if (op === 'level') { await page.evaluate(([d, l]) => window.__dial.setLevel(d, l), args); await page.waitForTimeout(700) }
  else if (op === 'all') { await page.evaluate((d) => window.__dial.setLevel(null, d, true), args[0]); await page.waitForTimeout(700) }
  else if (op === 'hoverNode') {
    const box = await page.locator(`.node`).filter({ hasText: args[0] }).first().boundingBox()
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2); await page.waitForTimeout(300)
  }
  else if (op === 'wait') await page.waitForTimeout(args[0])
  else if (op === 'caption') console.log((await page.locator('#caption').innerText()).replace(/\s+/g, ' '), '|', await page.locator('#count').innerText())
  else if (op === 'shot') await page.screenshot({ path: `${out}-${args[0] ?? n++}.png`, clip: args[1] })
  else if (op === 'walk') console.log((await page.evaluate(() => window.__dial.walk())).map((c, i) => `${i + 1}. ${c}`).join('\n'))
  else if (op === 'side') console.log((await page.locator('#side .def').allInnerTexts()).map((t) => t.replace(/\s+/g, ' ')).join('\n'))
  else if (op === 'film' || op === 'filmAs') {
    const label = op === 'filmAs' ? args.shift() : ''
    const times = args
    await page.evaluate(() => window.__dial.forward())
    const t0 = Date.now(); const files = []
    for (const t of times) { const w = t - (Date.now() - t0); if (w > 0) await page.waitForTimeout(w); const f = `${out}${label}-f${t}.png`; await page.locator('#stage').screenshot({ path: f }); files.push(f) }
    await page.waitForTimeout(600)
    const { execFileSync } = await import('node:child_process')
    execFileSync('montage', [...files, '-tile', `${files.length}x1`, '-geometry', `${process.env.FILM_W ?? 520}x+3+3`, '-background', '#999', `${out}${label}-film.png`])
    console.log('film', `${out}${label}-film.png`)
  }
}
console.log('errors', errors)
await browser.close()
