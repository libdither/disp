// Opens Learn in headless Chromium with ?tour-check and fails when the SKI
// walkthrough (src/lib/learn/ski-tour.ts) no longer matches the playground's
// tree viewer. Needs the website served: by default the always-on dev server.
//   npm run tour-check [-- <url of the learn page>]
import { spawn, execFileSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const site = () => {
  const out = execFileSync(new URL('../../scripts/dev-server.sh', import.meta.url).pathname, { encoding: 'utf8' })
  const m = out.match(/disp-site\s+\S+\s+(\S+)/)
  if (!m) throw new Error('no disp-site in scripts/dev-server.sh output:\n' + out)
  return m[1]
}
const url = new URL(process.argv[2] ?? site() + 'learn/')
url.searchParams.set('tour-check', '')

const profile = mkdtempSync(join(tmpdir(), 'tour-check-'))
const chrome = spawn(process.env.CHROMIUM ?? 'chromium', ['--headless=new', '--remote-debugging-port=0', `--user-data-dir=${profile}`, 'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'] })
const done = (code: number, text: string) => {
  console.log(text)
  chrome.once('exit', () => {
    rmSync(profile, { recursive: true, force: true })
    process.exit(code)
  })
  chrome.kill()
}
setTimeout(() => done(2, `timed out waiting for the walkthrough check at ${url}`), 120_000)

const wsUrl = await new Promise<string>((resolve) => {
  let err = ''
  chrome.stderr.on('data', (d) => {
    err += d
    const m = err.match(/DevTools listening on (ws:\S+)/)
    if (m) resolve(m[1])
  })
})
const ws = new WebSocket(wsUrl)
await new Promise((r) => ws.addEventListener('open', r))
let next = 0
const waiting = new Map<number, (v: any) => void>()
ws.addEventListener('message', (e) => {
  const msg = JSON.parse(String(e.data))
  waiting.get(msg.id)?.(msg.result ?? msg)
})
const send = (method: string, params: object = {}, sessionId?: string): Promise<any> =>
  new Promise((r) => {
    const id = ++next
    waiting.set(id, r)
    ws.send(JSON.stringify({ id, method, params, sessionId }))
  })

const { targetId } = await send('Target.createTarget', { url: url.href })
const { sessionId } = await send('Target.attachToTarget', { targetId, flatten: true })
for (;;) {
  await new Promise((r) => setTimeout(r, 500))
  const res = await send(
    'Runtime.evaluate',
    { expression: `(() => { const el = document.querySelector('[data-tour-check]'); return el ? el.dataset.tourCheck + '\\n' + el.innerText : '' })()`, returnByValue: true },
    sessionId
  )
  const v: string = res?.result?.value ?? ''
  if (!v) continue
  const [status, ...text] = v.split('\n')
  done(status === 'ok' ? 0 : 1, text.join('\n').trim())
}
