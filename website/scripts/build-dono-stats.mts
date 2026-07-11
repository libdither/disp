// Refresh donation stats (src/lib/dono-stats.json) from the two funding
// sources, at build time (the Pages workflow runs this before vite build, and
// a weekly cron keeps the 30-day window honest between pushes):
//
//   XMR — a REAL view-only wallet scan via monero-ts against a public node.
//   The address and private VIEW key below are published deliberately: a view
//   key can only reveal incoming transactions, never spend, and publishing it
//   makes the donation ledger verifiable by anyone. The scan is incremental —
//   the committed JSON carries scannedToHeight + the tx list, and each run
//   resumes ~a day behind it (reorg margin), so CI never rescans the chain.
//
//   GitHub Sponsors — the aggregate GraphQL fields (sponsor count + recurring
//   monthly dollars) are readable with the default CI token. True lifetime
//   history needs the maintainer's own token (read:user); until one is wired
//   in as a secret, lifetime accrues as an ESTIMATE: recurring-monthly rate
//   integrated over the time between refreshes (labeled as such on the page).
//
// Every source fails SOFT: on error the previous committed numbers stand and
// the build proceeds — stats staleness must never break a deploy.

import { readFileSync, writeFileSync, existsSync } from 'node:fs'
import { execSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import moneroTs from 'monero-ts'

// The published donation address is a SUBADDRESS (mainnet tag 0x2a, leading
// '8'); view-only wallets root at a primary. XMR_SCAN_PRIMARY is the
// reconstruction B' = D - Hs("SubAddr\0"||a||(0,1))*G — a wallet whose
// subaddress (0,1) IS the donation address, so view-key output detection
// finds exactly the donations (and nothing else the real wallet receives
// elsewhere). The script asserts the derivation below before trusting a scan.
const XMR_DONATION_ADDRESS =
  '87ErQFwijiDCqcPs1yy8qGdaLBA2BG1BqAy93aE4pGYe2KFbZuxJDiGjjgLtDrzgxFaK98YHv7rctYfyCfptAvmHDWN8MGv'
const XMR_SCAN_PRIMARY =
  '4AdZ6xFa89H6Lk5TNQTCawVAMk9T6rYtzZxxt5fFD3k6H75DZoGZiZHXad4PQRNihs7jpfneJV6uw1X4sBcb9WrnGmaJAVj'
const XMR_VIEW_KEY = 'e6cf097d491f78f5a8443436c43b54985a0c5a0702e683f2515a57ff29942306' // view-only; published for transparency
const XMR_BIRTH_HEIGHT = 3_713_800 // address created 2026-07-10; never scan earlier
const XMR_NODES = [
  'https://xmr-node.cakewallet.com:18081',
  'https://node.sethforprivacy.com',
  'https://node.moneroworld.com:18089'
]
const GH_LOGIN = 'zyansheep'
const ATOMIC = 1e12
const OUT = fileURLToPath(new URL('../src/lib/dono-stats.json', import.meta.url))
const REPO_ROOT = fileURLToPath(new URL('../..', import.meta.url))

interface XmrTx {
  hash: string
  xmr: number
  ts: string // ISO; block time, estimated from height when the block is unavailable
  height: number
}
interface Stats {
  updatedAt: string
  xmr: { ok: boolean; scannedToHeight: number; txs: XmrTx[]; note: string }
  github: {
    ok: boolean
    sponsorCount: number
    monthlyUsd: number
    lifetimeUsdEstimate: number
    lastAccrualAt: string
    note: string
  }
  xmrUsd: number
  repo: { commits: number; firstCommitAt: string }
  totals: { last30Usd: number; lifetimeUsd: number; perCommitUsd: number; avgMonthlyUsd: number }
}

const prev: Stats | null = existsSync(OUT) ? JSON.parse(readFileSync(OUT, 'utf-8')) : null
const now = new Date()

// ---- XMR: incremental view-only scan ----------------------------------------
// public nodes hang more often than they error: deadline every long call so a
// bad node costs seconds, not a wedged CI job (the workflow adds an outer
// `timeout` as the second belt)
const deadline = <T>(p: Promise<T>, ms: number, what: string): Promise<T> =>
  Promise.race([p, new Promise<never>((_, rej) => setTimeout(() => rej(new Error(`${what} timed out after ${ms}ms`)), ms).unref())])

async function scanXmr(): Promise<Stats['xmr']> {
  const startFrom = Math.max(XMR_BIRTH_HEIGHT, (prev?.xmr.scannedToHeight ?? 0) - 720)
  let lastErr: unknown
  for (const uri of XMR_NODES) {
    let wallet: Awaited<ReturnType<typeof moneroTs.createWalletFull>> | undefined
    try {
      wallet = await deadline(
        moneroTs.createWalletFull({
          networkType: moneroTs.MoneroNetworkType.MAINNET,
          primaryAddress: XMR_SCAN_PRIMARY,
          privateViewKey: XMR_VIEW_KEY,
          restoreHeight: startFrom,
          server: { uri }
        }),
        60_000,
        `wallet create via ${uri}`
      )
      // the reconstruction proof: this wallet's subaddress (0,1) must BE the
      // published donation address, or scans would silently count nothing
      const derived = await wallet.getAddress(0, 1)
      if (derived !== XMR_DONATION_ADDRESS)
        throw new Error(`scan wallet does not derive the donation subaddress at (0,1): ${derived}`)
      await deadline(wallet.sync(), 240_000, `sync via ${uri}`)
      const tip = await wallet.getHeight()
      const transfers = (await wallet.getIncomingTransfers({ txQuery: { isConfirmed: true } })).filter(
        (tr) => tr.getAddress() === XMR_DONATION_ADDRESS
      )
      const merged = new Map<string, XmrTx>((prev?.xmr.txs ?? []).map((t) => [t.hash, t]))
      for (const tr of transfers) {
        const tx = tr.getTx()
        const height = tx.getHeight() ?? 0
        const blockTs = tx.getBlock()?.getTimestamp()
        const ts = blockTs
          ? new Date(Number(blockTs) * 1000).toISOString()
          : new Date(now.getTime() - (tip - height) * 120_000).toISOString()
        const hash = tx.getHash()
        const xmr = Number(tr.getAmount()) / ATOMIC
        const seen = merged.get(hash)
        // one tx can carry several outputs for us: sum them under the hash
        merged.set(hash, seen && seen.height === height ? { ...seen, xmr: seen.xmr + xmr } : { hash, xmr, ts, height })
      }
      await wallet.close()
      const txs = [...merged.values()].sort((a, b) => a.height - b.height)
      return { ok: true, scannedToHeight: tip, txs, note: `scanned to ${tip} via ${new URL(uri).host}` }
    } catch (e) {
      lastErr = e
      try {
        if (wallet) await deadline(wallet.close(), 10_000, 'wallet close')
      } catch {}
    }
  }
  console.warn('xmr scan failed on all nodes:', lastErr)
  return { ...(prev?.xmr ?? { scannedToHeight: 0, txs: [], note: '' }), ok: false, note: 'scan failed; previous numbers stand' }
}

// ---- GitHub Sponsors ---------------------------------------------------------
async function fetchGithub(): Promise<Stats['github']> {
  const base = prev?.github ?? {
    ok: false,
    sponsorCount: 0,
    monthlyUsd: 0,
    lifetimeUsdEstimate: 0,
    lastAccrualAt: now.toISOString(),
    note: ''
  }
  let token = process.env.SPONSORS_TOKEN || process.env.GITHUB_TOKEN || ''
  if (!token) {
    try {
      token = execSync('gh auth token', { encoding: 'utf-8' }).trim()
    } catch {}
  }
  if (!token) return { ...base, ok: false, note: 'no token; previous numbers stand' }
  try {
    const res = await fetch('https://api.github.com/graphql', {
      method: 'POST',
      headers: { Authorization: `bearer ${token}`, 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query: `query { user(login: "${GH_LOGIN}") {
          sponsors { totalCount }
          sponsorshipsAsMaintainer(first: 1, activeOnly: true) { totalRecurringMonthlyPriceInDollars }
        } }`
      })
    })
    const json = (await res.json()) as {
      data?: {
        user?: {
          sponsors: { totalCount: number }
          sponsorshipsAsMaintainer: { totalRecurringMonthlyPriceInDollars: number | null }
        }
      }
      errors?: unknown[]
    }
    const u = json.data?.user
    if (!u) throw new Error(JSON.stringify(json.errors ?? json).slice(0, 200))
    // lifetime accrues between refreshes at the PREVIOUS recurring rate
    const monthsSince = (now.getTime() - new Date(base.lastAccrualAt).getTime()) / (30.44 * 864e5)
    const accrued = base.lifetimeUsdEstimate + base.monthlyUsd * Math.max(0, monthsSince)
    return {
      ok: true,
      sponsorCount: u.sponsors.totalCount,
      monthlyUsd: u.sponsorshipsAsMaintainer.totalRecurringMonthlyPriceInDollars ?? 0,
      lifetimeUsdEstimate: Math.round(accrued * 100) / 100,
      lastAccrualAt: now.toISOString(),
      note: 'recurring pledges; lifetime is an accrued estimate'
    }
  } catch (e) {
    console.warn('github sponsors fetch failed:', e)
    return { ...base, ok: false, note: 'fetch failed; previous numbers stand' }
  }
}

// ---- price + repo ------------------------------------------------------------
async function xmrPrice(): Promise<number> {
  try {
    const r = await fetch('https://api.coingecko.com/api/v3/simple/price?ids=monero&vs_currencies=usd')
    const j = (await r.json()) as { monero?: { usd?: number } }
    if (j.monero?.usd) return j.monero.usd
  } catch {}
  return prev?.xmrUsd ?? 0
}

const commits = Number(execSync('git rev-list --count HEAD', { cwd: REPO_ROOT, encoding: 'utf-8' }).trim())
const firstCommitAt = execSync('git log --reverse --format=%aI', { cwd: REPO_ROOT, encoding: 'utf-8' })
  .split('\n')[0]
  .trim()

// ---- assemble ----------------------------------------------------------------
const [xmr, github, rate] = await Promise.all([scanXmr(), fetchGithub(), xmrPrice()])

const cutoff30 = now.getTime() - 30 * 864e5
const xmrLifetime = xmr.txs.reduce((s, t) => s + t.xmr, 0)
const xmrLast30 = xmr.txs.filter((t) => new Date(t.ts).getTime() >= cutoff30).reduce((s, t) => s + t.xmr, 0)
const lifetimeUsd = xmrLifetime * rate + github.lifetimeUsdEstimate
const last30Usd = xmrLast30 * rate + github.monthlyUsd
const projectMonths = Math.max(1, (now.getTime() - new Date(firstCommitAt).getTime()) / (30.44 * 864e5))
const round2 = (n: number) => Math.round(n * 100) / 100

const stats: Stats = {
  updatedAt: now.toISOString(),
  xmr,
  github,
  xmrUsd: rate,
  repo: { commits, firstCommitAt },
  totals: {
    last30Usd: round2(last30Usd),
    lifetimeUsd: round2(lifetimeUsd),
    perCommitUsd: round2(lifetimeUsd / commits),
    avgMonthlyUsd: round2(lifetimeUsd / projectMonths)
  }
}

writeFileSync(OUT, JSON.stringify(stats, null, 2) + '\n')
console.log(
  `dono-stats: xmr ${xmr.ok ? 'ok' : 'STALE'} (${xmr.txs.length} txs, ${xmrLifetime.toFixed(4)} XMR lifetime) · ` +
    `github ${github.ok ? 'ok' : 'STALE'} (${github.sponsorCount} sponsors, $${github.monthlyUsd}/mo) · ` +
    `$${stats.totals.lifetimeUsd} lifetime / $${stats.totals.last30Usd} last 30d / $${stats.totals.perCommitUsd} per commit (${commits})`
)
// monero-ts worker threads can hold the event loop open after a failed close;
// the stats are on disk, so end the process rather than trust the loop drains
process.exit(0)
