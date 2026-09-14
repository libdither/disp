// Build-time loader for the comparison page: reads research/awesome-langs
// (the master table, the axes file with disp's self-assessment, and each
// language write-up) and serves the result as the `virtual:awesome-langs`
// module; `virtual:awesome-langs/summary` is the landing page's small cut.
// Node-only (the .server suffix keeps it out of client code); vite dev
// reloads the page when any of the markdown files change.
import { readFileSync, readdirSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { Marked } from 'marked'
import type { Plugin } from 'vite'
import { AXIS_IDS, type Axis, type AxisId, type Lang, type LangsData, type LangsSummary, type Score } from './types.ts'

const DIR = resolve(fileURLToPath(new URL('../../../../research/awesome-langs/', import.meta.url)))
const GH = 'https://github.com/libdither/disp/blob/main/research/awesome-langs/'
const VIRTUAL = 'virtual:awesome-langs'
const SUMMARY = VIRTUAL + '/summary'
const RESOLVED = '\0' + VIRTUAL
const RESOLVED_SUMMARY = '\0' + SUMMARY

// links open in a new tab; relative "foo.md" links point at the file on GitHub
const md = new Marked({
  renderer: {
    link({ href, title, tokens }) {
      const text = this.parser.parseInline(tokens)
      const url = /^https?:/.test(href) ? href : /\.md(#|$)/.test(href) ? GH + href : href
      const t = title ? ` title="${title}"` : ''
      return `<a href="${url}"${t} target="_blank" rel="noopener">${text}</a>`
    }
  }
})
const html = (s: string) => md.parse(s.trim(), { async: false })
const inline = (s: string) => md.parseInline(s.trim(), { async: false })

const read = (file: string) => readFileSync(join(DIR, file), 'utf-8')
const cells = (row: string) => row.split('|').slice(1, -1).map((c) => c.trim())

const levelOf = (pct: number): 0 | 1 | 2 => (pct < 25 ? 0 : pct < 80 ? 1 : 2)

/// One table cell: `◐ 50% (tag)` in a scorecard, `**◐ 50%**ᶠ` in the master
/// table (bold = ahead of disp, superscripts are footnotes). The level derives
/// from the percentage when there is one, else from the symbol.
function parseCell(cell: string): Score {
  const ahead = /\*\*.*\*\*/.test(cell)
  let raw = cell.replaceAll('**', '').trim()
  const tm = raw.match(/\(([^)]+)\)\s*$/)
  const tag = tm?.[1].trim()
  if (tm) raw = raw.slice(0, tm.index).trim()
  const pm = raw.match(/(\d{1,3})%(\?)?/)
  const pct = pm ? Number(pm[1]) : undefined
  const provisional = !!pm?.[2]
  if (pm) raw = (raw.slice(0, pm.index) + raw.slice(pm.index! + pm[0].length)).replace(/\s+/g, '')
  const sym = [...raw].find((c) => c === '✅' || c === '◐' || c === '✗')
  const level = pct != null ? levelOf(pct) : sym === '✅' ? 2 : sym === '◐' ? 1 : sym === '✗' ? 0 : null
  return { level, pct, provisional, ahead, raw, tag }
}

/// Rows shaped `| A1 Reflection | ◐ 50% (tag) | note | 1 · 0 · ½ — why |`
/// (per-language scorecards and disp's own table share the shape).
function parseScorecard(text: string): Partial<Record<AxisId, Score>> {
  const out: Partial<Record<AxisId, Score>> = {}
  for (const line of text.split('\n')) {
    const m = line.match(/^\| (A[1-6]) /)
    if (!m) continue
    const [, score, note, clauses] = cells(line)
    const id = m[1] as AxisId
    const [vals, why] = (clauses ?? '').split(' — ')
    out[id] = {
      ...parseCell(score ?? ''),
      noteHtml: note ? inline(note) : undefined,
      clauses: vals?.trim() || undefined,
      whyHtml: why ? inline(why) : undefined
    }
  }
  return out
}

/// `## Title` sections → body text (### subsections stay inside their parent).
function sections(text: string): Map<string, string> {
  const out = new Map<string, string>()
  for (const part of text.split(/^## /m).slice(1)) {
    const nl = part.indexOf('\n')
    out.set(part.slice(0, nl).trim(), part.slice(nl + 1).replace(/\n---\s*$/, '').trim())
  }
  return out
}

function parseAxes(text: string): { axes: Axis[]; disp: Record<AxisId, Score> } {
  const axes: Axis[] = []
  // two tables start their rows with `| **A1** |`: the axes (bold name, four
  // columns) and the grading clauses (three plain columns)
  for (const line of text.split('\n')) {
    const m = line.match(/^\| \*\*(A[1-6])\*\* /)
    if (!m) continue
    const id = m[1] as AxisId
    const row = cells(line).slice(1)
    if (row[0]?.startsWith('**')) {
      const [name, requires, source] = row
      const clean = name.replaceAll('**', '')
      axes.push({ id, name: clean, short: clean.split(/ \/ | \+ | \(|:/)[0].trim(), requiresHtml: inline(requires), source, clauses: [] })
    } else {
      const ax = axes.find((a) => a.id === id)
      if (ax) ax.clauses = row.slice(0, 3)
    }
  }
  const stands = sections(text).get('Where disp stands') ?? ''
  const disp = parseScorecard(stands) as Record<AxisId, Score>
  for (const id of AXIS_IDS) if (!disp[id]) throw new Error(`_AXES.md: "Where disp stands" lacks ${id}`)
  return { axes, disp }
}

function parseLangFile(file: string): Pick<Lang, 'name' | 'author' | 'repoUrl' | 'differsHtml' | 'verdictHtml'> & {
  notes: Partial<Record<AxisId, Score>>
} {
  const text = read(file)
  const title = text.match(/^# (.+)$/m)?.[1] ?? file
  const [name, ...rest] = title.split(' — ')
  const secs = sections(text)
  const differs = secs.get('Where disp differs')
  const verdict = secs.get('Verdict')
  return {
    name: name.trim().replace(/\\([*_`])/g, '$1'),
    author: rest.length ? rest.join(' — ').trim() : undefined,
    repoUrl: text.match(/^\*\*Repos?:\*\*.*?(https?:\/\/\S+)/m)?.[1],
    differsHtml: differs ? html(differs) : null,
    verdictHtml: verdict ? html(verdict) : null,
    notes: parseScorecard(secs.get('Scorecard') ?? secs.get('Scorecard (cluster-level)') ?? '')
  }
}

export function loadLangs(): LangsData {
  const master = read('AWESOME-LANGS.md')
  const { axes, disp } = parseAxes(read('_AXES.md'))
  const langs: Lang[] = []
  const footnotes: Record<string, string> = {}
  for (const line of master.split('\n')) {
    const m = line.match(/^\| \[\*\*(.+?)\*\*(.*?)\]\((.+?\.md)\) \|/)
    if (m) {
      const [, ...rest] = cells(line) // name, six scores, closest
      const file = m[3]
      const detail = parseLangFile(file)
      const scores = {} as Record<AxisId, Score>
      // the master table carries symbol and footnotes; the write-up's
      // scorecard carries the percentage, how-tag, clauses and note; "ahead"
      // is derived from the percentages when both sides have one
      AXIS_IDS.forEach((id, i) => {
        const master = parseCell(rest[i] ?? '')
        const note = detail.notes[id]
        const pct = note?.pct ?? master.pct
        const dispPct = disp[id].pct
        scores[id] = {
          ...master,
          ...(note ? { tag: note.tag, clauses: note.clauses, whyHtml: note.whyHtml, noteHtml: note.noteHtml, provisional: note.provisional } : {}),
          pct,
          level: pct != null ? levelOf(pct) : master.level,
          ahead: pct != null && dispPct != null ? pct > dispPct : master.ahead
        }
      })
      langs.push({
        slug: file.replace(/\.md$/, ''),
        name: (m[1] + m[2]).trim().replace(/\\([*_`])/g, '$1'),
        author: detail.author,
        repoUrl: detail.repoUrl,
        sourceUrl: GH + file,
        closestHtml: inline(rest[AXIS_IDS.length] ?? ''),
        scores,
        scored: AXIS_IDS.some((id) => scores[id].level != null),
        differsHtml: detail.differsHtml,
        verdictHtml: detail.verdictHtml
      })
      continue
    }
    // the footnote legend under the table: `ᵈ designed, not built · ᶜ …`
    if (/^[ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ] /u.test(line)) {
      for (const part of line.split(' · ')) footnotes[part[0]] = part.slice(2).trim()
    }
  }
  if (!langs.length) throw new Error('AWESOME-LANGS.md: no master-table rows found')
  return {
    surveyed: master.match(/goals, ([^.]+)\./)?.[1] ?? '',
    axes,
    disp,
    langs,
    footnotes,
    surveyUrl: GH + 'AWESOME-LANGS.md'
  }
}

export function summarize(d: LangsData): LangsSummary {
  const ahead = {} as LangsSummary['ahead']
  for (const id of AXIS_IDS) {
    ahead[id] = d.langs.filter((l) => l.scores[id].ahead).map((l) => ({ slug: l.slug, name: l.name }))
  }
  return {
    surveyed: d.surveyed,
    axes: d.axes,
    disp: d.disp,
    ahead,
    count: d.langs.filter((l) => AXIS_IDS.every((id) => l.scores[id].level != null)).length,
    surveyUrl: d.surveyUrl
  }
}

export function awesomeLangs(): Plugin {
  // both modules come from one parse; a markdown edit drops the memo
  let cache: LangsData | null = null
  const data = () => (cache ??= loadLangs())
  return {
    name: 'awesome-langs',
    resolveId(id) {
      if (id === VIRTUAL) return RESOLVED
      if (id === SUMMARY) return RESOLVED_SUMMARY
    },
    load(id) {
      if (id !== RESOLVED && id !== RESOLVED_SUMMARY) return
      for (const f of readdirSync(DIR)) this.addWatchFile(join(DIR, f))
      return `export default ${JSON.stringify(id === RESOLVED ? data() : summarize(data()))}`
    },
    configureServer(server) {
      server.watcher.add(DIR)
      server.watcher.on('change', (file) => {
        if (!resolve(file).startsWith(DIR)) return
        cache = null
        for (const rid of [RESOLVED, RESOLVED_SUMMARY]) {
          const mod = server.moduleGraph.getModuleById(rid)
          if (mod) server.moduleGraph.invalidateModule(mod)
        }
        server.ws.send({ type: 'full-reload' })
      })
    }
  }
}
