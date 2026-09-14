// The comparison data the site draws from research/awesome-langs/*.md,
// assembled at build time by loader.server.ts (the `virtual:awesome-langs`
// module). Levels follow the survey's rating key: 0 ✗ absent · 1 ◐ partial ·
// 2 ✅ has it · null not scored (—). `ahead` is the survey's bold: ahead of
// disp on that axis and worth stealing from (hand-curated, not derived).

export type AxisId = 'A1' | 'A2' | 'A3' | 'A4' | 'A5'
export const AXIS_IDS: AxisId[] = ['A1', 'A2', 'A3', 'A4', 'A5']

export interface Axis {
  id: AxisId
  name: string // "Reflection / programs-as-data"
  short: string // "Reflection"
  requiresHtml: string // what disp needs, from _AXES.md
  source: string // where the requirement comes from (GOALS / FOUNDATIONS refs)
  clauses: string[] // the three or four clauses 100% means, plain text
}

export interface Score {
  level: 0 | 1 | 2 | null // derived from pct when present: ✗ below 25, ◐ to 79, ✅ from 80
  pct?: number // percent of disp's requirement met, the mean of its clauses
  provisional?: boolean // a clause the write-up leaves open
  ahead: boolean // a higher pct than disp's on the axis (the master table's bold when no pct)
  raw: string // the symbol as written, with any footnote superscript
  tag?: string // how the level is reached: "quotation", "native", "SMT" …
  clauses?: string // "1 · 0 · ½†"
  whyHtml?: string // the clause-level justification
  noteHtml?: string // the per-language scorecard note
}

export interface Lang {
  slug: string // file name without .md
  name: string
  author?: string
  repoUrl?: string
  sourceUrl: string // the markdown file on GitHub
  closestHtml: string // "Closest to disp on" from the master table
  scores: Record<AxisId, Score>
  scored: boolean // false for the unscored entries (adjacent substrates, graveyard)
  differsHtml: string | null // "## Where disp differs"
  verdictHtml: string | null // "## Verdict"
}

export interface LangsData {
  surveyed: string // "August 2026"
  axes: Axis[]
  disp: Record<AxisId, Score> // the self-assessment table in _AXES.md
  langs: Lang[] // master-table order
  footnotes: Record<string, string> // superscript letter → meaning
  surveyUrl: string
}

/// What the landing page needs: the axes, disp's own row, and who the survey
/// rates ahead of disp per axis. A few kilobytes instead of every write-up.
export interface LangsSummary {
  surveyed: string
  axes: Axis[]
  disp: Record<AxisId, Score>
  ahead: Record<AxisId, { slug: string; name: string }[]> // master-table order
  count: number // projects with every axis scored
  surveyUrl: string
}

export const LEVEL_SYMBOL = ['✗', '◐', '✅'] as const
export const LEVEL_WORD = ['absent', 'partial', 'has it'] as const
