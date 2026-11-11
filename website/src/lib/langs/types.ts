// The comparison data the site draws from research/awesome-langs/*.md,
// assembled at build time by loader.server.ts (the `virtual:awesome-langs`
// module). Levels follow the survey's rating key: 0 ✗ absent · 1 ◐ partial ·
// 2 ✅ has it · null not scored (—). `ahead` is the survey's bold: a higher
// percentage than disp's theoretical row on that axis, derived by the loader.

// G for goal: the five parts of disp's own goal, in the order of the loop
export type AxisId = 'G1' | 'G2' | 'G3' | 'G4' | 'G5'
export const AXIS_IDS: AxisId[] = ['G1', 'G2', 'G3', 'G4', 'G5']

export interface Axis {
  id: AxisId
  name: string // "Substrate"
  short: string // the name up to its first " / ", " + ", "(" or ":"
  requiresHtml: string // what disp needs, from _AXES.md
  source: string // where the requirement comes from (GOALS / FOUNDATIONS refs)
  clauses: string[] // the three or four clauses 100% means, plain text
}

export interface Score {
  level: 0 | 1 | 2 | null // derived from pct when present: ✗ below 25, ◐ to 79, ✅ from 80
  pct?: number // percent of disp's requirement met, the weighted mean of its clauses
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
  scored: boolean // false for the unscored entries (graveyard)
  differsHtml: string | null // "## Where disp differs"
  verdictHtml: string | null // "## Verdict"
}

export interface LangsData {
  surveyed: string // "November 2026"
  axes: Axis[]
  disp: Record<AxisId, Score> // what the design could reach: "Where disp could get, in theory" in _AXES.md
  dispToday: Record<AxisId, Score> // what the repo has: "Where disp is today"
  dispCommit: string // the commit dispToday was checked at
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
  dispToday: Record<AxisId, Score>
  dispCommit: string
  ahead: Record<AxisId, { slug: string; name: string }[]> // master-table order
  count: number // projects with every axis scored
  surveyUrl: string
}

export const LEVEL_SYMBOL = ['✗', '◐', '✅'] as const
export const LEVEL_WORD = ['absent', 'partial', 'has it'] as const
