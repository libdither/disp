// Static disp -> HTML highlighter for non-editable code blocks (landing page,
// Learn). Token rules mirror disp-language.ts / editors/vscode-disp, including
// the rust-style call-site coloring of application heads.

const KEYWORDS = new Set(['use', 'open', 'match', 'if', 'then', 'else', 'raw', 'given'])
const DECORATORS = new Set(['let', 'test', 'guard', 'freeze', 'license_guard', 'sig', 'base'])
const CONSTANTS = new Set(['true', 'false', 't', '_'])

const esc = (s: string) =>
  s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;')

const span = (cls: string, s: string) => `<span class="hl-${cls}">${esc(s)}</span>`

export function highlightDisp(code: string): string {
  let out = ''
  let i = 0
  let exprStart = true
  const n = code.length
  while (i < n) {
    const rest = code.slice(i)
    let m: RegExpMatchArray | null

    if ((m = rest.match(/^\/\/[^\n]*/))) {
      out += span('comment', m[0])
      i += m[0].length
      continue
    }
    if ((m = rest.match(/^\/\*[\s\S]*?\*\//))) {
      out += span('comment', m[0])
      i += m[0].length
      continue
    }
    if ((m = rest.match(/^"(?:[^"\\\n]|\\.)*"/))) {
      out += span('string', m[0])
      i += m[0].length
      exprStart = false
      continue
    }
    if ((m = rest.match(/^[0-9]+/))) {
      out += span('number', m[0])
      i += m[0].length
      exprStart = false
      continue
    }
    if ((m = rest.match(/^(:=|=>|->|→)/))) {
      out += span('op', m[0])
      i += m[0].length
      exprStart = true
      continue
    }
    if ((m = rest.match(/^[=:]/))) {
      out += span('op', m[0])
      i += 1
      exprStart = true
      continue
    }
    if ((m = rest.match(/^[({[,;]/))) {
      out += span('punct', m[0])
      i += 1
      exprStart = true
      continue
    }
    if ((m = rest.match(/^[)}\]]/))) {
      out += span('punct', m[0])
      i += 1
      exprStart = false
      continue
    }
    if ((m = rest.match(/^\./))) {
      out += span('op', '.')
      i += 1
      exprStart = false
      continue
    }
    if ((m = rest.match(/^[A-Za-z_][A-Za-z0-9_']*/))) {
      const w = m[0]
      i += w.length
      if (KEYWORDS.has(w)) {
        out += span('kw', w)
        exprStart = true
        continue
      }
      if (DECORATORS.has(w) && exprStart) {
        out += span('kw', w)
        exprStart = true
        continue
      }
      if (CONSTANTS.has(w)) {
        out += span('atom', w)
        exprStart = false
        continue
      }
      if (/^[A-Z]/.test(w)) {
        out += span('type', w)
        exprStart = false
        continue
      }
      const head = exprStart && /^[ \t]+[A-Za-z0-9_("△{[]/.test(code.slice(i))
      out += head ? span('fn', w) : span('var', w)
      exprStart = false
      continue
    }
    if (code[i] === '\n') {
      out += '\n'
      i++
      exprStart = true
      continue
    }
    out += esc(code[i])
    i++
  }
  return out
}
