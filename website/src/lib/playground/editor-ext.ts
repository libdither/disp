// The playground's editor smarts, all synchronous against an index of the
// buffer so typing never waits on the compiler: completion, signature
// help, folding, cost bars, and the links between the code and the tree view.

import {
  EditorView,
  ViewPlugin,
  Decoration,
  WidgetType,
  gutter,
  GutterMarker,
  keymap,
  showTooltip,
  hoverTooltip,
  rectangularSelection,
  crosshairCursor,
  dropCursor,
  highlightSpecialChars,
  type DecorationSet,
  type Tooltip,
  type ViewUpdate
} from '@codemirror/view'
import { EditorState, Prec, RangeSetBuilder, StateEffect, StateField, type Extension, type Text } from '@codemirror/state'
import {
  autocompletion,
  closeBracketsKeymap,
  completionKeymap,
  snippetCompletion,
  type Completion,
  type CompletionContext,
  type CompletionResult
} from '@codemirror/autocomplete'
import { codeFolding, foldGutter, foldKeymap, foldService, indentOnInput } from '@codemirror/language'
import { highlightSelectionMatches } from '@codemirror/search'
import type { ScopeItem } from '$lib/disp/protocol'

export interface BufferDef {
  name: string
  line: number
  endLine: number
  sig?: string
  params?: string[]
  doc?: string
  decor?: string
  /// the record this accessor reads a field of
  of?: string
}
export interface BufferTest {
  line: number
  endLine: number
  text: string
}
export interface BufferIndex {
  defs: BufferDef[]
  tests: BufferTest[]
}

const DEF_RE = /^(?:(let|guard\(.*?\)|sig|base)\s+)?([A-Za-z_][\w']*)\s*(?::(?!=)\s*(.*?)\s*)?:=>?\s*(.*)$/
const IGNORE_DEF = new Set(['open', 'use', 'test', 'if', 'then', 'else', 'match', 'given'])

function paramsFrom(rest: string): string[] | undefined {
  const fixed = rest.match(/^fix\s*\(\s*\{([^}]*)\}/)
  if (fixed) {
    const ps = fixed[1].split(',').map((p) => p.split(':')[0].trim()).filter(Boolean)
    return ps.length > 1 ? ps.slice(1) : undefined
  }
  const out: string[] = []
  let r = rest
  for (;;) {
    const m = r.match(/^\{([^}]*)\}\s*(?:=>|->)\s*/)
    if (!m) break
    out.push(...m[1].split(',').map((p) => p.split(':')[0].trim()).filter(Boolean))
    r = r.slice(m[0].length)
  }
  return out.length ? out : undefined
}

/// Split at the commas that aren't nested in brackets.
function splitTop(s: string): string[] {
  const out: string[] = []
  let depth = 0
  let from = 0
  for (let i = 0; i < s.length; i++) {
    const c = s[i]
    if (c === '(' || c === '{' || c === '[') depth++
    else if (c === ')' || c === '}' || c === ']') depth--
    else if (c === ',' && depth === 0) {
      out.push(s.slice(from, i))
      from = i + 1
    }
  }
  out.push(s.slice(from))
  return out.map((x) => x.trim()).filter(Boolean)
}

/// `record NAME :: {f : T, …} -> R` declares a constructor and one accessor per field; the annotation may run over several lines.
function recordDefs(doc: Text, n: number): BufferDef[] | null {
  const head = /^record\s+([A-Za-z_][\w']*)\s*::?\s*/.exec(doc.line(n).text)
  if (!head) return null
  let text = doc.line(n).text.slice(head[0].length)
  for (let k = n + 1; k <= doc.lines && k < n + 40; k++) {
    let depth = 0
    for (const c of text.replace(/\/\/.*$/gm, '')) depth += c === '{' ? 1 : c === '}' ? -1 : 0
    if (depth <= 0) break
    text += ' ' + doc.line(k).text.trim()
  }
  const m = /^\{(.*)\}\s*(?:->|→)\s*(.*?)\s*(?:\/\/.*)?$/.exec(text.replace(/\s+/g, ' ').trim())
  if (!m) return null
  // `f`, `f : T`, or with a default, `f : T := d`
  const fields = splitTop(m[1]).map((f) => {
    const p = /^([^:]*?)\s*(?::(?!=)\s*(.*?))?\s*(?::=\s*(.*))?$/.exec(f)!
    return { name: p[1], type: p[2] || 'Tree', dflt: p[3] as string | undefined }
  })
  if (fields.length === 0 || fields.some((f) => !/^[A-Za-z_][\w']*$/.test(f.name))) return null
  const name = head[1]
  // accessors are named after the record's type (`Knob.level`), so that's what `Knob.` completes
  const owner = /^[A-Za-z_][\w']*$/.test(m[2]) ? m[2] : name
  const ctor: BufferDef = { name, line: n, endLine: n, sig: `{${m[1].trim()}} -> ${m[2]}`, params: fields.map((f) => f.name), decor: 'record' }
  return [
    ctor,
    ...fields.map((f) => ({
      name: f.name,
      line: n,
      endLine: n,
      sig: `${m[2]} -> ${f.type}`,
      params: ['r'],
      doc: `The ${f.name} field of a ${owner}${f.dflt ? ` (${f.dflt} unless given)` : ''}; read it as x.${f.name}`,
      of: owner
    }))
  ]
}

/// A line scan of the buffer's top-level items: names, tests, the comment above each. Works on code that doesn't parse.
export function indexBuffer(doc: Text): BufferIndex {
  const defs: BufferDef[] = []
  const tests: BufferTest[] = []
  const starts: { line: number; def?: BufferDef; test?: BufferTest; also?: BufferDef[] }[] = []
  let inBlock = false
  for (let n = 1; n <= doc.lines; n++) {
    const text = doc.line(n).text
    if (inBlock) {
      if (text.includes('*/')) inBlock = false
      continue
    }
    if (text.startsWith('/*') && !text.includes('*/')) {
      inBlock = true
      continue
    }
    const c0 = text.charCodeAt(0)
    if (!c0 || c0 === 32 || c0 === 9 || text.startsWith('//')) continue
    if (text.startsWith('test ') || text.startsWith('test(')) {
      const t = { line: n, endLine: n, text: text.replace(/\s*\/\/.*$/, '') }
      tests.push(t)
      starts.push({ line: n, test: t })
      continue
    }
    const rec = recordDefs(doc, n)
    if (rec) {
      defs.push(...rec)
      starts.push({ line: n, def: rec[0], also: rec.slice(1) })
      continue
    }
    const m = DEF_RE.exec(text.replace(/\s*\/\/.*$/, ''))
    if (m && !IGNORE_DEF.has(m[2])) {
      const doc_: string[] = []
      for (let k = n - 1; k >= 1 && doc_.length < 8; k--) {
        const c = doc.line(k).text.trim()
        if (!c.startsWith('//') || /^\/\/\s*[-=─]{3,}/.test(c)) break
        doc_.unshift(c.replace(/^\/\/\/?\s?/, ''))
      }
      const def: BufferDef = {
        name: m[2],
        line: n,
        endLine: n,
        sig: m[3] || undefined,
        params: paramsFrom(m[4]),
        doc: doc_.join('\n') || undefined,
        decor: m[1]
      }
      defs.push(def)
      starts.push({ line: n, def })
      continue
    }
    starts.push({ line: n })
  }
  // an item runs until the next one starts, trailing blanks and comments excluded
  for (let i = 0; i < starts.length; i++) {
    const limit = i + 1 < starts.length ? starts[i + 1].line - 1 : doc.lines
    let end = limit
    while (end > starts[i].line) {
      const t = doc.line(end).text.trim()
      if (t === '' || t.startsWith('//')) end--
      else break
    }
    if (starts[i].def) starts[i].def!.endLine = end
    for (const d of starts[i].also ?? []) d.endLine = end
    if (starts[i].test) starts[i].test!.endLine = end
  }
  return { defs, tests }
}

// ---- completion -------------------------------------------------------------

const SNIPPETS = [
  snippetCompletion('test ${lhs} = ${rhs}', { label: 'test', detail: 'an equation', type: 'keyword', boost: 6 }),
  snippetCompletion('if ${cond} then ${a} else ${b}', { label: 'if', detail: 'if … then … else', type: 'keyword', boost: 3 }),
  snippetCompletion('{${x}} => ${body}', { label: 'lambda', detail: '{x} => body', type: 'keyword' }),
  snippetCompletion('fix({self, ${x}} => ${body})', { label: 'fix', detail: 'recursion', type: 'keyword', boost: 2 }),
  snippetCompletion('open use "${../prelude.disp}" {}', { label: 'open use', detail: 'bring a file into scope', type: 'keyword' }),
  snippetCompletion('let ${name} := ${value}', { label: 'let', detail: 'private definition', type: 'keyword' }),
  snippetCompletion('match ${value} {\n\t${Case} ${x} => ${body}\n}', { label: 'match', detail: 'on a sum type', type: 'keyword' })
]
const LEAF_OPTION: Completion = { label: 't', type: 'constant', detail: 'the leaf', boost: -5 }

const iconType = (params?: string[]) => (params ? 'function' : 'variable')

function binderNames(state: EditorState, pos: number, itemStart: number): string[] {
  const text = state.doc.sliceString(itemStart, pos)
  const out = new Set<string>()
  const re = /\{([^{}]*)\}\s*(?:=>|->)/g
  let m: RegExpExecArray | null
  while ((m = re.exec(text))) for (const p of m[1].split(',')) {
    const n = p.split(':')[0].trim()
    if (/^[A-Za-z_][\w']*$/.test(n)) out.add(n)
  }
  return [...out]
}

export interface PlaygroundLink {
  /// the current buffer's index
  index: () => BufferIndex
  scope: () => ScopeItem[]
  paths: () => string[]
  currentPath: () => string
  /// exports of the file a `name := use "…"` alias points at
  moduleItems: (rel: string) => Promise<ScopeItem[]>
  shortPath: (p: string) => string
  jumpDef: (name: string) => void
  back: () => void
  rename: () => void
  nextProblem: () => void
}

function completionSource(link: PlaygroundLink) {
  return async (ctx: CompletionContext): Promise<CompletionResult | null> => {
    const line = ctx.state.doc.lineAt(ctx.pos)
    const before = line.text.slice(0, ctx.pos - line.from)
    // comments: nothing to offer
    if (/\/\//.test(before.replace(/"[^"]*"/g, ''))) return null

    // use "…: paths relative to this file
    const pathM = before.match(/use\s+(?:raw\s+)?"([^"]*)$/)
    if (pathM) {
      const from = ctx.pos - pathM[1].length
      return {
        from,
        validFor: /^[^"]*$/,
        options: link.paths().map((p) => ({ label: relPath(link.currentPath(), p), type: 'text', detail: 'file' }))
      }
    }
    if (/"[^"]*$/.test(before.replace(/"[^"]*"/g, ''))) return null

    // alias.: the module's exports; Record.: its fields; anything else.: a field, or any function, called with it first
    const dotM = before.match(/([A-Za-z_][\w')\]}]*)\.([\w']*)$/)
    if (dotM) {
      const re = new RegExp(`^(?:let\\s+)?${dotM[1].replace(/'/g, "\\'")}\\s*:=\\s*use\\s+(?:raw\\s+)?"([^"]+)"`, 'm')
      const m = /^[A-Za-z_][\w']*$/.test(dotM[1]) ? re.exec(ctx.state.doc.toString()) : null
      const from = ctx.pos - dotM[2].length
      if (m) return { from, validFor: /^[\w']*$/, options: (await link.moduleItems(m[1])).map((it) => optionFor(it, link)) }
      const fields = recordFields(link)
      const own = fields.filter((f) => f.of === dotM[1])
      if (own.length > 0) return { from, validFor: /^[\w']*$/, options: own.map((f) => fieldOption(f)) }
      const seen = new Set<string>()
      const options: Completion[] = []
      for (const f of fields)
        if (!seen.has(f.name)) {
          seen.add(f.name)
          options.push(fieldOption(f))
        }
      for (const d of [...link.index().defs].reverse())
        if (d.params && !d.of && !seen.has(d.name)) {
          seen.add(d.name)
          options.push({ label: d.name, type: 'function', detail: `{${d.params.join(', ')}}`, info: d.doc, boost: -20 })
        }
      for (const it of link.scope())
        if (it.params && !it.name.includes('.') && !seen.has(it.name)) {
          seen.add(it.name)
          options.push({ ...optionFor(it, link), boost: -30 })
        }
      return { from, validFor: /^[\w']*$/, options }
    }

    const word = ctx.matchBefore(/[A-Za-z_][\w']*/)
    if (!word && !ctx.explicit) return null
    const from = word?.from ?? ctx.pos
    const cursorLine = line.number
    const idx = link.index()
    const seen = new Set<string>()
    const options: Completion[] = []
    let itemStart = 1
    for (const d of idx.defs) if (d.line <= cursorLine) itemStart = Math.max(itemStart, d.line)
    for (const t of idx.tests) if (t.line <= cursorLine) itemStart = Math.max(itemStart, t.line)
    const inFirstLineHead = before.trim() === (word ? word.text : '')

    for (const n of binderNames(ctx.state, ctx.pos, ctx.state.doc.line(itemStart).from)) {
      if (seen.has(n) || (word && n === word.text)) continue
      seen.add(n)
      options.push({ label: n, type: 'variable', detail: 'parameter', boost: 40 })
    }
    for (let i = idx.defs.length - 1; i >= 0; i--) {
      const d = idx.defs[i]
      // a name is only in scope below its definition
      if (seen.has(d.name) || d.line >= cursorLine) continue
      seen.add(d.name)
      options.push({
        label: d.name,
        type: iconType(d.params),
        detail: d.sig ?? (d.params ? `{${d.params.join(', ')}}` : ''),
        info: d.doc,
        boost: 30
      })
    }
    for (const it of link.scope()) {
      if (seen.has(it.name)) continue
      seen.add(it.name)
      options.push(optionFor(it, link))
    }
    for (const f of recordFields(link)) {
      if (seen.has(f.name)) continue
      seen.add(f.name)
      options.push({ ...fieldOption(f), boost: 0 })
    }
    options.push(LEAF_OPTION)
    if (inFirstLineHead) options.push(...SNIPPETS)
    else options.push(SNIPPETS[1], SNIPPETS[2], SNIPPETS[3])
    return { from, options, validFor: /^[A-Za-z_][\w']*$/ }
  }
}

/// Every record field in reach: the buffer's own records, then the opened files' `Record.field` accessors.
function recordFields(link: PlaygroundLink): { name: string; of: string; sig?: string }[] {
  const out: { name: string; of: string; sig?: string }[] = []
  for (const d of link.index().defs) if (d.of) out.push({ name: d.name, of: d.of, sig: d.sig })
  for (const it of link.scope()) {
    const dot = it.name.lastIndexOf('.')
    if (dot > 0) out.push({ name: it.name.slice(dot + 1), of: it.name.slice(0, dot), sig: it.sig })
  }
  return out
}
const fieldOption = (f: { name: string; of: string; sig?: string }): Completion => ({ label: f.name, type: 'property', detail: `field of ${f.of}`, info: f.sig, boost: 20 })

function optionFor(it: ScopeItem, link: PlaygroundLink): Completion {
  return {
    label: it.name,
    type: iconType(it.params),
    detail: it.sig ?? (it.params ? `{${it.params.join(', ')}}` : link.shortPath(it.path)),
    info: it.doc ? `${it.doc}\n\n${link.shortPath(it.path)}:${it.line}` : `${link.shortPath(it.path)}:${it.line}`
  }
}

export function relPath(from: string, to: string): string {
  const a = from.split('/').slice(0, -1)
  const b = to.split('/')
  let i = 0
  while (i < a.length && i < b.length && a[i] === b[i]) i++
  const ups = a.length - i
  return (ups === 0 ? './' : '../'.repeat(ups)) + b.slice(i).join('/')
}

// ---- signature help ---------------------------------------------------------

/// The application around the cursor: its head name and which argument the cursor is on.
function callAt(state: EditorState): { head: string; arg: number; pos: number } | null {
  const sel = state.selection.main
  if (!sel.empty) return null
  const line = state.doc.lineAt(sel.head)
  let text = line.text.slice(0, sel.head - line.from)
  if (text.includes('//')) return null
  text = text.replace(/"[^"]*"/g, (s) => '_'.repeat(s.length))
  if (text.includes('"')) return null
  // inside a call, f(a, b|): the head sits before the paren, commas count the arguments
  let commas = 0
  let nest = 0
  for (let k = text.length - 1; k >= 0; k--) {
    const c = text[k]
    if (c === ')' || c === '}' || c === ']') nest++
    else if (c === '(' || c === '{' || c === '[') {
      if (nest > 0) {
        nest--
        continue
      }
      const glued = c === '(' ? text.slice(0, k).match(/([A-Za-z_][\w']*)$/) : null
      if (glued) return { head: glued[1], arg: commas, pos: line.from + k - glued[1].length }
      break
    } else if (c === ',' && nest === 0) commas++
  }
  return null
}

function signatureField(link: PlaygroundLink) {
  const compute = (state: EditorState): Tooltip | null => {
    const c = callAt(state)
    if (!c) return null
    const def = link.index().defs.findLast((d) => d.name === c.head) ?? link.scope().find((s) => s.name === c.head)
    if (!def?.params || c.arg >= def.params.length) return null
    const params = def.params
    return {
      pos: c.pos,
      above: true,
      strictSide: false,
      create() {
        const dom = document.createElement('div')
        dom.className = 'disp-sig-help'
        const h = document.createElement('span')
        h.className = 'disp-sig-head'
        h.textContent = c.head
        dom.appendChild(h)
        params.forEach((p, i) => {
          dom.appendChild(document.createTextNode(' '))
          const s = document.createElement('span')
          s.textContent = p
          if (i === c.arg) s.className = 'disp-sig-on'
          dom.appendChild(s)
        })
        return { dom }
      }
    }
  }
  return StateField.define<Tooltip | null>({
    create: compute,
    update: (v, tr) => (tr.docChanged || tr.selection ? (tr.docChanged || v ? compute(tr.state) : null) : v),
    provide: (f) => showTooltip.from(f)
  })
}

/// Hovering a name: its parameters, type, where it lives and its doc comment.
function nameHover(link: PlaygroundLink) {
  return hoverTooltip((view, pos) => {
    const line = view.state.doc.lineAt(pos)
    const cmt = line.text.indexOf('//')
    if (cmt >= 0 && pos - line.from > cmt) return null
    const word = view.state.wordAt(pos)
    if (!word) return null
    const name = view.state.sliceDoc(word.from, word.to)
    if (!/^[A-Za-z_][\w']*$/.test(name) || IGNORE_DEF.has(name)) return null
    const own = link.index().defs.findLast((d) => d.name === name)
    const item = own ?? link.scope().find((x) => x.name === name)
    if (!item) return null
    const where = own ? `line ${own.line}` : `${link.shortPath((item as ScopeItem).path)}:${item.line}`
    return {
      pos: word.from,
      end: word.to,
      above: true,
      create() {
        const dom = document.createElement('div')
        dom.className = 'disp-name-hover'
        const head = document.createElement('div')
        head.className = 'disp-sig-help'
        const h = document.createElement('span')
        h.className = 'disp-sig-head'
        h.textContent = name
        head.appendChild(h)
        if (item.params?.length) head.appendChild(document.createTextNode(`(${item.params.join(', ')})`))
        if (item.sig) head.appendChild(document.createTextNode(` : ${item.sig}`))
        const at = document.createElement('span')
        at.className = 'disp-hover-where'
        at.textContent = where
        head.appendChild(at)
        dom.appendChild(head)
        if (item.doc) {
          const body = document.createElement('div')
          body.className = 'disp-hover-doc'
          body.textContent = item.doc
          dom.appendChild(body)
        }
        return { dom }
      }
    }
  })
}

// ---- parameter hints -------------------------------------------------------

class ParamHint extends WidgetType {
  constructor(readonly name: string) {
    super()
  }
  override eq(o: ParamHint) {
    return o.name === this.name
  }
  override toDOM() {
    const s = document.createElement('span')
    s.className = 'disp-param-hint'
    s.setAttribute('aria-hidden', 'true')
    s.textContent = this.name + ':'
    return s
  }
}

/// Where each argument of every glued call starts, with the parameter it fills. Strings and comments are blanked first.
export function paramHints(text: string, paramsOf: (name: string, recv: string | undefined) => string[] | undefined): { pos: number; name: string }[] {
  // a string still starts an argument, so it blanks to a placeholder; a comment blanks to space
  const code = text.replace(/\/\/[^\n]*|"(?:[^"\\\n]|\\.)*"?/g, (m) => (m[0] === '/' ? ' ' : '#').repeat(m.length))
  const out: { pos: number; name: string }[] = []
  const head = /([A-Za-z_][\w']*)(?=\(|\{[^{}]*:=)/g
  let m: RegExpExecArray | null
  while ((m = head.exec(code))) {
    const at = m.index
    if (at > 0 && /[\w']/.test(code[at - 1])) continue
    const recv = code[at - 1] === '.' ? /([\w')\]]+)\.$/.exec(code.slice(Math.max(0, at - 80), at))?.[1] : undefined
    const params = paramsOf(m[1], recv)
    if (!params) continue
    // what's still unfilled: f(a) fills from the front, f{b := 1} fills b, and the chain goes on after either
    let left = [...params]
    let i = at + m[1].length
    while ((code[i] === '(' || code[i] === '{') && left.length > 0) {
      const brace = code[i] === '{'
      const named: string[] = []
      let depth = 1
      let expect = true
      let j = i + 1
      for (; j < code.length && depth > 0; j++) {
        const c = code[j]
        if (expect && depth === 1 && c !== ')' && c !== '}' && !/\s/.test(c)) {
          expect = false
          if (brace) {
            const n = /^([A-Za-z_][\w']*)\s*:=/.exec(code.slice(j, j + 60))?.[1]
            if (n) named.push(n)
          } else {
            const name = left.shift()
            const arg = /^[A-Za-z_][\w']*/.exec(code.slice(j, j + 40))?.[0]
            if (name && name.length > 1 && name !== arg && name !== '_') out.push({ pos: j, name })
          }
        }
        if (c === '(' || c === '{' || c === '[') depth++
        else if (c === ')' || c === '}' || c === ']') depth--
        else if (c === ',' && depth === 1) expect = true
      }
      if (depth > 0) break
      if (brace) left = left.filter((p) => !named.includes(p))
      i = j
    }
  }
  // an outer call's later arguments are found before the calls nested in its earlier ones
  return out.sort((a, b) => a.pos - b.pos)
}

function hintPlugin(link: PlaygroundLink) {
  return ViewPlugin.fromClass(
    class {
      decorations: DecorationSet = Decoration.none
      constructor(view: EditorView) {
        this.build(view)
      }
      update(u: ViewUpdate) {
        if (u.docChanged || u.viewportChanged || u.transactions.some((t) => t.effects.length > 0)) this.build(u.view)
      }
      build(view: EditorView) {
        const doc = view.state.doc
        // a call that starts above the viewport can still have arguments in it
        const from = doc.lineAt(Math.max(0, view.viewport.from - 3000)).from
        const to = doc.lineAt(Math.min(doc.length, view.viewport.to + 300)).to
        const text = doc.sliceString(from, to)
        const aliases = new Set<string>()
        for (const a of doc.toString().matchAll(/^(?:let\s+)?([A-Za-z_][\w']*)\s*:=\s*use\b/gm)) aliases.add(a[1])
        const params = new Map<string, string[] | undefined>()
        for (const it of link.scope()) if (!params.has(it.name)) params.set(it.name, it.params)
        for (const d of link.index().defs) params.set(d.name, d.params)
        const b = new RangeSetBuilder<Decoration>()
        const hints = paramHints(text, (name, recv) => {
          // alias.f is the module's f, which this index doesn't know
          if (recv && aliases.has(recv)) return undefined
          const ps = (recv && params.get(`${recv}.${name}`)) || params.get(name)
          // x.f(a) is f(x, a); Record.f(a) is just f
          return recv && !/^[A-Z]/.test(recv) ? ps?.slice(1) : ps
        })
        for (const h of hints) b.add(from + h.pos, from + h.pos, Decoration.widget({ widget: new ParamHint(h.name), side: -1 }))
        this.decorations = b.finish()
      }
    },
    { decorations: (v) => v.decorations }
  )
}

// ---- folding ----------------------------------------------------------------

const indentOf = (s: string) => s.search(/\S/)
const foldByIndent = foldService.of((state, start) => {
  const first = state.doc.lineAt(start)
  const base = indentOf(first.text)
  if (base < 0 || first.number >= state.doc.lines) return null
  let last = first.number
  for (let n = first.number + 1; n <= state.doc.lines; n++) {
    const t = state.doc.line(n).text
    const ind = indentOf(t)
    if (ind < 0) continue
    if (ind <= base) {
      // a lone closer belongs to the fold
      if (ind === base && /^[)\]}]/.test(t.trim())) last = n
      break
    }
    last = n
  }
  if (last === first.number) return null
  return { from: first.to, to: state.doc.line(last).to }
})

// ---- what the page drives: costs, the subject, glowing names ----------------

export const setCosts = StateEffect.define<Map<number, { frac: number; steps: number }>>()
const costField = StateField.define<Map<number, { frac: number; steps: number }>>({
  create: () => new Map(),
  update(v, tr) {
    for (const e of tr.effects) if (e.is(setCosts)) return e.value
    return v
  }
})

const fmtSteps = (n: number) => (n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(1) + 'k' : String(n))
class CostMarker extends GutterMarker {
  constructor(
    readonly frac: number,
    readonly steps: number
  ) {
    super()
  }
  override eq(o: CostMarker) {
    return o.frac === this.frac && o.steps === this.steps
  }
  override toDOM() {
    const d = document.createElement('div')
    d.className = 'disp-cost'
    d.title = `${fmtSteps(this.steps)} evaluator steps`
    d.style.setProperty('--w', `${Math.max(8, Math.round(this.frac * 100))}%`)
    return d
  }
}
const costGutter = gutter({
  class: 'disp-cost-gutter',
  lineMarker(view, line) {
    const c = view.state.field(costField).get(view.state.doc.lineAt(line.from).number)
    return c ? new CostMarker(c.frac, c.steps) : null
  },
  lineMarkerChange: (u) => u.transactions.some((t) => t.effects.some((e) => e.is(setCosts)))
})

export const setSubjectLines = StateEffect.define<{ from: number; to: number } | null>()
const subjectField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes)
    for (const e of tr.effects)
      if (e.is(setSubjectLines)) {
        const b = new RangeSetBuilder<Decoration>()
        if (e.value)
          for (let n = e.value.from; n <= Math.min(e.value.to, tr.state.doc.lines); n++) {
            const l = tr.state.doc.line(n)
            b.add(l.from, l.from, Decoration.line({ class: 'disp-subject' }))
          }
        deco = b.finish()
      }
    return deco
  },
  provide: (f) => EditorView.decorations.from(f)
})

export const setGlowName = StateEffect.define<string | null>()
const glowMark = Decoration.mark({ class: 'disp-glow' })
const glowPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet = Decoration.none
    name: string | null = null
    update(u: ViewUpdate) {
      let changed = u.docChanged || u.viewportChanged
      for (const t of u.transactions)
        for (const e of t.effects)
          if (e.is(setGlowName)) {
            this.name = e.value
            changed = true
          }
      if (changed) this.build(u.view)
    }
    build(view: EditorView) {
      if (!this.name || !/^[A-Za-z_][\w']*$/.test(this.name)) {
        this.decorations = Decoration.none
        return
      }
      const b = new RangeSetBuilder<Decoration>()
      const re = new RegExp(`(?<![\\w'])${this.name.replace(/'/g, "\\'")}(?![\\w'])`, 'g')
      for (const r of view.visibleRanges) {
        const text = view.state.doc.sliceString(r.from, r.to)
        let m: RegExpExecArray | null
        while ((m = re.exec(text))) b.add(r.from + m.index, r.from + m.index + m[0].length, glowMark)
      }
      this.decorations = b.finish()
    }
  },
  { decorations: (v) => v.decorations }
)

const theme = EditorView.theme({
  '.disp-cost-gutter': { width: '14px' },
  '.disp-cost': { height: '100%', position: 'relative', minHeight: '1em' },
  '.disp-cost::after': {
    content: "''",
    position: 'absolute',
    right: '3px',
    top: '50%',
    height: '3px',
    marginTop: '-1px',
    width: 'var(--w)',
    borderRadius: '2px',
    background: 'var(--warn)'
  },
  '.cm-line.disp-subject': { boxShadow: 'inset 2px 0 0 var(--accent)' },
  '.disp-glow': { background: 'color-mix(in oklab, var(--accent) 26%, transparent)', borderRadius: '3px' },
  '.disp-param-hint': {
    fontFamily: 'var(--font-mono)',
    fontSize: '0.82em',
    color: 'var(--fg-faint)',
    background: 'color-mix(in oklab, var(--fg-faint) 12%, transparent)',
    borderRadius: '3px',
    padding: '0 3px',
    marginRight: '3px',
    userSelect: 'none',
    pointerEvents: 'none'
  },
  '.disp-sig-help': { fontFamily: 'var(--font-mono)', fontSize: '12px', padding: '3px 8px', color: 'var(--fg-muted)' },
  '.disp-sig-head': { color: 'var(--hl-fn)' },
  '.disp-name-hover': { maxWidth: '62ch' },
  '.disp-hover-where': { marginLeft: '1.2em', color: 'var(--fg-faint)', fontSize: '11px' },
  '.disp-hover-doc': { padding: '0 8px 5px', fontSize: '12px', color: 'var(--fg-muted)', whiteSpace: 'pre-wrap' },
  '.disp-sig-on': { color: 'var(--fg)', fontWeight: '700', textDecoration: 'underline', textUnderlineOffset: '3px' },
  '.cm-tooltip': {
    background: 'var(--bg-elev)',
    border: '1px solid var(--border-strong)',
    borderRadius: '8px',
    boxShadow: 'var(--shadow-lift)',
    color: 'var(--fg)'
  },
  '.cm-tooltip.cm-tooltip-autocomplete > ul': { fontFamily: 'var(--font-mono)', fontSize: '12.5px', maxHeight: '15em', minWidth: '16em' },
  '.cm-tooltip.cm-tooltip-autocomplete > ul > li': { padding: '2px 8px', lineHeight: '1.5' },
  '.cm-tooltip-autocomplete ul li[aria-selected]': { background: 'color-mix(in oklab, var(--accent) 26%, transparent)', color: 'var(--fg)' },
  '.cm-completionLabel': { color: 'var(--fg)' },
  '.cm-completionMatchedText': { textDecoration: 'none', color: 'var(--accent)', fontWeight: '700' },
  '.cm-completionDetail': { marginLeft: '1em', color: 'var(--fg-faint)', fontStyle: 'normal', fontSize: '11px' },
  '.cm-completionIcon': { opacity: '0.55', paddingRight: '0.5em', width: '1em', fontSize: '80%' },
  '.cm-completionInfo': { fontFamily: 'var(--font-body)', fontSize: '12.5px', padding: '6px 9px', maxWidth: '28em', whiteSpace: 'pre-wrap', lineHeight: '1.45' },
  '.cm-foldGutter .cm-gutterElement': { color: 'var(--fg-faint)', opacity: '0', transition: 'opacity 0.1s', cursor: 'pointer' },
  '.cm-gutters:hover .cm-foldGutter .cm-gutterElement': { opacity: '1' },
  '.cm-foldPlaceholder': { background: 'var(--bg-panel-hover)', border: 'none', color: 'var(--fg-muted)', padding: '0 6px', borderRadius: '4px' },
  '.cm-selectionMatch': { background: 'color-mix(in oklab, var(--accent) 14%, transparent)' },
  '.cm-searchMatch': { background: 'color-mix(in oklab, var(--warn) 30%, transparent)', outline: 'none' },
  '.cm-searchMatch.cm-searchMatch-selected': { background: 'color-mix(in oklab, var(--warn) 55%, transparent)' },
  '.cm-panels': { background: 'var(--bg-elev)', color: 'var(--fg)', borderColor: 'var(--border)' },
  '.cm-panel.cm-search input, .cm-panel.cm-search button': { fontFamily: 'var(--font-body)', fontSize: '12.5px' },
  '.cm-textfield': { background: 'var(--bg-panel)', border: '1px solid var(--border-strong)', borderRadius: '5px', color: 'var(--fg)' },
  '.cm-button': { background: 'var(--bg-panel)', backgroundImage: 'none', border: '1px solid var(--border-strong)', borderRadius: '5px', color: 'var(--fg)' },
})

export function playgroundExtensions(link: PlaygroundLink): Extension[] {
  return [
    highlightSpecialChars(),
    dropCursor(),
    rectangularSelection(),
    crosshairCursor(),
    indentOnInput(),
    codeFolding(),
    foldGutter({ openText: '▾', closedText: '▸' }),
    foldByIndent,
    costField,
    costGutter,
    subjectField,
    glowPlugin,
    highlightSelectionMatches({ highlightWordAroundCursor: true, minSelectionLength: 2, wholeWords: true }),
    autocompletion({
      override: [completionSource(link)],
      activateOnTypingDelay: 40,
      maxRenderedOptions: 60,
      icons: false,
      tooltipClass: () => 'disp-complete'
    }),
    signatureField(link),
    nameHover(link),
    hintPlugin(link),
    theme,
    Prec.high(
      keymap.of([
        ...closeBracketsKeymap,
        ...completionKeymap,
        ...foldKeymap,
        { key: 'F12', run: (v) => (jumpAtCursor(v, link), true) },
        { key: 'Alt-ArrowLeft', run: () => (link.back(), true) },
        { key: 'F2', run: () => (link.rename(), true) },
        { key: 'F8', run: () => (link.nextProblem(), true) }
      ])
    )
  ]
}

function jumpAtCursor(view: EditorView, link: PlaygroundLink) {
  const w = view.state.wordAt(view.state.selection.main.head)
  if (w) link.jumpDef(view.state.sliceDoc(w.from, w.to))
}

/// Replace every code occurrence of `from` with `to`, skipping comments and strings.
export function renameChanges(doc: Text, from: string, to: string): { from: number; to: number; insert: string }[] {
  const out: { from: number; to: number; insert: string }[] = []
  const re = new RegExp(`(?<![\\w'])${from.replace(/'/g, "\\'")}(?![\\w'])`, 'g')
  for (let n = 1; n <= doc.lines; n++) {
    const line = doc.line(n)
    const cmt = line.text.indexOf('//')
    const code = cmt >= 0 ? line.text.slice(0, cmt) : line.text
    const masked = code.replace(/"[^"]*"/g, (s) => ' '.repeat(s.length))
    let m: RegExpExecArray | null
    re.lastIndex = 0
    while ((m = re.exec(masked))) out.push({ from: line.from + m.index, to: line.from + m.index + m[0].length, insert: to })
  }
  return out
}
