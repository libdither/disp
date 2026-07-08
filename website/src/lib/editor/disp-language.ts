// CodeMirror 6 language support for disp: a StreamLanguage tokenizer that
// mirrors editors/vscode-disp's rust-style call-site coloring — the head of an
// application spine (an identifier that starts an expression and is applied to
// at least one atom) gets the "function" color.

import { StreamLanguage, HighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { tags as t } from '@lezer/highlight'
import { EditorView } from '@codemirror/view'

const KEYWORDS = new Set(['use', 'open', 'match', 'if', 'then', 'else', 'raw', 'given'])
const DECORATORS = new Set(['let', 'test', 'guard', 'guard_eq', 'freeze', 'license_guard', 'sig', 'base'])
const CONSTANTS = new Set(['true', 'false', 't'])

interface S {
  exprStart: boolean
  inBlockComment: boolean
}

const IDENT = /^[A-Za-z_][A-Za-z0-9_']*/
// after the ident: whitespace then an atom => the ident is an application head
const HEAD_LOOKAHEAD = /^[ \t]+[A-Za-z0-9_("△{[]/

export const dispStream = StreamLanguage.define<S>({
  name: 'disp',
  startState: () => ({ exprStart: true, inBlockComment: false }),
  token(stream, state) {
    // a fresh line is (almost always) a fresh declaration/expression head
    if (stream.sol()) state.exprStart = true
    if (state.inBlockComment) {
      if (stream.match(/^.*?\*\//)) state.inBlockComment = false
      else stream.skipToEnd()
      return 'comment'
    }
    if (stream.eatSpace()) return null
    if (stream.match('//')) {
      stream.skipToEnd()
      return 'comment'
    }
    if (stream.match('/*')) {
      state.inBlockComment = !stream.match(/^.*?\*\//)
      if (state.inBlockComment) stream.skipToEnd()
      return 'comment'
    }
    if (stream.match(/^"(?:[^"\\]|\\.)*"?/)) {
      state.exprStart = false
      return 'string'
    }
    if (stream.match(/^[0-9]+/)) {
      state.exprStart = false
      return 'number'
    }
    if (stream.match(':=') || stream.match('=>') || stream.match('->') || stream.match('→')) {
      state.exprStart = true
      return 'operator'
    }
    if (stream.match(/^[=:]/)) {
      state.exprStart = true
      return 'operator'
    }
    if (stream.match(/^[({[,;]/)) {
      state.exprStart = true
      return 'punctuation'
    }
    if (stream.match(/^[)}\]]/)) {
      state.exprStart = false
      return 'punctuation'
    }
    if (stream.match(/^\./)) {
      state.exprStart = false
      return 'operator'
    }
    if (stream.match('△')) {
      state.exprStart = false
      return 'atom'
    }
    const id = stream.match(IDENT) as RegExpMatchArray | null
    if (id) {
      const word = id[0]
      if (KEYWORDS.has(word)) {
        state.exprStart = true
        return 'keyword'
      }
      if (DECORATORS.has(word)) {
        // decorator only at expression start; `test` mid-expression is a variable
        if (state.exprStart) {
          state.exprStart = true
          return 'keyword'
        }
        state.exprStart = false
        return 'variableName'
      }
      if (CONSTANTS.has(word)) {
        state.exprStart = false
        return 'atom'
      }
      if (word === '_') {
        state.exprStart = false
        return 'atom'
      }
      if (/^[A-Z]/.test(word)) {
        state.exprStart = false
        return 'typeName'
      }
      const isHead = state.exprStart && HEAD_LOOKAHEAD.test(stream.string.slice(stream.pos))
      state.exprStart = false
      return isHead ? 'variableName.function' : 'variableName'
    }
    stream.next()
    state.exprStart = false
    return null
  },
  languageData: {
    commentTokens: { line: '//', block: { open: '/*', close: '*/' } }
  }
})

// pastel botanical palette (kept in step with app.css's .hl-* classes)
export const dispHighlight = HighlightStyle.define([
  { tag: t.comment, color: '#7c8b74', fontStyle: 'italic' },
  { tag: t.string, color: '#2b8a52' },
  { tag: t.number, color: '#1d7f8a' },
  { tag: t.keyword, color: '#7c5cbf' },
  { tag: t.operator, color: '#66766b' },
  { tag: t.punctuation, color: '#85937f' },
  { tag: t.atom, color: '#bf5a92' },
  { tag: t.typeName, color: '#0f766e' },
  { tag: [t.function(t.variableName)], color: '#a8770f' },
  { tag: t.variableName, color: '#2e4034' }
])

export const dispEditorTheme = EditorView.theme(
  {
    '&': {
      backgroundColor: 'transparent',
      color: '#2e4034',
      fontSize: '13.5px',
      height: '100%'
    },
    '.cm-content': {
      fontFamily: "'JetBrains Mono Variable', 'JetBrains Mono', monospace",
      padding: '12px 0',
      caretColor: '#2f9e6e'
    },
    '.cm-cursor, .cm-dropCursor': { borderLeftColor: '#2f9e6e' },
    '&.cm-focused': { outline: 'none' },
    '.cm-gutters': {
      backgroundColor: 'transparent',
      color: '#8ea08b',
      border: 'none',
      fontFamily: "'JetBrains Mono Variable', monospace",
      fontSize: '12px'
    },
    '.cm-activeLineGutter': { backgroundColor: 'rgba(74,104,82,0.08)' },
    '.cm-activeLine': { backgroundColor: 'rgba(74,104,82,0.05)' },
    '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
      backgroundColor: 'rgba(88,179,104,0.22) !important'
    },
    '.cm-line.disp-line-pass': { backgroundColor: 'rgba(58,157,99,0.1)' },
    '.cm-line.disp-line-fail': { backgroundColor: 'rgba(201,95,109,0.13)' },
    '.cm-line.disp-line-error': { backgroundColor: 'rgba(217,154,43,0.15)' }
  },
  { dark: false }
)

export const dispLanguageExtensions = [dispStream, syntaxHighlighting(dispHighlight), dispEditorTheme]
