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

// palette wired to the site design tokens
export const dispHighlight = HighlightStyle.define([
  { tag: t.comment, color: '#5d6a82', fontStyle: 'italic' },
  { tag: t.string, color: '#4ade80' },
  { tag: t.number, color: '#22d3ee' },
  { tag: t.keyword, color: '#c084fc' },
  { tag: t.operator, color: '#94a3b8' },
  { tag: t.punctuation, color: '#7c8aa5' },
  { tag: t.atom, color: '#f0abfc' },
  { tag: t.typeName, color: '#5eead4' },
  { tag: [t.function(t.variableName)], color: '#fbbf24' },
  { tag: t.variableName, color: '#e9edf6' }
])

export const dispEditorTheme = EditorView.theme(
  {
    '&': {
      backgroundColor: 'transparent',
      color: '#e9edf6',
      fontSize: '13.5px',
      height: '100%'
    },
    '.cm-content': {
      fontFamily: "'JetBrains Mono Variable', 'JetBrains Mono', monospace",
      padding: '12px 0',
      caretColor: '#22d3ee'
    },
    '.cm-cursor, .cm-dropCursor': { borderLeftColor: '#22d3ee' },
    '&.cm-focused': { outline: 'none' },
    '.cm-gutters': {
      backgroundColor: 'transparent',
      color: '#5d6a82',
      border: 'none',
      fontFamily: "'JetBrains Mono Variable', monospace",
      fontSize: '12px'
    },
    '.cm-activeLineGutter': { backgroundColor: 'rgba(148,163,184,0.08)' },
    '.cm-activeLine': { backgroundColor: 'rgba(148,163,184,0.05)' },
    '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
      backgroundColor: 'rgba(129,140,248,0.25) !important'
    },
    '.cm-line.disp-line-pass': { backgroundColor: 'rgba(74,222,128,0.07)' },
    '.cm-line.disp-line-fail': { backgroundColor: 'rgba(251,113,133,0.12)' },
    '.cm-line.disp-line-error': { backgroundColor: 'rgba(251,191,36,0.12)' }
  },
  { dark: true }
)

export const dispLanguageExtensions = [dispStream, syntaxHighlighting(dispHighlight), dispEditorTheme]
