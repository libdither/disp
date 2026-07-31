// CodeMirror 6 language support for disp: a StreamLanguage tokenizer that
// mirrors editors/vscode-disp's rust-style call-site coloring — the head of an
// application spine (an identifier that starts an expression and is applied to
// at least one atom) gets the "function" color.

import { StreamLanguage, HighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { tags as t } from '@lezer/highlight'
import { EditorView } from '@codemirror/view'

const KEYWORDS = new Set(['use', 'open', 'match', 'if', 'then', 'else', 'raw', 'given'])
const DECORATORS = new Set(['let', 'test', 'guard', 'freeze', 'license_guard', 'sig', 'base'])
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

/// Palette lives in app.css (the --hl-* tokens, shared with its .hl-* classes),
/// so the editor follows the light/dark theme without rebuilding the extension.
export const dispHighlight = HighlightStyle.define([
  { tag: t.comment, color: 'var(--hl-comment)', fontStyle: 'italic' },
  { tag: t.string, color: 'var(--hl-string)' },
  { tag: t.number, color: 'var(--hl-number)' },
  { tag: t.keyword, color: 'var(--hl-kw)' },
  { tag: t.operator, color: 'var(--hl-op)' },
  { tag: t.punctuation, color: 'var(--hl-punct)' },
  { tag: t.atom, color: 'var(--hl-atom)' },
  { tag: t.typeName, color: 'var(--hl-type)' },
  { tag: [t.function(t.variableName)], color: 'var(--hl-fn)' },
  { tag: t.variableName, color: 'var(--hl-var)' }
])

export const dispEditorTheme = EditorView.theme(
  {
    '&': {
      backgroundColor: 'transparent',
      color: 'var(--hl-var)',
      fontSize: '13.5px',
      height: '100%'
    },
    '.cm-content': {
      fontFamily: "'JetBrains Mono Variable', 'JetBrains Mono', monospace",
      padding: '12px 0',
      caretColor: 'var(--accent)'
    },
    '.cm-cursor, .cm-dropCursor': { borderLeftColor: 'var(--accent)' },
    '&.cm-focused': { outline: 'none' },
    '.cm-gutters': {
      backgroundColor: 'transparent',
      color: 'var(--ed-gutter)',
      border: 'none',
      fontFamily: "'JetBrains Mono Variable', monospace",
      fontSize: '12px'
    },
    '.cm-activeLineGutter': { backgroundColor: 'var(--ed-line-gutter)' },
    '.cm-activeLine': { backgroundColor: 'var(--ed-line)' },
    '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
      backgroundColor: 'var(--ed-sel) !important'
    },
    '.cm-line.disp-line-pass': { backgroundColor: 'var(--ed-pass-line)' },
    '.cm-line.disp-line-fail': { backgroundColor: 'var(--ed-fail-line)' },
    // ctrl/cmd+hover on a clickable import string (VS Code's link affordance)
    '.disp-link': {
      textDecoration: 'underline',
      textUnderlineOffset: '3px',
      textDecorationColor: 'var(--accent)',
      cursor: 'pointer'
    },
    // errors underline the offending line (wavy red, IDE-style)
    '.disp-error-underline': {
      textDecoration: 'underline wavy var(--err)',
      textDecorationThickness: '1.2px',
      textUnderlineOffset: '4px'
    },
    // inline evaluation notes (LineMark.note) rendered after the line's text
    '.disp-note': {
      marginLeft: '1.4em',
      fontSize: '0.85em',
      fontStyle: 'italic',
      opacity: '0.9',
      userSelect: 'none',
      pointerEvents: 'none'
    },
    '.disp-note-pass': { color: 'var(--ok)' },
    '.disp-note-fail': { color: 'var(--err)' },
    '.disp-note-error': { color: 'var(--err)' },
    // notebook output cells (LineMark.block): a block widget under the line.
    // Collapsed to one ellipsized line; `.open` (click) wraps the full text.
    // `contain: inline-size` zeroes the widget's intrinsic width so a long
    // nowrap value can't stretch .cm-content (a min-width:auto flex item)
    // into a horizontal scroll — the text clips to the editor instead.
    '.disp-out': {
      contain: 'inline-size',
      margin: '2px 14px 8px 2.2em',
      padding: '4px 12px',
      borderLeft: '2px solid var(--ed-out-rule)',
      borderRadius: '0 8px 8px 0',
      background: 'var(--ed-out-bg)',
      fontFamily: "'JetBrains Mono Variable', 'JetBrains Mono', monospace",
      fontSize: '0.85em',
      lineHeight: '1.55',
      color: 'var(--ed-out-fg)',
      whiteSpace: 'nowrap',
      overflow: 'hidden',
      textOverflow: 'ellipsis',
      cursor: 'pointer',
      transition: 'opacity 0.15s ease, font-size 0.15s ease, padding 0.15s ease'
    },
    '.disp-out.open': {
      whiteSpace: 'pre-wrap',
      wordBreak: 'break-all'
    },
    // interactive mode: one row per value (a def's value; got/want pairs)
    '.disp-out-row': {
      overflow: 'hidden',
      textOverflow: 'ellipsis'
    },
    '.disp-out.open .disp-out-row': {
      whiteSpace: 'pre-wrap',
      wordBreak: 'break-all'
    },
    '.disp-out-label': {
      opacity: '0.55',
      marginRight: '0.7em',
      fontSize: '0.92em',
      userSelect: 'none'
    },
    // text-mode blocks get their glyph from CSS; structured rows carry an
    // inline .disp-out-glyph span instead (a block-level row after an inline
    // ::before would break onto a second line)
    '.disp-out:not(:has(.disp-out-row))::before': {
      content: "'⟵ '",
      opacity: '0.5',
      userSelect: 'none'
    },
    '.disp-out-glyph': {
      opacity: '0.5',
      marginRight: '0.55em',
      userSelect: 'none'
    },
    '.disp-out-fail': {
      borderLeftColor: 'var(--ed-out-fail-rule)',
      background: 'var(--ed-out-fail-bg)',
      color: 'var(--ed-out-fail-fg)'
    },
    '.disp-out-fail:not(:has(.disp-out-row))::before': { content: "'✗ '" },
    '.disp-out-error:not(:has(.disp-out-row))::before': { content: "'⚠ '" },
    '.disp-out-error': {
      borderLeftColor: 'var(--ed-out-fail-rule)',
      background: 'var(--ed-out-fail-bg)',
      color: 'var(--ed-out-fail-fg)'
    },
    // a def's value ON its line: [tree] → value, capped and ellipsized
    '.disp-ival': {
      display: 'inline-block',
      verticalAlign: 'text-bottom',
      maxWidth: '46ch',
      overflow: 'hidden',
      textOverflow: 'ellipsis',
      whiteSpace: 'nowrap',
      marginLeft: '1.1em',
      padding: '0 0.55em',
      borderRadius: '7px',
      background: 'var(--ed-chip)',
      fontFamily: "'JetBrains Mono Variable', 'JetBrains Mono', monospace",
      fontSize: '0.85em',
      color: 'var(--ed-out-fg)',
      transition: 'opacity 0.15s ease, max-width 0.15s ease'
    },
    '.disp-ival-arrow': {
      opacity: '0.5',
      margin: '0 0.4em 0 0.15em',
      userSelect: 'none'
    },
    '.disp-ival-viz': {
      background: 'none',
      border: 'none',
      padding: '0',
      margin: '0',
      font: 'inherit',
      cursor: 'pointer',
      color: 'var(--ed-gutter)',
      verticalAlign: '-0.15em'
    },
    '.disp-ival-viz:hover': { color: 'var(--accent)' },
    '.disp-ival-viz svg': { width: '1.05em', height: '1.05em' },
    '.disp-ival.away': {
      opacity: '0.4',
      maxWidth: '16ch'
    },
    '.disp-ival.away:hover': {
      opacity: '0.85'
    },
    // focus-collapse: a block whose def line the cursor is NOT on shrinks to
    // a quiet sliver; landing the cursor there (or hovering) brings it back
    '.disp-out.away': {
      opacity: '0.4',
      fontSize: '0.72em',
      padding: '1px 12px'
    },
    '.disp-out.away:hover': {
      opacity: '0.85'
    }
  },
  { dark: false }
)

export const dispLanguageExtensions = [dispStream, syntaxHighlighting(dispHighlight), dispEditorTheme]
