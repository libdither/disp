// Hover a name defined in the buffer → its `::` doc annotation (the item's
// docType) and the `//` comment block right above its definition.
import { hoverTooltip, showTooltip, EditorView, type Tooltip } from '@codemirror/view'
import { StateEffect, StateField, type Text } from '@codemirror/state'

interface DefDoc {
  sig: string | null
  comment: string[]
}

const IDENT = /[A-Za-z_][A-Za-z0-9_']*/g

/// The definition of `name` at the top level of `doc`: its signature (if annotated) and leading comments.
export function findDef(doc: Text, name: string): DefDoc | null {
  const head = new RegExp(`^(?:rec\\s+)?${name.replace(/'/g, "\\'")}\\s*(::|:=|:)`)
  for (let n = 1; n <= doc.lines; n++) {
    const text = doc.line(n).text
    const m = head.exec(text)
    if (!m) continue
    let sig: string | null = null
    if (m[1] === '::') {
      const rest = text.slice(m.index + m[0].length)
      const end = rest.search(/:=>?/)
      sig = (end >= 0 ? rest.slice(0, end) : rest).trim()
    }
    const comment: string[] = []
    for (let k = n - 1; k >= 1; k--) {
      const c = doc.line(k).text.trim()
      if (!c.startsWith('//') || c.startsWith('// ----')) break
      comment.unshift(c.replace(/^\/\/\/?\s?/, ''))
    }
    return { sig, comment }
  }
  return null
}

/// The doc tooltip for the identifier under `pos`, if it names a documented def.
function docTipAt(doc: Text, pos: number): Tooltip | null {
  const line = doc.lineAt(pos)
  const col = pos - line.from
  const cmt = line.text.indexOf('//')
  if (cmt >= 0 && col > cmt) return null
  let m: RegExpExecArray | null
  IDENT.lastIndex = 0
  while ((m = IDENT.exec(line.text))) {
    if (col < m.index || col > m.index + m[0].length) continue
    const name = m[0]
    const def = findDef(doc, name)
    if (!def || (!def.sig && def.comment.length === 0)) return null
    return {
      pos: line.from + m.index,
      end: line.from + m.index + name.length,
      above: true,
      create() {
        const dom = document.createElement('div')
        dom.className = 'disp-doc-hover'
        const sig = document.createElement('div')
        sig.className = 'disp-doc-sig'
        sig.textContent = def.sig ? `${name} :: ${def.sig}` : name
        dom.appendChild(sig)
        if (def.comment.length) {
          const body = document.createElement('div')
          body.className = 'disp-doc-body'
          body.textContent = def.comment.join('\n')
          dom.appendChild(body)
        }
        return { dom }
      }
    }
  }
  return null
}

export const docHover = hoverTooltip((view, pos) => docTipAt(view.state.doc, pos))

// Touch has no hover: a tap on a documented name pins its tooltip; any other tap clears it.
const setTapTip = StateEffect.define<Tooltip | null>()
const tapTip = StateField.define<Tooltip | null>({
  create: () => null,
  update(tip, tr) {
    for (const e of tr.effects) if (e.is(setTapTip)) return e.value
    return tr.docChanged ? null : tip
  },
  provide: (f) => showTooltip.from(f)
})
let lastPointer = 'mouse'
export const docTap = [
  tapTip,
  EditorView.domEventHandlers({
    pointerdown(e) {
      lastPointer = e.pointerType
      return false
    },
    click(e, view) {
      if (lastPointer !== 'touch') return false
      const pos = view.posAtCoords({ x: e.clientX, y: e.clientY })
      const tip = pos == null ? null : docTipAt(view.state.doc, pos)
      view.dispatch({ effects: setTapTip.of(tip) })
      return false
    }
  })
]

export const docHoverTheme = EditorView.baseTheme({
  '.disp-doc-hover': { padding: '4px 8px', maxWidth: '60ch', fontSize: '0.9em' },
  '.disp-doc-sig': { fontFamily: 'var(--font-mono, monospace)', whiteSpace: 'pre-wrap' },
  '.disp-doc-body': { marginTop: '4px', whiteSpace: 'pre-wrap', opacity: '0.8' }
})
