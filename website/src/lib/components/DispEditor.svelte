<script lang="ts">
  import { onMount, onDestroy, mount, unmount } from 'svelte'
  import TreeValue, { type TreeValueCtl } from './TreeValue.svelte'
  import type { ValueNode } from '$lib/disp/protocol'
  import { EditorView, keymap, lineNumbers, highlightActiveLine, highlightActiveLineGutter, drawSelection } from '@codemirror/view'
  import { EditorState, StateEffect, StateField, Compartment } from '@codemirror/state'
  import { defaultKeymap, history, historyKeymap, indentWithTab } from '@codemirror/commands'
  import { searchKeymap } from '@codemirror/search'
  import { bracketMatching } from '@codemirror/language'
  import { closeBrackets } from '@codemirror/autocomplete'
  import { Decoration, WidgetType, type DecorationSet } from '@codemirror/view'
  import { dispLanguageExtensions } from '$lib/editor/disp-language'

  export interface LineMark {
    line: number
    kind: 'pass' | 'fail' | 'error' | 'value'
    // inline evaluation note rendered after the line's text (e.g. '✓', '✗')
    note?: string
    // block output rendered UNDER the line, notebook-style: a def's value,
    // a failing test's got/want, an error message. Click toggles wrapping.
    block?: string
    // structured alternative to `block`: one row per entry, rendered as an
    // interactive TreeValue (click-to-unfold; needs the expandValue prop)
    values?: { label?: string; node: ValueNode }[]
    // first line of the item this block belongs to (its anchor `line` is the
    // last): the block stays full-size while the cursor is in [focusFrom,
    // line] and collapses to a quiet sliver elsewhere
    focusFrom?: number
  }

  class NoteWidget extends WidgetType {
    text: string
    kind: string
    constructor(text: string, kind: string) {
      super()
      this.text = text
      this.kind = kind
    }
    override eq(other: NoteWidget): boolean {
      return other.text === this.text && other.kind === this.kind
    }
    override toDOM(): HTMLElement {
      const s = document.createElement('span')
      s.className = `disp-note disp-note-${this.kind}`
      s.textContent = this.text
      return s
    }
    override ignoreEvent(): boolean {
      return true
    }
  }

  // A def's value, ON its line: [tree button] → value — an inline widget
  // capped by max-width (CSS ellipsis), dimming away with the cursor like
  // the blocks. Buttons (visualize + the TreeValue atoms) handle their own
  // clicks; everywhere else CM's ordinary cursor placement applies.
  class InlineValueWidget extends WidgetType {
    node: ValueNode
    lines: { from: number; to: number } | undefined
    expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>
    onVisualize?: (tree: ValueNode, ctl: TreeValueCtl) => void
    onFoldChange?: (tree: ValueNode, ctl: TreeValueCtl) => void
    #mounted: object | null = null
    constructor(
      node: ValueNode,
      expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>,
      onVisualize?: (tree: ValueNode, ctl: TreeValueCtl) => void,
      onFoldChange?: (tree: ValueNode, ctl: TreeValueCtl) => void,
      lines?: { from: number; to: number }
    ) {
      super()
      this.node = node
      this.expand = expand
      this.onVisualize = onVisualize
      this.onFoldChange = onFoldChange
      this.lines = lines
    }
    override eq(other: InlineValueWidget): boolean {
      return (
        other.node === this.node &&
        other.lines?.from === this.lines?.from &&
        other.lines?.to === this.lines?.to
      )
    }
    override toDOM(): HTMLElement {
      const s = document.createElement('span')
      s.className = 'disp-ival'
      if (this.lines) {
        s.dataset.from = String(this.lines.from)
        s.dataset.to = String(this.lines.to)
        if (cursorLine < this.lines.from || cursorLine > this.lines.to) s.classList.add('away')
      }
      let ctl: TreeValueCtl | null = null
      const btn = document.createElement('button')
      btn.className = 'disp-ival-viz'
      btn.title = 'visualize reduction (folded names stay symbolic)'
      btn.setAttribute('aria-label', 'visualize reduction')
      btn.innerHTML =
        '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 4.5 4.8 10.5M8 4.5l3.2 6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" fill="none"/><circle cx="8" cy="3.4" r="1.6" fill="currentColor"/><circle cx="4.8" cy="11.6" r="1.6" fill="currentColor"/><circle cx="11.2" cy="11.6" r="1.6" fill="currentColor"/></svg>'
      btn.addEventListener('click', (e) => {
        e.stopPropagation()
        if (ctl) this.onVisualize?.(ctl.snapshot(), ctl)
      })
      s.appendChild(btn)
      const arrow = document.createElement('span')
      arrow.className = 'disp-ival-arrow'
      arrow.textContent = '→'
      s.appendChild(arrow)
      this.#mounted = mount(TreeValue, {
        target: s,
        props: {
          node: this.node,
          expand: this.expand,
          onFoldChange: this.onFoldChange,
          ctlRef: (c: TreeValueCtl) => (ctl = c)
        }
      })
      return s
    }
    override destroy(): void {
      if (this.#mounted) void unmount(this.#mounted)
      this.#mounted = null
    }
    override ignoreEvent(e: Event): boolean {
      // buttons are the widget's own; anything else is the editor's
      return (e.target as HTMLElement).closest('button') != null
    }
  }

  // The notebook output cell: a block widget below its line. Collapsed to one
  // ellipsized line per row; clicking the background toggles full wrapped
  // text. Plain-text mode (`text`) or interactive mode (`values`: TreeValue
  // rows — clicks on atoms/… unfold subterms and stopPropagation past the
  // collapse toggle).
  class OutBlockWidget extends WidgetType {
    text: string | undefined
    values: { label?: string; node: ValueNode }[] | undefined
    kind: string
    lines: { from: number; to: number } | undefined
    expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>
    onVisualize?: (tree: ValueNode, ctl: TreeValueCtl) => void
    onFoldChange?: (tree: ValueNode, ctl: TreeValueCtl) => void
    #mounted: object[] = []
    constructor(
      kind: string,
      text?: string,
      values?: { label?: string; node: ValueNode }[],
      expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>,
      onVisualize?: (tree: ValueNode, ctl: TreeValueCtl) => void,
      onFoldChange?: (tree: ValueNode, ctl: TreeValueCtl) => void,
      lines?: { from: number; to: number }
    ) {
      super()
      this.kind = kind
      this.text = text
      this.values = values
      this.expand = expand
      this.onVisualize = onVisualize
      this.onFoldChange = onFoldChange
      this.lines = lines
    }
    override eq(other: OutBlockWidget): boolean {
      // values compare by reference: each run builds fresh mark objects, so
      // fresh results recreate the widget (and reset its unfold history)
      return (
        other.kind === this.kind &&
        other.text === this.text &&
        other.values === this.values &&
        other.lines?.from === this.lines?.from &&
        other.lines?.to === this.lines?.to
      )
    }
    override get estimatedHeight(): number {
      return 26 * (this.values?.length ?? 1)
    }
    override toDOM(): HTMLElement {
      const d = document.createElement('div')
      d.className = `disp-out disp-out-${this.kind}`
      if (this.lines) {
        d.dataset.from = String(this.lines.from)
        d.dataset.to = String(this.lines.to)
        // collapse immediately when born off-cursor (widgets materialize
        // lazily as the viewport scrolls)
        if (cursorLine < this.lines.from || cursorLine > this.lines.to) d.classList.add('away')
      }
      // background click = put the cursor in the item's line, at the clicked
      // x — the block regains focus size without any layout surprise (the old
      // behavior, toggling wrapped display, now lives on double-click)
      d.addEventListener('mousedown', (e) => {
        if ((e.target as HTMLElement).closest('button')) return
        e.preventDefault()
        if (!this.lines || !view) return
        const ln = Math.min(this.lines.to, view.state.doc.lines)
        const line = view.state.doc.line(ln)
        const rect = view.coordsAtPos(line.from)
        const pos = rect
          ? (view.posAtCoords({ x: e.clientX, y: (rect.top + rect.bottom) / 2 }) ?? line.to)
          : line.to
        view.dispatch({
          selection: { anchor: Math.max(line.from, Math.min(pos, line.to)) },
          scrollIntoView: true
        })
        view.focus()
      })
      d.addEventListener('dblclick', (e) => {
        if ((e.target as HTMLElement).closest('button')) return
        e.preventDefault()
        d.classList.toggle('open')
      })
      if (this.values) {
        let first = true
        for (const v of this.values) {
          const row = document.createElement('div')
          row.className = 'disp-out-row'
          if (first) {
            // the kind glyph rides INSIDE the first row: an inline ::before
            // followed by a block-level row would break onto its own line
            const g = document.createElement('span')
            g.className = 'disp-out-glyph'
            g.textContent = this.kind === 'fail' ? '✗' : '⟵'
            row.appendChild(g)
            first = false
          }
          if (v.label) {
            const l = document.createElement('span')
            l.className = 'disp-out-label'
            l.textContent = v.label
            row.appendChild(l)
          }
          this.#mounted.push(
            mount(TreeValue, {
              target: row,
              props: {
                node: v.node,
                expand: this.expand,
                onGrew: () => d.classList.add('open'),
                onVisualize: this.onVisualize,
                onFoldChange: this.onFoldChange
              }
            })
          )
          d.appendChild(row)
        }
      } else {
        d.textContent = this.text ?? ''
        d.title = this.text ?? ''
      }
      return d
    }
    override destroy(): void {
      for (const m of this.#mounted) void unmount(m)
      this.#mounted = []
    }
    override ignoreEvent(): boolean {
      return true
    }
  }

  interface Props {
    doc: string
    onDocChange?: (doc: string) => void
    onRunFile?: () => void
    onRunToCursor?: () => void
    // subtree re-render for interactive value blocks (LineMark.values)
    expandValue?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>
    // hand a value's current fold-state tree (+ its controller) to the
    // reduction visualizer
    onVisualizeValue?: (tree: ValueNode, ctl: TreeValueCtl) => void
    // fold-state changes stream out for two-way sync with the visualizer
    onValueFoldChange?: (tree: ValueNode, ctl: TreeValueCtl) => void
    // ctrl/cmd-click on a "….disp" string literal reports it (jump-to-file);
    // the host resolves it against its notion of the current file
    onOpenPath?: (path: string) => void
    // ctrl/cmd-click on an identifier: jump to its definition
    onJumpDef?: (name: string) => void
    // does this identifier resolve? (the hover underline only shows for
    // names that actually go somewhere — VS Code honesty)
    onResolveIdent?: (name: string) => Promise<boolean>
    // select an expression and, when `validate` accepts it, a small action
    // button appears at the selection's right edge — `run` receives the text
    // (the playground's visualize-this-expression affordance)
    selectionAction?: {
      validate: (text: string) => boolean
      run: (text: string) => void
      title?: string
    }
    api?: (a: EditorApi) => void
  }

  export interface EditorApi {
    getDoc(): string
    getDocToCursor(): string
    setDoc(d: string): void
    setMarks(marks: LineMark[]): void
    gotoLine(line: number): void
  }

  let { doc, onDocChange, onRunFile, onRunToCursor, expandValue, onVisualizeValue, onValueFoldChange, onOpenPath, onJumpDef, onResolveIdent, selectionAction, api }: Props = $props()

  let host: HTMLDivElement
  let view: EditorView | undefined
  // marks describe a completed run; edits make them stale (dimmed) until the
  // next setMarks delivers fresh results
  let stale = $state(false)

  // the cursor's current line: blocks outside their item's line range wear
  // `.away` (collapsed sliver). Widgets read it at creation; selection moves
  // sweep the live DOM.
  let cursorLine = 1
  function updateBlockFocus() {
    if (!view) return
    cursorLine = view.state.doc.lineAt(view.state.selection.main.head).number
    queueMicrotask(() => {
      host?.querySelectorAll<HTMLElement>('.disp-out[data-from], .disp-ival[data-from]').forEach((el) => {
        const from = Number(el.dataset.from)
        const to = Number(el.dataset.to)
        el.classList.toggle('away', cursorLine < from || cursorLine > to)
      })
    })
  }

  const setMarksEffect = StateEffect.define<LineMark[]>()

  // ---- highlight an expression → a floating action button -----------------
  // Tracks the selection; when the host's validator accepts the selected
  // text, a small button appears just past the selection's end.
  let selBtn = $state<{ x: number; y: number; text: string } | null>(null)
  function updateSelectionAction() {
    if (!view || !selectionAction) return
    const sel = view.state.selection.main
    if (sel.empty || sel.to - sel.from > 300) {
      selBtn = null
      return
    }
    const text = view.state.doc.sliceString(sel.from, sel.to).trim()
    if (!text || !selectionAction.validate(text)) {
      selBtn = null
      return
    }
    const coords = view.coordsAtPos(sel.to)
    if (!coords) {
      selBtn = null
      return
    }
    const hostRect = host.getBoundingClientRect()
    selBtn = { x: coords.right - hostRect.left + 6, y: coords.top - hostRect.top - 3, text }
  }

  // ---- ctrl/cmd+hover link affordance (VS Code style) ----------------------
  // While the modifier is held, the clickable "….disp" string under the
  // pointer underlines and takes a pointer cursor.
  const setLinkRange = StateEffect.define<{ from: number; to: number } | null>()
  const linkField = StateField.define<DecorationSet>({
    create: () => Decoration.none,
    update(deco, tr) {
      deco = deco.map(tr.changes)
      for (const e of tr.effects)
        if (e.is(setLinkRange))
          deco = e.value
            ? Decoration.set([Decoration.mark({ class: 'disp-link' }).range(e.value.from, e.value.to)])
            : Decoration.none
      return deco
    },
    provide: (f) => EditorView.decorations.from(f)
  })

  // the one link detector, shared by hover and click: import strings jump to
  // files, identifiers jump to definitions
  type LinkHit =
    | { kind: 'path'; from: number; to: number; path: string }
    | { kind: 'ident'; from: number; to: number; name: string }
  const LINK_KEYWORDS = new Set(['use', 'open', 'match', 'if', 'then', 'else', 'raw', 'given', 'let', 'test', 't'])
  function linkAt(v: EditorView, x: number, y: number): LinkHit | null {
    const pos = v.posAtCoords({ x, y })
    if (pos == null) return null
    const line = v.state.doc.lineAt(pos)
    const text = line.text
    // strings first (a hit inside a non-.disp string is NOT an identifier)
    const re = /"([^"]*)"/g
    let m: RegExpExecArray | null
    while ((m = re.exec(text))) {
      const from = line.from + m.index
      const to = from + m[0].length
      if (pos >= from && pos <= to) return m[1].endsWith('.disp') ? { kind: 'path', from, to, path: m[1] } : null
    }
    const cmt = text.indexOf('//')
    const idRe = /[A-Za-z_][A-Za-z0-9_']*/g
    while ((m = idRe.exec(text))) {
      const s = m.index
      const e = s + m[0].length
      if (s > pos - line.from) break
      if (pos - line.from < s || pos - line.from > e) continue
      if (cmt >= 0 && s > cmt) return null // commented out
      if (LINK_KEYWORDS.has(m[0])) return null
      return { kind: 'ident', from: line.from + s, to: line.from + e, name: m[0] }
    }
    return null
  }

  let modHeld = false
  let lastMouse: { x: number; y: number } | null = null
  let shownLink: { from: number; to: number } | null = null
  // identifier resolvability, probed once per name (cleared on any edit —
  // the answer depends on the buffer's defs and opens)
  const identKnown = new Map<string, boolean>()
  function refreshLink() {
    if (!view) return
    const l = modHeld && lastMouse ? linkAt(view, lastMouse.x, lastMouse.y) : null
    let next: { from: number; to: number } | null = null
    if (l?.kind === 'path' && onOpenPath) {
      next = { from: l.from, to: l.to }
    } else if (l?.kind === 'ident' && onJumpDef) {
      const known = identKnown.get(l.name)
      if (known === true) next = { from: l.from, to: l.to }
      else if (known === undefined && onResolveIdent) {
        identKnown.set(l.name, false) // pending — no repeat probes
        void onResolveIdent(l.name).then((ok) => {
          identKnown.set(l.name, ok)
          if (ok) refreshLink()
        })
      }
    }
    if (next?.from === shownLink?.from && next?.to === shownLink?.to) return
    shownLink = next
    view.dispatch({ effects: setLinkRange.of(next) })
  }
  const onModKey = (e: KeyboardEvent) => {
    if (e.key !== 'Control' && e.key !== 'Meta') return
    modHeld = e.type === 'keydown'
    refreshLink()
  }
  const onWinBlur = () => {
    modHeld = false
    refreshLink()
  }

  // Marks survive edits (mapped through changes) rather than clearing: the
  // host dims them via the `marks-stale` class until fresh results arrive, so
  // typing doesn't collapse the layout and re-run flicker stays gentle.
  const markField = StateField.define<DecorationSet>({
    create: () => Decoration.none,
    update(deco, tr) {
      deco = deco.map(tr.changes)
      for (const e of tr.effects) {
        if (e.is(setMarksEffect)) {
          const builder: { from: number; to?: number; deco: Decoration }[] = []
          for (const m of e.value) {
            if (m.line < 1 || m.line > tr.state.doc.lines) continue
            const l = tr.state.doc.line(m.line)
            if (m.kind === 'error') {
              // errors wear a red wavy underline over the line's text (the
              // compiler reports errors by LINE — no finer spans yet)
              const text = tr.state.doc.sliceString(l.from, l.to)
              const start = text.search(/\S/)
              if (start >= 0) {
                const end = text.replace(/\s+$/, '').length
                builder.push({
                  from: l.from + start,
                  to: l.from + end,
                  deco: Decoration.mark({ class: 'disp-error-underline' })
                })
              }
            } else if (m.kind !== 'value') {
              builder.push({ from: l.from, deco: Decoration.line({ class: `disp-line-${m.kind}` }) })
            }
            if (m.note)
              builder.push({
                from: l.to,
                deco: Decoration.widget({ widget: new NoteWidget(m.note, m.kind), side: 1 })
              })
            if (m.kind === 'value' && m.values?.length === 1)
              // a def's value rides ITS line: [tree] → value
              builder.push({
                from: l.to,
                deco: Decoration.widget({
                  widget: new InlineValueWidget(
                    m.values[0].node,
                    expandValue,
                    onVisualizeValue,
                    onValueFoldChange,
                    { from: m.focusFrom ?? m.line, to: m.line }
                  ),
                  side: 2
                })
              })
            else if (m.block || m.values)
              builder.push({
                from: l.to,
                deco: Decoration.widget({
                  widget: new OutBlockWidget(
                    m.kind,
                    m.block,
                    m.values,
                    expandValue,
                    onVisualizeValue,
                    onValueFoldChange,
                    { from: m.focusFrom ?? m.line, to: m.line }
                  ),
                  side: 2,
                  block: true
                })
              })
          }
          deco = Decoration.set(
            builder.map((b) => (b.to != null ? b.deco.range(b.from, b.to) : b.deco.range(b.from))),
            true
          )
        }
      }
      return deco
    },
    provide: (f) => EditorView.decorations.from(f)
  })

  onMount(() => {
    view = new EditorView({
      parent: host,
      state: EditorState.create({
        doc,
        extensions: [
          lineNumbers(),
          history(),
          drawSelection(),
          highlightActiveLine(),
          highlightActiveLineGutter(),
          bracketMatching(),
          closeBrackets(),
          EditorView.lineWrapping,
          markField,
          keymap.of([
            {
              key: 'Mod-Enter',
              run: () => {
                onRunFile?.()
                return true
              }
            },
            {
              key: 'Shift-Enter',
              run: () => {
                onRunToCursor?.()
                return true
              }
            },
            indentWithTab,
            ...defaultKeymap,
            ...historyKeymap,
            ...searchKeymap
          ]),
          // ctrl/cmd-click a "….disp" string → jump to that file; hovering
          // with the modifier held shows the underline affordance
          linkField,
          EditorView.domEventHandlers({
            mousedown: (e, v) => {
              if (!(e.ctrlKey || e.metaKey) || e.button !== 0) return false
              const l = linkAt(v, e.clientX, e.clientY)
              if (l?.kind === 'path' && onOpenPath) {
                e.preventDefault()
                onOpenPath(l.path)
                return true
              }
              if (l?.kind === 'ident' && onJumpDef) {
                e.preventDefault()
                onJumpDef(l.name)
                return true
              }
              return false
            },
            mousemove: (e) => {
              lastMouse = { x: e.clientX, y: e.clientY }
              modHeld = e.ctrlKey || e.metaKey
              refreshLink()
              return false
            },
            mouseleave: () => {
              lastMouse = null
              refreshLink()
              return false
            }
          }),
          ...dispLanguageExtensions,
          EditorView.updateListener.of((u) => {
            if (u.docChanged) {
              stale = true
              identKnown.clear() // resolvability follows the buffer's defs/opens
              onDocChange?.(u.state.doc.toString())
            }
            if (u.selectionSet || u.docChanged) {
              updateBlockFocus()
              updateSelectionAction()
            }
          })
        ]
      })
    })

    api?.({
      getDoc: () => view!.state.doc.toString(),
      getDocToCursor: () => {
        const pos = view!.state.selection.main.head
        const line = view!.state.doc.lineAt(pos)
        return view!.state.doc.sliceString(0, line.to)
      },
      setDoc: (d: string) => {
        // a wholesale replacement (example switch) invalidates all marks
        view!.dispatch({
          changes: { from: 0, to: view!.state.doc.length, insert: d },
          effects: setMarksEffect.of([])
        })
        stale = false
      },
      setMarks: (marks) => {
        view!.dispatch({ effects: setMarksEffect.of(marks) })
        stale = false
        updateBlockFocus()
      },
      gotoLine: (n: number) => {
        if (n < 1 || n > view!.state.doc.lines) return
        const l = view!.state.doc.line(n)
        view!.dispatch({ selection: { anchor: l.from }, scrollIntoView: true })
        view!.focus()
      }
    })
  })

  onMount(() => {
    // modifier press/release can happen while the pointer is already resting
    // on a link — track it at the window level
    window.addEventListener('keydown', onModKey)
    window.addEventListener('keyup', onModKey)
    window.addEventListener('blur', onWinBlur)
    return () => {
      window.removeEventListener('keydown', onModKey)
      window.removeEventListener('keyup', onModKey)
      window.removeEventListener('blur', onWinBlur)
    }
  })

  onDestroy(() => view?.destroy())
</script>

<div class="editor-host" class:marks-stale={stale} bind:this={host}>
  {#if selBtn && selectionAction}
    <button
      class="sel-action"
      style="left: {selBtn.x}px; top: {selBtn.y}px"
      title={selectionAction.title ?? 'run on this selection'}
      aria-label={selectionAction.title ?? 'run on this selection'}
      onmousedown={(e) => e.preventDefault()}
      onclick={() => {
        if (selBtn) selectionAction.run(selBtn.text)
      }}
    >
      <svg viewBox="0 0 16 16" aria-hidden="true">
        <path d="M8 4.5 4.8 10.5M8 4.5l3.2 6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" fill="none" />
        <circle cx="8" cy="3.4" r="1.6" fill="currentColor" />
        <circle cx="4.8" cy="11.6" r="1.6" fill="currentColor" />
        <circle cx="11.2" cy="11.6" r="1.6" fill="currentColor" />
      </svg>
    </button>
  {/if}
</div>

<style>
  .editor-host {
    position: relative; /* anchors the floating selection-action button */
    height: 100%;
    overflow: clip; /* clip, not hidden: only .cm-scroller should ever scroll */
  }
  .sel-action {
    position: absolute;
    z-index: 6;
    display: grid;
    place-items: center;
    width: 1.5rem;
    height: 1.5rem;
    padding: 0;
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    background: var(--bg-elev);
    color: var(--accent);
    cursor: pointer;
    box-shadow: var(--shadow-soft);
  }
  .sel-action:hover {
    background: #fff;
  }
  .sel-action svg {
    width: 0.95rem;
    height: 0.95rem;
  }
  .editor-host :global(.cm-editor) {
    height: 100%;
  }
  .editor-host :global(.cm-scroller) {
    overflow: auto;
  }
  /* stale results (buffer edited since the run): dim, don't clear — layout
     stays put and the next setMarks lifts the veil */
  .editor-host :global(.disp-out),
  .editor-host :global(.disp-ival),
  .editor-host :global(.disp-note) {
    transition: opacity 0.18s ease;
  }
  .editor-host.marks-stale :global(.disp-out),
  .editor-host.marks-stale :global(.disp-ival),
  .editor-host.marks-stale :global(.disp-note) {
    opacity: 0.3;
  }
  .editor-host.marks-stale :global(.cm-line.disp-line-pass),
  .editor-host.marks-stale :global(.cm-line.disp-line-fail) {
    background: transparent;
  }
  .editor-host.marks-stale :global(.disp-error-underline) {
    text-decoration-color: transparent;
  }
</style>
