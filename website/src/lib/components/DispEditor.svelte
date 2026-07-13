<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
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

  // The notebook output cell: a block widget below its line. Collapsed to one
  // ellipsized line; clicking toggles full wrapped text.
  class OutBlockWidget extends WidgetType {
    text: string
    kind: string
    constructor(text: string, kind: string) {
      super()
      this.text = text
      this.kind = kind
    }
    override eq(other: OutBlockWidget): boolean {
      return other.text === this.text && other.kind === this.kind
    }
    override get estimatedHeight(): number {
      return 26
    }
    override toDOM(): HTMLElement {
      const d = document.createElement('div')
      d.className = `disp-out disp-out-${this.kind}`
      d.textContent = this.text
      d.title = this.text
      d.addEventListener('mousedown', (e) => {
        e.preventDefault()
        d.classList.toggle('open')
      })
      return d
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
    api?: (a: EditorApi) => void
  }

  export interface EditorApi {
    getDoc(): string
    getDocToCursor(): string
    setDoc(d: string): void
    setMarks(marks: LineMark[]): void
    gotoLine(line: number): void
  }

  let { doc, onDocChange, onRunFile, onRunToCursor, api }: Props = $props()

  let host: HTMLDivElement
  let view: EditorView | undefined
  // marks describe a completed run; edits make them stale (dimmed) until the
  // next setMarks delivers fresh results
  let stale = $state(false)

  const setMarksEffect = StateEffect.define<LineMark[]>()

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
            if (m.block)
              builder.push({
                from: l.to,
                deco: Decoration.widget({
                  widget: new OutBlockWidget(m.block, m.kind),
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
          ...dispLanguageExtensions,
          EditorView.updateListener.of((u) => {
            if (u.docChanged) {
              stale = true
              onDocChange?.(u.state.doc.toString())
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
      },
      gotoLine: (n: number) => {
        if (n < 1 || n > view!.state.doc.lines) return
        const l = view!.state.doc.line(n)
        view!.dispatch({ selection: { anchor: l.from }, scrollIntoView: true })
        view!.focus()
      }
    })
  })

  onDestroy(() => view?.destroy())
</script>

<div class="editor-host" class:marks-stale={stale} bind:this={host}></div>

<style>
  .editor-host {
    height: 100%;
    overflow: clip; /* clip, not hidden: only .cm-scroller should ever scroll */
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
  .editor-host :global(.disp-note) {
    transition: opacity 0.18s ease;
  }
  .editor-host.marks-stale :global(.disp-out),
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
