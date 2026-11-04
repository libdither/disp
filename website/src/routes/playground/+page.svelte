<script lang="ts">
  // The playground: the real toolchain in a tab. Code on the left with results
  // inline; an inspector on the right that follows the cursor and shows the
  // tree under it, or runs it step by step. A palette (ctrl-K) reaches the rest.
  import { onMount } from 'svelte'
  import type { EditorState } from '@codemirror/state'
  import type { EditorView } from '@codemirror/view'
  import { disp } from '$lib/disp/client.svelte'
  import type { RunOutcome, ItemEvent, ScopeItem } from '$lib/disp/protocol'
  import { examples, EXAMPLE_STORAGE_KEY } from '$lib/disp/examples'
  import { encodeShared, decodeShared, isSharedHash } from '$lib/disp/share'
  import DispEditor, { type EditorApi, type LineMark } from '$lib/components/DispEditor.svelte'
  import Inspector from '$lib/playground/Inspector.svelte'
  import Sidebar, { type OutlineRow } from '$lib/playground/Sidebar.svelte'
  import Palette, { type PaletteItem } from '$lib/playground/Palette.svelte'
  import type { Subject } from '$lib/playground/subject'
  import {
    indexBuffer,
    playgroundExtensions,
    renameChanges,
    relPath,
    setCosts,
    setGlowName,
    setSubjectLines,
    type BufferIndex,
    type PlaygroundLink
  } from '$lib/playground/editor-ext'

  const SCRATCH_PATH = '/lib/tests/playground.disp'
  const LS_DOC = 'disp-playground-doc'
  const LS_LIVE = 'disp-playground-live'
  const LS_WELCOME = 'disp-playground-welcomed'
  const LS_TABS = 'disp-playground-tabs'
  const LS_USER = 'disp-playground-userfiles'
  const LS_LAYOUT = 'disp-playground-layout'

  // svelte-ignore non_reactive_update
  let editorApi: EditorApi | undefined
  let currentDoc = $state('')
  let exampleId = $state('walkthrough')
  let initialDoc = examples[0].source

  // ---- tabs -----------------------------------------------------------------
  // The scratch buffer plus opened library files. Each tab runs at its own path
  // and keeps its own editor state (undo, folds, cursor). A single-click open
  // lands in one reusable preview slot, like VS Code.
  type Tab = { id: string; title: string; path: string; doc: string; kind: 'scratch' | 'file'; dirty?: boolean; preview?: boolean }
  let tabs = $state<Tab[]>([{ id: 'scratch', title: 'playground.disp', path: SCRATCH_PATH, doc: '', kind: 'scratch' }])
  let activeTabId = $state('scratch')
  const activeTab = () => tabs.find((t) => t.id === activeTabId) ?? tabs[0]
  const tabStates = new Map<string, { state: EditorState; scroll: ReturnType<EditorView['scrollSnapshot']> }>()

  function activate(id: string) {
    const prev = activeTab()
    const view = editorApi?.view()
    const scroll = view?.scrollSnapshot()
    activeTabId = id
    const next = activeTab()
    prev.doc = view?.state.doc.toString() ?? prev.doc
    const saved = tabStates.get(id)
    const old = editorApi?.swapState(saved?.state ?? null, next.doc)
    if (old && scroll) tabStates.set(prev.id, { state: old, scroll })
    if (saved) view?.dispatch({ effects: saved.scroll })
    currentDoc = next.doc
    index = bufferIndex()
    outcome = null
    subject = null
    persistTabs()
    void refreshScope()
    if (live && canAutoRun(next.doc)) void execute(next.doc, 'checking')
  }

  function closeTab(id: string) {
    const idx = tabs.findIndex((t) => t.id === id)
    if (idx < 0 || tabs[idx].kind === 'scratch') return
    if (id === activeTabId) activate(tabs[Math.max(0, idx - 1)].id)
    tabs.splice(tabs.findIndex((t) => t.id === id), 1)
    tabStates.delete(id)
    persistTabs()
  }

  async function openFile(path: string, opts?: { permanent?: boolean; line?: number }) {
    const existing = tabs.find((t) => t.path === path)
    if (existing) {
      if (opts?.permanent) existing.preview = false
      if (existing.id !== activeTabId) activate(existing.id)
    } else {
      const text = await disp.read(path)
      if (text == null) {
        toast(`No such file in the library: ${path}`, 'warn')
        return
      }
      const title = path.split('/').pop() ?? path
      const pv = tabs.find((t) => t.preview)
      if (!opts?.permanent && pv && pv.id !== activeTabId) {
        tabStates.delete(pv.id)
        pv.id = path
        pv.path = path
        pv.title = title
        pv.doc = text
        pv.dirty = false
        activate(path)
      } else if (!opts?.permanent && pv) {
        tabStates.delete(pv.id)
        const fresh: Tab = { id: path, title, path, doc: text, kind: 'file', preview: true }
        const at = tabs.indexOf(pv)
        tabs.splice(at + 1, 0, fresh)
        activate(path)
        tabs.splice(tabs.indexOf(pv), 1)
      } else {
        tabs.push({ id: path, title, path, doc: text, kind: 'file', preview: !opts?.permanent })
        activate(path)
      }
    }
    if (opts?.line) editorApi?.gotoLine(opts.line)
  }

  function resolvePath(rel: string, fromPath: string): string {
    if (rel.startsWith('/')) return rel
    const parts = fromPath.split('/').slice(0, -1)
    for (const seg of rel.split('/')) {
      if (seg === '..') parts.pop()
      else if (seg !== '.') parts.push(seg)
    }
    return parts.join('/')
  }

  // ---- navigation -----------------------------------------------------------
  const navStack: { tab: string; line: number }[] = []
  function pushNav() {
    const v = editorApi?.view()
    if (v) navStack.push({ tab: activeTabId, line: v.state.doc.lineAt(v.state.selection.main.head).number })
    if (navStack.length > 50) navStack.shift()
  }
  function navBack() {
    const to = navStack.pop()
    if (!to) return
    if (to.tab !== activeTabId && tabs.some((t) => t.id === to.tab)) activate(to.tab)
    editorApi?.gotoLine(to.line)
  }
  async function jumpDef(name: string) {
    pushNav()
    const site = await disp.def(name, activeTab().path, editorApi?.getDoc() ?? currentDoc)
    if (!site) {
      navStack.pop()
      toast(`No definition found for ${name}`, 'warn')
      return
    }
    await openFile(site.path, { line: site.line })
    if (site.path === activeTab().path) editorApi?.gotoLine(site.line)
  }
  const goto = (line: number) => editorApi?.gotoLine(line)

  // ---- the library ------------------------------------------------------------
  let filePaths = $state<string[] | null>(null)
  let userFiles = $state<Record<string, string>>({})
  async function refreshFiles() {
    filePaths = await disp.ls()
  }
  function persistUserFiles() {
    try {
      localStorage.setItem(LS_USER, JSON.stringify(userFiles))
    } catch {}
  }
  async function restoreUserFiles() {
    for (const [p, text] of Object.entries(userFiles)) await disp.write(p, text)
  }
  async function createFile(name: string) {
    let n = name.trim()
    if (!n) return
    if (!n.endsWith('.disp')) n += '.disp'
    const path = n.startsWith('/') ? n : `/lib/tests/${n}`
    if ((await disp.read(path)) != null) {
      void openFile(path)
      return
    }
    const base = path.split('/').pop()
    const text = `// ${base}: reach this file from the scratch buffer with use "./${base}"\n\nopen use raw "../prelude.disp" {}\n`
    userFiles[path] = text
    persistUserFiles()
    await disp.write(path, text)
    await refreshFiles()
    void openFile(path, { permanent: true })
  }
  async function deleteFile(path: string) {
    if (!(path in userFiles)) return
    const open = tabs.find((t) => t.path === path)
    if (open) closeTab(open.id)
    delete userFiles[path]
    persistUserFiles()
    await disp.rm(path)
    await refreshFiles()
    toast(`Deleted ${path.split('/').pop()}`)
  }

  // ---- what the editor knows, kept instant --------------------------------------
  let cachedIndexDoc: unknown = null
  let cachedIndex: BufferIndex = { defs: [], tests: [] }
  function bufferIndex(): BufferIndex {
    const v = editorApi?.view()
    if (!v) return cachedIndex
    if (v.state.doc !== cachedIndexDoc) {
      cachedIndexDoc = v.state.doc
      cachedIndex = indexBuffer(v.state.doc)
    }
    return cachedIndex
  }
  let index = $state.raw<BufferIndex>({ defs: [], tests: [] })
  let scopeItems: ScopeItem[] = []
  let scopeKey = ''
  async function refreshScope() {
    const doc = editorApi?.getDoc() ?? currentDoc
    const path = activeTab().path
    const key = path + '\0' + (doc.match(/\bopen\s+use\s+(?:raw\s+)?"[^"]+"/g) ?? []).join('\0')
    if (key === scopeKey) return
    scopeKey = key
    scopeItems = await disp.scope(path, doc)
  }

  const shortPath = (p: string) => p.replace(/^\/lib\//, '')
  const link: PlaygroundLink = {
    index: bufferIndex,
    scope: () => scopeItems,
    paths: () => filePaths ?? [],
    currentPath: () => activeTab().path,
    moduleItems: (rel) => disp.scope(activeTab().path, `open use raw "${rel}"`),
    shortPath,
    jumpDef: (name) => void jumpDef(name),
    back: navBack,
    rename: startRename,
    nextProblem
  }
  const extensions = playgroundExtensions(link)

  // ---- rename, problems -----------------------------------------------------
  let paletteOpen = $state(false)
  let paletteQuery = $state('')
  let palettePrompt = $state<{ title: string; submit: (t: string) => void } | null>(null)
  function startRename() {
    const v = editorApi?.view()
    if (!v) return
    const w = v.state.wordAt(v.state.selection.main.head)
    if (!w) return
    const from = v.state.sliceDoc(w.from, w.to)
    if (!/^[A-Za-z_][\w']*$/.test(from) || from === 't') return
    paletteQuery = from
    palettePrompt = {
      title: `Rename ${from} in this file to`,
      submit: (to) => {
        const name = to.trim()
        if (!name || name === from) return
        if (!/^[A-Za-z_][\w']*$/.test(name)) {
          toast(`${name} isn't a valid name`, 'warn')
          return
        }
        const changes = renameChanges(v.state.doc, from, name)
        v.dispatch({ changes })
        v.focus()
        toast(`Renamed ${changes.length} use${changes.length === 1 ? '' : 's'}`)
      }
    }
    paletteOpen = true
  }
  function closePalette() {
    paletteOpen = false
    palettePrompt = null
    editorApi?.view().focus()
  }
  let paletteFilesFirst = false
  function openPalette(q: string, filesFirst = false) {
    palettePrompt = null
    paletteFilesFirst = filesFirst
    paletteQuery = q
    paletteOpen = true
  }

  const problems = $derived.by(() => {
    const out: { line: number; kind: 'fail' | 'error' }[] = []
    if (outcome?.errorLine) out.push({ line: outcome.errorLine, kind: 'error' })
    for (const t of outcome?.tests ?? []) if (!t.pass && t.line) out.push({ line: t.line, kind: 'fail' })
    return out.sort((a, b) => a.line - b.line)
  })
  function nextProblem() {
    if (problems.length === 0) return
    const v = editorApi?.view()
    const cur = v ? v.state.doc.lineAt(v.state.selection.main.head).number : 0
    goto((problems.find((p) => p.line > cur) ?? problems[0]).line)
  }

  // ---- toasts ---------------------------------------------------------------
  let toasts = $state<{ id: number; text: string; kind: 'info' | 'warn' }[]>([])
  let toastId = 0
  function toast(text: string, kind: 'info' | 'warn' = 'info') {
    const id = ++toastId
    toasts.push({ id, text, kind })
    setTimeout(() => (toasts = toasts.filter((t) => t.id !== id)), 5000)
  }

  // ---- running --------------------------------------------------------------
  let busy = $state(false)
  let live = $state(true)
  let outcome = $state.raw<RunOutcome | null>(null)
  let runVersion = $state(0)
  let progress = $state<{ label: string; count: number; current: string } | null>(null)
  let lastRunMs = 0

  disp.onItem = (e: ItemEvent) => {
    if (e.depth > 0 && progress) {
      progress.count++
      progress.current = `${shortPath(e.sourcePath ?? '')}${e.name ? ' · ' + e.name : ''}`
    }
  }

  const expandValue = (handle: number, rawRoot: boolean) => disp.render(handle, 80, rawRoot)

  function marksFromOutcome(out: RunOutcome): LineMark[] {
    const marks: LineMark[] = []
    for (const d of out.defs) {
      const line = d.endLine ?? d.line
      if (!line) continue
      // a function's raw tree says nothing on its line; the tree view plays it instead
      if (paramsOf(d.name)?.length || paramsOf(d.name.slice(d.name.lastIndexOf('.') + 1))?.length) continue
      if (d.value) marks.push({ line, focusFrom: d.line, kind: 'value', values: [{ node: d.value }] })
      else if (d.pretty) marks.push({ line, focusFrom: d.line, kind: 'value', block: d.pretty })
    }
    for (const t of out.tests) {
      const line = t.endLine ?? t.line
      if (!line) continue
      if (t.pass) marks.push({ line, kind: 'pass', note: '✓' })
      else if (t.lhsNode && t.rhsNode)
        marks.push({
          line,
          focusFrom: t.line,
          kind: 'fail',
          note: '✗',
          values: [
            { label: 'got', node: t.lhsNode },
            { label: 'want', node: t.rhsNode }
          ]
        })
      else marks.push({ line, focusFrom: t.line, kind: 'fail', note: '✗', block: `got  ${t.lhs ?? '?'}\nwant ${t.rhs ?? '?'}` })
    }
    if (out.error && out.errorLine) marks.push({ line: out.errorLine, kind: 'error', block: out.error })
    return marks
  }

  /// the line last typed on: where an error with no position most likely is
  let editLine = 0
  function applyOutcome(out: RunOutcome, coldLoad: boolean) {
    if (out.error && !out.errorLine && editLine)
      out = { ...out, errorLine: editLine, error: `${out.error} (no position given; line ${editLine} is where you last typed)` }
    outcome = out
    lastRunMs = out.elapsedMs
    runVersion++
    editorApi?.setMarks(marksFromOutcome(out))
    const v = editorApi?.view()
    if (v) {
      let max = 1
      for (const c of out.costs ?? []) if (c.steps > max) max = c.steps
      const m = new Map<number, { frac: number; steps: number }>()
      for (const c of out.costs ?? [])
        if (c.steps > 0) m.set(c.line, { frac: Math.log1p(c.steps) / Math.log1p(max), steps: c.steps })
      v.dispatch({ effects: setCosts.of(m) })
    }
    refreshSubject(out)
    updateSubject()
    if (live && !coldLoad && out.elapsedMs > 2500) {
      setLive(false, false)
      toast(`Live checking paused: the last run took ${fmtMs(out.elapsedMs)}. Ctrl+Enter still runs.`)
    }
  }

  async function execute(source: string, label: string) {
    if (busy) return
    busy = true
    const forTab = activeTabId
    progress = { label: disp.kernelLoaded ? label : `${label}: the first run checks the kernel too`, count: 0, current: '' }
    try {
      const out = await disp.run(source, { wantDefPretty: true, path: activeTab().path })
      if (forTab === activeTabId) applyOutcome(out, (progress?.count ?? 0) > 100)
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e)
      if (msg !== 'interrupted') toast(msg, 'warn')
    } finally {
      progress = null
      busy = false
    }
  }

  const runFile = () => void execute(editorApi?.getDoc() ?? currentDoc, 'running')
  const runToCursor = () => {
    const prefix = editorApi?.getDocToCursor()
    if (prefix?.trim()) void execute(prefix, 'running to the cursor')
  }

  // a cold session never starts the kernel load uninvited; raw buffers are instant
  // a typed `use` needs the kernel; the word in a comment doesn't count
  const code = (doc: string) => doc.replace(/\/\/.*$/gm, '')
  /// a module that declares `given` dependencies only runs through a file that opens it
  const isDependent = (doc: string) => /^\s*(?:open\s+)?given\b/m.test(code(doc))
  const canAutoRun = (doc: string) => !isDependent(doc) && (disp.kernelLoaded || !/\buse\s+(?!raw\b)/.test(code(doc)))
  let editTimer: ReturnType<typeof setTimeout> | undefined
  let indexTimer: ReturnType<typeof setTimeout> | undefined
  function onEdit(doc: string) {
    currentDoc = doc
    const v = editorApi?.view()
    if (v) editLine = v.state.doc.lineAt(v.state.selection.main.head).number
    const tab = activeTab()
    if (doc !== tab.doc) {
      tab.doc = doc
      if (tab.kind === 'file') {
        tab.dirty = true
        tab.preview = false
        void disp.write(tab.path, doc)
        if (tab.path in userFiles) {
          userFiles[tab.path] = doc
          persistUserFiles()
        }
      }
    }
    if (tab.kind === 'scratch') persist()
    clearTimeout(indexTimer)
    indexTimer = setTimeout(() => {
      index = bufferIndex()
      void refreshScope()
    }, 90)
    if (!live) return
    clearTimeout(editTimer)
    // re-check as soon as typing pauses, later when runs are slow
    editTimer = setTimeout(
      () => {
        if (!live || disp.status === 'dead' || !canAutoRun(currentDoc)) return
        if (busy) return onEdit(currentDoc)
        void execute(currentDoc, 'checking')
      },
      Math.min(450, Math.max(90, lastRunMs * 1.5))
    )
  }

  function setLive(v: boolean, save = true) {
    live = v
    if (save)
      try {
        localStorage.setItem(LS_LIVE, v ? '1' : '0')
      } catch {}
    if (!v) clearTimeout(editTimer)
    else {
      const doc = editorApi?.getDoc() ?? currentDoc
      if (canAutoRun(doc) && !busy) void execute(doc, 'checking')
    }
  }

  async function preloadKernel() {
    if (busy || disp.kernelLoaded) return
    busy = true
    progress = { label: 'elaborating and self-verifying the kernel', count: 0, current: '' }
    try {
      const out = await disp.loadKernel()
      toast(out.ok ? `Kernel checked in ${(out.elapsedMs / 1000).toFixed(1)}s. Later runs reuse it.` : `Kernel load failed: ${out.error}`, out.ok ? 'info' : 'warn')
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e)
      if (msg !== 'interrupted') toast(msg, 'warn')
    } finally {
      progress = null
      busy = false
    }
    if (live && disp.kernelLoaded) void execute(editorApi?.getDoc() ?? currentDoc, 'checking')
  }

  /// Drop the precompiled kernel and watch the type system really re-check itself.
  async function verifyKernel() {
    if (busy) return
    busy = true
    try {
      await disp.reset({ fromSource: true })
      editorApi?.setMarks([])
      outcome = null
      subject = null
      toast('Precompiled kernel dropped. Re-checking from source.')
    } finally {
      busy = false
    }
    await preloadKernel()
  }

  function stop() {
    disp.interrupt()
    progress = null
    busy = false
    editorApi?.setMarks([])
    outcome = null
    subject = null
    void restoreUserFiles()
    toast('Stopped. The engine was reset.', 'warn')
  }

  async function resetSession() {
    if (busy) return
    await disp.reset()
    outcome = null
    subject = null
    editorApi?.setMarks([])
    toast(disp.kernelLoaded ? 'Fresh session, with the precompiled kernel.' : 'Fresh session. The kernel cache is empty.')
  }

  function loadExample(id: string) {
    const ex = examples.find((x) => x.id === id)
    if (!ex) return
    exampleId = id
    if (activeTabId !== 'scratch') activate('scratch')
    editorApi?.setDoc(ex.source)
    currentDoc = ex.source
    outcome = null
    subject = null
    index = bufferIndex()
    void refreshScope()
    persist()
    if (live && canAutoRun(ex.source) && !busy) void execute(ex.source, 'checking the example')
  }

  async function share() {
    const doc = editorApi?.getDoc() ?? currentDoc
    history.replaceState(null, '', `#${await encodeShared(doc)}`)
    try {
      await navigator.clipboard.writeText(location.href)
      toast('Link copied. The code travels in the URL.')
    } catch {
      toast('Link ready in the address bar.')
    }
  }

  // ---- the inspector's subject ----------------------------------------------
  let subject = $state.raw<Subject | null>(null)
  let pinned = $state(false)
  let glow = $state<string | null>(null)
  let cursorLine = $state(1)
  let cursorCol = $state(1)
  let cursorTimer: ReturnType<typeof setTimeout> | undefined

  function onCursor(v: EditorView) {
    const head = v.state.selection.main.head
    const line = v.state.doc.lineAt(head)
    cursorLine = line.number
    cursorCol = head - line.from + 1
    clearTimeout(cursorTimer)
    cursorTimer = setTimeout(updateSubject, 70)
  }

  function balanced(text: string): boolean {
    const stack: string[] = []
    const closes: Record<string, string> = { ')': '(', '}': '{', ']': '[' }
    for (const c of text.replace(/"[^"]*"/g, '')) {
      if (c === '(' || c === '{' || c === '[') stack.push(c)
      else if (closes[c] && stack.pop() !== closes[c]) return false
    }
    return stack.length === 0
  }

  function exprSubject(expr: string): Subject {
    return { key: `expr:${expr}`, kind: 'expr', title: expr, term: { expr } }
  }

  function updateSubject() {
    const v = editorApi?.view()
    if (!v) return
    const sel = v.state.selection.main
    const w = v.state.wordAt(sel.head)
    glow = w && sel.empty ? v.state.sliceDoc(w.from, w.to) : null
    if (pinned) return
    if (!sel.empty) {
      const text = v.state.sliceDoc(sel.from, sel.to).trim()
      // an expression, not a run of items or prose in a comment
      if (text && text.length < 500 && balanced(text) && !/^\/\//.test(text) && !/(^|\n)\s*[\w']+\s*:=/.test(text) && !/^\s*test\b/.test(text)) {
        const s = exprSubject(text)
        if (s.key !== subject?.key) setSubject(s)
      }
      return
    }
    const out = outcome
    if (!out) return
    const n = cursorLine
    const inRange = (a?: number, b?: number) => a != null && n >= a && n <= (b ?? a)
    const test = out.tests.find((t) => inRange(t.line, t.endLine))
    if (test) {
      const src = v.state.doc.line(Math.min(test.line!, v.state.doc.lines)).text.replace(/\s*\/\/.*$/, '')
      setSubject({
        key: `test:${test.line}:${test.lhsH}:${test.rhsH}`,
        kind: 'test',
        title: src,
        line: test.line,
        endLine: test.endLine,
        lhsH: test.lhsH,
        rhsH: test.rhsH,
        pass: test.pass,
        term: { path: activeTab().path, text: v.state.doc.toString(), line: test.line, side: 'lhs', value: test.lhsH }
      })
      return
    }
    const def = out.defs.find((d) => inRange(d.line, d.endLine))
    if (def && def.h != null) {
      setSubject({
        key: `def:${def.name}:${def.line}:${def.h}`,
        kind: 'def',
        title: def.name,
        line: def.line,
        endLine: def.endLine,
        h: def.h,
        term: { path: activeTab().path, text: v.state.doc.toString(), line: def.line, value: def.h }
      })
    }
  }

  /// A run may have renamed, changed or removed what's being inspected.
  function refreshSubject(out: RunOutcome) {
    const s = subject
    if (!s || pinned || s.kind === 'expr') return
    const v = editorApi?.view()
    if (!v) return
    if (s.kind === 'def') {
      const d = out.defs.find((x) => x.name === s.title)
      if (d?.h == null) {
        subject = null
        v.dispatch({ effects: setSubjectLines.of(null) })
      } else
        setSubject({ ...s, key: `def:${d.name}:${d.line}:${d.h}`, line: d.line, endLine: d.endLine, h: d.h, term: { ...s.term, text: v.state.doc.toString(), line: d.line, value: d.h } })
    } else if (!out.tests.some((t) => t.line === s.line)) {
      subject = null
      v.dispatch({ effects: setSubjectLines.of(null) })
    }
  }

  function setSubject(s: Subject) {
    if (s.key === subject?.key) return
    subject = s
    editorApi?.view().dispatch({ effects: setSubjectLines.of(s.line ? { from: s.line, to: s.endLine ?? s.line } : null) })
  }

  const paramsOf = (name: string): string[] | undefined =>
    bufferIndex().defs.findLast((d) => d.name === name)?.params ?? scopeItems.find((s) => s.name === name)?.params

  /// The first test that calls this definition shows it working.
  function suggestRun(s: Subject): string | null {
    if (s.kind !== 'def') return null
    const re = new RegExp(`^test\\s+(${s.title.replace(/'/g, "\\'")}\\([^=]*?)\\s*=(?!>)`)
    for (const t of bufferIndex().tests) {
      const m = re.exec(t.text)
      if (m) return m[1]
    }
    return null
  }

  function togglePin() {
    pinned = !pinned
    if (!pinned) updateSubject()
  }
  function inspectExpr(expr: string) {
    pinned = true
    setSubject(exprSubject(expr))
  }
  function onTreeGlow(name: string | null) {
    editorApi?.view().dispatch({ effects: setGlowName.of(name) })
  }

  // ---- the outline ----------------------------------------------------------
  const outline = $derived.by<OutlineRow[]>(() => {
    const rows: OutlineRow[] = []
    const costs = new Map((outcome?.costs ?? []).map((c) => [c.line, c.steps]))
    let max = 1
    for (const c of costs.values()) if (c > max) max = c
    const frac = (c: number) => (c > 0 ? Math.log1p(c) / Math.log1p(max) : 0)
    const byLine = new Map((outcome?.defs ?? []).map((d) => [d.line, d]))
    for (const d of index.defs) {
      if (d.of) continue
      const run = byLine.get(d.line)
      const c = costs.get(d.line) ?? 0
      const pretty = run?.pretty
      rows.push({
        line: d.line,
        endLine: d.endLine,
        kind: d.decor === 'let' ? 'let' : 'def',
        name: d.name,
        value: pretty && pretty.length < 40 && !d.params?.length ? pretty : undefined,
        cost: c,
        costFrac: frac(c),
        params: d.params
      })
    }
    const testByLine = new Map((outcome?.tests ?? []).map((t) => [t.line, t]))
    for (const t of index.tests) {
      const run = testByLine.get(t.line)
      const c = costs.get(t.line) ?? 0
      rows.push({
        line: t.line,
        endLine: t.endLine,
        kind: 'test',
        name: t.text.replace(/^test\s+/, ''),
        pass: run?.pass,
        cost: c,
        costFrac: frac(c)
      })
    }
    return rows.sort((a, b) => a.line - b.line)
  })
  const openTabPaths = $derived(new Set(tabs.map((t) => t.path)))

  // ---- layout ---------------------------------------------------------------
  let sideOpen = $state(true)
  let sideTab = $state<'outline' | 'files' | 'examples'>('outline')
  let inspOpen = $state(true)
  let sideW = $state(232)
  let inspW = $state(0.42)
  // narrow screens show one pane at a time
  let pane = $state<'code' | 'tree' | 'side'>('code')
  let shellW = $state(1200)

  function persistLayout() {
    try {
      localStorage.setItem(LS_LAYOUT, JSON.stringify({ sideOpen, sideW, inspOpen, inspW, sideTab }))
    } catch {}
  }
  let shell = $state<HTMLDivElement>()
  function drag(which: 'side' | 'insp', e: PointerEvent) {
    e.preventDefault()
    const el = e.currentTarget as HTMLElement
    el.setPointerCapture(e.pointerId)
    const move = (ev: PointerEvent) => {
      const r = shell!.getBoundingClientRect()
      if (which === 'side') sideW = Math.max(150, Math.min(460, ev.clientX - r.left))
      else inspW = Math.max(0.2, Math.min(0.75, (r.right - ev.clientX) / r.width))
    }
    const up = () => {
      el.removeEventListener('pointermove', move)
      el.removeEventListener('pointerup', up)
      persistLayout()
    }
    el.addEventListener('pointermove', move)
    el.addEventListener('pointerup', up)
  }

  // ---- the palette's contents -----------------------------------------------
  const mod = typeof navigator !== 'undefined' && /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘' : 'Ctrl+'
  function commands(): PaletteItem[] {
    return [
      { id: 'run', title: 'Run file', hint: `${mod}Enter`, run: runFile },
      { id: 'cursor', title: 'Run to cursor', hint: 'Shift+Enter', run: runToCursor },
      { id: 'live', title: live ? 'Turn off live checking' : 'Turn on live checking', run: () => setLive(!live) },
      { id: 'share', title: 'Copy a share link', detail: 'the code travels in the URL', run: () => void share() },
      { id: 'side', title: sideOpen ? 'Hide the sidebar' : 'Show the sidebar', hint: `${mod}B`, run: () => toggleSide() },
      { id: 'insp', title: inspOpen ? 'Hide the tree view' : 'Show the tree view', hint: `${mod}J`, run: () => toggleInsp() },
      { id: 'pin', title: pinned ? 'Tree view: follow the cursor' : 'Tree view: stay on this one', run: togglePin },
      { id: 'rename', title: 'Rename the name under the cursor', hint: 'F2', run: startRename },
      { id: 'def', title: 'Go to definition', hint: 'F12', run: () => {
        const v = editorApi?.view()
        const w = v?.state.wordAt(v.state.selection.main.head)
        if (v && w) void jumpDef(v.state.sliceDoc(w.from, w.to))
      } },
      { id: 'back', title: 'Go back', hint: 'Alt+←', run: navBack },
      { id: 'problem', title: 'Next problem', hint: 'F8', run: nextProblem },
      { id: 'reset', title: 'Reset the session', detail: 'a fresh arena', run: () => void resetSession() },
      { id: 'kernel', title: 'Verify the kernel from source', detail: 'about a minute of the type system checking itself', run: () => void verifyKernel() },
      { id: 'about', title: 'About this playground', run: () => (aboutOpen = true) }
    ]
  }
  function fuzzy(q: string, s: string): number {
    if (!q) return 1
    const t = s.toLowerCase()
    const i = t.indexOf(q)
    if (i >= 0) return 1000 - i - t.length * 0.01
    let at = 0
    let score = 0
    for (const ch of q) {
      const f = t.indexOf(ch, at)
      if (f < 0) return 0
      score += f === at ? 3 : 1
      at = f + 1
    }
    return score
  }
  function rank(q: string, items: PaletteItem[]): PaletteItem[] {
    return items
      .map((it) => ({ it, s: fuzzy(q, it.title) + 0.3 * fuzzy(q, it.detail ?? '') }))
      .filter((x) => x.s > 0)
      .sort((a, b) => b.s - a.s)
      .map((x) => x.it)
  }
  function paletteSearch(raw: string): PaletteItem[] {
    if (raw.startsWith('@')) {
      const q = raw.slice(1).trim().toLowerCase()
      const own = bufferIndex().defs.map((d) => ({ id: `d${d.line}`, title: d.name, detail: `line ${d.line}`, run: () => goto(d.line) }))
      const ext = scopeItems.map((s) => ({ id: `s${s.path}:${s.name}`, title: s.name, detail: shortPath(s.path), run: () => void openFile(s.path, { line: s.line }) }))
      return rank(q, [...own, ...ext])
    }
    const q = raw.trim().toLowerCase()
    const files = (filePaths ?? []).map((p) => ({ id: p, title: p.split('/').pop() ?? p, detail: shortPath(p), run: () => void openFile(p) }))
    const exs = examples.map((x) => ({ id: `ex:${x.id}`, title: x.label, detail: 'example', run: () => loadExample(x.id) }))
    return rank(q, paletteFilesFirst ? [...files, ...exs, ...commands()] : [...commands(), ...exs, ...files])
  }
  // on a narrow screen these switch the one pane shown instead
  function toggleSide() {
    if (narrow) return void (pane = pane === 'side' ? 'code' : 'side')
    sideOpen = !sideOpen
    persistLayout()
  }
  function toggleInsp() {
    if (narrow) return void (pane = pane === 'tree' ? 'code' : 'tree')
    inspOpen = !inspOpen
    persistLayout()
  }

  function onWindowKey(e: KeyboardEvent) {
    const m = e.ctrlKey || e.metaKey
    if (m && !e.altKey) {
      const k = e.key.toLowerCase()
      if (k === 'k' || (k === 'p' && e.shiftKey)) {
        e.preventDefault()
        openPalette('')
      } else if (k === 'p') {
        e.preventDefault()
        openPalette('', true)
      } else if (k === 'o' && e.shiftKey) {
        e.preventDefault()
        openPalette('@')
      } else if (k === 'b') {
        e.preventDefault()
        toggleSide()
      } else if (k === 'j') {
        e.preventDefault()
        toggleInsp()
      }
    }
    if ((e.key === 'w' || e.key === 'W') && !e.shiftKey && (e.ctrlKey || e.metaKey || e.altKey)) {
      e.preventDefault()
      const t = activeTab()
      if (t.kind === 'file') closeTab(t.id)
    }
  }

  // ---- persistence and boot -------------------------------------------------
  function persist() {
    try {
      localStorage.setItem(LS_DOC, currentDoc)
      localStorage.setItem(EXAMPLE_STORAGE_KEY, exampleId)
    } catch {}
  }
  function persistTabs() {
    try {
      localStorage.setItem(LS_TABS, JSON.stringify(tabs.filter((t) => t.kind === 'file').map((t) => t.path)))
    } catch {}
  }
  let aboutOpen = $state(false)
  function dismissAbout() {
    aboutOpen = false
    try {
      localStorage.setItem(LS_WELCOME, '1')
    } catch {}
  }

  onMount(() => {
    void (async () => {
      try {
        live = localStorage.getItem(LS_LIVE) !== '0'
        aboutOpen = !localStorage.getItem(LS_WELCOME)
        userFiles = JSON.parse(localStorage.getItem(LS_USER) ?? '{}') as Record<string, string>
        const l = JSON.parse(localStorage.getItem(LS_LAYOUT) ?? '{}')
        sideOpen = l.sideOpen ?? true
        sideW = l.sideW ?? 232
        inspOpen = l.inspOpen ?? true
        inspW = l.inspW ?? 0.42
        sideTab = l.sideTab ?? 'outline'
      } catch {}
      const hash = location.hash.slice(1)
      let loaded = false
      if (isSharedHash(hash)) {
        const doc = await decodeShared(hash)
        if (doc != null) {
          currentDoc = doc
          editorApi?.setDoc(doc)
          loaded = true
        }
      }
      const wanted = new URLSearchParams(location.search).get('example')
      if (!loaded && wanted && examples.some((x) => x.id === wanted)) {
        loadExample(wanted)
        loaded = true
      }
      if (!loaded) {
        try {
          const saved = localStorage.getItem(LS_DOC)
          const savedEx = localStorage.getItem(EXAMPLE_STORAGE_KEY)
          if (savedEx && examples.some((x) => x.id === savedEx)) exampleId = savedEx
          if (saved) {
            currentDoc = saved
            editorApi?.setDoc(saved)
          } else currentDoc = initialDoc
        } catch {
          currentDoc = initialDoc
        }
      }
      index = bufferIndex()
      try {
        await disp.init()
      } catch {
        return
      }
      await restoreUserFiles()
      void refreshFiles()
      void refreshScope()
      try {
        const saved = JSON.parse(localStorage.getItem(LS_TABS) ?? '[]') as string[]
        for (const p of saved) {
          if (tabs.some((t) => t.path === p)) continue
          const text = await disp.read(p)
          if (text != null) tabs.push({ id: p, title: p.split('/').pop() ?? p, path: p, doc: text, kind: 'file' })
        }
      } catch {}
      const doc = editorApi?.getDoc() ?? currentDoc
      if (live && canAutoRun(doc) && !busy) void execute(doc, 'checking')
    })()
    return () => {
      clearTimeout(editTimer)
      clearTimeout(indexTimer)
      clearTimeout(cursorTimer)
    }
  })

  // ---- formatting -----------------------------------------------------------
  const fmtMs = (ms: number) => (ms >= 1000 ? (ms / 1000).toFixed(1) + 's' : ms.toFixed(0) + 'ms')
  const fmtSteps = (n: number) => (n >= 1e9 ? (n / 1e9).toFixed(1) + 'B' : n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(1) + 'k' : String(n))
  const passCount = $derived(outcome?.tests.filter((t) => t.pass).length ?? 0)
  const failCount = $derived(outcome?.tests.filter((t) => !t.pass).length ?? 0)
  const memPct = $derived(Math.min(100, (disp.memBytes / (4096 * 1024 * 1024)) * 100))
  const narrow = $derived(shellW < 820)
</script>

<svelte:head>
  <title>Playground · disp</title>
</svelte:head>

<svelte:window
  onkeydown={onWindowKey}
  onpointerdown={(e) => {
    if (aboutOpen && !(e.target as HTMLElement).closest('.about')) dismissAbout()
  }}
/>

<div class="pg" bind:this={shell} bind:clientWidth={shellW} class:narrow data-pane={pane}>
  <div class="bar">
    <button class="ibtn" class:on={narrow ? pane === 'side' : sideOpen} onclick={toggleSide} title="Sidebar ({mod}B)" aria-label="toggle sidebar" aria-pressed={sideOpen}>
      <svg viewBox="0 0 16 16" aria-hidden="true"><rect x="1.5" y="2.5" width="13" height="11" rx="1.5" fill="none" stroke="currentColor" stroke-width="1.3" /><path d="M6 3v10" stroke="currentColor" stroke-width="1.3" /></svg>
    </button>
    <div class="tabs" role="tablist">
      {#each tabs as tb (tb.id)}
        <div
          class="tab"
          class:active={tb.id === activeTabId}
          class:preview={tb.preview}
          role="tab"
          aria-selected={tb.id === activeTabId}
          tabindex="0"
          title={tb.path}
          onclick={() => tb.id !== activeTabId && activate(tb.id)}
          ondblclick={() => (tb.preview = false)}
          onkeydown={(e) => e.key === 'Enter' && tb.id !== activeTabId && activate(tb.id)}
        >
          <span>{tb.title}</span>
          {#if tb.dirty}<span class="dirty" title="Edited for this session only">●</span>{/if}
          {#if tb.kind === 'file'}
            <button class="x" onclick={(e) => { e.stopPropagation(); closeTab(tb.id) }} aria-label="close {tb.title}">×</button>
          {/if}
        </div>
      {/each}
    </div>
    <div class="actions">
      <button class="btn-run" onclick={runFile} disabled={busy} title="Run ({mod}Enter)">▶ Run</button>
      <button class="ibtn word" onclick={() => void share()} title="Copy a link with this code in it">Share</button>
      <button class="ibtn word kbdish" onclick={() => openPalette('')} title="Everything else">{mod}K</button>
      <button class="ibtn" class:on={narrow ? pane === 'tree' : inspOpen} onclick={toggleInsp} title="Tree view ({mod}J)" aria-label="toggle tree view" aria-pressed={inspOpen}>
        <svg viewBox="0 0 16 16" aria-hidden="true"><circle cx="8" cy="3.4" r="1.7" fill="currentColor" /><circle cx="4" cy="12" r="1.7" fill="currentColor" /><circle cx="12" cy="12" r="1.7" fill="currentColor" /><path d="M8 4 4 12M8 4l4 8" stroke="currentColor" stroke-width="1.3" /></svg>
      </button>
    </div>
  </div>

  <div class="main">
    {#if sideOpen || (narrow && pane === 'side')}
      <div class="sidepane" style="width:{narrow ? '100%' : sideW + 'px'}">
        <Sidebar
          tab={sideTab}
          rows={outline}
          activeLine={cursorLine}
          {filePaths}
          openPaths={openTabPaths}
          {userFiles}
          activePath={activeTab().path}
          {examples}
          {exampleId}
          onTab={(t) => {
            sideTab = t
            persistLayout()
            if (t === 'files' && filePaths == null) void refreshFiles()
          }}
          onGoto={(l) => {
            pane = 'code'
            goto(l)
          }}
          onOpenFile={(p, permanent) => {
            pane = 'code'
            void openFile(p, { permanent })
          }}
          onNewFile={(n) => void createFile(n)}
          onDeleteFile={(p) => void deleteFile(p)}
          onExample={(id) => {
            pane = 'code'
            loadExample(id)
          }}
        />
      </div>
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <div class="grip" onpointerdown={(e) => drag('side', e)}></div>
    {/if}
    <div class="codepane">
      <DispEditor
        doc={currentDoc || initialDoc}
        onDocChange={onEdit}
        onRunFile={runFile}
        onRunToCursor={runToCursor}
        {expandValue}
        onOpenPath={(rel) => void openFile(resolvePath(rel, activeTab().path))}
        onJumpDef={(n) => void jumpDef(n)}
        onResolveIdent={(n) => disp.def(n, activeTab().path, editorApi?.getDoc() ?? currentDoc).then((s) => s !== null)}
        {extensions}
        ownHover
        {onCursor}
        api={(a) => (editorApi = a)}
      />
      {#if problems.length > 0 && editorApi}
        <div class="ruler" aria-hidden="true">
          {#each problems as p (`${p.kind}${p.line}`)}
            <button class="tick {p.kind}" tabindex="-1" style="top:{((p.line - 1) / Math.max(1, editorApi.view().state.doc.lines)) * 100}%" onclick={() => goto(p.line)} title="line {p.line}" aria-label="go to line {p.line}"></button>
          {/each}
        </div>
      {/if}
      {#if outcome?.error && !outcome.errorLine}
        <div class="errstrip" title={outcome.error}>⚠ {outcome.error}</div>
      {/if}
      <div class="toasts">
        {#each toasts as t (t.id)}
          <div class="toast" class:warn={t.kind === 'warn'}>{t.text}</div>
        {/each}
      </div>
    </div>
    {#if inspOpen || (narrow && pane === 'tree')}
      {#if !narrow}
        <!-- svelte-ignore a11y_no_static_element_interactions -->
        <div class="grip" onpointerdown={(e) => drag('insp', e)}></div>
      {/if}
      <div class="insppane" style="width:{narrow ? '100%' : inspW * 100 + '%'}">
        <Inspector
          {subject}
          {runVersion}
          {pinned}
          {glow}
          suggest={suggestRun}
          paramsOf={paramsOf}
          notRun={!outcome && !busy && disp.status === 'ready'}
          notRunWhy={isDependent(currentDoc) ? "This file declares what it's given, so it only runs through a file that opens it, like playground.disp." : undefined}
          fileError={outcome?.error ? { line: outcome.errorLine } : null}
          onPinToggle={togglePin}
          onExpr={inspectExpr}
          onGlow={onTreeGlow}
          onGoto={goto}
        />
      </div>
    {/if}
  </div>

  <footer class="status">
    <span class="state" class:busy class:dead={disp.status === 'dead'} class:bad={!!outcome?.error || failCount > 0}><i></i></span>
    {#if busy && progress && progress.count > 0}
      <span class="prog" title={progress.label}>{progress.count} items · {progress.current}</span>
      <button class="stlink" onclick={stop}>Stop</button>
    {:else if busy}
      <span class="dim">{progress?.label ?? 'running'}…</span>
    {:else if disp.status === 'dead'}
      <span class="bad">Engine crashed.</span>
      <button class="stlink" onclick={() => void resetSession()}>Reset</button>
    {:else if disp.status === 'cold' || disp.status === 'booting' || disp.status === 'restoring-kernel'}
      <span class="dim">Starting the engine…</span>
    {:else if outcome?.error}
      <button class="stlink bad" onclick={nextProblem} title={outcome.error}>{outcome.errorLine ? `Error on line ${outcome.errorLine}` : 'Error'}</button>
    {:else if outcome}
      <span class="dim">Checked in {fmtMs(outcome.elapsedMs)}</span>
    {:else}
      <span class="dim">Ready</span>
    {/if}
    {#if outcome}
      <span class="sep">·</span><span class="dim">{outcome.defs.length} defs</span>
      {#if outcome.tests.length > 0}
        <span class="sep">·</span>
        <button class="stlink" class:quiet={failCount === 0} onclick={nextProblem} title="Next problem (F8)">
          <span class="good">✓ {passCount}</span>{#if failCount > 0}&nbsp;<span class="bad">✗ {failCount}</span>{/if}
        </button>
      {/if}
      {#if outcome.steps > 0}<span class="sep">·</span><span class="dim" title="Evaluator steps this run">{fmtSteps(outcome.steps)} {outcome.steps === 1 ? 'step' : 'steps'}</span>{/if}
    {/if}
    <span class="grow"></span>
    {#if !disp.kernelLoaded && !busy && disp.status === 'ready'}
      <button class="stlink" onclick={() => void preloadKernel()}>Load kernel</button>
    {/if}
    <button class="stlink" class:quiet={live} onclick={() => setLive(!live)} title="Re-check as you type" aria-pressed={live}>
      <i class="livedot" class:on={live}></i>Live
    </button>
    {#if disp.memBytes > 0}
      <span class="memw" title="WASM memory: {(disp.memBytes / 1e9).toFixed(2)}GB of 4GB"><span class="memf" class:hot={memPct > 85} style="width:{memPct}%"></span></span>
    {/if}
    <span class="dim pos">Ln {cursorLine}, Col {cursorCol}</span>
    <button class="stlink quiet" onclick={() => (aboutOpen = true)} aria-label="about this playground">?</button>
  </footer>

  {#if narrow}
    <nav class="panes" aria-label="panes">
      <button class:on={pane === 'side'} onclick={() => (pane = 'side')}>Outline</button>
      <button class:on={pane === 'code'} onclick={() => (pane = 'code')}>Code</button>
      <button class:on={pane === 'tree'} onclick={() => (pane = 'tree')}>Tree</button>
    </nav>
  {/if}

  <Palette open={paletteOpen} bind:query={paletteQuery} prompt={palettePrompt} search={paletteSearch} onClose={closePalette} />

  {#if aboutOpen}
    <div class="about" role="dialog" aria-label="about this playground">
      <button class="aboutx" onclick={dismissAbout} aria-label="dismiss">×</button>
      <h2>The real toolchain, in your tab</h2>
      <p>This runs disp's actual elaborator and evaluator, compiled to WebAssembly. Nothing goes to a server.</p>
      <p>Each definition shows its value on its line and each test its verdict, and everything re-checks as you type. The panel on the right plays whatever is under your cursor as trees (the Tree tab on a phone), one step at a time: each binder turning into S, K and I (themselves small trees), and the tree reducing by its rules. Select any expression to see just that part.</p>
      <p>The kernel arrives precompiled so typed code starts fast. Don't trust it? <button class="stlink" onclick={() => { dismissAbout(); void verifyKernel() }}>Verify it from source</button>: about a minute of the type system checking itself.</p>
      <p class="keys"><kbd>{mod}K</kbd> commands <kbd>{mod}P</kbd> files <kbd>{mod}Enter</kbd> run <kbd>F12</kbd> definition <kbd>F2</kbd> rename <kbd>F8</kbd> next problem <kbd>←</kbd><kbd>→</kbd> step through the tree</p>
    </div>
  {/if}
</div>

<style>
  .pg {
    display: flex;
    flex-direction: column;
    flex: 1;
    min-height: 0;
    background: var(--bg-code);
  }
  .bar {
    flex: none;
    display: flex;
    align-items: stretch;
    gap: 0.35rem;
    padding: 0.22rem 0.45rem 0;
    background: var(--bg-elev);
    border-bottom: 1px solid var(--border);
  }
  .ibtn {
    flex: none;
    display: grid;
    place-items: center;
    align-self: center;
    min-width: 1.7rem;
    height: 1.7rem;
    padding: 0;
    margin-bottom: 0.2rem;
    background: none;
    border: none;
    border-radius: 6px;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .ibtn svg {
    width: 1rem;
    height: 1rem;
  }
  .ibtn:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .ibtn.on {
    color: var(--accent);
  }
  .ibtn.word {
    padding: 0 0.5rem;
    font-size: 0.76rem;
    color: var(--fg-muted);
  }
  .kbdish {
    font-family: var(--font-mono);
    border: 1px solid var(--border) !important;
    height: 1.45rem !important;
    font-size: 0.68rem !important;
  }
  .tabs {
    flex: 1;
    min-width: 0;
    display: flex;
    align-items: flex-end;
    gap: 2px;
    overflow-x: auto;
    scrollbar-width: none;
  }
  .tab {
    display: inline-flex;
    align-items: center;
    gap: 0.35em;
    padding: 0.28rem 0.7rem;
    font-family: var(--font-mono);
    font-size: 0.75rem;
    color: var(--fg-muted);
    border: 1px solid transparent;
    border-bottom: none;
    border-radius: 7px 7px 0 0;
    cursor: pointer;
    white-space: nowrap;
    user-select: none;
  }
  .tab:hover {
    background: var(--bg-panel-hover);
  }
  .tab.active {
    background: var(--bg-code);
    border-color: var(--border);
    color: var(--fg);
    margin-bottom: -1px;
    padding-bottom: calc(0.28rem + 1px);
  }
  .tab.preview > span:first-child {
    font-style: italic;
  }
  .dirty {
    color: var(--warn);
    font-size: 0.55rem;
  }
  .x {
    background: none;
    border: none;
    padding: 0 0.1em;
    color: var(--fg-faint);
    font-size: 0.92rem;
    line-height: 1;
    cursor: pointer;
    border-radius: 4px;
  }
  .x:hover {
    color: var(--err);
  }
  .actions {
    flex: none;
    display: flex;
    align-items: center;
    gap: 0.25rem;
  }
  .btn-run {
    align-self: center;
    margin-bottom: 0.2rem;
    padding: 0.18rem 0.65rem;
    background: var(--accent);
    color: var(--bg);
    border: none;
    border-radius: 6px;
    font-size: 0.78rem;
    font-weight: 600;
    cursor: pointer;
  }
  .btn-run:disabled {
    opacity: 0.55;
    cursor: default;
  }
  .main {
    flex: 1;
    min-height: 0;
    display: flex;
    position: relative;
  }
  .sidepane {
    flex: none;
    min-height: 0;
  }
  .codepane {
    flex: 1;
    min-width: 0;
    min-height: 0;
    position: relative;
  }
  .insppane {
    flex: none;
    min-width: 0;
    min-height: 0;
  }
  .grip {
    flex: none;
    width: 5px;
    margin: 0 -2px;
    z-index: 4;
    cursor: col-resize;
    position: relative;
    touch-action: none;
  }
  .grip::after {
    content: '';
    position: absolute;
    inset: 0 2px;
    background: var(--border);
    transition: background 0.1s;
  }
  .grip:hover::after {
    background: var(--accent);
  }
  .ruler {
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    width: 7px;
    z-index: 3;
    pointer-events: none;
  }
  .tick {
    position: absolute;
    right: 0;
    width: 7px;
    height: 4px;
    padding: 0;
    border: none;
    border-radius: 1px;
    pointer-events: auto;
    cursor: pointer;
  }
  .tick.fail,
  .tick.error {
    background: var(--err);
  }
  .errstrip {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    z-index: 5;
    padding: 0.4rem 0.8rem;
    font-family: var(--font-mono);
    font-size: 0.75rem;
    color: var(--err);
    background: color-mix(in oklab, var(--err) 8%, var(--bg-elev));
    border-top: 1px solid var(--border);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .toasts {
    position: absolute;
    top: 0.5rem;
    right: 0.9rem;
    z-index: 8;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 0.35rem;
    pointer-events: none;
    max-width: min(26rem, calc(100% - 2rem));
  }
  .toast {
    padding: 0.4rem 0.7rem;
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    background: var(--bg-elev);
    box-shadow: var(--shadow-soft);
    font-size: 0.78rem;
    color: var(--fg-muted);
  }
  .toast.warn {
    border-color: color-mix(in oklab, var(--err) 40%, transparent);
    color: var(--err);
  }
  .status {
    flex: none;
    display: flex;
    align-items: center;
    gap: 0.4rem;
    min-height: 1.5rem;
    padding: 0 0.6rem;
    background: var(--bg-elev);
    border-top: 1px solid var(--border);
    font-size: 0.72rem;
    color: var(--fg-muted);
    white-space: nowrap;
    overflow: hidden;
  }
  .state i {
    display: block;
    width: 7px;
    height: 7px;
    border-radius: 50%;
    background: var(--ok);
  }
  .state.bad i,
  .state.dead i {
    background: var(--err);
  }
  .state.busy i {
    background: var(--accent);
    animation: pulse 1s ease-in-out infinite;
  }
  @keyframes pulse {
    50% {
      opacity: 0.3;
    }
  }
  .dim {
    color: var(--fg-faint);
  }
  .sep {
    color: var(--fg-faint);
  }
  .good {
    color: var(--ok);
  }
  .bad {
    color: var(--err);
  }
  .grow {
    flex: 1;
  }
  .prog {
    font-family: var(--font-mono);
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 40vw;
  }
  .stlink {
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    color: var(--accent);
    cursor: pointer;
  }
  .stlink.quiet {
    color: var(--fg-muted);
  }
  .stlink.bad {
    color: var(--err);
  }
  .livedot {
    display: inline-block;
    width: 6px;
    height: 6px;
    margin-right: 0.3em;
    border-radius: 50%;
    background: var(--fg-faint);
    vertical-align: 0.05em;
  }
  .livedot.on {
    background: var(--ok);
  }
  .memw {
    display: inline-block;
    width: 44px;
    height: 4px;
    border-radius: 2px;
    background: var(--bg-panel-hover);
    overflow: hidden;
  }
  .memf {
    display: block;
    height: 100%;
    background: var(--accent);
  }
  .memf.hot {
    background: var(--err);
  }
  .about {
    position: absolute;
    left: 1rem;
    bottom: 2.2rem;
    z-index: 40;
    width: min(30rem, calc(100vw - 2rem));
    padding: 0.9rem 1.1rem;
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: 12px;
    box-shadow: var(--shadow-lift);
    font-size: 0.86rem;
    color: var(--fg-muted);
  }
  .about h2 {
    margin: 0 0 0.4rem;
    font-family: var(--font-display);
    font-size: 1.15rem;
    color: var(--fg);
  }
  .about p {
    margin: 0 0 0.5rem;
  }
  .about p:last-child {
    margin-bottom: 0;
  }
  .about b {
    color: var(--fg);
  }
  .aboutx {
    position: absolute;
    top: 0.35rem;
    right: 0.55rem;
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 1.1rem;
    cursor: pointer;
  }
  .keys {
    font-size: 0.75rem;
  }
  kbd {
    background: var(--bg-panel-hover);
    border: 1px solid var(--border);
    border-radius: 4px;
    padding: 0 0.35em;
    font-size: 0.72rem;
    margin: 0 0.1em 0 0.5em;
  }
  .panes {
    display: none;
  }

  /* one pane at a time on small screens, with a switcher along the bottom */
  .pg.narrow .sidepane,
  .pg.narrow .insppane,
  .pg.narrow .codepane {
    display: none;
    width: 100%;
  }
  .pg.narrow .grip {
    display: none;
  }
  .pg.narrow[data-pane='side'] .sidepane,
  .pg.narrow[data-pane='tree'] .insppane,
  .pg.narrow[data-pane='code'] .codepane {
    display: block;
  }
  .pg.narrow .panes {
    flex: none;
    display: flex;
    position: sticky;
    bottom: 0;
    z-index: 20;
    background: var(--bg-elev);
    border-top: 1px solid var(--border);
    padding-bottom: env(safe-area-inset-bottom, 0);
  }
  .panes button {
    flex: 1;
    background: none;
    border: none;
    padding: 0.55rem 0;
    font-size: 0.85rem;
    color: var(--fg-muted);
  }
  .panes button.on {
    color: var(--accent);
    font-weight: 600;
  }
  /* the editor's floating Edit button sits above the pane switcher */
  .pg.narrow :global(.mode-btn) {
    bottom: calc(3.4rem + env(safe-area-inset-bottom, 0px));
  }
  @media (pointer: coarse) {
    .btn-run {
      min-height: 2.2rem;
    }
    .stlink.quiet {
      min-width: 2rem;
      min-height: 2rem;
    }
  }
  .pg.narrow .pos,
  .pg.narrow .memw,
  .pg.narrow .kbdish {
    display: none;
  }
  @media (max-width: 720px) {
    .pg {
      flex: none;
      min-height: calc(100dvh - var(--nav-h));
    }
    .bar {
      position: sticky;
      top: 0;
      z-index: 20;
    }
    .main {
      flex: 1 0 auto;
    }
    .pg.narrow[data-pane='side'] .sidepane,
    .pg.narrow[data-pane='tree'] .insppane {
      height: calc(100dvh - 7rem);
    }
  }
</style>
