<script lang="ts">
  // The playground is a notebook, not an IDE: one full-width buffer whose
  // results render INLINE — every definition shows its reduced value under
  // its line, every test its verdict, errors their message — and, once the
  // kernel is warm, edits re-check live on a debounce. No output sidebar;
  // the remaining chrome is a toolbar, a run-progress overlay, toasts for
  // engine messages, and a one-line eval prompt at the bottom.
  import { onMount } from 'svelte'
  import { disp } from '$lib/disp/client.svelte'
  import type { RunOutcome, ItemEvent, ValueNode, RawTree } from '$lib/disp/protocol'
  import { parseTree, type T } from '$lib/treecalc/treecalc'
  import { examples } from '$lib/disp/examples'
  import DispEditor, { type EditorApi, type LineMark } from '$lib/components/DispEditor.svelte'
  import TreeValue, { type TreeValueCtl } from '$lib/components/TreeValue.svelte'
  import TreeVis from '$lib/components/TreeVis.svelte'

  // approximate item count of a cold kernel elaboration (measured; only used
  // to shape the progress bar)
  const KERNEL_ITEMS = 762

  let editorApi: EditorApi | undefined
  let currentDoc = $state('')
  let exampleId = $state('welcome')

  // ---- tabs -----------------------------------------------------------------
  // The scratch buffer (persistent, examples/share land here) plus any library
  // files opened from the Files list or by ctrl-clicking a "….disp" import.
  // Each tab runs with ITS OWN path, so a library file's relative opens
  // resolve exactly as they do for the elaborator; edits to file tabs are
  // local to the tab (the library the checker reads is untouched).
  const SCRATCH_PATH = '/lib/tests/playground.disp'
  // `preview` is the VS Code convention: a single-click open lands in ONE
  // reusable slot (italic title) so browsing doesn't spam tabs; editing the
  // file or double-clicking (tab or list entry) makes it permanent.
  type Tab = { id: string; title: string; path: string; doc: string; kind: 'scratch' | 'file'; dirty?: boolean; preview?: boolean }
  let tabs = $state<Tab[]>([
    { id: 'scratch', title: 'playground.disp', path: SCRATCH_PATH, doc: '', kind: 'scratch' }
  ])
  let activeTabId = $state('scratch')
  const activeTab = () => tabs.find((t) => t.id === activeTabId) ?? tabs[0]

  function switchTab(id: string) {
    if (id === activeTabId) return
    const cur = activeTab()
    cur.doc = editorApi?.getDoc() ?? cur.doc
    activeTabId = id
    const next = activeTab()
    currentDoc = next.doc
    editorApi?.setDoc(next.doc)
    summary = null
    stripError = null
    persistTabs()
  }

  function closeTab(id: string) {
    const idx = tabs.findIndex((t) => t.id === id)
    if (idx < 0 || tabs[idx].kind === 'scratch') return
    const wasActive = id === activeTabId
    tabs.splice(idx, 1)
    if (wasActive) {
      const next = tabs[Math.max(0, idx - 1)]
      activeTabId = next.id
      currentDoc = next.doc
      editorApi?.setDoc(next.doc)
      summary = null
      stripError = null
    }
    persistTabs()
  }

  async function openFile(path: string, opts?: { permanent?: boolean }) {
    const existing = tabs.find((t) => t.path === path)
    if (existing) {
      if (opts?.permanent) existing.preview = false
      return switchTab(existing.id)
    }
    const text = await disp.read(path)
    if (text == null) {
      toast(`no such file in the bundled library: ${path}`, 'warn')
      return
    }
    const title = path.split('/').pop() ?? path
    const pv = tabs.find((t) => t.preview)
    if (!opts?.permanent && pv) {
      // reuse the preview slot in place
      const cur = activeTab()
      if (cur.id !== pv.id) cur.doc = editorApi?.getDoc() ?? cur.doc
      pv.id = path
      pv.path = path
      pv.title = title
      pv.doc = text
      pv.dirty = false
      activeTabId = path
      currentDoc = text
      editorApi?.setDoc(text)
      summary = null
      stripError = null
      persistTabs()
      return
    }
    tabs.push({ id: path, title, path, doc: text, kind: 'file', preview: !opts?.permanent })
    switchTab(path)
  }

  // "../kernel/prelude.disp" relative to the ACTIVE tab's path
  function resolvePath(rel: string, fromPath: string): string {
    if (rel.startsWith('/')) return rel
    const parts = fromPath.split('/').slice(0, -1)
    for (const seg of rel.split('/')) {
      if (seg === '..') parts.pop()
      else if (seg !== '.') parts.push(seg)
    }
    return parts.join('/')
  }
  const onOpenPath = (rel: string) => void openFile(resolvePath(rel, activeTab().path))

  // jump-to-definition: the worker BFS-parses the buffer's open graph
  async function jumpDef(name: string) {
    const site = await disp.def(name, activeTab().path, editorApi?.getDoc() ?? currentDoc)
    if (!site) {
      toast(`no definition found for '${name}'`, 'warn')
      return
    }
    if (site.path !== activeTab().path) await openFile(site.path)
    editorApi?.gotoLine(site.line)
  }
  const resolveIdent = (name: string) =>
    disp.def(name, activeTab().path, editorApi?.getDoc() ?? currentDoc).then((s) => s !== null)

  // ---- the file browser (the worker's library) -----------------------------
  // USER FILES are real library entries: created/edited files write through
  // to the worker's vfs (so `use "./name.disp"` works from any buffer) and
  // persist in localStorage, replayed into the vfs on every boot. The
  // bundled files can be edited too (session-only — the originals return
  // with a fresh worker); stale module-cache entries drop on every write.
  let browserOpen = $state(false)
  let filePaths = $state<string[] | null>(null)
  let userFiles = $state<Record<string, string>>({})
  let newFileOpen = $state(false)
  let newFileName = $state('')

  async function refreshFiles() {
    filePaths = await disp.ls()
  }
  function toggleBrowser() {
    browserOpen = !browserOpen
    try {
      localStorage.setItem(LS_BROWSER, browserOpen ? '1' : '0')
    } catch {}
    if (browserOpen && filePaths == null) void refreshFiles()
  }
  // hierarchical view: nested collapsible folders (tests starts folded —
  // it's long and mostly regression pins)
  type DirNode = { name: string; path: string; dirs: DirNode[]; files: { path: string; name: string }[] }
  const fileTree = $derived.by(() => {
    const root: DirNode = { name: 'lib', path: 'lib', dirs: [], files: [] }
    const byPath = new Map<string, DirNode>([['lib', root]])
    for (const p of filePaths ?? []) {
      const segs = p.replace(/^\/lib\//, '').split('/')
      let node = root
      for (let i = 0; i < segs.length - 1; i++) {
        const dpath = `${node.path}/${segs[i]}`
        let child = byPath.get(dpath)
        if (!child) {
          child = { name: segs[i], path: dpath, dirs: [], files: [] }
          byPath.set(dpath, child)
          node.dirs.push(child)
        }
        node = child
      }
      node.files.push({ path: p, name: segs[segs.length - 1] })
    }
    return root
  })
  let collapsedDirs = $state(new Set(['lib/tests']))
  function toggleDir(path: string) {
    const next = new Set(collapsedDirs)
    if (next.has(path)) next.delete(path)
    else next.add(path)
    collapsedDirs = next
  }

  function persistUserFiles() {
    try {
      localStorage.setItem(LS_USER, JSON.stringify(userFiles))
    } catch {}
  }
  // replay user files into the (fresh) worker vfs — boot and post-interrupt
  async function restoreUserFiles() {
    for (const [p, text] of Object.entries(userFiles)) await disp.write(p, text)
  }

  async function createFile(name: string) {
    let n = name.trim()
    if (!n) return
    if (!n.endsWith('.disp')) n += '.disp'
    const path = n.startsWith('/') ? n : `/lib/tests/${n}`
    newFileOpen = false
    newFileName = ''
    if ((await disp.read(path)) != null) {
      void openFile(path) // exists — just open it
      return
    }
    const base = path.split('/').pop()
    const text = `// ${base} — use "./${base}" reaches this file from the scratch buffer\n\nopen use raw "../prelude.disp" {}\n`
    userFiles[path] = text
    persistUserFiles()
    await disp.write(path, text)
    await refreshFiles()
    void openFile(path)
  }

  async function deleteFile(path: string) {
    if (!(path in userFiles)) return // only user-created files delete
    const open = tabs.find((t) => t.path === path)
    if (open) closeTab(open.id)
    delete userFiles[path]
    persistUserFiles()
    await disp.rm(path)
    await refreshFiles()
    toast(`deleted ${path.split('/').pop()}`)
  }
  let busy = $state(false)
  let live = $state(true)
  let summary = $state<{ defs: number; pass: number; total: number; ms: number; errorLine?: number; error?: string } | null>(null)
  // line-less errors (no source attribution) fall back to a strip over the
  // editor bottom; attributed errors render inline at their line
  let stripError = $state<string | null>(null)
  let progress = $state<{ label: string; count: number; current: string } | null>(null)
  let showWelcome = $state(false)

  // ---- repl ----
  let replInput = $state('')
  let evalOut = $state<{ expr: string; value?: string; node?: ValueNode; hint?: string; error?: string } | null>(null)
  let replHistory: string[] = []
  let replHistIdx = -1

  // subtree re-render for every interactive value (blocks + eval strip):
  // raw for recognized atoms, deeper for budget cuts
  const expandValue = (handle: number, rawRoot: boolean) => disp.render(handle, 80, rawRoot)

  // ---- reduction-visualizer pop-out -----------------------------------------
  // A value's CURRENT fold state serializes into the pedagogical visualizer's
  // expression syntax: concrete structure goes in as trees; each folded atom
  // (name, string, big nat, … cut) becomes an identifier whose REAL structure
  // is pre-fetched into the panel's defs dictionary — so it enters as a green
  // POD: folded in the drawing, but genuine structure that computes when a
  // reduction consumes it (or when clicked open). Atoms too big to ship
  // (> POD_MAX_NODES) get no dictionary entry and stay symbolic fruit.
  // `atoms` (ident -> handle) is the sync bridge: a pod click in the panel
  // maps back to the buffer subtree it stands for. `vizCtl` is the source
  // TreeValue's controller — null in expression mode (eval pop-outs), where
  // the strip's VALUE doesn't correspond to the input expression's parts.
  let viz = $state<{ expr: string; defs: Record<string, T>; atoms: Map<string, number> } | null>(null)
  let vizCtl: TreeValueCtl | null = null
  let vizApi: { setProgram: (expr: string, newDefs?: Record<string, T>) => void } | null = null
  // the panel is sized to FIT (no scrollbar): the drawing gets whatever the
  // body height leaves after the instrument's own chrome (edit box +
  // transport ≈ 115px — minimal mode shows no readout text)
  let vizBodyH = $state(0)

  // seed the panel: in place when it's already open (the tree animates from
  // its current drawing — no remount flash), fresh mount otherwise
  function applySeed(expr: string, defs: Record<string, T>, atoms: Map<string, number>) {
    if (viz && vizApi) {
      viz = { expr, defs, atoms }
      vizApi.setProgram(expr, defs)
    } else {
      viz = { expr, defs, atoms }
    }
  }
  // strings are unary codepoint chains ("leaf" alone is ~830 nodes), so the
  // ceiling must be generous; a pod only draws its nodes once opened anyway
  const POD_MAX_NODES = 3000

  const rawToT = (r: RawTree): T =>
    r === 0
      ? { tag: 'leaf' }
      : r.length === 1
        ? { tag: 'stem', c: rawToT(r[0]) }
        : { tag: 'fork', l: rawToT(r[0]), r: rawToT(r[1]) }

  function serializeForViz(node: ValueNode): { expr: string; atoms: Map<string, number> } {
    const atoms = new Map<string, number>()
    let cuts = 0
    const mangle = (s: string) => {
      let m = s.replace(/[^A-Za-z0-9_]/g, '_')
      if (!/^[A-Za-z_]/.test(m)) m = '_' + m
      if (m === 't') m = '_t' // the leaf token
      return m
    }
    // same ident, different tree (two strings mangling alike): suffix apart
    const claim = (want: string, h: number): string => {
      let m = want
      let i = 2
      while (atoms.has(m) && atoms.get(m) !== h) m = `${want}_${i++}`
      atoms.set(m, h)
      return m
    }
    const go = (n: ValueNode, atom: boolean): string => {
      switch (n.k) {
        case 'leaf':
          return 't'
        case 'nat':
          // small nats go in concrete (the encodings agree); big ones pod
          return n.n <= 24 ? String(n.n) : claim(mangle(`n_${n.n}`), n.h)
        case 'str':
          return claim(mangle(n.s), n.h)
        case 'name':
          return claim(mangle(n.name), n.h)
        case 'more':
          return claim(`_${++cuts}`, n.h)
        case 'stem': {
          const s = `t ${go(n.c[0], true)}`
          return atom ? `(${s})` : s
        }
        case 'fork': {
          const s = `t ${go(n.c[0], true)} ${go(n.c[1], true)}`
          return atom ? `(${s})` : s
        }
      }
    }
    const expr = go(node, false)
    return { expr, atoms }
  }

  async function seedViz(tree: ValueNode) {
    const { expr, atoms } = serializeForViz(tree)
    const defs: Record<string, T> = {}
    await Promise.all(
      [...atoms].map(async ([ident, handle]) => {
        const rt = await disp.raw({ handle }, POD_MAX_NODES)
        if (rt !== null) defs[ident] = rawToT(rt)
      })
    )
    applySeed(expr, defs, atoms)
  }

  // value pop-out (def blocks, got/want rows): synced with its TreeValue
  const onVisualizeValue = (tree: ValueNode, ctl: TreeValueCtl) => {
    vizCtl = ctl
    void seedViz(tree)
  }

  // two-way sync, buffer -> panel: any fold change in the SOURCE value
  // re-seeds the panel at its new fold state (a fresh step-0 tree)
  const onValueFoldChange = (tree: ValueNode, ctl: TreeValueCtl) => {
    if (viz && vizCtl === ctl && ctl.alive) void seedViz(tree)
  }

  // two-way sync, panel -> buffer: before any step, clicking a pod unfolds
  // the corresponding subtree in the buffer instead — the fold-change above
  // then re-seeds the panel. A dead controller (the buffer re-ran) falls
  // back to the panel's local reveal.
  const onPodOpen = (label: string): boolean => {
    const handle = viz?.atoms.get(label)
    if (handle == null || !vizCtl?.alive) return false
    void vizCtl.unfoldByHandle(handle)
    return true
  }

  // eval pop-out: seed with the EXPRESSION when the visualizer's grammar can
  // hold it, so the panel replays the actual reduction the REPL performed
  // (names resolve to pods on demand; strings parse to real cons trees).
  // Expressions beyond that grammar (binders…) fall back to the value.
  function onVisualizeEval(tree: ValueNode, ctl: TreeValueCtl) {
    const expr = evalOut?.expr?.trim()
    if (expr) {
      try {
        parseTree(expr, {})
        vizCtl = null
        applySeed(expr, {}, new Map())
        return
      } catch {
        /* not treecalc-expressible — show the value instead */
      }
    }
    onVisualizeValue(tree, ctl)
  }

  // names TYPED into the panel's edit box resolve against the live playground
  // scope — the panel computes with the same definitions the buffer does
  const resolveScopeDef = async (name: string): Promise<T | null> => {
    const rt = await disp.raw({ name }, POD_MAX_NODES)
    return rt === null ? null : rawToT(rt)
  }

  // highlight an expression in the buffer → a floating tree button appears
  // when it fits the visualizer's grammar; pressing it seeds the panel with
  // the SELECTION (names resolve to pods / engine steps as usual)
  const selectionAction = {
    validate: (text: string) => {
      if (text.length > 300) return false
      try {
        parseTree(text, {})
        return true
      } catch {
        return false
      }
    },
    run: (text: string) => {
      vizCtl = null
      applySeed(text, {}, new Map())
    },
    title: 'visualize the reduction of this expression'
  }

  // engine macro-step: the panel is stuck on a name whose definition never
  // shipped (kernel-tier values explode as trees) — perform the application
  // on the real evaluator and hand back the (usually small) result. Fruit
  // born from the pop-out's own atoms carries its handle in the atoms table;
  // typed names resolve by name against the current scope.
  const tToRaw = (t: T): RawTree => {
    if (t.tag === 'leaf') return 0
    if (t.tag === 'stem') return [tToRaw(t.c)]
    if (t.tag === 'fork') return [tToRaw(t.l), tToRaw(t.r)]
    throw new Error('non-concrete argument') // stuckSpine guarantees concreteness
  }
  const engineStep = async (name: string, args: T[]): Promise<T | null> => {
    try {
      const handle = viz?.atoms.get(name)
      const rt = await disp.applySpine(
        handle != null ? { handle } : { name },
        args.map(tToRaw),
        POD_MAX_NODES
      )
      return rt === null ? null : rawToT(rt)
    } catch {
      return null
    }
  }

  // ---- toasts (engine messages: kernel loaded, reset, interrupted…) ----
  let toasts = $state<{ id: number; text: string; kind: 'info' | 'warn' }[]>([])
  let toastId = 0
  function toast(text: string, kind: 'info' | 'warn' = 'info') {
    const id = ++toastId
    toasts.push({ id, text, kind })
    setTimeout(() => (toasts = toasts.filter((t) => t.id !== id)), 6500)
  }

  disp.onItem = (e: ItemEvent) => {
    if (e.depth > 0 && progress) {
      progress.count++
      progress.current = `${shortPath(e.sourcePath)}${e.name ? ' · ' + e.name : ''}`
    }
  }
  const shortPath = (p?: string) => (p ? p.replace(/^.*\/lib\//, 'lib/') : '')

  // ---- inline results -----------------------------------------------------

  // Notebook marks: value blocks anchor at a def's LAST line (a multi-line
  // def's output belongs after its body), test verdicts likewise. Values are
  // structured (interactive unfold); plain text stays the fallback.
  function marksFromOutcome(out: RunOutcome): LineMark[] {
    const marks: LineMark[] = []
    for (const d of out.defs) {
      const line = d.endLine ?? d.line
      if (!line) continue
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

  function applyOutcome(out: RunOutcome, opts?: { coldLoad?: boolean }) {
    editorApi?.setMarks(marksFromOutcome(out))
    summary = {
      defs: out.defs.length,
      pass: out.tests.filter((t) => t.pass).length,
      total: out.tests.length,
      ms: out.elapsedMs,
      error: out.error,
      errorLine: out.errorLine
    }
    stripError = out.error && !out.errorLine ? out.error : null
    // a heavy buffer shouldn't re-run on every keystroke — pause live mode.
    // A run that just paid the one-time kernel elaboration is exempt: its
    // slowness says nothing about the buffer.
    if (live && !opts?.coldLoad && out.elapsedMs > 2500) {
      setLive(false, { persist: false })
      toast(`live re-check paused — the last run took ${fmtMs(out.elapsedMs)}. Re-enable it in the toolbar.`)
    }
  }

  // ---- actions --------------------------------------------------------------

  async function execute(source: string, label: string) {
    if (busy) return
    busy = true
    stripError = null
    progress = {
      label: disp.kernelLoaded ? label : `${label} — the first run checks the kernel too`,
      count: 0,
      current: ''
    }
    try {
      // each tab runs at its own path — library files resolve their opens
      // exactly as the elaborator would
      const out = await disp.run(source, { wantDefPretty: true, path: activeTab().path })
      // >100 module-depth items streamed = this run elaborated the kernel
      applyOutcome(out, { coldLoad: (progress?.count ?? 0) > 100 })
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e)
      if (msg !== 'interrupted') toast(`⚠ ${msg}`, 'warn') // stop() already announced
    } finally {
      progress = null
      busy = false
    }
  }

  const runFile = () => {
    const doc = editorApi?.getDoc() ?? currentDoc
    void execute(doc, 'run file')
  }
  const runToCursor = () => {
    const prefix = editorApi?.getDocToCursor()
    if (!prefix?.trim()) return
    const lines = prefix.trimEnd().split('\n').length
    void execute(prefix, `run to line ${lines}`)
  }

  // live re-check: rerun as soon as typing pauses. Gated so a cold session
  // never starts the minute-long kernel load uninvited: fires once the kernel
  // is warm, OR when the buffer opens nothing but raw modules (`use raw` —
  // instant, no kernel involved). Warm reruns cost single-digit milliseconds;
  // an in-flight run re-arms the debounce.
  const canAutoRun = (doc: string) => disp.kernelLoaded || !/\buse\s+(?!raw\b)/.test(doc)
  let editTimer: ReturnType<typeof setTimeout> | undefined
  function onEdit(doc: string) {
    currentDoc = doc
    const tab = activeTab()
    if (doc !== tab.doc) {
      tab.doc = doc
      if (tab.kind === 'file') {
        // write-through: the session library sees the edit immediately (its
        // module cache invalidates), so dependents re-elaborate on next use
        tab.dirty = true
        tab.preview = false // an edited preview earns its keep
        void disp.write(tab.path, doc)
        if (tab.path in userFiles) {
          userFiles[tab.path] = doc
          persistUserFiles()
        }
      }
    }
    if (tab.kind === 'scratch') persist()
    if (!live) return
    clearTimeout(editTimer)
    editTimer = setTimeout(() => {
      if (!live || disp.status === 'dead' || !canAutoRun(currentDoc)) return
      if (busy) return onEdit(currentDoc)
      void execute(currentDoc, 'live re-check')
    }, 350)
  }

  // `persist: false` = the auto-pause path: session-only, so a slow buffer
  // today doesn't leave live mode silently off for every future visit
  function setLive(v: boolean, opts?: { persist?: boolean }) {
    live = v
    if (opts?.persist !== false)
      try {
        localStorage.setItem(LS_LIVE, v ? '1' : '0')
      } catch {}
    if (!v) clearTimeout(editTimer)
    else {
      const doc = editorApi?.getDoc() ?? currentDoc
      if (canAutoRun(doc) && !busy) void execute(doc, 'live re-check')
    }
  }

  async function runRepl() {
    const expr = replInput.trim()
    if (!expr || busy) return
    replHistory.push(expr)
    replHistIdx = replHistory.length
    replInput = ''
    busy = true
    progress = { label: `▸ ${expr}`, count: 0, current: '' }
    try {
      const out = await disp.evalExpr(editorApi?.getDoc() ?? currentDoc, expr, {
        wantDefPretty: true,
        path: activeTab().path
      })
      if (out.error) {
        evalOut = { expr, error: out.error }
      } else {
        // an eval elaborates the whole buffer too — refresh the inline marks
        applyOutcome(out, { coldLoad: (progress?.count ?? 0) > 100 })
        evalOut = {
          expr,
          value: out.value ?? '(no value)',
          node: out.valueNode,
          hint: out.valueHint && out.valueHint !== out.value ? out.valueHint : undefined
        }
      }
    } catch (e) {
      evalOut = { expr, error: e instanceof Error ? e.message : String(e) }
    } finally {
      progress = null
      busy = false
    }
  }

  function replKeydown(e: KeyboardEvent) {
    if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
      e.preventDefault()
      runFile()
    } else if (e.key === 'Enter') {
      e.preventDefault()
      void runRepl()
    } else if (e.key === 'ArrowUp' && replHistIdx > 0) {
      e.preventDefault()
      replHistIdx--
      replInput = replHistory[replHistIdx] ?? ''
    } else if (e.key === 'ArrowDown') {
      e.preventDefault()
      replHistIdx = Math.min(replHistIdx + 1, replHistory.length)
      replInput = replHistory[replHistIdx] ?? ''
    }
  }

  async function preloadKernel() {
    if (busy || disp.kernelLoaded) return
    busy = true
    progress = { label: 'elaborating + self-verifying the kernel', count: 0, current: '' }
    try {
      const out = await disp.loadKernel()
      toast(
        out.ok
          ? `kernel loaded + verified · ${(out.elapsedMs / 1000).toFixed(1)}s · ${fmtSteps(out.steps)} steps — later runs reuse it`
          : `kernel load failed: ${out.error}`,
        out.ok ? 'info' : 'warn'
      )
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e)
      if (msg !== 'interrupted') toast(`⚠ ${msg}`, 'warn') // stop() already announced
    } finally {
      progress = null
      busy = false
    }
    // the kernel is warm now — greet with inline results
    if (live && disp.kernelLoaded) void execute(editorApi?.getDoc() ?? currentDoc, 'checking the buffer')
  }

  // The don't-trust-the-snapshot path: drop the precompiled kernel and watch
  // the type system genuinely re-check itself from source.
  async function verifyKernel() {
    if (busy) return
    busy = true
    try {
      await disp.reset({ fromSource: true })
      // the reset swapped arenas — inline values hold dead handles
      editorApi?.setMarks([])
      evalOut = null
      summary = null
      toast('precompiled kernel dropped — re-elaborating from source')
    } finally {
      busy = false
    }
    await preloadKernel()
  }

  function stop() {
    disp.interrupt()
    progress = null
    busy = false
    // handles inside the inline values died with the arena — drop them
    editorApi?.setMarks([])
    evalOut = null
    summary = null
    // the fresh worker's vfs reverts to the bundle — replay user files
    void restoreUserFiles()
    toast('interrupted — engine reset, kernel cache cleared', 'warn')
  }

  async function resetSession() {
    if (busy) return
    await disp.reset()
    summary = null
    evalOut = null
    stripError = null
    editorApi?.setMarks([]) // inline values hold handles from the old arena
    toast(
      disp.kernelLoaded
        ? 'session reset — fresh arena, precompiled kernel re-installed'
        : 'session reset — fresh arena, kernel cache cleared'
    )
  }

  function loadExample(id: string) {
    const ex = examples.find((x) => x.id === id)
    if (!ex) return
    exampleId = id
    switchTab('scratch') // examples always land in the scratch buffer
    editorApi?.setDoc(ex.source)
    currentDoc = ex.source
    summary = null
    stripError = null
    persist()
    if (live && canAutoRun(ex.source) && !busy) void execute(ex.source, 'checking the example')
  }

  // ---- share (the buffer travels in the URL hash) --------------------------

  function b64url(b: Uint8Array): string {
    let s = ''
    for (const x of b) s += String.fromCharCode(x)
    return btoa(s).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '')
  }
  async function share() {
    const doc = editorApi?.getDoc() ?? currentDoc
    const bytes = new TextEncoder().encode(doc)
    let payload: string
    if (typeof CompressionStream !== 'undefined') {
      const stream = new Blob([bytes]).stream().pipeThrough(new CompressionStream('deflate-raw'))
      payload = 'c' + b64url(new Uint8Array(await new Response(stream).arrayBuffer()))
    } else {
      payload = 'r' + b64url(bytes)
    }
    history.replaceState(null, '', `#${payload}`)
    try {
      await navigator.clipboard.writeText(location.href)
      toast('link copied — the buffer travels in the URL')
    } catch {
      toast('link ready in the address bar')
    }
  }
  async function decodeShared(h: string): Promise<string | null> {
    try {
      const raw = Uint8Array.from(atob(h.slice(1).replaceAll('-', '+').replaceAll('_', '/')), (c) =>
        c.charCodeAt(0)
      )
      if (h[0] === 'r') return new TextDecoder().decode(raw)
      if (h[0] !== 'c' || typeof DecompressionStream === 'undefined') return null
      const stream = new Blob([raw]).stream().pipeThrough(new DecompressionStream('deflate-raw'))
      return await new Response(stream).text()
    } catch {
      return null
    }
  }

  // ---- persistence ----------------------------------------------------------

  const LS_DOC = 'disp-playground-doc'
  const LS_EX = 'disp-playground-example'
  const LS_LIVE = 'disp-playground-live'
  const LS_WELCOME = 'disp-playground-welcomed'
  const LS_TABS = 'disp-playground-tabs'
  const LS_USER = 'disp-playground-userfiles'
  const LS_BROWSER = 'disp-playground-browser'
  function persist() {
    try {
      localStorage.setItem(LS_DOC, currentDoc)
      localStorage.setItem(LS_EX, exampleId)
    } catch {}
  }
  // open file tabs persist as paths (content re-reads from the bundled
  // library — local edits to file tabs are session-only by design)
  function persistTabs() {
    try {
      localStorage.setItem(LS_TABS, JSON.stringify(tabs.filter((t) => t.kind === 'file').map((t) => t.path)))
    } catch {}
  }
  function dismissWelcome() {
    showWelcome = false
    try {
      localStorage.setItem(LS_WELCOME, '1')
    } catch {}
  }

  let initialDoc = examples[0].source
  onMount(() => {
    void (async () => {
      try {
        live = localStorage.getItem(LS_LIVE) !== '0'
        showWelcome = !localStorage.getItem(LS_WELCOME)
        userFiles = JSON.parse(localStorage.getItem(LS_USER) ?? '{}') as Record<string, string>
        browserOpen = localStorage.getItem(LS_BROWSER) === '1'
      } catch {}
      // doc precedence: #shared-code > ?example=<id> deep link > saved buffer
      const hash = location.hash.slice(1)
      let loaded = false
      if (hash.length > 1 && (hash[0] === 'c' || hash[0] === 'r')) {
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
          const savedEx = localStorage.getItem(LS_EX)
          if (savedEx && examples.some((x) => x.id === savedEx)) exampleId = savedEx
          if (saved) {
            currentDoc = saved
            editorApi?.setDoc(saved)
          } else {
            currentDoc = initialDoc
          }
        } catch {
          currentDoc = initialDoc
        }
      }
      try {
        await disp.init()
      } catch {
        return // status chip already says 'dead'
      }
      // user files enter the session library before anything reads them
      await restoreUserFiles()
      if (browserOpen) void refreshFiles()
      // reopen last session's file tabs (scratch stays active)
      try {
        const saved = JSON.parse(localStorage.getItem(LS_TABS) ?? '[]') as string[]
        for (const p of saved) {
          if (tabs.some((t) => t.path === p)) continue
          const text = await disp.read(p)
          if (text != null) tabs.push({ id: p, title: p.split('/').pop() ?? p, path: p, doc: text, kind: 'file' })
        }
      } catch {}
      // greet with inline results when the precompiled kernel restored (or
      // the buffer is kernel-free) — never spend a visitor's minute uninvited
      const doc = editorApi?.getDoc() ?? currentDoc
      if (live && canAutoRun(doc) && !busy) void execute(doc, 'checking the buffer')
    })()
    return () => clearTimeout(editTimer)
  })

  // ---- formatting -----------------------------------------------------------

  const fmtSteps = (n: number) =>
    n >= 1e9 ? (n / 1e9).toFixed(1) + 'B' : n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(1) + 'k' : String(n)
  const fmtMs = (ms: number) => (ms >= 1000 ? (ms / 1000).toFixed(1) + 's' : ms.toFixed(0) + 'ms')
  const memPct = $derived(Math.min(100, (disp.memBytes / (4096 * 1024 * 1024)) * 100))

  const statusLabel = $derived(
    disp.status === 'cold'
      ? 'engine cold'
      : disp.status === 'booting'
        ? 'booting…'
        : disp.status === 'dead'
          ? 'engine crashed, hit reset'
          : busy
            ? 'running…'
            : summary?.error
              ? `⚠ error${summary.errorLine ? ` · line ${summary.errorLine}` : ''}`
              : summary
                ? `${summary.defs} def${summary.defs === 1 ? '' : 's'} · ${summary.pass}/${summary.total} ✓ · ${fmtMs(summary.ms)}`
                : disp.kernelLoaded
                  ? 'kernel ✓'
                  : 'ready · kernel not loaded'
  )
  const statusBad = $derived(!!summary?.error || (summary != null && summary.pass < summary.total))
</script>

<svelte:head>
  <title>Playground · disp</title>
</svelte:head>

<!-- the first-visit info panel stays pinned until the visitor starts doing
     something else (any pointer-down outside it) or dismisses it directly -->
<svelte:window
  onpointerdown={(e) => {
    if (showWelcome && !(e.target as HTMLElement).closest('.info-wrap')) dismissWelcome()
  }}
  onkeydown={(e) => {
    // close the active file tab: ctrl/cmd-w where the platform lets a page
    // claim it (installed app / fullscreen); alt-w works everywhere
    if ((e.key === 'w' || e.key === 'W') && !e.shiftKey && (e.ctrlKey || e.metaKey || e.altKey)) {
      e.preventDefault()
      const t = activeTab()
      if (t.kind === 'file') closeTab(t.id)
    }
  }}
/>

{#snippet dirNode(node: DirNode, depth: number)}
  {#each node.dirs as d (d.path)}
    <button
      class="fdir-row"
      style="padding-left: {0.35 + depth * 0.75}rem"
      onclick={() => toggleDir(d.path)}
      aria-expanded={!collapsedDirs.has(d.path)}
    >
      <span class="chev" class:closed={collapsedDirs.has(d.path)}>▾</span>
      {d.name}
    </button>
    {#if !collapsedDirs.has(d.path)}
      {@render dirNode(d, depth + 1)}
    {/if}
  {/each}
  {#each node.files as f (f.path)}
    <div
      class="fitem-row"
      class:opened={tabs.some((t) => t.path === f.path)}
      style="padding-left: {0.55 + depth * 0.75}rem"
    >
      <button
        class="fitem"
        onclick={() => void openFile(f.path)}
        ondblclick={() => void openFile(f.path, { permanent: true })}
        title={f.path}
      >
        {f.name}
      </button>
      {#if f.path in userFiles}
        <button class="f-del" onclick={() => void deleteFile(f.path)} title="delete (yours)" aria-label="delete {f.name}">×</button>
      {/if}
    </div>
  {/each}
{/snippet}

<div class="pg">
  <div class="toolbar">
    <button class="btn primary tb" onclick={runFile} disabled={busy} title="Ctrl/⌘-Enter">
      ▶ Run
    </button>
    <button class="btn tb" onclick={runToCursor} disabled={busy} title="Shift-Enter">
      ⇣ To cursor
    </button>
    {#if busy}
      <button class="btn tb stop" onclick={stop}>■ Stop</button>
    {/if}
    <select
      class="ex-select"
      value={exampleId}
      onchange={(e) => loadExample((e.target as HTMLSelectElement).value)}
    >
      {#each examples as ex}
        <option value={ex.id}>{ex.label}</option>
      {/each}
    </select>
    <button
      class="btn tb toggle"
      class:on={live}
      onclick={() => setLive(!live)}
      title="re-check the buffer as you type (once the kernel is warm)"
      aria-pressed={live}
    >
      <span class="tdot"></span> live
    </button>
    <button class="btn tb subtle" onclick={() => void share()} title="copy a link that carries this buffer">
      ↗ Share
    </button>
    <div class="spacer"></div>
    <div class="status" class:live={busy} class:dead={disp.status === 'dead'} class:bad={statusBad}>
      <span class="dot"></span>
      {#if busy && progress}
        <!-- the run-progress indicator lives right here in the toolbar: a
             small bar + the item currently elaborating (full label in the
             tooltip). The fade-in delay keeps millisecond reruns flicker-free. -->
        <div class="mini-prog" title={progress.label}>
          <div class="mp-bar">
            <div class="mp-fill" style="width:{Math.min(99, (progress.count / KERNEL_ITEMS) * 100)}%"></div>
          </div>
          <span class="mp-txt">
            {progress.count > 0 ? `${progress.count} · ${progress.current}` : progress.label}
          </span>
        </div>
      {:else}
        {statusLabel}
        {#if !disp.kernelLoaded && !busy && disp.status !== 'dead'}
          <button class="linkbtn" onclick={preloadKernel}>load kernel</button>
        {:else if disp.kernelLoaded && !busy && disp.status !== 'dead'}
          <button
            class="linkbtn"
            onclick={verifyKernel}
            title="drop the precompiled kernel and re-elaborate + self-verify it from source (~a minute)"
          >
            verify from source
          </button>
        {/if}
      {/if}
    </div>
    {#if disp.memBytes > 0}
      <div class="mem-wrap" title="WASM memory arena: {(disp.memBytes / 1e9).toFixed(2)}GB of the 4GB ceiling">
        <svg class="mem-ico" viewBox="0 0 16 16" aria-hidden="true">
          <rect x="1.5" y="4.5" width="13" height="7" rx="1.2" fill="none" stroke="currentColor" stroke-width="1.3" />
          <path d="M4 11.5v2M8 11.5v2M12 11.5v2" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" />
          <rect x="4" y="6.5" width="3" height="3" rx="0.6" fill="currentColor" />
          <rect x="9" y="6.5" width="3" height="3" rx="0.6" fill="currentColor" />
        </svg>
        <div class="mem">
          <div class="mem-fill" class:hot={memPct > 85} style="width:{memPct}%"></div>
        </div>
      </div>
    {/if}
    <div class="info-wrap" class:pinned={showWelcome}>
      <button
        class="btn tb subtle icon"
        onclick={() => (showWelcome ? dismissWelcome() : (showWelcome = true))}
        title="about this playground"
        aria-label="about this playground"
      >
        ⓘ
      </button>
      <div class="info-pop" role="note">
        <div class="info-card">
          {#if showWelcome}
            <button class="x" onclick={dismissWelcome} aria-label="dismiss">×</button>
          {/if}
          <p class="w-title">The <span class="grad-text">real toolchain</span>, in your tab.</p>
          <p>
            This playground runs disp's actual elaborator and its Rust evaluator (compiled to
            WebAssembly) — the same code that checks the test suite. Nothing is sent to a server.
          </p>
          <p>
            Results live in the buffer, notebook-style: every definition shows its reduced value,
            every test its verdict, and edits re-check live once the kernel is warm. Values are
            explorable — click a number, string, or name to unfold the tree it stands for, click
            <em>…</em> to render deeper, <em>−</em> to fold a subterm back, and the tree button to
            pop the value into the reduction visualizer (folded names ride along as symbolic
            fruit).
          </p>
          <p>
            The kernel arrives <em>precompiled</em> — a small snapshot of the checked module cache —
            so typed runs start in seconds. Don't trust it?
            <button class="linkbtn" onclick={() => { dismissWelcome(); void verifyKernel() }}>Verify from source</button>:
            about a minute of the type system genuinely re-checking itself, right here.
          </p>
          <p class="hint-row">
            <kbd>⌘⏎</kbd> run file · <kbd>⇧⏎</kbd> run to cursor · <kbd>⌥W</kbd> close tab
            (<kbd>⌃W</kbd> too, where the browser allows) · the <kbd>▸</kbd> prompt below
            evaluates expressions against the buffer
          </p>
        </div>
      </div>
    </div>
    <button class="btn tb subtle" onclick={resetSession} disabled={busy} title="Fresh session: clears the arena and the kernel cache">
      Reset
    </button>
  </div>

  <div class="tabbar">
    <button
      class="fb-toggle"
      class:on={browserOpen}
      onclick={toggleBrowser}
      aria-expanded={browserOpen}
      title="file browser — every library file, plus your own (ctrl-click an import string jumps too)"
    >
      ❦
    </button>
    {#each tabs as tb (tb.id)}
      <div
        class="tab"
        class:active={tb.id === activeTabId}
        role="button"
        tabindex="0"
        class:preview={tb.preview}
        title={tb.path}
        onclick={() => switchTab(tb.id)}
        ondblclick={() => (tb.preview = false)}
        onkeydown={(e) => e.key === 'Enter' && switchTab(tb.id)}
      >
        <span class="tab-title">{tb.title}</span>
        {#if tb.dirty}<span class="tab-dirty" title="local edits — the library the checker reads is untouched">●</span>{/if}
        {#if tb.kind === 'file'}
          <button
            class="tab-x"
            onclick={(e) => {
              e.stopPropagation()
              closeTab(tb.id)
            }}
            aria-label="close {tb.title}"
          >
            ×
          </button>
        {/if}
      </div>
    {/each}
    <div class="tab-spacer"></div>
  </div>

  <div class="stage">
    {#if browserOpen}
      <aside class="fb">
        <div class="fb-head">
          <span class="fb-title">library</span>
          <button
            class="fb-new"
            onclick={() => {
              newFileOpen = !newFileOpen
              if (filePaths == null) void refreshFiles()
            }}
            title="new file (lands in lib/tests/ — reach it with use &quot;./name.disp&quot;)"
            aria-label="new file"
          >
            ＋
          </button>
        </div>
        {#if newFileOpen}
          <form
            class="fb-newform"
            onsubmit={(e) => {
              e.preventDefault()
              void createFile(newFileName)
            }}
          >
            <!-- svelte-ignore a11y_autofocus -->
            <input
              bind:value={newFileName}
              placeholder="name.disp"
              spellcheck="false"
              autocomplete="off"
              autofocus
              onkeydown={(e) => e.key === 'Escape' && ((newFileOpen = false), (newFileName = ''))}
            />
          </form>
        {/if}
        <div class="fb-list">
          {#if filePaths == null}
            <div class="files-loading">listing…</div>
          {:else}
            {@render dirNode(fileTree, 0)}
          {/if}
        </div>
      </aside>
    {/if}
    <div class="ed-wrap">
      <DispEditor
        doc={currentDoc || initialDoc}
        onDocChange={onEdit}
        onRunFile={runFile}
        onRunToCursor={runToCursor}
        {expandValue}
        {onVisualizeValue}
        {onValueFoldChange}
        {onOpenPath}
        {selectionAction}
        onJumpDef={(n) => void jumpDef(n)}
        onResolveIdent={resolveIdent}
        api={(a) => (editorApi = a)}
      />
    </div>

    {#if viz}
      <aside class="viz-panel">
        <button class="viz-x" onclick={() => { viz = null; vizApi = null }} aria-label="close visualizer">×</button>
        <div class="viz-body" bind:clientHeight={vizBodyH}>
          {#if vizBodyH > 0}
            <TreeVis
              variant="lab"
              exprs={[viz.expr]}
              height={Math.max(200, vizBodyH - 120)}
              defs={viz.defs}
              resolveDef={resolveScopeDef}
              minimal
              lazyParse
              {onPodOpen}
              {engineStep}
              api={(a) => (vizApi = a)}
              namesHint="Green pods are real structure, folded — reduction computes through them; clicking one before stepping unfolds it in the buffer too (the buffer's ↺ folds both). Names resolve against the playground's scope; ones too large to ship stay orange fruit, and stepping hands their applications to the real evaluator."
            />
          {/if}
        </div>
      </aside>
    {/if}

    {#if stripError}
      <div class="errstrip" title={stripError}>⚠ {stripError}</div>
    {/if}

    <div class="toasts">
      {#each toasts as t (t.id)}
        <div class="toast" class:warn={t.kind === 'warn'}>{t.text}</div>
      {/each}
    </div>
  </div>

  {#if evalOut}
    <div class="eval-result" class:bad={!!evalOut.error}>
      <span class="ee">▸ {evalOut.expr}</span>
      {#if evalOut.error}
        <pre class="err">{evalOut.error}</pre>
      {:else}
        <span class="ev">
          {#if evalOut.node}
            {#key evalOut}
              <TreeValue node={evalOut.node} expand={expandValue} onVisualize={onVisualizeEval} />
            {/key}
          {:else}
            {evalOut.value}
          {/if}
        </span>
        {#if evalOut.hint}<span class="hint">≈ {evalOut.hint}</span>{/if}
      {/if}
      <button class="x" onclick={() => (evalOut = null)} aria-label="dismiss result">×</button>
    </div>
  {/if}
  <div class="repl">
    <span class="prompt" class:busy>▸</span>
    <input
      type="text"
      bind:value={replInput}
      onkeydown={replKeydown}
      placeholder={busy ? 'running…' : 'evaluate an expression against the buffer, e.g.  double 21'}
      disabled={busy}
      spellcheck="false"
      autocomplete="off"
    />
  </div>
</div>

<style>
  .pg {
    display: flex;
    flex-direction: column;
    /* the layout's app mode gives .content the exact space under the nav —
       fill it (robust to the nav wrapping taller on narrow screens) */
    flex: 1;
    min-height: 0;
  }

  /* ---- toolbar ---- */
  .toolbar {
    display: flex;
    align-items: center;
    gap: 0.55rem;
    padding: 0.55rem clamp(0.6rem, 2vw, 1.2rem);
    border-bottom: 1px solid var(--border);
    background: var(--bg-elev);
    flex-wrap: wrap;
  }
  .tb { padding: 0.38em 0.9em; font-size: 0.88rem; }
  .tb.stop { border-color: color-mix(in oklab, var(--err) 50%, transparent); color: var(--err); }
  .tb.subtle { opacity: 0.8; }
  .tb.icon { padding: 0.38em 0.55em; }
  .tb.toggle { display: inline-flex; align-items: center; gap: 0.4em; }
  .tb.toggle .tdot {
    width: 7px;
    height: 7px;
    border-radius: 50%;
    background: var(--fg-faint);
    transition: background 0.15s ease, box-shadow 0.15s ease;
  }
  .tb.toggle.on { border-color: color-mix(in oklab, var(--ok) 45%, transparent); }
  .tb.toggle.on .tdot {
    background: var(--ok);
    box-shadow: 0 0 6px color-mix(in oklab, var(--ok) 60%, transparent);
  }
  .ex-select {
    background: var(--bg-panel);
    color: var(--fg);
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    padding: 0.42em 0.6em;
    font-family: var(--font-body);
    font-size: 0.86rem;
    max-width: 40vw;
  }
  .ex-select option { background: var(--bg-elev); }
  .spacer { flex: 1; }
  .status {
    display: inline-flex;
    align-items: center;
    gap: 0.45em;
    font-size: 0.82rem;
    color: var(--fg-muted);
    white-space: nowrap;
  }
  .status .dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: var(--ok);
    box-shadow: 0 0 6px color-mix(in oklab, var(--ok) 60%, transparent);
  }
  .status.live .dot { background: var(--accent); animation: pulse 1.1s ease-in-out infinite; }
  .status.dead .dot, .status.bad .dot { background: var(--err); }
  @keyframes pulse { 50% { opacity: 0.35; } }
  .linkbtn {
    background: none;
    border: none;
    color: var(--accent);
    cursor: pointer;
    font: inherit;
    padding: 0;
    text-decoration: underline;
    text-underline-offset: 3px;
  }
  /* run progress, inline in the status chip: small bar + current item */
  .mini-prog {
    display: inline-flex;
    align-items: center;
    gap: 0.5em;
    min-width: 0;
    animation: mp-in 0.15s ease 0.25s backwards; /* millisecond reruns never flash it */
  }
  @keyframes mp-in { from { opacity: 0; } }
  .mp-bar {
    width: 74px;
    height: 6px;
    border-radius: 3px;
    background: rgba(74, 104, 82, 0.14);
    overflow: hidden;
    flex: none;
  }
  .mp-fill {
    height: 100%;
    background: var(--grad-brand);
    transition: width 0.25s ease;
  }
  .mp-txt {
    font-family: var(--font-mono);
    font-size: 0.74rem;
    color: var(--fg-muted);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: min(340px, 30vw);
  }

  .mem-wrap {
    display: inline-flex;
    align-items: center;
    gap: 0.35em;
    color: var(--fg-faint);
  }
  .mem-ico { width: 15px; height: 15px; flex: none; }
  .mem {
    width: 68px;
    height: 6px;
    border-radius: 3px;
    background: rgba(74, 104, 82, 0.14);
    overflow: hidden;
  }
  .mem-fill { height: 100%; background: var(--grad-brand); transition: width 0.3s ease; }
  .mem-fill.hot { background: var(--err); }

  /* ---- the ⓘ popover: hover/focus opens it, first visit pins it ---- */
  .info-wrap { position: relative; }
  .info-pop {
    display: none;
    position: absolute;
    top: 100%;
    right: 0;
    z-index: 30;
    width: min(38rem, 92vw);
    padding-top: 0.5rem; /* hover bridge between button and card */
  }
  .info-wrap:hover .info-pop,
  .info-wrap:focus-within .info-pop,
  .info-wrap.pinned .info-pop {
    display: block;
  }
  .info-card {
    position: relative;
    color: var(--fg-muted);
    font-size: 0.92rem;
    padding: 1.1rem 1.3rem;
    border: 1px solid var(--border-strong);
    border-radius: var(--radius);
    background: color-mix(in oklab, var(--bg-elev) 97%, transparent);
    backdrop-filter: blur(8px);
    box-shadow: var(--shadow-lift);
  }
  .info-card .w-title {
    font-size: 1.2rem;
    font-family: var(--font-display);
    color: var(--fg);
    margin: 0 0 0.5rem;
  }
  .info-card p { margin: 0 0 0.55rem; }
  .info-card .hint-row { margin-bottom: 0; }
  .info-card .x {
    position: absolute;
    top: 0.4rem;
    right: 0.6rem;
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 1.1rem;
    cursor: pointer;
    padding: 0.2rem;
  }
  .info-card .x:hover { color: var(--fg); }
  kbd {
    background: rgba(74, 104, 82, 0.12);
    border: 1px solid var(--border);
    border-radius: 5px;
    padding: 0.05em 0.4em;
    font-size: 0.82em;
  }

  /* ---- tabs: scratch + opened library files ---- */
  .tabbar {
    display: flex;
    align-items: stretch;
    gap: 2px;
    padding: 0.25rem 0.6rem 0;
    border-bottom: 1px solid var(--border);
    background: var(--bg-elev);
    overflow-x: auto;
    scrollbar-width: thin;
  }
  .tab {
    display: inline-flex;
    align-items: center;
    gap: 0.35em;
    padding: 0.3em 0.7em;
    font-family: var(--font-mono);
    font-size: 0.78rem;
    color: var(--fg-muted);
    background: transparent;
    border: 1px solid transparent;
    border-bottom: none;
    border-radius: 8px 8px 0 0;
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
    margin-bottom: -1px; /* merge into the editor surface */
  }
  .tab.preview .tab-title {
    font-style: italic; /* a browsing stop, not a kept tab (VS Code style) */
  }
  .tab-dirty {
    color: var(--warn, #a8770f);
    font-size: 0.6rem;
  }
  .tab-x {
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 0.95rem;
    cursor: pointer;
    padding: 0 0.1em;
    border-radius: 4px;
    line-height: 1;
  }
  .tab-x:hover {
    color: var(--err);
    background: var(--bg-panel-hover);
  }
  .tab-spacer {
    flex: 1;
  }
  .fb-toggle {
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 0.95rem;
    cursor: pointer;
    padding: 0 0.5em;
    border-radius: 6px;
    align-self: center;
  }
  .fb-toggle:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .fb-toggle.on {
    color: var(--accent);
  }

  /* ---- the file browser drawer (left of the buffer) ---- */
  .fb {
    width: 13.5rem;
    flex: none;
    display: flex;
    flex-direction: column;
    min-height: 0;
    border-right: 1px solid var(--border);
    background: var(--bg-elev);
  }
  .fb-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.45rem 0.5rem 0.35rem 0.8rem;
    border-bottom: 1px solid var(--border);
  }
  .fb-title {
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.09em;
    color: var(--fg-faint);
  }
  .fb-new {
    background: none;
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    color: var(--fg-muted);
    font-size: 0.85rem;
    line-height: 1.2;
    cursor: pointer;
    padding: 0 0.35em;
  }
  .fb-new:hover {
    color: var(--accent);
    border-color: var(--accent);
  }
  .fb-newform {
    padding: 0.4rem 0.6rem;
    border-bottom: 1px solid var(--border);
  }
  .fb-newform input {
    width: 100%;
    background: var(--bg-panel);
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.78rem;
    padding: 0.25em 0.5em;
    outline: none;
  }
  .fb-list {
    flex: 1;
    overflow-y: auto;
    padding: 0.4rem 0.35rem;
    scrollbar-width: thin;
  }
  .files-loading {
    color: var(--fg-faint);
    font-size: 0.8rem;
    padding: 0.3rem 0.6rem;
  }
  .fdir-row {
    display: flex;
    align-items: center;
    gap: 0.3em;
    width: 100%;
    text-align: left;
    background: none;
    border: none;
    color: var(--fg-muted);
    font-size: 0.76rem;
    font-weight: 600;
    padding-top: 0.18rem;
    padding-bottom: 0.14rem;
    border-radius: 6px;
    cursor: pointer;
  }
  .fdir-row:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .chev {
    display: inline-block;
    font-size: 0.65rem;
    color: var(--fg-faint);
    transition: transform 0.15s ease;
  }
  .chev.closed {
    transform: rotate(-90deg);
  }
  .fitem-row {
    display: flex;
    align-items: center;
  }
  .fitem-row.opened .fitem {
    color: var(--fg);
    font-weight: 550;
  }
  .fitem {
    flex: 1;
    min-width: 0;
    text-align: left;
    background: none;
    border: none;
    color: var(--fg-muted);
    font-family: var(--font-mono);
    font-size: 0.78rem;
    padding: 0.16rem 0.4rem 0.16rem 0.85em; /* left inset past the chevron column */
    border-radius: 6px;
    cursor: pointer;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .fitem:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .f-del {
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 0.85rem;
    cursor: pointer;
    padding: 0 0.35em;
    border-radius: 4px;
    flex: none;
  }
  .f-del:hover {
    color: var(--err);
  }
  @media (max-width: 720px) {
    .fb {
      width: 11rem;
    }
  }

  /* ---- stage: the buffer (+ the on-demand visualizer panel) + overlays ---- */
  .stage {
    flex: 1;
    position: relative;
    display: flex;
    min-height: 0;
    background: var(--bg-code);
  }
  .ed-wrap {
    flex: 1;
    min-width: 0;
    min-height: 0;
  }

  /* ---- the reduction-visualizer pop-out ---- */
  /* no header, no scrollbar: the drawing is sized to what the body leaves
     after the instrument's own chrome; the close button floats. The panel
     explanation lives in the instrument's ⓘ (namesHint). */
  .viz-panel {
    position: relative;
    width: clamp(24rem, 42%, 46rem);
    min-width: 0;
    display: flex;
    flex-direction: column;
    border-left: 1px solid var(--border);
    background: var(--bg);
    overflow: hidden;
  }
  .viz-x {
    position: absolute;
    top: 0.35rem;
    right: 0.5rem;
    z-index: 5;
    background: none;
    border: none;
    color: var(--fg-faint);
    font-size: 1.2rem;
    cursor: pointer;
    padding: 0.1rem 0.3rem;
  }
  .viz-x:hover {
    color: var(--fg);
  }
  .viz-body {
    flex: 1;
    min-height: 0;
    padding: 0.4rem 0.6rem;
  }
  /* the instrument's lab frame brings page margins the panel doesn't want */
  .viz-body :global(.vis.lab) {
    margin: 0;
  }
  @media (max-width: 900px) {
    .stage {
      flex-direction: column;
    }
    .viz-panel {
      width: 100%;
      max-height: 55%;
      border-left: none;
      border-top: 1px solid var(--border);
    }
  }

  .errstrip {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    z-index: 5;
    padding: 0.45rem 0.9rem;
    font-family: var(--font-mono);
    font-size: 0.76rem;
    color: var(--err);
    background: color-mix(in oklab, var(--err) 7%, var(--bg-elev));
    border-top: 1px solid var(--border);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  /* ---- toasts ---- */
  .toasts {
    position: absolute;
    top: 0.6rem;
    right: 0.8rem;
    z-index: 8;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 0.4rem;
    pointer-events: none;
    max-width: min(26rem, calc(100% - 2rem));
  }
  .toast {
    padding: 0.45rem 0.75rem;
    border: 1px solid var(--border-strong);
    border-radius: 9px;
    background: color-mix(in oklab, var(--bg-elev) 95%, transparent);
    backdrop-filter: blur(6px);
    box-shadow: var(--shadow-soft);
    font-size: 0.8rem;
    color: var(--fg-muted);
    animation: toast-in 0.2s ease;
  }
  .toast.warn {
    border-color: color-mix(in oklab, var(--err) 40%, transparent);
    color: var(--err);
  }
  @keyframes toast-in { from { opacity: 0; transform: translateY(-4px); } }

  /* ---- eval result + repl ---- */
  .eval-result {
    display: flex;
    align-items: baseline;
    gap: 0.7rem;
    border-top: 1px solid var(--border);
    padding: 0.5rem 0.9rem;
    background: var(--bg-panel);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    max-height: 9rem;
    overflow-y: auto;
  }
  .eval-result.bad { border-left: 3px solid var(--err); }
  .eval-result .ee { color: var(--accent); font-size: 0.8rem; white-space: nowrap; }
  .eval-result .ev { word-break: break-all; }
  .eval-result .hint { color: var(--g1); }
  .eval-result pre.err {
    margin: 0;
    color: var(--err);
    font-size: 0.78rem;
    white-space: pre-wrap;
    word-break: break-word;
    flex: 1;
  }
  .eval-result .x {
    margin-left: auto;
    background: none;
    border: none;
    color: var(--fg-faint);
    cursor: pointer;
    font-size: 1rem;
    padding: 0 0.2rem;
    align-self: flex-start;
  }
  .eval-result .x:hover { color: var(--fg); }

  .repl {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    border-top: 1px solid var(--border);
    padding: 0.55rem 0.9rem;
    background: var(--bg-elev);
  }
  .prompt { color: var(--accent); font-weight: 700; }
  .prompt.busy { animation: pulse 1.1s ease-in-out infinite; }
  .repl input {
    flex: 1;
    background: none;
    border: none;
    outline: none;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.86rem;
  }
  .repl input::placeholder { color: var(--fg-faint); }

</style>
