<script lang="ts">
  import { onMount } from 'svelte'
  import { disp } from '$lib/disp/client.svelte'
  import type { RunOutcome, ItemEvent } from '$lib/disp/protocol'
  import { examples } from '$lib/disp/examples'
  import DispEditor, { type EditorApi, type LineMark } from '$lib/components/DispEditor.svelte'

  // ---- output model -------------------------------------------------------

  type Entry =
    | { kind: 'sys'; text: string }
    | { kind: 'progress'; count: number; current: string; done: boolean; label: string }
    | { kind: 'run'; label: string; outcome: RunOutcome; kernelItems: number }
    | { kind: 'eval'; expr: string; outcome: RunOutcome }

  let entries = $state<Entry[]>([])
  let outputEl: HTMLDivElement | undefined = $state()

  // approximate item count of a cold kernel elaboration (measured; only used
  // to shape the progress bar)
  const KERNEL_ITEMS = 762

  let editorApi: EditorApi | undefined
  let currentDoc = $state('')
  let exampleId = $state('welcome')
  let replInput = $state('')
  let replHistory: string[] = []
  let replHistIdx = -1
  let busy = $state(false)

  // live progress for the in-flight request
  let progressEntry: (Entry & { kind: 'progress' }) | null = null
  let kernelItemsThisRun = 0

  disp.onItem = (e: ItemEvent) => {
    if (e.depth > 0) {
      kernelItemsThisRun++
      if (progressEntry) {
        progressEntry.count = kernelItemsThisRun
        progressEntry.current = `${shortPath(e.sourcePath)}${e.name ? ' · ' + e.name : ''}`
      }
    }
  }

  const shortPath = (p?: string) => (p ? p.replace(/^.*\/lib\//, 'lib/') : '')

  function pushEntry<T extends Entry>(e: T): T {
    entries.push(e)
    const stored = entries[entries.length - 1] as T
    queueMicrotask(() => outputEl?.scrollTo({ top: outputEl.scrollHeight, behavior: 'smooth' }))
    return stored
  }

  function beginProgress(label: string) {
    kernelItemsThisRun = 0
    progressEntry = pushEntry({ kind: 'progress', count: 0, current: '', done: false, label })
  }

  function endProgress() {
    if (progressEntry) {
      progressEntry.done = true
      progressEntry = null
    }
  }

  // ---- actions ------------------------------------------------------------

  function marksFromOutcome(out: RunOutcome): LineMark[] {
    const marks: LineMark[] = []
    for (const t of out.tests) if (t.line) marks.push({ line: t.line, kind: t.pass ? 'pass' : 'fail' })
    if (out.errorLine) marks.push({ line: out.errorLine, kind: 'error' })
    return marks
  }

  async function execute(source: string, label: string) {
    if (busy) return
    busy = true
    beginProgress(disp.kernelLoaded ? label : `${label} (first run checks the kernel too)`)
    try {
      const out = await disp.run(source)
      endProgress()
      pushEntry({ kind: 'run', label, outcome: out, kernelItems: kernelItemsThisRun })
      editorApi?.setMarks(marksFromOutcome(out))
    } catch (e) {
      endProgress()
      pushEntry({ kind: 'sys', text: `⚠ ${e instanceof Error ? e.message : e}` })
    } finally {
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

  async function runRepl() {
    const expr = replInput.trim()
    if (!expr || busy) return
    replHistory.push(expr)
    replHistIdx = replHistory.length
    replInput = ''
    busy = true
    beginProgress(`▸ ${expr}`)
    try {
      const out = await disp.evalExpr(editorApi?.getDoc() ?? currentDoc, expr)
      endProgress()
      pushEntry({ kind: 'eval', expr, outcome: out })
    } catch (e) {
      endProgress()
      pushEntry({ kind: 'sys', text: `⚠ ${e instanceof Error ? e.message : e}` })
    } finally {
      busy = false
    }
  }

  function replKeydown(e: KeyboardEvent) {
    if (e.key === 'Enter') {
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
    beginProgress('elaborating + self-verifying the kernel')
    try {
      const out = await disp.loadKernel()
      endProgress()
      pushEntry({
        kind: 'sys',
        text: out.ok
          ? `kernel loaded + verified · ${(out.elapsedMs / 1000).toFixed(1)}s · ${fmtSteps(out.steps)} steps. later runs reuse it`
          : `kernel load failed: ${out.error}`
      })
    } catch (e) {
      endProgress()
      pushEntry({ kind: 'sys', text: `⚠ ${e instanceof Error ? e.message : e}` })
    } finally {
      busy = false
    }
  }

  function stop() {
    disp.interrupt()
    endProgress()
    busy = false
    pushEntry({ kind: 'sys', text: 'interrupted. engine reset, kernel cache cleared' })
  }

  async function resetSession() {
    if (busy) return
    await disp.reset()
    pushEntry({ kind: 'sys', text: 'session reset. fresh arena, kernel cache cleared' })
  }

  function loadExample(id: string) {
    const ex = examples.find((x) => x.id === id)
    if (!ex) return
    exampleId = id
    editorApi?.setDoc(ex.source)
    currentDoc = ex.source
    persist()
  }

  // ---- persistence --------------------------------------------------------

  const LS_DOC = 'disp-playground-doc'
  const LS_EX = 'disp-playground-example'
  function persist() {
    try {
      localStorage.setItem(LS_DOC, currentDoc)
      localStorage.setItem(LS_EX, exampleId)
    } catch {}
  }

  let initialDoc = examples[0].source
  onMount(() => {
    // ?example=<id> deep-links (landing page showcase) override the saved doc
    const wanted = new URLSearchParams(location.search).get('example')
    if (wanted && examples.some((x) => x.id === wanted)) {
      loadExample(wanted)
    } else {
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
    void disp.init()
  })

  // ---- formatting ---------------------------------------------------------

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
            : disp.kernelLoaded
              ? 'kernel ✓'
              : 'ready · kernel not loaded'
  )
</script>

<svelte:head>
  <title>Playground · disp</title>
</svelte:head>

<div class="pg">
  <div class="toolbar">
    <button class="btn primary tb" onclick={runFile} disabled={busy} title="Ctrl/⌘-Enter">
      ▶ Run file
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
    <div class="spacer"></div>
    <div class="status" class:live={busy} class:dead={disp.status === 'dead'}>
      <span class="dot"></span>
      {statusLabel}
      {#if !disp.kernelLoaded && !busy && disp.status !== 'dead'}
        <button class="linkbtn" onclick={preloadKernel}>load kernel</button>
      {/if}
    </div>
    {#if disp.memBytes > 0}
      <div class="mem" title="WASM arena: {(disp.memBytes / 1e9).toFixed(2)}GB of 4GB">
        <div class="mem-fill" class:hot={memPct > 85} style="width:{memPct}%"></div>
      </div>
    {/if}
    <button class="btn tb subtle" onclick={resetSession} disabled={busy} title="Fresh session: clears the arena and the kernel cache">
      Reset
    </button>
  </div>

  <div class="split">
    <section class="pane ed">
      <DispEditor
        doc={currentDoc || initialDoc}
        onDocChange={(d) => {
          currentDoc = d
          persist()
        }}
        onRunFile={runFile}
        onRunToCursor={runToCursor}
        api={(a) => (editorApi = a)}
      />
    </section>

    <section class="pane out">
      <div class="entries" bind:this={outputEl}>
        {#if entries.length === 0}
          <div class="empty">
            <p class="empty-title">The <span class="grad-text">real toolchain</span>, in your tab.</p>
            <p>
              This playground runs disp's actual elaborator and its Rust evaluator (compiled to
              WebAssembly), the same code that checks the test suite. Nothing is sent to a server.
            </p>
            <p>
              The first run that touches the type system elaborates <em>and re-verifies</em> the
              kernel: about a minute of genuine self-checking, cached for the session.
              <button class="linkbtn" onclick={preloadKernel}>Start it now</button>, or pick the
              raw tree-calculus example for instant gratification.
            </p>
            <p class="hint-row">
              <kbd>⌘⏎</kbd> run file · <kbd>⇧⏎</kbd> run to cursor · the <kbd>▸</kbd> prompt below
              evaluates expressions
            </p>
          </div>
        {/if}
        {#each entries as e}
          {#if e.kind === 'sys'}
            <div class="entry sys">{e.text}</div>
          {:else if e.kind === 'progress'}
            {#if !e.done}
              <div class="entry progress">
                <div class="prog-label">{e.label}</div>
                <div class="prog-bar">
                  <div class="prog-fill" style="width:{Math.min(99, (e.count / KERNEL_ITEMS) * 100)}%"></div>
                </div>
                <div class="prog-current">{e.count > 0 ? `${e.count} items · ${e.current}` : 'elaborating…'}</div>
              </div>
            {/if}
          {:else if e.kind === 'run'}
            <div class="entry run" class:ok={e.outcome.ok} class:bad={!e.outcome.ok}>
              <div class="run-head">
                <span class="run-label">{e.label}</span>
                <span class="run-meta">
                  {fmtMs(e.outcome.elapsedMs)} · {fmtSteps(e.outcome.steps)} steps
                  {#if e.kernelItems > 100}· kernel loaded ✓{/if}
                </span>
              </div>
              {#if e.outcome.error}
                <pre class="err">{e.outcome.error}</pre>
              {:else}
                <div class="run-sum">
                  {e.outcome.defs.length} def{e.outcome.defs.length === 1 ? '' : 's'}
                  {#if e.outcome.defs.length > 0}
                    <span class="defnames">({e.outcome.defs.map((d) => d.name).join(', ')})</span>
                  {/if}
                  · {e.outcome.tests.filter((t) => t.pass).length}/{e.outcome.tests.length} tests pass
                </div>
                {#each e.outcome.tests as t}
                  <button class="test" class:pass={t.pass} class:fail={!t.pass} onclick={() => t.line && editorApi?.gotoLine(t.line)}>
                    <span class="mark">{t.pass ? '✓' : '✗'}</span>
                    test {t.index}{t.line ? ` · line ${t.line}` : ''}
                    {#if !t.pass && t.lhs}
                      <pre class="mismatch">lhs = {t.lhs}
rhs = {t.rhs}</pre>
                    {/if}
                  </button>
                {/each}
              {/if}
            </div>
          {:else if e.kind === 'eval'}
            <div class="entry eval" class:bad={!!e.outcome.error}>
              <div class="eval-expr">▸ {e.expr}</div>
              {#if e.outcome.error}
                <pre class="err">{e.outcome.error}</pre>
              {:else}
                <div class="eval-val">
                  {e.outcome.value ?? '(no value)'}
                  {#if e.outcome.valueHint && e.outcome.valueHint !== e.outcome.value}
                    <span class="hint">≈ {e.outcome.valueHint}</span>
                  {/if}
                </div>
              {/if}
            </div>
          {/if}
        {/each}
      </div>
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
    </section>
  </div>
</div>

<style>
  .pg {
    display: flex;
    flex-direction: column;
    height: calc(100dvh - var(--nav-h));
    min-height: 480px;
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
  .status.dead .dot { background: var(--err); }
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
  .mem {
    width: 68px;
    height: 6px;
    border-radius: 3px;
    background: rgba(74, 104, 82, 0.14);
    overflow: hidden;
  }
  .mem-fill { height: 100%; background: var(--grad-brand); transition: width 0.3s ease; }
  .mem-fill.hot { background: var(--err); }

  /* ---- split ---- */
  .split {
    flex: 1;
    display: grid;
    grid-template-columns: minmax(0, 11fr) minmax(0, 9fr);
    min-height: 0;
  }
  .pane { min-height: 0; }
  .ed { border-right: 1px solid var(--border); background: var(--bg-code); }
  .out {
    display: flex;
    flex-direction: column;
    background: var(--bg);
    min-height: 0;
  }
  .entries {
    flex: 1;
    overflow-y: auto;
    padding: 0.9rem;
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
  }

  /* ---- empty state ---- */
  .empty {
    color: var(--fg-muted);
    font-size: 0.92rem;
    padding: 1.2rem 1.3rem;
    border: 1px dashed var(--border-strong);
    border-radius: var(--radius);
  }
  .empty-title { font-size: 1.25rem; font-family: var(--font-display); color: var(--fg); margin: 0 0 0.5rem; }
  .hint-row { margin-bottom: 0; }
  kbd {
    background: rgba(74, 104, 82, 0.12);
    border: 1px solid var(--border);
    border-radius: 5px;
    padding: 0.05em 0.4em;
    font-size: 0.82em;
  }

  /* ---- entries ---- */
  .entry {
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 0.6rem 0.8rem;
    font-size: 0.85rem;
    background: var(--bg-panel);
  }
  .entry.sys { color: var(--fg-muted); font-family: var(--font-mono); font-size: 0.8rem; }
  .entry.run.ok { border-left: 3px solid var(--ok); }
  .entry.run.bad, .entry.eval.bad { border-left: 3px solid var(--err); }

  .entry.progress { border-left: 3px solid var(--accent); }
  .prog-label { font-weight: 550; margin-bottom: 0.4rem; }
  .prog-bar {
    height: 7px;
    border-radius: 4px;
    background: rgba(74, 104, 82, 0.14);
    overflow: hidden;
    margin-bottom: 0.35rem;
  }
  .prog-fill {
    height: 100%;
    background: var(--grad-brand);
    transition: width 0.25s ease;
    box-shadow: 0 0 12px color-mix(in oklab, var(--g2) 50%, transparent);
  }
  .prog-current {
    font-family: var(--font-mono);
    font-size: 0.72rem;
    color: var(--fg-muted);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .run-head { display: flex; justify-content: space-between; gap: 1rem; margin-bottom: 0.3rem; }
  .run-label { font-weight: 600; }
  .run-meta { color: var(--fg-faint); font-size: 0.76rem; white-space: nowrap; }
  .run-sum { color: var(--fg-muted); margin-bottom: 0.35rem; }
  .defnames { color: var(--fg-faint); }

  .test {
    display: block;
    width: 100%;
    text-align: left;
    background: none;
    border: none;
    color: var(--fg-muted);
    font: inherit;
    font-size: 0.82rem;
    padding: 0.14rem 0.2rem;
    border-radius: 6px;
    cursor: pointer;
  }
  .test:hover { background: var(--bg-panel-hover); }
  .test .mark { display: inline-block; width: 1.2em; font-weight: 700; }
  .test.pass .mark { color: var(--ok); }
  .test.fail { color: var(--fg); }
  .test.fail .mark { color: var(--err); }
  .mismatch {
    margin: 0.3rem 0 0.2rem 1.2em;
    padding: 0.5rem 0.7rem;
    font-size: 0.76rem;
    white-space: pre-wrap;
    word-break: break-all;
  }
  pre.err {
    margin: 0.2rem 0 0;
    padding: 0.5rem 0.7rem;
    color: var(--err);
    font-size: 0.78rem;
    white-space: pre-wrap;
    word-break: break-word;
    border-color: color-mix(in oklab, var(--err) 30%, transparent);
  }

  .eval-expr { font-family: var(--font-mono); color: var(--accent); font-size: 0.82rem; }
  .eval-val {
    font-family: var(--font-mono);
    font-size: 0.88rem;
    margin-top: 0.25rem;
    word-break: break-all;
  }
  .eval-val .hint { color: var(--g1); margin-left: 0.6em; }

  /* ---- repl ---- */
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

  @media (max-width: 860px) {
    .pg { height: auto; }
    .split { grid-template-columns: minmax(0, 1fr); }
    .ed { height: 46dvh; border-right: none; border-bottom: 1px solid var(--border); }
    .out { height: 44dvh; }
  }
</style>
