<script lang="ts">
  // The playground is a notebook, not an IDE: one full-width buffer whose
  // results render INLINE — every definition shows its reduced value under
  // its line, every test its verdict, errors their message — and, once the
  // kernel is warm, edits re-check live on a debounce. No output sidebar;
  // the remaining chrome is a toolbar, a run-progress overlay, toasts for
  // engine messages, and a one-line eval prompt at the bottom.
  import { onMount } from 'svelte'
  import { disp } from '$lib/disp/client.svelte'
  import type { RunOutcome, ItemEvent } from '$lib/disp/protocol'
  import { examples } from '$lib/disp/examples'
  import DispEditor, { type EditorApi, type LineMark } from '$lib/components/DispEditor.svelte'

  // approximate item count of a cold kernel elaboration (measured; only used
  // to shape the progress bar)
  const KERNEL_ITEMS = 762

  let editorApi: EditorApi | undefined
  let currentDoc = $state('')
  let exampleId = $state('welcome')
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
  let evalOut = $state<{ expr: string; value?: string; hint?: string; error?: string } | null>(null)
  let replHistory: string[] = []
  let replHistIdx = -1

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
  // def's output belongs after its body), test verdicts likewise.
  function marksFromOutcome(out: RunOutcome): LineMark[] {
    const marks: LineMark[] = []
    for (const d of out.defs) {
      const line = d.endLine ?? d.line
      if (line && d.pretty) marks.push({ line, kind: 'value', block: d.pretty })
    }
    for (const t of out.tests) {
      const line = t.endLine ?? t.line
      if (!line) continue
      marks.push(
        t.pass
          ? { line, kind: 'pass', note: '✓' }
          : { line, kind: 'fail', note: '✗', block: `got  ${t.lhs ?? '?'}\nwant ${t.rhs ?? '?'}` }
      )
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
    if (showWelcome && !out.error) dismissWelcome()
    // a heavy buffer shouldn't re-run on every keystroke — pause live mode.
    // A run that just paid the one-time kernel elaboration is exempt: its
    // slowness says nothing about the buffer.
    if (live && !opts?.coldLoad && out.elapsedMs > 2500) {
      setLive(false)
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
      const out = await disp.run(source, { wantDefPretty: true })
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
    persist()
    if (!live) return
    clearTimeout(editTimer)
    editTimer = setTimeout(() => {
      if (!live || disp.status === 'dead' || !canAutoRun(currentDoc)) return
      if (busy) return onEdit(currentDoc)
      void execute(currentDoc, 'live re-check')
    }, 350)
  }

  function setLive(v: boolean) {
    live = v
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
      const out = await disp.evalExpr(editorApi?.getDoc() ?? currentDoc, expr, { wantDefPretty: true })
      if (out.error) {
        evalOut = { expr, error: out.error }
      } else {
        // an eval elaborates the whole buffer too — refresh the inline marks
        applyOutcome(out, { coldLoad: (progress?.count ?? 0) > 100 })
        evalOut = {
          expr,
          value: out.value ?? '(no value)',
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
    toast('interrupted — engine reset, kernel cache cleared', 'warn')
  }

  async function resetSession() {
    if (busy) return
    await disp.reset()
    summary = null
    evalOut = null
    stripError = null
    editorApi?.setMarks([])
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
  function persist() {
    try {
      localStorage.setItem(LS_DOC, currentDoc)
      localStorage.setItem(LS_EX, exampleId)
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
    </div>
    {#if disp.memBytes > 0}
      <div class="mem" title="WASM arena: {(disp.memBytes / 1e9).toFixed(2)}GB of 4GB">
        <div class="mem-fill" class:hot={memPct > 85} style="width:{memPct}%"></div>
      </div>
    {/if}
    <button class="btn tb subtle icon" onclick={() => (showWelcome = true)} title="about this playground" aria-label="about this playground">
      ⓘ
    </button>
    <button class="btn tb subtle" onclick={resetSession} disabled={busy} title="Fresh session: clears the arena and the kernel cache">
      Reset
    </button>
  </div>

  <div class="stage">
    <DispEditor
      doc={currentDoc || initialDoc}
      onDocChange={onEdit}
      onRunFile={runFile}
      onRunToCursor={runToCursor}
      api={(a) => (editorApi = a)}
    />

    {#if progress}
      <div class="progress-overlay">
        <div class="prog-label">{progress.label}</div>
        <div class="prog-bar">
          <div class="prog-fill" style="width:{Math.min(99, (progress.count / KERNEL_ITEMS) * 100)}%"></div>
        </div>
        <div class="prog-current">
          {progress.count > 0 ? `${progress.count} items · ${progress.current}` : 'elaborating…'}
        </div>
      </div>
    {/if}

    {#if stripError}
      <div class="errstrip" title={stripError}>⚠ {stripError}</div>
    {/if}

    {#if showWelcome}
      <div class="welcome">
        <button class="x" onclick={dismissWelcome} aria-label="dismiss">×</button>
        <p class="w-title">The <span class="grad-text">real toolchain</span>, in your tab.</p>
        <p>
          This playground runs disp's actual elaborator and its Rust evaluator (compiled to
          WebAssembly) — the same code that checks the test suite. Nothing is sent to a server.
        </p>
        <p>
          Results live in the buffer, notebook-style: every definition shows its reduced value,
          every test its verdict, and edits re-check live once the kernel is warm.
        </p>
        <p>
          The kernel arrives <em>precompiled</em> — a small snapshot of the checked module cache —
          so typed runs start in seconds. Don't trust it?
          <button class="linkbtn" onclick={() => { dismissWelcome(); void verifyKernel() }}>Verify from source</button>:
          about a minute of the type system genuinely re-checking itself, right here.
        </p>
        <p class="hint-row">
          <kbd>⌘⏎</kbd> run file · <kbd>⇧⏎</kbd> run to cursor · the <kbd>▸</kbd> prompt below
          evaluates expressions against the buffer
        </p>
      </div>
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
        <span class="ev">{evalOut.value}</span>
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
  .mem {
    width: 68px;
    height: 6px;
    border-radius: 3px;
    background: rgba(74, 104, 82, 0.14);
    overflow: hidden;
  }
  .mem-fill { height: 100%; background: var(--grad-brand); transition: width 0.3s ease; }
  .mem-fill.hot { background: var(--err); }

  /* ---- stage: the full-width buffer + overlays ---- */
  .stage {
    flex: 1;
    position: relative;
    min-height: 0;
    background: var(--bg-code);
  }

  .progress-overlay {
    position: absolute;
    top: 0.6rem;
    left: 50%;
    transform: translateX(-50%);
    width: min(34rem, calc(100% - 2rem));
    z-index: 7;
    padding: 0.55rem 0.8rem;
    border: 1px solid var(--border-strong);
    border-left: 3px solid var(--accent);
    border-radius: 10px;
    background: color-mix(in oklab, var(--bg-elev) 94%, transparent);
    backdrop-filter: blur(6px);
    box-shadow: var(--shadow-soft);
    /* warm runs finish in milliseconds — only surface the overlay when a run
       actually lasts (the delay keeps fast reruns flicker-free) */
    animation: prog-in 0.18s ease 0.25s backwards;
  }
  @keyframes prog-in { from { opacity: 0; transform: translate(-50%, -4px); } }
  .prog-label { font-weight: 550; margin-bottom: 0.4rem; font-size: 0.85rem; }
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

  /* ---- welcome overlay ---- */
  .welcome {
    position: absolute;
    left: 50%;
    bottom: 1rem;
    transform: translateX(-50%);
    width: min(38rem, calc(100% - 2rem));
    z-index: 6;
    color: var(--fg-muted);
    font-size: 0.92rem;
    padding: 1.1rem 1.3rem;
    border: 1px solid var(--border-strong);
    border-radius: var(--radius);
    background: color-mix(in oklab, var(--bg-elev) 96%, transparent);
    backdrop-filter: blur(8px);
    box-shadow: var(--shadow-lift);
  }
  .welcome .w-title {
    font-size: 1.2rem;
    font-family: var(--font-display);
    color: var(--fg);
    margin: 0 0 0.5rem;
  }
  .welcome p { margin: 0 0 0.55rem; }
  .welcome .hint-row { margin-bottom: 0; }
  .welcome .x {
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
  .welcome .x:hover { color: var(--fg); }
  kbd {
    background: rgba(74, 104, 82, 0.12);
    border: 1px solid var(--border);
    border-radius: 5px;
    padding: 0.05em 0.4em;
    font-size: 0.82em;
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

  @media (max-width: 860px) {
    .welcome { bottom: 0.6rem; }
  }
</style>
