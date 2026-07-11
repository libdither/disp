<script lang="ts">
  // The landing page's live code card: a real disp editor wired to the shared
  // compiler worker (the same session the Playground uses), with test results
  // rendered inline in the code — rechecked on a debounce after every edit.
  // Two faces on a 3D flip (code / ambient TreeVis; the initial face is
  // random): the flip axis matches the layout — up-down in the normal card,
  // left-right in theatre — and the floating right-edge picker orients the
  // same way. The YouTube-style theatre toggle's actual layout change lives
  // in +page.svelte (the grid owner); the card only reports the press.
  import { onMount } from 'svelte'
  import DispEditor, { type EditorApi, type LineMark } from './DispEditor.svelte'
  import TreeVis from './TreeVis.svelte'
  import { disp } from '$lib/disp/client.svelte'
  import { examples } from '$lib/disp/examples'

  interface Props {
    theatre?: boolean
    flipped?: boolean
    onToggleTheatre?: () => void
  }
  let { theatre = false, flipped = $bindable(false), onToggleTheatre }: Props = $props()

  const hero = examples.find((e) => e.id === 'hello') ?? examples[0]

  let editor: EditorApi | undefined
  let running = $state(false)
  let summary = $state<{ pass: number; total: number; ms: number } | null>(null)
  let runError = $state<string | null>(null)

  // TreeVis mounts on first flip and survives the flip-back animation, then
  // unmounts so it isn't animating unseen behind the editor.
  let showBack = $state(false)
  let backTimer: ReturnType<typeof setTimeout> | undefined
  $effect(() => {
    if (flipped) {
      clearTimeout(backTimer)
      showBack = true
    } else if (showBack) {
      clearTimeout(backTimer)
      backTimer = setTimeout(() => (showBack = false), 700)
    }
  })

  // The flip transition is suppressed while the axis swaps with theatre mode
  // (a live rotateX -> rotateY transition tumbles the card) and for the
  // randomized initial face on mount.
  let snap = $state(true)
  $effect(() => {
    void theatre
    snap = true
    const t = setTimeout(() => (snap = false), 80)
    return () => clearTimeout(t)
  })

  // Flipping can reflow content ABOVE the card (the hero button's label
  // follows the face), which would slide the card under the viewport in
  // theatre mode. Measure the card's viewport position across the DOM update
  // and scroll the residual shift away — a no-op when the browser's own
  // scroll anchoring (or the fixed-width button) already kept it still.
  let rootEl: HTMLDivElement | undefined
  let preFlipTop: number | null = null
  $effect.pre(() => {
    void flipped
    preFlipTop = rootEl?.getBoundingClientRect().top ?? null
  })
  $effect(() => {
    void flipped
    if (preFlipTop == null || !rootEl) return
    const d = rootEl.getBoundingClientRect().top - preFlipTop
    preFlipTop = null
    if (Math.abs(d) > 1) window.scrollBy({ top: d, behavior: 'instant' as ScrollBehavior })
  })

  // Recheck on edit: rerun the buffer as soon as typing pauses (only once the
  // kernel is warm — a cold session never starts the minute-long load
  // uninvited). The debounce is short because a warm rerun costs single-digit
  // milliseconds; an in-flight run just re-arms it.
  let editTimer: ReturnType<typeof setTimeout> | undefined
  function onEdit(): void {
    clearTimeout(editTimer)
    editTimer = setTimeout(() => {
      if (!disp.kernelLoaded || disp.status === 'dead') return
      if (running) return onEdit()
      void run()
    }, 150)
  }

  const statusText = $derived.by(() => {
    switch (disp.status) {
      case 'cold':
      case 'booting':
        return 'starting engine…'
      case 'restoring-kernel':
        return 'waking the kernel…'
      case 'loading-kernel':
        return 'verifying the kernel from source…'
      case 'running':
        return 'running…'
      case 'dead':
        return 'engine crashed — reload the page'
      case 'ready':
        if (summary)
          return `${summary.pass}/${summary.total} tests ✓ · ${Math.max(1, Math.round(summary.ms))}ms`
        return disp.kernelLoaded ? 'kernel ready' : 'ready'
    }
  })
  const statusKind = $derived(
    disp.status === 'dead'
      ? 'bad'
      : disp.status !== 'ready'
        ? 'busy'
        : summary && summary.pass < summary.total
          ? 'bad'
          : summary
            ? 'good'
            : 'idle'
  )

  async function run(): Promise<void> {
    if (running || !editor) return
    running = true
    runError = null
    try {
      const out = await disp.run(editor.getDoc())
      const marks: LineMark[] = []
      for (const t of out.tests) {
        if (!t.line) continue
        marks.push(
          t.pass
            ? { line: t.line, kind: 'pass', note: '✓' }
            : { line: t.line, kind: 'fail', note: `✗ got ${t.lhs ?? '?'}, want ${t.rhs ?? '?'}` }
        )
      }
      if (out.error) {
        runError = out.error
        if (out.errorLine)
          marks.push({
            line: out.errorLine,
            kind: 'error',
            note: out.error.length > 90 ? out.error.slice(0, 90) + '…' : out.error
          })
      }
      editor.setMarks(marks)
      summary = out.error
        ? null
        : { pass: out.tests.filter((t) => t.pass).length, total: out.tests.length, ms: out.elapsedMs }
    } catch (e) {
      runError = e instanceof Error ? e.message : String(e)
      summary = null
    } finally {
      running = false
    }
  }

  onMount(() => {
    flipped = Math.random() < 0.5 // land on either face (the snap guard keeps this from animating)
    void (async () => {
      try {
        await disp.init()
        // Auto-run only when the precompiled kernel restored — never spend a
        // visitor's minute uninvited.
        if (disp.kernelLoaded) await run()
      } catch {
        /* status chip already says 'dead' */
      }
    })()
    return () => {
      clearTimeout(backTimer)
      clearTimeout(editTimer)
    }
  })
</script>

<div class="hero-card" class:flipped class:theatre bind:this={rootEl}>
  <div class="flipper" class:snap>
    <div class="face front" style:pointer-events={flipped ? 'none' : 'auto'}>
      <div class="toolbar">
        <span class="fname">hello.disp</span>
        <span class="status {statusKind}">{statusText}</span>
        <button class="tbtn run" onclick={() => void run()} disabled={running || disp.status === 'dead'} title="run (Ctrl/⌘-Enter)">
          <svg viewBox="0 0 16 16" aria-hidden="true"><path d="M5 3.5v9l7.2-4.5z" fill="currentColor" /></svg>
          Run
        </button>
        <button
          class="tbtn icon"
          onclick={() => onToggleTheatre?.()}
          title={theatre ? 'exit theatre mode' : 'theatre mode'}
          aria-label={theatre ? 'exit theatre mode' : 'theatre mode'}
        >
          {#if theatre}
            <svg viewBox="0 0 16 16" aria-hidden="true">
              <rect x="1.5" y="3.5" width="13" height="9" rx="1.2" fill="none" stroke="currentColor" stroke-width="1.4" />
              <rect x="4.5" y="6" width="7" height="4" rx="0.8" fill="currentColor" />
            </svg>
          {:else}
            <svg viewBox="0 0 16 16" aria-hidden="true">
              <rect x="1.5" y="3.5" width="13" height="9" rx="1.2" fill="none" stroke="currentColor" stroke-width="1.4" />
            </svg>
          {/if}
        </button>
      </div>
      <div class="editwrap">
        <DispEditor
          doc={hero.source}
          onDocChange={onEdit}
          onRunFile={() => void run()}
          api={(a) => (editor = a)}
        />
      </div>
      {#if runError}
        <div class="errstrip" title={runError}>⚠ {runError}</div>
      {/if}
    </div>
    <div class="face back" style:pointer-events={flipped ? 'auto' : 'none'}>
      <div class="vizwrap">
        {#if showBack}
          <TreeVis />
        {/if}
      </div>
    </div>
  </div>
  <!-- uniform face picker: rides OUTSIDE the flipper so it never mirrors;
       stacks with the flip axis (column for up-down, row for left-right) -->
  <div class="face-picker" role="group" aria-label="card face">
    <button
      class="fp-btn"
      class:active={!flipped}
      onclick={() => (flipped = false)}
      title="see the code"
      aria-label="see the code"
      aria-pressed={!flipped}
    >
      <svg viewBox="0 0 16 16" aria-hidden="true">
        <path
          d="M5.5 4.5 2.5 8l3 3.5M10.5 4.5l3 3.5-3 3.5"
          fill="none"
          stroke="currentColor"
          stroke-width="1.6"
          stroke-linecap="round"
          stroke-linejoin="round"
        />
      </svg>
    </button>
    <button
      class="fp-btn"
      class:active={flipped}
      onclick={() => (flipped = true)}
      title="visualize the tree calculus"
      aria-label="visualize the tree calculus"
      aria-pressed={flipped}
    >
      <svg viewBox="0 0 16 16" aria-hidden="true">
        <path d="M8 5.5 4.6 10.5M8 5.5l3.4 5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" fill="none" />
        <circle cx="8" cy="4" r="1.8" fill="currentColor" />
        <circle cx="4.6" cy="11.5" r="1.8" fill="currentColor" />
        <circle cx="11.4" cy="11.5" r="1.8" fill="currentColor" />
      </svg>
    </button>
  </div>
</div>

<style>
  .hero-card {
    position: relative;
    height: 100%;
    min-height: 30rem;
    perspective: 2200px;
  }
  .flipper {
    position: absolute;
    inset: 0;
    transform-style: preserve-3d;
    transition: transform 0.65s cubic-bezier(0.35, 0.1, 0.22, 1);
  }
  .flipper.snap {
    transition: none;
  }
  /* flip axis follows the layout: up-down in the normal card, left-right in theatre */
  .hero-card.flipped:not(.theatre) .flipper {
    transform: rotateX(180deg);
  }
  .hero-card.flipped.theatre .flipper {
    transform: rotateY(180deg);
  }
  .face {
    position: absolute;
    inset: 0;
    backface-visibility: hidden;
    display: flex;
    flex-direction: column;
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: var(--radius);
    box-shadow: var(--shadow-lift);
    overflow: clip; /* clip, not hidden: never a scroll-reveal target */
  }
  .face.back {
    background:
      radial-gradient(60% 55% at 50% 88%, color-mix(in oklab, var(--g1) 10%, transparent), transparent 75%),
      var(--bg-elev);
  }
  .hero-card:not(.theatre) .face.back {
    transform: rotateX(180deg);
  }
  .hero-card.theatre .face.back {
    transform: rotateY(180deg);
  }

  /* ---- front: toolbar + editor ---- */
  .toolbar {
    display: flex;
    align-items: center;
    gap: 0.6rem;
    padding: 0.45rem 0.6rem 0.45rem 0.85rem;
    border-bottom: 1px solid var(--border);
    background: color-mix(in oklab, var(--g2) 4%, var(--bg-elev));
    flex: none;
  }
  .fname {
    font-family: var(--font-mono);
    font-size: 0.78rem;
    color: var(--fg-muted);
  }
  .status {
    margin-left: auto;
    font-size: 0.78rem;
    color: var(--fg-faint);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .status.good {
    color: var(--ok);
  }
  .status.bad {
    color: var(--err);
  }
  .status.busy {
    color: var(--fg-muted);
  }
  .status.busy::after {
    content: '';
    display: inline-block;
    width: 0.55em;
    height: 0.55em;
    margin-left: 0.45em;
    border-radius: 50%;
    border: 1.5px solid currentColor;
    border-top-color: transparent;
    animation: hc-spin 0.8s linear infinite;
    vertical-align: -0.05em;
  }
  @keyframes hc-spin {
    to {
      transform: rotate(360deg);
    }
  }
  .tbtn {
    display: inline-flex;
    align-items: center;
    gap: 0.35em;
    padding: 0.3em 0.7em;
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    background: var(--bg-elev);
    color: var(--fg);
    font: inherit;
    font-size: 0.8rem;
    font-weight: 550;
    cursor: pointer;
    transition:
      background 0.15s ease,
      border-color 0.15s ease,
      transform 0.12s ease;
  }
  .tbtn:hover:not(:disabled) {
    background: #fff;
    border-color: var(--accent);
    transform: translateY(-1px);
  }
  .tbtn:disabled {
    opacity: 0.55;
    cursor: default;
  }
  .tbtn svg {
    width: 0.95em;
    height: 0.95em;
  }
  .tbtn.run {
    color: var(--accent);
  }
  .tbtn.icon {
    padding: 0.3em 0.45em;
  }
  .editwrap {
    flex: 1;
    min-height: 0;
  }
  .editwrap :global(.cm-editor) {
    font-size: 13.5px;
  }
  .errstrip {
    /* overlays the editor bottom: appearing/vanishing on fast rechecks must
       not reflow the editor */
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    z-index: 5;
    padding: 0.4rem 0.85rem;
    font-family: var(--font-mono);
    font-size: 0.74rem;
    color: var(--err);
    background: color-mix(in oklab, var(--err) 7%, var(--bg-elev));
    border-top: 1px solid var(--border);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  /* ---- back: the visualizer ---- */
  /* controls (when opened) sit at the TOP like a toolbar; the svg flexes to
     fill the rest and its preserveAspectRatio letterboxing centers the scene */
  .vizwrap {
    flex: 1;
    min-height: 0;
    padding: 0.75rem;
    display: flex;
  }
  .vizwrap :global(.vis) {
    display: flex;
    flex-direction: column;
  }
  .vizwrap :global(.vis > svg) {
    flex: 1;
    min-height: 0;
  }

  /* ---- the uniform face picker (code | tree) ---- */
  .face-picker {
    position: absolute;
    right: 0.55rem;
    top: 50%;
    transform: translateY(-50%);
    display: flex;
    flex-direction: column;
    gap: 2px;
    padding: 3px;
    background: color-mix(in oklab, var(--bg-elev) 92%, transparent);
    border: 1px solid var(--border-strong);
    border-radius: 999px;
    box-shadow: var(--shadow-soft);
    z-index: 6;
  }
  /* hang just OUTSIDE the card's right edge where the page margin has room;
     narrower viewports fall back to the inside placement above */
  @media (min-width: 1200px) {
    .face-picker {
      right: 0;
      transform: translate(calc(100% + 0.6rem), -50%);
    }
  }
  /* theatre: the flip is left-right, so the picker rides HORIZONTALLY, and it
     hangs above the card's top edge (the theatre scroll + grid gap reserve
     the lane) instead of off the right side */
  .hero-card.theatre .face-picker {
    flex-direction: row;
    top: 0;
    right: 1rem;
    transform: translateY(calc(-100% - 0.55rem));
  }
  .fp-btn {
    width: 1.85rem;
    height: 1.85rem;
    display: grid;
    place-items: center;
    border: none;
    border-radius: 50%;
    background: transparent;
    color: var(--fg-faint);
    cursor: pointer;
    transition:
      background 0.15s ease,
      color 0.15s ease;
  }
  .fp-btn:hover {
    color: var(--fg);
  }
  .fp-btn.active {
    background: color-mix(in oklab, var(--g2) 15%, var(--bg-elev));
    color: var(--accent);
  }
  .fp-btn svg {
    width: 1rem;
    height: 1rem;
  }
</style>
