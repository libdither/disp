<script lang="ts">
  // A highlighted disp code block, optionally runnable against the shared
  // in-browser compiler session (the same worker the Playground uses).
  import { highlightDisp } from '$lib/editor/highlight'
  import { disp } from '$lib/disp/client.svelte'
  import type { RunOutcome } from '$lib/disp/protocol'

  interface Props {
    code: string
    // runnable blocks get a ▶ button; `context` (opens/defs) is prepended but
    // not displayed
    runnable?: boolean
    context?: string
    lang?: string
  }
  let { code, runnable = false, context = '' }: Props = $props()

  let running = $state(false)
  let outcome = $state<RunOutcome | null>(null)
  let error = $state<string | null>(null)
  let kernelNote = $state(false)

  async function run() {
    if (running) return
    running = true
    error = null
    outcome = null
    kernelNote = !disp.kernelLoaded
    try {
      const src = context ? context.trimEnd() + '\n' + code + '\n' : code + '\n'
      const out = await disp.run(src)
      outcome = out
      if (!out.error) disp.kernelLoaded = disp.kernelLoaded || true
    } catch (e) {
      error = e instanceof Error ? e.message : String(e)
    } finally {
      running = false
      kernelNote = false
    }
  }

  const passCount = $derived(outcome ? outcome.tests.filter((t) => t.pass).length : 0)
</script>

<div class="codeblock">
  <pre>{@html highlightDisp(code)}</pre>
  {#if runnable}
    <div class="runbar">
      <button class="runbtn" onclick={run} disabled={running}>
        {#if running}
          <span class="spin"></span> {kernelNote ? 'verifying the kernel first (≈1 min, once per session)…' : 'running…'}
        {:else}
          ▶ run this block
        {/if}
      </button>
      {#if outcome}
        {#if outcome.error}
          <span class="res bad">error: {outcome.error.slice(0, 160)}</span>
        {:else}
          <span class="res" class:ok={outcome.ok} class:bad={!outcome.ok}>
            {passCount}/{outcome.tests.length} tests pass · {outcome.elapsedMs > 1000
              ? (outcome.elapsedMs / 1000).toFixed(1) + 's'
              : outcome.elapsedMs.toFixed(0) + 'ms'}
          </span>
        {/if}
      {:else if error}
        <span class="res bad">{error.slice(0, 160)}</span>
      {/if}
    </div>
  {/if}
</div>

<style>
  .codeblock { margin: 1.1rem 0; }
  pre {
    margin: 0;
    font-size: 0.82rem;
  }
  .runbar {
    display: flex;
    align-items: center;
    gap: 0.8rem;
    flex-wrap: wrap;
    border: 1px solid var(--border);
    border-top: none;
    border-radius: 0 0 var(--radius) var(--radius);
    padding: 0.35rem 0.8rem;
    background: var(--bg-elev);
    font-size: 0.78rem;
  }
  .codeblock:has(.runbar) pre {
    border-bottom-left-radius: 0;
    border-bottom-right-radius: 0;
  }
  .runbtn {
    display: inline-flex;
    align-items: center;
    gap: 0.5em;
    background: none;
    border: none;
    color: var(--accent);
    font: inherit;
    font-size: 0.78rem;
    font-weight: 600;
    cursor: pointer;
    padding: 0.2em 0;
  }
  .runbtn:hover:not(:disabled) { text-decoration: underline; text-underline-offset: 3px; }
  .runbtn:disabled { color: var(--fg-muted); cursor: wait; }
  .spin {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    border: 2px solid var(--accent);
    border-top-color: transparent;
    animation: rot 0.8s linear infinite;
  }
  @keyframes rot { to { transform: rotate(360deg); } }
  .res { font-family: var(--font-mono); }
  .res.ok { color: var(--ok); }
  .res.bad { color: var(--err); }
</style>
