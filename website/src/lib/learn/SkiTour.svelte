<script lang="ts">
  // A guided walk through the playground's tree viewer. The viewer is the
  // playground's own component; each step drives it and checks that what the
  // text says is what it shows (ski-tour.ts). In dev, or with ?tour-check in
  // the URL, a hidden second viewer checks every step on load.
  import { onMount } from 'svelte'
  import { disp } from '$lib/disp/client.svelte'
  import Inspector, { type ViewerApi } from '$lib/playground/Inspector.svelte'
  import type { Subject } from '$lib/playground/subject'
  import { steps, mismatches, place, type TourStep } from './ski-tour'

  const subjectFor = (expr: string): Subject => ({ key: `tour:${expr}`, kind: 'expr', title: expr, term: { expr } })
  const TIMEOUT_MS = 20000
  const frame = () => new Promise((r) => requestAnimationFrame(r))

  /// Load a step into a viewer and say where it disagrees with the step's text.
  async function visit(get: () => ViewerApi | undefined, setSubject: (s: Subject) => void, step: TourStep, alive: () => boolean): Promise<string[]> {
    await disp.init()
    const key = subjectFor(step.expr).key
    if (get()?.state().subject !== key) {
      setSubject(subjectFor(step.expr))
      await frame()
    }
    const t0 = performance.now()
    for (;;) {
      if (!alive()) return []
      const v = get()
      const s = v?.state()
      if (v && s && s.subject === key && (s.ready || s.error)) break
      if (performance.now() - t0 > TIMEOUT_MS) return [`the viewer didn't finish loading ${step.expr}`]
      await frame()
    }
    const v = get()!
    v.setSk(step.sk ?? true)
    v.seek(place(step, v.state()))
    return mismatches(step, v.state(), v.root)
  }

  // ---- the walkthrough you read ----
  let viewer: ViewerApi | undefined
  let subject = $state<Subject | null>(null)
  let i = $state(0)
  let errors = $state<string[]>([])
  let busy = $state(false)
  let started = $state(false)
  let token = 0
  let pointed: Element | null = null

  async function go(n: number) {
    const t = ++token
    i = n
    started = true
    busy = true
    errors = []
    pointed?.classList.remove('tour-point')
    const found = await visit(() => viewer, (s) => (subject = s), steps[n], () => t === token)
    if (t !== token) return
    busy = false
    errors = found
    for (const e of found) console.error(`[Learn SKI walkthrough] step ${n + 1} (${steps[n].title}): ${e}`)
    const p = steps[n].point
    pointed = p ? (viewer?.root.querySelector(`[data-tour="${p}"]`) ?? null) : null
    pointed?.classList.add('tour-point')
  }

  // ---- the check of every step ----
  let checkViewer: ViewerApi | undefined
  let checkSubject = $state<Subject | null>(null)
  let checking = $state(false)
  let report = $state<{ step: number; errors: string[] }[] | null>(null)
  const failed = $derived(report?.filter((r) => r.errors.length > 0) ?? [])

  async function checkAll() {
    const out: { step: number; errors: string[] }[] = []
    for (let n = 0; n < steps.length; n++) out.push({ step: n, errors: await visit(() => checkViewer, (s) => (checkSubject = s), steps[n], () => true) })
    report = out
    for (const r of out) for (const e of r.errors) console.error(`[Learn SKI walkthrough] step ${r.step + 1} (${steps[r.step].title}): ${e}`)
  }

  let host: HTMLDivElement
  onMount(() => {
    if (import.meta.env.DEV || new URLSearchParams(location.search).has('tour-check')) {
      checking = true
      void checkAll()
    }
    // start once the walkthrough is near the screen, so the page doesn't boot the compiler for nothing
    const io = new IntersectionObserver(
      (es) => {
        if (es.some((e) => e.isIntersecting) && !started) void go(0)
      },
      { rootMargin: '300px' }
    )
    io.observe(host)
    return () => io.disconnect()
  })
</script>

<div class="tour" bind:this={host}>
  {#if report}
    <div class="check" class:bad={failed.length > 0} data-tour-check={failed.length > 0 ? 'fail' : 'ok'}>
      {#if failed.length === 0}
        Walkthrough check: all {steps.length} steps match the viewer.
      {:else}
        <b>This walkthrough no longer matches the playground's viewer.</b> Update <code>website/src/lib/learn/ski-tour.ts</code>:
        <ul>
          {#each failed as r}
            {#each r.errors as e}<li>step {r.step + 1} ({steps[r.step].title}): {e}</li>{/each}
          {/each}
        </ul>
      {/if}
    </div>
  {:else if checking}
    <div class="check">Checking the walkthrough against the viewer…</div>
  {/if}

  <div class="panel">
    <ol class="dots" aria-label="steps">
      {#each steps as st, n}
        <li><button class:on={n === i} onclick={() => go(n)} title={st.title} aria-label="step {n + 1}: {st.title}"></button></li>
      {/each}
    </ol>
    <div class="say">
      <span class="count">{i + 1} / {steps.length}</span>
      <b>{steps[i].title}</b>
      <code class="expr">{steps[i].expr}</code>
      <p>{@html steps[i].text}</p>
      {#if errors.length > 0}
        <div class="stale">
          <b>Out of date:</b> the viewer no longer shows what this step says.
          <ul>{#each errors as e}<li>{e}</li>{/each}</ul>
        </div>
      {/if}
    </div>
    <div class="nav">
      <button class="btn sm" onclick={() => go(i - 1)} disabled={i === 0}>◀ Back</button>
      <button class="btn sm primary" onclick={() => go(i + 1)} disabled={i === steps.length - 1}>Next ▶</button>
      {#if busy}<span class="busy">loading…</span>{/if}
    </div>
  </div>

  <div class="stage">
    <Inspector {subject} runVersion={0} pinned={false} onExpr={(e) => (subject = subjectFor(e))} tour={(a) => (viewer = a)} />
  </div>
  <p class="foot">This is the <a href="../playground/">playground</a>'s tree viewer. Type your own expression in its box at the top.</p>

  {#if checking}
    <div class="offstage" aria-hidden="true">
      <Inspector subject={checkSubject} runVersion={0} pinned={false} onExpr={() => {}} tour={(a) => (checkViewer = a)} />
    </div>
  {/if}
</div>

<style>
  .tour {
    margin: 1.4rem 0;
  }
  .panel {
    border: 1px solid var(--border);
    border-bottom: none;
    border-radius: var(--radius) var(--radius) 0 0;
    background: var(--bg-panel);
    padding: 0.8rem 1rem;
  }
  .dots {
    display: flex;
    gap: 0.35rem;
    list-style: none;
    margin: 0 0 0.6rem;
    padding: 0;
  }
  .dots button {
    width: 0.7rem;
    height: 0.7rem;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: transparent;
    padding: 0;
    cursor: pointer;
  }
  .dots button.on {
    background: var(--accent);
    border-color: var(--accent);
  }
  .say {
    min-height: 6.5rem;
    font-size: 0.92rem;
  }
  .say p {
    margin: 0.4rem 0 0;
  }
  .count {
    color: var(--fg-faint);
    font-family: var(--font-mono);
    font-size: 0.78rem;
    margin-right: 0.5em;
  }
  .expr {
    margin-left: 0.6em;
    font-size: 0.8rem;
  }
  .nav {
    display: flex;
    gap: 0.5rem;
    align-items: center;
    margin-top: 0.6rem;
  }
  .btn.sm {
    padding: 0.3em 0.85em;
    font-size: 0.8rem;
  }
  .busy {
    color: var(--fg-faint);
    font-size: 0.8rem;
  }
  .stage {
    height: 470px;
    border: 1px solid var(--border);
    border-radius: 0 0 var(--radius) var(--radius);
    overflow: hidden;
  }
  .foot {
    color: var(--fg-muted);
    font-size: 0.82rem;
    margin: 0.4rem 0 0;
  }
  .stale,
  .check.bad {
    border: 1px solid var(--err);
    background: color-mix(in oklab, var(--err) 8%, transparent);
    border-radius: 8px;
    padding: 0.5rem 0.8rem;
    margin-top: 0.6rem;
    font-size: 0.85rem;
  }
  .stale ul,
  .check ul {
    margin: 0.3rem 0 0;
    padding-left: 1.2rem;
  }
  .check {
    color: var(--fg-muted);
    font-size: 0.8rem;
    margin-bottom: 0.5rem;
  }
  .check.bad {
    color: var(--fg);
  }
  .offstage {
    position: absolute;
    left: -10000px;
    top: 0;
    width: 640px;
    height: 400px;
    visibility: hidden;
  }
  :global(.tour-point) {
    outline: 2px solid var(--accent) !important;
    outline-offset: 2px;
    border-radius: 6px;
    animation: tour-pulse 1.6s ease-in-out infinite;
  }
  @keyframes tour-pulse {
    50% {
      outline-color: color-mix(in oklab, var(--accent) 35%, transparent);
    }
  }
</style>
