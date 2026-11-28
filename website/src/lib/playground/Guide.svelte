<script lang="ts">
  // Guided lesson panel inside the playground's left sidebar: two tracks
  // (Tree Calculus -> Algorithms, Type Theory through Disp), interactive
  // tree-viewer jumps, live goal verification, and "Break it" experiments.
  import { tracks, lessonsForTrack, type Lesson, type LessonWatch, type TrackId } from './curriculum'

  interface Props {
    lesson: Lesson
    completed: Set<string>
    breakMode: boolean
    breakTriggered: boolean
    goalMet: boolean
    busy: boolean
    onSelectLesson: (id: string) => void
    onReset: () => void
    onSolve: () => void
    onBreakIt: () => void
    onExitBreak: () => void
    onWatch: (w: LessonWatch) => void
    onNext: () => void
    onPrev: () => void
    hasPrev: boolean
    hasNext: boolean
    nextLabel: string
  }

  let {
    lesson,
    completed,
    breakMode,
    breakTriggered,
    goalMet,
    busy,
    onSelectLesson,
    onReset,
    onSolve,
    onBreakIt,
    onExitBreak,
    onWatch,
    onNext,
    onPrev,
    hasPrev,
    hasNext,
    nextLabel
  }: Props = $props()

  let showHint = $state(false)

  // Reset hint disclosure whenever the learner switches lessons
  $effect(() => {
    void lesson.id
    showHint = false
  })

  const trackLessons = $derived(lessonsForTrack(lesson.track))
  const trackMeta = $derived(tracks.find((t) => t.id === lesson.track)!)

  function trackDone(id: TrackId): number {
    return lessonsForTrack(id).filter((l) => completed.has(l.id)).length
  }

  function switchTrack(id: TrackId) {
    if (id === lesson.track) return
    const list = lessonsForTrack(id)
    const firstTodo = list.find((l) => !completed.has(l.id)) ?? list[0]
    if (firstTodo) onSelectLesson(firstTodo.id)
  }

  /// Split inline `code` and **bold** spans safely without raw HTML.
  type Run = { kind: 'text' | 'code' | 'bold'; text: string }
  function parseRuns(src: string): Run[] {
    const out: Run[] = []
    const re = /`([^`]+)`|\*\*([^*]+)\*\*/g
    let last = 0
    let m: RegExpExecArray | null
    while ((m = re.exec(src))) {
      if (m.index > last) out.push({ kind: 'text', text: src.slice(last, m.index) })
      if (m[1] != null) out.push({ kind: 'code', text: m[1] })
      else if (m[2] != null) out.push({ kind: 'bold', text: m[2] })
      last = re.lastIndex
    }
    if (last < src.length) out.push({ kind: 'text', text: src.slice(last) })
    return out
  }
</script>

{#snippet rich(text: string)}
  {#each parseRuns(text) as r}
    {#if r.kind === 'code'}<code>{r.text}</code>{:else if r.kind === 'bold'}<b>{r.text}</b>{:else}{r.text}{/if}
  {/each}
{/snippet}

<div class="guide">
  <div class="tracks" role="tablist" aria-label="Learning tracks">
    {#each tracks as tr (tr.id)}
      {@const total = lessonsForTrack(tr.id).length}
      {@const done = trackDone(tr.id)}
      <button
        role="tab"
        class="trbtn"
        class:on={tr.id === lesson.track}
        aria-selected={tr.id === lesson.track}
        onclick={() => switchTrack(tr.id)}
        title={tr.subtitle}
      >
        <span class="trname">{tr.short}</span>
        <span class="trprog" class:all={done === total}>{done}/{total}</span>
      </button>
    {/each}
  </div>

  <div class="stepper" role="list" aria-label="Lessons in {trackMeta.title}">
    {#each trackLessons as l (l.id)}
      {@const isCur = l.id === lesson.id}
      {@const isDone = completed.has(l.id)}
      <button
        class="step"
        class:cur={isCur}
        class:done={isDone}
        onclick={() => onSelectLesson(l.id)}
        title="{l.number}. {l.title}"
        aria-current={isCur ? 'step' : undefined}
      >
        {isDone && !isCur ? '✓' : l.number}
      </button>
    {/each}
  </div>

  <div class="body">
    <div class="head">
      <div class="eyebrow">
        <span>Lesson {lesson.number} of {trackLessons.length}</span>
        {#if lesson.verifyTypes}
          <span class="badge">Typechecked</span>
        {:else}
          <span class="badge raw">Instant</span>
        {/if}
      </div>
      <h3>{lesson.title}</h3>
      <p class="tag">{lesson.tagline}</p>
    </div>

    <div class="prose">
      {#each lesson.body as para}
        <p>{@render rich(para)}</p>
      {/each}
    </div>

    <div class="concept">
      <span class="clabel">Key idea</span>
      <p>{@render rich(lesson.concept)}</p>
    </div>

    {#if lesson.watches.length > 0 && !breakMode}
      <div class="section">
        <span class="slabel">Watch in tree view</span>
        <div class="watches">
          {#each lesson.watches as w}
            <button class="watch" onclick={() => onWatch(w)}>
              <span class="wtitle">◎ {w.label}</span>
              {#if w.note}<span class="wnote">{w.note}</span>{/if}
            </button>
          {/each}
        </div>
      </div>
    {/if}

    {#if breakMode && lesson.breakIt}
      <div class="card break" class:caught={breakTriggered}>
        <div class="chead">
          <span>{breakTriggered ? '✓ Caught by the kernel!' : '⚡ Break-it experiment'}</span>
          <button class="link" onclick={onExitBreak}>← Back to exercise</button>
        </div>
        <p>{@render rich(lesson.breakIt.prompt)}</p>
        {#if breakTriggered}
          <p class="why">{@render rich(lesson.breakIt.why)}</p>
        {:else if !busy}
          <p class="muted">Run the buffer (or keep it broken) to watch the rejection appear inline.</p>
        {/if}
      </div>
    {:else}
      <div class="card goal" class:met={goalMet}>
        <div class="chead">
          <span>{goalMet ? '✓ Lesson complete' : 'Try it'}</span>
          <div class="gacts">
            <button class="link" onclick={() => (showHint = !showHint)}>{showHint ? 'Hide hint' : 'Hint'}</button>
            <button class="link" onclick={onReset} title="Restore starter code">Reset</button>
            <button class="link" onclick={onSolve} title="Load working solution">Solution</button>
          </div>
        </div>
        <p>{@render rich(lesson.goal.prompt)}</p>
        {#if showHint}
          <p class="hint"><b>Hint:</b> {@render rich(lesson.goal.hint)}</p>
        {/if}
        {#if goalMet && hasNext}
          <button class="nextcta" onclick={onNext}>{nextLabel} →</button>
        {/if}
      </div>

      {#if lesson.breakIt}
        <button class="breakbtn" onclick={onBreakIt}>
          <span>⚡ {lesson.breakIt.label}</span>
          <span class="bsub">See what the kernel rejects</span>
        </button>
      {/if}
    {/if}
  </div>

  <div class="foot">
    <button class="navbtn" disabled={!hasPrev} onclick={onPrev}>← Prev</button>
    <button class="navbtn primary" disabled={!hasNext} onclick={onNext}>{nextLabel} →</button>
  </div>
</div>

<style>
  .guide {
    flex: 1;
    min-height: 0;
    display: flex;
    flex-direction: column;
    font-size: 0.8rem;
    color: var(--fg);
  }
  .tracks {
    flex: none;
    display: flex;
    gap: 0.25rem;
    padding: 0.4rem 0.45rem 0.25rem;
  }
  .trbtn {
    flex: 1;
    min-width: 0;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.25rem;
    padding: 0.28rem 0.45rem;
    background: var(--bg-panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    color: var(--fg-muted);
    font: inherit;
    font-size: 0.72rem;
    cursor: pointer;
    text-align: left;
  }
  .trbtn:hover {
    color: var(--fg);
    border-color: var(--border-strong);
  }
  .trbtn.on {
    background: color-mix(in oklab, var(--accent) 14%, var(--bg-panel));
    border-color: color-mix(in oklab, var(--accent) 55%, var(--border));
    color: var(--fg);
    font-weight: 600;
  }
  .trname {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .trprog {
    flex: none;
    font-family: var(--font-mono);
    font-size: 0.66rem;
    color: var(--fg-faint);
  }
  .trprog.all {
    color: var(--ok);
  }

  .stepper {
    flex: none;
    display: flex;
    gap: 0.2rem;
    padding: 0.2rem 0.45rem 0.45rem;
    border-bottom: 1px solid var(--border);
  }
  .step {
    flex: 1;
    height: 1.45rem;
    padding: 0;
    background: none;
    border: 1px solid var(--border);
    border-radius: 5px;
    font-family: var(--font-mono);
    font-size: 0.7rem;
    color: var(--fg-muted);
    cursor: pointer;
  }
  .step:hover {
    background: var(--bg-panel-hover);
    color: var(--fg);
  }
  .step.done {
    border-color: color-mix(in oklab, var(--ok) 45%, var(--border));
    color: var(--ok);
    background: color-mix(in oklab, var(--ok) 10%, transparent);
  }
  .step.cur {
    background: var(--accent);
    border-color: var(--accent);
    color: var(--bg);
    font-weight: 700;
  }

  .body {
    flex: 1;
    min-height: 0;
    overflow: auto;
    padding: 0.6rem 0.65rem 0.8rem;
    display: flex;
    flex-direction: column;
    gap: 0.6rem;
    scrollbar-width: thin;
  }
  .eyebrow {
    display: flex;
    align-items: center;
    justify-content: space-between;
    font-size: 0.68rem;
    color: var(--fg-faint);
    text-transform: uppercase;
    letter-spacing: 0.04em;
    margin-bottom: 0.15rem;
  }
  .badge {
    padding: 0.05rem 0.38rem;
    border-radius: 999px;
    font-size: 0.64rem;
    text-transform: none;
    letter-spacing: 0;
    background: color-mix(in oklab, var(--accent) 16%, transparent);
    color: var(--accent);
    font-weight: 600;
  }
  .badge.raw {
    background: color-mix(in oklab, var(--ok) 14%, transparent);
    color: var(--ok);
  }
  h3 {
    margin: 0;
    font-size: 0.96rem;
    line-height: 1.25;
    color: var(--fg);
  }
  .tag {
    margin: 0.15rem 0 0;
    font-size: 0.75rem;
    color: var(--fg-muted);
  }

  .prose p {
    margin: 0 0 0.45rem;
    line-height: 1.5;
    color: var(--fg);
  }
  .prose p:last-child {
    margin-bottom: 0;
  }
  .body :global(code) {
    font-family: var(--font-mono);
    font-size: 0.74rem;
    padding: 0.05em 0.3em;
    border-radius: 4px;
    background: var(--bg-panel);
    border: 1px solid var(--border);
  }

  .concept {
    padding: 0.45rem 0.55rem;
    border-left: 2.5px solid var(--accent);
    background: color-mix(in oklab, var(--accent) 7%, var(--bg-panel));
    border-radius: 0 6px 6px 0;
  }
  .clabel,
  .slabel {
    display: block;
    font-size: 0.66rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--fg-faint);
    margin-bottom: 0.2rem;
    font-weight: 600;
  }
  .concept p {
    margin: 0;
    line-height: 1.45;
    font-size: 0.77rem;
  }

  .watches {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  .watch {
    display: flex;
    flex-direction: column;
    gap: 0.08rem;
    padding: 0.32rem 0.5rem;
    background: var(--bg-panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    text-align: left;
    font: inherit;
    color: var(--fg);
    cursor: pointer;
  }
  .watch:hover {
    border-color: var(--accent);
    background: var(--bg-panel-hover);
  }
  .wtitle {
    font-size: 0.76rem;
    font-weight: 550;
    color: var(--accent);
  }
  .wnote {
    font-size: 0.7rem;
    color: var(--fg-muted);
  }

  .card {
    padding: 0.55rem 0.6rem;
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    background: var(--bg-panel);
  }
  .card.goal.met {
    border-color: color-mix(in oklab, var(--ok) 55%, var(--border));
    background: color-mix(in oklab, var(--ok) 9%, var(--bg-panel));
  }
  .card.break {
    border-color: color-mix(in oklab, var(--warn) 55%, var(--border));
    background: color-mix(in oklab, var(--warn) 8%, var(--bg-panel));
  }
  .card.break.caught {
    border-color: color-mix(in oklab, var(--ok) 55%, var(--border));
    background: color-mix(in oklab, var(--ok) 9%, var(--bg-panel));
  }
  .chead {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.4rem;
    margin-bottom: 0.3rem;
    font-size: 0.73rem;
    font-weight: 650;
  }
  .card.goal.met .chead > span:first-child,
  .card.break.caught .chead > span:first-child {
    color: var(--ok);
  }
  .gacts {
    display: flex;
    gap: 0.45rem;
  }
  .link {
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    font-size: 0.7rem;
    font-weight: 500;
    color: var(--accent);
    cursor: pointer;
  }
  .link:hover {
    text-decoration: underline;
  }
  .card p {
    margin: 0;
    line-height: 1.45;
    font-size: 0.77rem;
  }
  .hint {
    margin-top: 0.4rem !important;
    padding-top: 0.35rem;
    border-top: 1px dashed var(--border);
    color: var(--fg-muted);
  }
  .why {
    margin-top: 0.4rem !important;
    padding-top: 0.35rem;
    border-top: 1px solid color-mix(in oklab, var(--ok) 25%, var(--border));
    color: var(--fg);
  }
  .muted {
    margin-top: 0.35rem !important;
    color: var(--fg-muted);
    font-size: 0.72rem !important;
  }
  .nextcta {
    margin-top: 0.5rem;
    width: 100%;
    padding: 0.36rem 0.6rem;
    background: var(--ok);
    color: var(--bg);
    border: none;
    border-radius: 6px;
    font: inherit;
    font-size: 0.76rem;
    font-weight: 650;
    cursor: pointer;
  }

  .breakbtn {
    display: flex;
    flex-direction: column;
    gap: 0.08rem;
    width: 100%;
    padding: 0.42rem 0.55rem;
    background: color-mix(in oklab, var(--warn) 8%, transparent);
    border: 1px dashed color-mix(in oklab, var(--warn) 55%, var(--border));
    border-radius: 7px;
    text-align: left;
    font: inherit;
    font-size: 0.75rem;
    font-weight: 600;
    color: var(--fg);
    cursor: pointer;
  }
  .breakbtn:hover {
    background: color-mix(in oklab, var(--warn) 15%, transparent);
  }
  .bsub {
    font-size: 0.69rem;
    font-weight: 400;
    color: var(--fg-muted);
  }

  .foot {
    flex: none;
    display: flex;
    gap: 0.35rem;
    padding: 0.45rem;
    border-top: 1px solid var(--border);
    background: var(--bg-elev);
  }
  .navbtn {
    flex: 1;
    padding: 0.3rem 0.5rem;
    background: var(--bg-panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    font: inherit;
    font-size: 0.74rem;
    color: var(--fg);
    cursor: pointer;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .navbtn:hover:not(:disabled) {
    border-color: var(--border-strong);
    background: var(--bg-panel-hover);
  }
  .navbtn.primary {
    border-color: color-mix(in oklab, var(--accent) 50%, var(--border));
    color: var(--accent);
    font-weight: 600;
  }
  .navbtn:disabled {
    opacity: 0.4;
    cursor: default;
  }
</style>
