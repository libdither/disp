<script lang="ts">
  // Two-way programs on the site's tree-calculus instrument (TreeVis), one local move at a time. Two
  // actions: ▸ fires (a round of moves, every node that can move when parallel is on; ▶ keeps firing),
  // and ⇆ flips (every dot turned around, nothing computed, so firing runs it back). Clicking a live dot
  // fires it, a ring or a waiting bud plays the part under it until that part has run (as TreeVis plays what a
  // clicked application waits on), and a dot that has run flips. ◂ takes the last action back. The
  // engine is machine.ts over core.ts.
  import { onDestroy } from "svelte";
  import { fade } from "svelte/transition";
  import { backOut } from "svelte/easing";
  import Grove from "./Grove.svelte";
  import { show, size, type V } from "./core";
  import { parseProg, parseVal, progLabel, progText, valText, type Reads } from "./text";
  import { PRESETS } from "./library";
  import { done, find, fire, fireAll, flip, measure, movable, say, start, tag, update, weight, type Node } from "./machine";

  let preset = $state(0);
  let progSrc = $state(PRESETS[0].prog);
  let inputSrc = $state(PRESETS[0].input);
  let reads = $state<Reads>(PRESETS[0].reads);
  let parallel = $state(true);
  let motion = $state(true);
  let labels = $state(true);
  let programs = $state(true);
  let running = $state(false);
  /** While playing: the node whose part plays, or null for the whole run. */
  let focus: number | null = null;
  let ruleMsg = $state("");
  let steps = $state(0);
  let moves = $state(0);
  let timer: ReturnType<typeof setTimeout> | undefined;
  const TOO_BIG = 2600;

  type Compiled = { err: string } | { err: null; prog: V; x: V };
  const compiled: Compiled = $derived.by(() => {
    try {
      return { err: null, prog: parseProg(progSrc), x: parseVal(inputSrc) };
    } catch (e) {
      return { err: (e as Error).message };
    }
  });
  // the first preset is laid out at once, so the page arrives with its tree drawn
  let m = $state.raw<Node | null>(start(parseProg(PRESETS[0].prog), parseVal(PRESETS[0].input)));
  let history: { m: Node; msg: string; steps: number; moves: number }[] = [];
  let historyLen = $state(0);

  function load() {
    stopRun();
    history = [];
    historyLen = 0;
    ruleMsg = "";
    steps = 0;
    moves = 0;
    m = compiled.err === null ? start(compiled.prog, compiled.x) : null;
  }
  // parse as you type: a valid edit reloads the tree, an invalid one leaves it standing
  $effect(() => {
    compiled;
    if (compiled.err === null) load();
  });
  const whole = $derived(compiled.err === null ? measure(compiled.prog, compiled.x, 400_000, 400) : null);
  /** Play speed: a run of any length plays in about twenty seconds, a few rounds a frame when it is long. */
  const PLAY_MS = 20000;
  const stepMs = $derived(whole && !whole.long ? Math.max(40, Math.min(720, PLAY_MS / Math.max(1, whole.rounds))) : 300);
  const perTick = $derived(whole && !whole.long ? Math.max(1, Math.round((whole.rounds * stepMs) / PLAY_MS)) : 1);
  const smooth = $derived(motion && (!running || stepMs >= 160));

  function pick(i: number) {
    preset = i;
    progSrc = PRESETS[i].prog;
    inputSrc = PRESETS[i].input;
    reads = PRESETS[i].reads;
  }
  /** The example before or after this one, stopping at the ends as TreeVis does. */
  function cyclePreset(d: 1 | -1) {
    const i = preset < 0 ? (d === 1 ? 0 : PRESETS.length - 1) : preset + d;
    if (i >= 0 && i < PRESETS.length) pick(i);
  }

  // ---- the cassette, as TreeVis has it: a chip wearing the example's symbol opens into a tape of every
  // example, the first at the bottom; press anywhere on the tape and its thumb jumps there, drag and it
  // scrubs, loading each example it passes ----
  const CELLW = 34;
  const CELLH = 28;
  let cassetteOpen = $state(false);
  let cassetteWrapEl: HTMLDivElement | undefined = $state();
  let tapeEl: HTMLDivElement | undefined = $state();
  let selDragging = $state(false);
  const selY = $derived(Math.max(0, preset) * CELLH);
  const chip = $derived(preset >= 0 ? PRESETS[preset] : { sym: "✎", sub: undefined, name: "your own" });
  function cellAtY(clientY: number): number {
    if (!tapeEl) return preset;
    const r = tapeEl.getBoundingClientRect();
    return Math.max(0, Math.min(PRESETS.length - 1, Math.floor((r.bottom - 3 - clientY) / CELLH)));
  }
  function tapeDown(e: PointerEvent) {
    selDragging = true;
    tapeEl?.setPointerCapture(e.pointerId);
    const i = cellAtY(e.clientY);
    if (i !== preset) pick(i);
  }
  function tapeMove(e: PointerEvent) {
    if (!selDragging) return;
    const i = cellAtY(e.clientY);
    if (i !== preset) pick(i);
  }
  const tapeUp = () => (selDragging = false);
  /** The thumb rises out of the chip to its cell as the tape unfurls. */
  const selectorIn = (_n: Element, { duration = 300 } = {}) => ({
    duration,
    easing: backOut,
    css: (t: number) => `opacity: ${t}; transform: translateY(${-selY * t}px)`
  });

  // ---- moving the tree ----

  function describe(ns: Node[]): string {
    if (ns.length === 1) return say(ns[0]);
    const count = new Map<string, number>();
    for (const n of ns) count.set(tag(n), (count.get(tag(n)) ?? 0) + 1);
    const list = [...count].map(([k, v]) => (v > 1 ? `${k} ×${v}` : k)).join(", ");
    return `${ns.length} moves at once: ${list}`;
  }
  /** One action on the tree, remembered so ◂ can take it back. */
  function act(next: Node, msg: string, fired: number) {
    if (!m) return;
    history.push({ m, msg: ruleMsg, steps, moves });
    if (history.length > 4000) history.shift();
    historyLen = history.length;
    ruleMsg = msg;
    m = next;
    steps++;
    moves += fired;
  }
  function fireSet(ns: Node[]): boolean {
    if (!m || !ns.length) return false;
    act(fireAll(m, new Set(ns.map((n) => n.id))), describe(ns), ns.length);
    return true;
  }
  /** The next round in a part of the tree, or the whole of it: every node there that can fire. */
  function stepIn(scope: Node | null): boolean {
    if (!m) return false;
    if (weight(m) > TOO_BIG) {
      ruleMsg = "the tree has grown too big to draw (it may never stop)";
      return false;
    }
    const ms = movable(scope ?? m);
    return fireSet(parallel ? ms : ms.slice(0, 1));
  }

  const finished = $derived(!!m && done(m));
  function endMsg(): string {
    if (!m) return "";
    if (done(m)) return `done: ${valText(m.x!, reads)}, and the count is at the end — ⇆ flips it, and ▸ runs it back`;
    return "stuck: a move that does not fit what it was given";
  }
  function step() {
    stopRun();
    if (!stepIn(null)) ruleMsg = endMsg();
  }
  function back() {
    const h = history.pop();
    if (!h) return;
    historyLen = history.length;
    stopRun();
    m = h.m;
    steps = h.steps;
    moves = h.moves;
    ruleMsg = h.msg || "stepped back";
  }
  function stopRun() {
    running = false;
    focus = null;
    clearTimeout(timer);
  }
  /** The part being played, while it has not run yet: null when it has, or when the whole run plays. */
  const playing = (): Node | null => {
    const n = focus === null || !m ? null : find(m, focus);
    return n && !done(n) ? n : null;
  };
  function schedule() {
    clearTimeout(timer);
    // a part plays at least this fast: it is usually a small piece of a long run
    timer = setTimeout(() => {
      if (!running) return;
      let moved = false;
      for (let i = 0; i < perTick; i++) {
        if (focus !== null && !playing()) break;
        if (!stepIn(playing())) break;
        moved = true;
      }
      if (moved && (focus === null || playing())) schedule();
      else {
        const part = focus !== null && m ? find(m, focus) : null;
        running = false;
        focus = null;
        ruleMsg = part && done(part) ? `that part has run: ${valText(part.x!, reads)} — ▶ plays the rest` : endMsg();
      }
    }, focus === null ? stepMs : Math.min(stepMs, 260));
  }
  /** Play the part in flight under a dot until it has run, the rest of the tree waiting. */
  function playPart(id: number) {
    stopRun();
    focus = id;
    running = true;
    if (stepIn(playing())) schedule();
    else stopRun();
  }
  function toggleRun() {
    if (running) return stopRun();
    if (!m || movable(m).length === 0) load();
    running = true;
    if (stepIn(null)) schedule();
    else running = false;
  }
  /** Flip the whole tree: every dot turned around; nothing is computed. */
  function flipAll() {
    stopRun();
    if (!m) return;
    act(flip(m), "flipped: every dot turned around and nothing computed, so ▸ now runs it the other way", 0);
  }
  onDestroy(stopRun);

  function onSite(id: number) {
    stopRun();
    const n = m && find(m, id);
    if (!n) return;
    if (done(n)) act(update(m!, new Set([id]), flip), `flipped: ${progLabel(n.p)} turned around and counts from the other end, so firing runs it back`, 0);
    else if (fire(n)) fireSet([n]);
    else if (n.kids.length) playPart(id);
    else ruleMsg = `stuck: ${say(n)}`;
  }

  function onKey(e: KeyboardEvent) {
    const inEdit = (e.target as HTMLElement).tagName === "INPUT" || (e.target as HTMLElement).tagName === "SELECT";
    if (inEdit) return;
    if (e.key === "ArrowLeft" || e.key === "ArrowRight") {
      e.preventDefault();
      if (e.shiftKey) cyclePreset(e.key === "ArrowRight" ? 1 : -1);
      else if (e.key === "ArrowLeft") back();
      else step();
    } else if (e.key === " " && e.target === e.currentTarget) {
      e.preventDefault();
      toggleRun();
    } else if (!e.ctrlKey && !e.metaKey && !e.altKey && (e.key === "r" || e.key === "R")) {
      e.preventDefault();
      load();
    } else if (!e.ctrlKey && !e.metaKey && !e.altKey && (e.key === "f" || e.key === "F")) {
      e.preventDefault();
      flipAll();
    }
  }

  const next = $derived(!parallel && m ? (movable(m)[0]?.id ?? null) : null);

  const count = (n: number, what: string) => `${n.toLocaleString("en")} ${what}${n === 1 ? "" : "s"}`;
  const exprText = $derived(compiled.err === null ? `${progLabel(compiled.prog)} · ${inputSrc}` : progSrc);
  const nowText = $derived(!m ? "" : m.x ? valText(m.x, reads) + (done(m) ? " (done)" : "") : "…");
</script>

{#snippet icoPrev()}<svg class="ico" viewBox="0 0 24 24"><rect x="6" y="6" width="2.2" height="12" rx="0.6" /><path d="M18 6 L10 12 L18 18 Z" /></svg>{/snippet}
{#snippet icoBack()}<svg class="ico" viewBox="0 0 24 24"><path d="M15.5 6 L8 12 L15.5 18 Z" /></svg>{/snippet}
{#snippet icoPlay()}<svg class="ico" viewBox="0 0 24 24"><path d="M8 5.5 L18.5 12 L8 18.5 Z" /></svg>{/snippet}
{#snippet icoPause()}<svg class="ico" viewBox="0 0 24 24"><rect x="7.5" y="5.5" width="3.2" height="13" rx="0.8" /><rect x="13.3" y="5.5" width="3.2" height="13" rx="0.8" /></svg>{/snippet}
{#snippet icoFwd()}<svg class="ico" viewBox="0 0 24 24"><path d="M8.5 6 L16 12 L8.5 18 Z" /></svg>{/snippet}
{#snippet icoNext()}<svg class="ico" viewBox="0 0 24 24"><path d="M6 6 L14 12 L6 18 Z" /><rect x="15.8" y="6" width="2.2" height="12" rx="0.6" /></svg>{/snippet}
{#snippet icoReset()}<svg class="ico stroke" viewBox="0 0 24 24"><polyline points="3 5 3 10.5 8.5 10.5" /><path d="M5.2 15.5 a8 8 0 1 0 1.9 -8.4 L3 10.5" /></svg>{/snippet}
{#snippet icoParallel()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M4 8.5 h9" /><path d="M10.5 5.5 L13.5 8.5 L10.5 11.5" /><path d="M4 15.5 h9" /><path d="M10.5 12.5 L13.5 15.5 L10.5 18.5" /></svg>{/snippet}
{#snippet icoDirection()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M4 8 h15" /><path d="M16 5 L19 8 L16 11" /><path d="M20 16 h-15" /><path d="M8 13 L5 16 L8 19" /></svg>{/snippet}
{#snippet icoMotion()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M9.6 4.6 A2 2 0 1 1 11 8 H2.5 M12.6 19.4 A2 2 0 1 0 14 16 H2.5 M17.7 7.7 A2.5 2.5 0 1 1 19.5 12 H2.5" /></svg>{/snippet}
{#snippet icoPrograms()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M8 5 L3 12 L8 19" /><path d="M16 5 L21 12 L16 19" /><path d="M13.5 4 L10.5 20" /></svg>{/snippet}
{#snippet icoLabels()}<svg class="ico stroke" viewBox="0 0 24 24"><path d="M20.4 13.4 l-7 7 a2 2 0 0 1 -2.8 0 L2.5 12 V2.5 h9.5 l8.4 8.4 a2 2 0 0 1 0 2.5 Z" /><circle cx="7.3" cy="7.3" r="1.15" fill="currentColor" stroke="none" /></svg>{/snippet}

<svelte:document
  onpointerdown={(e) => {
    if (cassetteOpen && cassetteWrapEl && !cassetteWrapEl.contains(e.target as globalThis.Node)) cassetteOpen = false;
  }}
/>

<!-- svelte-ignore a11y_no_noninteractive_tabindex, a11y_no_noninteractive_element_interactions -->
<div
  class="vis lab"
  tabindex="0"
  role="application"
  aria-label="two-way program visualizer — space runs or pauses, arrow keys step back and forward, shift+arrows change example, f flips, r resets"
  onkeydown={onKey}
  onmousedown={(e) => {
    if ((e.target as HTMLElement).closest("button")) e.preventDefault();
  }}
>
  {#if compiled.err !== null && !m}
    <p class="err">{compiled.err}</p>
  {:else if m}
    <div class="scroller">
      <Grove root={m} {reads} {labels} {programs} motion={smooth} {next} epoch={compiled} {onSite} />
    </div>
    <div class="readout">
      <div class="rline">
        <span class="expr">{exprText}</span>
        <span class="rule" class:nf={finished}>{ruleMsg || "press ▸ (or →) for the next round of moves, or click a bud"}</span>
      </div>
      <div class="rline sub">
        <span class="now"><span class="flabel">value</span>{nowText}</span>
        <span class="count"
          >{running && perTick > 1 ? `playing ${perTick} steps a frame · ` : ""}{count(steps, "step")} · {count(moves, "move")}{whole
            ? whole.long
              ? " · whole run: too long to count"
              : whole.stuck
                ? ` · gets stuck after ${count(whole.rounds, "round")}`
                : ` · whole run: ${count(whole.moves, "move")} in ${count(whole.rounds, "round")}`
            : ""}</span
        >
      </div>
    </div>
  {/if}

  <div class="editrow">
    <input
      class="editbox prog"
      class:invalid={compiled.err !== null}
      type="text"
      bind:value={progSrc}
      oninput={() => (preset = -1)}
      onfocus={stopRun}
      spellcheck="false"
      autocomplete="off"
      aria-label="program"
    />
    <span class="on">on</span>
    <input
      class="editbox arg"
      class:invalid={compiled.err !== null}
      type="text"
      bind:value={inputSrc}
      oninput={() => (preset = -1)}
      onfocus={stopRun}
      spellcheck="false"
      autocomplete="off"
      aria-label="input"
    />
    <select class="reads" bind:value={reads} aria-label="read values as">
      <option value="bool">booleans</option>
      <option value="nat">numbers</option>
      <option value="bits">a word</option>
      <option value="regs">registers</option>
      <option value="tree">plain trees</option>
    </select>
    <span class="info">
      <button type="button" class="info-btn" aria-label="what can I type here?">i</button>
      <span class="info-pop" role="tooltip">
        Edits parse as you type; <kbd>space</kbd> plays, <kbd>→</kbd> fires, <kbd>←</kbd> takes it back,
        <kbd>f</kbd> flips, <kbd>shift</kbd>+arrows change example, <kbd>r</kbd> resets. A program:
        <code>swap</code> <code>cross</code> <code>sink</code> <code>stem</code> <code>unit</code> <code>eval</code>
        <code>id</code>, <code>a ; b</code>, <code>a†</code>, <code>tri(a, b, c)</code>, <code>map(a, b, c)</code>,
        <code>fix(b)</code> with <code>self</code> inside, the shorthands <code>first(f)</code>
        <code>second(f)</code> <code>par(f, g)</code> <code>when(f)</code>, the library's names, or any tree as
        <code>{"{"}△△△{"}"}</code>. A value: <code>△</code> <code>△v</code> <code>(u, v)</code> <code>T</code>
        <code>F</code>, numbers, a word <code>0b1011</code>, registers <code>&lt;0b10, 0b01&gt;</code>, or a program
        as a tree, <code>[add]</code>.
      </span>
    </span>
  </div>
  {#if compiled.err !== null && m}<p class="err small">{compiled.err}</p>{/if}
  {#if preset >= 0}<p class="tip-line">{PRESETS[preset].tip}</p>{/if}

  <div class="bottombar">
    <div class="cassette-wrap corner-left" bind:this={cassetteWrapEl}>
      <button
        type="button"
        class="cassette-chip"
        class:open={cassetteOpen}
        onclick={() => (cassetteOpen = !cassetteOpen)}
        aria-haspopup="true"
        aria-expanded={cassetteOpen}
        aria-label={`examples — now: ${chip.name}`}
        data-tip={`examples — now: ${chip.name}`}
        ><span class="glyph">{chip.sym}{#if chip.sub}<sub>{chip.sub}</sub>{/if}</span></button
      >
      {#if cassetteOpen}
        <!-- svelte-ignore a11y_no_static_element_interactions -->
        <div
          class="cassette-tape"
          class:grabbing={selDragging}
          bind:this={tapeEl}
          role="slider"
          aria-orientation="vertical"
          tabindex="-1"
          aria-label="examples"
          aria-valuemin="0"
          aria-valuemax={PRESETS.length - 1}
          aria-valuenow={Math.max(0, preset)}
          aria-valuetext={chip.name}
          style="width: {CELLW + 6}px"
          out:fade={{ duration: 150 }}
          onpointerdown={tapeDown}
          onpointermove={tapeMove}
          onpointerup={tapeUp}
          onpointercancel={tapeUp}
        >
          {#each PRESETS as p, i}
            <div class="tcell" class:on={i === preset} style="height: {CELLH}px; --ci: {i}" data-tip={p.name}>
              {p.sym}{#if p.sub}<sub>{p.sub}</sub>{/if}
            </div>
          {/each}
          {#if preset >= 0}
            <div
              class="selector"
              style="width: {CELLW}px; height: {CELLH}px; transform: translateY({-selY}px)"
              aria-hidden="true"
              in:selectorIn={{ duration: 300 }}
            ></div>
          {/if}
        </div>
      {/if}
    </div>
    <div class="tside tleft">
      <button class="tbtn" onclick={() => cyclePreset(-1)} aria-label="previous example">{@render icoPrev()}</button>
      <button class="tbtn" onclick={back} disabled={historyLen === 0} aria-label="undo the last step">{@render icoBack()}</button>
    </div>
    <button class="tbtn play" onclick={toggleRun} aria-label={running ? "pause" : "play"}
      >{#if running}{@render icoPause()}{:else}{@render icoPlay()}{/if}</button
    >
    <div class="tside tright">
      <button class="tbtn" onclick={step} aria-label="fire: the next round of moves">{@render icoFwd()}</button>
      <button class="tbtn flipbtn" onclick={flipAll} aria-label="flip: turn every dot around" title="flip: turn every dot around (f)"
        >{@render icoDirection()}</button
      >
      <button class="tbtn" onclick={() => cyclePreset(1)} aria-label="next example">{@render icoNext()}</button>
      <button class="tbtn" onclick={load} aria-label="reset">{@render icoReset()}</button>
    </div>
    <div class="selectors corner-right">
      <button
        class="mpi sel"
        class:on={parallel}
        aria-pressed={parallel}
        data-tip="parallel — every node that can move moves in one step"
        aria-label="parallel"
        onclick={() => (parallel = !parallel)}>{@render icoParallel()}</button
      >
      <button
        class="mpi sel"
        class:on={motion}
        aria-pressed={motion}
        data-tip="motion — glide, grow and shrink vs instant"
        aria-label="motion"
        onclick={() => (motion = !motion)}>{@render icoMotion()}</button
      >
      <button
        class="mpi sel"
        class:on={programs}
        aria-pressed={programs}
        data-tip="programs — each run's program drawn open, or folded into one pod"
        aria-label="programs"
        onclick={() => (programs = !programs)}>{@render icoPrograms()}</button
      >
      <button
        class="mpi sel"
        class:on={labels}
        aria-pressed={labels}
        data-tip="labels — values on the data"
        aria-label="labels"
        onclick={() => (labels = !labels)}>{@render icoLabels()}</button
      >
    </div>
  </div>
</div>

{#if compiled.err === null}
  <details class="shapes">
    <summary>This program as one tree ({size(compiled.prog)} nodes)</summary>
    <p class="raw"><code>{progText(compiled.prog, true, true)}</code></p>
    <p class="raw"><code>{show(compiled.prog)}</code></p>
  </details>
{/if}

<style>
  .vis {
    position: relative;
    width: 100%;
    outline: none;
    border-radius: var(--radius, 12px);
  }
  .vis:focus,
  .vis:focus-visible {
    outline: none;
  }
  .vis.lab {
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-panel);
    padding: 1rem 1.1rem;
    margin: 1.4rem 0 0.6rem;
  }
  .err {
    color: var(--err);
    font-family: var(--font-mono);
    font-size: 0.86rem;
    text-align: center;
  }
  .err.small {
    font-size: 0.75rem;
    margin: 0.1rem 0 0;
  }

  /* ---- readout, as TreeVis writes it ---- */
  .readout {
    margin-top: 0.3rem;
    font-size: 0.8rem;
  }
  .rline {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    gap: 0.25rem 0.75rem;
  }
  .expr {
    font-family: var(--font-mono);
    color: var(--fg);
  }
  .rule {
    font-family: var(--font-mono);
    font-style: italic;
    color: var(--fg-muted);
  }
  .rule.nf {
    color: var(--g2);
    font-style: normal;
  }
  .scroller {
    overflow-x: auto;
    overflow-y: hidden;
  }
  .scroller > :global(.stage) {
    min-width: 540px;
  }
  .rline.sub {
    margin-top: 0.15rem;
    justify-content: space-between;
  }
  .now {
    font-family: var(--font-mono);
    color: var(--fg-muted);
    word-break: break-all;
  }
  .now .flabel {
    margin-right: 0.5em;
  }
  .tbtn.flipbtn {
    color: #4a7fc0;
  }
  .count {
    font-family: var(--font-mono);
    color: var(--fg-faint);
    font-size: 0.75rem;
  }
  .flabel {
    font-size: 0.68rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--fg-faint);
    flex: none;
  }

  /* ---- the edit row: bare text, centred, on top of the transport ---- */
  .editrow {
    display: flex;
    justify-content: center;
    align-items: center;
    flex-wrap: wrap;
    gap: 0.35rem;
    margin-top: 0.4rem;
  }
  .editbox {
    min-width: 0;
    background: none;
    border: none;
    outline: none;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    text-align: center;
    padding: 0.14em 0.5em;
    border-radius: 7px;
    caret-color: var(--accent);
    border-bottom: 1px dashed color-mix(in oklab, var(--fg-faint) 50%, transparent);
  }
  .editbox.prog {
    flex: 1 1 18rem;
    max-width: 30rem;
  }
  .editbox.arg {
    flex: 0 1 12rem;
  }
  .editbox:hover {
    border-bottom-color: var(--fg-muted);
    background: color-mix(in oklab, var(--bg-code) 45%, transparent);
  }
  .editbox:focus {
    border-bottom: 1px solid var(--accent);
    background: color-mix(in oklab, var(--bg-code) 60%, transparent);
  }
  .editbox.invalid {
    border-bottom-color: color-mix(in oklab, var(--err) 65%, transparent);
  }
  .on {
    color: var(--fg-faint);
    font-size: 0.78rem;
  }
  .reads {
    font: inherit;
    font-size: 0.75rem;
    color: var(--fg-muted);
    background: none;
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 0.05rem 0.2rem;
  }
  .info {
    position: relative;
    flex: none;
    display: inline-flex;
  }
  .info-btn {
    width: 16px;
    height: 16px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: none;
    color: var(--fg-faint);
    font-family: var(--font-body);
    font-size: 10px;
    font-style: italic;
    font-weight: 700;
    line-height: 1;
    display: inline-grid;
    place-items: center;
    cursor: help;
    padding: 0;
  }
  .info-btn:hover,
  .info-btn:focus-visible {
    color: var(--accent);
    border-color: var(--accent);
    outline: none;
  }
  .info-pop {
    position: absolute;
    bottom: calc(100% + 9px);
    right: -0.35rem;
    width: 280px;
    padding: 0.55rem 0.7rem;
    border-radius: 9px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    box-shadow: 0 12px 28px -14px color-mix(in oklab, var(--fg) 42%, transparent);
    color: var(--fg-muted);
    font-size: 0.72rem;
    line-height: 1.5;
    text-align: left;
    z-index: 50;
    opacity: 0;
    visibility: hidden;
    transform: translateY(4px);
    transition:
      opacity 0.15s ease,
      transform 0.15s ease,
      visibility 0.15s;
    pointer-events: none;
  }
  .info:hover .info-pop,
  .info-btn:focus-visible + .info-pop {
    opacity: 1;
    visibility: visible;
    transform: none;
  }
  .info-pop code {
    background: color-mix(in oklab, var(--fg) 9%, transparent);
    padding: 0 0.25em;
    border-radius: 3px;
    font-size: 0.92em;
  }
  .info-pop kbd {
    background: var(--bg-code);
    border: 1px solid var(--border-strong);
    border-radius: 4px;
    padding: 0 0.3em;
    font-size: 0.88em;
    font-family: var(--font-mono);
  }
  .tip-line {
    color: var(--fg-faint);
    font-size: 0.8rem;
    margin: 0.3rem 0 0;
    font-style: italic;
    text-align: center;
  }

  /* ---- the bottom bar: play dead centre, the transport tight around it ---- */
  .bottombar {
    position: relative;
    display: grid;
    grid-template-columns: 1fr auto 1fr;
    align-items: center;
    gap: 0.15rem;
    margin-top: 0.3rem;
    min-height: 30px;
  }
  .tside {
    display: flex;
    align-items: center;
    gap: 0.1rem;
  }
  .tleft {
    justify-self: end;
  }
  .tright {
    justify-self: start;
  }
  .corner-left {
    position: absolute;
    left: 0;
    top: 50%;
    transform: translateY(-50%);
  }
  .corner-right {
    position: absolute;
    right: 0;
    top: 50%;
    transform: translateY(-50%);
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .tbtn {
    flex: none;
    border: none;
    background: none;
    color: var(--fg-muted);
    display: inline-grid;
    place-items: center;
    padding: 0.18em;
    border-radius: 6px;
    cursor: pointer;
    transition:
      color 0.18s ease,
      background 0.18s ease;
  }
  .tbtn:hover {
    color: var(--accent);
    background: color-mix(in oklab, var(--accent) 12%, transparent);
  }
  .tbtn:disabled {
    opacity: 0.28;
    cursor: default;
    color: var(--fg-muted);
    background: none;
  }
  .tbtn :global(.ico) {
    width: 18px;
    height: 18px;
    display: block;
    fill: currentColor;
  }
  .tbtn :global(.ico.stroke),
  .mpi :global(.ico.stroke) {
    fill: none;
    stroke: currentColor;
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
  .tbtn.play {
    color: var(--g2);
    padding: 0.1em;
  }
  .tbtn.play :global(.ico) {
    width: 24px;
    height: 24px;
  }
  .mpi {
    flex: none;
    width: 30px;
    height: 30px;
    display: inline-grid;
    place-items: center;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg-muted);
    border-radius: 50%;
    padding: 0;
    cursor: pointer;
    position: relative;
  }
  .mpi:hover {
    border-color: var(--accent);
    color: var(--accent);
  }
  .mpi :global(.ico) {
    width: 16px;
    height: 16px;
    display: block;
    fill: currentColor;
  }
  .mpi.sel.on {
    border-color: var(--g2);
    color: var(--g2);
    background: color-mix(in oklab, var(--g1) 12%, var(--bg-elev));
  }
  [data-tip]::after {
    content: attr(data-tip);
    position: absolute;
    right: 0;
    bottom: calc(100% + 7px);
    width: max-content;
    max-width: 15em;
    padding: 0.35em 0.6em;
    border-radius: 7px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--fg-muted);
    font-family: var(--font-body);
    font-size: 0.7rem;
    font-weight: 400;
    line-height: 1.4;
    text-align: left;
    white-space: normal;
    z-index: 60;
    opacity: 0;
    visibility: hidden;
    pointer-events: none;
    transition: opacity 0.15s ease;
  }
  [data-tip]:hover::after {
    opacity: 1;
    visibility: visible;
  }
  .corner-left [data-tip]::after {
    right: auto;
    left: 0;
  }
  .cassette-tape .tcell::after {
    right: auto;
    left: calc(100% + 9px);
    bottom: auto;
    top: 50%;
    transform: translate(0, -50%);
  }
  .cassette-tape .tcell.on::after {
    opacity: 1;
    visibility: visible;
  }

  /* ---- the cassette: a chip in the bar; the tape pops up above it (as in TreeVis) ---- */
  .cassette-wrap {
    display: inline-flex;
  }
  .cassette-chip {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--g2);
    font-family: var(--font-mono);
    font-size: 1rem;
    line-height: 1;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    cursor: pointer;
    opacity: 0.6;
    transition:
      opacity 0.2s ease,
      border-color 0.2s ease,
      color 0.2s ease;
  }
  .vis:hover .cassette-chip,
  .cassette-chip.open {
    opacity: 1;
  }
  .cassette-chip:hover {
    border-color: var(--accent);
    color: var(--accent);
  }
  .cassette-chip .glyph {
    display: inline-flex;
    align-items: center;
    transition: transform 0.2s ease;
  }
  .cassette-chip:hover .glyph {
    transform: scale(1.06);
  }
  .cassette-chip sub,
  .tcell sub {
    font-size: 0.6em;
    margin-left: 0.03em;
  }
  /* a column of every example and a draggable thumb, expanding upward out of the chip: the first example
     at the bottom. The whole tape is the slider's track. */
  .cassette-tape {
    position: absolute;
    bottom: calc(100% + 8px);
    left: 0;
    z-index: 30;
    display: flex;
    flex-direction: column-reverse;
    align-items: stretch;
    box-sizing: border-box;
    padding: 3px;
    border-radius: 10px;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    box-shadow: 0 12px 26px -14px color-mix(in oklab, var(--fg) 48%, transparent);
    animation: tapeReveal 0.2s ease;
    cursor: pointer;
    touch-action: none;
    user-select: none;
    -webkit-user-select: none;
  }
  .cassette-tape.grabbing {
    cursor: grabbing;
  }
  @keyframes tapeReveal {
    from {
      opacity: 0;
      transform: translateY(8px);
    }
  }
  /* the cells stagger in bottom to top as the tape unfurls */
  @keyframes cellIn {
    from {
      opacity: 0;
      transform: translateY(6px) scale(0.6);
    }
  }
  .tcell {
    position: relative;
    flex: none;
    width: 100%;
    color: var(--fg-muted);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    line-height: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    pointer-events: none;
    animation: cellIn 0.3s calc(var(--ci, 0) * 24ms) both cubic-bezier(0.34, 1.6, 0.5, 1);
  }
  .tcell.on {
    color: var(--g2);
    font-weight: 650;
    transition: color 0.15s ease;
  }
  /* the thumb: it rests on the bottom cell and rides up to the chosen one, snapping with an overshoot */
  .selector {
    position: absolute;
    left: 3px;
    bottom: 3px;
    border-radius: 7px;
    border: 1.5px solid var(--g2);
    background: color-mix(in oklab, var(--g1) 16%, transparent);
    box-shadow: 0 1px 6px -2px color-mix(in oklab, var(--g2) 70%, transparent);
    pointer-events: none;
    transition: transform 0.24s cubic-bezier(0.34, 1.7, 0.5, 1);
  }

  @media (max-width: 700px) {
    .corner-right {
      position: static;
      transform: none;
      grid-column: 1 / -1;
      justify-content: center;
      margin-top: 0.3rem;
    }
  }

  .shapes {
    font-size: 0.86rem;
    margin: 0 0 1.4rem;
  }
  .shapes summary {
    cursor: pointer;
    color: var(--fg-muted);
  }
  .raw code {
    font-size: 0.78rem;
    word-break: break-all;
  }
</style>
