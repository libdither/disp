<script lang="ts">
  // Two-way programs on the site's tree-calculus instrument (TreeVis), run backward as well as
  // forward. The tree starts as one bud: a program applied to its input. ▶ runs, ▸ fires one step
  // (every ready call at once when parallel is on), ◂ takes the last step back, and clicking a bud
  // fires that call. A fired call keeps its program, flipped to its dagger; clicking it runs what is
  // ready above it, then once that is done, steps it back until it is a bud again. The engine is
  // steps.ts over patterns.ts.
  import { onDestroy } from "svelte";
  import Grove from "./Grove.svelte";
  import {
    PRESETS,
    parseProg,
    parseVal,
    progName,
    valText,
    asBool,
    asNat,
    clauseSides,
    show,
    size,
    flipProg,
    type Reads,
    type V,
  } from "./patterns";
  import {
    start,
    fireAll,
    undoAll,
    readyIn,
    undoableIn,
    find,
    toV,
    isValue,
    bare,
    children,
    measure,
    ruleOf,
    type Machine,
    type Term,
    type Call,
    type Let,
    type Done,
  } from "./steps";

  let preset = $state(0);
  let progText = $state(PRESETS[0].prog);
  let inputText = $state(PRESETS[0].input);
  let reads = $state<Reads>(PRESETS[0].reads);
  let parallel = $state(true);
  let motion = $state(true);
  let labels = $state(true);
  let keep = $state(true);
  let running = $state(false);
  let ruleMsg = $state("");
  let stepCount = $state(0);
  let timer: ReturnType<typeof setTimeout> | undefined;
  const STEP_MS = 900;

  type Compiled = { err: string } | { err: null; prog: V; x: V };
  const compiled: Compiled = $derived.by(() => {
    try {
      return { err: null, prog: parseProg(progText), x: parseVal(inputText) };
    } catch (e) {
      return { err: (e as Error).message };
    }
  });
  let m = $state.raw<Machine | null>(null);
  /** Which way each fired call is going when clicked: forward until done, then back. */
  let modes = new Map<number, "fwd" | "bwd">();
  // each step remembers where it came from, so ◂ can walk back
  let history: { m: Machine; msg: string; steps: number }[] = [];
  let historyLen = $state(0);

  function load() {
    stopRun();
    modes = new Map();
    history = [];
    historyLen = 0;
    ruleMsg = "";
    stepCount = 0;
    m = compiled.err === null ? start(compiled.prog, compiled.x) : null;
  }
  // parse as you type: a valid edit reloads the tree, an invalid one leaves it standing
  $effect(() => {
    compiled;
    if (compiled.err === null) load();
  });
  const whole = $derived(compiled.err === null ? measure(compiled.prog, compiled.x) : null);

  function pick(i: number) {
    preset = i;
    progText = PRESETS[i].prog;
    inputText = PRESETS[i].input;
    reads = PRESETS[i].reads;
  }
  function cyclePreset(d: 1 | -1) {
    pick((((preset < 0 ? 0 : preset) + d) % PRESETS.length + PRESETS.length) % PRESETS.length);
  }

  // ---- saying what a step does ----

  function says(n: Call | Let): string {
    if (n.k === "let") return "match: the calls it waited on are done, so it matches and builds";
    const r = ruleOf(n);
    if (/^[0-9]+$/.test(r)) {
      const sides = clauseSides(n.prog, +r - 1, reads);
      if (!sides) return `${n.name} · clause ${r}`;
      const [l, rr] = n.back ? [sides[1], sides[0]] : sides;
      return `${n.name} · clause ${r}${n.back ? ", run backward" : ""}: ${l} ⇄ ${rr}`;
    }
    if (r === "△") return `${n.name}: no clause fits, so △ leaves it as it is`;
    if (r === "†") return `${n.name}: (△, b) runs b the other way`;
    if (r === "∘") return `${n.name}: (△a, b) runs a, then b`;
    if (r === "a") return `${n.name}: △a pairs it with a`;
    return n.name;
  }
  const tag = (n: Call | Let) => (n.k === "let" ? "⇄" : `${n.name}·${ruleOf(n)}`);
  function summary(ns: (Call | Let)[]): string {
    const count = new Map<string, number>();
    for (const n of ns) count.set(tag(n), (count.get(tag(n)) ?? 0) + 1);
    return [...count].map(([k, v]) => (v > 1 ? `${k} ×${v}` : k)).join(", ");
  }
  const budTip = (id: number): string => {
    const n = m && find(m.root, id);
    if (!n || (n.k !== "call" && n.k !== "let")) return "";
    if (n.k === "call" && !ruleOf(n)) return `${n.name}: waits for its argument (click to run what it needs)`;
    return `${says(n)} (click to fire)`;
  };

  // ---- moving the tree ----

  const remember = () => {
    if (!m) return;
    history.push({ m, msg: ruleMsg, steps: stepCount });
    if (history.length > 600) history.shift();
    historyLen = history.length;
  };
  function fireSet(ns: (Call | Let)[]): boolean {
    if (!m || !ns.length) return false;
    remember();
    ruleMsg = ns.length === 1 ? says(ns[0]) : `${ns.length} calls fired in parallel (${summary(ns)})`;
    m = fireAll(m, ns);
    stepCount++;
    return true;
  }
  function undoSet(ds: Done[]): boolean {
    if (!m || !ds.length) return false;
    remember();
    ruleMsg =
      ds.length === 1
        ? `stepped back: ${ds[0].was.k === "call" ? ds[0].was.name : "a match"} undone`
        : `stepped back: ${ds.length} calls undone at once`;
    m = undoAll(m, ds);
    stepCount++;
    return true;
  }
  /** The next step from the whole tree, or from a part of it. */
  function forward(scope: Term | null): boolean {
    if (!m) return false;
    const ready = readyIn(scope ?? m.root);
    return fireSet(parallel ? ready : ready.slice(0, 1));
  }
  const finished = $derived(!!m && isValue(m.root));
  const stuck = $derived(!!m && !finished && readyIn(m.root).length === 0);
  function endMsg(): string {
    if (!m) return "";
    if (stuck) return "stuck: a call that cannot fire on what it was given";
    const r = m.root.k === "done" && m.root.was.k === "call" ? progName(flipProg(m.root.was.prog)) : null;
    return `done: the value is ${valText(toV(m.root)!, reads)}, and the residual ${r ?? "f†"} stays on the left`;
  }

  function step() {
    stopRun();
    if (!forward(null)) ruleMsg = endMsg();
  }
  function back() {
    const h = history.pop();
    if (!h) return;
    historyLen = history.length;
    stopRun();
    m = h.m;
    stepCount = h.steps;
    ruleMsg = h.msg || "stepped back";
  }
  function stopRun() {
    running = false;
    clearTimeout(timer);
  }
  function schedule() {
    clearTimeout(timer);
    timer = setTimeout(() => {
      if (!running) return;
      if (forward(null)) schedule();
      else {
        running = false;
        ruleMsg = endMsg();
      }
    }, STEP_MS);
  }
  function toggleRun() {
    if (running) return stopRun();
    if (!m || finished || stuck) load();
    running = true;
    if (forward(null)) schedule();
    else running = false;
  }
  function reset() {
    load();
  }
  onDestroy(stopRun);

  function onBud(id: number) {
    stopRun();
    const n = m && find(m.root, id);
    if (!n || (n.k !== "call" && n.k !== "let")) return;
    // a ready call fires; one still waiting on its argument runs what it needs first
    if (readyIn(n)[0] === n) fireSet([n]);
    else forward(n);
  }
  function ringDir(d: Done): "fwd" | "bwd" | null {
    if (!m) return null;
    const can = (dir: "fwd" | "bwd") =>
      dir === "fwd" ? readyIn(d).length > 0 : undoableIn(m!.root, d).length > 0;
    const want = modes.get(d.id) ?? "fwd";
    if (can(want)) return want;
    const other = want === "fwd" ? "bwd" : "fwd";
    return can(other) ? other : null;
  }
  function onRing(id: number) {
    stopRun();
    const d = m && find(m.root, id);
    if (!m || !d || d.k !== "done") return;
    const dir = ringDir(d);
    if (!dir) return;
    modes.set(id, dir);
    if (dir === "fwd") forward(d);
    else undoSet(undoableIn(m.root, d));
    if (m && find(m.root, id)?.k !== "done") modes.delete(id);
  }

  function onKey(e: KeyboardEvent) {
    const inEdit = (e.target as HTMLElement).tagName === "INPUT";
    if (!inEdit && (e.key === "ArrowLeft" || e.key === "ArrowRight")) {
      e.preventDefault();
      if (e.shiftKey) cyclePreset(e.key === "ArrowRight" ? 1 : -1);
      else if (e.key === "ArrowLeft") back();
      else step();
      return;
    }
    if (e.key === " " && e.target === e.currentTarget) {
      e.preventDefault();
      toggleRun();
      return;
    }
    if (!inEdit && (e.key === "r" || e.key === "R") && !e.ctrlKey && !e.metaKey && !e.altKey) {
      e.preventDefault();
      reset();
    }
  }

  // ---- what the tree says ----

  const marks = $derived.by(() => {
    const out = new Map<number, string>();
    if (!m) return out;
    const walk = (t: Term) => {
      if (t.k === "done") {
        const dir = ringDir(t);
        if (dir) out.set(t.id, dir === "fwd" ? "▶" : "↺");
      }
      children(t).forEach(walk);
    };
    walk(m.root);
    return out;
  });
  const next = $derived(!parallel && m ? (readyIn(m.root)[0]?.id ?? null) : null);

  /** Values worth a word: booleans, bits, numbers, and programs by name. */
  const valueLabels = $derived.by(() => {
    const out = new Map<number, string>();
    if (!m || !labels) return out;
    const walk = (t: Term) => {
      if (t.k === "call" || t.k === "let") return children(t).forEach(walk);
      const u = bare(t);
      const v = isValue(u) ? toV(u) : null;
      if (v) {
        const p = v.tag === "fork" && size(v) > 8 ? progName(v) : null;
        if (p) return void out.set(u.id, p);
        if (reads === "bits" || reads === "regs") {
          const n = asNat(v);
          if (n !== null && n <= 1) return void out.set(u.id, String(n));
        }
        if (reads === "bool") {
          const b = asBool(v);
          if (b) return void out.set(u.id, b);
        }
        if (reads === "nat") {
          const n = asNat(v);
          if (n !== null) return void out.set(u.id, String(n));
        }
      }
      children(u).forEach(walk);
    };
    walk(m.root);
    return out;
  });

  function termText(t: Term, r: Reads): string {
    if (isValue(t)) return valText(toV(t)!, r);
    const u = bare(t);
    if (u.k === "call") return `${u.name}(${termText(u.arg, r)})`;
    if (u.k === "let") return `⇄[${u.binds.map((b) => termText(b.t, r)).join(", ")}]`;
    if (u.k === "stem") return "△" + termText(u.c, r);
    if (u.k === "fork") return `(${termText(u.l, r)}, ${termText(u.r, r)})`;
    return "△";
  }
  const depthOf = (t: Term): number => 1 + Math.max(0, ...children(t).map(depthOf));
  let tallest = $state(0);
  $effect(() => {
    if (m) tallest = Math.max(tallest, depthOf(m.root));
  });
  $effect(() => {
    compiled;
    tallest = 0;
  });
  const height = $derived(Math.max(330, Math.min(700, 150 + tallest * 40)));
  const exprText = $derived(
    compiled.err === null ? `${progName(compiled.prog) ?? progText} · ${inputText}` : progText,
  );
</script>

{#snippet icoPrev()}<svg class="ico" viewBox="0 0 24 24"
    ><rect x="6" y="6" width="2.2" height="12" rx="0.6" /><path d="M18 6 L10 12 L18 18 Z" /></svg
  >{/snippet}
{#snippet icoBack()}<svg class="ico" viewBox="0 0 24 24"><path d="M15.5 6 L8 12 L15.5 18 Z" /></svg>{/snippet}
{#snippet icoPlay()}<svg class="ico" viewBox="0 0 24 24"><path d="M8 5.5 L18.5 12 L8 18.5 Z" /></svg>{/snippet}
{#snippet icoPause()}<svg class="ico" viewBox="0 0 24 24"
    ><rect x="7.5" y="5.5" width="3.2" height="13" rx="0.8" /><rect x="13.3" y="5.5" width="3.2" height="13" rx="0.8" /></svg
  >{/snippet}
{#snippet icoFwd()}<svg class="ico" viewBox="0 0 24 24"><path d="M8.5 6 L16 12 L8.5 18 Z" /></svg>{/snippet}
{#snippet icoNext()}<svg class="ico" viewBox="0 0 24 24"
    ><path d="M6 6 L14 12 L6 18 Z" /><rect x="15.8" y="6" width="2.2" height="12" rx="0.6" /></svg
  >{/snippet}
{#snippet icoReset()}<svg class="ico stroke" viewBox="0 0 24 24"
    ><polyline points="3 5 3 10.5 8.5 10.5" /><path d="M5.2 15.5 a8 8 0 1 0 1.9 -8.4 L3 10.5" /></svg
  >{/snippet}
{#snippet icoParallel()}<svg class="ico stroke" viewBox="0 0 24 24"
    ><path d="M4 8.5 h9" /><path d="M10.5 5.5 L13.5 8.5 L10.5 11.5" /><path d="M4 15.5 h9" /><path
      d="M10.5 12.5 L13.5 15.5 L10.5 18.5"
    /></svg
  >{/snippet}
{#snippet icoMotion()}<svg class="ico stroke" viewBox="0 0 24 24"
    ><path d="M9.6 4.6 A2 2 0 1 1 11 8 H2.5 M12.6 19.4 A2 2 0 1 0 14 16 H2.5 M17.7 7.7 A2.5 2.5 0 1 1 19.5 12 H2.5" /></svg
  >{/snippet}
{#snippet icoLabels()}<svg class="ico stroke" viewBox="0 0 24 24"
    ><path d="M20.4 13.4 l-7 7 a2 2 0 0 1 -2.8 0 L2.5 12 V2.5 h9.5 l8.4 8.4 a2 2 0 0 1 0 2.5 Z" /><circle
      cx="7.3"
      cy="7.3"
      r="1.15"
      fill="currentColor"
      stroke="none"
    /></svg
  >{/snippet}

<!-- svelte-ignore a11y_no_noninteractive_tabindex, a11y_no_noninteractive_element_interactions -->
<div
  class="vis lab"
  tabindex="0"
  role="application"
  aria-label="two-way program visualizer — space runs or pauses, arrow keys step back and forward, shift+arrows change example, r resets"
  onkeydown={onKey}
  onmousedown={(e) => {
    if ((e.target as HTMLElement).closest("button")) e.preventDefault();
  }}
>
  <div class="presets">
    {#each PRESETS as p, i}
      <button type="button" class="pill" class:on={preset === i} onclick={() => pick(i)}>{p.name}</button>
    {/each}
  </div>

  {#if compiled.err !== null && !m}
    <p class="err">{compiled.err}</p>
  {:else if m}
    <Grove
      root={m.root}
      {marks}
      labelOf={(t) => valueLabels.get(t.id) ?? null}
      {budTip}
      {onBud}
      {onRing}
      {reads}
      {keep}
      {motion}
      {next}
      {height}
    />
    <div class="readout">
      <div class="rline">
        <span class="expr">{exprText}</span>
        <span class="rule" class:nf={finished}>{ruleMsg || "press ▶ (or →) to fire the next step, or click a bud"}</span>
        <span class="count"
          >{stepCount} step{stepCount === 1 ? "" : "s"}{parallel ? " · parallel" : ""}{whole
            ? ` · all at once: ${whole.fired} calls in ${whole.rounds} rounds`
            : ""}</span
        >
      </div>
      <div class="forms">
        <span class="flabel">named</span><code>{termText(m.root, reads)}</code>
      </div>
      <div class="forms">
        <span class="flabel">raw</span><code>{termText(m.root, "tree")}</code>
      </div>
    </div>
  {/if}

  <div class="editrow">
    <input
      class="editbox prog"
      class:invalid={compiled.err !== null}
      type="text"
      bind:value={progText}
      oninput={() => (preset = -1)}
      spellcheck="false"
      autocomplete="off"
      aria-label="program: clauses l ⇄ r split by ; or any tree"
    />
    <span class="on">on</span>
    <input
      class="editbox arg"
      class:invalid={compiled.err !== null}
      type="text"
      bind:value={inputText}
      oninput={() => (preset = -1)}
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
        Edits parse as you type; <kbd>space</kbd> runs, <kbd>←</kbd> <kbd>→</kbd> step,
        <kbd>shift</kbd>+arrows change example, <kbd>r</kbd> resets. A program is clauses
        <code>l ⇄ r</code> split by <code>;</code> (or <code>&lt;=&gt;</code>), or any tree. In a
        pattern: <code>△</code> a leaf, <code>△p</code> a stem, <code>(p, q)</code> a fork,
        <code>x</code> a variable, <code>f(p)</code> a call (<code>self</code>, a variable holding a
        program, or a named one), <code>f†(p)</code> a call run backward. Data: <code>T</code>
        <code>F</code>, numbers, words <code>0b1010</code>, two registers <code>&lt;0b10, 0b01&gt;</code>,
        and named programs.
      </span>
    </span>
  </div>
  {#if compiled.err !== null && m}<p class="err small">{compiled.err}</p>{/if}
  {#if preset >= 0}<p class="tip-line">{PRESETS[preset].tip}</p>{/if}

  <div class="bottombar">
    <div class="tside tleft">
      <button class="tbtn" onclick={() => cyclePreset(-1)} aria-label="previous example">{@render icoPrev()}</button>
      <button class="tbtn" onclick={back} disabled={historyLen === 0} aria-label="step back">{@render icoBack()}</button>
    </div>
    <button class="tbtn play" onclick={toggleRun} aria-label={running ? "pause" : "play"}
      >{#if running}{@render icoPause()}{:else}{@render icoPlay()}{/if}</button
    >
    <div class="tside tright">
      <button class="tbtn" onclick={step} aria-label="step forward">{@render icoFwd()}</button>
      <button class="tbtn" onclick={() => cyclePreset(1)} aria-label="next example">{@render icoNext()}</button>
      <button class="tbtn" onclick={reset} aria-label="reset">{@render icoReset()}</button>
    </div>
    <div class="selectors corner-right">
      <button
        class="mpi sel"
        class:on={parallel}
        aria-pressed={parallel}
        data-tip="parallel — fire every ready call per step"
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
        class:on={labels}
        aria-pressed={labels}
        data-tip="labels — values on the data"
        aria-label="labels"
        onclick={() => (labels = !labels)}>{@render icoLabels()}</button
      >
      <button
        class="mpi sel glyphbtn"
        class:on={keep}
        aria-pressed={keep}
        data-tip="residuals — a fired call keeps its program, flipped, on its left"
        aria-label="residuals"
        onclick={() => (keep = !keep)}>†</button
      >
    </div>
  </div>
</div>

<details class="shapes">
  <summary>Every tree is a program, and every tree is a pattern: the five shapes</summary>
  <table>
    <thead>
      <tr><th>shape</th><th>tree calculus, on x</th><th>as a program here, on x</th><th>as a pattern (in a clause)</th></tr>
    </thead>
    <tbody>
      <tr><td><code>△</code></td><td>△x</td><td>x, unchanged</td><td>a leaf</td></tr>
      <tr><td><code>△a</code></td><td>(a, x)</td><td>(a, x), the same rule</td><td>a stem around pattern a</td></tr>
      <tr><td><code>(△, b)</code></td><td>b (K: x is thrown away)</td><td>b run backward (the dagger)</td><td>the variable named b</td></tr>
      <tr><td><code>(△a, b)</code></td><td>(b x)(a x) (S: x is copied)</td><td>a, then b</td><td>a fork of patterns a and b</td></tr>
      <tr>
        <td><code>((w, v), b)</code></td>
        <td>triage on x's shape</td>
        <td>clause w ⇄ v; if w does not fit x, b</td>
        <td>a call: program w run on b (v = △ forward, else backward)</td>
      </tr>
    </tbody>
  </table>
  {#if compiled.err === null}
    <p class="raw">This program as one tree ({size(compiled.prog)} nodes): <code>{show(compiled.prog)}</code></p>
  {/if}
</details>

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
  .presets {
    display: flex;
    flex-wrap: wrap;
    gap: 0.3rem;
    justify-content: center;
  }
  .pill {
    font: inherit;
    font-size: 0.78rem;
    color: var(--fg-muted);
    background: none;
    border: 1px solid var(--border);
    border-radius: 999px;
    padding: 0.12rem 0.6rem;
    cursor: pointer;
  }
  .pill:hover {
    border-color: var(--border-strong);
    color: var(--fg);
  }
  .pill.on {
    border-color: var(--g2);
    color: var(--g2);
    background: color-mix(in oklab, var(--g1) 12%, var(--bg-elev));
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
  .count {
    font-family: var(--font-mono);
    color: var(--fg-faint);
    font-size: 0.75rem;
  }
  .forms {
    display: flex;
    gap: 0.6rem;
    align-items: baseline;
    margin-top: 0.15rem;
  }
  .flabel {
    font-size: 0.68rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--fg-faint);
    flex: none;
    width: 3.2em;
  }
  .forms code {
    font-size: 0.78rem;
    color: var(--fg-muted);
    background: none;
    padding: 0;
    word-break: break-all;
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
  .glyphbtn {
    font-family: var(--font-mono);
    font-size: 0.95rem;
    font-weight: 700;
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
  table {
    border-collapse: collapse;
    margin-top: 0.5rem;
    width: 100%;
  }
  th,
  td {
    text-align: left;
    padding: 0.25rem 0.5rem;
    border-bottom: 1px solid var(--border);
    vertical-align: top;
  }
  th {
    font-weight: 600;
    color: var(--fg-faint);
    font-size: 0.78rem;
  }
  .raw code {
    font-size: 0.78rem;
    word-break: break-all;
  }
</style>
