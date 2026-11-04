<script lang="ts">
  // One view of whatever the cursor is on, as a single reel: binders coming off
  // one rewrite at a time, as one more kind of step beside the tree's own rule
  // firings. Every frame is compiled by the real elaborator; a function reduces
  // on symbols, one per parameter, so it has something to do. Should the
  // store's binder rewrites ever disagree with the compiler's replay, the reel
  // falls back to the replay first and the run after.
  import { untrack } from 'svelte'
  import { disp } from '$lib/disp/client.svelte'
  import type { TermDump, BracketDump, BracketRule } from '$lib/disp/protocol'
  import TreeCanvas, { type CanvasApi, type TreeLook } from './TreeCanvas.svelte'
  import Timeline from './Timeline.svelte'
  import { Store, APP, CUT, LEAF, STEM, FORK, VAR, LAM, HELD_K, ATOM, SPLIT, R_K, R_S, R_F, R_LEAF, R_STEM, R_BIND, RULE_GLYPH, type Id } from './store.ts'
  import { stepGuide } from './guides.ts'
  import { Trace, type Mode, type Call, type TraceEnd } from './trace.ts'
  import { layoutScene, diffTrees, keyPath, K_POD, K_APP, K_LAM, K_VAR, type Scene, type VNode } from './layout.ts'
  import type { Subject } from './subject.ts'

  /// What the viewer is showing. The Learn walkthrough drives the viewer through this and checks its text against it.
  export interface ViewerState {
    subject: string | null
    ready: boolean
    error: string
    phase: 'binders' | 'run'
    at: number
    lead: number
    frame: number
    /// binder rewrites the compiler made
    rewrites: number
    runSteps: number
    end: TraceEnd
    caption: { from: string; to: string; why: string }
    /// the S, K, I and F readings drawn on the tree
    combs: string[]
    /// the rule the marked application fires next: K, S or F, '' when none
    next: string
    sk: boolean
    symbols: string[]
    verdict: string | null
  }
  export interface ViewerApi {
    state: () => ViewerState
    seek: (at: number) => void
    setSk: (on: boolean) => void
    /// holds the controls, each named by a data-tour attribute
    root: HTMLElement
  }

  const MAX_DUMP = 60000
  /// a binder rewrite needs reading, however fast the run plays
  const BINDER_TICK_MS = 650
  /// one per name, the same in a binder, its uses, and the symbols it turns into
  const VAR_COLORS = ['#d9480f', '#1971c2', '#9c36b5', '#2b8a3e', '#c2255c', '#0b7285', '#b35c00', '#5f3dc4']

  interface Props {
    subject: Subject | null
    /// bumped after every run: handles and names may have changed
    runVersion: number
    pinned: boolean
    glow?: string | null
    /// no pin button without it
    onPinToggle?: () => void
    onExpr: (expr: string) => void
    onGlow?: (name: string | null) => void
    onGoto?: (line: number) => void
    /// an expression worth running when the subject is a function waiting for arguments
    suggest?: (subject: Subject) => string | null
    /// the buffer hasn't run yet, so there is nothing to show
    notRun?: boolean
    /// why it hasn't, when pressing Run wouldn't help
    notRunWhy?: string
    /// the file doesn't check, so no definition has a tree
    fileError?: { line?: number } | null
    /// a function's parameter names, to label the arguments of its calls
    paramsOf?: (name: string) => string[] | undefined
    tour?: (a: ViewerApi) => void
  }
  let { paramsOf, notRun = false, notRunWhy, fileError = null, subject, runVersion, pinned, glow = null, onPinToggle, onExpr, onGlow, onGoto, suggest, tour }: Props = $props()

  // svelte-ignore non_reactive_update
  let store = new Store()
  let storeGen = disp.generation
  let open = new Set<Id>()
  let closed = new Set<Id>()
  let labels = $state(true)
  /// read forks and stems as the S, K, I and F they encode
  let sk = $state(true)
  const LOOK_KEY = 'disp-tree-style'
  const LOOKS: TreeLook[] = ['moss', 'garden', 'quiet', 'ink', 'bold']
  const LOOK_TEXT: Record<TreeLook, string> = {
    moss: 'green, with gold where the next steps are',
    garden: 'bark and leaves, coloured only at the next step',
    quiet: 'grey, thin lines',
    ink: 'drawn in pen, like a textbook',
    bold: 'full colour everywhere'
  }
  let look = $state<TreeLook>((LOOKS as string[]).includes(globalThis.localStorage?.getItem(LOOK_KEY) ?? '') ? (localStorage.getItem(LOOK_KEY) as TreeLook) : 'moss')
  function setLook(l: TreeLook) {
    look = l
    localStorage.setItem(LOOK_KEY, l)
  }
  let scene = $state.raw<Scene | null>(null)
  let animMs = $state(0)
  let canvas: CanvasApi | undefined
  let hoverText = $state('')
  let error = $state('')
  let loading = $state(false)
  let exprDraft = $state('')
  let showOptions = $state(false)

  // the reel: bFrames[0 .. lead), then trace.frames
  let bracket = $state.raw<BracketDump | null>(null)
  let bFrames = $state.raw<Id[]>([])
  let bracketKey = ''
  let trace = $state.raw<Trace | null>(null)
  let lead = $state(0)
  let at = $state(0)
  let version = $state(0)
  const frame = $derived(Math.max(0, at - lead))
  /// binders are steps of the run itself (lead stays 0); false while showing the replay first
  let unified = $state(false)
  // the first frame with no binder left, found as the trace fills in
  let bindScan = 0
  let bindEndAt = -1
  const bindEnd = $derived.by(() => {
    void version
    if (!unified || !trace) return lead
    while (bindEndAt < 0 && bindScan <= trace.last) {
      if (!store.isBinder(trace.frames[bindScan])) bindEndAt = bindScan
      bindScan++
    }
    return bindEndAt >= 0 ? bindEndAt : trace.last
  })
  const inBinders = $derived.by(() => {
    void version
    return unified ? !!trace && store.isBinder(trace.frames[frame] ?? trace.frames[0]) : at < lead
  })
  /// the Binders and Run buttons' counts; a parallel step can fire binders and applications together
  const phase = $derived.by(() => {
    void version
    if (!unified || !trace) return { bind: Math.min(at, bindEnd), binds: bindEnd, run: Math.max(0, at - bindEnd), runs: lastAt - bindEnd }
    const f = Math.min(frame, trace.last)
    const e = Math.min(bindEnd, trace.last)
    return { bind: trace.bound[f], binds: trace.bound[e], run: trace.fired[f] - trace.bound[f], runs: trace.fired[trace.last] - trace.bound[trace.last] }
  })
  const bindSteps = $derived.by(() => {
    void version
    return unified ? (trace?.rules.filter((r) => r === R_BIND).length ?? 0) : Math.max(0, (bracket?.steps.length ?? 1) - 1)
  })

  let realValue = -1
  let playing = $state(false)
  let speed = $state(4) // steps per second
  /// a clicked application that has to wait on what's below it: play up to where it fires, then stop
  let stopAt: number | null = null
  let mode = $state<Mode>('eager')
  let hideBuild = $state(true)
  let dump: TermDump | null = null
  let dumpKey = ''
  let token = 0
  /// parameter names the current trace runs on; empty for an ordinary run
  let symbols = $state.raw<string[]>([])
  let useSymbols = $state(true)
  let hasParams = $state(false)
  /// what's on the canvas instead of a frame: both sides of a failing test, or the real value
  let peek = $state.raw<{ roots: Id[]; note: string } | null>(null)
  let diffRoots = $state.raw<Id[]>([])
  let showSteps = $state(false)
  let collapsed = $state.raw(new Set<Call>())

  const varIndex = new Map<string, number>()
  let varSubject = ''
  const varColor = (name: string) => VAR_COLORS[(varIndex.get(name) ?? 0) % VAR_COLORS.length]

  function show(next: Scene) {
    for (const n of next.nodes) if ((n.kind === K_VAR || n.kind === K_LAM) && n.text && !varIndex.has(n.text)) varIndex.set(n.text, varIndex.size)
    scene = next
  }

  function freshStore() {
    if (disp.generation !== storeGen || store.n > 3_000_000) {
      store = new Store()
      storeGen = disp.generation
      open = new Set()
      closed = new Set()
      bracketKey = ''
    }
  }

  // ---- loading ---------------------------------------------------------------
  const keyFor = (s: Subject) => `${s.key}@${runVersion}`

  async function ensureDump(s: Subject, t: number): Promise<TermDump | null> {
    if (dump && dumpKey === keyFor(s)) return dump
    const d = await disp.term(s.term, MAX_DUMP)
    if (t !== token) return null
    dump = d
    dumpKey = keyFor(s)
    return d
  }

  async function ensureBracket(s: Subject, t: number): Promise<BracketDump | null> {
    if (bracket && bracketKey === keyFor(s)) return bracket
    const { expr, path, text, line, side } = s.term
    const d = await disp.bracket({ expr, path, text, line, side }, MAX_DUMP)
    if (t !== token || !d) return null
    bracket = d
    bracketKey = keyFor(s)
    bFrames = d.error ? [] : store.load(d.graph)
    return d
  }

  async function load(s: Subject, t: number) {
    const [d, b] = await Promise.all([ensureDump(s, t), ensureBracket(s, t)])
    if (t !== token || !d || !b) return
    const failed = d.error ?? b.error
    if (failed) {
      error = failed
      trace = null
      scene = null
      return
    }
    error = ''
    peek = null
    const roots = store.load(d.graph)
    const compiled = roots[0]
    realValue = roots[1] ?? -1
    const names = b.params.length > 0 ? b.params : s.kind === 'def' ? (paramsOf?.(s.title) ?? []) : []
    hasParams = names.length > 0
    const params = useSymbols ? names : []
    let root = compiled
    if (params.length > 0) {
      // the definition itself is what you came to see: its name mustn't fold it away
      open.add(root)
      for (const p of params) root = store.app(root, store.variable(p))
      realValue = -1
    }
    symbols = params
    // binders come off as steps of the run when the store's rewrites land where the compiler's replay does
    unified = false
    if (bFrames.length > 1) {
      try {
        unified = store.encode(store.folded(store.removeBinders(bFrames[0]))) === store.encode(bFrames[bFrames.length - 1])
      } catch {
        unified = false
      }
    }
    if (unified) {
      root = bFrames[0]
      for (const p of params) root = store.app(root, store.variable(p))
    }
    // when nothing is applied, the replay's last frame is the run's first
    const shared = params.length === 0 && store.encode(bFrames[bFrames.length - 1]) === compiled
    lead = unified ? 0 : shared ? bFrames.length - 1 : bFrames.length
    // an edit elsewhere in the file leaves this term alone: keep your place
    if (trace?.store === store && trace.mode === mode && trace.frames[0] === root) {
      at = Math.min(at, lead + trace.last)
      draw(0)
    } else startTrace(root)
    diffRoots = []
    if (s.kind === 'test' && s.pass === false && s.lhsH != null && s.rhsH != null) {
      const g = await disp.graph({ handles: [s.lhsH, s.rhsH] }, MAX_DUMP)
      if (t === token && g) diffRoots = store.load(g)
    }
  }

  function startTrace(root: Id) {
    bindScan = 0
    bindEndAt = -1
    trace = new Trace(store, root, mode)
    collapsed = new Set()
    playing = false
    at = 0
    version++
    compute(trace)
    draw(0)
  }

  // frames fill in behind the viewer, a few milliseconds per display frame
  function compute(tr: Trace) {
    const tick = () => {
      if (tr !== trace) return
      const more = tr.advance(6)
      version++
      if (more) requestAnimationFrame(tick)
    }
    requestAnimationFrame(tick)
  }

  /// A finished value is data, so the S and K reading would only mislead.
  function readsSk(): boolean {
    if (!sk) return false
    if (!trace || trace.end !== 'normal' || at !== lead + trace.last || hasParams) return true
    return trace.last === 0 && lead > 0
  }

  /// the frame drawn last, so a single step forward can be drawn with its guide
  let drawnAt = -1
  let lastNearOpen = new Set<Id>()
  function draw(ms: number) {
    animMs = ms
    const was = drawnAt
    drawnAt = peek ? -1 : at
    if (peek) {
      const [a, b] = peek.roots
      if (peek.roots.length === 2 && a !== b) {
        const d = diffTrees(store, a, b)
        show(layoutScene(store, peek.roots, { open, closed }, { same: d.same, diff: d.diff, diffInside: d.inside, titles: ['got', 'want'], badTitles: [true, false], labels }))
      } else show(layoutScene(store, [a], { open, closed }, { labels }))
      return
    }
    if (at < lead || !trace) {
      const steps = bracket?.steps
      if (!steps?.[at]) {
        scene = null
        return
      }
      const root = sk ? bFrames[at] : store.encode(bFrames[at])
      show(layoutScene(store, [root], { open, closed }, { labels, sk, plainNats: !sk, nextPath: steps[at + 1]?.at }))
      return
    }
    const root = trace.frames[frame]
    const keepOpen = ms > 0 && at > was && at - was <= 4 ? lastNearOpen : undefined
    const next = layoutScene(store, [root], { open, closed }, { nextPath: trace.nextPath(root), alsoNext: mode === 'parallel' ? trace.readyPaths(root) : undefined, labels, sk: readsSk(), foldData: labels ? 40 : undefined, keepOpen })
    lastNearOpen = next.nearOpen
    if (ms > 0 && was >= lead && at > was && at - was <= 4) next.guide = stepGuide(store, trace, was - lead)
    show(next)
  }

  $effect(() => {
    const s = subject
    void runVersion
    untrack(() => {
      const t = ++token
      freshStore()
      playing = false
      hoverText = ''
      if (!s) {
        scene = null
        trace = null
        dump = null
        bracket = null
        bracketKey = ''
        error = ''
        return
      }
      if (s.key !== varSubject) {
        varSubject = s.key
        varIndex.clear()
      }
      loading = true
      void load(s, t).finally(() => {
        if (t === token) loading = false
      })
    })
  })

  // ---- transport -------------------------------------------------------------
  const isBuild = (r: number) => r === R_LEAF || r === R_STEM
  function seek(i: number, ms = 0) {
    peek = null
    at = Math.max(0, Math.min(lead + (trace?.last ?? 0), i))
    draw(ms)
  }
  const goto = (f: number, ms = 0) => seek(lead + f, ms)
  /// With building skipped, a scrub lands on the real step a build step leads up to.
  function snap(i: number): number {
    if (!hideBuild || !trace || i < lead) return i
    let f = i - lead
    while (f < trace.last && lead + f !== bindEnd && isBuild(trace.rules[f + 1])) f++
    return lead + f
  }
  /// With building skipped, a step also takes in the stems and forks right after it, so the next one highlighted is real work.
  function stepForward(ms = 320) {
    if (at < lead) {
      seek(at + 1, ms)
      return true
    }
    if (!trace) return false
    let f = frame + 1
    if (f > trace.last) return false
    // never past the frame where the last binder came off, so that frame is always shown
    if (hideBuild) while (f < trace.last && lead + f !== bindEnd && isBuild(trace.rules[f + 1])) f++
    if (stopAt !== null && playing) f = Math.min(f, stopAt - lead)
    // a binder rewrite moves more than a rule firing, and a K drop, an S copy or a triage pick needs a beat to see
    const r = trace.rules[frame + 1]
    goto(f, ms > 0 && r === R_BIND ? Math.max(ms, 520) : ms > 0 && !playing && (r === R_K || r === R_S || r === R_F) ? Math.max(ms, 440) : ms)
    return true
  }
  function stepBack() {
    if (at === 0) return
    if (at <= lead || !trace) {
      seek(at - 1, 260)
      return
    }
    let f = frame - 1
    if (hideBuild) while (f > 0 && lead + f !== bindEnd && isBuild(trace.rules[f + 1])) f--
    goto(f, 260)
  }
  /// Jump to where the next (or previous) binder starts being removed.
  function binderJump(dir: 1 | -1) {
    const steps = bracket?.steps
    if (!steps) return
    const name = steps[at + 1]?.name
    let i = at + dir
    while (i > 0 && i < lead && steps[i + 1]?.name === name) i += dir
    if (dir < 0) while (i > 0 && steps[i]?.name === steps[i + 1]?.name) i--
    seek(i, 320)
  }
  /// Skip the whole next call, like a debugger's step over.
  function stepOver() {
    const c = visibleCalls.find((x) => x.c.start >= frame)?.c
    if (c && c.end > 0) goto(c.end, 320)
    else stepForward()
  }
  /// Jump to where the next call begins.
  function stepIn() {
    const c = visibleCalls.find((x) => x.c.start > frame)?.c
    if (c) goto(c.start, 320)
    else stepForward()
  }
  /// Finish the call we're inside.
  function stepOut() {
    if (!trace) return
    const c = activeCall
    goto(c && c.end > 0 ? c.end : trace.last, 320)
  }
  const lastAt = $derived.by(() => {
    void version
    return lead + (trace?.last ?? 0)
  })
  const runEnded = $derived.by(() => {
    void version
    return !!trace?.end
  })
  $effect(() => {
    if (!playing) stopAt = null
  })
  let playTimer: ReturnType<typeof setTimeout> | undefined
  $effect(() => {
    if (!playing) return
    const runMs = 1000 / speed
    const run = () => {
      if (!playing) return
      if (stopAt !== null && at >= stopAt) {
        playing = false
        stopAt = null
        return
      }
      const binderNext = at < lead || (unified && trace?.rules[frame + 1] === R_BIND)
      const tickMs = binderNext ? Math.max(BINDER_TICK_MS, runMs) : stopAt !== null ? Math.min(runMs, 380) : runMs
      if (!stepForward(tickMs >= 70 ? Math.min(binderNext ? 560 : 380, tickMs * 0.85) : 0)) {
        if (trace?.end) playing = false
        else playTimer = setTimeout(run, 60)
        return
      }
      playTimer = setTimeout(run, tickMs)
    }
    untrack(run)
    return () => clearTimeout(playTimer)
  })
  function togglePlay() {
    if (!trace) return
    stopAt = null
    if (!playing && at >= lastAt && trace.end) seek(0)
    playing = !playing
  }
  function restart() {
    if (subject) {
      const t = ++token
      trace = null
      dump = null
      void load(subject, t)
    }
  }

  function onKey(e: KeyboardEvent) {
    const tag = (e.target as HTMLElement).tagName
    if (tag === 'INPUT' || tag === 'SELECT' || (tag === 'BUTTON' && e.key === ' ')) return
    if (e.key === 'f') {
      canvas?.fit()
      e.preventDefault()
    } else if (e.key === '+' || e.key === '=') canvas?.zoom(1.25)
    else if (e.key === '-') canvas?.zoom(0.8)
    else if (e.key === 'Escape') showOptions = false
    else if (trace) {
      if (e.key === 'ArrowDown') stepIn()
      else if (e.key === 'ArrowRight' && e.shiftKey) inBinders ? binderJump(1) : stepOver()
      else if (e.key === 'ArrowLeft' && e.shiftKey && inBinders) binderJump(-1)
      else if (e.key === 'ArrowRight' && e.altKey) stepOut()
      else if (e.key === 'ArrowRight') {
        playing = false
        stepForward()
      } else if (e.key === 'ArrowLeft') {
        playing = false
        stepBack()
      } else if (e.key === ' ') togglePlay()
      else if (e.key === 'Home') seek(0)
      else if (e.key === 'End') seek(lastAt)
      else return
      e.preventDefault()
    }
  }

  // ---- canvas input ----------------------------------------------------------
  /// What a subtree of this shape does once it's called, by how many operands it holds already.
  const COMB_TEXT: Record<string, string[]> = {
    S: ['S: called on f it becomes S(f)', 'S(f) = t(t(f)): called on g it becomes S(f, g)', 'S(f, g) = t(t(f), g): called on x, the S rule gives f(x, g(x))'],
    K: ['K = t(t): called on x it becomes K(x)', 'K(x) = t(t, x): called on y, the K rule drops y and gives x'],
    I: ['I = t(t(t, t), t): called on x, gives back x'],
    F: ['', '', '', 't(t(c, d), b): called on x, the F rule looks at the shape of x']
  }
  function combWord(head: VNode): string {
    const c = store.readComb(head.id)
    const word = c ? COMB_TEXT[c.op][c.kids.length] : ''
    return word || 'combinator'
  }

  function kindWord(n: VNode): string {
    if (n.comb) return combWord(n)
    const head = n.group >= 0 ? scene?.nodes[n.group] : undefined
    if (head?.comb) return `part of ${combWord(head)}`
    const t = store.tag[n.id]
    if (t === LEAF) return 'leaf'
    if (t === VAR) {
      const name = store.strs[store.a[n.id]]
      if (symbols.includes(name)) return `${name}: a symbol standing in for an argument`
      return inBinders ? `${name}: a use of its binder, which will be removed` : `${name}: a free name, not bound in this scope`
    }
    if (n.kind === K_LAM) return n.rule ? `binder ${n.text}, ready to come off` : `binder ${n.text}, comes off once the binders inside it have`
    if (t === STEM) return 'stem'
    if (t === FORK) return 'fork'
    if (t === CUT) return 'too big to send; click to fetch'
    if (store.rule[n.id] && !n.rule) return 'application, waiting for the binder around it to come off'
    return store.rule[n.id] ? 'application, ready to fire' : 'application, waiting on its parts'
  }

  function handleHover(n: VNode | null) {
    if (!n) {
      hoverText = ''
      onGlow?.(null)
      return
    }
    const size = store.size[n.id]
    const copies = scene ? scene.nodes.filter((m) => m.id === n.id).length : 1
    const head = n.kind === K_POD ? (n.text ?? '') : kindWord(n)
    const fires = !peek && (unified ? n.kind === K_APP || n.kind === K_LAM : n.kind === K_APP && !inBinders)
    const hint = n.kind === K_POD ? 'click to open' : fires ? 'click to fire' : n.kind !== K_APP && size > 1 ? 'click to fold' : ''
    hoverText = [head, size > 1 ? `${fmt(size)} nodes` : '', copies > 1 ? `${copies} copies here, all one shared tree` : '', hint].filter(Boolean).join(' · ')
    onGlow?.(store.names.get(n.id) ?? (n.kind === K_POD ? n.text : null))
  }

  async function handleClick(n: VNode) {
    // a combinator folds as a whole
    if (n.group >= 0 && !n.comb && scene) n = scene.nodes[n.group]
    if (n.kind === K_POD) {
      closed.delete(n.id)
      if (store.tag[n.id] === CUT) {
        const g = await disp.graph({ handles: [store.a[n.id]] }, MAX_DUMP)
        if (!g) return
        const [r] = store.load(g)
        if (r !== n.id) store.resolved.set(n.id, r)
      }
      open.add(n.id)
    } else if (n.kind === K_APP || (unified && n.kind === K_LAM)) {
      if (!trace || (inBinders && !unified) || peek) return
      playing = false
      const was = trace
      const from = frame
      const end = was.fireAt(from, keyPath(n.key))
      version++
      compute(was)
      if (end - from <= 1) {
        stopAt = null
        at = lead + end
        draw(320)
      } else {
        // what it waits on fires first, one step at a time, then it does
        stopAt = lead + end
        seek(lead + from + 1, 320)
        playing = true
      }
      return
    } else if (store.size[n.id] > 1) {
      closed.add(n.id)
      open.delete(n.id)
    } else return
    draw(260)
  }

  function resetFolds() {
    open = new Set()
    closed = new Set()
    draw(260)
  }

  // ---- derived readouts ------------------------------------------------------
  const fmt = (n: number) =>
    n >= 1e12 ? n.toExponential(1) : n >= 1e9 ? (n / 1e9).toFixed(1) + 'B' : n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e4 ? (n / 1e3).toFixed(1) + 'k' : String(n)

  function ruleWords(rule: BracketRule, x: string): string {
    switch (rule) {
      case 'start':
        return ''
      case 'split':
        return `${x} is used in this application, so send it to both sides: S(f, g) gives its argument to f and to g.`
      case 'const':
        return `${x} isn't used in here, so K wraps it: K(e) drops the argument and gives e.`
      case 'id':
        return `This is ${x} itself, so it becomes I, which gives back its argument.`
      case 'eta':
        return `${x} is only passed straight on as the last argument, so the function can take it directly.`
      case 'eta-after':
        return 'S(K(p), I) gives p its argument and nothing else, so it shortens to p.'
      case 'k-comp':
        return 'S(K(p), K(q)) ignores its argument on both sides, so it shortens to K(p(q)).'
      case 'lam':
        return `Removing ${x} from a binder still in place.`
      case 'skip':
        return `The rest of the replay, ${bracket?.skipped ?? 0} rewrites, is left out; next is the finished result.`
    }
  }

  function fireWords(site: Id): string {
    const f = store.a[site]
    const x = store.pretty(store.b[site], 8, sk)
    switch (store.rule[site]) {
      case R_K:
        return `K keeps what it holds and drops ${x}.`
      case R_S:
        return `S hands ${x} to both sides.`
      case R_F:
        if (f === store.i || store.a[f] === store.i) return `I gives back ${x}.`
        switch (store.tt(store.b[site])) {
          case LEAF:
            return `${x} is a leaf, so it takes the first branch.`
          case STEM:
            return `${x} is a stem, so the second branch gets what it holds.`
          case FORK:
            return `${x} is a fork, so the third branch gets both its halves.`
          default:
            return `Looks at the shape of ${x} to pick a branch.`
        }
      default:
        return 'This only builds a tree node, nothing else happens.'
    }
  }

  const BIND_NAMES: Record<string, string> = { split: 'S split', const: 'K wrap', id: 'I', eta: 'eta', 'eta-after': 'eta', 'k-comp': 'K shortcut' }
  /// Names in the tree that are neither parameters nor defined anywhere: they stay symbols.
  const unknown = $derived.by(() => {
    void version
    if (!trace) return []
    const out = new Set<string>()
    const seen = new Set<Id>()
    const stack = [trace.frames[0]]
    while (stack.length && seen.size < 5000) {
      const id = stack.pop()!
      if (seen.has(id)) continue
      seen.add(id)
      const t = store.tag[id]
      if (t === VAR) {
        const name = store.strs[store.a[id]]
        if (!symbols.includes(name)) out.add(name)
      } else if (t === LAM) stack.push(store.b[id])
      else if (t === STEM || t === HELD_K || t === ATOM) stack.push(store.a[id])
      else if (t === FORK || t === APP || t === SPLIT) stack.push(store.a[id], store.b[id])
    }
    return [...out]
  })
  /// What a parallel step fires, by kind: "Fires 3 steps at once: 2 K, 1 binder."
  function parallelWords(root: Id): string {
    const paths = trace!.readyPaths(root)
    const by = new Map<string, number>()
    for (const p of paths) {
      const site = trace!.subterm(root, p)
      const r = store.rule[site]
      const bind = r === R_BIND ? store.bindKind(site) : null
      const kind = bind ? BIND_NAMES[bind] : r === R_K ? 'K' : r === R_S ? 'S' : r === R_F ? 'triage' : 'build'
      by.set(kind, (by.get(kind) ?? 0) + 1)
    }
    if (paths.length === 1) return `Fires the one ready step (${[...by.keys()][0]}).`
    return `Fires ${paths.length} steps at once: ${[...by].map(([k, n]) => `${n} ${k}`).join(', ')}.`
  }

  /// The step about to happen, which the tree's highlight points at. Words only once there's nothing next.
  const caption = $derived.by<{ from: string; to: string; why: string; full?: string }>(() => {
    void version
    if (peek) return { from: '', to: '', why: peek.note }
    if (at < lead) {
      const next = bracket?.steps[at + 1]
      if (next) return { from: next.before, to: next.after, why: ruleWords(next.rule, next.name) }
      return { from: store.pretty(bFrames[at], 90, sk), to: '', why: `All binders are gone. Next, call it with ${symbols.join(', ')}.` }
    }
    if (!trace) return { from: '', to: '', why: '' }
    const root = trace.frames[frame] ?? trace.frames[0]
    const path = trace.nextPath(root)
    if (!path) return { from: store.pretty(root, 90, readsSk()), to: '', why: '' }
    const binderless = unified && frame > 0 && frame === bindEnd ? 'All binders are gone. ' : ''
    const start = frame === 0 ? (lead > 0 && symbols.length === 0 ? 'All binders are gone. ' : symbols.length > 0 ? `Called with ${symbols.join(', ')}. ` : '') : binderless
    if (mode === 'parallel') return { from: store.pretty(root, 60, sk), to: '', why: `${start}${parallelWords(root)}` }
    const site = trace.subterm(root, path)
    const bind = store.bindKind(site)
    if (bind) return { from: store.pretty(site, 60, sk), to: store.pretty(trace.fireNode(site), 60, sk), why: start + ruleWords(bind, store.tag[site] === LAM ? store.strs[store.a[site]] : '') }
    const from = store.pretty(site, 24, sk, 3)
    const to = store.pretty(trace.fireNode(site), 24, sk, 3)
    // read as S, K and I, a step that only adds a branch can look like nothing at all
    if (from === to) return { from, to: '', why: start + 'The next step only adds a branch inside this; read as S, K and I nothing changes.', full: store.pretty(site, 200, sk) }
    return { from, to, why: start + fireWords(site), full: store.pretty(site, 200, sk) }
  })

  /// Split code into runs, marking the names the tree is drawing in colour.
  function tokens(text: string): { text: string; color: string | null }[] {
    void scene
    return text.split(/([A-Za-z_][\w']*)/).map((p, i) => ({ text: p, color: i % 2 === 1 && varIndex.has(p) ? varColor(p) : null }))
  }

  const counts = $derived.by(() => {
    void version
    const c = { K: 0, S: 0, F: 0, build: 0 }
    if (!trace) return c
    for (let i = 1; i <= frame && i < trace.rules.length; i++) {
      const r = trace.rules[i]
      if (r === R_K) c.K++
      else if (r === R_S) c.S++
      else if (r === R_F) c.F++
      else if (r !== R_BIND) c.build++
    }
    return c
  })
  const curSize = $derived.by(() => {
    void version
    if (peek || !trace) return null
    const id = at < lead ? bFrames[at] : (trace.frames[frame] ?? trace.frames[0])
    return { size: store.size[id], depth: store.depth[id], reads: store.label(id)?.text ?? null }
  })
  const verdict = $derived.by(() => {
    void version
    if (!trace?.end || peek) return null
    const end = trace.frames[trace.last]
    if (trace.end === 'limit') return { kind: 'warn', text: `Stopped drawing after ${fmt(trace.last)} steps. The real evaluator still finished this.` }
    if (trace.end === 'stuck' && symbols.length > 0) return { kind: 'ok', text: 'Stops here: nothing more can happen until these symbols are real values.' }
    if (trace.end === 'stuck') return { kind: 'warn', text: 'Stuck on a free name, so it can\'t reduce further.' }
    if (symbols.length > 0) return { kind: 'ok', text: `This is what it does with any ${symbols.join(', ')}.` }
    if (realValue >= 0 && store.tag[realValue] !== CUT && end !== realValue) return { kind: 'err', text: 'Differs from the real evaluator\'s tree, which is a bug in this view.' }
    if (hasParams) return { kind: 'ok', text: 'This is a function, waiting for its arguments.' }
    if (realValue >= 0 && store.tag[realValue] !== CUT) return { kind: 'ok', text: 'Ends at the same tree the real evaluator computed.' }
    return { kind: 'ok', text: 'Reached a normal form.' }
  })

  // the call tree, windowed: a run can make a hundred thousand calls
  const ROW = 22
  let callsEl = $state<HTMLDivElement>()
  let callsTop = $state(0)
  let callsH = $state(0)
  // the plumbing of recursion and branching: true, but it buries the program's own calls
  const PLUMBING = new Set(['I', 'K', 'S', 'omega', 'wait', 'fix', 'triage', 'id', 'cond', 'select', 'test'])
  let allCalls = $state(false)
  const visibleCalls = $derived.by(() => {
    void version
    if (!trace) return []
    const out: { c: Call; depth: number }[] = []
    const ends: number[] = []
    let skipUntil = -1
    for (const c of trace.calls) {
      if (c.start < skipUntil) continue
      if (!allCalls && PLUMBING.has(c.name)) continue
      while (ends.length > 0 && ends[ends.length - 1] <= c.start) ends.pop()
      out.push({ c, depth: ends.length })
      const end = c.end < 0 ? Infinity : c.end
      ends.push(end)
      if (collapsed.has(c)) skipUntil = end
    }
    return out
  })
  const hiddenCalls = $derived.by(() => {
    void version
    return allCalls || !trace ? 0 : trace.calls.length - visibleCalls.length
  })
  const activeCall = $derived.by(() => {
    let best: Call | null = null
    for (const { c } of visibleCalls) {
      if (c.start > frame) break
      if (c.end < 0 || c.end > frame) best = c
    }
    return best
  })
  const callWindow = $derived.by(() => {
    const from = Math.max(0, Math.floor(callsTop / ROW) - 4)
    const to = Math.min(visibleCalls.length, from + Math.ceil(callsH / ROW) + 8)
    return { from, items: visibleCalls.slice(from, to) }
  })
  // keep the active row on screen while playing or scrubbing
  $effect(() => {
    const c = activeCall
    const el = untrack(() => callsEl)
    if (!c || !el) return
    const i = untrack(() => visibleCalls).findIndex((v) => v.c === c)
    if (i < 0) return
    const y = i * ROW
    if (y < el.scrollTop || y > el.scrollTop + el.clientHeight - ROW) el.scrollTop = Math.max(0, y - el.clientHeight / 2)
  })
  function callText(c: Call): string {
    const names = paramsOf?.(c.name)
    const args = c.args.map((a, i) => (names?.[i] ? `${names[i]}: ` : '') + store.pretty(a, 6, sk))
    return `${c.name}(${args.join(', ')})`
  }
  function hasKids(c: Call, depth: number, i: number): boolean {
    const next = visibleCalls[i + 1]
    return collapsed.has(c) || (next != null && next.depth > depth)
  }
  function toggleCall(c: Call) {
    const next = new Set(collapsed)
    if (next.has(c)) next.delete(c)
    else next.add(c)
    collapsed = next
  }

  function toggleSk() {
    sk = !sk
    draw(260)
  }
  function setSymbols(on: boolean) {
    useSymbols = on
    restart()
  }
  function setMode(m: Mode) {
    mode = m
    restart()
  }
  function compareSides() {
    peek = peek ? null : { roots: diffRoots, note: 'Shared parts fold away. Red marks where the two sides disagree; open a red pod to get closer.' }
    draw(260)
  }
  function showValue() {
    peek = { roots: [realValue], note: 'The value the real evaluator computed. Step or scrub to go back to the run.' }
    draw(260)
  }
  function keepVisible(el: HTMLElement, on: boolean) {
    const show = (v: boolean) => {
      if (v) el.scrollIntoView({ block: 'nearest' })
    }
    show(on)
    return { update: show }
  }

  let rootEl: HTMLElement
  $effect(() => {
    if (!tour) return
    untrack(() =>
      tour({
        state: () => ({
          subject: subject?.key ?? null,
          ready: !!subject && !loading && !!trace?.end,
          error,
          phase: inBinders ? 'binders' : 'run',
          at,
          lead: bindEnd,
          frame,
          rewrites: bindSteps,
          runSteps: lastAt - bindEnd,
          end: trace?.end ?? null,
          caption,
          combs: scene?.nodes.flatMap((n) => (n.comb ? [n.comb] : [])) ?? [],
          next: RULE_GLYPH[scene?.nodes.find((n) => n.next)?.rule ?? 0] ?? '',
          sk,
          symbols,
          verdict: verdict?.text ?? null
        }),
        seek: (i) => {
          playing = false
          seek(i, 320)
        },
        setSk: (on) => {
          if (sk !== on) toggleSk()
        },
        root: rootEl
      })
    )
  })
</script>

<svelte:window
  onpointerdown={(e) => {
    if (showOptions && !(e.target as HTMLElement).closest('.options, .optbtn')) showOptions = false
  }}
/>

<!-- svelte-ignore a11y_no_noninteractive_tabindex, a11y_no_noninteractive_element_interactions -->
<section class="insp" tabindex="0" onkeydown={onKey} aria-label="tree view" bind:this={rootEl}>
  <header class="head">
    {#if subject}
      {#if subject.line && onGoto}
        <button class="subj-title" onclick={() => onGoto?.(subject!.line!)} title="Jump to line {subject.line}">
          <span class="kind">{subject.kind}</span>{subject.title.replace(/^test\s+/, '')}
        </button>
      {:else}
        <span class="subj-title static" title={subject.title}><span class="kind">expr</span>{subject.title}</span>
      {/if}
      {#if subject.kind === 'test' && subject.pass != null}<span class={subject.pass ? 'pass' : 'fail'}>{subject.pass ? '✓' : '✗'}</span>{/if}
    {/if}
    <form
      class="exprform"
      onsubmit={(e) => {
        e.preventDefault()
        if (exprDraft.trim()) onExpr(exprDraft.trim())
      }}
    >
      <input
        bind:value={exprDraft}
        placeholder="Try an expression"
        spellcheck="false"
        autocomplete="off"
        aria-label="expression to inspect"
        data-tour="expr"
      />
    </form>
    <div class="tools">
      {#if onPinToggle}<button class="ic" class:on={pinned} onclick={onPinToggle} title={pinned ? 'Pinned. Click to follow the cursor again' : 'Pin: stop following the cursor'} aria-label="pin this tree" aria-pressed={pinned}>
        <svg viewBox="0 0 16 16" aria-hidden="true"><path d="M9.8 1.8 14.2 6.2 12.9 6.6 10.3 9.2 10 12.4 8.8 13.6 6 10.8 2.6 14.2 1.8 13.4 5.2 10 2.4 7.2 3.6 6 6.8 5.7 9.4 3.1Z" fill="currentColor" /></svg>
      </button>{/if}
      <button class="ic" onclick={() => canvas?.fit()} title="Fit to view (f)" aria-label="fit to view">
        <svg viewBox="0 0 16 16" aria-hidden="true"><path d="M2 6V2h4M10 2h4v4M14 10v4h-4M6 14H2v-4" fill="none" stroke="currentColor" stroke-width="1.5" /></svg>
      </button>
      <button class="ic" class:on={!labels} aria-label="fold named parts" onclick={() => { labels = !labels; draw(260) }} title={labels ? 'Named parts and big data are folded. Click to draw everything open' : 'Everything is drawn open. Click to fold named parts and big data'} aria-pressed={!labels}>
        <svg viewBox="0 0 16 16" aria-hidden="true"><circle cx="8" cy="3.4" r="1.7" fill="currentColor" /><circle cx="4" cy="12" r="1.7" fill="currentColor" /><circle cx="12" cy="12" r="1.7" fill="currentColor" /><path d="M8 4 4 12M8 4l4 8" stroke="currentColor" stroke-width="1.3" /></svg>
      </button>
      <div class="lookpick">
        <button class="lookchip" onclick={() => setLook(LOOKS[(LOOKS.indexOf(look) + 1) % LOOKS.length])} title="Tree style: {look}, {LOOK_TEXT[look]}. Click for the next style, hover to pick" aria-label="tree style {look}">
          <i class="sw sw-{look}"></i>{look}
        </button>
        <div class="lookmenu" role="menu">
          {#each LOOKS as l (l)}
            <button role="menuitemradio" tabindex="-1" aria-checked={look === l} class:on={look === l} onclick={() => setLook(l)} title={LOOK_TEXT[l]}><i class="sw sw-{l}"></i>{l}</button>
          {/each}
        </div>
      </div>
      <button class="ic sk" class:on={sk} onclick={toggleSk} data-tour="sk" title={sk ? 'Reading the tree as S, K and I. Click for the plain tree' : 'Showing the plain tree. Click to read it as S, K and I'} aria-pressed={sk}>SK</button>
      <button class="ic" onclick={resetFolds} title="Reset what you've opened and folded" aria-label="reset folds">
        <svg viewBox="0 0 16 16" aria-hidden="true"><path d="M3 3v4h4M3.4 7A5 5 0 1 1 3 9.5" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" /></svg>
      </button>
    </div>
  </header>

  <div class="stagewrap" data-tour="tree">
    {#if subject && !error}
      <TreeCanvas {scene} {animMs} {glow} {varColor} {look} steady={playing} falling={!peek} onHover={handleHover} onClick={(n) => void handleClick(n)} api={(a) => (canvas = a)} />
      <div class="hoverbar" class:show={!!hoverText}>{hoverText}</div>
    {/if}
    {#if !subject}
      <div class="empty">
        {#if fileError}<p class="big">The file has an error{fileError.line ? ` on line ${fileError.line}` : ''}, so there are no trees until it's fixed.</p>
        {:else if notRun && notRunWhy}<p class="big">{notRunWhy}</p>
        {:else}
        {#if notRun}<p class="waiting">This file hasn't run yet. Press Run to see its trees.</p>{/if}
        <p class="big">Put your cursor on a definition or test.</p>
        <p>You'll watch its binders come off and the tree reduce, one step at a time. Select any expression to see just that part.</p>
        <dl class="key">
          <div><dt><i class="k-var"></i></dt><dd>a variable, in one colour wherever it goes</dd></div>
          <div><dt><i class="k-leaf"></i></dt><dd>leaf: <code>t</code>, the only atom</dd></div>
          <div><dt><i class="k-stem"></i></dt><dd>stem: a leaf applied to one thing</dd></div>
          <div><dt><i class="k-fork"></i></dt><dd>fork: a leaf applied to two things</dd></div>
          <div><dt><i class="k-app"></i></dt><dd>an application still waiting to run</dd></div>
          <div><dt><i class="k-ready">K</i></dt><dd>one that can run now, lettered with its rule (K, S, or F for triage); petals mark the one that runs next</dd></div>
          <div><dt><i class="k-ready">△</i></dt><dd>△ or f: a step that only adds a branch</dd></div>
          <div><dt><i class="k-comb">S</i></dt><dd>{look === 'bold' ? 'a tinted part' : 'a branch labelled S'} that runs the S rule once it's called (K and I likewise)</dd></div>
        </dl>
        {/if}
      </div>
    {:else if error}
      <div class="empty errbox"><p class="big">Can't show this.</p><p><code>{error}</code></p></div>
    {/if}
    {#if loading}<div class="loadbar"></div>{/if}
  </div>

  {#if subject && !error && trace}
    {#if lastAt === 0}
      {@const idea = suggest?.(subject)}
      <div class="runbar still">
        <p>
          {#if curSize?.reads}<b class="reads">{curSize.reads}</b> · {/if}{#if store.tag[trace.frames[0]] === APP}{#if unknown.length}{unknown.join(', ')} {unknown.length === 1 ? "isn't" : "aren't"} defined, so {unknown.length === 1 ? 'it is' : 'they are'} drawn as a symbol and nothing can fire.{:else}Nothing can fire here yet.{/if}{:else}Already a finished tree, so there's nothing to run.{/if}
          {#if curSize}<span class="dim"> {fmt(curSize.size)} node{curSize.size === 1 ? '' : 's'}, depth {curSize.depth}</span>{/if}
        </p>
        {#if idea}<button class="idea" onclick={() => onExpr(idea)}>Run <code>{idea}</code></button>{/if}
      </div>
    {:else}
      <div class="runbar">
        <div class="now" class:binder={inBinders} data-tour="caption">
          {#if caption.from}
            <code class="rewrite" title={caption.full}>
              {#each tokens(caption.from) as p}{#if p.color}<b style="color:{p.color}">{p.text}</b>{:else}{p.text}{/if}{/each}
              {#if caption.to}<span class="arrow">&nbsp;→&nbsp;</span>{#each tokens(caption.to) as p}{#if p.color}<b style="color:{p.color}">{p.text}</b>{:else}{p.text}{/if}{/each}{/if}
            </code>
          {/if}
          {#if caption.why}<span class="why">{caption.why}</span>{/if}
          {#if verdict && at >= lastAt}
            <span class="verdict {verdict.kind}">{verdict.text}</span>
            {#if trace.end === 'limit' && realValue >= 0}<button class="idea" onclick={showValue}>Show the value</button>{/if}
            {#if symbols.length > 0 || hasParams}
              {@const idea = suggest?.(subject)}
              {#if idea}<button class="idea" onclick={() => onExpr(idea)}>Run <code>{idea}</code></button>{/if}
            {/if}
          {/if}
          {#if diffRoots.length === 2}
            <span class="verdict err">This test fails.</span>
            <button class="idea" onclick={compareSides}>{peek ? 'Back to the run' : 'Compare both sides'}</button>
          {/if}
          {#if bracket?.mismatch}<span class="verdict err">This replay didn't land on the compiler's output, which is a bug in this view.</span>{/if}
        </div>
        <Timeline {trace} {at} {lead} {version} onScrub={(i) => { playing = false; seek(snap(i)) }} />
        <div class="transport">
          <button class="tb" onclick={() => seek(0)} title="Start (Home)" aria-label="start" data-tour="start">⏮</button>
          <button class="tb" onclick={() => { playing = false; stepBack() }} title="Step back (←)" aria-label="step back" data-tour="back">◀</button>
          <button class="tb play" onclick={togglePlay} data-tour="play" title="Play or pause (space)" aria-label={playing ? 'pause' : 'play'}>{playing ? '❚❚' : '▶'}</button>
          <button class="tb" onclick={() => { playing = false; stepForward() }} title="Step (→)" aria-label="step" data-tour="step">▶|</button>
          <button class="tb" onclick={() => { playing = false; seek(lastAt) }} title="End (End)" aria-label="end" data-tour="end">⏭</button>
          {#if bindEnd > 0}
            <button class="phase" class:on={inBinders} data-tour="binders" onclick={() => { playing = false; seek(0) }} title={unified ? 'Binders coming off, each a step like any other. Shift + arrows jump between binders' : 'The compiler removing binders. Shift + arrows jump between binders'}>Binders <span>{phase.bind}/{phase.binds}</span></button>
          {/if}
          <button class="phase" class:on={!inBinders} data-tour="run" onclick={() => { playing = false; seek(bindEnd) }} title="The tree reducing, one rule at a time">Run <span>{phase.run}/{phase.runs}{runEnded ? '' : '…'}</span></button>
          <span class="size">{#if curSize}{fmt(curSize.size)} node{curSize.size === 1 ? '' : 's'}{/if}</span>
          {#if showSteps && !inBinders && mode === 'eager'}
            <button class="tb word" onclick={() => { playing = false; stepIn() }} title="Jump to the next call (↓)">in</button>
            <button class="tb word" onclick={() => { playing = false; stepOver() }} title="Skip the next call (shift →)">over</button>
            <button class="tb word" onclick={() => { playing = false; stepOut() }} title="Finish this call (alt →)">out</button>
          {/if}
          <button class="tb" class:on={showSteps} data-tour="steps" onclick={() => (showSteps = !showSteps)} title={inBinders ? 'Every rewrite' : 'Every call'} aria-label={inBinders ? 'every rewrite' : 'every call'} aria-pressed={showSteps}>≣</button>
          <button class="tb optbtn" class:on={showOptions} onclick={() => (showOptions = !showOptions)} title="Options and key" aria-label="options" aria-expanded={showOptions}>⋯</button>
        </div>
        {#if showOptions}
          <div class="options">
            <label class="row">Speed <input class="speed" type="range" min="1" max="120" bind:value={speed} aria-label="speed" /> <span class="dim">{speed} a second</span></label>
            {#if hasParams}<label title="Apply this function to one symbol per parameter, so you can watch what it does with them"><input type="checkbox" checked={useSymbols} onchange={(e) => setSymbols((e.target as HTMLInputElement).checked)} />Run on symbols</label>{/if}
            <label title="Steps take in the stems and forks they leave to build"><input type="checkbox" bind:checked={hideBuild} />Skip steps that only build</label>
            <label title="Fire every ready application at once. The call list needs one at a time."><input type="checkbox" checked={mode === 'parallel'} onchange={(e) => setMode((e.target as HTMLInputElement).checked ? 'parallel' : 'eager')} />Fire all at once</label>
            <div class="tally">So far: <b class="r-k">K</b>{counts.K} <b class="r-s">S</b>{counts.S} <b class="r-f">F</b>{counts.F} <b class="r-b">build</b>{counts.build}</div>
            <div class="legend">
              <span><i class="k-var"></i>variable</span><span><i class="k-leaf"></i>leaf</span><span><i class="k-stem"></i>stem</span><span><i class="k-fork"></i>fork</span><span><i class="k-app"></i>waiting</span><span><i class="k-comb">S</i>runs S when called</span>
            </div>
            <div class="encode"><b>S(f, g)</b> = t(t(f), g) · <b>K(x)</b> = t(t, x) · <b>I</b> = t(t(t, t), t). {look === 'bold' ? 'The tinted band' : 'The labelled branch'} is the S, K or I itself; what hangs below it is f, g or x.</div>
          </div>
        {/if}
      </div>
      {#if showSteps}
        {#if inBinders}
          <div class="calls">
            <div class="calllist">
              {#each bracket?.steps ?? [] as st, i}
                {#if i > 0}
                  <button class="brow" class:active={i === at + 1} onclick={() => { playing = false; seek(i - 1, 300) }} use:keepVisible={i === at + 1}>
                    <span class="bbefore">{st.before}</span>{#if st.after}<span class="arrow">→</span><span class="callres">{st.after}</span>{/if}
                  </button>
                {/if}
              {/each}
            </div>
          </div>
        {:else if mode === 'eager'}
          <div class="calls">
            {#if hiddenCalls > 0 || allCalls}
              <label class="allcalls" title="Show calls to I, K, omega and other building blocks of recursion and branching"><input type="checkbox" bind:checked={allCalls} />show plumbing ({fmt(hiddenCalls)})</label>
            {/if}
            {#if visibleCalls.length === 0}
              <p class="nocalls">No named functions were called. Named calls show up here as the run reaches them.</p>
            {:else}
              <div class="calllist" bind:this={callsEl} bind:clientHeight={callsH} onscroll={(e) => (callsTop = (e.target as HTMLElement).scrollTop)}>
                <div style="height:{visibleCalls.length * ROW}px; position: relative">
                  {#each callWindow.items as { c, depth }, i (c)}
                    {@const idx = callWindow.from + i}
                    <div class="call" class:active={c === activeCall} style="top:{idx * ROW}px; padding-left:{Math.min(depth, 24) * 11 + 4}px">
                      {#if hasKids(c, depth, idx)}
                        <button class="fold" onclick={() => toggleCall(c)} aria-label="collapse"><span class="chev" class:shut={collapsed.has(c)}>▾</span></button>
                      {:else}<span class="foldgap"></span>{/if}
                      <button class="callbtn" onclick={() => { playing = false; goto(c.start, 0) }} title="Jump to this call">
                        <span class="callname">{callText(c)}</span>
                        {#if c.result >= 0}<span class="arrow">→</span><span class="callres">{store.pretty(c.result, 6, sk)}</span>{/if}
                      </button>
                      {#if c.end > 0}<span class="callcost" title="steps inside this call">{fmt(c.end - c.start)}</span>{/if}
                    </div>
                  {/each}
                </div>
              </div>
            {/if}
          </div>
        {/if}
      {/if}
    {/if}
  {/if}
</section>

<style>
  .insp {
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 0;
    background: var(--bg-code);
    outline: none;
    font-size: 0.82rem;
    color: var(--fg-muted);
  }
  .insp:focus-visible {
    box-shadow: inset 0 0 0 2px color-mix(in oklab, var(--accent) 60%, transparent);
  }
  .head {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 0.45rem;
    padding: 0.35rem 0.5rem;
    border-bottom: 1px solid var(--border);
    background: var(--bg-elev);
  }
  .exprform {
    flex: 1 1 6rem;
    min-width: 0;
  }
  /* on a desktop the title and expression box give up width before the icons wrap */
  @media (min-width: 821px) {
    .head {
      flex-wrap: nowrap;
    }
    .exprform {
      min-width: 4.5rem;
    }
  }
  .exprform input {
    width: 100%;
    background: var(--bg-panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 0.22rem 0.5rem;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.76rem;
    outline: none;
  }
  .exprform input:focus {
    border-color: var(--accent);
  }
  .exprform input:focus-visible {
    outline: 2px solid color-mix(in oklab, var(--accent) 45%, transparent);
    outline-offset: 1px;
  }
  .tools {
    display: flex;
    gap: 1px;
    flex: none;
  }
  .lookpick {
    position: relative;
  }
  .lookchip,
  .lookmenu button {
    display: flex;
    align-items: center;
    gap: 0.35em;
    padding: 0.12rem 0.5rem 0.12rem 0.35rem;
    border: 1px solid var(--border);
    border-radius: 999px;
    background: var(--bg-elev);
    color: var(--fg-muted);
    font: 500 0.7rem var(--font-body);
    cursor: pointer;
    white-space: nowrap;
  }
  /* one width for every style's name, so switching doesn't reflow the header */
  .lookchip {
    min-width: 4.5rem;
  }
  .lookchip:hover {
    color: var(--fg);
    border-color: var(--border-strong);
  }
  /* the other styles hang under the chip on hover, like the theme menu */
  .lookmenu {
    position: absolute;
    top: 100%;
    right: 0;
    z-index: 30;
    display: none;
    flex-direction: column;
    gap: 0.2rem;
    padding: 0.35rem 0 0;
  }
  .lookpick:hover .lookmenu,
  .lookpick:focus-within .lookmenu {
    display: flex;
  }
  .lookmenu button {
    box-shadow: var(--shadow-lift);
  }
  .lookmenu button.on {
    color: var(--fg);
    border-color: var(--accent);
  }
  .sw {
    display: inline-block;
    width: 0.7rem;
    height: 0.7rem;
    border-radius: 50%;
    flex: none;
  }
  .sw-moss {
    background: radial-gradient(circle at 50% 50%, var(--g4) 0 32%, #8fb38a 34%);
  }
  .sw-garden {
    background: linear-gradient(135deg, #8b6f4e 0 50%, #6dbd7e 50%);
  }
  .sw-quiet {
    background: var(--bg-code);
    border: 1.5px solid var(--fg-faint);
  }
  .sw-ink {
    background: var(--bg-code);
    border: 1.5px solid var(--fg);
  }
  .sw-bold {
    background: conic-gradient(var(--hl-kw) 0 33%, var(--warn) 0 66%, #1971c2 0);
  }
  .ic {
    display: grid;
    place-items: center;
    width: 1.6rem;
    height: 1.6rem;
    padding: 0;
    background: none;
    border: none;
    border-radius: 5px;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .ic svg {
    width: 0.92rem;
    height: 0.92rem;
  }
  .ic:hover {
    color: var(--fg);
    background: var(--bg-panel-hover);
  }
  .ic.on {
    color: var(--accent);
  }
  .subj-title {
    flex: 0 1 38%;
    min-width: 0;
    text-align: left;
    background: none;
    border: none;
    padding: 0;
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.78rem;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    cursor: pointer;
  }
  .subj-title.static {
    cursor: default;
  }
  .kind {
    font-family: var(--font-body);
    font-size: 0.66rem;
    text-transform: uppercase;
    letter-spacing: 0.07em;
    color: var(--fg-faint);
    margin-right: 0.3em;
  }
  .stagewrap {
    position: relative;
    flex: 1 1 0;
    min-height: 8rem;
  }
  .hoverbar {
    position: absolute;
    left: 0.6rem;
    right: 0.6rem;
    bottom: 0.4rem;
    padding: 0.2rem 0.5rem;
    border-radius: 6px;
    background: color-mix(in oklab, var(--bg-elev) 92%, transparent);
    border: 1px solid var(--border);
    font-size: 0.74rem;
    color: var(--fg);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.08s linear;
  }
  .hoverbar.show {
    opacity: 1;
  }
  .legend {
    display: flex;
    flex-wrap: wrap;
    gap: 0.7rem;
    font-size: 0.68rem;
    color: var(--fg-faint);
  }
  .legend span {
    display: inline-flex;
    align-items: center;
    gap: 0.28em;
  }
  i[class^='k-'] {
    display: inline-block;
    width: 0.62rem;
    height: 0.62rem;
    border-radius: 50%;
  }
  .k-var {
    background: #1971c2;
    width: 1rem !important;
    border-radius: 0.4rem !important;
  }
  .k-leaf {
    background: #4aa96c;
    border-radius: 0 60% 0 60% !important;
  }
  .k-stem {
    border: 1.5px solid var(--fg-muted);
  }
  .k-fork {
    background: var(--fg-muted);
  }
  .k-app {
    border: 1.5px solid var(--hl-kw);
  }
  .k-ready {
    border: 1.5px solid #c99a2e;
    background: #f6e3b0;
    color: #6b4e12;
    width: 0.85rem !important;
    height: 0.85rem !important;
    font: 700 0.55rem/0.8rem var(--font-mono);
    font-style: normal;
    text-align: center;
  }
  .k-comb {
    background: var(--hl-kw);
    color: #fff;
    width: 0.8rem !important;
    height: 0.8rem !important;
    font: 700 0.55rem/0.8rem var(--font-mono);
    font-style: normal;
    text-align: center;
    border-radius: 0.2rem !important;
  }
  .ic.sk {
    font-family: var(--font-mono);
    font-size: 0.64rem;
    font-weight: 700;
  }
  .encode {
    margin-top: 0.15rem;
    font-family: var(--font-mono);
    font-size: 0.68rem;
    color: var(--fg-faint);
  }
  .encode b {
    color: var(--fg-muted);
    font-weight: 600;
  }
  .brow {
    display: flex;
    gap: 0.35em;
    width: 100%;
    height: 22px;
    align-items: center;
    padding: 0 0.5rem;
    background: none;
    border: none;
    font-family: var(--font-mono);
    font-size: 0.72rem;
    color: var(--fg);
    text-align: left;
    white-space: nowrap;
    overflow: hidden;
    cursor: pointer;
  }
  .brow:hover {
    background: var(--bg-panel-hover);
  }
  .brow.active {
    background: color-mix(in oklab, var(--accent) 16%, transparent);
  }
  .bbefore {
    color: var(--hl-fn);
    overflow: hidden;
    text-overflow: ellipsis;
    flex: 0 1 auto;
    min-width: 2ch;
  }
  .empty {
    position: absolute;
    inset: 0;
    display: flex;
    flex-direction: column;
    justify-content: center;
    padding: 1.5rem;
    max-width: 26rem;
    margin: auto;
    color: var(--fg-muted);
  }
  .empty p {
    margin: 0 0 0.5rem;
  }
  .waiting {
    color: var(--warn);
  }
  .big {
    font-size: 1rem;
    color: var(--fg);
    font-weight: 550;
  }
  .key {
    margin: 0.7rem 0 0;
    display: grid;
    gap: 0.3rem;
  }
  .key div {
    display: flex;
    align-items: flex-start;
    gap: 0.55rem;
  }
  .key dt,
  .key dd {
    margin: 0;
  }
  .key dt {
    flex: none;
    width: 1rem;
    padding-top: 0.2em;
  }
  .errbox code {
    color: var(--err);
    font-family: var(--font-mono);
    font-size: 0.76rem;
    background: none;
    word-break: break-word;
  }
  .loadbar {
    position: absolute;
    left: 0;
    top: 0;
    height: 2px;
    width: 40%;
    background: var(--accent);
    animation: slide 0.9s ease-in-out 0.25s infinite alternate backwards;
    opacity: 0.7;
  }
  @keyframes slide {
    from {
      left: 0;
    }
    to {
      left: 60%;
    }
  }
  .runbar {
    position: relative;
    flex: none;
    border-top: 1px solid var(--border);
    background: var(--bg-elev);
    padding: 0.2rem 0.5rem 0.35rem;
  }
  .runbar.still {
    padding: 0.5rem 0.7rem 0.6rem;
    font-size: 0.8rem;
  }
  .still p {
    margin: 0 0 0.35rem;
  }
  .idea {
    background: none;
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    padding: 0.2rem 0.55rem;
    font: inherit;
    color: var(--fg);
    cursor: pointer;
  }
  .idea:hover {
    border-color: var(--accent);
  }
  .idea code {
    font-family: var(--font-mono);
    font-size: 0.76rem;
    background: none;
    border: none;
    padding: 0;
    color: var(--hl-fn);
  }
  .transport {
    display: flex;
    align-items: center;
    flex-wrap: wrap;
    gap: 0.15rem;
    margin-top: 0.2rem;
  }
  .tb {
    min-width: 1.6rem;
    height: 1.5rem;
    padding: 0 0.25rem;
    background: none;
    border: none;
    border-radius: 5px;
    color: var(--fg-muted);
    font-size: 0.72rem;
    cursor: pointer;
  }
  @media (pointer: coarse) {
    .ic {
      width: 2.4rem;
      height: 2.4rem;
    }
    .phase,
    .lookchip {
      min-height: 2.2rem;
    }
    .tb {
      min-width: 2.5rem;
      height: 2.4rem;
      font-size: 0.85rem;
    }
  }
  .tb:hover {
    background: var(--bg-panel-hover);
    color: var(--fg);
  }
  .tb.play {
    background: var(--accent);
    color: var(--bg);
    min-width: 2rem;
  }
  .tb.word {
    font-size: 0.7rem;
    border: 1px solid var(--border);
    margin: 0 0.05rem;
  }
  .speed {
    width: 4.2rem;
    height: 0.9rem;
    margin: 0 0.3rem;
    accent-color: var(--accent);
  }
  .now {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    gap: 0.15rem 0.6rem;
    margin: 0.15rem 0 0.3rem;
    font-size: 0.76rem;
    /* room held for a three-line caption, so a shorter one doesn't nudge the tree */
    min-height: 5em;
  }
  .rewrite {
    flex: 1 1 100%;
    border: none;
    padding: 0;
    background: none;
    font-family: var(--font-mono);
    font-size: 0.84rem;
    color: var(--fg);
    overflow-wrap: anywhere;
    display: -webkit-box;
    -webkit-box-orient: vertical;
    -webkit-line-clamp: 2;
    line-clamp: 2;
    overflow: hidden;
  }
  .rewrite b {
    font-weight: 700;
  }
  .why {
    color: var(--fg-muted);
  }
  .reads {
    color: var(--fg);
    font-family: var(--font-mono);
    font-weight: 550;
  }
  .dim {
    color: var(--fg-faint);
    font-weight: 400;
  }
  .pass {
    flex: none;
    color: var(--ok);
    font-weight: 700;
  }
  .fail {
    flex: none;
    color: var(--err);
    font-weight: 700;
  }
  .phase {
    margin-left: 0.3rem;
    padding: 0.08rem 0.45rem;
    background: none;
    border: 1px solid transparent;
    border-radius: 999px;
    font: inherit;
    font-size: 0.72rem;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .phase span {
    font-family: var(--font-mono);
    font-size: 0.7rem;
  }
  .phase:hover {
    color: var(--fg);
  }
  .phase.on {
    border-color: var(--border-strong);
    color: var(--fg);
    font-weight: 550;
  }
  .size {
    margin-left: auto;
    min-width: 6.5em;
    text-align: right;
    font-variant-numeric: tabular-nums;
    padding-right: 0.3rem;
    font-size: 0.7rem;
    color: var(--fg-faint);
    white-space: nowrap;
  }
  .tb.on {
    color: var(--accent);
  }
  .options {
    position: absolute;
    right: 0.5rem;
    bottom: calc(100% + 0.3rem);
    z-index: 3;
    display: grid;
    gap: 0.35rem;
    width: min(21rem, calc(100% - 1rem));
    padding: 0.55rem 0.65rem;
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    box-shadow: 0 6px 22px color-mix(in oklab, var(--fg) 16%, transparent);
    font-size: 0.74rem;
  }
  .options label {
    display: flex;
    align-items: center;
    gap: 0.35em;
    cursor: pointer;
  }
  .options input[type='checkbox'] {
    accent-color: var(--accent);
    margin: 0;
  }
  .tally {
    font-size: 0.72rem;
  }
  .tally b {
    margin-left: 0.45em;
    margin-right: 0.15em;
    font-weight: 600;
  }
  .r-k {
    color: var(--warn);
  }
  .r-s {
    color: var(--hl-kw);
  }
  .r-f {
    color: var(--err);
  }
  .r-b {
    color: var(--fg-faint);
  }
  .verdict {
    margin-top: 0.2rem;
    font-size: 0.74rem;
  }
  .verdict.ok {
    color: var(--ok);
  }
  .verdict.warn {
    color: var(--warn);
  }
  .verdict.err {
    color: var(--err);
  }
  .calls {
    position: relative;
    flex: 0 0 26%;
    min-height: 3rem;
    display: flex;
    flex-direction: column;
    border-top: 1px solid var(--border);
    background: var(--bg-elev);
  }
  .chev {
    display: inline-block;
    font-size: 0.65rem;
    transition: transform 0.12s ease;
  }
  .chev.shut {
    transform: rotate(-90deg);
  }
  .allcalls {
    align-self: flex-end;
    padding: 0.15rem 0.5rem 0;
    display: inline-flex;
    align-items: center;
    gap: 0.25em;
    font-size: 0.68rem;
    color: var(--fg-faint);
    cursor: pointer;
    z-index: 1;
  }
  .allcalls input {
    margin: 0;
    accent-color: var(--accent);
  }
  .nocalls {
    margin: 0;
    padding: 0.2rem 0.6rem 0.5rem;
    font-size: 0.74rem;
    color: var(--fg-faint);
  }
  .calllist {
    flex: 1;
    min-height: 0;
    overflow: auto;
    scrollbar-width: thin;
  }
  .call {
    position: absolute;
    left: 0;
    right: 0;
    height: 22px;
    display: flex;
    align-items: center;
    gap: 0.2rem;
    padding-right: 0.5rem;
    font-family: var(--font-mono);
    font-size: 0.72rem;
    white-space: nowrap;
  }
  .call:hover {
    background: var(--bg-panel-hover);
  }
  .call.active {
    background: color-mix(in oklab, var(--accent) 16%, transparent);
  }
  .fold,
  .foldgap {
    flex: none;
    width: 1rem;
    text-align: center;
  }
  .fold {
    background: none;
    border: none;
    padding: 0;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .callbtn {
    flex: 1;
    min-width: 0;
    display: flex;
    align-items: baseline;
    gap: 0.35em;
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    color: var(--fg);
    cursor: pointer;
    text-align: left;
    overflow: hidden;
  }
  .callname {
    color: var(--hl-fn);
    overflow: hidden;
    text-overflow: ellipsis;
    flex: 0 1 auto;
    min-width: 2ch;
  }
  .arrow {
    color: var(--fg-faint);
    flex: none;
  }
  .callres {
    color: var(--hl-number);
    overflow: hidden;
    text-overflow: ellipsis;
    flex: 1 1 0;
    min-width: 2ch;
  }
  .callcost {
    flex: none;
    color: var(--fg-faint);
    font-size: 0.66rem;
  }
</style>
