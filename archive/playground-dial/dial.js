// The viewer: lays out a frame, plays a beat with the renderer its dial setting picks, and draws the dial.

const X = 36
const Y = 60
const COLORS = { S: '#7a4fc0', K: '#1d8a6c', I: '#c48600' }
const LEVEL_COLOR = ['#a7b1ab', '#8a6bd1', '#2a7fd4', '#d48a2a']
const ATOM_TINT = { f: '#f3d9ec', g: '#d9e8f7', h: '#e6f2d6', a: '#fde3c8' }
const reduced = matchMedia('(prefers-reduced-motion: reduce)').matches

const svg = document.getElementById('stage')
const ring = document.getElementById('ring')
const suggest = document.getElementById('suggest')
const $ = (id) => document.getElementById(id)

let ex, exKey, trace, beats
let levels = {}
/// what Shift adds to every dial; kept apart so turning everything down and back up returns to where you were
let offset = 0
/// a one-time finer look at one beat: { def, level, until }
let once = null
const dismissed = new Set()
let kinds = {}
let cur = 0 // the frame on stage
let anim = null
let hoverDef = null
let hoverUid = null
let pinned = null
let playing = false
const replays = new Map()
let lastForward = []
/// the beat that just played, so a dial turned right after it can show it again at the new setting
let lastPlayed = null
const beatsIn = (r) => beats.filter((b) => b.to > r.from && b.from < r.to).map((b) => [b.from, b.to, b.level])
/// definitions the reader set by hand: the viewer doesn't second-guess them
const handSet = new Set()
/// set once Shift is used: the reader has said how much they want everywhere, so skimming isn't a hint any more
let shifted = false
/// whether the last → cut the beat before short
let raced = null
/// when the beat now playing started, how long it takes in all, and whose it is
let beatClock = { t0: 0, total: 1, owner: null }
/// between a beat's own work and what rides along: nothing new is marked yet
let holding = false
let pendingTarget = null

const isYours = (name) => kinds[name] === 'yours'
const levelOf = (def) => {
  if (once && once.def === def) return once.level
  // a constant is quiet or shown rule by rule, and Shift (which moves code) leaves it be
  if (kinds[def] === 'constant') {
    if (handSet.has(def)) return (levels[def] ?? 0) >= 2 ? 3 : 0
    // one Shift notch past "all code at Rules" shows constants' rules too
    const bases = ex.defs.filter((d) => d.kind !== 'constant' && d.name !== 'main' && !handSet.has(d.name)).map((d) => levels[d.name])
    if (bases.length && offset > 3 - Math.min(...bases)) return 3
    return (levels[def] ?? 0) >= 2 ? 3 : 0
  }
  // a dial you set by hand stays where you put it; Shift moves the rest
  return Math.max(0, Math.min(3, (levels[def] ?? 2) + (handSet.has(def) ? 0 : offset)))
}
const ease = (t) => 1 - Math.pow(1 - t, 3)
const clamp = (t) => Math.max(0, Math.min(1, t))
const lerp = (a, b, t) => a + (b - a) * t

// ---------- layout ----------

function textWidth(text) {
  return Math.max(1, text.length * 0.3 + 0.7)
}

function layout(term) {
  const items = []
  const pos = new Map()
  const hidden = new Map()
  const width = new Map()
  /// a folded piece of code: hide its own nodes, and hand back what it holds that someone else built
  /// a folded call: hide its own nodes, and hand back what it holds that is other code's (its inputs).
  /// Constants written in its own text are its own; other named definitions are not.
  const fold = (t, pod, out, path, me, spine = true) => {
    if (t.uid !== pod) hidden.set(t.uid, pod)
    if (t.kind !== 'app') return out
    ;[t.f, t.a].forEach((c, i) => {
      // the definition itself, still whole, holds nothing of anyone else's
      if (c.label === me && !(spine && i === 1)) {
        hideAll(c, pod)
        out.whole = true
      }
      else if (mine(c, me, spine && i === 1) && !(startsCall(c) && !(spine && i === 0))) fold(c, pod, out, [...path, i], me, spine && i === 0)
      else out.push([c, [...path, i], spine && i === 1])
    })
    return out
  }
  const hideAll = (t, pod) => {
    hidden.set(t.uid, pod)
    if (t.kind === 'app') {
      hideAll(t.f, pod)
      hideAll(t.a, pod)
    }
  }
  const walk = (t, parent, side, depth, path = []) => {
    // code whose dial is at Result or Skip folds into one box named for it, holding its inputs
    const me = codeOf(t)
    const top = !parent || parent.def !== me || (startsCall(t) && parent.kind === 'pod')
    // a box is code at work (or a definition itself); a finished value is shown as what it is
    const closed = top && me !== 'main' && levelOf(me) <= 1 && (t.label === me || hasRedex(t) || waiting(t))
    const source = top && me !== 'main' && levelOf(me) === 2 && bigCode(me) && (t.label === me || hasRedex(t) || waiting(t))
    const it = { uid: t.uid, def: me, parent, side, depth, path, kids: [] }
    items.push(it)
    if (source) {
      // the body as it was written, with what has been handed in so far and a hole for each parameter still to come
      // arguments already taken fill their holes; the ones still waiting on the call hang below, as for any call
      const all = t.label === me ? [] : fold(t, t.uid, [], path, me)
      const ownCall = trace.calls.find((c) => c.name === me && c.start <= cur && cur < c.end)
      const isArg = (c) => !ownCall || ownCall.args.some((a) => inFamily(c.uid, a))
      const taken = all.filter(([c, , slot]) => !slot || !isArg(c))
      const waitingArgs = all.filter(([c, , slot]) => slot && isArg(c))
      for (const [c] of taken) hideAll(c, t.uid)
      if (t.label === me) hideAll(t, t.uid)
      Object.assign(it, { kind: 'pod', text: holesText(me, taken.map(([c]) => c)), source: true })
      it.kids = waitingArgs.map(([c, p]) => walk(c, it, 1, depth + 1, p))
      width.set(t.uid, Math.max(textWidth(it.text) + 0.4, it.kids.reduce((w, k) => w + width.get(k.uid), 0)))
    } else if (closed) {
      // a box holds its inputs below it; "…" marks a call partway through, whose own code isn't whole any more
      const all = fold(t, t.uid, [], path, me)
      // copies S made of one argument are one input to the reader: show it once
      const seen = new Set()
      const held = all.filter(([c]) => {
        let root = c.uid
        for (let u = originOf.get(root); u !== undefined; u = originOf.get(u)) root = u
        if (seen.has(root)) {
          hideAll(c, t.uid)
          return false
        }
        seen.add(root)
        return true
      })
      held.whole = all.whole
      const partial = t.label !== me && !held.whole
      Object.assign(it, { kind: 'pod', text: partial ? `${me}…` : me, partial })
      it.kids = held.map(([c, p]) => walk(c, it, 1, depth + 1, p))
      width.set(t.uid, Math.max(textWidth(it.text) + 0.4, it.kids.reduce((w, k) => w + width.get(k.uid), 0)))
    } else if (t.kind === 'atom') {
      Object.assign(it, { kind: 'atom', text: t.name })
      width.set(t.uid, textWidth(t.name))
    } else if (t.kind !== 'app') {
      Object.assign(it, { kind: t.kind, text: t.kind, tag: top && me !== 'main' ? nameTag(t, me) : undefined })
      width.set(t.uid, 1)
    } else {
      // open code is named at its top, with "…" once a call has started taking it apart
      Object.assign(it, { kind: 'app', tag: top && me !== 'main' ? nameTag(t, me) : undefined })
      it.kids = [walk(t.f, it, 0, depth + 1, [...path, 0]), walk(t.a, it, 1, depth + 1, [...path, 1])]
      width.set(t.uid, it.kids.reduce((w, k) => w + width.get(k.uid), 0))
    }
    return it
  }
  const root = walk(term, null, 0, 0)
  const place = (it, left) => {
    it.y = it.depth * Y
    if (!it.kids.length) it.x = (left + width.get(it.uid) / 2) * X
    else {
      let l = left
      for (const k of it.kids) {
        place(k, l)
        l += width.get(k.uid)
      }
      it.x = (it.kids[0].x + it.kids[it.kids.length - 1].x) / 2
    }
    pos.set(it.uid, { x: it.x, y: it.y })
  }
  place(root, 0)
  let minX = Infinity
  let maxX = -Infinity
  let maxY = 0
  for (const it of items) {
    minX = Math.min(minX, it.x)
    maxX = Math.max(maxX, it.x)
    maxY = Math.max(maxY, it.y)
  }
  return { items, pos, hidden, bounds: { minX, maxX, maxY } }
}

/// where a node of the later frame was in the earlier one: itself, the pod it was folded in, or what it was copied from
function lookIn(L, uid) {
  for (let u = uid; u !== undefined; u = originOf.get(u)) {
    if (L.pos.has(u)) return L.pos.get(u)
    if (L.hidden.has(u)) return L.pos.get(L.hidden.get(u))
  }
  return null
}

// ---------- beats ----------

const frameAt = (b) => (b < 0 ? 0 : beats[b].to)
/// the beat that starts at or runs across the frame on stage
const beatFrom = (f) => beats.findIndex((b) => b.to > f)


/// code too big to follow as S/K/I at Flow: more than two parameters, or a sizeable scaffold
function bigCode(name) {
  return combinators(name) > BIG_SCAFFOLD
}
/// past this many S/K/I, a hand-off's route lights too much to follow: Flow draws the source with holes instead
const BIG_SCAFFOLD = 12
const scaffoldSize = new Map()
function combinators(name) {
  if (!scaffoldSize.has(name)) {
    const find = (t) => (t.label === name ? t : t.kind === 'app' ? (find(t.f) ?? find(t.a)) : null)
    const def = find(trace.frames[0])
    const count = (t) => (t.kind === 'app' ? count(t.f) + count(t.a) : ARITY[t.kind] && t.def === name ? 1 : 0)
    scaffoldSize.set(name, def ? count(def) : 0)
  }
  return scaffoldSize.get(name)
}

/// `rev`'s body with the arguments handed in so far, and calls in it that have finished replaced by their results:
/// mix: f(false, ◌c◌, p, g(◌c◌)). Empty holes are marked ◌name◌ and drawn faded.
function holesText(name, held) {
  const src = sourceOf(name)
  const call = trace.calls.find((c) => c.name === name && c.start <= cur && cur < c.end) ?? trace.calls.find((c) => c.name === name)
  const isIn = (k) => call && (held.some((c) => inFamily(c.uid, call.args[k])) || (cur >= call.start && trace.steps.slice(call.start, cur).some((st) => inFamily(st.arg, call.args[k]))))
  const argText = (k) => said(findIn(trace.frames[call.start], call.args[k]) ?? { kind: 'atom', name: '?' })
  const render = (e) => {
    if (e.name !== undefined) {
      const k = src.params.indexOf(e.name)
      if (k < 0) return e.name
      return isIn(k) ? argText(k) : `◌${e.name}◌`
    }
    const args = []
    let f = e
    while (f.name === undefined) {
      args.unshift(f.x)
      f = f.f
    }
    const parts = args.map(render)
    // a call of another definition in the body that has already finished reads as its result
    const done = call && trace.calls.find((c) => c.name === f.name && c.start >= call.start && c.end <= cur && c.parent === call.id && args.every((x) => x.name !== undefined && src.params.includes(x.name) && c.args.some((u) => inFamily(u, call.args[src.params.indexOf(x.name)]))))
    if (done) return said(at(trace.frames[done.end], done.path))
    return `${render(f)}(${parts.join(', ')})`
  }
  return `${name}: ${render(parseExpr(src.body))}`
}

/// an application of a whole named definition: a call of its own, even of the same function as the box around it
function startsCall(c) {
  if (c.kind !== 'app') return false
  for (let f = c.f; ; f = f.f) {
    if (f.label) return true
    if (f.kind !== 'app') return false
  }
}

/// is c part of the code `me` (rather than an input to it)? Its own code is; a constant written in its text is,
/// unless it sits where the call's arguments go
function mine(c, me, argSlot) {
  // a whole named value where an argument goes is passed in, even when it is the same definition (`true(x, true)`)
  if (argSlot && c.label) return false
  const d = codeOf(c)
  return d === me || (!argSlot && kinds[d] === 'constant' && !!c.homes?.includes(me))
}

const code = (s) => `<code>${s.replace(/</g, '&lt;')}</code>`

/// a term as text, the way it is drawn: folded code reads as its name holding its inputs, so a half-applied
/// `and` is `and(true)` and the same thing applied once more is `and(true, false)`
function said(t, parentDef = null, depth = 0) {
  if (t.label) return t.label
  if (depth > 3) return '…'
  const me = codeOf(t)
  if (t.kind === 'app' && me !== parentDef && me !== 'main' && levelOf(me) <= 1) {
    const held = []
    const walk = (u, spine) => {
      if (u.kind !== 'app') return
      ;[u.f, u.a].forEach((c, i) => {
        if (c.label === me && !(spine && i === 1)) return
        if (mine(c, me, spine && i === 1) && !(startsCall(c) && !(spine && i === 0))) walk(c, spine && i === 0)
        else held.push(c)
      })
    }
    walk(t, true)
    return held.length ? `${me}(${held.map((c) => said(c, me, depth + 1)).join(', ')})` : me
  }
  if (t.kind === 'atom') return t.name
  if (t.kind !== 'app') return t.kind
  const args = []
  let head = t
  while (head.kind === 'app' && !(head !== t && (head.label || (codeOf(head) !== me && levelOf(codeOf(head)) <= 1)))) {
    args.unshift(head.a)
    head = head.f
  }
  const h = said(head, me, depth)
  const inner = args.map((a) => said(a, me, depth + 1)).join(', ')
  return h.endsWith(')') && !h.startsWith('(') ? `${h.slice(0, -1)}, ${inner})` : `${h}(${inner})`
}

/// where a beat's own work ends and what rides along begins
function splitOf(b, from = b.from) {
  if (b.level === 2 && b.pick) {
    let end = from
    for (let j = from; j < b.to; j++) {
      const st = trace.steps[j]
      if (st.call === b.call && st.rule !== 'K' && b.pick.args.some((a) => inFamily(st.arg, a))) end = j + 1
    }
    // only copies being thrown away after it: they settle with the hand-off, in one move
    if (trace.steps.slice(end, b.to).every((st) => st.call === b.call && st.rule === 'K')) return b.to
    return Math.max(end, from + 1)
  }
  if (b.level === 1 && b.pick) return Math.max(from + 1, Math.min(b.to, b.pick.end))
  // a rule beat is its one step; anything after it rides along
  if (b.level === 3) return from + 1
  return b.to
}

/// a call as text, as it stood once all its arguments were in place
function callText(c) {
  const f = trace.frames[c.start]
  let p = c.path
  while (p.length && p[p.length - 1] === 0 && at(f, p.slice(0, -1)).kind === 'app') p = p.slice(0, -1)
  return said(at(f, p))
}

/// "a", "a and b", "a, b and c"
const listed = (xs) => (xs.length < 2 ? xs.join('') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`)

const callOf = (b) => (b.call !== null ? trace.calls[b.call] : null)

function describe(b, from) {
  const s = trace.steps[from]
  const n = b.to - from
  const start = trace.frames[from]
  let text
  if (b.level === 3) {
    if (s.rule === 'S') text = `S hands ${code(s.argText)} to both sides: to ${code(s.parts[0])} and to ${code(s.parts[1])}.`
    else if (s.rule === 'K') text = `K keeps ${code(s.parts[0])} and drops the argument after it, ${code(s.argText)}.`
    else text = `I passes ${code(s.argText)} straight through.`
  } else if (b.level === 2) {
    const c = callOf(b)
    const src = c && sourceOf(c.name)
    const first = trace.steps.slice(from, b.to).find((st) => st.call === b.call) ?? s
    const k = c ? c.args.findIndex((a) => inFamily(first.arg, a)) : -1
    if (src && k >= 0 && src.params[k]) {
      // read it the way it was written: the argument takes its parameter's place in the body
      const argTexts = c.args.map((a) => said(findIn(trace.frames[c.start], a) ?? { kind: 'atom', name: '?' }))
      // big code is drawn as its source with holes, so its caption uses the same holes
      const body = bigCode(b.owner) ? boxTextAt(b.owner, b.to) : src.params.reduce((t, p, j) => t.replace(new RegExp(`\\b${p}\\b`, 'g'), j <= k ? argTexts[j] : p), src.body)
      const later = src.params.slice(k + 1)
      text = `${code(argTexts[k])} is handed to ${code(b.owner)}, taking ${code(src.params[k])}'s place: ${code(body)}${later.length && !bigCode(b.owner) ? `, waiting for ${listed(later.map(code))}` : ''}.`
    } else {
      const { landed } = handoff(b, from)
      text = `${code(first.argText)} is handed to ${code(b.owner)}${landed.length ? '' : `, which doesn't keep it`}.`
    }
  } else {
    const c = callOf(b)
    const P = c ? c.path : []
    const before = said(at(start, P))
    if (b.level === 0) text = `${code(before)} runs quietly: ${code(said(at(trace.frames[Math.min(b.to, c?.end ?? b.to)], P)))}.`
    else {
    // a call cut into pieces by a call inside it that you're watching more closely
    const next = beats.find((x) => x.from >= b.to)
    if (c && c.end > b.to && next) text = `${code(before)} runs until ${code(said(at(trace.frames[next.from], next.pick?.path ?? [])))} starts, which you're watching more closely.`
    else if (c && c.start < from) text = `${code(c.name)} finishes: ${code(said(at(trace.frames[b.to], P)))}.`
    else text = `${code(before)} gives ${code(said(at(trace.frames[Math.min(b.to, c?.end ?? b.to)], P)))}.`
    }
  }
  const tag = `<span class="lvl" style="background:${LEVEL_COLOR[b.level]}">${LEVELS[b.level]} · ${b.owner === 'main' ? 'the run' : b.owner}</span>`
  const count = n > 1 ? ` <span style="color:var(--muted)">${n} rule steps in one beat.</span>` : ''
  const end = splitOf(b, from)
  const callOfStep = (r) => trace.calls[trace.steps[r].call]
  // the outermost calls that start inside the riding part: each reads as a call
  const ridingCalls = trace.calls.filter((c) => c.start >= end && c.start < b.to && c.name !== b.owner && !(c.parent !== null && trace.calls[c.parent].start >= end && trace.calls[c.parent].start < b.to))
  const inCalls = (r) => ridingCalls.some((c) => r >= c.start && r < c.end)
  const quietIn = [...new Set(b.riders.filter((r) => r >= from && r < end).map(callOfStep).filter((c) => c && c.name !== b.owner && levelOf(c.name) === 0).map((c) => c.name))]
  const drops = b.riders.filter((r) => r >= end && !inCalls(r) && trace.steps[r].rule === 'K').map((r) => trace.steps[r])
  const notes = [
    ...(quietIn.length ? [`${quietIn.map(code).join(', ')} work${quietIn.length > 1 ? '' : 's'} quietly in it`] : []),
    ...ridingCalls.map((c) => `then ${code(callText(c))} gives ${code(said(at(trace.frames[c.end], c.path)))}, quietly`),
    ...drops.map((d) => `then ${code(trace.calls[d.call]?.name ?? 'the run')} throws away a copy of ${code(d.argText)}`),
  ]
  const counted = []
  for (const n of notes) {
    const same = counted.find((x) => x.n === n)
    if (same) same.k++
    else counted.push({ n, k: 1 })
  }
  notes.length = 0
  for (const { n, k } of counted) notes.push(k > 1 ? n.replace('a copy of', `${k} copies of`) : n)
  const riders = notes.length ? ` <span style="color:var(--muted)">(${notes.join('; ')}.)</span>` : ''
  return tag + text + count + riders
}

/// a source box's text as it stands at a frame, faded holes shown as their names
function boxTextAt(name, frame) {
  const keep = cur
  cur = frame
  const L = layout(trace.frames[frame])
  const box = L.items.find((it) => it.source && it.def === name)
  cur = keep
  if (box) return box.text.slice(box.text.indexOf(':') + 2).replace(/◌/g, '')
  // the call has finished: its body, all filled in, is what's there
  const c = trace.calls.find((x) => x.name === name && x.end <= frame && x.end > frame - 1)
  return c ? said(at(trace.frames[frame], c.path)) : sourceOf(name).body
}

/// a definition's parameters and body, read off its source line
function sourceOf(name) {
  const d = ex.defs.find((x) => x.name === name)
  const m = d?.src.match(/^\s*\w+\s*:=\s*\{([^}]*)\}\s*=>\s*(.*)$/)
  return m ? { params: m[1].split(',').map((x) => x.trim()), body: m[2].trim() } : null
}

const findIn = (t, uid) => (t.uid === uid ? t : t.kind === 'app' ? (findIn(t.f, uid) ?? findIn(t.a, uid)) : null)

/// where a landed copy ended up: as an argument of what
function landingSpot(term, uid) {
  const find = (t) => {
    if (t.kind !== 'app') return null
    if (t.a.uid === uid) {
      const owner = codeOf(t)
      if (owner !== 'main' && levelOf(owner) <= 1) return `${owner}(…)`
      let h = t.f
      while (h.kind === 'app' && !h.label) h = h.f
      // inside open S/K/I code a place has no name worth giving
      return ARITY[h.kind] ? null : `${said(h)}(…)`
    }
    return find(t.f) ?? find(t.a)
  }
  return find(term)
}

function handoff(b, from) {
  const alive = new Set([b.arg])
  for (let i = from; i < b.to; i++) {
    const s = trace.steps[i]
    if (!alive.has(s.arg)) continue
    if (s.rule === 'S') alive.add(s.clone[1])
    if (s.rule === 'K') alive.delete(s.arg)
  }
  return { landed: [...alive] }
}

// ---------- transitions ----------

/// what playing frames a..b looks like: the renderer comes from the beat's dial setting
function transition(a, b, beat, mode, A = layout(trace.frames[a])) {
  const B = layout(trace.frames[b])
  const from = new Map()
  const fresh = new Set()
  const emerging = new Set()
  const changed = new Set()
  const Aby = new Map(A.items.map((i) => [i.uid, i]))
  const Bby = new Map(B.items.map((i) => [i.uid, i]))
  // a box is the definition it holds: when a call gathers its arguments, the box glides on as the call's box
  const continues = new Map()
  for (const it of A.items) {
    const into = it.kind === 'pod' ? B.hidden.get(it.uid) : undefined
    const box = into !== undefined ? Bby.get(into) : undefined
    if (box?.kind === 'pod' && box.def === it.def && !continues.has(into)) continues.set(into, it.uid)
    // a source box goes on being itself as its holes fill
    if (it.source && Bby.get(it.uid)?.source) changed.delete?.(it.uid)
  }
  const carried = new Set(continues.values())
  for (const it of B.items) {
    if (continues.has(it.uid)) {
      from.set(it.uid, A.pos.get(continues.get(it.uid)))
      continue
    }
    const was = Aby.get(it.uid)
    const p = lookIn(A, it.uid)
    // the same node now reading as something else (a box becoming a call under way) cross-fades where it stands
    if (was && (was.kind !== it.kind || (was.text !== it.text && !(was.source && it.source)))) {
      changed.add(it.uid)
      fresh.add(it.uid)
      from.set(it.uid, A.pos.get(it.uid))
    } else if (p) {
      from.set(it.uid, p)
      if (!A.pos.has(it.uid)) emerging.add(it.uid)
    } else {
      fresh.add(it.uid)
      from.set(it.uid, it.parent ? from.get(it.parent.uid) : { x: it.x, y: it.y })
    }
  }
  const dropped = new Set()
  for (let i = a; i < b; i++) for (const u of trace.steps[i].droppedAll) dropped.add(u)
  const ghosts = []
  for (const it of A.items) {
    if (carried.has(it.uid)) continue
    if (B.pos.has(it.uid) && !changed.has(it.uid)) continue
    const into = B.hidden.has(it.uid) ? B.pos.get(B.hidden.get(it.uid)) : null
    ghosts.push({ it, into, drop: dropped.has(it.uid) })
  }
  const T = { A, B, from, fresh, emerging, ghosts, mode, beat, a, b, continues, changed, t0: performance.now() }
  // a source box keeps its old text until the argument reaches its hole
  T.oldText = new Map(B.items.filter((it) => it.source).map((it) => [it.uid, Aby.get(continues.get(it.uid) ?? it.uid)?.text]).filter(([, t]) => t))
  // a Result beat keeps its call outlined while it plays, fading as the result arrives
  if (mode === 'morph' && beat?.level === 1) {
    const region = beatRegion(beat)
    const under = A.items.filter((it) => it.path.length >= region.length && region.every((x, i) => it.path[i] === x))
    if (under.length)
      T.hull = { x0: Math.min(...under.map((i) => i.x)), x1: Math.max(...under.map((i) => i.x)), y0: Math.min(...under.map((i) => i.y)), y1: Math.max(...under.map((i) => i.y)) }
  }
  T.dur = reduced ? 1 : { rule: 650, morph: 900, flow: 1800, restructure: 450 }[mode]
  if (mode === 'flow') flowPaths(T)
  return T
}

/// the drawn node standing for uid: itself, the pod it is folded into, or what it was copied from
function anchorIn(L, uid) {
  for (let u = uid; u !== undefined; u = originOf.get(u)) {
    if (L.pos.has(u)) return u
    if (L.hidden.has(u)) return L.hidden.get(u)
  }
  return null
}

/// the way along the tree's own edges from one node to another: up to where they meet, then down
function routeItems(L, u1, u2) {
  const byUid = L.byUid ?? (L.byUid = new Map(L.items.map((it) => [it.uid, it])))
  const up = (it) => {
    const chain = []
    for (let c = it; c; c = c.parent) chain.push(c)
    return chain
  }
  const a = up(byUid.get(u1))
  const b = up(byUid.get(u2))
  const inB = new Set(b.map((c) => c.uid))
  const meet = a.findIndex((c) => inB.has(c.uid))
  const down = b.slice(0, b.findIndex((c) => c.uid === a[meet].uid)).reverse()
  return [...a.slice(0, meet + 1), ...down]
}

function route(L, u1, u2) {
  const byUid = L.byUid ?? (L.byUid = new Map(L.items.map((it) => [it.uid, it])))
  const up = (it) => {
    const chain = []
    for (let c = it; c; c = c.parent) chain.push(c)
    return chain
  }
  const a = up(byUid.get(u1))
  const b = up(byUid.get(u2))
  const inB = new Set(b.map((c) => c.uid))
  const meet = a.findIndex((c) => inB.has(c.uid))
  const down = b.slice(0, b.findIndex((c) => c.uid === a[meet].uid)).reverse()
  return [...a.slice(0, meet + 1), ...down].map((c) => ({ x: c.x, y: c.y }))
}

/// the argument's journey: through each combinator that hands it on, splitting at S, ending at K
function flowPaths(T) {
  const { A, B, beat, a, b } = T
  const start = anchorIn(A, beat.arg)
  const paths = new Map([[beat.arg, { pts: [lookIn(A, beat.arg)], at: start, via: [] }]])
  const died = new Set()
  T.heads = []
  for (let i = a; i < b; i++) {
    const s = trace.steps[i]
    const path = paths.get(s.arg)
    if (!path) continue
    const h = anchorIn(A, s.head)
    if (h !== null && path.at !== null) {
      const way = route(A, path.at, h)
      path.via.push(...routeItems(A, path.at, h).map((it) => it.uid))
      path.pts.push(...way.slice(1))
      path.pts[path.pts.length - 1] = { ...path.pts[path.pts.length - 1], dwell: true }
      path.at = h
      T.heads.push({ uid: s.head, at: A.pos.get(h), rule: s.rule })
    }
    if (s.rule === 'S') paths.set(s.clone[1], { pts: [...path.pts], at: path.at, via: [...path.via] })
    if (s.rule === 'K') died.add(s.arg)
  }
  for (const u of paths.keys()) if (!died.has(u) && !B.pos.has(u) && !B.hidden.has(u)) died.add(u)
  T.paths = new Map([...paths].map(([u, p]) => [u, p.pts]))
  T.died = died
  // into a source box, an argument lands on its own hole rather than the middle of the box
  const box = B.items.find((it) => it.source && it.def === beat.owner)
  const call = beat.pick
  if (box && call) {
    const k = call.args.findIndex((a) => inFamily(beat.arg, a))
    const text = box.text.replace(/◌/g, '')
    const arg = said(findIn(trace.frames[call.start], call.args[k]) ?? { kind: 'atom', name: '?' })
    const filledText = box.text
    const at = filledText.replace(/◌\w+◌/g, (m) => m.slice(1, -1)).indexOf(arg, filledText.indexOf(':') + 1)
    if (at >= 0) T.hole = { x: box.x + (at + arg.length / 2 - text.length / 2) * 7.6, y: box.y }
  }
  // the plumbing that carries a copy somewhere it lands; everything else of this code is drawn faint this beat
  T.onRoute = new Set([...paths].filter(([u]) => !died.has(u)).flatMap(([, p]) => p.via))
  T.text = trace.steps[a].argText
  // what lands starts from the last combinator it passed, and everything under it comes out with it
  T.family = new Map()
  for (const it of B.items) {
    const own = T.paths.has(it.uid) && !died.has(it.uid) ? it.uid : it.parent ? T.family.get(it.parent.uid) : undefined
    if (own !== undefined) {
      T.family.set(it.uid, own)
      T.from.set(it.uid, T.paths.get(own).at(-1))
      T.fresh.delete(it.uid)
    }
  }
}

/// a point u of the way along a path, at an even pace, pausing briefly at each combinator
function along(path, u) {
  if (path.length === 1) return path[0]
  const lens = []
  let total = 0
  for (let i = 1; i < path.length; i++) {
    const l = Math.hypot(path[i].x - path[i - 1].x, path[i].y - path[i - 1].y) + (path[i].dwell ? 40 : 0)
    lens.push(l)
    total += l
  }
  let d = ease(u) * total
  for (let i = 0; i < lens.length; i++) {
    if (d <= lens[i]) {
      const move = lens[i] - (path[i + 1].dwell ? 40 : 0)
      const k = move > 0 ? clamp(d / move) : 1
      return { x: lerp(path[i].x, path[i + 1].x, k), y: lerp(path[i].y, path[i + 1].y, k) }
    }
    d -= lens[i]
  }
  return path[path.length - 1]
}

// ---------- drawing ----------

function frameScene(T, p) {
  const flow = T.mode === 'flow'
  const move = flow ? ease(clamp((p - 0.55) / 0.45)) : ease(p)
  const land = ease(clamp((p - 0.5) / 0.5))
  const nodes = []
  const where = new Map()
  for (const it of T.B.items) {
    const f = T.from.get(it.uid)
    const fam = flow && T.family.has(it.uid)
    const x = lerp(f.x, it.x, fam ? land : move)
    const y = lerp(f.y, it.y, fam ? land : move)
    let alpha = 1
    if (T.fresh.has(it.uid)) alpha = flow ? clamp((p - 0.5) / 0.3) : clamp((p - 0.3) / 0.6)
    // what comes out of a box appears as the box goes, not on top of it
    else if (T.emerging.has(it.uid) && !fam) alpha = clamp((p - 0.25) / 0.5)
    if (fam && p < 0.5) alpha = 0
    // off the routes that land, this code's own plumbing steps back while the hand-off plays
    if (flow && T.onRoute && it.def === T.beat?.owner && !T.onRoute.has(it.uid) && !fam && !T.fresh.has(it.uid)) alpha *= 1 - 0.65 * Math.min(1, p / 0.15) * (1 - clamp((p - 0.5) / 0.3))
    const text = T.oldText?.has(it.uid) && p < (flow ? 0.5 : 0.4) ? T.oldText.get(it.uid) : null
    nodes.push({ it: text ? { ...it, text } : it, x, y, alpha })
    where.set(it.uid, { x, y, alpha })
  }
  // the flying argument stands for its copies: their old selves don't linger
  const ghosts = T.ghosts.filter((g) => !(flow && T.paths && [...T.paths.keys()].some((u) => inFamily(g.it.uid, u) || inFamily(u, g.it.uid)))).map((g) => {
    let { x, y } = g.it
    let alpha
    if (g.into) {
      x = lerp(x, g.into.x, ease(p))
      y = lerp(y, g.into.y, ease(p))
      alpha = 1 - ease(p)
    } else if (g.drop) {
      y += 46 * ease(p)
      alpha = 1 - p * p
    } else alpha = flow ? 1 - clamp((p - 0.5) / 0.3) : 1 - ease(p)
    where.set(g.it.uid, where.get(g.it.uid) ?? { x, y, alpha })
    return { it: g.it, x, y, alpha }
  })
  const particles = []
  const trails = []
  if (flow) {
    const u = clamp(p / 0.5)
    const fadeTrail = 1 - clamp((p - 0.5) / 0.12)
    for (const [uid, path] of T.paths) {
      const dead = T.died.has(uid)
      const q = T.hole && u > 0.85 ? { x: lerp(along(path, 0.85).x, T.hole.x, (u - 0.85) / 0.15), y: lerp(along(path, 0.85).y, T.hole.y, (u - 0.85) / 0.15) } : along(path, u)
      // the route so far stays drawn for a moment, so the picture of where the argument went outlives the motion
      const pts = [path[0]]
      for (let i = 1; i <= 40; i++) pts.push(along(path, (u * i) / 40))
      // a copy that is only thrown away leaves a faint, thin trail: the eye follows the ones that land
      if (fadeTrail > 0) trails.push({ pts, alpha: (dead ? 0.14 : 0.55) * fadeTrail, dead })
      if (p < 0.52) {
        const shrink = dead ? 1 - clamp((u - 0.85) / 0.15) : 1
        particles.push({ x: q.x, y: q.y, alpha: shrink, s: 0.6 + 0.4 * shrink })
      }
    }
  }
  // edges cross-fade from the old shape to the new one, so no line ever spans the two
  const edgesA = []
  const edgesB = []
  const old = flow ? 1 - clamp((p - 0.5) / 0.2) : 1 - ease(p)
  const neu = flow ? clamp((p - 0.5) / 0.3) : ease(p)
  for (const it of T.A.items) {
    if (!it.parent) continue
    if (flow && T.paths.has(it.uid)) continue
    // an old edge belongs to the old picture: a node that now stands for something else is drawn where it was
    const was = (n) => (T.continues?.has(n.uid) || T.changed?.has(n.uid) ? null : where.get(n.uid))
    const c = was(it) ?? { x: it.x, y: it.y }
    const q = was(it.parent) ?? { x: it.parent.x, y: it.parent.y }
    edgesA.push({ a: q, b: c, side: it.side, alpha: old })
  }
  for (const n of nodes) {
    if (!n.it.parent) continue
    const q = where.get(n.it.parent.uid)
    edgesB.push({ a: q, b: n, side: n.it.side, alpha: Math.min(n.alpha, q.alpha) * neu })
  }
  // a combinator lights up as the argument reaches it, and stays lit until the hand-off is done
  const glow = flow
    ? T.heads.map((h) => {
        const lit = trails.some((t) => !t.dead && t.pts.some((q) => Math.hypot(q.x - h.at.x, q.y - h.at.y) < 6))
        return { ...h.at, rule: h.rule, on: lit ? 1 - clamp((p - 0.5) / 0.15) : 0 }
      })
    : []
  const hull = T.hull ? { ...T.hull, alpha: 1 - ease(p) } : null
  return { nodes, ghosts, particles, trails, edgesA, edgesB, glow, text: T.text, hull }
}

function staticScene() {
  const L = layout(trace.frames[cur])
  const nodes = L.items.map((it) => ({ it, x: it.x, y: it.y, alpha: 1 }))
  const where = new Map(nodes.map((n) => [n.it.uid, n]))
  const edgesB = nodes.filter((n) => n.it.parent).map((n) => ({ a: where.get(n.it.parent.uid), b: n, side: n.it.side, alpha: 1 }))
  return { L, nodes, ghosts: [], particles: [], edgesA: [], edgesB, glow: [] }
}

let view = null

function fit(bounds) {
  const w = svg.clientWidth
  const h = svg.clientHeight
  const bw = bounds.maxX - bounds.minX + 4 * X
  const bh = bounds.maxY + 3 * Y
  const k = Math.min(1.6, w / bw, h / bh)
  return { k, x: w / 2 - ((bounds.minX + bounds.maxX) / 2) * k, y: Math.max(40, (h - bounds.maxY * k) / 2 - 10) }
}

function render(scene, cam) {
  const out = []
  const tf = (p) => `translate(${(cam.x + p.x * cam.k).toFixed(1)},${(cam.y + p.y * cam.k).toFixed(1)}) scale(${cam.k.toFixed(3)})`
  const line = (e, color) =>
    e.alpha > 0.01 &&
    out.push(
      `<line x1="${cam.x + e.a.x * cam.k}" y1="${cam.y + e.a.y * cam.k}" x2="${cam.x + e.b.x * cam.k}" y2="${cam.y + e.b.y * cam.k}" stroke="${color}" stroke-width="${1.6 * cam.k}" ${e.side ? `stroke-dasharray="${4 * cam.k} ${3 * cam.k}"` : ''} opacity="${e.alpha.toFixed(2)}"/>`,
    )
  for (const e of scene.edgesA) line(e, '#9aa79f')
  for (const e of scene.edgesB) line(e, '#7d8b83')
  for (const t of scene.trails ?? [])
    out.push(
      `<polyline points="${t.pts.map((q) => `${(cam.x + q.x * cam.k).toFixed(1)},${(cam.y + q.y * cam.k).toFixed(1)}`).join(' ')}" fill="none" stroke="#e0a42a" stroke-width="${(t.dead ? 2.5 : 5) * cam.k}" stroke-linecap="round" stroke-linejoin="round" opacity="${t.alpha.toFixed(2)}"/>`,
    )
  for (const g of scene.glow)
    if (g.on > 0.01) out.push(`<g transform="${tf(g)}" opacity="${g.on.toFixed(2)}"><circle r="17" fill="none" stroke="${COLORS[g.rule]}" stroke-width="3.5"/></g>`)
  const next = anim || holding ? null : nextMarks()
  const hull = anim ? scene.hull : callHull(scene)
  if (hull && (hull.alpha ?? 1) > 0.02)
    out.push(
      `<rect x="${cam.x + hull.x0 * cam.k - 22}" y="${cam.y + hull.y0 * cam.k - 20}" width="${(hull.x1 - hull.x0) * cam.k + 44}" height="${(hull.y1 - hull.y0) * cam.k + 40}" rx="18" fill="#8a6bd1" fill-opacity="0.08" stroke="#8a6bd1" stroke-opacity="0.35" stroke-dasharray="5 4" opacity="${(hull.alpha ?? 1).toFixed(2)}"/>`,
    )
  for (const g of scene.ghosts) if (g.alpha > 0.01) out.push(`<g transform="${tf(g)}" opacity="${g.alpha.toFixed(2)}">${shape(g.it, false)}</g>`)
  for (const n of scene.nodes) {
    if (n.alpha <= 0.01) continue
    const hot = hoverDef && n.it.def === hoverDef
    const mark = next?.has(n.it.uid)
    out.push(
      `<g class="node" data-uid="${n.it.uid}" transform="${tf(n)}" opacity="${n.alpha.toFixed(2)}">${hot ? `<circle r="${n.it.kind === 'app' ? 9 : 18}" fill="#f5c84c" opacity="0.28"/>` : ''}${mark ? `<circle r="${n.it.kind === 'pod' || n.it.kind === 'atom' ? 22 : 17}" fill="none" stroke="#e0a42a" stroke-width="2.5"/>` : ''}${shape(n.it, hot)}</g>`,
    )
  }
  for (const q of scene.particles)
    out.push(`<g transform="${tf(q)}" opacity="${q.alpha.toFixed(2)}"><g transform="scale(${q.s})">${pill(scene.text, '#fff6dc', '#e0a42a', 2.4)}</g></g>`)
  svg.innerHTML = out.join('')
}

function pill(text, fill, stroke, sw = 1.2) {
  const w = Math.max(24, text.length * 7.6 + 14)
  return `<rect x="${-w / 2}" y="-11" width="${w}" height="22" rx="11" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"/><text text-anchor="middle" dy="4.5" font-size="12.5" font-family="ui-monospace,monospace" fill="#22302a">${text}</text>`
}

function shape(it, hot) {
  if (it.kind === 'app') return `<circle r="4" fill="#fff" stroke="#7d8b83" stroke-width="1.4"/>${tag(it)}`
  if (it.kind === 'atom') return pill(it.text, ATOM_TINT[it.text] ?? '#eee', '#9aa79f')
  if (it.kind === 'pod' && it.source) {
    const plain = it.text.replace(/◌/g, '')
    const spans = it.text.split(/(◌\w+◌)/).map((x) => (x.startsWith('◌') ? `<tspan opacity="0.35">${x.slice(1, -1)}</tspan>` : x)).join('')
    return pill(plain, '#eef5fd', LEVEL_COLOR[2], 1.8).replace(`>${plain}</text>`, ` xml:space="preserve">${spans}</text>`)
  }
  if (it.kind === 'pod') {
    const lv = levelOf(it.def)
    return `${pill(it.text, '#fff', LEVEL_COLOR[lv], 1.8).replace('stroke-width="1.8"', lv === 0 ? 'stroke-width="1.6" stroke-dasharray="3 2"' : 'stroke-width="1.8"')}<circle cx="${Math.max(24, it.text.length * 7.6 + 14) / 2 - 2}" cy="-9" r="3.5" fill="${LEVEL_COLOR[lv]}"/>`
  }
  return `<circle r="11" fill="${COLORS[it.kind]}"/><text text-anchor="middle" dy="4.5" font-size="12" font-weight="700" fill="#fff">${it.kind}</text>${tag(it)}`
}

/// the name of an open piece of code, small, above its top node
/// the name above open code: plain while the definition is whole (a call that hasn't started), "…" once taken apart
function nameTag(t, me) {
  for (let f = t; ; f = f.f) {
    if (f.label === me) return me
    if (f.kind !== 'app') return `${me}…`
  }
}

function tag(it) {
  if (!it.tag) return ''
  return `<text x="-14" y="-14" text-anchor="end" font-size="11.5" font-style="italic" fill="${LEVEL_COLOR[levelOf(it.def)]}">${it.tag}</text>`
}

/// what a Result beat covers: its call's site
const beatRegion = (b) => callOf(b)?.path ?? []

/// the extent of the call the next Result beat runs
function callHull(scene) {
  const bi = beatFrom(cur)
  if (bi < 0 || beats[bi].level !== 1 || !scene.L) return null
  const region = beatRegion(beats[bi])
  // what lies under the call's site, plus a box the site is folded into
  const under = (it) => it.path.length >= region.length && region.every((x, i) => it.path[i] === x)
  const box = (it) => it.kind === 'pod' && it.path.length < region.length && it.path.every((x, i) => region[i] === x) && scene.L.hidden.has(at(trace.frames[cur], region).uid)
  const inCall = scene.L.items.filter((it) => under(it) || box(it))
  if (!inCall.length) return null
  return {
    x0: Math.min(...inCall.map((i) => i.x)),
    x1: Math.max(...inCall.map((i) => i.x)),
    y0: Math.min(...inCall.map((i) => i.y)),
    y1: Math.max(...inCall.map((i) => i.y)),
  }
}

/// what the next beat acts on: the argument it hands on, or the call it runs
function nextMarks() {
  const bi = beatFrom(cur)
  if (bi < 0) return null
  const b = beats[bi]
  const s = trace.steps[Math.max(cur, b.from)]
  const L = layout(trace.frames[cur])
  const marks = new Set()
  const mark = (uid) => {
    if (L.pos.has(uid)) marks.add(uid)
    else if (L.hidden.has(uid)) marks.add(L.hidden.get(uid))
  }
  if (b.level === 1 && b.pick) {
    mark(at(trace.frames[cur], b.pick.path).uid)
    return marks
  }
  if (b.level === 2) mark(s.arg)
  mark(s.head)
  return marks
}

// ---------- the loop ----------

function tick(now) {
  if (anim) {
    const raw = clamp((now - anim.t0) / anim.dur)
    const p = anim.reverse ? 1 - raw : raw
    const cam = {
      k: lerp(anim.camA.k, anim.camB.k, ease(p)),
      x: lerp(anim.camA.x, anim.camB.x, ease(p)),
      y: lerp(anim.camA.y, anim.camB.y, ease(p)),
    }
    render(frameScene(anim, p), cam)
    if (raw < 1) return requestAnimationFrame(tick)
    const then = anim.then
    anim = null
    if (then) {
      // a short hold on the result before what rides along plays
      holding = true
      pendingTarget = then.target
      setTimeout(() => {
        if (!holding) return
        holding = false
        pendingTarget = null
        if (anim) return
        playTo(then.target, then.mode, then.beat)
        anim.dur = reduced ? 1 : 450
        anim.captionOf = then.captionOf
      }, reduced ? 0 : 250)
      draw()
      return
    }
    if (once && cur >= once.until) {
      once = null
      rebuild()
    }
    if (playing) setTimeout(() => playing && forward(), 380)
  }
  draw()
}

let camNow = null
function draw() {
  const sc = staticScene()
  camNow = fit(sc.L.bounds)
  render(sc, camNow)
  placeRing()
  updateUI()
}

function playTo(target, mode, beat) {
  const T = transition(cur, target, beat, mode)
  const both = { minX: Math.min(T.A.bounds.minX, T.B.bounds.minX), maxX: Math.max(T.A.bounds.maxX, T.B.bounds.maxX), maxY: Math.max(T.A.bounds.maxY, T.B.bounds.maxY) }
  T.camA = mode === 'flow' ? fit(both) : (camNow ?? fit(T.A.bounds))
  T.camB = mode === 'flow' ? fit(both) : fit(T.B.bounds)
  cur = target
  anim = T
  updateUI()
  requestAnimationFrame(tick)
}

function forward() {
  suggest.hidden = true
  // → while a beat plays lands on its end; if it cut the beat short, that was a race
  if ((anim && !anim.reverse) || holding) {
    const cutShort = performance.now() - beatClock.t0 < beatClock.total / 2
    const target = anim?.then?.target ?? pendingTarget ?? cur
    anim = null
    holding = false
    pendingTarget = null
    cur = target
    if (once && cur >= once.until) {
      once = null
      rebuild()
    }
    noteRace(cutShort ? beatClock.owner : null)
    draw()
    return
  }
  if (anim) return
  const bi = beatFrom(cur)
  if (bi < 0) {
    playing = false
    updateUI()
    return
  }
  const b = beats[bi]
  const mode = b.level === 2 && cur === b.from ? 'flow' : b.level === 1 ? 'morph' : 'rule'
  noteForward(b)
  lastPlayed = { from: cur, to: b.to }
  beatClock = { t0: performance.now(), total: { rule: 650, morph: 900, flow: 1800 }[mode] + (b.riders.length ? 700 : 0), owner: b.owner }
  // the beat's own work first, then (after a moment on its result) whatever rides along
  const split = mode === 'rule' ? b.to : splitOf(b, cur)
  if (split > cur && split < b.to) {
    playTo(split, mode, b)
    anim.then = { target: b.to, mode: 'morph', beat: null, captionOf: { beat: b, from: cur } }
  } else playTo(b.to, mode, b)
}

function back() {
  if (anim || cur === 0) return
  const bi = beats.findIndex((b) => b.to >= cur)
  const b = beats[bi]
  const prev = b && b.from < cur ? b.from : 0
  // the beat's own animation, played backwards
  const mode = b.level === 2 && cur === b.to ? 'flow' : b.level === 1 ? 'morph' : 'rule'
  const T = transition(prev, cur, b, mode)
  T.reverse = true
  T.camA = fit(T.A.bounds)
  T.camB = camNow ?? fit(T.B.bounds)
  cur = prev
  anim = T
  updateUI()
  requestAnimationFrame(tick)
}

// ---------- the dial ----------

function resetOne(def) {
  const before = layout(trace.frames[cur])
  handSet.delete(def)
  const keep = { ...levels }
  defaults()
  for (const d of ex.defs) if (d.name !== def && handSet.has(d.name)) levels[d.name] = keep[d.name]
  rebuild()
  if (anim) return
  const T = transition(cur, cur, null, 'restructure', before)
  T.camA = camNow ?? fit(T.A.bounds)
  T.camB = fit(T.B.bounds)
  anim = T
  requestAnimationFrame(tick)
}

function setLevel(def, level, all = false) {
  if (!all && handSet.has(def) && levelOf(def) === level) return resetOne(def)
  const before = layout(trace.frames[cur])
  if (all) {
    shifted = true
    // stop only where no further notch would change any beat or its setting
    const now = cutOf()
    let changes = false
    for (let k = 1; k <= 4 && !changes; k++) {
      offset += level * k
      changes = cutOf() !== now
      offset -= level * k
    }
    if (changes) offset += level
  } else {
    for (const k of [...replays.keys()]) if (k.startsWith(`${def}:`)) replays.delete(k)
    const keep = { ...levels }
    const mine = new Set(handSet)
    defaults()
    const dflt = levels[def]
    levels = keep
    for (const d of mine) handSet.add(d)
    const want = kinds[def] === 'constant' ? (level >= 2 ? 3 : 0) : Math.max(0, Math.min(3, level))
    // choosing a dial's default is the same as leaving it alone
    if (want === dflt && !(offset && !mine.has(def))) handSet.delete(def)
    else handSet.add(def)
    levels[def] = want
  }
  suggest.hidden = true
  if (anim) {
    anim = null
  }
  const played = lastPlayed
  const watched = played && JSON.stringify(beatsIn(played))
  rebuild()
  // the beat just watched, now cut or shown differently: go back to its start to watch it again
  const was = played ? JSON.parse(watched) : []
  const now = played ? beatsIn(played) : []
  const finer = now.length > was.length || Math.max(...now.map((x) => x[2])) > Math.max(...was.map((x) => x[2]))
  if (played && played.to === cur && finer) {
    cur = played.from
    lastPlayed = null
  }
  // the same frame with pods opening or closing: everything glides from where it was on screen
  const T = transition(cur, cur, null, 'restructure', before)
  T.camA = camNow ?? fit(T.A.bounds)
  T.camB = fit(T.B.bounds)
  anim = T
  requestAnimationFrame(tick)
}

/// a cut, as boundaries and settings, for telling whether a turn changed anything
const cutOf = () => beatsOf(trace.steps, levelOf, trace.frames, trace.calls, isYours).map((b) => `${b.from}:${b.level}`).join(' ')
const boundariesOf = () => beatsOf(trace.steps, levelOf, trace.frames, trace.calls, isYours).map((b) => b.from).join(' ')

function rebuild() {
  beats = beatsOf(trace.steps, levelOf, trace.frames, trace.calls, isYours)
  updateSide()
  updateUI()
}

function placeRing() {
  const uid = pinned ?? hoverUid
  const g = uid && svg.querySelector(`.node[data-uid="${uid}"]`)
  if (!g) {
    ring.hidden = true
    return
  }
  const it = layout(trace.frames[cur]).items.find((i) => i.uid === uid)
  if (!it) return
  const box = g.getBoundingClientRect()
  const host = svg.getBoundingClientRect()
  const lv = levelOf(it.def)
  const r = 34
  const two = kinds[it.def] === 'constant'
  const arcs = LEVELS.map((_, i) => {
    if (two && (i === 1 || i === 2)) return ''
    const a0 = Math.PI * (1.25 - i * 0.5 * 0.92)
    const a1 = a0 - Math.PI * 0.42
    const p = (a) => `${50 + r * Math.cos(a)},${50 - r * Math.sin(a)}`
    return `<path data-l="${i}" d="M${p(a0)} A${r},${r} 0 0 1 ${p(a1)}" stroke="${i <= lv ? LEVEL_COLOR[lv] : '#c9d3c6'}" stroke-width="${i === lv ? 7 : 5}" fill="none" stroke-linecap="round" style="pointer-events:stroke;cursor:pointer"><title>${LEVELS[i]}</title></path>`
  }).join('')
  if (it.def === 'main' && (it.kind === 'atom' || it.kind === 'app')) {
    ring.innerHTML = `<div class="tip" style="left:20px">a value passed in: it has no code of its own, so no dial</div>`
    ring.style.left = `${box.left + box.width / 2 - host.left}px`
    ring.style.top = `${box.top + box.height / 2 - host.top}px`
    ring.hidden = false
    return
  }
  const kind = ex.defs.find((d) => d.name === it.def)?.kind
  ring.innerHTML = `<svg width="100" height="100">${arcs}</svg><div class="tip"><b>${it.def}</b> · ${LEVELS[lv]} <i>— ${LEVEL_SAYS[lv]}</i><br><i>${kind ? `${kind === 'yours' ? 'your code' : kind === 'prelude' ? 'from the prelude' : 'a constant'} · ` : ''}+ more · − less · click a notch · Shift: everything</i></div>`
  ring.querySelectorAll('path[data-l]').forEach((el) => (el.onclick = () => setLevel(it.def, Number(el.dataset.l))))
  ringAt = { x: box.left + box.width / 2, y: box.top + box.height / 2 }
  ring.style.left = `${box.left + box.width / 2 - host.left}px`
  ring.style.top = `${box.top + box.height / 2 - host.top}px`
  ring.hidden = false
}

let ringAt = null
svg.addEventListener('pointermove', (e) => {
  const g = e.target.closest?.('.node')
  const uid = g ? Number(g.dataset.uid) : null
  if (uid === null && ringAt && Math.hypot(e.clientX - ringAt.x, e.clientY - ringAt.y) < 70) return
  if (uid === hoverUid) return
  hoverUid = uid
  const it = uid && layout(trace.frames[cur]).items.find((i) => i.uid === uid)
  hoverDef = it?.def ?? (pinned ? hoverDef : null)
  if (!anim) draw()
})
ring.addEventListener('pointerleave', () => {
  hoverUid = null
  if (!pinned) hoverDef = null
  if (!anim) draw()
})
svg.addEventListener('pointerleave', (e) => {
  if (ring.contains(e.relatedTarget)) return
  hoverUid = null
  if (!pinned) hoverDef = null
  if (!anim) draw()
})
svg.addEventListener('click', (e) => {
  const g = e.target.closest?.('.node')
  pinned = g ? Number(g.dataset.uid) : null
  if (!anim) draw()
})
svg.addEventListener(
  'wheel',
  (e) => {
    // plain scroll stays the camera's; Alt+scroll turns the dial
    if (!hoverDef || !e.altKey) return
    e.preventDefault()
    turn(e.deltaY < 0 ? 1 : -1, e.shiftKey)
  },
  { passive: false },
)

function turn(d, all) {
  if (all) return setLevel(null, d, true)
  if (!hoverDef || hoverDef === 'main') return
  // a constant is either quiet or shown rule by rule
  if (kinds[hoverDef] === 'constant') return setLevel(hoverDef, d > 0 ? 3 : 0)
  setLevel(hoverDef, levelOf(hoverDef) + d)
}

addEventListener('keydown', (e) => {
  // + is Shift+= on most keyboards, so + and − always mean one dial; Shift+] and Shift+[ turn every dial
  if (e.code === 'Equal' || e.code === 'NumpadAdd') turn(1, false)
  else if (e.code === 'Minus' || e.code === 'NumpadSubtract') turn(-1, false)
  else if (e.code === 'BracketRight') turn(1, e.shiftKey)
  else if (e.code === 'BracketLeft') turn(-1, e.shiftKey)
  else if (e.code === 'ArrowRight') forward()
  else if (e.code === 'ArrowLeft') back()
  else if (e.code === 'Space') togglePlay()
  else if (e.code === 'Escape') {
    pinned = null
    hoverDef = null
    draw()
  } else return
  e.preventDefault()
})

// ---------- inferring what the reader wants ----------

/// Watching a beat a third time says "I didn't follow that"; racing through one code's beats says "I don't need
/// this". Either way the viewer only offers; a "no" is remembered for that code.
function noteForward(b) {
  const key = `${b.owner}:${b.from}`
  const n = (replays.get(key) ?? 0) + 1
  replays.set(key, n)
  const now = performance.now()

  if (dismissed.has(b.owner) || handSet.has(b.owner) || once) return
  const what = b.pick ? code(said(at(trace.frames[b.from], b.pick.path))) : 'this beat'
  if (n >= 3 && b.level < 3) offer(b, +1, `You've watched ${what} ${n} times.`)

}

/// two beats of the same code cut short in a row: offer fewer of them (or, with none left, for next time)
function noteRace(owner) {
  const now = performance.now()
  if (!owner) {
    lastForward = []
    return
  }
  lastForward = [...lastForward.filter((f) => now - f.t < 8000 && f.owner === owner), { t: now, owner }]
  if (playing || shifted || handSet.has(owner) || dismissed.has(owner) || levelOf(owner) === 0 || lastForward.length < 2) return
  const left = beats.filter((x) => x.from >= cur && x.owner === owner).length
  const first = beats.find((x) => x.owner === owner)
  if (first) offer(first, -1, left ? `Skimming through ${code(owner)}?` : `Skimmed ${code(owner)}. Next time:`)
}

function offer(b, d, text) {
  const def = b.owner
  const to = LEVELS[levelOf(def) + d]
  suggest.innerHTML =
    d > 0
      ? `${text}<br><button id="once">Replay it one notch finer (${to}), just once</button> <button id="yes">Always ${to} for ${def}</button> <button id="no">No</button>`
      : `${text}<br><button id="yes">${to} for ${def}</button> <button id="no">No</button>`
  suggest.hidden = false
  if ($('once'))
    $('once').onclick = () => {
      suggest.hidden = true
      once = { def, level: levelOf(def) + 1, until: b.to }
      cur = b.from
      rebuild()
      draw()
      forward()
    }
  $('yes').onclick = () => setLevel(def, levelOf(def) + d)
  $('no').onclick = () => {
    dismissed.add(def)
    suggest.hidden = true
  }
  updateSide(def, levelOf(def) + d)
}

// ---------- panels ----------

/// a definition whose calls all run inside another's beat: its own setting has nothing to show here
function steppedOver(name) {
  const mine = trace.calls.filter((c) => c.name === name)
  if (!mine.length || levelOf(name) === 0 || beats.some((b) => b.owner === name)) return ''
  const host = beats.find((b) => b.from <= mine[0].start && mine[0].start < b.to)
  return host ? ` (stepped over inside ${host.owner === 'main' ? 'the run' : host.owner})` : ''
}

function updateSide(ghostDef, ghostLevel) {
  const side = $('side')
  side.innerHTML =
    `<p>${ex.blurb}</p>${offset ? `<p><b>Shift ${offset > 0 ? '+' : ''}${offset}</b> on every dial${handSet.size ? `, except ${listed([...handSet].map(code))} (you set ${handSet.size > 1 ? 'them' : 'it'})` : ''}.</p>` : ''}<h3>Your code and what it uses</h3>` +
    ex.defs
      .filter((d) => d.name !== 'main' || trace.steps.some((s) => s.call === null))
      .map(
        (d) =>
          `<div class="def ${hoverDef === d.name ? 'hot' : ''}" data-def="${d.name}"><code>${d.src}</code><div class="why"><b>${LEVELS[levelOf(d.name)]}</b>${offset && d.kind !== 'constant' && !handSet.has(d.name) ? ` (${LEVELS[Math.max(0, Math.min(3, levels[d.name]))]}, ${offset > 0 ? '+' : ''}${offset} from Shift)` : ''}${steppedOver(d.name)}${levelOf(d.name) === 2 && bigCode(d.name) ? ` (drawn as its source: ${combinators(d.name)} combinators)` : ''}${handSet.has(d.name) ? ' 📌 (you set this; Shift leaves it; click it again for the default)' : big.has(d.name) ? ` because it's your code, but ${big.get(d.name)} hand-offs is a lot: + for Flow` : d.kind === 'constant' && levelOf(d.name) === 3 ? ` (Quiet; Shift's top notch shows every rule)` : ` because it's ${DEFAULT_WHY[d.kind]}`}</div><div class="seg">${LEVELS.map((l, i) => ((d.kind === 'constant' && (i === 1 || i === 2)) || (d.name === 'main' && i < 2) ? '' : `<button class="l${i} ${levelOf(d.name) === i ? 'on' : ''} ${ghostDef === d.name && ghostLevel === i ? 'ghost' : ''}" data-def="${d.name}" data-l="${i}" title="${LEVEL_SAYS[i]}">${l}</button>`)).join('')}</div></div>`,
      )
      .join('') +
    `<h3>What the dial does</h3>` +
    LEVELS.map((l, i) => `<p><b style="color:${LEVEL_COLOR[i]}">${l}</b>: ${LEVEL_SAYS[i]}.</p>`).join('') +
    `<p>Every node belongs to the function being applied there; turning the dial on any node sets it for that definition. Shift turns every dial one notch, and Shift the other way undoes it.</p><p><button id="reset">Back to the defaults</button></p>`
  side.querySelectorAll('.seg button').forEach((b) => (b.onclick = () => setLevel(b.dataset.def, Number(b.dataset.l))))
  side.querySelectorAll('.def').forEach((el) => {
    el.onmouseenter = () => {
      hoverDef = el.dataset.def
      if (!anim) draw()
    }
    el.onmouseleave = () => {
      hoverDef = null
      if (!anim) draw()
    }
  })
  $('reset').onclick = () => {
    defaults()
    offset = 0
    shifted = false
    handSet.clear()
    dismissed.clear()
    rebuild()
    draw()
  }
}

function updateUI() {
  // while a beat plays, it is the one highlighted; the counter moves on when it lands
  const shown = anim && !anim.reverse && lastPlayed ? lastPlayed.from : holding && lastPlayed ? lastPlayed.from : cur
  const bi = beatFrom(shown)
  const playingBeat = anim?.beat
  $('caption').innerHTML = anim?.captionOf
    ? describe(anim.captionOf.beat, anim.captionOf.from)
    : playingBeat
    ? describe(playingBeat, anim.a)
    : bi < 0
      ? `Done: ${code(said(trace.frames[cur]))}.`
      : `<span style="color:var(--muted)">Next:</span> ` + describe(beats[bi], cur)
  $('beats').innerHTML = beats
    .map((b, i) => {
      const n = b.to - b.from
      const state = b.to <= shown ? 'done' : i === bi ? 'cur' : ''
      const head = cur > b.from && cur < b.to ? `<span class="playhead" style="left:${(100 * (cur - b.from)) / (b.to - b.from)}%"></span>` : ''
      return `<div class="l${b.level}bg ${state}" style="flex:${Math.max(1, Math.sqrt(n))}" data-i="${i}" title="${LEVELS[b.level]} · ${b.owner} · ${n} rule step${n > 1 ? 's' : ''}"><div class="ticks">${n > 1 ? Array.from({ length: Math.min(n, 12) }, () => '<span></span>').join('') : ''}</div>${head}</div>`
    })
    .join('')
  $('beats')
    .querySelectorAll('div[data-i]')
    .forEach((el) => {
      el.onclick = () => {
        if (anim) return
        cur = beats[Number(el.dataset.i)].from
        draw()
      }
    })
  const done = beats.filter((b) => b.to <= shown).length
  $('count').textContent = `beat ${Math.min(done + 1, beats.length)} of ${beats.length} · ${trace.steps.length} rule steps in all`
  $('play').textContent = playing ? '❚❚' : '▶'
}

function togglePlay() {
  playing = !playing
  updateUI()
  if (playing) forward()
}

/// past this many hand-offs, your own code starts at Result, with a note saying why
const BIG = 30

function defaults() {
  levels = Object.fromEntries(ex.defs.map((d) => [d.name, DEFAULT_LEVEL[d.kind]]))
  kinds = Object.fromEntries(ex.defs.map((d) => [d.name, d.kind]))
  big = new Map()
  if (!trace) return
  for (const d of ex.defs.filter((x) => x.kind === 'yours' && x.name !== 'main')) {
    const n = beatsOf(trace.steps, (o) => (o === d.name ? 2 : 1), trace.frames, trace.calls, isYours).filter((b) => b.owner === d.name).length
    if (n > BIG) {
      levels[d.name] = 1
      big.set(d.name, n)
    }
  }
}
let big = new Map()

function load(key) {
  exKey = key
  ex = EXAMPLES[key]
  originOf.clear()
  scaffoldSize.clear()
  trace = run(ex.build())
  handSet.clear()
  dismissed.clear()
  offset = 0
  shifted = false
  defaults()
  cur = 0
  anim = null
  playing = false
  replays.clear()
  suggest.hidden = true
  rebuild()
  document.querySelectorAll('nav button').forEach((b) => b.classList.toggle('on', b.dataset.k === key))
  camNow = null
  draw()
}

$('examples').innerHTML = Object.entries(EXAMPLES)
  .map(([k, e]) => `<button data-k="${k}">${e.title}</button>`)
  .join('')
document.querySelectorAll('nav button').forEach((b) => (b.onclick = () => load(b.dataset.k)))
$('fwd').onclick = forward
$('back').onclick = back
$('play').onclick = togglePlay
addEventListener('resize', () => !anim && draw())

const params = new URLSearchParams(location.search)
load(params.get('ex') ?? 'check')
window.__dial = { walk: () => beats.map((b) => describe(b, b.from).replace(/<[^>]+>/g, '')), forward, back, setLevel, load, get cur() { return cur }, get beats() { return beats }, hover: (def) => { hoverDef = def; draw() } }
