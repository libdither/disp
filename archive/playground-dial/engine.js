// A toy S/K/I reducer that remembers where every node came from, and the rules that group its steps into beats.
// The playground runs tree calculus, not S/K/I, but its steps read as S, K, I and triage, so the grouping carries over.

let UID = 0
/// uid of a copy -> uid of what it was copied from
const originOf = new Map()

const N = (kind, o = {}) => ({ uid: ++UID, kind, ...o })
const S = () => N('S')
const K = () => N('K')
const I = () => N('I')
/// a free name: nobody's code
const atom = (name) => N('atom', { name, def: 'main' })
const ap = (f, ...args) => args.reduce((g, a) => N('app', { f: g, a }), f)

/// every node of t that has no owner yet belongs to def
function own(t, def) {
  if (!t.def) t.def = def
  if (t.kind === 'app') {
    own(t.f, def)
    own(t.a, def)
  }
  return t
}
/// every node written in a definition's text remembers it, so a folded definition keeps its own constants inside
function home(t, name) {
  t.homes = [...(t.homes ?? []), name]
  if (t.kind === 'app') {
    home(t.f, name)
    home(t.a, name)
  }
}
/// a named piece of code or data: its nodes belong to it, and its top carries the name
const named = (name, t) => {
  home(t, name)
  return Object.assign(own(t, name), { label: name })
}

function clone(t) {
  const c = { ...t, uid: ++UID }
  originOf.set(c.uid, t.uid)
  if (t.kind === 'app') {
    c.f = clone(t.f)
    c.a = clone(t.a)
  }
  return c
}

function spine(t) {
  const args = []
  const apps = []
  while (t.kind === 'app') {
    args.unshift(t.a)
    apps.unshift(t)
    t = t.f
  }
  return { head: t, args, apps }
}

const ARITY = { S: 3, K: 2, I: 1 }

/// One step, eager, arguments first: along an application's spine, every argument is worked out (left to right)
/// before the function takes any of them. Then the innermost stage that has all it needs fires. Steps on disjoint
/// subtrees commute, so this is the same run as innermost-first in another order, with one difference that matters
/// to a reader: a call never starts until its arguments are ready, so each call is one unbroken stretch of steps.
function step(t) {
  if (t.kind !== 'app') return null
  const { head, args, apps } = spine(t)
  // a named call works out all its arguments before it starts; inside a body, a combinator fires as soon as
  // its own arguments are ready, before the calls the body built further along the spine
  let namedCall = false
  for (let f = t; f.kind === 'app'; f = f.f) if (f.f.label) namedCall = true
  const need = ARITY[head.kind]
  const first = !namedCall && need && args.length >= need ? need : args.length
  for (let k = 0; k < first; k++) {
    const r = step(args[k])
    if (!r) continue
    let res = head
    for (let m = 0; m < args.length; m++) res = { ...apps[m], f: res, a: m === k ? r.term : args[m] }
    return { term: res, info: r.info }
  }
  if (need && args.length >= need && first === need) return fire(head, args, apps, need)
  for (let k = first; k < args.length; k++) {
    const r = step(args[k])
    if (!r) continue
    let res = head
    for (let m = 0; m < args.length; m++) res = { ...apps[m], f: res, a: m === k ? r.term : args[m] }
    return { term: res, info: r.info }
  }
  if (!need || args.length < need) return null
  return fire(head, args, apps, need)
}

function fire(head, args, apps, need) {
  const site = apps[need - 1]
  const info = { rule: head.kind, owner: head.def, head: head.uid, site: site.uid, dropped: [], clone: null }
  let res
  if (head.kind === 'S') {
    const [x, y, z] = args
    const z2 = clone(z)
    info.arg = z.uid
    info.clone = [z.uid, z2.uid]
    info.argText = show(z)
    info.parts = [show(x), show(y)]
    // the new top keeps the old top's identity, so the spine above it doesn't flicker; S's code owns it
    res = { ...site, def: head.def, f: N('app', { f: x, a: z, def: head.def }), a: N('app', { f: y, a: z2, def: head.def }) }
  } else if (head.kind === 'K') {
    const [x, y] = args
    info.arg = y.uid
    info.dropped = [y.uid]
    info.argText = show(y)
    info.parts = [show(x)]
    res = x
  } else {
    info.arg = args[0].uid
    info.argText = show(args[0])
    res = args[0]
  }
  for (let k = need; k < args.length; k++) res = { ...apps[k], f: res }
  return { term: res, info }
}

/// whose code a node is, as the reader sees it: an application belongs to the function being applied
function codeOf(t) {
  while (t.kind === 'app') t = t.f
  return t.def
}

/// is there anything left to fire in t?
function hasRedex(t) {
  if (t.kind !== 'app') return false
  if (hasRedex(t.f) || hasRedex(t.a)) return true
  const { head, args } = spine(t)
  return ARITY[head.kind] !== undefined && args.length === ARITY[head.kind]
}

function show(t, depth = 0) {
  if (t.label) return t.label
  if (t.kind === 'atom') return t.name
  if (t.kind !== 'app') return t.kind
  if (depth > 2) return '…'
  // a named function stays its name, so a call reads as and(true, not(true))
  const args = []
  let head = t
  while (head.kind === 'app' && !(head.label && head !== t)) {
    args.unshift(head.a)
    head = head.f
  }
  return `${show(head, depth)}(${args.map((a) => show(a, depth + 1)).join(', ')})`
}

function uidsOf(t, out = new Set()) {
  out.add(t.uid)
  if (t.kind === 'app') {
    uidsOf(t.f, out)
    uidsOf(t.a, out)
  }
  return out
}

function pathOf(term, uid, path = []) {
  if (term.uid === uid) return path
  if (term.kind !== 'app') return null
  return pathOf(term.f, uid, [...path, 0]) ?? pathOf(term.a, uid, [...path, 1])
}

/// the whole call a site belongs to: up the spine while it is the function side
function callPath(path) {
  let p = path
  while (p.length && p[p.length - 1] === 0) p = p.slice(0, -1)
  return p
}

const at = (term, path) => path.reduce((t, k) => (t.kind === 'app' ? (k ? t.a : t.f) : t), term)

const within = (path, region) => path.length >= region.length && region.every((x, i) => path[i] === x)

/// Run to the end, keeping the calls the way the playground's Trace does: a call opens when the function part of
/// the firing application is a named definition (climbing its spine, so `and(true, x)` is one call), and closes once
/// nothing under its site can fire. With arguments first, a call's steps are one unbroken stretch, and calls nest.
function run(term, max = 300) {
  const frames = [term]
  const steps = []
  const calls = []
  const stack = []
  for (let i = 0; i < max; i++) {
    const root = frames[frames.length - 1]
    const r = step(root)
    if (!r) break
    const path = pathOf(root, r.info.site)
    const site = at(root, path)
    // the named definition being applied, if any (a constant like `true` used as a function counts)
    let fn = site.f
    while (fn.kind === 'app' && !fn.label) fn = fn.f
    if (fn.label && !(stack.length && stack[stack.length - 1].fn === fn.uid)) {
      let top = path
      while (top.length && top[top.length - 1] === 0) top = top.slice(0, -1)
      const { args } = spineUpTo(at(root, top), fn)
      const call = { id: calls.length, name: fn.label, fn: fn.uid, start: steps.length, end: -1, path: top, args: args.map((a) => a.uid), parent: stack.length ? stack[stack.length - 1].id : null }
      calls.push(call)
      stack.push(call)
    }
    r.info.path = path
    r.info.call = stack.length ? stack[stack.length - 1].id : null
    r.info.droppedAll = new Set()
    for (const d of r.info.dropped) {
      const sub = find(root, d)
      if (sub) uidsOf(sub, r.info.droppedAll)
    }
    steps.push(r.info)
    frames.push(r.term)
    while (stack.length && !hasRedex(at(r.term, stack[stack.length - 1].path))) stack.pop().end = steps.length
  }
  for (const c of calls) if (c.end < 0) c.end = steps.length
  return { frames, steps, calls }
}

/// the arguments a named function is applied to, from the top of its call down to the function itself
function spineUpTo(t, fn) {
  const args = []
  while (t.kind === 'app' && t !== fn) {
    args.unshift(t.a)
    t = t.f
  }
  return { args }
}

function find(t, uid) {
  if (t.uid === uid) return t
  if (t.kind !== 'app') return null
  return find(t.f, uid) ?? find(t.a, uid)
}

/// is uid the argument z, or a copy of it (or of a copy)?
function inFamily(uid, root) {
  for (let u = uid; u !== undefined; u = originOf.get(u)) if (u === root) return true
  return false
}

const LEVELS = ['Quiet', 'Result', 'Flow', 'Rules']
const LEVEL_SAYS = [
  'it never gets a beat of its own: its steps ride along with the beat around them',
  'each call is one beat: the call becomes its result',
  'each hand-off is one beat: the argument travels to where it is used',
  'every S, K and I step is its own beat',
]

/// The composition rules. Beats are cut at boundaries between steps, and whether there is a boundary between two
/// steps is a yes/no question decided by the calls around it. Every condition below can only switch off as a dial
/// goes down, so turning a dial down only ever removes boundaries: it merges beats, never moves them.
///
/// At each boundary, some calls have a say:
///  - the innermost call holding both steps (M), by its setting:
///    Rules: always, except before a Quiet call, which rides along with the step before it;
///    Flow: before a call that isn't Quiet, or between its own steps when the argument being handed on changes.
///      A step that hands on none of its arguments, or only throws a copy away (K), rides: no boundary before it;
///    Result and Quiet: never inside themselves;
///  - each call starting or ending there, if it is set to Result or finer: a call is a beat of its own
///    (but neither a step of M's that rides nor quiet work starts a beat after a call ends).
/// A say is overruled when a Result or Quiet call encloses the call that has it, unless something between them
/// breaks out: a call set finer than Result, or, in your own code, a recursive call of the same definition (each
/// call its own beat). Imported code at Result steps over its own recursion.
function beatsOf(steps, levelOf, frames, calls, yours = () => true) {
  const L = (c) => (c === null ? Math.max(2, levelOf('main')) : levelOf(calls[c].name))
  const parent = (c) => (c === null ? undefined : calls[c].parent)
  const chainOf = (i) => {
    const chain = []
    for (let c = steps[i].call; c !== null; c = calls[c].parent) chain.unshift(c)
    return chain
  }
  // a recursive call of your own code is a beat of its own at Result; imported code steps over its recursion
  const exempt = (x, e) => L(x) >= 2 || (L(x) >= 1 && e !== null && calls[x].name === calls[e].name && yours(calls[x].name))
  /// is what call y says overruled by a Result or Quiet call around it?
  const overruled = (y) => {
    const path = []
    for (let e = parent(y); e !== undefined; e = parent(e)) {
      if (L(e) <= 1 && [y, ...path].every((x) => x === null || !exempt(x, e))) return true
      path.push(e)
      if (e === null) break
    }
    return false
  }
  /// for a step of call m's own: which of m's arguments it hands on (-1 for none), and whether it rides
  const own = (i, m) => {
    const st = steps[i]
    const fam = m === null ? 0 : calls[m].args.findIndex((a) => inFamily(st.arg, a))
    return { fam, rides: fam < 0 || st.rule === 'K' }
  }
  // what a beat is shown as: the outermost Result call around its first step, unless broken out of; else the innermost
  const shownAs = (i) => {
    let pick = null
    for (const c of chainOf(i)) {
      if (pick !== null && L(pick) <= 1 && !exempt(c, pick)) continue
      pick = c
    }
    return pick
  }
  const lastFam = new Map()
  const boundary = [false]
  for (let i = 1; i < steps.length; i++) {
    const A = chainOf(i - 1)
    const B = chainOf(i)
    let k = 0
    while (k < A.length && k < B.length && A[k] === B[k]) k++
    const m = k > 0 ? A[k - 1] : null
    const a = A.length > k ? A[k] : 'own'
    const b = B.length > k ? B[k] : 'own'
    const says = []
    const quiet = (x) => x !== 'own' && L(x) === 0
    const mine = b === 'own' ? own(i, m) : null
    // the family of m's last own step that didn't ride, for telling hand-offs apart
    if (a === 'own') {
      const prev = own(i - 1, m)
      if (!prev.rides) lastFam.set(m, prev.fam)
    }
    if (L(m) === 3 && !quiet(b)) says.push(m)
    if (L(m) === 2) {
      if (b !== 'own' && !quiet(b)) says.push(m)
      else if (b === 'own' && !mine.rides && ((a !== 'own' && !quiet(a)) || lastFam.get(m) !== mine.fam)) says.push(m)
    }
    for (const x of B.slice(k)) if (L(x) >= 1) says.push(x)
    // riding is a property of the step, not of anyone's setting, so it can't make a turn add a boundary
    const ridesOn = b === 'own' && mine.rides
    // nor does a call's end split off quiet work that follows it (a Quiet call, or steps shown as a Quiet call's)
    const quietNext = quiet(b) || L(shownAs(i)) === 0
    for (const x of A.slice(k)) if (L(x) >= 1 && !ridesOn && !quietNext) says.push(x)
    boundary.push(says.some((y) => !overruled(y)))
  }
  const beats = []
  for (let i = 0; i < steps.length; i++) {
    if (i === 0 || boundary[i]) {
      const pick = shownAs(i)
      const allQuiet = (j) => steps[j].call !== null && L(steps[j].call) === 0
      beats.push({ from: i, to: i + 1, call: pick, pick: pick === null ? null : calls[pick], owner: pick === null ? 'main' : calls[pick].name, level: L(pick), arg: steps[i].arg, riders: [], allQuiet })
    } else beats[beats.length - 1].to = i + 1
  }
  for (const b of beats) {
    for (let j = b.from; j < b.to; j++) if (steps[j].call !== b.call) b.riders.push(j)
    // a beat is tagged with its call's own setting (a Quiet call with nothing to ride with is still Quiet)
    b.level = L(b.call)
    delete b.allQuiet
  }
  return beats
}

/// a combinator holding fewer arguments than it needs: a function waiting for more
function waiting(t) {
  if (t.kind !== 'app') return false
  const { head, args } = spine(t)
  return ARITY[head.kind] !== undefined && args.length < ARITY[head.kind]
}

// ---------- a small compiler, so every example is built from the source line it shows ----------

/// `name(args…)` or a bare name; S, K and I are the combinators
function parseExpr(src) {
  let i = 0
  const ws = () => {
    while (src[i] === ' ') i++
  }
  const expr = () => {
    ws()
    const m = /^\w+/.exec(src.slice(i))
    if (!m) throw new Error(`can't read ${src}`)
    i += m[0].length
    let e = { name: m[0] }
    ws()
    while (src[i] === '(') {
      i++
      const args = []
      for (;;) {
        args.push(expr())
        ws()
        if (src[i] === ',') i++
        else break
      }
      ws()
      i++
      e = args.reduce((f, x) => ({ f, x }), e)
      ws()
    }
    return e
  }
  return expr()
}

const free = (x, e) => (e.name !== undefined ? e.name === x : free(x, e.f) || free(x, e.x))

/// bracket abstraction, as disp's compiler does it: [x]x = I, [x]M = K(M) when x isn't in M,
/// [x]M(x) = M (eta), [x]M(N) = S([x]M, [x]N)
function abstract(x, e) {
  if (e.name === x) return { name: 'I' }
  if (!free(x, e)) return { f: { name: 'K' }, x: e }
  if (e.x.name === x && !free(x, e.f)) return e.f
  return { f: { f: { name: 'S' }, x: abstract(x, e.f) }, x: abstract(x, e.x) }
}

/// build an example's term from its source lines: definitions become named trees, other names are free
function compileDefs(defs) {
  const src = new Map()
  for (const d of defs) {
    const m = d.src.match(/^\s*(\w+)\s*:=\s*(?:\{([^}]*)\}\s*=>\s*)?(.*)$/)
    if (m) src.set(m[1], { params: m[2] ? m[2].split(',').map((p) => p.trim()) : [], body: m[3] })
  }
  const toNode = (e) => {
    if (e.name !== undefined) {
      if (e.name === 'S') return S()
      if (e.name === 'K') return K()
      if (e.name === 'I') return I()
      if (src.has(e.name)) return build(e.name)
      return atom(e.name)
    }
    return ap(toNode(e.f), toNode(e.x))
  }
  const build = (name) => {
    const { params, body } = src.get(name)
    const e = params.reduceRight((t, p) => abstract(p, t), parseExpr(body))
    return named(name, toNode(e))
  }
  const main = defs.find((d) => d.name === 'main')
  return own(toNode(parseExpr(main.src)), 'main')
}

/// the examples: user code, a prelude, and the constants they use
const PRELUDE = [
  { name: 'and', src: 'and := {p, q} => p(q, false)', kind: 'prelude' },
  { name: 'not', src: 'not := {b} => b(false, true)', kind: 'prelude' },
  { name: 'true', src: 'true := K', kind: 'constant' },
  { name: 'false', src: 'false := K(I)', kind: 'constant' },
]
const EXAMPLES = {
  fan: {
    title: 'fan(a)',
    blurb: 'One argument handed through a compiled function: S copies it, K throws one copy away.',
    defs: [
      { name: 'fan', src: 'fan := {x} => f(x, g(x), h)', kind: 'yours' },
      { name: 'main', src: 'fan(a)', kind: 'yours' },
    ],
  },
  check: {
    title: 'check(true)',
    blurb: 'Your function calls two prelude functions. By default you watch your code and the prelude runs as single beats.',
    defs: [
      { name: 'check', src: 'check := {x} => and(x, not(x))', kind: 'yours' },
      { name: 'main', src: 'check(true)', kind: 'yours' },
      ...PRELUDE,
    ],
  },
  twice: {
    title: 'twice(not, true)',
    blurb: 'A function handed to a function: twice passes not along, then not runs twice.',
    defs: [
      { name: 'twice', src: 'twice := {f, x} => f(f(x))', kind: 'yours' },
      { name: 'main', src: 'twice(not, true)', kind: 'yours' },
      ...PRELUDE.filter((d) => d.name !== 'and'),
    ],
  },
  mix: {
    title: 'mix(true, p, q)',
    blurb: 'A big function whose body calls another: not(a) can run as soon as a is in, before b and c arrive.',
    defs: [
      { name: 'mix', src: 'mix := {a, b, c} => f(not(a), c, b, g(c))', kind: 'yours' },
      { name: 'main', src: 'mix(true, p, q)', kind: 'yours' },
      ...PRELUDE.filter((d) => d.name !== 'and'),
    ],
  },
  rev: {
    title: 'rev(p, q, r, s)',
    blurb: 'Four arguments, four hand-offs: each finds its place in the body. Race through them and the viewer offers fewer beats.',
    defs: [
      { name: 'rev', src: 'rev := {a, b, c, d} => f(d, c, b, a)', kind: 'yours' },
      { name: 'main', src: 'rev(p, q, r, s)', kind: 'yours' },
    ],
  },
}
for (const ex of Object.values(EXAMPLES)) ex.build = () => compileDefs(ex.defs)

/// what the dial starts at, and why: your code shows its hand-offs, the prelude its results, constants nothing
const DEFAULT_LEVEL = { yours: 2, prelude: 1, constant: 0 }
const DEFAULT_WHY = { yours: 'your code', prelude: 'from the prelude', constant: 'a constant' }

if (typeof module !== 'undefined') module.exports = { EXAMPLES, run, beatsOf, show, LEVELS, DEFAULT_LEVEL, inFamily, codeOf, at }
