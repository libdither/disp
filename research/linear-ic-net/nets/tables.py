"""Reflective nets with one-agent functions and a kept table: a prototype reducer.

When a program is loaded, every λ of it becomes a definition in a fixed list: (binder, x, body, names), the
body closed over x and the captured names, its own λs replaced by references ('#', j) to their definitions
(each capturing names of this body). Code is then: names, 'ρ', (f, a), ('#', k), ('keep', s1, …, s6).

Agents (principal port first, then auxiliary ports):
  code and values, inert, principal facing up toward whoever uses it
    clo_k(p; e1..en)     the function of definition k, its n captured values on e1..en
    cloL_k(p; e1..en)    the same function opened: applying it hands back its body as code, not run
    app(p; f, a)         frozen application
    var(p; s)            an occurrence in code: s leads to the value it stands for
    rho(p)               the primitive: a value with no rule of its own (applying it is stuck)
    tab(p; a1..a6)       a kept table keep[e1|…|e6], one slot per kind of value; a value
  running
    O(p; a, r)           apply: p faces the function, a the argument, r gives the result
    T(p; o)              thaw: p faces code, o gives the running result
    Ctab(p; a1..a6, r)   a table reading: p faces the value read, a1..a6 hold the slots, r gives the result
  resources
    sig(p; a, b)         sharer: where a name used twice splits; fires as a copy once a value arrives
    dup(p; a, b)         a copy in flight
    era(p)               the one eraser

Rules, one per pair:
  O ⋈ clo_k    layout: definition k's body, already running, in one rewrite. Its x goes to the argument and its
               names to e1..en; an application is an O, a variable a wire, ρ a rho, a table a tab with running
               slots, an inner λ a clo_j. A name used twice splits through sig, one never used goes to era.
  O ⋈ cloL_k   layout as code: the same body with app, var, rho, tab, clo_j, to r
  O ⋈ tab      Ctab: p → the argument, the six slots, r → O's result
  Ctab ⋈ X     X of kind i (clo 0, cloL 1, app 2, var 3, rho 4, tab 5): a dup on slot i; one copy goes back into a
               rebuilt tab, the other is applied to that tab and then to X's parts: clo → X as cloL, cloL → X as
               clo, app → f a, var → s, rho → ρ, tab → its six slots
  T ⋈ app      O with T on f and on a, r → o
  T ⋈ var      s–o (an occurrence resolves; whatever it leads to is not thawed)
  T ⋈ tab      the tab with a T on each slot
  T ⋈ clo | cloL | rho   the value itself, at o
  sig | dup ⋈ X   copy X, a code agent; the copies' ports get dup agents
  era ⋈ X      erase X, a code agent; its ports get erasers

Every auxiliary port of a code agent takes a value in, so a copier or an eraser only ever faces a value: no
copy meets a copy and no eraser meets an eraser, and there is no annihilation. A pair with no rule (O ⋈ rho,
O ⋈ app, O ⋈ var: applying ρ or code) is stuck.
"""
import itertools, random
from collections import Counter, deque

ARITY = {'app': 2, 'var': 1, 'rho': 0, 'tab': 6, 'O': 2, 'T': 1, 'Ctab': 7,
         'sig': 2, 'dup': 2, 'era': 0, 'root': 0, 'ph': 1}       # clo, cloL: one port per captured name
NSLOT = 6
KIND = {'clo': 0, 'cloL': 1, 'app': 2, 'var': 3, 'rho': 4, 'tab': 5}
VALUES = ('clo', 'cloL', 'rho')


def is_tab(t):
    return isinstance(t, tuple) and len(t) == NSLOT + 1 and t[0] == 'keep'


def merge(*lists):
    out = []
    for l in lists:
        for x in l:
            if x not in out: out.append(x)
    return tuple(out)


class Defs:
    """The fixed list of definitions of a program: each λ as (binder, x, body, names)."""
    def __init__(self):
        self.table, self.key, self.memo = [], {}, {}

    def convert(self, t):
        """(code, names): t with each λ replaced by ('#', k), and the names free in t in the order first used"""
        got = self.memo.get(id(t))
        if got is not None and got[0] is t: return got[1], got[2]
        if t == 'ρ': return t, ()
        if isinstance(t, str): return t, (t,)
        if is_tab(t):
            parts = [self.convert(s) for s in t[1:]]
            r = ('keep',) + tuple(c for c, _ in parts), merge(*(f for _, f in parts))
        elif len(t) == 3:
            body, fv = self.convert(t[2])
            names = tuple(x for x in fv if x != t[1])
            d = (t[0], t[1], body, names)
            k = self.key.get(d)
            if k is None:
                k = self.key[d] = len(self.table)
                self.table.append(d)
            r = ('#', k), names
        else:
            (f, ff), (a, fa) = self.convert(t[0]), self.convert(t[1])
            r = (f, a), merge(ff, fa)
        self.memo[id(t)] = (t, r[0], r[1])
        return r


class Stuck(Exception):
    pass


class Net:
    def __init__(self, seed=None):
        self.kind, self.ar, self.tag, self.mate = {}, {}, {}, {}
        self.ids = itertools.count()
        self.queue = deque()
        self.rng = random.Random(seed) if seed is not None else None
        self.stats = Counter()
        self.op = {}          # the copy each dup belongs to
        self.ops = itertools.count()
        self.stuck = []
        self.made = 0         # agents made, placeholders aside
        self.defs = []

    # ---- wiring ----
    def new(self, kind, k=None):
        a = next(self.ids)
        self.kind[a] = kind
        if k is not None:
            self.tag[a] = k
            self.ar[a] = len(self.defs[k][3])
        else:
            self.ar[a] = ARITY[kind]
        if kind != 'ph': self.made += 1
        return a

    def link(self, p, q):
        self.mate[p] = q
        self.mate[q] = p
        if p[1] == 0 and q[1] == 0 and self.kind[p[0]] not in ('root', 'ph') and self.kind[q[0]] not in ('root', 'ph'):
            self.queue.append((p[0], q[0]))

    def remove(self, a):
        for i in range(self.ar[a] + 1):
            self.mate.pop((a, i), None)
        del self.kind[a], self.ar[a]
        self.tag.pop(a, None)
        self.op.pop(a, None)

    # ---- laying out code ----
    def closure(self, k, env):
        c = self.new('cloL' if self.defs[k][0] == 'Λ' else 'clo', k)
        for i, nm in enumerate(self.defs[k][3]): env[nm].append((c, i + 1))
        return (c, 0)

    def code(self, t, env):
        """Code for t; returns its principal port. env maps a name to the list collecting its occurrences."""
        if t == 'ρ': return (self.new('rho'), 0)
        if isinstance(t, str):
            v = self.new('var')
            env[t].append((v, 1))
            return (v, 0)
        if t[0] == '#': return self.closure(t[1], env)
        if is_tab(t):
            a = self.new('tab')
            for j, s in enumerate(t[1:]): self.link((a, j + 1), self.code(s, env))
            return (a, 0)
        a = self.new('app')
        self.link((a, 1), self.code(t[0], env))
        self.link((a, 2), self.code(t[1], env))
        return (a, 0)

    def running(self, t, env, dest):
        """t already running, its value going to the port dest"""
        if isinstance(t, str) and t != 'ρ':
            env[t].append(dest)
        elif is_tab(t):
            a = self.new('tab')
            self.link(dest, (a, 0))
            for j, s in enumerate(t[1:]): self.running(s, env, (a, j + 1))
        elif isinstance(t, tuple) and t[0] != '#':
            o = self.new('O')
            self.link(dest, (o, 2))
            self.running(t[0], env, (o, 0))
            self.running(t[1], env, (o, 1))
        else:
            self.link(dest, self.code(t, env))

    def bind(self, port, occ):
        """Wire a value's port to its occurrences: an eraser for none, a chain of sharers for several."""
        if not occ:
            self.link(port, (self.new('era'), 0))
            return
        while len(occ) > 1:
            s = self.new('sig')
            self.link(port, (s, 0))
            self.link((s, 1), occ.pop(0))
            port = (s, 2)
        self.link(port, occ[0])

    def load(self, t):
        """Put term t in the net as code under a thaw, with its result at a root agent; returns the root."""
        d = Defs()
        top, free = d.convert(t)
        if free: raise NameError(f'nothing binds {free[0]}')
        self.defs = d.table
        root = self.new('root')
        th = self.new('T')
        self.link((th, 0), self.code(top, {}))
        self.link((th, 1), (root, 0))
        return root

    # ---- rewriting ----
    def rewrite(self, a, b, rule):
        """Replace the active pair a, b. rule(slot) builds the replacement; slot(agent, i) is the place where
        agent's auxiliary port i was connected. Placeholders keep internal wires (a's port to b's port) intact."""
        ports = [(x, i) for x in (a, b) for i in range(1, self.ar[x] + 1)]
        ph = {p: self.new('ph') for p in ports}
        for p in ports:
            e = self.mate[p]
            if e in ph:
                if p < e: self.link((ph[p], 0), (ph[e], 0))
            else:
                self.link((ph[p], 0), e)
        self.remove(a)
        self.remove(b)
        rule(lambda x, i: (ph[(x, i)], 1))
        for h in ph.values():
            x, y = self.mate.get((h, 0)), self.mate.get((h, 1))
            if x is None or y is None:
                raise AssertionError(f'placeholder left unconnected: {x} {y}')
            if x != (h, 1):
                self.link(x, y)
            self.remove(h)

    def fire(self, a, b):
        ka, kb = self.kind[a], self.kind[b]
        for (x, y) in ((a, b), (b, a)):
            kx, ky = self.kind[x], self.kind[y]
            f = RULES.get((kx, ky))
            if f is None and kx in ('sig', 'dup') and ky in KIND:
                f = copy_rule
            if f is None and kx == 'era' and ky in KIND:
                f = erase_rule
            if f is not None:
                self.stats[f'copy {ky} by {kx}' if f is copy_rule else f'erase {ky}' if f is erase_rule else f'{kx}⋈{ky}'] += 1
                self.stats['interactions'] += 1
                f(self, x, y)
                return
        self.stuck.append((ka, kb))
        raise Stuck(f'no rule for {ka} ⋈ {kb}')

    def run(self, limit=200000):
        """Rewrite until no pair is left, or past the limit. With a seed, a random waiting pair fires each time;
        with none, the oldest."""
        n = 0
        while self.queue:
            if self.rng is not None and len(self.queue) > 1:
                self.queue.rotate(-self.rng.randrange(len(self.queue)))
            a, b = self.queue.popleft()
            if a not in self.kind or b not in self.kind or self.mate.get((a, 0)) != (b, 0):
                continue
            n += 1
            if n > limit:
                return 'limit'
            try:
                self.fire(a, b)
            except Stuck:
                continue
        return 'stuck' if self.stuck else 'done'

    def size(self):
        return sum(1 for k in self.kind.values() if k != 'root')


# ---- the rules ----
def layout(run):
    def rule(n, o, c):
        binder, x, body, names = n.defs[n.tag[c]]
        def build(slot):
            env = {x: [], **{nm: [] for nm in names}}
            if run:
                n.running(body, env, slot(o, 2))
            else:
                n.link(n.code(body, env), slot(o, 2))
            n.bind(slot(o, 1), env[x])
            for i, nm in enumerate(names): n.bind(slot(c, i + 1), env[nm])
        n.rewrite(o, c, build)
    return rule

def r_read_start(n, o, tb):   # O ⋈ tab
    def build(slot):
        c = n.new('Ctab')
        n.link((c, 0), slot(o, 1))
        for j in range(1, NSLOT + 1): n.link((c, j), slot(tb, j))
        n.link((c, NSLOT + 1), slot(o, 2))
    n.rewrite(o, tb, build)

def r_read(n, c, x):          # Ctab ⋈ X
    kx, tag = n.kind[x], n.tag.get(x)
    i = KIND[kx]
    def build(slot):
        if kx in ('clo', 'cloL'):
            f = n.new('cloL' if kx == 'clo' else 'clo', tag)
            for j in range(1, n.ar[f] + 1): n.link((f, j), slot(x, j))
            parts = [(f, 0)]
        elif kx == 'rho': parts = [(n.new('rho'), 0)]
        else: parts = [slot(x, j) for j in range(1, ARITY[kx] + 1)]
        d = n.new('dup')
        n.op[d] = next(n.ops)
        n.link((d, 0), slot(c, i + 1))
        t = n.new('tab')
        for j in range(NSLOT): n.link((t, j + 1), (d, 1) if j == i else slot(c, j + 1))
        cur = (d, 2)
        for a in [(t, 0)] + parts:
            o = n.new('O')
            n.link((o, 0), cur)
            n.link((o, 1), a)
            cur = (o, 2)
        n.link(cur, slot(c, NSLOT + 1))
    n.rewrite(c, x, build)

def r_thaw_app(n, t, ap):
    def build(slot):
        o, tf, ta = n.new('O'), n.new('T'), n.new('T')
        n.link((tf, 0), slot(ap, 1))
        n.link((tf, 1), (o, 0))
        n.link((ta, 0), slot(ap, 2))
        n.link((ta, 1), (o, 1))
        n.link((o, 2), slot(t, 1))
    n.rewrite(t, ap, build)

def r_thaw_var(n, t, v):
    n.rewrite(t, v, lambda slot: n.link(slot(t, 1), slot(v, 1)))

def r_thaw_tab(n, t, v):
    def build(slot):
        w = n.new('tab')
        n.link((w, 0), slot(t, 1))
        for j in range(1, NSLOT + 1):
            th = n.new('T')
            n.link((th, 0), slot(v, j))
            n.link((th, 1), (w, j))
    n.rewrite(t, v, build)

def r_thaw_value(n, t, v):
    k, tag = n.kind[v], n.tag.get(v)
    def build(slot):
        w = n.new(k, tag)
        n.link((w, 0), slot(t, 1))
        for i in range(1, n.ar[w] + 1): n.link((w, i), slot(v, i))
    n.rewrite(t, v, build)

RULES = {
    ('O', 'clo'): layout(True),
    ('O', 'cloL'): layout(False),
    ('O', 'tab'): r_read_start,
    **{('Ctab', k): r_read for k in KIND},
    ('T', 'app'): r_thaw_app,
    ('T', 'var'): r_thaw_var,
    ('T', 'tab'): r_thaw_tab,
    **{('T', k): r_thaw_value for k in VALUES},
}

def copy_rule(n, c, x):
    """c (a sharer or a copy in flight) meets a code agent x: two copies of x; each auxiliary port of x gets a copy in flight"""
    k, tag = n.kind[x], n.tag.get(x)
    op = n.op.get(c) if n.kind[c] == 'dup' else next(n.ops)
    def build(slot):
        x1, x2 = n.new(k, tag), n.new(k, tag)
        n.link((x1, 0), slot(c, 1))
        n.link((x2, 0), slot(c, 2))
        for i in range(1, n.ar[x1] + 1):
            d = n.new('dup')
            n.op[d] = op
            n.link((d, 0), slot(x, i))
            n.link((d, 1), (x1, i))
            n.link((d, 2), (x2, i))
    n.rewrite(c, x, build)

def erase_rule(n, e, x):
    ar = n.ar[x]
    def build(slot):
        for i in range(1, ar + 1):
            n.link((n.new('era'), 0), slot(x, i))
    n.rewrite(e, x, build)


# ---- reading a finished net back as a term ----
def value_at(n, port):
    """the port of the value an occurrence port leads to, through sharers"""
    src = n.mate[port]
    while n.kind[src[0]] == 'sig' and src[1] in (1, 2):
        src = n.mate[(src[0], 0)]
    return src

def unfold(defs, k, binder, env, fresh):
    """definition k as a λ-term, its captured names read from env"""
    _, x, body, _ = defs[k]
    y = f'y{next(fresh)}'
    env = {**env, x: y}
    def go(t):
        if t == 'ρ': return t
        if isinstance(t, str): return env[t]
        if t[0] == '#': return unfold(defs, t[1], defs[t[1]][0], {nm: env[nm] for nm in defs[t[1]][3]}, fresh)
        if is_tab(t): return ('keep',) + tuple(go(s) for s in t[1:])
        return (go(t[0]), go(t[1]))
    return (binder, y, go(body))

def readback(n, port, fresh=None):
    """The term at a principal port: λ-terms with splices ('~', value) for captured values, ('keep', …) tables."""
    fresh = itertools.count() if fresh is None else fresh
    a, i = port
    if i != 0: raise ValueError(f'not a value: port {port} of {n.kind.get(a)}')
    k = n.kind[a]
    if k in ('clo', 'cloL'):
        names = n.defs[n.tag[a]][3]
        env = {nm: ('~', readback(n, value_at(n, (a, j + 1)), fresh)) for j, nm in enumerate(names)}
        return unfold(n.defs, n.tag[a], 'λ' if k == 'clo' else 'Λ', env, fresh)
    if k == 'app':
        return (readback(n, n.mate[(a, 1)], fresh), readback(n, n.mate[(a, 2)], fresh))
    if k == 'rho':
        return 'ρ'
    if k == 'var':
        return ('~', readback(n, value_at(n, (a, 1)), fresh))
    if k == 'tab':
        return ('keep',) + tuple(readback(n, n.mate[(a, j)], fresh) for j in range(1, NSLOT + 1))
    raise ValueError(f'not finished: {k} at the result')
