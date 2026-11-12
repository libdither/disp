"""Reflective linear nets: a prototype reducer.

Agents (principal port first, then auxiliary ports):
  code, inert, principal facing up toward whoever uses it
    lam(p; x, b)   binder: x hands the argument to the occurrences, b takes the body
    Lam(p; x, b)   opened binder (what reflection hands back): applying it returns the body as code, not run
    app(p; f, a)   frozen application
    var(p; s)      an occurrence: s leads to the binder's x (through sharers), or to a captured value (a splice)
    rho(p)         the reflection primitive, a value
  running
    O(p; a, r)     apply: p faces the function, a the argument, r gives the result
    T(p; o)        thaw: p faces code, o gives the running result
    R(p; o)        reflect: p faces the value being read, o gives its view
  resources
    sig(p; a, b)   sharer in code: the one place a variable splits; fires as a copy once a value arrives
    dup(p; a, b)   a copy in flight
    era(p)         the one eraser

Rules, one per pair:
  O ⋈ lam   run:   x–a, b → T → r
  O ⋈ Lam   open:  x–a, b–r
  O ⋈ rho   reflect:  a → R → r
  T ⋈ app   O with T on f and on a, r → o
  T ⋈ var   s–o   (an occurrence resolves; whatever it leads to is not thawed)
  T ⋈ lam | Lam | rho   the value itself, at o
  R ⋈ lam | Lam | app | var | rho   a view: the arm for that kind of value runs (see view_term).
                                    Reading a function opens it; reading an opened function closes it again.
  sig | dup ⋈ X   copy X (X is any agent but dup and era); the copies' ports get dup agents
  dup ⋈ dup   annihilate: a–a, b–b
  era ⋈ X     erase X; its ports get erasers.  era ⋈ era: both vanish

Agents here ignore which way a port faces, so one `dup` and one `era` stand for the two orientations of
the polarized table (`../lean/Tables.lean`). That table has no rule for an out-facing eraser meeting an
in-facing copier, a pair no run makes; here `era ⋈ dup` covers both ways round.
"""
import itertools, random
from collections import Counter, deque

ARITY = {'lam': 2, 'Lam': 2, 'app': 2, 'var': 1, 'rho': 0,
         'O': 2, 'T': 1, 'R': 1, 'sig': 2, 'dup': 2, 'era': 0, 'root': 0, 'ph': 1}
VALUES = {'lam', 'Lam', 'rho'}
CODE = {'lam', 'Lam', 'app', 'var', 'rho'}

# Reflection hands a value to the arm for its kind, one arm per kind of value; the arm is handed the other
# arms, then the parts. VARIANT picks the table. 'toggle' is the design. The two earlier ones are kept so that
# selfinterp.py can show what they get wrong:
#   'four'  an opened function reads exactly like a closed one
#   'five'  an opened function has its own arm, but stays opened
VARIANT = 'toggle'
PARTS = {'fun': ['F'], 'opened': ['F'], 'app': ['f', 'a'], 'var': ['w'], 'prim': []}
def arms():
    return ['fun', 'app', 'var', 'prim'] if VARIANT == 'four' else ['fun', 'opened', 'app', 'var', 'prim']
def view_term(kind):
    """λk1 … kn. arm others… ~parts"""
    if VARIANT == 'four' and kind == 'opened': kind = 'fun'
    ks = [f'k{i + 1}' for i in range(len(arms()))]
    arm = ks[arms().index(kind)]
    body = arm
    for k in ks:
        if k != arm: body = (body, k)
    for p in PARTS[kind]: body = (body, ('~', p))
    for k in reversed(ks): body = ('λ', k, body)
    return body


class Stuck(Exception):
    pass


class Net:
    def __init__(self, seed=None):
        self.kind = {}
        self.mate = {}
        self.ids = itertools.count()
        self.queue = deque()
        self.rng = random.Random(seed) if seed is not None else None
        self.stats = Counter()
        self.op = {}          # copy operation of each dup, to check annihilations pair up the same copy
        self.ops = itertools.count()
        self.stuck = []

    # ---- wiring ----
    def new(self, kind):
        a = next(self.ids)
        self.kind[a] = kind
        return a

    def link(self, p, q):
        self.mate[p] = q
        self.mate[q] = p
        if p[1] == 0 and q[1] == 0 and self.kind[p[0]] not in ('root', 'ph') and self.kind[q[0]] not in ('root', 'ph'):
            self.queue.append((p[0], q[0]))

    def remove(self, a):
        for i in range(ARITY[self.kind[a]] + 1):
            self.mate.pop((a, i), None)
        del self.kind[a]
        self.op.pop(a, None)

    # ---- building code ----
    def emit(self, t, env):
        """Code for term t; returns its principal port. env maps a bound name to the list collecting its occurrences.
        t: a bound name, (f, a), ('λ', x, body), ('Λ', x, body), ('~', port) a splice of whatever is at port, 'ρ'."""
        if t == 'ρ':
            return (self.new('rho'), 0)
        if isinstance(t, tuple) and len(t) == 2 and t[0] == '~':
            v = self.new('var')
            self.link((v, 1), t[1])
            return (v, 0)
        if isinstance(t, tuple) and len(t) == 3:
            binder, x, body = t
            lam = self.new('Lam' if binder == 'Λ' else 'lam')
            occ = []
            self.link((lam, 2), self.emit(body, {**env, x: occ}))
            self.bind((lam, 1), occ)
            return (lam, 0)
        if isinstance(t, tuple):
            a = self.new('app')
            self.link((a, 1), self.emit(t[0], env))
            self.link((a, 2), self.emit(t[1], env))
            return (a, 0)
        if t not in env: raise NameError(f'nothing binds {t}')
        v = self.new('var')
        env[t].append((v, 1))
        return (v, 0)

    def bind(self, port, occ):
        """Wire a binder's x to its occurrences: an eraser for none, a chain of sharers for several."""
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
        """Put term t in the net under a thaw, with its result at a root agent; returns the root."""
        root = self.new('root')
        th = self.new('T')
        self.link((th, 0), self.emit(t, {}))
        self.link((th, 1), (root, 0))
        return root

    # ---- rewriting ----
    def rewrite(self, a, b, rule):
        """Replace the active pair a, b. rule(slot) builds the replacement; slot(agent, i) is the place where
        agent's auxiliary port i was connected. Placeholders keep internal wires (a's port to b's port) intact."""
        ports = [(x, i) for x in (a, b) for i in range(1, ARITY[self.kind[x]] + 1)]
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
            if x == (h, 1):
                pass                       # a closed loop
            else:
                self.link(x, y)
            self.remove(h)

    def fire(self, a, b):
        ka, kb = self.kind[a], self.kind[b]
        for (x, y) in ((a, b), (b, a)):
            kx, ky = self.kind[x], self.kind[y]
            f = RULES.get((kx, ky))
            if f is None and kx in ('sig', 'dup') and ky not in ('dup', 'era'):
                f = copy_rule
            if f is None and kx == 'era':
                f = erase_rule
            if f is not None:
                self.stats[f'copy {ky} by {kx}' if f is copy_rule else f'erase {ky}' if f is erase_rule else f'{kx}⋈{ky}'] += 1
                self.stats['interactions'] += 1
                f(self, x, y)
                return
        self.stuck.append((ka, kb))
        raise Stuck(f'no rule for {ka} ⋈ {kb}')

    def run(self, limit=200000, width=None):
        """Rewrite until no pair is left, or past the limit. With a seed, a random waiting pair fires each
        time. With none, the pairs fire in rounds: all that wait when a round starts (the oldest `width`
        of them, if a width is given), then all that those made. `rounds` counts the rounds in which a
        pair fired: how long the run takes when `width` pairs can fire at once, or any number. `widest`
        is the most pairs fired in one round, and `peak` the most agents there were when a round began."""
        n = left = fired = 0
        self.rounds = self.widest = 0
        self.peak = self.size()
        while self.queue:
            if self.rng is not None:
                if len(self.queue) > 1: self.queue.rotate(-self.rng.randrange(len(self.queue)))
            elif not left:
                left, fired = len(self.queue), 0
                self.peak = max(self.peak, len(self.kind) - 1)
            left -= 1
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
            fired += 1
            if fired == 1: self.rounds += 1
            if fired > self.widest: self.widest = fired
            if fired == width: left = 0                  # the round is full: the rest wait for the next
        return 'stuck' if self.stuck else 'done'

    def size(self):
        return sum(1 for k in self.kind.values() if k != 'root')


# ---- the rules ----
def r_run(n, o, l):          # O ⋈ lam
    def build(slot):
        t = n.new('T')
        n.link(slot(o, 1), slot(l, 1))
        n.link((t, 0), slot(l, 2))
        n.link((t, 1), slot(o, 2))
    n.rewrite(o, l, build)

def r_open(n, o, l):         # O ⋈ Lam
    n.rewrite(o, l, lambda slot: (n.link(slot(o, 1), slot(l, 1)), n.link(slot(o, 2), slot(l, 2))))

def r_reflect_start(n, o, rho):
    def build(slot):
        r = n.new('R')
        n.link((r, 0), slot(o, 1))
        n.link((r, 1), slot(o, 2))
    n.rewrite(o, rho, build)

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

def r_thaw_value(n, t, v):
    k = n.kind[v]
    def build(slot):
        w = n.new(k)
        n.link((w, 0), slot(t, 1))
        for i in range(1, ARITY[k] + 1):
            n.link((w, i), slot(v, i))
    n.rewrite(t, v, build)

def reflect_with(view, parts):
    def rule(n, r, v):
        def build(slot):
            ports = parts(n, v, slot)
            n.link(n.emit(view_term(view), {}) if not ports else n.emit(subst_parts(view_term(view), ports), {}), slot(r, 1))
        n.rewrite(r, v, build)
    return rule

def subst_parts(t, ports):
    if isinstance(t, tuple) and len(t) == 2 and t[0] == '~':
        return ('~', ports[t[1]])
    if isinstance(t, tuple) and len(t) == 3:
        return (t[0], t[1], subst_parts(t[2], ports))
    if isinstance(t, tuple):
        return (subst_parts(t[0], ports), subst_parts(t[1], ports))
    return t

def function_as(kind):
    """the function goes back to the reader as an agent of this kind, on the same binder and body"""
    def parts(n, v, slot):
        f = n.new(kind())
        n.link((f, 1), slot(v, 1))
        n.link((f, 2), slot(v, 2))
        return {'F': (f, 0)}
    return parts

RULES = {
    ('O', 'lam'): r_run,
    ('O', 'Lam'): r_open,
    ('O', 'rho'): r_reflect_start,
    ('T', 'app'): r_thaw_app,
    ('T', 'var'): r_thaw_var,
    **{('T', k): r_thaw_value for k in VALUES},
    ('R', 'lam'): reflect_with('fun', function_as(lambda: 'Lam')),
    ('R', 'Lam'): reflect_with('opened', function_as(lambda: 'lam' if VARIANT == 'toggle' else 'Lam')),
    ('R', 'app'): reflect_with('app', lambda n, v, slot: {'f': slot(v, 1), 'a': slot(v, 2)}),
    ('R', 'var'): reflect_with('var', lambda n, v, slot: {'w': slot(v, 1)}),
    ('R', 'rho'): reflect_with('prim', lambda n, v, slot: {}),
}

def copy_rule(n, c, x):
    """c (a sharer or a copy in flight) meets x: two copies of x; each auxiliary port of x gets a copy in flight."""
    k = n.kind[x]
    op = n.op.get(c) if n.kind[c] == 'dup' else next(n.ops)
    def build(slot):
        x1, x2 = n.new(k), n.new(k)
        if k == 'dup': n.op[x1] = n.op[x2] = n.op[x]
        n.link((x1, 0), slot(c, 1))
        n.link((x2, 0), slot(c, 2))
        for i in range(1, ARITY[k] + 1):
            d = n.new('dup')
            n.op[d] = op
            n.link((d, 0), slot(x, i))
            n.link((d, 1), (x1, i))
            n.link((d, 2), (x2, i))
    n.rewrite(c, x, build)

def r_annihilate(n, d1, d2):
    if n.op.get(d1) != n.op.get(d2):
        n.stats['annihilated across two copies'] += 1
    n.rewrite(d1, d2, lambda slot: (n.link(slot(d1, 1), slot(d2, 1)), n.link(slot(d1, 2), slot(d2, 2))))
RULES[('dup', 'dup')] = r_annihilate

def erase_rule(n, e, x):
    k = n.kind[x]
    def build(slot):
        for i in range(1, ARITY[k] + 1):
            n.link((n.new('era'), 0), slot(x, i))
    n.rewrite(e, x, build)


# ---- reading a finished net back as a term ----
SHARERS = ('sig',)   # the agents a variable's uses split through
def readback(n, port, names=None):
    """The term at a principal port: λ-terms with splices ('~', value) for captured values."""
    names = {} if names is None else names
    a, i = port
    if i != 0: raise ValueError(f'not a value: port {port} of {n.kind.get(a)}')
    k = n.kind[a]
    if k in ('lam', 'Lam'):
        x = f'x{len(names)}'
        names[(a, 1)] = x
        return ('λ' if k == 'lam' else 'Λ', x, readback(n, n.mate[(a, 2)], names))
    if k == 'app':
        return (readback(n, n.mate[(a, 1)], names), readback(n, n.mate[(a, 2)], names))
    if k == 'rho':
        return 'ρ'
    if k == 'var':
        src = n.mate[(a, 1)]
        while n.kind[src[0]] in SHARERS and src[1] in (1, 2):
            src = n.mate[(src[0], 0)]
        if src in names:
            return names[src]
        return ('~', readback(n, src, names))
    raise ValueError(f'not finished: {k} at the result')
